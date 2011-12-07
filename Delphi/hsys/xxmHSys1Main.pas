unit xxmHSys1Main;

interface

uses
  SysUtils, ActiveX, xxm, Classes, xxmContext, xxmPReg, xxmThreadPool,
  xxmHSysPReg, xxmParams, xxmParUtils, xxmHeaders, httpapi1;

const
  XxmHSys1ContextDataSize=$1000;

type
  TXxmPostDataStream=class(TCustomMemoryStream)
  private
    FHSysQueue:THandle;
    FRequestID:THTTP_REQUEST_ID;
    FInputRead,FInputSize:cardinal;
  public
    constructor Create(HSysQueue:THandle;RequestID:THTTP_REQUEST_ID;InputSize:cardinal);
    destructor Destroy; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    procedure SetSize(NewSize: Integer); override;
  end;

  TXxmHSys1Context=class(TXxmQueueContext, IxxmHttpHeaders)
  private
    FData:array[0..XxmHSys1ContextDataSize-1] of byte;
    FHSysQueue:THandle;
    FReq:PHTTP_REQUEST;
    FRes:THTTP_RESPONSE;
    FUnknownHeaders: array of THTTP_UNKNOWN_HEADER;
    FStringCache:array of AnsiString;
    FStringCacheSize,FStringCacheIndex:integer;
    FConnected:boolean;
    FURI,FRedirectPrefix,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    FBuffer:TMemoryStream;
    procedure SetResponseHeader(id:THTTP_HEADER_ID;Value:AnsiString);
    procedure CacheString(x: AnsiString; var xLen: USHORT; var xPtr: PCSTR);
  protected
    procedure SendRaw(Data: WideString); override;
    procedure SendStream(s:IStream); override;
    procedure DispositionAttach(FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    procedure Redirect(RedirectURL:WideString; Relative:boolean); override;
    function Connected:boolean; override;
    function GetSessionID:WideString; override;
    procedure SendHeader; override;
    function GetCookie(Name:WideString):WideString; override;
    procedure SetCookie(Name,Value:WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;
    procedure SetBufferSize(ABufferSize: Integer); override;
    procedure Flush; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    procedure AddResponseHeader(Name, Value: WideString); override;

    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    Queue:TXxmHSys1Context;

    constructor Create(HSysQueue:THandle);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageRedirected=class(Exception);

implementation

uses Windows, Variants, ComObj, xxmCommonUtils, WinSock, xxmHSysHeaders;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  StringCacheGrowStep=$20;

{ TXxmHSys1Context }

constructor TXxmHSys1Context.Create(HSysQueue:THandle);
var
  l:cardinal;
begin
  inherited Create('');//empty here, see Execute
  Queue:=nil;//used by thread pool

  FHSysQueue:=HSysQueue;
  FReq:=PHTTP_REQUEST(@FData[0]);
  ZeroMemory(FReq,XxmHSys1ContextDataSize);
  HttpCheck(HttpReceiveHttpRequest(HSysQueue,HTTP_NULL_ID,
    0,//HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY,
    FReq,XxmHSys1ContextDataSize,l,nil));

  //SetLength(FUnknownHeaders,0);
  ZeroMemory(@FRes,SizeOf(THTTP_RESPONSE));
  FRes.Version:=FReq.Version;//:=HTTP_VERSION_1_1;
  //more: see SendHeader

  FStringCacheSize:=0;
  FStringCacheIndex:=0;

  FConnected:=true;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FBuffer:=nil;
  FRedirectPrefix:='';
end;

destructor TXxmHSys1Context.Destroy;
begin
  if FBuffer<>nil then
   begin
    FBuffer.Free;
    FBuffer:=nil;
   end;
  inherited;
end;

procedure TXxmHSys1Context.Execute;
var
  i,j,l:integer;
  x:AnsiString;
begin
  try
    FURL:=FReq.CookedUrl.pFullUrl;
    FURI:=FReq.pRawUrl;

    AddResponseHeader('X-Powered-By',SelfVersion);

    //TODO: RequestHeaders['Host']?
    l:=Length(FURI);
    i:=2;
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=l) and not(char(FURI[i]) in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(FURI,2,i-2);
      if FProjectName='' then
       begin
        if (i<=l) and (FURI[i]='/') then x:='' else x:='/';
        Redirect('/'+XxmProjectCache.DefaultProject+x+Copy(FURI,i,l-i+1),false);
       end;
      FPageClass:='['+FProjectName+']';
      if (i>l) and (l>1) then Redirect(FURI+'/',false) else
        if (FURI[i]='/') then inc(i);
      FRedirectPrefix:='/'+FProjectName;
     end
    else
     begin
      FProjectName:=XxmProjectCache.SingleProject;
      FPageClass:='[SingleProject]';
     end;
    j:=i;
    while (i<=l) and not(char(FURI[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURI,j,i-j);
    if (i<=l) then inc(i);
    FQueryStringIndex:=i;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    if FReq.Headers.KnownHeaders[HttpHeaderContentLength].RawValueLength<>0 then
      FPostData:=TXxmPostDataStream.Create(FHSysQueue,FReq.RequestId,
        StrToInt(FReq.Headers.KnownHeaders[HttpHeaderContentLength].pRawValue));

    BuildPage;

  except
    on e:EXxmPageRedirected do
      ;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        //TODO: get fragment 500.xxm?
        ForceStatus(500,'Internal Server Error');//TODO:setting?
        try
          if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',[
          'ERRORCLASS',e.ClassName,
          'ERROR',HTMLEncode(e.Message),
          'CLASS',FPageClass,
          'URL',HTMLEncode(ContextString(csURL)),
          'POSTDATA',x,
          'QUERYSTRING',HTMLEncode(ContextString(csQueryString)),
          'VERSION',ContextString(csVersion)
        ]);
       end;
  end;
  //assert HttpSendHttpResponse done
  //HttpCheck(
  HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT,//if keep-alive?
    0,nil,cardinal(nil^),nil,0,nil,nil);
end;

function TXxmHSys1Context.GetProjectEntry: TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmHSys1Context.Connected: boolean;
begin
  Result:=FConnected;
  //TODO: set to false when client disconnect
end;

function TXxmHSys1Context.ContextString(cs: TXxmContextString): WideString;
const
  HttpVerb:array[THTTP_VERB] of WideString=(
    '',//HttpVerbUnparsed,
    '',//HttpVerbUnknown,
    '',//HttpVerbInvalid,
    'OPTIONS',//HttpVerbOPTIONS,
    'GET',//HttpVerbGET,
    'HEAD',//HttpVerbHEAD,
    'POST',//HttpVerbPOST,
    'PUT',//HttpVerbPUT,
    'DELETE',//HttpVerbDELETE,
    'TRACE',//HttpVerbTRACE,
    'CONNECT',//HttpVerbCONNECT,
    'TRACK',//HttpVerbTRACK,
    'MOVE',//HttpVerbMOVE,
    'COPY',//HttpVerbCOPY,
    'PROPFIND',//HttpVerbPROPFIND,
    'PROPPATCH',//HttpVerbPROPPATCH,
    'MKCOL',//HttpVerbMKCOL,
    'LOCK',//HttpVerbLOCK,
    'UNLOCK',//HttpVerbUNLOCK,
    'SEARCH',//HttpVerbSEARCH,
    '' //HttpVerbMaximum
  );
var
  x:THTTP_HEADER_ID;
begin
  x:=THTTP_HEADER_ID(-1);
  case cs of
    csVersion:Result:=SelfVersion;//+' '+??HttpHeaderServer ? 'Microsoft-HTTPAPI/1.0'?
    csExtraInfo:Result:='';//???
    csVerb:
      if FReq.Verb in [HttpVerbUnparsed,HttpVerbUnknown,HttpVerbInvalid] then
        Result:=FReq.pUnknownVerb
      else
        Result:=HttpVerb[FReq.Verb];
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:x:=HttpHeaderUserAgent;
    csAcceptedMimeTypes:x:=HttpHeaderAccept;
    csPostMimeType:x:=HttpHeaderContentType;
    csURL:Result:=FReq.pRawUrl;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:x:=HttpHeaderReferer;
    csLanguage:x:=HttpHeaderContentLanguage;
    csRemoteAddress:Result:=inet_ntoa(FReq.Address.pRemoteAddress.sin_addr);
    csRemoteHost:Result:=inet_ntoa(FReq.Address.pRemoteAddress.sin_addr);//TODO: resolve name
    csAuthUser:;//TODO:Result:=GetCGIValue('AUTH_USER');
    csAuthPassword:;//TODO:Result:=GetCGIValue('AUTH_PASSWORD');
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  if x<>THTTP_HEADER_ID(-1) then Result:=FReq.Headers.KnownHeaders[x].pRawValue;
end;

procedure TXxmHSys1Context.DispositionAttach(FileName: WideString);
begin
  AddResponseHeader('Content-disposition','attachment; filename="'+FileName+'"');
end;

function TXxmHSys1Context.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReq.Headers.KnownHeaders[HttpHeaderCookie].pRawValue;
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmHSys1Context.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  SetResponseHeader(HttpHeaderCacheControl,'no-cache="set-cookie"');
  SetResponseHeader(HttpHeaderSetCookie,Name+'="'+Value+'"');
end;

procedure TXxmHSys1Context.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:WideString;
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  SetResponseHeader(HttpHeaderCacheControl,'no-cache="set-cookie"');
  x:=Name+'="'+Value+'"';
  //'; Version=1';
  if Comment<>'' then
    x:=x+'; Comment="'+Comment+'"';
  if Domain<>'' then
    x:=x+'; Domain="'+Domain+'"';
  if Path<>'' then
    x:=x+'; Path="'+Path+'"';
  x:=x+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires="'+RFC822DateGMT(Now+KeepSeconds/86400)+'"';
  if Secure then
    x:=x+'; Secure'+#13#10;
  if HttpOnly then
    x:=x+'; HttpOnly'+#13#10;
  SetResponseHeader(HttpHeaderSetCookie,x);
  //TODO: Set-Cookie2
end;

function TXxmHSys1Context.GetSessionID: WideString;
const
  SessionCookie='xxmSessionID';
begin
  if FSessionID='' then
   begin
    FSessionID:=GetCookie(SessionCookie);
    if FSessionID='' then
     begin
      FSessionID:=Copy(CreateClassID,2,32);
      SetCookie(SessionCookie,FSessionID);//expiry?
     end;
   end;
  Result:=FSessionID;
end;

procedure TXxmHSys1Context.Redirect(RedirectURL: WideString;
  Relative: boolean);
var
  NewURL,RedirBody:WideString;
begin
  inherited;
  SetStatus(301,'Moved Permanently');//does CheckHeaderNotSent;
  //TODO: move this to execute's except?
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then NewURL:=FRedirectPrefix+NewURL;
  RedirBody:='<a href="'+HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a>'#13#10;
  SetResponseHeader(HttpHeaderLocation,NewURL);
  case FAutoEncoding of
    aeUtf8:SetResponseHeader(HttpHeaderContentLength,IntToStr(Length(UTF8Encode(RedirBody))+3));
    aeUtf16:SetResponseHeader(HttpHeaderContentLength,IntToStr(Length(RedirBody)*2+2));
    aeIso8859:SetResponseHeader(HttpHeaderContentLength,IntToStr(Length(AnsiString(RedirBody))));
  end;
  SendRaw(RedirBody);
  if FBufferSize<>0 then Flush;  
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmHSys1Context.SendRaw(Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;

  procedure SendChunk(x:pointer;l:cardinal);
  var
    c:THTTP_DATA_CHUNK;
  begin
    ZeroMemory(@c,SizeOf(THTTP_DATA_CHUNK));
    c.DataChunkType:=HttpDataChunkFromMemory;
    c.pBuffer:=x;
    c.BufferLength:=l;
    HttpCheck(HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
      HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
      1,@c,l,nil,0,nil,nil));
  end;

begin
  //TODO: catch WriteFile returned values!
  if Data<>'' then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
          if FBuffer=nil then
            SendChunk(@Utf8ByteOrderMark[1],3)
          else
            FBuffer.Write(Utf8ByteOrderMark[1],3);
        aeUtf16:
          if FBuffer=nil then
            SendChunk(@Utf16ByteOrderMark[1],2)
          else
            FBuffer.Write(Utf16ByteOrderMark[1],2);
      end;
    case FAutoEncoding of
      aeUtf16:
        if FBuffer=nil then
          SendChunk(@Data[1],Length(Data)*2)
        else
          FBuffer.Write(Data[1],Length(Data)*2);
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        if FBuffer=nil then
          SendChunk(@s[1],Length(s))
        else
          FBuffer.Write(s[1],Length(s));
       end;
      else
       begin
        s:=Data;
        if FBuffer=nil then
          SendChunk(@s[1],Length(s))
        else
          FBuffer.Write(s[1],Length(s));
       end;
    end;
    if (FBuffer<>nil) and (FBuffer.Position>=FBufferSize) then Flush;
   end;
end;

procedure TXxmHSys1Context.SendStream(s: IStream);
const
  dSize=$10000;
var
  l,l1:cardinal;
  d:array[0..dSize-1] of byte;
  c:THTTP_DATA_CHUNK;
begin
  CheckSendStart;
  ZeroMemory(@c,SizeOf(THTTP_DATA_CHUNK));
  c.DataChunkType:=HttpDataChunkFromMemory;
  c.pBuffer:=@d[0];
  repeat
    l:=dSize;
    OleCheck(s.Read(@d[0],l,@l));
    if l<>0 then
     begin
      if FBuffer<>nil then Flush;
      c.BufferLength:=l;
      HttpCheck(HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
        HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
        1,@c,l1,nil,0,nil,nil));
      if l<>l1 then raise Exception.Create('Stream Write Failed');
     end;
  until l=0;
end;

procedure TXxmHSys1Context.SendHeader;
var
  l:cardinal;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  //TODO: Content-Length?
  //TODO: Connection keep?
  FRes.StatusCode:=StatusCode;
  CacheString(StatusText,FRes.ReasonLength,FRes.pReason);
  if FAutoEncoding<>aeContentDefined then
    CacheString(FContentType+AutoEncodingCharset[FAutoEncoding],
      FRes.Headers.KnownHeaders[HttpHeaderContentType].RawValueLength,
      FRes.Headers.KnownHeaders[HttpHeaderContentType].pRawValue);
  l:=Length(FUnknownHeaders);
  FRes.Headers.UnknownHeaderCount:=l;
  if l=0 then
    FRes.Headers.pUnknownHeaders:=nil
  else
    FRes.Headers.pUnknownHeaders:=@FUnknownHeaders[0];
  HttpCheck(HttpSendHttpResponse(FHSysQueue,FReq.RequestId,
    HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
    @FRes,nil,l,nil,0,nil,nil));
end;

function TXxmHSys1Context.GetRequestHeaders: IxxmDictionaryEx;
var
  s:AnsiString;
  x:THTTP_HEADER_ID;
  i:integer;
type
  THTTP_UNKNOWN_HEADER_ARRAY=array[0..0] of THTTP_UNKNOWN_HEADER;
  PHTTP_UNKNOWN_HEADER_ARRAY=^THTTP_UNKNOWN_HEADER_ARRAY;
begin
  s:='';
  for x:=HttpHeaderStart to HttpHeaderMaximum do
    if FReq.Headers.KnownHeaders[x].RawValueLength<>0 then
      s:=s+HttpRequestHeaderName[x]+': '+FReq.Headers.KnownHeaders[x].pRawValue+#13#10;
  for i:=0 to FReq.Headers.UnknownHeaderCount-1 do
    s:=s+PHTTP_UNKNOWN_HEADER_ARRAY(FReq.Headers.pUnknownHeaders)[i].pName+': '+
      PHTTP_UNKNOWN_HEADER_ARRAY(FReq.Headers.pUnknownHeaders)[i].pRawValue+#13#10;
  Result:=TRequestHeaders.Create(s+#13#10);
end;

function TXxmHSys1Context.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=nil;//TODO: TxxmHSysResponseHeaders.Create(
end;

procedure TXxmHSys1Context.AddResponseHeader(Name, Value: WideString);
var
  i:integer;
  x:THTTP_HEADER_ID;
begin
  inherited;
  //TODO: encode when non-UTF7 characters?
  x:=HttpHeaderStart;
  while (x<=HttpHeaderResponseMaximum) and (CompareText(HttpResponseHeaderName[x],Name)<>0) do inc(x);
  if x>HttpHeaderResponseMaximum then
   begin
    i:=0;
    while (i<Length(FUnknownHeaders)) and (CompareText(FUnknownHeaders[i].pName,Name)<>0) do inc(i);
    if i=Length(FUnknownHeaders) then
     begin
      SetLength(FUnknownHeaders,i+1);
      CacheString(Name,FUnknownHeaders[i].NameLength,FUnknownHeaders[i].pName);
     end;
    CacheString(Value,FUnknownHeaders[i].RawValueLength,FUnknownHeaders[i].pRawValue);
   end
  else
    CacheString(Value,FRes.Headers.KnownHeaders[x].RawValueLength,FRes.Headers.KnownHeaders[x].pRawValue);
end;

procedure TXxmHSys1Context.Flush;
var
  i,l:cardinal;
  c:THTTP_DATA_CHUNK;
begin
  if FBuffer<>nil then
   begin
    i:=FBuffer.Position;
    if i<>0 then
     begin
      ZeroMemory(@c,SizeOf(THTTP_DATA_CHUNK));
      c.DataChunkType:=HttpDataChunkFromMemory;
      c.pBuffer:=FBuffer.Memory;
      c.BufferLength:=i;
      HttpCheck(HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
        HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
        1,@c,l,nil,0,nil,nil));
      FBuffer.Position:=0;
     end;
   end;
end;

procedure TXxmHSys1Context.SetBufferSize(ABufferSize: Integer);
begin
  inherited;
  if ABufferSize=0 then
   begin
    if FBuffer<>nil then
     begin
      Flush;
      FBuffer.Free;
      FBuffer:=nil;
     end;
   end
  else
   begin
    if FBuffer=nil then FBuffer:=TMemoryStream.Create;//TODO: tmp file when large buffer
    if FBuffer.Position>ABufferSize then Flush;
    FBuffer.Size:=ABufferSize;
   end;
end;

procedure TXxmHSys1Context.SetResponseHeader(id: THTTP_HEADER_ID;
  Value: AnsiString);
begin
  CacheString(Value,
    FRes.Headers.KnownHeaders[id].RawValueLength,
    FRes.Headers.KnownHeaders[id].pRawValue);
end;

procedure TXxmHSys1Context.CacheString(x: AnsiString; var xLen: USHORT;
  var xPtr: PCSTR);
begin
  //TODO: check duplicate?
  if FStringCacheIndex=FStringCacheSize then
   begin
    inc(FStringCacheSize,StringCacheGrowStep);
    SetLength(FStringCache,FStringCacheSize);
   end;
  FStringCache[FStringCacheIndex]:=x;
  inc(FStringCacheIndex);
  xLen:=Length(x);
  xPtr:=PAnsiChar(x);
end;

{ TXxmPostDataStream }

constructor TXxmPostDataStream.Create(HSysQueue:THandle;RequestID:THTTP_REQUEST_ID;InputSize:cardinal);
begin
  inherited Create;
  FHSysQueue:=HSysQueue;
  FRequestID:=RequestID;
  FInputRead:=0;
  FInputSize:=InputSize;
  SetPointer(GlobalAllocPtr(GMEM_MOVEABLE,FInputSize),FInputSize);
end;

destructor TXxmPostDataStream.Destroy;
begin
  GlobalFreePtr(Memory);
  inherited;
end;

function TXxmPostDataStream.Read(var Buffer; Count: Integer): Integer;
var
  l:cardinal;
  p:pointer;
begin
  l:=Position+Count;
  if l>FInputSize then l:=FInputSize;
  if l>FInputRead then
   begin
    dec(l,FInputRead);
    if l<>0 then
     begin
      p:=Memory;
      inc(cardinal(p),FInputRead);
      HttpCheck(HttpReceiveRequestEntityBody(FHSysQueue,FRequestId,
        HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER,
        p,l,l,nil));
      inc(FInputRead,l);
     end;
   end;
  Result:=inherited Read(Buffer,Count);
end;

procedure TXxmPostDataStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create('Post data is read-only.');
end;

function TXxmPostDataStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('Post data is read-only.');
end;

end.
