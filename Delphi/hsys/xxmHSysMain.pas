unit xxmHSysMain;

interface

uses
  SysUtils, ActiveX, xxm, Classes, xxmContext, xxmPReg, xxmThreadPool,
  {$IFDEF HSYS1}httpapi1,{$ENDIF}
  {$IFDEF HSYS2}httpapi2,{$ENDIF}
  xxmPRegXml, xxmParams, xxmParUtils, xxmHeaders;

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
    FURI,FRedirectPrefix,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    procedure SetResponseHeader(id:THTTP_HEADER_ID;const Value:AnsiString);
    procedure CacheString(const x: AnsiString; var xLen: USHORT; var xPtr: PCSTR);
    function GetResponseHeader(const Name:WideString):WideString;
    function GetResponseHeaderCount:integer;
    function GetResponseHeaderName(Idx:integer):WideString;
    function GetResponseHeaderIndex(Idx:integer):WideString;
    procedure SetResponseHeaderIndex(Idx:integer;const Value:WideString);
  protected
    function SendData(const Buffer; Count: LongInt): LongInt;
    procedure DispositionAttach(FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    procedure Redirect(RedirectURL:WideString; Relative:boolean); override;
    function Connected:boolean; override;
    function GetSessionID:WideString; override;
    procedure SendHeader; override;
    function GetCookie(Name:WideString):WideString; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

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

implementation

uses Windows, Variants, ComObj, xxmCommonUtils, xxmHSysHeaders, WinSock;

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

  SendDirect:=SendData;
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

  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FRedirectPrefix:='';
end;

destructor TXxmHSys1Context.Destroy;
begin
  //
  inherited;
end;

procedure TXxmHSys1Context.Execute;
var
  i:integer;
  x:AnsiString;
begin
  try
    FURL:=FReq.CookedUrl.pFullUrl;
    FURI:=FReq.pRawUrl;

    AddResponseHeader('X-Powered-By',SelfVersion);

    i:=2;
    if XxmProjectCache.ProjectFromURI(Self,FURI,i,FProjectName,FFragmentName) then
      FRedirectPrefix:='/'+FProjectName;
    FPageClass:='['+FProjectName+']';
    FQueryStringIndex:=i;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    if FReq.Headers.KnownHeaders[HttpHeaderContentLength].RawValueLength<>0 then
      FPostData:=TXxmPostDataStream.Create(FHSysQueue,FReq.RequestId,
        StrToInt(FReq.Headers.KnownHeaders[HttpHeaderContentLength].pRawValue));

    BuildPage;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
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
  Result:=true;//HttpSend* fails on disconnect
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
    csLanguage:x:=HttpHeaderAcceptLanguage;//HttpHeaderContentLanguage?
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
  SendStr(RedirBody);
  if BufferSize<>0 then Flush;  
  raise EXxmPageRedirected.Create(RedirectURL);
end;

function TXxmHSys1Context.SendData(const Buffer; Count: LongInt): LongInt;
var
  c:THTTP_DATA_CHUNK;
begin
  if Count=0 then Result:=0 else
   begin
    ZeroMemory(@c,SizeOf(THTTP_DATA_CHUNK));
    c.DataChunkType:=HttpDataChunkFromMemory;
    c.pBuffer:=@Buffer;
    c.BufferLength:=Count;
    Result:=Count;
    HttpCheck(HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
      HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
      1,@c,cardinal(Result),nil,0,nil,nil));
   end;
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
  Result:=TxxmHSysResponseHeaders.Create(
    GetResponseHeader,AddResponseHeader,
    GetResponseHeaderCount,GetResponseHeaderName,
    GetResponseHeaderIndex,SetResponseHeaderIndex);
end;

function TXxmHSys1Context.GetResponseHeader(const Name: WideString): WideString;
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
    if i=Length(FUnknownHeaders) then Result:='' else Result:=FUnknownHeaders[i].pRawValue;
   end
  else
    Result:=FRes.Headers.KnownHeaders[x].pRawValue;
end;

function TXxmHSys1Context.GetRequestHeader(const Name: WideString): WideString;
begin
  //TODO: more?
  if Name='If-Modified-Since' then
    Result:=WideString(FReq.Headers.KnownHeaders
      [HttpHeaderIfModifiedSince].pRawValue)
  else
    Result:='';
end;

procedure TXxmHSys1Context.AddResponseHeader(const Name, Value: WideString);
var
  i:integer;
  x:THTTP_HEADER_ID;
begin
  inherited;
  HeaderCheckName(Name);
  HeaderCheckValue(Value);
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

procedure TXxmHSys1Context.SetResponseHeader(id: THTTP_HEADER_ID;
  const Value: AnsiString);
begin
  //TODO: SettingCookie allow multiples
  CacheString(Value,
    FRes.Headers.KnownHeaders[id].RawValueLength,
    FRes.Headers.KnownHeaders[id].pRawValue);
end;

procedure TXxmHSys1Context.CacheString(const x: AnsiString; var xLen: USHORT;
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

function TXxmHSys1Context.GetResponseHeaderCount: integer;
begin
  Result:=integer(HttpHeaderResponseMaximum)+Length(FUnknownHeaders);
  //TODO: skip empty ones?
end;

function TXxmHSys1Context.GetResponseHeaderName(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    Result:=HttpResponseHeaderName[THTTP_HEADER_ID(Idx)]
  else
    if (Idx>=0) and (Idx<Length(FUnknownHeaders)) then
      Result:=FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pName
    else
      raise ERangeError.Create('GetResponseHeaderName: Out of range');
end;

function TXxmHSys1Context.GetResponseHeaderIndex(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    Result:=FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].pRawValue
  else
    if (Idx>=0) and (Idx<Length(FUnknownHeaders)) then
      Result:=FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pRawValue
    else
      raise ERangeError.Create('GetResponseHeaderIndex: Out of range');
end;

procedure TXxmHSys1Context.SetResponseHeaderIndex(Idx: integer;
  const Value: WideString);
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    CacheString(Value,
      FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].RawValueLength,
      FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].pRawValue)
  else
    if (Idx>=0) and (Idx<=Length(FUnknownHeaders)) then
      CacheString(Value,
        FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].RawValueLength,
        FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pRawValue)
    else
      raise ERangeError.Create('SetResponseHeaderIndex: Out of range');
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
