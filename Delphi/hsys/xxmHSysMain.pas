unit xxmHSysMain;

interface

uses
  Windows, SysUtils, ActiveX, Classes, xxm, xxmContext, xxmThreadPool,
  {$IFDEF HSYS1}httpapi1,{$ENDIF}
  {$IFDEF HSYS2}httpapi2,{$ENDIF}
  xxmPReg, xxmPRegJson, xxmParams, xxmParUtils, xxmHeaders;

const
  XxmHSysContextDataSize=$1000;

type
  TXxmPostDataStream=class(TCustomMemoryStream)
  private
    FHSysQueue:THandle;
    FRequestID:THTTP_REQUEST_ID;
    FInputRead,FInputSize:cardinal;
  public
    constructor Create(HSysQueue:THandle;RequestID:THTTP_REQUEST_ID;
      InputSize:cardinal);
    destructor Destroy; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    procedure SetSize(NewSize: Integer); override;
  end;

  TXxmHSysContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmContextSuspend)
  private
    FData:array[0..XxmHSysContextDataSize-1] of byte;
    FHSysQueue:THandle;
    FReq:PHTTP_REQUEST;
    FRes:THTTP_RESPONSE;
    FUnknownHeaders: array of THTTP_UNKNOWN_HEADER;
    FStringCache:array of AnsiString;
    FStringCacheSize,FStringCacheIndex:integer;
    FURI:AnsiString;
    FRedirectPrefix,FSessionID:WideString;
    FCookieParsed, FAuthStoreCache: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    FReqHeaders:TRequestHeaders;
    procedure SetResponseHeader(id:THTTP_HEADER_ID;const Value:AnsiString);
    procedure CacheString(const x: AnsiString; var xLen: USHORT; var xPtr: PCSTR);
    function GetResponseHeaderCount:integer;
    function GetResponseHeaderName(Idx:integer):WideString;
    function GetResponseHeaderIndex(Idx:integer):WideString;
    procedure SetResponseHeaderIndex(Idx:integer;const Value:WideString);
    procedure ResponseStr(const Body,RedirMsg:WideString);
    procedure AuthNTLM;
  protected
    function SendData(const Buffer; Count: LongInt): LongInt;
    procedure DispositionAttach(const FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    procedure Redirect(const RedirectURL:WideString; Relative:boolean); override;
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;
    function Connected:boolean; override;
    function GetSessionID:WideString; override;
    procedure SendHeader; override;
    function GetCookie(const Name:WideString):WideString; override;
    {$IFDEF HSYS2}
    function GetRawSocket: IStream; override;
    {$ENDIF}

    function GetProjectEntry:TXxmProjectEntry; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    function GetResponseHeader(const Name:WideString):WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

    {  }
    function GetProjectPage(const FragmentName: WideString):IXxmFragment; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Load(HSysQueue:THandle);
  end;

{$IFDEF HSYS2}
const
  RawSocketBufferSize=$10000;

type
  {$IF not(Declared(FixedUInt))}
  FixedUInt=LongInt;
  PFixedUInt=PLongInt;
  LargeUInt=LargeInt;
  XDWORD=LongInt;
  {$ELSE}
  XDWORD=DWORD;
  {$IFEND}
  TRawSocketData=class(TInterfacedObject, IStream, IXxmRawSocket)
  private
    FHSysQueue:THandle;
    FRequestID:THTTP_REQUEST_ID;
    FBuffer:array[0..RawSocketBufferSize-1] of byte;
    FBuffer1,FBuffer2:cardinal;
    FCallOut:boolean;
    FCall:TOverlapped;
  public
    constructor Create(HSysQueue:THandle;RequestID:THTTP_REQUEST_ID);
    destructor Destroy; override;
    { IStream }
    function Seek(dlibMove: Largeint; dwOrigin: XDWORD;
      out libNewPosition: LargeUInt): HResult; stdcall;
    function SetSize(libNewSize: LargeUInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt;
      out cbWritten: LargeUInt): HResult; stdcall;
    function Commit(grfCommitFlags: XDWORD): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: LargeUInt; cb: LargeUInt;
      dwLockType: XDWORD): HResult; stdcall;
    function UnlockRegion(libOffset: LargeUInt; cb: LargeUInt;
      dwLockType: XDWORD): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: XDWORD): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
    { ISequentialStream }
    function Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
      stdcall;
    function Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
      stdcall;
    { IXxmRawSocket }
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure Disconnect;
  end;
{$ENDIF}

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

implementation

uses Variants, ComObj, xxmCommonUtils, xxmHSysHeaders, WinSock,
  Math, xxmSSPI;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmContextAlreadySuspended='Context has already been suspended';

const
  StringCacheGrowStep=$20;

var
  SessionCookie:string;

{ TXxmHSysContext }

procedure TXxmHSysContext.AfterConstruction;
begin
  inherited;
  SendDirect:=SendData;
  FReqHeaders:=nil;//TRequestHeaders.Create;//see GetRequestHeaders
end;

destructor TXxmHSysContext.Destroy;
begin
  FreeAndNil(FReqHeaders);
  inherited;
end;

procedure TXxmHSysContext.Load(HSysQueue:THandle);
var
  l:cardinal;
begin
  FHSysQueue:=HSysQueue;
  FReq:=PHTTP_REQUEST(@FData[0]);
  ZeroMemory(FReq,XxmHSysContextDataSize);
  HttpCheck(HttpReceiveHttpRequest(HSysQueue,HTTP_NULL_ID,
    0,//HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY,
    FReq,XxmHSysContextDataSize,l,nil));

  //SetLength(FUnknownHeaders,0);
  ZeroMemory(@FRes,SizeOf(THTTP_RESPONSE));
  FRes.Version:=FReq.Version;//:=HTTP_VERSION_1_1;
  //more: see SendHeader

  BeginRequest;
  PageLoaderPool.Queue(Self,ctHeaderNotSent);
end;

procedure TXxmHSysContext.BeginRequest;
begin
  inherited;
  FStringCacheSize:=0;
  FStringCacheIndex:=0;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FRedirectPrefix:='';
  if FReqHeaders<>nil then FReqHeaders.Reset; 
end;

procedure TXxmHSysContext.EndRequest;
var
  f:cardinal;
begin
  inherited;
  //assert HttpSendHttpResponse done
  if (FRes.Headers.KnownHeaders[HttpHeaderContentLength].RawValueLength=0) and
    (FRes.Headers.KnownHeaders[HttpHeaderTransferEncoding].pRawValue<>'chunked') then
    f:=HTTP_SEND_RESPONSE_FLAG_DISCONNECT
  else
    f:=0;
  //HttpCheck(
  HttpSendResponseEntityBody(FHSysQueue,FReq.RequestId,
    f,0,nil,cardinal(nil^),nil,0,nil,nil);
end;

procedure TXxmHSysContext.HandleRequest;
var
  i:integer;
  x:AnsiString;
begin
  try
    FURL:=FReq.CookedUrl.pFullUrl;
    FURI:=FReq.pRawUrl;
    AllowChunked:=//FReq.Version=HTTP_VERSION_1_1;
      (FReq.Version.MajorVersion=1) and (FReq.Version.MinorVersion=1);

    //AddResponseHeader('X-Powered-By',SelfVersion);

    i:=2;
    if XxmProjectCache.ProjectFromURI(Self,FURI,i,FProjectName,FFragmentName) then
      FRedirectPrefix:='/'+FProjectName;
    FPageClass:='['+FProjectName+']';
    FQueryStringIndex:=i;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    if FReq.Headers.KnownHeaders[HttpHeaderContentLength].RawValueLength<>0 then
      FPostData:=TXxmPostDataStream.Create(FHSysQueue,FReq.RequestId,
        StrToInt(string(FReq.Headers.KnownHeaders[HttpHeaderContentLength].pRawValue)));

    BuildPage;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');//TODO:setting?
        try
          if FPostData=nil then x:='none' else x:=AnsiString(IntToStr(FPostData.Size))+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',e.ClassName,e.Message);
       end;
  end;
end;

procedure TXxmHsysContext.AuthNTLM;
var
  s,t:AnsiString;
  c:PCredHandle;
  p,p1:PCtxtHandle;
  r,f:cardinal;
  d1,d2:TSecBufferDesc;
  d:array of TSecBuffer;
  n:TSecPkgContextNames;
begin
  FAuthStoreCache:=false;//default
  s:=XxmProjectCache.GetAuthCache(GetCookie(SessionCookie));
  if s<>'' then
    AuthSet(s,'') //TODO: update Expires?
  else
   begin
    s:=AuthParse('NTLM');
    if s='' then
     begin
      SetStatus(401,'Unauthorized');
      SetResponseHeader(HttpHeaderConnection,'keep-alive');
      SetResponseHeader(HttpHeaderWwwAuthenticate,'NTLM');
      ResponseStr('<h1>Authorization required</h1>','401');
     end
    else
     begin
      SSPICache.GetContext(FReq.ConnectionId,c,p);

      SetLength(d,3);
      SetLength(t,$10000);

      d1.ulVersion:=SECBUFFER_VERSION;
      d1.cBuffers:=2;
      d1.pBuffers:=@d[0];

      d[0].cbBuffer:=Length(s);
      d[0].BufferType:=SECBUFFER_TOKEN;
      d[0].pvBuffer:=@s[1];

      d[1].cbBuffer:=0;
      d[1].BufferType:=SECBUFFER_EMPTY;
      d[1].pvBuffer:=nil;

      d2.ulVersion:=SECBUFFER_VERSION;
      d2.cBuffers:=1;
      d2.pBuffers:=@d[2];

      d[2].cbBuffer:=$10000;;
      d[2].BufferType:=SECBUFFER_TOKEN;
      d[2].pvBuffer:=@t[1];

      if (p.dwLower=nil) and (p.dwUpper=nil) then p1:=nil else p1:=p;
      r:=AcceptSecurityContext(c,p1,@d1,
        ASC_REQ_REPLAY_DETECT or ASC_REQ_SEQUENCE_DETECT,SECURITY_NATIVE_DREP,
        p,@d2,@f,nil);

      if r=SEC_E_OK then
       begin
        r:=QueryContextAttributes(p,SECPKG_ATTR_NAMES,@n);
        if r=0 then
          AuthSet(n.sUserName,'')
        else
          AuthSet(AnsiString('???'+SysErrorMessage(r)),'');//raise?
        SSPICache.Clear(FReq.ConnectionId);
       end
      else
      if r=SEC_I_CONTINUE_NEEDED then
       begin
        SetLength(t,d[2].cbBuffer);
        SetStatus(401,'Unauthorized');
        AddResponseHeader('Connection','keep-alive');
        AddResponseHeader('WWW-Authenticate',WideString('NTLM '+Base64Encode(t)));
        ResponseStr('<h1>Authorization required</h1>','401.1');
       end
      else
        raise Exception.Create(SysErrorMessage(r));
     end;
   end;
end;

function TXxmHSysContext.GetProjectPage(const FragmentName: WideString):IXxmFragment;
begin
  if (ProjectEntry as TXxmProjectCacheEntry).NTLM then AuthNTLM;
  Result:=inherited GetProjectPage(FragmentName);
end;

function TXxmHSysContext.GetProjectEntry: TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmHSysContext.Connected: boolean;
begin
  Result:=true;//HttpSend* fails on disconnect
  //TODO: async HttpWaitForDisconnect?
end;

function TXxmHSysContext.ContextString(cs: TXxmContextString): WideString;
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
    csVersion:Result:=SelfVersion;//+' '+??HttpHeaderServer ? 'Microsoft-HTTPAPI/?.0'?
    csExtraInfo:Result:='';//???
    csVerb:
      if FReq.Verb in [HttpVerbUnparsed,HttpVerbUnknown,HttpVerbInvalid] then
        Result:=WideString(FReq.pUnknownVerb)
      else
        Result:=HttpVerb[FReq.Verb];
    csQueryString:Result:=UTF8ToWideString(Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1));
    csUserAgent:x:=HttpHeaderUserAgent;
    csAcceptedMimeTypes:x:=HttpHeaderAccept;
    csPostMimeType:x:=HttpHeaderContentType;
    csURL:Result:=UTF8ToWideString(FReq.pRawUrl);
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:x:=HttpHeaderReferer;
    csLanguage:x:=HttpHeaderAcceptLanguage;//HttpHeaderContentLanguage?
    csRemoteAddress:Result:=WideString(inet_ntoa(FReq.Address.pRemoteAddress.sin_addr));
    csRemoteHost:Result:=WideString(inet_ntoa(FReq.Address.pRemoteAddress.sin_addr));//TODO: resolve name
    csAuthUser,csAuthPassword:Result:=UTF8ToWideString(AuthValue(cs));
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  if x<>THTTP_HEADER_ID(-1) then Result:=UTF8ToWideString(FReq.Headers.KnownHeaders[x].pRawValue);
end;

procedure TXxmHSysContext.DispositionAttach(const FileName: WideString);
var
  s:WideString;
  i:integer;
begin
  s:=FileName;
  for i:=1 to Length(s) do
    if AnsiChar(s[i]) in ['\','/',':','*','?','"','<','>','|'] then
      s[i]:='_';
  AddResponseHeader('Content-disposition','attachment; filename="'+s+'"');
end;

function TXxmHSysContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReq.Headers.KnownHeaders[HttpHeaderCookie].pRawValue;
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=UTF8ToWideString(GetParamValue(FCookie,FCookieIdx,UTF8Encode(Name)));
end;

function TXxmHSysContext.GetSessionID: WideString;
begin
  if FSessionID='' then
   begin
    FSessionID:=GetCookie(SessionCookie);
    if FSessionID='' then
     begin
      FSessionID:=Copy(CreateClassID,2,32);
      SetCookie(SessionCookie,FSessionID);//expiry?
     end;
    if FAuthStoreCache then
      XxmProjectCache.SetAuthCache(FSessionID,AuthValue(csAuthUser));
   end;
  Result:=FSessionID;
end;

procedure TXxmHSysContext.Redirect(const RedirectURL: WideString;
  Relative: boolean);
var
  NewURL:WideString;
begin
  inherited;
  SetStatus(301,'Moved Permanently');//does CheckHeaderNotSent;
  //TODO: move this to execute's except?
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then
    NewURL:=FRedirectPrefix+NewURL;
  SetResponseHeader(HttpHeaderLocation,UTF8Encode(NewURL));
  ResponseStr('<h1>Object moved</h1><a href="'+
    HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a>'#13#10,RedirectURL);
end;

function TXxmHSysContext.SendData(const Buffer; Count: LongInt): LongInt;
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

procedure TXxmHsysContext.ResponseStr(const Body,RedirMsg:WideString);
begin
  case FAutoEncoding of
    aeUtf8:SetResponseHeader(HttpHeaderContentLength,
      AnsiString(IntToStr(Length(UTF8Encode(Body))+3)));
    aeUtf16:SetResponseHeader(HttpHeaderContentLength,
      AnsiString(IntToStr(Length(Body)*2+2)));
    aeIso8859:SetResponseHeader(HttpHeaderContentLength,
      AnsiString(IntToStr(Length(AnsiString(Body)))));
  end;
  SendStr(Body);
  if BufferSize<>0 then Flush;
  raise EXxmPageRedirected.Create(RedirMsg);
end;

procedure TXxmHSysContext.SendHeader;
var
  l,f:cardinal;
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
  CacheString(AnsiString(StatusText),FRes.ReasonLength,FRes.pReason);
  if (FContentType<>'') and (FAutoEncoding<>aeContentDefined) then
    CacheString(AnsiString(FContentType+AutoEncodingCharset[FAutoEncoding]),
      FRes.Headers.KnownHeaders[HttpHeaderContentType].RawValueLength,
      FRes.Headers.KnownHeaders[HttpHeaderContentType].pRawValue);
  l:=Length(FUnknownHeaders);
  FRes.Headers.UnknownHeaderCount:=l;
  if l=0 then
    FRes.Headers.pUnknownHeaders:=nil
  else
    FRes.Headers.pUnknownHeaders:=@FUnknownHeaders[0];
  f:=HTTP_SEND_RESPONSE_FLAG_MORE_DATA;
  {$IFDEF HSYS2}
  if StatusCode=101 then f:=f or HTTP_SEND_RESPONSE_FLAG_OPAQUE;
  {$ENDIF}
  HttpCheck(HttpSendHttpResponse(FHSysQueue,FReq.RequestId,
    f,@FRes,nil,l,nil,0,nil,nil));
  inherited;
end;

function TXxmHSysContext.GetRequestHeaders: IxxmDictionaryEx;
var
  s:AnsiString;
  x:THTTP_HEADER_ID;
  i:integer;
type
  THTTP_UNKNOWN_HEADER_ARRAY=array[0..0] of THTTP_UNKNOWN_HEADER;
  PHTTP_UNKNOWN_HEADER_ARRAY=^THTTP_UNKNOWN_HEADER_ARRAY;
begin
  if FReqHeaders=nil then FReqHeaders:=TRequestHeaders.Create;
  if FReqHeaders.Count=0 then //assert at least one header
   begin
    s:='';
    for x:=HttpHeaderStart to HttpHeaderMaximum do
      if FReq.Headers.KnownHeaders[x].RawValueLength<>0 then
        s:=s+HttpRequestHeaderName[x]+': '
          +FReq.Headers.KnownHeaders[x].pRawValue+#13#10;
    for i:=0 to FReq.Headers.UnknownHeaderCount-1 do
      s:=s+PHTTP_UNKNOWN_HEADER_ARRAY(FReq.Headers.pUnknownHeaders)[i].pName+': '+
        PHTTP_UNKNOWN_HEADER_ARRAY(FReq.Headers.pUnknownHeaders)[i].pRawValue+#13#10;
    FReqHeaders.Load(s+#13#10);
   end;
  Result:=FReqHeaders;
end;

function TXxmHSysContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=TxxmHSysResponseHeaders.Create(
    GetResponseHeader,AddResponseHeader,
    GetResponseHeaderCount,GetResponseHeaderName,
    GetResponseHeaderIndex,SetResponseHeaderIndex);
end;

function TXxmHSysContext.GetResponseHeader(const Name: WideString): WideString;
var
  i:integer;
  x:THTTP_HEADER_ID;
begin
  inherited;
  //TODO: encode when non-UTF7 characters?
  x:=HttpHeaderStart;
  while (x<=HttpHeaderResponseMaximum)
    and (CompareText(HttpResponseHeaderName[x],Name)<>0) do inc(x);
  if x>HttpHeaderResponseMaximum then
   begin
    i:=0;
    while (i<Length(FUnknownHeaders))
      and (CompareText(string(FUnknownHeaders[i].pName),Name)<>0) do inc(i);
    if i=Length(FUnknownHeaders) then Result:=''
      else Result:=UTF8ToWideString(FUnknownHeaders[i].pRawValue);
   end
  else
    Result:=UTF8ToWideString(FRes.Headers.KnownHeaders[x].pRawValue);
end;

function TXxmHSysContext.GetRequestHeader(const Name: WideString): WideString;
var
  i:THTTP_HEADER_ID;
begin
  //TODO: more? (see also TxxmHSysResponseHeaders, here internal use only)
  if Name='If-Modified-Since' then i:=HttpHeaderIfModifiedSince else
  if Name='Authorization' then i:=HttpHeaderAuthorization else
  if Name='Upgrade' then i:=HttpHeaderUpgrade else
    i:=THTTP_HEADER_ID(-1);
  if i=THTTP_HEADER_ID(-1) then Result:='' else
    Result:=WideString(FReq.Headers.KnownHeaders[i].pRawValue);
end;

procedure TXxmHSysContext.AddResponseHeader(const Name, Value: WideString);
var
  i:integer;
  x:THTTP_HEADER_ID;
begin
  inherited;
  HeaderCheckName(Name);
  HeaderCheckValue(Value);
  //TODO: encode when non-UTF7 characters?
  x:=HttpHeaderStart;
  while (x<=HttpHeaderResponseMaximum)
    and (CompareText(HttpResponseHeaderName[x],Name)<>0) do inc(x);
  if x>HttpHeaderResponseMaximum then
   begin
    i:=0;
    while (i<Length(FUnknownHeaders))
      and (CompareText(string(FUnknownHeaders[i].pName),Name)<>0) do inc(i);
    if i=Length(FUnknownHeaders) then
     begin
      SetLength(FUnknownHeaders,i+1);
      CacheString(AnsiString(Name),FUnknownHeaders[i].NameLength,
        FUnknownHeaders[i].pName);
     end;
    CacheString(UTF8Encode(Value),FUnknownHeaders[i].RawValueLength,
      FUnknownHeaders[i].pRawValue);
   end
  else
    CacheString(UTF8Encode(Value),FRes.Headers.KnownHeaders[x].RawValueLength,
      FRes.Headers.KnownHeaders[x].pRawValue);
end;

procedure TXxmHSysContext.SetResponseHeader(id: THTTP_HEADER_ID;
  const Value: AnsiString);
begin
  //TODO: SettingCookie allow multiples
  CacheString(Value,
    FRes.Headers.KnownHeaders[id].RawValueLength,
    FRes.Headers.KnownHeaders[id].pRawValue);
end;

procedure TXxmHSysContext.CacheString(const x: AnsiString; var xLen: USHORT;
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

function TXxmHSysContext.GetResponseHeaderCount: integer;
begin
  Result:=integer(HttpHeaderResponseMaximum)+Length(FUnknownHeaders);
  //TODO: skip empty ones?
end;

function TXxmHSysContext.GetResponseHeaderName(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    Result:=HttpResponseHeaderName[THTTP_HEADER_ID(Idx)]
  else
    if (Idx>=0) and (Idx<Length(FUnknownHeaders)) then
      Result:=UTF8ToWideString(FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pName)
    else
      raise ERangeError.Create('GetResponseHeaderName: Out of range');
end;

function TXxmHSysContext.GetResponseHeaderIndex(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    Result:=UTF8ToWideString(FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].pRawValue)
  else
    if (Idx>=0) and (Idx<Length(FUnknownHeaders)) then
      Result:=UTF8ToWideString(FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pRawValue)
    else
      raise ERangeError.Create('GetResponseHeaderIndex: Out of range');
end;

procedure TXxmHSysContext.SetResponseHeaderIndex(Idx: integer;
  const Value: WideString);
begin
  if (Idx>=0) and (Idx<=integer(HttpHeaderResponseMaximum)) then
    CacheString(UTF8Encode(Value),
      FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].RawValueLength,
      FRes.Headers.KnownHeaders[THTTP_HEADER_ID(Idx)].pRawValue)
  else
    if (Idx>=0) and (Idx<=Length(FUnknownHeaders)) then
      CacheString(UTF8Encode(Value),
        FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].RawValueLength,
        FUnknownHeaders[Idx-integer(HttpHeaderResponseMaximum)-1].pRawValue)
    else
      raise ERangeError.Create('SetResponseHeaderIndex: Out of range');
end;

{$IFDEF HSYS2}
function TXxmHSysContext.GetRawSocket: IStream;
var
  i:integer;
begin
  if FReqHeaders['Upgrade']='' then Result:=nil else
   begin
    FContentType:='';

    //fix Connection header!
    if FRes.Headers.KnownHeaders[HttpHeaderConnection].RawValueLength<>0 then
     begin
      i:=Length(FUnknownHeaders);
      SetLength(FUnknownHeaders,i+1);
      CacheString('Connection',FUnknownHeaders[i].NameLength,
        FUnknownHeaders[i].pName);
      FUnknownHeaders[i].RawValueLength:=FRes.Headers.KnownHeaders[HttpHeaderConnection].RawValueLength;
      FUnknownHeaders[i].pRawValue:=FRes.Headers.KnownHeaders[HttpHeaderConnection].pRawValue;
      FRes.Headers.KnownHeaders[HttpHeaderConnection].RawValueLength:=0;
      FRes.Headers.KnownHeaders[HttpHeaderConnection].pRawValue:=nil;
     end;

    SetBufferSize(0);//!
    CheckSendStart(false);
    Result:=TRawSocketData.Create(FHSysQueue,FReq.RequestId);
   end;
end;
{$ENDIF}

{ TXxmPostDataStream }

constructor TXxmPostDataStream.Create(HSysQueue:THandle;
  RequestID:THTTP_REQUEST_ID;InputSize:cardinal);
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

{$IF not(Declared(NativeUInt))}
type
  NativeUInt=cardinal;
{$IFEND}

function TXxmPostDataStream.Read(var Buffer; Count: Integer): Integer;
var
  l:cardinal;
  p:pointer;
  p1:NativeUInt absolute p;
begin
  l:=Position+Count;
  if l>FInputSize then l:=FInputSize;
  if l>FInputRead then
   begin
    dec(l,FInputRead);
    if l<>0 then
     begin
      p:=Memory;
      inc(p1,FInputRead);
      HttpCheck(HttpReceiveRequestEntityBody(FHSysQueue,FRequestId,0,p,l,l,nil));
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

{$IFDEF HSYS2}

{ TRawSocketData }

constructor TRawSocketData.Create(HSysQueue: THandle;
  RequestID: THTTP_REQUEST_ID);
begin
  inherited Create;
  FHSysQueue:=HSysQueue;
  FRequestID:=RequestID;
  FBuffer1:=0;
  FBuffer2:=0;
  FCallOut:=false;
  FCall.Internal:=0;
  FCall.InternalHigh:=0;
  FCall.Offset:=0;
  FCall.OffsetHigh:=0;
  FCall.hEvent:=CreateEvent(nil,true,false,nil);
end;

destructor TRawSocketData.Destroy;
begin
  FHSysQueue:=0;
  FRequestID:=0;
  //if FCallOut then abort?
  CloseHandle(FCall.hEvent);
  inherited;
end;

function TRawSocketData.Clone(out stm: IStream): HResult;
begin
  raise Exception.Create('TRawSocketData.Clone not supported');
end;

function TRawSocketData.Commit(grfCommitFlags: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Commit not supported');
end;

function TRawSocketData.CopyTo(stm: IStream; cb: LargeUInt; out cbRead,
  cbWritten: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.CopyTo not supported');
end;

function TRawSocketData.LockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.LockRegion not supported');
end;

function TRawSocketData.Revert: HResult;
begin
  raise Exception.Create('TRawSocketData.Revert not supported');
end;

function TRawSocketData.Seek(dlibMove: Largeint; dwOrigin: XDWORD;
  out libNewPosition: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.Seek not supported');
end;

function TRawSocketData.SetSize(libNewSize: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.SetSize not supported');
end;

function TRawSocketData.Stat(out statstg: TStatStg;
  grfStatFlag: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Stat not supported');
end;

function TRawSocketData.UnlockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.UnlockRegion not supported');
end;

function TRawSocketData.DataReady(TimeoutMS: cardinal): boolean;
var
  r,l:cardinal;
begin
  if not FCallOut then
   begin
    FBuffer1:=0;
    ResetEvent(FCall.hEvent);
    r:=HttpReceiveRequestEntityBody(FHSysQueue,FRequestId,0,
      @FBuffer[0],RawSocketBufferSize,l,@FCall);
    if r=ERROR_IO_PENDING then
      FCallOut:=true
    else
      HttpCheck(r);
   end;
  r:=WaitForSingleObject(FCall.hEvent,TimeoutMS);
  case r of
    WAIT_OBJECT_0:
     begin
      FCallOut:=false;
      if not GetOverlappedResult(FHSysQueue,FCall,FBuffer2,false) then
        RaiseLastOSError;
      Result:=true;//Result:=FBuffer2<>0;
     end;
    WAIT_TIMEOUT:
      Result:=false;
    //WAIT_FAILED
    else
     begin
      RaiseLastOSError;
      Result:=false;//counter warning
     end;
  end;
end;

function TRawSocketData.Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
var
  l:cardinal;
begin
  if FCallOut then
   begin
    if not GetOverlappedResult(FHSysQueue,FCall,FBuffer2,true) then
      RaiseLastOSError;
    FCallOut:=false;
   end;
  if FBuffer2=0 then
    HttpCheck(HttpReceiveRequestEntityBody(FHSysQueue,FRequestId,0,pv,cb,l,nil))
  else
   begin
    l:=FBuffer2-FBuffer1;
    if cardinal(cb)<=FBuffer2-FBuffer1 then l:=cardinal(cb);
    Move(FBuffer[FBuffer1],pv^,l);
    inc(l,FBuffer1);
    if FBuffer1=FBuffer2 then
     begin
      FBuffer1:=0;
      FBuffer2:=0;
     end;
   end;
  if pcbRead<>nil then pcbRead^:=l;
  Result:=S_OK;
end;

function TRawSocketData.Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
var
  c:THTTP_DATA_CHUNK;
  l:cardinal;
begin
  ZeroMemory(@c,SizeOf(THTTP_DATA_CHUNK));
  c.DataChunkType:=HttpDataChunkFromMemory;
  c.pBuffer:=pv;
  c.BufferLength:=cb;
  l:=cb;
  HttpCheck(HttpSendResponseEntityBody(FHSysQueue,FRequestID,
    HTTP_SEND_RESPONSE_FLAG_MORE_DATA,1,@c,l,nil,0,nil,nil));
  if pcbWritten<>nil then pcbWritten^:=l;
  Result:=S_OK;
end;

procedure TRawSocketData.Disconnect;
begin
  HttpSendResponseEntityBody(FHSysQueue,FRequestId,
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT,//if keep-alive?
    0,nil,cardinal(nil^),nil,0,nil,nil);
end;

{$ENDIF}

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
  SessionCookie:='xxm'+Copy(CreateClassID,2,8);
end.
