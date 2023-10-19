unit xxmIsapiMain;

interface

uses Windows, SysUtils, Classes, ActiveX, isapi4, xxm, xxmContext,
  xxmPReg, xxmPRegJson, xxmParams, xxmParUtils, xxmHeaders, xxmThreadPool;

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;

type
  TXxmIsapiContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmContextSuspend)
  private
    ecb: PEXTENSION_CONTROL_BLOCK;
    FIOState: integer;
    FIOStream: TStream;
    FURI, FRedirectPrefix, FSessionID: WideString;
    FReqHeaders: TRequestHeaders;
    FResHeaders: TResponseHeaders;
    FProjectCache: TXxmProjectCacheLocal;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FResumeFragment,FDropFragment:WideString;
    FResumeValue,FDropValue:OleVariant;
    procedure ServerFunction(HSERRequest: DWORD; Buffer: Pointer;
      Size, DataType: LPDWORD);
  protected
    function GetSessionID: WideString; override;
    procedure DispositionAttach(const FileName: WideString); override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;
    procedure Redirect(const RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(const Name: WideString): WideString; override;

    function GetProjectEntry: TXxmProjectEntry; override;
    function GetProjectPage(const FragmentName: WideString):IXxmFragment; override;
    procedure SendHeader; override;
    procedure SetStatus(Code: Integer; const Text: WideString); override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

    { IxxmHttpHeaders }
    function GetRequestHeaders: IxxmDictionaryEx;
    function GetResponseHeaders: IxxmDictionaryEx;
    { IXxmContextSuspend }
    procedure Suspend(const EventKey: WideString;
      CheckIntervalMS, MaxWaitTimeSec: cardinal;
      const ResumeFragment: WideString; ResumeValue: OleVariant;
      const DropFragment: WideString; DropValue: OleVariant);

    procedure FlushFinal; override;
    procedure FlushStream(AData: TStream; ADataSize: Int64); override;
    procedure Spool; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Load(pecb: PEXTENSION_CONTROL_BLOCK);
    procedure Recycle; override;
  end;

  EXxmContextStringUnknown=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

const
  PoolMaxThreads=$200;//TODO: from setting?
  SpoolingThreshold=$10000;//64KiB

implementation

uses Variants, ComObj, xxmCommonUtils, xxmIsapiStream;

resourcestring
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmContextAlreadySuspended='Context has already been suspended';

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
const
  dSize=$1000;
var
  d:array[0..dSize-1] of byte;
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  r:TResourceStream;
  p:PAnsiChar;
begin
  try
    r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      r.Read(d[0],dSize);
    finally
      r.Free;
    end;
    if VerQueryValueA(@d[0],'\',pointer(verblock),verlen) then
      Ver.dwExtensionVersion:=verblock.dwFileVersionMS;
    if VerQueryValueA(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
      Move(p^,Ver.lpszExtensionDesc[0],verlen);
    if ContextPool=nil then
      ContextPool:=TXxmContextPool.Create(TXxmIsapiContext);
    if PageLoaderPool=nil then
      PageLoaderPool:=TXxmPageLoaderPool.Create(PoolMaxThreads);
    if XxmProjectCache=nil then
      XxmProjectCache:=TXxmProjectCacheJson.Create;
    Result:=true;
  except
    on e:Exception do
     begin
      XxmProjectCacheError:='['+e.ClassName+']'+e.Message;
      Result:=false;
     end;
  end;
end;

function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
begin
  try
    (ContextPool.GetContext as TXxmIsapiContext).Load(PECB);
    Result:=HSE_STATUS_PENDING; //HSE_STATUS_SUCCESS
  except
    Result:=HSE_STATUS_ERROR;
  end;
end;

function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;
begin
  try
    FreeAndNil(PageLoaderPool);
  except
    //silent (log?)
  end;
  try
    FreeAndNil(XxmProjectCache);
  except
    //silent (log?)
  end;
  Result:=true;
end;

const
  IOState_None    =0;//lowest bit 0 for 'sync' states
  IOState_Error   =2;
  IOState_Pending =1;//lowest bit 1 for 'async' states
  IOState_Final   =3;
  IOState_Stream  =5;

procedure ContextIOCompletion(var ECB: TEXTENSION_CONTROL_BLOCK;
  pContext: TXxmIsapiContext; cbIO, dwError: DWORD) stdcall;
begin
  //assert pContext.ecb=ECB
  try
    //TODO: if dwError<>0 then EndRequest? Recycle?
    case pContext.FIOState of
      IOState_Pending:
       begin
        BufferStore.AddBuffer(TMemoryStream(pContext.FIOStream));
        if dwError=0 then
          pContext.FIOState:=IOState_None
        else
         begin
          pContext.FIOStream:=TStream(pointer(dwError));
          pContext.FIOState:=IOState_Error;
         end;
       end;
      IOState_Final:
       begin
        pContext.FIOState:=IOState_None;
        BufferStore.AddBuffer(TMemoryStream(pContext.FIOStream));
        pContext.Recycle;
       end;
      IOState_Stream:
       begin
        if dwError=0 then
          PageLoaderPool.Queue(pContext,ctSpooling)
        else
         begin
          pContext.FIOState:=IOState_None;
          FreeAndNil(pContext.FIOStream);
          pContext.Recycle;
         end;
       end;
    end;
  except
    //silent (log?)
  end;
end;

function GetVar(pecb: PEXTENSION_CONTROL_BLOCK; const key:AnsiString):AnsiString;
var
  l:cardinal;
begin
  l:=$10000;
  SetLength(Result,l);
  //TODO: 'UNICODE_'+
  if not(pecb.GetServerVariable(pecb.ConnID,PAnsiChar(key),PAnsiChar(Result),l)) then
    if GetLastError=ERROR_INVALID_INDEX then l:=1 else RaiseLastOSError;
  SetLength(Result,l-1);
end;

{ TXxmIsapiContext }

procedure TXxmIsapiContext.AfterConstruction;
begin
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  FProjectCache:=TXxmProjectCacheLocal.Create;
  inherited;
end;

destructor TXxmIsapiContext.Destroy;
begin
  FReqHeaders.Free;
  FResHeaders.Free;
  FProjectCache.Free;
  inherited;
end;

const
  httpScheme:array[boolean] of WideString=('http://','https://');

procedure TXxmIsapiContext.Load(pecb: PEXTENSION_CONTROL_BLOCK);
begin
  ecb:=pecb;
  FURI:=WideString(GetVar(ecb,AnsiString('HTTP_URL')));
  FURL:=
    httpScheme[UpperCase(string(GetVar(ecb,'HTTPS')))='ON']+
    WideString(GetVar(ecb,'HTTP_HOST'))+FURI;//TODO: unicode?
  SendDirect:=SendData;
  BeginRequest;
  PageLoaderPool.Queue(Self,ctHeaderNotSent);
end;

procedure TXxmIsapiContext.BeginRequest;
begin
  inherited;
  FCookieParsed:=false;
  FSessionID:='';//see GetSessionID
  FIOState:=IOState_None;
  FIOStream:=nil;
  FResHeaders.Reset;
  FReqHeaders.Reset;
end;

procedure TXxmIsapiContext.EndRequest;
var
  i:DWORD;
begin
  ecb.dwHttpStatusCode:=StatusCode;
  //ServerFunction(HSE_REQ_CLOSE_CONNECTION,nil,nil,nil);
  i:=HSE_STATUS_SUCCESS_AND_KEEP_CONN;
  ecb.ServerSupportFunction(ecb.ConnID,HSE_REQ_DONE_WITH_SESSION,@i,nil,nil);
  inherited;
  BufferStore.AddBuffer(FContentBuffer);
end;

procedure TXxmIsapiContext.HandleRequest;
var
  x,y:AnsiString;
  i:integer;
begin
  try
    ServerFunction(HSE_REQ_IO_COMPLETION,@ContextIOCompletion,nil,PDWORD(Self));
    //parse url
    x:=AnsiString(FURI);//GetVar(ecb,'HTTP_URL');
    y:=GetVar(ecb,'SCRIPT_NAME');
    if y=ecb.lpszPathInfo then
     begin
      //called mapped
      FRedirectPrefix:='';
     end
    else
     begin
      //called directly
      FRedirectPrefix:=WideString(y);
      x:=Copy(x,Length(y)+1,Length(x)-Length(y));
     end;

    //FResHeaders['X-Powered-By']:=SelfVersion;

    //project name
    i:=1;
    if i>Length(x) then Redirect('/',true) else
      if x[i]<>'/' then Redirect('/'+WideString(Copy(x,i,Length(x)-i+1)),true);
    //redirect raises EXxmPageRedirected
    inc(i);
    if XxmProjectCache=nil then
      raise Exception.Create('Failed loading project registry:'#13#10+
        XxmProjectCacheError);
    if XxmProjectCache.ProjectFromURI(Self,x,i,FProjectName,FFragmentName) then
      FRedirectPrefix:=FRedirectPrefix+'/'+FProjectName;
    FPageClass:='['+FProjectName+']';

    BuildPage;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ; //assert AutoBuild handler did output
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'ERROR');
        try
          if not(e is EXxmTransferError) and Connected then
           begin
            //TODO: consider HSE_REQ_SEND_CUSTOM_ERROR?
            try
              if FPostData=nil then y:='none' else y:=AnsiString(IntToStr(FPostData.Size)+' bytes');
            except
              y:='unknown';
            end;
            SendError('error',e.ClassName,e.Message);
           end;
        except
          //silent
        end;
        //TODO:ServerFunction(HSE_REQ_ABORTIVE_CLOSE,nil,nil,nil);?
       end;
  end;
end;

procedure TXxmIsapiContext.Recycle;
begin
  if (FIOState and 1)=0 then inherited;
end;

function TXxmIsapiContext.GetProjectEntry:TXxmProjectEntry;
begin
  Result:=FProjectCache.GetProject(FProjectName);
end;

function TXxmIsapiContext.GetProjectPage(const FragmentName: WideString):IXxmFragment;
var
  p:IXxmPage;
begin
  Result:=inherited GetProjectPage(FragmentName);
  if Result<>nil then
   begin
    if Result.QueryInterface(IID_IXxmPage,p)=S_OK then
     begin
      if ecb.cbTotalBytes<>0 then
       begin
        if ecb.cbAvailable=ecb.cbTotalBytes then
         begin
          FPostData:=THeapStream.Create;
          FPostData.Write(ecb.lpbData^,ecb.cbAvailable);
         end
        else
         begin
          SetLength(FPostTempFile,$400);
          SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));//TODO: setting
          FPostTempFile:=FPostTempFile+'xxm_'+
            IntToHex(GetCurrentThreadId,4)+'_'+IntToHex(ecb.ConnID,8)+'.dat';
          FPostData:=TXxmIsapiStreamAdapter.Create(ecb,TFileStream.Create(FPostTempFile,fmCreate));
         end;
        FPostData.Seek(0,soFromBeginning);
       end;
     end;
    //else raise EXxmDirectInclude.Create(SXxmDirectInclude);//see BuildPage
    p:=nil;
   end;
end;

function TXxmIsapiContext.ContextString(cs: TXxmContextString): WideString;
begin
  //TODO
  case cs of
    csVersion:Result:=SelfVersion+', '+WideString(GetVar(ecb,'SERVER_SOFTWARE'));
      //'IIS '+IntToStr(HiWord(ecb.dwVersion))+'.'+IntToStr(LoWord(ecb.dwVersion));
    csExtraInfo:         Result:='';//TODO
    csVerb:              Result:=WideString(ecb.lpszMethod);
    csQueryString:       Result:=WideString(ecb.lpszQueryString);
    csUserAgent:         Result:=WideString(GetVar(ecb,'HTTP_USER_AGENT'));
    csAcceptedMimeTypes: Result:=WideString(GetVar(ecb,'HTTP_ACCEPT'));
    csPostMimeType:      Result:=WideString(ecb.lpszContentType);
    csURL:               Result:=GetURL;//'HTTP_URL'?
    csProjectName:       Result:=FProjectName;
    csLocalURL:          Result:=FFragmentName;
    csReferer:           Result:=WideString(GetVar(ecb,'HTTP_REFERER'));
    csLanguage:          Result:=WideString(GetVar(ecb,'HTTP_ACCEPT_LANGUAGE'));
    csRemoteAddress:     Result:=WideString(GetVar(ecb,'REMOTE_ADDR'));
    csRemoteHost:        Result:=WideString(GetVar(ecb,'REMOTE_HOST'));
    csAuthUser://TODO: setting?
     begin
      Result:=WideString(GetVar(ecb,'AUTH_USER'));
      if Result='' then Result:=WideString(AuthValue(cs));
     end;
    csAuthPassword:
      if GetVar(ecb,'AUTH_USER')='' then
        Result:=WideString(AuthValue(cs))
      else
        Result:=WideString(GetVar(ecb,'AUTH_PASSWORD'));
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  //ecb.lpszPathInfo;
  //ecb.lpszPathTranslated;
end;

procedure TXxmIsapiContext.DispositionAttach(const FileName: WideString);
var
  s:WideString;
  i:integer;
begin
  s:=FileName;
  for i:=1 to Length(s) do
    if AnsiChar(s[i]) in ['\','/',':','*','?','"','<','>','|'] then
      s[i]:='_';
  AddResponseHeader('Content-disposition','attachment; filename="'+s+'"');
  FResHeaders.SetComplex('Content-disposition','attachment')['filename']:=s;
end;

function TXxmIsapiContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Count=0 then Result:=0 else
   begin
    while (FIOState and 1)<>0 do Sleep(1);
    if FIOState=IOState_Error then
      raise EXxmTransferError.Create(SysErrorMessage(DWORD(FIOStream)));
    Result:=Count;
    if (BufferSize<>0) and (Count>SpoolingThreshold)
      and (@Buffer=FContentBuffer.Memory) then
     begin
      //async
      FIOState:=IOState_Pending;
      FIOStream:=FContentBuffer;
      if not(ecb.WriteClient(ecb.ConnID,@Buffer,cardinal(Result),HSE_IO_ASYNC)) then
        raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
      //continue with another content buffer
      FContentBuffer:=nil;
      SetBufferSize(BufferSize);
     end
    else
     begin
      //sync
      if not(ecb.WriteClient(ecb.ConnID,@Buffer,cardinal(Result),HSE_IO_SYNC)) then
        raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
     end;
   end;
end;

procedure TXxmIsapiContext.ServerFunction(HSERRequest: DWORD;
  Buffer: Pointer; Size, DataType: LPDWORD);
begin
  if not(ecb.ServerSupportFunction(ecb.ConnID,HSERRequest,Buffer,Size,DataType)) then
    RaiseLastOSError;
end;

procedure TXxmIsapiContext.SendHeader;
var
  head:THSE_SEND_HEADER_EX_INFO;
  s,t:AnsiString;
begin
  //TODO: only IIS7 or higher? see http://support.microsoft.com/kb/946086
  ecb.ServerSupportFunction(ecb.ConnID,HSE_REQ_SET_FLUSH_FLAG,pointer(true),nil,nil);

  //send header
  s:=AnsiString(IntToStr(StatusCode)+' '+StatusText);
  head.pszStatus:=PAnsiChar(s);
  head.cchStatus:=Length(s);
  //use FResHeader.Complex?
  if FContentType<>'' then
   begin
    case FAutoEncoding of
      aeUtf8:   t:='; charset="utf-8"';
      aeUtf16:  t:='; charset="utf-16"';
      aeIso8859:t:='; charset="iso-8859-15"';
      else t:='';
    end;
    FResHeaders['Content-Type']:=FContentType+WideString(t);
   end;
  t:=FResHeaders.Build+#13#10;
  //TODO cookies? redirect?
  head.pszHeader:=PAnsiChar(t);
  head.cchHeader:=Length(t);
  head.fKeepConn:=FResHeaders['Content-Length']<>'';//TODO: chunked encoding
  ServerFunction(HSE_REQ_SEND_RESPONSE_HEADER_EX,@head,nil,nil);
  inherited;
end;

function TXxmIsapiContext.Connected: Boolean;
var
  b:BOOL;
begin
  ServerFunction(HSE_REQ_IS_CONNECTED,@b,nil,nil);
  Result:=b;
end;

procedure TXxmIsapiContext.SetStatus(Code: Integer; const Text: WideString);
begin
  inherited;
  //TODO
  ecb.dwHttpStatusCode:=Code;
  //ecb.
end;

procedure TXxmIsapiContext.Redirect(const RedirectURL: WideString;
  Relative: Boolean);
var
  s:WideString;
  i:integer;
begin
  inherited;
  CheckHeaderNotSent;
  if Relative then
   begin
    //TODO: proper combine?
    if (RedirectURL<>'') and (RedirectURL[1]='/') then
      s:=httpScheme[UpperCase(string(GetVar(ecb,'HTTPS')))='ON']+
        WideString(GetVar(ecb,'HTTP_HOST'))+
        FRedirectPrefix+RedirectURL
    else
     begin
      s:=FURI;
      i:=Length(s);
      while (i<>0) and (s[i]<>'/') do dec(i);
      s:=httpScheme[UpperCase(string(GetVar(ecb,'HTTPS')))='ON']+
        WideString(GetVar(ecb,'HTTP_HOST'))+
        Copy(s,1,i)+RedirectURL;
     end;
   end
  else
    s:=RedirectURL;
  if FResHeaders['Cache-Control']='' then
    AddResponseHeader('Cache-Control','no-cache');
  ServerFunction(HSE_REQ_SEND_URL_REDIRECT_RESP,PAnsiChar(UTF8Encode(s)),nil,nil);
  raise EXxmPageRedirected.Create(s);
end;

function TXxmIsapiContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=GetVar(ecb,'HTTP_COOKIE');
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=WideString(GetParamValue(FCookie,FCookieIdx,AnsiString(Name)));
end;

function TXxmIsapiContext.GetSessionID: WideString;
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

function TXxmIsapiContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  FReqHeaders.Load(GetVar(ecb,'ALL_RAW'));
  Result:=FReqHeaders;
end;

function TXxmIsapiContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

function TXxmIsapiContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=GetRequestHeaders.Item[Name];
end;

procedure TXxmIsapiContext.AddResponseHeader(const Name, Value: WideString);
begin
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

procedure TXxmIsapiContext.FlushFinal;
var
  l:cardinal;
begin
  //no inherited! override default behaviour:
  if (BufferSize<>0) and (FContentBuffer.Position>SpoolingThreshold) then
   begin
    while (FIOState and 1)<>0 do Sleep(1);
    if FIOState=IOState_Error then
      raise EXxmTransferError.Create(SysErrorMessage(DWORD(FIOStream)));
    CheckSendStart(true);
    //async
    FIOState:=IOState_Final;
    FIOStream:=FContentBuffer;
    l:=FContentBuffer.Position;
    if not(ecb.WriteClient(ecb.ConnID,FContentBuffer.Memory,cardinal(l),HSE_IO_ASYNC)) then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
    FContentBuffer:=nil;
   end
  else
    inherited;//Flush;
end;

procedure TXxmIsapiContext.FlushStream(AData: TStream; ADataSize: Int64);
begin
  //no inherited! override default behaviour:
  if (BufferSize<>0) and (ADataSize>SpoolingThreshold) then
   begin
    //assert AData.Size=ADataSize
    while (FIOState and 1)<>0 do Sleep(1);
    if FIOState=IOState_Error then
      raise EXxmTransferError.Create(SysErrorMessage(DWORD(FIOStream)));
    CheckSendStart(true);
    //async
    _AddRef;//_Release of either TXxmIsapiHandler or ContextIOCompletion will be destroying
    FIOState:=IOState_Stream;
    FIOStream:=AData;
    FIOStream.Position:=0;
    Spool;
   end
  else
    inherited;//assert WasKept=false
end;

procedure TXxmIsapiContext.Spool;
var
  l:integer;
begin
  //inherited;
  l:=FIOStream.Read(FContentBuffer.Memory^,BufferSize);
  if l=0 then
   begin
    FIOState:=IOState_None;
    FreeAndNil(FIOStream);
    Recycle;
   end
  else
   begin
    //FIOState:=IOState_Stream;//assert already set
    if not(ecb.WriteClient(ecb.ConnID,FContentBuffer.Memory,cardinal(l),HSE_IO_ASYNC)) then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end;
end;

procedure TXxmIsapiContext.Suspend(const EventKey: WideString;
  CheckIntervalMS, MaxWaitTimeSec: cardinal;
  const ResumeFragment: WideString; ResumeValue: OleVariant;
  const DropFragment: WideString; DropValue: OleVariant);
begin
  if State=ctSuspended then
    raise EXxmContextAlreadySuspended.Create(SXxmContextAlreadySuspended);
  while (FIOState and 1)<>0 do Sleep(1);
  if FIOState=IOState_Error then
    raise EXxmTransferError.Create(SysErrorMessage(DWORD(FIOStream)));
  FResumeFragment:=ResumeFragment;
  FResumeValue:=ResumeValue;
  FDropFragment:=DropFragment;
  FDropValue:=DropValue;
  PageLoaderPool.EventsController.SuspendContext(Self,EventKey,
    CheckIntervalMS,MaxWaitTimeSec);
end;

initialization
  StatusException:=500;//TODO: from settings?
  StatusBuildError:=503;
  StatusFileNotFound:=404;
  XxmProjectCache:=nil;//TXxmProjectCache.Create;//see Execute above
  XxmProjectCacheError:='';
finalization
  //assert PageLoaderPool=nil by TerminateExtension
end.

