unit xxmIsapiMain;

interface

uses Windows, SysUtils, Classes, ActiveX, isapi4, xxm, xxmContext,
  xxmPReg, xxmIsapiPReg, xxmParams, xxmParUtils, xxmHeaders;

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;

type
  TXxmIsapiContext=class(TXxmGeneralContext, IxxmHttpHeaders)
  private
    FURI:WideString;
    FRedirectPrefix,FSessionID:AnsiString;
    ecb:PEXTENSION_CONTROL_BLOCK;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    procedure ServerFunction(HSERRequest: DWORD; Buffer: Pointer; Size, DataType: LPDWORD);
  protected

    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    procedure SendRaw(Data: WideString); override;
    procedure SendStream(s: IStream); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;
    procedure SetCookie(Name: WideString; Value: WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;

    function GetProjectEntry(ProjectName: WideString):TXxmProjectEntry; override;
    function GetProjectPage(FragmentName: WideString):IXxmFragment; override;
    procedure SendHeader; override;
    procedure SetStatus(Code: Integer; Text: WideString); override;
    procedure AddResponseHeader(Name, Value: WideString); override;

    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    Queue:TXxmIsapiContext;//used by thread pool

    constructor Create(pecb:PEXTENSION_CONTROL_BLOCK);
    destructor Destroy; override;
    procedure Execute;
  end;

  TXxmIsapiHandler=class(TThread)
  private
    FInUse:boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property InUse:boolean read FInUse;
  end;

  TXxmIsapiHandlerPool=class(TObject)
  private
    FHandlers:array of TXxmIsapiHandler;
    FHandlerSize:integer;
    FLock:TRTLCriticalSection;
    FQueue:TXxmIsapiContext;
    procedure SetSize(x:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmIsapiContext);//called from handler
    function Unqueue:TXxmIsapiContext;//called from threads
  end;

  TXxmAutoBuildHandler=function(pce:TXxmProjectCacheEntry;
    Context: IXxmContext; ProjectName:WideString):boolean;

  EXxmContextStringUnknown=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmPageRedirected=class(Exception);

const
  PoolMaxThreads=64;//TODO: from setting?

var
  IsapiHandlerPool:TXxmIsapiHandlerPool;

implementation

uses Variants, ComObj, xxmCommonUtils, xxmIsapiStream;

resourcestring
  SXxmContextStringUnknown='Unknown ContextString __';

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
var
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  r:TResourceStream;
  m:TMemoryStream;
  p:PAnsiChar;
begin
  m:=TMemoryStream.Create;
  try
    r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      r.SaveToStream(m);
    finally
      r.Free;
    end;
    m.Position:=0;
    if VerQueryValueA(m.Memory,'\',pointer(verblock),verlen) then
      Ver.dwExtensionVersion:=verblock.dwFileVersionMS;
    if VerQueryValueA(m.Memory,'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
      Move(p^,Ver.lpszExtensionDesc[0],verlen);
  finally
    m.Free;
  end;
  Result:=true;
  //IsapiHandlerPool:=TXxmIsapiHandlerPool.Create;?
end;

function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
begin
  try
    IsapiHandlerPool.Queue(TXxmIsapiContext.Create(PECB));
    Result:=HSE_STATUS_PENDING; //HSE_STATUS_SUCCESS
  except
    on e:Exception do
     begin
      //TODO output error?
      Result:=HSE_STATUS_ERROR;
     end;
  end;
end;

function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;
begin
  FreeAndNil(IsapiHandlerPool);
  Result:=true;
end;

{
procedure ContextIOCompletion(var ECB: TEXTENSION_CONTROL_BLOCK;
  pContext: Pointer; cbIO, dwError: DWORD) stdcall;
begin
  //assert TXxmIsapiContext(pContext).ecb=ECB
  TXxmIsapiContext(pContext).ReportComplete(cbIO,dwError);
end;
}

{ TXxmIsapiContext }

function GetVar(pecb: PEXTENSION_CONTROL_BLOCK; key:AnsiString):AnsiString;
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

constructor TXxmIsapiContext.Create(pecb: PEXTENSION_CONTROL_BLOCK);
var
  uri:WideString;
begin
  uri:=GetVar(pecb,'HTTP_URL');
  inherited Create('http://'+GetVar(pecb,'HTTP_HOST')+uri);//TODO: unicode?
  ecb:=pecb;
  FURI:=uri;
  FCookieParsed:=false;
  FSessionID:='';//see GetSessionID
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
end;

destructor TXxmIsapiContext.Destroy;
begin
  if FReqHeaders<>nil then
   begin
    (FReqHeaders as IUnknown)._Release;
    FReqHeaders:=nil;
   end;
  if FResHeaders<>nil then
   begin
    (FResHeaders as IUnknown)._Release;
    FResHeaders:=nil;
   end;
  inherited;
end;

procedure TXxmIsapiContext.Execute;
var
  x,y:AnsiString;
  i,j:integer;
begin
  //ServerFunction(HSE_REQ_IO_COMPLETION,@ContextIOCompletion,nil,PDWORD(Self));
  try
    //parse url
    x:=FURI;//GetVar(ecb,'HTTP_URL');
    y:=GetVar(ecb,'SCRIPT_NAME');
    if y=ecb.lpszPathInfo then
     begin
      //called mapped
      FRedirectPrefix:='';
     end
    else
     begin
      //called directly
      FRedirectPrefix:=y;
      x:=Copy(x,Length(y)+1,Length(x)-Length(y));
     end;

    FResHeaders['X-Powered-By']:=SelfVersion;
    XxmProjectCache.Refresh;

    //project name
    i:=1;
    if i>Length(x) then Redirect('/',true) else
      if x[i]<>'/' then Redirect('/'+Copy(x,i,Length(x)-i+1),true);
    //redirect raises EXxmPageRedirected
    inc(i);
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=Length(x)) and not(char(x[i]) in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(x,2,i-2);
      if FProjectName='' then
       begin
        //FProjectName:=XxmProjectCache.DefaultProject;
        if (i<=Length(x)) and (x[i]='/') then y:='' else y:='/';
        Redirect('/'+XxmProjectCache.DefaultProject+y+Copy(x,i,Length(x)-i+1),true)
        //redirect raises EXxmPageRedirected
       end;
      FPageClass:='['+FProjectName+']';
      FRedirectPrefix:=FRedirectPrefix+'/'+FProjectName;
      if i>Length(x) then Redirect('/',true) else
        if x[i]<>'/' then Redirect('/'+Copy(x,i,Length(x)-i+1),true);
      //redirect raises EXxmPageRedirected
      inc(i);
     end
    else
     begin
      FProjectName:=XxmProjectCache.SingleProject;
      FPageClass:='[SingleProject]';
     end;

    //fragment name
    j:=i;
    while (j<=Length(x)) and not(char(x[j]) in ['?','&','$','#']) do inc(j);
    FFragmentName:=Copy(x,i,j-i);

    BuildPage;

  except
    on EXxmPageRedirected do ;//silent
    on EXxmAutoBuildFailed do ;
     //assert AutoBuild handler already displays message
    on e:Exception do
     begin
      ForceStatus(500,'ERROR');
      try
        if Connected then
         begin
          //TODO: consider HSE_REQ_SEND_CUSTOM_ERROR?
          //TODO: get fragment 500.xxm?
          try
            if FPostData=nil then y:='none' else y:=IntToStr(FPostData.Size)+' bytes';
          except
            y:='unknown';
          end;
          SendError('error',[
            'URL',HTMLEncode(FURL),
            'CLASS',FPageClass,
            'POSTDATA',y,
            'QUERYSTRING',ecb.lpszQueryString,
            'ERROR',e.Message,
            'ERRORCLASS',e.ClassName,
            'VERSION',ContextString(csVersion)
          ]);
         end;
      except
        //silent
      end;

      //TODO:ServerFunction(HSE_REQ_ABORTIVE_CLOSE,nil,nil,nil);?
     end;
  end;
  //TODO: support keep connection?
  ecb.dwHttpStatusCode:=StatusCode;
  //ServerFunction(HSE_REQ_CLOSE_CONNECTION,nil,nil,nil);
  ServerFunction(HSE_REQ_DONE_WITH_SESSION,nil,nil,nil);
end;

function TXxmIsapiContext.GetProjectEntry(ProjectName: WideString):TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmIsapiContext.GetProjectPage(FragmentName: WideString):IXxmFragment;
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
          FPostData:=TMemoryStream.Create;
          FPostData.Write(ecb.lpbData^,ecb.cbAvailable);
         end
        else
         begin
          SetLength(FPostTempFile,$400);
          SetLength(FPostTempFile,GetTempPathA($400,PAnsiChar(FPostTempFile)));//TODO: setting
          FPostTempFile:=FPostTempFile+'xxm_'+IntToHex(GetCurrentThreadId,4)+'_'+IntToHex(ecb.ConnID,8)+'.dat';
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
    csVersion:Result:=SelfVersion+', '+GetVar(ecb,'SERVER_SOFTWARE');
      //'IIS '+IntToStr(HiWord(ecb.dwVersion))+'.'+IntToStr(LoWord(ecb.dwVersion));
    csExtraInfo:         Result:='';//TODO
    csVerb:              Result:=ecb.lpszMethod;
    csQueryString:       Result:=ecb.lpszQueryString;
    csUserAgent:         Result:=GetVar(ecb,'HTTP_USER_AGENT');
    csAcceptedMimeTypes: Result:=GetVar(ecb,'HTTP_ACCEPT');
    csPostMimeType:      Result:=ecb.lpszContentType;
    csURL:               Result:=GetURL;//'HTTP_URL'?
    csProjectName:       Result:=FProjectName;
    csLocalURL:          Result:=FFragmentName;
    csReferer:           Result:=GetVar(ecb,'HTTP_REFERER');
    csLanguage:          Result:=GetVar(ecb,'HTTP_ACCEPT_LANGUAGE');
    csRemoteAddress:     Result:=GetVar(ecb,'REMOTE_ADDR');
    csRemoteHost:        Result:=GetVar(ecb,'REMOTE_HOST');
    csAuthUser:          Result:=GetVar(ecb,'AUTH_USER');
    csAuthPassword:      Result:=GetVar(ecb,'AUTH_PASSWORD');
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  //ecb.lpszPathInfo;
  //ecb.lpszPathTranslated;
end;

procedure TXxmIsapiContext.DispositionAttach(FileName: WideString);
begin
  FResHeaders.SetComplex('Content-disposition','attachment')
    ['filename']:=FileName;
end;

procedure TXxmIsapiContext.SendRaw(Data: WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:cardinal;
begin
  if Data<>'' then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
         begin
          l:=3;
          if not(ecb.WriteClient(ecb.ConnID,PAnsiChar(Utf8ByteOrderMark),l,HSE_IO_SYNC)) then
            RaiseLastOSError;
         end;
        aeUtf16:
         begin
          l:=2;
          if not(ecb.WriteClient(ecb.ConnID,PAnsiChar(Utf16ByteOrderMark),l,HSE_IO_SYNC)) then
            RaiseLastOSError;
         end;
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        if not(ecb.WriteClient(ecb.ConnID,PWideChar(Data),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if not(ecb.WriteClient(ecb.ConnID,PAnsiChar(s),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if not(ecb.WriteClient(ecb.ConnID,PAnsiChar(s),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
    end;
    //ReportData;
   end;
end;

procedure TXxmIsapiContext.SendStream(s: IStream);
const
  dSize=$10000;
var
  l:cardinal;
  d:array[0..dSize-1] of byte;
begin
  inherited;
  //TODO: keep-connection since content-length known?
  //if s.Size<>0 then
   begin
    CheckSendStart;
    //no autoencoding here
    repeat
      l:=dSize;
      OleCheck(s.Read(@d[0],l,@l));
      if l<>0 then
        if not(ecb.WriteClient(ecb.ConnID,@d[0],l,HSE_IO_SYNC)) then
          RaiseLastOSError;
      //ReportData;
    until l=0;
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
  s:=IntToStr(StatusCode)+' '+StatusText;
  head.pszStatus:=PAnsiChar(s);
  head.cchStatus:=Length(s);
  //use FResHeader.Complex?
  case FAutoEncoding of
    aeUtf8:   FResHeaders['Content-Type']:=FContentType+'; charset="utf-8"';
    aeUtf16:  FResHeaders['Content-Type']:=FContentType+'; charset="utf-16"';
    aeIso8859:FResHeaders['Content-Type']:=FContentType+'; charset="iso-8859-15"';
    else      FResHeaders['Content-Type']:=FContentType;
  end;
  t:=FResHeaders.Build+#13#10;
  //TODO cookies? redirect?
  head.pszHeader:=PAnsiChar(t);
  head.cchHeader:=Length(t);
  head.fKeepConn:=FResHeaders['Content-Length']<>'';//TODO: chunked encoding
  ServerFunction(HSE_REQ_SEND_RESPONSE_HEADER_EX,@head,nil,nil);
end;

function TXxmIsapiContext.Connected: Boolean;
var
  b:BOOL;
begin
  ServerFunction(HSE_REQ_IS_CONNECTED,@b,nil,nil);
  Result:=b;
end;

procedure TXxmIsapiContext.SetStatus(Code: Integer; Text: WideString);
begin
  inherited;
  //TODO
  ecb.dwHttpStatusCode:=Code;
  //ecb.
end;

procedure TXxmIsapiContext.Redirect(RedirectURL: WideString;
  Relative: Boolean);
var
  s:WideString;
  i:integer;
begin
  inherited;
  if Relative then
   begin
    //TODO: proper combine?
    if (RedirectURL<>'') and (RedirectURL[1]='/') then
      s:='http://'+GetVar(ecb,'HTTP_HOST')+FRedirectPrefix+RedirectURL
    else
     begin
      s:=FURI;
      i:=Length(s);
      while (i<>0) and (s[i]<>'/') do dec(i);
      s:='http://'+GetVar(ecb,'HTTP_HOST')+Copy(s,1,i)+RedirectURL;
     end;
   end
  else
    s:=RedirectURL;
  //utf?
  ServerFunction(HSE_REQ_SEND_URL_REDIRECT_RESP,PAnsiChar(UTF8Encode(s)),nil,nil);
  raise EXxmPageRedirected.Create(s);
end;

function TXxmIsapiContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=GetVar(ecb,'HTTP_COOKIE');
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmIsapiContext.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
  FResHeaders.Add('Set-Cookie',Name+'="'+Value+'"');
end;

procedure TXxmIsapiContext.SetCookie(Name,Value:WideString;
  KeepSeconds:cardinal; Comment,Domain,Path:WideString;
  Secure,HttpOnly:boolean);
var
  x:WideString;
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
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
  FResHeaders.Add('Set-Cookie',x);
  //TODO: Set-Cookie2
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
  if FReqHeaders=nil then
   begin
    FReqHeaders:=TRequestHeaders.Create(GetVar(ecb,'ALL_RAW'));
    (FReqHeaders as IUnknown)._AddRef;
   end;
  Result:=FReqHeaders;
end;

function TXxmIsapiContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

procedure TXxmIsapiContext.AddResponseHeader(Name, Value: WideString);
begin
  FResHeaders[Name]:=Value;
end;

{ TXxmIsapiHandler }

constructor TXxmIsapiHandler.Create;
begin
  inherited Create(false);
  //FInUse:=false;
end;

procedure TXxmIsapiHandler.Execute;
var
  Context:TXxmIsapiContext;
begin
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Context:=IsapiHandlerPool.Unqueue;
    if Context=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      Suspend;
      FInUse:=true;
     end
    else
     begin
      Context.Execute;//assert all exceptions handled!
      Context._Release;
     end;
   end;
  CoUninitialize;
end;

{ TXxmIsapiHandlerPool }

constructor TXxmIsapiHandlerPool.Create;
begin
  inherited Create;
  FHandlerSize:=0;
  FQueue:=nil;
  InitializeCriticalSection(FLock);
  SetSize(PoolMaxThreads);//TODO: setting
  //TODO: setting no pool
end;

destructor TXxmIsapiHandlerPool.Destroy;
begin
  SetSize(0);
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmIsapiHandlerPool.SetSize(x: integer);
begin
  EnterCriticalSection(FLock);
  try
    if FHandlerSize<x then
     begin
      SetLength(FHandlers,x);
      while FHandlerSize<>x do
       begin
        FHandlers[FHandlerSize]:=nil;
        inc(FHandlerSize);
       end;
     end
    else
     begin
      while FHandlerSize<>x do
       begin
        dec(FHandlerSize);
        //FreeAndNil(FHandlers[FHandlerSize]);
        if FHandlers[FHandlerSize]<>nil then
         begin
          try
            FHandlers[FHandlerSize].Terminate;
            FHandlers[FHandlerSize].Resume;
            FHandlers[FHandlerSize].Free;
          except
            //silent
          end;
          FHandlers[FHandlerSize]:=nil;
         end;
       end;
      SetLength(FHandlers,x);
     end;
    //if FLoaderIndex>=FLoaderSize then FLoaderIndex:=0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmIsapiHandlerPool.Queue(Context: TXxmIsapiContext);
var
  c:TXxmIsapiContext;
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //add to queue
    Context._AddRef;
    if FQueue=nil then FQueue:=Context else
     begin
      c:=FQueue;
      while c.Queue<>nil do c:=c.Queue;
      c.Queue:=Context;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FHandlerSize) and (FHandlers[i]<>nil) and FHandlers[i].InUse do inc(i);
  if i=FHandlerSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FHandlers[i]=nil then
      FHandlers[i]:=TXxmIsapiHandler.Create //start thread
    else
      FHandlers[i].Resume; //resume on waiting unqueues
    //TODO: expire unused threads on low load
   end;
end;

function TXxmIsapiHandlerPool.Unqueue: TXxmIsapiContext;
begin
  if FQueue=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueue;
      if Result<>nil then
       begin
        FQueue:=FQueue.Queue;
        Result.Queue:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

initialization
  IsapiHandlerPool:=TXxmIsapiHandlerPool.Create;
finalization
  //assert IsapiHandlerPool=nil by TerminateExtension
  FreeAndNil(IsapiHandlerPool);
end.

