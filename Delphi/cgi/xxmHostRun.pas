unit xxmHostRun;

interface

type
  TXxmHandleMessagesProc=procedure(var QuitApp:boolean);

procedure XxmRunHoster(HandleMessagesProc:TXxmHandleMessagesProc);

procedure HandleWindowsMessages(var QuitApp:boolean);

implementation

uses Windows, SysUtils, ActiveX, xxmPReg, xxmPRegJson,
  xxmThreadPool, xxmHostMain, xxmCGIHeader, xxmContext;

procedure XxmRunHoster(HandleMessagesProc:TXxmHandleMessagesProc);
type
  TParameters=(
    cpPipePath,
    cpLoadCopy,
    cpThreads,
    //add new here
    cp_Unknown);
const
  ParameterKey:array[TParameters] of AnsiString=(
    'pipe',
    'loadcopy',
    'threads',
    //add new here (lowercase)
    '');
  FILE_FLAG_FIRST_PIPE_INSTANCE=$00080000;
var
  i,j,Threads:integer;
  s,t,PipePath:AnsiString;
  h,h1,h2:THandle;
  par:TParameters;
  QuitApp:boolean;
  r:DWORD;
  ch:TxxmCGIHeader;
  l:cardinal;
begin
  PipePath:='xxm';//default
  Threads:=$100;//default
  QuitApp:=false;

  for i:=1 to ParamCount do
   begin
    s:=AnsiString(ParamStr(i));
    j:=1;
    while (j<=Length(s)) and not(s[j]='=') do inc(j);
    t:=AnsiString(LowerCase(string(Copy(s,1,j-1))));
    par:=TParameters(0);
    while not(par=cp_Unknown) and not(t=ParameterKey[par]) do inc(par);
    case par of
      cpPipePath:
        PipePath:=Copy(s,j+1,Length(s)-j);
      cpLoadCopy:
        GlobalAllowLoadCopy:=Copy(s,j+1,Length(s)-j)<>'0';
      cpThreads:
        Threads:=StrToInt(string(Copy(s,j+1,Length(s)-j)));
      //add new here
      cp_Unknown:
        raise Exception.Create('Unknown setting: '+string(t));
    end;
   end;

  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCacheJson.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);

  h:=INVALID_HANDLE_VALUE;
  ch.Size:=SizeOf(TxxmCGIHeader);
  ch.ServerProcessID:=GetCurrentProcessId;

  //TODO: queue of pipe handles (recycle disconnected pipe handles?)
  //TODO: try except
  //TODO: mutex?
  while not(QuitApp) do
   begin
    if h=INVALID_HANDLE_VALUE then
      h:=CreateNamedPipeA(PAnsiChar('\\.\pipe\'+PipePath),
        PIPE_ACCESS_OUTBOUND,
        PIPE_TYPE_BYTE or PIPE_NOWAIT,
        PIPE_UNLIMITED_INSTANCES,//setting?
        $1000,//in buffer size
        $1000,//out buffer size
        30000,//timeout
        nil);
    if h=INVALID_HANDLE_VALUE then RaiseLastOSError;
    if ConnectNamedPipe(h,nil) then r:=ERROR_PIPE_CONNECTED else r:=GetLastError;
    if r=ERROR_PIPE_CONNECTED then
     begin
      CreatePipe(h1,ch.PipeRequest,nil,$1000);
      CreatePipe(ch.PipeResponse,h2,nil,$1000);
      if WriteFile(h,ch,ch.Size,l,nil) then
        (ContextPool.GetContext as TXxmHostedContext).Load(h1,h2)
      else
       begin
        CloseHandle(h);
        CloseHandle(h1);
        CloseHandle(h2);
        CloseHandle(ch.PipeRequest);
        CloseHandle(ch.PipeResponse);
       end;
      h:=INVALID_HANDLE_VALUE;
     end
    else
      if r=ERROR_PIPE_LISTENING then
        HandleMessagesProc(QuitApp)
      else
        raise Exception.Create(IntToStr(r)+' '+SysErrorMessage(r));
   end;
end;

procedure HandleWindowsMessages(var QuitApp:boolean);
var
  Msg:TMsg;
const
  WM_QUIT = $0012;//from Messages
begin
  if PeekMessage(Msg,0,0,0,PM_REMOVE) then
    if Msg.message=WM_QUIT then
      QuitApp:=true
    else
     begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
     end
  else
    Sleep(1);//don't take 100% CPU!
end;

end.
