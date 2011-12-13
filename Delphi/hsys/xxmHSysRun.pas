unit xxmHSysRun;

interface

type
  TXxmHandleMessagesProc=procedure(var QuitApp:boolean);

procedure XxmRunHSys(HandleMessagesProc:TXxmHandleMessagesProc);

procedure HandleWindowsMessages(var QuitApp:boolean);

implementation

uses Windows, SysUtils, Classes, ActiveX,
  httpapi1, xxmHSysPReg, xxmThreadPool, xxmHSys1Main;

procedure XxmRunHSys(HandleMessagesProc:TXxmHandleMessagesProc);
var
  i:integer;
  s:AnsiString;
  w:WideString;
  hrq:THandle;
  QuitApp:boolean;
begin
  QuitApp:=false;

  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCache.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create;

  HttpCheck(HttpInitialize(HTTPAPI_VERSION_1_0,HTTP_INITIALIZE_SERVER,nil));
  HttpCheck(HttpCreateHttpHandle(hrq,0));

  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    //TODO: parameters?
    w:='http://+:80/'+s+'/';
    HttpCheck(HttpAddUrl(hrq,PWideChar(w),nil));
   end;
  //TODO: check any loaded? load from xxm.xml?

  //TODO: try except
  //TODO: mutex?
  //TODO: overlapped/completionport
  while not QuitApp do
   begin
    //if WaitForSingleObject(hrq,0)=WAIT_OBJECT_0 then ???
      PageLoaderPool.Queue(TXxmHSys1Context.Create(hrq));
    HandleMessagesProc(QuitApp);
   end;

  //HttpCheck(HttpRemoveUrl(
  HttpTerminate(HTTP_INITIALIZE_SERVER,nil);
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
