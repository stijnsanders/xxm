unit xxmHttpSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
    xxmHttpRun, xxmSock;

type
  TxxmService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer,FServer6:TTcpServer;
    FListener,FListener6:TXxmHttpServerListener;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  xxmService: TxxmService;

implementation

uses Registry, xxmPReg, ActiveX, xxmThreadPool, xxmContext, xxmHttpCtx,
  xxmKeptCon, xxmSpoolingCon;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  xxmService.Controller(CtrlCode);
end;

function TxxmService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TxxmService.ServiceStart(Sender: TService;
  var Started: Boolean);
var
  p,t:integer;
  r:TRegistry;
  s:string;
const
  ParameterKey:array[TXxmHttpRunParameters] of string=(
    'Port',
    'LoadCopy',
    'StartURL',
    'Threads',
    //add new here
    '');
begin
  p:=80;//default
  t:=$100;//default;
  GlobalAllowLoadCopy:=true;//default
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    r.OpenKey('\Software\xxm\service',true);
    s:=ParameterKey[rpPort];
    if r.ValueExists(s) then p:=r.ReadInteger(s) else r.WriteInteger(s,p);
    s:=ParameterKey[rpLoadCopy];
    if r.ValueExists(s) then GlobalAllowLoadCopy:=r.ReadBool(s) else r.WriteBool(s,GlobalAllowLoadCopy);
    s:=ParameterKey[rpThreads];
    if r.ValueExists(s) then t:=r.ReadInteger(s) else r.WriteInteger(s,t);
  finally
    r.Free;
  end;

  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  XxmProjectCache:=TXxmProjectCache.Create;
  ContextPool:=TXxmContextPool.Create(TXxmHttpContext);
  KeptConnections:=TXxmKeptConnections.Create;
  SpoolingConnections:=TXxmSpoolingConnections.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(t);

  //IPv4
  FServer:=TTcpServer.Create(AF_INET);
  FServer.Bind('',p);
  FServer.Listen;
  FListener:=TXxmHttpServerListener.Create(FServer);

  //IPv6
  FServer6:=TTcpServer.Create(AF_INET6);
  FListener6:=nil;//default
  try
    FServer6.Bind('',p);
    FServer6.Listen;
    FListener6:=TXxmHttpServerListener.Create(FServer6);
  except
    FServer6:=nil;
  end;

end;

procedure TxxmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FListener.Free;
  FListener6.Free;
  FServer.Free;
  FServer6.Free;

  //first make ReleasingContexts/ReleaseProject run
  try
    FreeAndNil(XxmProjectCache);
  except
    //log?
  end;

  KeptConnections.Free;
  SpoolingConnections.Free;
end;

end.
