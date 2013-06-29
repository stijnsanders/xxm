unit xxmHttpSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
    xxmHttpMain, xxmSock;

type
  TxxmService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer:TTcpServer;
    FListener:TXxmHttpServerListener;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  xxmService: TxxmService;

implementation

uses Registry, xxmPReg, xxmPRegXml, ActiveX, xxmThreadPool;

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
  XxmProjectCache:=TXxmProjectCacheXml.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(t);
  FServer:=TTcpServer.Create;
  FServer.Bind('',p);
  FServer.Listen;
  FListener:=TXxmHttpServerListener.Create(FServer);
end;

procedure TxxmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FListener.Free;
  FServer.Free;
  FreeAndNil(XxmProjectCache);
end;

end.
