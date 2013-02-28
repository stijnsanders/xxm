unit xxmSynaSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
    xxmSynaMain;

type
  TxxmService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer:TXxmSynaServer;
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
  ParameterKey:array[TXxmSynaRunParameters] of string=(
    'Port',
    'Silent',
    'LoadCopy',
    'StartURL',
    'Threads',
    //add new here
    '');
begin
  //defaults
  p:=80;
  t:=$100;
  GlobalAllowLoadCopy:=true;
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
  FServer:=TXxmSynaServer.Create(p);
  //if not FServer.Listening then raise?
end;

procedure TxxmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServer.Free;
end;

end.
