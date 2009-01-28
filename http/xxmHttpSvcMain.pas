unit xxmHttpSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
    xxmHttpMain;

type
  TTxxmService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer:TXxmHTTPServer;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  TxxmService: TTxxmService;

implementation

uses Registry;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  TxxmService.Controller(CtrlCode);
end;

function TTxxmService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TTxxmService.ServiceStart(Sender: TService;
  var Started: Boolean);
var
  p:integer;
  r:TRegistry;
begin
  p:=80;//default;
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    r.OpenKey('\Software\xxm\service',true);
    if r.ValueExists('Port') then
      p:=r.ReadInteger('Port')
    else
      r.WriteInteger('Port',p);
  finally
    r.Free;
  end;
  FServer:=TXxmHTTPServer.Create(nil);
  FServer.LocalPort:=IntToStr(p);
  FServer.Open;
end;

procedure TTxxmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServer.Free;
end;

end.
