unit xxmHostSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs;

type
  TxxmHostService = class(TService)
    procedure ServiceExecute(Sender: TService);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  xxmHostService: TxxmHostService;

implementation

uses
  xxmHostRun;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  xxmHostService.Controller(CtrlCode);
end;

function TxxmHostService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure HandleServiceMessages(var QuitApp:boolean);
begin
  xxmHostService.ServiceThread.ProcessRequests(false);
  Sleep(1);//prevent 100% CPU usage
  if xxmHostService.Status in [csStopped, csStopPending] then QuitApp:=true;
end;

procedure TxxmHostService.ServiceExecute(Sender: TService);
begin
  XxmRunHoster(HandleServiceMessages);
end;

end.
