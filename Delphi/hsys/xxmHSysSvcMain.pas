unit xxmHSysSvcMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs;

type
  TxxmService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
  public
    function GetServiceController: TServiceController; override;
  end;

  TxxmRunThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  xxmService: TxxmService;
  xxmRunThread: TxxmRunThread;

implementation

uses Registry, xxmPReg, xxmHSysMain,
  {$IFDEF HSYS1}xxmHSys1Run;{$ENDIF}
  {$IFDEF HSYS2}xxmHSys2Run;{$ENDIF}

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
begin
  xxmRunThread:=TxxmRunThread.Create(false);
end;

procedure TxxmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  xxmRunThread.Free;//calls Terminate
end;

{ TxxmRunThread }

procedure CheckRunThread(var QuitApp:boolean);
begin
  if xxmRunThread.Terminated then QuitApp:=true;
end;

procedure TxxmRunThread.Execute;
begin
  XxmRunHSys(CheckRunThread);
end;

end.
