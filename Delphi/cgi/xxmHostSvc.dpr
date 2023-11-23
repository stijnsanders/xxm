program xxmHostSvc;

{$R 'xxmData.res' '..\common\xxmData.rc'}

uses
  SvcMgr,
  xxmHostSvcMain in 'xxmHostSvcMain.pas' {xxmHostService: TService},
  xxm in '..\bin\public\xxm.pas',
  xxmCGIHeader in 'xxmCGIHeader.pas',
  xxmHostRun in 'xxmHostRun.pas',
  xxmHostMain in 'xxmHostMain.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  jsonDoc in '..\common\jsonDoc.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TxxmHostService, xxmHostService);
  Application.Run;
end.
