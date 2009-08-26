program xxmHostSvc;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SvcMgr,
  xxmHostSvcMain in 'xxmHostSvcMain.pas' {xxmHostService: TService},
  xxm in '..\public\xxm.pas',
  xxmCGIHeader in 'xxmCGIHeader.pas',
  xxmHostRun in 'xxmHostRun.pas',
  xxmHostMain in 'xxmHostMain.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHttpPReg in '..\http\xxmHttpPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TxxmHostService, xxmHostService);
  Application.Run;
end.
