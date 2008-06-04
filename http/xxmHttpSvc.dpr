program xxmHttpSvc;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SvcMgr,
  xxmHttpSvcMain in 'xxmHttpSvcMain.pas' {TxxmService: TService},
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmReadHandler in 'xxmReadHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTxxmService, TxxmService);
  Application.Run;
end.
