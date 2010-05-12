program xxmHttpSvcAU;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SvcMgr,
  xxmHttpSvcMain in 'xxmHttpSvcMain.pas' {TxxmService: TService},
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHttpPReg in 'xxmHttpPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  Application.Initialize;
  Application.CreateForm(TTxxmService, TxxmService);
  Application.Run;
end.
