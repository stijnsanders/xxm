program xxmHttpSvcAU;

{$R 'xxmData.res' '..\common\xxmData.rc'}

uses
  SvcMgr,
  xxmHttpSvcMain in 'xxmHttpSvcMain.pas' {TxxmService: TService},
  xxm in '..\bin\public\xxm.pas',
  xxmHttpCtx in 'xxmHttpCtx.pas',
  xxmHttpRun in 'xxmHttpRun.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmSock in 'xxmSock.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmKeptCon in 'xxmKeptCon.pas',
  xxmSpoolingCon in 'xxmSpoolingCon.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  Application.Initialize;
  Application.CreateForm(TxxmService, xxmService);
  Application.Run;
end.
