program xxmHttpSvcAU;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}

uses
  SvcMgr,
  xxmHttpSvcMain in 'xxmHttpSvcMain.pas' {TxxmService: TService},
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegXml in '..\common\xxmPRegXml.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmSock in 'xxmSock.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  Application.Initialize;
  Application.CreateForm(TxxmService, xxmService);
  Application.Run;
end.
