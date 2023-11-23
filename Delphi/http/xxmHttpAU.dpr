program xxmHttpAU;

{$R 'xxmData.res' '..\common\xxmData.rc'}
{$R 'xxmAU_manifest.res' '..\common\xxmAU_manifest.rc'}

uses
  SysUtils,
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
  XxmRunServer;
end.
