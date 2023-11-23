program xxmSynaAU;

{$R 'xxmData.res' '..\common\xxmData.rc'}
{$R 'xxmAU_manifest.res' '..\common\xxmAU_manifest.rc'}

uses
  SysUtils,
  xxmSynaMain in 'xxmSynaMain.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmSynaKept in 'xxmSynaKept.pas',
  xxmSynaSpool in 'xxmSynaSpool.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  XxmRunServer;
end.
