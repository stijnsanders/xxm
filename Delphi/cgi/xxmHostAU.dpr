program xxmHostAU;

{$R 'xxmData.res' '..\common\xxmData.rc'}
{$R 'xxmAU_manifest.res' '..\common\xxmAU_manifest.rc'}

uses
  SysUtils,
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
  jsonDoc in '..\common\jsonDoc.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  XxmRunHoster(HandleWindowsMessages);
end.
