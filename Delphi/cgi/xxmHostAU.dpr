program xxmHostAU;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmAU_manifest.res' '..\common\xxmAU_manifest.rc'}

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
  xxmPRegJson in '..\common\xxmPRegJson.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmConvert2 in '..\conv\xxmConvert2.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  XxmRunHoster(HandleWindowsMessages);
end.
