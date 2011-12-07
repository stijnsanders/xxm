library xxmLocalDev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}

uses
  ComServ,
  xxm in '..\bin\public\xxm.pas',
  xxmHandler in 'xxmHandler.pas',
  xxmLoader in 'xxmLoader.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmWinInet in 'xxmWinInet.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in 'xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
