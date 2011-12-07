library xxmLocalAU;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}

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
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
  XxmAutoBuildHandler:=AutoUpdate;
end.
