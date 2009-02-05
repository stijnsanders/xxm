library xxmLocalAU;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  ComServ,
  xxm in '..\public\xxm.pas',
  xxmHandler in 'xxmHandler.pas',
  xxmLoader in 'xxmLoader.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmWinInet in 'xxmWinInet.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmThreadPool in 'xxmThreadPool.pas',
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
