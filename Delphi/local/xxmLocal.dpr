library xxmLocal;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  ComServ,
  xxm in '..\public\xxm.pas',
  xxmHandler in 'xxmHandler.pas',
  xxmLoader in 'xxmLoader.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmWinInet in 'xxmWinInet.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in 'xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmThreadPool in 'xxmThreadPool.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

end.
