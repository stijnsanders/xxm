program xxmSyna;

{$R 'xxmData.res' '..\common\xxmData.rc'}

uses
  SysUtils,
  xxmSynaMain in 'xxmSynaMain.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegJson in '..\common\xxmPRegJson.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmSynaKept in 'xxmSynaKept.pas',
  xxmSynaSpool in 'xxmSynaSpool.pas';

{$R *.res}

begin
  XxmRunServer;
end.
