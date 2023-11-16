library xxmAhttpd;

{$R 'xxmData.res' '..\common\xxmData.rc'}

uses
  httpd24 in 'httpd24.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmAhttpdModule in 'xxmAhttpdModule.pas',
  xxmAhttpdContext in 'xxmAhttpdContext.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegJson in '..\common\xxmPRegJson.pas',
  xxmAhttpdClientStream in 'xxmAhttpdClientStream.pas',
  xxmAhttpdPars in 'xxmAhttpdPars.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmContext in '..\common\xxmContext.pas';

{$R *.RES}

end.
