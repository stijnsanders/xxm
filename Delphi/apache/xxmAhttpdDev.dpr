library xxmAhttpdDev;

{$R 'xxmData.res' '..\common\xxmData.rc'}
{$R 'xxmDataDev.res' '..\common\xxmDataDev.rc'}

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
  xxmAhttpdClientStream in 'xxmAhttpdClientStream.pas',
  xxmAhttpdPars in 'xxmAhttpdPars.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  jsonDoc in '..\common\jsonDoc.pas';

{$R *.RES}

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
