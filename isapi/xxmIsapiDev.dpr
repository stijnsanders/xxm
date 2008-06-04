library xxmIsapiDev;

{$R 'xxmData.res' 'xxmData.rc'}
{$R 'xxmDataDev.res' 'xxmDataDev.rc'}

uses
  SysUtils,
  Classes,
  isapi4 in 'isapi4.pas',
  xxm in '..\public\xxm.pas',
  xxmIsapiMain in 'xxmIsapiMain.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
