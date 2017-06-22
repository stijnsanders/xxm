library xxmIsapiDev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}

uses
  SysUtils,
  Classes,
  isapi4 in 'isapi4.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmIsapiMain in 'xxmIsapiMain.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegJson in '..\common\xxmPRegJson.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmIsapiStream in 'xxmIsapiStream.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmConvert2 in '..\conv\xxmConvert2.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmConvertXML in '..\common\xxmConvertXML.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
