program xxmSynaDev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}

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
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmSynaKept in 'xxmSynaKept.pas',
  xxmSynaSpool in 'xxmSynaSpool.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmConvertXML in '..\common\xxmConvertXML.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
  XxmRunServer;
end.
