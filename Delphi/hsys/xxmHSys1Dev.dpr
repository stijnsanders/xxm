program xxmHSys1Dev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}
{$IFNDEF HSYS1}{$MESSAGE FATAL 'HSYS1 not defined.'}{$ENDIF}

uses
  SysUtils,
  xxm in '..\bin\public\xxm.pas',
  xxmHSys1Run in 'xxmHSys1Run.pas',
  httpapi1 in 'httpapi1.pas',
  xxmHSysMain in 'xxmHSysMain.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegJson in '..\common\xxmPRegJson.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmConvert2 in '..\conv\xxmConvert2.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmHSysHeaders in 'xxmHSysHeaders.pas',
  xxmConvertXML in '..\common\xxmConvertXML.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
  XxmRunHSys(HandleWindowsMessages);
end.
