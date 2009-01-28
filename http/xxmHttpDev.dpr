program xxmHttpDev;

{$R 'xxmData.res' 'xxmData.rc'}
{$R 'xxmDataDev.res' 'xxmDataDev.rc'}

uses
  SysUtils,
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
  XxmRunServer;
end.
