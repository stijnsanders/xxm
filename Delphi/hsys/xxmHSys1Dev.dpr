program xxmHSys1Dev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}

uses
  SysUtils,
  xxm in '..\bin\public\xxm.pas',
  xxmHSysRun in 'xxmHSysRun.pas',
  httpapi1 in 'httpapi1.pas',
  xxmHSys1Main in 'xxmHSys1Main.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHSysPReg in 'xxmHSysPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmHSysHeaders in 'xxmHSysHeaders.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
  XxmRunHSys(HandleWindowsMessages);
end.
