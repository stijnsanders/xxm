library xxmGeckoDev;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}
{$R '..\common\xxmDataDev.res' '..\common\xxmDataDev.rc'}

uses
  SysUtils,
  xxm in '..\bin\public\xxm.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmGeckoChannel in 'xxmGeckoChannel.pas',
  xxmGeckoModule in 'xxmGeckoModule.pas',
  xxmGeckoInterfaces in 'xxmGeckoInterfaces.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in '..\common\xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmGeckoStreams in 'xxmGeckoStreams.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
