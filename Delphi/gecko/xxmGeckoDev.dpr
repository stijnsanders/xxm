library xxmGeckoDev;

{$R 'xxmData.res' 'xxmData.rc'}
{$R 'xxmDataDev.res' 'xxmDataDev.rc'}

uses
  SysUtils,
  xxm in '..\public\xxm.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmGeckoProtocol in 'xxmGeckoProtocol.pas',
  xxmGeckoChannel in 'xxmGeckoChannel.pas',
  xxmGeckoModule in 'xxmGeckoModule.pas',
  xxmGeckoInterfaces in 'xxmGeckoInterfaces.pas',
  Debug1 in 'debug\Debug1.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in '..\local\xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmGeckoStreams in 'xxmGeckoStreams.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
