library xxmGeckoAU;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SysUtils,
  xxm in '..\public\xxm.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmGeckoProtocol in 'xxmGeckoProtocol.pas',
  xxmGeckoChannel in 'xxmGeckoChannel.pas',
  xxmGeckoModule in 'xxmGeckoModule.pas',
  xxmGeckoInterfaces in 'xxmGeckoInterfaces.pas',
  xxmSettings in 'xxmSettings.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in '..\local\xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmGeckoStreams in 'xxmGeckoStreams.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
end.
