library xxmGeckoAU;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}

uses
  SysUtils,
  xxm in '..\bin\public\xxm.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmGeckoChannel in 'xxmGeckoChannel.pas',
  xxmGeckoSettings in 'xxmGeckoSettings.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegLocal in '..\common\xxmPRegLocal.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
end.
