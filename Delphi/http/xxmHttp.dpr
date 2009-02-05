program xxmHttp;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SysUtils,
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas';

{$R *.res}

begin
  XxmRunServer;
end.
