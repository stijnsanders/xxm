library xxmIsapiEx;

{$R 'xxmData.res' 'xxmData.rc'}

uses
  SysUtils,
  Classes,
  isapi4 in 'isapi4.pas',
  xxm in '..\public\xxm.pas',
  xxmIsapiMain in 'xxmIsapiMain.pas',
  xxmPReg in 'xxmPReg.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

end.
