program xxmHSys1;

{$R 'xxmData.res' '..\common\xxmData.rc'}
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
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmHSysHeaders in 'xxmHSysHeaders.pas',
  xxmSSPI in 'xxmSSPI.pas';

{$R *.res}

begin
  XxmRunHSys(HandleWindowsMessages);
end.
