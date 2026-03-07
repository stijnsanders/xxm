program xxmHSys2;

{$IFNDEF HSYS2}{$MESSAGE FATAL 'HSYS2 not defined.'}{$ENDIF}
{$R 'xxmData.res' '..\common\xxmData.rc'}
{$R 'xxmHSys2_ver.res' 'xxmHSys2_ver.rc'}

uses
  SysUtils,
  xxm in '..\bin\public\xxm.pas',
  xxmHSys2Run in 'xxmHSys2Run.pas',
  httpapi2 in 'httpapi2.pas',
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
