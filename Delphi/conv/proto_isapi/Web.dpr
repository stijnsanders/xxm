library [[ProjectName]];

{
  xxm 'skip the handler'
  Dedicated ISAPI extension dll from xxm project
  
  Usage:
    xxmConv /proto <path to these files> /x:XxmSourcePath <path to xxm source> /src <path to new folder for generated code> <path to xxm project>
	
  Rember to set up version info on the resulting project.
  
  $Rev$ $Date$
}

{$R '[[XxmSourcePath]]\common\xxmData.res' '[[XxmSourcePath]]\common\xxmData.rc'}

[[ProjectSwitches]]
uses
  SysUtils,
  Classes,
  isapi4 in '[[XxmSourcePath]]\isapi\isapi4.pas',
  xxm in '[[XxmSourcePath]]\bin\public\xxm.pas',
  xxmIsapiMain in '[[XxmSourcePath]]\isapi\xxmIsapiMain.pas',
  xxmPReg in '[[XxmSourcePath]]\common\xxmPReg.pas',
  xxmPRegXml in '[[XxmSourcePath]]\conv\xxmPRegXml.pas', //ATTENTION: rigged for single project
  xxmParams in '[[XxmSourcePath]]\common\xxmParams.pas',
  xxmParUtils in '[[XxmSourcePath]]\common\xxmParUtils.pas',
  xxmHeaders in '[[XxmSourcePath]]\bin\public\xxmHeaders.pas',
  MSXML2_TLB in '[[XxmSourcePath]]\common\MSXML2_TLB.pas',
  xxmCommonUtils in '[[XxmSourcePath]]\common\xxmCommonUtils.pas',
  xxmContext in '[[XxmSourcePath]]\common\xxmContext.pas',
  xxmIsapiStream in '[[XxmSourcePath]]\isapi\xxmIsapiStream.pas',
  [[@Include]][[IncludeUnit]] in '[[ProjectPath]][[IncludePath]][[IncludeUnit]].pas',
  [[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
  [[@]][[UsesClause]]
  xxmp in '[[ProjectPath]]xxmp.pas';

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

[[ProjectHeader]]
begin
  XxmProjectName:='[[ProjectName]]';
  [[ProjectBody]]
end.