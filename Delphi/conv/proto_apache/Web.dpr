library [[ProjectName]];

{
  xxm 'skip the handler'
  Dedicated Apache httpd module dll from xxm project
  
  Usage:
    xxmConv /proto <path to these files> /x:XxmSourcePath <path to xxm source> /src <path to new folder for generated code> <path to xxm project>
	
  Rember to set up version info on the resulting project.
  
  $Rev$ $Date$
}

{$R '[[XxmSourcePath]]\common\xxmData.res' '[[XxmSourcePath]]\common\xxmData.rc'}

[[ProjectSwitches]]
uses
  HTTPD2 in '[[XxmSourcePath]]\apache\HTTPD2.pas',
  xxm in '[[XxmSourcePath]]\bin\public\xxm.pas',
  xxmHeaders in '[[XxmSourcePath]]\bin\public\xxmHeaders.pas',
  xxmParUtils in '[[XxmSourcePath]]\common\xxmParUtils.pas',
  xxmParams in '[[XxmSourcePath]]\common\xxmParams.pas',
  xxmAhttpdModule in '[[XxmSourcePath]]\apache\xxmAhttpdModule.pas',
  xxmAhttpdContext in '[[XxmSourcePath]]\apache\xxmAhttpdContext.pas',
  xxmCommonUtils in '[[XxmSourcePath]]\common\xxmCommonUtils.pas',
  xxmContext in '[[XxmSourcePath]]\common\xxmContext.pas',
  xxmPReg in '[[XxmSourcePath]]\common\xxmPReg.pas',
  xxmPRegXml in '[[XxmSourcePath]]\conv\xxmPRegXml.pas', //ATTENTION: rigged for single project
  xxmAhttpdClientStream in '[[XxmSourcePath]]\apache\xxmAhttpdClientStream.pas',
  xxmAhttpdPars in '[[XxmSourcePath]]\apache\xxmAhttpdPars.pas',
  MSXML2_TLB in '[[XxmSourcePath]]\common\MSXML2_TLB.pas',
  [[@Include]][[IncludeUnit]] in '[[ProjectPath]][[IncludePath]][[IncludeUnit]].pas',
  [[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
  [[@]][[UsesClause]]
  xxmp in '[[ProjectPath]]xxmp.pas';

{$R *.RES}

[[ProjectHeader]]
begin
  XxmProjectName:='[[ProjectName]]';
  [[ProjectBody]]
end.