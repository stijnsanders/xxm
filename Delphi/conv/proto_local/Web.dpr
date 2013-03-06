library [[ProjectName]];

{
  xxm 'skip the handler'
  Dedicated Internet Explorer IInternetProtocol implementation dll from xxm project
  
  Usage:
    xxmConv /proto <path to these files> /x:XxmSourcePath <path to xxm source> /src <path to new folder for generated code> <path to xxm project>
	
  Rember to set up version info on the resulting project.
  
  $Rev$ $Date$
}

{$R '[[XxmSourcePath]]\common\xxmData.res' '[[XxmSourcePath]]\common\xxmData.rc'}

[[ProjectSwitches]]
uses
  ComServ,
  xxm in '[[XxmSourcePath]]\bin\public\xxm.pas',
  xxmHandler in '[[XxmSourcePath]]\local\xxmHandler.pas',
  xxmLoader in '[[XxmSourcePath]]\local\xxmLoader.pas',
  xxmSettings in '[[XxmSourcePath]]\local\xxmSettings.pas',
  xxmWinInet in '[[XxmSourcePath]]\local\xxmWinInet.pas',
  xxmPReg in '[[XxmSourcePath]]\common\xxmPReg.pas',
  xxmPRegLocal in '[[XxmSourcePath]]\conv\xxmPRegLocal.pas', //ATTENTION: rigged for single project
  xxmParams in '[[XxmSourcePath]]\common\xxmParams.pas',
  xxmParUtils in '[[XxmSourcePath]]\common\xxmParUtils.pas',
  xxmHeaders in '[[XxmSourcePath]]\bin\public\xxmHeaders.pas',
  xxmThreadPool in '[[XxmSourcePath]]\common\xxmThreadPool.pas',
  xxmCommonUtils in '[[XxmSourcePath]]\common\xxmCommonUtils.pas',
  xxmContext in '[[XxmSourcePath]]\common\xxmContext.pas',
  xxmReadHandler in '[[XxmSourcePath]]\http\xxmReadHandler.pas',
  [[@Include]][[IncludeUnit]] in '[[ProjectPath]][[IncludePath]][[IncludeUnit]].pas',
  [[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
  [[@]][[UsesClause]]
  xxmp in '[[ProjectPath]]xxmp.pas';

{$R *.RES}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

[[ProjectHeader]]
begin
  URLSchema:='[[ProjectName]]';
  URLSchemaDescription:='[[ProjectName]] URL Protocol'; 
  [[ProjectBody]]
end.