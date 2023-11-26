program [[ProjectName]];

{
  xxm 'skip the handler'
  HTTP dedicated exe from xxm project

  Usage:
    xxmConv /proto <path to these files> /x:XxmSourcePath <path to xxm source> /src <path to new folder for generated code> <path to xxm project>

  Rember to set up version info on the resulting project.

  $Rev$ $Date$
}

{$R '[[XxmSourcePath]]\common\xxmData.res' '[[XxmSourcePath]]\common\xxmData.rc'}

[[ProjectSwitches]]
uses
  SysUtils,
  xxmHttpCtx in '[[XxmSourcePath]]\http\xxmHttpCtx.pas',
  xxmHttpRun in '[[XxmSourcePath]]\http\xxmHttpRun.pas',
  xxm in '[[XxmSourcePath]]\bin\public\xxm.pas',
  xxmParams in '[[XxmSourcePath]]\common\xxmParams.pas',
  xxmParUtils in '[[XxmSourcePath]]\common\xxmParUtils.pas',
  xxmHeaders in '[[XxmSourcePath]]\bin\public\xxmHeaders.pas',
  jsonDoc in '[[XxmSourcePath]]\common\jsonDoc.pas',
  xxmPReg in '[[XxmSourcePath]]\common\xxmPReg.pas', //ATTENTION: see XXM_INLINE_PROJECT
  xxmCommonUtils in '[[XxmSourcePath]]\common\xxmCommonUtils.pas',
  xxmContext in '[[XxmSourcePath]]\common\xxmContext.pas',
  xxmReadHandler in '[[XxmSourcePath]]\http\xxmReadHandler.pas',
  xxmLocalOnly in '[[XxmSourcePath]]\conv\proto_http_localOnly\xxmLocalOnly.pas',
  xxmSock in '[[XxmSourcePath]]\http\xxmSock.pas',
  xxmThreadPool in '[[XxmSourcePath]]\common\xxmThreadPool.pas',
  xxmKeptCon in '[[XxmSourcePath]]\http\xxmKeptCon.pas',
  xxmSpoolingCon in '[[XxmSourcePath]]\http\xxmSpoolingCon.pas',
  [[@Include]][[IncludeUnit]] in '[[ProjectPath]][[IncludePath]][[IncludeUnit]].pas',
  [[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
  [[@]][[UsesClause]]
  xxmp in '[[ProjectPath]]xxmp.pas';

{$R *.RES}

[[ProjectHeader]]
begin
  XxmProjectName:='[[ProjectName]]';
  HttpListenPort:=8877;
  HttpBindIPv4:='127.0.0.1';
  HttpBindIPv6:='::1';
  XxmStartURL;
  [[ProjectBody]]
  if XxmGlobalMutex(XxmProjectName) then
    XxmRunServer;
end.
