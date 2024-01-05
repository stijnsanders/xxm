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
  SvcMgr,
  SysUtils,
  xxmHttpSvcMain in '[[XxmSourcePath]]\http\xxmHttpSvcMain.pas' {TxxmService: TService},
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
  [[ProjectBody]]
  Application.Initialize;
  Application.CreateForm(TxxmService, xxmService);
  xxmService.DisplayName:=XxmProjectName;//+'Svc';?
  //TODO: more arguments?
  if (ParamCount=1) and (Copy(ParamStr(1),1,5)='Port=') then
    HttpListenPort:=StrToInt(Copy(ParamStr(1),6,5));
  Application.Run;
end.
