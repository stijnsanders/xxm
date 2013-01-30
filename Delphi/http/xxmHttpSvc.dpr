program xxmHttpSvc;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}

uses
  SvcMgr,
  xxmHttpSvcMain in 'xxmHttpSvcMain.pas' {TxxmService: TService},
  xxmHttpMain in 'xxmHttpMain.pas',
  xxm in '..\bin\public\xxm.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmPRegXml in '..\common\xxmPRegXml.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmReadHandler in 'xxmReadHandler.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TxxmService, xxmService);
  Application.Run;
end.
