program xxmHSys1SvcAU;

{$R '..\common\xxmData.res' '..\common\xxmData.rc'}

uses
  SvcMgr,
  xxmHSysSvcMain in 'xxmHSysSvcMain.pas' {TxxmService: TService},
  xxm in '..\bin\public\xxm.pas',
  xxmHSysRun in 'xxmHSysRun.pas',
  httpapi1 in 'httpapi1.pas',
  xxmHSys1Main in 'xxmHSys1Main.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmHeaders in '..\bin\public\xxmHeaders.pas',
  xxmThreadPool in '..\common\xxmThreadPool.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHSysPReg in 'xxmHSysPReg.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoUpdate in '..\common\xxmAutoUpdate.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmHSysHeaders in 'xxmHSysHeaders.pas';

{$R *.res}

begin
  XxmAutoBuildHandler:=AutoUpdate;
  Application.Initialize;
  Application.CreateForm(TxxmService, xxmService);
  Application.Run;
end.
