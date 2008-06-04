program xxmProject;

uses
  Forms,
  xxmEditProject in 'xxmEditProject.pas' {EditProjectMainForm},
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmUtilities in 'xxmUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEditProjectMainForm, EditProjectMainForm);
  Application.Run;
end.
