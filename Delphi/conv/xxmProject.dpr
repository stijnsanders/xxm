program xxmProject;

{$R 'xxmProject_ver.res' 'xxmProject_ver.rc'}

uses
  Forms,
  xxmEditProject in 'xxmEditProject.pas' {EditProjectMainForm},
  xxmUtilities in 'xxmUtilities.pas',
  jsonDoc in '..\common\jsonDoc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEditProjectMainForm, EditProjectMainForm);
  Application.Run;
end.
