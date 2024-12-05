program xxmProject;

uses
  Vcl.Forms,
  xxmEditProject in 'xxmEditProject.pas' {EditProjectForm},
  jsonDoc in '..\common\jsonDoc.pas',
  xxmDefs in 'xxmDefs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEditProjectForm, EditProjectForm);
  Application.Run;
end.
