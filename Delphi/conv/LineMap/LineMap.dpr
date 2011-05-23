program LineMap;

uses
  Forms,
  LineMap1 in 'LineMap1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
