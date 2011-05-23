unit LineMap1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  f:TFileStream;
  l,i:integer;
  lnrs:array of record
    ln1,ln2:word;
  end;
begin
  if ParamCount>0 then
   begin
    Caption:=ParamStr(1);
    f:=TFileStream.Create(ParamStr(1),fmOpenRead);
    try
      l:=f.Size div 4;
      SetLength(lnrs,l);
      f.Read(lnrs[0],f.Size);
      for i:=0 to l-1 do Memo1.Lines.Add(Format('%.5d %.5d',[lnrs[i].ln1,lnrs[i].ln2]));
    finally
      f.Free;
    end;
   end;
end;

end.
