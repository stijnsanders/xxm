unit xxmDictionary;

interface

type
  TxxmDictionary=class(TObject)
  private
    FData:array of record
      Name,Value:string;
    end;
    function GetItem(Name: string): string;
    procedure SetItem(Name: string; const Value: string);
  public
    property Item[Name:string]:string read GetItem write SetItem; default;
  end;

implementation

uses SysUtils;

{ TxxmDictionary }

function TxxmDictionary.GetItem(Name: string): string;
var
  i,l:integer;
  n:string;
begin
  //TODO: something better than a plain unordered list
  //TODO: something better than a plain forward search
  i:=0;
  l:=Length(FData);
  n:=LowerCase(Name);
  while (i<l) and not(n=FData[i].Name) do inc(i);
  if i=l then Result:='' else Result:=FData[i].Value;
end;

procedure TxxmDictionary.SetItem(Name: string;
  const Value: string);
var
  i,l:integer;
  n:string;
begin
  //TODO: something better than a plain unordered list
  //TODO: something better than a plain forward search
  i:=0;
  l:=Length(FData);
  n:=LowerCase(Name);
  while (i<l) and not(n=FData[i].Name) do inc(i);
  if i=l then
   begin
    SetLength(FData,l+1);
    FData[i].Name:=n;
    FData[i].Value:=Value;
   end
  else FData[i].Value:=Value;
end;

end.
