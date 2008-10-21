unit xxmFReg;

interface

uses xxm, Classes;

//$Rev$
//$Date$

type
  TXxmFragmentClass=class of TXxmFragment;

  TXxmFragmentRegistry=class(TObject)
  private
    Registry:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(FName:string;FType:TXxmFragmentClass);
    function GetClass(FName:string):TXxmFragmentClass;
  end;

var
  XxmFragmentRegistry:TXxmFragmentRegistry;

const
  XXmDefaultPage:string='default.xxm';

implementation

uses SysUtils;

{ TXxmFragmentRegistry }

constructor TXxmFragmentRegistry.Create;
begin
  inherited Create;
  Registry:=TStringList.Create;
  Registry.Sorted:=true;
  Registry.Duplicates:=dupIgnore;//dupError?setting?
  Registry.CaseSensitive:=false;//setting?
end;

destructor TXxmFragmentRegistry.Destroy;
begin
  //Registry.Clear;//?
  Registry.Free;
  inherited;
end;

procedure TXxmFragmentRegistry.RegisterClass(FName: string;
  FType: TXxmFragmentClass);
begin
  Registry.AddObject(FName,TObject(FType));
end;

function TXxmFragmentRegistry.GetClass(FName: string): TXxmFragmentClass;
var
  i:integer;
begin
  i:=Registry.IndexOf(FName);
  if i=-1 then
    if (FName='') or (FName[Length(FName)]='/') then 
	  i:=Registry.IndexOf(FName+XxmDefaultPage)
	else
	  i:=Registry.IndexOf(FName+'/'+XxmDefaultPage);
  if i=-1 then Result:=nil else Result:=TXxmFragmentClass(Registry.Objects[i]);
end;

initialization
  XxmFragmentRegistry:=TXxmFragmentRegistry.Create;
finalization
  XxmFragmentRegistry.Free;

end.
