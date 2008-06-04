unit xxmFReg;

interface

uses xxm;

type
  TXxmFragmentClass=class of TXxmFragment;

  TXxmFragmentRegistry=class(TObject)
  private
    Size,Count:integer;
    Registry:array of record
      FName:string;
      FType:TXxmFragmentClass;
    end;
  public
    FilesRootPath:string;
    //TODO: virtual directories
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(FName:string;FType:TXxmFragmentClass);
    function GetClass(FName:string):TXxmFragmentClass;
  end;

var
  XxmFragmentRegistry:TXxmFragmentRegistry;

implementation

uses SysUtils, Registry;

{ TXxmFragmentRegistry }

constructor TXxmFragmentRegistry.Create;
begin
  inherited Create;
  Size:=0;
  Count:=0;
  FilesRootPath:='';
end;

destructor TXxmFragmentRegistry.Destroy;
begin
  SetLength(Registry,0);
  inherited;
end;

procedure TXxmFragmentRegistry.RegisterClass(FName: string;
  FType: TXxmFragmentClass);
begin
  if Count=Size then
   begin
    inc(Size,64);
    SetLength(Registry,Size);
   end;
  Registry[Count].FName:=LowerCase(FName);//case insensitive?
  Registry[Count].FType:=FType;
  //sort? binary tree?
  inc(Count);
end;

type
  EXxmFragmentNotFound=class(Exception)
  end;

function TXxmFragmentRegistry.GetClass(FName: string): TXxmFragmentClass;
begin
  i:=0;
  l:=LowerCase(FName);
  while (i<Count) and not(Registry[i].FName=l) do inc(i);
  if (i<Count) then Result:=Registry[i].FType else Result:=nil;
end;

initialization
  XxmFragmentRegistry:=TXxmFragmentRegistry.Create;
finalization
  XxmFragmentRegistry.Free;

end.
