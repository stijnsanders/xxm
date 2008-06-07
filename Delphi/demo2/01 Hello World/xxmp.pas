unit xxmp;

{
  $Rev: 204 $ $Date: 2008-04-08 21:39:14 +0200 (di, 08 apr 2008) $
}

interface

uses xxm;

type
  TXxmdemo2=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Address: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo2.Create(AProjectName);
end;

{ TXxmdemo2 }

function TXxmdemo2.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //TODO: link session to request
  Result:=LoadFragment(Address);
end;

function TXxmdemo2.LoadFragment(Address: WideString): IXxmFragment;
var
  fc:TXxmFragmentClass;
begin
  fc:=XxmFragmentRegistry.GetClass(Address);
  if fc=nil then Result:=nil else Result:=fc.Create(Self);
  //TODO: cache created instance, incease ref count
end;

procedure TXxmdemo2.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

initialization
  IsMultiThread:=true;
end.
