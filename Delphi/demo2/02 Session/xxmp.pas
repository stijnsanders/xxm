unit xxmp;

{
  $Rev: 204 $ $Date: 2008-04-08 21:39:14 +0200 (di, 08 apr 2008) $
}

interface

uses xxm;

type
  TXxmdemo=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Address: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg, xxmSession;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  SetSession(Context);
  Result:=LoadFragment(Address);
end;

function TXxmdemo.LoadFragment(Address: WideString): IXxmFragment;
var
  fc:TXxmFragmentClass;
begin
  fc:=XxmFragmentRegistry.GetClass(Address);
  if fc=nil then Result:=nil else Result:=fc.Create(Self);
  //TODO: cache created instance, incease ref count
end;

procedure TXxmdemo.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

initialization
  IsMultiThread:=true;
end.
