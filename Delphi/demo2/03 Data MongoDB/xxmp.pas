unit xxmp;

interface

{
  xxm Project Unit
  demo: 03 Data

This file was generated using the xxmp.pas template, then added as described in "demo/02 Session"
}

uses xxm;

type
  TXxmdemo=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg, xxmSession;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
begin
  inherited;
  SetSession(Context);
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
end;

function TXxmdemo.LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
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
