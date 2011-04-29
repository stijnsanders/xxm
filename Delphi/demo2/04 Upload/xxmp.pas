unit xxmp;

interface

{
  xxm Project Unit
  demo: 04 Upload

This file was generated using the xxmp.pas template, then added as described in "demo/02 Session"
}

uses xxm;

type
  TXxmdemo=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //TODO: link session to request
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
end;

function TXxmdemo.LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
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
