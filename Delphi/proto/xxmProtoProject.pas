unit xxmProtoProject;

interface

uses xxm;

{
  update "proto/xxmp.pas" with major changes made here
}

type
  TXxmProject1=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

exports
  XxmProjectLoad;

implementation

uses xxmFReg, xxmSession;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmProject1.Create(AProjectName);
end;

{ TXxmProject1 }

function TXxmProject1.LoadPage(Context: IXxmContext;
  Address: WideString): IXxmFragment;
begin
  if Session=nil then SetSession(Context);
  Result:=LoadFragment(Context,Address,'') as IXxmPage;
end;

function TXxmProject1.LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
  if Result<>nil then Result._AddRef;
  //TODO: cache created instance?
end;

procedure TXxmProject1.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL
  Fragment._Release;//Fragment.Free;
end;

end.
