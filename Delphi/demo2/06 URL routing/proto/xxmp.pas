unit xxmp;

interface

{
  xxm Project Unit
  demo: 06 URL routing

This xxmp.pas template has been adapted to use the FRegRouting unit instead of the default xxmFReg unit.
See GetPageAndParameters for how request addresses are mapped onto page objects to handle the request.
}

uses xxm;

type
  TXxm[[ProjectName]]=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses FRegRouting, xxmHeaders;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxm[[ProjectName]].Create(AProjectName);
end;

{ TXxm[[ProjectName]] }

function TXxm[[ProjectName]].LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //TODO: link session to request
  Result:=GetPageAndParameters(Self,Context,Address);
end;

function TXxm[[ProjectName]].LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  inherited;
  //TODO: resolve relativeto
  Result:=GetIncludeFragment(Self,Address);
end;

procedure TXxm[[ProjectName]].UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //
end;

initialization
  IsMultiThread:=true;
end.
