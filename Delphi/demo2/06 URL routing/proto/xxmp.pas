unit xxmp;

interface

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
  Result:=GetPageAndParameters(Context,Address);
end;

function TXxm[[ProjectName]].LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  inherited;
  //TODO: resolve relativeto
  Result:=GetIncludeFragment(Address);
end;

procedure TXxm[[ProjectName]].UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //
end;

initialization
  IsMultiThread:=true;
end.
