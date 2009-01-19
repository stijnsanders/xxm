unit xxmp;

{
  $Rev: 204 $ $Date: 2008-04-08 21:39:14 +0200 (di, 08 apr 2008) $
}

interface

uses xxm;

type
  TXxmdemo=class(TXxmProject)
  public
    constructor Create(AProjectName: WideString);
	function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Address: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses SysUtils, Windows, xxmFReg;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);  
end;

{ TXxmdemo }

constructor TXxmdemo.Create(AProjectName: WideString);
var
  s:string;
begin
  inherited Create(AProjectName);
  SetLength(s,1024);
  SetLength(s,GetModuleFileName(HInstance,PChar(s),1024));
  SetCurrentDir(ExtractFilePath(s));
end;

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //SetSession(Context);
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
