unit xxmp;

interface

uses xxm;

type
  TXxmsvnwiki=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg, wikiObj;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmsvnwiki.Create(AProjectName);
end;

{ TXxmsvnwiki }

function TXxmsvnwiki.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //SetSession(Context);
  //Result:=LoadFragment(Address);
  if Address='svnwiki.css' then 
    Result:=nil //return nil here and default to file-search
  else
    if (Address<>'') and (char(Address[1]) in ['~','+']) then
	  Result:=XxmFragmentRegistry.GetFragment(Self,'BackLinks.xxm','')
	else
	  if Context.ContextString(csQueryString)='' then
        Result:=XxmFragmentRegistry.GetFragment(Self,'Default.xxm','')
	  else
	    Result:=XxmFragmentRegistry.GetFragment(Self,'Search.xxm','');
end;

function TXxmsvnwiki.LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
  //TODO: cache created instance, incease ref count
end;

procedure TXxmsvnwiki.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

initialization
  IsMultiThread:=true;
end.
