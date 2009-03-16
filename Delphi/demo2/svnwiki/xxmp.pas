unit xxmp;

interface

uses xxm;

type
  TXxmsvnwiki=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Address: WideString): IXxmFragment; override;
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
var
  fc:TXxmFragmentClass;
begin
  inherited;
  //SetSession(Context);
  //Result:=LoadFragment(Address);
  if Address='svnwiki.css' then 
    fc:=nil //return nil here and default to file-search
  else
    if not(Address='') and (Address[1] in [WideChar('~'),WideChar('+')]) then
	  fc:=XxmFragmentRegistry.GetClass('BackLinks.xxm')
	else
	  if Context.ContextString(csQueryString)='' then
        fc:=XxmFragmentRegistry.GetClass('Default.xxm')
	  else
	    fc:=XxmFragmentRegistry.GetClass('Search.xxm');
  if fc=nil then Result:=nil else Result:=fc.Create(Self);
end;

function TXxmsvnwiki.LoadFragment(Address: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetClass(Address).Create(Self);
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
