unit xxmp;

interface

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

uses xxmFReg, xxmHeaders;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
  //TODO: link session to request
  
  {// to require authentication on the entire site, 
   // create a file "AuthReqMsg.xxm" that contains "<h1>Authentication Required</h1>"
   // and use this code:
  
  if Context.ContextString(csAuthUser)='' then
   begin
    Context.SetStatus(401,'Authentication Required');
	(Context as IxxmHttpHeaders).ResponseHeaders['WWW-Authenticate']:='Basic realm="demo"';
	Result:=XxmFragmentRegistry.GetFragment(Self,'AuthReqMsg.xxm','');
   end
  else
  
  }
  
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
  //TODO: if Context.ContextString(csVerb)='OPTION' then...
end;

function TXxmdemo.LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
end;

procedure TXxmdemo.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //
end;

initialization
  IsMultiThread:=true;
end.
