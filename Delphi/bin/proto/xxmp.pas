unit xxmp;

{
  xxm Project

This is a default xxm Project class inheriting from TXxmProject. You are free to change this one for your project.
Use LoadPage to process URL's as a requests is about to start.
(Be carefull with sending content from here though.)
It is advised to link each request to a session here, if you want session management.
(See  an example xxmSession.pas in the public folder.)
Use LoadFragment to handle calls made to Context.Include.

  $Rev: 457 $ $Date: 2018-07-12 22:39:00 +0200 (do, 12 jul 2018) $
}

interface

uses xxm;

type
  TXxm[[ProjectName]]=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject;
begin
  Result:=TXxm[[ProjectName]].Create(AProjectName);
end;

type
  TRespondNotImplemented=class(TXxmPage)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

{ TXxm[[ProjectName]] }

function TXxm[[ProjectName]].LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
var
  verb:WideString;
begin
  inherited;
  Context.BufferSize:=$10000;

  //TODO: link session to request
  //  see demo project "02 Session"
  //SetSession(Context);
  
  verb:=Context.ContextString(csVerb);
  if (verb='OPTIONS') or (verb='TRACE') then
    Result:=TRespondNotImplemented.Create(Self)
  else
    Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
end;

function TXxm[[ProjectName]].LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
end;

procedure TXxm[[ProjectName]].UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

{ TRespondNotImplemented }

procedure TRespondNotImplemented.Build(const Context: IXxmContext; const Caller: IXxmFragment;
  const Values: array of OleVariant; const Objects: array of TObject);
begin
  inherited;
  Context.SetStatus(501,'Not Implemented');
end;

initialization
  IsMultiThread:=true;
end.
