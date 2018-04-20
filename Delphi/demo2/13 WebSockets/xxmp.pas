unit xxmp;

{
  xxm Project

This is a default xxm Project class inheriting from TXxmProject. You are free to change this one for your project.
Use LoadPage to process URL's as a requests is about to start.
(Be carefull with sending content from here though.)
It is advised to link each request to a session here, if you want session management.
(See  an example xxmSession.pas in the public folder.)
Use LoadFragment to handle calls made to Context.Include.

  $Rev: 331 $ $Date: 2014-06-20 23:12:52 +0200 (vr, 20 jun 2014) $
}

interface

uses xxm;

type
  TXxmdemo=class(TXxmProject, IXxmProjectEvents1)
  protected
    function HandleException(Context: IXxmContext; const PageClass,
      ExceptionClass, ExceptionMessage: WideString): boolean;
    procedure ReleasingContexts;
    procedure ReleasingProject;
  public
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject; stdcall;

var
  ClosingDown:boolean;

implementation

uses xxmFReg;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
begin
  inherited;
  //TODO: link session to request
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
  //TODO: if Context.ContextString(csVerb)='OPTION' then...
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

function TXxmdemo.HandleException(Context: IXxmContext; const PageClass,
  ExceptionClass, ExceptionMessage: WideString): boolean;
begin
  Result:=false;
end;

procedure TXxmdemo.ReleasingContexts;
begin
  //cause current WebSockets to close, see ws1: TDemoThread.Execute
  ClosingDown:=true;
end;

procedure TXxmdemo.ReleasingProject;
begin
  //
end;

initialization
  IsMultiThread:=true;
  ClosingDown:=false;
end.
