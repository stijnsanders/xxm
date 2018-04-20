unit xxmp;

{
  xxm Project Unit
  demo: 12 Long Polling

This file was first generated using the xxmp.pas template, but then modified
to add the IXxmProjectEvents2 implementation to the TXxmdemo object.
}

interface

uses xxm;

type
  TXxmdemo=class(TXxmProject, IXxmProjectEvents2)
  private
    function CheckEvent(const EventKey: WideString; var CheckIntervalMS: cardinal): boolean;
  public
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject; stdcall;

implementation

uses SysUtils, xxmFReg;

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

var
  CheckedLast:cardinal;

function TXxmdemo.CheckEvent(const EventKey: WideString; var CheckIntervalMS: cardinal): boolean;
var
  th,tm,ts,tz:word;
  i,j:cardinal;
begin
  //if EventKey='demo12'?
  DecodeTime(Now,th,tm,ts,tz);
  i:=th+tm+ts;
  if CheckedLast=i then Result:=false else
   begin
    CheckedLast:=i;
    j:=2;
    //TODO: while (j<=isqtr(j))
    while (j<i) and ((i mod j)<>0) do inc(j);
    Result:=j=i;
   end;
end;

initialization
  IsMultiThread:=true;
end.
