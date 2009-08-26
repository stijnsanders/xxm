unit xxmCGIThread;

interface

uses Classes;

type
  TForwardThread=class(TThread)
  private
    FIn,FOut:THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(hIn,hOut:THandle);
  end;

implementation

uses Windows;

{ TForwardThread }

constructor TForwardThread.Create(hIn, hOut: THandle);
begin
  inherited Create(false);
  FIn:=hIn;
  FOut:=hOut;
end;

procedure TForwardThread.Execute;
var
  d:array[0..$FFF] of byte;
  l:cardinal;
  h:THandle;
begin
  try
    if DuplicateHandle(GetCurrentProcess,FOut,GetCurrentProcess,@h,0,true,
      DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
      FOut:=h;
    while not(Terminated) do
     begin
      l:=$10000;
      if not(ReadFile(FIn,d[0],$1000,l,nil) and WriteFile(FOut,d[0],l,l,nil)) then Terminate;
     end;
  finally
    PostQuitMessage(0);
  end;
end;

end.
