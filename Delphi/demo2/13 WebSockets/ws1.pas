unit ws1;

interface

uses xxmWebSocket, Classes;

type
  TMyWebSocket=class(TXxmWebSocket)
  private
    FThread:TThread;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ReceiveText(const Data:UTF8String); override;
  end;

  //only one single thread and websockets register to the thread
  TDemoThread=class(TThread)
  protected
    procedure Execute; override;
  public
    MyWebSocket: TMyWebSocket; //TODO: proper private var set by custom constructor
  end;

implementation

uses SysUtils, Windows, xxmFReg, xxmp;

{ TMyWebSocket }

procedure TMyWebSocket.AfterConstruction;
begin
  FThread:=TDemoThread.Create(false);
  (FThread as TDemoThread).MyWebSocket:=Self;//TODO: via parameter or constructor
end;

procedure TMyWebSocket.BeforeDestruction;
begin
  FreeAndNil(FThread);
end;

procedure TMyWebSocket.ReceiveText(const Data: UTF8String);
var
  x: UTF8String;
begin
  x:='Received at '+FormatDateTime('hh:nn:ss.zzz',Now)+': "'+Data+'".';
  //TODO: don't sleep here, message to central 'processing' thread+queue
  Sleep(1000);
  SendText(x);
end;

{ TDemoThread }

procedure TDemoThread.Execute;
var
  th,tm,ts,tz:word;
  i,j:cardinal;
begin
  Sleep(2000);
  while not Terminated do
   begin
    DecodeTime(Now,th,tm,ts,tz);
    i:=th+tm+ts;
    j:=2;
    //TODO: while (j<=isqtr(j))
    while (j<i) and ((i mod j)<>0) do inc(j);
    if j=i then
      MyWebSocket.SendText(Format('%d + %d + %d = %d is prime',[th,tm,ts,th+tm+ts]));
    Sleep(1000);
    if ClosingDown then
     begin
      MyWebSocket.SendText('Service shutting down...');
      MyWebSocket.Disconnect;
     end;
   end;
end;

initialization
  XxmFragmentRegistry.RegisterClass('ws1',TMyWebSocket);

end.
