unit xxmHttpRun;

interface

uses SysUtils, Classes, WinSock2, xxmSock;

type
  TXxmHttpServerListener=class(TThread)
  private
    FServer:TTcpServer;
  protected
    procedure Execute; override;
  public
    constructor Create(Server:TTcpServer);
    destructor Destroy; override;
  end;

var
  HttpListenPort:Word;
  HttpBindIPv4,HttpBindIPv6:string;

procedure XxmRunServer;

implementation

uses Windows, xxmPReg, xxmThreadPool, xxmContext;

//uses Messages? only need WM_QUIT:
const
  WM_QUIT = $0012;

procedure RaiseLast(const Prefix:string);
var
  r:integer;
  e:EOSError;
begin
  r:=GetLastError;
  e:=EOSError.Create(Prefix+': '+SysErrorMessage(r));
  e.ErrorCode:=r;
  raise e at ReturnAddress;
end;

procedure XxmRunServer;
var
  i,j,l:integer;
  s,t:string;
  r:integer;
  d:TWsaData;

  Server4,Server6:TTcpServer;
  Listener4,Listener6:TXxmHttpServerListener;
  Msg:TMsg;

begin

  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    l:=Length(s);
    j:=1;
    while (j<=l) and (s[j]<>'=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    s:=Copy(s,j+1,l-j);
    if t='port' then
      HttpListenPort:=StrToInt(s)
    else
    if t='loadcopy' then
      GlobalAllowLoadCopy:=s<>'0'
    else
    if t='threads' then
      //Threads:=StrToInt(s)
    else
      raise Exception.Create('Unknown setting "'+t+'"');
   end;

  //TODO: selfversion?

  //CoInitialize(nil);//here?
  SetErrorMode(SEM_FAILCRITICALERRORS);

  r:=WSAStartup($0202,d);
  if r<>0 then raise Exception.Create('WSAStartup failed: '+IntToStr(r));

  XxmProjectRegistry:=TProjectRegistry.Create;

  //TODO:CreateIoCompletionPort()

  Server4:=TTcpServer.Create(AF_INET);
  Server6:=TTcpServer.Create(AF_INET6);
  try

    //if not NoIPv4?
    Server4.Bind('',HttpListenPort);
    Server4.Listen;

    try
      Server6.Bind('',HttpListenPort);
      //TODO: bind to multiple ports
      Server6.Listen;
      Listener6:=TXxmHttpServerListener.Create(Server6);
    except
      //silent? log? raise?
      Listener6:=nil;
    end;

    Listener4:=TXxmHttpServerListener.Create(Server4);
    try
      repeat
        if GetMessage(Msg,0,0,0) then
          if Msg.message<>WM_QUIT then
           begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
           end;
      until Msg.message=WM_QUIT;
    finally
      Listener4.Free;
      Listener6.Free;
      //KeptConnections.Free;
      //SpoolingConnections.Free;
    end;

  finally
    Server4.Free;
    Server6.Free;

    XxmProjectRegistry.Free;
  end;

end;

{ TXxmHttpServerListener }

constructor TXxmHttpServerListener.Create(Server: TTcpServer);
begin
  inherited Create(false);
  FServer:=Server;
  //
end;

destructor TXxmHttpServerListener.Destroy;
begin
  Terminate;//FTerminated:=true;
  closesocket(FServer.Handle);//forces WaitForConnection to return
  inherited;
end;

procedure TXxmHttpServerListener.Execute;
var
  s:TTcpSocket;
  i:integer;
  c:TxxmContext;
begin
  //inherited;
  //assert FServer.Bind called
  while not Terminated do
    try
      s:=FServer.Accept;

      i:=30000;//TODO: setting
      if (setsockopt(s.Handle,SOL_SOCKET,SO_RCVTIMEO,@i,4)<>0) or
         (setsockopt(s.Handle,SOL_SOCKET,SO_SNDTIMEO,@i,4)<>0) then
        ;//RaiseLastOSError;
      i:=$10000;//TODO: setting
      if (setsockopt(s.Handle,SOL_SOCKET,SO_RCVBUF,@i,4)<>0) or
         (setsockopt(s.Handle,SOL_SOCKET,SO_SNDBUF,@i,4)<>0) then
        ;//RaiseLastOSError;

      c:=ContextPool.GetContext;
      c.Bind(s);
      PageLoaderPool.Queue(c.HandleRequest);

    except
      //TODO: log? display?
    end;
end;

initialization
  //defaults
  HttpListenPort:=80;
  HttpBindIPv4:='';
  HttpBindIPv6:='';

end.
