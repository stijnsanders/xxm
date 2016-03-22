unit xxmHttpRun;

interface

uses SysUtils, Classes, xxmSock, xxmHttpCtx, xxmPReg;

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

  TXxmHttpRunParameters=(
    rpPort,
    rpLoadCopy,
    rpStartURL,
    rpThreads,
    //add new here
    rp_Unknown);

var
  HttpListenPort:Word;
  HttpBindIPv4,HttpBindIpV6:string;
    
procedure XxmRunServer;

implementation

uses Windows, ActiveX, ShellApi, xxmParams, xxmPRegXml,
  xxmThreadPool, xxmContext, xxmKeptCon, xxmSpoolingCon;

procedure XxmRunServer;
const
  ParameterKey:array[TXxmHttpRunParameters] of AnsiString=(
    'port',
    'loadcopy',
    'starturl',
    'threads',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server,Server6:TTcpServer;
  Listener,Listener6:TXxmHttpServerListener;
  i,j,Threads:integer;
  StartURL,s,t:AnsiString;
  Msg:TMsg;
  par:TXxmHttpRunParameters;
begin
  //default values
  StartURL:='';
  Threads:=$200;

  //process command line parameters
  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    j:=1;
    while (j<=Length(s)) and (s[j]<>'=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    par:=TXxmHttpRunParameters(0);
    while (par<>rp_Unknown) and (t<>ParameterKey[par]) do inc(par);
    case par of
      rpPort:
        HttpListenPort:=StrToInt(Copy(s,j+1,Length(s)-j));
      rpLoadCopy:
        GlobalAllowLoadCopy:=Copy(s,j+1,Length(s)-j)<>'0';
      rpStartURL:
        StartURL:=Copy(s,j+1,Length(s)-j);
      rpThreads:
        Threads:=StrToInt(Copy(s,j+1,Length(s)-j));
      //add new here
      rp_Unknown:
        raise Exception.Create('Unknown setting: '+t);
    end;
   end;

  //build HTTP version string
  {
  i:=Length(SelfVersion);
  while (i<>0) and (SelfVersion[i]<>' ') do dec(i);
  HttpSelfVersion:=StringReplace(Copy(SelfVersion,1,i-1),' ','_',[rfReplaceAll])+
    '/'+Copy(SelfVersion,i+1,Length(SelfVersion)-i);
  }

  //
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  XxmProjectCache:=TXxmProjectCacheXml.Create;
  ContextPool:=TXxmContextPool.Create(TXxmHttpContext);
  KeptConnections:=TXxmKeptConnections.Create;
  SpoolingConnections:=TXxmSpoolingConnections.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
  Server:=TTcpServer.Create;
  Server6:=TTcpServer.Create(AF_INET6);
  try
    Server.Bind('',HttpListenPort);
    //TODO: bind to multiple ports
    Server.Listen;

    if StartURL<>'' then
      ShellExecute(GetDesktopWindow,nil,PChar(StartURL),nil,nil,SW_NORMAL);//check result?

    try
      Server6.Bind('',HttpListenPort);
      //TODO: bind to multiple ports
      Server6.Listen;
      Listener6:=TXxmHttpServerListener.Create(Server6);
    except
      //silent? log? raise?
      Listener6:=nil;
    end;

    Listener:=TXxmHttpServerListener.Create(Server);
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
      Listener.Free;
      Listener6.Free;
      KeptConnections.Free;
      SpoolingConnections.Free;
    end;

  finally
    Server.Free;
    Server6.Free;
  end;
end;

{ TXxmHttpServerListener }

constructor TXxmHttpServerListener.Create(Server: TTcpServer);
begin
  inherited Create(false);
  FServer:=Server;
  //Priority:=tpNormal;?
end;

destructor TXxmHttpServerListener.Destroy;
begin
  Terminate;//FTerminated:=true;
  closesocket(FServer.Handle);//forces WaitForConnection to return
  inherited;
end;

procedure TXxmHttpServerListener.Execute;
begin
  //inherited;
  //assert FServer.Bind called
  while not Terminated do
    try
      PageLoaderPool.Queue((ContextPool.GetContext as TXxmHttpContext)
        .Accept(FServer.Accept),ctHeaderNotSent);
    except
      //TODO: log? display?
    end;
end;

initialization
  //defaults
  HttpListenPort:=80;
  HttpBindIPv4:='';
  HttpBindIpV6:='';


end.
