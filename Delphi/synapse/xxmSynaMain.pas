unit xxmSynaMain;

interface

uses
  SysUtils, blcksock, xxm, Classes, ActiveX, xxmContext, xxmThreadPool,
  xxmPReg, xxmParams, xxmParUtils, xxmHeaders, xxmSynaKept, xxmSynaSpool;

type
  TXxmSynaServer = class(TThread)
  private
    FPort:integer;
    FListening:boolean;
    FSocket:TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(Port:integer);
    destructor Destroy; override;
    function Listening:boolean;
  end;

  TXxmSynaContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmContextSuspend,
    IXxmSocketSuspend)
  private
    FSocket:TTCPBlockSocket;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FHTTPVersion,FVerb:AnsiString;
    FURI,FRedirectPrefix:WideString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    procedure Accept(SocketHandle:THandle);
  protected

    function SendData(const Buffer; Count: LongInt): LongInt;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(const RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(const Name: WideString): WideString; override;

    procedure SendHeader; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;
    procedure FlushFinal; override;
    procedure FlushStream(AData:TStream;ADataSize:int64); override;
    function GetRawSocket: IStream; override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

    { IXxmSocketSuspend }
    procedure SuspendSocket(Handler: IXxmRawSocket); override;

    {  }

    function GetProjectPage(const FragmentName: WideString):IXxmFragment; override;

    procedure ProcessRequestHeaders; virtual;
    procedure PreProcessRequest; virtual;
    procedure PreProcessRequestPage; virtual;
    procedure PostProcessRequest; virtual;

    property HTTPVersion: AnsiString read FHTTPVersion;
    property ReqHeaders:TRequestHeaders read FReqHeaders;
    property ResHeaders:TResponseHeaders read FResHeaders;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Recycle; override;

    property Socket:TTCPBlockSocket read FSocket;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

  TXxmSynaRunParameters=(
    rpPort,
    rpSilent,
    rpLoadCopy,
    rpStartURL,
    rpThreads,
    //add new here
    rp_Unknown);

var
  KeptConnections:TXxmKeptConnections;
  SpoolingConnections:TXxmSpoolingConnections;

procedure XxmRunServer;

implementation

uses Windows, Variants, ComObj, AxCtrls, WinSock, ShellApi,
  xxmCommonUtils, xxmReadHandler;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmContextAlreadySuspended='Context has already been suspended';

const
  HTTPMaxHeaderLines=$400;//1KiB
  HTTPMaxHeaderParseTimeMS=10000;
  PostDataThreshold=$100000;//1MiB
  SpoolingThreshold=$10000;//64KiB
  DefaultRecvTimeout=250;//ms

procedure XxmRunServer;
const
  ParameterKey:array[TXxmSynaRunParameters] of string=(
    'port',
    'silent',
    'loadcopy',
    'starturl',
    'threads',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server:TxxmSynaServer;
  i,j,Port,Threads:integer;
  Silent:boolean;
  StartURL,s,t:string;
  Msg:TMsg;
  par:TXxmSynaRunParameters;
begin
  //default values
  Port:=80;
  Threads:=$200;
  Silent:=false;
  StartURL:='';

  //process command line parameters
  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    j:=1;
    while (j<=Length(s)) and (s[j]<>'=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    par:=TXxmSynaRunParameters(0);
    while (par<>rp_Unknown) and (t<>ParameterKey[par]) do inc(par);
    case par of
      rpPort:
        Port:=StrToInt(Copy(s,j+1,Length(s)-j));
      rpSilent:
        Silent:=Copy(s,j+1,Length(s)-j)<>'0';
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

  {
  //build HTTP version string
  i:=Length(SelfVersion);
  while (i<>0) and (SelfVersion[i]<>' ') do dec(i);
  HttpSelfVersion:=StringReplace(Copy(SelfVersion,1,i-1),' ','_',[rfReplaceAll])+
    '/'+Copy(SelfVersion,i+1,Length(SelfVersion)-i);
  }

  //
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  XxmProjectCache:=TXxmProjectCache.Create;
  ContextPool:=TXxmContextPool.Create(TXxmSynaContext);
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
  KeptConnections:=TXxmKeptConnections.Create;
  SpoolingConnections:=TXxmSpoolingConnections.Create;
  Server:=TxxmSynaServer.Create(Port);
  try
    //TODO: listen on multiple ports

    if StartURL<>'' then
      ShellExecute(GetDesktopWindow,nil,PChar(StartURL),nil,nil,SW_NORMAL);//check result?

    if not Server.Listening then
      if Silent then exit else
        raise Exception.Create('Failed to listen on port '+IntToStr(Port));

    repeat
      if GetMessage(Msg,0,0,0) then
        if Msg.message<>WM_QUIT then
         begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
         end;
    until Msg.message=WM_QUIT;

  finally
    Server.Free;

    //first make ReleasingContexts/ReleaseProject run
    try
      FreeAndNil(XxmProjectCache);
    except
      //log?
    end;

    KeptConnections.Free;
    SpoolingConnections.Free;
  end;
end;

{ TxxmSynaServer }

constructor TXxmSynaServer.Create(Port:integer);
begin
  inherited Create(false);
  FPort:=Port;
  FListening:=false;
  FSocket:=TTCPBlockSocket.Create;
end;

destructor TXxmSynaServer.Destroy;
begin
  FSocket.Free;
  inherited;
end;

procedure TXxmSynaServer.Execute;
var
  ch:THandle;
begin
  CoInitialize(nil);
  FSocket.CreateSocket;
  FSocket.SetLinger(true,500);//?
  FSocket.Bind(cAnyHost,IntToStr(FPort));
  FSocket.Listen;
  FListening:=true;
  while not Terminated do
   begin
    if FSocket.CanRead(500) then
     begin
      ch:=FSocket.Accept;
      if FSocket.LastError=0 then
        (ContextPool.GetContext as TXxmSynaContext).Accept(ch);
      //TODO else raise?
     end;
   end;
  //TODO: try except log?
end;

function TXxmSynaServer.Listening: boolean;
begin
  while not(Terminated) and not(FListening) do
    SwitchToThread;//prevent 100% cpu
  Result:=FListening;
end;

{ TXxmSynaContext }

procedure TXxmSynaContext.AfterConstruction;
begin
  SendDirect:=SendData;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  inherited;
end;

destructor TXxmSynaContext.Destroy;
begin
  //see also EndRequest
  BufferStore.AddBuffer(FContentBuffer);
  FSocket.Free;
  FReqHeaders.Free;
  FResHeaders.Free;
  inherited;
end;

procedure TXxmSynaContext.Accept(SocketHandle:THandle);
var
  i,l:integer;
begin
  FSocket:=TTCPBlockSocket.Create;
  FSocket.Socket:=SocketHandle;
  i:=1;
  l:=4;
  setsockopt(FSocket.Socket,IPPROTO_TCP,TCP_NODELAY,@i,l);
  PageLoaderPool.Queue(Self,ctHeaderNotSent); //KeptConnections.Queue(?
end;

procedure TXxmSynaContext.BeginRequest;
begin
  inherited;
  FReqHeaders.Reset;
  FResHeaders.Reset;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FURI:='';//see Execute
  FRedirectPrefix:='';
end;

procedure TXxmSynaContext.EndRequest;
begin
  inherited;
  try
    PostProcessRequest;
  except
    //silent (log?)
  end;
  if FSocket<>nil then
    FSocket.CloseSocket;//Disconnect
end;

procedure TXxmSynaContext.FlushFinal;
begin
  //no inherited! override default behaviour:
  if (BufferSize<>0) //and (ContentBuffer.Position<>0) then
    and (FContentBuffer.Position>SpoolingThreshold) then
   begin
    CheckSendStart(true);
    SpoolingConnections.Add(Self,FContentBuffer,false);
    FContentBuffer:=nil;//since spooling will free it when done
   end
  else
    inherited;//Flush;
end;

procedure TXxmSynaContext.FlushStream(AData: TStream; ADataSize: int64);
begin
  //no inherited! override default behaviour:
  if ADataSize>SpoolingThreshold then
   begin
    AData.Seek(0,soFromEnd);//used by SpoolingConnections.Add
    CheckSendStart(true);
    SpoolingConnections.Add(Self,AData,true);
   end
  else
    inherited;
end;

procedure TXxmSynaContext.HandleRequest;
var
  i,j,l,xi:integer;
  x:AnsiString;
  s:TStream;
  si:int64;
  tc:cardinal;
  cl:string;
begin
  try

    //TODO if secure then
    {
    Sock.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle.pem';
    Sock.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_cacert.pem';
    Sock.SSL.PrivateKeyFile := ExtractFilePath(ParamStr(0)) + 's_cakey.pem';
    Sock.SSL.KeyPassword := 's_cakey';
    Sock.SSL.verifyCert := True;

    try
      if (not Sock.SSLAcceptConnection) or
         (Sock.SSL.LastError <> 0) then
      begin
        MessageDlg('Error while accepting SSL connection: ' + Sock.SSL.LastErrorDesc, mtError, [mbAbort], 0);
        Exit;
      end;
    except
      MessageDlg('Exception while accepting SSL connection', mtError, [mbAbort], 0);
      Exit;
    end;
    }

    //command line and headers
    tc:=GetTickCount;
    x:=FSocket.RecvPacket(DefaultRecvTimeout);
    l:=Length(x);
    j:=0;
    xi:=1;
    repeat
      i:=xi;
      while (xi<=l) and (x[xi]<>#13) and (x[xi]<>#10) do
       begin
        if xi=l then
         begin
          x:=x+FSocket.RecvPacket(DefaultRecvTimeout);
          l:=Length(x);
          if cardinal(GetTickCount-tc)>HTTPMaxHeaderParseTimeMS then
            raise EXxmTransferError.Create('Header Parse Timeout');
         end;
        inc(xi);
       end;
      if j=0 then
       begin
        //i:=1;
        while (i<=l) and (x[i]>' ') do inc(i);
        FVerb:=AnsiString(UpperCase(string(Copy(x,1,i-1))));
        inc(i);
        xi:=i;
        while (xi<=l) and (x[xi]>' ') do inc(xi);
        FURI:=UTF8ToWideString(Copy(x,i,xi-i));
        inc(xi);
        i:=xi;
        while (xi<=l) and (x[xi]<>#13) and (x[xi]<>#10) do inc(xi);
        FHTTPVersion:=Copy(x,i,xi-i);
        inc(j);
       end
      else
       begin
        if i=xi then j:=-1 else
         begin
          inc(j);
          if j=HTTPMaxHeaderLines then
            raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
         end;
       end;
      inc(xi);//#13
      if (xi<=l) and (x[xi]=#10) then inc(xi);
    until j=-1;
    FReqHeaders.Data:=x;
    FReqHeaders.Load(xi,l-xi+1);

    ProcessRequestHeaders;

    if (FURI<>'') and (FURI[1]='/') then
     begin
      FQueryStringIndex:=2;
      if XxmProjectCache.ProjectFromURI(Self,UTF8Encode(FURI),FQueryStringIndex,FProjectName,FFragmentName) then
        FRedirectPrefix:='/'+FProjectName;
      FPageClass:='['+FProjectName+']';
     end
    else
     begin
      ForceStatus(400,'Bad Request');
      FProjectName:='';
      FFragmentName:='';
      SendError('error','','Bad Request');
      raise EXxmPageRedirected.Create(string(FHTTPVersion)+' 400 Bad Request');
     end;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    PreProcessRequest;

    //if Verb<>'GET' then?
    cl:=FReqHeaders['Content-Length'];
    if cl<>'' then
     begin
      si:=StrToInt(cl);
      if si<PostDataThreshold then
        s:=THeapStream.Create
      else
       begin
        SetLength(FPostTempFile,$400);
        SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));
        FPostTempFile:=FPostTempFile+'xxm_'+
          IntToHex(integer(Self),8)+'_'+IntToHex(GetTickCount,8)+'.dat';
        s:=TFileStream.Create(FPostTempFile,fmCreate);
       end;
      s.Size:=si;
      s.Position:=0;
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,x);
     end;

    if FVerb='OPTIONS' then
     begin
      State:=ctHeaderOnly;
      FAutoEncoding:=aeContentDefined;//prevent Content-Type
      AddResponseHeader('Allow','OPTIONS, GET, HEAD, POST');
      AddResponseHeader('Public','OPTIONS, GET, HEAD, POST');
      AddResponseHeader('Content-Length','0');
      LoadPage;//IXxmProject.LoadPage without IXxmPage.Build
      CheckSendStart(true);
     end
    else if FVerb='TRACE' then
     begin
      SetStatus(501,'Not Implemented');
      SendStr('<h1>Not Implemented</h1>');
      Flush;
     end
    else
     begin
      if FVerb='HEAD' then
       begin
        State:=ctHeaderOnly;
        AddResponseHeader('Content-Length','0');
       end;
      BuildPage;
     end;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    //TODO: on EXxmConnectionLost do ;
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');
        SendError('error',e.ClassName,e.Message);
       end;
  end;
end;

function TXxmSynaContext.GetProjectPage(const FragmentName: WideString):IXxmFragment;
begin
  Result:=inherited GetProjectPage(FragmentName);
  PreProcessRequestPage;
end;

function TXxmSynaContext.Connected: boolean;
begin
  Result:=FSocket.CanWrite(0);
end;

function TXxmSynaContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion;
    csExtraInfo:Result:='';//???
    csVerb:Result:=WideString(FVerb);
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders['Accept'];
    csPostMimeType:Result:=FReqHeaders['Content-Type'];
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=FReqHeaders['Referer'];
    csLanguage:Result:=FReqHeaders['Accept-Language'];
    csRemoteAddress:Result:=FSocket.GetRemoteSinIP;
    csRemoteHost:Result:=FSocket.ResolveIPToName(FSocket.GetRemoteSinIP);
    csAuthUser,csAuthPassword:Result:=UTF8ToWideString(AuthValue(cs));
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

function TXxmSynaContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=UTF8Encode(FReqHeaders['Cookie']);
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=UTF8ToWideString(GetParamValue(FCookie,FCookieIdx,Name));
end;

procedure TXxmSynaContext.Redirect(const RedirectURL: WideString;
  Relative: boolean);
var
  NewURL,RedirBody:WideString;
begin
  inherited;
  SetStatus(302,'Object moved');//SetStatus(301,'Moved Permanently');
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then NewURL:=FRedirectPrefix+NewURL;
  RedirBody:='<h1>Object moved</h1><p><a href="'+HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a></p>'#13#10;
  FResHeaders['Location']:=NewURL;
  case FAutoEncoding of
    aeUtf8:FResHeaders['Content-Length']:=IntToStr(Length(UTF8Encode(RedirBody))+3);
    aeUtf16:FResHeaders['Content-Length']:=IntToStr(Length(RedirBody)*2+2);
    aeIso8859:FResHeaders['Content-Length']:=IntToStr(Length(AnsiString(RedirBody)));
  end;
  SendStr(RedirBody);
  raise EXxmPageRedirected.Create(RedirectURL);
end;

function TXxmSynaContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Count=0 then Result:=0 else
    Result:=FSocket.SendBuffer(@Buffer,Count);
end;

procedure TXxmSynaContext.SendHeader;
var
  x:AnsiString;
  l:cardinal;
  d:array of byte;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  //use FResHeader.Complex?
  if FContentType<>'' then
    FResHeaders['Content-Type']:=FContentType+
      AutoEncodingCharset[FAutoEncoding];
  x:=FHTTPVersion+' '+AnsiString(IntToStr(StatusCode))+' '+AnsiString(StatusText)+#13#10+
    FResHeaders.Build+#13#10;
  l:=Length(x);
  SetLength(d,l);
  Move(x[1],d[0],l);
  FSocket.SendBuffer(@d[0],l);
  //TODO: transfer encoding chunked
  inherited;
end;

function TXxmSynaContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders[Name];
end;

procedure TXxmSynaContext.AddResponseHeader(const Name, Value: WideString);
begin
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

function TXxmSynaContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  Result:=FReqHeaders;
end;

function TXxmSynaContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

procedure TXxmSynaContext.ProcessRequestHeaders;
var
  p:integer;
begin
  //'Authorization' ?
  //'If-Modified-Since' ? 304
  //'Connection: Keep-alive' ? with sent Content-Length

  //FResHeaders['Server']:=HttpSelfVersion; //X-Powered-By?
  FURL:=FReqHeaders['Host'];
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting?
    p:=FSocket.GetLocalSinPort;
    if p<>80 then FURL:=FURL+':'+IntToStr(p);
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
  //SetThreadName('xxm:'+FURL);
end;

procedure TXxmSynaContext.PreProcessRequest;
begin
  //inheritants can perform post-page logging here
end;

procedure TXxmSynaContext.PreProcessRequestPage;
begin
  //similar to PreProcessRequest, but right after project and fragment load
end;

procedure TXxmSynaContext.PostProcessRequest;
begin
  //inheritants can perform pre-page-build logging or checking here
  //ATTENTION: (v1.2.2) when Context.BufferSize is set,
  //this may get called with data on the buffer not sent yet
  //  see also TXxmSpoolingConnections
end;

function TXxmSynaContext.GetRawSocket: IStream;
begin
  if FReqHeaders['Upgrade']='' then Result:=nil else
   begin
    FContentType:='';
    CheckSendStart(false);
    SetBufferSize(0);
    Result:=TRawSocketData.Create(FSocket);
   end;
end;

procedure TXxmSynaContext.SuspendSocket(Handler: IXxmRawSocket);
begin
  inherited;
  KeptConnections.Queue(Self,ctSocketResume);
end;

procedure TXxmSynaContext.Recycle;
var
  i:integer;
begin
  if (State<>ctSocketDisconnect)
    and ((FResHeaders['Content-Length']<>'') or (State=ctHeaderOnly)) then
   begin
    try
      EndRequest;
    except
      //silent
    end;
    KeptConnections.Queue(Self,ctHeaderNotSent);
   end
  else
   begin
    if FSocket<>nil then
     begin
      //if FSocket.Connected then
       begin
        i:=1;
        setsockopt(FSocket.Socket,SOL_SOCKET,SO_REUSEADDR,@i,4);
        FSocket.CloseSocket;
       end;
      FreeAndNil(FSocket);
     end;
    inherited;
   end;
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
end.
