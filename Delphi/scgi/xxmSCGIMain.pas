unit xxmSCGIMain;

interface

uses
  SysUtils, Windows, xxmSock, xxmThreadPool, xxm, Classes, ActiveX,
  xxmContext, xxmPReg, xxmPRegXml, xxmParams, xxmParUtils, xxmHeaders;

type
  TXxmSCGIServerListener=class(TThread)
  private
    FServer:TTcpServer;
  protected
    procedure Execute; override;
  public
    constructor Create(Server:TTcpServer);
    destructor Destroy; override;
  end;

  TXxmSCGIContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmContextSuspend,
    IXxmSocketSuspend)
  private
    FSocket:TTcpSocket;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FCGIValues:array of record
      Name,Value:AnsiString;
    end;
    FCGIValuesSize,FCGIValuesCount:integer;
    FHTTPVersion,FVerb,FURI,FRedirectPrefix,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    procedure Load(Socket:TTcpSocket);
    function GetCGIValue(const Name: AnsiString): AnsiString;
  protected
    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    procedure SendHeader; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure FlushFinal; override;
    procedure FlushStream(AData:TStream;ADataSize:int64); override;
    function GetRawSocket: IStream; override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

    { IXxmSocketSuspend }
    procedure SuspendSocket(Handler: IXxmRawSocket); override;

    {  }
    function GetProjectPage(FragmentName: WideString):IXxmFragment; override;

    procedure ProcessRequestHeaders; virtual;
    procedure PreProcessRequest; virtual;
    procedure PreProcessRequestPage; virtual;
    procedure PostProcessRequest; virtual;

    property HTTPVersion: AnsiString read FHTTPVersion;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Recycle; override;
    property Socket: TTcpSocket read FSocket;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

  TXxmSCGIRunParameters=(
    rpPort,
    rpLoadCopy,
    rpThreads,
    //add new here
    rp_Unknown);

procedure XxmRunServer;

implementation

uses Variants, ComObj, xxmCommonUtils, xxmReadHandler, ShellApi,
  xxmKeptCon, xxmSpoolingCon;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmContextAlreadySuspended='Context has already been suspended';

const
  HTTPMaxHeaderLines=$400;//1KiB
  HTTPMaxHeaderParseTimeMS=10000;
  PostDataThreshold=$100000;//1MiB
  SpoolingThreshold=$10000;//64KiB

var
  HttpSelfVersion:AnsiString;
  //TODO: array, spread evenly over background threads
  KeptConnections:TXxmKeptConnections;
  SpoolingConnections:TXxmSpoolingConnections;

type
  EXxmConnectionLost=class(Exception);

procedure XxmRunServer;
const
  ParameterKey:array[TXxmSCGIRunParameters] of AnsiString=(
    'port',
    'loadcopy',
    'threads',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server,Server6:TTcpServer;
  Listener,Listener6:TXxmSCGIServerListener;
  i,j,Port,Threads:integer;
  s,t:AnsiString;
  Msg:TMsg;
  par:TXxmSCGIRunParameters;
begin
  //default values
  Port:=4000;//?
  Threads:=$200;

  //process command line parameters
  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    j:=1;
    while (j<=Length(s)) and (s[j]<>'=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    par:=TXxmSCGIRunParameters(0);
    while (par<>rp_Unknown) and (t<>ParameterKey[par]) do inc(par);
    case par of
      rpPort:
        Port:=StrToInt(Copy(s,j+1,Length(s)-j));
      rpLoadCopy:
        GlobalAllowLoadCopy:=Copy(s,j+1,Length(s)-j)<>'0';
      rpThreads:
        Threads:=StrToInt(Copy(s,j+1,Length(s)-j));
      //add new here
      rp_Unknown:
        raise Exception.Create('Unknown setting: '+t);
    end;
   end;

  //build HTTP version string
  i:=Length(SelfVersion);
  while (i<>0) and (SelfVersion[i]<>' ') do dec(i);
  HttpSelfVersion:=StringReplace(Copy(SelfVersion,1,i-1),' ','_',[rfReplaceAll])+
    '/'+Copy(SelfVersion,i+1,Length(SelfVersion)-i);

  //
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  XxmProjectCache:=TXxmProjectCacheXml.Create;
  ContextPool:=TXxmContextPool.Create(TXxmSCGIContext);
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
  Server:=TTcpServer.Create;
  Server6:=TTcpServer.Create(AF_INET6);
  try
    Server.Bind('',Port);
    //TODO: bind to multiple ports
    Server.Listen;
    try
      Server6.Bind('',Port);
      //TODO: bind to multiple ports
      Server6.Listen;
      Listener6:=TXxmSCGIServerListener.Create(Server6);
    except
      //silent? log? raise?
      Listener6:=nil;
    end;

    Listener:=TXxmSCGIServerListener.Create(Server);
    KeptConnections:=TXxmKeptConnections.Create;
    SpoolingConnections:=TXxmSpoolingConnections.Create;
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

{ TXxmSCGIContext }

procedure TXxmSCGIContext.AfterConstruction;
begin
  inherited;
  SendDirect:=nil;
  FCGIValuesSize:=0;
  FCGIValuesCount:=0;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  FSocket:=nil;
end;

destructor TXxmSCGIContext.Destroy;
begin
  BufferStore.AddBuffer(FContentBuffer);
  FreeAndNil(FSocket);
  SetLength(FCGIValues,0);
  FReqHeaders.Free;
  FResHeaders.Free;
  inherited;
end;

procedure TXxmSCGIContext.Load(Socket: TTcpSocket);
var
  i:integer;
begin
  FSocket:=Socket;
  SendDirect:=FSocket.SendBuf;
  i:=30000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVTIMEO,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDTIMEO,@i,4)<>0) then
    ;//RaiseLastOSError;
  i:=$10000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVBUF,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDBUF,@i,4)<>0) then
    ;//RaiseLastOSError;
  PageLoaderPool.Queue(Self,ctHeaderNotSent);
  //KeptConnections.Queue(?
end;

procedure TXxmSCGIContext.BeginRequest;
begin
  inherited;
  FReqHeaders.Reset;
  FResHeaders.Reset;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FURI:='';//see Execute
  FRedirectPrefix:='';
  FCGIValuesCount:=0;
end;

procedure TXxmSCGIContext.Recycle;
var
  i:integer;
begin
  if FSocket<>nil then
   begin
    //if (StatusCode<400) and (FHeaderSent=XxmHeaderSent) then?
     begin
      i:=1;
      setsockopt(FSocket.Handle,SOL_SOCKET,SO_REUSEADDR,@i,4)
     end;
    FSocket.Disconnect;
    FreeAndNil(FSocket);
   end;
  inherited;
end;

procedure TXxmSCGIContext.FlushFinal;
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

procedure TXxmSCGIContext.FlushStream(AData: TStream; ADataSize: int64);
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

procedure TXxmSCGIContext.HandleRequest;
var
  i,j,k,l,m,n,o:integer;
  x,y,z:AnsiString;
  s:TStream;
  si:int64;
  tc:cardinal;
const
  CGIValuesGrowStep=$100;
begin
  try
    //command line and headers
    tc:=GetTickCount;
    y:='';
    k:=0;
    l:=0;
    m:=0;
    i:=1;
    j:=1;
    o:=0;
    repeat
      if j>l then
       begin
        if l=k then
         begin
          inc(k,$10000);
          SetLength(x,k);
         end;
        n:=FSocket.ReceiveBuf(x[l+1],k-l);
        if (n<=0) or (cardinal(GetTickCount-tc)>HTTPMaxHeaderParseTimeMS) then
          raise EXxmConnectionLost.Create('Connection Lost');
        inc(l,n);
       end;

      if m=0 then
       begin
        while (j<=l) and (x[j] in ['0'..'9']) do inc(j);
        if (j>l) or (x[j]<>':') then
         begin
          FSocket.Disconnect;
          raise EXxmTransferError.Create('Invalid SCGI header');
         end;
        //:=StrToInt(x,1,j-1)? don't enforce, use header delimiters
        inc(m);
       end;

      if (j<=l) and (x[j]=',') then
       begin
        inc(j);
        m:=-1; //done
       end
      else
       begin
        if o=0 then
         begin
          while (j<=l) and (x[j]<>#0) do inc(j);
          if (j<=l) and (x[j]=#0) then
           begin
            o:=j;
            inc(j);
           end;
         end;
        while (j<=l) and (x[j]<>#0) do inc(j);
        if (j<=l) and (x[j]=#0) and (o<>0) then
         begin
          if (o-i>4) and (Copy(x,i,5)='HTTP_') then
            y:=y+x[i+5]+LowerCase(StringReplace(
              Copy(x,i+6,o-i-6),'_','-',[rfReplaceAll]))+
              ': '+Copy(x,o+1,j-o-1)+#13#10
          else
           begin
            if FCGIValuesCount=FCGIValuesSize then
             begin
              inc(FCGIValuesSize,CGIValuesGrowStep);
              SetLength(FCGIValues,FCGIValuesSize);
             end;
            FCGIValues[FCGIValuesCount].Name:=Copy(x,i,o-i);
            FCGIValues[FCGIValuesCount].Value:=Copy(x,o+1,j-o-1);
            inc(FCGIValuesCount);
           end;
          inc(m);
          if m=HTTPMaxHeaderLines then
            raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
          inc(j);
          i:=j;
          o:=0;
         end;
       end;
    until m=-1;

    z:=Copy(x,j,l-j+1);
    FReqHeaders.Load(y);

    x:=GetCGIValue('SERVER_PROTOCOL');//http or https
    i:=1;
    l:=Length(x);
    while (i<=l) and (x[i]<>'/') do inc(i);
    y:=FReqHeaders['Host'];
    if y='' then y:='localhost';//if not port=80 then +':'+?
    FRedirectPrefix:=LowerCase(Copy(x,1,i-1))+'://'+y;

    x:='';//GetCGIValue('SCRIPT_NAME');
    y:=GetCGIValue('REQUEST_URI');
    l:=Length(x);
    if x=Copy(y,1,l) then
     begin
      FURI:=Copy(y,l+1,Length(y)-l);
      FRedirectPrefix:=FRedirectPrefix+x;
     end
    else
     begin
      FURI:=y;
      //FURLPrefix:= should be ok
     end;

    FURL:=FRedirectPrefix+FURI;

    ProcessRequestHeaders;
    //if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCacheXml.Create;

    if (FURI<>'') and (FURI[1]='/') then
     begin
      FQueryStringIndex:=2;
      if XxmProjectCache.ProjectFromURI(Self,FURI,FQueryStringIndex,FProjectName,FFragmentName) then
        FRedirectPrefix:='/'+FProjectName;
      FPageClass:='['+FProjectName+']';
     end
    else
     begin
      ForceStatus(400,'Bad Request');
      FProjectName:='';
      FFragmentName:='';
      SendError('error','','Bad Request');
      raise EXxmPageRedirected.Create(FHTTPVersion+' 400 Bad Request');
     end;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    PreProcessRequest;

    //if Verb<>'GET' then?
    y:=FReqHeaders['Content-Length'];
    if y<>'' then
     begin
      si:=StrToInt(y);
      if si<PostDataThreshold then
        s:=THeapStream.Create
      else
       begin
        SetLength(FPostTempFile,$400);
        SetLength(FPostTempFile,GetTempPathA($400,PAnsiChar(FPostTempFile)));
        FPostTempFile:=FPostTempFile+'xxm_'+
          IntToHex(integer(Self),8)+'_'+IntToHex(GetTickCount,8)+'.dat';
        s:=TFileStream.Create(FPostTempFile,fmCreate);
       end;
      s.Size:=si;
      s.Position:=0;
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,z);
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
    on EXxmConnectionLost do ;
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');
        SendError('error',e.ClassName,e.Message);
       end;
  end;
end;

function TXxmSCGIContext.GetProjectEntry:TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmSCGIContext.GetProjectPage(FragmentName: WideString):IXxmFragment;
begin
  Result:=inherited GetProjectPage(FragmentName);
  PreProcessRequestPage;
end;

function TXxmSCGIContext.Connected: boolean;
begin
  Result:=FSocket.Connected;
end;

function TXxmSCGIContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion;
    csExtraInfo:Result:='';//???
    csVerb:Result:=FVerb;
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders['Accept'];
    csPostMimeType:Result:=FReqHeaders['Content-Type'];
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=FReqHeaders['Referer'];
    csLanguage:Result:=FReqHeaders['Accept-Language'];
    csRemoteAddress:Result:=FSocket.Address;
    csRemoteHost:Result:=FSocket.HostName;
    csAuthUser,csAuthPassword:Result:=AuthValue(cs);
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TXxmSCGIContext.DispositionAttach(FileName: WideString);
begin
  FResHeaders.SetComplex('Content-disposition','attachment')
    ['filename']:=FileName;
end;

function TXxmSCGIContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders['Cookie'];
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

function TXxmSCGIContext.GetSessionID: WideString;
const
  SessionCookie='xxmSessionID';
begin
  if FSessionID='' then
   begin
    FSessionID:=GetCookie(SessionCookie);
    if FSessionID='' then
     begin
      FSessionID:=Copy(CreateClassID,2,32);
      SetCookie(SessionCookie,FSessionID);//expiry?
     end;
   end;
  Result:=FSessionID;
end;

procedure TXxmSCGIContext.Redirect(RedirectURL: WideString; Relative: boolean);
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

procedure TXxmSCGIContext.SendHeader;
var
  x:AnsiString;
  l:integer;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  inherited;
  //use FResHeader.Complex?
  if FContentType<>'' then
    FResHeaders['Content-Type']:=FContentType+
      AutoEncodingCharset[FAutoEncoding];
  x:='Status: '+IntToStr(StatusCode)+' '+StatusText+#13#10+
    FResHeaders.Build+#13#10;
  l:=Length(x);
  if FSocket.SendBuf(x[1],l)<>l then
    raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
  //TODO: transfer encoding chunked
  {
  if Next=ntHeaderOnly then
   begin
    Next:=ntKeep;//assert FResHeaders['Content-Length']='0'
    raise EXxmPageRedirected.Create(FVerb);
   end
  else
    if FResHeaders['Content-Length']<>'' then Next:=ntKeep;
  }
end;

function TXxmSCGIContext.GetCGIValue(const Name: AnsiString): AnsiString;
var
  i:integer;
begin
  i:=0;
  while (i<FCGIValuesCount) and (Name<>FCGIValues[i].Name) do inc(i); //TODO: case-insensitive?
  if i=FCGIValuesCount then Result:='' else Result:=FCGIValues[i].Value;
end;

function TXxmSCGIContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders[Name];
end;

procedure TXxmSCGIContext.AddResponseHeader(const Name, Value: WideString);
begin
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

function TXxmSCGIContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  Result:=FReqHeaders;
end;

function TXxmSCGIContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

procedure TXxmSCGIContext.ProcessRequestHeaders;
begin
  //'Authorization' ?
  //'If-Modified-Since' ? 304
  //'Connection: Keep-alive' ? with sent Content-Length
  //FResHeaders['Server']:=HttpSelfVersion; //X-Powered-By?
  FURL:=FReqHeaders['Host'];
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting?
    if (FSocket.Port<>0) and (FSocket.Port<>80) then
      FURL:=FURL+':'+IntToStr(FSocket.Port);
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
end;

procedure TXxmSCGIContext.PreProcessRequest;
begin
  //inheritants can perform pre-page-build logging or checking here
end;

procedure TXxmSCGIContext.PreProcessRequestPage;
begin
  //similar to PreProcessRequest, but right after project and fragment load
end;

procedure TXxmSCGIContext.PostProcessRequest;
begin
  //inheritants can perform post-page logging here
  //ATTENTION: (v1.2.2) when Context.BufferSize is set,
  //this may get called with data on the buffer not sent yet
  //  see also TXxmSpoolingConnections
end;

function TXxmSCGIContext.GetRawSocket: IStream;
begin
  if FReqHeaders['Upgrade']='' then Result:=nil else
   begin
    FContentType:='';
    CheckSendStart(false);
    SetBufferSize(0);//!
    Result:=TRawSocketData.Create(FSocket);
   end;
end;

procedure TXxmSCGIContext.SuspendSocket(Handler: IXxmRawSocket);
begin
  inherited;
  KeptConnections.Queue(Self,ctSocketResume);
end;

{ TXxmSCGIServerListener }

constructor TXxmSCGIServerListener.Create(Server: TTcpServer);
begin
  inherited Create(false);
  FServer:=Server;
  //Priority:=tpNormal;?
end;

destructor TXxmSCGIServerListener.Destroy;
begin
  Terminate;//FTerminated:=true;
  closesocket(FServer.Handle);//forces WaitForConnection to return
  inherited;
end;

procedure TXxmSCGIServerListener.Execute;
begin
  //inherited;
  //assert FServer.Bind called
  while not Terminated do
    try
      //TODO: limit to IP-range(s)? local IP?
      FServer.WaitForConnection;
      if not Terminated then
        (ContextPool.GetContext as TXxmSCGIContext).Load(FServer.Accept);
    except
      //TODO: log? display?
    end;
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
end.
