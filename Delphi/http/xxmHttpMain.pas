unit xxmHttpMain;

interface

uses
  SysUtils, Windows, xxmSock, xxmThreadPool, xxm, Classes, ActiveX,
  xxmContext, xxmPReg, xxmPRegXml, xxmParams, xxmParUtils, xxmHeaders;

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

  TXxmHttpContext=class(TXxmQueueContext, IxxmHttpHeaders)
  private
    FSocket:TTcpSocket;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FHTTPVersion,FVerb,FURI,FRedirectPrefix,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    FKeepConnection, FHeaderOnly:boolean;
    procedure HandleRequest;
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
    procedure EndRequest; override;
    procedure FlushFinal; override;
    procedure FlushStream(AData:TStream;ADataSize:int64); override;

    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

    function GetProjectPage(FragmentName: WideString):IXxmFragment; override;

    procedure ProcessRequestHeaders; virtual;
    procedure PreProcessRequest; virtual;
    procedure PreProcessRequestPage; virtual;
    procedure PostProcessRequest; virtual;

    property HTTPVersion: AnsiString read FHTTPVersion;
    property ReqHeaders:TRequestHeaders read FReqHeaders;
    property ResHeaders:TResponseHeaders read FResHeaders;
  public
    WasKept:boolean;
    KeptCount:cardinal;
    constructor Create(Socket:TTcpSocket);
    destructor Destroy; override;
    procedure Execute; override;
    procedure PostExecute;
    property Socket: TTcpSocket read FSocket;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);

  TXxmHttpRunParameters=(
    rpPort,
    rpLoadCopy,
    rpStartURL,
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
  i,j,Port,Threads:integer;
  StartURL,s,t:AnsiString;
  Msg:TMsg;
  par:TXxmHttpRunParameters;
begin
  //default values
  Port:=80;
  StartURL:='';
  Threads:=64;

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
        Port:=StrToInt(Copy(s,j+1,Length(s)-j));
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
  i:=Length(SelfVersion);
  while (i<>0) and (SelfVersion[i]<>' ') do dec(i);
  HttpSelfVersion:=StringReplace(Copy(SelfVersion,1,i-1),' ','_',[rfReplaceAll])+
    '/'+Copy(SelfVersion,i+1,Length(SelfVersion)-i);

  //
  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCacheXml.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
  Server:=TTcpServer.Create;
  Server6:=TTcpServer.Create(AF_INET6);
  try
    Server.Bind('',Port);
    //TODO: bind to multiple ports
    Server.Listen;

    if StartURL<>'' then
      ShellExecute(GetDesktopWindow,nil,PChar(StartURL),nil,nil,SW_NORMAL);//check result?

    try
      Server6.Bind('',Port);
      //TODO: bind to multiple ports
      Server6.Listen;
      Listener6:=TXxmHttpServerListener.Create(Server6);
    except
      //silent? log? raise?
      Listener6:=nil;
    end;

    Listener:=TXxmHttpServerListener.Create(Server);
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

{ TXxmHttpContext }

constructor TXxmHttpContext.Create(Socket:TTcpSocket);
var
  i:integer;
begin
  inherited Create('');//URL is parsed by Execute
  FSocket:=Socket;
  i:=30000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVTIMEO,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDTIMEO,@i,4)<>0) then
    RaiseLastOSError;
  i:=$10000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVBUF,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDBUF,@i,4)<>0) then
    RaiseLastOSError;
  WasKept:=false;
  SendDirect:=FSocket.SendBuf;
end;

destructor TXxmHttpContext.Destroy;
begin
  FSocket.Free;
  inherited;
end;

procedure TXxmHttpContext.BeginRequest;
begin
  inherited;
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FURI:='';//see Execute
  FRedirectPrefix:='';
  FHeaderOnly:=false;
  FKeepConnection:=false;
  if WasKept then
   begin
    WasKept:=false;
    _Release;
   end;
end;

procedure TXxmHttpContext.EndRequest;
begin
  inherited;
  if FReqHeaders<>nil then
   begin
    (FReqHeaders as IUnknown)._Release;
    FReqHeaders:=nil;
   end;
  if FResHeaders<>nil then
   begin
    (FResHeaders as IUnknown)._Release;
    FResHeaders:=nil;
   end;
end;

procedure TXxmHttpContext.Execute;
begin
  //while here now done by KeptConnections
  BeginRequest;
  try
    HandleRequest;
  finally
    EndRequest;
  end;
  if WasKept then
    WasKept:=false //FlushFinal did SpoolingConnections.Add
  else
    PostExecute;
end;

procedure TXxmHttpContext.FlushFinal;
begin
  //no inherited! override default behaviour:
  if (BufferSize<>0) //and (ContentBuffer.Position<>0) then
    and (ContentBuffer.Position>SpoolingThreshold) then
   begin
    CheckSendStart(true);
    WasKept:=true;//skip PostExecute in Execute
    SpoolingConnections.Add(Self,ContentBuffer);
    ContentBuffer:=nil;//since spooling will free it when done
   end
  else
    inherited;//Flush;//assert WasKept=false
end;

procedure TXxmHttpContext.FlushStream(AData: TStream; ADataSize: int64);
begin
  //no inherited! override default behaviour:
  if ADataSize>SpoolingThreshold then
   begin
    AData.Seek(0,soFromEnd);//used by SpoolingConnections.Add
    CheckSendStart(true);
    WasKept:=true;//skip PostExecute in Execute
    SpoolingConnections.Add(Self,AData);
   end
  else
    inherited;//assert WasKept=false
end;

procedure TXxmHttpContext.PostExecute;
begin
  if FKeepConnection then
    KeptConnections.Queue(Self)
  else
    FSocket.Disconnect;
end;

procedure TXxmHttpContext.HandleRequest;
var
  i,j,k,l,m,n:integer;
  x,y:AnsiString;
  s:TStream;
  si:int64;
  tc:cardinal;
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
      while (j<=l) and (x[j]<>#13) and (x[j]<>#10) do inc(j);
      if (j<=l) and ((x[j]=#13) or (x[j]=#10)) then
       begin
        if m=0 then
         begin
          //i:=1;
          while (i<=l) and (x[i]>' ') do inc(i);
          FVerb:=UpperCase(Copy(x,1,i-1));
          inc(i);
          j:=i;
          while (j<=l) and (x[j]>' ') do inc(j);
          FURI:=Copy(x,i,j-i);    
          inc(j);
          i:=j;
          while (j<=l) and (x[j]<>#13) and (x[j]<>#10) do inc(j);
          FHTTPVersion:=Copy(x,i,j-i);
          inc(m);
         end
        else
         begin
          y:=y+Copy(x,i,j-i)+#13#10;
          if i=j then m:=-1 else
           begin
            inc(m);
            if m=HTTPMaxHeaderLines then
              raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
           end;
         end;
        inc(j);
        if (j<=l) and (x[j]=#10) then inc(j);
        i:=j;
       end;
    until m=-1;
    x:=Copy(x,j,l-j+1);
    FReqHeaders:=TRequestHeaders.Create(y);
    (FReqHeaders as IUnknown)._AddRef;

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
        s:=TMemoryStream.Create
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
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,x);
     end;

    if FVerb='OPTIONS' then
     begin
      FHeaderOnly:=true;
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
      if FVerb='HEAD' then FHeaderOnly:=true;//see SendHeader
      BuildPage;
     end;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on EXxmConnectionLost do ;
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(500,'Internal Server Error');
        SendError('error',e.ClassName,e.Message);
       end;
  end;
  PostProcessRequest;
end;

function TXxmHttpContext.GetProjectEntry:TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmHttpContext.GetProjectPage(FragmentName: WideString):IXxmFragment;
begin
  Result:=inherited GetProjectPage(FragmentName);
  PreProcessRequestPage;
end;

function TXxmHttpContext.Connected: boolean;
begin
  Result:=FSocket.Connected;
end;

function TXxmHttpContext.ContextString(cs: TXxmContextString): WideString;
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

procedure TXxmHttpContext.DispositionAttach(FileName: WideString);
begin
  FResHeaders.SetComplex('Content-disposition','attachment')
    ['filename']:=FileName;
end;

function TXxmHttpContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders['Cookie'];
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

function TXxmHttpContext.GetSessionID: WideString;
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

procedure TXxmHttpContext.Redirect(RedirectURL: WideString; Relative: boolean);
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

procedure TXxmHttpContext.SendHeader;
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
  //use FResHeader.Complex?
  FResHeaders['Content-Type']:=FContentType+AutoEncodingCharset[FAutoEncoding];
  x:=FHTTPVersion+' '+IntToStr(StatusCode)+' '+StatusText+#13#10+
    FResHeaders.Build+#13#10;
  l:=Length(x);
  if FSocket.SendBuf(x[1],l)<>l then
    raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
  if FResHeaders['Content-Length']<>'' then FKeepConnection:=true;
  //TODO: transfer encoding chunked
  if FHeaderOnly then raise EXxmPageRedirected.Create(FVerb);
end;

function TXxmHttpContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders[Name];
end;

procedure TXxmHttpContext.AddResponseHeader(const Name, Value: WideString);
begin
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

function TXxmHttpContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  //assert not(FReqHeaders=nil) since parsed at start of Execute
  Result:=FReqHeaders;
end;

function TXxmHttpContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

procedure TXxmHttpContext.ProcessRequestHeaders;
begin
  //'Authorization' ?
  //'If-Modified-Since' ? 304
  //'Connection: Keep-alive' ? with sent Content-Length
  FResHeaders['Server']:=HttpSelfVersion; //X-Powered-By?
  FURL:=FReqHeaders['Host'];
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting?
    if (FSocket.Port<>0) and (FSocket.Port<>80) then
      FURL:=FURL+':'+IntToStr(FSocket.Port);
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
end;

procedure TXxmHttpContext.PreProcessRequest;
begin
  //inheritants can perform pre-page-build logging or checking here
end;

procedure TXxmHttpContext.PreProcessRequestPage;
begin
  //similar to PreProcessRequest, but right after project and fragment load
end;

procedure TXxmHttpContext.PostProcessRequest;
begin
  //inheritants can perform post-page logging here
  //ATTENTION: (v1.2.2) when Context.BufferSize is set,
  //this may get called with data on the buffer not sent yet
  //  see also TXxmSpoolingConnections
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
  closesocket(FServer.Handle);//forces WaitForConnection to return
  inherited;
end;

procedure TXxmHttpServerListener.Execute;
begin
  //inherited;
  //assert FServer.Bind called
  while not Terminated do
    try
      FServer.WaitForConnection;
      if not Terminated then
        KeptConnections.Queue( //PageLoaderPool.Queue(
          TXxmHttpContext.Create(
            FServer.Accept));
    except
      //TODO: log? display?
    end;
end;

end.
