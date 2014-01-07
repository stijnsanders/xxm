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
    FKeepConnection:boolean;
    procedure HandleRequest;
  protected
    WasKept:boolean;
    KeptCount:integer;
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
    constructor Create(Socket:TTcpSocket);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of TXxmHttpContext;
    FContextIndex,FContextSize:integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmHttpContext);
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

uses Variants, ComObj, xxmCommonUtils, xxmReadHandler, ShellApi;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  HTTPMaxHeaderLines=$400;
  HTTPMaxHeaderParseTimeMS=10000;
  PostDataThreshold=$100000;

var
  HttpSelfVersion:AnsiString;
  KeptConnections:TXxmKeptConnections;

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
  Server:TTcpServer;
  Listener:TXxmHttpServerListener;
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
  try
    Server.Bind('',Port);
    //TODO: bind to multiple ports
    Server.Listen;

    if StartURL<>'' then
      ShellExecute(GetDesktopWindow,nil,PChar(StartURL),nil,nil,SW_NORMAL);//check result?

    Listener:=TXxmHttpServerListener.Create(Server);
    KeptConnections:=TXxmKeptConnections.Create;
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
      KeptConnections.Free;
    end;

  finally
    Server.Free;
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
  FKeepConnection:=true;
  //while FKeepConnection do
   begin
    FKeepConnection:=false;
    BeginRequest;
    try
      HandleRequest;
    finally
      EndRequest;
    end;
   end;
  if FKeepConnection then
    KeptConnections.Queue(Self)
  else
    FSocket.Disconnect;
end;

procedure TXxmHttpContext.HandleRequest;
var
  i,j,k,l,m:integer;
  x,y:AnsiString;
  s:TStream;
  si:int64;
  tc:cardinal;
begin
  try
    //command line and headers
    tc:=GetTickCount;
    y:='';
    k:=$10000;
    SetLength(x,k);
    l:=FSocket.ReceiveBuf(x[1],k);
    if l<=0 then raise EXxmConnectionLost.Create('Connection Lost');
    m:=0;
    j:=1;
    repeat
      i:=j;
      while (j<=l) and (x[j]<>#13) and (x[j]<>#10) do
       begin
        if j=l then
         begin
          if l=k then
           begin
            inc(k,$10000);
            SetLength(x,k);
           end;
          l:=l+FSocket.ReceiveBuf(x[l+1],k-l);
          if (l<=j) or (cardinal(GetTickCount-tc)>HTTPMaxHeaderParseTimeMS) then
            raise EXxmConnectionLost.Create('Connection Lost');
         end;
        inc(j);
       end;
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
      SendError('error',[
        'ERRORCLASS','',
        'ERROR','Bad Request',
        'CLASS','',
        'URL',HTMLEncode(FURI),
        'POSTDATA','',
        'QUERYSTRING','',
        'VERSION',SelfVersion
      ]);
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
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,x);
     end;

    BuildPage;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on EXxmConnectionLost do ;
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(500,'Internal Server Error');
        try
          if FPostData=nil then
            x:='none'
          else
            x:=IntToStr(FPostData.Size)+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',[
          'ERRORCLASS',e.ClassName,
          'ERROR',HTMLEncode(e.Message),
          'CLASS',FPageClass,
          'URL',HTMLEncode(ContextString(csURL)),
          'POSTDATA',x,
          'QUERYSTRING',HTMLEncode(ContextString(csQueryString)),
          'VERSION',SelfVersion
        ]);
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
    csAuthUser:Result:='';//TODO:
    csAuthPassword:Result:='';//TODO:
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
  Flush;
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmHttpContext.SendHeader;
var
  x:AnsiString;
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
  FSocket.SendBuf(x[1],Length(x));
  if FResHeaders['Content-Length']<>'' then FKeepConnection:=true;
  //TODO: transfer encoding chunked
end;

function TXxmHttpContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders.Item[Name];
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

{ TXxmKeptConnections }

constructor TXxmKeptConnections.Create;
begin
  inherited Create(false);
  Priority:=tpLower;//?
  FContextIndex:=0;
  FContextSize:=0;
  InitializeCriticalSection(FLock);
  FQueueEvent:=CreateEventA(nil,true,false,
    PAnsiChar('xxmHttp:KeepConnection:'+IntToHex(GetCurrentThreadId,8)));
end;

destructor TXxmKeptConnections.Destroy;
var
  i:integer;
begin
  SetEvent(FQueueEvent);//wake up thread
  Terminate;
  WaitFor;
  CloseHandle(FQueueEvent);
  DeleteCriticalSection(FLock);
  for i:=0 to FContextIndex-1 do
    if FContexts[i]<>nil then
      try
        FContexts[i]._Release;//FContexts[i].Free;
      except
        //ignore
      end;
  inherited;
end;

procedure TXxmKeptConnections.Queue(Context: TXxmHttpContext);
const
  GrowStep=$100;
var
  i:integer;
begin
  //TODO: maximum lingering connections? or close oldest on queue newer?
  EnterCriticalSection(FLock);
  try
    i:=0;
    while (i<FContextIndex) and (FContexts[i]<>nil) do inc(i);
    if i=FContextIndex then
     begin
      if FContextIndex=FContextSize then
       begin
        inc(FContextSize,GrowStep);
        SetLength(FContexts,FContextSize);
       end;
      FContexts[FContextIndex]:=Context;
      inc(FContextIndex);
     end
    else
      FContexts[i]:=Context;
    Context.KeptCount:=0;
    //protect from destruction by TXxmPageLoader.Execute:
    Context.WasKept:=true;
    Context._AddRef;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.Execute;
var
  r,x:TFDSet;
  i,ii,j,k:integer;
  t:TTimeVal;
begin
  inherited;
  i:=0;
  while not Terminated do
   begin
    EnterCriticalSection(FLock);
    try
      r.fd_count:=0;
      x.fd_count:=0;
      j:=0;
      while (j<FContextIndex) and (r.fd_count<64) do
       begin
        ii:=(i+j) mod FContextIndex;
        if FContexts[ii]<>nil then
         begin
          inc(FContexts[ii].KeptCount);
          //timed out? (see also t value below: 300x100ms~=30s)
          if FContexts[ii].KeptCount=300 then
           begin
            try
              FContexts[ii]._Release; //FContexts[ii].Free;
            except
              //ignore
            end;
            FContexts[ii]:=nil;
           end
          else
           begin
            r.fd_array[r.fd_count]:=FContexts[ii].FSocket.Handle;
            inc(r.fd_count);
            x.fd_array[x.fd_count]:=FContexts[ii].FSocket.Handle;
            inc(x.fd_count);
           end;
         end;
        inc(j);
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
    if FContextIndex=0 then i:=0 else i:=(i+j) mod FContextIndex;
    if r.fd_count=0 then
     begin
      ResetEvent(FQueueEvent);
      WaitForSingleObject(FQueueEvent,INFINITE);
     end
    else
     begin
      t.tv_sec:=0;
      t.tv_usec:=100000;//microseconds
      if select(0,@r,nil,@x,@t)=SOCKET_ERROR then
       begin
        //TODO: raise? log? sleep?
       end
      else
       begin
        EnterCriticalSection(FLock);
        try
          //errors
          for k:=0 to x.fd_count-1 do
           begin
            j:=0;
            while (j<FContextIndex) and not((FContexts[j]<>nil)
              and (FContexts[j].FSocket.Handle=x.fd_array[k])) do inc(j);
            if j<FContextIndex then
             begin
              try
                FContexts[j]._Release; //FContexts[j].Free;
              except
                //ignore
              end;
              FContexts[j]:=nil;
             end;
            //else raise?
           end;
          //reads
          for k:=0 to r.fd_count-1 do
           begin
            j:=0;
            while (j<FContextIndex) and not((FContexts[j]<>nil)
              and (FContexts[j].FSocket.Handle=r.fd_array[k])) do inc(j);
            if j<FContextIndex then
             begin
              PageLoaderPool.Queue(FContexts[j]);
              FContexts[j]:=nil;
             end;
            //else raise?
           end;
          //clean-up
          while (FContextIndex>0) and (FContexts[FContextIndex-1]=nil) do
            dec(FContextIndex);
        finally
          LeaveCriticalSection(FLock);
        end;
       end;
     end;
   end;
end;

end.
