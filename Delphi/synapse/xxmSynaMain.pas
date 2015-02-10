unit xxmSynaMain;

interface

uses
  SysUtils, blcksock, xxm, Classes, ActiveX, xxmContext, xxmThreadPool,
  xxmPReg, xxmPRegXml, xxmParams, xxmParUtils, xxmHeaders;

type
  TXxmSynaServer = class(TThread)
  private
    FPort:integer;
    FListening:boolean;
    FSocket:TTCPBlockSocket;
  public
    constructor Create(Port:integer);
    destructor Destroy; override;
    procedure Execute; override;
    function Listening:boolean;
  end;

type
  TXxmSynaContext=class(TXxmQueueContext, IxxmHttpHeaders)
  private
    FSocket:TTCPBlockSocket;
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

    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function SendData(const Buffer; Count: LongInt): LongInt;
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
    constructor Create(SocketHandle:THandle);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);

  TXxmSynaRunParameters=(
    rpPort,
    rpSilent,
    rpLoadCopy,
    rpStartURL,
    rpThreads,
    //add new here
    rp_Unknown);

procedure XxmRunServer;

implementation

uses Windows, Variants, ComObj, AxCtrls, WinSock,
  xxmCommonUtils, xxmReadHandler, ShellApi;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  HTTPMaxHeaderLines=$400;
  PostDataThreshold=$100000;
  DefaultRecvTimeout=250;//ms

var
  HttpSelfVersion:AnsiString;

procedure XxmRunServer;
const
  ParameterKey:array[TXxmSynaRunParameters] of AnsiString=(
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
  StartURL,s,t:AnsiString;
  Msg:TMsg;
  par:TXxmSynaRunParameters;
begin
  //defualt values
  Port:=80;
  Threads:=$100;
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

  //build HTTP version string
  i:=Length(SelfVersion);
  while (i<>0) and (SelfVersion[i]<>' ') do dec(i);
  HttpSelfVersion:=StringReplace(Copy(SelfVersion,1,i-1),' ','_',[rfReplaceAll])+
    '/'+Copy(SelfVersion,i+1,Length(SelfVersion)-i);

  //
  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCacheXml.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
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
        PageLoaderPool.Queue(TXxmSynaContext.Create(ch));
      //TODO else raise?
     end;
   end;
  //TODO: try except log?
end;

function TXxmSynaServer.Listening: boolean;
begin
  while not(Terminated) and not(FListening) do Sleep(10);
  Result:=FListening;
end;

{ TXxmSynaContext }

constructor TXxmSynaContext.Create(SocketHandle:THandle);
var
  i,l:integer;
begin
  inherited Create('');//URL is parsed by Execute
  FSocket:=TTCPBlockSocket.Create;
  FSocket.Socket:=SocketHandle;
  SendDirect:=nil;//see BeginRequest to detect AfterConstruction
  i:=1;
  l:=4;
  setsockopt(FSocket.Socket,IPPROTO_TCP,TCP_NODELAY,@i,l);
end;

destructor TXxmSynaContext.Destroy;
begin
  //see also EndRequest
  FSocket.Free;
  inherited;
end;

procedure TXxmSynaContext.BeginRequest;
begin
  inherited;
  FReqHeaders:=nil;
  if @SendDirect=nil then
   begin
    SendDirect:=SendData;
    FResHeaders:=nil;
   end
  else
   begin
    FResHeaders:=TResponseHeaders.Create;
    (FResHeaders as IUnknown)._AddRef;
   end;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FURI:='';//see Execute
  FRedirectPrefix:='';
end;

procedure TXxmSynaContext.EndRequest;
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

procedure TXxmSynaContext.Execute;
begin
  FKeepConnection:=true;
  while FKeepConnection do
   begin
    FKeepConnection:=false;
    BeginRequest;
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
      HandleRequest;
    finally
      EndRequest;
    end;
   end;
end;

procedure TXxmSynaContext.HandleRequest;
var
  i,j,l,xi:integer;
  x,y:AnsiString;
  s:TStream;
  si:int64;
begin
  try
    //command line and headers
    //command line and headers
    x:=FSocket.RecvPacket(DefaultRecvTimeout);
    y:='';
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
         end;
        inc(xi);
       end;
      if j=0 then
       begin
        //i:=1;
        while (i<=l) and (x[i]>' ') do inc(i);
        FVerb:=UpperCase(Copy(x,1,i-1));
        inc(i);
        xi:=i;
        while (xi<=l) and (x[xi]>' ') do inc(xi);
        FURI:=Copy(x,i,xi-i);
        inc(xi);
        i:=xi;
        while (xi<=l) and (x[xi]<>#13) and (x[xi]<>#10) do inc(xi);
        FHTTPVersion:=Copy(x,i,xi-i);
        inc(j);
       end
      else
       begin
        y:=y+Copy(x,i,xi-i)+#13#10;
        if i=xi then j:=-1 else
         begin
          inc(j);
          if j=HTTPMaxHeaderLines then
            raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
         end;
       end;
      inc(xi);
      if (xi<=l) and (x[xi]=#10) then inc(xi);
    until j=-1;
    x:=Copy(x,xi,l-xi+1);
    FReqHeaders:=TRequestHeaders.Create(y);
    (FReqHeaders as IUnknown)._AddRef;

    ProcessRequestHeaders;
    //if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCacheXml.Create;

    if (FURI<>'') and (FURI[1]='/') then
     begin
      i:=2;
      if XxmProjectCache.ProjectFromURI(Self,FURI,i,FProjectName,FFragmentName) then
        FRedirectPrefix:='/'+FProjectName;
      FPageClass:='['+FProjectName+']';
      FQueryStringIndex:=i;
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
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,x);
     end;

    BuildPage;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');
        try
          if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',e.ClassName,e.Message);
       end;
  end;
  PostProcessRequest;
end;

function TXxmSynaContext.GetProjectEntry:TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmSynaContext.GetProjectPage(FragmentName: WideString):IXxmFragment;
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
    csRemoteAddress:Result:=FSocket.GetRemoteSinIP;
    csRemoteHost:Result:=FSocket.ResolveIPToName(FSocket.GetRemoteSinIP);
    csAuthUser,csAuthPassword:Result:=AuthValue(cs);
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TXxmSynaContext.DispositionAttach(FileName: WideString);
begin
  FResHeaders.SetComplex('Content-disposition','attachment')
    ['filename']:=FileName;
end;

function TXxmSynaContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders['Cookie'];
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

function TXxmSynaContext.GetSessionID: WideString;
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

procedure TXxmSynaContext.Redirect(RedirectURL: WideString;
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
  if BufferSize<>0 then Flush;
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
  FResHeaders['Content-Type']:=FContentType+AutoEncodingCharset[FAutoEncoding];
  x:=FHTTPVersion+' '+IntToStr(StatusCode)+' '+StatusText+#13#10+
    FResHeaders.Build+#13#10;
  l:=Length(x);
  SetLength(d,l);
  Move(x[1],d[0],l);
  FSocket.SendBuffer(@d[0],l);
  if FResHeaders['Content-Length']<>'' then FKeepConnection:=true;
  //TODO: transfer encoding chunked
end;

function TXxmSynaContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders.Item[Name];
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
  //assert not(FReqHeaders=nil) since parsed at start of Execute
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

  FResHeaders['Server']:=HttpSelfVersion; //X-Powered-By?
  FURL:=FReqHeaders['Host'];
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting?
    p:=FSocket.GetLocalSinPort;
    if p<>80 then FURL:=FURL+':'+IntToStr(p);
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
  SetThreadName('xxm:'+FURL);
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
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
end.
