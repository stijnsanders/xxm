unit xxmSynaMain;

interface

uses
  SysUtils, blcksock, xxm, Classes, ActiveX, xxmContext, xxmThreadPool,
  xxmPReg, xxmHttpPReg, xxmParams, xxmParUtils, xxmHeaders;

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
    FConnected: boolean;
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
    procedure SendRaw(const Data: WideString); override;
    procedure SendStream(s: IStream); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;
    procedure SetBufferSize(ABufferSize: Integer); override;
    procedure Flush; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    procedure SendHeader; override;
    procedure AddResponseHeader(Name, Value: WideString); override;

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
  EXxmAutoBuildFailed=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageRedirected=class(Exception);

  TXxmSynaRunParameters=(
    rpPort,
    rpSilent,
    rpLoadCopy,
    rpStartURL,
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

threadvar
  ContentBuffer:TMemoryStream;

procedure XxmRunServer;
const
  ParameterKey:array[TXxmSynaRunParameters] of AnsiString=(
    'port',
    'silent',
    'loadcopy',
    'starturl',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server:TxxmSynaServer;
  i,j,Port:integer;
  Silent:boolean;
  StartURL,s,t:AnsiString;
  Msg:TMsg;
  par:TXxmSynaRunParameters;
begin
  //defualt values
  Port:=80;
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
  XxmProjectCache:=TXxmProjectCache.Create;
  PageLoaderPool:=TXxmPageLoaderPool.Create;
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
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FConnected:=true;
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
      HandleRequest;
    finally
      EndRequest;
    end;
   end;
end;

procedure TXxmSynaContext.HandleRequest;
var
  i,j,l,xi:integer;
  x,y,xx:AnsiString;
  s:TStream;
  si:int64;
begin
  try
    //command line
    xx:=FSocket.RecvPacket(DefaultRecvTimeout);
    //assert first block contains first request line
    l:=Length(xx);
    xi:=1;
    while (xi<=l) and (xx[xi]<>#13) and (xx[xi]<>#10) do inc(xi);
    j:=xi-1;
    while (j>0) and (xx[j]<>' ') do dec(j);
    FHTTPVersion:=Copy(xx,j+1,xi-j-1);
    dec(j);
    i:=0;
    while (i<xi-1) and (xx[i]<>' ') do inc(i);
    FVerb:=UpperCase(Copy(xx,1,i-1));
    inc(i);

    FURI:=Copy(xx,i,j-i+1);
    if (xi<l) and (xx[xi]=#13) and (xx[xi+1]=#10) then inc(xi);
    inc(xi);

    //headers
    x:='';
    j:=0;
    repeat
    
     i:=xi;
     while (xi<=l) and (xx[xi]<>#13) and (xx[xi]<>#10) do
      begin
       if xi=l then
        begin
         xx:=xx+FSocket.RecvPacket(DefaultRecvTimeout);
         l:=Length(x);
        end;
       inc(xi);
      end;
     y:=Copy(xx,i,xi-i);
     if (xi<l) and (xx[xi]=#13) and (xx[xi+1]=#10) then inc(xi);
     if (y<>'') and (xi=l) then
      begin
       xx:=xx+FSocket.RecvPacket(DefaultRecvTimeout);
       l:=Length(x);
      end;
     inc(xi);

     if y<>'' then
      begin
       inc(j);
       if j=HTTPMaxHeaderLines then
         raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
       x:=x+y+#13#10;
      end;
    until y='';
    FReqHeaders:=TRequestHeaders.Create(x);
    (FReqHeaders as IUnknown)._AddRef;

    ProcessRequestHeaders;

    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //TODO: RequestHeaders['Host']?
    l:=Length(FURI);
    if (FURI<>'') and (FURI[1]='/') then
     begin
      i:=2;
      if XxmProjectCache.SingleProject='' then
       begin
        while (i<=l) and not(char(FURI[i]) in ['/','?','&','$','#']) do inc(i);
        FProjectName:=Copy(FURI,2,i-2);
        if FProjectName='' then
         begin
          if (i<=l) and (FURI[i]='/') then x:='' else x:='/';
          Redirect('/'+XxmProjectCache.DefaultProject+x+Copy(FURI,i,l-i+1),true);
         end;
        FPageClass:='['+FProjectName+']';
        if (i>l) and (l>1) then Redirect(FURI+'/',true) else
          if (FURI[i]='/') then inc(i);
        FRedirectPrefix:='/'+FProjectName;
       end
      else
       begin
        FProjectName:=XxmProjectCache.SingleProject;
        FPageClass:='[SingleProject]';
       end;
      j:=i;
      while (i<=l) and not(char(FURI[i]) in ['?','&','$','#']) do inc(i);
      FFragmentName:=Copy(FURI,j,i-j);
      if (i<=l) then inc(i);
      FQueryStringIndex:=i;
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
    x:=FReqHeaders['Content-Length'];
    if x<>'' then
     begin
      si:=StrToInt(x);
      if si<PostDataThreshold then
        s:=TMemoryStream.Create
      else
       begin
        SetLength(FPostTempFile,$400);
        SetLength(FPostTempFile,GetTempPathA($400,PAnsiChar(FPostTempFile)));//TODO: setting
        FPostTempFile:=FPostTempFile+'xxm_'+IntToHex(integer(Self),8)+'_'+IntToHex(GetTickCount,8)+'.dat';
        s:=TFileStream.Create(FPostTempFile,fmCreate);
       end;
      s.Size:=si;
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,
        Copy(xx,xi,Length(xx)-xi+1));
     end;

    BuildPage;

  except
    on e:EXxmPageRedirected do
      Flush;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(500,'Internal Server Error');
        try
          if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
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
  Result:=FConnected;
  //TODO: set to false when client disconnect
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
    csReferer:Result:=FReqHeaders['Referer'];//TODO:
    csLanguage:Result:=FReqHeaders['Language'];//TODO:
    csRemoteAddress:Result:=FSocket.GetRemoteSinIP;//TODO to name?
    csRemoteHost:Result:=FSocket.ResolveIPToName(FSocket.GetRemoteSinIP);
    csAuthUser:Result:='';//TODO:
    csAuthPassword:Result:='';//TODO:
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
  //TODO: move this to execute's except?
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then NewURL:=FRedirectPrefix+NewURL;
  RedirBody:='<h1>Object moved</h1><p><a href="'+HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a></p>'#13#10;
  FResHeaders['Location']:=NewURL;
  case FAutoEncoding of
    aeUtf8:FResHeaders['Content-Length']:=IntToStr(Length(UTF8Encode(RedirBody))+3);
    aeUtf16:FResHeaders['Content-Length']:=IntToStr(Length(RedirBody)*2+2);
    aeIso8859:FResHeaders['Content-Length']:=IntToStr(Length(AnsiString(RedirBody)));
  end;
  SendRaw(RedirBody);
  if FBufferSize<>0 then Flush;
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmSynaContext.SendRaw(const Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:cardinal;
begin
  if Data<>'' then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
          if FBufferSize=0 then
            FSocket.SendBuffer(@Utf8ByteOrderMark[1],3)
          else
            ContentBuffer.Write(Utf8ByteOrderMark[1],3);
        aeUtf16:
          if FBufferSize=0 then
            FSocket.SendBuffer(@Utf16ByteOrderMark[1],2)
          else
            ContentBuffer.Write(Utf16ByteOrderMark[1],2);
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        if FBufferSize=0 then FSocket.SendBuffer(@Data[1],l) else ContentBuffer.Write(Data[1],l);
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if FBufferSize=0 then FSocket.SendBuffer(@s[1],l) else ContentBuffer.Write(s[1],l);
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if FBufferSize=0 then FSocket.SendBuffer(@s[1],l) else ContentBuffer.Write(s[1],l);
       end;
    end;
    if (FBufferSize<>0) and (ContentBuffer.Position>=FBufferSize) then Flush;
   end;
end;

procedure TXxmSynaContext.SendStream(s: IStream);
var
  os:TOleStream;
begin
  //if s.Size<>0 then
   begin
    CheckSendStart;
    if FBufferSize<>0 then Flush;
    //no autoencoding here
    os:=TOleStream.Create(s);
    try
      FSocket.SendStreamRaw(os);
    finally
      os.Free;
    end;
   end;
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

  //clear buffer just in case
  if ContentBuffer<>nil then ContentBuffer.Position:=0;
end;

procedure TXxmSynaContext.AddResponseHeader(Name, Value: WideString);
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
    FURL:='localhost';//TODO: from binding? setting;
    p:=FSocket.GetLocalSinPort;
    if p<>80 then FURL:=FURL+':'+IntToStr(p);
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
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

procedure TXxmSynaContext.Flush;
var
  i:int64;
begin
  if FBufferSize<>0 then
   begin
    i:=ContentBuffer.Position;
    if i<>0 then
     begin
      FSocket.SendBuffer(@ContentBuffer.Memory^,i);
      ContentBuffer.Position:=0;
     end;
   end;
end;

procedure TXxmSynaContext.SetBufferSize(ABufferSize: Integer);
begin
  inherited;
  if ABufferSize<>0 then
   begin
    if ContentBuffer=nil then ContentBuffer:=TMemoryStream.Create;//TODO: tmp file when large buffer
    if ContentBuffer.Position>ABufferSize then Flush;
    if ContentBuffer.Size<ABufferSize then ContentBuffer.Size:=ABufferSize;
   end;
end;

end.
