unit xxmHttpMain;

interface

uses
  SysUtils, Sockets, xxm, Classes, ActiveX, xxmContext,
  xxmPReg, xxmPRegXml, xxmParams, xxmParUtils, xxmHeaders;

type
  TXxmHttpServer = class(TCustomTcpServer)
  protected
    procedure DoAccept(ClientSocket: TCustomIpClient); override;
    //procedure DoHandleError; override;//?
  end;

type
  TXxmHttpContext=class(TXxmGeneralContext, IxxmHttpHeaders)
  private
    FSocket:TCustomIpClient;
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
    constructor Create(Socket:TCustomIpClient);
    destructor Destroy; override;

    procedure Execute;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageRedirected=class(Exception);

  TXxmHttpRunParameters=(
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

var
  HttpSelfVersion:AnsiString;

threadvar
  ContentBuffer:TMemoryStream;

procedure XxmRunServer;
const
  ParameterKey:array[TXxmHttpRunParameters] of AnsiString=(
    'port',
    'silent',
    'loadcopy',
    'starturl',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server:TxxmHttpServer;
  i,j,Port:integer;
  Silent:boolean;
  StartURL,s,t:AnsiString;
  Msg:TMsg;
  par:TXxmHttpRunParameters;
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
    par:=TXxmHttpRunParameters(0);
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
  Server:=TxxmHttpServer.Create(nil);
  try
    Server.LocalPort:=IntToStr(Port);
    //TODO: listen on multiple ports
    Server.Open;

    if StartURL<>'' then
      ShellExecute(GetDesktopWindow,nil,PChar(StartURL),nil,nil,SW_NORMAL);//check result?

    if not Server.Listening then
      if Silent then exit else
        raise Exception.Create('Failed to listen on port '+Server.LocalPort);

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

{ TxxmHttpServer }

procedure TXxmHttpServer.DoAccept(ClientSocket: TCustomIpClient);
var
  cx:TXxmHttpContext;
begin
  inherited;
  CoInitialize(nil);
  try
    cx:=TXxmHttpContext.Create(ClientSocket);
    cx._AddRef;//strange, param fill calls release
    try
      cx.Execute;
    finally
      cx._Release;
    end;
  finally
    Sleep(10);//odd, on really small content disconnect comes too fast
    ClientSocket.Disconnect;
  end;
end;

{ TXxmHttpContext }

constructor TXxmHttpContext.Create(Socket:TCustomIpClient);
var
  i,l:integer;
begin
  inherited Create('');//URL is parsed by Execute
  FSocket:=Socket;
  i:=1;
  l:=4;
  setsockopt(FSocket.Handle,IPPROTO_TCP,TCP_NODELAY,@i,l);
end;

destructor TXxmHttpContext.Destroy;
begin
  //nothing here, see EndRequest
  inherited;
end;

procedure TXxmHttpContext.BeginRequest;
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

procedure TXxmHttpContext.HandleRequest;
var
  i,j,k,l,m:integer;
  x,y:AnsiString;
  s:TStream;
  si:int64;
begin
  try
    //command line
    k:=$10000;
    SetLength(x,k);
    l:=FSocket.ReceiveBuf(x[1],k);
    if l=-1 then RaiseLastOSError;
    while l=0 do
     begin
      //TODO: keep all 'keep-alive' connections on a single listener thread
      Sleep(25);
      l:=FSocket.ReceiveBuf(x[1],k);
      if l=-1 then RaiseLastOSError;
     end;
    i:=1;
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
    inc(j);
    if (j<=l) and (x[j]=#10) then inc(j);

    //headers
    y:='';
    m:=0;
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
          if l=j-1 then RaiseLastOSError;
          if (l=j) then
           begin
            Sleep(25);//odd, data takes some time to get in
            dec(j);
           end;
         end;
        inc(j);
       end;
      y:=y+Copy(x,i,j-i)+#13#10;
      if i=j then m:=0 else
       begin
        inc(m);
        if m=HTTPMaxHeaderLines then
          raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
       end;
      inc(j);
      if (j<=l) and (x[j]=#10) then inc(j);
    until m=0;
    x:=Copy(x,j,l-j+1);
    FReqHeaders:=TRequestHeaders.Create(y);
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
    y:=FReqHeaders['Content-Length'];
    if y<>'' then
     begin
      si:=StrToInt(y);
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
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,x);
     end;

    BuildPage;

  except
    on EXxmPageRedirected do
      Flush;
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
  Result:=FConnected;
  //TODO: set to false when client disconnect
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
    csReferer:Result:=FReqHeaders['Referer'];//TODO:
    csLanguage:Result:=FReqHeaders['Language'];//TODO:
    csRemoteAddress:Result:=FSocket.RemoteHost;//TODO: name to address?
    csRemoteHost:Result:=FSocket.LookupHostName(FSocket.RemoteHost);
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

procedure TXxmHttpContext.Redirect(RedirectURL: WideString;
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

procedure TXxmHttpContext.SendRaw(const Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:cardinal;
  p:pointer;
begin
  if Data<>'' then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
          if FBufferSize=0 then
            FSocket.SendBuf(PAnsiChar(Utf8ByteOrderMark)^,3)
          else
            ContentBuffer.Write(Utf8ByteOrderMark[1],3);
        aeUtf16:
          if FBufferSize=0 then
            FSocket.SendBuf(PAnsiChar(Utf16ByteOrderMark)^,2)
          else
            ContentBuffer.Write(Utf16ByteOrderMark[1],2);
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        p:=@Data[1];
        if FBufferSize=0 then FSocket.SendBuf(p^,l) else ContentBuffer.Write(Data[1],l);
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if FBufferSize=0 then FSocket.SendBuf(s[1],l) else ContentBuffer.Write(s[1],l);
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if FBufferSize=0 then FSocket.SendBuf(s[1],l) else ContentBuffer.Write(s[1],l);
       end;
    end;
    if (FBufferSize<>0) and (ContentBuffer.Position>=FBufferSize) then Flush;
   end;
end;

procedure TXxmHttpContext.SendStream(s: IStream);
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
      FSocket.SendStream(os);
    finally
      os.Free;
    end;
   end;
end;

procedure TXxmHttpContext.SendHeader;
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
  FSocket.SendBuf(d[0],l);
  if FResHeaders['Content-Length']<>'' then FKeepConnection:=true;
  //TODO: transfer encoding chunked

  //clear buffer just in case
  if ContentBuffer<>nil then ContentBuffer.Position:=0;
end;

procedure TXxmHttpContext.AddResponseHeader(Name, Value: WideString);
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
    FURL:='localhost';//TODO: from binding? setting;
    if (FSocket.LocalPort<>'') and (FSocket.LocalPort<>'80') then
      FURL:=FURL+':'+FSocket.LocalPort;
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

procedure TXxmHttpContext.Flush;
var
  i:int64;
begin
  if FBufferSize<>0 then
   begin
    i:=ContentBuffer.Position;
    if i<>0 then
     begin
      FSocket.SendBuf(ContentBuffer.Memory^,i);
      ContentBuffer.Position:=0;
     end;
   end;
end;

procedure TXxmHttpContext.SetBufferSize(ABufferSize: Integer);
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
