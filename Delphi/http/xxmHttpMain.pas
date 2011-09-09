unit xxmHttpMain;

interface

uses
  SysUtils, Sockets, xxm, Classes, ActiveX, xxmContext,
  xxmPReg, xxmHttpPReg, xxmParams, xxmParUtils, xxmHeaders;

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
    FConnected:boolean;
    FHTTPVersion,FVerb,FURI,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    FKeepConnection:boolean;
    procedure HandleRequest;
  protected

    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    procedure SendRaw(Data: WideString); override;
    procedure SendStream(s: IStream); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;
    procedure SetCookie(Name: WideString; Value: WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;

    function GetProjectEntry(ProjectName: WideString):TXxmProjectEntry; override;
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

procedure XxmRunServer;

implementation

uses Windows, Variants, ComObj, AxCtrls, xxmCommonUtils, xxmReadHandler;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  HTTPMaxHeaderLines=$400;
  PostDataThreshold=$100000;

procedure XxmRunServer;
type
  TParameters=(cpPort,
  //add new here
  cp_Unknown);
const
  ParameterKey:array[TParameters] of AnsiString=(
    'port',
    //add new here (lowercase)
    '');
  WM_QUIT = $0012;//from Messages
var
  Server:TxxmHttpServer;
  i,j,Port:integer;
  s,t:AnsiString;
  Msg:TMsg;
  par:TParameters;
begin
  Port:=80;//default

  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    j:=1;
    while (j<=Length(s)) and (s[j]<>'=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    par:=TParameters(0);
    while (par<>cp_Unknown) and (t<>ParameterKey[par]) do inc(par);
    case par of
      cpPort:Port:=StrToInt(Copy(s,j+1,Length(s)-j));
      //add new here
      cp_Unknown: raise Exception.Create('Unknown setting: '+t);
    end;
   end;

  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCache.Create;
  Server:=TxxmHttpServer.Create(nil);
  try
    Server.LocalPort:=IntToStr(Port);
    //TODO: listen on multiple ports
    Server.Open;

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
    ClientSocket.Disconnect;
  end;
end;

{ TXxmHttpContext }

constructor TXxmHttpContext.Create(Socket:TCustomIpClient);
begin
  inherited Create('');//URL is parsed by Execute
  FSocket:=Socket;
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
  i,j,l:integer;
  x,y:AnsiString;
  s:TStream;
  si:int64;
begin
  try
    //command line
    x:=FSocket.Receiveln;
    l:=Length(x);
    j:=l;
    while (j>0) and (x[j]<>' ') do dec(j);
    FHTTPVersion:=Copy(x,j+1,l-j);
    dec(j);
    i:=0;
    while (i<l) and (x[i]<>' ') do inc(i);
    FVerb:=UpperCase(Copy(x,1,i-1));
    inc(i);

    FURI:=Copy(x,i,j-i+1);
    
    //headers
    i:=0;
    x:='';
    repeat
     y:=FSocket.Receiveln;
     if y<>'' then
      begin
       inc(i);
       if i=HTTPMaxHeaderLines then
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
        'URL',HTMLEncode(FURI),
        'CLASS','',
        'POSTDATA','',
        'QUERYSTRING','',
        'ERROR','Bad Request',
        'ERRORCLASS','',
        'VERSION',ContextString(csVersion)
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
      s.Size:=StrToInt(x);
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s);
     end;

    BuildPage;

  except
    on e:EXxmPageRedirected do
      ;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
     begin
      //TODO: get fragment 500.xxm?
      ForceStatus(500,'Internal Server Error');
      try
        if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
      except
        x:='unknown';
      end;
      SendError('error',[
        'URL',HTMLEncode(ContextString(csURL)),
        'CLASS',FPageClass,
        'POSTDATA',x,
        'QUERYSTRING',HTMLEncode(ContextString(csQueryString)),
        'ERROR',HTMLEncode(e.Message),
        'ERRORCLASS',e.ClassName,
        'VERSION',ContextString(csVersion)
      ]);
     end;
  end;
  PostProcessRequest;
end;

function TXxmHttpContext.GetProjectEntry(ProjectName: WideString):TXxmProjectEntry;
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
    csRemoteHost:Result:=FSocket.RemoteHost;
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

procedure TXxmHttpContext.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
  FResHeaders.Add('Set-Cookie',Name+'="'+Value+'"');
end;

procedure TXxmHttpContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:WideString;
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
  x:=Name+'="'+Value+'"';
  //'; Version=1';
  if Comment<>'' then
    x:=x+'; Comment="'+Comment+'"';
  if Domain<>'' then
    x:=x+'; Domain="'+Domain+'"';
  if Path<>'' then
    x:=x+'; Path="'+Path+'"';
  x:=x+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires="'+RFC822DateGMT(Now+KeepSeconds/86400)+'"';
  if Secure then
    x:=x+'; Secure'+#13#10;
  if HttpOnly then
    x:=x+'; HttpOnly'+#13#10;
  FResHeaders.Add('Set-Cookie',x);
  //TODO: Set-Cookie2
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
begin
  inherited;
  SetStatus(301,'Moved Permanently');
  //TODO: relative
  FResHeaders['Location']:=RedirectURL;
  //TODO: move this to execute's except?
  SendHTML('<a href="'+HTMLEncode(RedirectURL)+'">'+HTMLEncode(RedirectURL)+'</a>');
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmHttpContext.SendRaw(Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:cardinal;
  d:array of byte;
begin
  if Data<>'' then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
         begin
          l:=3;
          SetLength(d,l);
          Move(Utf8ByteOrderMark[1],d[0],l);
          FSocket.SendBuf(d[0],l);
         end;
        aeUtf16:
         begin
          l:=2;
          SetLength(d,l);
          Move(Utf16ByteOrderMark[1],d[0],l);
          FSocket.SendBuf(d[0],l);
         end;
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        SetLength(d,l);
        Move(Data[1],d[0],l);
        FSocket.SendBuf(d[0],l);
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        SetLength(d,l);
        Move(s[1],d[0],l);
        FSocket.SendBuf(d[0],l);
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        SetLength(d,l);
        Move(s[1],d[0],l);
        FSocket.SendBuf(d[0],l);
       end;
    end;
   end;
end;

procedure TXxmHttpContext.SendStream(s: IStream);
var
  os:TOleStream;
begin
  //if s.Size<>0 then
   begin
    CheckSendStart;
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
end;

procedure TXxmHttpContext.AddResponseHeader(Name, Value: WideString);
begin
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

  //data (Content-Length

  FResHeaders['Server']:=SelfVersion; //X-Powered-By?
  FURL:=FReqHeaders['Host'];
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting;
    if FSocket.LocalPort<>'80' then
      FURL:=FURL+':'+FSocket.LocalPort;
   end;
  FURL:='http://'+FURL+FURI;//TODO: 'https' if SSL?
end;

procedure TXxmHttpContext.PreProcessRequest;
begin
  //inheritants can perform post-page logging here
end;

procedure TXxmHttpContext.PreProcessRequestPage;
begin
  //similar to PreProcessRequest, but right after project and fragment load
end;

procedure TXxmHttpContext.PostProcessRequest;
begin
  //inheritants can perform pre-page-build logging or checking here
end;

end.
