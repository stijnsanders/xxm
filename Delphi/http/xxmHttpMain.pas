unit xxmHttpMain;

interface

uses
  SysUtils, IdCustomTCPServer, IdContext, xxm, Classes,
  xxmPReg, xxmParams, xxmParUtils;

type
  TXxmHTTPServer = class(TIdCustomTCPServer)
  protected
    procedure DoConnect(AContext: TIdContext); override;
    function DoExecute(AContext: TIdContext): Boolean; override;
  end;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TXxmHttpContext=class(TInterfacedObject, IXxmContext)
  private
    FContext:TIdContext;
    FReqHeaders:TStringList;
    FHeaderSent:boolean;
    FPage, FBuilding: IXxmFragment;
    FConnected:boolean;
    HTTPVersion,Verb,URI,FSessionID:string;
    FProjectEntry:TXxmProjectCacheEntry;
    FParams: TXxmReqPars;
    FIncludeDepth:integer;
    FStatusCode:integer;
    FStatusText,FExtraHeaders:string;
    FContentType: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FCookieParsed: boolean;
    FCookie: string;
    FCookieIdx: TParamIndexes;
    FPostData:TStream;
    FPageClass:string;
    FQueryStringIndex:integer;
    procedure HeaderOK;
    function CheckHeader: boolean;
    procedure SendRaw(Data: WideString);
    procedure SendError(res:string;vals:array of string);
  public
    constructor Create(Context:TIdContext;CommandLine:string);
    destructor Destroy; override;

    procedure Execute;

    property RequestHeaders:TStringList read FReqHeaders;

    function GetURL:WideString;
    function GetPage:IXxmFragment;
    function GetContentType:WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding:TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key:OleVariant):IXxmParameter;
    function GetParameterCount:integer;
    function GetSessionID:WideString;

    procedure Send(Data: OleVariant);
    procedure SendHTML(Data: OleVariant);
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s:TStream);
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(FileName: WideString);

    function ContextString(cs:TXxmContextString):WideString;
    function PostData:TStream;
    function Connected:boolean;

    procedure SetStatus(Code:integer;Text:WideString);
    procedure Redirect(RedirectURL:WideString; Relative:boolean);
    function GetCookie(Name:WideString):WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;

  end;

  EXxmDirectInclude=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmPageRedirected=class(Exception);

procedure XxmRunServer;

implementation

uses Windows, IdGlobal, IdSSL, IdStack, IdStackConsts, IdExceptionCore,
  Variants, ActiveX, ComObj, xxmCommonUtils, Math, xxmReadHandler;

resourcestring
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmResponseHeaderAlreadySent='Response header has already been send.';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

procedure XxmRunServer;
type
  TParameters=(cpPort,
  //add new here
  cp_Unknown);
const
  ParameterKey:array[TParameters] of string=(
    'port',
    //add new here (lowercase)
    '');

var
  Server:TXxmHTTPServer;
  i,j,Port:integer;
  s,t:string;
  Msg:TMsg;
  par:TParameters;
const
  WM_QUIT = $0012;//from Messages

begin
  Port:=80;//default

  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    j:=1;
    while (j<=Length(s)) and not(s[j]='=') do inc(j);
    t:=LowerCase(Copy(s,1,j-1));
    par:=TParameters(0);
    while not(par=cp_Unknown) and not(t=ParameterKey[par]) do inc(par);
    case par of
      cpPort:Port:=StrToInt(Copy(s,j+1,Length(s)-j));
      //add new here
      cp_Unknown: raise Exception.Create('Unknown setting: '+t);
    end;
   end;

  CoInitialize(nil);
  XxmProjectCache:=TXxmProjectCache.Create;
  Server:=TXxmHTTPServer.Create;
  try
    Server.DefaultPort:=Port;
    Server.Active:=true;
    //TODO: listen on multiple ports

    repeat
      if GetMessage(Msg,0,0,0) then
        if not(Msg.message=WM_QUIT) then
         begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
         end;
    until Msg.message=WM_QUIT;

  finally
    Server.Free;
  end;
end;

{ TxxmHTTPServer }

procedure TXxmHTTPServer.DoConnect(AContext: TIdContext);
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then begin
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough:=false;
  end;
  inherited DoConnect(AContext);
end;

function TXxmHTTPServer.DoExecute(AContext: TIdContext): Boolean;
var
  s,t:string;
  i,j:integer;
  cx:TXxmHttpContext;
const
  HTTPMaxHeaderLines=$400;
begin
  CoInitialize(nil);
  try
    try
      //command line
      cx:=TXxmHttpContext.Create(AContext,AContext.Connection.IOHandler.ReadLn);

      //headers
      i:=0;
      t:='';
      repeat
       s:=AContext.Connection.IOHandler.ReadLn;
       if i=HTTPMaxHeaderLines then raise Exception.Create('Maximum header lines exceeded.');
       if not(s='') then
        begin
         inc(i);
         j:=1;
         while (j<=Length(s)) and (s[j] in [#9,' ']) do inc(j);
         if j=1 then
          begin
           while (j<=Length(s)) and not(s[j]=':') do inc(j);
           t:=Copy(s,1,j-1);
           inc(j);
           while (j<=Length(s)) and (s[j] in [#9,' ']) do inc(j);
           cx.RequestHeaders.Values[t]:=Copy(s,j,Length(s)-j+1);
          end
         else
           cx.RequestHeaders.Values[t]:=
             cx.RequestHeaders.Values[t]+Copy(s,j,Length(s)-j+1);
        end;
      until s='';

      //'Authorization' ?
      //'If-Modified-Since' ? 304
      //'Connection: Keep-alive' ? with sent Content-Length

      //data (Content-Length

      cx._AddRef;//strange, param fill calls release
      try
        cx.Execute;
      finally
        cx._Release;
      end;

    except
      on E: EIdSocketError do begin
        if E.LastError <> Id_WSAECONNRESET then begin
          raise;
        end;
      end;
      on E: EIdClosedSocket do begin
        AContext.Connection.Disconnect;
      end;
    end;
  finally
    AContext.Connection.Disconnect(False);
  end;

  Result := False;
  if AContext <> nil then begin
    if AContext.Connection <> nil then begin
      Result := AContext.Connection.Connected;
    end;
  end;
end;

{ TXxmHttpContext }

constructor TXxmHttpContext.Create(Context:TIdContext;CommandLine:string);
var
  i,j,l:integer;
begin
  inherited Create;
  FContext:=Context;
  FProjectEntry:=nil;
  FReqHeaders:=TStringList.Create;
  FHeaderSent:=false;
  FConnected:=true;
  FParams:=nil;//see GetParameter
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FPage:=nil;
  FCookieParsed:=false;
  FStatusCode:=200;
  FStatusText:='OK';
  FExtraHeaders:='';
  FPostData:=nil;
  FIncludeDepth:=0;
  FPageClass:='';
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID

  l:=Length(CommandLine);
  j:=l;
  while (j>0) and not(CommandLine[j]=' ') do dec(j);
  HTTPVersion:=Copy(CommandLine,j+1,l-j);
  dec(j);
  i:=0;
  while (i<l) and not(CommandLine[i]=' ') do inc(i);
  Verb:=UpperCase(Copy(CommandLine,1,i-1));
  inc(i);

  URI:=Copy(CommandLine,i,j-i+1);
end;

destructor TXxmHttpContext.Destroy;
begin
  FReqHeaders.Free;
  FreeAndNil(FParams);
  FreeAndNil(FPostData);
  if not(FProjectEntry=nil) then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  inherited;
end;

procedure TXxmHttpContext.Execute;
var
  i,j,l:integer;
  ProjectName,FragmentName,x,y:string;
  p:IxxmPage;
begin
  try
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //TODO: RequestHeaders['Host']?
    l:=Length(URI);
    if not(URI='') and (URI[1]='/') then
     begin
      i:=2;
      if XxmProjectCache.SingleProject='' then
       begin
        while (i<=l) and not(URI[i] in ['/','?','&','$','#']) do inc(i);
        ProjectName:=Copy(URI,2,i-2);
        if ProjectName='' then
         begin
          if (i<=l) and (URI[i]='/') then x:='' else x:='/';
          Redirect('/'+XxmProjectCache.DefaultProject+x+Copy(URI,i,l-i+1),true);
         end;
        FPageClass:='['+ProjectName+']';
        if (i<=l) then inc(i) else if l>1 then Redirect(URI+'/',true);
       end
      else
       begin
        ProjectName:=XxmProjectCache.SingleProject;
        FPageClass:='[SingleProject]';
       end;
      j:=i;
      while (i<=l) and not(URI[i] in ['?','&','$','#']) do inc(i);
      FragmentName:=Copy(URI,j,i-j);
      if (i<=l) then inc(i);
      FQueryStringIndex:=i;
     end
    else
     begin
      FStatusCode:=400;
      FStatusText:='Bad Request';
      ProjectName:='';
      FragmentName:='';
      SendError('error',[
        'URL',HTMLEncode(URI),
        'CLASS','',
        'POSTDATA','',
        'QUERYSTRING','',
        'ERROR','Bad Request',
        'ERRORCLASS','',
        'VERSION',ContextString(csVersion)
      ]);
      raise EXxmPageRedirected.Create(HTTPVersion+' 400 Bad Request');
     end;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    //if not(Verb='GET') then?
    x:=FReqHeaders.Values['Content-Length'];
    if not(x='') then FPostData:=THandlerReadStreamAdapter.Create(
      FContext.Connection.IOHandler,StrToInt(x));

    FProjectEntry:=XxmProjectCache.GetProject(ProjectName);
    if not(@XxmAutoBuildHandler=nil) then
      if not(XxmAutoBuildHandler(FProjectEntry,Self,ProjectName)) then
        raise EXxmAutoBuildFailed.Create(ProjectName);
    FProjectEntry.OpenContext;
    FPage:=FProjectEntry.Project.LoadPage(Self,FragmentName);

    if FPage=nil then
     begin
      //find a file
      //ask project to translate? project should have given a fragment!
      FPageClass:='['+ProjectName+']GetFilePath';
      FProjectEntry.GetFilePath(FragmentName,x,y);
      if FileExists(x) then
       begin
        //TODO: Last Modified
        //TODO: if directory file-list?
        FContentType:=y;
        SendFile(x);
       end
      else
       begin
        FPageClass:='['+ProjectName+']404:'+FragmentName;
        FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
        if FPage=nil then
          SendError('fnf',[
            'URL',HTMLEncode(ContextString(csURL)),
            'PROJECT',ProjectName,
            'ADDRESS',FragmentName,
            'PATH',HTMLEncode(x),
            'VERSION',ContextString(csVersion)
          ])
        else
          try
            FPageClass:=FPage.ClassNameEx;
            FBuilding:=FPage;
            FPage.Build(Self,nil,[FragmentName,x,y],[]);//any parameters?
          finally
            FBuilding:=nil;
            //let project free, cache or recycle
            FProjectEntry.Project.UnloadFragment(FPage);
            FPage:=nil;
          end;
       end;
     end
    else
      try
        FPageClass:=FPage.ClassNameEx;
        //mime type moved to CheckSendStart;
        //OleCheck(ProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,));
        //TODO: cache output?

        //TODO: setting?
        if not(FPage.QueryInterface(IID_IXxmPage,p)=S_OK) then
          raise EXxmDirectInclude.Create(SXxmDirectInclude);
        p:=nil;

        //build page
        FBuilding:=FPage;
        FPage.Build(Self,nil,[],[]);//any parameters?

      finally
        FBuilding:=nil;
        //let project decide to free or not
        FProjectEntry.Project.UnloadFragment(FPage);
        FPage:=nil;
      end;

  except
    on e:EXxmPageRedirected do
      ;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
     begin
      //TODO: get fragment 500.xxm?
      FStatusCode:=500;//TODO:setting?
      FStatusText:='Internal Server Error';
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
    csVerb:Result:=Verb;
    csQueryString:Result:=Copy(URI,FQueryStringIndex,Length(URI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders.Values['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders.Values['Accept'];//TODO:
    csPostMimeType:Result:=FReqHeaders.Values['Post-Mime'];//TODO:
    csURL:Result:=GetURL;
    csReferer:Result:=FReqHeaders.Values['Referer'];//TODO:
    csLanguage:Result:=FReqHeaders.Values['Language'];//TODO:
    csRemoteAddress:Result:=FContext.Connection.Socket.BoundIP;
    csRemoteHost:Result:=FContext.Connection.Socket.BoundIP;//TODO: resolve
    csAuthUser:Result:='';//TODO:
    csAuthPassword:Result:='';//TODO:
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TXxmHttpContext.DispositionAttach(FileName: WideString);
begin
  //TODO: dispositionattach
end;

procedure TXxmHttpContext.HeaderOK;
begin
  if FHeaderSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

function TXxmHttpContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

procedure TXxmHttpContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  HeaderOK;
  FAutoEncoding:=Value;
end;

function TXxmHttpContext.GetContentType: WideString;
begin
  Result:=FContentType;
end;

procedure TXxmHttpContext.SetContentType(const Value: WideString);
begin
  HeaderOK;
  FContentType:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

function TXxmHttpContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=';'+FReqHeaders.Values['Cookie'];
    SplitHeaderValue(FCookie,FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmHttpContext.SetCookie(Name, Value: WideString);
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FExtraHeaders:=FExtraHeaders+'Cache-Control: no-cache="set-cookie"'#13#10;//only once!
  FExtraHeaders:=FExtraHeaders+'Set-Cookie: '+Name+'="'+Value+'"'+#13#10;
end;

procedure TXxmHttpContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FExtraHeaders:=FExtraHeaders+'Cache-Control: no-cache="set-cookie"'#13#10;//only once!
  FExtraHeaders:=FExtraHeaders+'Set-Cookie: '+Name+'="'+Value+'"';
  //'; Version=1';
  if not(Comment='') then
    FExtraHeaders:=FExtraHeaders+'; Comment="'+Comment+'"';
  if not(Domain='') then
    FExtraHeaders:=FExtraHeaders+'; Domain="'+Domain+'"';
  if not(Path='') then
    FExtraHeaders:=FExtraHeaders+'; Path="'+Path+'"';
  FExtraHeaders:=FExtraHeaders+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires="'+RFC822DateGMT(Now+KeepSeconds/86400)+'"';
  if Secure then
    FExtraHeaders:=FExtraHeaders+'; Secure'+#13#10;
  if HttpOnly then
    FExtraHeaders:=FExtraHeaders+'; HttpOnly'+#13#10;
  FExtraHeaders:=FExtraHeaders+#13#10;
  //TODO: Set-Cookie2
end;

function TXxmHttpContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TXxmHttpContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  if VarIsNumeric(Key) then Result:=FParams.GetItem(Key) else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TXxmHttpContext.GetParameterCount: integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  Result:=FParams.Count;
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

function TXxmHttpContext.GetURL: WideString;
var
  s:string;
begin
  Result:='http://';//TODO: get from port? ssl?
  s:=FReqHeaders.Values['Host'];
  if s='' then
   begin
    s:='localhost';//TODO: from binding? setting;
    if not(FContext.Connection.Socket.Binding.Port=80) then
      s:=s+':'+IntToStr(FContext.Connection.Socket.Binding.Port);
   end;
  Result:=Result+s+URI;
end;

procedure TXxmHttpContext.SetStatus(Code: integer; Text: WideString);
begin
  HeaderOK;
  FStatusCode:=Code;
  FStatusText:=Text;
  //StatusSet:=true;
end;

procedure TXxmHttpContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TXxmHttpContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TXxmHttpContext.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
var
  f,fb:IXxmFragment;
  pc:string;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  //FPage.Project??
  f:=FProjectEntry.Project.LoadFragment(Address);
  if f=nil then
    raise EXxmIncludeFragmentNotFound.Create(StringReplace(
      SXxmIncludeFragmentNotFound,'__',Address,[]));
  fb:=FBuilding;
  pc:=FPageClass;
  FBuilding:=f;
  inc(FIncludeDepth);
  try
    FPageClass:=f.ClassNameEx;
    f.Build(Self,fb,Values,Objects);//queue to avoid building up stack?
  finally
    dec(FIncludeDepth);
    FBuilding:=fb;
    FPageClass:=pc;
    fb:=nil;
    FProjectEntry.Project.UnloadFragment(f);
    f:=nil;
  end;
end;

function TXxmHttpContext.PostData: TStream;
begin
  Result:=FPostData;
end;

procedure TXxmHttpContext.Redirect(RedirectURL: WideString;
  Relative: boolean);
begin
  inherited;
  HeaderOK;
  FStatusCode:=301;
  FStatusText:='Moved Permanently';
  //TODO: relative
  FExtraHeaders:=FExtraHeaders+'Location: '+RedirectURL+#13#10;
  //TODO: move this to execute's except?
  SendHTML('<a href="'+HTMLEncode(RedirectURL)+'">'+HTMLEncode(RedirectURL)+'</a>');
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmHttpContext.Send(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(HTMLEncode(Data));
end;

procedure TXxmHttpContext.SendHTML(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(Data);
end;

procedure TXxmHttpContext.SendFile(FilePath: WideString);
var
  f:TFileStream;
begin
  inherited;
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone);
  try
    SendStream(f);
  finally
    f.Free;
  end;
end;

procedure TXxmHttpContext.SendRaw(Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:string;
  l:cardinal;
  d:TIdBytes;
begin
  if not(Data='') then
   begin
    if CheckHeader then
      case FAutoEncoding of
        aeUtf8:
         begin
          l:=3;
          SetLength(d,l);
          Move(Utf8ByteOrderMark[1],d[0],l);
          FContext.Connection.IOHandler.WriteDirect(d);
         end;
        aeUtf16:
         begin
          l:=2;
          SetLength(d,l);
          Move(Utf16ByteOrderMark[1],d[0],l);
          FContext.Connection.IOHandler.WriteDirect(d);
         end;
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        SetLength(d,l);
        Move(Data[1],d[0],l);
        FContext.Connection.IOHandler.WriteDirect(d);
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        SetLength(d,l);
        Move(s[1],d[0],l);
        FContext.Connection.IOHandler.WriteDirect(d);
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        SetLength(d,l);
        Move(s[1],d[0],l);
        FContext.Connection.IOHandler.WriteDirect(d);
       end;
    end;
   end;
end;

procedure TXxmHttpContext.SendStream(s: TStream);
begin
  FExtraHeaders:=FExtraHeaders+'Content-Length: '+IntToStr(s.Size)+
    #13#10'Accept-Ranges: bytes'#13#10;
  //TODO: keep-connection since content-length known?
  //if not(s.Size=0) then
   begin
    CheckHeader;
    //no autoencoding here
    FContext.Connection.IOHandler.Write(s);
   end;
end;

function TXxmHttpContext.CheckHeader:boolean;
var
  t:string;
  l:cardinal;
  d:TIdBytes;
begin
  Result:=not(FHeaderSent);
  if Result then
   begin
    t:=HTTPVersion+' '+IntToStr(FStatusCode)+' '+FStatusText+
      #13#10'X-Powered-By: '+SelfVersion+#13#10;
    //TODO: Content-Length?
    //TODO: Connection keep?
    //TODO: additional headers?
    case FAutoEncoding of
      aeUtf8:   t:=t+'Content-Type: '+FContentType+'; charset="utf-8"'#13#10;
      aeUtf16:  t:=t+'Content-Type: '+FContentType+'; charset="utf-16"'#13#10;
      aeIso8859:t:=t+'Content-Type: '+FContentType+'; charset="iso-8859-15"'#13#10;
      else      t:=t+'Content-Type: '+FContentType+#13#10;
    end;
    //assert FExtraHeaders blank or ends with #13#10
    t:=t+FExtraHeaders+#13#10;
    l:=Length(t);
    SetLength(d,l);
    Move(t[1],d[0],l);
    FContext.Connection.IOHandler.WriteDirect(d);
    FHeaderSent:=true;
   end;
end;

procedure TXxmHttpContext.SendError(res: string; vals: array of string);
var
  s:string;
  i:integer;
  r:TResourceStream;
  l:Int64;
const
  RT_HTML = MakeIntResource(23);
begin
  r:=TResourceStream.Create(HInstance,res,RT_HTML);
  try
    l:=r.Size;
    SetLength(s,l);
    r.Read(s[1],l);
  finally
    r.Free;
  end;
  for i:=0 to (Length(vals) div 2)-1 do
    s:=StringReplace(s,'[['+vals[i*2]+']]',vals[i*2+1],[rfReplaceAll]);
  if not(FHeaderSent) then
   begin
    FContentType:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

end.
