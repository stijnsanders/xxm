unit xxmLoader;

interface

uses Windows, SysUtils, ActiveX, UrlMon, Classes, xxm, xxmContext,
  xxmPReg, xxmPRegLocal, xxmParams, xxmParUtils, xxmHeaders, xxmThreadPool;

type
  TXxmLocalContext=class(TXxmQueueContext,
    IXxmHttpHeaders)
  private
    ProtSink:IInternetProtocolSink;
    BindInfo:IInternetBindInfo;
    FTotalSize:cardinal;
    FVerb,FExtraInfo,FQueryString,FAuthUsr,FAuthPwd:WideString;
    FLock:TRTLCriticalSection;
    FGotSessionID:boolean;
    FReqHeaders: TRequestHeaders;
    FResHeaders: TResponseHeaders;
    FHttpNegotiate: IHttpNegotiate;
    FResumeFragment,FDropFragment:WideString;
    FResumeValue,FDropValue:OleVariant;
    function StgMediumAsStream(stgmed:TStgMedium):TStream;
    function LocaleLanguage: WideString;
    procedure CheckReqHeaders;
  protected
    procedure SendHeader; override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function Connected: Boolean; override;
    procedure Redirect(const RedirectURL: WideString; Relative:boolean); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function GetSessionID: WideString; override;
    procedure DispositionAttach(const FileName: WideString); override;
    function GetCookie(const Name: WideString): WideString; override;
    procedure SetCookie(const Name,Value:WideString); overload; override;
    procedure SetCookie(const Name,Value:WideString; KeepSeconds:cardinal;
      const Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;
    function GetProjectEntry:TXxmProjectEntry; override;
    function GetProjectPage(const FragmentName: WideString):IXxmFragment; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    OutputData:TStream;
    OutputSize:Int64;
    DataRead,Terminated:boolean;
    Loader:TXxmPageLoader;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Load(const URL:WideString;
      OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
    procedure Recycle;override;

    procedure Lock;
    procedure Unlock;

    property Verb: WideString read FVerb;
  end;

  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageLoadAborted=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

var
  //see xxmSettings
  DefaultProjectName:WideString;

implementation

uses Variants, ComObj, AxCtrls, xxmCommonUtils, xxmAuth;

const //resourcestring?
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmPageLoadAborted='Page Load Aborted';
  SXxmContextAlreadySuspended='Context has already been suspended';

{ TXxmLocalContext }

procedure TXxmLocalContext.AfterConstruction;
begin
  Terminated:=false;
  OutputData:=nil;
  OutputSize:=0;
  SendDirect:=SendData;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  InitializeCriticalSection(FLock);
  inherited;//here because calls BeginRequest;
end;

destructor TXxmLocalContext.Destroy;
begin
  FHttpNegotiate:=nil;
  BindInfo:=nil;
  ProtSink:=nil;
  FReqHeaders.Free;
  FResHeaders.Free;
  FreeAndNil(OutputData);
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmLocalContext.Load(const URL:WideString;
  OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
begin
  FURL:=URL;
  BindInfo:=OIBindInfo;
  ProtSink:=OIProtSink;
end;

procedure TXxmLocalContext.BeginRequest;
begin
  inherited;
  Terminated:=false;
  DataRead:=true;
  FReqHeaders.Reset;
  FResHeaders.Reset;
  FTotalSize:=0;
  FHttpNegotiate:=nil;
  FGotSessionID:=false;
  FAuthUsr:='';
  FAuthPwd:='';
  FResumeFragment:='';
  FResumeValue:=Null;
  FDropFragment:='';
  FDropValue:=Null;
end;

procedure TXxmLocalContext.EndRequest;
begin
  if not Terminated then
   begin
    ProtSink.ReportData(BSCF_LASTDATANOTIFICATION or BSCF_DATAFULLYAVAILABLE,FTotalSize,0);
    if StatusCode=200 then
      ProtSink.ReportResult(S_OK,StatusCode,nil)
    else
      ProtSink.ReportResult(S_OK,StatusCode,PWideChar(StatusText));
    //if Terminated then Recycle;//see TXxmLocalHandler.Terminate
    Terminated:=true;
    State:=ctSpooling;//prevent send to pool on recycle (see Terminate)
   end;

  BindInfo:=nil;
  ProtSink:=nil;
  FHttpNegotiate:=nil;
  FResumeValue:=Null;
  FDropValue:=Null;
  inherited;
end;

procedure TXxmLocalContext.Recycle;
begin
  Lock;
  try
    if State<>ctSpooling then inherited;
  finally
    Unlock;
  end;
end;

function TXxmLocalContext.GetProjectEntry: TXxmProjectEntry;
begin
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_CONNECTING, PWideChar(FProjectName)));
  if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCacheLocal.Create;
  Result:=XxmProjectCache.GetProject(FProjectName);
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_ENCODING, PWideChar(FProjectName)));//?
end;

function TXxmLocalContext.GetProjectPage(const FragmentName: WideString): IXxmFragment;
begin
  Result:=inherited GetProjectPage(FragmentName);
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_SENDINGREQUEST, PWideChar(FFragmentName)));
end;

procedure TXxmLocalContext.HandleRequest;
var
  i,j,l:integer;
  ba:TBindInfoF;
  bi:TBindInfo;
  DoBuild:boolean;
begin
  try
    //bind parameters
    ZeroMemory(@bi,SizeOf(bi));
    bi.cbSize:=SizeOf(bi);
    BindInfo.GetBindInfo(ba,bi);

    //if (ba and BINDF_)<>0 then
    {
    BINDF_ASYNCSTORAGE
    BINDF_NOPROGRESSIVERENDERING
    BINDF_OFFLINEOPERATION
    BINDF_GETNEWESTVERSION
    BINDF_NOWRITECACHE
    BINDF_NEEDFILE
    BINDF_PULLDATA
    BINDF_IGNORESECURITYPROBLEM
    BINDF_RESYNCHRONIZE
    BINDF_HYPERLINK
    BINDF_NO_UI
    BINDF_SILENTOPERATION
    BINDF_PRAGMA_NO_CACHE
    BINDF_FREE_THREADED
    BINDF_DIRECT_READ
    BINDF_FORMS_SUBMIT
    BINDF_GETFROMCACHE_IF_NET_FAIL
    }

    case bi.dwBindVerb of
      BINDVERB_GET:FVerb:='GET';
      BINDVERB_POST:FVerb:='POST';
      BINDVERB_PUT:FVerb:='PUT';
      else FVerb:=bi.szCustomVerb;
    end;
    if bi.szExtraInfo=nil then FExtraInfo:='' else FExtraInfo:=bi.szExtraInfo;
    //bi.grfBindInfoF
    //bi.dwCodePage?
    if bi.cbstgmedData<>0 then FPostData:=StgMediumAsStream(bi.stgmedData);

    OleCheck(ProtSink.ReportProgress(BINDSTATUS_FINDINGRESOURCE, nil));
    //parse URL
    i:=1;
    l:=Length(FURL);
    while (i<=l) and (FURL[i]<>':') do inc(i);
    inc(i);
    //assert starts with 'xxm:'
    if (i>l) then
     begin
      //nothing after 'xxm:'
      FURL:=Copy(FURL,1,i-1)+'//'+Copy(FURL,1,i-2)+'/';
      inc(i,2);
      l:=Length(FURL);
      OleCheck(ProtSink.ReportProgress(BINDSTATUS_REDIRECTING,PWideChar(FURL)));
     end
    else
     begin
      j:=i;
      if (i<=l) and (FURL[i]='/') then inc(i);
      if (i<=l) and (FURL[i]='/') then inc(i);
      if i-j<>2 then
       begin
        FURL:=Copy(FURL,1,j-1)+'//'+Copy(FURL,i,l-i+1);
        i:=j+2;
        l:=Length(FURL);
        OleCheck(ProtSink.ReportProgress(BINDSTATUS_REDIRECTING,PWideChar(FURL)));
       end;
     end;
    j:=i;
    while (i<=l) and not(AnsiChar(FURL[i]) in ['/','?','&','$','#']) do inc(i);
    //if server then remote?
    FProjectName:=Copy(FURL,j,i-j);
    if FProjectName='' then
     begin
      FProjectName:=DefaultProjectName;
      FURL:=Copy(FURL,1,j-1)+FProjectName+Copy(FURL,i,Length(FURL)-i+1);
      OleCheck(ProtSink.ReportProgress(BINDSTATUS_REDIRECTING,PWideChar(FURL)));
     end;
    FPageClass:='['+FProjectName+']';
    if (i>l) then
     begin
      FURL:=FURL+'/';
      inc(l);
      OleCheck(ProtSink.ReportProgress(BINDSTATUS_REDIRECTING,PWideChar(FURL)));
     end;
    if (FURL[i]='/') then inc(i);

    j:=i;
    while (i<=l) and not(AnsiChar(FURL[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURL,j,i-j);
    if (FURL[i]='?') then inc(i);
    j:=i;
    while (j<=l) and (FURL[j]<>'#') do inc(j);
    FQueryString:=Copy(FURL,i,j-i);

    //TODO: ctHeaderOnly?

    DoBuild:=true;
    while DoBuild do
     begin
      DoBuild:=false;
      try
        BuildPage;
      except
        //TODO: IAuthenticate doesn't work?
        //custom authentication interface and credentials store:
        on e1:EXxmUserAuthenticated do
         begin
          FAuthUsr:=e1.UserName;
          FAuthPwd:=e1.Password;
          DoBuild:=true;
          //assert State=ctHeaderNotSent
         end;
      end;
     end;

  except
    on EXxmPageRedirected do Flush;

    on EXxmAutoBuildFailed do
     begin
      //assert AutoBuild handler already displays message
      ForceStatus(StatusBuildError,'BUILDFAILED');
     end;

    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'ERROR');
        SendError('error',e.ClassName,e.Message);
       end;

  end;
end;

procedure TXxmLocalContext.DispositionAttach(const FileName: WideString);
const
  BINDSTATUS_CONTENTDISPOSITIONATTACH=BINDSTATUS_LOADINGMIMEHANDLER+1;
  BINDSTATUS_CONTENTDISPOSITIONFILENAME = 49;
var
  fn,s:WideString;
  i:integer;
begin
  inherited;
  //CheckSendStart/FMimeTypeSent?
  if OutputData=nil then
   begin
    SetLength(fn,1024);
    SetLength(fn,GetTempPathW(1023,PWideChar(fn)));
    CreateDirectoryW(PWideChar(fn+'xxm'),nil);
    //TODO: +FileNameSafe(FileName)?
    fn:=fn+'xxm\'+FileName;
    OutputData:=TFileStream.Create(fn,fmCreate);
   end
  else
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);

  ProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,PWideChar(fn));

  s:=FileName;
  for i:=1 to Length(s) do
    if AnsiChar(s[i]) in ['\','/',':','*','?','"','<','>','|'] then
      s[i]:='_';
  AddResponseHeader('Content-disposition','attachment; filename="'+s+'"');

  ProtSink.ReportProgress(BINDSTATUS_CONTENTDISPOSITIONATTACH,'');
  ProtSink.ReportProgress(BINDSTATUS_CONTENTDISPOSITIONFILENAME,PWideChar(FileName));
end;

function TXxmLocalContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Terminated then
    raise EXxmPageLoadAborted.Create(SXxmPageLoadAborted);
  if Count=0 then Result:=0 else
   begin
    Lock;
    try
      if OutputData=nil then OutputData:=THeapStream.Create;
      OutputData.Position:=OutputSize;
      Result:=OutputData.Write(Buffer,Count);
      OutputSize:=OutputSize+Result; //OutputSize:=OutputData.Position;
    finally
      Unlock;
    end;

    //BSCF_AVAILABLEDATASIZEUNKNOWN?
    if FTotalSize=0 then
     begin
      FTotalSize:=Count;
      OleCheck(ProtSink.ReportData(BSCF_FIRSTDATANOTIFICATION,FTotalSize,0));
     end
    else
     begin
      inc(FTotalSize,Count);
      if DataRead then
       begin
        DataRead:=false;
        OleCheck(ProtSink.ReportData(BSCF_INTERMEDIATEDATANOTIFICATION,FTotalSize,0));
       end;
     end;
    //if Next=ntAborted then raise?

   end;
end;

function TXxmLocalContext.ContextString(cs: TXxmContextString): WideString;
var
  st:ULONG;
  i,c:cardinal;
  d:array[0..255] of POleStr;
  r:HResult;
  ss:TStringStream;
  def:AnsiString;
begin
  st:=0;
  def:='';
  case cs of
    csVersion:           Result:=SelfVersion;
    csExtraInfo:         Result:=FExtraInfo;
    csVerb:              Result:=FVerb;
    csQueryString:       Result:=FQueryString;
    //strange, not all bindstrings are supported!
    csUserAgent:
     begin
      c:=1024;
      SetLength(def,c);
      if ObtainUserAgentString(0,PAnsiChar(def),c)=0 then
       begin
        SetLength(def,c-1);
        Result:=WideString(def);
       end
      else
       begin
        //raise? Result:=''?
        st:=BINDSTRING_USER_AGENT;//try this way
       end;
     end;
    csAcceptedMimeTypes: Result:='*/*';//st:=BINDSTRING_ACCEPT_MIMES;//invalid index?
    //csCookie:            st:=BINDSTRING_POST_COOKIE;//nothing about cookies! it's a secundary cache key!
    csPostMimeType:
     begin
      st:=BINDSTRING_POST_DATA_MIME;
      def:=MimeFormUrlEncoded;
     end;
    csURL:               st:=BINDSTRING_URL;
    csProjectName:       Result:=FProjectName;
    csLocalURL:          Result:=FFragmentName;
    csReferer:           Result:=GetRequestHeader('Referer');
    csLanguage:
     begin
      //st:=BINDSTRING_LANGUAGE;//doc says not supported, get current
      Result:=GetRequestHeader('Accept-Language');
      if Result='' then Result:=LocaleLanguage;
     end;
    csRemoteAddress:     Result:='127.0.0.1';//TODO: IPV6?
    csRemoteHost:        Result:='localhost';
    //csAuthUser,csAuthPassword:Result:=AuthValue(cs);
    csAuthUser:          Result:=FAuthUsr;
    csAuthPassword:      Result:=FAuthPwd;
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  if st<>0 then
   begin
    r:=BindInfo.GetBindString(st,@d,256,c);
    if r=INET_E_USE_DEFAULT_SETTING then
      Result:=WideString(def)
    else
     begin
      OleCheck(r);
      if c=0 then Result:='' else
       begin
        if c=1 then Result:=d[0] else
         begin
          ss:=TStringStream.Create('');
          try
            for i:=0 to c-1 do ss.WriteString(d[i]+#13#10);
            Result:=ss.DataString;
          finally
            ss.Free;
          end;
         end;
        for i:=c-1 downto 0 do CoTaskMemFree(d[i]);
       end;
     end;
   end;
end;

procedure TXxmLocalContext.SendHeader;
var
  px:WideString;
  py:PWideChar;
begin
  if Terminated then
    raise EXxmPageLoadAborted.Create(SXxmPageLoadAborted);
  if StatusCode=401 then
   begin
    px:=FResHeaders.Item['WWW-Authenticate'];
    if px<>'' then
     begin
      //?OleCheck(ProtSink.ReportResult(INET_E_AUTHENTICATION_REQUIRED,0,0));
      XxmAuthenticateUser(FProjectName,FURL,px);
     end;
   end;

  OleCheck(ProtSink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE,PWideChar(FContentType)));
  if FHttpNegotiate=nil then
    OleCheck((ProtSink as IServiceProvider).QueryService(
      IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
  px:=WideString(FResHeaders.Build)+#13#10;
  py:=nil;
  OleCheck(FHttpNegotiate.OnResponse(StatusCode ,PWideChar(px),nil,py));

  //BINDSTATUS_ENCODING
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE,PWideChar(FContentType)));
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_BEGINDOWNLOADDATA,''));
  
  inherited;//State:=ctResponding;
end;

function TXxmLocalContext.StgMediumAsStream(stgmed: TStgMedium): TStream;
var
  p:pointer;
  l:cardinal;
  m:TMemoryStream;
  f:TFileStream;
  s:IStream;
const
  StreamThreshold=$10000;//TODO: setting?
begin
  case stgmed.tymed of
    TYMED_HGLOBAL:
     begin
      p:=GlobalLock(stgmed.hGlobal);
      l:=GlobalSize(stgmed.hGlobal);
      m:=THeapStream.Create;
      m.Write(p^,l);
      m.Position:=0;
      GlobalUnlock(stgmed.hGlobal);
      Result:=m;
     end;
    TYMED_FILE:
      Result:=TFileStream.Create(stgmed.lpszFileName,fmOpenRead or fmShareDenyNone);
    TYMED_ISTREAM:
     begin
      m:=THeapStream.Create;
      try
        l:=StreamThreshold;
        m.SetSize(l);
        s:=IUnknown(stgmed.stm) as IStream;
        OleCheck(s.Read(m.Memory,l,@l));
        if l=StreamThreshold then
         begin
          SetLength(FPostTempFile,$400);
          SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));//TODO: setting
          FPostTempFile:=FPostTempFile+'xxm_'+IntToHex(integer(Self),8)+'.dat';
          f:=TFileStream.Create(FPostTempFile,fmCreate);
          f.Write(m.Memory^,l);
          while l<>0 do
           begin
            l:=StreamThreshold;
            OleCheck(s.Read(m.Memory,l,@l));
            if l<>0 then f.Write(m.Memory^,l);
           end;
          f.Seek(0,soFromBeginning);
          Result:=f;
         end
        else
         begin
          //assert m.Position:=0;
          m.SetSize(l);
          Result:=m;
          m:=nil;
         end;
      finally
        if m<>nil then m.Free;
        s:=nil;
      end;
     end;
    //TYMED_ISTORAGE?
    else
      raise EXxmUnknownPostDataTymed.Create(
        'PostData has unsupported TYMED '+IntToHex(stgmed.tymed,8));
  end;
end;

function TXxmLocalContext.Connected: Boolean;
begin
  Result:=not(Terminated);
end;

procedure TXxmLocalContext.Redirect(const RedirectURL: WideString; Relative:boolean);
var
  l:cardinal;
  s:WideString;
begin
  inherited;
  CheckHeaderNotSent;
  State:=ctHeaderOnly;
  //BINDSTATUS_REDIRECTING?
  if Relative then
   begin
    //TODO: use own combine? (implement IInternetProtocolInfo)
    l:=$400;
    SetLength(s,l);
    OleCheck(CoInternetCombineUrl(PWideChar(FURL),PWideChar(RedirectURL),0,PWideChar(s),l,l,0));
    SetLength(s,l);
   end
  else
    s:=RedirectURL;
  OleCheck(ProtSink.ReportResult(INET_E_REDIRECTING,0,PWideChar(s)));
  ForceStatus(301,'Moved Permanently');
  //SendRaw('Redirected to <a href=""></a>')?
  raise EXxmPageRedirected.Create(s);
end;

function TXxmLocalContext.LocaleLanguage: WideString;
var
  i:integer;
  s,t:WideString;
begin
  i:=$10;
  SetLength(s,i);
  SetLength(s,GetLocaleInfoW(GetThreadLocale,LOCALE_SISO639LANGNAME,PWideChar(s),i));
  i:=$10;
  SetLength(t,i);
  SetLength(t,GetLocaleInfoW(GetThreadLocale,LOCALE_SISO3166CTRYNAME,PWideChar(t),i));
  Result:=LowerCase(s+'-'+t);
end;

procedure TXxmLocalContext.CheckReqHeaders;
var
  px:PWideChar;
  ps:AnsiString;
begin
  if FReqHeaders.Count=0 then //assert at least one request header line
   begin
    px:=nil;
    if FHttpNegotiate=nil then
      OleCheck((ProtSink as IServiceProvider).QueryService(
        IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
    OleCheck(FHttpNegotiate.BeginningTransaction(PWideChar(FURL),nil,0,px));
    ps:=AnsiString(string(px));//TODO: encoding?
    CoTaskMemFree(px);
    if FPostData<>nil then
      ps:=ps+AnsiString('Content-Length: '+IntToStr(FPostData.Size)+#13#10);
    FReqHeaders.Load(ps);
   end;
end;

function XmlDate(s:AnsiString):TDateTime;
var
  i,l,dy,dm,dd,th,tm,ts:word;
  function next:integer;
  begin
    Result:=0;
    while (i<=l) and (s[i] in ['0'..'9']) do
     begin
      Result:=Result*10+byte(s[i])-48;
      inc(i);
     end;
    while (i<=l) and not(s[i] in ['0'..'9']) do inc(i);
  end;
begin
  i:=1;
  l:=Length(s);
  dy:=next;
  dm:=next;
  dd:=next;
  //timezone?
  th:=next;
  tm:=next;
  ts:=next;
  Result:=EncodeDate(dy,dm,dd)+EncodeTime(th,tm,ts,0);
end;

function TXxmLocalContext.GetCookie(const Name: WideString): WideString;
var
  fn:string;
  s:AnsiString;
  f:TFileStream;
  b,b1:boolean;
  i,j,l:integer;
  eols:array[0..4] of integer;
begin
  fn:=(ProjectEntry as TXxmProjectCacheEntry).CookieFile(Name);
  b:=true;
  if FileExists(fn) then
   begin
    f:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
    try
      SetLength(s,3);
      f.Read(s[1],3);
      if (byte(s[1])=Utf8ByteOrderMark[0]) and
         (byte(s[2])=Utf8ByteOrderMark[1]) and
         (byte(s[3])=Utf8ByteOrderMark[2]) then
       begin
        l:=f.Size-3;
        SetLength(s,l);
        f.Read(s[1],l);
       end
      else if (byte(s[1])=Utf16ByteOrderMark[0]) and
              (byte(s[2])=Utf16ByteOrderMark[1]) then
       begin
        f.Position:=2;
        l:=(f.Size div 2)-1;
        SetLength(Result,l);
        f.Read(Result[1],l*2);
        s:=UTF8Encode(Result);
        l:=Length(s);
       end
      else
       begin
        l:=f.Size;
        SetLength(s,l);
        f.Read(s[4],l-3);
       end;
      j:=1;
      for i:=0 to 4 do
       begin
        b1:=false;
        while (j<=l) and not(b1 and (s[j]=#10)) do
         begin
          b1:=s[j]=#13;
          inc(j);
         end;
        eols[i]:=j+1;
       end;
    finally
      f.Free;
    end;
    b:=Now>=XmlDate(Copy(s,eols[0],eols[1]-eols[0]-2));
    if b then
      DeleteFile(fn) //keep a backup?
    else
     begin
      //TODO: check path! (domain? secure? http?)
      Result:=UTF8ToWideString(copy(s,1,eols[0]-2));
     end;
   end;
  if b then Result:=(ProjectEntry as TXxmProjectCacheEntry).GetSessionCookie(Name);
end;

procedure TXxmLocalContext.SetCookie(const Name, Value: WideString);
var
  pce:TXxmProjectCacheEntry;
begin
  CheckHeaderNotSent;
  pce:=ProjectEntry as TXxmProjectCacheEntry;
  pce.SetSessionCookie(Name,Value);
  //TODO: clear persistent one or change it's value?
  //TODO: make this value 'unavailable' from GetCookie until next request
  DeleteFile(pce.CookieFile(Name));
end;

procedure TXxmLocalContext.SetCookie(const Name,Value:WideString;
  KeepSeconds:cardinal; const Comment,Domain,Path:WideString;
  Secure,HttpOnly:boolean);
var
  fn:string;
  s:AnsiString;
  f:TFileStream;
  pce:TXxmProjectCacheEntry;
  function CookieEncode(x:WideString):AnsiString;
  var
    i,j,l:integer;
    y:AnsiString;
  begin
    y:=UTF8Encode(x);
    l:=Length(y);
    j:=0;
    for i:=1 to l do if y[i] in ['%',#13,#10] then inc(j);
    SetLength(Result,l+j);
    i:=1;
    j:=0;
    while i<=l do
     begin
      case y[i] of
        '%':
         begin
          inc(j); Result[j]:='%';
          inc(j); Result[j]:='_';
         end;
        #13:
         begin
          inc(j); Result[j]:='%';
          inc(j); Result[j]:='|';
          if (i<l) and (y[i+1]=#10) then inc(i);
         end;
        #10:
         begin
          inc(j); Result[j]:='%';
          inc(j); Result[j]:='|';
         end;
        else
         begin
          inc(j); Result[j]:=y[i];
         end;
      end;
      inc(i);
     end;
    SetLength(Result,j);
  end;
begin
  CheckHeaderNotSent;
  pce:=ProjectEntry as TXxmProjectCacheEntry;
  fn:=pce.CookieFile(Name);
  if KeepSeconds=0 then
   begin
    DeleteFile(fn);
    pce.SetSessionCookie(Name,Value);
   end
  else
   begin
    f:=TFileStream.Create(fn,fmCreate);
    try
      s:=
        CookieEncode(Value)+
        AnsiString(FormatDateTime('yyyy-mm-dd.hh:nn:ss',Now+KeepSeconds/86400)+#13#10)+
        CookieEncode(Domain)+
        CookieEncode(Path)+
        CookieEncode(Comment);
      if Secure then s:=s+'S';
      if HttpOnly then s:=s+'H';
      s:=s+#13#10;
      f.Write(Utf8ByteOrderMark,3);
      f.Write(s[1],Length(s));
    finally
      f.Free;
    end;
   end;
end;

function TXxmLocalContext.GetSessionID: WideString;
begin
  //http implementations do a SetCookie the first time,
  //which requires CheckHeaderNotSent, simulated here with a boolean
  if not(FGotSessionID) then CheckHeaderNotSent;
  FGotsessionID:=true;
  Result:=IntToHex(HInstance,8)+IntToHex(GetCurrentProcessId,8);
  //GetCurrentThreadId?
end;

procedure TXxmLocalContext.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TXxmLocalContext.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TXxmLocalContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  CheckReqHeaders;
  Result:=FReqHeaders;
end;

function TXxmLocalContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

function TXxmLocalContext.GetRequestHeader(const Name: WideString): WideString;
begin
  CheckReqHeaders;
  Result:=FReqHeaders.Item[Name];
end;

procedure TXxmLocalContext.AddResponseHeader(const Name, Value: WideString);
begin
  FResHeaders.Add(Name,Value);
end;

initialization
  ContextPool:=TXxmContextPool.Create(TXxmLocalContext);
end.
