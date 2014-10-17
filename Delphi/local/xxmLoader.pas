unit xxmLoader;

interface

uses Windows, SysUtils, ActiveX, UrlMon, Classes, xxm, xxmContext,
  xxmPReg, xxmPRegLocal, xxmParams, xxmParUtils, xxmHeaders, xxmThreadPool;

type
  TXxmLocalContext=class(TXxmQueueContext, IxxmHttpHeaders)
  private
    FVerb: WideString;
    ProtSink: IInternetProtocolSink;
    BindInfo: IInternetBindInfo;
    PrPos,PrMax:Integer;
    FLock:TRTLCriticalSection;
    FExtraInfo,FQueryString:WideString;
    FirstData:boolean;
    FGotSessionID:boolean;
    FReqHeaders: TRequestHeaders;
    FResHeaders: TResponseHeaders;
    FHttpNegotiate: IHttpNegotiate;
    function StgMediumAsStream(stgmed:TStgMedium):TStream;
    function LocaleLanguage: AnsiString;
    procedure CheckReqHeaders;
  protected
    procedure SendHeader; override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function ContextString(cs: TXxmContextString): WideString; override;
    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function GetCookie(Name: WideString): WideString; override;
    procedure SetCookie(Name: WideString; Value: WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;
    function GetRequestParam(Name: AnsiString):AnsiString;
    function GetProjectEntry:TXxmProjectEntry; override;
    function GetProjectPage(FragmentName: WideString):IXxmFragment; override;
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
  public
    OutputData:TStream;
    OutputSize:Int64;
    PageComplete,Redirected,DataReported,Aborted:boolean;
    Loader:TXxmPageLoader;

    constructor Create(URL:WideString;
      OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
    destructor Destroy; override;

    procedure Execute; override;

    procedure Lock;
    procedure Unlock;

    property Verb: WideString read FVerb;
  end;
  
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageLoadAborted=class(Exception);

var
  //see xxmSettings
  DefaultProjectName:AnsiString;

implementation

uses Variants, ComObj, AxCtrls, xxmCommonUtils;

const //resourcestring?
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmPageLoadAborted='Page Load Aborted';

{ TXxmLocalContext }

constructor TXxmLocalContext.Create(URL:WideString;
  OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
begin
  inherited Create(URL);
  PageComplete:=false;
  Redirected:=false;
  DataReported:=false;
  Aborted:=false;
  OutputData:=nil;
  OutputSize:=0;
  SendDirect:=SendData;
  InitializeCriticalSection(FLock);

  BindInfo:=OIBindInfo;
  ProtSink:=OIProtSink;
  PrPos:=0;
  PrMax:=1;
  FirstData:=true;
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FHttpNegotiate:=nil;
  FGotSessionID:=false;
end;

destructor TXxmLocalContext.Destroy;
begin
  FHttpNegotiate:=nil;
  BindInfo:=nil;
  ProtSink:=nil;
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
  FreeAndNil(OutputData);
  DeleteCriticalSection(FLock);
  inherited;
end;

function TXxmLocalContext.GetProjectEntry: TXxmProjectEntry;
begin
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_CONNECTING, PWideChar(FProjectName)));
  if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCacheLocal.Create;
  Result:=XxmProjectCache.GetProject(FProjectName);
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_ENCODING, PWideChar(FProjectName)));//?
end;

function TXxmLocalContext.GetProjectPage(FragmentName: WideString): IXxmFragment;
begin
  Result:=inherited GetProjectPage(FragmentName);
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_SENDINGREQUEST, PWideChar(FFragmentName)));
end;

procedure TXxmLocalContext.Execute;
var
  i,j,l:integer;
  ba:TBindInfoF;
  bi:TBindInfo;
  x:AnsiString;
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
    while (i<=l) and not(char(FURL[i]) in ['/','?','&','$','#']) do inc(i);
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
    while (i<=l) and not(char(FURL[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURL,j,i-j);
    if (FURL[i]='?') then inc(i);
    j:=i;
    while (j<=l) and (FURL[j]<>'#') do inc(j);
    FQueryString:=Copy(FURL,i,j-i);

    //IHttpNegotiate here? see GetRequestParam

    BuildPage;

  except
    on EXxmPageRedirected do
     begin
      ForceStatus(301,'Moved Permanently');
      //SendRaw('Redirected to <a href=""></a>')?
     end;

    on EXxmAutoBuildFailed do
     begin
      //assert AutoBuild handler already displays message
      ForceStatus(StatusBuildError,'BUILDFAILED');
     end;

    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'ERROR');
        try
          if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',e.ClassName,e.Message);
       end;
  end;

  PageComplete:=true;//see Handler.Read
  if Redirected then
    //redirect calls it's own ReportResult
  else
   begin
    //OleCheck?
    //PrPos:=PrMax;
    ProtSink.ReportData(BSCF_LASTDATANOTIFICATION or BSCF_DATAFULLYAVAILABLE,PrPos,PrMax);
    if StatusCode=200 then
      ProtSink.ReportResult(S_OK,StatusCode,nil)
    else
      ProtSink.ReportResult(S_OK,StatusCode,PWideChar(StatusText))
    //TODO: find out why iexplore keeps counting up progress sometimes (even after terminate+unlock)
   end;
end;

procedure TXxmLocalContext.DispositionAttach(FileName: WideString);
const
  BINDSTATUS_CONTENTDISPOSITIONATTACH=BINDSTATUS_LOADINGMIMEHANDLER+1;
  BINDSTATUS_CONTENTDISPOSITIONFILENAME = 49;
var
  fn:WideString;
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

  ProtSink.ReportProgress(BINDSTATUS_CONTENTDISPOSITIONATTACH,'');
  ProtSink.ReportProgress(BINDSTATUS_CONTENTDISPOSITIONFILENAME,PWideChar(FileName));
end;

function TXxmLocalContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Aborted then raise EXxmPageLoadAborted.Create(SXxmPageLoadAborted);
  if Count=0 then Result:=0 else
   begin
    Lock;
    try
      if OutputData=nil then OutputData:=TMemoryStream.Create;
      OutputData.Position:=OutputSize;
      Result:=OutputData.Write(Buffer,Count);
      OutputSize:=OutputSize+Result; //OutputSize:=OutputData.Position;
    finally
      Unlock;
    end;

    //BSCF_AVAILABLEDATASIZEUNKNOWN?
    if FirstData then
     begin
      OleCheck(ProtSink.ReportData(BSCF_FIRSTDATANOTIFICATION,PrPos,PrMax));
      FirstData:=false;
     end
    else
      if not(DataReported) then
       begin
        DataReported:=true;
        inc(PrPos);//needs to change for IE to react
        if PrPos>PrMax then PrMax:=PrMax*10;
        OleCheck(ProtSink.ReportData(BSCF_INTERMEDIATEDATANOTIFICATION,PrPos,PrMax));
       end;
    //if Aborted then raise?

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
        Result:=def;
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
    csReferer:           Result:=GetRequestParam('Referer');
    csLanguage:
     begin
      //st:=BINDSTRING_LANGUAGE;//doc says not supported, get current
      Result:=GetRequestParam('Accept-Language');
      if Result='' then Result:=LocaleLanguage;
     end;
    csRemoteAddress:     Result:='127.0.0.1';//TODO: IPV6?
    csRemoteHost:        Result:='localhost';
    csAuthUser:          //st:=BINDSTRING_USERNAME;//doc says not supported
      //TODO: GetUserNameEx?
      Result:=GetEnvironmentVariable('USERDOMAIN')+'\'+GetEnvironmentVariable('USERNAME');
    csAuthPassword:      //st:=BINDSTRING_PASSWORD;//doc says not supported
      Result:='';
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  if st<>0 then
   begin
    r:=BindInfo.GetBindString(st,@d,256,c);
    if r=INET_E_USE_DEFAULT_SETTING then Result:=def else
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
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE,PWideChar(FContentType)));

  if FHttpNegotiate=nil then
    OleCheck((ProtSink as IServiceProvider).QueryService(
      IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
  px:=FResHeaders.Build+#13#10;
  py:=nil;
  OleCheck(FHttpNegotiate.OnResponse(StatusCode,PWideChar(px),nil,py));

  //BINDSTATUS_ENCODING
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE,PWideChar(FContentType)));
  OleCheck(ProtSink.ReportProgress(BINDSTATUS_BEGINDOWNLOADDATA,''));
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
      m:=TMemoryStream.Create;
      m.Write(p^,l);
      m.Position:=0;
      GlobalUnlock(stgmed.hGlobal);
      Result:=m;
     end;
    TYMED_FILE:
      Result:=TFileStream.Create(stgmed.lpszFileName,fmOpenRead or fmShareDenyNone);
    TYMED_ISTREAM:
     begin
      m:=TMemoryStream.Create;
      try
        l:=StreamThreshold;
        m.SetSize(l);
        s:=IUnknown(stgmed.stm) as IStream;
        OleCheck(s.Read(m.Memory,l,@l));
        if l=StreamThreshold then
         begin
          SetLength(FPostTempFile,$400);
          SetLength(FPostTempFile,GetTempPathA($400,PAnsiChar(FPostTempFile)));//TODO: setting
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
        'PostData has unkown TYMED '+IntToHex(stgmed.tymed,8));
  end;
end;

function TXxmLocalContext.Connected: Boolean;
begin
  Result:=not(Aborted);
end;

procedure TXxmLocalContext.Redirect(RedirectURL: WideString; Relative:boolean);
var
  l:cardinal;
  s:WideString;
begin
  inherited;
  CheckHeaderNotSent;
  Redirected:=true;
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
  raise EXxmPageRedirected.Create(s);
end;

function TXxmLocalContext.LocaleLanguage: AnsiString;
var
  i:integer;
  s,t:AnsiString;
begin
  i:=$10;
  SetLength(s,i);
  SetLength(s,GetLocaleInfoA(GetThreadLocale,LOCALE_SISO639LANGNAME,PAnsiChar(s),i));
  i:=$10;
  SetLength(t,i);
  SetLength(t,GetLocaleInfoA(GetThreadLocale,LOCALE_SISO3166CTRYNAME,PAnsiChar(t),i));
  Result:=LowerCase(s+'-'+t);
end;

procedure TXxmLocalContext.CheckReqHeaders;
var
  px:PWideChar;
  ps:AnsiString;
begin
  if FReqHeaders=nil then
   begin
    //catch extra headers
    px:=nil;
    if FHttpNegotiate=nil then
      OleCheck((ProtSink as IServiceProvider).QueryService(
        IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
    OleCheck(FHttpNegotiate.BeginningTransaction(PWideChar(FURL),nil,0,px));
    ps:=px;//TODO: encoding?
    CoTaskMemFree(px);
    if FPostData<>nil then ps:=ps+'Content-Length: '+IntToStr(FPostData.Size)+#13#10;
    FReqHeaders:=TRequestHeaders.Create(ps);
    (FReqHeaders as IUnknown)._AddRef;
   end;
end;

function TXxmLocalContext.GetRequestParam(Name: AnsiString): AnsiString;
begin
  CheckReqHeaders;
  Result:=FReqHeaders.Item[Name];
end;

function XmlDate(s:AnsiString):TDateTime;
var
  i,l,dy,dm,dd,th,tm,ts:word;
  function next:integer;
  begin
    Result:=0;
    while (i<=l) and (char(s[i]) in ['0'..'9']) do
     begin
      Result:=Result*10+byte(s[i])-48;
      inc(i);
     end;
    while (i<=l) and not(char(s[i]) in ['0'..'9']) do inc(i);
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

function TXxmLocalContext.GetCookie(Name: WideString): WideString;
var
  fn,s:AnsiString;
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
      if s=Utf8ByteOrderMark then
       begin
        l:=f.Size-3;
        SetLength(s,l);
        f.Read(s[1],l);
       end
      else if Copy(s,1,2)=Utf16ByteOrderMark then
       begin
        f.Position:=2;
        l:=(f.Size div 2)-1;
        SetLength(Result,l);
        f.Read(Result[1],l*2);
        s:=UTF8Decode(Result);
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

procedure TXxmLocalContext.SetCookie(Name, Value: WideString);
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

procedure TXxmLocalContext.SetCookie(Name,Value:WideString;
  KeepSeconds:cardinal; Comment,Domain,Path:WideString;
  Secure,HttpOnly:boolean);
var
  fn,s:AnsiString;
  f:TFileStream;
  pce:TXxmProjectCacheEntry;
  function CookieEncode(x:WideString):AnsiString;
  begin
    Result:=StringReplace(StringReplace(UTF8Encode(x),
      '%','%_',[rfReplaceAll]),#13#10,'%|',[rfReplaceAll])+#13#10;
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
      s:=CookieEncode(Value)+
        FormatDateTime('yyyy-mm-dd.hh:nn:ss',Now+KeepSeconds/86400)+#13#10+
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
  Result:=FResHeaders.Item[Name];
end;

procedure TXxmLocalContext.AddResponseHeader(const Name, Value: WideString);
begin
  FResHeaders.Add(Name,Value);
end;

end.
