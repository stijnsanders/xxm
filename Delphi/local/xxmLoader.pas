unit xxmLoader;

interface

uses Windows, SysUtils, ActiveX, UrlMon, Classes, xxm,
  xxmPReg, xxmPRegLocal, xxmParams, xxmParUtils, xxmHeaders;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TXxmPageLoader=class;//forward

  TXxmLocalContext=class(TInterfacedObject, IXxmContext, IxxmHttpHeaders)
  private
    FVerb: WideString;
    ProtSink: IInternetProtocolSink;
    BindInfo: IInternetBindInfo;
    PrPos,PrMax:Integer;
    FURL:WideString;
    FLock:TRTLCriticalSection;
    FProjectEntry:TXxmProjectCacheEntry;
    FStatusCode:integer;
    FStatusText,FProjectName,FFragmentName,FExtraInfo,FQueryString:WideString;
    FirstData,StatusSet,Aborted:boolean;
    FPostData:TStream;
    FPostTempFile,FPageClass:string;
    FSingleFileSent:WideString;
    FMimeTypeSent,FGotSessionID:boolean;
    FIncludeDepth:integer;
    FParams: TXxmReqPars;
    FBuilding: IXxmFragment;
    FReqHeaders: TRequestHeaders;
    FResHeaders: TResponseHeaders;
    FHttpNegotiate: IHttpNegotiate;
    procedure ReportData;
    function CheckSendStart:boolean;
    procedure SendRaw(Data: WideString);
    procedure SendError(res:string;vals:array of string);
    function StgMediumAsStream(stgmed:TStgMedium):TStream;
    procedure HeaderOK;
    procedure CheckReqHeaders;
  protected
    FContentType: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FPage: IXxmFragment;
    function GetURL: WideString;
    function GetPage: IXxmFragment;
    function GetContentType: WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
    function LocaleLanguage: string;
    function GetRequestParam(Name: string):string;
    function GetSessionID: WideString;
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    OutputData:TStream;
    OutputSize,ClippedSize:Int64;
    PageComplete,Redirected,DataReported:boolean;
    Queue:TXxmLocalContext;
    Loader:TXxmPageLoader;

    constructor Create(URL:WideString;
      OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
    destructor Destroy; override;

    procedure Execute;

    procedure Lock;
    procedure Unlock;
    procedure Disconnect;

    property URL:WideString read GetURL;
    property ContentType:WideString read FContentType;
    property StatusCode:integer read FStatusCode;
    property StatusText:WideString read FStatusText;
    property Verb:WideString read FVerb;
    property SingleFileSent:WideString read FSingleFileSent;

    procedure DispositionAttach(FileName: WideString);
    //TODO: progress
    procedure Send(Data: OleVariant); overload;
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s: TStream);
    function ContextString(cs: TXxmContextString): WideString;
    function PostData: TStream;
    procedure SetStatus(Code: Integer; Text: WideString);
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    function Connected: Boolean;
    procedure Redirect(RedirectURL: WideString; Relative:boolean);
    function GetCookie(Name: WideString): WideString;
    procedure SetCookie(Name: WideString; Value: WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
  end;

  TXxmPageLoader=class(TThread)
  protected
    FInUse:boolean;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property InUse:boolean read FInUse;
  end;

  EXxmDirectInclude=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmPageRedirected=class(Exception);

var
  //see xxmSettings
  StatusBuildError,StatusException,StatusFileNotFound:integer;
  DefaultProjectName:string;

implementation

uses Variants, ComObj, AxCtrls, xxmThreadPool;

const //resourcestring?
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

{ TXxmLocalContext }

constructor TXxmLocalContext.Create(URL:WideString;
  OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo);
begin
  inherited Create;
  PageComplete:=false;
  Redirected:=false;
  DataReported:=false;
  Aborted:=false;
  OutputData:=nil;
  OutputSize:=0;
  ClippedSize:=0;
  Queue:=nil;
  InitializeCriticalSection(FLock);

  FURL:=URL;
  FProjectEntry:=nil;
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FPage:=nil;
  FBuilding:=nil;
  BindInfo:=OIBindInfo;
  ProtSink:=OIProtSink;
  PrPos:=0;
  PrMax:=1;
  FirstData:=true;
  FIncludeDepth:=0;
  StatusSet:=false;
  FStatusCode:=200;
  FProjectName:='';//parsed from URL later
  FFragmentName:='';//parsed from URL later
  FStatusText:='';
  FPostData:=nil;
  FPostTempFile:='';
  FMimeTypeSent:=false;
  FParams:=nil;//see GetParameter
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FHttpNegotiate:=nil;
  FPageClass:='';
  FSingleFileSent:='';
  FGotSessionID:=false;
end;

destructor TXxmLocalContext.Destroy;
begin
  FHttpNegotiate:=nil;
  BindInfo:=nil;
  ProtSink:=nil;
  FreeAndNil(FParams);
  if not(FReqHeaders=nil) then
   begin
    (FReqHeaders as IUnknown)._Release;
    FReqHeaders:=nil;
   end;
  if not(FResHeaders=nil) then
   begin
    (FResHeaders as IUnknown)._Release;
    FResHeaders:=nil;
   end;
  FreeAndNil(FPostData);
  if not(FProjectEntry=nil) then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  try
    if not(FPostTempFile='') then
     begin
      DeleteFile(FPostTempFile);
      FPostTempFile:='';
     end;
  except
    //silent
  end;
  FreeAndNil(OutputData);
  DeleteCriticalSection(FLock);
  inherited;
end;

function TXxmLocalContext.GetURL: WideString;
begin
  Result:=FURL;
end;

function TXxmLocalContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

procedure TXxmLocalContext.Execute;
var
  i,j,l:integer;
  x:WideString;
  ba:TBindInfoF;
  bi:TBindInfo;
  p:IXxmPage;
begin
  try
    //bind parameters
    ZeroMemory(@bi,SizeOf(bi));
    bi.cbSize:=SizeOf(bi);
    BindInfo.GetBindInfo(ba,bi);

    //if not((ba and BINDF_)=0) then
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
    if not(bi.cbstgmedData=0) then FPostData:=StgMediumAsStream(bi.stgmedData);

    OleCheck(ProtSink.ReportProgress(BINDSTATUS_FINDINGRESOURCE, nil));
    //parse URL
    i:=1;
    l:=Length(FURL);
    while (i<=l) and not(FURL[i]=':') do inc(i);
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
      if not(i-j=2) then
       begin
        FURL:=Copy(FURL,1,j-1)+'//'+Copy(FURL,i,l-i+1);
        i:=j+2;
        l:=Length(FURL);
        OleCheck(ProtSink.ReportProgress(BINDSTATUS_REDIRECTING,PWideChar(FURL)));
       end;
     end;
    j:=i;
    while (i<=l) and not(Char(FURL[i]) in ['/','?','&','$','#']) do inc(i);
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
    inc(i);

    j:=i;
    while (i<=l) and not(Char(FURL[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURL,j,i-j);
    if (FURL[i]='?') then inc(i);
    FQueryString:=Copy(FURL,i,l-i+1);

    //IHttpNegotiate here? see GetRequestParam

    //create page object
    OleCheck(ProtSink.ReportProgress(BINDSTATUS_CONNECTING, PWideChar(FProjectName)));
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;
    FProjectEntry:=XxmProjectCache.GetProject(FProjectName);
    if not(@XxmAutoBuildHandler=nil) then
     begin
      OleCheck(ProtSink.ReportProgress(BINDSTATUS_ENCODING, PWideChar(FProjectName)));//?
      if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
        raise EXxmAutoBuildFailed.Create(FProjectName);
     end;
    FProjectEntry.OpenContext;
    OleCheck(ProtSink.ReportProgress(BINDSTATUS_SENDINGREQUEST, PWideChar(FFragmentName)));
    FPage:=FProjectEntry.Project.LoadPage(Self,FFragmentName);

    if FPage=nil then
     begin
      //find a file
      //ask project to translate? project should have given a fragment!
      FPageClass:='['+FProjectName+']GetFilePath';
      FProjectEntry.GetFilePath(FFragmentName,FSingleFileSent,x);
      if FileExists(FSingleFileSent) then
       begin
        //TODO: if directory file-list?
        FContentType:=x;
        if not(Aborted) then SendFile(FSingleFileSent);
       end
      else
       begin
        FPageClass:='['+FProjectName+']404:'+FFragmentName;
        FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
        if FPage=nil then
         begin
          FStatusCode:=StatusFileNotFound;
          FStatusText:='File not found';
          SendError('fnf',[
            'URL',HTMLEncode(URL),
            'PROJECT',FProjectName,
            'ADDRESS',FFragmentName,
            'PATH',HTMLEncode(FSingleFileSent),
            'VERSION',ContextString(csVersion)
          ]);
         end
        else
          try
            FPageClass:=FPage.ClassNameEx;
            FBuilding:=FPage;
            FPage.Build(Self,nil,[FFragmentName,FSingleFileSent,x],[]);//any parameters?
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
    on EXxmPageRedirected do
     begin
      FStatusCode:=301;//??
      FStatusText:='Moved Permanently';
      //SendHTML('Redirected to <a href=""></a>')?
     end;

    on EXxmAutoBuildFailed do
     begin
      //assert AutoBuild handler already displays message
      StatusSet:=true;
      FStatusCode:=StatusBuildError;
      FStatusText:='BUILDFAILED';
     end;

    on e:Exception do
     begin
      StatusSet:=true;
      FStatusCode:=StatusException;
      FStatusText:='ERROR';
      //TODO: get fragment 500.xxm?
      try
        if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
      except
        x:='unknown';
      end;
      SendError('error',[
        'URL',HTMLEncode(URL),
        'CLASS',FPageClass,
        'POSTDATA',x,
        'QUERYSTRING',FQueryString,
        'ERROR',HTMLEncode(e.Message),
        'ERRORCLASS',e.ClassName,
        'VERSION',ContextString(csVersion)
      ]);
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
    if not(StatusSet) or (FStatusCode=200) then
      ProtSink.ReportResult(S_OK,FStatusCode,nil)
    else
      ProtSink.ReportResult(S_OK,FStatusCode,PWideChar(FStatusText))
    //TODO: find out why iexplore keeps couting up progress sometimes (even after terminate+unlock)
   end;
end;

procedure TXxmLocalContext.ReportData;
begin
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

procedure TXxmLocalContext.SendHTML(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(VarToWideStr(Data));
end;

procedure TXxmLocalContext.Send(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(HTMLEncode(VarToWideStr(Data)));
end;

const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;

procedure TXxmLocalContext.SendRaw(Data: WideString);
var
  s:string;
  b:boolean;
begin
  inherited;
  if not(Data='') then
   begin
    //report mime type makes handler lock, so check before lock
    b:=CheckSendStart;
    Lock;
    try
      if OutputData=nil then OutputData:=TMemoryStream.Create;
      OutputData.Position:=OutputSize;
      //check start and start with UTF Byte Order Mark
      if b then
        case FAutoEncoding of
          aeUtf8:OutputData.Write(Utf8ByteOrderMark,3);
          aeUtf16:OutputData.Write(Utf16ByteOrderMark,2);
        end;
      case FAutoEncoding of
        aeUtf16:OutputData.Write(Data[1],Length(Data)*2);
        aeUtf8:
         begin
          s:=UTF8Encode(Data);
          OutputData.Write(s[1],Length(s));
         end;
        else
         begin
          s:=Data;
          OutputData.Write(s[1],Length(s));
         end;
      end;
      OutputSize:=OutputData.Position;
    finally
      Unlock;
    end;
    ReportData;
   end;
end;

procedure TXxmLocalContext.SendFile(FilePath: WideString);
var
  f:TFileStream;
  b:boolean;
begin
  inherited;
  //TODO: auto mimetype by extension?
  b:=not(FMimeTypeSent);
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone);
  try
    SendStream(f);//does CheckSendStart
    if b then FSingleFileSent:=FilePath;
  finally
    f.Free;
  end;
end;

procedure TXxmLocalContext.SendStream(s: TStream);
const
  SendBufferSize=$10000;
var
  l:integer;
  d:array[0..SendBufferSize-1] of byte;
begin
  inherited;
  //if not(s.Size=0) then
   begin
    CheckSendStart;
    //no autoencoding here!
    l:=SendBufferSize;
    repeat
      Lock;
      try
        if OutputData=nil then OutputData:=TMemoryStream.Create;
        OutputData.Position:=OutputSize;
        l:=s.Read(d[0],l);
        OutputData.Write(d[0],l);
        OutputSize:=OutputData.Position;
      finally
        Unlock;
      end;
      ReportData;
    until not(l=SendBufferSize);
   end;
end;

function TXxmLocalContext.ContextString(cs: TXxmContextString): WideString;
var
  st:ULONG;
  i,c:cardinal;
  d:array[0..255] of POleStr;
  r:HResult;
  ss:TStringStream;
  def:string;
begin
  st:=0;
  def:='';
  case cs of
    csVersion:           Result:=SelfVersion;
    csExtraInfo:         Result:=FExtraInfo;
    csVerb:              Result:=FVerb;
    csQueryString:       Result:=FQueryString;
    //strange, not all bindstrings are supported!
    csUserAgent:         st:=BINDSTRING_USER_AGENT;
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
  if not(st=0) then
   begin
    r:=BindInfo.GetBindString(st,@d,256,c);
    //TODO: not enough mem?
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

function TXxmLocalContext.PostData: TStream;
begin
  Result:=FPostData;
end;

procedure TXxmLocalContext.SendError(res: string; vals: array of string);
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
  if not(FMimeTypeSent) then
   begin
    FContentType:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

function TXxmLocalContext.CheckSendStart:boolean;
var
  px:WideString;
  py:PWideChar;
begin
  Result:=not(FMimeTypeSent);
  if Result then
   begin
    //FAutoEncoding: see SendHTML
    OleCheck(ProtSink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE,PWideChar(FContentType)));

    if FHttpNegotiate=nil then
      OleCheck((ProtSink as IServiceProvider).QueryService(
        IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
    px:=FResHeaders.Build+#13#10;
    py:=nil;
    OleCheck(FHttpNegotiate.OnResponse(FStatusCode,PWideChar(px),nil,py));
    //TODO: add py to FResHeaders?

    //BINDSTATUS_ENCODING
    OleCheck(ProtSink.ReportProgress(BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE,PWideChar(FContentType)));
    OleCheck(ProtSink.ReportProgress(BINDSTATUS_BEGINDOWNLOADDATA,''));
    FMimeTypeSent:=true;
   end
  else
    FSingleFileSent:='';
end;

function TXxmLocalContext.GetContentType: WideString;
begin
  Result:=FContentType;
end;

procedure TXxmLocalContext.HeaderOK;
begin
  if FMimeTypeSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

procedure TXxmLocalContext.SetContentType(const Value: WideString);
begin
  HeaderOK;
  FContentType:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

function TXxmLocalContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

procedure TXxmLocalContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  HeaderOK;
  FAutoEncoding:=Value;
end;

procedure TXxmLocalContext.SetStatus(Code: Integer; Text: WideString);
begin
  HeaderOK;
  FStatusCode:=Code;
  FStatusText:=Text;
  StatusSet:=true;
end;

procedure TXxmLocalContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TXxmLocalContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TXxmLocalContext.Include(Address: WideString;
  const Values: array of OleVariant;
  const Objects: array of TObject);
var
  f,fb:IXxmFragment;
  pc:string;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  //FPage.Project?
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
    FPageClass:=pc;
  finally
    dec(FIncludeDepth);
    FBuilding:=fb;
    fb:=nil;
    FProjectEntry.Project.UnloadFragment(f);
    f:=nil;
  end;
end;

function TXxmLocalContext.StgMediumAsStream(stgmed: TStgMedium): TStream;
var
  p:pointer;
  l:cardinal;
  m:TMemoryStream;
  f:TFileStream;
  s:IStream;
const
  StreamTreshold=$10000;//TODO: setting?
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
        l:=StreamTreshold;
        m.SetSize(l);
        s:=IUnknown(stgmed.stm) as IStream;
        OleCheck(s.Read(m.Memory,l,@l));
        if l=StreamTreshold then
         begin
          SetLength(FPostTempFile,$400);
          SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));//TODO: setting
          FPostTempFile:=FPostTempFile+'xxm_'+IntToHex(integer(Self),8)+'.dat';
          f:=TFileStream.Create(FPostTempFile,fmCreate);
          f.Write(m.Memory^,l);
          while l=StreamTreshold do
           begin
            OleCheck(s.Read(m.Memory,l,@l));
            f.Write(m.Memory^,l);
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
        if not(m=nil) then m.Free;
        s:=nil;
      end;
     end;
    //TYMED_ISTORAGE?
    else
      raise EXxmUnknownPostDataTymed.Create(
        'PostData has unkown TYMED '+IntToHex(stgmed.tymed,8));
  end;
end;

function TXxmLocalContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  //parse parameters on first use
  if FParams=nil then
   begin
    FParams:=TXxmReqPars.Create(Self);
    //redirect on post? invalidate postdata!
    if FParams.PostDataOnRedirect then FreeAndNil(FPostData);
   end;
  if VarIsNumeric(Key) then Result:=FParams.GetItem(Key) else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TXxmLocalContext.GetParameterCount: Integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  Result:=FParams.Count;
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
  HeaderOK;
  Redirected:=true;
  //BINDSTATUS_REDIRECTING?
  if Relative then
   begin
    //TODO: use own combine? (implement IInternetProtocolInfo)
    l:=$400;
    SetLength(s,l);
    OleCheck(CoInternetCombineUrl(PWideChar(URL),PWideChar(RedirectURL),0,PWideChar(s),l,l,0));
    SetLength(s,l);
   end
  else
    s:=RedirectURL;
  OleCheck(ProtSink.ReportResult(INET_E_REDIRECTING,0,PWideChar(s)));
  raise EXxmPageRedirected.Create(s);
end;

function TXxmLocalContext.LocaleLanguage: string;
var
  i:integer;
  s,t:string;
begin
  i:=$10;
  SetLength(s,i);
  SetLength(s,GetLocaleInfo(GetThreadLocale,LOCALE_SISO639LANGNAME,PChar(s),i));
  i:=$10;
  SetLength(t,i);
  SetLength(t,GetLocaleInfo(GetThreadLocale,LOCALE_SISO3166CTRYNAME,PChar(t),i));
  Result:=LowerCase(s+'-'+t);
end;

procedure TXxmLocalContext.CheckReqHeaders;
var
  px:PWideChar;
begin
  if FReqHeaders=nil then
   begin
    //catch extra headers
    px:=nil;
    if FHttpNegotiate=nil then
      OleCheck((ProtSink as IServiceProvider).QueryService(
        IID_IHttpNegotiate,IID_IHttpNegotiate,FHttpNegotiate));
    OleCheck(FHttpNegotiate.BeginningTransaction(PWideChar(URL),nil,0,px));
    FReqHeaders:=TRequestHeaders.Create(px);
    (FReqHeaders as IUnknown)._AddRef;
   end;
end;

function TXxmLocalContext.GetRequestParam(Name: string): string;
begin
  CheckReqHeaders;
  Result:=FReqHeaders.Item[Name];
end;

function XmlDate(s:string):TDateTime;
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

function TXxmLocalContext.GetCookie(Name: WideString): WideString;
var
  fn,s:string;
  f:TFileStream;
  b,b1:boolean;
  i,j,l:integer;
  eols:array[0..4] of integer;
begin
  fn:=FProjectEntry.CookieFile(Name);
  b:=true;
  if FileExists(fn) then
   begin
    f:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
    try
      SetLength(s,3);
      f.Read(s[1],3);
      //TODO: other encodings?
      if not(s=Utf8ByteOrderMark) then
        raise Exception.Create('File "'+fn+'" is not UTF8');
      l:=f.Size-3;
      SetLength(s,l);
      f.Read(s[1],l);
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
      Result:=UTF8Decode(copy(s,1,eols[0]-2));
     end;
   end;
  if b then Result:=FProjectEntry.GetSessionCookie(Name);
end;

procedure TXxmLocalContext.SetCookie(Name, Value: WideString);
begin
  HeaderOK;
  FProjectEntry.SetSessionCookie(Name,Value);
  //TODO: clear persistent one or change it's value?
  DeleteFile(FProjectEntry.CookieFile(Name));
end;

procedure TXxmLocalContext.SetCookie(Name,Value:WideString;
  KeepSeconds:cardinal; Comment,Domain,Path:WideString;
  Secure,HttpOnly:boolean);
var
  fn,s:string;
  f:TFileStream;
  function CookieEncode(x:WideString):string;
  begin
    Result:=StringReplace(StringReplace(UTF8Encode(x),
      '%','%_',[rfReplaceAll]),#13#10,'%|',[rfReplaceAll])+#13#10;
  end;
begin
  HeaderOK;
  fn:=FProjectEntry.CookieFile(Name);
  if KeepSeconds=0 then
   begin
    DeleteFile(fn);
    FProjectEntry.SetSessionCookie(Name,Value);
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
  //which requires HeaderOK, simulated here with booleans
  if not(FGotSessionID) then HeaderOK;
  FGotsessionID:=true;
  Result:=IntToHex(HInstance,8)+IntToHex(GetCurrentProcessId,8);
  //GetCurrentThreadId?
end;

procedure TXxmLocalContext.Disconnect;
begin
  Aborted:=true;
  //throw exception in thread?
end;

procedure TXxmLocalContext.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TXxmLocalContext.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

procedure TXxmLocalContext.Send(Value: int64);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmLocalContext.Send(Value: integer);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmLocalContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(HTMLEncode(Values[i]));
end;

procedure TXxmLocalContext.Send(Value: cardinal);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmLocalContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(VarToWideStr(Values[i]));
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

{ TXxmPageLoader }

constructor TXxmPageLoader.Create;
begin
  inherited Create(false);
  //FInUse:=false;
end;

destructor TXxmPageLoader.Destroy;
begin
  inherited;
end;

procedure TXxmPageLoader.Execute;
var
  Context:TXxmLocalContext;
  ContextI:IXxmContext;
begin
  inherited;
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Context:=PageLoaderPool.Unqueue;
    if Context=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      Suspend;
      FInUse:=true;
     end
    else
     begin
      ContextI:=Context;//keep refcount up for premature terminate
      try
        Context.Execute;//assert all exceptions handled!
      finally
        ContextI:=nil;
      end;
     end;
   end;
  CoUninitialize;
end;

end.
