unit xxmGeckoChannel;

interface

uses nsXPCOM, nsTypes, nsGeckoStrings, nsThreadUtils, xxm,
  Windows, Classes, SysUtils, xxmDictionary, xxmGeckoStreams, xxmPReg, xxmParams;

type
  TxxmChannel=class(TInterfacedObject,
    IInterface,
    nsIRequest,
    nsIChannel,
    nsIHttpChannel,
    //nsIClassInfo,
    //nsIInterfaceRequestor,
    //nsITransportEventSink,??
    //nsIUploadChannel //TODO!!!
    //nsIPropertyBag2
    //nsIXPCScriptable,
    //nsIXPConnectWrappedJS,
    //nsISecurityCheckedComponent,
    IxxmContext)
  private
    FOwner:nsISupports;
    FURI,FOrigURI:nsIURI;
    FURL:WideString;
    FLoadFlags:nsLoadFlags;
    FLoadGroup:nsILoadGroup;
    FListenerContext:nsISupports;
    FListener:nsIStreamListener;
    FCallBacks:nsIInterfaceRequestor;
    FConnected,FComplete,FHeaderSent:boolean;
    FStatus,FSuspendCount:integer;
    FStatusCode:word;
    FStatusText,FVerb,FProjectName,FFragmentName,FPageClass,FQueryString:string;
    FProjectEntry:TXxmProjectCacheEntry;
    FPage: IXxmFragment;
    FAutoEncoding: TXxmAutoEncoding;
    FRequestHeaders,FResponseHeaders:TxxmDictionary;
    FData:TxxmInputStream;
    FSingleFileSent: WideString;
    procedure CheckHeader(Sent:boolean);
    procedure CheckSuspend;
    procedure SendRaw(Data: WideString);
    procedure SendError(res:string;vals:array of string);
  protected
    //nsIInterfaceRequestor
    //procedure nsGetInterface(const uuid: TGUID; out _result); safecall;
    //procedure nsIInterfaceRequestor.GetInterface=nsGetInterface;
    //nsIRequest
    procedure GetName(aName: nsAUTF8String); safecall;
    function IsPending: PRBool; safecall;
    function GetStatus: nsresult; safecall;
    property Status: nsresult read GetStatus;
    procedure Cancel(aStatus: nsresult); safecall;
    procedure Suspend; safecall;
    procedure Resume; safecall;
    function GetLoadGroup: nsILoadGroup; safecall;
    procedure SetLoadGroup(aLoadGroup: nsILoadGroup); safecall;
    property LoadGroup: nsILoadGroup read GetLoadGroup write SetLoadGroup;
    function GetLoadFlags: nsLoadFlags; safecall;
    procedure SetLoadFlags(aLoadFlags: nsLoadFlags); safecall;
    property LoadFlags: nsLoadFlags read GetLoadFlags write SetLoadFlags;
    //nsIChannel
    function Open: nsIInputStream; safecall;
    procedure AsyncOpen(aListener: nsIStreamListener;
      aContext: nsISupports); safecall;
    function GetOriginalURI: nsIURI; safecall;
    procedure SetOriginalURI(aOriginalURI: nsIURI); safecall;
    function GetURI: nsIURI; safecall;
    function GetOwner: nsISupports; safecall;
    procedure SetOwner(aOwner: nsISupports); safecall;
    function GetNotificationCallbacks: nsIInterfaceRequestor; safecall;
    procedure SetNotificationCallbacks(aNotificationCallbacks: nsIInterfaceRequestor);
      safecall;
    function GetSecurityInfo: nsISupports; safecall;
    procedure GetContentType(aContentType: nsACString); overload; safecall;
    procedure SetContentType(const aContentType: nsACString); overload; safecall;
    procedure GetContentCharset(aContentCharset: nsACString); safecall;
    procedure SetContentCharset(const aContentCharset: nsACString);
      safecall;
    function GetContentLength: Integer; safecall;
    procedure SetContentLength(aContentLength: Integer); safecall;
    //nsIHttpChannel
    procedure GetRequestMethod(aRequestMethod: nsACString); safecall;
    procedure SetRequestMethod(const aRequestMethod: nsACString); safecall;
    function GetReferrer: nsIURI; safecall;
    procedure SetReferrer(aReferrer: nsIURI); safecall;
    property Referrer: nsIURI read GetReferrer write SetReferrer;
    function GetRequestHeader(const aHeader: nsACString): nsACString; safecall;
    procedure SetRequestHeader(const aHeader: nsACString; const aValue: nsACString; aMerge: PRBool); safecall;
    procedure VisitRequestHeaders(aVisitor: nsIHttpHeaderVisitor); safecall;
    function GetAllowPipelining: PRBool; safecall;
    procedure SetAllowPipelining(aAllowPipelining: PRBool); safecall;
    property AllowPipelining: PRBool read GetAllowPipelining write SetAllowPipelining;
    function GetRedirectionLimit: PRUint32; safecall;
    procedure SetRedirectionLimit(aRedirectionLimit: PRUint32); safecall;
    property RedirectionLimit: PRUint32 read GetRedirectionLimit write SetRedirectionLimit;
    function GetResponseStatus: PRUint32; safecall;
    property ResponseStatus: PRUint32 read GetResponseStatus;
    procedure GetResponseStatusText(aResponseStatusText: nsACString); safecall;
    function GetRequestSucceeded: PRBool; safecall;
    property RequestSucceeded: PRBool read GetRequestSucceeded;
    function GetResponseHeader(const header: nsACString): nsACString; safecall;
    procedure SetResponseHeader(const header: nsACString; const value: nsACString; merge: PRBool); safecall;
    procedure VisitResponseHeaders(aVisitor: nsIHttpHeaderVisitor); safecall;
    function IsNoStoreResponse: PRBool; safecall;
    function IsNoCacheResponse: PRBool; safecall;
    //nsIUploadChannel
    //TODO:
    //IxxmContext
    function GetURL:WideString;
    function GetPage:IXxmFragment;
    function GetContentType:WideString; overload;
    procedure SetContentType(const Value: WideString); overload;
    function GetAutoEncoding:TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key:OleVariant):IXxmParameter;
    function GetParameterCount:integer;
    function GetSessionID:WideString;

    procedure Send(Data: OleVariant); overload;
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s:TStream); //TODO: IStream
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(FileName: WideString);

    function ContextString(cs:TXxmContextString):WideString;
    function PostData:TStream; //TODO: IStream
    function Connected:boolean;

    //(local:)progress
    procedure SetStatus(Code:integer;Text:WideString);
    procedure Redirect(RedirectURL:WideString; Relative:boolean);
    function GetCookie(Name:WideString):WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
    //procedure SetCookie2();

    //TODO: pointer to project?

    function QueryInterface2(const IID: TGUID; out Obj): HResult; stdcall;
    function IInterface.QueryInterface=QueryInterface2;
  public
    //
    Queue:TxxmChannel;//used by thread pool
    constructor Create(aURI: nsIURI);
    destructor Destroy; override;
    procedure Execute;
  end;

  TXxmGeckoLoader=class(TThread)
  private
    FInUse:boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property InUse:boolean read FInUse;
  end;

  TXxmGeckoLoaderPool=class(TObject)
  private
    FLoaders:array of TXxmGeckoLoader;
    FLoadersSize:integer;
    FLock:TRTLCriticalSection;
    FQueue:TxxmChannel;
    procedure SetSize(x:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Channel:TxxmChannel);//called from handler
    function Unqueue:TxxmChannel;//called from threads
  end;

  TxxmListenerCall=(lcActivate,lcStart,lcData,lcStop);

  TxxmListenerCaller=class(TInterfacedObject,
    nsIRunnable)
  private
    FOwner:TxxmChannel;
    FCall:TxxmListenerCall;
    FOffset,FCount:cardinal;
    debugid:integer;
  protected
    procedure run; safecall;
  public
    constructor Create(Owner:TxxmChannel;Call:TxxmListenerCall;
      Offset,Count:cardinal);
    destructor Destroy; override;
  end;

  EXxmContextStringUnknown=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);
  EXxmResponseHeaderNotSent=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmPageRedirected=class(Exception);
  EXxmIncludeStackFull=class(Exception);

const
  PoolMaxThreads=64;//TODO: from setting?

var
  GeckoLoaderPool:TXxmGeckoLoaderPool;
  //see xxmSettings
  StatusBuildError,StatusException,StatusFileNotFound:integer;
  DefaultProjectName:string;

implementation

uses ActiveX, Variants, Debug1, nsInit, xxmGeckoInterfaces;

resourcestring
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmResponseHeaderAlreadySent='Response header has already been sent.';
  SXxmResponseHeaderNotSent='Response header has not been sent.';
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

{ TxxmChannel }

constructor TxxmChannel.Create(aURI: nsIURI);
var
  x:IInterfacedUTF8String;
begin
  inherited Create;
  FOwner:=nil;
  FLoadGroup:=nil;
  FLoadFlags:=0;//?
  FListener:=nil;
  FCallBacks:=nil;
  FURI:=aURI;
  FOrigURI:=aURI;
  FData:=TxxmInputStream.Create;
  FRequestHeaders:=TxxmDictionary.Create;
  FResponseHeaders:=TxxmDictionary.Create;
  FSuspendCount:=1;//see AsyncOpen, TxxmListenerCaller/lcActivate
  FConnected:=false;//see AsyncOpen
  FComplete:=false;
  FHeaderSent:=false;
  FStatus:=NS_OK;
  FStatusCode:=200;
  FStatusText:='OK';
  FVerb:='GET';//default
  FResponseHeaders['Content-Type']:='text/html';//default (setting?)
  FResponseHeaders['Content-Charset']:='utf-8';//used by GetContentCharset/SetContentCharset
  FAutoEncoding:=aeUtf8;//default (setting?)
  x:=NewUTF8String;
  aURI.GetSpec(x.AUTF8String);
  FURL:=UTF8Decode(x.ToString);
  FProjectName:='';//parsed from URL
  FFragmentName:='';//parsed from URL
  FQueryString:='';//parsed from URL
  FProjectEntry:=nil;
  FPage:=nil;
  FPageClass:='';
  FSingleFileSent:='';
Debug('TxxmChannel.Create('+FURL);
end;

destructor TxxmChannel.Destroy;
begin
Debug('TxxmChannel.Destroying');
  if not(FProjectEntry=nil) then FProjectEntry.CloseContext;
  FProjectEntry:=nil;
  FPage:=nil;
  FOwner:=nil;
  FListenerContext:=nil;
  FListener:=nil;
  FCallBacks:=nil;
  FOrigURI:=nil;
  FURI:=nil;
  FData.Free;
  FRequestHeaders.Free;
  FResponseHeaders.Free;
  //TODO: clean-up
Debug('TxxmChannel.Destroy');
  inherited;
end;

function TxxmChannel.Open: nsIInputStream;
begin
  //deprecated: use AsyncOpen
  Result:=nil;
Debug('TxxmChannel.Open');
  raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.AsyncOpen(aListener: nsIStreamListener;
  aContext: nsISupports);
begin
Debug('>TxxmChannel.AsyncOpen');
  FConnected:=true;
  FListener:=aListener;
  FListenerContext:=aContext;
  //if not(FLoadGroup=nil) then FLoadGroup.AddRequest(Self as nsIRequest,nil);
  GeckoLoaderPool.Queue(Self);
  NS_DispatchToCurrentThread(TxxmListenerCaller.Create(Self,lcActivate,0,0));
Debug('<TxxmChannel.AsyncOpen');
end;

procedure TxxmChannel.Execute;
var
  i,j,l:integer;
  x:WideString;
begin
  //called from TXxmGeckoLoader
  try

    //TODO: use FLoadFlags?

    //parse URL
    //TODO: use FURI?
    l:=Length(FURL);
    i:=1;
    while (i<=l) and not(FURL[i]=':') do inc(i); //skip "xxm://"
    inc(i);
    if FURL[i]='/' then inc(i);
    if FURL[i]='/' then inc(i);
    j:=i;
    while (i<=l) and not(Char(FURL[i]) in ['/','?','&','$','#']) do inc(i);
    //if server then remote?
    FProjectName:=Copy(FURL,j,i-j);
    if FProjectName='' then
     begin
      FProjectName:=DefaultProjectName;
      FURL:=Copy(FURL,1,j-1)+FProjectName+Copy(FURL,i,Length(FURL)-i+1);
      FURI.SetSpec(NewCString(FURL).ACString);
     end;
    FPageClass:='['+FProjectName+']';
    if (i>l) then
     begin
      FURL:=FURL+'/';
      FURI.SetSpec(NewCString(FURL).ACString);
      inc(l);
     end;
    inc(i);

    j:=i;
    while (i<=l) and not(Char(FURL[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURL,j,i-j);
    if (FURL[i]='?') then inc(i);
    FQueryString:=Copy(FURL,i,l-i+1);

    //create page object
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;
    FProjectEntry:=XxmProjectCache.GetProject(FProjectName);
    if not(@XxmAutoBuildHandler=nil) then
      if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
        raise EXxmAutoBuildFailed.Create(FProjectName);
    FProjectEntry.OpenContext;
    //FPage:=FProjectEntry.Project.LoadPage(Self,FFragmentName);
FPage:=nil;

    if FPage=nil then
     begin
      //find a file
      //ask project to translate? project should have given a fragment!
      FPageClass:='['+FProjectName+']GetFilePath';
      FProjectEntry.GetFilePath(FFragmentName,FSingleFileSent,x);
      if FileExists(FSingleFileSent) then
       begin
        //TODO: if directory file-list?
        FResponseHeaders['Content-Type']:=x;
        if FConnected then SendFile(FSingleFileSent);
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
            'URL',HTMLEncode(FURL),
            'PROJECT',FProjectName,
            'ADDRESS',FFragmentName,
            'PATH',HTMLEncode(FSingleFileSent),
            'VERSION',ContextString(csVersion)
          ]);
         end
        else
{
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
}
       end;
     end
    else
{      try
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
};

  except
{//TODO
    on EXxmPageRedirected do
     begin
      FStatusCode:=301;//??
      FStatusText:='Moved Permanently';
      //SendHTML('Redirected to <a href=""></a>')?
     end;
}

{//TODO
    on EXxmAutoBuildFailed do
     begin
      //assert AutoBuild handler already displays message
      StatusSet:=true;
      FStatusCode:=StatusBuildError;
      FStatusText:='BUILDFAILED';
     end;
}

    on e:Exception do
     begin
Debug('ex:'+e.ClassName+':'+e.Message);
      FStatusCode:=StatusException;
      FStatusText:='ERROR';
      //TODO: get fragment 500.xxm?
      try
x:='//TODO';      
        //if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
      except
        x:='unknown';
      end;
      SendError('error',[
        'URL',HTMLEncode(FURL),
        'CLASS',FPageClass,
        'POSTDATA',x,
        'QUERYSTRING',FQueryString,
        'ERROR',HTMLEncode(e.Message),
        'ERRORCLASS',e.ClassName,
        'VERSION',ContextString(csVersion)
      ]);
     end;
  end;

  try
    if FConnected then
     begin
      CheckSuspend;
      NS_DispatchToMainThread(TxxmListenerCaller.Create(Self,lcStop,0,0));
     end;
  except
    //silent!
  end;

  //if not(FLoadGroup=nil) then FLoadGroup.RemoveRequest(Self as nsIRequest,nil,NS_OK);
  FComplete:=true;
  //TODO: FProjectEntry.CloseContext?
Debug('execute done');
end;

procedure TxxmChannel.GetName(aName: nsAUTF8String);
var
  x:UTF8String;
begin
Debug('TxxmChannel.GetName');
  x:=UTF8Encode(FURL);
  NS_CStringSetData(aName,PAnsiChar(x),Length(x));
end;

function TxxmChannel.GetURI: nsIURI;
begin
Debug('TxxmChannel.GetURI');
  Result:=FURI;
end;

function TxxmChannel.GetRequestSucceeded: PRBool;
begin
Debug('TxxmChannel.GetRequestSucceeded');
  //TODO
  Result:=FStatusCode=200;//FStatus=NS_OK?
  //Result:=FStatusCode in [200,404,500]??
end;

function TxxmChannel.GetStatus: nsresult;
begin
  Result:=FStatus;
end;

function TxxmChannel.GetResponseStatus: PRUint32;
begin
Debug('TxxmChannel.GetResponseStatus');
  //CheckHeader(true);?
  Result:=FStatusCode;
end;

procedure TxxmChannel.GetResponseStatusText(
  aResponseStatusText: nsACString);
begin
Debug('TxxmChannel.GetResponseStatusText');
  //CheckHeader(true);?
  NS_CStringSetData(aResponseStatusText,PAnsiChar(FStatusText),Length(FStatusText));
end;

function TxxmChannel.GetSecurityInfo: nsISupports;
begin
  Result:=nil;
  //TODO: find out more!
end;

function TxxmChannel.IsNoCacheResponse: PRBool;
begin
Debug('TxxmChannel.IsNoCacheResponse');
  Result:=true;
end;

function TxxmChannel.IsNoStoreResponse: PRBool;
begin
Debug('TxxmChannel.IsNoStoreResponse');
  //TODO
  Result:=true;
end;

procedure TxxmChannel.Cancel(aStatus: nsresult);
begin
  //TODO: test here
Debug('TxxmChannel.Cancel');
  FConnected:=false;
end;

function TxxmChannel.GetAllowPipelining: PRBool;
begin
  //TODO: ??
  Result:=false;
end;

procedure TxxmChannel.SetAllowPipelining(aAllowPipelining: PRBool);
begin
  //TODO:
  raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.GetContentCharset(aContentCharset: nsACString);
var
  x:string;
begin
  x:=FResponseHeaders['Content-Charset'];
  NS_CStringSetData(aContentCharset,PAnsiChar(x),Length(x));
end;

procedure TxxmChannel.SetContentCharset(const aContentCharset: nsACString);
begin
  CheckHeader(false);
  FAutoEncoding:=aeContentDefined;
  FResponseHeaders['Content-Charset']:=GetCString(aContentCharset);
end;

function TxxmChannel.GetContentLength: Integer;
begin
  //TODO:
  if FData=nil then Result:=-1 else Result:=FData.TotalSize;
Debug('TxxmChannel.GetContentLength:'+IntToStr(Result));
end;

procedure TxxmChannel.SetContentLength(aContentLength: Integer);
begin
  //TODO:
  raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.GetContentType(aContentType: nsACString);
begin
  //TODO:
  Debug('TxxmChannel.GetContentType('+FResponseHeaders['Content-Type']);
  SetCString(aContentType,FResponseHeaders['Content-Type']);
end;

procedure TxxmChannel.SetContentType(const aContentType: nsACString);
begin
  Debug('TxxmChannel.SetContentType('+GetCString(aContentType));
  FResponseHeaders['Content-Type']:=GetCString(aContentType);
  //TODO:
  raise EInvalidOperation.Create('Not implemented');
end;

function TxxmChannel.GetLoadFlags: nsLoadFlags;
begin
Debug('TxxmChannel.GetLoadFlags('+IntToHex(FLoadFlags,8));
  Result:=FLoadFlags;
end;

procedure TxxmChannel.SetLoadFlags(aLoadFlags: nsLoadFlags);
begin
Debug('TxxmChannel.SetLoadFlags('+IntToHex(aLoadFlags,8));
  FLoadFlags:=aLoadFlags;
end;

function TxxmChannel.GetLoadGroup: nsILoadGroup;
begin
Debug('TxxmChannel.GetLoadGroup');
  Result:=FLoadGroup;
end;

procedure TxxmChannel.SetLoadGroup(aLoadGroup: nsILoadGroup);
begin
Debug('TxxmChannel.SetLoadGroup');
  //TODO: unregister if not nil?
  FLoadGroup:=aLoadGroup;
end;

function TxxmChannel.GetNotificationCallbacks: nsIInterfaceRequestor;
begin
Debug('TxxmChannel.GetNotificationCallbacks');
  Result:=nil;//FNotificationCallbacks;
end;

procedure TxxmChannel.SetNotificationCallbacks(
  aNotificationCallbacks: nsIInterfaceRequestor);
begin
Debug('TxxmChannel.SetNotificationCallbacks');
  //if aNotificationCallbacks.GetInterface()=NS_NOINTERFACE then :=nil;
  //nsIProgressEventSink, nsIPrompt, nsIAuthPrompt/nsIAuthPrompt2
  //TODO: store this ref!
end;

function TxxmChannel.GetOriginalURI: nsIURI;
begin
  Result:=FOrigURI;
end;

procedure TxxmChannel.SetOriginalURI(aOriginalURI: nsIURI);
var
  x:IInterfacedCString;
begin
  FOrigURI:=aOriginalURI;
x:=NewCString;
FOrigURI.GetSpec(x.ACString);
Debug('TxxmChannel.SetOriginalURI('+x.ToString);
end;

function TxxmChannel.GetOwner: nsISupports;
begin
  Result:=FOwner;
end;

procedure TxxmChannel.SetOwner(aOwner: nsISupports);
begin
  FOwner:=aOwner;
end;

function TxxmChannel.GetRedirectionLimit: PRUint32;
begin
Debug('TxxmChannel.GetRedirectionLimit');
end;

procedure TxxmChannel.SetRedirectionLimit(aRedirectionLimit: PRUint32);
begin
Debug('TxxmChannel.SetRedirectionLimit');
end;

function TxxmChannel.GetReferrer: nsIURI;
begin
Debug('TxxmChannel.GetReferrer');
  //TODO
  Result:=nil;
end;

procedure TxxmChannel.SetReferrer(aReferrer: nsIURI);
begin
Debug('TxxmChannel.SetReferrer');
  //TODO
end;

function TxxmChannel.GetRequestHeader(
  const aHeader: nsACString): nsACString;
begin
Debug('TxxmChannel.GetRequestHeader('+GetCString(aHeader)+','+FRequestHeaders[GetCString(aHeader)]);
  SetCString(Result,FRequestHeaders[GetCString(aHeader)]);
end;

procedure TxxmChannel.SetRequestHeader(const aHeader, aValue: nsACString;
  aMerge: PRBool);
begin
Debug('TxxmChannel.SetRequestHeader('+GetCString(aHeader)+','+GetCString(aValue));
  if not(aMerge) then raise Exception.Create('set header without merge not supported');
  FRequestHeaders[GetCString(aHeader)]:=GetCString(aValue);
end;

procedure TxxmChannel.GetRequestMethod(aRequestMethod: nsACString);
begin
Debug('TxxmChannel.GetRequestMethod('+FVerb);
  SetCString(aRequestMethod,FVerb);
end;

procedure TxxmChannel.SetRequestMethod(const aRequestMethod: nsACString);
begin
  FVerb:=GetCString(aRequestMethod);
Debug('TxxmChannel.SetRequestMethod('+FVerb);
end;

function TxxmChannel.GetResponseHeader(
  const header: nsACString): nsACString;
begin
Debug('TxxmChannel.GetResponseHeader('+GetCString(header)+'='+FResponseHeaders[GetCString(header)]);
  SetCString(Result,FResponseHeaders[GetCString(header)]);
end;

procedure TxxmChannel.SetResponseHeader(const header, value: nsACString;
  merge: PRBool);
begin
Debug('TxxmChannel.SetResponseHeader('+GetCString(header)+','+GetCString(value));
  if not(merge) then raise Exception.Create('set header without merge not supported');
  FResponseHeaders[GetCString(header)]:=GetCString(value);
end;

function TxxmChannel.IsPending: PRBool;
begin
Debug('TxxmChannel.IsPending');
  Result:=FComplete;
end;

procedure TxxmChannel.Suspend;
begin
Debug('TxxmChannel.Suspend');
  InterlockedIncrement(FSuspendCount);
  //raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.Resume;
begin
Debug('TxxmChannel.Resume');
  if FSuspendCount<=0 then raise Exception.Create('Can''t resume, not suspended') else
    InterlockedDecrement(FSuspendCount);
  //raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.VisitRequestHeaders(aVisitor: nsIHttpHeaderVisitor);
begin
Debug('TxxmChannel.VisitRequestHeaders');
end;

procedure TxxmChannel.VisitResponseHeaders(aVisitor: nsIHttpHeaderVisitor);
begin
Debug('TxxmChannel.VisitResponseHeaders');
end;

//IxxmContext

function TxxmChannel.Connected: boolean;
begin
  //TODO: test this!
  Result:=FConnected;
end;

function TxxmChannel.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:
      Result:=SelfVersion;
    csExtraInfo:
      Result:='';//???
    csVerb:
      Result:=FVerb;
    csQueryString:
      Result:=FQueryString;
    csUserAgent:
      Result:=FRequestHeaders['user-agent'];
    csAcceptedMimeTypes:
      Result:=FRequestHeaders['accept-mime-type'];
    csPostMimeType:
      Result:=FRequestHeaders['content-type'];
    csURL:
      Result:=FURL;//FURI.GetSpec?
    csReferer:
      Result:=FRequestHeaders['referer'];
    csLanguage:
      Result:=FRequestHeaders['accept-language'];
    csRemoteAddress:
      Result:='127.0.0.1';//TODO: IPV6?
    csRemoteHost:
      Result:='localhost';
    csAuthUser:
      //TODO: GetUserNameEx?
      Result:=GetEnvironmentVariable('USERDOMAIN')+'\'+GetEnvironmentVariable('USERNAME');
    csAuthPassword:
      Result:='';
    csProjectName:
      Result:=FProjectName;
    csLocalURL:
      Result:=FFragmentName;
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TxxmChannel.DispositionAttach(FileName: WideString);
begin
  //TODO:
end;

function TxxmChannel.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

function TxxmChannel.GetContentType: WideString;
begin
  Result:=FResponseHeaders['Content-Type'];
end;

function TxxmChannel.GetCookie(Name: WideString): WideString;
begin
  //TODO:
end;

function TxxmChannel.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TxxmChannel.GetParameter(Key: OleVariant): IXxmParameter;
begin
  //TODO:
end;

function TxxmChannel.GetParameterCount: integer;
begin
  //TODO:
  Result:=0;
end;

function TxxmChannel.GetSessionID: WideString;
begin
  CheckHeader(true);
  Result:=IntToHex(HInstance,8)+IntToHex(GetCurrentProcessId,8);
end;

function TxxmChannel.GetURL: WideString;
begin
  Result:=FURL;
end;

procedure TxxmChannel.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
begin
  //TODO:
end;

procedure TxxmChannel.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  //TODO:
end;

procedure TxxmChannel.Include(Address: WideString);
begin
  //TODO:
end;

function TxxmChannel.PostData: TStream;
begin
  //TODO:
  Result:=nil;
end;

procedure TxxmChannel.Redirect(RedirectURL: WideString; Relative: boolean);
begin
  //TODO:

end;

procedure TxxmChannel.SendHTML(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(VarToWideStr(Data));
end;

procedure TxxmChannel.Send(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(HTMLEncode(VarToWideStr(Data)));
end;

procedure TxxmChannel.SendFile(FilePath: WideString);
begin
  //TODO:

end;

procedure TxxmChannel.SendStream(s: TStream);
begin
  //TODO:

end;

procedure TxxmChannel.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  CheckHeader(false);
  FAutoEncoding:=Value;
  case FAutoEncoding of
    aeUtf8:FResponseHeaders['Content-Charset']:='utf-8';
    aeUtf16:FResponseHeaders['Content-Charset']:='utf-16';
    aeIso8859:FResponseHeaders['Content-Charset']:='iso-8859-15';//-1?
    aeContentDefined:FResponseHeaders['Content-Charset']:='';//??
  end;
end;

procedure TxxmChannel.SetContentType(const Value: WideString);
begin
  CheckHeader(false);
  FResponseHeaders['Content-Type']:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

procedure TxxmChannel.SetCookie(Name, Value: WideString);
begin
  //TODO:
end;

procedure TxxmChannel.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
begin
  //TODO:
end;

procedure TxxmChannel.SetStatus(Code: integer; Text: WideString);
begin
  CheckHeader(false);
  FStatusCode:=Code;
  FStatusText:=Text;
end;

procedure TxxmChannel.CheckHeader(Sent: boolean);
begin
  if not(FHeaderSent=Sent) then
    if Sent then
      raise EXxmResponseHeaderNotSent.Create(SXxmResponseHeaderNotSent)
    else
      raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

{
procedure TxxmChannel.nsGetInterface(const uuid: TGUID; out _result);
begin
  if not(GetInterface(uuid,_result)) then
    raise EIntfCastError.Create('Interface not supported');
end;
}

function TxxmChannel.QueryInterface2(const IID: TGUID; out Obj): HResult;
begin
  Result:=QueryInterface(IID,Obj);
Debug('TxxmChannel.QueryInterface('+GUIDToString(IID)+')'+IntToHex(Result,8));
end;

procedure TxxmChannel.Send(Value: int64);
begin
  //TODO:

end;

procedure TxxmChannel.Send(Value: integer);
begin
  //TODO:

end;

procedure TxxmChannel.Send(const Values: array of OleVariant);
begin
  //TODO:

end;

procedure TxxmChannel.Send(Value: cardinal);
begin
  //TODO:

end;

procedure TxxmChannel.SendHTML(const Values: array of OleVariant);
begin
  //TODO:

end;

procedure TxxmChannel.CheckSuspend;
begin
  while not(FSuspendCount=0) do Sleep(5);
end;

const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;

procedure TxxmChannel.SendRaw(Data: WideString);
var
  s:string;
  startdata:boolean;
begin
  inherited;
  if not(Data='') then
   begin
    startdata:=not(FHeaderSent);
    if startdata then //do this outside of lock
      if FConnected then
       begin
        CheckSuspend;
        NS_DispatchToMainThread(TxxmListenerCaller.Create(Self,lcStart,0,0));
       end;
    FData.Lock;
    try
      if startdata then
       begin
        FHeaderSent:=true;
        case FAutoEncoding of
          aeUtf8:FData.Write(Utf8ByteOrderMark,3);
          aeUtf16:FData.Write(Utf16ByteOrderMark,2);
        end;
       end;

      case FAutoEncoding of
        aeUtf16:FData.Write(Data[1],Length(Data)*2);
        aeUtf8:
         begin
          s:=UTF8Encode(Data);
          FData.Write(s[1],Length(s));
         end;
        else
         begin
          s:=Data;
          FData.Write(s[1],Length(s));
         end;
        end;
      if FConnected and not(FData.ReportPending) then
       begin
        CheckSuspend;//is this no problem inside of lock?
        FData.ReportPending:=true;
        NS_DispatchToMainThread(TxxmListenerCaller.Create(Self,lcData,0,FData.ReportSize));
       end;
    finally
      FData.Unlock;
    end;
   end;
end;

procedure TxxmChannel.SendError(res: string; vals: array of string);
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
    FResponseHeaders['Content-Type']:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

{ TXxmGeckoLoader }

constructor TXxmGeckoLoader.Create;
begin
  inherited Create(false);
  //FInUse:=false;
end;

procedure TXxmGeckoLoader.Execute;
var
  Channel:TxxmChannel;
begin
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Channel:=GeckoLoaderPool.Unqueue;
    if Channel=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      Suspend;
      FInUse:=true;
     end
    else
     begin
      Sleep(10);//let AsyncOpen return...
      Channel.Execute;//assert all exceptions handled!
      //Channel._Release;
     end;
   end;
  CoUninitialize;
end;

{ TXxmGeckoLoaderPool }

constructor TXxmGeckoLoaderPool.Create;
begin
  inherited Create;
  FLoadersSize:=0;
  FQueue:=nil;
  InitializeCriticalSection(FLock);
  SetSize(PoolMaxThreads);//TODO: setting
  //TODO: setting no pool
end;

destructor TXxmGeckoLoaderPool.Destroy;
begin
  SetSize(0);
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmGeckoLoaderPool.SetSize(x: integer);
begin
  EnterCriticalSection(FLock);
  try
    if FLoadersSize<x then
     begin
      SetLength(FLoaders,x);
      while not(FLoadersSize=x) do
       begin
        FLoaders[FLoadersSize]:=nil;
        inc(FLoadersSize);
       end;
     end
    else
     begin
      while not(FLoadersSize=X) do
       begin
        dec(FLoadersSize);
        //FreeAndNil(FLoaders[FLoadersSize]);
        if not(FLoaders[FLoadersSize]=nil) then
         begin
          FLoaders[FLoadersSize].FreeOnTerminate:=true;
          FLoaders[FLoadersSize].Terminate;
          FLoaders[FLoadersSize].Resume;
          FLoaders[FLoadersSize]:=nil;
         end;
       end;
      SetLength(FLoaders,x);
     end;
    //if FLoaderIndex>=FLoadersSize then FLoaderIndex:=0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmGeckoLoaderPool.Queue(Channel: TxxmChannel);
var
  c:TxxmChannel;
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //add to queue
    Channel._AddRef;
    if FQueue=nil then FQueue:=Channel else
     begin
      c:=FQueue;
      while not(c.Queue=nil) do c:=c.Queue;
      c.Queue:=Channel;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FLoadersSize) and not(FLoaders[i]=nil) and FLoaders[i].InUse do inc(i);
  if i=FLoadersSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FLoaders[i]=nil then
      FLoaders[i]:=TxxmGeckoLoader.Create //start thread
    else
      FLoaders[i].Resume; //resume on waiting unqueues
    //TODO: expire unused threads on low load
   end;
end;

function TXxmGeckoLoaderPool.Unqueue: TxxmChannel;
begin
  if FQueue=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueue;
      if not(Result=nil) then
       begin
        FQueue:=FQueue.Queue;
        Result.Queue:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

{ TxxmListenerCaller }

const
  lcName:array[TxxmListenerCall] of string=(
    'Activate',
    'OnStartRequest',
    'OnDataAvailable',
    'OnStopRequest');

var
  DebugListenerCount: integer;

constructor TxxmListenerCaller.Create(Owner: TxxmChannel;
  Call: TxxmListenerCall;Offset,Count:cardinal);
begin
  inherited Create;
  FOwner:=Owner;
  FCall:=Call;
  FOffset:=Offset;
  FCount:=Count;
  inc(DebugListenerCount);
  debugid:=DebugListenerCount;
  Debug('Q>'+lcName[FCall]+' '+IntToStr(debugid));
end;

destructor TxxmListenerCaller.Destroy;
begin
  Debug('Q<'+lcName[FCall]+' '+IntToStr(debugid));
  inherited;
end;

procedure TxxmListenerCaller.run;
begin
Debug('>'+lcName[FCall]+' '+IntToStr(debugid));
try
  case FCall of
    lcActivate:FOwner.Resume;
    lcStart:FOwner.FListener.OnStartRequest(FOwner,FOwner.FListenerContext);
    lcData:FOwner.FListener.OnDataAvailable(FOwner,FOwner.FListenerContext,FOwner.FData,FOffset,FCount);
    lcStop:FOwner.FListener.OnStopRequest(FOwner,FOwner.FListenerContext,FOwner.FStatusCode);
  end;
Debug('<'+lcName[FCall]+' '+IntToStr(debugid));
except
  on e:Exception do
Debug('<'+lcName[FCall]+' '+IntToStr(debugid)+' !!! '+e.ClassName+':'+e.Message);
end;
end;

initialization
  GeckoLoaderPool:=TXxmGeckoLoaderPool.Create;
finalization
  FreeAndNil(GeckoLoaderPool);
end.
