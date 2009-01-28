unit xxmGeckoChannel;

interface

uses nsXPCOM, nsTypes, nsGeckoStrings, xxm, Windows, Classes, SysUtils,
  xxmDictionary, xxmGeckoStreams;

type
  TxxmChannel=class(TInterfacedObject,
    IInterface,
    nsIRequest,
    nsIChannel,
    nsIHttpChannel,
    //nsIInterfaceRequestor,
    //nsITransportEventSink,??
    //nsIUploadChannel
    //nsIPropertyBag2
    IxxmContext)
  private
{
  nsRefPtr<nsInputStreamPump>         mPump;
  nsCOMPtr<nsIInterfaceRequestor>     mCallbacks;
  nsCOMPtr<nsIProgressEventSink>      mProgressSink;
  nsCOMPtr<nsIURI>                    mOriginalURI;
  nsCOMPtr<nsIURI>                    mURI;
  nsCOMPtr<nsILoadGroup>              mLoadGroup;
  nsCOMPtr<nsISupports>               mOwner;
  nsCOMPtr<nsISupports>               mSecurityInfo;
  nsCOMPtr<nsIStreamListener>         mListener;
  nsCOMPtr<nsISupports>               mListenerContext;
  nsCString                           mContentType;
  nsCString                           mContentCharset;
  PRUint32                            mLoadFlags;
  nsresult                            mStatus;
  PRPackedBool                        mQueriedProgressSink;
  PRPackedBool                        mSynthProgressEvents;
  PRPackedBool                        mWasOpened;
}  
    FOwner:nsISupports;
    FURI,FOrigURI:nsIURI;
    FURL:WideString;
    FLoadFlags:nsLoadFlags;
    FLoadGroup:nsILoadGroup;
    FListenerContext:nsISupports;
    FListener:nsIStreamListener;
    FConnected,FComplete,FHeaderSent:boolean;
    FStatusCode:word;
    FStatusText,FVerb:string;
    FAutoEncoding: TXxmAutoEncoding;
    FRequestHeaders,FResponseHeaders:TxxmDictionary;
    FReturnString:IInterfacedCString;
    FData:TxxmInputStream;
    procedure CheckHeader(Sent:boolean);
  protected
    //nsIInterfaceRequestor
    //procedure nsGetInterface(const uuid: TGUID; out _result); safecall;
    //procedure nsIInterfaceRequestor.GetInterface=nsGetInterface;
    //nsIRequest
    procedure GetName(aName: nsACString); safecall;
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

    procedure Send(Data: OleVariant);
    procedure SendHTML(Data: OleVariant);
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
{
  TXxmAutoBuildHandler=function(pce:TXxmProjectCacheEntry;
    Context: IXxmContext; ProjectName:WideString):boolean;
}
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
//  XxmAutoBuildHandler:TXxmAutoBuildHandler;

implementation

uses ActiveX, Math, Debug1, nsThreadUtils;

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
  x:IInterfacedCString;
begin
  inherited Create;
  //TODO:
  FOwner:=nil;
  FLoadGroup:=nil;
  FListener:=nil;
  FURI:=aURI;
  FOrigURI:=aURI;
  FData:=TxxmInputStream.Create;
  FRequestHeaders:=TxxmDictionary.Create;
  FResponseHeaders:=TxxmDictionary.Create;
  FConnected:=false;//see AsyncOpen
  FComplete:=false;
  FHeaderSent:=false;
  FStatusCode:=0;//200;
  FStatusText:='OK';
  FVerb:='GET';//default
  FResponseHeaders['Content-Type']:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  x:=NewCString;
  aURI.GetSpec(x.ACString);
  FURL:=x.ToString;
Debug('TxxmChannel.Create('+FURL);
end;

destructor TxxmChannel.Destroy;
begin
Debug('TxxmChannel.Destroying');
  FOwner:=nil;
  FListenerContext:=nil;
  FListener:=nil;
  FOrigURI:=nil;
  FURI:=nil;
  FReturnString:=nil;
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
  FConnected:=true;
  FListener:=aListener;
  FListenerContext:=aContext;
Debug('TxxmChannel.AsyncOpen');
  if not(FLoadGroup=nil) then FLoadGroup.AddRequest(Self as nsIRequest,FListenerContext);

  GeckoLoaderPool.Queue(Self);
end;

procedure TxxmChannel.Execute;
begin
  //called from TXxmGeckoLoader
  try

    //TODO: use FLoadFlags
    //TODO: catch suspend/resume

    FData.Data.LoadFromFile('C:\temp\test.html');

Debug('pre OnStartRequest');
    if FConnected then FListener.OnStartRequest(Self as nsIRequest,FListenerContext);
Debug('post OnStartRequest');

Sleep(2000);

NS_ProcessNextEvent(nil,false);

Debug('pre OnDataAvailable');
    FListener.OnDataAvailable(Self as nsIRequest,FListenerContext,FData,0,FData.Data.Size);
Debug('post OnDataAvailable');

  except
    on e:Exception do
     begin
      //TODO:
Debug('ex:'+e.ClassName+':'+e.Message);     
     end;
  end;

  try
Debug('pre OnStopRequest');
    if FConnected then FListener.OnStopRequest(Self as nsIRequest,FListenerContext,FStatusCode);
Debug('post OnStopRequest');
  except
    //silent
  end;

Debug('execute done');
end;

procedure TxxmChannel.GetName(aName: nsACString);
begin
Debug('TxxmChannel.GetName');
  NewCString(aName).Assign(FURL);
end;

function TxxmChannel.GetURI: nsIURI;
begin
Debug('TxxmChannel.GetURI');
  Result:=FURI;
end;

function TxxmChannel.GetRequestSucceeded: PRBool;
begin
Debug('TxxmChannel.GetRequestSucceeded');
  Result:=true;//TODO
  //Result:=FStatusCode in [200,404,500]??
end;

function TxxmChannel.GetStatus: nsresult;
begin
  Result:=0;//NS_OK;
end;

function TxxmChannel.GetResponseStatus: PRUint32;
begin
Debug('TxxmChannel.GetResponseStatus');
  //TODO?
  //CheckHeader(true);
  Result:=FStatusCode;
end;

procedure TxxmChannel.GetResponseStatusText(
  aResponseStatusText: nsACString);
begin
Debug('TxxmChannel.GetResponseStatusText');
  //TODO?
  //CheckHeader(true);
  NewCString(aResponseStatusText).Assign(FStatusText);
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
  Result:=false;
end;

procedure TxxmChannel.Cancel(aStatus: nsresult);
begin
  //TODO: test here
Debug('TxxmChannel.Cancel');
  FConnected:=false;
end;

function TxxmChannel.GetAllowPipelining: PRBool;
begin
  //TODO:
  Result:=false;
end;

procedure TxxmChannel.SetAllowPipelining(aAllowPipelining: PRBool);
begin
  //TODO:
  raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.GetContentCharset(aContentCharset: nsACString);
begin
  //TODO:
  NewCString(aContentCharset).Assign('utf-8');
end;

procedure TxxmChannel.SetContentCharset(const aContentCharset: nsACString);
begin
  //TODO:
  raise EInvalidOperation.Create('Not implemented');
end;

function TxxmChannel.GetContentLength: Integer;
begin
  //TODO:
  if FData=nil then Result:=-1 else Result:=FData.Data.Size;
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
  NewCString(aContentType).Assign('text/html');
end;

procedure TxxmChannel.SetContentType(const aContentType: nsACString);
begin
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
Debug('TxxmChannel.GetRequestHeader('+NewCString(aHeader).ToString);
  FReturnString:=NewCString;
  FReturnString.Assign(FRequestHeaders[NewCString(aHeader).ToString]);
  Result:=FReturnString.ACString;
end;

procedure TxxmChannel.SetRequestHeader(const aHeader, aValue: nsACString;
  aMerge: PRBool);
begin
Debug('TxxmChannel.SetRequestHeader('+NewCString(aHeader).ToString+','+NewCString(aValue).ToString);
  if not(aMerge) then raise Exception.Create('set header without merge not supported');
  FRequestHeaders[NewCString(aHeader).ToString]:=NewCString(aValue).ToString;
end;

procedure TxxmChannel.GetRequestMethod(aRequestMethod: nsACString);
begin
Debug('TxxmChannel.GetRequestMethod('+FVerb);
  NewCString(aRequestMethod).Assign(FVerb);
end;

procedure TxxmChannel.SetRequestMethod(const aRequestMethod: nsACString);
begin
  FVerb:=NewCString(aRequestMethod).ToString;
Debug('TxxmChannel.SetRequestMethod('+FVerb);
end;

function TxxmChannel.GetResponseHeader(
  const header: nsACString): nsACString;
begin
Debug('TxxmChannel.GetResponseHeader('+NewCString(header).ToString);
  FReturnString:=NewCString;
  FReturnString.Assign(FResponseHeaders[NewCString(header).ToString]);
  Result:=FReturnString.ACString;
end;

procedure TxxmChannel.SetResponseHeader(const header, value: nsACString;
  merge: PRBool);
begin
Debug('TxxmChannel.SetResponseHeader('+NewCString(header).ToString+','+NewCString(value).ToString);
  if not(merge) then raise Exception.Create('set header without merge not supported');
  FResponseHeaders[NewCString(header).ToString]:=NewCString(value).ToString;
end;

function TxxmChannel.IsPending: PRBool;
begin
Debug('TxxmChannel.IsPending');
  Result:=FComplete;
end;

procedure TxxmChannel.Suspend;
begin
Debug('TxxmChannel.Suspend');
  //raise EInvalidOperation.Create('Not implemented');
end;

procedure TxxmChannel.Resume;
begin
Debug('TxxmChannel.Resume');
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

end;

procedure TxxmChannel.DispositionAttach(FileName: WideString);
begin

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

end;

function TxxmChannel.GetPage: IXxmFragment;
begin

end;

function TxxmChannel.GetParameter(Key: OleVariant): IXxmParameter;
begin

end;

function TxxmChannel.GetParameterCount: integer;
begin
  //TODO:
  Result:=0;
end;

function TxxmChannel.GetSessionID: WideString;
begin

end;

function TxxmChannel.GetURL: WideString;
begin
  Result:=FURL;
end;

procedure TxxmChannel.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
begin

end;

procedure TxxmChannel.Include(Address: WideString;
  const Values: array of OleVariant);
begin

end;

procedure TxxmChannel.Include(Address: WideString);
begin

end;

function TxxmChannel.PostData: TStream;
begin
  //TODO:
  Result:=nil;
end;

procedure TxxmChannel.Redirect(RedirectURL: WideString; Relative: boolean);
begin

end;

procedure TxxmChannel.Send(Data: OleVariant);
begin

end;

procedure TxxmChannel.SendFile(FilePath: WideString);
begin

end;

procedure TxxmChannel.SendHTML(Data: OleVariant);
begin

end;

procedure TxxmChannel.SendStream(s: TStream);
begin

end;

procedure TxxmChannel.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  CheckHeader(false);
  FAutoEncoding:=Value;
end;

procedure TxxmChannel.SetContentType(const Value: WideString);
begin
  CheckHeader(false);
  FResponseHeaders['Content-Type']:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

procedure TxxmChannel.SetCookie(Name, Value: WideString);
begin

end;

procedure TxxmChannel.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
begin

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
      Channel.Execute;//assert all exceptions handled!
      Channel._Release;
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

initialization
  GeckoLoaderPool:=TXxmGeckoLoaderPool.Create;
finalization
  FreeAndNil(GeckoLoaderPool);
end.
