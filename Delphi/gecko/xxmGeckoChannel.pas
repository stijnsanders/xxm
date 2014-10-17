unit xxmGeckoChannel;

interface

uses xxm, xxmContext,
  Windows, Classes, SysUtils, ActiveX, xxmHeaders, xxmParUtils,
  xxmPReg, xxmPRegLocal, xxmParams;

type
  TxxmChannel=class(TXxmGeneralContext,
    //IxxmContext,//see TXxmGeneralContext
    IxxmHttpHeaders)
  private
    FComplete,FGotSessionID:boolean;
    FPipePrefix,FVerb,FQueryString:AnsiString;
    FPipeIn,FPipeOut,FPipeCmd:THandle;
    FFlagUp,FFlagDown:THandle;
    FFlagMsg,FFlagValue:AnsiString;
    FRequestHeaders,FResponseHeaders:TResponseHeaders;//both TResponseHeaders?! see Create
    FCookie:AnsiString;
    FCookieIdx: TParamIndexes;
    FCookieParsed: boolean;
    FTotalSize,FOutputSize:int64;
    procedure FlagMsg(const msg: AnsiString);
    function FlagValue(const msg: AnsiString): AnsiString;
  protected
    //IxxmContext
    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;

    //other TXxmGeneralContext abstract methods
    function GetProjectEntry:TXxmProjectEntry; override;
    procedure SendHeader; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
    function GetRequestHeader(const Name: WideString): WideString; override;

    //IxxmHttpHeaders
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    //
    Queue:TxxmChannel;//used by thread pool
    constructor Create(aVerb, aURI: PAnsiChar);
    destructor Destroy; override;
    procedure Execute;
    property PipePrefix:AnsiString read FPipePrefix;
  end;

  TXxmGeckoLoader=class(TThread)
  private
    FInUse:boolean;
    FNextJobEvent:THandle;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SignalNextJob;
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

  EXxmContextStringUnknown=class(Exception);

const
  PoolMaxThreads=64;//TODO: setting?

var
  GeckoLoaderPool:TXxmGeckoLoaderPool;
  //see xxmSettings
  StatusBuildError,StatusException,StatusFileNotFound:integer;
  DefaultProjectName:AnsiString;

procedure SetThreadName(ThreadDisplayName:AnsiString);
function IsDebuggerPresent: BOOL; stdcall;

function xxmOpen(aVerb,aURI:PAnsiChar):TxxmChannel; stdcall;
function xxmPrefix(channel:TxxmChannel):PAnsiChar; stdcall;
function xxmNext(channel:TxxmChannel):PAnsiChar; stdcall;
procedure xxmSet(channel:TxxmChannel;value:PAnsiChar); stdcall;
procedure xxmClose(channel:TxxmChannel); stdcall;

exports
  xxmOpen,
  xxmPrefix,
  xxmNext,
  xxmSet,
  xxmClose;

implementation

uses Variants, xxmCommonUtils, ComObj;

resourcestring
  SXxmContextStringUnknown='Unknown ContextString __';

function IsDebuggerPresent; external 'kernel32.dll';

procedure SetThreadName(ThreadDisplayName:AnsiString);
var
  ThreadInfo:record
    dwType:LongWord;
    szName:PAnsiChar;
    dwThreadID:LongWord;
    dwFlags:LongWord;
  end;
begin
  if IsDebuggerPresent then
    begin
      ThreadInfo.dwType:=$1000;
      ThreadInfo.szName:=PAnsiChar(ThreadDisplayName);
      ThreadInfo.dwThreadID:=LongWord(-1);//calling thread
      ThreadInfo.dwFlags:=0;
      try
        RaiseException($406D1388,0,SizeOf(ThreadInfo) div SizeOf(LongWord),@ThreadInfo);
      except
        //
      end;
    end;
end;

function xxmOpen(aVerb,aURI:PAnsiChar):TxxmChannel; stdcall;
begin
  Result:=TxxmChannel.Create(aVerb,aURI);
  Result._AddRef;
  GeckoLoaderPool.Queue(Result);
end;

function xxmPrefix(channel:TxxmChannel):PAnsiChar; stdcall;
begin
  Result:=PAnsiChar(channel.PipePrefix);
  WaitForSingleObject(channel.FFlagDown,INFINITE);
end;

function xxmNext(channel:TxxmChannel):PAnsiChar; stdcall;
begin
  SetEvent(channel.FFlagDown);
  WaitForSingleObject(channel.FFlagUp,INFINITE);
  Result:=PAnsiChar(channel.FFlagMsg);
end;

procedure xxmSet(channel:TxxmChannel;value:PAnsiChar); stdcall;
begin
  channel.FFlagValue:=value;
  SetEvent(channel.FFlagDown);//see TxxmChannel.FlagValue
end;

procedure xxmClose(channel:TxxmChannel); stdcall;
begin
  WaitForSingleObject(channel.FFlagUp,INFINITE);
  Sleep(5);
  channel._Release;//channel.Free;
end;

var
  ChannelCounter:integer;

const
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;

{ TxxmChannel }

constructor TxxmChannel.Create(aVerb, aURI: PAnsiChar);
begin
  inherited Create(aURI);
  FVerb:=aVerb;
  FFlagUp:=CreateEvent(nil,false,false,nil);
  FFlagDown:=CreateEvent(nil,false,false,nil);
  FFlagMsg:='';
  FFlagValue:='';

  FPipePrefix:=Format('xxmGecko%.8x%.2x,%.4x',[GetTickCount,
    InterlockedIncrement(ChannelCounter) and $FF,
    integer(Self) and $FFFF]);

  FGotSessionID:=false;
  SendDirect:=SendData;
  FOutputSize:=0;
  FTotalSize:=0;
  //FRequestHeaders is TResponseHeaders because data is set later
  //and because TResponseHeaders has full IxxmDictionaryEx implementation
  FRequestHeaders:=TResponseHeaders.Create();
  (FRequestHeaders as IUnknown)._AddRef;
  FResponseHeaders:=TResponseHeaders.Create;
  (FResponseHeaders as IUnknown)._AddRef;
  FCookieParsed:=false;
  FComplete:=false;
  FResponseHeaders['Content-Type']:='text/html';//default (setting?)
  FResponseHeaders['Content-Charset']:='utf-8';//used by GetContentCharset/SetContentCharset
  FQueryString:='';//parsed from URL
end;

destructor TxxmChannel.Destroy;
begin
  CloseHandle(FFlagUp);
  CloseHandle(FFlagDown);
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
  CloseHandle(FPipeCmd);
  
  (FRequestHeaders as IUnknown)._Release;
  FRequestHeaders:=nil;
  (FResponseHeaders as IUnknown)._Release;
  FResponseHeaders:=nil;
  inherited;
end;

procedure TxxmChannel.FlagMsg(const msg:AnsiString);
begin
  WaitForSingleObject(FFlagDown,INFINITE);
  FFlagMsg:=msg;
  SetEvent(FFlagUp);
end;

function TxxmChannel.FlagValue(const msg:AnsiString):AnsiString;
begin
  WaitForSingleObject(FFlagDown,INFINITE);
  FFlagMsg:=msg;
  SetEvent(FFlagUp);
  //wait for xxmSet
  WaitForSingleObject(FFlagDown,INFINITE);
  Result:=FFlagValue;
end;

procedure TxxmChannel.Execute;
var
  i,j,l:integer;
  x:WideString;

  procedure RaiseLastOSErrorP;
  var
    r:integer;
  begin
    r:=GetLastError;
    if r<>ERROR_PIPE_CONNECTED then
      raise Exception.Create(SysErrorMessage(r));
  end;

begin
  FPipeIn:=CreateNamedPipeA(PAnsiChar('\\.\pipe\'+FPipePrefix+'_A'),
    PIPE_ACCESS_INBOUND or FILE_FLAG_FIRST_PIPE_INSTANCE,
    PIPE_TYPE_BYTE,1,0,$10000,1000,nil);
  FPipeOut:=CreateNamedPipeA(PAnsiChar('\\.\pipe\'+FPipePrefix+'_B'),
    PIPE_ACCESS_OUTBOUND or FILE_FLAG_FIRST_PIPE_INSTANCE,
    PIPE_TYPE_BYTE,1,$10000,0,1000,nil);
  FPipeCmd:=CreateNamedPipeA(PAnsiChar('\\.\pipe\'+FPipePrefix+'_C'),
    PIPE_ACCESS_OUTBOUND or FILE_FLAG_FIRST_PIPE_INSTANCE,
    PIPE_TYPE_BYTE,1,$10000,0,1000,nil);
  try
    //parse URL
    //TODO: use FURI?
    l:=Length(FURL);
    i:=1;
    while (i<=l) and (FURL[i]<>':') do inc(i); //skip "xxm://"
    inc(i);
    if FURL[i]='/' then inc(i);
    if FURL[i]='/' then inc(i);
    j:=i;
    while (i<=l) and not(char(FURL[i]) in ['/','?','&','$','#']) do inc(i);
    //if server then remote?
    FProjectName:=Copy(FURL,j,i-j);
    if FProjectName='' then
     begin
      FProjectName:=DefaultProjectName;
      FURL:=Copy(FURL,1,j-1)+FProjectName+Copy(FURL,i,Length(FURL)-i+1);
      //FURI.SetSpec(NewCString(FURL).ACString);
     end;
    FPageClass:='['+FProjectName+']';
    if (i>l) then
     begin
      FURL:=FURL+'/';
      //FURI.SetSpec(NewCString(FURL).ACString);
      inc(l);
     end;
    if (FURL[i]='/') then inc(i);

    j:=i;
    while (i<=l) and not(char(FURL[i]) in ['?','&','$','#']) do inc(i);
    FFragmentName:=Copy(FURL,j,i-j);
    if (FURL[i]='?') then inc(i);
    j:=i;
    while (j<=l) and (FURL[j]<>'#') do inc(j);
    FQueryString:=Copy(FURL,i,j-i);

    SetEvent(FFlagDown);//see xxmPrefix

    //TODO: FPostData

    //if not ConnectNamedPipe(FPipeIn,nil) then RaiseLastOSErrorP;
    if not ConnectNamedPipe(FPipeOut,nil) then RaiseLastOSErrorP;
    //if not ConnectNamedPipe(FPipeCmd,nil) then RaiseLastOSErrorP;

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
  FComplete:=true;

  FlagMsg('end');

  DisconnectNamedPipe(FPipeIn);
  DisconnectNamedPipe(FPipeOut);
  DisconnectNamedPipe(FPipeCmd);
  SetEvent(FFlagUp);//see xxmClose
end;

function TxxmChannel.GetProjectEntry:TXxmProjectEntry;
begin
  if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCacheLocal.Create;
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TxxmChannel.GetRequestHeader(const Name: WideString): WideString;
begin
  //inherited;
  Result:=FRequestHeaders[Name];
end;

procedure TxxmChannel.AddResponseHeader(const Name, Value: WideString);
begin
  FlagMsg('hdr'+Name+' '+Value);
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResponseHeaders.Add(Name,Value);
   end
  else
    FResponseHeaders[Name]:=Value;
end;

//IxxmContext

function TxxmChannel.Connected: boolean;
begin
  //TODO: catch nsIRequest.cancel()
  Result:=true;
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
      Result:=FlagValue('usa');
    csAcceptedMimeTypes:
      Result:=FRequestHeaders['accept-mime-type'];
    csPostMimeType:
      Result:=FRequestHeaders['content-type'];
    csURL:
      Result:=FURL;//FURI.GetSpec?
    csReferer:
      Result:=FlagValue('ref');//Result:=FRequestHeaders['referer'];
    csLanguage:
      Result:=FRequestHeaders['accept-language'];
    csRemoteAddress:
      Result:='127.0.0.1';//TODO: IPV6?
    csRemoteHost:
      Result:='localhost';
    csAuthUser:
      //TODO: GetUserNameEx?
      //TODO: get firefox profile descriptor?
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
var
  x:WideString;
  i:integer;
begin
  x:=FileName;
  for i:=1 to Length(x) do if x[i]='"' then x[i]:='_';
  FResponseHeaders['Content-Disposition']:='attachment; filname="'+x+'"';
end;

function TxxmChannel.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FRequestHeaders['Cookie'];
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

function TxxmChannel.GetSessionID: WideString;
begin
  if not FGotSessionID then CheckHeaderNotSent;
  FGotSessionID:=true;
  Result:=IntToHex(HInstance,8)+IntToHex(GetCurrentProcessId,8);
end;

procedure TxxmChannel.Redirect(RedirectURL: WideString; Relative: boolean);
begin
  //TODO:
  //if 307 then forward as POST else as GET? (see RedirectSync)
  CheckHeaderNotSent;
  if Relative then
    FlagMsg('rdl'+UTF8Encode(RedirectURL))
  else
    FlagMsg('rdr'+UTF8Encode(RedirectURL));
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TxxmChannel.SendHeader;
begin
  FlagMsg('hdr'+'Content-Type '+FContentType);
  case FAutoEncoding of
    aeUtf8:FlagMsg('hdr'+'Content-Charset '+'utf-8');
    aeUtf16:FlagMsg('hdr'+'Content-Charset '+'utf-16');
    aeIso8859:FlagMsg('hdr'+'Content-Charset '+'iso-8859-1');
    //else?
  end;
  ////FResponseHeaders['Content-Length']?
  FlagMsg('xxx');
  FResponseHeaders['Content-Type']:=FContentType;
  //TODO: +'; charset='+?
end;

function TxxmChannel.SendData(const Buffer; Count: LongInt): LongInt;
begin
  //assert within Lock/Unlock try/finally!
  if Count<>0 then
   begin
    FlagMsg('dta'+IntToStr(Count));
    if not WriteFile(FPipeOut,Buffer,Count,cardinal(Result),nil) then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end;
end;

function TxxmChannel.GetRequestHeaders: IxxmDictionaryEx;
begin
  Result:=FRequestHeaders;
end;

function TxxmChannel.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResponseHeaders;
end;

{ TXxmGeckoLoader }

constructor TXxmGeckoLoader.Create;
begin
  inherited Create(false);
  //FInUse:=false;
  FNextJobEvent:=CreateEventA(nil,true,false,
    PAnsiChar('xxmGecko:NextJob:'+IntToHex(GetCurrentThreadId,8)));
end;

destructor TXxmGeckoLoader.Destroy;
begin
  CloseHandle(FNextJobEvent);
  inherited;
end;

procedure TXxmGeckoLoader.Execute;
var
  Channel:TxxmChannel;
begin
  CoInitialize(nil);
  //SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Channel:=GeckoLoaderPool.Unqueue;
    if Channel=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      SetThreadName('(xxmPageLoader)');
      ResetEvent(FNextJobEvent);
      WaitForSingleObject(FNextJobEvent,INFINITE);
      FInUse:=true;
     end
    else
     begin
      Sleep(10);//let AsyncOpen return...
      SetThreadName('xxmPageLoader:'+Channel.FURL);
      Channel.Execute;//assert all exceptions handled!
      Channel._Release;
     end;
   end;
  //CoUninitialize;
end;

procedure TXxmGeckoLoader.SignalNextJob;
begin
  //assert thread waiting on FNextJobEvent
  SetEvent(FNextJobEvent);
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
      while FLoadersSize<>x do
       begin
        FLoaders[FLoadersSize]:=nil;
        inc(FLoadersSize);
       end;
     end
    else
     begin
      while FLoadersSize<>x do
       begin
        dec(FLoadersSize);
        //FreeAndNil(FLoaders[FLoadersSize]);
        if FLoaders[FLoadersSize]<>nil then
         begin
          FLoaders[FLoadersSize].FreeOnTerminate:=true;
          FLoaders[FLoadersSize].Terminate;
          FLoaders[FLoadersSize].SignalNextJob;
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
      while c.Queue<>nil do c:=c.Queue;
      c.Queue:=Channel;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FLoadersSize) and (FLoaders[i]<>nil) and FLoaders[i].InUse do inc(i);
  if i=FLoadersSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FLoaders[i]=nil then
      FLoaders[i]:=TxxmGeckoLoader.Create //start thread
    else
      FLoaders[i].SignalNextJob; //resume on waiting unqueues
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
      if Result<>nil then
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
  ChannelCounter:=0;
finalization
  FreeAndNil(GeckoLoaderPool);
end.
