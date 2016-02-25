unit xxmGeckoChannel;

interface

uses xxm, xxmContext, xxmThreadPool,
  Windows, Classes, SysUtils, ActiveX, xxmHeaders, xxmParUtils,
  xxmPReg, xxmPRegLocal, xxmParams;

type
  TxxmChannel=class(TXxmQueueContext,
    IxxmHttpHeaders)
  private
    FComplete,FGotSessionID:boolean;
    FPipePrefix,FVerb,FQueryString:AnsiString;
    FPipeIn,FPipeOut,FPipeCmd:THandle;
    FFlagUp,FFlagDown:THandle;
    FFlagMsg,FFlagValue:AnsiString;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FCookie:AnsiString;
    FCookieIdx: TParamIndexes;
    FCookieParsed: boolean;
    FTotalSize,FOutputSize:int64;
    procedure FlagMsg(const msg: AnsiString);
    function FlagValue(const msg: AnsiString): AnsiString;
  protected
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    procedure SendHeader; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
    function GetRequestHeader(const Name: WideString): WideString; override;

    { IxxmContext }
    function GetSessionID: WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(Name: WideString): WideString; override;

    { IxxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Load(aVerb, aURI: PAnsiChar): TxxmChannel;
    property PipePrefix:AnsiString read FPipePrefix;
  end;

  EXxmContextStringUnknown=class(Exception);

const
  PoolMaxThreads=64;//TODO: setting?

var
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
  Result:=(ContextPool.GetContext as TxxmChannel).Load(aVerb,aURI);
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
  //channel.Recycle?
end;

var
  ChannelCounter:integer;

const
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;

{ TxxmChannel }

procedure TxxmChannel.AfterConstruction;
begin
  inherited Create;
  FFlagUp:=CreateEvent(nil,false,false,nil);
  FFlagDown:=CreateEvent(nil,false,false,nil);
  FFlagMsg:='';
  FFlagValue:='';

  SendDirect:=SendData;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
end;

destructor TxxmChannel.Destroy;
begin
  CloseHandle(FFlagUp);
  CloseHandle(FFlagDown);

  FReqHeaders.Free;
  FResHeaders.Free;
  inherited;
end;

function TxxmChannel.Load(aVerb, aURI: PAnsiChar): TxxmChannel;
begin
  FURL:=aURI;
  FVerb:=aVerb;
  FPipePrefix:=Format('xxmGecko%.8x%.2x,%.4x',[GetTickCount,
    InterlockedIncrement(ChannelCounter) and $FF,
    integer(Self) and $FFFF]);

  PageLoaderPool.Queue(Self,ctHeaderNotSent);
  Result:=Self;
end;

procedure TxxmChannel.BeginRequest;
begin
  inherited;
  FResHeaders.Reset;
  FReqHeaders.Reset;

  FGotSessionID:=false;
  FOutputSize:=0;
  FTotalSize:=0;
  FCookieParsed:=false;
  FComplete:=false;
  FResHeaders['Content-Type']:='text/html';//default (setting?)
  FResHeaders['Content-Charset']:='utf-8';//used by GetContentCharset/SetContentCharset
  FQueryString:='';//parsed from URL

  ResetEvent(FFlagUp);
  ResetEvent(FFlagDown);
  FFlagMsg:='';
  FFlagValue:='';
end;

procedure TxxmChannel.EndRequest;
begin
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
  CloseHandle(FPipeCmd);
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

procedure RaiseLastOSErrorP;
var
  r:integer;
begin
  r:=GetLastError;
  if r<>ERROR_PIPE_CONNECTED then
    raise Exception.Create(SysErrorMessage(r));
end;

procedure TxxmChannel.HandleRequest;
var
  i,j,l:integer;
  x:WideString;
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
    while (i<=l) and not(AnsiChar(FURL[i]) in ['/','?','&','$','#']) do inc(i);
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
    while (i<=l) and not(AnsiChar(FURL[i]) in ['?','&','$','#']) do inc(i);
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
  Result:=FReqHeaders[Name];
end;

procedure TxxmChannel.AddResponseHeader(const Name, Value: WideString);
begin
  FlagMsg('hdr'+Name+' '+Value);
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
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
      Result:=FReqHeaders['accept-mime-type'];
    csPostMimeType:
      Result:=FReqHeaders['content-type'];
    csURL:
      Result:=FURL;//FURI.GetSpec?
    csReferer:
      Result:=FlagValue('ref');//Result:=FRequestHeaders['referer'];
    csLanguage:
      Result:=FReqHeaders['accept-language'];
    csRemoteAddress:
      Result:='127.0.0.1';//TODO: IPV6?
    csRemoteHost:
      Result:='localhost';
    //csAuthUser,csAuthPassword:Result:=AuthValue(cs);
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
  FResHeaders['Content-Disposition']:='attachment; filname="'+x+'"';
end;

function TxxmChannel.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders['Cookie'];
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
  ForceStatus(301,'Moved Permanently');
  //SendRaw('Redirected to <a href=""></a>')?
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
  if FContentType<>'' then
    FResHeaders['Content-Type']:=FContentType;
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
  Result:=FReqHeaders;
end;

function TxxmChannel.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

initialization
  ChannelCounter:=0;
  ContextPool:=TXxmContextPool.Create(TxxmChannel);
  PageLoaderPool:=TXxmPageLoaderPool.Create(PoolMaxThreads);
end.
