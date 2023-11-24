unit xxmHostMain;

interface

uses
  Windows, SysUtils, ActiveX, xxm, Classes, xxmContext, xxmThreadPool,
  xxmPReg, xxmParams, xxmParUtils, xxmHeaders;

type
  TXxmPostDataStream=class(TCustomMemoryStream)
  private
    FInput:THandle;
    FInputRead,FInputSize:cardinal;
  public
    constructor Create(Input:THandle;InputSize:cardinal);
    destructor Destroy; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    procedure SetSize(NewSize: Integer); override;
  end;

  TXxmHostedContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmContextSuspend)
  private
    FPipeIn,FPipeOut:THandle;
    FCGIValues:array of record
      Name,Value:AnsiString;
    end;
    FCGIValuesSize,FCGIValuesCount:integer;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FProjectCache:TXxmProjectCacheLocal;
    FConnected:boolean;
    FURI,FRedirectPrefix,FSessionID:WideString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    function GetCGIValue(const Name:AnsiString):AnsiString;
  protected
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;

    function SendData(const Buffer; Count: LongInt): LongInt;
    procedure DispositionAttach(const FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    procedure Redirect(const RedirectURL:WideString; Relative:boolean); override;
    function Connected:boolean; override;
    function GetSessionID:WideString; override;
    procedure SendHeader; override;
    function GetCookie(const Name:WideString):WideString; override;

    function GetProjectEntry:TXxmProjectEntry; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
    function GetRawSocket: IStream; override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Load(PipeIn,PipeOut:THandle);
  end;

  {$IF not(Declared(FixedUInt))}
  FixedUInt=LongInt;
  PFixedUInt=PLongInt;
  LargeInt=LongLongInt;
  LargeUInt=LargeInt;
  XDWORD=Longint;
  {$ELSE}
  XDWORD=DWORD;
  {$IFEND}

  TRawSocketData=class(TInterfacedObject, IStream, IXxmRawSocket)
  private
    FPipeIn, FPipeOut: THandle;
  public
    constructor Create(PipeIn,PipeOut:THandle);
    destructor Destroy; override;
    { IStream }
    function Seek(dlibMove: LargeInt; dwOrigin: XDWORD;
      out libNewPosition: LargeUInt): HResult; stdcall;
    function SetSize(libNewSize: LargeUInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt;
      out cbWritten: LargeUInt): HResult; stdcall;
    function Commit(grfCommitFlags: XDWORD): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset, cb: LargeUInt; dwLockType: XDWORD): HResult; stdcall;
    function UnlockRegion(libOffset, cb: LargeUInt; dwLockType: XDWORD): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: XDWORD): HResult; stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
    { ISequentialStream }
    function Read(pv: Pointer; cb: FixedUInt;
      pcbRead: PFixedUInt): HResult; stdcall;
    function Write(pv: Pointer; cb: FixedUInt;
      pcbWritten: PFixedUInt): HResult; stdcall;
    { IXxmRawSocket }
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure Disconnect;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmContextAlreadySuspended=class(Exception);

implementation

uses Variants, ComObj, xxmCommonUtils;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmContextAlreadySuspended='Context has already been suspended';

const
  HTTPMaxHeaderLines=$400;

{ TXxmHostedContext }

procedure TXxmHostedContext.AfterConstruction;
begin
  SendDirect:=SendData;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  FProjectCache:=TXxmProjectCacheLocal.Create;
  FCGIValuesSize:=0;
  inherited;
end;

destructor TXxmHostedContext.Destroy;
begin
  FReqHeaders.Free;
  FResHeaders.Free;
  FProjectCache.Free;
  SetLength(FCGIValues,0);
  inherited;
end;

procedure TXxmHostedContext.Load(PipeIn,PipeOut:THandle);
begin
  FPipeIn:=PipeIn;
  FPipeOut:=PipeOut;
  BeginRequest;
  PageLoaderPool.Queue(Self,ctHeaderNotSent);
end;

procedure TXxmHostedContext.BeginRequest;
begin
  inherited;
  FConnected:=true;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FRedirectPrefix:='';
  FReqHeaders.Reset;
  FResHeaders.Reset;
  FCGIValuesCount:=0;
end;

procedure TXxmHostedContext.EndRequest;
begin
  inherited;
  FlushFileBuffers(FPipeOut);
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
end;

procedure TXxmHostedContext.HandleRequest;
var
  i,j,k,l,m:integer;
  l1:cardinal;
  x,y:AnsiString;
const
  CGIValuesGrowStep=$100;
begin
  try
    //read CGI values
    if not(ReadFile(FPipeIn,l,4,l1,nil)) then RaiseLastOSError;
    SetLength(x,l);
    if not(ReadFile(FPipeIn,x[1],l,l1,nil)) then RaiseLastOSError;
    //process values
    i:=1;
    m:=0;
    while (i<l) do
     begin
      j:=i;
      while (j<=l) and (x[j]<>'=') do inc(j);
      k:=j+1;
      while (k<=l) and (x[k]<>#0) do inc(k);
      if (j-i>4) and (Copy(x,i,5)='HTTP_') then
       begin
        y:=y+x[i+5]+AnsiString(LowerCase(StringReplace(string(Copy(x,i+6,j-i-6)),
          '_','-',[rfReplaceAll])))+': '+Copy(x,j+1,k-j-1)+#13#10;
       end
      else
        if j<=l then
         begin
          if FCGIValuesCount=FCGIValuesSize then
           begin
            inc(FCGIValuesSize,CGIValuesGrowStep);
            SetLength(FCGIValues,FCGIValuesSize);
           end;
          FCGIValues[FCGIValuesCount].Name:=Copy(x,i,j-i);
          FCGIValues[FCGIValuesCount].Value:=Copy(x,j+1,k-j-1);
          inc(FCGIValuesCount);
         end;
      i:=k+1;
      inc(m);
      if m=HTTPMaxHeaderLines then
        raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
     end;
    y:=y+#13#10;

    FReqHeaders.Load(y,1,Length(y));

    x:=GetCGIValue('SERVER_PROTOCOL');//http or https
    i:=1;
    l:=Length(x);
    while (i<=l) and (x[i]<>'/') do inc(i);
    y:=UTF8Encode(FReqHeaders['Host']);
    if y='' then y:='localhost';//if not port=80 then +':'+?
    FRedirectPrefix:=LowerCase(string(Copy(x,1,i-1)))+'://'+UTF8ToWideString(y);

    x:=GetCGIValue('SCRIPT_NAME');
    y:=GetCGIValue('REQUEST_URI');
    l:=Length(x);
    if x=Copy(y,1,l) then
     begin
      FURI:=UTF8ToWideString(Copy(y,l+1,Length(y)-l));
      FRedirectPrefix:=FRedirectPrefix+UTF8ToWideString(x);
     end
    else
     begin
      FURI:=UTF8ToWideString(y);
      //FURLPrefix:= should be ok
     end;

    FURL:=FRedirectPrefix+FURI;

    //'Authorization' ?
    //'If-Modified-Since' ? 304
    //'Connection: Keep-alive' ? with sent Content-Length

    //FResHeaders['X-Powered-By']:=SelfVersion;

    FQueryStringIndex:=2;
    if XxmProjectCache.ProjectFromURI(Self,
      UTF8Encode(FURI),FQueryStringIndex,FProjectName,FFragmentName) then
      FRedirectPrefix:=FRedirectPrefix+'/'+FProjectName;
    FPageClass:='['+FProjectName+']';

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    //if Verb<>'GET' then?
    x:=GetCGIValue('CONTENT_LENGTH');
    if x<>'' then FPostData:=TXxmPostDataStream.Create(FPipeIn,StrToInt(string(x)));

    BuildPage;

  except
    on EXxmPageRedirected do
      Flush;
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');//TODO:setting?
        try
          if FPostData=nil then x:='none' else x:=AnsiString(IntToStr(FPostData.Size))+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',e.ClassName,e.Message);
       end;
  end;
end;

function TXxmHostedContext.GetProjectEntry: TXxmProjectEntry;
begin
  Result:=FProjectCache.GetProject(FProjectName);
end;

function TXxmHostedContext.Connected: boolean;
begin
  Result:=FConnected;
  //TODO: set to false when client disconnect
end;

function TXxmHostedContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion+' '+string(GetCGIValue('SERVER_SOFTWARE'));
    csExtraInfo:Result:='';//???
    csVerb:Result:=string(GetCGIValue('REQUEST_METHOD'));
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders['Accept'];//TODO:
    csPostMimeType:Result:=string(GetCGIValue('CONTENT_TYPE'));//TODO:
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=FReqHeaders['Referer'];
    csLanguage:Result:=FReqHeaders['Accept-Language'];
    csRemoteAddress:Result:=string(GetCGIValue('REMOTE_ADDR'));
    csRemoteHost:Result:=string(GetCGIValue('REMOTE_HOST'));
    csAuthUser,csAuthPassword:Result:=UTF8ToWideString(AuthValue(cs));
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TXxmHostedContext.DispositionAttach(const FileName: WideString);
var
  s:WideString;
  i:integer;
begin
  s:=FileName;
  for i:=1 to Length(s) do
    if AnsiChar(s[i]) in ['\','/',':','*','?','"','<','>','|'] then
      s[i]:='_';
  FResHeaders.SetComplex('Content-disposition','attachment')['filename']:=s;
end;

function TXxmHostedContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=UTF8Encode(FReqHeaders['Cookie']);
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=UTF8ToWideString(GetParamValue(FCookie,FCookieIdx,UTF8Encode(Name)));
end;

function TXxmHostedContext.GetSessionID: WideString;
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

procedure TXxmHostedContext.Redirect(const RedirectURL: WideString;
  Relative: boolean);
var
  NewURL,RedirBody:WideString;
begin
  //inherited;
  SetStatus(301,'Moved Permanently');//does CheckHeaderNotSent;
  //TODO: move this to execute's except?
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then NewURL:=FRedirectPrefix+NewURL;
  RedirBody:='<a href="'+HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a>'#13#10;
  FResHeaders['Location']:=NewURL;
  case FAutoEncoding of
    aeUtf8:FResHeaders['Content-Length']:=IntToStr(Length(UTF8Encode(RedirBody))+3);
    aeUtf16:FResHeaders['Content-Length']:=IntToStr(Length(RedirBody)*2+2);
    aeIso8859:FResHeaders['Content-Length']:=IntToStr(Length(AnsiString(RedirBody)));
  end;
  SendStr(RedirBody);
  if BufferSize<>0 then Flush;
  raise EXxmPageRedirected.Create(RedirectURL);
end;

function TXxmHostedContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Count=0 then Result:=0 else
   begin
    Result:=Count;
    if not WriteFile(FPipeOut,Buffer,Count,cardinal(Result),nil) then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end;
end;

procedure TXxmHostedContext.SendHeader;
var
  x:AnsiString;
  i:integer;
  l:cardinal;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  //TODO: Content-Length?
  //TODO: Connection keep?
  //use FResHeader.Complex?
  if FContentType<>'' then
    FResHeaders['Content-Type']:=FContentType+
      AutoEncodingCharset[FAutoEncoding];
  i:=StatusCode;
  WriteFile(FPipeOut,i,4,l,nil);
  x:=//GetCGIValue('SERVER_PROTOCOL')+' '+
    'Status: '+AnsiString(IntToStr(i))+' '+AnsiString(StatusText)+#13#10+
    FResHeaders.Build+#13#10;
  WriteFile(FPipeOut,x[1],Length(x),l,nil);
  inherited;
end;

function TXxmHostedContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  //assert FReqHeaders<>nil since parsed at start of Execute
  Result:=FReqHeaders;
end;

function TXxmHostedContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

function TXxmHostedContext.GetCGIValue(const Name: AnsiString): AnsiString;
var
  i:integer;
begin
  i:=0;
  while (i<FCGIValuesCount) and (Name<>FCGIValues[i].Name) do inc(i); //TODO: case-insensitive?
  if i=FCGIValuesCount then Result:='' else Result:=FCGIValues[i].Value;
end;

function TXxmHostedContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders[Name];
end;

procedure TXxmHostedContext.AddResponseHeader(const Name, Value: WideString);
begin
  //inherited;?
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

function TXxmHostedContext.GetRawSocket: IStream;
begin
  if FReqHeaders['Upgrade']='' then Result:=nil else
   begin
    FContentType:='';
    CheckSendStart(false);
    SetBufferSize(0);
    FlushFileBuffers(FPipeOut);
    Result:=TRawSocketData.Create(FPipeIn,FPipeOut);
   end;
end;

{ TXxmPostDataStream }

constructor TXxmPostDataStream.Create(Input:THandle;InputSize:cardinal);
begin
  inherited Create;
  FInput:=Input;
  FInputRead:=0;
  FInputSize:=InputSize;
  SetPointer(GlobalAllocPtr(GMEM_MOVEABLE,FInputSize),FInputSize);
end;

destructor TXxmPostDataStream.Destroy;
begin
  GlobalFreePtr(Memory);
  inherited;
end;

{$IF not(Declared(NativeUInt))}
type
  NativeUInt=cardinal;
{$IFEND}

function TXxmPostDataStream.Read(var Buffer; Count: Integer): Integer;
var
  l:cardinal;
  p:pointer;
  p1:NativeUInt absolute p;
begin
  l:=Position+Count;
  if l>FInputSize then l:=FInputSize;
  if l>FInputRead then
   begin
    dec(l,FInputRead);
    if l<>0 then
     begin
      p:=Memory;
      inc(p1,FInputRead);
      if not(ReadFile(FInput,p^,l,l,nil)) then RaiseLastOSError;
      inc(FInputRead,l);
     end;
   end;
  Result:=inherited Read(Buffer,Count);
end;

procedure TXxmPostDataStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create('Post data is read-only.');
end;

function TXxmPostDataStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('Post data is read-only.');
end;

{ TRawSocketData }

constructor TRawSocketData.Create(PipeIn,PipeOut:THandle);
begin
  inherited Create;
  FPipeIn:=PipeIn;
  FPipeOut:=PipeOut;
end;

destructor TRawSocketData.Destroy;
begin
  //FPipeIn:=
  //FPipeOut:=
  inherited;
end;

function TRawSocketData.Clone(out stm: IStream): HResult;
begin
  raise Exception.Create('TRawSocketData.Clone not supported');
end;

function TRawSocketData.Commit(grfCommitFlags: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Commit not supported');
end;

function TRawSocketData.CopyTo(stm: IStream; cb: LargeUInt; out cbRead,
  cbWritten: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.CopyTo not supported');
end;

function TRawSocketData.LockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.LockRegion not supported');
end;

function TRawSocketData.Revert: HResult;
begin
  raise Exception.Create('TRawSocketData.Revert not supported');
end;

function TRawSocketData.Seek(dlibMove: LargeInt; dwOrigin: XDWORD;
  out libNewPosition: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.Seek not supported');
end;

function TRawSocketData.SetSize(libNewSize: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.SetSize not supported');
end;

function TRawSocketData.Stat(out statstg: TStatStg;
  grfStatFlag: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Stat not supported');
end;

function TRawSocketData.UnlockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.UnlockRegion not supported');
end;

function TRawSocketData.Read(pv: Pointer; cb: FixedUInt;
  pcbRead: PFixedUInt): HResult;
var
  l:cardinal;
begin
  if not(ReadFile(FPipeIn,pv^,cb,l,nil)) then RaiseLastOSError;
  if pcbRead<>nil then pcbRead^:=l;
  Result:=S_OK;
end;

function TRawSocketData.Write(pv: Pointer; cb: FixedUInt;
  pcbWritten: PFixedUInt): HResult;
var
  l:cardinal;
begin
  if not(WriteFile(FPipeOut,pv^,cb,l,nil)) then RaiseLastOSError;
  if pcbWritten<>nil then pcbWritten^:=l;
  Result:=S_OK;
end;

function TRawSocketData.DataReady(TimeoutMS: cardinal): boolean;
var
  l,tc:cardinal;
begin
  if not(PeekNamedPipe(FPipeIn,nil,0,nil,@l,nil)) then
    RaiseLastOSError;
  if l=0 then
   begin
    tc:=GetTickCount;
    repeat
      Sleep(1);
      if not(PeekNamedPipe(FPipeIn,nil,0,nil,@l,nil)) then
        RaiseLastOSError;
    until (cardinal(GetTickCount-tc)>=TimeoutMS) or (l<>0);
   end;
  Result:=l<>0;
end;

procedure TRawSocketData.Disconnect;
begin
  //???
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
  ContextPool:=TXxmContextPool.Create(TXxmHostedContext);
end.
