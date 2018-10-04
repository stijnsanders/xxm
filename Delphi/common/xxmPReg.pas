unit xxmPReg;

interface

uses xxm, SysUtils, Windows;

type
  TXxmProjectEntry=class(TObject)
  private
    FName:WideString;
    FProject:IXxmProject;
    FContextCount,FLoadCount:integer;
    FHandle:THandle;
    FLoadSignature:string;
    FCheckMutex:THandle;
    FFilePath,FLoadPath:WideString;
    FLoadCopy:boolean;
  protected
    FSignature:string;
    FBufferSize:integer;
    function GetProject: IXxmProject;
    function LoadProject: IXxmProject; virtual;
    function GetModulePath:WideString; virtual;
    procedure SetSignature(const Value: string); virtual; abstract;
    procedure SetFilePath(const FilePath: WideString; LoadCopy: boolean);
    function ProjectLoaded:boolean;
    function GetExtensionMimeType(const x:AnsiString): AnsiString; virtual;
    function GetAllowInclude:boolean; virtual; abstract;
    property FilePath: WideString read FFilePath;
  public
    //used by auto-build/auto-update
    LastCheck:cardinal;
    LastResult:WideString;

    constructor Create(const Name: WideString);
    destructor Destroy; override;

    procedure Lock; //used by auto-build/auto-update
    procedure Unlock; //used by auto-build/auto-update
    procedure Release; //virtual;?
    procedure AfterConstruction; override; //creates the lock mutex
    property ModulePath:WideString read GetModulePath;
    property Signature:string read FSignature write SetSignature;
    property LoadSignature:string read FLoadSignature;
    property LoadCount:integer read FLoadCount;
    property AllowInclude:boolean read GetAllowInclude;

    //used by xxmContext
    procedure OpenContext;
    procedure CloseContext;
    procedure GetFilePath(const Address:WideString;var Path,MimeType:WideString);
    function GetProjectInterface(const IID: TGUID):IUnknown;
    property BufferSize: integer read FBufferSize;
    property Name: WideString read FName;
    property Project: IXxmProject read GetProject;
  end;

  TXxmProjectCache=class(TObject)
  protected
    FLock:TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  EXxmProjectNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmModuleNotFound=class(Exception);

  TXxmAutoBuildHandler=function(Entry: TXxmProjectEntry;
    Context: IXxmContext; const ProjectName: WideString): boolean;

var
  XxmAutoBuildHandler:TXxmAutoBuildHandler;
  //XxmProjectCache:TXxmProjectCache;
  GlobalAllowLoadCopy:boolean;

const //resourcestring?
  SXxmProjectNotFound='xxm Project "__" not defined.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';
  SXxmModuleNotFound='xxm Module "__" does not exist.';

implementation

uses Registry, xxmCommonUtils;

const //resourcestring?
  SXxmLoadProjectCopyFailed='LoadProject: Create load copy failed: ';
  SXxmLoadProjectLoadFailed='LoadProject: LoadLibrary failed: ';
  SXxmLoadProjectProcFailed='LoadProject: GetProcAddress failed: ';
  SXxmProjectEntryAcq='ProjectEntry acquire UpdateLock failed: ';
  SXxmProjectEntryRel='ProjectEntry release UpdateLock failed: ';

{ TXxmProjectEntry }

constructor TXxmProjectEntry.Create(const Name: WideString);
begin
  inherited Create;
  FName:=Name;
  FContextCount:=0;
  FLoadCount:=0;
  FProject:=nil;
  FHandle:=0;
  FBufferSize:=0;
  FFilePath:='';//see SetFilePath
  FLoadPath:='';
  FLoadCopy:=false;
  FSignature:='';//used for auto-build
  FLoadSignature:='';//used for auto-update
  FCheckMutex:=0;
  LastCheck:=GetTickCount-100000;
  LastResult:='';//default
end;

procedure TXxmProjectEntry.AfterConstruction;
var
  mn:WideString;
  i,l:integer;
begin
  inherited;
  if @XxmAutoBuildHandler<>nil then
   begin
    //prepare mutex name
    mn:=GetModulePath;
    l:=Length(mn);
    if l>248 then
     begin
      mn:=Copy(mn,1,120)+'('+IntToStr(l-240)+')'+Copy(mn,l-119,120);
      l:=Length(mn);
     end;
    for i:=1 to l do if AnsiChar(mn[i]) in ['\',':','/',' ','.'] then mn[i]:='|';
    mn:='Global\'+mn;
    //get mutex
    FCheckMutex:=CreateMutexW(nil,false,PWideChar(mn));
    if FCheckMutex=0 then RaiseLastOSError;//?
   end;
end;

destructor TXxmProjectEntry.Destroy;
begin
  Release;
  if FCheckMutex<>0 then CloseHandle(FCheckMutex);
  inherited;
end;

procedure TXxmProjectEntry.OpenContext;
begin
  InterlockedIncrement(FContextCount);
end;

procedure TXxmProjectEntry.CloseContext;
begin
  InterlockedDecrement(FContextCount);
end;

procedure TXxmProjectEntry.Release;
var
  pe:IXxmProjectEvents1;
begin
  //attention: deadlock danger, use OpenContext,CloseContext
  //XxmAutoBuildHandler should lock new requests

  if (FProject<>nil) and (FProject.QueryInterface(IXxmProjectEvents1,pe)=S_OK) then
   begin
    try
      pe.ReleasingContexts;
    except
      //silent
    end;
    pe:=nil;
   end;

  //assert only one thread at once, use Lock/Unlock!
  while (FContextCount>0) do Sleep(1);

  if (FProject<>nil) and (FProject.QueryInterface(IXxmProjectEvents1,pe)=S_OK) then
   begin
    try
      pe.ReleasingProject;
    except
      //silent
    end;
    pe:=nil;
   end;

  //finalization gets called on last loaded libraries first,
  //so FProject release may fail on finalization
  try
    FProject:=nil;
  except
    pointer(FProject):=nil;
  end;
  if FHandle<>0 then
   begin
    FreeLibrary(FHandle);
    FHandle:=0;
    //FContextCount:=0;
    if FLoadPath<>'' then
     begin
      //SetFileAttributesW(PWideChar(FLoadPath),0);
      DeleteFileW(PWideChar(FLoadPath));//ignore errors
      FLoadPath:='';
     end;
   end;
end;

function TXxmProjectEntry.GetProject: IXxmProject;
begin
  if FProject=nil then
   begin
    Lock;
    try
      //check again in case other thread was locking also
      if FProject=nil then
       begin
        FProject:=LoadProject;
        if FProject=nil then
          raise EXxmProjectLoadFailed.Create(StringReplace(
            SXxmProjectLoadFailed,'__',FFilePath,[]));
       end;
    finally
      Unlock;
    end;
   end;
  Result:=FProject;
end;

function TXxmProjectEntry.LoadProject: IXxmProject;
var
  fn,d:WideString;
  lp:TXxmProjectLoadProc;
  i,r:DWORD;
begin
  //assert within Lock/Unlock
  inc(FLoadCount);
  FLoadSignature:=GetFileSignature(FFilePath);
  if FLoadSignature='' then //if not(FileExists(FFilePath)) then
    raise EXxmModuleNotFound.Create(StringReplace(
      SXxmModuleNotFound,'__',FFilePath,[]));
  if not(FLoadCopy and GlobalAllowLoadCopy) then
    fn:=FFilePath
  else
   begin
    FLoadPath:=Copy(FFilePath,1,Length(FFilePath)-4)+
      '_'+WideString(FLoadSignature)+'.xxlc';
    r:=100;
    while (r<>0) do
     begin
      if CopyFileW(PWideChar(FFilePath),PWideChar(FLoadPath),true) then
       begin
        SetFileAttributesW(PWideChar(FLoadPath),
          FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);//ignore error
        r:=0;//done
       end
      else
       begin
        i:=GetLastError;
        if i=ERROR_FILE_EXISTS then r:=0 else
         begin
          dec(r);
          if (r=0) or (i<>ERROR_ACCESS_DENIED) then
            raise EXxmProjectLoadFailed.Create(SXxmLoadProjectCopyFailed+
              SysErrorMessage(i))
          else
            Sleep(20+(GetCurrentThreadId and $3F));
         end;
        //else assert files are equal
       end;
     end;
    fn:=FLoadPath;
   end;
  FHandle:=LoadLibraryW(PWideChar(fn));
  if (FHandle=0) and (GetLastError=ERROR_MOD_NOT_FOUND) then
   begin
    //tried SetDllDirectory, doesn't work...
    SetLength(d,MAX_PATH);
    SetLength(d,GetCurrentDirectoryW(MAX_PATH,PWideChar(d)));
    i:=Length(fn);
    while (i<>0) and (fn[i]<>'\') do dec(i);
    SetCurrentDirectoryW(PWideChar(Copy(fn,1,i-1)));
    FHandle:=LoadLibraryW(PWideChar(fn));
    SetCurrentDirectoryW(PWideChar(d));
   end;
  if FHandle=0 then
    raise EXxmProjectLoadFailed.Create(SXxmLoadProjectLoadFailed+
      SysErrorMessage(GetLastError));
  @lp:=GetProcAddress(FHandle,'XxmProjectLoad');
  if @lp=nil then
    raise EXxmProjectLoadFailed.Create(SXxmLoadProjectProcFailed+
      SysErrorMessage(GetLastError));
  Result:=lp(FName);//try?
end;

function TXxmProjectEntry.ProjectLoaded: boolean;
begin
  Result:=FProject<>nil;
end;

function TXxmProjectEntry.GetModulePath: WideString;
begin
  Result:=FFilePath;
end;

procedure TXxmProjectEntry.GetFilePath(const Address: WideString; var Path,
  MimeType: WideString);
var
  rf,sf,s:WideString;
  i,j,l:integer;
begin
  rf:=GetModulePath;
  i:=Length(rf);
  while (i<>0) and (rf[i]<>PathDelim) do dec(i);
  SetLength(rf,i);
  sf:='';
  i:=1;
  l:=Length(Address);
  while (i<=l) do
   begin
    j:=i;
    while (j<=l) and not(AnsiChar(Address[j]) in ['/','\']) do inc(j);
    s:=Copy(Address,i,j-i);
    if (s='') or (s='.') then
      //nothing
    else
    if (s='..') then
     begin
      //try to go back, but not into rf (raise?)
      i:=Length(sf)-1;
      while (i>0) and (sf[i]<>PathDelim) do dec(i);
      SetLength(sf,i);
     end
    else
      sf:=sf+s;//DirectoryExists()??
    if (j<=l) and (AnsiChar(Address[j]) in ['/','\']) then sf:=sf+PathDelim;
    i:=j+1;
   end;
  Path:=rf+sf;
  //find a MIME-type from registry
  i:=Length(sf)-1;
  while (i>0) and (sf[i]<>'.') do dec(i);
  MimeType:=WideString(GetExtensionMimeType(AnsiString(
    LowerCase(Copy(sf,i,Length(sf)-i+1)))));
end;

function TXxmProjectEntry.GetExtensionMimeType(const x: AnsiString): AnsiString;
var
  r:TRegistry;
begin
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(string(x)) and r.ValueExists('Content Type') then
      Result:=AnsiString(r.ReadString('Content Type'))
    else
      if (x='.log') or (x='.ini') then //override default for a few known types
        Result:='text/plain'
      else if x='.js' then Result:='text/javascript'
      else if x='.css' then Result:='text/css'
      else
        Result:='application/octet-stream';
  finally
    r.Free;
  end;
end;

procedure TXxmProjectEntry.Lock;
begin
  if FCheckMutex<>0 then
    if WaitForSingleObject(FCheckMutex,INFINITE)<>WAIT_OBJECT_0 then
      raise Exception.Create(SXxmProjectEntryAcq+SysErrorMessage(GetLastError));
end;

procedure TXxmProjectEntry.Unlock;
begin
  if FCheckMutex<>0 then
    if not ReleaseMutex(FCheckMutex) then
      raise Exception.Create(SXxmProjectEntryRel+SysErrorMessage(GetLastError));
end;

function TXxmProjectEntry.GetProjectInterface(const IID: TGUID): IUnknown;
begin
  if (Self=nil) or (FProject=nil) or (FProject.QueryInterface(IID,Result)<>S_OK) then Result:=nil;
end;

procedure TXxmProjectEntry.SetFilePath(const FilePath: WideString;
  LoadCopy: boolean);
begin
  //assert FProject=nil//if FFilePath<>'' then Release;
  FFilePath:=FilePath;
  FLoadPath:='';
  FLoadCopy:=LoadCopy;
  //if FLoadCopy then FLoadPath:=FFilePath+'_'+IntToHex(GetCurrentProcessId,4);
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TXxmProjectCache.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

initialization
  GlobalAllowLoadCopy:=true;//default
end.
