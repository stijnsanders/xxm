unit xxmPReg;

interface

uses xxm, SysUtils, Windows, jsonDoc;

type
  TXxmProjectEntry=class(TObject)
  private
    FName:WideString;
    FProject:IXxmProject;
    FContextCount,FLoadCount:integer;
    FHandle:THandle;
    FLoadSignature:string;
{$IFNDEF XXM_INLINE_PROJECT}
    FCheckMutex:THandle;
{$ENDIF}
    FFilePath,FLoadPath:WideString;
    FLoadCopy,FAllowInclude,FNTLM,FNegotiate:boolean;
  protected
    FSignature:string;
    FBufferSize:integer;
    function GetProject: IXxmProject;
    function GetModulePath:WideString;
{$IFNDEF XXM_INLINE_PROJECT}
    function LoadProject: IXxmProject;
    procedure SetSignature(const Value: string);
{$ENDIF}
    procedure SetFilePath(const FilePath: WideString; LoadCopy: boolean);
    function ProjectLoaded:boolean;
    function GetExtensionMimeType(const x:string): string;
    function GetAllowInclude:boolean;
    property FilePath: WideString read FFilePath;
  public
    //used by auto-build/auto-update
    LastCheck:cardinal;
    LastResult:WideString;

    constructor Create(const Name, FilePath: WideString; LoadCopy: boolean);
    destructor Destroy; override;

{$IFNDEF XXM_INLINE_PROJECT}
    procedure Lock; //used by auto-build/auto-update
    procedure Unlock; //used by auto-build/auto-update
    procedure AfterConstruction; override; //creates the lock mutex
{$ENDIF}
    procedure Release;
    property ModulePath:WideString read GetModulePath;
{$IFNDEF XXM_INLINE_PROJECT}
    property Signature:string read FSignature write SetSignature;
    property LoadSignature:string read FLoadSignature;
    property LoadCount:integer read FLoadCount;
{$ENDIF}
    property AllowInclude:boolean read GetAllowInclude;
    property NTLM:boolean read FNTLM;
    property Negotiate:boolean read FNegotiate;

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
{$IFDEF XXM_INLINE_PROJECT}
    FProjectEntry:TXxmProjectEntry;
    FDefaultProject,FSingleProject:string;
{$ELSE}
    FProjectsLength,FProjectsCount:integer;
    FProjects:array of record
      Name,Alias:string;
      Entry:TXxmProjectEntry;
      LoadCheck:boolean;
      SortIndex:integer;
    end;
    FRegFilePath,FRegSignature,FDefaultProject,FSingleProject:string;
    FRegLastCheckTC:cardinal;
{$ENDIF}
    FFavIcon:OleVariant;
    FLock:TRTLCriticalSection;
    FCacheIndex:cardinal;
    FAuthCache:array of record
      SessionID:string;
      AuthName:AnsiString;
      Expires:TDateTime;
    end;
    FAuthCacheIndex,FAuthCacheSize:integer;

{$IFNDEF XXM_INLINE_PROJECT}
    procedure FindProject(const Name: string; var n: string;
      var i, a: integer);
    function GetRegistrySignature: string;
    function GetRegistry: IJSONDocument;
    procedure SetSignature(const Name: WideString; const Value: string);
{$ENDIF}
    procedure LoadFavIcon(const FilePath: string);
  public
    constructor Create;
    destructor Destroy; override;

    function ProjectFromURI(Context:IXxmContext;const URI:AnsiString;
      var i:integer; var ProjectName,FragmentName:WideString):boolean;

{$IFDEF XXM_INLINE_PROJECT}
    property ProjectEntry:TXxmProjectEntry read FProjectEntry;
{$ELSE}
    procedure CheckRegistry;
    function GetProject(const Name:WideString):TXxmProjectEntry;
    procedure ReleaseProject(const Name:WideString);

{$ENDIF}
    function GetAuthCache(const SessionID:string):AnsiString;
    procedure SetAuthCache(const SessionID:string;const AuthName:AnsiString);

    property CacheIndex:cardinal read FCacheIndex;
  end;

  EXxmProjectNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmProjectRegistryError=class(Exception);
  EXxmProjectAliasDepth=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);
  EXxmPageRedirected=class(Exception);

  TXxmAutoBuildHandler=function(Entry: TXxmProjectEntry;
    Context: IXxmContext; const ProjectName: WideString): boolean;

var
  XxmAutoBuildHandler:TXxmAutoBuildHandler;
  XxmProjectCache:TXxmProjectCache;
  XxmProjectCacheError:string;
  GlobalAllowLoadCopy:boolean;
{$IFDEF XXM_INLINE_PROJECT}
  XxmProjectName:string;
{$ENDIF}

const //resourcestring?
  SXxmProjectNotFound='xxm Project "__" not defined.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';
  SXxmModuleNotFound='xxm Module "__" does not exist.';
  SXxmFileTypeAccessDenied='Access denied to this type of file';

implementation

uses Registry, Classes, Variants,
{$IFDEF XXM_INLINE_PROJECT}
  xxmp,
{$ENDIF}
  xxmCommonUtils, xxmHeaders;

const //resourcestring?
  SXxmLoadProjectCopyFailed='LoadProject: Create load copy failed: ';
  SXxmLoadProjectLoadFailed='LoadProject: LoadLibrary failed: ';
  SXxmLoadProjectProcFailed='LoadProject: GetProcAddress failed: ';
  SXxmProjectEntryAcq='ProjectEntry acquire UpdateLock failed: ';
  SXxmProjectEntryRel='ProjectEntry release UpdateLock failed: ';
  SXxmProjectRegistryError='Could not open project registry "__"';
  SXxmProjectAliasDepth='xxm Project "__": aliasses are limited to 8 in sequence';

const
  XxmRegFileName='xxm.json';
  XxmRegCheckIntervalMS=1000;

{
function PathIsRelative(lpszPath:PWideChar):LongBool;
  stdcall; external 'shlwapi.dll' name 'PathIsRelativeW';
function PathCombine(lpszDest,lpszDir,lpszFile:PWideChar):PWideChar;
  stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
}

{$IF not Declared(UTF8ToWideString)}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$IFEND}


{ TXxmProjectEntry }

constructor TXxmProjectEntry.Create(const Name, FilePath: WideString;
  LoadCopy: boolean);
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
{$IFNDEF XXM_INLINE_PROJECT}
  FCheckMutex:=0;
{$ENDIF}  
  LastCheck:=GetTickCount-100000;
  LastResult:='';//default
  SetFilePath(FilePath,LoadCopy);
  FAllowInclude:=false;//default
  FNTLM:=false;//default
  FNegotiate:=false;//default
end;

{$IFNDEF XXM_INLINE_PROJECT}
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
{$ENDIF}

destructor TXxmProjectEntry.Destroy;
begin
  //pointer(FProject):=nil;//strange, project modules get closed before this happens
  Release;
{$IFNDEF XXM_INLINE_PROJECT}
  if FCheckMutex<>0 then CloseHandle(FCheckMutex);
{$ENDIF}
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
    if not FreeLibrary(FHandle) then
      RaiseLastOSError;
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

{$IFDEF XXM_INLINE_PROJECT}
function TXxmProjectEntry.GetProject: IXxmProject;
begin
  Result:=FProject;
end;

{$ELSE}
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

procedure DeferredLoad(tc:cardinal;const fn:WideString;var h:THandle); stdcall;
begin
  if (tc and 3)=0 then SwitchToThread;
  h:=LoadLibraryW(PWideChar(fn));
end;

type
  PDeferredLoad=procedure(tc:cardinal;const fn:WideString;var h:THandle);

function TXxmProjectEntry.LoadProject: IXxmProject;
var
  p:PDeferredLoad;
  fn,d:WideString;
  lp:TXxmProjectLoadProc;
  i,r:DWORD;
begin
  p:=@DeferredLoad;
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

    //xxmHttpAU.exe gets misidintified as Trojan:Win32/Bearfoos.A!ml
    //  and Trojan:Win32/Wacatac.B!ml, trying to work around detection
    //  with deferred call:

    //FHandle:=LoadLibraryW(PWideChar(fn));
    p(GetTickCount,fn,FHandle);

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
{$ENDIF}

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
  MimeType:=GetExtensionMimeType(
    LowerCase(Copy(sf,i,Length(sf)-i+1)));
end;

function TXxmProjectEntry.GetExtensionMimeType(const x: string): string;
var
  r:TRegistry;
begin
  if (x='.xxl') or (x='.xxu') or (x='.xxmp') or (x='.xxlc')
    or (x='.exe') or (x='.dll') or (x='.udl') //or (x='.pas')?
    //more? settings?
  then
    raise EXxmFileTypeAccessDenied.Create(SXxmFileTypeAccessDenied);

  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(x) and r.ValueExists('Content Type') then
      Result:=r.ReadString('Content Type')
    else
      if (x='.log') or (x='.ini') then //override default for a few known types
        Result:='text/plain'
      else if x='.js' then Result:='text/javascript'
      else if x='.css' then Result:='text/css'
      //TODO: more? from config?
      else
        Result:='application/octet-stream';
  finally
    r.Free;
  end;
end;

{$IFNDEF XXM_INLINE_PROJECT}
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
{$ENDIF}

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

{$IFNDEF XXM_INLINE_PROJECT}
procedure TXxmProjectEntry.SetSignature(const Value: string);
begin
  FSignature:=Value;
  XxmProjectCache.SetSignature(Name,Value);
end;
{$ENDIF}

function TXxmProjectEntry.GetAllowInclude: boolean;
begin
{$IFNDEF XXM_INLINE_PROJECT}
  XxmProjectCache.CheckRegistry;
{$ENDIF}
  Result:=FAllowInclude;
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
var
  i:integer;
  r:TResourceStream;
  p:pointer;
const
  RT_HTML = MakeIntResource(23);
begin
  inherited Create;

  FAuthCacheIndex:=0;
  FAuthCacheSize:=0;
  FCacheIndex:=GetTickCount;//random?
  InitializeCriticalSection(FLock);

{$IFDEF XXM_INLINE_PROJECT}
  FProjectEntry:=TXxmProjectEntry.Create(XxmProjectName,'',false);
  FProjectEntry.FProject:=XxmProjectLoad(XxmProjectName);
  FDefaultProject:=XxmProjectName;
  {$IFDEF HSYS1}{$DEFINE IgnoreProjectNameInURL}{$ENDIF}
  {$IFDEF HSYS2}{$DEFINE IgnoreProjectNameInURL}{$ENDIF}
  {$IFDEF IgnoreProjectNameInURL}
  FSingleProject:='';//see ProjectFromURI
  {$ELSE}
  FSingleProject:=xxmProjectName;
  {$ENDIF}
{$ELSE}

  FProjectsLength:=0;
  FProjectsCount:=0;
  FRegSignature:='-';
  FRegLastCheckTC:=GetTickCount-XxmRegCheckIntervalMS-1;

  SetLength(FRegFilePath,MAX_PATH);
  SetLength(FRegFilePath,GetModuleFileName(HInstance,
    PChar(FRegFilePath),MAX_PATH));
  if Copy(FRegFilePath,1,4)='\\?\' then
    FRegFilePath:=Copy(FRegFilePath,5,Length(FRegFilePath)-4);
  i:=Length(FRegFilePath);
  while (i<>0) and (FRegFilePath[i]<>PathDelim) do dec(i);
  FRegFilePath:=Copy(FRegFilePath,1,i);

  //settings?
  
  //assert CoInitialize called
  CheckRegistry;
{$ENDIF}

  r:=TResourceStream.Create(HInstance,'favicon',RT_HTML);
  try
    i:=r.Size;
    FFavIcon:=VarArrayCreate([0,i-1],varByte);
    p:=VarArrayLock(FFavIcon);
    try
      r.Read(p^,i);
    finally
      VarArrayUnlock(FFavIcon);
    end;
  finally
    r.Free;
  end;
end;

destructor TXxmProjectCache.Destroy;
{$IFDEF XXM_INLINE_PROJECT}
begin
  FProjectEntry.Free;
{$ELSE}
var
  i:integer;
begin
  for i:=0 to FProjectsCount-1 do
    try
      FreeAndNil(FProjects[i].Entry);
    except
      //silent
    end;
  SetLength(FProjects,0);
{$ENDIF}
  DeleteCriticalSection(FLock);
  inherited;
end;

{$IFNDEF XXM_INLINE_PROJECT}
procedure TXxmProjectCache.FindProject(const Name: string;
  var n: string; var i,a:integer);
var
  b,c,m:integer;
begin
  n:=LowerCase(Name);
  //assert cache stores ProjectName already LowerCase!
  a:=0;
  b:=FProjectsCount-1;
  i:=-1;
  while a<=b do
   begin
    c:=(a+b) div 2;
    m:=RawCompare(n,FProjects[FProjects[c].SortIndex].Name);
    if m<0 then
      if b=c then dec(b) else b:=c
    else
      if m>0 then
        if a=c then inc(a) else a:=c
      else
       begin
        a:=c;
        b:=a-1;//end loop
        i:=FProjects[c].SortIndex;
       end;
   end;
end;

function TXxmProjectCache.GetRegistrySignature: string;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  //assert in FLock
  FRegLastCheckTC:=GetTickCount;
  fh:=FindFirstFile(PChar(FRegFilePath+XxmRegFileName),fd);
  if fh=INVALID_HANDLE_VALUE then Result:='' else
   begin
    Result:=
      IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
end;

function TXxmProjectCache.GetRegistry: IJSONDocument;
var
  f:TFileStream;
  i:integer;
  s:AnsiString;
  w:WideString;
begin
  //assert in FLock
  //assert CoInitialize called
  Result:=JSON;
  f:=TFileStream.Create(FRegFilePath+XxmRegFileName,
    fmOpenRead or fmShareDenyWrite);
  try
    i:=f.Size;
    SetLength(s,i);
    if f.Read(s[1],i)<>i then RaiseLastOSError;
    if (i>=3) and (s[1]=#$EF) and (s[2]=#$BB) and (s[3]=#$BF) then
      Result.Parse(UTF8ToWideString(Copy(s,4,i-3)))
    else
    if (i>=2) and (s[1]=#$FF) and (s[2]=#$FE) then
     begin
      SetLength(w,(i div 2)-1);
      Move(s[3],w[1],(i*2)-1);
      Result.Parse(w);
     end
    else
      Result.Parse(WideString(s));
  finally
    f.Free;
  end;
end;

function BSize(const x:string):integer;
var
  i,l:integer;
begin
  Result:=0;//default
  i:=1;
  l:=Length(x);
  if l<>0 then
    case x[1] of
      '$','#','h','H','x','X':inc(i);//hex
      '0':if (l>2) and ((x[2]='x') or (x[2]='X')) then inc(i,2);
    end;
  if i<>1 then
    while (i<=l) do
     begin
      case x[i] of
        '0'..'9':
          Result:=Result*$10+(byte(x[i]) and $F);
        'A'..'F','a'..'f':
          Result:=Result*$10+9+(byte(x[i]) and $F);
        else raise Exception.Create('Invalid hexadecimal value "'+x+'"');
      end;
      inc(i);
     end
  else
    while (i<=l) do
     begin
      case x[i] of
        '0'..'9':
          Result:=Result*10+(byte(x[i]) and $F);
        'K','k':Result:=Result*$400;//kilobyte
        'M','m':Result:=Result*$100000;//megabyte
        //'G','g':Result:=Result*$40000000;//gigabyte
        'B','I','b','i':;//ignore
        else raise Exception.Create('Invalid numeric value "'+x+'"');
      end;
      inc(i);
     end;
end;

function VarToBool(const v:OleVariant):boolean;
begin
  Result:=not(VarIsNull(v)) and boolean(v);
end;

procedure TXxmProjectCache.CheckRegistry;
var
  s,n:string;
  p:WideString;
  i,j,a:integer;
  d,d1:IJSONDocument;
  e:IJSONEnumerator;
begin
  if cardinal(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
   begin
    EnterCriticalSection(FLock);
    try
      //check again for threads that were waiting for lock
      if cardinal(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
       begin
        //signature
        s:=GetRegistrySignature;
        if FRegSignature<>s then
         begin
          FRegSignature:=s;
          for i:=0 to FProjectsCount-1 do FProjects[i].LoadCheck:=false;
          d:=GetRegistry;
          FDefaultProject:=VarToStr(d['defaultProject']);
          if FDefaultProject='' then FDefaultProject:='xxm';
          FSingleProject:=VarToStr(d['singleProject']);
          e:=JSONEnum(d['projects']);
          while e.Next do
           begin
            d1:=JSON(e.Value);
            FindProject(e.Key,n,i,a);
            if (i<>-1) and (FProjects[i].LoadCheck) then i:=-1;//duplicate! raise?
            if i=-1 then
             begin
              //new
              if FProjectsCount=FProjectsLength then
               begin
                inc(FProjectsLength,8);
                SetLength(FProjects,FProjectsLength);
               end;
              i:=FProjectsCount;
              inc(FProjectsCount);
              FProjects[i].Name:=n;
              FProjects[i].Entry:=nil;//create see below
              //sort index
              j:=i;
              while j>a do
               begin
                FProjects[j].SortIndex:=FProjects[j-1].SortIndex;
                dec(j);
               end;
              FProjects[j].SortIndex:=i;
             end;
            FProjects[i].LoadCheck:=true;
            FProjects[i].Alias:=VarToStr(d1['alias']);
            if FProjects[i].Alias='' then
             begin
              p:=StringReplace(
                VarToStr(d1['path']),'/',PathDelim,[rfReplaceAll]);
              if p='' then raise EXxmProjectNotFound.Create(StringReplace(
                SXxmProjectNotFound,'__',e.Key,[]));
              {
              if PathIsRelative(PWideChar(p)) then
               begin
                SetLength(p,MAX_PATH);
                PathCombine(PWideChar(p),PWideChar(WideString(FRegFilePath)),PWideChar(y.text));
                SetLength(p,Length(p));
               end;
              }
              if (Length(p)>2) and not((p[2]=':') or ((p[1]='\') and (p[2]='\'))) then
                p:=FRegFilePath+p;
              if FProjects[i].Entry=nil then
                FProjects[i].Entry:=TXxmProjectEntry.Create(e.Key,p,
                  VarToBool(d1['loadCopy']))
              else
                if p<>FProjects[i].Entry.FilePath then
                  FProjects[i].Entry.SetFilePath(p,VarToBool(d1['loadCopy']));
              FProjects[i].Entry.FAllowInclude:=VarToBool(d1['allowInclude']);
              FProjects[i].Entry.FSignature:=VarToStr(d1['signature']);
              FProjects[i].Entry.FBufferSize:=BSize(VarToStr(d1['bufferSize']));
              FProjects[i].Entry.FNTLM:=VarToBool(d1['ntlm']);
              FProjects[i].Entry.FNegotiate:=VarToBool(d1['negotiate']);
             end
            else
             begin
              try
                FreeAndNil(FProjects[i].Entry);
              except
                //silent
              end;
              inc(FCacheIndex);
             end;
           end;
          //clean-up items removed from registry
          for i:=0 to FProjectsCount-1 do
            if not FProjects[i].LoadCheck then
             begin
              FProjects[i].Name:='';
              FProjects[i].Alias:='';
              try
                FreeAndNil(FProjects[i].Entry);
              except
                //silent
              end;
              inc(FCacheIndex);
             end;
          if FSingleProject<>'' then
            LoadFavIcon(FSingleProject+'.ico');
         end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TXxmProjectCache.SetSignature(const Name:WideString;
  const Value:string);
var
  d,d1:IJSONDocument;
  s:AnsiString;
  f:TFileStream;
begin
  CheckRegistry;//?
  EnterCriticalSection(FLock);
  try
    d:=GetRegistry;
    d1:=JSON(JSON(d['projects'])[Name]);
    if d1=nil then
      raise EXxmProjectNotFound.Create(StringReplace(
        SXxmProjectNotFound,'__',Name,[]));
    d1['signature']:=Value;
    //save
    s:=
      #$EF#$BB#$BF+//Utf8ByteOrderMark+
      UTF8Encode(d.ToString);
    f:=TFileStream.Create(FRegFilePath+XxmRegFileName,fmCreate);
    try
      f.Write(s[1],Length(s));
    finally
      f.Free;
    end;
    FRegSignature:=GetRegistrySignature;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TXxmProjectCache.GetProject(const Name: WideString):
  TXxmProjectEntry;
var
  n:string;
  i,a,d:integer;
  found:boolean;
  e:TXxmProjectEntry;
begin
{$IF CompilerVersion<20}
  e:=nil;//counter warning
{$IFEND}
  CheckRegistry;
  EnterCriticalSection(FLock);
  try
    found:=false;
    d:=0;
    FindProject(Name,n,i,a);
    while (i<>-1) and not(found) do
      if FProjects[i].Alias='' then found:=true else
       begin
        inc(d);
        if d=8 then raise EXxmProjectAliasDepth.Create(StringReplace(
          SXxmProjectAliasDepth,'__',Name,[]));
        FindProject(FProjects[i].Alias,n,i,a);
       end;
    if i=-1 then
      raise EXxmProjectNotFound.Create(StringReplace(
        SXxmProjectNotFound,'__',Name,[]))
    else
      e:=FProjects[i].Entry;
  finally
    LeaveCriticalSection(FLock);
  end;
  Result:=e;
end;

procedure TXxmProjectCache.ReleaseProject(const Name: WideString);
var
  n:string;
  i,a:integer;
begin
  //CheckRegistry?
  EnterCriticalSection(FLock);
  try
    FindProject(Name,n,i,a);
    //if i=-1 then raise?
    if i<>-1 then
     begin
      FProjects[i].Name:='';
      FProjects[i].Alias:='';
      try
        FreeAndNil(FProjects[i].Entry);
      except
        //silent
      end;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;
{$ENDIF}

function TXxmProjectCache.ProjectFromURI(Context:IXxmContext;
  const URI:AnsiString;var i:integer;
  var ProjectName,FragmentName:WideString):boolean;
var
  j,l:integer;
  x:AnsiString;
begin
{$IFNDEF XXM_INLINE_PROJECT}
  CheckRegistry;
{$ENDIF}
  l:=Length(URI);
{$IFDEF IgnoreProjectNameInURL}
  if true then
{$ELSE}
  if FSingleProject='' then
{$ENDIF}
   begin
    while (i<=l) and not(URI[i] in ['/','?','&','$','#']) do inc(i);
    ProjectName:=WideString(Copy(URI,2,i-2));
    if ProjectName='' then
     begin
      if (i<=l) and (URI[i]='/') then x:='' else x:='/';
      Context.Redirect('/'+FDefaultProject+WideString(x+Copy(URI,i,l-i+1)),true);
     end;
    if (i>l) and (l>1) then
      if URI='/favicon.ico' then
       begin
        Context.ContentType:='image/x-icon';
        (Context as IxxmHttpHeaders).ResponseHeaders['Content-Length']:=
          IntToStr(VarArrayHighBound(FFavIcon,1)+1);
        Context.SendHTML(FFavIcon);
        raise EXxmPageRedirected.Create(string(URI));
       end
      else
        Context.Redirect(WideString(URI)+'/',true)
    else
      if (URI[i]='/') then inc(i);
    Result:=true;
   end
  else
   begin
    ProjectName:=FSingleProject;
    Result:=false;
   end;
  j:=i;
  while (i<=l) and not(URI[i] in ['?','&','$','#']) do inc(i);
  FragmentName:=URLDecode(Copy(URI,j,i-j));
  if (i<=l) then inc(i);
end;

procedure TXxmProjectCache.LoadFavIcon(const FilePath:string);
var
  f:TFileStream;
  i:integer;
  p:pointer;
begin
  if FilePath<>'' then
    try
      f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
      try
        i:=f.Size;
        FFavIcon:=VarArrayCreate([0,i-1],varByte);
        p:=VarArrayLock(FFavIcon);
        try
          f.Read(p^,i);
        finally
          VarArrayUnlock(FFavIcon);
        end;
      finally
        f.Free;
      end;
    except
      on EFOpenError do ;//silent
    end;
end;

function TXxmProjectCache.GetAuthCache(const SessionID: string): AnsiString;
var
  i:integer;
begin
  Result:='';//default
  if SessionID<>'' then
   begin
    EnterCriticalSection(FLock);
    try
      i:=0;
      while (i<FAuthCacheIndex) and (FAuthCache[i].SessionID<>SessionID) do inc(i);
      if (i<FAuthCacheIndex) and (FAuthCache[i].Expires>Now) then
        Result:=FAuthCache[i].AuthName;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TXxmProjectCache.SetAuthCache(const SessionID: string;
  const AuthName: AnsiString);
var
  i:integer;
const
  AuthCacheTimeoutMins=15;//TODO: from config?
begin
  if AuthName<>'' then
   begin
    EnterCriticalSection(FLock);
    try
      i:=0;
      while (i<FAuthCacheIndex) and (FAuthCache[i].SessionID<>SessionID) do inc(i);
      if i=FAuthCacheIndex then
       begin
        if FAuthCacheIndex=FAuthCacheSize then
         begin
          inc(FAuthCacheSize,4);//growstep
          SetLength(FAuthCache,FAuthCacheSize);
         end;
        inc(FAuthCacheIndex);
       end;
      FAuthCache[i].SessionID:=SessionID;
      FAuthCache[i].AuthName:=AuthName;
      FAuthCache[i].Expires:=Now+AuthCacheTimeoutMins/MinsPerDay;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

initialization
{$IFDEF XXM_INLINE_PROJECT}
  XxmProjectName:='xxm';//default, set by dpr
  GlobalAllowLoadCopy:=false;
{$ELSE}
  GlobalAllowLoadCopy:=true;//default
  //XxmProjectCache:=TXxmProjectCache.Create;//see handler start-up
{$ENDIF}
finalization
  XxmProjectCache.Free;
end.
