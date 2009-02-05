unit xxmPReg;

interface

uses Windows, SysUtils, xxm;

type
  TXxmProjectCacheEntry=class(TObject)
  private
    FName,FFilePath,FCookiePath:WideString;
    FProject:IXxmProject;
    FHandle:THandle;
    FSignature,FUserName:string;
    FCookies:array of record
      Name,Value:WideString;
      //TODO: expiry, domain, path...
    end;
    FContextCount:integer;
    procedure GetRegisteredPath;
    function GetModulePath:WideString;
    function GetProject:IXxmProject;
    procedure SetSignature(const Value: string);
  published
    constructor Create(Name:WideString);
  public
    LastCheck:cardinal;
    procedure Release;
    destructor Destroy; override;
    procedure OpenContext;
    procedure CloseContext;
    procedure GetFilePath(Address:WideString;var Path,MimeType:WideString);
    function GetSessionCookie(Name: WideString): WideString;
    procedure SetSessionCookie(Name: WideString; Value: WideString);
    property Name:WideString read FName;
    property Project:IXxmProject read GetProject;
    property ModulePath:WideString read GetModulePath;
    property Signature:string read FSignature write SetSignature;
    function CookieFile(Name:string):string;
  end;

  TXxmProjectCache=class(TObject)
  private
    FLock:TRTLCriticalSection;
    ProjectCacheSize:integer;
    ProjectCache:array of TXxmProjectCacheEntry;
    procedure ClearAll;
    function Grow:integer;
    function FindProject(Name:WideString):integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetProject(Name:WideString):TXxmProjectCacheEntry;
    procedure ReleaseProject(Name:WideString);

  end;

  TXxmAutoBuildHandler=function(pce:TXxmProjectCacheEntry;
    Context:IXxmContext; ProjectName:WideString):boolean;

  EXxmProjectNotFound=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);

var
  XxmProjectCache:TXxmProjectCache;
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

procedure XxmProjectRegister(
  hwnd:HWND;        // handle to owner window
  hinst:cardinal;   // instance handle for the DLL
  lpCmdLine:LPTSTR; // string the DLL will parse
  nCmdShow:integer  // show state
); stdcall;

exports
  XxmProjectRegister;

implementation

uses Registry;

const //resourcestring?
  SXxmProjectNotFound='xxm Project "__" not defined.';
  SXxmModuleNotFound='xxm Module "__" does not exist.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';

procedure XxmProjectRegister(
  hwnd:HWND;        // handle to owner window
  hinst:cardinal;   // instance handle for the DLL
  lpCmdLine:LPTSTR; // string the DLL will parse
  nCmdShow:integer  // show state
); stdcall;
var
  r:TRegistry;
  s,t:string;
  i,j:integer;
begin
  s:=lpCmdLine;

  i:=Length(s);
  while not(i=0) and not(s[i]='.') do dec(i);
  j:=i;
  while not(j=0) and not(s[j]='\') do dec(j);
  inc(j);
  t:=Copy(s,j,i-j);

  r:=TRegistry.Create;
  try
    //r.RootKey:=HKEY_LOCAL_MACHINE;//setting?
    r.RootKey:=HKEY_CURRENT_USER;
    r.OpenKey('\Software\xxm\local\'+t,true);
    r.WriteString('',s);
    //TODO: default settings?
  finally
    r.Free;
  end;

  MessageBox(GetDesktopWindow,PChar('Project "'+t+'" registered.'),
    'xxm Local Handler',MB_OK or MB_ICONINFORMATION);
end;

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(Name: WideString);
begin
  inherited Create;
  FName:=LowerCase(Name);//lowercase here!
  FFilePath:='';
  FProject:=nil;
  FHandle:=0;
  FUserName:='';
  LastCheck:=GetTickCount-100000;
  FContextCount:=0;
end;

destructor TXxmProjectCacheEntry.Destroy;
begin
  Release;
  inherited;
end;

function TXxmProjectCacheEntry.GetProject: IXxmProject;
var
  lp:TXxmProjectLoadProc;
begin
  if FProject=nil then
   begin
    if (FFilePath='') or (FileExists(FFilePath)) then GetRegisteredPath;//refresh
    if not(FileExists(FFilePath)) then
      raise EXxmModuleNotFound.Create(StringReplace(
        SXxmModuleNotFound,'__',FFilePath,[]));
    FHandle:=LoadLibraryW(PWideChar(FFilePath));
    //TODO: see if DisableThreadLibraryCalls applies
    if FHandle=0 then RaiseLastOSError;
    @lp:=GetProcAddress(FHandle,'XxmProjectLoad');
    if @lp=nil then RaiseLastOSError;
    FProject:=lp(Name);//try?
    if FProject=nil then
     begin
      FFilePath:='';//force refresh next time
      raise EXxmProjectLoadFailed.Create(StringReplace(
        SXxmProjectLoadFailed,'__',FFilePath,[]));
     end;
   end;
  Result:=FProject;
end;

procedure TXxmProjectCacheEntry.Release;
begin
  //attention: deadlock danger, use OpenContext,CloseContext
  //XxmAutoBuildHandler should lock new requests
  while (FContextCount>0) do Sleep(1);

  //finalization gets called on last loaded libraries first,
  //so FProject release may fail on finalization
  try
    FProject:=nil;
  except
    pointer(FProject):=nil;
  end;

  if not(FHandle=0) then
   begin
    FreeLibrary(FHandle);
    FHandle:=0;
    FContextCount:=0;
   end;
end;

procedure TXxmProjectCacheEntry.GetFilePath(Address:WideString;
  var Path, MimeType: WideString);
var
  rf,sf,s:string;
  i,j,l:integer;
  r:TRegistry;
begin
  //TODO: widestring all the way?

  //TODO: virtual directories?
  rf:=FFilePath;
  i:=Length(rf);
  while not(i=0) and not(rf[i]=PathDelim) do dec(i);
  SetLength(rf,i);
  sf:='';

  i:=1;
  l:=Length(Address);
  while (i<=l) do
   begin

    j:=i;
    while (j<=l) and not(char(Address[j]) in ['/','\']) do inc(j);
    s:=Copy(Address,i,j-i);
    i:=j+1;

    if (s='') or (s='.') then
     begin
      //nothing
     end
    else
    if (s='..') then
     begin
      //try to go back, but not into rf (raise?)
      j:=Length(sf)-1;
      while (j>0) and not(sf[j]=PathDelim) do dec(j);
      SetLength(sf,j);
     end
    else
     begin
      sf:=sf+s+PathDelim;
      //DirectoryExists()??
     end;

   end;

  Path:=rf+Copy(sf,1,Length(sf)-1);

  i:=Length(sf)-1;
  while (i>0) and not(sf[i]='.') do dec(i);
  s:=LowerCase(copy(sf,i,Length(sf)-i));
  //TODO: get from settings or list? or project?
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(s) and r.ValueExists('Content Type') then
      MimeType:=r.ReadString('Content Type')
    else
      if (s='.log') then //override default for a few known types
        MimeType:='text/plain'
      else
        MimeType:='application/octet-stream';
  finally
    r.Free;
  end;

end;

function TXxmProjectCacheEntry.GetSessionCookie(Name: WideString): WideString;
var
  i:integer;
begin
  i:=0;
  while (i<Length(FCookies)) and not(FCookies[i].Name=Name) do inc(i);//case?
  if (i<Length(FCookies)) then Result:=FCookies[i].Value else Result:='';
end;

procedure TXxmProjectCacheEntry.SetSessionCookie(Name, Value: WideString);
var
  i:integer;
begin
  i:=0;
  while (i<Length(FCookies)) and not(FCookies[i].Name=Name) do inc(i);//case?
  if (i<Length(FCookies)) then FCookies[i].Value:=Value else
   begin
    SetLength(FCookies,i+1);
    FCookies[i].Name:=Name;
    FCookies[i].Value:=Value;
   end;
end;

function TXxmProjectCacheEntry.CookieFile(Name: string): string;
var
  l:cardinal;
begin
  if FUserName='' then
   begin
    l:=1024;
    SetLength(FUserName,l);
    if GetUserName(PChar(FUserName),l) then
      SetLength(FUserName,l-1)
    else
      FUserName:=GetEnvironmentVariable('USERNAME');
   end;
  //TODO: filenamesafe?
  Result:=FCookiePath+FUserName+'_'+Name+'.txt';
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: string);
var
  r:TRegistry;
  k:string;
begin
  FSignature:=Value;
  k:='\Software\xxm\local\'+FName;
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CURRENT_USER;
    if not(r.OpenKey(k,false)) then
     begin
      r.RootKey:=HKEY_LOCAL_MACHINE;
      if not(r.OpenKey(k,false)) then
        raise EXxmProjectNotFound.Create(StringReplace(
          SXxmProjectNotFound,'__',FName,[]));
     end;
    r.WriteString('Signature',FSignature);
  finally
    r.Free;
  end;
end;

procedure TXxmProjectCacheEntry.GetRegisteredPath;
var
  r:TRegistry;
  k:string;
  i:integer;
begin
  k:='\Software\xxm\local\'+FName;
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CURRENT_USER;
    if r.OpenKeyReadOnly(k) then
      FFilePath:=r.ReadString('')
    else
     begin
      r.RootKey:=HKEY_LOCAL_MACHINE;
      if r.OpenKeyReadOnly(k) then
        FFilePath:=r.ReadString('')
      else
        FFilePath:='';
     end;
    if FFilePath='' then
      raise EXxmProjectNotFound.Create(StringReplace(
        SXxmProjectNotFound,'__',FName,[]));

    //TODO: alias? (see xxm.xml)

    //TODO: extra flags,settings?

    //TODO: from setting?
    i:=Length(FFilePath);
    while not(i=0) and not(FFilePath[i]=PathDelim) do dec(i);
    FCookiePath:=Copy(FFilePath,1,i);

    if r.ValueExists('Signature') then
      FSignature:=r.ReadString('Signature')
    else
      FSignature:='';

  finally
    r.Free;
  end;
end;

function TXxmProjectCacheEntry.GetModulePath: WideString;
begin
  if FFilePath='' then GetRegisteredPath;
  Result:=FFilePath;
end;

procedure TXxmProjectCacheEntry.OpenContext;
begin
  InterlockedIncrement(FContextCount);
end;

procedure TXxmProjectCacheEntry.CloseContext;
begin
  InterlockedDecrement(FContextCount);
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
begin
  inherited;
  ProjectCacheSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TXxmProjectCache.Destroy;
begin
  ClearAll;
  DeleteCriticalSection(FLock);
  inherited;
end;

function TXxmProjectCache.Grow: integer;
var
  i:integer;
begin
  i:=ProjectCacheSize;
  Result:=i;
  inc(ProjectCacheSize,16);//const growstep
  SetLength(ProjectCache,ProjectCacheSize);
  while (i<ProjectCacheSize) do
   begin
    ProjectCache[i]:=nil;
    inc(i);
   end;
end;

function TXxmProjectCache.FindProject(Name: WideString): integer;
var
  l:string;
begin
  Result:=0;
  l:=LowerCase(Name);
  //assert cache stores ProjectName already LowerCase!
  while (Result<ProjectCacheSize) and (
    (ProjectCache[Result]=nil) or not(ProjectCache[Result].Name=l)) do inc(Result);
  if Result=ProjectCacheSize then Result:=-1;
end;

function TXxmProjectCache.GetProject(Name: WideString): TXxmProjectCacheEntry;
var
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    i:=FindProject(Name);
    if i=-1 then
     begin
      Result:=TXxmProjectCacheEntry.Create(Name);
      //add to cache
      i:=0;
      while (i<ProjectCacheSize) and not(ProjectCache[i]=nil) do inc(i);
      if (i=ProjectCacheSize) then i:=Grow;
      ProjectCache[i]:=Result;
     end
    else
      Result:=ProjectCache[i];
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmProjectCache.ReleaseProject(Name: WideString);
var
  i:integer;
begin
  i:=FindProject(Name);
  //if i=-1 then raise?
  if not(i=-1) then FreeAndNil(ProjectCache[i]);
end;

procedure TXxmProjectCache.ClearAll;
var
  i:integer;
begin
  for i:=0 to ProjectCacheSize-1 do
    try
      FreeAndNil(ProjectCache[i]);
    except
      //silent
    end;
  SetLength(ProjectCache,0);
  ProjectCacheSize:=0;
end;

initialization
  XxmProjectCache:=nil;//TXxmProjectCache.Create;//see Handler.Start
finalization
  FreeAndNil(XxmProjectCache);

end.
