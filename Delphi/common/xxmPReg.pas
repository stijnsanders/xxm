unit xxmPReg;

interface

uses xxm, SysUtils;

type
  TXxmProjectEntry=class(TObject)
  private
    FName:WideString;
    FProject:IXxmProject;
    FContextCount:integer;
    FHandle:THandle;
    FLoadSignature:AnsiString;
    FCheckMutex:THandle;
  protected
    FSignature:AnsiString;
    FFilePath,FLoadPath:WideString;
    function GetProject: IXxmProject;
    procedure LoadProject; virtual;
    function GetModulePath:WideString; virtual;
    procedure SetSignature(const Value: AnsiString); virtual; abstract;
    function ProjectLoaded:boolean;
    function GetExtensionMimeType(x:AnsiString): AnsiString; virtual;
    function GetAllowInclude:boolean; virtual; abstract;
  published
    constructor Create(Name:WideString);//abstract! only here for initialization
    destructor Destroy; override;
  public
    //used by auto-build/auto-update
    LastCheck:cardinal;
    procedure Lock; //used by auto-build/auto-update
    procedure Unlock; //used by auto-build/auto-update
    procedure Release; //virtual;?
    procedure AfterConstruction; override; //creates the lock mutex
    property ModulePath:WideString read GetModulePath;
    property Signature:AnsiString read FSignature write SetSignature;
    property LoadSignature:AnsiString read FLoadSignature;
    property AllowInclude:boolean read GetAllowInclude;

    //used by xxmContext
    procedure OpenContext;
    procedure CloseContext;
    procedure GetFilePath(Address:WideString;var Path,MimeType:WideString);
    function GetProjectInterface(const IID: TGUID):IUnknown;
    property Name: WideString read FName;
    property Project: IXxmProject read GetProject;
  end;

  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);

  TXxmAutoBuildHandler=function(Entry: TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString): boolean;

var
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

implementation

uses Windows, Registry, xxmCommonUtils;

const //resourcestring?
  SXxmModuleNotFound='xxm Module "__" does not exist.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';

{ TXxmProjectEntry }

constructor TXxmProjectEntry.Create(Name:WideString);
begin
  inherited Create;
  FName:=Name;
  FContextCount:=0;
  FProject:=nil;
  FHandle:=0;
  FFilePath:='';//set by inheriters
  FLoadPath:='';//set by inheriters
  FSignature:='';//used for auto-build
  FLoadSignature:='';//used for auto-update
  FCheckMutex:=0;
  LastCheck:=GetTickCount-100000;
end;

procedure TXxmProjectEntry.AfterConstruction;
var
  mn:AnsiString;
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
    for i:=1 to l do if char(mn[i]) in ['\',':','/',' ','.'] then mn[i]:='|';
    mn:='Global\'+mn;
    //get mutex
    FCheckMutex:=CreateMutexA(nil,false,PAnsiChar(mn));
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
  if FHandle<>0 then
   begin
    FreeLibrary(FHandle);
    FHandle:=0;
    //FContextCount:=0;
    if FLoadPath<>'' then
     begin
      //SetFileAttributesW(PWideChar(FLoadPath),0);
      DeleteFileW(PWideChar(FLoadPath));
     end;
   end;
end;

function TXxmProjectEntry.GetProject: IXxmProject;
begin
  if FProject=nil then
   begin
    LoadProject;
    if FProject=nil then
      raise EXxmProjectLoadFailed.Create(StringReplace(
        SXxmProjectLoadFailed,'__',FFilePath,[]));
   end;
  Result:=FProject;
end;

procedure TXxmProjectEntry.LoadProject;
var
  lp:TXxmProjectLoadProc;
begin
  FLoadSignature:=GetFileSignature(FFilePath);
  if FLoadSignature='' then //if not(FileExists(FFilePath)) then
    raise EXxmModuleNotFound.Create(StringReplace(
      SXxmModuleNotFound,'__',FFilePath,[]));
  if FLoadPath='' then
    FHandle:=LoadLibraryW(PWideChar(FFilePath))
  else
   begin
    if not(CopyFileW(PWideChar(FFilePath),PWideChar(FLoadPath),false)) then
      raise EXxmProjectLoadFailed.Create('LoadProject: Create load copy failed: '+SysErrorMessage(GetLastError));
    SetFileAttributesW(PWideChar(FLoadPath),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
    FHandle:=LoadLibraryW(PWideChar(FLoadPath));
   end;
  if FHandle=0 then
    raise EXxmProjectLoadFailed.Create('LoadProject: LoadLibrary failed: '+SysErrorMessage(GetLastError));
  @lp:=GetProcAddress(FHandle,'XxmProjectLoad');
  if @lp=nil then
    raise EXxmProjectLoadFailed.Create('LoadProject: GetProcAddress failed: '+SysErrorMessage(GetLastError));
  FProject:=lp(FName);//try?
end;

function TXxmProjectEntry.ProjectLoaded: boolean;
begin
  Result:=FProject<>nil;
end;

function TXxmProjectEntry.GetModulePath: WideString;
begin
  Result:=FFilePath;
end;

procedure TXxmProjectEntry.GetFilePath(Address: WideString; var Path,
  MimeType: WideString);
var
  rf,sf,s:WideString;
  i,j,l:integer;
begin
  //TODO: widestring all the way?
  //TODO: virtual directories?
  rf:=FFilePath;
  i:=Length(rf);
  while not(i=0) and (rf[i]<>PathDelim) do dec(i);
  SetLength(rf,i);
  sf:='';
  i:=1;
  l:=Length(Address);
  while (i<=l) do
   begin
    j:=i;
    while (j<=l) and not(char(Address[j]) in ['/','\']) do inc(j);
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
    if (j<=l) and (char(Address[j]) in ['/','\']) then sf:=sf+PathDelim;
    i:=j+1;
   end;
  Path:=rf+sf;

  //find a MIME-type from registry
  i:=Length(sf)-1;
  while (i>0) and (sf[i]<>'.') do dec(i);
  MimeType:=GetExtensionMimeType(LowerCase(copy(sf,i,Length(sf)-i+1)));
end;

function TXxmProjectEntry.GetExtensionMimeType(x: AnsiString): AnsiString;
var
  r:TRegistry;
begin
  //TODO: get from settings or list? or project?
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(x) and r.ValueExists('Content Type') then
      Result:=r.ReadString('Content Type')
    else
      if (x='.log') or (x='.ini') then //override default for a few known types
        Result:='text/plain'
      else
        Result:='application/octet-stream';
  finally
    r.Free;
  end;
end;

procedure TXxmProjectEntry.Lock;
begin
  //assert FCheckMutex<>0
  if WaitForSingleObject(FCheckMutex,INFINITE)<>WAIT_OBJECT_0 then
    raise Exception.Create('ProjectEntry acquire UpdateLock failed: '+SysErrorMessage(GetLastError));
end;

procedure TXxmProjectEntry.Unlock;
begin
  //assert FCheckMutex<>0
  if not ReleaseMutex(FCheckMutex) then
    raise Exception.Create('ProjectEntry release UpdateLock failed: '+SysErrorMessage(GetLastError));
end;

function TXxmProjectEntry.GetProjectInterface(const IID: TGUID): IUnknown;
begin
  if (Self=nil) or (FProject=nil) or (FProject.QueryInterface(IID,Result)<>S_OK) then Result:=nil;
end;

end.
