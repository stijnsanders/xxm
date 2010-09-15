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
  protected
    FSignature:AnsiString;
    FFilePath:WideString;
    function GetProject: IXxmProject;
    procedure LoadProject; virtual;
    function GetModulePath:WideString; virtual;
    procedure SetSignature(const Value: AnsiString); virtual; abstract;
    function ProjectLoaded:boolean;
    function GetExtensionMimeType(x:AnsiString): AnsiString; virtual;
  published
    constructor Create(Name:WideString);//abstract! only here for initialization
    destructor Destroy; override;
  public
    LastCheck:cardinal;
    procedure OpenContext;
    procedure CloseContext;
    procedure Release; virtual;
    procedure GetFilePath(Address:WideString;var Path,MimeType:WideString);
    property Name: WideString read FName;
    property Project: IXxmProject read GetProject;
    property ModulePath:WideString read GetModulePath;
    property Signature:AnsiString read FSignature write SetSignature;
  end;

  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);

  TXxmAutoBuildHandler=function(pce:TXxmProjectEntry;
    Context:IXxmContext; ProjectName:WideString):boolean;

var
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

implementation

uses Windows, Registry;

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
  FSignature:='';//used for auto-build/auto-update
  LastCheck:=GetTickCount-100000;
end;

destructor TXxmProjectEntry.Destroy;
begin
  Release;
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
  if not(FileExists(FFilePath)) then
    raise EXxmModuleNotFound.Create(StringReplace(
      SXxmModuleNotFound,'__',FFilePath,[]));
  FHandle:=LoadLibraryW(PWideChar(FFilePath));
  if FHandle=0 then RaiseLastOSError;
  @lp:=GetProcAddress(FHandle,'XxmProjectLoad');
  if @lp=nil then RaiseLastOSError;
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
    if (s='') or (s='.') then
      //nothing
    else
    if (s='..') then
     begin
      //try to go back, but not into rf (raise?)
      i:=Length(sf)-1;
      while (i>0) and not(sf[i]=PathDelim) do dec(i);
      SetLength(sf,i);
     end
    else
      sf:=sf+s+PathDelim;//DirectoryExists()??
    if (j<=l) and (char(Address[j]) in ['/','\']) then sf:=sf+PathDelim;
    i:=j+1;
   end;
  Path:=rf+Copy(sf,1,Length(sf)-1);

  //find a MIME-type from registry
  i:=Length(sf)-1;
  while (i>0) and not(sf[i]='.') do dec(i);
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

end.
