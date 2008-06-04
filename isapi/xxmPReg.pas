unit xxmPReg;

interface

uses Windows, SysUtils, xxm, MSXML2_TLB;

type
  TXxmProjectCacheEntry=class(TObject)
  private
    FName,FFilePath:WideString;
    FProject: IXxmProject;
    FHandle:THandle;
    FSignature:string;
    FContextCount:integer;
    function GetProject: IXxmProject;
    procedure SetSignature(const Value: string);
  published
    constructor Create(Name,FilePath:WideString);
  public
    LastCheck:cardinal;
    procedure Release;
    destructor Destroy; override;
    procedure GetFilePath(Address:WideString;var Path,MimeType:string);
    procedure OpenContext;
    procedure CloseContext;
    property Name:WideString read FName;
    property Project: IXxmProject read GetProject;
    property ModulePath:WideString read FFilePath;
    property Signature:string read FSignature write SetSignature;
  end;

  TXxmProjectCache=class(TObject)
  private
    FLock:TRTLCriticalSection;
    ProjectCacheSize:integer;
    ProjectCache:array of TXxmProjectCacheEntry;
    FRegFilePath,FRegSignature:string;
    FRegDoc:DOMDocument;
    procedure ClearAll;
    function Grow:integer;
    function FindProject(Name:WideString):integer;
    function LoadRegistry:IXMLDOMElement;
  public
    constructor Create;
    destructor Destroy; override;

    function GetProject(Name:WideString):TXxmProjectCacheEntry;
    function DefaultProject:string;
    function SingleProject:string;
    procedure ReleaseProject(Name:WideString);
  end;

  TXxmAutoBuildHandler=function(pce:TXxmProjectCacheEntry;
    Context: IXxmContext; ProjectName:WideString):boolean;

  EXxmProjectRegistryError=class(Exception);
  EXxmProjectNotFound=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);
  EXxmProjectAliasDepth=class(Exception);

var
  XxmProjectCache:TXxmProjectCache;

implementation

uses Registry, Variants;

resourcestring
  SXxmProjectRegistryError='Could not open project registry "__"';
  SXxmProjectNotFound='xxm Project "__" not defined.';
  SXxmModuleNotFound='xxm Module "__" does not exist.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';
  SXxmFileTypeAccessDenied='Access denied to this type of file';
  SXxmProjectAliasDepth='xxm Project "__": aliasses are limited to 8 in sequence';

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(Name, FilePath: WideString);
begin
  inherited Create;
  FName:=LowerCase(Name);//lowercase here!
  FFilePath:=FilePath;
  FProject:=nil;
  FHandle:=0;
  FContextCount:=0;
end;

procedure TXxmProjectCacheEntry.Release;
begin
  //attention for deadlocks! use OpenContext/CloseContext
  //XxmAutoBuildHandler is supposed to lock any new requests
  while not(FContextCount=0) do Sleep(1);

  //finalization gets called on last-loaded libraries first,
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
   end;
end;

destructor TXxmProjectCacheEntry.Destroy;
begin
  pointer(FProject):=nil;//strange, project modules get closed before this happens
  Release;
  inherited;
end;

procedure TXxmProjectCacheEntry.GetFilePath(Address:WideString;
  var Path, MimeType: string);
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
  sf:=LowerCase(copy(sf,i,Length(sf)-i));

  if (sf='.xxl') or (sf='.exe') or (sf='.dll') or (sf='.xxmp') then //more? settings?
    raise EXxmFileTypeAccessDenied.Create(SXxmFileTypeAccessDenied);

  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(sf) and r.ValueExists('Content Type') then
      MimeType:=r.ReadString('Content Type');
    if MimeType='' then MimeType:='application/octet-stream';
  finally
    r.Free;
  end;

end;

function TXxmProjectCacheEntry.GetProject: IXxmProject;
var
  lp:TXxmProjectLoadProc;
begin
  if FProject=nil then
   begin
    if not(FileExists(FFilePath)) then
      raise EXxmModuleNotFound.Create(StringReplace(
        SXxmModuleNotFound,'__',FFilePath,[]));
    FHandle:=LoadLibraryW(PWideChar(FFilePath));
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

procedure TXxmProjectCacheEntry.SetSignature(const Value: string);
var
  x:IXMLDOMElement;
begin
  FSignature := Value;
  x:=XxmProjectCache.LoadRegistry.selectSingleNode(
    'Project[@Name="'+FName+'"]') as IXMLDOMElement;
  if x=nil then
    raise EXxmProjectNotFound.Create(StringReplace(
      SXxmProjectNotFound,'__',FName,[]));
  x.setAttribute('Signature',FSignature);
  x.ownerDocument.save(XxmProjectCache.FRegFilePath);
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
var
  i:integer;
begin
  inherited;
  ProjectCacheSize:=0;
  InitializeCriticalSection(FLock);
  FRegDoc:=nil;
  FRegSignature:='-';

  SetLength(FRegFilePath,$400);
  SetLength(FRegFilePath,GetModuleFileName(HInstance,PChar(FRegFilePath),$400));
  if Copy(FRegFilePath,1,4)='\\?\' then FRegFilePath:=Copy(FRegFilePath,5,Length(FRegFilePath)-4);
  i:=Length(FRegFilePath);
  while not(i=0) and not(FRegFilePath[i]=PathDelim) do dec(i);
  FRegFilePath:=Copy(FRegFilePath,1,i)+'xxm.xml';

  //settings?
end;

destructor TXxmProjectCache.Destroy;
begin
  ClearAll;
  DeleteCriticalSection(FLock);
  FRegDoc:=nil;
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

function TXxmProjectCache.LoadRegistry: IXMLDOMElement;
var
  fh:THandle;
  fd:TWin32FindData;
  s:string;
begin
  if FRegDoc=nil then FRegDoc:=CoDOMDocument.Create;

  //signature
  fh:=FindFirstFile(PChar(FRegFilePath),fd);
  if fh=INVALID_HANDLE_VALUE then s:='' else
   begin
    s:=IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
  if not(FRegSignature=s) then
   begin
    if not(FRegDoc.load(FRegFilePath)) then
      raise EXxmProjectRegistryError.Create(StringReplace(
        SXxmProjectRegistryError,'__',FRegFilePath,[])+#13#10+
        FRegDoc.parseError.reason);
    FRegSignature:=s;
   end;
  //assert documentElement.nodeName='ProjectRegistry'
  Result:=FRegDoc.documentElement;
end;

function TXxmProjectCache.GetProject(Name: WideString): TXxmProjectCacheEntry;
var
  i,d:integer;
  x,y:IXMLDOMNode;
  n:WideString;
  found:boolean;
begin
  Result:=nil;//counter warning;
  EnterCriticalSection(FLock);
  try
    //assert CoInitialize called
    i:=FindProject(Name);
    if i=-1 then
     begin
      n:=Name;
      d:=0;
      found:=false;
      repeat
        x:=LoadRegistry.selectSingleNode('Project[@Name="'+n+'"]');
        if not(x=nil) then
         begin
          y:=x.attributes.getNamedItem('Alias');
          if y=nil then found:=true else
           begin
            inc(d);
            if d=8 then raise EXxmProjectAliasDepth.Create(StringReplace(
              SXxmProjectAliasDepth,'__',Name,[]));
            n:=y.text;
           end;
         end;
      until (x=nil) or found;

      if x=nil then y:=nil else y:=x.selectSingleNode('ModulePath') as IXMLDOMElement;
      if y=nil then raise EXxmProjectNotFound.Create(StringReplace(
        SXxmProjectNotFound,'__',Name,[]));

      //TODO: extra flags,settings?

      Result:=TXxmProjectCacheEntry.Create(Name,y.text);

      Result.FSignature:=VarToStr((x as IXMLDOMElement).getAttribute('Signature'));

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
  for i:=0 to ProjectCacheSize-1 do FreeAndNil(ProjectCache[i]);
  SetLength(ProjectCache,0);
  ProjectCacheSize:=0;
end;

function TXxmProjectCache.DefaultProject: string;
begin
  Result:=VarToStr(LoadRegistry.getAttribute('DefaultProject'));
  if Result='' then Result:='xxm';
end;

function TXxmProjectCache.SingleProject: string;
begin
  Result:=VarToStr(LoadRegistry.getAttribute('SingleProject'));
end;

initialization
  XxmProjectCache:=TXxmProjectCache.Create;
finalization
  XxmProjectCache.Free;

end.
