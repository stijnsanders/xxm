unit xxmHttpPReg;

interface

uses Windows, SysUtils, xxm, xxmPReg, MSXML2_TLB;

type
  TXxmProjectCacheEntry=class(TXxmProjectEntry)
  protected
    procedure SetSignature(const Value: AnsiString); override;
    function GetExtensionMimeType(x:AnsiString): AnsiString; override;
    function GetAllowInclude: boolean; override;
  published
    constructor Create(Name,FilePath:WideString;LoadCopy:boolean);
  public
    destructor Destroy; override;
  end;

  TXxmProjectCache=class(TObject)
  private
    FLock:TRTLCriticalSection;
    ProjectCacheSize:integer;
    ProjectCache:array of TXxmProjectCacheEntry;
    FRegFilePath,FRegSignature:AnsiString;
    FRegDoc:DOMDocument;
    procedure ClearAll;
    function Grow:integer;
    function FindProject(Name:WideString):integer;
    function LoadRegistry:IXMLDOMElement;
  public
    constructor Create;
    destructor Destroy; override;

    function GetProject(Name:WideString):TXxmProjectCacheEntry;
    function DefaultProject:AnsiString;
    function SingleProject:AnsiString;
    procedure ReleaseProject(Name:WideString);
  end;

  EXxmProjectRegistryError=class(Exception);
  EXxmProjectNotFound=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);
  EXxmProjectAliasDepth=class(Exception);

var
  XxmProjectCache:TXxmProjectCache;
  GlobalAllowLoadCopy:boolean;

implementation

uses Registry, Variants;

resourcestring
  SXxmProjectRegistryError='Could not open project registry "__"';
  SXxmProjectNotFound='xxm Project "__" not defined.';
  SXxmModuleNotFound='xxm Module "__" does not exist.';
  SXxmProjectLoadFailed='xxm Project load "__" failed.';
  SXxmFileTypeAccessDenied='Access denied to this type of file';
  SXxmProjectAliasDepth='xxm Project "__": aliasses are limited to 8 in sequence';

const
  XxmRegFileName='xxm.xml';

{
function PathIsRelative(lpszPath:PWideChar):LongBool;
  stdcall; external 'shlwapi.dll' name 'PathIsRelativeW';
function PathCombine(lpszDest,lpszDir,lpszFile:PWideChar):PWideChar;
  stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
}

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(Name, FilePath: WideString; LoadCopy: boolean);
begin
  inherited Create(LowerCase(Name));//lowercase here!
  FFilePath:=FilePath;
  if LoadCopy then FLoadPath:=FFilePath+'_'+IntToHex(GetCurrentProcessId,4);
end;

destructor TXxmProjectCacheEntry.Destroy;
begin
  //pointer(FProject):=nil;//strange, project modules get closed before this happens
  inherited;
end;

function TXxmProjectCacheEntry.GetExtensionMimeType(x: AnsiString): AnsiString;
begin
  if (x='.xxl') or (x='.xxu') or (x='.exe') or (x='.dll') or (x='.xxmp') or (x='.udl') then //more? settings?
    raise EXxmFileTypeAccessDenied.Create(SXxmFileTypeAccessDenied);
  Result:=inherited GetExtensionMimeType(x);
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: AnsiString);
var
  x:IXMLDOMElement;
begin
  FSignature := Value;
  x:=XxmProjectCache.LoadRegistry.selectSingleNode(
    'Project[@Name="'+Name+'"]') as IXMLDOMElement;
  if x=nil then
    raise EXxmProjectNotFound.Create(StringReplace(
      SXxmProjectNotFound,'__',Name,[]));
  x.setAttribute('Signature',FSignature);
  x.ownerDocument.save(XxmProjectCache.FRegFilePath);
end;

function TXxmProjectCacheEntry.GetAllowInclude: boolean;
var
  x:IXMLDOMElement;
begin
  x:=XxmProjectCache.LoadRegistry.selectSingleNode(
    'Project[@Name="'+Name+'"]') as IXMLDOMElement;
  if x=nil then
    raise EXxmProjectNotFound.Create(StringReplace(
      SXxmProjectNotFound,'__',Name,[]));
  Result:=VarToStr(x.getAttribute('AllowInclude'))='1';
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
var
  i:integer;
begin
  inherited;
  ProjectCacheSize:=0;
  InitializeCriticalSection(FLock);
  //assert coinitialize called?
  FRegDoc:=CoDOMDocument.Create;
  FRegSignature:='-';

  SetLength(FRegFilePath,MAX_PATH);
  SetLength(FRegFilePath,GetModuleFileNameA(HInstance,PAnsiChar(FRegFilePath),MAX_PATH));
  if Copy(FRegFilePath,1,4)='\\?\' then FRegFilePath:=Copy(FRegFilePath,5,Length(FRegFilePath)-4);
  i:=Length(FRegFilePath);
  while (i<>0) and (FRegFilePath[i]<>PathDelim) do dec(i);
  FRegFilePath:=Copy(FRegFilePath,1,i);

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
  l:AnsiString;
begin
  Result:=0;
  l:=LowerCase(Name);
  //assert cache stores ProjectName already LowerCase!
  while (Result<ProjectCacheSize) and (
    (ProjectCache[Result]=nil) or (ProjectCache[Result].Name<>l)) do inc(Result);
  if Result=ProjectCacheSize then Result:=-1;
end;

function TXxmProjectCache.LoadRegistry: IXMLDOMElement;
var
  fh:THandle;
  fd:TWin32FindDataA;
  s:AnsiString;
begin
  //signature
  fh:=FindFirstFileA(PAnsiChar(FRegFilePath+XxmRegFileName),fd);
  if fh=INVALID_HANDLE_VALUE then s:='' else
   begin
    s:=IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
  if FRegSignature<>s then
   begin
    if not(FRegDoc.load(FRegFilePath+XxmRegFileName)) then
      raise EXxmProjectRegistryError.Create(StringReplace(
        SXxmProjectRegistryError,'__',FRegFilePath+XxmRegFileName,[])+#13#10+
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
  n,p:WideString;
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
        if x<>nil then
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

      p:=y.text;
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

      //TODO: extra flags,settings?
      Result:=TXxmProjectCacheEntry.Create(Name,p,
        GlobalAllowLoadCopy and (VarToStr((x as IXMLDOMElement).getAttribute('LoadCopy'))<>'0'));//='1'));

      Result.FSignature:=VarToStr((x as IXMLDOMElement).getAttribute('Signature'));

      i:=0;
      while (i<ProjectCacheSize) and (ProjectCache[i]<>nil) do inc(i);
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
  if i<>-1 then FreeAndNil(ProjectCache[i]);
end;

procedure TXxmProjectCache.ClearAll;
var
  i:integer;
begin
  for i:=0 to ProjectCacheSize-1 do FreeAndNil(ProjectCache[i]);
  SetLength(ProjectCache,0);
  ProjectCacheSize:=0;
end;

function TXxmProjectCache.DefaultProject: AnsiString;
begin
  Result:=VarToStr(LoadRegistry.getAttribute('DefaultProject'));
  if Result='' then Result:='xxm';
end;

function TXxmProjectCache.SingleProject: AnsiString;
begin
  Result:=VarToStr(LoadRegistry.getAttribute('SingleProject'));
end;

initialization
  GlobalAllowLoadCopy:=true;//default
  //XxmProjectCache:=TXxmProjectCache.Create;//moved to project source
finalization
  XxmProjectCache.Free;

end.
