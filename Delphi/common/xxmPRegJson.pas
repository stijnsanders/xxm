unit xxmPRegJson;

interface

uses Windows, SysUtils, xxm, xxmPReg, jsonDoc;

type
  TXxmProjectCacheEntry=class(TXxmProjectEntry)
  private
    FAllowInclude:boolean;
  protected
    procedure SetSignature(const Value: AnsiString); override;
    function GetExtensionMimeType(const x: AnsiString): AnsiString; override;
    function GetAllowInclude: boolean; override;
  public
    constructor Create(const Name, FilePath: WideString;
      LoadCopy, AllowInclude: boolean);
    destructor Destroy; override;
  end;

  TXxmProjectCacheJson=class(TXxmProjectCache)
  private
    FProjectsLength,FProjectsCount:integer;
    FProjects:array of record
      Name,Alias:AnsiString;
      Entry:TXxmProjectCacheEntry;
      LoadCheck:boolean;
    end;
    FRegFilePath,FRegSignature,FDefaultProject,FSingleProject:AnsiString;
    FRegLastCheckTC:cardinal;
    FFavIcon:OleVariant;
    function FindProject(const Name:WideString):integer;
    function GetRegistrySignature: AnsiString;
    function GetRegistry: IJSONDocument;
    procedure SetSignature(const Name: WideString;
      const Value: AnsiString);
    procedure LoadFavIcon(const FilePath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckRegistry;

    function ProjectFromURI(Context:IXxmContext;const URI:AnsiString;
      var i:integer; var ProjectName,FragmentName:WideString):boolean;
    function GetProject(const Name:WideString):TXxmProjectCacheEntry;
    procedure ReleaseProject(const Name:WideString);
  end;

  EXxmProjectRegistryError=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);
  EXxmProjectAliasDepth=class(Exception);

var
  XxmProjectCache:TXxmProjectCacheJson;

implementation

uses Registry, Variants, Classes, xxmHeaders, xxmContext, xxmConvert2;

resourcestring
  SXxmProjectRegistryError='Could not open project registry "__"';
  SXxmFileTypeAccessDenied='Access denied to this type of file';
  SXxmProjectAliasDepth='xxm Project "__": aliasses are limited to 8 in sequence';

const
  XxmRegFileName_XML='xxm.xml';
  XxmRegFileName='xxm.json';
  XxmRegCheckIntervalMS=1000;

{
function PathIsRelative(lpszPath:PWideChar):LongBool;
  stdcall; external 'shlwapi.dll' name 'PathIsRelativeW';
function PathCombine(lpszDest,lpszDir,lpszFile:PWideChar):PWideChar;
  stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
}

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(const Name, FilePath: WideString;
  LoadCopy, AllowInclude: boolean);
begin
  inherited Create(Name);
  FAllowInclude:=AllowInclude;
  SetFilePath(FilePath,LoadCopy);
end;

destructor TXxmProjectCacheEntry.Destroy;
begin
  //pointer(FProject):=nil;//strange, project modules get closed before this happens
  inherited;
end;

function TXxmProjectCacheEntry.GetExtensionMimeType(const x: AnsiString): AnsiString;
begin
  if (x='.xxl') or (x='.xxu') or (x='.xxmp') or (x='.xxlc')
    or (x='.exe') or (x='.dll') or (x='.udl') //or (x='.pas')?
    //more? settings?
  then
    raise EXxmFileTypeAccessDenied.Create(SXxmFileTypeAccessDenied);
  Result:=inherited GetExtensionMimeType(x);
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: AnsiString);
begin
  FSignature:=Value;
  XxmProjectCache.SetSignature(Name,Value);
end;

function TXxmProjectCacheEntry.GetAllowInclude: boolean;
begin
  XxmProjectCache.CheckRegistry;
  Result:=FAllowInclude;
end;

{ TXxmProjectCacheXml }

constructor TXxmProjectCacheJson.Create;
var
  i:integer;
  r:TResourceStream;
const
  RT_HTML = MakeIntResource(23);
begin
  inherited;
  //assert coinitialize called?
  FProjectsLength:=0;
  FProjectsCount:=0;
  FRegSignature:='-';
  FRegLastCheckTC:=GetTickCount-XxmRegCheckIntervalMS-1;

  SetLength(FRegFilePath,MAX_PATH);
  SetLength(FRegFilePath,GetModuleFileNameA(HInstance,
    PAnsiChar(FRegFilePath),MAX_PATH));
  if Copy(FRegFilePath,1,4)='\\?\' then
    FRegFilePath:=Copy(FRegFilePath,5,Length(FRegFilePath)-4);
  i:=Length(FRegFilePath);
  while (i<>0) and (FRegFilePath[i]<>PathDelim) do dec(i);
  FRegFilePath:=Copy(FRegFilePath,1,i);

  //settings?

  CheckRegistry;

  r:=TResourceStream.Create(HInstance,'favicon',RT_HTML);
  try
    i:=r.Size;
    FFavIcon:=VarArrayCreate([0,i-1],varByte);
    r.Read(VarArrayLock(FFavIcon)^,i);
    VarArrayUnlock(FFavIcon);
  finally
    r.Free;
  end;
end;

destructor TXxmProjectCacheJson.Destroy;
var
  i:integer;
begin
  for i:=0 to FProjectsCount-1 do FreeAndNil(FProjects[i].Entry);
  SetLength(FProjects,0);
  inherited;
end;

function TXxmProjectCacheJson.FindProject(const Name: WideString): integer;
var
  n:AnsiString;
begin
  n:=LowerCase(Name);
  //assert cache stores ProjectName already LowerCase!
  Result:=0;
  while (Result<FProjectsCount) and (FProjects[Result].Name<>n) do
    inc(Result);
  if Result=FProjectsCount then Result:=-1;
end;

function TXxmProjectCacheJson.GetRegistrySignature:AnsiString;
var
  fh:THandle;
  fd:TWin32FindDataA;
begin
  //assert in FLock
  FRegLastCheckTC:=GetTickCount;
  fh:=FindFirstFileA(PAnsiChar(FRegFilePath+XxmRegFileName),fd);
  if fh=INVALID_HANDLE_VALUE then Result:='' else
   begin
    Result:=IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
end;

function TXxmProjectCacheJson.GetRegistry:IJSONDocument;
var
  f:TFileStream;
  i:integer;
  s:AnsiString;
  w:WideString;
begin
  //assert in FLock
  //assert CoInitialize called
  Result:=JSON;

  //TRANSITIONAL
  try
    f:=TFileStream.Create(FRegFilePath+XxmRegFileName,
      fmOpenRead or fmShareDenyWrite);

  except
    on EFOpenError do
     begin
      ConvertProjectReg;
      f:=TFileStream.Create(FRegFilePath+XxmRegFileName,
        fmOpenRead or fmShareDenyWrite);
     end;
  end;

  try
    i:=f.Size;
    SetLength(s,i);
    if f.Read(s[1],i)<>i then RaiseLastOSError;
    if (i>=3) and (s[1]=#$EF) and (s[2]=#$BB) and (s[3]=#$BF) then
      Result.Parse(UTF8Decode(Copy(s,4,i-3)))
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

procedure TXxmProjectCacheJson.CheckRegistry;
var
  s:AnsiString;
  p:WideString;
  i:integer;
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
            i:=FindProject(e.Key);
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
              FProjects[i].Name:=LowerCase(e.Key);
              FProjects[i].Entry:=nil;//create see below
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
                FProjects[i].Entry:=TXxmProjectCacheEntry.Create(e.Key,p,
                  VarToBool(d1['loadCopy']),
                  VarToBool(d1['allowInclude']))
              else
               begin
                if p<>FProjects[i].Entry.FilePath then
                 begin
                  FProjects[i].Entry.SetFilePath(p,
                    VarToBool(d1['loadCopy']));
                  FProjects[i].Entry.FAllowInclude:=
                    VarToBool(d1['allowInclude']);
                 end;
               end;
              FProjects[i].Entry.FSignature:=
                VarToStr(d1['signature']);
              FProjects[i].Entry.FBufferSize:=
                BSize(VarToStr(d1['bufferSize']));
             end
            else
              FreeAndNil(FProjects[i].Entry);
           end;
          //clean-up items removed from XML
          for i:=0 to FProjectsCount-1 do
            if not FProjects[i].LoadCheck then
             begin
              FProjects[i].Name:='';
              FProjects[i].Alias:='';
              FreeAndNil(FProjects[i].Entry);
             end;
          LoadFavIcon(VarToStr(d['singleProject'])+'.ico');
         end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TXxmProjectCacheJson.SetSignature(const Name:WideString;
  const Value:AnsiString);
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
    s:=Utf8ByteOrderMark+UTF8Encode(d.ToString);
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

function TXxmProjectCacheJson.GetProject(const Name: WideString):
  TXxmProjectCacheEntry;
var
  i,d:integer;
  found:boolean;
begin
  Result:=nil;//counter warning;
  CheckRegistry;
  EnterCriticalSection(FLock);
  try
    found:=false;
    d:=0;
    i:=FindProject(Name);
    while (i<>-1) and not(found) do
      if FProjects[i].Alias='' then found:=true else
       begin
        inc(d);
        if d=8 then raise EXxmProjectAliasDepth.Create(StringReplace(
          SXxmProjectAliasDepth,'__',Name,[]));
        i:=FindProject(FProjects[i].Alias);
       end;
    if i=-1 then raise EXxmProjectNotFound.Create(StringReplace(
      SXxmProjectNotFound,'__',Name,[]));
    Result:=FProjects[i].Entry;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmProjectCacheJson.ReleaseProject(const Name: WideString);
var
  i:integer;
begin
  //CheckRegistry?
  EnterCriticalSection(FLock);
  try
    i:=FindProject(Name);
    //if i=-1 then raise?
    if i<>-1 then
     begin
      FProjects[i].Name:='';
      FProjects[i].Alias:='';
      FreeAndNil(FProjects[i].Entry);
     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TXxmProjectCacheJson.ProjectFromURI(Context:IXxmContext;
  const URI:AnsiString;var i:integer;
  var ProjectName,FragmentName:WideString):boolean;
var
  j,l:integer;
  x:AnsiString;
begin
  CheckRegistry;
  l:=Length(URI);
  if FSingleProject='' then
   begin
    while (i<=l) and not(URI[i] in ['/','?','&','$','#']) do inc(i);
    ProjectName:=Copy(URI,2,i-2);
    if ProjectName='' then
     begin
      if (i<=l) and (URI[i]='/') then x:='' else x:='/';
      Context.Redirect('/'+FDefaultProject+x+Copy(URI,i,l-i+1),true);
     end;
    if (i>l) and (l>1) then
      if URI='/favicon.ico' then
       begin
        Context.ContentType:='image/x-icon';
        (Context as IxxmHttpHeaders).ResponseHeaders['Content-Length']:=
          IntToStr(VarArrayHighBound(FFavIcon,1)+1);
        Context.SendHTML(FFavIcon);
        raise EXxmPageRedirected.Create(URI);
       end
      else
        Context.Redirect(URI+'/',true)
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

procedure TXxmProjectCacheJson.LoadFavIcon(const FilePath:string);
var
  f:TFileStream;
  i:integer;
begin
  if FilePath<>'' then
    try
      f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
      try
        i:=f.Size;
        FFavIcon:=VarArrayCreate([0,i-1],varByte);
        f.Read(VarArrayLock(FFavIcon)^,i);
        VarArrayUnlock(FFavIcon);
      finally
        f.Free;
      end;
    except
      on EFOpenError do ;//silent
    end;
end;

initialization
  //XxmProjectCache:=TXxmProjectCacheXml.Create;//moved to project source
finalization
  XxmProjectCache.Free;

end.
