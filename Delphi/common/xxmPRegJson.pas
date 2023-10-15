unit xxmPRegJson;

interface

uses Windows, SysUtils, xxm, xxmPReg, jsonDoc;

type
  TXxmProjectCacheEntry=class(TXxmProjectEntry)
  private
    FAllowInclude,FNTLM,FNegotiate:boolean;
  protected
    procedure SetSignature(const Value: string); override;
    function GetExtensionMimeType(const x: AnsiString): AnsiString; override;
    function GetAllowInclude: boolean; override;
  public
    constructor Create(const Name, FilePath: WideString; LoadCopy: boolean);
    destructor Destroy; override;
    property NTLM:boolean read FNTLM;
    property Negotiate:boolean read FNegotiate;
  end;

  TXxmProjectCacheJson=class(TXxmProjectCache)
  private
    FProjectsLength,FProjectsCount:integer;
    FProjects:array of record
      Name,Alias:string;
      Entry:TXxmProjectCacheEntry;
      LoadCheck:boolean;
      SortIndex:integer;
    end;
    FRegFilePath,FRegSignature,FDefaultProject,FSingleProject:string;
    FRegLastCheckTC:cardinal;
    FFavIcon:OleVariant;
    FAuthCache:array of record
      SessionID:string;
      AuthName:AnsiString;
      Expires:TDateTime;
    end;
    FAuthCacheIndex,FAuthCacheSize:integer;
    function FindProject(const Name: string): integer;
    function GetRegistrySignature: string;
    function GetRegistry: IJSONDocument;
    procedure SetSignature(const Name: WideString; const Value: string);
    procedure LoadFavIcon(const FilePath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckRegistry;

    function ProjectFromURI(Context:IXxmContext;const URI:AnsiString;
      var i:integer; var ProjectName,FragmentName:WideString):boolean;
    function GetProject(const Name:WideString):TXxmProjectCacheEntry;
    procedure ReleaseProject(const Name:WideString);

    function GetAuthCache(const SessionID:string):AnsiString;
    procedure SetAuthCache(const SessionID:string;const AuthName:AnsiString);
  end;

  EXxmProjectRegistryError=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);
  EXxmProjectAliasDepth=class(Exception);

var
  XxmProjectCache:TXxmProjectCacheJson;
  XxmProjectCacheError:string;

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

{$IF not Declared(UTF8ToWideString)}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$IFEND}

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(const Name, FilePath: WideString;
  LoadCopy: boolean);
begin
  inherited Create(Name);
  SetFilePath(FilePath,LoadCopy);
  FAllowInclude:=false;//default
  FNTLM:=false;//default
  FNegotiate:=false;//default
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

procedure TXxmProjectCacheEntry.SetSignature(const Value: string);
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
  p:pointer;
const
  RT_HTML = MakeIntResource(23);
begin
  inherited;
  //assert coinitialize called?
  FProjectsLength:=0;
  FProjectsCount:=0;
  FRegSignature:='-';
  FRegLastCheckTC:=GetTickCount-XxmRegCheckIntervalMS-1;
  FAuthCacheIndex:=0;
  FAuthCacheSize:=0;

  SetLength(FRegFilePath,MAX_PATH);
  SetLength(FRegFilePath,GetModuleFileName(HInstance,
    PChar(FRegFilePath),MAX_PATH));
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

destructor TXxmProjectCacheJson.Destroy;
var
  i:integer;
begin
  for i:=0 to FProjectsCount-1 do FreeAndNil(FProjects[i].Entry);
  SetLength(FProjects,0);
  inherited;
end;

function LCSC(const a,b:string):integer; //lower case string compare
var
  i,al,bl:integer;
begin
  al:=Length(a);
  bl:=Length(b);
  i:=1;
  Result:=0;
  while (Result=0) and (i<=al) and (i<=bl) do
    if a[i]<b[i] then
      Result:=-1
    else
      if a[i]>b[i] then
        Result:=1
      else
        inc(i);
  if Result=0 then
    if (i<=al) then
      Result:=1
    else
      if (i<=bl) then
        Result:=-1;
end;

function TXxmProjectCacheJson.FindProject(const Name: string): integer;
var
  n:string;
  a,b,c,m:integer;
begin
  n:=LowerCase(Name);
  //assert cache stores ProjectName already LowerCase!
  a:=0;
  b:=FProjectsCount-1;
  Result:=-1;
  while a<=b do
   begin
    c:=(a+b) div 2;
    m:=LCSC(n,FProjects[FProjects[c].SortIndex].Name);
    if m<0 then
      if b=c then dec(b) else b:=c
    else
      if m>0 then
        if a=c then inc(a) else a:=c
      else
       begin
        b:=a-1;//end loop
        Result:=FProjects[c].SortIndex;
       end;
   end;
end;

function TXxmProjectCacheJson.GetRegistrySignature: string;
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

procedure TXxmProjectCacheJson.CheckRegistry;
var
  s,n:string;
  p:WideString;
  i,j,a,b,c,m:integer;
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
            //i:=FindProject(e.Key);

            n:=LowerCase(e.Key);
            a:=0;
            b:=FProjectsCount-1;
            i:=-1;
            while a<=b do
             begin
              c:=(a+b) div 2;
              m:=LCSC(n,FProjects[FProjects[c].SortIndex].Name);
              if m<0 then
                if b=c then dec(b) else b:=c
              else
                if m>0 then
                  if a=c then inc(a) else a:=c
                else
                 begin
                  a:=c;
                  b:=a-1;
                  i:=c;
                 end;
             end;

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
                FProjects[i].Entry:=TXxmProjectCacheEntry.Create(e.Key,p,
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
          if FSingleProject<>'' then
            LoadFavIcon(FSingleProject+'.ico');
         end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TXxmProjectCacheJson.SetSignature(const Name:WideString;
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
      AnsiChar(Utf8ByteOrderMark[0])+
      AnsiChar(Utf8ByteOrderMark[1])+
      AnsiChar(Utf8ByteOrderMark[2])+
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

function TXxmProjectCacheJson.GetProject(const Name: WideString):
  TXxmProjectCacheEntry;
var
  i,d:integer;
  found:boolean;
  e:TXxmProjectCacheEntry;
begin
{$IF CompilerVersion<20}
  e:=nil;//counter warning
{$IFEND}
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

procedure TXxmProjectCacheJson.LoadFavIcon(const FilePath:string);
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

function TXxmProjectCacheJson.GetAuthCache(const SessionID: string): AnsiString;
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

procedure TXxmProjectCacheJson.SetAuthCache(const SessionID: string;
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
  //XxmProjectCache:=TXxmProjectCacheXml.Create;//moved to project source
finalization
  XxmProjectCache.Free;

end.
