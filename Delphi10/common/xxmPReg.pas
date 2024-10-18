unit xxmPReg;

interface

uses Windows, SysUtils, xxm2;

type
  TProjectEntry=class(TObject)
  private
    FLock:TRTLCriticalSection;
    FContextCount:integer;
    FName:UTF8String;
    FFilePath,FLoadPath,FLoadSignature:string;
    FLoadCopy:boolean;
    FLibrary:THandle;
    FProject:PxxmProject;
    procedure CheckLibrary;
  public
    AllowInclude,Negotiate,NTLM:boolean;
    BufferSize:integer;
    Signature:string;

    xxmPage:FxxmPage;
    xxmFragment:FxxmFragment;
    xxmClearContext:FxxmClearContext;
    xxmHandleException:FxxmHandleException;
    xxmReleasingContexts:FxxmReleasingContexts;
    xxmReleasingProject:FxxmReleasingProject;

    constructor Create(const Name:UTF8String;const FilePath:string;LoadCopy:boolean);
    destructor Destroy; override;
    procedure SetFilePath(const FilePath:string;LoadCopy:boolean);
    property FilePath:string read FFilePath;
    property Project:PxxmProject read FProject;

    procedure OpenContext;
    procedure CloseContext;

    procedure GetFilePath(const Address:UTF8String;var Path:string;var MimeType:UTF8String);

  end;

  TProjectRegistry=class(TObject)
  private
    FLock:TRTLCriticalSection;
    FRegFilePath,FRegSignature:string;
    FRegLastCheckTC:cardinal;
    FProjects:array of record
      Name:UTF8String;
      Entry:TProjectEntry;
      LoadCheck:boolean;
    end;
    FProjectsIndex,FProjectsSize:integer;
    FDefaultProject,FSingleProject:UTF8String;
    FHosts:array of record
      Host,DefaultProject,SingleProject:UTF8String;
      Projects:array of UTF8String;
      LoadCheck:boolean;
    end;
    FHostsIndex,FHostsSize:integer;
    function FindProject(Name:PUTF8Char):integer;
    function FindHost(Host:PUTF8Char):integer;
  public
    FavIcon:array of byte;

    constructor Create;
    destructor Destroy; override;
    procedure CheckRegistry;

    function GetProjectName(Host,Name:PUTF8Char):PUTF8Char;
    function GetProjectEntry(Name:PUTF8Char):TProjectEntry;
  end;

  EXxmProjectNotFound=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);

  TXxmAutoBuildHandler=function(Entry: TProjectEntry;
    Context: TObject; const ProjectName: UTF8String): boolean;

var
  Xxm2:Pxxm2;
  XxmProjectRegistry:TProjectRegistry;
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

  GlobalAllowLoadCopy:boolean;

implementation

uses Classes, Variants, jsonDoc, xxmTools, Registry;

const
  XxmRegFileName='xxm2.json';
  XxmRegCheckIntervalMS=10000;//TODO: from config? signal?

function LoadJSON(const FilePath:string):IJSONDocument;
var
  f:TFileStream;
  i:integer;
  s:AnsiString;
  w:WideString;
begin
  Result:=JSON;
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
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

function VarToBool(const v:OleVariant):boolean;
begin
  Result:=not(VarIsNull(v)) and boolean(v);
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

{ TProjectRegistry }

constructor TProjectRegistry.Create;
var
  i,l:integer;
  h1,h2:THandle;
  p:pointer;
begin
  inherited;
  InitializeCriticalSection(FLock);
  //TODO: InitializeCriticalSectionAndSpinCount?
  FProjectsIndex:=0;
  FProjectsSize:=0;
  FRegSignature:='-';
  FRegLastCheckTC:=GetTickCount-XxmRegCheckIntervalMS-1;//see CheckRegistry below
  FDefaultProject:='';
  FSingleProject:='';
  FHostsIndex:=0;
  FHostsSize:=0;

  SetLength(FRegFilePath,MAX_PATH);
  SetLength(FRegFilePath,GetModuleFileName(HInstance,
    PChar(FRegFilePath),MAX_PATH));
  if Copy(FRegFilePath,1,4)='\\?\' then
    FRegFilePath:=Copy(FRegFilePath,5,Length(FRegFilePath)-4);
  i:=Length(FRegFilePath);
  while (i<>0) and (FRegFilePath[i]<>PathDelim) do dec(i);
  FRegFilePath:=Copy(FRegFilePath,1,i);

  CheckRegistry;

  //resource icon #1 as favicon
  h1:=FindResource(HInstance,pointer(1),RT_ICON);
  h2:=LoadResource(HInstance,h1);
  p:=LockResource(h2);
  l:=SizeofResource(HInstance,h1);
  SetLength(FavIcon,l);
  Move(p^,FavIcon[0],l);
  UnlockResource(h1);
  FreeResource(h1);
end;

destructor TProjectRegistry.Destroy;
var
  i:integer;
begin
  for i:=0 to FProjectsIndex-1 do
    try
      FreeAndNil(FProjects[i].Entry);
    except
      //silent
    end;
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TProjectRegistry.CheckRegistry;
var
  fn,s:string;
  n:UTF8String;
  i,j:integer;
  d,d1:IJSONDocument;
  e:IJSONEnumerator;
  v:Variant;
begin
  if cardinal(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
   begin
    EnterCriticalSection(FLock);
    try
      //check again for threads that were waiting for lock
      if cardinal(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
       begin
        //signature
        FRegLastCheckTC:=GetTickCount;
        fn:=FRegFilePath+XxmRegFileName;
        s:=GetFileSignature(fn);
        if FRegSignature<>s then
         begin
          FRegSignature:=s;
          d:=LoadJSON(fn);

          FDefaultProject:=UTF8Encode(VarToWideStr(d['defaultProject']));
          if FDefaultProject='' then FDefaultProject:='xxm';
          FSingleProject:=UTF8Encode(VarToWideStr(d['singleProject']));

          for i:=0 to FProjectsIndex-1 do FProjects[i].LoadCheck:=false;
          e:=JSONEnum(d['projects']);
          while e.Next do
           begin
            n:=UTF8Encode(e.Key);
            d1:=JSON(e.Value);
            i:=FindProject(PUTF8Char(n));
            if (i<>-1) and (FProjects[i].LoadCheck) then i:=-1;//duplicate! raise?
            if i=-1 then
             begin
              //new
              i:=0;
              while (i<FProjectsIndex) and (FProjects[i].Name<>'') do inc(i);
              if i=FProjectsIndex then
               begin
                if FProjectsIndex=FProjectsSize then
                 begin
                  inc(FProjectsSize,8);//grow step
                  SetLength(FProjects,FProjectsSize);
                 end;
                inc(FProjectsIndex);
               end;
              FProjects[i].Name:=n;
              FProjects[i].Entry:=nil;//create see below
             end;
            FProjects[i].LoadCheck:=true;
            fn:=StringReplace(
              VarToStr(d1['path']),'/',PathDelim,[rfReplaceAll]);
            if fn='' then raise EXxmProjectNotFound.Create(
              'xxm Project invalid path "'+e.Key+'"');
            {
            if PathIsRelative(fn) then
             begin
              SetLength(fn,MAX_PATH);
              PathCombine(PChar(fn),PChar(FRegFilePath),PChar());
              SetLength(fn,Length(fn));
             end;
            }
            if (Length(fn)>2) and not((fn[2]=':') or ((fn[1]='\') and (fn[2]='\'))) then
              fn:=FRegFilePath+fn;
            if FProjects[i].Entry=nil then
              FProjects[i].Entry:=TProjectEntry.Create(n,fn,
                VarToBool(d1['loadCopy']))
            else
              if fn<>FProjects[i].Entry.FilePath then
                FProjects[i].Entry.SetFilePath(fn,VarToBool(d1['loadCopy']));
            FProjects[i].Entry.AllowInclude:=VarToBool(d1['allowInclude']);
            FProjects[i].Entry.Signature:=VarToStr(d1['signature']);
            FProjects[i].Entry.BufferSize:=BSize(VarToStr(d1['bufferSize']));
            FProjects[i].Entry.Negotiate:=VarToBool(d1['negotiate']);
            FProjects[i].Entry.NTLM:=VarToBool(d1['ntlm']);
           end;
          //clean-up projects removed from config
          for i:=0 to FProjectsIndex-1 do
            if not FProjects[i].LoadCheck then
             begin
              FProjects[i].Name:='';
              //FProjects[i].Alias:='';
              FreeAndNil(FProjects[i].Entry);
             end;
          {//TODO
          if FSingleProject<>'' then
            LoadFavIcon(FSingleProject+'.ico');
          }

          for i:=0 to FHostsIndex-1 do FHosts[i].LoadCheck:=false;
          e:=JSONEnum(d['hosts']);
          while e.Next do
           begin
            n:=UTF8Encode(e.Key);
            d1:=JSON(e.Value);
            i:=FindHost(PUTF8Char(n));
            if (i<>-1) and (FHosts[i].LoadCheck) then i:=-1;//duplicate! raise?
            if i=-1 then
             begin
              //new
              i:=0;
              while (i<FHostsIndex) and (FHosts[i].Host<>'') do inc(i);
              if i=FHostsIndex then
               begin
                if FHostsIndex=FHostsSize then
                 begin
                  inc(FHostsSize,8);//grow step
                  SetLength(FHosts,FHostsSize);
                 end;
                inc(FHostsIndex);
               end;
              FHosts[i].Host:=n;
             end;
            FHosts[i].LoadCheck:=true;

            FHosts[i].DefaultProject:=UTF8Encode(VarToWideStr(d1['defaultProject']));
            FHosts[i].SingleProject:=UTF8Encode(VarToWideStr(d1['singleProject']));

            v:=d1['projects'];
            if VarIsNull(v) then
              SetLength(FHosts[i].Projects,0)
            else
             begin
              //assert VarArrayLowBound(v,0)=0
              for j:=0 to VarArrayHighBound(v,0) do
                FHosts[i].Projects[j]:=UTF8Encode(VarToWideStr(v[j]));
             end;

           end;
          //clean-up hosts removed from config
          for i:=0 to FHostsIndex-1 do
            if not FHosts[i].LoadCheck then
              FHosts[i].Host:='';
         end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

function TProjectRegistry.FindProject(Name:PUTF8Char):integer;
var
  i:integer;
begin
  if (Name=nil) or (Name^=#0) then
    Result:=-1
  else
   begin
    i:=0;
    while (i<FProjectsIndex) and not(UTF8CmpI(Name,PUTF8Char(FProjects[i].Name))) do inc(i);
    if i<FProjectsIndex then
      Result:=i
    else
      Result:=-1;
   end;
end;

function TProjectRegistry.FindHost(Host:PUTF8Char):integer;
var
  i:integer;
begin
  if (Host=nil) or (Host^=#0) then
    Result:=-1
  else
   begin
    i:=0;
    while (i<FHostsIndex) and not(UTF8CmpI(Host,PUTF8Char(FHosts[i].Host))) do inc(i);
    if i<FHostsIndex then
      Result:=i
    else
      Result:=-1;
   end;
end;

function TProjectRegistry.GetProjectName(Host,Name:PUTF8Char):PUTF8Char;
var
  i,j,l:integer;
begin
  //TODO: lock?
  i:=FindHost(Host);
  if i=-1 then
   begin
    if FSingleProject<>'' then
      Result:=PUTF8Char(FSingleProject)
    else
    if Name='' then
      if FDefaultProject<>'' then
        Result:=PUTF8Char(FDefaultProject)
      else
        Result:='xxm'
    else
      Result:=Name;
   end
  else
   begin
    if FHosts[i].SingleProject<>'' then
      Result:=PUTF8Char(FHosts[i].SingleProject)
    else
    if Name='' then
      if FHosts[i].DefaultProject<>'' then
        Result:=PUTF8Char(FHosts[i].DefaultProject)
      else
        Result:='xxm'
    else
     begin
      l:=Length(FHosts[i].Projects);
      if l=0 then //no projects array: allow all by default?
        Result:=Name
      else
       begin
        j:=0;
        while (j<l) and not(UTF8CmpI(Name,PUTF8Char(FHosts[i].Projects[j]))) do inc(j);
        if j<l then
          Result:=PUTF8Char(FHosts[i].Projects[j])
        else
          raise EXxmProjectNotFound.Create('xxm Project disallowed by configuration "'+Name+'"');
       end;
     end;
   end;
end;

function TProjectRegistry.GetProjectEntry(Name:PUTF8Char):TProjectEntry;
var
  i:integer;
begin
  i:=0;
  while (i<FProjectsIndex) and not(UTF8CmpI(Name,PUTF8Char(FProjects[i].Name))) do inc(i);
  if i<FProjectsIndex then
    Result:=FProjects[i].Entry
  else
    Result:=nil;
end;

{ TProjectEntry }

constructor TProjectEntry.Create(const Name: UTF8String; const FilePath: string;
  LoadCopy: boolean);
begin
  inherited Create;
  FName:=Name;
  FFilePath:=FilePath;
  FLoadPath:='';
  FLoadSignature:='-';
  FLoadCopy:=LoadCopy;
  FContextCount:=0;
  FLibrary:=INVALID_HANDLE_VALUE;
  FProject:=nil;
  InitializeCriticalSection(FLock);
end;

destructor TProjectEntry.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TProjectEntry.SetFilePath(const FilePath: string; LoadCopy: boolean);
begin
  //assert FProject=nil//if FFilePath<>'' then Release;

  if FLoadPath<>'' then
   begin
    //TODO:
   end;

  FFilePath:=FilePath;
  FLoadPath:='';
  FLoadCopy:=LoadCopy;
  //if FLoadCopy then FLoadPath:=FFilePath+'_'+IntToHex(GetCurrentProcessId,4);
end;

procedure TProjectEntry.CheckLibrary;
var
  fn,d:string;
  h:THandle;
  xxmInitialize:FxxmInitialize;
  i,r:DWORD;
begin
  if FLibrary=INVALID_HANDLE_VALUE then
   begin
    EnterCriticalSection(FLock);
    try
      //check again for threads that were waiting for lock
      if FLibrary=INVALID_HANDLE_VALUE then
       begin
        //inc(FLoadCount);
        FLoadSignature:=GetFileSignature(FFilePath);
        if FLoadSignature='' then //if not(FileExists(FFilePath)) then
          raise EXxmModuleNotFound.Create('xxm Module not found "'+FFilePath+'"');
        if not(FLoadCopy and GlobalAllowLoadCopy) then
          fn:=FFilePath
        else
         begin
          FLoadPath:=Copy(FFilePath,1,Length(FFilePath)-4)+
            '_'+FLoadSignature+'.xxlc';
          r:=100;
          while (r<>0) do
           begin
            if CopyFile(PChar(FFilePath),PChar(FLoadPath),true) then
             begin
              SetFileAttributes(PChar(FLoadPath),
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
                  raise EXxmProjectLoadFailed.Create('xxm Project load failed: '+
                    SysErrorMessage(i))
                else
                  Sleep(20+(GetCurrentThreadId and $3F));
               end;
              //else assert files are equal
             end;
           end;
          fn:=FLoadPath;
         end;
        h:=LoadLibrary(PChar(fn));
        if (h=0) and (GetLastError=ERROR_MOD_NOT_FOUND) then
         begin
          //tried SetDllDirectory, doesn't work...
          SetLength(d,MAX_PATH);
          SetLength(d,GetCurrentDirectory(MAX_PATH,PChar(d)));
          i:=Length(fn);
          while (i<>0) and (fn[i]<>'\') do dec(i);
          SetCurrentDirectory(PChar(Copy(fn,1,i-1)));
          h:=LoadLibrary(PChar(fn));
          SetCurrentDirectory(PChar(d));
         end;
        if h=0 then
          raise EXxmProjectLoadFailed.Create('xxm Project load failed: '+
            SysErrorMessage(GetLastError));

        //required:
        @xxmInitialize:=GetProcAddress(h,'XxmInitialize');
        if @xxmInitialize=nil then
          raise EXxmProjectLoadFailed.Create('xxm Project get "XxmInitialize" failed: '+
            SysErrorMessage(GetLastError));

        @xxmPage:=GetProcAddress(h,'XxmPage');
        if @xxmPage=nil then
          raise EXxmProjectLoadFailed.Create('xxm Project get "XxmPage" failed: '+
            SysErrorMessage(GetLastError));

        //optional:
        @xxmFragment:=GetProcAddress(h,'XxmFragment');
        @xxmClearContext:=GetProcAddress(h,'XxmClearContext');
        @xxmHandleException:=GetProcAddress(h,'XxmHandleException');
        @xxmReleasingContexts:=GetProcAddress(h,'XxmReleasingContexts');
        @xxmReleasingProject:=GetProcAddress(h,'XxmReleasingProject');

        //initialize:
        try
          FProject:=xxmInitialize(XxmAPILevel,Xxm2,PUTF8Char(FName));
          FLibrary:=h;
        except
          FreeLibrary(h);
          raise;
        end;

       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TProjectEntry.OpenContext;
begin
  //TODO
  InterlockedIncrement(FContextCount);
  CheckLibrary;//todo: each property Project get?
end;

procedure TProjectEntry.CloseContext;
begin
  //...
  InterlockedDecrement(FContextCount);
end;

procedure TProjectEntry.GetFilePath(const Address:UTF8String; var Path:string;
  var MimeType:UTF8String);
var
  rf:string;
  sf,s:UTF8String;
  i,j,l:integer;
  r:TRegistry;
begin
  rf:=FFilePath;
  i:=Length(rf);
  while (i<>0) and (rf[i]<>PathDelim) do dec(i);
  SetLength(rf,i);
  sf:='';
  i:=1;
  l:=Length(Address);
  while (i<=l) do
   begin
    j:=i;
    while (j<=l) and not(Address[j] in ['/','\']) do inc(j);
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
  Path:=string(UTF8Encode(rf)+sf);
  //find a MIME-type from registry
  i:=Length(sf)-1;
  while (i>0) and (sf[i]<>'.') do dec(i);

  s:=Copy(sf,i,Length(sf)-i+1);
  //LowerCase(
  for j:=1 to Length(s) do if s[j] in ['A'..'Z'] then inc(byte(s[j]),$20);

  if (s='.xxl') or (s='.xxu') or (s='.xxmp') or (s='.xxlc')
    or (s='.exe') or (s='.dll') or (s='.udl') //or (x='.pas')?
    //more? settings?
  then
    raise EXxmFileTypeAccessDenied.Create('Access denied to this type of file');

  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    if r.OpenKeyReadOnly(string(s)) and r.ValueExists('Content Type') then
      MimeType:=UTF8Encode(r.ReadString('Content Type'))
    else if (s='.log') or (s='.ini') then MimeType:='text/plain'
    else if s='.js' then MimeType:='text/javascript'
    else if s='.css' then MimeType:='text/css'
    else if s='.xml' then MimeType:='application/xml'
    else if s='.json' then MimeType:='application/json'
    //TODO: more? from config?
    else MimeType:='application/octet-stream';
  finally
    r.Free;
  end;

end;

initialization
  Xxm2:=nil;//default, handler must initialize!
  GlobalAllowLoadCopy:=true;//default

end.
