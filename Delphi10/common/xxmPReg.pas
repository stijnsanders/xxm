unit xxmPReg;

interface

uses Windows, SysUtils, Classes, xxm2, jsonDoc;

type
  TProjectEntry=class; //forward

  TProjectRegistry=class(TObject)
  private
    FLock:TRTLCriticalSection;
    FRegFilePath,FRegSignature:string;
    FRegLastCheckTC:DWORD;
    FProjects:array of record
      Name,Alias:UTF8String;
      Entry:TProjectEntry;
      SortIndex:integer;
      LoadCheck:boolean;
    end;
    FProjectsIndex,FProjectsSize:integer;
    FDefaultProject,FSingleProject:UTF8String;
    FHandlerPath,FProtoPath:string;
    FHosts:array of record
      Host,DefaultProject,SingleProject:UTF8String;
      Projects:array of UTF8String;
      SortIndex:integer;
      LoadCheck:boolean;
    end;
    FHostsIndex,FHostsSize:integer;
    procedure FindProject(Name:PUTF8Char;var i,a:integer);
    procedure FindHost(Host:PUTF8Char;var i,a:integer);
  public
    FavIcon:array of byte;

    constructor Create;
    destructor Destroy; override;
    procedure CheckRegistry;

    function GetProjectName(Host,Name:PUTF8Char):PUTF8Char;
    function GetProjectEntry(Name:PUTF8Char):TProjectEntry;
    function GetProjectData(const Name:string):IJSONDocument;

    property HandlerPath:string read FHandlerPath;
    property ProtoPath:string read FProtoPath;
  end;

  TXxmProjectCheckHandler=function(Entry: TProjectEntry;
    Context: CxxmContext; const ProjectName: UTF8String): boolean;

  TXxmContextResumeHandler=procedure(Entry: TProjectEntry;
    Context: CxxmContext; const EventKey, Fragment: UTF8String;
    const Values: array of Variant);

  TEventsController=class;//forward
  TContextHolder=class;//forward

  TProjectEntry=class(TObject)
  private
    FMutex:THandle;//previously FLock:TRTLCriticalSection;
    FContextCount,FLoadCount:integer;
    FName:UTF8String;
    FFilePath,FLoadPath,FLoadSignature:string;
    FLoadCopy:boolean;
    FLibrary:THandle;
    FProject:PxxmProject;
    FEventsController:TEventsController;
    procedure CheckLibrary;
  protected
    procedure SetFilePath(const FilePath:string;LoadCopy:boolean);
    procedure LoadConfiguration(d: IJSONDocument);
  public
    AllowInclude,Negotiate,NTLM:boolean;
    BufferSize:NativeUInt;
    Signature,ProtoPath,HandlerPath:string;
    LastCheck,ReleaseContextsTimeoutMS:DWORD;
    LastResult:UTF8String;

    xxmPage:FxxmPage;
    xxmFragment:FxxmFragment;
    xxmClearContext:FxxmClearContext;
    xxmHandleException:FxxmHandleException;
    xxmReleasingContexts:FxxmReleasingContexts;
    xxmReleasingProject:FxxmReleasingProject;

    constructor Create(const Name:UTF8String;const FilePath:string;
      LoadCopy:boolean);
    destructor Destroy; override;
    property FilePath:string read FFilePath;
    property LoadSignature:string read FLoadSignature;
    property LoadCount:integer read FLoadCount;
    property Project:PxxmProject read FProject;

    procedure Lock;
    procedure Unlock;

    procedure OpenContext;
    procedure CloseContext;
    procedure Release;

    procedure GetFilePath(const Address:UTF8String;var Path:string;
      var MimeType:UTF8String);

    procedure ClearContext(Context:CxxmContext);
    function HandleException(Context:CxxmContext;const PageClass,
      ExceptionClass,ExceptionMessage:UTF8String):boolean;
    function EventsController: TEventsController;
  end;

  TEventsController=class(TThread)
  private
    FParent:TProjectEntry;
    FLock:TRTLCriticalSection;
    FEvents:array of record
      Key:UTF8String;
      SortIndex:integer;
      Check:CxxmCheckEvent;
      Active:boolean;
      CheckedLastTC:DWORD;
      CheckIntervalMS,MaxWaitTimeSec:NativeUInt;
      ResumeFragment:UTF8String;
      ResumeValues:array of Variant;
      DropFragment:UTF8String;
      DropValues:array of Variant
    end;
    FEventsIndex,FEventsSize:integer;
    FContexts:array of TContextHolder;
    FContextsIndex,FContextsSize:integer;
    procedure FindKey(Key:PUTF8Char;var i,a:integer);
  protected
    procedure Execute; override;
  public
    constructor Create(Parent:TProjectEntry);
    destructor Destroy; override;
    procedure RegisterEvent(const EventKey:UTF8String;
      CheckHandler:CxxmCheckEvent;CheckIntervalMS,MaxWaitTimeSec:NativeUInt;
      const ResumeFragment:UTF8String;const ResumeValues:array of Variant;
      const DropFragment:UTF8String;const DropValues:array of Variant);
    procedure SuspendContext(Context:CxxmContext;const EventKey:UTF8String);
  end;

  TContextHolder=class(TObject)
  private
    FParent:TEventsController;
    FEventIndex:integer;
    FContext:CxxmContext;
    FSinceTC,FSuspendMax:DWORD;
    procedure Resume(Sender: TObject);
    procedure Drop(Sender: TObject);
  end;

  EXxmProjectNotFound=class(Exception);
  EXxmModuleNotFound=class(Exception);
  EXxmEventNotFound=class(Exception);
  EXxmProjectLoadFailed=class(Exception);
  EXxmProjectAliasDepth=class(Exception);
  EXxmFileTypeAccessDenied=class(Exception);

var
  XxmProjectRegistry:TProjectRegistry;
  XxmProjectCheckHandler:TXxmProjectCheckHandler;
  XxmContextResumeHandler:TXxmContextResumeHandler;

  GlobalAllowLoadCopy:boolean;

threadvar
  XxmIntializingProjectEntry: TProjectEntry;

implementation

uses Variants, xxmTools, Registry, xxmThreadPool;

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

function BSize(const x:string):NativeUInt;
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

type
  TLoadLibPatch=procedure(tc:DWORD;var h:THandle;fn:PChar);

procedure LoadLibPatch(tc:DWORD;var h:THandle;fn:PChar);
begin
  if (tc and 1111)=0 then SwitchToThread;
  h:=LoadLibrary(fn);
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
  FHandlerPath:='';
  FProtoPath:='';
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
  i,j,a:integer;
  d,d1:IJSONDocument;
  e:IJSONEnumerator;
  v:Variant;
begin
  if DWORD(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
   begin
    EnterCriticalSection(FLock);
    try
      //check again for threads that were waiting for lock
      if DWORD(GetTickCount-FRegLastCheckTC)>XxmRegCheckIntervalMS then
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
          FHandlerPath:=VarToStr(d['handlerPath']);
          FProtoPath:=VarToStr(d['protoPath']);

          for i:=0 to FProjectsIndex-1 do FProjects[i].LoadCheck:=false;
          e:=JSONEnum(d['projects']);
          while e.Next do
           begin
            n:=UTF8Encode(e.Key);
            d1:=JSON(e.Value);
            FindProject(PUTF8Char(n),i,a);
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
            FProjects[i].Alias:=UTF8Encode(VarToStr(d1['alias']));
            if FProjects[i].Alias='' then
             begin
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
              FProjects[i].Entry.LoadConfiguration(d1);
             end
            else
             begin
              try
                FreeAndNil(FProjects[i].Entry);
              except
                //silent
              end;
              //TODO: invalidate project cache(s)
             end;
           end;
          //clean-up projects removed from config
          for i:=0 to FProjectsIndex-1 do
            if not FProjects[i].LoadCheck then
             begin
              //FProjects[i].Name:='';//keep Name and SortIndex
              FProjects[i].Alias:='';
              try
                FreeAndNil(FProjects[i].Entry);
              except
                //silent
              end;
             end;

          for i:=0 to FHostsIndex-1 do FHosts[i].LoadCheck:=false;
          e:=JSONEnum(d['hosts']);
          while e.Next do
           begin
            n:=UTF8Encode(e.Key);
            d1:=JSON(e.Value);
            FindHost(PUTF8Char(n),i,a);
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
              //sort index
              j:=i;
              while j>a do
               begin
                FHosts[j].SortIndex:=FHosts[j-1].SortIndex;
                dec(j);
               end;
              FHosts[j].SortIndex:=i;
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
             begin
              //FHosts[i].Host:='';//Keep Name,SortIndex
              FHosts[i].DefaultProject:='';
              FHosts[i].SingleProject:='';
              SetLength(FHosts[i].Projects,0);
             end;
         end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

procedure TProjectRegistry.FindProject(Name:PUTF8Char;var i,a:integer);
var
  b,c,m:integer;
begin
  //TODO: FLock?
  i:=-1;
  if (Name=nil) or (Name^=#0) then
    a:=-1
  else
   begin
    a:=0;
    b:=FProjectsIndex-1;
    while a<=b do
     begin
      c:=(a+b) div 2;
      m:=UTF8CmpI(Name,PUTF8Char(FProjects[FProjects[c].SortIndex].Name));
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
end;

procedure TProjectRegistry.FindHost(Host:PUTF8Char;var i,a:integer);
var
  b,c,m:integer;
begin
  //TODO: FLock?
  i:=-1;
  if (Host=nil) or (Host^=#0) then
    a:=-1
  else
   begin
    a:=0;
    b:=FHostsIndex-1;
    while a<=b do
     begin
      c:=(a+b) div 2;
      m:=UTF8CmpI(Host,PUTF8Char(FHosts[FHosts[c].SortIndex].Host));
      if m<0 then
        if b=c then dec(b) else b:=c
      else
      if m>0 then
        if a=c then inc(a) else a:=c
      else
       begin
        a:=c;
        b:=a-1;//end loop
        i:=FHosts[c].SortIndex;
       end;
     end;
   end;
end;

function TProjectRegistry.GetProjectName(Host,Name:PUTF8Char):PUTF8Char;
var
  i,j,l,a:integer;
begin
  //TODO: lock?
  FindHost(Host,i,a);
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
        while (j<l) and (UTF8CmpI(Name,PUTF8Char(FHosts[i].Projects[j]))<>0) do inc(j);
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
  n:PUTF8Char;
  i,a,d:integer;
begin
  Result:=nil;//default
  n:=Name;
  d:=0;
  while n<>nil do
   begin
    FindProject(Name,i,a);
    if i=-1 then
      n:=nil//end loop
    else
      if FProjects[i].Alias<>'' then
       begin
        n:=PUTF8Char(FProjects[i].Alias);
        inc(d);
        if d=8 then raise EXxmProjectAliasDepth.Create(
          'xxm Project aliasses are limited to 8 in sequence "'+string(Name)+'"');
       end
      else
       begin
        n:=nil;//end loop
        Result:=FProjects[i].Entry
       end;
   end;
end;

function TProjectRegistry.GetProjectData(const Name: string): IJSONDocument;
var
  fn:string;
  d:IJSONDocument;
begin
  //for use by auto-update or auto-compile handlers
  fn:=FRegFilePath+XxmRegFileName;
  d:=LoadJSON(fn);
  Result:=JSON(JSON(d['projects'])[Name]);
end;

{ TProjectEntry }

constructor TProjectEntry.Create(const Name: UTF8String; const FilePath: string;
  LoadCopy: boolean);
var
  mn:WideString;
  i,l:integer;
begin
  inherited Create;
  FName:=Name;
  FFilePath:=FilePath;
  FLoadPath:='';
  FLoadSignature:='-';
  FLoadCopy:=LoadCopy;
  FContextCount:=0;
  FLoadCount:=0;
  FLibrary:=INVALID_HANDLE_VALUE;
  FProject:=nil;
  FEventsController:=nil;

  //InitializeCriticalSection(FLock);

  //prepare mutex name
  mn:='Global\xxm||'+FFilePath;
  l:=Length(mn);
  if l>248 then
   begin
    mn:=Copy(mn,1,120)+'('+IntToStr(l-240)+')'+Copy(mn,l-119,120);
    l:=Length(mn);
   end;
  for i:=1 to l do case mn[i] of '\',':','/',' ','.': mn[i]:='|'; end;

  //get mutex
  FMutex:=CreateMutexW(nil,false,PWideChar(mn));
  if FMutex=0 then RaiseLastOSError;//?

  //for use by ProjectCheckHandlers
  LastCheck:=GetTickCount-3600000;
  LastResult:='';
end;

destructor TProjectEntry.Destroy;
begin
  Release;

  //DeleteCriticalSection(FLock);
  CloseHandle(FMutex);
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

procedure TProjectEntry.Lock;
begin
  //EnterCriticalSection(FLock);
  if WaitForSingleObject(FMutex,INFINITE)<>WAIT_OBJECT_0 then
    raise Exception.Create('ProjectEntry acquire UpdateLock failed: '+
      SysErrorMessage(GetLastError));
end;

procedure TProjectEntry.Unlock;
begin
  //LeaveCriticalSection(FLock);
  if not ReleaseMutex(FMutex) then
    raise Exception.Create('ProjectEntry release UpdateLock failed: '+
      SysErrorMessage(GetLastError));
end;

procedure TProjectEntry.CheckLibrary;
var
  fn,d:string;
  h:THandle;
  xxmInitialize:FxxmInitialize;
  i,r:DWORD;
  pp:TLoadLibPatch;
begin
  if FLibrary=INVALID_HANDLE_VALUE then
   begin
    Lock;
    try
      //check again for threads that were waiting for lock
      if FLibrary=INVALID_HANDLE_VALUE then
       begin
        pp:=@LoadLibPatch;
        inc(FLoadCount);
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

        //xxmHttpAU.exe gets misidintified as Trojan:Win32/Bearfoos.A!ml
        //  and Trojan:Win32/Wacatac.B!ml, trying to work around detection
        //  with deferred call:

        //h:=LoadLibrary(PChar(fn));
        pp(GetTickCount,h,PChar(fn));
        if (h=0) and (GetLastError=ERROR_MOD_NOT_FOUND) then
         begin
          //tried SetDllDirectory, doesn't work...
          SetLength(d,MAX_PATH);
          SetLength(d,GetCurrentDirectory(MAX_PATH,PChar(d)));
          i:=Length(fn);
          while (i<>0) and (fn[i]<>'\') do dec(i);
          SetCurrentDirectory(PChar(Copy(fn,1,i-1)));
          //h:=LoadLibrary(PChar(fn));
          pp(GetTickCount,h,PChar(fn));
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
        XxmIntializingProjectEntry:=Self;
        try
          FProject:=xxmInitialize(XxmAPILevel,xxm2.xxm,PUTF8Char(FName));
          FLibrary:=h;
          XxmIntializingProjectEntry:=nil;
        except
          FreeLibrary(h);
          XxmIntializingProjectEntry:=nil;
          raise;
        end;

       end;
    finally
      Unlock;
    end;
   end;
end;

procedure TProjectEntry.LoadConfiguration(d: IJSONDocument);
var
  v:Variant;
  i:integer;
begin
  AllowInclude:=VarToBool(d['allowInclude']);
  Signature:=VarToStr(d['signature']);
  BufferSize:=BSize(VarToStr(d['bufferSize']));
  Negotiate:=VarToBool(d['negotiate']);
  NTLM:=VarToBool(d['ntlm']);
  ProtoPath:=VarToStr(d['protoPath']);
  HandlerPath:=VarToStr(d['handlerPath']);
  v:=d['releaseContextsTimeout'];
  if VarIsNull(v) then i:=30 else i:=v;
  ReleaseContextsTimeoutMS:=i*1000;
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

procedure TProjectEntry.ClearContext(Context: CxxmContext);
begin
  if @xxmClearContext<>nil then
    try
      xxmClearContext(FProject,Context);
    except
      //silent
    end;
end;

procedure TProjectEntry.Release;
var
  tc:DWORD;
begin
  //attention: deadlock danger, use OpenContext,CloseContext
  //XxmProjectCheckHandler should lock new requests

  if @xxmReleasingContexts<>nil then
    try
      xxmReleasingContexts(FProject);
    except
      //silent
    end;

  if FEventsController<>nil then
    FreeAndNil(FEventsController);

  //assert only one thread at once, use Lock/Unlock!
  tc:=GetTickCount;//TODO: if timeout then raise? log?
  while (FContextCount>0) and (DWORD(GetTickCount-tc)<=ReleaseContextsTimeoutMS) do
    SwitchToThread;
  FContextCount:=0;

  if @xxmReleasingProject<>nil then
    try
      xxmReleasingProject(FProject);
    except
      //silent
    end;

  FProject:=nil;

  if FLibrary<>INVALID_HANDLE_VALUE then
   begin
    if not FreeLibrary(FLibrary) then
      RaiseLastOSError;
    FLibrary:=INVALID_HANDLE_VALUE;
    //FContextCount:=0;

    xxmPage:=nil;
    xxmFragment:=nil;
    xxmClearContext:=nil;
    xxmHandleException:=nil;
    xxmReleasingContexts:=nil;
    xxmReleasingProject:=nil;

    if FLoadPath<>'' then
     begin
      //SetFileAttributes(PChar(FLoadPath),0);
      DeleteFile(PChar(FLoadPath));//ignore errors
      FLoadPath:='';
     end;
   end;
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

function TProjectEntry.HandleException(Context: CxxmContext; const PageClass,
  ExceptionClass, ExceptionMessage: UTF8String): boolean;
begin
  try
    if (Self=nil) or (@xxmHandleException=nil) then
      Result:=false
    else
      Result:=xxmHandleException(FProject,Context,PUTF8Char(PageClass),
        PUTF8Char(ExceptionClass),PUTF8Char(ExceptionMessage));
  except
    //raise?
    Result:=false;
  end;

end;

function TProjectEntry.EventsController: TEventsController;
begin
  if @XxmContextResumeHandler=nil then
    raise Exception.Create('xxm Handler did not provide context resume handler');

  Lock;//EnterCriticalSection(FLock);
  try
    if FEventsController=nil then
      FEventsController:=TEventsController.Create(Self);
    Result:=FEventsController;
  finally
    Unlock;//LeaveCriticalSection(FLock);
  end;
end;

{ TEventsController }

constructor TEventsController.Create(Parent:TProjectEntry);
begin
  inherited Create;
  FParent:=Parent;
  FEventsIndex:=0;
  FEventsSize:=0;
  FContextsIndex:=0;
  FContextsSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TEventsController.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TEventsController.Execute;
var
  ii,i,k,l:integer;
  x,y,z,tc:DWORD;
  c:TContextHolder;
begin
  inherited;
  x:=0;
  while not Terminated do
    try
      if x<>0 then Sleep(x);
      x:=250;//default
      if FEventsIndex<>0 then
       begin
        EnterCriticalSection(FLock);
        try
          tc:=GetTickCount;
          ii:=0;
          i:=FEventsIndex;
          z:=0;
          while ii<FEventsIndex do
           begin
            if FEvents[ii].Active then
             begin
              y:=DWORD(tc-FEvents[ii].CheckedLastTC);
              if y>FEvents[ii].CheckIntervalMS then
               begin
                if (y>z) or (i=FEventsIndex) then
                 begin
                  z:=y;
                  i:=ii;
                  if FEvents[ii].CheckIntervalMS<x then
                    x:=FEvents[ii].CheckIntervalMS;
                 end;
               end
              else
                if y<x then x:=y;
             end;
            inc(ii);
           end;
          if i<>FEventsIndex then
           begin
            try
              if FEvents[i].Check(FParent.FProject,PUTF8Char(FEvents[i].Key),
                FEvents[i].CheckIntervalMS) then
               begin
                //resume
                for k:=0 to FContextsIndex-1 do
                  if (FContexts[k].FEventIndex=i) and (FContexts[k].FContext.__Context<>nil) then
                    PageLoaderPool.Queue(FContexts[k].Resume);
                FEvents[i].Active:=false;
               end
              else
               begin
                //drop any past SuspendMax or that lost connection
                l:=0;
                for k:=0 to FContextsIndex-1 do
                 begin
                  c:=FContexts[k];
                  if (c.FEventIndex=i) and (c.FContext.__Context<>nil) then
                    if not(xxm.Context_Connected(c.FContext)) or
                      ((c.FSuspendMax<>0) and (integer(tc)-integer(c.FSuspendMax)>0)) then
                      PageLoaderPool.Queue(FContexts[k].Drop)
                    else
                      inc(l);
                 end;
                FEvents[i].Active:=l<>0;
               end;
            except
              //TODO: HandleException(?
              //drop all
              for k:=0 to FContextsIndex-1 do
                if (FContexts[k].FEventIndex=i) and (FContexts[k].FContext.__Context<>nil) then
                  PageLoaderPool.Queue(FContexts[k].Drop);
              FEvents[i].Active:=false;//?
            end;
            FEvents[i].CheckedLastTC:=GetTickCount;
           end;
        finally
          LeaveCriticalSection(FLock);
        end;
       end;
    except
      //silent (log?)
    end;

  //terminate: drop any waiting (force CloseContext)
  try
    for k:=0 to FContextsIndex-1 do
      if (FContexts[k].FContext.__Context<>nil) then
        PageLoaderPool.Queue(FContexts[k].Drop);
  except
    //silent (log?)
  end;
end;

procedure TEventsController.FindKey(Key: PUTF8Char; var i, a: integer);
var
  b,c,m:integer;
begin
  //assert entered into FLock
  i:=-1;
  if (Key=nil) or (Key^=#0) then
    a:=-1
  else
   begin
    a:=0;
    b:=FEventsIndex-1;
    while a<=b do
     begin
      c:=(a+b) div 2;
      m:=UTF8CmpI(Key,PUTF8Char(FEvents[FEvents[c].SortIndex].Key));
      if m<0 then
        if b=c then dec(b) else b:=c
      else
      if m>0 then
        if a=c then inc(a) else a:=c
      else
       begin
        a:=c;
        b:=a-1;//end loop
        i:=FEvents[c].SortIndex;
       end;
     end;
   end;
end;

procedure TEventsController.RegisterEvent(const EventKey:UTF8String;
  CheckHandler:CxxmCheckEvent;CheckIntervalMS,MaxWaitTimeSec:NativeUInt;
  const ResumeFragment:UTF8String;const ResumeValues:array of Variant;
  const DropFragment:UTF8String;const DropValues:array of Variant);
var
  i,j,a,vi,vl:integer;
  tc:DWORD;
begin
  EnterCriticalSection(FLock);
  try
    tc:=GetTickCount;
    FindKey(PUtf8Char(EventKey),i,a);
    if i=-1 then
     begin
      //new key

      i:=FEventsIndex;
      if FEventsIndex=FEventsSize then
       begin
        inc(FEventsSize,$20);//grow
        SetLength(FEvents,FEventsSize);
       end;
      inc(FEventsIndex);

      FEvents[i].Key:=EventKey;
      FEvents[i].Check:=CheckHandler;
      FEvents[i].Active:=false;//see SuspendContext
      FEvents[i].CheckedLastTC:=tc;
      FEvents[i].CheckIntervalMS:=CheckIntervalMS;
      FEvents[i].MaxWaitTimeSec:=MaxWaitTimeSec;

      FEvents[i].ResumeFragment:=ResumeFragment;
      //FEvents[i].ResumeValues:=ResumeValues;
      vl:=Length(ResumeValues);
      SetLength(FEvents[i].ResumeValues,vl);
      for vi:=0 to vl-1 do FEvents[i].ResumeValues[vi]:=ResumeValues[vi];

      FEvents[i].DropFragment:=DropFragment;
      //FEvents[i].DropValues:=DropValues;
      vl:=Length(DropValues);
      SetLength(FEvents[i].DropValues,vl);
      for vi:=0 to vl-1 do FEvents[i].DropValues[vi]:=DropValues[vi];

      //sort index
      j:=i;
      while j>a do
       begin
        FEvents[j].SortIndex:=FEvents[j-1].SortIndex;
        dec(j);
       end;
      FEvents[j].SortIndex:=i;

     end
    else
     begin
      //existing key

      if CheckIntervalMS<FEvents[i].CheckIntervalMS then
        FEvents[i].CheckIntervalMS:=CheckIntervalMS;
      if DWORD(tc-FEvents[i].CheckedLastTC)>FEvents[i].CheckIntervalMS then
        FEvents[i].CheckedLastTC:=tc;
      //TODO: check/force other values equal?
      //(update if not?)

     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TEventsController.SuspendContext(Context:CxxmContext;
  const EventKey:UTF8String);
var
  i,j,a:integer;
  tc:DWORD;
begin
  EnterCriticalSection(FLock);
  try
    tc:=GetTickCount;
    FindKey(PUTF8Char(EventKey),i,a);
    if i=-1 then
      raise EXxmEventNotFound.Create('Event "'+string(EventKey)+'" not found');

    //if not(FEvents[i].Active) then FEvents[i].CheckedLastTC:=tc;//?
    FEvents[i].Active:=true;
    FParent.OpenContext;//see TContextHolder Resume,Drop

    j:=0;
    while (j<FContextsIndex) and (FContexts[j].FContext.__Context<>nil) do inc(j);
    if j=FContextsIndex then
     begin
      if FContextsIndex=FContextsSize then
       begin
        inc(FContextsSize,$20);//grow
        SetLength(FContexts,FContextsSize);
       end;
      FContexts[j]:=TContextHolder.Create;
      FContexts[j].FParent:=Self;
      inc(FContextsIndex);
     end;
    FContexts[j].FEventIndex:=i;
    FContexts[j].FContext:=Context;
    FContexts[j].FSinceTC:=tc;
    if FEvents[i].MaxWaitTimeSec=0 then FContexts[j].FSuspendMax:=0 else
     begin
      FContexts[j].FSuspendMax:=tc+FEvents[i].MaxWaitTimeSec*1000;
      if FContexts[j].FSuspendMax=0 then inc(FContexts[j].FSuspendMax);
     end;

  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TContextHolder }

procedure TContextHolder.Resume(Sender: TObject);
begin
  try
    XxmContextResumeHandler(FParent.FParent,FContext,
      FParent.FEvents[FEventIndex].Key,
      FParent.FEvents[FEventIndex].ResumeFragment,
      FParent.FEvents[FEventIndex].ResumeValues);
  finally
    FParent.FParent.CloseContext;
    FContext.__Context:=nil;//release holding position
  end;
end;

procedure TContextHolder.Drop(Sender: TObject);
begin
  try
    XxmContextResumeHandler(FParent.FParent,FContext,
      FParent.FEvents[FEventIndex].Key,
      FParent.FEvents[FEventIndex].DropFragment,
      FParent.FEvents[FEventIndex].DropValues);
  finally
    FParent.FParent.CloseContext;
    FContext.__Context:=nil;//release holding position
  end;
end;

initialization
  XxmProjectRegistry:=nil;
  XxmProjectCheckHandler:=nil;
  XxmContextResumeHandler:=nil;
  GlobalAllowLoadCopy:=true;//default

end.
