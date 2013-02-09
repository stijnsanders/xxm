unit xxmPRegLocal;

interface

uses Windows, SysUtils, xxm, xxmPReg;

type
  TXxmProjectCacheEntry=class(TXxmProjectEntry)
  private
    FCookiePath:WideString;
    FUserName:AnsiString;
    FCookies:array of record
      Name,Value:WideString;
      //TODO: expiry, domain, path...
    end;
    procedure GetRegisteredPath;
  protected
    procedure LoadProject; override;
    function GetModulePath:WideString; override;
    procedure SetSignature(const Value: AnsiString); override;
    function GetAllowInclude: Boolean; override;
  published
    constructor Create(Name:WideString);
  public
    function GetSessionCookie(Name: WideString): WideString; virtual;
    procedure SetSessionCookie(Name: WideString; Value: WideString);
    function CookieFile(Name:AnsiString):AnsiString;
  end;

  TXxmProjectCache=class(TObject)
  private
    FLock:TRTLCriticalSection;
    ProjectCacheSize:integer;
    ProjectCache:array of TXxmProjectCacheEntry;
    function Grow:integer;
    function FindProject(Name:WideString):integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetProject(Name:WideString):TXxmProjectCacheEntry;
    procedure ReleaseProject(Name:WideString);

  end;

var
  XxmProjectCache:TXxmProjectCache;

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

procedure XxmProjectRegister(
  hwnd:HWND;        // handle to owner window
  hinst:cardinal;   // instance handle for the DLL
  lpCmdLine:LPTSTR; // string the DLL will parse
  nCmdShow:integer  // show state
); stdcall;
var
  r:TRegistry;
  s,t,u:AnsiString;
  i,j:integer;
begin
  s:=lpCmdLine;

  i:=Length(s);
  while (i<>0) and (s[i]<>'.') do dec(i);
  j:=i;
  while (j<>0) and (s[j]<>'\') do dec(j);
  inc(j);
  t:=Copy(s,j,i-j);

  if MessageBoxA(GetDesktopWindow,PAnsiChar('Register xxm project "'+t+'" for local handler?'),
    'xxm Local Handler',MB_OKCANCEL or MB_ICONQUESTION or MB_SYSTEMMODAL)=idOk then
   begin
    r:=TRegistry.Create;
    try
      //r.RootKey:=HKEY_LOCAL_MACHINE;//setting?
      r.RootKey:=HKEY_CURRENT_USER;
      r.OpenKey('\Software\xxm\local\'+t,true);
      u:=r.ReadString('');
      if (u='') or (u=s) or (MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+
        '" was already registered as'#13#10'  '+u+
        #13#10'Do you want to overwrite this registration?'#13#10'  '+s),
        'xxm Local Handler',MB_OKCANCEL or MB_ICONQUESTION or MB_SYSTEMMODAL)=idOK) then
       begin
        r.WriteString('',s);
        r.DeleteValue('Signature');
        //TODO: default settings?
        MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+'" registered.'),
          'xxm Local Handler',MB_OK or MB_ICONINFORMATION or MB_SYSTEMMODAL);
       end;
    finally
      r.Free;
    end;
   end;
end;

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(Name: WideString);
begin
  inherited Create(Name);
  FFilePath:='';
  FUserName:='';
end;

procedure TXxmProjectCacheEntry.LoadProject;
begin
  if not ProjectLoaded and ((FFilePath='') or not(FileExists(FFilePath))) then GetRegisteredPath;//refresh
  inherited;
  if not ProjectLoaded then FFilePath:='';//force refresh next time
end;

function TXxmProjectCacheEntry.GetSessionCookie(Name: WideString): WideString;
var
  i:integer;
begin
  i:=0;
  while (i<Length(FCookies)) and (FCookies[i].Name<>Name) do inc(i);//case?
  if (i<Length(FCookies)) then Result:=FCookies[i].Value else Result:='';
end;

procedure TXxmProjectCacheEntry.SetSessionCookie(Name, Value: WideString);
var
  i:integer;
begin
  i:=0;
  while (i<Length(FCookies)) and (FCookies[i].Name<>Name) do inc(i);//case?
  if (i<Length(FCookies)) then FCookies[i].Value:=Value else
   begin
    SetLength(FCookies,i+1);
    FCookies[i].Name:=Name;
    FCookies[i].Value:=Value;
   end;
end;

function TXxmProjectCacheEntry.CookieFile(Name: AnsiString): AnsiString;
var
  l:cardinal;
begin
  if FUserName='' then
   begin
    l:=1024;
    SetLength(FUserName,l);
    if GetUserNameA(PAnsiChar(FUserName),l) then
      SetLength(FUserName,l-1)
    else
      FUserName:=GetEnvironmentVariable('USERNAME');
   end;
  //TODO: filenamesafe?
  Result:=FCookiePath+FUserName+'_'+Name+'.txt';
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: AnsiString);
var
  r:TRegistry;
  k:AnsiString;
begin
  FSignature:=Value;
  k:='\Software\xxm\local\'+Name;
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CURRENT_USER;
    if not(r.OpenKey(k,false)) then
     begin
      r.RootKey:=HKEY_LOCAL_MACHINE;
      if not(r.OpenKey(k,false)) then
        raise EXxmProjectNotFound.Create(StringReplace(
          SXxmProjectNotFound,'__',Name,[]));
     end;
    r.WriteString('Signature',FSignature);
  finally
    r.Free;
  end;
end;

procedure TXxmProjectCacheEntry.GetRegisteredPath;
var
  r:TRegistry;
  k:AnsiString;
  i:integer;
begin
  k:='\Software\xxm\local\'+Name;
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
        SXxmProjectNotFound,'__',Name,[]));

    //TODO: alias? (see xxm.xml)

    //TODO: extra flags,settings?

    //TODO: from setting?
    i:=Length(FFilePath);
    while (i<>0) and (FFilePath[i]<>PathDelim) do dec(i);
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

function TXxmProjectCacheEntry.GetAllowInclude: Boolean;
var
  r:TRegistry;
  k:AnsiString;
begin
  Result:=false;//default
  k:='\Software\xxm\local\'+Name;
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CURRENT_USER;
    if not(r.OpenKeyReadOnly(k)) then
     begin
      r.RootKey:=HKEY_LOCAL_MACHINE;
      if not(r.OpenKeyReadOnly(k)) then
        raise EXxmProjectNotFound.Create(StringReplace(
          SXxmProjectNotFound,'__',Name,[]));
     end;
    if r.ValueExists('AllowInclude') then Result:=r.ReadBool('AllowInclude');
  finally
    r.Free;
  end;
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
begin
  inherited;
  ProjectCacheSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TXxmProjectCache.Destroy;
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
  l:AnsiString;
begin
  Result:=0;
  l:=Name;
  //assert cache stores ProjectName already LowerCase!
  while (Result<ProjectCacheSize) and ((ProjectCache[Result]=nil) or
    (CompareText(ProjectCache[Result].Name,l)<>0)) do inc(Result);
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

initialization
  XxmProjectCache:=nil;//TXxmProjectCache.Create;//see Handler.Start
finalization
  FreeAndNil(XxmProjectCache);
end.
