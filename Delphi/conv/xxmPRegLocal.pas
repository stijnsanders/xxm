unit xxmPRegLocal;

{

ATTENTION:
  this is an alternative xxmPRegLocal unit
  to serve only a single xxm project
  (the real xxmPRegLocal is in the folder "common")

}

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
  protected
    function GetModulePath:WideString; override;
    function GetAllowInclude: Boolean; override;
    procedure SetSignature(const Value: String); override;
    function LoadProject: IXxmProject; override;
  public
    constructor Create(const Name:WideString);
    function GetSessionCookie(const Name: WideString): WideString; virtual;
    procedure SetSessionCookie(const Name, Value: WideString);
    function CookieFile(const Name:AnsiString):AnsiString;
  end;

  TXxmProjectCacheLocal=class(TXxmProjectCache)
  private
    FProject:TXxmProjectCacheEntry;
  public
    constructor Create;
    destructor Destroy; override;
    function GetProject(const Name:WideString):TXxmProjectCacheEntry;
  end;

var
  XxmProjectName:string;
  XxmProjectCache:TXxmProjectCacheLocal;

implementation

uses xxmp;

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(const Name: WideString);
begin
  inherited Create(Name);
  FUserName:='';
end;

function TXxmProjectCacheEntry.GetSessionCookie(const Name: WideString): WideString;
var
  i:integer;
begin
  i:=0;
  while (i<Length(FCookies)) and (FCookies[i].Name<>Name) do inc(i);//case?
  if (i<Length(FCookies)) then Result:=FCookies[i].Value else Result:='';
end;

procedure TXxmProjectCacheEntry.SetSessionCookie(const Name, Value: WideString);
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

function TXxmProjectCacheEntry.CookieFile(const Name: AnsiString): AnsiString;
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

function TXxmProjectCacheEntry.GetModulePath: WideString;
begin
  SetLength(Result,MAX_PATH);
  SetLength(Result,GetModuleFileNameW(HInstance,PWideChar(Result),MAX_PATH));
end;

function TXxmProjectCacheEntry.GetAllowInclude: Boolean;
begin
  Result:=false;
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: String);
begin
  //
end;

function TXxmProjectCacheEntry.LoadProject: IXxmProject;
begin
  //overriding! inherited;
  Result:=XxmProjectLoad(Name);
end;

{ TXxmProjectCacheLocal }

constructor TXxmProjectCacheLocal.Create;
begin
  inherited;
  FProject:=TXxmProjectCacheEntry.Create(XxmProjectName);
end;

destructor TXxmProjectCacheLocal.Destroy;
begin
  FProject.Free;
  inherited;
end;

function TXxmProjectCacheLocal.GetProject(const Name: WideString): TXxmProjectCacheEntry;
begin
  Result:=FProject;
end;

initialization
  XxmProjectName:='xxm';//default, set by dpr
  GlobalAllowLoadCopy:=false;
  XxmProjectCache:=nil;//TXxmProjectCacheLocal.Create;//see Handler.Start
finalization
  FreeAndNil(XxmProjectCache);
end.
