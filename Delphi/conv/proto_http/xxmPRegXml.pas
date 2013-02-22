unit xxmPRegXml;

interface

uses Windows, SysUtils, xxm, xxmPReg;

type
  TXxmProjectCacheEntry=class(TXxmProjectEntry)
  private
    FAllowInclude:boolean;
  protected
    procedure SetSignature(const Value: AnsiString); override;
    function GetExtensionMimeType(const x: AnsiString): AnsiString; override;
    function GetAllowInclude: boolean; override;
    function LoadProject: IXxmProject; override;
  published
    constructor Create(const Name, FilePath: WideString;
      LoadCopy, AllowInclude: boolean);
    destructor Destroy; override;
  end;

  TXxmProjectCache=class(TObject)
  private
    FProject:TXxmProjectCacheEntry;
  public
    constructor Create;
    destructor Destroy; override;
    function GetProject(const Name:WideString):TXxmProjectCacheEntry;
    function SingleProject: AnsiString;
    function DefaultProject: AnsiString;
  end;

  EXxmFileTypeAccessDenied=class(Exception);

var
  XxmProjectCache:TXxmProjectCache;
  GlobalAllowLoadCopy:boolean;

implementation

uses xxmp;

resourcestring
  SXxmFileTypeAccessDenied='Access denied to this type of file';

{ TXxmProjectCacheEntry }

constructor TXxmProjectCacheEntry.Create(const Name, FilePath: WideString;
  LoadCopy, AllowInclude: boolean);
begin
  inherited Create(Name);
  FAllowInclude:=AllowInclude;
  //
end;

destructor TXxmProjectCacheEntry.Destroy;
begin
  //
  inherited;
end;

function TXxmProjectCacheEntry.GetAllowInclude: boolean;
begin
  Result:=FAllowInclude;
end;

function TXxmProjectCacheEntry.GetExtensionMimeType(
  const x: AnsiString): AnsiString;
begin
  if (x='.xxl') or (x='.xxu') or (x='.exe') or (x='.dll') or (x='.xxmp') or (x='.udl') then //more? settings?
    raise EXxmFileTypeAccessDenied.Create(SXxmFileTypeAccessDenied);
  Result:=inherited GetExtensionMimeType(x);
end;

function TXxmProjectCacheEntry.LoadProject: IXxmProject;
begin
  //overriding! inherited;
  Result:=XxmProjectLoad(Name);
end;

procedure TXxmProjectCacheEntry.SetSignature(const Value: AnsiString);
begin
  inherited;
  raise Exception.Create('SetSignature: not implemented');
end;

{ TXxmProjectCache }

constructor TXxmProjectCache.Create;
var
  i,j:integer;
  s:string;
begin
  inherited Create;
  s:=ParamStr(0);
  j:=Length(s);
  while (j<>0) and (s[j]<>'.') do dec(j);
  i:=j;
  while (i<>0) and (s[i]<>'\') do dec(i);
  FProject:=TXxmProjectCacheEntry.Create(Copy(s,i+1,j-i-1),s,false,true);
  //TODO: GlobalAllowLoadCopy?
end;

destructor TXxmProjectCache.Destroy;
begin
  FProject.Free;
  inherited;
end;

function TXxmProjectCache.GetProject(
  const Name: WideString): TXxmProjectCacheEntry;
begin
  Result:=FProject;
end;

function TXxmProjectCache.SingleProject: AnsiString;
begin
  Result:=FProject.Name;
end;

function TXxmProjectCache.DefaultProject: AnsiString;
begin
  Result:=FProject.Name;
end;

initialization
  GlobalAllowLoadCopy:=true;//default
finalization
  XxmProjectCache.Free;

end.
