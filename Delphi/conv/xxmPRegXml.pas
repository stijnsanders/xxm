unit xxmPRegXml;

{

ATTENTION: this is an alternative xxmPRegXml unit to serve only a single xxm project.

}

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

  TXxmProjectCacheXml=class(TXxmProjectCache)
  private
    FProject:TXxmProjectCacheEntry;
  public
    constructor Create;
    destructor Destroy; override;
    function ProjectFromURI(Context:IXxmContext;const URI:AnsiString;
      var i:integer; var ProjectName,FragmentName:WideString):boolean;
    function GetProject(const Name:WideString):TXxmProjectCacheEntry;
  end;

  EXxmFileTypeAccessDenied=class(Exception);

var
  XxmProjectCache:TXxmProjectCacheXml;
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

{ TXxmProjectCacheXml }

constructor TXxmProjectCacheXml.Create;
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

destructor TXxmProjectCacheXml.Destroy;
begin
  FProject.Free;
  inherited;
end;

function TXxmProjectCacheXml.GetProject(
  const Name: WideString): TXxmProjectCacheEntry;
begin
  Result:=FProject;
end;

function TXxmProjectCacheXml.ProjectFromURI(Context: IXxmContext;
  const URI: AnsiString; var i: integer; var ProjectName,
  FragmentName: WideString): boolean;
var
  j,l:integer;
begin
  l:=Length(URI);
  Result:=false;
  j:=i;
  while (i<=l) and not(char(URI[i]) in ['?','&','$','#']) do inc(i);
  FragmentName:=Copy(URI,j,i-j);
  if (i<=l) then inc(i);
end;

initialization
  GlobalAllowLoadCopy:=true;//default
finalization
  XxmProjectCache.Free;

end.
