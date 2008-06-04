unit xxm;

interface

uses SysUtils, Classes;

const
  //$Date: 2008-03-11 23:44:29 +0100 (di, 11 mrt 2008) $
  XxmRevision='$Rev: 201 $';
  
type
  IXxmContext=interface;//forward
  IXxmFragment=interface; //forward

  IXxmProject=interface
    ['{78786D00-0000-0002-C000-000000000002}']
    function GetProjectName: WideString;
    property Name:WideString read GetProjectName;
    function LoadPage(Context:IXxmContext;Address:WideString):IXxmFragment;
    function LoadFragment(Address:WideString):IXxmFragment;
    procedure UnloadFragment(Fragment: IXxmFragment);
  end;

  TXxmProjectLoadProc=function(AProjectName:WideString): IXxmProject; stdcall;

  TXxmContextString=(
    csVersion,
    csExtraInfo,
    csVerb,
    csQueryString,
    csUserAgent,
    csAcceptedMimeTypes,
    csPostMimeType,
    csURL,
    csReferer,
    csLanguage,
    csRemoteAddress,
    csRemoteHost,
    csAuthUser,
    csAuthPassword
  );

  TXxmVersion=record
    Major,Minor,Release,Build:integer;
  end;

  TXxmAutoEncoding=(
    aeContentDefined, //content will specify which content to use
    aeUtf8,           //send UTF-8 byte order mark
    aeUtf16,          //send UTF-16 byte order mark
    aeIso8859         //send using the closest new thing to ASCII
  );

  IXxmParameter=interface
    ['{78786D00-0000-0007-C000-000000000007}']
    function GetName:WideString;
    function GetValue:WideString;
    property Name:WideString read GetName;
    property Value:WideString read GetValue;
    function AsInteger:integer;
    function NextBySameName:IXxmParameter;
  end;

  IXxmParameterGet=interface(IXxmParameter)
    ['{78786D00-0000-0008-C000-000000000008}']
  end;

  IxxmParameterPost=interface(IXxmParameter)
    ['{78786D00-0000-0009-C000-000000000009}']
  end;

  IxxmParameterPostFile=interface(IxxmParameterPost)
    ['{78786D00-0000-000A-C000-00000000000A}']
    function GetSize:integer;
    function GetMimeType:WideString;
    property Size:integer read GetSize;
    property MimeType:WideString read GetMimeType;
    procedure SaveToFile(FilePath:string);//TODO: WideString
    function SaveToStream(Stream:TStream):integer;//TODO: IStream
  end;

  IXxmContext=interface
    ['{78786D00-0000-0003-C000-000000000003}']

    function GetURL:WideString;
    function GetPage:IXxmFragment;
    function GetContentType:WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding:TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key:OleVariant):IXxmParameter;
    function GetParameterCount:integer;
    function GetSessionID:WideString;

    procedure Send(Data: OleVariant);
    procedure SendHTML(Data: OleVariant);
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s:TStream); //TODO: IStream
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(FileName: WideString);

    function ContextString(cs:TXxmContextString):WideString;
    function PostData:TStream; //TODO: IStream
    function Connected:boolean;

    //(local:)progress
    procedure SetStatus(Code:integer;Text:WideString);
    procedure Redirect(RedirectURL:WideString; Relative:boolean);
    function GetCookie(Name:WideString):WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
    //procedure SetCookie2();

    //TODO: pointer to project?

    property URL:WideString read GetURL;
    property ContentType:WideString read GetContentType write SetContentType;
    property AutoEncoding:TXxmAutoEncoding read GetAutoEncoding write SetAutoEncoding;
    property Page:IXxmFragment read GetPage;
    property Parameter[Key:OleVariant]:IXxmParameter read GetParameter; default;
    property ParameterCount:integer read GetParameterCount;
    property SessionID:WideString read GetSessionID;
    property Cookie[Name:WideString]:WideString read GetCookie;
  end;

  IXxmFragment=interface
    ['{78786D00-0000-0004-C000-000000000004}']
    function GetProject: IXxmProject;
    property Project:IXxmProject read GetProject;
    function ClassNameEx: WideString;
    procedure Build(const Context:IXxmContext; const Caller:IXxmFragment;
      const Values: array of OleVariant;
      const Objects: array of TObject);
  end;

  IXxmPage=interface(IXxmFragment)
    ['{78786D00-0000-0005-C000-000000000005}']
  end;

  IXxmInclude=interface(IXxmFragment)
    ['{78786D00-0000-0006-C000-000000000006}']
  end;

const
  IID_IXxmProject: TGUID = '{78786D00-0000-0002-C000-000000000002}';
  IID_IXxmContext: TGUID = '{78786D00-0000-0003-C000-000000000003}';
  IID_IXxmFragment: TGUID = '{78786D00-0000-0004-C000-000000000004}';
  IID_IXxmPage: TGUID = '{78786D00-0000-0005-C000-000000000005}';
  IID_IXxmInclude: TGUID = '{78786D00-0000-0006-C000-000000000006}';
  IID_IXxmParameter: TGUID = '{78786D00-0000-0007-C000-000000000007}';
  IID_IXxmParameterGet: TGUID = '{78786D00-0000-0008-C000-000000000008}';
  IID_IXxmParameterPost: TGUID = '{78786D00-0000-0009-C000-000000000009}';
  IID_IXxmParameterPostFile: TGUID = '{78786D00-0000-000A-C000-00000000000A}';

type
  TXxmProject=class(TInterfacedObject, IXxmProject)//abstract
  private
    FProjectName: WideString;
    function GetProjectName: WideString;
  public
    constructor Create(AProjectName: WideString);
    destructor Destroy; override;
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; virtual; abstract;
    function LoadFragment(Address: WideString): IXxmFragment; virtual; abstract;
    procedure UnloadFragment(Fragment: IXxmFragment); virtual; abstract;
    property Name:WideString read GetProjectName;
  end;

  TXxmFragment=class(TInterfacedObject, IXxmFragment)//abstract
  private
    FProject: TXxmProject;
    function GetProject: IXxmProject;
  public
    constructor Create(AProject: TXxmProject);
    destructor Destroy; override;
    function ClassNameEx: WideString; virtual;
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant;
      const Objects: array of TObject); virtual; abstract;
    property Project:IXxmProject read GetProject;
  end;

  TXxmPage=class(TXxmFragment, IXxmPage)
  end;

  TXxmInclude=class(TXxmFragment, IXxmInclude)
  end;


function XxmVersion:TXxmVersion;
function HTMLEncode(Data:WideString):WideString; overload;
function HTMLEncode(Data:OleVariant):WideString; overload;
function URLEncode(Data:OleVariant):string;
function URLDecode(Data:string):WideString;

implementation

uses Variants;

{ Helper Functions }

function HTMLEncode(Data:OleVariant):WideString;
begin
  if VarIsNull(Data) then Result:='' else Result:=HTMLEncode(VarToWideStr(Data));
end;

function HTMLEncode(Data:WideString):WideString;
begin
  if Data='' then Result:='' else
    Result:=
      UTF8Decode(
      StringReplace(
      StringReplace(
      StringReplace(
      StringReplace(
      StringReplace(
      UTF8Encode(
        Data),
        '&','&amp;',[rfReplaceAll]),
        '<','&lt;',[rfReplaceAll]),
        '>','&gt;',[rfReplaceAll]),
        '"','&quot;',[rfReplaceAll]),
        #13#10,'<br />'#13#10,[rfReplaceAll])
      );
end;

const
  Hex: array[0..15] of char='0123456789ABCDEF';

function URLEncode(Data:OleVariant):string;
var
  s,t:string;
  p,q,l:integer;
begin
  if VarIsNull(Data) then Result:='' else
   begin
    s:=UTF8Encode(VarToWideStr(Data));
    q:=1;
    l:=Length(s)+$80;
    SetLength(t,l);
    for p:=1 to Length(s) do
     begin
      if q+4>l then
       begin
        inc(l,$80);
        SetLength(t,l);
       end;
      case s[p] of
        #0..#31,'"','#','$','%','&','''','+','/','<','>','?','@','[','\',']','^','`','{','|','}','´':
         begin
          t[q]:='%';
          t[q+1]:=Hex[byte(s[p]) shr 4];
          t[q+2]:=Hex[byte(s[p]) and $F];
          inc(q,2);
         end;
        ' ':
          t[q]:='+';
        else
          t[q]:=s[p];
      end;
      inc(q);
     end;
    SetLength(t,q-1);
    Result:=t;
   end;
end;

function URLDecode(Data:string):WideString;
var
  t:string;
  p,q,l:integer;
  b:byte;
begin
  l:=Length(Data);
  SetLength(t,l);
  q:=1;
  p:=1;
  while (p<=l) do
   begin
    case Data[p] of
      '+':t[q]:=' ';
      '%':
       begin
        inc(p);
        b:=0;
        case Data[p] of
          '0'..'9':inc(b,byte(Data[p]) and $F);
          'A'..'F','a'..'f':inc(b,(byte(Data[p]) and $F)+9);
        end;
        inc(p);
        b:=b shl 4;
        case Data[p] of
          '0'..'9':inc(b,byte(Data[p]) and $F);
          'A'..'F','a'..'f':inc(b,(byte(Data[p]) and $F)+9);
        end;
        t[q]:=char(b);
       end
      else
        t[q]:=Data[p];
    end;
    inc(p);
    inc(q);
   end;
  SetLength(t,q-1);
  Result:=UTF8Decode(t);
  if not(q=0) and (Result='') then Result:=t;
end;

function XxmVersion: TXxmVersion;
var
  s:string;
begin
  s:=XxmRevision;
  Result.Major:=1;
  Result.Minor:=0;
  Result.Release:=0;
  Result.Build:=StrToInt(Copy(s,7,Length(s)-8));
end;

{ TXxpProject }

constructor TXxmProject.Create(AProjectName: WideString);
begin
  inherited Create;
  FProjectName:=AProjectName;
end;

destructor TXxmProject.Destroy;
begin
  inherited;
end;

function TXxmProject.GetProjectName: WideString;
begin
  Result:=FProjectName;
end;

{ TXxmFragment }

constructor TXxmFragment.Create(AProject: TXxmProject);
begin
  inherited Create;
  FProject:=AProject;
end;

function TXxmFragment.GetProject: IXxmProject;
begin
  Result:=FProject;
end;

function TXxmFragment.ClassNameEx: WideString;
begin
  Result:=ClassName;
end;

destructor TXxmFragment.Destroy;
begin
  inherited;
end;

initialization
  IsMultiThread:=true;
end.
