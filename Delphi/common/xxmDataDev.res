        ��  ��                    (   �� B F A I L       0         <html><head><title>Build failed: $P</title></head>
<body style="font-family:sans-serif;background-color:white;color:black;margin:0em;">
<h1 style="background-color:#0000CC;color:white;margin:0em;padding:0.2em;">Build failed: $P</h1>
<xmp style="margin:0.1em;">$L</xmp>
<p style="background-color:#0000CC;color:white;font-size:0.8em;margin:0em;padding:0.2em;text-align:right;">
<a href="$U" style="float:left;color:white;">refresh</a>
<a href="http://yoy.be/xxm/" style="color:white;">xxm</a> $N</p></body></html>    ,   �� B E R R O R         0         <html><head><title>Error building: $P</title></head>
<body style="font-family:sans-serif;background-color:white;color:black;margin:0em;">
<h1 style="background-color:#0000CC;color:white;margin:0em;padding:0.2em;">Error building: $P</h1>
<p style="margin:0.1em;">An error occurred while building the module.<br />
<i>$1</i><br /><b>$2</b></p>
<p style="background-color:#0000CC;color:white;font-size:0.8em;margin:0em;padding:0.2em;text-align:right;">
<a href="http://yoy.be/xxm/" style="color:white;">xxm</a> $N</p></body></html>  J<  $   ���X X M       0         unit xxm;

interface

uses SysUtils, Classes, ActiveX;

const
  //$Date: 2023-06-02 17:46:58 +0200 (vr, 02 jun 2023) $
  XxmRevision = '$Rev: 468 $';

type
  IXxmContext = interface;//forward
  IXxmFragment = interface;//forward

  IXxmProject = interface
    ['{78786D00-0000-0002-C000-000000000002}']
    function GetProjectName: WideString;
    property Name: WideString read GetProjectName;
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
    function LoadFragment(Context: IXxmContext;
      const Address, RelativeTo: WideString):IXxmFragment;
    procedure UnloadFragment(Fragment: IXxmFragment);
  end;

  TXxmProjectLoadProc = function(const AProjectName: WideString): IXxmProject; stdcall;

  TXxmContextString = integer;//enumeration values see below

  TXxmVersion = record
    Major, Minor, Release, Build: integer;
  end;

  TXxmAutoEncoding = (
    aeContentDefined, //content will specify which content to use
    aeUtf8,           //send UTF-8 byte order mark
    aeUtf16,          //send UTF-16 byte order mark
    aeIso8859         //send using the closest new thing to ASCII
  );

  IXxmParameter = interface
    ['{78786D00-0000-0007-C000-000000000007}']
    function GetName: WideString;
    function GetValue: WideString;
    property Name: WideString read GetName;
    property Value: WideString read GetValue;
    function AsInteger: integer;
    function NextBySameName: IXxmParameter;
  end;

  IXxmParameterGet = interface(IXxmParameter)
    ['{78786D00-0000-0008-C000-000000000008}']
  end;

  IxxmParameterPost = interface(IXxmParameter)
    ['{78786D00-0000-0009-C000-000000000009}']
  end;

  IxxmParameterPostFile = interface(IxxmParameterPost)
    ['{78786D00-0000-000A-C000-00000000000A}']
    function GetSize: integer;
    function GetMimeType: WideString;
    property Size: integer read GetSize;
    property MimeType: WideString read GetMimeType;
    procedure SaveToFile(const FilePath: AnsiString);//TODO: WideString
    function SaveToStream(Stream: IStream): integer;
  end;

  IXxmContext = interface
    ['{78786D00-0000-0003-C000-000000000003}']
    function GetURL: WideString;
    function GetPage: IXxmFragment;
    function GetContentType: WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: integer;
    function GetSessionID: WideString;

    procedure Send(Data: OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendFile(const FilePath: WideString);
    procedure SendStream(s:IStream);
    procedure Include(const Address: WideString); overload;
    procedure Include(const Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(const Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(const FileName: WideString);

    function ContextString(cs: TXxmContextString): WideString;
    function PostData: IStream;
    function Connected: boolean;

    //(local:)progress
    procedure SetStatus(Code: integer; const Text: WideString);
    procedure Redirect(const RedirectURL: WideString; Relative: boolean);
    function GetCookie(const Name: WideString): WideString;
    procedure SetCookie(const Name, Value: WideString); overload;
    procedure SetCookie(const Name, Value: WideString; KeepSeconds: cardinal;
      const Comment, Domain, Path: WideString; Secure, HttpOnly: boolean); overload;
    //procedure SetCookie2();

    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;

    function GetBufferSize: integer;
    procedure SetBufferSize(ABufferSize: integer);
    procedure Flush;

    property URL:WideString read GetURL;
    property ContentType:WideString read GetContentType write SetContentType;
    property AutoEncoding:TXxmAutoEncoding read GetAutoEncoding
      write SetAutoEncoding;
    property Page:IXxmFragment read GetPage;
    property Parameter[Key: OleVariant]: IXxmParameter
      read GetParameter; default;
    property ParameterCount:integer read GetParameterCount;
    property SessionID:WideString read GetSessionID;
    property Cookie[const Name: WideString]: WideString read GetCookie;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
  end;

  IXxmFragment = interface
    ['{78786D00-0000-0004-C000-000000000004}']
    function GetProject: IXxmProject;
    function ClassNameEx: WideString;
    procedure Build(const Context:IXxmContext; const Caller:IXxmFragment;
      const Values: array of OleVariant;
      const Objects: array of TObject);
    function GetRelativePath: WideString;
    property Project:IXxmProject read GetProject;
    property RelativePath: WideString read GetRelativePath;
  end;

  IXxmPage = interface(IXxmFragment)
    ['{78786D00-0000-0005-C000-000000000005}']
  end;

  IXxmInclude = interface(IXxmFragment)
    ['{78786D00-0000-0006-C000-000000000006}']
  end;

  IXxmProjectEvents = interface
    ['{78786D00-0000-0013-C000-000000000013}']
    function HandleException(Context: IXxmContext; const PageClass: WideString;
      Ex: Exception): boolean;
  end;

  IXxmProjectEvents1 = interface
    ['{78786D00-0000-0014-C000-000000000014}']
    function HandleException(Context: IXxmContext; const PageClass,
      ExceptionClass, ExceptionMessage: WideString): boolean;
    procedure ReleasingContexts;
    procedure ReleasingProject;
  end;

  IXxmContextSuspend = interface
    ['{78786D00-0000-0015-C000-000000000015}']
    procedure Suspend(const EventKey: WideString;
      CheckIntervalMS, MaxWaitTimeSec: cardinal;
      const ResumeFragment: WideString; ResumeValue: OleVariant;
      const DropFragment: WideString; DropValue: OleVariant);
  end;

  IXxmProjectEvents2 = interface
    ['{78786D00-0000-0016-C000-000000000016}']
    function CheckEvent(const EventKey: WideString;
      var CheckIntervalMS: cardinal): boolean;
  end;

  IXxmRawSocket = interface(ISequentialStream)
    ['{78786D00-0000-0017-C000-000000000017}']
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure Disconnect;
  end;

  IXxmSocketSuspend = interface
    ['{78786D00-0000-0018-C000-000000000018}']
    procedure SuspendSocket(Handler: IXxmRawSocket);
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
  IID_IXxmProjectEvents: TGUID = '{78786D00-0000-0013-C000-000000000013}';
  IID_IXxmProjectEvents1: TGUID = '{78786D00-0000-0014-C000-000000000014}';
  IID_IXxmContextSuspend: TGUID = '{78786D00-0000-0015-C000-000000000015}';
  IID_IXxmProjectEvents2: TGUID = '{78786D00-0000-0016-C000-000000000016}';
  IID_IXxmRawSocket: TGUID = '{78786D00-0000-0017-C000-000000000017}';

const
  //TXxmContextString enumeration values
  csVersion           = -1000;
  csProjectName       = -1001;
  csURL               = -1002;
  csLocalURL          = -1003;
  csVerb              = -1004;
  csExtraInfo         = -1005;
  csUserAgent         = -1006;
  csQueryString       = -1007;
  csPostMimeType      = -1008;
  csReferer           = -1009;
  csLanguage          = -1010;
  csAcceptedMimeTypes = -1011;
  csRemoteAddress     = -1012;
  csRemoteHost        = -1013;
  csAuthUser          = -1014;
  csAuthPassword      = -1015;
  //
  cs_Max              = -1100;//used by GetParameter
  
type
  TXxmProject = class(TInterfacedObject, IXxmProject)//abstract
  private
    FProjectName: WideString;
    function GetProjectName: WideString;
  public
    constructor Create(const AProjectName: WideString);
    destructor Destroy; override;
    function LoadPage(Context: IXxmContext;
      const Address: WideString): IXxmFragment; virtual; abstract;
    function LoadFragment(Context: IXxmContext;
      const Address, RelativeTo: WideString): IXxmFragment; virtual; abstract;
    procedure UnloadFragment(Fragment: IXxmFragment); virtual; abstract;
    property Name:WideString read GetProjectName;
  end;

  TXxmFragment = class(TInterfacedObject, IXxmFragment)//abstract
  private
    FProject: TXxmProject;
    FRelativePath: WideString;
    function GetProject: IXxmProject;
    procedure SetRelativePath(const Value: WideString);
  public
    constructor Create(AProject: TXxmProject);
    destructor Destroy; override;
    function ClassNameEx: WideString; virtual;
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant;
      const Objects: array of TObject); virtual; abstract;
    function GetRelativePath: WideString;
    property Project:IXxmProject read GetProject;
    property RelativePath: WideString read GetRelativePath
      write SetRelativePath;
  end;

  TXxmPage = class(TXxmFragment, IXxmPage)
  end;

  TXxmInclude = class(TXxmFragment, IXxmInclude)
  end;

function XxmVersion: TXxmVersion;
function HTMLEncode(const Data: WideString): WideString; overload;
function HTMLEncode(const Data: OleVariant): WideString; overload;
function URLEncode(const Data: OleVariant): AnsiString; overload;
function URLDecode(const Data: AnsiString): WideString;
function URLEncode(const KeyValuePairs: array of OleVariant): AnsiString;
  overload;

implementation

uses Variants;

{ Delphi cross-version declarations }

{$IF not Declared(UTF8ToWideString)}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$IFEND}

{ Helper Functions }

function HTMLEncode(const Data:OleVariant):WideString;
begin
  Result:=HTMLEncode(VarToWideStr(Data));
end;

function HTMLEncode(const Data:WideString):WideString;
const
  GrowStep=$1000;
var
  i,di,ri,dl,rl:integer;
  x:WideString;
begin
  Result:=Data;
  di:=1;
  dl:=Length(Data);
  while (di<=dl) and not(AnsiChar(Data[di]) in ['&','<','"','>',#13,#10]) do
    inc(di);
  if di<=dl then
   begin
    ri:=di;
    rl:=((dl div GrowStep)+1)*GrowStep;
    SetLength(Result,rl);
    while (di<=dl) do
     begin
      case Data[di] of
        '&':x:='&amp;';
        '<':x:='&lt;';
        '>':x:='&gt;';
        '"':x:='&quot;';
        #13,#10:
         begin
          if (di<dl) and (Data[di]=#13) and (Data[di+1]=#10) then inc(di);
          x:='<br />'#13#10;
         end;
        else x:=Data[di];
      end;
      if ri+Length(x)>rl then
       begin
        inc(rl,GrowStep);
        SetLength(Result,rl);
       end;
      for i:=1 to Length(x) do
       begin
        Result[ri]:=x[i];
        inc(ri);
       end;
      inc(di);
     end;
    SetLength(Result,ri-1);
   end;
end;

const
  Hex: array[0..15] of AnsiChar='0123456789ABCDEF';

function URLEncode(const Data:OleVariant):AnsiString;
var
  s,t:AnsiString;
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
        #0..#31,'"','#','$','%','&','''','+','/',
        '<','>','?','@','[','\',']','^','`','{','|','}',
        #$80..#$FF:
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

function URLDecode(const Data:AnsiString):WideString;
var
  t:AnsiString;
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
        t[q]:=AnsiChar(b);
       end
      else
        t[q]:=Data[p];
    end;
    inc(p);
    inc(q);
   end;
  SetLength(t,q-1);
  Result:=UTF8ToWideString(t);
  //plain decode in case of encoding error?
  if (q>1) and (Result='') then Result:=WideString(t);
end;

function URLEncode(const KeyValuePairs:array of OleVariant):AnsiString;
  overload;
var
  i,l:integer;
begin
  Result:='';
  l:=Length(KeyValuePairs);
  if l<>0 then
   begin
    i:=0;
    while i<l do
     begin
      Result:=Result+'&'+URLEncode(KeyValuePairs[i])+'=';
      inc(i);
      if i<l then
        if VarIsNumeric(KeyValuePairs[i]) then
          Result:=Result+AnsiString(VarToStr(KeyValuePairs[i]))
        else
          Result:=Result+URLEncode(KeyValuePairs[i]);
      inc(i);
     end;
    if i<l then
      Result:=Result+'&'+URLEncode(KeyValuePairs[i])+'=';
    Result[1]:='?';
   end;
end;

function XxmVersion: TXxmVersion;
begin
  Result.Major:=1;
  Result.Minor:=2;
  Result.Release:=6;
  Result.Build:=StrToInt(Copy(XxmRevision,7,Length(XxmRevision)-8));
end;

{ TXxpProject }

constructor TXxmProject.Create(const AProjectName: WideString);
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
  FRelativePath:='';//set by xxmFReg, used for relative includes
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

function TXxmFragment.GetRelativePath: WideString;
begin
  Result:=FRelativePath;
end;

procedure TXxmFragment.SetRelativePath(const Value: WideString);
begin
  FRelativePath:=Value;
end;

initialization
  IsMultiThread:=true;
end.
  N  ,   ���X X M F R E G       0         unit xxmFReg;

{

  xxm Fragment Registry

This is a default fragment registry. You are free to change this one
or create a new one for your project.
The TxxmProject (xxmp.pas) calls GetClass with the page section
of the URL, or can pre-process the URL.

  $Rev: 456 $ $Date: 2018-05-04 23:45:27 +0200 (vr, 04 mei 2018) $

}

interface

uses xxm, Classes;

type
  TXxmFragmentClass=class of TXxmFragment;

  TXxmFragmentRegistry=class(TObject)
  private
    Registry:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(const FName: string; FType: TXxmFragmentClass);
    function GetFragment(Project: TxxmProject;
	  const FName, RelativeTo: WideString): IxxmFragment;
  end;

var
  XxmFragmentRegistry:TXxmFragmentRegistry;

const
  XxmDefaultPage='default.xxm';

implementation

uses SysUtils;

{ TXxmFragmentRegistry }

constructor TXxmFragmentRegistry.Create;
begin
  inherited Create;
  Registry:=TStringList.Create;
  Registry.Sorted:=true;
  Registry.Duplicates:=dupIgnore;//dupError?setting?
  Registry.CaseSensitive:=false;//setting?
end;

destructor TXxmFragmentRegistry.Destroy;
begin
  //Registry.Clear;//?
  Registry.Free;
  inherited;
end;

procedure TXxmFragmentRegistry.RegisterClass(const FName: string;
  FType: TXxmFragmentClass);
begin
  Registry.AddObject(FName,TObject(FType));
end;

function TXxmFragmentRegistry.GetFragment(Project: TxxmProject;
  const FName, RelativeTo: WideString): IxxmFragment;
var
  i,j,l:integer;
  a,b:WideString;
  f:TxxmFragment;
begin
  l:=Length(FName);
  if (l<>0) and (FName[1]='/') then
   begin
    //absolute path
    a:=Copy(FName,2,l-1);
   end
  else
   begin
    //expand relative path
    i:=Length(RelativeTo);
    while (i>0) and (RelativeTo[i]<>'/') do dec(i);
    a:=Copy(RelativeTo,1,i);
    i:=1;
    while i<l do
     begin
      j:=i;
      while (i<l) and (FName[i]<>'/') do inc(i);
      inc(i);
      b:=Copy(FName,j,i-j);
      if b='../' then
       begin
        j:=Length(a)-1;
        while (j<>0) and (a[j]<>'/') do dec(j);
        SetLength(a,j);
       end
      else
        if b<>'./' then
          a:=a+b;
     end;
   end;
  //get fragment class
  i:=Registry.IndexOf(a);            
  //folder? add index page name
  if i=-1 then
    if (FName='') or (FName[Length(FName)]='/') then 
	    i:=Registry.IndexOf(FName+XxmDefaultPage)
	  else
	    i:=Registry.IndexOf(FName+'/'+XxmDefaultPage);
  if i=-1 then
    Result:=nil
  else
   begin
    f:=TXxmFragmentClass(Registry.Objects[i]).Create(Project);
    //TODO: cache created instance, incease ref count
    f.RelativePath:=a;
    Result:=f;
   end;
end;

initialization
  XxmFragmentRegistry:=TXxmFragmentRegistry.Create;
finalization
  XxmFragmentRegistry.Free;

end.
  W  4   ���X X M S E S S I O N         0         unit xxmSession;

{

Use a copy of this unit in your xxm project to enable session data.

Extend the TXxmSession class definition with extra data to store with the session.

Add this unit to the uses clause of the project source file (xxmp.pas) and add this line to the LoadPage function of the project object:

function TXxmSomeProject.LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
begin
  inherited;
>>>  SetSession(Context);  <<<
  Result:=LoadFragment(Address);
end;

}

interface

uses xxm, Classes;

type
  TXxmSession=class(TObject)
  private
    FID:WideString;
  public

    //TODO: full properties?
    Authenticated:boolean;
    Name:AnsiString;

    constructor Create(const ID: WideString; Context: IXxmContext);

    //CSRF protection by posting session cookie value
    function FormProtect:WideString;
    procedure CheckProtect(Context: IXxmContext);

    property ID:WideString read FID;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses SysUtils;

var
  SessionStore:TStringList;

procedure SetSession(Context: IXxmContext);
var
  i:integer;
  sid:WideString;
begin
  if SessionStore=nil then
   begin
    SessionStore:=TStringList.Create;
    SessionStore.Sorted:=true;
    SessionStore.CaseSensitive:=true;
    //SessionStore.Duplicates:=dupError;
   end;
  sid:=Context.SessionID+
    '|'+Context.ContextString(csUserAgent);//TODO: hash
  //TODO: more ways to prevent session hijacking?
  i:=SessionStore.IndexOf(sid);
  //TODO: session expiry!!!
  if (i<>-1) then Session:=SessionStore.Objects[i] as TXxmSession else
   begin
    //as a security measure, disallow  new sessions on a first POST request
    if Context.ContextString(csVerb)='POST' then
      raise Exception.Create('Access denied.');
    Session:=TXxmSession.Create(sid,Context);
    SessionStore.AddObject(sid,Session);
   end;
end;

//call AbandonSession to release session data (e.g. logoff)
procedure AbandonSession;
begin
  SessionStore.Delete(SessionStore.IndexOf(Session.ID));
  FreeAndNil(Session);
end;

{ TxxmSession }

constructor TXxmSession.Create(const ID: WideString; Context: IXxmContext);
begin
  inherited Create;
  FID:=ID;
  //TODO: initiate expiry

  //default values
  Authenticated:=false;
  Name:='';

end;

function TXxmSession.FormProtect:WideString;
begin
  Result:='<input type="hidden" name="XxmSessionID" value="'+HTMLEncode(FID)+'" />';
end;

procedure TXxmSession.CheckProtect(Context: IXxmContext);
var
  p:IXxmParameter;
  pp:IXxmParameterPost;
begin
  if Context.ContextString(csVerb)='POST' then
   begin
    p:=Context.Parameter['XxmSessionID'];
    if not((p.QueryInterface(IxxmParameterPost,pp)=S_OK) and (p.Value=FID)) then
      raise Exception.Create('Invalid POST source detected.');
   end
  else
    raise Exception.Create('xxmSession.CheckProtect only works on POST requests.');
end;

initialization
  SessionStore:=nil;//see SetSession
finalization
  FreeAndNil(SessionStore);

end.
 �%  0   ���X X M S T R I N G       0         unit xxmString;

interface

uses
  SysUtils, Classes, ActiveX, xxm;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TStringContext=class(TInterfacedObject, IXxmContext)
  private
    FContext:IXxmContext;
    FBuilding:IXxmFragment;
    FIncludeDepth,FIndex:integer;
    FResult:WideString;
    FAutoEncoding:TXxmAutoEncoding;
    function GetResult:WideString;
    procedure WriteString(const Value:WideString);
  protected
    function Connected: Boolean;
    function ContextString(cs: TXxmContextString): WideString;
    procedure DispositionAttach(const FileName: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    function GetContentType: WideString;
    function GetCookie(const Name: WideString): WideString;
    function GetPage: IXxmFragment;
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
    function GetSessionID: WideString;
    function GetURL: WideString;
    function PostData: IStream;
    procedure Redirect(const RedirectURL: WideString; Relative: Boolean);
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    procedure SetContentType(const Value: WideString);
    procedure SetCookie(const Name,Value:WideString); overload;
    procedure SetCookie(const Name,Value:WideString; KeepSeconds:cardinal;
      const Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
    procedure SetStatus(Code: Integer; const Text: WideString);
    function GetBufferSize: integer;
    procedure SetBufferSize(ABufferSize: integer);
    procedure Flush;
  public
    constructor Create(AContext: IXxmContext; ACaller: IXxmFragment);
    destructor Destroy; override;
    procedure Send(Data: OleVariant); overload;
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendFile(const FilePath: WideString);
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendStream(s: IStream);
    procedure Include(const Address: WideString); overload;
    procedure Include(const Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(const Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
  	procedure Reset;

    property AutoEncoding:TXxmAutoEncoding read GetAutoEncoding write SetAutoEncoding;
    property Result:WideString read GetResult;
    procedure SaveToFile(const FileName: string);
  end;

  EXxmUnsupported=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmIncludeStackFull=class(Exception);

implementation

uses
  Variants;

resourcestring
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

{ TStringContext }

constructor TStringContext.Create(AContext: IXxmContext; ACaller: IXxmFragment);
begin
  inherited Create;
  FContext:=AContext;
  FBuilding:=ACaller;
  FIncludeDepth:=0;
  FAutoEncoding:=AContext.AutoEncoding;
  Reset;
end;

destructor TStringContext.Destroy;
begin
  FContext:=nil;
  FBuilding:=nil;
  inherited;
end;

function TStringContext.GetResult: WideString;
begin
  Result:=Copy(FResult,1,FIndex);
end;

procedure TStringContext.WriteString(const Value: WideString);
const
  GrowStep=$10000;
var
  l,x:integer;
begin
  l:=Length(FResult);
  x:=Length(Value);
  if FIndex+x>l then SetLength(FResult,l+((x div GrowStep)+1)*GrowStep);
  Move(Value[1],FResult[FIndex+1],x*2);
  inc(FIndex,x);
end;

procedure TStringContext.Reset;
begin
  FIndex:=0;
  FResult:='';
end;

procedure TStringContext.SaveToFile(const FileName: string);
const
  Utf8ByteOrderMark:AnsiString=#$EF#$BB#$BF;
  Utf16ByteOrderMark:AnsiString=#$FF#$FE;
var
  f:TFileStream;
  s:AnsiString;
begin
  f:=TFileStream.Create(FileName,fmCreate);
  try
    case FAutoEncoding of
      aeContentDefined:
        raise EXxmUnsupported.Create('StringContext.SaveToFile doesn''t support AutoEncoding=aeContentDefined');
      aeUtf8:
       begin
        f.Write(Utf8ByteOrderMark[1],3);
        s:=UTF8Encode(Copy(FResult,1,FIndex));
        f.Write(s[1],Length(s));
       end;
      aeUtf16:
       begin
        f.Write(Utf16ByteOrderMark[1],2);
        f.Write(FResult[1],FIndex*2);
       end;
      aeIso8859:
       begin
        s:=AnsiString(Copy(FResult,1,FIndex));
        f.Write(s[1],Length(s));
       end;
    end;
  finally
    f.Free;
  end;
end;

function TStringContext.Connected: Boolean;
begin
  Result:=FContext.Connected;
end;

function TStringContext.ContextString(cs: TXxmContextString): WideString;
begin
  Result:=FContext.ContextString(cs);
end;

function TStringContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FContext.AutoEncoding;
end;

function TStringContext.GetContentType: WideString;
begin
  Result:=FContext.ContentType;
end;

function TStringContext.GetCookie(const Name: WideString): WideString;
begin
  Result:=FContext.Cookie[Name];
end;

function TStringContext.GetPage: IXxmFragment;
begin
  Result:=FContext.Page;
end;

function TStringContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  Result:=FContext.Parameter[Key];
end;

function TStringContext.GetParameterCount: Integer;
begin
  Result:=FContext.ParameterCount;
end;

function TStringContext.GetSessionID: WideString;
begin
  Result:=FContext.SessionID;
end;

function TStringContext.GetURL: WideString;
begin
  Result:=FContext.URL;
end;

function TStringContext.PostData: IStream;
begin
  Result:=FContext.PostData;
end;

procedure TStringContext.DispositionAttach(const FileName: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support DispositionAttach');
end;

procedure TStringContext.Redirect(const RedirectURL: WideString;
  Relative: Boolean);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support Redirect');
end;

procedure TStringContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  FAutoEncoding:=Value;
end;

procedure TStringContext.SetContentType(const Value: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support ContentType');
end;

procedure TStringContext.SetStatus(Code: Integer; const Text: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support Status');
end;

procedure TStringContext.Include(const Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TStringContext.Include(const Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TStringContext.Include(const Address: WideString;
  const Values: array of OleVariant;
  const Objects: array of TObject);
var
  p:IXxmProject;
  f,fb:IXxmFragment;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  p:=FBuilding.Project;//p:=FContext.Page.Project;?
  try
    //TODO: relative path to FContext.ContextString(clLocalURL)
    f:=p.LoadFragment(FContext,Address,FBuilding.RelativePath);
    if f=nil then
      raise EXxmIncludeFragmentNotFound.Create(StringReplace(
        SXxmIncludeFragmentNotFound,'__',Address,[]));
    fb:=FBuilding;
    FBuilding:=f;
    inc(FIncludeDepth);
    try
      //TODO: catch exceptions?
      f.Build(Self,fb,Values,Objects);
    finally
      dec(FIncludeDepth);
      FBuilding:=fb;
      fb:=nil;
      p.UnloadFragment(f);
      f:=nil;
    end;
  finally
    p:=nil;
  end;
end;

procedure TStringContext.Send(Data: OleVariant);
begin
  WriteString(HTMLEncode(Data));
end;

procedure TStringContext.SendHTML(Data: OleVariant);
begin
  WriteString(VarToStr(Data));
end;

procedure TStringContext.SendFile(const FilePath: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support SendFile');
end;

procedure TStringContext.SendStream(s: IStream);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support SendStream');
end;

procedure TStringContext.SetCookie(const Name, Value: WideString);
begin
  FContext.SetCookie(Name, Value);
end;

procedure TStringContext.SetCookie(const Name, Value: WideString;
  KeepSeconds: cardinal; const Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
begin
  FContext.SetCookie(Name, Value, KeepSeconds, Comment, Domain, Path,
    Secure, HttpOnly);
end;

procedure TStringContext.Send(Value: int64);
begin
  WriteString(IntToStr(Value));
end;

procedure TStringContext.Send(Value: integer);
begin
  WriteString(IntToStr(Value));
end;

procedure TStringContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do
    WriteString(HTMLEncode(Values[i]));
end;

procedure TStringContext.Send(Value: cardinal);
begin
  WriteString(IntToStr(Value));
end;

procedure TStringContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do
    WriteString(VarToStr(Values[i]));
end;

procedure TStringContext.Flush;
begin
  //ignore
end;

function TStringContext.GetBufferSize: integer;
begin
  //ignore
  Result:=-1;
end;

procedure TStringContext.SetBufferSize(ABufferSize: integer);
begin
  //ignore
end;

end.
 �  8   ���W E B _ D P R _ P R O T O       0         library [[ProjectName]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 273 $ $Date: 2013-02-22 22:34:20 +0100 (vr, 22 feb 2013) $
}

[[ProjectSwitches]]
uses
	[[@Include]][[IncludeUnit]] in '..\[[IncludePath]][[IncludeUnit]].pas',
	[[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
	[[@]][[UsesClause]]
	xxmp in '..\xxmp.pas';

{$E xxl}
[[ProjectHeader]]
exports
	XxmProjectLoad;
begin
[[ProjectBody]]
end.
 2
  <   ���X X M P _ P A S _ P R O T O         0         unit xxmp;

{
  xxm Project

This is a default xxm Project class inheriting from TXxmProject. You are free to change this one for your project.
Use LoadPage to process URL's as a requests is about to start.
(Be carefull with sending content from here though.)
It is advised to link each request to a session here, if you want session management.
(See  an example xxmSession.pas in the public folder.)
Use LoadFragment to handle calls made to Context.Include.

  $Rev: 457 $ $Date: 2018-07-12 22:39:00 +0200 (do, 12 jul 2018) $
}

interface

uses xxm;

type
  TXxm[[ProjectName]]=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject; stdcall;

implementation

uses xxmFReg;

function XxmProjectLoad(const AProjectName:WideString): IXxmProject;
begin
  Result:=TXxm[[ProjectName]].Create(AProjectName);
end;

type
  TRespondNotImplemented=class(TXxmPage)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

{ TXxm[[ProjectName]] }

function TXxm[[ProjectName]].LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
var
  verb:WideString;
begin
  inherited;
  Context.BufferSize:=$10000;

  //TODO: link session to request
  //  see demo project "02 Session"
  //SetSession(Context);
  
  verb:=Context.ContextString(csVerb);
  if (verb='OPTIONS') or (verb='TRACE') then
    Result:=TRespondNotImplemented.Create(Self)
  else
    Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
end;

function TXxm[[ProjectName]].LoadFragment(Context: IXxmContext; const Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
end;

procedure TXxm[[ProjectName]].UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

{ TRespondNotImplemented }

procedure TRespondNotImplemented.Build(const Context: IXxmContext; const Caller: IXxmFragment;
  const Values: array of OleVariant; const Objects: array of TObject);
begin
  inherited;
  Context.SetStatus(501,'Not Implemented');
end;

initialization
  IsMultiThread:=true;
end.
  j  8   ���X X M _ P A S _ P R O T O       0         unit [[FragmentUnit]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 102 $ $Date: 2010-09-15 14:42:45 +0200 (wo, 15 sep 2010) $
}

interface

uses xxm;

type
  [[FragmentID]]=class(TXxmPage)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

implementation

uses 
  SysUtils, 
[[UsesClause]]
  xxmFReg;
  
[[FragmentDefinitions]]
{ [[FragmentID]] }

procedure [[FragmentID]].Build(const Context: IXxmContext; const Caller: IXxmFragment; 
      const Values: array of OleVariant; const Objects: array of TObject);
[[FragmentHeader]]
begin
  inherited;
[[FragmentBody]]
end;

initialization
  XxmFragmentRegistry.RegisterClass('[[FragmentAddress]]',[[FragmentID]]);
[[FragmentFooter]]

end.
  l  <   ���X X M I _ P A S _ P R O T O         0         unit [[FragmentUnit]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 102 $ $Date: 2010-09-15 14:42:45 +0200 (wo, 15 sep 2010) $
}

interface

uses xxm;

type
  [[FragmentID]]=class(TXxmInclude)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

implementation

uses 
  SysUtils, 
[[UsesClause]]
  xxmFReg;
  
[[FragmentDefinitions]]
{ [[FragmentID]] }

procedure [[FragmentID]].Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject);
[[FragmentHeader]]
begin
  inherited;
[[FragmentBody]]
end;

initialization
  XxmFragmentRegistry.RegisterClass('[[FragmentAddress]]',[[FragmentID]]);
[[FragmentFooter]]

end.
