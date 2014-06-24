unit xxmString;

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
    procedure WriteString(Value:WideString);
  protected
    function Connected: Boolean;
    function ContextString(cs: TXxmContextString): WideString;
    procedure DispositionAttach(FileName: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    function GetContentType: WideString;
    function GetCookie(Name: WideString): WideString;
    function GetPage: IXxmFragment;
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
    function GetSessionID: WideString;
    function GetURL: WideString;
    function PostData: IStream;
    procedure Redirect(RedirectURL: WideString; Relative: Boolean);
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    procedure SetContentType(const Value: WideString);
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
    procedure SetStatus(Code: Integer; Text: WideString);
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
    procedure SendFile(FilePath: WideString);
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendStream(s: IStream);
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
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

procedure TStringContext.WriteString(Value: WideString);
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
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
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
        f.Write(Utf8ByteOrderMark,3);
        s:=UTF8Encode(Copy(FResult,1,FIndex));
        f.Write(s[1],Length(s));
       end;
      aeUtf16:
       begin
        f.Write(Utf16ByteOrderMark,2);
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

function TStringContext.GetCookie(Name: WideString): WideString;
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

procedure TStringContext.DispositionAttach(FileName: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support DispositionAttach');
end;

procedure TStringContext.Redirect(RedirectURL: WideString;
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

procedure TStringContext.SetStatus(Code: Integer; Text: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support Status');
end;

procedure TStringContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TStringContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TStringContext.Include(Address: WideString;
  const Values: array of OleVariant;
  const Objects: array of TObject);
var
  p:IXxmProject;
  f,fb:IXxmFragment;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  p:=FContext.Page.Project;
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

procedure TStringContext.SendFile(FilePath: WideString);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support SendFile');
end;

procedure TStringContext.SendStream(s: IStream);
begin
  raise EXxmUnsupported.Create('StringContext doesn''t support SendStream');
end;

procedure TStringContext.SetCookie(Name, Value: WideString);
begin
  FContext.SetCookie(Name, Value);
end;

procedure TStringContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
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
