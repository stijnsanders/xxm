unit xxmParUtils;

interface

uses SysUtils, Classes, xxmHeaders;

type
  TParamIndexes=array of record
    NameStart,NameLength,ValueStart,ValueLength:integer;
  end;

  TRequestHeaders=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FData:AnsiString;
    FIdx:TParamIndexes;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create(const Data:AnsiString);
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx: integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
    function Complex(Name:OleVariant;out Items:IxxmDictionary):WideString;
  end;

  TRequestSubValues=class(TInterfacedObject, IxxmDictionary)
  private
    FData:WideString;
    FIdx:TParamIndexes;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create(const Data:WideString;ValueStart,ValueLength:integer;
      var FirstValue:WideString);
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx: integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
  end;

  TResponseSubValues=class;//forward

  TResponseHeaders=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FItems:array of record
      Name,Value:WideString;
      SubValues:TResponseSubValues;
    end;
    FItemsSize,FItemsCount:integer;
    FBuilt:boolean;
    procedure Grow;
    function GetCount:integer;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
  public
    constructor Create;
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx: integer]:WideString read GetName write SetName;
    property Count:integer read FItemsCount;//read GetCount;
    function Complex(Name:OleVariant;out Items:IxxmDictionary):WideString;
    function Build:AnsiString;
    procedure Add(const Name,Value:WideString);
    procedure Remove(const Name:WideString);
    function SetComplex(const Name,Value:WideString):TResponseSubValues;
  end;

  TResponseSubValues=class(TInterfacedObject, IxxmDictionary)
  private
    FItems:array of record
      Name,Value:WideString;
    end;
    FItemsSize,FItemsCount:integer;
    FBuilt:boolean;
    procedure Grow;
    function GetCount:integer;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
  public
    constructor Create;
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Count:integer read FItemsCount;//read GetCount;
    procedure Build(ss:TStringStream);
  end;

  TStreamNozzle=class(TObject)
  private
    FSource: TStream;
    SourceAtEnd: boolean;
    Data: AnsiString;
    Size, Index, Done, ReportStep: integer;
    FDataAgent, FFileAgent: IxxmUploadProgressAgent;
    function Ensure(EnsureSize: integer): boolean;
    procedure Flush;
    procedure SkipWhiteSpace;
  public
    constructor Create(Source: TStream; DataProgressAgent, FileProgressAgent: IxxmUploadProgressAgent;
      FileProgressStep: integer);
    destructor Destroy; override;
    procedure CheckBoundary(var Boundary: AnsiString);
    function GetHeader(var Params: TParamIndexes): AnsiString;
    function GetString(Boundary: AnsiString): AnsiString;
    procedure GetData(Boundary, FieldName, FileName: AnsiString; var Pos: integer;var Len: integer);
    function MultiPartDone: boolean;
  end;

  EXxmRequestHeadersReadOnly=class(Exception);
  EXxmResponseHeaderInvalidChar=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);

const //resourcestring
  SXxmRequestHeadersReadOnly='Request headers are read-only.';
  SXxmResponseHeaderInvalidChar='Response header add: value contains invalid character.';
  SXxmResponseHeaderAlreadySent='Response header has already been sent.';

procedure SplitHeader(const Value:AnsiString; var Params:TParamIndexes);
function SplitHeaderValue(const Value:AnsiString;ValueStart,ValueLength:integer;
  var Params:TParamIndexes):AnsiString;
function GetParamValue(const Data:AnsiString;
  Params:TParamIndexes; const Name:AnsiString):AnsiString;
procedure HeaderCheckName(const Name: WideString);
procedure HeaderCheckValue(const Value: WideString);

{$IF not Declared(UTF8ToWideString)}
{$DEFINE NOT_DECLARED_UTF8ToWideString}
{$IFEND}

{$IFDEF NOT_DECLARED_UTF8ToWideString}
function UTF8ToWideString(const s: UTF8String): WideString;
{$ENDIF}

implementation

uses Variants;

procedure SplitHeader(const Value:AnsiString; var Params:TParamIndexes);
var
  b:boolean;
  p,q,l,r,i:integer;
begin
  q:=1;
  i:=0;
  l:=Length(Value);
  while (q<=l) do
   begin
    p:=q;
    b:=false;
    while (q<=l) and not(b and (Value[q]=#10)) do
     begin
      b:=Value[q]=#13;
      inc(q);
     end;
    inc(q);

    if q-p<>2 then
     begin
      r:=p;
      while (r<=q) and (char(Value[r]) in [#1..#32]) do inc(r);
      if r=p then
       begin
        SetLength(Params,i+1);
        Params[i].NameStart:=p;
        r:=p;
        while (r<=q) and (Value[r]<>':') do inc(r);
        Params[i].NameLength:=r-p;
        inc(r);
        while (r<=q) and (char(Value[r]) in [#1..#32]) do inc(r);
        Params[i].ValueStart:=r;
        Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
        inc(i);
       end
      else
       begin
        //assert i<>0
        Params[i].ValueLength:=q-Params[i].ValueStart-2;
        //TODO: kill EOF and whitespace?
       end;
     end;
   end;
end;

function SplitHeaderValue(const Value:AnsiString;
  ValueStart,ValueLength:integer;var Params:TParamIndexes):AnsiString;
var
  i,j,l,q:integer;
begin
  l:=ValueStart+ValueLength-1;
  i:=ValueStart;//set to 0 to start parsing sub-values
  if i=0 then inc(l) else while (i<=l) and (Value[i]<>';') do inc(i);
  if i<=l then
   begin
    if i=0 then Result:='' else Result:=Copy(Value,ValueStart,i-ValueStart);
    q:=0;
    while i<=l do
     begin
      SetLength(Params,q+1);
      inc(i);
      while (i<=l) and (char(Value[i]) in [#1..#32]) do inc(i);
      Params[q].NameStart:=i;
      j:=i;
      while (j<=l) and (Value[j]<>'=') do inc(j);
      Params[q].NameLength:=j-i;
      i:=j+1;
      if (i<=l) and (Value[i]='"') then
       begin
        //in quotes
        inc(i);
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and (Value[j]<>'"') do inc(j);
        Params[q].ValueLength:=j-i;
        while (j<=l) and (Value[j]<>';') do inc(j);//ignore
       end
      else
       begin
        //not in quotes
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and (Value[j]<>';') do inc(j);
        Params[q].ValueLength:=j-i;
       end;
      i:=j;
      inc(q);
     end;
   end
  else
   begin
    Result:=Copy(Value,ValueStart,ValueLength);
    SetLength(Params,0);
   end;
end;

function GetParamValue(const Data:AnsiString;
  Params:TParamIndexes; const Name:AnsiString):AnsiString;
var
  l,i:integer;
begin
  l:=Length(Params);
  i:=0;
  while (i<l) and (CompareText(Copy(Data,Params[i].NameStart,Params[i].NameLength),Name)<>0) do inc(i);
  if (i<l) then Result:=Copy(Data,Params[i].ValueStart,Params[i].ValueLength) else Result:='';
end;

procedure HeaderCheckName(const Name: WideString);
var
  i:integer;
begin
  for i:=1 to Length(Name) do if char(Name[i]) in [#0..' ',
    '(',')','<','>','@',',',';',':','\','"','/',
    '[',']','?','=','{','}',#127..#255] then
    raise EXxmResponseHeaderInvalidChar.Create(SXxmResponseHeaderInvalidChar);
end;

procedure HeaderCheckValue(const Value: WideString);
var
  i:integer;
begin
  for i:=1 to Length(Value) do if char(Value[i]) in [#0,#10,#13] then //more?
    raise EXxmResponseHeaderInvalidChar.Create(SXxmResponseHeaderInvalidChar);
end;

{$IFDEF NOT_DECLARED_UTF8ToWideString}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$ENDIF}

{ TStreamNozzle }

constructor TStreamNozzle.Create(Source: TStream; DataProgressAgent,
  FileProgressAgent: IxxmUploadProgressAgent; FileProgressStep: integer);
begin
  inherited Create;
  FSource:=Source;
  Size:=0;
  Index:=1;
  Done:=0;
  SourceAtEnd:=false;
  FDataAgent:=DataProgressAgent;
  FFileAgent:=FileProgressAgent;
  ReportStep:=FileProgressStep;
end;

destructor TStreamNozzle.Destroy;
begin
  FDataAgent:=nil;
  FFileAgent:=nil;
  inherited;
end;

function TStreamNozzle.Ensure(EnsureSize: integer):boolean;
var
  i:integer;
const
  GrowStep=$10000;
begin
  //assert EnsureSize<=GrowStep
  if Index+EnsureSize>Size then
   begin
    if SourceAtEnd then Result:=false else
     begin
      i:=GrowStep;
      SetLength(Data,Size+i);
      i:=FSource.Read(Data[Size+1],i);
      inc(Size,i);
      if i=0 then SourceAtEnd:=true;
      Result:=Index+EnsureSize<=Size;
      if FDataAgent<>nil then FDataAgent.ReportProgress('','',Done+Size);
     end;
   end
  else
    Result:=true;
end;

procedure TStreamNozzle.Flush;
const
  FlushThreshold=$1000;
var
  l:integer;
begin
  if Index>FlushThreshold then
   begin
    l:=Size-Index+1;
    Move(PAnsiChar(@Data[Index])^,PAnsiChar(@Data[1])^,l);
    SetLength(Data,l);
    Size:=l;
    inc(Done,Index-1);
    Index:=1;
   end;
end;

procedure TStreamNozzle.SkipWhiteSpace;
begin
  //if '--' then multipart done?
  while Ensure(1) and (char(Data[Index]) in [#0..#31]) do inc(Index);
end;

procedure TStreamNozzle.CheckBoundary(var Boundary: AnsiString);
var
  bl:integer;
begin
  bl:=Length(Boundary);
  Ensure(bl+5);
  //assert Index=1;
  if Copy(Data,3,bl)<>Boundary then
    raise Exception.Create('Multipart data does not start with boundary');
  Index:=bl+3;
  //TODO:detect EOL now?
  SkipWhiteSpace;
  Boundary:=#13#10'--'+Boundary;
  //Flush;?
end;

function TStreamNozzle.GetHeader(var Params: TParamIndexes): AnsiString;
const
  GrowStep=$1000;
var
  b:boolean;
  p,q,r,s,i:integer;
begin
  p:=0;
  q:=1;
  i:=0;
  s:=0;
  while Ensure(1) and (q-p<>2) do //2 being Length(EOL)
   begin
    p:=q;
    b:=false;
    while Ensure(1) and not(b and (Data[Index]=#10)) do
     begin
      if q>s then
       begin
        inc(s,GrowStep);
        SetLength(Result,s);
       end;
      Result[q]:=Data[Index];
      b:=Data[Index]=#13;
      inc(Index);
      inc(q);
     end;
    Result[q]:=Data[Index];
    inc(Index);
    inc(q);

    if q-p<>2 then
     begin
      SetLength(Params,i+1);
      Params[i].NameStart:=p;
      r:=p;
      while (r<=q) and (Result[r]<>':') do inc(r);
      Params[i].NameLength:=r-p;
      inc(r);
      while (r<=q) and (char(Result[r]) in [#1..#32]) do inc(r);
      Params[i].ValueStart:=r;
      Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
      inc(i);
     end;
   end;
  SetLength(Result,q-1);
  Flush;
end;

function TStreamNozzle.GetString(Boundary: AnsiString): AnsiString;
var
  l,p,q:integer;
begin
  l:=Length(Boundary);
  p:=0;
  q:=Index;
  while Ensure(l) and (p<>l) do
   begin
    p:=0;
    while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
    if p<>l then inc(Index);
   end;
  SetLength(Result,Index-q);
  Move(PAnsiChar(@Data[q])^,PAnsiChar(@Result[1])^,Index-q);
  inc(Index,l);
  SkipWhiteSpace;
  Flush;
end;

procedure TStreamNozzle.GetData(Boundary, FieldName, FileName: AnsiString; var Pos: integer; var Len: integer);
var
  l,p,x,s:integer;
begin
  Pos:=Done+Index-1;
  l:=Length(Boundary);
  p:=0;
  if (ReportStep=0) or (FFileAgent=nil) then
   begin
    //short loop
    while Ensure(l) and (p<>l) do
     begin
      Flush;//depends on flush threshold
      p:=0;
      while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
      if p<>l then inc(Index);
     end;
   end
  else
   begin
    //full loop
    x:=ReportStep;
    s:=0;
    while Ensure(l) and (p<>l) do
     begin
      Flush;//depends on flush threshold
      p:=0;
      while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
      if p<>l then
       begin
        inc(Index);
        inc(s);
        dec(x);
        if x=0 then
         begin
          FFileAgent.ReportProgress(FieldName,FileName,s);
          x:=ReportStep;
         end
        else
          inc(x);
       end;
      FFileAgent.ReportProgress(FieldName,FileName,s);
     end;
   end;
  Len:=Done+Index-(Pos+1);
  //skip boundary
  inc(Index,l);
  SkipWhiteSpace;
  Flush;
end;

function TStreamNozzle.MultiPartDone: boolean;
begin
  //assert just matched boundary
  Result:=not(Ensure(2)) or ((Data[Index]='-') and (Data[Index+1]='-'));
end;

{ TRequestHeaders }

constructor TRequestHeaders.Create(const Data: AnsiString);
begin
  inherited Create;
  FData:=Data;
  SplitHeader(FData,FIdx);
end;

destructor TRequestHeaders.Destroy;
begin
  SetLength(FIdx,0);
  FData:='';
  inherited;
end;

function TRequestHeaders.GetCount: integer;
begin
  Result:=Length(FIdx);
end;

function TRequestHeaders.GetItem(Name: OleVariant): WideString;
begin
  if VarIsNumeric(Name) then
    with FIdx[integer(Name)] do
      Result:=Copy(FData,ValueStart,ValueLength)
  else
    Result:=GetParamValue(FData,FIdx,Name);
end;

function TRequestHeaders.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  l,i:integer;
  sv:TRequestSubValues;
begin
  l:=Length(FIdx);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<l) and (CompareText(Copy(FData,FIdx[i].NameStart,FIdx[i].NameLength),Name)<>0) do inc(i);//lower?
   end;
  if (i<l) then
    sv:=TRequestSubValues.Create(FData,FIdx[i].ValueStart,FIdx[i].ValueLength,Result)
  else
    sv:=TRequestSubValues.Create('',1,0,Result);//raise?
  if @Items=nil then sv.Free else Items:=sv;
end;

procedure TRequestHeaders.SetItem(Name: OleVariant; const Value: WideString);
begin
  raise EXxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

function TRequestHeaders.GetName(Idx: integer): WideString;
begin
  Result:=Copy(FData,FIdx[Idx].NameStart,FIdx[Idx].NameLength);
end;

procedure TRequestHeaders.SetName(Idx: integer; Value: WideString);
begin
  raise EXxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

{ TRequestSubValues }

constructor TRequestSubValues.Create(const Data: WideString; ValueStart,
  ValueLength: integer; var FirstValue: WideString);
begin
  inherited Create;
  FData:=Data;//assert reference counting, full copy is senseless
  FirstValue:=SplitHeaderValue(FData,ValueStart,ValueLength,FIdx);
end;

destructor TRequestSubValues.Destroy;
begin
  SetLength(FIdx,0);
  FData:='';
  inherited;
end;

function TRequestSubValues.GetCount: integer;
begin
  Result:=Length(FIdx);
end;

function TRequestSubValues.GetItem(Name: OleVariant): WideString;
begin
  if VarIsNumeric(Name) then
    with FIdx[integer(Name)] do
     Result:=Copy(FData,ValueStart,ValueLength)
  else
    Result:=GetParamValue(FData,FIdx,Name);
end;

function TRequestSubValues.GetName(Idx: integer): WideString;
begin
  Result:=Copy(FData,FIdx[Idx].NameStart,FIdx[Idx].NameLength);
end;

procedure TRequestSubValues.SetItem(Name: OleVariant; const Value: WideString);
begin
  raise EXxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

procedure TRequestSubValues.SetName(Idx: integer; Value: WideString);
begin
  raise EXxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

{ TResponseHeaders }

constructor TResponseHeaders.Create;
begin
  inherited;
  FBuilt:=false;
  FItemsCount:=0;
  FItemsSize:=0;
  //SetLength(FItems,0);
end;

destructor TResponseHeaders.Destroy;
var
  i:integer;
begin
  for i:=0 to FItemsCount-1 do
    if FItems[i].SubValues<>nil then
      (FItems[i].SubValues as IUnknown)._Release;
  SetLength(FItems,0);
  inherited;
end;

procedure TResponseHeaders.Grow;
begin
  if FItemsCount=FItemsSize then
   begin
    inc(FItemsSize,64);
    SetLength(FItems,FItemsSize);
   end;
  inc(FItemsCount);
end;

function TResponseHeaders.GetCount: integer;
begin
  Result:=FItemsCount;
end;

function TResponseHeaders.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
   end;
  if (i<FItemsCount) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseHeaders.SetItem(Name: OleVariant; const Value: WideString);
var
  i:integer;
begin
  if FBuilt then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  //TODO: add sorted, query with minimax
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
    if i=FItemsCount then
     begin
      HeaderCheckName(Name);
      Grow;
      FItems[i].Name:=Name;
      FItems[i].SubValues:=nil;
     end;
   end;
  HeaderCheckValue(Value);
  FItems[i].Value:=Value;
end;

function TResponseHeaders.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
    if i=FItemsCount then
     begin
      HeaderCheckName(Name);
      Grow;
      FItems[i].Name:=Name;
      FItems[i].Value:='';
      FItems[i].SubValues:=nil;
     end;
   end;
  if FItems[i].SubValues=nil then
    FItems[i].SubValues:=TResponseSubValues.Create;
  Result:=FItems[i].Value;
  (FItems[i].SubValues as IUnknown)._AddRef;
  Items:=FItems[i].SubValues;
end;

function TResponseHeaders.Build: AnsiString;
var
  ss:TStringStream;
  i:integer;
begin
  ss:=TStringStream.Create('');
  try
    for i:=0 to FItemsCount-1 do
     begin
      ss.WriteString(FItems[i].Name);
      ss.WriteString(': ');
      ss.WriteString(FItems[i].Value);//TODO: encoding?
      if FItems[i].SubValues<>nil then
        FItems[i].SubValues.Build(ss);
      ss.WriteString(#13#10);
     end;
    Result:=ss.DataString;
  finally
    ss.Free;
  end;
  FBuilt:=true;
end;

procedure TResponseHeaders.Add(const Name, Value: WideString);
var
  i:integer;
begin
  HeaderCheckName(Name);
  HeaderCheckValue(Value);
  i:=FItemsCount;
  Grow;
  FItems[i].Name:=Name;
  FItems[i].SubValues:=nil;
  FItems[i].Value:=Value;
end;

function TResponseHeaders.SetComplex(const Name,
  Value: WideString): TResponseSubValues;
var
  i:integer;
begin
  i:=0;
  while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
  if i=FItemsCount then
   begin
    HeaderCheckName(Name);
    Grow;
    FItems[i].Name:=Name;
    FItems[i].SubValues:=nil;
   end;
  HeaderCheckValue(Value);
  FItems[i].Value:=Value;
  if FItems[i].SubValues=nil then
    FItems[i].SubValues:=TResponseSubValues.Create;
  (FItems[i].SubValues as IUnknown)._AddRef;
  Result:=FItems[i].SubValues;
end;

procedure TResponseHeaders.Remove(const Name: WideString);
var
  i,l:integer;
begin
  i:=0;
  l:=FItemsCount;
  while (i<l) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
  if i<l then
   begin
    if FItems[i].SubValues<>nil then FItems[i].SubValues.Free;
    dec(l);
    while i<l do
     begin
      FItems[i]:=FItems[i+1];
      inc(i);
     end;
    if FItems[i].SubValues<>nil then FItems[i].SubValues.Free;
    dec(FItemsCount);
   end;
end;

function TResponseHeaders.GetName(Idx: integer): WideString;
begin
  Result:=FItems[Idx].Name;
end;

procedure TResponseHeaders.SetName(Idx: integer; Value: WideString);
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  HeaderCheckName(Value);
  FItems[Idx].Name:=Value;
end;

{ TResponseSubValues }

constructor TResponseSubValues.Create;
begin
  inherited;
  FBuilt:=false;
  FItemsCount:=0;
  FItemsSize:=0;
  //SetLength(FItems,0);
end;

destructor TResponseSubValues.Destroy;
begin
  SetLength(FItems,0);
  inherited;
end;

procedure TResponseSubValues.Grow;
begin
  if FItemsCount=FItemsSize then
   begin
    inc(FItemsSize,32);
    SetLength(FItems,FItemsSize);
   end;
  inc(FItemsCount);
end;

function TResponseSubValues.GetCount: integer;
begin
  Result:=FItemsCount;
end;

function TResponseSubValues.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
   end;
  if (i<FItemsCount) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseSubValues.SetItem(Name: OleVariant; const Value: WideString);
var
  i:integer;
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  HeaderCheckValue(Value);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<FItemsCount) and (CompareText(FItems[i].Name,Name)<>0) do inc(i);
    if i=FItemsCount then
     begin
      HeaderCheckName(Name);
      Grow;
      FItems[i].Name:=Name;
     end;
   end;
  FItems[i].Value:=Value;
end;

procedure TResponseSubValues.Build(ss: TStringStream);
var
  i:integer;
begin
  for i:=0 to FItemsCount-1 do
   begin
    ss.WriteString('; ');
    ss.WriteString(FItems[i].Name);
    ss.WriteString('="');
    ss.WriteString(FItems[i].Value);//todo: encoding, escape
    ss.WriteString('"');
   end;
  FBuilt:=true;
end;

function TResponseSubValues.GetName(Idx: integer): WideString;
begin
  Result:=FItems[Idx].Name;
end;

procedure TResponseSubValues.SetName(Idx: integer; Value: WideString);
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  HeaderCheckName(Value);
  FItems[Idx].Name:=Value;
end;

end.
