unit xxmParUtils;

interface

uses SysUtils, Classes, xxmHeaders;

type
  TParamIndexes=array of record
    NameStart,NameLength,ValueStart,ValueLength:integer;
  end;

  TRequestHeaders=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FData:WideString;
    FIdx:TParamIndexes;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create(Data:string);
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
    constructor Create(Data:WideString;ValueStart,ValueLength:integer;
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
    FBuilt:boolean;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx: integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
    function Complex(Name:OleVariant;out Items:IxxmDictionary):WideString;
    function Build:string;
    procedure Add(Name,Value:WideString);
    procedure Remove(Name:WideString);
    function SetComplex(Name,Value:WideString):TResponseSubValues;
  end;

  TResponseSubValues=class(TInterfacedObject, IxxmDictionary)
  private
    FItems:array of record
      Name,Value:WideString;
    end;
    FBuilt:boolean;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Count:integer read GetCount;
    procedure Build(ss:TStringStream);
  end;

  TStreamNozzle=class(TObject)
  private
    FSource: TStream;
    SourceAtEnd: boolean;
    Data: string;
    Size,Index,Done:integer;
    function Ensure(EnsureSize:integer):boolean;
    procedure Flush;
    procedure SkipWhiteSpace;
  public
    constructor Create(Source:TStream);
    procedure CheckBoundary(var Boundary:string);
    function GetHeader(var Params:TParamIndexes):string;
    function GetString(Boundary:string):string;
    procedure GetData(Boundary:string;var Pos:integer;var Len:integer);
    function MultiPartDone:boolean;
  end;

  ExxmRequestHeadersReadOnly=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);

const //resourcestring
  SxxmRequestHeadersReadOnly='Request headers are read-only.';
  SXxmResponseHeaderAlreadySent='Response header has already been send.';

procedure SplitHeader(Value:string; var Params:TParamIndexes);
function SplitHeaderValue(Value:string;ValueStart,ValueLength:integer;
   var Params:TParamIndexes):string;
function GetParamValue(Data:string; Params:TParamIndexes; Name:string):string;

implementation

uses Variants;

procedure SplitHeader(Value:string; var Params:TParamIndexes);
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

    if not(q-p=2) then
     begin
      r:=p;
      while (r<=q) and (Value[r] in [#1..#32]) do inc(r);
      if r=p then
       begin
        SetLength(Params,i+1);
        Params[i].NameStart:=p;
        r:=p;
        while (r<=q) and not(Value[r]=':') do inc(r);
        Params[i].NameLength:=r-p;
        inc(r);
        while (r<=q) and (Value[r] in [#1..#32]) do inc(r);
        Params[i].ValueStart:=r;
        Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
        inc(i);
       end
      else
       begin
        //assert not(i=0)
        Params[i].ValueLength:=q-Params[i].ValueStart-2;
        //TODO: kill EOF and whitespace?
       end;
     end;
   end;
end;

function SplitHeaderValue(Value:string;ValueStart,ValueLength:integer;
  var Params:TParamIndexes):string;
var
  i,j,l,q:integer;
begin
  l:=ValueStart+ValueLength-1;
  i:=ValueStart;//set to 0 to start parsing sub-values
  if i=0 then inc(l) else while (i<=l) and not(Value[i]=';') do inc(i);
  if (i<=l) then
   begin
    if i=0 then Result:='' else Result:=Copy(Value,ValueStart,i-ValueStart);
    q:=0;

    while (i<=l) do
     begin
      SetLength(Params,q+1);
      inc(i);
      while (i<=l) and (Value[i] in [#1..#32]) do inc(i);
      Params[q].NameStart:=i;
      j:=i;
      while (j<=l) and not(Value[j]='=') do inc(j);
      Params[q].NameLength:=j-i;
      i:=j+1;
      if (i<=l) and (Value[i]='"') then
       begin
        //in quotes
        inc(i);
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and not(Value[j]='"') do inc(j);
        Params[q].ValueLength:=j-i;
        while (j<=l) and not(Value[j]=';') do inc(j);//ignore
       end
      else
       begin
        //not in quotes
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and not(Value[j]=';') do inc(j);
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

function GetParamValue(Data:string; Params:TParamIndexes; Name:string):string;
var
  l,i:integer;
begin
  l:=Length(Params);
  i:=0;
  while (i<l) and not(CompareText(Copy(Data,Params[i].NameStart,Params[i].NameLength),Name)=0) do inc(i);
  if (i<l) then Result:=Copy(Data,Params[i].ValueStart,Params[i].ValueLength) else Result:='';
end;

{ TStreamNozzle }

constructor TStreamNozzle.Create(Source: TStream);
begin
  inherited Create;
  FSource:=Source;
  Size:=0;
  Index:=1;
  Done:=0;
  SourceAtEnd:=false;
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
      if not(i=GrowStep) then SourceAtEnd:=true;
      Result:=Index+EnsureSize<=Size;
     end;
   end
  else
    Result:=true;
end;

procedure TStreamNozzle.Flush;
const
  FlushTreshold=$1000;
var
  l:integer;
begin
  if (Index>FlushTreshold) then
   begin
    l:=Size-Index+1;
    Move(Data[Index],Data[1],l);
    SetLength(Data,l);
    Size:=l;
    inc(Done,Index-1);
    Index:=1;
   end;
end;

procedure TStreamNozzle.SkipWhiteSpace;
begin
  //if '--' then multipart done?
  while Ensure(1) and (Data[Index] in [#0..#31]) do inc(Index);
end;

procedure TStreamNozzle.CheckBoundary(var Boundary: string);
var
  bl:integer;
begin
  bl:=Length(Boundary);
  Ensure(bl+5);
  //assert Index=1;
  if not(Copy(Data,3,bl)=Boundary) then
    raise Exception.Create('Multipart data does not start with boundary');
  Index:=bl+3;
  //TODO:detect EOL now?
  SkipWhiteSpace;
  Boundary:=#13#10'--'+Boundary;
  //Flush;?
end;

function TStreamNozzle.GetHeader(var Params: TParamIndexes): string;
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
  while Ensure(1) and not(q-p=2) do //2 being Length(EOL)
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

    if not(q-p=2) then
     begin
      SetLength(Params,i+1);
      Params[i].NameStart:=p;
      r:=p;
      while (r<=q) and not(Result[r]=':') do inc(r);
      Params[i].NameLength:=r-p;
      inc(r);
      while (r<=q) and (Result[r] in [#1..#32]) do inc(r);
      Params[i].ValueStart:=r;
      Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
      inc(i);
     end;
   end;
  SetLength(Result,q-1);
  Flush;
end;

function TStreamNozzle.GetString(Boundary: string): string;
var
  l,p,q:integer;
begin
  l:=Length(Boundary);
  p:=0;
  q:=Index;
  while Ensure(l) and not(p=l) do
   begin
    p:=0;
    while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
    if not(p=l) then inc(Index);
   end;
  SetLength(Result,Index-q);
  Move(Data[q],Result[1],Index-q);
  inc(Index,l);
  SkipWhiteSpace;
  Flush;
end;

procedure TStreamNozzle.GetData(Boundary: string; var Pos: integer; var Len: integer);
var
  l,p:integer;
begin
  Pos:=Done+Index-1;
  l:=Length(Boundary);
  p:=0;
  while Ensure(l) and not(p=l) do
   begin
    Flush;//depends on flush treshold
    p:=0;
    while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
    if not(p=l) then inc(Index);
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

constructor TRequestHeaders.Create(Data: string);
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
    while (i<l) and not(CompareText(Copy(FData,FIdx[i].NameStart,FIdx[i].NameLength),Name)=0) do inc(i);//lower?
   end;
  if (i<l) then
    sv:=TRequestSubValues.Create(FData,FIdx[i].ValueStart,FIdx[i].ValueLength,Result)
  else
    sv:=TRequestSubValues.Create('',1,0,Result);//raise?
  if @Items=nil then sv.Free else Items:=sv;
end;

procedure TRequestHeaders.SetItem(Name: OleVariant;
  const Value: WideString);
begin
  raise ExxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

function TRequestHeaders.GetName(Idx: integer): WideString;
begin
  Result:=Copy(FData,FIdx[Idx].NameStart,FIdx[Idx].NameLength);
end;

procedure TRequestHeaders.SetName(Idx: integer; Value: WideString);
begin
  raise ExxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

{ TRequestSubValues }

constructor TRequestSubValues.Create(Data: WideString; ValueStart,
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

procedure TRequestSubValues.SetItem(Name: OleVariant;
  const Value: WideString);
begin
  raise ExxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

procedure TRequestSubValues.SetName(Idx: integer; Value: WideString);
begin
  raise ExxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

{ TResponseHeaders }

constructor TResponseHeaders.Create;
begin
  inherited;
  FBuilt:=false;
  //SetLength(FItems,0);
end;

destructor TResponseHeaders.Destroy;
var
  i:integer;
begin
  for i:=0 to Length(FItems)-1 do
    if not(FItems[i].SubValues=nil) then
      (FItems[i].SubValues as IUnknown)._Release;
  SetLength(FItems,0);
  inherited;
end;

function TResponseHeaders.GetCount: integer;
begin
  Result:=Length(FItems);
end;

function TResponseHeaders.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
   end;
  if (i<Length(FItems)) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseHeaders.SetItem(Name: OleVariant;
  const Value: WideString);
var
  i:integer;
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
    if not(i<Length(FItems)) then
     begin
      SetLength(FItems,i+1);
      FItems[i].Name:=Name;
      FItems[i].SubValues:=nil;
     end;
   end;
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
    while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
    if not(i<Length(FItems)) then
     begin
      SetLength(FItems,i+1);
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

function TResponseHeaders.Build: string;
var
  ss:TStringStream;
  i:integer;
begin
  ss:=TStringStream.Create('');
  try
    for i:=0 to Length(FItems)-1 do
     begin
      ss.WriteString(FItems[i].Name);
      ss.WriteString(': ');
      ss.WriteString(FItems[i].Value);//TODO: encoding?
      if not(FItems[i].SubValues=nil) then
        FItems[i].SubValues.Build(ss);
      ss.WriteString(#13#10);
     end;
    Result:=ss.DataString;
  finally
    ss.Free;
  end;
  FBuilt:=true;
  //TODO: check max line length and separate over lines?
end;

procedure TResponseHeaders.Add(Name, Value: WideString);
var
  i:integer;
begin
  i:=Length(FItems);
  SetLength(FItems,i+1);
  FItems[i].Name:=Name;
  FItems[i].SubValues:=nil;
  FItems[i].Value:=Value;
end;

function TResponseHeaders.SetComplex(Name,
  Value: WideString): TResponseSubValues;
var
  i:integer;
begin
  i:=0;
  while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
  if not(i<Length(FItems)) then
   begin
    SetLength(FItems,i+1);
    FItems[i].Name:=Name;
    FItems[i].SubValues:=nil;
   end;
  FItems[i].Value:=Value;
  if FItems[i].SubValues=nil then
    FItems[i].SubValues:=TResponseSubValues.Create;
  (FItems[i].SubValues as IUnknown)._AddRef;
  Result:=FItems[i].SubValues;
end;

procedure TResponseHeaders.Remove(Name: WideString);
var
  i,l:integer;
begin
  i:=0;
  l:=Length(FItems);
  while (i<l) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
  if (i<l) then
   begin
    if not(FItems[i].SubValues=nil) then FItems[i].SubValues.Free;
    dec(l);
    while (i<l) do
     begin
      FItems[i]:=FItems[i+1];
      inc(i);
     end;
    if not(FItems[i].SubValues=nil) then FItems[i].SubValues.Free;
    SetLength(FItems,l);
   end;
end;

function TResponseHeaders.GetName(Idx: integer): WideString;
begin
  Result:=FItems[Idx].Name;
end;

procedure TResponseHeaders.SetName(Idx: integer; Value: WideString);
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  FItems[Idx].Name:=Value;
end;

{ TResponseSubValues }

constructor TResponseSubValues.Create;
begin
  inherited;
  FBuilt:=false;
  //SetLength(FItems,0);
end;

destructor TResponseSubValues.Destroy;
begin
  SetLength(FItems,0);
  inherited;
end;

function TResponseSubValues.GetCount: integer;
begin
  Result:=Length(FItems);
end;

function TResponseSubValues.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
   end;
  if (i<Length(FItems)) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseSubValues.SetItem(Name: OleVariant;
  const Value: WideString);
var
  i:integer;
begin
  if FBuilt then raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<Length(FItems)) and not(CompareText(FItems[i].Name,Name)=0) do inc(i);
    if not(i<Length(FItems)) then
     begin
      SetLength(FItems,i+1);
      FItems[i].Name:=Name;
     end;
   end;  
  FItems[i].Value:=Value;
end;

procedure TResponseSubValues.Build(ss: TStringStream);
var
  i:integer;
begin
  for i:=0 to Length(FItems)-1 do
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
  FItems[Idx].Name:=Value;
end;

end.
