unit xxmAhttpdPars;

interface

uses xxmHeaders, xxmParUtils, httpd24;

type
  TxxmAhttpdTable=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FPool:PPool;
    FTable:PTable;
  protected
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
    function Complex(Name: OleVariant; out Items: IxxmDictionary): WideString;
  public
    constructor Create(Pool: PPool; Table: PTable);
  end;

  TxxmAhttpdSubValues=class(TInterfacedObject, IxxmDictionary)
  private
    FTable:TxxmAhttpdTable;
    FIndex:integer;
    FData:AnsiString;
    FIdx:TParamIndexes;
    function ParseValue:WideString;
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create(Table: TxxmAhttpdTable; Index: integer;
      var FirstValue:WideString);
    destructor Destroy; override;
  end;

implementation

uses Classes, Variants, SysUtils, xxmCommonUtils;

{ TxxmAhttpdTable }

constructor TxxmAhttpdTable.Create(Pool: PPool; Table: PTable);
begin
  inherited Create;
  FPool:=Pool;
  FTable:=Table;
end;

function TxxmAhttpdTable.GetCount: integer;
begin
  Result:=FTable.a.nelts;
end;

function TxxmAhttpdTable.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  l,i:integer;
  n:string;
  sv:TxxmAhttpdSubValues;
begin
  l:=FTable.a.nelts;
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=LowerCase(string(Name));
    i:=0;
    while (i<l) and (RawCompare(LowerCase(string(FTable.a.elts[i].key)),n)<>0) do inc(i);//lower?
   end;
  if (i>=l) then i:=-1;
  sv:=TxxmAhttpdSubValues.Create(Self,i,Result);
  if @Items=nil then sv.Free else Items:=sv;
end;

function TxxmAhttpdTable.GetItem(Name: OleVariant): WideString;
var
  i:integer;
  n:AnsiString;
begin
  if VarIsNumeric(Name) then
   begin
    i:=integer(Name);
    if (i>=0) and (i<FTable.a.nelts) then
      Result:=UTF8ToWideString(FTable.a.elts[i].val)
    else
      raise ERangeError.Create('TxxmAhttpdTable.GetItem: Out of range');
   end
  else
   begin
    n:=AnsiString(VarToStr(Name));
    Result:=UTF8ToWideString(apr_table_get(FTable,PAnsiChar(n)));
   end;
end;

function TxxmAhttpdTable.GetName(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<FTable.a.nelts) then
    Result:=WideString(FTable.a.elts[Idx].key)
  else
    raise ERangeError.Create('TxxmAhttpdTable.GetName: Out of range');
end;

procedure TxxmAhttpdTable.SetItem(Name: OleVariant; const Value: WideString);
var
  i:integer;
begin
  HeaderCheckValue(Value);
  if VarIsNumeric(Name) then
   begin
    i:=integer(Name);
    if (i>=0) and (i<FTable.a.nelts) then
      FTable.a.elts[i].val:=apr_pstrdup(FPool,PAnsiChar(AnsiString(Value)))
    else
      raise ERangeError.Create('TxxmAhttpdTable.SetItem: Out of range');
   end
  else
   begin
    HeaderCheckName(VarToWideStr(Name));
    apr_table_set(FTable,PAnsiChar(AnsiString(Name)),PAnsiChar(AnsiString(Value)));
   end;
end;

procedure TxxmAhttpdTable.SetName(Idx: integer; Value: WideString);
begin
  HeaderCheckName(Value);
  if (Idx>=0) and (Idx<FTable.a.nelts) then
    FTable.a.elts[Idx].key:=
      apr_pstrdup(FPool,PAnsiChar(AnsiString(Value)))
  else
    raise ERangeError.Create('TxxmAhttpdTable.SetName: Out of range');
end;

{ TxxmAhttpdSubValues }

constructor TxxmAhttpdSubValues.Create(Table: TxxmAhttpdTable;
  Index: integer; var FirstValue: WideString);
begin
  inherited Create;
  FTable:=Table;
  FIndex:=Index;
  FirstValue:=ParseValue;
  FIdx.ParsIndex:=0;
  FIdx.ParsSize:=0;
end;

destructor TxxmAhttpdSubValues.Destroy;
begin
  FTable:=nil;
  inherited;
end;

function TxxmAhttpdSubValues.ParseValue: WideString;
begin
  if FIndex=-1 then FData:='' else FData:=UTF8Encode(FTable.GetItem(FIndex));
  Result:=UTF8ToWideString(SplitHeaderValue(FData,1,Length(FData),FIdx));
end;

function TxxmAhttpdSubValues.GetCount: integer;
begin
  ParseValue;
  Result:=FIdx.ParsIndex;
end;

function TxxmAhttpdSubValues.GetItem(Name: OleVariant): WideString;
var
  i:integer;
  n:string;
begin
  ParseValue;
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=LowerCase(VarToStr(Name));
    i:=0;
    while (i<FIdx.ParsIndex) and (RawCompare(FIdx.Pars[i].NameL,n)<>0) do inc(i);
   end;
  if (i>=0) and (i<FIdx.ParsIndex) then
    Result:=UTF8ToWideString(Copy(FData,
      FIdx.Pars[i].ValueStart,FIdx.Pars[i].ValueLength))
  else
    Result:='';
end;

procedure TxxmAhttpdSubValues.SetItem(Name: OleVariant; const Value: WideString);
var
  i,l:integer;
  n:string;
begin
  HeaderCheckValue(Value);
  ParseValue;
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=LowerCase(VarToStr(Name));
    i:=0;
    while (i<FIdx.ParsIndex) and (RawCompare(FIdx.Pars[i].NameL,n)<>0) do inc(i);
   end;
  if (i>=0) and (i<FIdx.ParsIndex) then
   begin
    if Value='' then
     begin
      if i+1=FIdx.ParsIndex then l:=Length(FData)+1 else l:=FIdx.Pars[i+1].NameStart;
      FTable.SetItem(FIndex,UTF8ToWideString(
        Copy(FData,1,FIdx.Pars[i].NameStart-1)+Copy(FData,l,Length(FData)-l+1)));
     end
    else
     begin
      l:=FIdx.Pars[i].ValueStart+FIdx.Pars[i].ValueLength;
      FTable.SetItem(FIndex,UTF8ToWideString(Copy(FData,1,FIdx.Pars[i].ValueStart-1))+
        Value+UTF8ToWideString(Copy(FData,l,Length(FData)-l+1)));
     end;
   end
  else
   begin
    HeaderCheckName(VarToWideStr(Name));
    FTable.SetItem(FIndex,UTF8ToWideString(FData)+'; '+VarToStr(Name)+'='+Value);
   end;
end;

function TxxmAhttpdSubValues.GetName(Idx: integer): WideString;
begin
  ParseValue;
  if (Idx>=0) and (Idx<FIdx.ParsIndex) then
    Result:=WideString(Copy(FData,FIdx.Pars[Idx].NameStart,FIdx.Pars[Idx].NameLength))
  else
    raise ERangeError.Create('TxxmAhttpdSubValues.GetName: Out of range');
end;

procedure TxxmAhttpdSubValues.SetName(Idx: integer; Value: WideString);
var
  l:integer;
begin
  HeaderCheckName(Value);
  ParseValue;
  if (Idx>=0) and (Idx<FIdx.ParsIndex) then
   begin
    l:=FIdx.Pars[Idx].NameStart+FIdx.Pars[Idx].NameLength;
    FTable.SetItem(FIndex,UTF8ToWideString(Copy(FData,1,FIdx.Pars[Idx].NameStart-1))+
      Value+UTF8ToWideString(Copy(FData,l,Length(FData)-l+1)));
   end
  else
    raise ERangeError.Create('TxxmAhttpdSubValues.SetName: Out of range');
end;

end.
