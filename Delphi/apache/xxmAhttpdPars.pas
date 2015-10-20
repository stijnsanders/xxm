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
    FData:WideString;
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

uses Classes, Variants, SysUtils;

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
  sv:TxxmAhttpdSubValues;
begin
  l:=FTable.a.nelts;
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<l) and not(CompareText(FTable.a.elts[i].key,Name)=0) do inc(i);//lower?
   end;
  if (i>=l) then i:=-1;
  sv:=TxxmAhttpdSubValues.Create(Self,i,Result);
  if @Items=nil then sv.Free else Items:=sv;
end;

function TxxmAhttpdTable.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then
   begin
    i:=integer(Name);
    if (i>=0) and (i<FTable.a.nelts) then
      Result:=FTable.a.elts[i].val
    else
      raise ERangeError.Create('TxxmAhttpdTable.GetItem: Out of range');
   end
  else
    Result:=apr_table_get(FTable,PAnsiChar(VarToStr(Name)));
end;

function TxxmAhttpdTable.GetName(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<FTable.a.nelts) then
    Result:=FTable.a.elts[Idx].key
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
end;

destructor TxxmAhttpdSubValues.Destroy;
begin
  FTable:=nil;
  inherited;
end;

function TxxmAhttpdSubValues.ParseValue: WideString;
begin
  if FIndex=-1 then FData:='' else FData:=FTable.GetItem(FIndex);
  Result:=SplitHeaderValue(FData,1,Length(FData),FIdx);
end;

function TxxmAhttpdSubValues.GetCount: integer;
begin
  ParseValue;
  Result:=Length(FIdx);
end;

function TxxmAhttpdSubValues.GetItem(Name: OleVariant): WideString;
var
  l,i:integer;
  n:string;
begin
  ParseValue;
  l:=Length(FIdx);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=VarToStr(Name);
    i:=0;
    while (i<l) and (CompareText(Copy(FData,FIdx[i].NameStart,FIdx[i].NameLength),n)<>0) do inc(i);
   end;
  if (i>=0) and (i<l) then
    Result:=Copy(FData,FIdx[i].ValueStart,FIdx[i].ValueLength)
  else
    Result:='';
end;

procedure TxxmAhttpdSubValues.SetItem(Name: OleVariant; const Value: WideString);
var
  l,i:integer;
  n:string;
begin
  HeaderCheckValue(Value);
  ParseValue;
  l:=Length(FIdx);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=VarToStr(Name);
    i:=0;
    while (i<l) and (CompareText(Copy(FData,FIdx[i].NameStart,FIdx[i].NameLength),n)<>0) do inc(i);
   end;
  if (i>=0) and (i<l) then
   begin
    if Value='' then
     begin
      if i+1=l then l:=Length(FData)+1 else l:=FIdx[i+1].NameStart;
      FTable.SetItem(FIndex,Copy(FData,1,FIdx[i].NameStart-1)+
        Copy(FData,l,Length(FData)-l+1));
     end
    else
     begin
      l:=FIdx[i].ValueStart+FIdx[i].ValueLength;
      FTable.SetItem(FIndex,Copy(FData,1,FIdx[i].ValueStart-1)+Value+
        Copy(FData,l,Length(FData)-l+1));
     end;
   end
  else
   begin
    HeaderCheckName(VarToWideStr(Name));
    FTable.SetItem(FIndex,FData+'; '+VarToStr(Name)+'='+Value);
   end;
end;

function TxxmAhttpdSubValues.GetName(Idx: integer): WideString;
begin
  ParseValue;
  if (Idx>=0) and (Idx<Length(FIdx)) then
    Result:=Copy(FData,FIdx[Idx].NameStart,FIdx[Idx].NameLength)
  else
    raise ERangeError.Create('TxxmAhttpdSubValues.GetName: Out of range');
end;

procedure TxxmAhttpdSubValues.SetName(Idx: integer; Value: WideString);
var
  l:integer;
begin
  HeaderCheckName(Value);
  ParseValue;
  if (Idx>=0) and (Idx<Length(FIdx)) then
   begin
    l:=FIdx[Idx].NameStart+FIdx[Idx].NameLength;
    FTable.SetItem(FIndex,Copy(FData,1,FIdx[Idx].NameStart-1)+Value+
      Copy(FData,l,Length(FData)-l+1));
   end
  else
    raise ERangeError.Create('TxxmAhttpdSubValues.SetName: Out of range');
end;

end.
