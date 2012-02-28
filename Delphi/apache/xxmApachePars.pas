unit xxmApachePars;

interface

uses xxmHeaders, HTTPD2;

type
  TxxmApacheTable=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FPool:Papr_pool_t;
    FTable:Papr_table_t;
  protected
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
    function Complex(Name: OleVariant; out Items: IxxmDictionary): WideString;
  public
    constructor Create(Pool:Papr_pool_t;Table:Papr_table_t);
  end;

implementation

uses Classes, Variants, xxmParUtils, SysUtils;

type
  table_entries=array[0..0] of apr_table_entry_t;
  Ptable_entries=^table_entries;

{ TxxmApacheTable }

constructor TxxmApacheTable.Create(Pool: Papr_pool_t; Table: Papr_table_t);
begin
  inherited Create;
  FPool:=Pool;
  FTable:=Table;
end;

function TxxmApacheTable.GetCount: integer;
begin
  Result:=FTable.a.nelts;
end;

function TxxmApacheTable.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  l,i:integer;
  sv:TRequestSubValues;
  x:WideString;
begin
  l:=FTable.a.nelts;
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    i:=0;
    while (i<l) and not(CompareText(Ptable_entries(FTable.a.elts)[i].key,Name)=0) do inc(i);//lower?
   end;
  if (i<l) then
   begin
    x:=Ptable_entries(FTable.a.elts)[i].val;
    sv:=TRequestSubValues.Create(x,1,Length(x),Result);
   end
  else
    sv:=TRequestSubValues.Create('',1,0,Result);//raise?
  if @Items=nil then sv.Free else Items:=sv;
end;

function TxxmApacheTable.GetItem(Name: OleVariant): WideString;
begin
  if VarIsNumeric(Name) then
    Result:=Ptable_entries(FTable.a.elts)[integer(Name)].val
  else
    Result:=apr_table_get(FTable,PAnsiChar(VarToStr(Name)));
end;

function TxxmApacheTable.GetName(Idx: integer): WideString;
begin
  Result:=Ptable_entries(FTable.a.elts)[Idx].key;
end;

procedure TxxmApacheTable.SetItem(Name: OleVariant;
  const Value: WideString);
begin
  HeaderCheckValue(Value);
  if VarIsNumeric(Name) then
    //TODO: check this!
    Ptable_entries(FTable.a.elts)[integer(Name)].val:=apr_pstrdup(FPool,PAnsiChar(AnsiString(Value)))
  else
   begin
    HeaderCheckName(VarToWideStr(Name));
    apr_table_set(FTable,PAnsiChar(AnsiString(Name)),PAnsiChar(AnsiString(Value)));
   end;
end;

procedure TxxmApacheTable.SetName(Idx: integer; Value: WideString);
begin
  //TODO: check this!
  HeaderCheckName(Value);
  Ptable_entries(FTable.a.elts)[Idx].key:=apr_pstrdup(FPool,PAnsiChar(AnsiString(Value)));
end;

end.
