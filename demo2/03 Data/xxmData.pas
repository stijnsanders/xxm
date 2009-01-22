unit xxmData;

{
xxmData provides a lightweight alternative to datamodules

SQL is stored in a file: "queries.xml"

Add this line to the project initialization code or the PrepareRequest method or session initialization code:
  if QueryStore=nil then QueryStore:=TQueryStore.Create;
  
(in unit initialization, CoInitialize is not called, which is needed for MSXML objects)

}

interface

uses SysUtils, ADODB_TLB, MSXML2_TLB;

type
  TQueryStore=class(TObject)
  private
    FQueries:array of record
      ID,SQL:string;
    end;
    procedure ReadQueriesXML;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSQL(QueryName:string):string;
  end;

  TQueryResult=class(TObject)
  private
    FRecordSet:Recordset;
    FFirstRead:boolean;
    function GetValue(Idx:OleVariant):OleVariant;
    function IsEof:boolean;
  public
    constructor Create(QueryName: string; const Values: array of Variant); overload;
    constructor Create(Recordset: Recordset); overload;
    constructor Create(SQL:string); overload;
    destructor Destroy; override;

    procedure Reset;
    function Read:boolean;
    property Fields[Idx:OleVariant]:OleVariant read GetValue; default;
    property EOF: boolean read IsEof;
    function GetInt(Idx:OleVariant):integer;
    function GetStr(Idx:OleVariant):WideString;
    function GetDate(Idx:OleVariant):TDateTime;
    function IsNull(Idx:OleVariant):boolean;
    function GetDefault(Idx,Default:OleVariant):OleVariant;
  end;

  TDataChanger=class(TObject)
  private
    FRecordSet:Recordset;
    function GetValue(Idx:OleVariant):OleVariant;
    procedure SetValue(Idx,Value:OleVariant);
  public
    constructor Create(TableName: string; Id:integer);
    destructor Destroy; override;

    class function Perform(QueryName:string; const Values: array of Variant): integer; overload;

    procedure Update;
    procedure Cancel;

    property Fields[Idx:OleVariant]:OleVariant read GetValue write SetValue; default;
  end;

  EQueryStoreError=class(Exception);
  EFieldNotFound=class(Exception);

var
  QueryStore: TQueryStore;

implementation

uses Variants, xxmSession, ComObj;

{ TQueryStore }

constructor TQueryStore.Create;
begin
  inherited Create;
  ReadQueriesXML;
end;

destructor TQueryStore.Destroy;
begin
  //SetLength(,0);?
  SetLength(FQueries,0);
  inherited;
end;

function TQueryStore.GetSQL(QueryName: string): string;
var
  i,l:integer;
begin
  //TODO: store sorted, better lookup algo?
  l:=Length(FQueries);
  i:=0;
  while (i<l) and not(FQueries[i].ID=QueryName) do inc(i);
  if i=l then
    raise EQueryStoreError.Create('Undefined query "'+QueryName+'"');
  Result:=FQueries[i].SQL;
end;

procedure TQueryStore.ReadQueriesXML;
var
  doc:DOMDocument;
  xl1:IXMLDOMNodeList;
  x1:IXMLDOMNode;
  i:integer;
begin
  //assert CoInitialize called
  //assert currentdir set to modulepath
  doc:=CoDOMDocument.Create;
  try
    if not(doc.load('queries.xml')) then
      raise EQueryStoreError.Create('queries.xml: '+doc.parseError.reason);
    xl1:=doc.documentElement.childNodes;
    x1:=xl1.nextNode;
    i:=0;
    while not(x1=nil) do
     begin
      //if x1.nodeType=NODE_ELEMENT then
      if x1.nodeName='query' then
       begin
        SetLength(FQueries,i+1);
        FQueries[i].ID:=(x1 as IXMLDOMElement).getAttribute('id');
		FQueries[i].SQL:=x1.text;
        inc(i);
       end;
	   
      x1:=xl1.nextNode;
     end;
  finally
    doc:=nil;
  end;
end;

{ TQueryResult }

constructor TQueryResult.Create(QueryName: string;
  const Values: array of Variant);
var
  cmd:Command;
  v:OleVariant;
  i:integer;
begin
  inherited Create;
  //FRecordSet:=Session.DbCon.Execute(,v,adCmdText);
  FFirstRead:=true;
  cmd:=CoCommand.Create;
  try
    cmd.CommandType:=adCmdText;
    cmd.CommandText:=QueryStore.GetSQL(QueryName);
    cmd.Set_ActiveConnection(Session.Connection);
    for i:=0 to Length(Values)-1 do
      cmd.Parameters.Append(cmd.CreateParameter('',adVariant,adParamInput,0,Values[i]));
    FRecordSet:=cmd.Execute(v,EmptyParam,0);
  finally
    cmd:=nil;
  end;
end;

constructor TQueryResult.Create(Recordset: Recordset);
begin
  inherited Create;
  FFirstRead:=true;
  FRecordSet:=Recordset;//Clone?
end;

constructor TQueryResult.Create(SQL: string);
begin
  inherited Create;
  FFirstRead:=true;
  //FRecordSet:=Session.DbCon.Execute(,v,adCmdText);
  FRecordSet:=CoRecordset.Create;
  FRecordSet.Open(
    SQL,
    Session.Connection,
    adOpenStatic,//? adOpenForwardOnly,//?
    adLockReadOnly,
    adCmdText);
end;

destructor TQueryResult.Destroy;
begin
  //FRecordSet.Close;
  FRecordSet:=nil;
  inherited;
end;

procedure TQueryResult.Reset;
begin
  FFirstRead:=true;
  FRecordSet.MoveFirst;
end;

function TQueryResult.GetInt(Idx: OleVariant): integer;
var
  v:OleVariant;
begin
  try
    //v:=FRecordSet.Fields[Idx].Value;
	v:=FRecordSet.Collect[idx];
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('GetInt: Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
  if VarIsNull(v) then Result:=0 else Result:=v;
end;

function TQueryResult.GetStr(Idx: OleVariant): WideString;
begin
  try
    //Result:=VarToWideStr(FRecordSet.Fields[Idx].Value);
	Result:=VarToWideStr(FRecordSet.Collect[Idx]);
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('GetStr: Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
end;

function TQueryResult.GetDate(Idx: OleVariant): TDateTime;
var
  v:OleVariant;
begin
  try
    //v:=FRecordSet.Fields[Idx].Value;
	v:=FRecordSet.Collect[Idx];
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('GetDate: Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
  if VarIsNull(v) then
    Result:=0 //Now?
  else
    Result:=VarToDateTime(v);
end;

function TQueryResult.GetValue(Idx: OleVariant): OleVariant;
begin
  try
    //Result:=FRecordSet.Fields[Idx].Value;
	Result:=FRecordSet.Collect[Idx];
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
end;

function TQueryResult.GetDefault(Idx,Default: OleVariant): OleVariant;
begin
  if FRecordSet.EOF then Result:=Default else Result:=GetValue(Idx);
end;

function TQueryResult.IsNull(Idx: OleVariant): boolean;
begin
  try
    //Result:=VarIsNull(FRecordSet.Fields[Idx].Value);
    Result:=VarIsNull(FRecordSet.Collect[Idx]);
  except
    on e:EOleException do
	 begin
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('GetInt: Field not found: '+VarToStr(Idx))
      else
        raise;
	  Result:=true;//counter warning
	 end;
  end;
end;

function TQueryResult.IsEof: boolean;
begin
  Result:=FRecordSet.EOF;
end;

function TQueryResult.Read: boolean;
begin
  if FRecordSet.EOF then Result:=false else
   begin
    if FFirstRead then FFirstRead:=false else FRecordSet.MoveNext;
    Result:=not(FRecordSet.EOF);
   end;
end;

{ TDataChanger }

constructor TDataChanger.Create(TableName: string; Id: integer);
begin
  inherited Create;
  FRecordSet:=CoRecordset.Create;
  //TODO: adCmdTable and find PK? first col?
  FRecordSet.Open(
    'SELECT * FROM '+TableName+' WHERE id='+IntToStr(id),
    Session.Connection,
    adOpenKeyset,//?
    adLockOptimistic,//adLockPessimistic?
    adCmdText);
  if id=0 then FRecordSet.AddNew(EmptyParam,EmptyParam);
  //else editmode?
end;

destructor TDataChanger.Destroy;
begin
  //FRecordSet.Close;
  FRecordSet:=nil;
  inherited;
end;

procedure TDataChanger.Update;
begin
  FRecordSet.Update(EmptyParam,EmptyParam);
end;

procedure TDataChanger.Cancel;
begin
  FRecordSet.CancelUpdate;
end;

function TDataChanger.GetValue(Idx: OleVariant): OleVariant;
begin
  try
    //Result:=FRecordSet.Fields[Idx].Value;
	Result:=FRecordSet.Collect[Idx];
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('GetValue: Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
end;

procedure TDataChanger.SetValue(Idx, Value: OleVariant);
begin
  try
    FRecordSet.Fields[Idx].Value:=Value;
  except
    on e:EOleException do
      if cardinal(e.ErrorCode)=$800A0CC1 then
        raise EFieldNotFound.Create('SetValue: Field not found: '+VarToStr(Idx));
      else
        raise;
  end;
end;

class function TDataChanger.Perform(QueryName: string;
  const Values: array of Variant): integer;
var
  cmd:Command;
  v:OleVariant;
  i:integer;
begin
  cmd:=CoCommand.Create;
  cmd.CommandType:=adCmdText;
  cmd.CommandText:=QueryStore.GetSQL(QueryName);
  cmd.Set_ActiveConnection(Session.Connection);
  for i:=0 to Length(Values)-1 do
    cmd.Parameters.Append(cmd.CreateParameter('',adVariant,adParamInput,0,Values[i]));
  cmd.Execute(v,EmptyParam,0);
  cmd:=nil;
  Result:=v;
end;

initialization
  //QueryStore created as needed (not in CoInit'ed thread here)
finalization
  FreeAndNil(QueryStore);

end.
