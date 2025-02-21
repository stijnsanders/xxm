unit xxmData;

{
xxmData provides a lightweight alternative to datamodules

SQL is stored in a file: "queries.sql"
Queries are defined by a preceding line with '--"QueryName"'

Add this line to the project initialization code
or the PrepareRequest method or session initialization code:
  if QueryStore=nil then QueryStore:=TQueryStore.Create;

}

interface

uses SysUtils, ADODB_TLB;

type
  TQueryStore=class(TObject)
  private
    FQueries:array of record
      ID, SQL: AnsiString;
    end;
    procedure ReadQueriesSQL;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSQL(const QueryName: AnsiString): AnsiString;
  end;

  TQueryResult=class(TObject)
  private
    FRecordSet:Recordset;
    FFirstRead:boolean;
    function GetValue(Idx:OleVariant):OleVariant;
    function IsEof:boolean;
  public
    constructor Create(const QueryName: AnsiString; const Values: array of Variant); overload;
    constructor Create(Recordset: Recordset); overload;
    constructor Create(const SQL: AnsiString); overload;
    destructor Destroy; override;

    class function SingleValue(const QueryName: AnsiString; const Values: array of Variant): Variant; overload;
    class function SingleValue(const SQL: AnsiString): Variant; overload;

    procedure Reset;
    procedure CheckResultSet;//use with multiple resultsets (e.g. when calling stored procedure)
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
    constructor Create(const TableName, PKName: AnsiString; const Id: Variant);
    destructor Destroy; override;

    class function Execute(const QueryName: AnsiString; const Values: array of Variant): integer; overload;

    function CheckNew: boolean;
    procedure Update;
    procedure Cancel;

    property Fields[Idx:OleVariant]:OleVariant read GetValue write SetValue; default;
  end;

  EQueryStoreError=class(Exception);
  EFieldNotFound=class(Exception);
  ESingleValueFailed=class(Exception);

var
  QueryStore: TQueryStore;

implementation

uses Variants, Classes, ComObj, xxmSession;

{ TQueryStore }

constructor TQueryStore.Create;
begin
  inherited Create;
  ReadQueriesSQL;
end;

destructor TQueryStore.Destroy;
begin
  //SetLength(,0);?
  SetLength(FQueries,0);
  inherited;
end;

function TQueryStore.GetSQL(const QueryName: AnsiString): AnsiString;
var
  i,l:integer;
begin
  //TODO: store sorted, better lookup algo?
  l:=Length(FQueries);
  i:=0;
  while (i<l) and (FQueries[i].ID<>QueryName) do inc(i);
  if i=l then
    raise EQueryStoreError.Create('Undefined query "'+QueryName+'"');
  Result:=FQueries[i].SQL;
end;

procedure TQueryStore.ReadQueriesSQL;
var
  s:string;
  f:TFileStream;
  i,l,q,ql,r1,r2,s1,s2,n1,n2:integer;
begin
  //assert currentdir set to modulepath
  f:=TFileStream.Create('queries.sql',fmOpenRead or fmShareDenyWrite);
  try
    //TODO: support unicode?
    l:=f.Size;
    SetLength(s,l);
    f.Read(s[1],l);
  finally
    f.Free;
  end;
  i:=1;
  q:=0;
  ql:=0;
  r1:=0;
  s1:=0;
  s2:=0;
  while (i<=l) do
   begin
    //does it start with '--"'
    if (i+3<l) and (s[i]='-') and (s[i+1]='-') and (s[i+2]='"') then
     begin
      r2:=i-1;
      inc(i,3);
      n1:=i;
      //skip trailing whitespace
      while (r2<>0) and (s[r2]<' ') do dec(r2);
      //and is it properly closed with '"'
      while (i<=l) and (s[i]<>'"') and (s[i]<>#13) and (s[i]<>#10) do inc(i);
      if (i<=l) and (s[i]='"') then
       begin
        n2:=i;
        //skip to EOL
        while (i<=l) and (s[i]<>#13) and (s[i]<>#10) do inc(i);
        if (i<l) and (s[i]=#13) and (s[i+1]=#10) then inc(i);
        inc(i);
        //skip preceding whitespace
        while (i<=l) and (s[i]<' ') do inc(i);
        //process previous marker
        if r1<>0 then
         begin
          if q=ql then
           begin
            inc(ql,$100);//grow
            SetLength(FQueries,ql);
           end;
          FQueries[q].ID:=Copy(s,s1,s2-s1);
          FQueries[q].SQL:=Copy(s,r1,r2-r1+1);
          inc(q);
         end;
        //set start marker
        r1:=i;
        s1:=n1;
        s2:=n2;
       end;
     end;
    //find EOL
    while (i<=l) and (s[i]<>#13) and (s[i]<>#10) do inc(i);
    if (i<l) and (s[i]=#13) and (s[i+1]=#10) then inc(i);
    inc(i);
   end;
  //process final query
  if r1=0 then
    SetLength(FQueries,q)
  else
   begin
    //skip trailing whitespace
    dec(i);
    while (i<>0) and (s[i]<' ') do dec(i);
    if q=ql then
     begin
      inc(ql);//,$100);
      SetLength(FQueries,ql);
     end;
    FQueries[q].ID:=Copy(s,s1,s2-s1);
    FQueries[q].SQL:=Copy(s,r1,i-r1+1);
    inc(q);
    SetLength(FQueries,q);
   end;
end;

procedure CmdParameters(Cmd:Command;const Values:array of Variant);
var
  i:integer;
  vt:TVarType;
begin
  cmd.Set_ActiveConnection(Session.Connection);
  for i:=0 to Length(Values)-1 do
   begin
    vt:=VarType(Values[i]);
    if (vt=varNull) or (vt=varString) or (vt=varOleStr) then
      cmd.Parameters.Append(cmd.CreateParameter('',adVariant,adParamInput,0,Values[i]))
    else
      cmd.Parameters.Append(cmd.CreateParameter('',vt,adParamInput,0,Values[i]));
   end;
end;

function ErrInfo(const QueryName: AnsiString; const Values: array of Variant):AnsiString;
var
  i,l:integer;
begin
  l:=Length(Values);
  Result:='';
  if l>0 then
   begin
    Result:=VarToStr(Values[0]);
    for i:=1 to l-1 do Result:=Result+','+VarToStr(Values[i]);
   end;
  Result:=#13#10'"'+QueryName+'"['+Result+']';
end;

{ TQueryResult }

constructor TQueryResult.Create(const QueryName: AnsiString; const Values: array of Variant);
var
  cmd:Command;
begin
  inherited Create;
  //FRecordSet:=Session.DbCon.Execute(,v,adCmdText);
  FFirstRead:=true;
  cmd:=CoCommand.Create;
  try
    cmd.CommandType:=adCmdText;
    cmd.CommandText:=QueryStore.GetSQL(QueryName);
    CmdParameters(cmd,Values);
    FRecordset:=CoRecordset.Create;
    FRecordset.CursorLocation:=adUseClient;
    FRecordset.Open(cmd,EmptyParam,adOpenStatic,adLockReadOnly,0);
  except
    on e:Exception do
     begin
      e.Message:=e.Message+ErrInfo(QueryName,Values);
      raise;
     end;
  end;
end;

constructor TQueryResult.Create(Recordset: Recordset);
begin
  inherited Create;
  FFirstRead:=true;
  FRecordSet:=Recordset;//Clone?
end;

constructor TQueryResult.Create(const SQL: AnsiString);
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

class function TQueryResult.SingleValue(const QueryName: AnsiString; const Values: array of Variant): Variant;
var
  cmd:Command;
  rs:Recordset;
  v:OleVariant;
begin
  inherited Create;
  try
    cmd:=CoCommand.Create;
    cmd.CommandType:=adCmdText;
    cmd.CommandText:=QueryStore.GetSQL(QueryName);
    CmdParameters(cmd,Values);
    rs:=cmd.Execute(v,EmptyParam,0);
    if rs.EOF then
      raise ESingleValueFailed.Create('SingleValue('+QueryName+') did not result a value')
    else
     begin
      Result:=rs.Fields[0].Value;
      rs.MoveNext;
      if not rs.EOF then
        raise ESingleValueFailed.Create('SingleValue('+QueryName+') resulted in more than one value')
     end;
  except
    on e:Exception do
     begin
      e.Message:=e.Message+ErrInfo(QueryName,Values);
      raise;
     end;
  end;
end;

class function TQueryResult.SingleValue(const SQL: AnsiString): Variant;
var
  rs:Recordset;
begin
  rs:=CoRecordset.Create;
  rs.Open(
    SQL,
    Session.Connection,
    adOpenStatic,//? adOpenForwardOnly,//?
    adLockReadOnly,
    adCmdText);
  if rs.EOF then
    raise ESingleValueFailed.Create('SingleValue did not result a value')
  else
   begin
    Result:=rs.Fields[0].Value;
    rs.MoveNext;
    if not rs.EOF then
      raise ESingleValueFailed.Create('SingleValue resulted in more than one value')
   end;
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
        raise EFieldNotFound.Create('IsNull: Field not found: '+VarToStr(Idx))
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

procedure TQueryResult.CheckResultSet;
var
  v:OleVariant;
begin
  while (FRecordSet<>nil) and (FRecordSet.State=adStateClosed) do FRecordSet:=FRecordSet.NextRecordset(v);
  FFirstRead:=true;
end;

{ TDataChanger }

constructor TDataChanger.Create(const TableName, PKName: AnsiString; const Id: Variant);
var
  sql:string;
  idX:int64;
begin
  inherited Create;
  FRecordSet:=CoRecordset.Create;
  //TODO: adCmdTable and find PK? first col?
  if VarIsNumeric(Id) then
   begin
    idX:=Id;
    sql:='SELECT * FROM '+TableName+' WHERE '+PKName+'='+IntToStr(idX);
   end
  else
    sql:='SELECT * FROM '+TableName+' WHERE '+PKName+'='''+StringReplace(VarToStr(Id),'''','''''',[rfReplaceAll])+'''';
  FRecordSet.Open(sql,
    Session.Connection,
    adOpenKeyset,//?
    adLockOptimistic,//adLockPessimistic?
    adCmdText);
  if VarIsNull(Id) or (Id=0) then FRecordSet.AddNew(EmptyParam,EmptyParam);
  //else editmode?
end;

destructor TDataChanger.Destroy;
begin
  //FRecordSet.Close;
  FRecordSet:=nil;
  inherited;
end;

function TDataChanger.CheckNew: boolean;
begin
  if FRecordSet.EOF then
   begin
    FRecordSet.AddNew(EmptyParam,EmptyParam);
    Result:=true;
   end
  else
    Result:=false;
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

class function TDataChanger.Execute(const QueryName: AnsiString; const Values: array of Variant): integer;
var
  cmd:Command;
  v:OleVariant;
begin
  try
    cmd:=CoCommand.Create;
    cmd.CommandType:=adCmdText;
    cmd.CommandText:=QueryStore.GetSQL(QueryName);
    CmdParameters(cmd,Values);
    cmd.Execute(v,EmptyParam,0);//rs:=
    //while (rs<>nil) and (rs.State=adStateClosed) do rs:=rs.NextRecordset(v);
    Result:=v;
  except
    on e:Exception do
     begin
      e.Message:=e.Message+ErrInfo(QueryName,Values);
      raise;
     end;
  end;
end;

initialization
  //QueryStore created as needed (not in CoInit'ed thread here)
finalization
  FreeAndNil(QueryStore);

end.
