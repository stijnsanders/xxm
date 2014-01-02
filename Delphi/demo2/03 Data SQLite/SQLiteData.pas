//////////////////////////////////////////////////
//  TSQLite                                     //
//    Delphi SQLite3 wrapper                    //
//                                              //
//  https://github.com/stijnsanders/TSQLite     //
//////////////////////////////////////////////////

unit SQLiteData;

interface

uses SysUtils, SQLite;

type
  TSQLiteConnection=class(TObject)
  private
    FHandle:HSQLiteDB;
    function GetLastInsertRowID:int64;
  public
    constructor Create(FileName:UTF8String);
    constructor CreateReadOnly(FileName:UTF8String);
    destructor Destroy; override;
    procedure Execute(SQL:UTF8String); overload;
    function Execute(SQL:UTF8String;const Parameters:array of OleVariant):boolean; overload;
    procedure Execute(SQL:UTF8String;const Parameters:array of OleVariant;var LastInsertRowID:integer); overload;
    function Exists(SQL:UTF8String):boolean; overload;
    function Exists(SQL:UTF8String;const Parameters:array of OleVariant):boolean; overload;
    property Handle:HSQLiteDB read FHandle;
    property LastInsertRowID:int64 read GetLastInsertRowID;
  end;

  TSQLiteStatement=class(TObject)
  private
    FDB:HSQLiteDB;
    FHandle:HSQLiteStatement;
    FOutOfData,FFirstRead,FFirstStep,FGotColumnNames,FGotParamNames:boolean;
    FColumnCount:integer;
    FColumnNames,FParamNames:array of WideString;
    function GetField(Idx:OleVariant):OleVariant;
    function GetFieldName(Idx:integer):WideString;
    function GetColumnIdx(Idx:OleVariant):integer;
    procedure GetColumnNames;
    procedure GetParamNames;
    function GetParameter(Idx: OleVariant): OleVariant;
    procedure SetParameter(Idx: OleVariant; Value: OleVariant);
    function GetParameterCount: integer;
    function GetParameterName(Idx: integer): WideString;
    function GetEOF: boolean;
    procedure DoInit;
    procedure DoStep;
  public
    constructor Create(Connection:TSQLiteConnection;SQL:UTF8String); overload;
    constructor Create(Connection:TSQLiteConnection;SQL:UTF8String;var NextIndex:integer); overload;
    constructor Create(Connection:TSQLiteConnection;SQL:UTF8String;const Parameters:array of OleVariant); overload;
    destructor Destroy; override;
    procedure ExecSQL;
    function Read:boolean;//Next?
    procedure Reset;
    property Handle:HSQLiteStatement read FHandle;
    property Field[Idx:OleVariant]:OleVariant read GetField; default;
    property FieldName[Idx:integer]:WideString read GetFieldName;
    property FieldCount:integer read FColumnCount;
    property Parameter[Idx:OleVariant]:OleVariant read GetParameter write SetParameter;
    property ParameterName[Idx:integer]:WideString read GetParameterName;
    property ParameterCount:integer read GetParameterCount;
    property Eof:boolean read GetEOF;
    function GetInt(Idx:OleVariant):integer;
    function GetStr(Idx:OleVariant):WideString;
    function GetDate(Idx:OleVariant):TDateTime;
	  function GetDefault(Idx,Default:OleVariant):OleVariant;
    function IsNull(Idx:OleVariant):boolean;
  end;

  ESQLiteDataException=class(Exception);
  
function VNow:OleVariant;

implementation

uses Variants;

function VNow:OleVariant;
begin
  Result:=VarFromDateTime(Now);
end;

{ TSQLiteConnection }

constructor TSQLiteConnection.Create(FileName: UTF8String);
begin
  inherited Create;
  sqlite3_check(sqlite3_open(PAnsiChar(FileName),FHandle));
end;

constructor TSQLiteConnection.CreateReadOnly(FileName: UTF8String);
begin
  inherited Create;
  sqlite3_check(sqlite3_open_v2(PAnsiChar(FileName),FHandle,SQLITE_OPEN_READONLY,nil));
end;

destructor TSQLiteConnection.Destroy;
begin
  {sqlite3_check}(sqlite3_close(FHandle));
  inherited;
end;

procedure TSQLiteConnection.Execute(SQL: UTF8String);
var
  e:PAnsiChar;
  s:string;
begin
  sqlite3_exec(FHandle,PAnsiChar(SQL),nil,nil,e);
  if e<>nil then
   begin
    s:=Utf8ToAnsi(e);
    sqlite3_free(e);
    raise ESQLiteDataException.Create(s);//TODO: prefix?
   end;
end;

function TSQLiteConnection.Execute(SQL: UTF8String;
  const Parameters: array of OleVariant):boolean;
var
  st:TSQLiteStatement;
begin
  st:=TSQLiteStatement.Create(Self,SQL,Parameters);
  try
    //TODO: next statement!!!
    Result:=st.Read;
  finally
    st.Free;
  end;
end;

procedure TSQLiteConnection.Execute(SQL: UTF8String;
  const Parameters: array of OleVariant; var LastInsertRowID: integer);
var
  st:TSQLiteStatement;
begin
  st:=TSQLiteStatement.Create(Self,SQL,Parameters);
  try
    //TODO: next statement!!!
    st.Read;
  finally
    st.Free;
  end;
  LastInsertRowID:=sqlite3_last_insert_rowid(FHandle);
end;

function TSQLiteConnection.Exists(SQL: UTF8String):boolean;
var
  h:HSQLiteStatement;
  r:integer;
begin
  sqlite3_check(FHandle,sqlite3_prepare_v2(FHandle,
    PAnsiChar(SQL),Length(SQL),h,PAnsiChar(nil^)));
  //TODO: tail!
  try
    r:=sqlite3_step(h);
    case r of
      //SQLITE_BUSY://TODO: wait a little and retry?
      SQLITE_DONE:Result:=false;
      SQLITE_ROW:Result:=true;
      //SQLITE_ERROR
      //SQLITE_MISUSE
      else
       begin
        sqlite3_check(FHandle,r);
        Result:=false;//counter warning
       end;
    end;
  finally
    {sqlite3_check}(sqlite3_finalize(h));
  end;
end;

function TSQLiteConnection.Exists(SQL: UTF8String;
  const Parameters: array of OleVariant):boolean;
var
  st:TSQLiteStatement;
begin
  st:=TSQLiteStatement.Create(Self,SQL,Parameters);
  try
    Result:=not st.Eof;
  finally
    st.Free;
  end;
end;

function TSQLiteConnection.GetLastInsertRowID: int64;
begin
  Result:=sqlite3_last_insert_rowid(FHandle);
end;

{ TSQLiteStatement }

constructor TSQLiteStatement.Create(Connection: TSQLiteConnection;
  SQL: UTF8String);
begin
  inherited Create;
  FDB:=Connection.Handle;
  sqlite3_check(FDB,sqlite3_prepare_v2(FDB,
    PAnsiChar(SQL),Length(SQL),FHandle,PAnsiChar(nil^)));
  DoInit;
end;

constructor TSQLiteStatement.Create(Connection: TSQLiteConnection;
  SQL: UTF8String; var NextIndex: integer);
var
  x,y:PAnsiChar;
begin
  inherited Create;
  FDB:=Connection.Handle;
  x:=PAnsiChar(SQL);
  sqlite3_check(FDB,sqlite3_prepare_v2(FDB,
    x,Length(x),FHandle,y));
  NextIndex:=integer(y)-integer(x);
  DoInit;
end;

constructor TSQLiteStatement.Create(Connection: TSQLiteConnection;
  SQL: UTF8String; const Parameters: array of OleVariant);
var
  i:integer;
begin
  inherited Create;
  FDB:=Connection.Handle;
  sqlite3_check(FDB,sqlite3_prepare_v2(FDB,
    PAnsiChar(SQL),Length(SQL),FHandle,PAnsiChar(nil^)));
  DoInit;
  for i:=0 to Length(Parameters)-1 do SetParameter(i+1,Parameters[i]);
end;

procedure TSQLiteStatement.DoInit;
begin
  //TODO: tail!
  FGotColumnNames:=false;
  FGotParamNames:=false;
  FOutOfData:=false;
  FFirstRead:=true;
  FFirstStep:=true;
  FColumnCount:=sqlite3_column_count(FHandle);
end;

destructor TSQLiteStatement.Destroy;
begin
  {sqlite3_check}(sqlite3_finalize(FHandle));
  inherited;
end;

procedure TSQLiteStatement.DoStep;
var
  r:integer;
begin
  FFirstStep:=false;
  //if not FOutOfData then?
  r:=sqlite3_step(FHandle);
  case r of
    //SQLITE_BUSY://TODO: wait a little and retry?
    SQLITE_DONE:FOutOfData:=true;
    SQLITE_ROW:;//Result:=true;
    //SQLITE_ERROR
    //SQLITE_MISUSE
    else sqlite3_check(FDB,r);
  end;
end;

procedure TSQLiteStatement.ExecSQL;
begin
  if FFirstStep then
   begin
    DoStep;
    if not FOutOfData then
      raise ESQLiteDataException.Create('ExecSQL with unexpected data, use Read instead.');
   end
  else
    raise ESQLiteDataException.Create('Calls to both ExecSQL and Read not supported.');
end;

function TSQLiteStatement.Read: boolean;
begin
  if FFirstStep then DoStep;
  if FOutOfData then Result:=false else
    if FFirstRead then
     begin
      FFirstRead:=false;
      Result:=true;
     end
    else
     begin
      DoStep;
      Result:=not FOutOfData;
     end;
end;

procedure TSQLiteStatement.Reset;
begin
  //if FFirstStep then DoStep;?
  sqlite3_check(sqlite3_reset(FHandle));
  sqlite3_check(sqlite3_clear_bindings(FHandle));//TODO: switch?
  FGotColumnNames:=false;
  FGotParamNames:=false;
  FOutOfData:=false;
  FFirstRead:=true;
  FFirstStep:=true;
  //FColumnCount:=sqlite3_column_count(FHandle);//assert no change
end;

procedure TSQLiteStatement.GetColumnNames;
var
  i:integer;
begin
  if not FGotColumnNames then
   begin
    SetLength(FColumnNames,FColumnCount);
    for i:=0 to FColumnCount-1 do FColumnNames[i]:=sqlite3_column_name16(FHandle,i);
    FGotColumnNames:=true;
   end;
end;

function TSQLiteStatement.GetColumnIdx(Idx: OleVariant): integer;
var
  s:WideString;
begin
  if VarIsNumeric(Idx) then Result:=Idx else
   begin
    GetColumnNames;
    Result:=0;
    s:=VarToWideStr(Idx);
    while (Result<FColumnCount) and (WideCompareText(s,FColumnNames[Result])<>0) do inc(Result);
   end;
  if (Result<0) or (Result>=FColumnCount) then
    raise ESQLiteDataException.Create('Invalid column index "'+VarToStr(Idx)+'"');
end;

function TSQLiteStatement.GetField(Idx: OleVariant): OleVariant;
var
  i,l:integer;
  p:pointer;
begin
  if FFirstStep then DoStep;
  i:=GetColumnIdx(Idx);
  //TODO: use HSQLiteValue?
  case sqlite3_column_type(FHandle,i) of
    SQLITE_INTEGER:Result:=sqlite3_column_int(FHandle,i);
    SQLITE_FLOAT:Result:=sqlite3_column_double(FHandle,i);
    SQLITE_TEXT:Result:=WideString(sqlite3_column_text16(FHandle,i));
    SQLITE_BLOB:
     begin
      l:=sqlite3_column_bytes(FHandle,i);
      if l=0 then Result:=Null else
       begin
        Result:=VarArrayCreate([0,l-1],varByte);
        p:=VarArrayLock(Result);
        try
          Move(sqlite3_column_blob(FHandle,i)^,p^,l);
        finally
          VarArrayUnlock(Result);
        end;
       end;
     end;
    SQLITE_NULL:Result:=Null;
    //TODO: detect rowid alias column (primary keu)
    else
      Result:=EmptyParam;//??
  end;
end;

function TSQLiteStatement.GetFieldName(Idx: integer): WideString;
begin
  GetColumnNames;
  if (Idx<0) or (Idx>=Length(FColumnNames)) then
    raise ESQLiteDataException.Create('Invalid column index "'+IntToStr(Idx)+'"');
  Result:=FColumnNames[Idx];
end;

{$IF not Declared(UTF8ToWideString)}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$IFEND}

procedure TSQLiteStatement.GetParamNames;
var
  i,l:integer;
begin
  if not FGotParamNames then
   begin
    l:=sqlite3_bind_parameter_count(FHandle);
    SetLength(FParamNames,l);
    for i:=0 to l-1 do FParamNames[i]:=UTF8ToWideString(sqlite3_bind_parameter_name(FHandle,i+1));
    FGotParamNames:=true;
   end;
end;

function TSQLiteStatement.GetParameter(Idx: OleVariant): OleVariant;
begin
  raise ESQLiteDataException.Create('Get parameter value not supported');
end;

procedure TSQLiteStatement.SetParameter(Idx: OleVariant; Value: OleVariant);
var
  i,j,l:integer;
  s:WideString;
  vt:TVarType;
  p:pointer;
const
  BoolInt:array[boolean] of integer=(0,1);  
begin
  l:=sqlite3_bind_parameter_count(FHandle);
  if VarIsNumeric(Idx) then i:=Idx else
   begin
    GetParamNames;
    i:=0;
    s:=VarToWideStr(Idx);
    while (i<l) and (WideCompareText(s,FParamNames[i])<>0) do inc(i);
    inc(i);
   end;
  if (i<1) or (i>l) then
    //raise ESQLiteDataException.Create('Invalid parameter index "'+VarToStr(Idx)+'"')
  else
   begin
    vt:=VarType(Value);
    if (vt and varArray)<>0 then
      case vt and varTypeMask of
        //TODO: support other array element types!
        varByte:
         begin
          l:=1;
          for j:=1 to VarArrayDimCount(Value) do
            l:=l*(VarArrayHighBound(Value,j)-VarArrayLowBound(Value,j));
          p:=VarArrayLock(Value);
          try
            sqlite3_bind_blob(FHandle,i,p^,l,nil);
          finally
            VarArrayUnlock(Value);
          end;
         end;
        else raise ESQLiteDataException.Create('Unsupported variant array type');
      end
    else
      case vt and varTypeMask of
        varNull:
          sqlite3_bind_null(FHandle,i);
        varSmallint,varInteger,varShortInt,varByte,varWord,varLongWord:
          sqlite3_bind_int(FHandle,i,Value);
        varInt64:
          sqlite3_bind_int64(FHandle,i,Value);
        varSingle,varDouble,varDate:
          sqlite3_bind_double(FHandle,i,Value);
        varBoolean:
          sqlite3_bind_int(FHandle,i,BoolInt[boolean(Value)]);
        varOleStr:
         begin
          s:=VarToWideStr(Value);
          sqlite3_bind_text16(FHandle,i,PWideChar(s),Length(s)*2,nil);
         end;
        //varVariant?
        //varUnknown IPersist? IStream?
        else raise ESQLiteDataException.Create('Unsupported variant type');
      end;
	end;
end;

function TSQLiteStatement.GetParameterCount: integer;
begin
  Result:=sqlite3_bind_parameter_count(FHandle);
end;

function TSQLiteStatement.GetParameterName(Idx: integer): WideString;
begin
  GetParamNames;
  if (Idx<1) or (Idx>Length(FParamNames)) then
    raise ESQLiteDataException.Create('Invalid parameter index "'+IntToStr(Idx)+'"');
  Result:=FParamNames[Idx-1];
end;

function TSQLiteStatement.GetDate(Idx: OleVariant): TDateTime;
begin
  if FFirstStep then DoStep;
  Result:=sqlite3_column_double(FHandle,GetColumnIdx(Idx));
end;

function TSQLiteStatement.GetInt(Idx: OleVariant): integer;
begin
  if FFirstStep then DoStep;
  Result:=sqlite3_column_int(FHandle,GetColumnIdx(Idx));
end;

function TSQLiteStatement.GetStr(Idx: OleVariant): WideString;
begin
  if FFirstStep then DoStep;
  Result:=WideString(sqlite3_column_text16(FHandle,GetColumnIdx(Idx)));
end;

function TSQLiteStatement.GetDefault(Idx,Default:OleVariant):OleVariant;
begin
  if FFirstStep then DoStep;
  if sqlite3_column_type(FHandle,GetColumnIdx(Idx))=SQLITE_NULL then
    Result:=Default
  else
    Result:=GetField(Idx);
end;

function TSQLiteStatement.IsNull(Idx: OleVariant): boolean;
begin
  if FFirstStep then DoStep;
  Result:=sqlite3_column_type(FHandle,GetColumnIdx(Idx))=SQLITE_NULL;
end;

function TSQLiteStatement.GetEOF: boolean;
begin
  if FFirstStep then DoStep;
  Result:=FOutOfData;
end;

end.
