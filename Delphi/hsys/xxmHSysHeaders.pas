unit xxmHSysHeaders;

interface

uses
  xxmHeaders, httpapi1, xxmParUtils;

type
  TxxmHeaderGet=function(Name: WideString): WideString of object;
  TxxmHeaderSet=procedure(Name, Value: WideString) of object;
  TxxmHeaderGetCount=function:integer of object;
  TxxmHeaderGetIndex=function(Idx: integer): WideString of object;
  TxxmHeaderSetIndex=procedure(Idx: integer; Value: WideString) of object;

  TxxmHSysResponseHeaders=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    FGet:TxxmHeaderGet;
    FSet:TxxmHeaderSet;
    FCount:TxxmHeaderGetCount;
    FGetName,FGetIndex:TxxmHeaderGetIndex;
    FSetIndex:TxxmHeaderSetIndex;
  protected
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
    function Complex(Name: OleVariant; out Items: IxxmDictionary): WideString;
  public
    constructor Create(xGet:TxxmHeaderGet;xSet:TxxmHeaderSet;xCount:TxxmHeaderGetCount;
      xGetName,xGetIndex:TxxmHeaderGetIndex;xSetIndex:TxxmHeaderSetIndex);
  end;

  TxxmHSysResponseSubHeader=class(TInterfacedObject, IxxmDictionary)
  private
    FPar:OleVariant;
    FValue:AnsiString;
    FPars:TParamIndexes;
    FGet:TxxmHeaderGet;
    FSet:TxxmHeaderSet;
    FGetName,FGetIndex:TxxmHeaderGetIndex;
    FSetIndex:TxxmHeaderSetIndex;
    function ParseValue: WideString;
  protected
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
  public
    constructor Create(xPar:OleVariant;xGet:TxxmHeaderGet;xSet:TxxmHeaderSet;
      xGetName,xGetIndex:TxxmHeaderGetIndex;xSetIndex:TxxmHeaderSetIndex;
      var xValue:WideString);
  end;

const
  HttpRequestHeaderName:array[THTTP_HEADER_ID] of AnsiString=(
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-MD5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept',
    'Accept-Charset',
    'Accept-Encoding',
    'Accept-Language',
    'Authorization',
    'Cookie',
    'Expect',
    'From',
    'Host',
    'If-Match',
    'If-Modified-Since',
    'If-None-Match',
    'If-Range',
    'If-Unmodified-Since',
    'Max-Forwards',
    'Proxy-Authorization',
    'Referer',
    'Range',
    'TE',
    'Translate',
    'User-Agent');

const
  HttpResponseHeaderName:array[THTTP_HEADER_ID] of AnsiString=(
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-MD5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept-Ranges',
    'Age',
    'ETag',
    'Location',
    'Proxy-Authenticate',
    'Retry-After',
    'Server',
    'Set-Cookie',
    'Vary',
    'WWW-Authenticate',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '');

implementation

uses Variants, SysUtils;

{ TxxmHSysResponseHeaders }

constructor TxxmHSysResponseHeaders.Create(xGet:TxxmHeaderGet;xSet:TxxmHeaderSet;
  xCount:TxxmHeaderGetCount;xGetName,xGetIndex:TxxmHeaderGetIndex;xSetIndex:TxxmHeaderSetIndex);
begin
  inherited Create;
  FGet:=xGet;
  FSet:=xSet;
  FCount:=xCount;
  FGetName:=xGetName;
  FGetIndex:=xGetIndex;
  FSetIndex:=xSetIndex;
end;

function TxxmHSysResponseHeaders.GetCount: integer;
begin
  Result:=FCount;
end;

function TxxmHSysResponseHeaders.GetItem(Name: OleVariant): WideString;
begin
  if VarIsNumeric(Name) then Result:=FGetIndex(Name) else Result:=FGet(Name);
end;

function TxxmHSysResponseHeaders.GetName(Idx: integer): WideString;
begin
  Result:=FGetName(Idx);
end;

function TxxmHSysResponseHeaders.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
begin
  Items:=TxxmHSysResponseSubHeader.Create(Name,FGet,FSet,FGetName,FGetIndex,FSetIndex,Result);
end;

procedure TxxmHSysResponseHeaders.SetItem(Name: OleVariant;
  const Value: WideString);
begin
  if VarIsNumeric(Name) then FSetIndex(Name,Value) else FSet(Name,Value);
end;

procedure TxxmHSysResponseHeaders.SetName(Idx: integer; Value: WideString);
begin
  raise Exception.Create('TxxmHSysResponseHeaders.SetName not supported');
end;

{ TxxmHSysResponseSubHeader }

constructor TxxmHSysResponseSubHeader.Create(xPar: OleVariant; xGet: TxxmHeaderGet;
  xSet: TxxmHeaderSet; xGetName, xGetIndex: TxxmHeaderGetIndex; xSetIndex: TxxmHeaderSetIndex;
  var xValue: WideString);
begin
  inherited Create;
  FPar:=xPar;
  FGet:=xGet;
  FSet:=xSet;
  FGetName:=xGetName;
  FGetIndex:=xGetIndex;
  FSetIndex:=xSetIndex;
  xValue:=ParseValue;
end;

function TxxmHSysResponseSubHeader.ParseValue: WideString;
begin
  //TODO: detect change, skip when no change?
  if VarIsNumeric(FPar) then FValue:=FGetIndex(FPar) else FValue:=FGet(FPar);
  Result:=SplitHeaderValue(FValue,1,Length(FValue),FPars);
end;

function TxxmHSysResponseSubHeader.GetCount: integer;
begin
  ParseValue;
  Result:=Length(FPars);
end;

function TxxmHSysResponseSubHeader.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  ParseValue;
  if VarIsNumeric(Name) then
    i:=Name
  else
   begin
    i:=0;
    while (i<Length(FPars)) and (CompareText(Name,Copy(FValue,FPars[i].NameStart,FPars[i].NameLength))<>0) do inc(i);
   end;
  if (i<0) or (i>=Length(FPars)) then Result:='' else Result:=Copy(FValue,FPars[i].ValueStart,FPars[i].ValueLength);
end;

function TxxmHSysResponseSubHeader.GetName(Idx: integer): WideString;
begin
  ParseValue;
  Result:=Copy(FValue,FPars[Idx].NameStart,FPars[Idx].NameLength);
end;

procedure TxxmHSysResponseSubHeader.SetItem(Name: OleVariant;
  const Value: WideString);
var
  i,j:integer;
  x:WideString;
begin
  ParseValue;
  if VarIsNumeric(Name) then
    i:=Name
  else
   begin
    i:=0;
    while (i<Length(FPars)) and (CompareText(Name,Copy(FValue,FPars[i].NameStart,FPars[i].NameLength))<>0) do inc(i);
   end;
  j:=FPars[i].ValueStart+FPars[i].ValueLength;
  x:=Copy(FValue,1,FPars[i].ValueStart-1)+Value+Copy(FValue,j,Length(FValue)-j+1);
  if VarIsNumeric(FPar) then FSetIndex(FPar,x) else FSet(FPar,x);
end;

procedure TxxmHSysResponseSubHeader.SetName(Idx: integer;
  Value: WideString);
var
  j:integer;
  x:WideString;
begin
  ParseValue;
  j:=FPars[Idx].NameStart+FPars[Idx].NameLength;
  x:=Copy(FValue,1,FPars[Idx].NameStart-1)+Value+Copy(FValue,j,Length(FValue)-j+1);
  if VarIsNumeric(FPar) then FSetIndex(FPar,x) else FSet(FPar,x);
end;

end.
