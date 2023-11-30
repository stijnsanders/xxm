unit xxmHSysHeaders;

interface

uses
  xxmHeaders,
  {$IFDEF HSYS1}httpapi1,{$ENDIF}
  {$IFDEF HSYS2}httpapi2,{$ENDIF}
  xxmParUtils;

type
  TxxmHeaderGet=function(const Name: WideString): WideString of object;
  TxxmHeaderSet=procedure(const Name, Value: WideString) of object;
  TxxmHeaderGetCount=function:integer of object;
  TxxmHeaderGetIndex=function(Idx: integer): WideString of object;
  TxxmHeaderSetIndex=procedure(Idx: integer; const Value: WideString) of object;

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
  HttpResponseHeaderName:array[THTTP_HEADER_ID] of WideString=(
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

uses Variants, SysUtils, xxmCommonUtils;

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
  Items:=TxxmHSysResponseSubHeader.Create(Name,
   FGet,FSet,FGetName,FGetIndex,FSetIndex,Result);
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
  FPars.ParsIndex:=0;
  FPars.ParsSize:=0;
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
  if VarIsNumeric(FPar) then
    FValue:=AnsiString(FGetIndex(FPar))
  else
    FValue:=AnsiString(FGet(FPar));
  Result:=WideString(SplitHeaderValue(FValue,1,Length(FValue),FPars));
end;

function TxxmHSysResponseSubHeader.GetCount: integer;
begin
  ParseValue;
  Result:=FPars.ParsIndex;
end;

function TxxmHSysResponseSubHeader.GetItem(Name: OleVariant): WideString;
var
  i:integer;
  n:string;
begin
  ParseValue;
  if VarIsNumeric(Name) then
    i:=Name
  else
   begin
    i:=0;
    n:=LowerCase(VarToStr(Name));
    while (i<FPars.ParsIndex) and (RawCompare(FPars.Pars[i].NameL,n)<>0) do inc(i);
   end;
  if (i<0) or (i>=FPars.ParsIndex) then
    Result:=''
  else
    Result:=WideString(Copy(FValue,FPars.Pars[i].ValueStart,FPars.Pars[i].ValueLength));
end;

function TxxmHSysResponseSubHeader.GetName(Idx: integer): WideString;
begin
  ParseValue;
  if (Idx>=0) and (Idx<FPars.ParsIndex) then
    Result:=WideString(Copy(FValue,FPars.Pars[Idx].NameStart,FPars.Pars[Idx].NameLength))
  else
    raise ERangeError.Create('TxxmHSysResponseSubHeader.GetName: Out of range');
end;

procedure TxxmHSysResponseSubHeader.SetItem(Name: OleVariant; const Value: WideString);
var
  i,j:integer;
  n:string;
  x:WideString;
begin
  ParseValue;
  if VarIsNumeric(Name) then
    i:=Name
  else
   begin
    i:=0;
    n:=LowerCase(VarToStr(Name));
    while (i<FPars.ParsIndex) and (RawCompare(FPars.Pars[i].NameL,n)<>0) do inc(i);
   end;
  if (i>=0) and (i<FPars.ParsIndex) then
   begin
    j:=FPars.Pars[i].ValueStart+FPars.Pars[i].ValueLength;
    x:=WideString(Copy(FValue,1,FPars.Pars[i].ValueStart-1))+
      Value+WideString(Copy(FValue,j,Length(FValue)-j+1));
   end
  else
    x:=WideString(FValue)+'; '+VarToWideStr(Name)+'='+Value;
  if VarIsNumeric(FPar) then FSetIndex(FPar,x) else FSet(FPar,x);
end;

procedure TxxmHSysResponseSubHeader.SetName(Idx: integer; Value: WideString);
var
  j:integer;
  x:WideString;
begin
  ParseValue;
  if (Idx>=0) and (Idx<FPars.ParsIndex) then
   begin
    j:=FPars.Pars[Idx].NameStart+FPars.Pars[Idx].NameLength;
    x:=WideString(Copy(FValue,1,FPars.Pars[Idx].NameStart-1))+
      Value+WideString(Copy(FValue,j,Length(FValue)-j+1));
    if VarIsNumeric(FPar) then FSetIndex(FPar,x) else FSet(FPar,x);
   end
  else
    raise ERangeError.Create('TxxmHSysResponseSubHeader.SetName: Out of range');
end;

end.
