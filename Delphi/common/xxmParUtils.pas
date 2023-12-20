unit xxmParUtils;

interface

uses SysUtils, Classes, ActiveX, xxmHeaders;

{$D-}
{$L-}

type
  {
  TControlledLifeTimeObject
  serves as an alternative to TInterfacedObject,
  but counts on an owner-object to call constructor and destructor
  and govern the external use of the object, so it's no longer in use
  when destroying. This way _AddRef and _Release no longer
  need to call interlocked operations, optionally improving performance
  on multi-code systems under load.
  }
  TControlledLifeTimeObject=class(TObject, IInterface)
  protected
    function QueryInterface(const IID:TGUID; out Obj):HResult; stdcall;
    function _AddRef:integer; stdcall;
    function _Release:integer; stdcall;
  end;

  TParamIndexes=record
    Pars:array of record
      ValueStart,ValueLength,//SortIndex,
      NameStart,NameLength,NameIndex:integer;
    end;
    ParsIndex,ParsSize:integer;
  end;

  TKnownHeaderName=(khContentType,khContentLength,khContentDisposition,
    khConnection,khTransferEncoding,khCacheControl,
    khAccept,khAuthorization,khWWWAuthenticate,khUpgrade,
    khServer,khHost,khUserAgent,khReferer,khLocation,
    khAcceptEncoding,khAcceptLanguage,
    khIfModifiedSince,khLastModified,khExpires,
    khCookie,khSetCookie);


  TRequestHeaders=class(TControlledLifeTimeObject,
    IxxmDictionary, IxxmDictionaryEx)
  private
    FIdx:TParamIndexes;
    function GetItem(Name:OleVariant):WideString;
    procedure SetItem(Name:OleVariant;const Value:WideString);
    function GetName(Idx:integer): WideString;
    procedure SetName(Idx:integer;Value:WideString);
    function GetCount:integer;
  public
    Data:AnsiString;
    constructor Create;
    destructor Destroy; override;
    procedure Load(StartIndex,DataLength:integer);
    procedure Reset;
    property Item[Name: OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx: integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
    function Complex(Name:OleVariant;out Items:IxxmDictionary):WideString;
    function KnownHeader(Header:TKnownHeaderName):AnsiString;
  end;

  TRequestSubValues=class(TInterfacedObject, IxxmDictionary)
  private
    FData:AnsiString;
    FIdx:TParamIndexes;
    function GetItem(Name:OleVariant):WideString;
    procedure SetItem(Name:OleVariant;const Value:WideString);
    function GetName(Idx:integer):WideString;
    procedure SetName(Idx:integer; Value:WideString);
    function GetCount:integer;
  public
    constructor Create(const Data:AnsiString;ValueStart,ValueLength:integer;
      var FirstValue:WideString);
    destructor Destroy; override;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx:integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
  end;

  TResponseSubValues=class;//forward

  TResponseHeaders=class(TControlledLifeTimeObject,
    IxxmDictionary, IxxmDictionaryEx)
  private
    FItems:array of record
      Name,Value:WideString;
      NameIndex:integer;
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
    procedure Reset;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx:integer]:WideString read GetName write SetName;
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
      NameIndex:integer;
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
    procedure Reset;
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
    function Ensure(EnsureSize: integer;
      const FieldName, FileName: AnsiString): boolean;
    procedure Flush;
    procedure SkipWhiteSpace;
  public
    constructor Create(Source: TStream; DataProgressAgent,
      FileProgressAgent: IxxmUploadProgressAgent; FileProgressStep: integer);
    destructor Destroy; override;
    procedure CheckBoundary(var Boundary: AnsiString);
    function GetHeader(var Params: TParamIndexes): AnsiString;
    function GetString(const Boundary: AnsiString): AnsiString;
    procedure GetData(const Boundary, FieldName, FileName: AnsiString;
      var Pos: integer; var Len: integer);
    function MultiPartDone: boolean;
  end;

  EXxmRequestHeadersReadOnly=class(Exception);
  EXxmResponseHeaderInvalidName=class(Exception);
  EXxmResponseHeaderInvalidChar=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);

const //resourcestring
  SXxmRequestHeadersReadOnly='Request headers are read-only.';
  SXxmResponseHeaderInvalidName='Response header add: invalid header name.';
  SXxmResponseHeaderInvalidChar='Response header add: value contains invalid character.';
  SXxmResponseHeaderAlreadySent='Response header has already been sent.';

function SplitHeaderValue(const Value:AnsiString;ValueStart,ValueLength:integer;
  var Params:TParamIndexes):AnsiString;
function GetParamValue(const Data:AnsiString;
  Params:TParamIndexes; const Name:WideString):AnsiString;

procedure HeaderNameNext(HeaderNameChar:AnsiChar;var HeaderNameIndex:integer);
function HeaderNameGet(const Name:WideString):integer;
function HeaderNameSet(const Name:WideString):integer;
procedure HeaderCheckValue(const Value: WideString);
function KnownHeaderIndex(KnownHeader:TKnownHeaderName):integer;

{$IF not Declared(UTF8ToWideString)}
{$DEFINE NOT_DECLARED_UTF8ToWideString}
{$IFEND}

{$IFDEF NOT_DECLARED_UTF8ToWideString}
function UTF8ToWideString(const s: UTF8String): WideString;
{$ENDIF}

implementation

uses Windows, Variants, xxmCommonUtils;

{ TControlledLifeTimeObject }

function TControlledLifeTimeObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result:=0 else Result:=E_NOINTERFACE;
end;

function TControlledLifeTimeObject._AddRef: integer;
begin
  //ignore
  Result:=1;
end;

function TControlledLifeTimeObject._Release: integer;
begin
  //ignore
  Result:=0;
end;

{  }

const
  HeaderNameMap:array[byte] of byte=(
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,48,00,49,50,51,52,53,00,54,55,00,00,20,56,00,
    10,11,12,13,14,15,16,17,18,19,53,54,00,00,00,00,//0-9
    00,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,//A-P
    36,37,38,39,40,41,42,43,44,45,46,00,00,00,00,55,//Q-Z
    00,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,//a-p
    36,37,38,39,40,41,42,43,44,45,46,00,00,00,00,00,//q-z
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00);


function SplitHeaderValue(const Value:AnsiString;
  ValueStart,ValueLength:integer;var Params:TParamIndexes):AnsiString;
var
  i,j,l,n:integer;
begin
  Params.ParsIndex:=0;
  l:=ValueStart+ValueLength-1;
  i:=ValueStart;//set to 0 to start parsing sub-values
  if i=0 then inc(l) else while (i<=l) and (Value[i]<>';') do inc(i);
  if i<=l then
   begin
    if i=0 then Result:='' else Result:=Copy(Value,ValueStart,i-ValueStart);
    while i<=l do
     begin
      if Params.ParsIndex=Params.ParsSize then
       begin
        inc(Params.ParsSize,$10);//growstep
        SetLength(Params.Pars,Params.ParsSize);
       end;
      inc(i);
      while (i<=l) and (Value[i] in [#1..#32]) do inc(i);
      Params.Pars[Params.ParsIndex].NameStart:=i;
      j:=i;
      n:=0;
      while (j<=l) and (Value[j]<>'=') and (HeaderNameMap[byte(Value[j])]<>0) do
       begin
        HeaderNameNext(Value[j],n);
        inc(j);
       end;
      Params.Pars[Params.ParsIndex].NameLength:=j-i;
      Params.Pars[Params.ParsIndex].NameIndex:=n;
      //Params.Pars[Params.ParsIndex].NameL:=LowerCase(string(Copy(Value,i,j-i)));
      //TODO: SortIndex
      i:=j+1;
      if (i<=l) and (Value[i]='"') then
       begin
        //in quotes
        inc(i);
        Params.Pars[Params.ParsIndex].ValueStart:=i;
        j:=i;
        while (j<=l) and (Value[j]<>'"') do inc(j);
        Params.Pars[Params.ParsIndex].ValueLength:=j-i;
        while (j<=l) and (Value[j]<>';') do inc(j);//ignore
       end
      else
       begin
        //not in quotes
        Params.Pars[Params.ParsIndex].ValueStart:=i;
        j:=i;
        while (j<=l) and (Value[j]<>';') do inc(j);
        Params.Pars[Params.ParsIndex].ValueLength:=j-i;
       end;
      i:=j;
      inc(Params.ParsIndex);
     end;
   end
  else
    Result:=Copy(Value,ValueStart,ValueLength);
end;

function GetParamValue(const Data:AnsiString;
  Params:TParamIndexes; const Name:WideString):AnsiString;
var
  n:integer;
  l,i:integer;
begin
  n:=HeaderNameGet(Name);
  l:=Params.ParsIndex;
  if n=0 then i:=l else i:=0;
  while (i<l) and (Params.Pars[i].NameIndex<>n) do inc(i);
  if i<l then
    Result:=Copy(Data,Params.Pars[i].ValueStart,Params.Pars[i].ValueLength)
  else
    Result:='';
end;

const
  HeaderNameNodesGrowStep=$1000;
  HeaderNamePreload:array[TKnownHeaderName] of AnsiString=(
    'Content-Type','Content-Length','Content-Disposition',
    'Connection','Transfer-Encoding','Cache-Control',
    'Accept','Authorization','WWW-Authenticate','Upgrade',
    'Server','Host','User-Agent','Referer','Location',
    'Accept-Encoding','Accept-Language',
    'If-Modified-Since','Last-Modified','Expires',
    'Cookie','Set-Cookie');

type
  THeaderNameNode=array[0..64] of integer;

var
  HeaderNameSearchTree:record
    Lock:TRTLCriticalSection;
    Nodes:array of THeaderNameNode;
    NodesIndex,NodesSize:integer;
  end;
  KnownHeaders:array[TKnownHeaderName] of integer;

procedure HeaderNameNodes_Init;
var
  si,i:integer;
  kh:TKnownHeaderName;
  n:byte;
  p:PInteger;
begin
  InitializeCriticalSection(HeaderNameSearchTree.Lock);
  HeaderNameSearchTree.NodesIndex:=1;
  HeaderNameSearchTree.NodesSize:=HeaderNameNodesGrowStep;
  SetLength(HeaderNameSearchTree.Nodes,HeaderNameNodesGrowStep);
  ZeroMemory(@HeaderNameSearchTree.Nodes[0],
    HeaderNameNodesGrowStep*SizeOf(THeaderNameNode));
  //pre-load
  for kh:=Low(TKnownHeaderName) to High(TKnownHeaderName) do
   begin
    i:=0;
    for si:=1 to Length(HeaderNamePreload[kh]) do
     begin
      n:=HeaderNameMap[byte(HeaderNamePreload[kh][si])];
      //assert n<>0
      p:=@HeaderNameSearchTree.Nodes[i][n];
      if p^=0 then
       begin
        i:=HeaderNameSearchTree.NodesIndex;
        //assert never i=HeaderNameSearchTree.NodesSize
        //  since HeaderNameNodesGrowStep is selected to fit all of
        //  HeaderNamePreload
        {
        if i=HeaderNameSearchTree.NodesSize then
         begin
          inc(HeaderNameSearchTree.NodesSize,HeaderNameNodesGrowStep);
          SetLength(HeaderNameSearchTree.Nodes,HeaderNameNodesGrowStep);
          ZeroMemory(@HeaderNameSearchTree.Nodes[i],
            HeaderNameNodesGrowStep*SizeOf(THeaderNameNode));
         end;
        }
        inc(HeaderNameSearchTree.NodesIndex);
        p^:=i;
       end
      else
        i:=p^;
     end;
    KnownHeaders[kh]:=i;
   end;
end;

procedure HeaderNameNext(HeaderNameChar:AnsiChar;var HeaderNameIndex:integer);
var
  n:byte;
  i:integer;
  p:PInteger;
begin
  n:=HeaderNameMap[byte(HeaderNameChar)];
  if n=0 then
    raise EXxmResponseHeaderInvalidChar.Create(SXxmResponseHeaderInvalidChar);

  //assert HeaderNameIndex>=0 and HeaderNameIndex<HeaderNameSearchTree.NodesIndex

  i:=HeaderNameSearchTree.Nodes[HeaderNameIndex][n];
  if i=0 then
   begin
    EnterCriticalSection(HeaderNameSearchTree.Lock);
    try
      //check again in case another thread just did the same
      p:=@HeaderNameSearchTree.Nodes[HeaderNameIndex][n];
      if p^=0 then
       begin
        i:=HeaderNameSearchTree.NodesIndex;
        if i=HeaderNameSearchTree.NodesSize then
         begin
          inc(HeaderNameSearchTree.NodesSize,HeaderNameNodesGrowStep);
          SetLength(HeaderNameSearchTree.Nodes,HeaderNameNodesGrowStep);
          ZeroMemory(@HeaderNameSearchTree.Nodes[i],
            HeaderNameNodesGrowStep*SizeOf(THeaderNameNode));
         end;
        inc(HeaderNameSearchTree.NodesIndex);
        p^:=i;
       end;
    finally
      LeaveCriticalSection(HeaderNameSearchTree.Lock);
    end;
   end;
  HeaderNameIndex:=i;
end;

function HeaderNameGet(const Name:WideString):integer;
var
  i,l:integer;
  w:word;
  n:byte;
begin
  Result:=0;
  l:=Length(Name);
  if l=0 then
    raise EXxmResponseHeaderInvalidName.Create(SXxmResponseHeaderInvalidName)
  else
   begin
    i:=1;
    repeat
      w:=word(Name[i]);
      if (w and $FF80)=0 then n:=HeaderNameMap[byte(Name[i])] else n:=0;
      inc(i);
      if n=0 then
        raise EXxmResponseHeaderInvalidChar.Create(SXxmResponseHeaderInvalidChar)//Result:=0
      else
        Result:=HeaderNameSearchTree.Nodes[Result][n];
    until (Result=0) or (i>l);
   end;
end;

function HeaderNameSet(const Name:WideString):integer;
var
  i,l:integer;
begin
  Result:=0;
  l:=Length(Name);
  if l=0 then
    raise EXxmResponseHeaderInvalidName.Create(SXxmResponseHeaderInvalidName)
  else
   begin
    i:=1;
    while i<=l do
     begin
      if (word(Name[i]) and $FF80)=0 then
        HeaderNameNext(AnsiChar(Name[i]),Result)
      else
        raise EXxmResponseHeaderInvalidChar.Create(SXxmResponseHeaderInvalidChar);
      inc(i);
     end;
   end;
end;

procedure HeaderCheckValue(const Value: WideString);
var
  i:integer;
begin
  for i:=1 to Length(Value) do
    if AnsiChar(Value[i]) in [#0,#10,#13] then //more?

end;

function KnownHeaderIndex(KnownHeader:TKnownHeaderName):integer;
begin
  Result:=KnownHeaders[KnownHeader];
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
  try
    FDataAgent:=nil;
  except
    //silent
    pointer(FDataAgent):=nil;
  end;
  try
    FFileAgent:=nil;
  except
    //silent
    pointer(FFileAgent):=nil;
  end;
  inherited;
end;

function TStreamNozzle.Ensure(EnsureSize: integer;
  const FieldName, FileName: AnsiString):boolean;
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
      if FDataAgent<>nil then
        try
          FDataAgent.ReportProgress(FieldName,FileName,Done+Size);
        except
          pointer(FDataAgent):=nil;
        end;
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
  while Ensure(1,'','') and (Data[Index] in [#0..#31]) do inc(Index);
end;

procedure TStreamNozzle.CheckBoundary(var Boundary: AnsiString);
var
  bl:integer;
begin
  bl:=Length(Boundary);
  Ensure(bl+5,'','');
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
var
  b:boolean;
  p,q,r,s,n:integer;
begin
  p:=0;
  q:=1;
  Params.ParsIndex:=0;
  s:=0;
  while Ensure(1,'','') and (q-p<>2) do //2 being Length(EOL)
   begin
    p:=q;
    b:=false;
    while Ensure(1,'','') and not(b and (Data[Index]=#10)) do
     begin
      if q>s then
       begin
        inc(s,$1000);//grow step
        SetLength(Result,s);
       end;
      Result[q]:=Data[Index];
      b:=Data[Index]=#13;
      inc(Index);
      inc(q);
     end;
    Result[q]:=Data[Index];//#10
    inc(Index);
    inc(q);

    if q-p<>2 then
     begin
      if Params.ParsIndex=Params.ParsSize then
       begin
        inc(Params.ParsSize,$10);//grow step
        SetLength(Params.Pars,Params.ParsSize);
       end;
      Params.Pars[Params.ParsIndex].NameStart:=p;
      r:=p;
      n:=0;
      while (r<=q) and (Result[r]<>':') do
       begin
        HeaderNameNext(Result[r],n);
        inc(r);
       end;
      Params.Pars[Params.ParsIndex].NameLength:=r-p;
      Params.Pars[Params.ParsIndex].NameIndex:=n;
      //Params.Pars[Params.ParsIndex].NameL:=LowerCase(string(Copy(Result,p,r-p)));
      inc(r);
      while (r<=q) and (Result[r] in [#1..#32]) do inc(r);
      Params.Pars[Params.ParsIndex].ValueStart:=r;
      Params.Pars[Params.ParsIndex].ValueLength:=q-r-2;//2 from Length(EOL)
      inc(Params.ParsIndex);
     end;
   end;
  SetLength(Result,q-1);
  Flush;
end;

function TStreamNozzle.GetString(const Boundary: AnsiString): AnsiString;
var
  l,p,q:integer;
begin
  l:=Length(Boundary);
  p:=0;
  q:=Index;
  while Ensure(l,'','') and (p<>l) do
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

procedure TStreamNozzle.GetData(const Boundary, FieldName, FileName: AnsiString;
  var Pos: integer; var Len: integer);
var
  l,p,x,s:integer;
begin
  Pos:=Done+Index-1;
  l:=Length(Boundary);
  p:=0;
  if (ReportStep=0) or (FFileAgent=nil) then
   begin
    //short loop
    while Ensure(l,FieldName,FileName) and (p<>l) do
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
    while Ensure(l,FieldName,FileName) and (p<>l) do
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
          if FFileAgent<>nil then
            try
              FFileAgent.ReportProgress(FieldName,FileName,s);
            except
              pointer(FFileAgent):=nil;
            end;
          x:=ReportStep;
         end;
       end;
     end;
    if FFileAgent<>nil then
      try
        FFileAgent.ReportProgress(FieldName,FileName,s);
      except
        pointer(FFileAgent):=nil;
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
  Result:=not(Ensure(2,'','')) or ((Data[Index]='-') and (Data[Index+1]='-'));
end;

{ TRequestHeaders }

constructor TRequestHeaders.Create;
begin
  inherited Create;
  Data:='';
  FIdx.ParsIndex:=0;
  FIdx.ParsSize:=0;
end;

destructor TRequestHeaders.Destroy;
begin
  Reset;
  inherited;
end;

procedure TRequestHeaders.Load(StartIndex,DataLength:integer);
var
  b:boolean;
  p,q,l,r,n:integer;
begin
  //caller load data into public Data:AnsiString;
  q:=StartIndex;//assert >0
  FIdx.ParsIndex:=0;
  //assert Length(FData)>=DataLength;
  l:=DataLength;
  while (q<=l) do
   begin
    p:=q;
    b:=false;
    while (q<=l) and not(b and (Data[q]=#10)) do
     begin
      b:=Data[q]=#13;
      inc(q);
     end;
    inc(q);
    if q-p=2 then
     begin
      //<CR><LF><CR><LF> ends header (except <CR><LF> at StartIndex)
      if p<>StartIndex then q:=l+1;
     end
    else
     begin
      r:=p;
      while (r<=q) and (Data[r] in [#1..#32]) do inc(r);
      if r=p then
       begin
        if FIdx.ParsIndex=FIdx.ParsSize then
         begin
          inc(FIdx.ParsSize,$10);//grow
          SetLength(FIdx.Pars,FIdx.ParsSize);
         end;
        FIdx.Pars[FIdx.ParsIndex].NameStart:=p;
        r:=p;
        n:=0;
        while (r<=q) and (Data[r]<>':') do
         begin
          HeaderNameNext(Data[r],n);
          inc(r);
         end;
        FIdx.Pars[FIdx.ParsIndex].NameLength:=r-p;
        FIdx.Pars[FIdx.ParsIndex].NameIndex:=n;
        //FIdx.Pars[FIdx.ParsIndex].NameL:=LowerCase(string(Copy(FData,p,r-p)));
        inc(r);//':'
        while (r<=q) and (Data[r] in [#1..#32]) do inc(r);
        FIdx.Pars[FIdx.ParsIndex].ValueStart:=r;
        FIdx.Pars[FIdx.ParsIndex].ValueLength:=q-r-2;//2 from Length(EOL)
        inc(FIdx.ParsIndex);
       end
      else
       begin
        //assert i<>0
        FIdx.Pars[FIdx.ParsIndex].ValueLength:=q-FIdx.Pars[FIdx.ParsIndex].ValueStart-2;
        //TODO: kill EOF and whitespace?
       end;
     end;
   end;
end;

procedure TRequestHeaders.Reset;
begin
  FIdx.ParsIndex:=0;
  //re-use Data, see Load
end;

function TRequestHeaders.GetCount: integer;
begin
  Result:=FIdx.ParsIndex;
end;

function TRequestHeaders.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  if VarIsNumeric(Name) then
   begin
    i:=integer(Name);
    if (i>=0) and (i<FIdx.ParsIndex) then
      Result:=WideString(Copy(Data,FIdx.Pars[i].ValueStart,FIdx.Pars[i].ValueLength))
    else
      raise ERangeError.Create('TRequestHeaders.GetItem: Out of range');
   end
  else
    Result:=WideString(GetParamValue(Data,FIdx,Name));
end;

function TRequestHeaders.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  n,i:integer;
  sv:TRequestSubValues;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameGet(Name);
    if n=0 then i:=FIdx.ParsIndex else i:=0;
    while (i<FIdx.ParsIndex) and (FIdx.Pars[i].NameIndex<>n) do inc(i);
   end;
  if (i>=0) and (i<FIdx.ParsIndex) then
    sv:=TRequestSubValues.Create(Data,
      FIdx.Pars[i].ValueStart,FIdx.Pars[i].ValueLength,Result)
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
  if (Idx>=0) and (Idx<FIdx.ParsIndex) then
    Result:=WideString(Copy(Data,FIdx.Pars[Idx].NameStart,FIdx.Pars[Idx].NameLength))
  else
    raise ERangeError.Create('TRequestHeaders.GetName: Out of range');
end;

function TRequestHeaders.KnownHeader(Header:TKnownHeaderName):AnsiString;
var
  n,i:integer;
begin
  n:=KnownHeaders[Header];
  i:=0;
  while (i<FIdx.ParsIndex) and (FIdx.Pars[i].NameIndex<>n) do inc(i);
  if i<FIdx.ParsIndex then
    Result:=Copy(Data,FIdx.Pars[i].ValueStart,FIdx.Pars[i].ValueLength)
  else
    Result:='';
end;

procedure TRequestHeaders.SetName(Idx: integer; Value: WideString);
begin
  raise EXxmRequestHeadersReadOnly.Create(SxxmRequestHeadersReadOnly);
end;

{ TRequestSubValues }

constructor TRequestSubValues.Create(const Data: AnsiString; ValueStart,
  ValueLength: integer; var FirstValue: WideString);
begin
  inherited Create;
  FData:=Data;//assert reference counting, full copy is senseless
  FIdx.ParsIndex:=0;
  FIdx.ParsSize:=0;
  FirstValue:=WideString(SplitHeaderValue(AnsiString(FData),ValueStart,ValueLength,FIdx));
end;

destructor TRequestSubValues.Destroy;
begin
  SetLength(FIdx.Pars,0);
  FData:='';
  inherited;
end;

function TRequestSubValues.GetCount: integer;
begin
  Result:=FIdx.ParsIndex;
end;

function TRequestSubValues.GetItem(Name: OleVariant): WideString;
var
  i:integer;
begin
  //TODO: UTF8ToWideString?
  if VarIsNumeric(Name) then
   begin
    i:=integer(Name);
    if (i>=0) and (i<FIdx.ParsIndex) then
      Result:=WideString(Copy(FData,FIdx.Pars[i].ValueStart,FIdx.Pars[i].ValueLength))
    else
      raise ERangeError.Create('TRequestSubValues.GetItem: Out of range');
   end
  else
    Result:=WideString(GetParamValue(FData,FIdx,Name));
end;

function TRequestSubValues.GetName(Idx: integer): WideString;
begin
  if (Idx>=0) and (Idx<FIdx.ParsIndex) then
    Result:=WideString(Copy(FData,FIdx.Pars[Idx].NameStart,FIdx.Pars[Idx].NameLength))
  else
    raise ERangeError.Create('TRequestSubValues.GetName: Out of range');
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
      FItems[i].SubValues.Free;
  SetLength(FItems,0);
  inherited;
end;

procedure TResponseHeaders.Reset;
var
  i:integer;
begin
  FBuilt:=false;
  for i:=0 to FItemsCount-1 do
    if FItems[i].SubValues<>nil then
      FItems[i].SubValues.Reset;
  FItemsCount:=0;
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
  n,i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameGet(Name);
    if n=0 then i:=FItemsCount else i:=0;
    while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
   end;
  if (i>=0) and (i<FItemsCount) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseHeaders.SetItem(Name: OleVariant; const Value: WideString);
var
  n,i:integer;
begin
  if FBuilt then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  //TODO: add sorted, query with minimax
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameSet(Name);
    i:=0;
    while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
    if i=FItemsCount then
     begin
      Grow;
      FItems[i].Name:=Name;
      FItems[i].NameIndex:=n;
      FItems[i].SubValues:=nil;
     end;
   end;
  HeaderCheckValue(Value);
  if (i>=0) and (i<FItemsCount) then
    FItems[i].Value:=Value
  else
    raise ERangeError.Create('TResponseHeaders.SetItem: Out of range');
end;

function TResponseHeaders.Complex(Name: OleVariant;
  out Items: IxxmDictionary): WideString;
var
  n,i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameSet(Name);
    i:=0;
    while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
    if i=FItemsCount then
     begin
      Grow;
      FItems[i].Name:=Name;
      FItems[i].NameIndex:=n;
      FItems[i].Value:='';
      FItems[i].SubValues:=nil;
     end;
   end;
  if FItems[i].SubValues=nil then
    FItems[i].SubValues:=TResponseSubValues.Create;
  if (i>=0) and (i<FItemsCount) then
   begin
    Result:=FItems[i].Value;
    (FItems[i].SubValues as IUnknown)._AddRef;
    Items:=FItems[i].SubValues;
   end
  else
    raise ERangeError.Create('TResponseHeaders.Complex: Out of range');
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
    Result:=AnsiString(ss.DataString);
  finally
    ss.Free;
  end;
  FBuilt:=true;
end;

procedure TResponseHeaders.Add(const Name, Value: WideString);
var
  n,i:integer;
begin
  n:=HeaderNameSet(Name);
  HeaderCheckValue(Value);
  i:=FItemsCount;
  Grow;
  FItems[i].Name:=Name;
  FItems[i].NameIndex:=n;
  FItems[i].Value:=Value;
  FItems[i].SubValues:=nil;
end;

function TResponseHeaders.SetComplex(const Name,
  Value: WideString): TResponseSubValues;
var
  n,i:integer;
begin
  n:=HeaderNameSet(Name);
  i:=0;
  while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
  if i=FItemsCount then
   begin
    Grow;
    FItems[i].Name:=Name;
    FItems[i].NameIndex:=n;
    FItems[i].SubValues:=nil;
   end;
  HeaderCheckValue(Value);
  if (i>=0) and (i<FItemsCount) then
   begin
    FItems[i].Value:=Value;
    if FItems[i].SubValues=nil then
      FItems[i].SubValues:=TResponseSubValues.Create;
    (FItems[i].SubValues as IUnknown)._AddRef;
    Result:=FItems[i].SubValues;
   end
  else
    raise ERangeError.Create('TResponseHeaders.SetComplex: Out of range');
end;

procedure TResponseHeaders.Remove(const Name: WideString);
var
  n,i,l:integer;
begin
  n:=HeaderNameGet(Name);
  l:=FItemsCount;
  if n=0 then i:=l else i:=0;
  while (i<l) and (FItems[i].NameIndex<>n) do inc(i);
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
  if (Idx>=0) and (Idx<Length(FItems)) then
    Result:=FItems[Idx].Name
  else
    raise ERangeError.Create('TResponseHeaders.GetName: Out of range');
end;

procedure TResponseHeaders.SetName(Idx: integer; Value: WideString);
var
  n:integer;
begin
  if FBuilt then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  n:=HeaderNameSet(Value);
  if (Idx>=0) and (Idx<Length(FItems)) then
   begin
    FItems[Idx].Name:=Value;
    FItems[Idx].NameIndex:=n;
   end
  else
    raise ERangeError.Create('TResponseHeaders.SetName: Out of range');
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

procedure TResponseSubValues.Reset;
begin
  FBuilt:=false;
  FItemsCount:=0;
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
  n,i:integer;
begin
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameGet(Name);
    if n=0 then i:=FItemsCount else i:=0;
    while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
   end;
  if (i>=0) and (i<FItemsCount) then Result:=FItems[i].Value else Result:='';
end;

procedure TResponseSubValues.SetItem(Name: OleVariant; const Value: WideString);
var
  n,i:integer;
begin
  if FBuilt then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  HeaderCheckValue(Value);
  if VarIsNumeric(Name) then i:=integer(Name) else
   begin
    n:=HeaderNameSet(Name);
    i:=0;
    while (i<FItemsCount) and (FItems[i].NameIndex<>n) do inc(i);
    if i=FItemsCount then
     begin
      Grow;
      FItems[i].Name:=Name;
      FItems[i].NameIndex:=n;
     end;
   end;
  if (i>=0) and (i<FItemsCount) then
    FItems[i].Value:=Value
  else
    raise ERangeError.Create('TResponseSubValues.SetItem: Out of range');
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
  if (Idx>=0) and (Idx<Length(FItems)) then
    Result:=FItems[Idx].Name
  else
    raise ERangeError.Create('TResponseSubValues.GetName: Out of range');
end;

procedure TResponseSubValues.SetName(Idx: integer; Value: WideString);
var
  n:integer;
begin
  if FBuilt then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
  n:=HeaderNameSet(Value);
  if (Idx>=0) and (Idx<Length(FItems)) then
   begin
    FItems[Idx].Name:=Value;
    FItems[Idx].NameIndex:=n;
   end
  else
    raise ERangeError.Create('TResponseSubValues.SetName: Out of range');
end;

initialization
  HeaderNameNodes_Init;
end.
