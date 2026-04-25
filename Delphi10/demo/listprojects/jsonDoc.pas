{

jsonDoc.pas

Copyright 2015-2026 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/jsonDoc

v1.3.0

}
unit jsonDoc;

{$WARN SYMBOL_PLATFORM OFF}
{$D-}
{$L-}
{$Y-}

{

Options:
Define here or in the project settings

  JSONDOC_JSON_STRICT
    to disallow missing quotes around key names

  JSONDOC_JSON_LOOSE
    to allow missing colons and comma's

  JSONDOC_JSON_PASCAL_STRINGS
    to allow pascal-style strings

  JSONDOC_P2
    to combine JSONDOC_JSON_LOOSE and JSONDOC_JSON_PASCAL_STRINGS

  JSONDOC_STOREINDENTING
    to make AsString write indentation EOL's and tabs

  JSONDOC_THREADSAFE
    to make IJSONDocument instances thread-safe

  JSONDOC_DEFAULT_USE_IJSONARRAY
    to set JSON_UseIJSONArray to true by default

  JSONDOC_DEFAULT_USE_IJSONDOCARRAY
    to set JSON_UseIJSONDocArray to true by default

  JSONDOC_NO_LOAD_AFTER_ERROR
    to suppress loading keys after parse error

}

interface

uses SysUtils;

const
  //COM GUID's
  IID_IJSONDocument
    : TGUID = '{4A534F4E-0001-0001-C000-000000000001}';
  CLASS_JSONDocument
    : TGUID = '{4A534F4E-0001-0002-C000-000000000002}';
  IID_IJSONEnumerator
    : TGUID = '{4A534F4E-0001-0003-C000-000000000003}';
  IID_IJSONEnumerable
    : TGUID = '{4A534F4E-0001-0004-C000-000000000004}';
  IID_IJSONArray
    : TGUID = '{4A534F4E-0001-0005-C000-000000000005}';
  IID_IJSONDocArray
    : TGUID = '{4A534F4E-0001-0006-C000-000000000006}';
  IID_IJSONEnumerableSorted
    : TGUID = '{4A534F4E-0001-0008-C000-000000000008}';
  IID_IJSONDocArrayEnumerator
    : TGUID = '{4A534F4E-0001-0009-C000-000000000009}';
  IID_IJSONMemBank
    : TGUID = '{4A534F4E-0001-000A-C000-00000000000A}';
  IID_IJSONMemBankLoadable
    : TGUID = '{4A534F4E-0001-000B-C000-00000000000B}';

type
{
  IJSONDocument interface
  the base JSON document interface that provides access to a set of
  key-value pairs.
  use AsString and Parse to convert JSON to and from string values.
  use AsVarArray to access the key-value pairs as a [x,2] variant array.
  use Clear to re-use a JSON doc for parsing or building a new similar
  document and keep the allocated memory for keys and values.
  see also: JSON function
}
  IJSONDocument = interface(IUnknown)
    ['{4A534F4E-0001-0001-C000-000000000001}']
    function Get_Item(const Key: WideString): Variant; stdcall;
    procedure Set_Item(const Key: WideString; const Value: Variant); stdcall;
    procedure Parse(const JSONData: WideString); stdcall;
    function ToString: WideString; stdcall;
    function ToVarArray: Variant; stdcall;
    procedure Clear; stdcall;
    procedure Delete(const Key: WideString); stdcall;
    function v0(const Key: WideString): pointer; stdcall;
    property Item[const Key: WideString]: Variant
      read Get_Item write Set_Item; default;
    property AsString: WideString read ToString write Parse;
  end;

{
  IJSONEnumerator interface
  use IJSONEnumerator to enumerate a document's key-value pairs
  see also: JSONEnum function
}
  //TODO: IEnumVariant?
  IJSONEnumerator = interface(IUnknown)
    ['{4A534F4E-0001-0003-C000-000000000003}']
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
    property Key: WideString read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  IJSONEnumerable interface
  used to get a IJSONEnumerable instance for a document
  see also: JSONEnum function
}
  IJSONEnumerable = interface(IUnknown)
    ['{4A534F4E-0001-0004-C000-000000000004}']
    function NewEnumerator: IJSONEnumerator; stdcall;
  end;

{
  IJSONArray interface
  When using VarArrayOf (declared in Variants.pas), a SafeArray is
  used internally for storage, but as a variant value VarCopy is called
  with each use, creating duplicate (deep) copies of the SafeArray.
  Use IJSONArray (and the ja function) to create a IJSONArray instance
  that uses a single copy of the data. It is also reference counted,
  which automates memory clean-up.
}
  IJSONArray = interface(IUnknown)
    ['{4A534F4E-0001-0005-C000-000000000005}']
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function ItemsCount: integer; stdcall;
    function ToString: WideString; stdcall;
    function v0(Index: integer): pointer; stdcall;
    property Item[Idx: integer]: Variant read Get_Item write Set_Item; default;
    property Count: integer read ItemsCount;
    property AsString: WideString read ToString;
  end;

{
  IJSONDocArray interface
  use IJSONDocArray to build an array of similar documents,
  ideally in combination with a single IJSONDocument instance and
  IJSONDocument.Clear to re-use the memory allocated for keys and values
  see also: JSONDocArr function
}
  IJSONDocArray = interface(IJSONArray)
    ['{4A534F4E-0001-0006-C000-000000000006}']
    function Add(const Doc: IJSONDocument): integer; stdcall;
    function AddJSON(const Data: WideString): integer; stdcall;
    procedure LoadItem(Index: integer; const Doc: IJSONDocument); stdcall;
    procedure Clear; stdcall;
    function GetJSON(Index: integer): WideString; stdcall;
    procedure Parse(const JSONData: WideString); stdcall;
  end;

{
  IJSONEnumerableSorted interface
  used to get a IJSONEnumerable instance for a document, that uses the
  document's internal keys sort order
  see also: JSONEnum function
}
  IJSONEnumerableSorted = interface(IUnknown)
    ['{4A534F4E-0001-0008-C000-000000000008}']
    function NewEnumerator: IJSONEnumerator; stdcall;
  end;

{
  IJSONDocArrayEnumerator
  extends IJSONDocument with enumerator functions, used by the JSONEnum
  overload for IJSONDocArray instances
}
  IJSONDocArrayEnumerator = interface(IJSONDocument)
    ['{4A534F4E-0001-0009-C000-000000000009}']
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_DocIndex: integer; stdcall;
    property DocIndex: integer read Get_DocIndex;
  end;

{
  IJSONMemBank
  interface used internally between jsonDoc objects to share memory
}
  IJSONMemBank = interface(IUnknown)
  ['{4A534F4E-0001-000A-C000-00000000000A}']
    function Bank:TObject;
  end;

{
  IJSONMemBankLoadable
  interface used internally between jsonDoc objects to share memory
}
  IJSONMemBankLoadable = interface(IUnknown)
  ['{4A534F4E-0001-000B-C000-00000000000B}']
    procedure ClearBank(MemBank: IJSONMemBank); stdcall;
    procedure LoadBank(Bank: IUnknown; Index: integer); stdcall;
    procedure Build(Builder: pointer; TabIndex: integer); stdcall;
  end;


{
  JSON function: JSON document factory
  call JSON without parameters do create a new blank document
}
function JSON: IJSONDocument; overload;

{
  JSON function: JSON document builder
  pass an array of alternately keys and values,
  suffix key with opening brace to start an embedded document,
  and key of a single closing brace to close it.
}
function JSON(const x: array of Variant): IJSONDocument; overload;

{
  JSON function: JSON document converter
  pass a single variant to have it converted to an IJSONDocument interface
  or a string with JSON parsed into a IJSONDocument
  or nil when VarIsNull
}
function JSON(const x: Variant): IJSONDocument; overload;

{
  JSON function: JSON prepare and parse
  combine the document builder with a parse call
}
function JSON(const x: array of Variant; const d: WideString): IJSONDocument;
  overload; //inline;

{
  JSONEnum function
  get a new enumerator to enumeratare the key-value pairs in the document
}
function JSONEnum(const x: IJSONDocument): IJSONEnumerator; overload; //inline;
function JSONEnum(const x: Variant): IJSONEnumerator; overload;
function JSON(const x: IJSONEnumerator): IJSONDocument; overload; //inline;
function JSONEnum(const x: IJSONEnumerator): IJSONEnumerator; overload; //inline;
function JSONEnum(const x: IJSONDocArray): IJSONDocArrayEnumerator; overload; //inline;

{
  JSONEnumSorted function
  get a new enumerator to enumeratare the key-value pairs in the document,
  using the document's internal keys sort order
}
function JSONEnumSorted(const x: IJSONDocument): IJSONEnumerator; overload; //inline;
function JSONEnumSorted(const x: Variant): IJSONEnumerator; overload;

{
  ja function
  create and populate a new IJSONArray instance
  or cast a Variant holding a JSONArray instance to the interface reference
}
function ja(const Items:array of Variant): IJSONArray; overload;
function ja(const Item:Variant): IJSONArray; overload;

{
  JSONDocArray function
  get a new IJSONDocArray instance
}
function JSONDocArray: IJSONDocArray; overload;
function JSONDocArray(const Items:array of IJSONDocument): IJSONDocArray; overload;
function JSONDocArray(const x: Variant): IJSONDocArray; overload;

{
  isJSON, isJSONArray, isJSONDocArray
  check whether a variant value holds an instance of IJSONDocument,
  IJSONArray, IJSONDocArray
}
function isJSON(const v: Variant; var d: IJSONDocument): boolean; //inline;
function isJSONArray(const v: Variant; var a: IJSONArray): boolean; //inline;
function isJSONDocArray(const v: Variant; var a: IJSONDocArray): boolean; //inline;

{
  newJSON, newJSONDocArray
  assigns new instances and returns a reference for concise syntax
}
function newJSON(var d: IJSONDocument): IJSONDocument;
function newJSONDocArray(var a: IJSONDocArray): IJSONDocArray;

{
  JSON_UseIJSONArray
  switch JSON so it will create IJSONArray instances to hold arrays of values
  instead of VarArrayCreate, default false
  see also TJSONDocument.UseIJSONArray property
}
var
  JSON_UseIJSONArray: boolean;

{
  JSON_UseIJSONDocArray
  switch JSON.Parse so it will create IJSONDOCArray instances to hold arrays of
  documents instead of VarArrayCreate, when a sequence of "[" and "{" is
  detected, default false
  see also TJSONDocument.UseIJSONDocArray property
}
var
  JSON_UseIJSONDocArray: boolean;

{
  JSONa function: JSON document factory with UseIJSONDocArray enabled
  create a new blank document, and sets UseIJSONDocArray for cals to Parse,
  creates an IJSONDocArray instance when a sequence of "[" and "{" is detected.
}
function JSONa: IJSONDocument;

type
{
  EJSONException class types
  exception types thrown from TJSONDocument's Parse and ToString
}
  EJSONException=class(Exception);
  EJSONDecodeException=class(EJSONException);
  EJSONEncodeException=class(EJSONException);

implementation

uses Variants, Windows, Math;

{
  KeyValueNode
  internal structure to key-value pairs using key hashes
}
const
  KeyValueNodeHashBits = 8;//bits
  KeyValueNodeHashMask = (1 shl KeyValueNodeHashBits)-1;//$F
  KeyValueItemMaxChain = 8;

type
  PPKeyValueNode = ^PKeyValueNode;
  PKeyValueNode = ^TKeyValueNode;
  TKeyValueNode = array[0..KeyValueNodeHashMask] of pointer;//of PKeyValueNode;

//strange! 'old' Delphi has NativeUInt=0..1; ??!!
{$IF not(Declared(NativeUInt)) or (SizeOf(cardinal)=SizeOf(pointer))}
  NativeUInt = cardinal;
{$IFEND}

  PKeyValueItem = ^TKeyValueItem;
  TKeyValueItem = record
    Hash: NativeUInt;
    Key: WideString;
    More, Next: PKeyValueItem;
    LoadIndex: cardinal;
    NodeIndex: integer;
    Value: Variant;
  end;

{
  TJSONImplBaseObj
  common base object JSON implementation objects inherit
  don't use directly
}

{$IFDEF JSONDOC_THREADSAFE}
  TJSONImplBaseObj = class(TInterfacedObject)
  protected
    FLock:TRTLCriticalSection;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

{$ELSE}
  //thread-unsafe anyway, so avoid locking when reference counting:
  TJSONImplBaseObj = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

{$ENDIF}

{
  TJSONBuilder
  used internally as a string builder
}
  TJSONBuilder = object//record
    Data: WideString;
    DataIndex, DataSize: cardinal;
    procedure Clear;
    function Reserve(l: cardinal): cardinal;
    procedure Append(const x: WideString);
    {$IFDEF JSONDOC_STOREINDENTING}
    procedure Write(const x; l: Cardinal);
    {$ENDIF}
    procedure VarToStr(const v: Variant);
    procedure EncodeStr(const x: WideString);
    function Output: WideString;
  end;
  PJSONBuilder = ^TJSONBuilder;

{
  TJSONMemBank
  used internally to allow jsonDoc objects to share memory
}
  TJSONMemNode = record
    KeyIndex, KeyLength, ValueIndex, ValueLength, Next, Child, F1, F2: integer;
  end;
  PJSONMemNode = ^TJSONMemNode; 

  TJSONMemBank = class(TJSONImplBaseObj, IJSONMemBank)
  private
    json: TJSONBuilder;
    FNodeIndex, FNodeSize: integer;
    FNodes: array of TJSONMemNode;
  protected
    function Bank: TObject;
    function ExVicinity(di: cardinal): WideString;
    function Key(n: integer): WideString;
    function GetStringValue(vi, vl: integer): WideString;

    function StartArray: integer;
    function Add(BaseIndex: integer; const Data: WideString;
      Doc: IJSONDocument): integer;

  public
    procedure AfterConstruction; override;
    procedure Parse(var DocIndex: integer; DataIndex: integer;
      const Data: WideString = '');
  end;

{
  TJSONDocument class
  the default IJSONDocument implementation
  see also: JSON function
}
  TJSONDocument = class(TJSONImplBaseObj, IJSONDocument, IJSONMemBankLoadable,
    IJSONEnumerable, IJSONEnumerableSorted)
  private
    FMemBank: IJSONMemBank;
    FRoot:PKeyValueNode;
    FFirst,FLast:PKeyValueItem;
    FLoadIndex: cardinal;
    {$IFNDEF JSONDOC_THREADSAFE}
    FGotItem:PKeyValueItem;
    {$ENDIF}
    FUseIJSONArray, FUseIJSONDocArray: boolean;
    function LookUpItem(const Key:WideString):PKeyValueItem;
    function StoreItem(const Key:WideString):PKeyValueItem;
    function v00(Item: PKeyValueItem; FailWhenMissing: boolean): PVariant;
    function v1(NodeIndex: integer): Variant;
  protected
    function Get_Item(const Key: WideString): Variant; stdcall;
    procedure Set_Item(const Key: WideString; const Value: Variant); stdcall;
    function v0(const Key: Widestring): pointer; stdcall;

    procedure ClearBank(MemBank: IJSONMemBank); stdcall;
    procedure LoadBank(Bank: IUnknown; Index: integer); stdcall;
    procedure Build(Builder: pointer; TabIndex: integer); stdcall;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Parse(const JSONData: WideString); stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONDocument.ToString=JSONToString;
    function ToVarArray: Variant; stdcall;
    procedure Clear; stdcall;
    function NewEnumerator: IJSONEnumerator; stdcall;
    procedure Delete(const Key: WideString); stdcall;
    property Item[const Key: WideString]: Variant
      read Get_Item write Set_Item; default;
    property AsString: WideString read JSONToString write Parse;
    property UseIJSONArray:boolean read FUseIJSONArray write FUseIJSONArray;
    property UseIJSONDocArray:boolean read FUseIJSONDocArray write FUseIJSONDocArray;
    function NewEnumeratorSorted: IJSONEnumerator; stdcall;
    function IJSONEnumerableSorted.NewEnumerator = NewEnumeratorSorted;
  end;

{
  TJSONEnumerator class
  the default IJSONEnumerator implementation
  see also: JSONEnum function
}
  TJSONEnumerator = class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:TJSONDocument;
    FFirst:boolean;
    FItem:PKeyValueItem;
  protected
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  public
    constructor Create(Data: TJSONDocument);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    property Key: Widestring read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  TJSONEnumeratorSorted class
  a IJSONEnumerator implementation that uses first sorts the keys
  see also: JSONEnumSorted function
}
  TJSONEnumeratorSorted = class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:TJSONDocument;
    FItems:array of PKeyValueItem;
    FIndex:integer;
  protected
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  public
    constructor Create(Data: TJSONDocument);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    property Key: Widestring read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  TJSONDocArrayEnumerator class
}
  TJSONDocArrayEnumerator = class(TJSONDocument, IJSONDocArrayEnumerator)
  private
    FDocArray: IJSONDocArray;
    FIndex: integer;
  protected
    function IJSONDocArrayEnumerator.Get_Item=Get_Item;
    procedure IJSONDocArrayEnumerator.Set_Item=Set_Item;
    procedure IJSONDocArrayEnumerator.Parse=Parse;
    function IJSONDocArrayEnumerator.ToString=JSONToString;
    function IJSONDocArrayEnumerator.ToVarArray=ToVarArray;
    procedure IJSONDocArrayEnumerator.Clear=Clear;
    procedure IJSONDocArrayEnumerator.Delete=Delete;
    function IJSONDocArrayEnumerator.v0=v0;
    function Get_DocIndex: integer; stdcall;
  public
    constructor Create(const Data: IJSONDocArray);
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    property DocIndex: integer read Get_DocIndex;
  end;

{
  TJSONArray class
  Default ILightArray implementation
}
  TJSONArray = class(TJSONImplBaseObj, IJSONArray, IJSONEnumerable)
  private
    FData:array of Variant;
  protected
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function ItemsCount: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
    function NewEnumerator: IJSONEnumerator; stdcall;
  public
    constructor Create(Size: integer);
    property Item[Idx: integer]: Variant read Get_Item write Set_Item; default;
    property AsString: WideString read JSONToString;
  end;

{
  TJSONArrayEnumerator
  an IJSONEnumerator implementation that iterates over a variant array
  used internally to convert to JSON
}
  TJSONArrayEnumerator= class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:IJSONArray;
    FIndex,FMax:integer;
  protected
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  public
    constructor Create(const Data:IJSONArray);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    property Key: Widestring read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  TJSONDocArray class
  the default IJSONDocArray implementation
  see also: JSONDocArray function
}
  TJSONDocArray = class(TJSONImplBaseObj, IJSONArray, IJSONDocArray,
    IJSONMemBankLoadable)
  private
    FMemBank:IJSONMemBank;
    FBaseIndex:integer;
    {$IFNDEF JSONDOC_THREADSAFE}
    FCurrentIndex,FCurrentNode:integer;
    {$ENDIF}
  protected
    //IJSONArray
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function ItemsCount: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
    //function IJSONArray.ToString=JSONToString;
    //IJSONDocArray
    function Add(const Doc: IJSONDocument): integer; stdcall;
    function AddJSON(const Data: WideString): integer; stdcall;
    procedure LoadItem(Index: integer; const Doc: IJSONDocument); stdcall;
    function IJSONDocArray.ToString=JSONToString;
    procedure Clear; stdcall;
    function GetJSON(Index: integer): WideString; stdcall;
    procedure Parse(const JSONData: WideString); stdcall;
    //IJSONMemBankLoadable
    procedure ClearBank(MemBank: IJSONMemBank); stdcall;
    procedure LoadBank(Bank: IUnknown; Index: integer); stdcall;
    procedure Build(Builder: pointer; TabIndex: integer); stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    property AsString: WideString read JSONToString;
  end;

{
  TVarArrayEnumerator
  an IJSONEnumerator implementation that iterates over a variant array
  used internally to convert to JSON
}
  TVarArrayEnumerator = class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:PVariant;
    FCurrent:Variant;
    FIndex,FMax,FCurrentIndex:integer;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  public
    constructor Create(const Data:PVariant);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    property Key: Widestring read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  TVarJSONArray
  an IJSONArray implementation that works on an existing variant array
  used internally by the ja function
}
  TVarJSONArray = class(TJSONImplBaseObj, IJSONArray)
  private
    FData,FCurrent:Variant;
    v1,v2,FCurrentIndex:integer;
  protected
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function ItemsCount: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
  public
    constructor Create(const Data: Variant);
    constructor CreateNoCopy(var Data: Variant);
    destructor Destroy; override;
    property AsString: WideString read JSONToString;
  end;

procedure VarMove(var Dest, Src: Variant);
begin
  //Dest:=Src;VarClear(Src);
  Move(Src,Dest,SizeOf(TVarData));
  ZeroMemory(@Src,SizeOf(TVarData));
end;

{
  kvsHash
  key hash function used for lookup
}

function kvsHash(const Key: WideString): NativeUInt;
const
  seed = $11BB9955;
var
  i,l:NativeUInt;
begin
  Result:=seed;
  l:=Length(Key);
  for i:=1 to l do
    Result:=(Result shl 1) + (word(Key[i]) * $0901);
  inc(Result,l);
end;

{
  key-value-node functions
}

function NewNode(var nn:PKeyValueNode):PKeyValueNode;
const
  ll=SizeOf(TKeyValueNode);
begin
  New(nn);//GetMem(nn,ll);
  FillChar(nn^,ll,0);
  Result:=nn;
end;

function NewItem(Hash:NativeUInt;const Key:Widestring;
  var pp:PKeyValueItem):pointer;
const
  ll=SizeOf(TKeyValueItem);
begin
  New(pp);//GetMem(pp,ll);
  FillChar(pp^,ll,0);
  Result:=pointer(NativeUInt(pp) or 1);
  //assert Hash=kvsHash(Key)
  pp.Hash:=Hash;
  pp.Key:=Key;
end;

function IsItem(p:pointer;var pp:PKeyValueItem):boolean; //inline;
begin
  Result:=(NativeUInt(p) and 1)<>0;
  if Result then pp:=PKeyValueItem(NativeUInt(p) xor 1);
end;

function AsItem(p:PKeyValueItem):pointer; //inline;
begin
  Result:=pointer(NativeUInt(p) or 1);
end;

{ TJSONImplBaseObj }

{$IFDEF JSONDOC_THREADSAFE}

procedure TJSONImplBaseObj.AfterConstruction;
begin
  inherited;
  InitializeCriticalSection(FLock);
end;

destructor TJSONImplBaseObj.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

{$ELSE}

procedure TJSONImplBaseObj.AfterConstruction;
begin
  inherited;
  dec(FRefCount);//see constructor
end;

procedure TJSONImplBaseObj.BeforeDestruction;
begin
  inherited;
  if RefCount<>0 then System.Error(reInvalidPtr);
end;

class function TJSONImplBaseObj.NewInstance: TObject;
begin
  Result:=inherited NewInstance;
  //see AfterConstruction, prevent detroy while creating
  TJSONImplBaseObj(Result).FRefCount:=1;
end;

function TJSONImplBaseObj.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID,Obj) then Result:=0 else Result:=E_NOINTERFACE;
end;

function TJSONImplBaseObj._AddRef: Integer;
begin
  inc(FRefCount);
  Result:=FRefCount;
end;

function TJSONImplBaseObj._Release: Integer;
begin
  dec(FRefCount);
  Result:=FRefCount;
  if Result=0 then Destroy;
end;

{$ENDIF}

{ TJSONBuilder }

const
  NodeGrowStep=1 shl 8;
  JSONBuilderGrowStep=1 shl 16;

  w2=SizeOf(WideChar);//2

procedure TJSONBuilder.Clear;
begin
  Data:='';
  DataIndex:=0;
  DataSize:=0;
end;

function TJSONBuilder.Reserve(l: cardinal): cardinal;
begin
  Result:=DataIndex+l;
  if Result>DataSize then
   begin
    if (Result and (JSONBuilderGrowStep-1))=0 then
      DataSize:=Result
    else
      DataSize:=(Result and not(JSONBuilderGrowStep-1))+JSONBuilderGrowStep;
    SetLength(Data,DataSize);
   end;
end;

procedure TJSONBuilder.Append(const x: WideString);
var
  l,d:cardinal;
begin
  l:=Length(x);
  d:=Reserve(l);
  Move(x[1],Data[DataIndex+1],l*w2);
  DataIndex:=d;
end;

function TJSONBuilder.Output: WideString;
begin
  //SetLength(Data,DataIndex);DataSize:=DataIndex;Result:=Data;
  SetLength(Result,DataIndex);
  Move(Data[1],Result[1],DataIndex*w2);
end;

{ TJSONMemBank }

procedure TJSONMemBank.AfterConstruction;
begin
  inherited;
  FNodeIndex:=0;
  FNodeSize:=0;
end;

const
  jfk_Mask = $F0000;
  
  jfkNoEsc = $00000;
  jfkWithEsc = $10000;
  jfkArrayIndex = $20000;
  jfkPascalString = $30000;
  jfkLoose = $40000;

  jfd_Mask = $FF;

  jfdObject = 1;
  jfdArray = 2;
  jfdNull = 3;
  jfdBoolTrue = 4;
  jfdBoolFalse = 5;
  jfdRawJSON = 7;
  jfdStringNoEsc = 8;
  jfdStringWithEsc = 9;
  jfdStringPascal = 10;
  jfdInt8 = 20;
  jfdInt16 = 21;
  jfdInt32 = 22;
  jfdInt64 = 23;
  jfdFloat = 11;

function TJSONMemBank.Bank: TObject;
begin
  Result:=Self;
end;

{$IFDEF JSONDOC_P2}
{$DEFINE JSONDOC_JSON_LOOSE}
{$DEFINE JSONDOC_JSON_PASCAL_STRINGS}
{$ENDIF}

type
  TxToken=(xtInvalid,xtWhitespace,xtAOpen,xtAClose,xtBOpen,xtBClose,
    xtDoubleQuotes,xtColon,xtComma,xtAlpha,xtNum,xtBackslash,
    xtSymbol,xtMinus,xtSemiColon,xtEquals,xtSingleQuote,xtHash,xtDollar);
const
  XTokenMap:array[0..$7F] of TxToken=(
    //$00
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    xtInvalid,xtWhitespace,xtWhitespace,xtWhitespace,
    xtWhitespace,xtWhitespace,xtInvalid,xtInvalid,
    //$10
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    xtInvalid,xtInvalid,xtInvalid,xtInvalid,
    //$20
    xtWhitespace,xtSymbol,xtDoubleQuotes,xtHash,
    xtDollar,xtSymbol,xtSymbol,xtSingleQuote,
    xtSymbol,xtSymbol,xtSymbol,xtSymbol,
    xtComma,xtMinus,xtSymbol,xtSymbol,
    //$30
    xtNum,xtNum,xtNum,xtNum,
    xtNum,xtNum,xtNum,xtNum,
    xtNum,xtNum,xtColon,xtSemiColon,
    xtSymbol,xtEquals,xtSymbol,xtSymbol,
    //$40
    xtSymbol,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    //$50
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtBOpen,
    xtBackslash,xtBClose,xtSymbol,xtSymbol,
    //$60
    xtSymbol,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    //$70
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAlpha,
    xtAlpha,xtAlpha,xtAlpha,xtAOpen,
    xtSymbol,xtAClose,xtSymbol,xtInvalid);

procedure TJSONMemBank.Parse(var DocIndex: integer; DataIndex: integer;
  const Data: WideString);
var
  i,l:integer;

  function Node(ki,kl,vi,vl,f1:integer):integer;
  var
    m:PJSONMemNode;
  begin
    if FNodeIndex=FNodeSize then
     begin
      inc(FNodeSize,NodeGrowStep);
      SetLength(FNodes,FNodeSize);
     end;
    Result:=FNodeIndex;
    m:=@FNodes[FNodeIndex];
    inc(FNodeIndex);
    m.KeyIndex:=ki;
    m.KeyLength:=kl;
    m.ValueIndex:=vi;
    m.ValueLength:=vl;
    m.Next:=0;
    m.Child:=0;
    m.F1:=f1;
    m.F2:=0;
  end;
  function SkipWhiteSpace:TxToken;
  var
    w:WideChar;
  begin
    while (i<l) and (json.Data[i]<=' ') do inc(i);
    if i<l then
     begin
      w:=json.Data[i];
      if word(w)<$0080 then
        Result:=xTokenMap[word(w) and $007F]
      else
        Result:=xtInvalid;
     end
    else
      Result:=xtInvalid;
  end;
  procedure Expect(c:WideChar;const msg:string);
  begin
    while (i<l) and (json.Data[i]<=' ') do inc(i);
    if (i<l) and (json.Data[i]=c) then
      inc(i)
    else
    {$IFDEF JSONDOC_JSON_LOOSE}
      ;//silent
    {$ELSE}
      raise EJSONDecodeException.Create(msg+ExVicinity(i));
    {$ENDIF}
  end;
  procedure GetStringIndexes(var si,sl:integer;var e:boolean);
  begin
    //assert json.Data[si]='"'
    e:=false;
    inc(i);
    si:=i;
    while (i<l) and (json.Data[i]<>'"') do
     begin
      if json.Data[i]='\' then
       begin
        e:=true;
        inc(i);
       end;
      inc(i);
     end;
    sl:=i-si;
    inc(i);
  end;
  {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
  procedure GetPascalIndexes(var si,sl:integer);
  begin
    si:=i;
    while (i<l) and ((json.Data[i]='''') or (json.Data[i]='#')) do
      if json.Data[i]='''' then
       begin
        inc(i);
        while (i<l) and (json.Data[i]<>'''') do inc(i);
        if i<l then inc(i);
       end
      else
       begin
        inc(i);
        if (i<l) and (json.Data[i]='$') then
         begin
          inc(i);
          while (i<l) and (word(json.Data[i]) in [$30..$39,$41..$5A,$61..$7A]) do inc(i);
         end
        else
          while (i<l) and (word(json.Data[i]) in [$30..$39]) do inc(i);
       end;
    sl:=i-si;
  end;
  {$ENDIF}
var
  IsArray,firstItem,b:boolean;
  ki,kl,kf,vi,vl,head,top,n,RestoreNext:integer;
  m:PJSONMemNode;
  xt:TxToken;
  v64:int64;
  v64p:packed record v64lo,v64hi:integer; end absolute v64;
begin
  RestoreNext:=0;//default
  head:=0;//counter warning
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  {$ENDIF}
  try

    if DataIndex=0 then
     begin
      head:=Node(0,0,1,0,jfdObject);
      i:=json.DataIndex;
      json.Append(Data);
      inc(i);
      l:=Length(Data);
     end
    else
     begin
      head:=DataIndex;
      m:=@FNodes[head];
      i:=m.ValueIndex;
      l:=m.ValueLength;
      m.F1:=(m.F1 and not(jfd_Mask)) or jfdObject;
      RestoreNext:=m.Next;
     end;
    DocIndex:=head;

    vi:=((l div $20) and (NodeGrowStep-1));//measured guess
    if (vi and (NodeGrowStep-1))<>0 then
      vi:=(vi and not(NodeGrowStep-1))+NodeGrowStep;
    inc(vi,FNodeIndex);
    if FNodeSize<vi then
     begin
      FNodeSize:=vi;
      SetLength(FNodes,FNodeSize);
     end;

    inc(l,i);
    Expect('{','JSON doesn''t define an object, "{" expected.');

    top:=0;
    IsArray:=false;
    firstItem:=true;
    while i<l do
     begin
      if firstItem then firstItem:=false else
        Expect(',','JSON element not delimited by comma');
      if IsArray then
       begin
        ki:=0;
        if top=0 then kl:=0 else kl:=FNodes[top].KeyLength;
        kf:=jfkArrayIndex;
       end
      else
       begin
        //key string
        xt:=SkipWhiteSpace;
        if xt=xtDoubleQuotes then //'"'
         begin
          GetStringIndexes(ki,kl,b);
          if b then kf:=jfkWithEsc else kf:=jfkNoEsc;
         end
        else
        {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
        if (xt=xtSingleQuote) or (xt=xtHash) then //'''','#'
         begin
          GetPascalIndexes(ki,kl);
          kf:=jfkPascalString;
         end
        else
        {$ENDIF}
        if xt=xtAClose then //'}'
         begin
          ki:=0;
          kl:=0;
          kf:=jfkArrayIndex;
         end
        else
        {$IFDEF JSONDOC_JSON_STRICT}
          raise EJSONDecodeException.Create(
            'JSON key string not enclosed in double quotes'+ExVicinity(i));
        {$ELSE}
         begin
          ki:=i;
          while (i<l) and (json.Data[i]>' ') and not(
            (json.Data[i]=':') or (json.Data[i]='"')
            {$IFDEF JSONDOC_JSON_LOOSE}
            or (json.Data[i]='{') or (json.Data[i]='[')
            or (json.Data[i]='=') or (json.Data[i]=';')
            {$ENDIF}
            {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
            or (json.Data[i]='''')
            {$ENDIF}
            ) do inc(i);
          kl:=i-ki;
          kf:=jfkLoose;
         end;
        {$ENDIF}
        If ki<>0 then
          Expect(':','JSON key, value not separated by colon');
        {$IFDEF JSONDOC_JSON_LOOSE}
        if (i<l) and (json.Data[i]='=') then inc(i);
        {$ENDIF}
       end;

      //value
      n:=0;
      xt:=SkipWhiteSpace;
      if xt=xtAOpen then//'{' object
       begin
        n:=Node(ki,kl,i,0,kf or jfdObject);
        IsArray:=false;
        firstItem:=true;
       end
      else
      if xt=xtAClose then//'}' close object
        //see below
      else
      if xt=xtBOpen then //'[' array
       begin
        n:=Node(ki,kl,i,0,kf or jfdArray);
        IsArray:=true;
        firstItem:=true;
       end
      else
      if xt=xtBClose then //']' close array
        //see below
      else
      if xt=xtDoubleQuotes then //'"' string
       begin
        GetStringIndexes(vi,vl,b);
        if b then kf:=kf or jfdStringWithEsc else kf:=kf or jfdStringNoEsc;
        n:=Node(ki,kl,vi,vl,kf);
       end
      else

      {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
      if (xt=xtSingleQuote) or (xt=xtHash) then //'''','#' pascal-style string
       begin
        GetPascalIndexes(vi,vl);
        n:=Node(ki,kl,vi,vl,kf or jfdStringPascal);
       end
      else
      if xt=xtDollar then //'$' pascal-style hex digit
       begin
        inc(i);
        vi:=i;
        v64:=0;
        while (i<l) and (word(json.Data[i]) in [$30..$39,$41..$46,$61..$66]) do
         begin
          if word(json.Data[i]) and $F0=$30 then
            v64:=(v64 shl 4) or (word(json.Data[i]) and $F)
          else
            v64:=(v64 shl 4) or ((word(json.Data[i]) and $1F)+9);
          inc(i);
         end;
        if i=vi then
          raise EJSONDecodeException.Create(
            'JSON Unrecognized value type'+ExVicinity(i));
        if v64>=$80000000 then kf:=kf or jfdInt64
        else if v64>=$8000 then kf:=kf or jfdInt32
        else if v64>=$80 then kf:=kf or jfdInt16
        else kf:=kf or jfdInt8;
        if b then v64:=-v64;
        vi:=v64p.v64hi;
        vl:=v64p.v64lo;
        n:=Node(ki,kl,vi,vl,kf);
       end
      else
      {$ENDIF}

      if (xt=xtNum) or (xt=xtMinus) then //number
       begin
        b:=json.Data[i]='-';
        vi:=i;
        if b then inc(i);
        v64:=0;
        while (i<l) and (word(json.Data[i]) in [$30..$39]) do
         begin
          v64:=v64*10+(word(json.Data[i]) and $F);//TODO: detect overflow
          inc(i);
         end;
        if word(json.Data[i]) in [word('.'),word('e'),word('E')] then
         begin
          //float
          inc(i);
          while (i<l) and (word(json.Data[i]) in
            [word('0')..word('9'),word('-'),word('+'),word('e'),word('E')]) do inc(i);
          vl:=i-vi;
          //:=StrToFloat(Copy(json.Data,ni,i-nl));
          n:=Node(ki,kl,vi,vl,kf or jfdFloat);
         end
        else
         begin
          //integer
          if v64>=$80000000 then kf:=kf or jfdInt64
          else if v64>=$8000 then kf:=kf or jfdInt32
          else if v64>=$80 then kf:=kf or jfdInt16
          else kf:=kf or jfdInt8;
          if b then v64:=-v64;
          vi:=v64p.v64hi;
          vl:=v64p.v64lo;
          n:=Node(ki,kl,vi,vl,kf);
         end;
       end
      else

      {$IFDEF JSONDOC_JSON_LOOSE}
      if xt=xtSemiColon then
        inc(i)
      else
       begin
        vi:=i;
        while (i<l) and (json.Data[i]>' ') and not(
          word(json.Data[i]) in [word(':'),word(','),word('"'),word('}'),word(']')
          {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
          ,word('''')
          {$ENDIF}
          ]) do inc(i);
        vl:=i;
        if vi=vl then
          raise EJSONDecodeException.Create(
          'JSON Value expected'+ExVicinity(i));
        dec(vl,vi);
        if (vl=4) and (json.Data[vi]='t') and (json.Data[vi+1]='r') and
          (json.Data[vi+2]='u') and (json.Data[vi+3]='e') then
          n:=Node(ki,kl,0,0,kf or jfdBoolTrue)
        else
        if (vl=5) and (json.Data[vi]='f') and (json.Data[vi+1]='a') and
          (json.Data[vi+2]='l') and (json.Data[vi+3]='s') and (json.Data[vi+4]='e') then
          n:=Node(ki,kl,0,0,kf or jfdBoolFalse)
        else
        if (vl=4) and (json.Data[vi]='n') and (json.Data[vi+1]='u') and
          (json.Data[vi+2]='l') and (json.Data[vi+3]='l') then
          n:=Node(ki,kl,0,0,kf or jfdNull)
        else
          n:=Node(ki,kl,vi,vl,kf or jfdStringNoEsc);
       end;
      {$ELSE}
      if xt=xtAlpha then
        case word(json.Data[i]) of
          word('t')://true
           begin
            inc(i);
            Expect('r','JSON true misspelled');
            Expect('u','JSON true misspelled');
            Expect('e','JSON true misspelled');
            n:=Node(ki,kl,0,0,kf or jfdBoolTrue);//i-4,4
           end;
          word('f')://false
           begin
            inc(i);
            Expect('a','JSON false misspelled');
            Expect('l','JSON false misspelled');
            Expect('s','JSON false misspelled');
            Expect('e','JSON false misspelled');
            n:=Node(ki,kl,0,0,kf or jfdBoolFalse);//i-5,5
           end;
          word('n')://null
           begin
            inc(i);
            Expect('u','JSON null misspelled');
            Expect('l','JSON null misspelled');
            Expect('l','JSON null misspelled');
            n:=Node(ki,kl,0,0,kf or jfdNull);//i-4,4
           end;

          else
            raise EJSONDecodeException.Create(
              'JSON Unrecognized value type'+ExVicinity(i));
        end
      else
        raise EJSONDecodeException.Create(
          'JSON Unrecognized value type'+ExVicinity(i));
      {$ENDIF}

      if n<>0 then
       begin
        //add to chain
        if top=0 then
          FNodes[head].Child:=n//assert was 0
        else
          FNodes[top].Next:=n;//assert was 0
        top:=n;
        if firstItem then //an object or array starts
         begin
          inc(i);
          m:=@FNodes[n];
          m.F2:=head;//assert was 0
          m.Next:=top;//assert was 0
          head:=n;
          top:=0;
          firstItem:=true;
         end;
       end;
      if not firstItem then
       begin
        b:=true;
        while b do
         begin
          xt:=SkipWhiteSpace;
          if xt=xtBClose then //']'
           begin
            if not IsArray then
              raise EJSONDecodeException.Create(
                'JSON Unexpected "]" inside of document'+ExVicinity(i));
           end
          else
          if xt=xtAClose then //'}'
           begin
            if IsArray then
              raise EJSONDecodeException.Create(
                'JSON Unexpected "}" and end of array'+ExVicinity(i));
           end
          else
            b:=false;
          if b then
           begin
            inc(i);
            m:=@FNodes[head];
            m.ValueLength:=i-m.ValueIndex;
            //pop from stack
            if head=0 then
             begin
              if SkipWhiteSpace<>xtInvalid then
                raise EJSONDecodeException.Create(
                  'JSON has unexpected data after root document '+ExVicinity(i));
             end
            else
             begin
              n:=head;
              m:=@FNodes[n];
              head:=m.F2;
              top:=m.Next;
              m.Next:=0;
              m.F2:=0;
              IsArray:=(FNodes[head].F1 and jfd_Mask)=jfdArray;
             end;
           end;
         end;
       end;
     end;
    if head<>0 then
     begin
      vi:=0;
      while head<>0 do
       begin
        inc(vi);
        m:=@FNodes[head];
        head:=m.F2;
        m.Next:=0;
        m.F2:=0;
       end;
      raise EJSONDecodeException.Create(
        'JSON with '+IntToStr(vi)+' objects or arrays not closed');
     end;


  finally
    {$IFNDEF JSONDOC_NO_LOAD_AFTER_ERROR}
    while head<>0 do
     begin
      m:=@FNodes[head];
      head:=m.F2;
      m.Next:=0;
      m.F2:=0;
     end;
    {$ENDIF}
    if DataIndex<>0 then FNodes[DataIndex].Next:=RestoreNext;
    {$IFDEF JSONDOC_THREADSAFE}
    LeaveCriticalSection(FLock);
    {$ENDIF}
  end;
end;

function TJSONMemBank.ExVicinity(di:cardinal):WideString;
const
  VicinityExtent=12;
var
  i:integer;
  dj:cardinal;
begin
  dj:=VicinityExtent;
  if di+dj>=json.DataIndex then
    dj:=json.DataIndex-di;
  if di<=VicinityExtent then
    Result:=#13#10'(#'+IntToStr(di)+')"'+Copy(json.Data,1,di-1)+
      ' >>> '+json.Data[di]+' <<< '+Copy(json.Data,di+1,dj)+'"'
  else
    Result:=#13#10'(#'+IntToStr(di)+')"...'+
      Copy(json.Data,di-VicinityExtent,VicinityExtent)+
      ' >>> '+json.Data[di]+' <<< '+Copy(json.Data,di+1,dj)+'"';
  for i:=1 to Length(Result) do
    if word(Result[i])<32 then Result[i]:='|';
end;

function TJSONMemBank.Key(n:integer):WideString;
var
  ki,kl:integer;
  m:PJSONMemNode;
begin
  //assert (n>0) and (n<FNodesIndex)
  m:=@FNodes[n];
  ki:=m.KeyIndex;
  kl:=m.KeyLength;
  case m.F1 and jfk_Mask of
    jfkNoEsc,
    jfkLoose:
     begin
      //Result:=Copy(json.Data,ki,kl);
      SetLength(Result,kl);
      Move(json.Data[ki],Result[1],kl*w2);
     end;
    jfkWithEsc,
    jfkPascalString:
      Result:=GetStringValue(ki,kl);
    else
      raise EJSONException.Create('Unexpected key type');
  end;
end;

function TJSONMemBank.GetStringValue(vi,vl:integer):WideString;
var
  vn,ii,di,u,v,w:integer;
begin
  //assert n<FNodesIndex
  //vi:=FNodes[n].ValueIndex;
  //vl:=FNodes[n].ValueLength;
  //assert vi<=l
  //assert vi+vl<=l
  {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
  if (json.Data[vi]='''') or (json.Data[vi]='#') then
   begin
    SetLength(Result,vl);
    ii:=1;
    di:=vi;
    vn:=vi+vl;
    while di<vn do
     begin
      case word(json.Data[di]) of
        word(''''):
         begin
          inc(di);
          u:=0;
          while (di<vn) and (u=0) do
           begin
            if json.Data[di]='''' then
             begin
              inc(di);
              if (di<=vl) and (json.Data[di]='''') then
               begin
                Result[ii]:='''';
                inc(ii);
                inc(di);
               end
              else
                u:=1;
             end
            else
             begin
              Result[ii]:=json.Data[di];
              inc(ii);
              inc(di);
             end;
           end;
         end;
        word('#'):
         begin
          inc(di);
          if (di<vn) and (json.Data[di]='$') then
           begin
            w:=0;
            u:=0;
            inc(di);
            while (u<4) and (di<vn) and (word(json.Data[di]) in [$30..$39,$41..$5A,$61..$7A]) do
             begin
              if di=vl then raise EJSONDecodeException.Create(
                'JSON Incomplete espace sequence'+ExVicinity(di));
              v:=word(json.Data[di]);
              case v of
                $30..$39:w:=(w shl 4) or (v and $F);
                $41..$5A,$61..$7A:w:=(w shl 4) or ((v and $1F)+9);
                else raise EJSONDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity(di));
              end;
              inc(di);
              inc(u);
             end;
            Result[ii]:=WideChar(w);
            inc(ii);
           end
          else
           begin
            w:=0;
            u:=0;
            while (u<5) and (di<vn) and (word(json.Data[di]) in [$30..$39]) do
             begin
              if di=vn then raise EJSONDecodeException.Create(
                'JSON Incomplete espace sequence'+ExVicinity(di));
              w:=w*10+(word(json.Data[di]) and $F);
              inc(di);
              inc(u);
             end;
            Result[ii]:=WideChar(w);
            inc(ii);
           end;
         end;
        else raise EJSONDecodeException.Create(
          'JSON Unknown pascal string syntax'+ExVicinity(di));
      end;
     end;
    SetLength(Result,ii-1);
   end
  else
  {$ENDIF}
   begin
    {$IFDEF JSONDOC_JSON_STRICT}
    //assert json.Data[vi]='"'
    //assert json.Data[vi+vl]='"';
    inc(i1);
    {$ELSE}
    if json.Data[vi]='"' then inc(vi);
    {$ENDIF}
    SetLength(Result,vl);
    ii:=1;
    di:=vi;
    vn:=vi+vl;
    while di<vn do
     begin
      //assert ii<=Length(Result);
      u:=di;
      while (u<vn) and (json.Data[u]<>'\') do inc(u);
      v:=u-di;
      if v<>0 then
       begin
        Move(json.Data[di],Result[ii],v*w2);
        inc(di,v);
        inc(ii,v);
       end;
      if (di<vn) and (json.Data[di]='\') then
       begin
        inc(di);
        case word(json.Data[di]) of
          word('"'),word('\'),word('/'):Result[ii]:=json.Data[di];
          word('b'):Result[ii]:=#8;
          word('t'):Result[ii]:=#9;
          word('n'):Result[ii]:=#10;
          word('f'):Result[ii]:=#12;
          word('r'):Result[ii]:=#13;
          word('x'):
           begin
            inc(di);
            if di=vn then raise EJSONDecodeException.Create(
              'JSON Incomplete espace sequence'+ExVicinity(di));
            v:=word(json.Data[di]);
            case v of
              $30..$39:w:=(v and $F) shl 4;
              $41..$5A,$61..$7A:w:=((v and $1F)+9) shl 4;
              else raise EJSONDecodeException.Create(
                'JSON Invalid espace sequence'+ExVicinity(di));
            end;
            inc(di);
            if di=vn then raise EJSONDecodeException.Create(
              'JSON Incomplete espace sequence'+ExVicinity(di));
            v:=word(json.Data[di]);
            case v of
              $30..$39:w:=w or (v and $F);
              $41..$5A,$61..$7A:w:=w or ((v and $1F)+9);
              else raise EJSONDecodeException.Create(
                'JSON Invalid espace sequence'+ExVicinity(di));
            end;
            Result[ii]:=WideChar(w);
           end;
          word('u'):
           begin
            w:=0;
            for u:=0 to 3 do
             begin
              inc(di);
              if di=vn then raise EJSONDecodeException.Create(
                'JSON Incomplete espace sequence'+ExVicinity(di));
              v:=word(json.Data[di]);
              case v of
                $30..$39:w:=(w shl 4) or (v and $F);
                $41..$5A,$61..$7A:w:=(w shl 4) or ((v and $1F)+9);
                else raise EJSONDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity(di));
              end;
             end;
            Result[ii]:=WideChar(w);
           end;
          else raise EJSONDecodeException.Create(
            'JSON Unknown escape sequence'+ExVicinity(di));
        end;
        inc(di);
        inc(ii);
       end;
     end;
    if ii-1<>vl then SetLength(Result,ii-1);
   end;
end;

function TJSONMemBank.StartArray: integer;
var
  m:PJSONMemNode;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FNodeIndex=FNodeSize then
     begin
      inc(FNodeSize,NodeGrowStep);
      SetLength(FNodes,FNodeSize);
     end;
    Result:=FNodeIndex;
    m:=@FNodes[FNodeIndex];
    inc(FNodeIndex);

    m.KeyIndex:=0;
    m.KeyLength:=0;
    m.ValueIndex:=0;
    m.ValueLength:=0;
    m.Next:=0;
    m.Child:=0;
    m.F1:=jfkNoEsc or jfdArray;//?
    m.F2:=0;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONMemBank.Add(BaseIndex: integer; const Data: WideString;
  Doc: IJSONDocument): integer;
var
  k:WideString;
  l:IJSONMemBankLoadable;
  l0,l1,l2:cardinal;
  i,f:integer;
  m:PJSONMemNode;
begin
  Result:=0;
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    i:=BaseIndex;
    if (i<0) or (i>=FNodeIndex) then
      raise EJSONException.Create('NodeIndex out of range');
    m:=@FNodes[i];
    i:=m.Child;
    if i=0 then
      m.Child:=FNodeIndex
    else
     begin
      m:=@FNodes[i];
      while m.Next<>0 do
       begin
        i:=m.Next;
        m:=@FNodes[i];
        inc(Result);
       end;
      m.Next:=FNodeIndex;
     end;

    k:=IntToStr(Result);
    f:=jfdRawJSON;

    json.Append('|');
    l0:=json.DataIndex;
    json.Append(k);
    json.Append('|');
    l1:=json.DataIndex;
    if Doc=nil then
      if Data='' then
        f:=jfdNull //json.Append('null')
      else
        json.Append(Data) //TODO: check valid JSON?
    else
      if Doc.QueryInterface(IID_IJSONMemBankLoadable,l)=S_OK then
        l.Build(@json,0) //TODO: try to keep NodeIndex when same MemBank
      else
        json.Append(Doc.ToString);
    l2:=json.DataIndex-l1;

    if FNodeIndex=FNodeSize then
     begin
      inc(FNodeSize,NodeGrowStep);
      SetLength(FNodes,FNodeSize);
     end;

    i:=FNodeIndex;
    m:=@FNodes[i];
    inc(FNodeIndex);

    m.KeyIndex:=l0+1;
    m.KeyLength:=Length(k);//l1-l0-2;
    m.ValueIndex:=l1+1;
    m.ValueLength:=l2;
    m.Next:=0;
    m.Child:=0;
    m.F1:=jfkNoEsc or f;
    m.F2:=0;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TJSONDocument }

procedure TJSONDocument.AfterConstruction;
begin
  inherited;
  FMemBank:=nil;
  FRoot:=nil;
  FFirst:=nil;
  FLast:=nil;
  FLoadIndex:=1;
  {$IFNDEF JSONDOC_THREADSAFE}
  FGotItem:=nil;
  {$ENDIF}
  FUseIJSONArray:=JSON_UseIJSONArray;
  FUseIJSONDocArray:=JSON_UseIJSONDocArray
end;

destructor TJSONDocument.Destroy;
   procedure ClearNode(n:PKeyValueNode);
   var
     i:NativeUInt;
     p,p1:PKeyValueItem;
   begin
     //assert n<>nil
     if IsItem(n,p) then
       while p<>nil do
        begin
         p1:=p;
         p:=p.More;
         try
           p1.Key:='';
           VarClear(p1.Value);
         except
           //silent
         end;
         Dispose(p1);//FreeMem(p1);
        end
     else
      begin
       for i:=0 to KeyValueNodeHashMask do
         if n[i]<>nil then
           ClearNode(n[i]);
       Dispose(n);//FreeMem(n);
      end;
   end;
begin
  FMemBank:=nil;
  try
    if FRoot<>nil then ClearNode(FRoot);
  finally
    FRoot:=nil;
    FFirst:=nil;
    FLast:=nil;
  end;
  inherited;
end;

function TJSONDocument.LookUpItem(const Key:WideString):PKeyValueItem;
var
  h,h0,h1:NativeUInt;
  n:PKeyValueNode;
begin
  //assert caller does locking
  h:=kvsHash(Key);
  {$IFNDEF JSONDOC_THREADSAFE}
  if (FGotItem<>nil) and (FGotItem.Hash=h) and (FGotItem.Key=Key) then
   begin
    Result:=FGotItem;
    Exit;
   end;
  {$ENDIF}
  h1:=h;
  n:=FRoot;
  Result:=nil;
  while n<>nil do
    if IsItem(n,Result) then
      n:=nil //end loop
    else
     begin
      h0:=h1 and KeyValueNodeHashMask;
      h1:=h1 shr KeyValueNodeHashBits;
      n:=n[h0];
     end;
  while (Result<>nil) and not((Result.Hash=h) and (Result.Key=Key)) do
    Result:=Result.More;
  {$IFNDEF JSONDOC_THREADSAFE}
  if Result<>nil then FGotItem:=Result;
  {$ENDIF}
end;

function TJSONDocument.StoreItem(const Key:WideString):PKeyValueItem;
const
  KeyValueNodeMaxLevel=(SizeOf(NativeUInt)*8+KeyValueNodeHashBits-1) div KeyValueNodeHashBits;
var
  //assert caller does locking
  r:PPKeyValueNode;
  h,h0,h1,c,l:NativeUInt;
  n:PKeyValueNode;
  p,p1,p2:PKeyValueItem;
begin
  h:=kvsHash(Key);
  h1:=h;
  l:=0;
  r:=@FRoot;
  p:=nil;
  while p=nil do
    if r^=nil then
      r^:=NewItem(h,Key,p)
    else
      if not IsItem(r^,p) then
       begin
        h0:=h1 and KeyValueNodeHashMask;
        h1:=h1 shr KeyValueNodeHashBits;
        inc(l);
        r:=@r^[h0];
       end;
  c:=0;
  p1:=p;
  while (p<>nil) and not((p.Hash=h) and (p.Key=Key)) do
   begin
    p:=p.More;
    inc(c);
   end;
  if p=nil then
    if (c>=KeyValueItemMaxChain) and (l<KeyValueNodeMaxLevel) then
     begin
      //convert to node
      n:=NewNode(r^);
      h0:=h1 and KeyValueNodeHashMask;
      n[h0]:=NewItem(h,Key,p);
      p2:=p1;
      while p2<>nil do
       begin
        p1:=p2;
        p2:=p2.More;
        p1.More:=nil;
        h0:=(p1.Hash shr (KeyValueNodeHashBits*l)) and KeyValueNodeHashMask;
        if n[h0]<>nil then IsItem(n[h0],p1.More);//assume true since set in this loop
        n[h0]:=AsItem(p1);
       end;
     end
    else
     begin
      //add to chain
      r^:=NewItem(h,Key,p);
      p.More:=p1;
     end;
  //assert p.Hash=h, p.Key=Key
  if p.LoadIndex<>FLoadIndex then
   begin
    p.LoadIndex:=FLoadIndex;
    p.Next:=nil;
    if FFirst=nil then
     begin
      FFirst:=p;
      FLast:=p;
     end
    else
     begin
      FLast.Next:=p;
      FLast:=p;
     end;
   end;
  Result:=p;
end;

function TJSONDocument.Get_Item(const Key: WideString): Variant;
var
  p:PKeyValueItem;
  v:PVariant;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if Self=nil then
      p:=nil
    else
      p:=LookUpItem(Key);
    v:=v00(p,false);
    if v=nil then Result:=Null else Result:=v^;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.v00(Item: PKeyValueItem;
  FailWhenMissing: boolean): PVariant;
begin
  //assert caller does FLock
  if (Item=nil) or (Item.LoadIndex<>FLoadIndex) then
    if FailWhenMissing then
      raise ERangeError.Create('Out of range')
    else
      Result:=nil
  else
   begin
    //assert FItem.LoadIndex=FLoadIndex and in FFirst..FLast sequence
    if Item.NodeIndex<>0 then
     begin
      Item.Value:=v1(Item.NodeIndex);
      Item.NodeIndex:=0;
     end;
    Result:=@Item.Value;
   end;
end;

function TJSONDocument.v1(NodeIndex: integer): Variant;
var
  n:integer absolute NodeIndex;
  b:TJSONMemBank;
  d:IJSONMemBankLoadable;
  v64:int64;
  v64p:packed record v64lo,v64hi:integer; end absolute v64;
  ods:char;
  bi,ai,an:integer;
  at,vt:TVarType;
  m:PJSONMemNode;
  IsDocArray:boolean;
begin
  //assert caller does FLock
  b:=FMemBank.Bank as TJSONMemBank;
  m:=@b.FNodes[n];
  case m.F1 and jfd_Mask of
    jfdObject:
     begin
{
      //check existing instance
      if (ReUse<>nil) and
        (TVarData(ReUse^).VType=varUnknown) and
        (TVarData(ReUse^).VUnknown<>nil) and
        (IUnknown(ReUse^).QueryInterface(IID_IJSONMemBankLoadable,d)=S_OK) then
        //use/store below
      else
}      
      d:=TJSONDocument.Create;
      d.LoadBank(FMemBank,n);
      Result:=IJSONDocument(d);
     end;

    jfdArray:
     begin
      //determine output array type
      an:=0;
      at:=varEmpty;//as initial value, see below
      IsDocArray:=true;//default, see below
      bi:=m.Child;
      while bi<>0 do
       begin
        case b.FNodes[bi].F1 and jfd_Mask of
          jfdObject:vt:=varUnknown;//IJSONDocument
          jfdArray:vt:=varVariant;//variant array
          jfdNull:vt:=varNull;
          jfdBoolTrue,jfdBoolFalse:vt:=varBoolean;
          jfdStringNoEsc,jfdStringWithEsc,jfdStringPascal:vt:=varOleStr;
          jfdInt8:vt:=varShortInt;
          jfdInt16:vt:=varSmallint;
          jfdInt32:vt:=varInteger;
          jfdInt64:vt:=varInt64;
          jfdFloat:vt:=varDouble;
          else vt:=varVariant;//raise?
        end;
        if IsDocArray and not((b.FNodes[bi].F1 and jfd_Mask)
          in [jfdObject,jfdNull]) then IsDocArray:=false;
        case at of
          varEmpty:at:=vt;//initial value
          varShortInt,varByte://i1,u1
            case vt of
              varSmallInt,varInteger,varSingle,varDouble,
              varLongWord,varInt64,$0015:
                at:=vt;
              varShortInt:
                ;//at:=varShortInt;
              else
                at:=varVariant;
            end;
          varSmallint,varWord://i2,u2
            case vt of
              varInteger,varSingle,varDouble,varLongWord,varInt64,$0015:
                at:=vt;
              varSmallInt,
              varShortInt,varByte,varWord:
                ;//at:=varSmallInt;
              else
                at:=varVariant;
            end;
          varInteger,varLongWord://i4,u4
            case vt of
              varSingle,varDouble,varInt64,$0015:
                at:=vt;
              varSmallInt,varInteger,
              varShortInt,varByte,varWord,varLongWord:
                ;//at:=varInteger;
              else
                at:=varVariant;
            end;
          varInt64,$0015://i8
            case vt of
              varSingle,varDouble:
                at:=vt;
              varSmallInt,varInteger,
              varShortInt,varByte,varWord,varLongWord,varInt64,$0015:
                ;//at:=varInt64;
              else
                at:=varVariant;
            end;
          varSingle:
            case vt of
              varDouble:
                at:=vt;
              varSmallInt,varInteger,varSingle,
              varShortInt,varByte,varWord,varLongWord:
                ;//at:=varSingle
              else
                at:=varVariant;
            end;
          varDouble:
            case vt of
              varSmallInt,varInteger,varSingle,varDouble,
              varShortInt,varByte,varWord,varLongWord:
                ;//at:=varDouble
              else
                at:=varVariant;
            end;
          varVariant:
            ;//Already creating an VarArray of varVariant
          else
            if at<>vt then at:=varVariant;
        end;
        inc(an);
        bi:=b.FNodes[bi].Next;
       end;
      if at=varEmpty then at:=varVariant;//varNull?

{
      //check existing instance
      if (ReUse<>nil) and
        (TVarData(ReUse^).VType=varUnknown) and
        (TVarData(ReUse^).VUnknown<>nil) and
        (IUnknown(ReUse^).QueryInterface(IID_IJSONMemBankLoadable,d)=S_OK) then
        //assert (IsDocArray and d is IJSONDocArray) or (at=varVariant and d is IJSONArray/TVarJSONArray)
       begin
        d.LoadBank(FMemBank,n);
        Result:=d;
       end
      else
}      
      if IsDocArray and FUseIJSONDocArray then
       begin
        d:=TJSONDocArray.Create;
        d.LoadBank(FMemBank,n);
        Result:=IJSONDocArray(d);
       end
      else
       begin
        Result:=VarArrayCreate([0,an-1],at);
        ai:=0;
        bi:=m.Child;
        while bi<>0 do
         begin
          Result[ai]:=v1(bi);
          inc(ai);
          bi:=b.FNodes[bi].Next;
         end;
        if FUseIJSONArray then
          Result:=TVarJSONArray.Create(Result) as IJSONArray
       end;
     end;

    jfdNull:Result:=Null;
    jfdBoolTrue:Result:=true;
    jfdBoolFalse:Result:=false;
    jfdStringNoEsc:
      Result:=Copy(b.json.Data,m.ValueIndex,m.ValueLength);
    jfdStringWithEsc,jfdStringPascal://TODO: split into GetEscStr,GetPascalStr
      Result:=b.GetStringValue(m.ValueIndex,m.ValueLength);
    jfdInt8:
      Result:=SmallInt(m.ValueLength);
    jfdInt16:
      Result:=word(m.ValueLength);
    jfdInt32:
      Result:=integer(m.ValueLength);
    jfdInt64:
     begin
      v64p.v64hi:=m.ValueIndex;
      v64p.v64lo:=m.ValueLength;
      Result:=v64;
     end;
    jfdFloat:
     begin
      //try except EConvertError?
      {$if CompilerVersion >= 24}
      ods:= FormatSettings.DecimalSeparator;
      {$else}
      ods:=DecimalSeparator;
      {$ifend}
      try
        {$if CompilerVersion >= 24}
        FormatSettings.DecimalSeparator:='.';
        {$else}
        DecimalSeparator:='.';
        {$ifend}
        Result:=StrToFloat(Copy(b.json.Data,m.ValueIndex,m.ValueLength));
      finally
        {$if CompilerVersion >= 24}
        FormatSettings.DecimalSeparator:=ods;
        {$else}
        DecimalSeparator:=ods;
        {$ifend}
      end;
     end;
    //else raise?
  end;
end;

procedure TJSONDocument.Set_Item(const Key: WideString; const Value: Variant);
var
  p:PKeyValueItem;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //if ((VarType(Value) and varArray)<>0) and (VarArrayDimCount(v)>1) then
    //  raise EJSONException.Create(
    //    'VarArray: multi-dimensional arrays not supported');
    p:=StoreItem(Key);
    //TODO: VarMove?
    p.Value:=Value;
    p.NodeIndex:=0;
    //FDirty:=true;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.v0(const Key: WideString): pointer;
var
  p:PKeyValueItem;
  n:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    p:=StoreItem(Key);
    n:=p.NodeIndex;
    if n<>0 then
     begin
      p.Value:=v1(n);
      p.NodeIndex:=0;
     end;
    Result:=@p.Value;
    //FDirty:=true;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Parse(const JSONData: WideString);
var
  b:TJSONMemBank;
  n:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //Clear;? let caller decide.

    if FMemBank=nil then
     begin
      b:=TJSONMemBank.Create;
      FMemBank:=b;
     end
    else
     begin
      b:=FMemBank.Bank as TJSONMemBank;
     end;

    {$IFNDEF JSONDOC_NO_LOAD_AFTER_ERROR}
    try    
    {$ENDIF}
    
      b.Parse(n,0,JSONData);
      LoadBank(b,n);

    {$IFNDEF JSONDOC_NO_LOAD_AFTER_ERROR}
    except
      on EJSONDecodeException do
       begin
        LoadBank(b,n);
        raise;
       end;
    end;
    {$ENDIF}

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.LoadBank(Bank: IUnknown; Index: integer);
var
  b:TJSONMemBank;
  bi:integer;
  p:PKeyValueItem;
  d:IJSONMemBankLoadable;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    FMemBank:=Bank as IJSONMemBank;
    b:=FMemBank.Bank as TJSONMemBank;
    if (Index<0) or (Index>=b.FNodeIndex) then
      raise EJSONException.Create('NodeIndex out of range');
    inc(FLoadIndex);
    if FLoadIndex=0 then inc(FLoadIndex);
    FFirst:=nil;
    FLast:=nil;
    bi:=b.FNodes[Index].Child;
    while bi<>0 do
     begin
      p:=StoreItem(b.Key(bi));
      p.NodeIndex:=bi;
      if (b.FNodes[bi].F1 and jfd_Mask) in [jfdObject,jfdArray] then
       begin
        if (TVarData(p.Value).VType=varUnknown) and
          (TVarData(p.Value).VUnknown<>nil) and
          (IUnknown(p.Value).QueryInterface(IID_IJSONMemBankLoadable,d)=S_OK) then
          try
            d.LoadBank(Bank,bi);
            p.NodeIndex:=0;//mark element loaded
          finally
            d:=nil;
          end;
       end
      else
        VarClear(p.Value);
      bi:=b.FNodes[bi].Next;
     end;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONBuilder.EncodeStr(const x:WideString);
const
  hex:array[0..15] of WideChar=(
    '0','1','2','3','4','5','6','7',
    '8','9','A','B','C','D','E','F');
var
  i,j,l:integer;
  w:word;
begin
  l:=Length(x);
  Append('"');
  i:=1;
  j:=1;
  while i<=l do
   begin
    w:=word(x[i]);
    if w in [0..31,word('"'),word('\')] then
     begin
      if i<>j then Append(Copy(x,j,i-j));
      Append('\');
      case w of
        8:Append('b');
        9:Append('t');
        10:Append('n');
        12:Append('f');
        13:Append('r');
        word('"'),word('\'):Append(x[i]);
        else
         begin
          Append('u');
          Append(hex[w shr 12]);
          Append(hex[w shr 8 and $F]);
          Append(hex[w shr 4 and $F]);
          Append(hex[w and $F]);
         end;
      end;
      inc(i);
      j:=i;
     end
    else
      inc(i);
   end;
  if i<>j then Append(Copy(x,j,i-j));
  Append('"');
end;

procedure TJSONBuilder.VarToStr(const v: Variant);
var
  uu:IUnknown;
  dm:IJSONMemBankLoadable;
  d:IJSONDocument;
  da:IJSONArray;
begin
  case TVarData(v).VType and varTypeMask of
    varNull:Append('null');
    varSmallint,varInteger,varShortInt,
    varByte,varWord,varLongWord,varInt64:
      Append(VarToWideStr(v));
    varSingle,varDouble,varCurrency:
      Append(FloatToStr(v));//?
    varDate:
      //Append(FloatToStr(VarToDateTime(v)));//?
      Append('"'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',
        VarToDateTime(v))+'"');
    varOleStr,varString,$0102:
      EncodeStr(VarToWideStr(v));
    varBoolean:
      if v then Append('true') else Append('false');
    varUnknown:
     begin
      uu:=IUnknown(v);
      if uu=nil then
        Append('null')
      else
      if uu.QueryInterface(IID_IJSONMemBankLoadable,dm)=S_OK then
       begin
        dm.Build(@Self,0);
        dm:=nil;
       end
      else
      if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
       begin
        //revert to ToString
        Append(d.ToString);
        d:=nil;
       end
      else
      if uu.QueryInterface(IID_IJSONArray,da)=S_OK then
       begin
        //TODO: re-do indenting [IFDEF JSONDOC_STOREINDENTING]
        Append(da.ToString);
        da:=nil;
       end
      else
      //IRegExp2? IStream? IPersistStream?
        raise EJSONEncodeException.Create(
          'No supported interface found on object');
     end;
    else raise EJSONEncodeException.Create(
      'Unsupported variant type '+IntToHex(TVarData(v).VType,4));
  end;
end;

function TJSONDocument.JSONToString: WideString;
var
  w:TJSONBuilder;
begin
  if Self=nil then
    Result:='null'
  else
   begin
    w.Clear;
    Build(@w,0);
    Result:=w.Output;
   end;
end;

{$IFDEF JSONDOC_STOREINDENTING}
const
  tabs=#13#10#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9
    +#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9;

procedure TJSONBuilder.Write(const x; l: Cardinal);
var
  l1,l2:cardinal;
begin
  l1:=DataIndex+l;
  if l1>DataSize then
   begin
    if (l1 and (JSONBuilderGrowStep-1))=0 then
      l2:=l1
    else
      l2:=(l1 and not(JSONBuilderGrowStep-1))+JSONBuilderGrowStep;
    SetLength(Data,l2);
   end;
  Move(x,Data[DataIndex+1],l*w2);
  DataIndex:=l1;
end;

procedure wr(w:PJSONBuilder;const xx,yy,zz:WideString);
var
  xi,xj,xk,xl,yi,yl,zl:cardinal;
begin
  xi:=1;
  xl:=Length(xx);
  yl:=Length(yy);//assert <>0
  zl:=Length(zz);
  while xi<=xl do
   begin
    xj:=xi;
    yi:=0;
    while (xi<=xl) and (yi<yl) do
     begin
      if (xx[xi]=yy[1]) and (xi+yl<=xl) then
       begin
        while (yi<yl) and (xx[xi+yi]=yy[1+yi]) do inc(yi);
        if yi<yl then inc(xi);
       end
      else
        inc(xi);
     end;
    xk:=xi-xj;
    w.Write(xx[xj],xk);
    if xi<=xl then
     begin
      inc(xi,yl);
      w.Write(zz[1],zl);
     end;
   end;
end;

{$ENDIF}

procedure TJSONDocument.Build(Builder: pointer; TabIndex: integer);
const
  stackGrowStep=$20;
var
  e:IJSONEnumerator;
  IsArray,firstItem:boolean;
  stack:array of record
    e:IJSONEnumerator;
    IsArray:boolean;
  end;
  stackLength,stackIndex:integer;
  w:PJSONBuilder absolute Builder;
  function ExTrace:string;
  var
    i:integer;
  begin
    if IsArray then
      Result:=' #'+e.Key
    else
      Result:=' "'+e.Key+'"';
    i:=stackIndex;
    while i<>0 do
     begin
      dec(i);
      if stack[i].IsArray then
        Result:=' #'+stack[i].e.Key+Result
      else
        Result:=' "'+stack[i].e.Key+'"'+Result;
     end;
  end;
  procedure Push(const NewEnum:IJSONEnumerator;NewIsArray:boolean);
  begin
    if stackIndex=stackLength then
     begin
      inc(stackLength,stackGrowStep);
      SetLength(stack,stackLength);
     end;
    stack[stackIndex].e:=e;
    stack[stackIndex].IsArray:=IsArray;
    inc(stackIndex);
    e:=NewEnum;
    IsArray:=NewIsArray;
    if IsArray then w.Append('[') else w.Append('{');
    firstItem:=true;
    {$IFDEF JSONDOC_STOREINDENTING}
    inc(TabIndex);
    {$ENDIF}
  end;
var
  ods:char;
  vt:TVarType;
  uu:IUnknown;
  dl:IJSONMemBankLoadable;
  d:IJSONDocument;
  de:IJSONEnumerable;
  da1:IJSONArray;
  da:IJSONDocArray;
begin
  if Self=nil then
   begin
    w.Append('null');
    Exit;
   end;
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
  
    //TODO: estimate length and w.Reserve?

    w.Append('{');

    stackLength:=0;
    stackIndex:=0;
    e:=TJSONEnumerator.Create(Self);
    IsArray:=false;

    {$if CompilerVersion >= 24}
    ods:=FormatSettings.DecimalSeparator;
    {$else}
    ods:=DecimalSeparator;
    {$ifend}
    try
      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:='.';
      {$else}
      DecimalSeparator:='.';
      {$ifend}

      //w('{');//see above
      firstItem:=true;
      {$IFDEF JSONDOC_STOREINDENTING}
      inc(TabIndex);
      {$ENDIF}
      while e<>nil do
        if e.Next then
         begin
          if firstItem then firstItem:=false else w.Append(',');
          {$IFDEF JSONDOC_STOREINDENTING}
          w.Append(Copy(tabs,1,TabIndex+2));
          {$ENDIF}
          if not IsArray then
           begin
            w.EncodeStr(e.Key);
            {$IFDEF JSONDOC_STOREINDENTING}
            w.Append(': ');
            {$ELSE}
            w.Append(':');
            {$ENDIF}
           end;
          //write value
          vt:=TVarData(PVariant(e.v0)^).VType;
          //if (vt and varByRef)<>0 then
          //  raise EJSONEncodeException.Create('VarByRef: not implemented'+ExTrace);
          if (vt and varArray)=0 then
           begin
            //not an array, plain value
            //TODO: deduplicate with TJSONBuilder.VarToStr (but somehow keep Push(NewEnumerator))
            case vt and varTypeMask of
              varNull:w.Append('null');
              varSmallint,varInteger,varShortInt,
              varByte,varWord,varLongWord,varInt64:
                w.Append(VarToWideStr(PVariant(e.v0)^));
              varSingle,varDouble,varCurrency:
                w.Append(FloatToStr(PVariant(e.v0)^));//?
              varDate:
               begin
                //w(FloatToStr(VarToDateTime(v)));//?
                w.Append('"');
                //TODO:"yyyy-mm-dd hh:nn:ss.zzz"? $date?
                w.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',VarToDateTime(PVariant(e.v0)^)));
                w.Append('"');
               end;
              varOleStr,varString,$0102:
                w.EncodeStr(VarToWideStr(PVariant(e.v0)^));
              varBoolean:
                if PVariant(e.v0)^ then w.Append('true') else w.Append('false');
              varUnknown:
               begin
                uu:=IUnknown(PVariant(e.v0)^);
                if uu=nil then
                  w.Append('null')
                else
                if uu.QueryInterface(IID_IJSONEnumerable,de)=S_OK then
                 begin
                  Push(de.NewEnumerator,false);
                  de:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONMemBankLoadable,dl)=S_OK then
                 begin
                  dl.Build(Builder,TabIndex);
                  dl:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
                 begin
                  //revert to ToString
                  w.Append(d.ToString);
                  d:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
                 begin
                  {$IFDEF JSONDOC_STOREINDENTING}
                  wr(w,da.ToString,#13#10,Copy(tabs,1,TabIndex+2));
                  {$ELSE}
                  w.Append(da.ToString);
                  {$ENDIF}
                  da:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONArray,da1)=S_OK then
                 begin
                  Push(TJSONArrayEnumerator.Create(da1),true);
                  da1:=nil;
                 end
                else
                //IRegExp2? IStream? IPersistStream?
                  raise EJSONEncodeException.Create(
                    'No supported interface found on object'+ExTrace);
               end;
              else raise EJSONEncodeException.Create(
                'Unsupported variant type '+IntToHex(vt,4)+ExTrace);
            end;
           end
          else
           begin
            //TODO: if (vt and varTypeMask)=varByte then BLOB?
            Push(TVarArrayEnumerator.Create(e.v0),true);
           end;
         end
        else
         begin
          {$IFDEF JSONDOC_STOREINDENTING}
          dec(TabIndex);
          if not firstItem then w.Append(Copy(tabs,1,TabIndex+2));
          {$ENDIF}
          if IsArray then w.Append(']') else w.Append('}');
          firstItem:=false;
          if stackIndex=0 then
            e:=nil //end loop
          else
           begin
            //pop from stack
            dec(stackIndex);
            e:=stack[stackIndex].e;
            IsArray:=stack[stackIndex].IsArray;
            stack[stackIndex].e:=nil;
           end;
         end;

    finally
      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:=ods;
      {$else}
      DecimalSeparator:=ods;
      {$ifend}
    end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.ToVarArray: Variant;
var
  p:PKeyValueItem;
  i,n:integer;
begin
  if Self=nil then
   begin
    Result:=Null;
    Exit;
   end;
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    p:=FFirst;
    i:=0;
    while p<>nil do
     begin
      inc(i);
      p:=p.Next;
     end;
    Result:=VarArrayCreate([0,i-1,0,1],varVariant);
    p:=FFirst;
    i:=0;
    while p<>nil do
     begin
      //assert p.LoadIndex=FLoadIndex
      n:=p.NodeIndex;
      if n<>0 then
       begin
        p.Value:=v1(n);
        p.NodeIndex:=0;
       end;
      Result[i,0]:=p.Key;
      Result[i,1]:=p.Value;
      inc(i);
      p:=p.Next;
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Clear;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ELSE}
    FGotItem:=nil;
  {$ENDIF}
    ClearBank(nil);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.ClearBank(MemBank:IJSONMemBank);
var
  p:PKeyValueItem;
  uu:IUnknown;
  dm:IJSONMemBankLoadable;
  d:IJSONDocument;
  da:IJSONDocArray;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ELSE}
    FGotItem:=nil;
  {$ENDIF}
    //FDirty:=false;
    //TODO: if FMemBank<>nil then? if MemBank<>nil then?
    FMemBank:=nil;
    p:=FFirst;
    while p<>nil do
     begin
      if TVarData(p.Value).VType=varUnknown then
       begin
        uu:=IUnknown(p.Value);
        if uu=nil then
          VarClear(p.Value)
        else
        if uu.QueryInterface(IID_IJSONMemBankLoadable,dm)=S_OK then
         begin
          dm.ClearBank(MemBank);
          dm:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
         begin
          d.Clear;
          d:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
         begin
          da.Clear;
          da:=nil;
         end
        else
          VarClear(p.Value);
       end
      else
        VarClear(p.Value);
      p:=p.Next;
     end;
    inc(FLoadIndex);
    if FLoadIndex=0 then inc(FLoadIndex);
    FFirst:=nil;
    FLast:=nil;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Delete(const Key: WideString);
var
  p,p1:PKeyValueItem;
  uu:IUnknown;
  d:IJSONDocument;
  da:IJSONDocArray;
  dl:IJSONMemBankLoadable;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    p:=LookUpItem(Key);
    if (p<>nil) and (p.LoadIndex=FLoadIndex) then
     begin
      if TVarData(p.Value).VType=varUnknown then
       begin
        uu:=IUnknown(p.Value);
        if uu=nil then
          VarClear(p.Value)
        else
        if uu.QueryInterface(IID_IJSONMemBankLoadable,dl)=S_OK then
         begin
          dl.ClearBank(FMemBank);
          dl:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
         begin
          d.Clear;
          d:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
         begin
          da.Clear;
          da:=nil;
         end
        else
          VarClear(p.Value);
       end
      else
        VarClear(p.Value);
      p.LoadIndex:=FLoadIndex-1;
      p.NodeIndex:=0;

      if FFirst=p then
        if FLast=p then
         begin
          FFirst:=nil;
          FLast:=nil;
         end
        else
         begin
          FFirst:=p.Next;
          p.Next:=nil;
         end
      else
       begin
        p1:=FFirst;
        while (p1<>nil) and (p1.Next<>p) do p1:=p1.Next;
        if p1<>nil then
         begin
          p1.Next:=p.Next;
          p.Next:=nil;
          if FLast=p then FLast:=p1;
         end;
       end

     end;
    //else raise?
    //FDirty:=true;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.NewEnumerator: IJSONEnumerator;
begin
  Result:=TJSONEnumerator.Create(Self);
end;

function TJSONDocument.NewEnumeratorSorted: IJSONEnumerator;
begin
  Result:=TJSONEnumeratorSorted.Create(Self);
end;

{ TJSONEnumerator }

constructor TJSONEnumerator.Create(Data: TJSONDocument);
begin
  inherited Create;
  FData:=Data;
  FFirst:=true;
  FItem:=nil;
  //TODO: hook into TJSONDocument destructor?
end;

destructor TJSONEnumerator.Destroy;
begin
  FData:=nil;
  FFirst:=false;
  FItem:=nil;
  inherited;
end;

function TJSONEnumerator.EOF: boolean;
begin
  if FFirst then
    Result:=(FData=nil) or (FData.FFirst=nil)
  else
    Result:=FItem=nil;
end;

function TJSONEnumerator.Next: boolean;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    if FFirst then
     begin
      if FData=nil then
        FItem:=nil
      else
        FItem:=FData.FFirst;
      FFirst:=false;
     end
    else
      if FItem<>nil then
        FItem:=FItem.Next;
    //if assert FItem is null or FItem.LoadIndex=FData.FLoadIndex
    Result:=FItem<>nil;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

function TJSONEnumerator.Get_Key: WideString;
begin
  if FItem=nil then
    raise ERangeError.Create('Out of range')
  else
    Result:=FItem.Key;
end;

function TJSONEnumerator.Get_Value: Variant;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    Result:=FData.v00(FItem,true)^;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

procedure TJSONEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    if FItem=nil then
      raise ERangeError.Create('Out of range')
    else
     begin
      //assert FItem.LoadIndex=FData.FLoadIndex and in FFirst..FLast sequence
      FItem.Value:=Value;
      FItem.NodeIndex:=0;
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

function TJSONEnumerator.v0: pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    Result:=FData.v00(FItem,true);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

{ TJSONEnumeratorSorted }

constructor TJSONEnumeratorSorted.Create(Data: TJSONDocument);
var
  i,l:integer;
  p:PKeyValueItem;

  function wc(const n1,n2:WideString):integer;
  var
    i,l1,l2:integer;
  begin
    //Result:=WideCompareStr(n1,n2);
    l1:=Length(n1);
    l2:=Length(n2);
    i:=1;
    while (i<=l1) and (i<=l2) and (n1[i]=n2[i]) do inc(i);
    if i>l1 then
      if i>l2 then
        Result:=0
      else
        Result:=1
    else
      if i>l2 then
        Result:=-1
      else
        if n1[i]<n2[i] then
          Result:=-1
        else
          Result:=1;//n1[i]>n2[i]
  end;

  procedure qs(a,b:integer);
  var
    p:PKeyValueItem;
    i,j,k:integer;
  begin
    if a<b then
      repeat
        i:=a;
        j:=b;
        k:=(a+b) div 2;
        repeat
          while wc(FItems[i].Key,FItems[k].Key)<0 do inc(i);
          while wc(FItems[j].Key,FItems[k].Key)>0 do dec(j);
          if i<=j then
           begin
            p:=FItems[i];
            FItems[i]:=FItems[j];
            FItems[j]:=p;
            if k=i then k:=j else if k=j then k:=i;
            inc(i);
            dec(j);
           end;
        until i>j;
        if a<j then qs(a,j);
        a:=i;
      until i>=b;
  end;

begin
  inherited Create;
  FData:=Data;
  //TODO: hook into TJSONDocument destructor?
  l:=0;
  p:=Data.FFirst;
  while p<>nil do
   begin
    inc(l);
    p:=p.Next;
   end;
  SetLength(FItems,l);
  p:=Data.FFirst;
  i:=0;
  while p<>nil do
   begin
    FItems[i]:=p;
    inc(i);
    //assert p.LoadIndex=Data.FLoadIndex
    p:=p.Next;
   end;
  //quick sort
  qs(0,l-1);
  FIndex:=-1;
end;

destructor TJSONEnumeratorSorted.Destroy;
begin
  FData:=nil;
  SetLength(FItems,0);
  inherited;
end;

function TJSONEnumeratorSorted.EOF: boolean;
begin
  Result:=FIndex>=Length(FItems);
end;

function TJSONEnumeratorSorted.Next: boolean;
begin
  inc(FIndex);
  Result:=FIndex<Length(FItems);
end;

function TJSONEnumeratorSorted.Get_Key: WideString;
begin
  if (FIndex<0) or (FIndex>=Length(FItems)) then
    raise ERangeError.Create('Out of range')
  else
    Result:=FItems[FIndex].Key;
end;

function TJSONEnumeratorSorted.Get_Value: Variant;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}

    if (FIndex<0) or (FIndex>=Length(FItems)) then
      raise ERangeError.Create('Out of range')
    else
      Result:=FData.v00(FItems[FIndex],true)^;
    
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

procedure TJSONEnumeratorSorted.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try 
  {$ENDIF}
  if (FIndex<0) or (FIndex>=Length(FItems)) then
    raise ERangeError.Create('Out of range')
  else
   begin
    //assert FItems[FIndex].LoadIndex=FData.FLoadIndex and in FFirst..FLast sequence
    FItems[FIndex].Value:=Value;
    FItems[FIndex].NodeIndex:=0;
   end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

function TJSONEnumeratorSorted.v0: pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    if (FIndex<0) or (FIndex>=Length(FItems)) then
      raise ERangeError.Create('Out of range')
    else
      Result:=FData.v00(FItems[FIndex],true);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

{ TVarArrayEnumerator }

constructor TVarArrayEnumerator.Create(const Data: PVariant);
begin
  inherited Create;
  if VarArrayDimCount(Data^)<>1 then
    raise EJSONException.Create('VarArray: multi-dimensional arrays not supported');
  FData:=Data;
  VarClear(FCurrent);
  FIndex:=VarArrayLowBound(Data^,1);
  FMax:=VarArrayHighBound(Data^,1)+1;
  FCurrentIndex:=FIndex-1;
  if FIndex<FMax then dec(FIndex);//see Next
end;

destructor TVarArrayEnumerator.Destroy;
begin
  VarClear(FCurrent);
  FData:=nil;
  inherited;
end;

function TVarArrayEnumerator.EOF: boolean;
begin
  Result:=not(FIndex<FMax);
end;

function TVarArrayEnumerator.Get_Key: WideString;
begin
  Result:=IntToStr(FIndex);
end;

function TVarArrayEnumerator.Get_Value: Variant;
begin
  Result:=FData^[FIndex];
end;

function TVarArrayEnumerator.Next: boolean;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    inc(FIndex);
    Result:=FIndex<FMax;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TVarArrayEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    FData^[FIndex]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarArrayEnumerator.v0: pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FCurrentIndex<>FIndex then
     begin
      FCurrent:=FData^[FIndex];//TODO: keep SafeArray locked for lifetime of TVarArrayEnumerator instance?
      FCurrentIndex:=FIndex;
     end;
    Result:=@FCurrent;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TVarJSONArray }

constructor TVarJSONArray.Create(const Data: Variant);
begin
  inherited Create;
  if (TVarData(Data).VType and varArray)=0 then
    raise EJSONException.Create('TVarJSONArray: array variant expected');
  if VarArrayDimCount(Data)<>1 then
    raise EJSONException.Create('TVarJSONArray: multi-dimensional arrays not supported');
  FData:=Data;
  v1:=VarArrayLowBound(FData,1);
  v2:=VarArrayHighBound(FData,1)+1;
  VarClear(FCurrent);
  FCurrentIndex:=-1;
end;

constructor TVarJSONArray.CreateNoCopy(var Data: Variant);
begin
  inherited Create;
  if (TVarData(Data).VType and varArray)=0 then
    raise EJSONException.Create('TVarJSONArray: array variant expected');
  if VarArrayDimCount(Data)<>1 then
    raise EJSONException.Create('TVarJSONArray: multi-dimensional arrays not supported');
  VarMove(FData,Data);
  v1:=VarArrayLowBound(FData,1);
  v2:=VarArrayHighBound(FData,1)+1;
  VarClear(FCurrent);
  FCurrentIndex:=-1;
end;

destructor TVarJSONArray.Destroy;
begin
  VarClear(FCurrent);
  VarClear(FData);
  inherited;
end;

function TVarJSONArray.ItemsCount: integer;
begin
  Result:=v2-v1;
end;

function TVarJSONArray.Get_Item(Index: integer): Variant;
begin
  if (Index<0) or (Index>=v2-v1) then
    raise ERangeError.Create('Out of range');
  Result:=FData[Index+v1];
end;

procedure TVarJSONArray.Set_Item(Index: integer; const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=v2-v1) then
      raise ERangeError.Create('Out of range');
    FData[Index+v1]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarJSONArray.v0(Index: integer): pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  Result:=nil;//counter warning
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FCurrentIndex<>Index then
     begin
      if (Index<0) or (Index>=v2-v1) then
        raise ERangeError.Create('Out of range');
      FCurrent:=FData[Index+v1];
      FCurrentIndex:=Index;
     end;
    Result:=@FCurrent;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarJSONArray.JSONToString: WideString;
var
  i,wi:integer;
  w:TJSONBuilder;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //TODO: indenting? [IFDEF JSONDOC_STOREINDENTING]
    if v1>v2 then
      Result:='[]'
    else
     begin
      w.Clear;
      i:=v1;
      wi:=w.DataIndex+1;
      while (i<v2) do
       begin
        w.Append(',');
        w.VarToStr(FData[i]);
        inc(i);
       end;
      w.Data[wi]:='[';
      w.Append(']');
      Result:=w.Output;
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TJSONArray }

constructor TJSONArray.Create(Size: integer);
begin
  inherited Create;
  SetLength(FData,Size);
end;

function TJSONArray.ItemsCount: integer;
begin
  Result:=Length(FData);
end;

function TJSONArray.Get_Item(Index: integer): Variant;
begin
  if (Index<0) or (Index>=Length(FData)) then
    raise ERangeError.Create('Out of range');
  Result:=FData[Index];
end;

procedure TJSONArray.Set_Item(Index: integer; const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=Length(FData)) then
      raise ERangeError.Create('Out of range');
    FData[Index]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArray.v0(Index: integer): pointer;
begin
  if (Index<0) or (Index>=Length(FData)) then
    raise ERangeError.Create('Out of range');
  Result:=@FData[Index];
end;

function TJSONArray.JSONToString: WideString;
var
  i,wi:integer;
  w:TJSONBuilder;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //TODO: indenting? [IFDEF JSONDOC_STOREINDENTING]
    if Length(FData)=0 then
      Result:='[]'
    else
     begin
      w.Clear;
      i:=0;
      wi:=w.DataIndex+1;
      while (i<Length(FData)) do
       begin
        w.Append(',');
        w.VarToStr(FData[i]);
        inc(i);
       end;
      w.Data[wi]:='[';
      w.Append(']');
      Result:=w.Output;
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArray.NewEnumerator: IJSONEnumerator;
begin
  Result:=TJSONArrayEnumerator.Create(Self);
end;

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(const Data: IJSONArray);
begin
  inherited Create;
  FData:=Data;
  FMax:=FData.Count;
  if FMax=0 then FIndex:=0 else FIndex:=-1;//see Next;
end;

destructor TJSONArrayEnumerator.Destroy;
begin
  FData:=nil;
  inherited;
end;

function TJSONArrayEnumerator.EOF: boolean;
begin
  Result:=not(FIndex<FMax);
end;

function TJSONArrayEnumerator.Get_Key: WideString;
begin
  Result:=IntToStr(FIndex);
end;

function TJSONArrayEnumerator.Get_Value: Variant;
begin
  Result:=FData[FIndex];
end;

function TJSONArrayEnumerator.Next: boolean;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    inc(FIndex);
    Result:=FIndex<FMax;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONArrayEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    FData[FIndex]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArrayEnumerator.v0: pointer;
begin
  Result:=FData.v0(FIndex);
end;

{ TJSONDocArray }

constructor TJSONDocArray.Create;
begin
  inherited Create;
  FMemBank:=nil;
  FBaseIndex:=0;
  {$IFNDEF JSONDOC_THREADSAFE}
  FCurrentIndex:=-1;
  FCurrentNode:=0;
  {$ENDIF}
end;

destructor TJSONDocArray.Destroy;
begin
  FMemBank:=nil;
  inherited;
end;

function TJSONDocArray.Get_Item(Index: integer): Variant;
var
  b:TJSONMemBank;
  bi,bn:integer;
  m:PJSONMemNode;
  d:IJSONMemBankLoadable;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FMemBank=nil then
      raise EJSONException.Create('IJSONDocArray currently not connected to an IJSONMemBank');
    if Index<0 then
      raise ERangeError.Create('Index out of range');
    b:=FMemBank.Bank as TJSONMemBank;
    {$IFNDEF JSONDOC_THREADSAFE}
    if Index=FCurrentIndex then
      bi:=FCurrentNode
    else
    if (Index<>0) and (Index=FCurrentIndex+1) then
     begin
      inc(FCurrentIndex);
      FCurrentNode:=b.FNodes[FCurrentNode].Next;
      bi:=FCurrentNode
     end
    else
    {$ENDIF}
     begin
      bi:=b.FNodes[FBaseIndex].Child;
      while (bi<>0) and (Index<>0) do
       begin
        bi:=b.FNodes[bi].Next;
        dec(Index);
       end;
     end;
    if bi=0 then
      raise ERangeError.Create('Index out of range');
    {$IFNDEF JSONDOC_THREADSAFE}
    FCurrentIndex:=Index;
    FCurrentNode:=bi;
    {$ENDIF}
    m:=@b.FNodes[bi];

    if (TVarData(Result).VType=varUnknown) and
      (TVarData(Result).VUnknown<>nil) and
      (IUnknown(Result).QueryInterface(IID_IJSONMemBankLoadable,d)=S_OK) then
      //d.LoadBank below
    else
      d:=nil;
    case m.F1 and jfd_Mask of
      jfdNull:
       begin
        //if d=nil then d.ClearBank(FMemBank) else//?
        Result:=Null;
        bn:=0;
       end;
      jfdObject:
        bn:=bi;
      jfdRawJSON:
        b.Parse(bn,bi);
      else
        raise EJSONException.Create('Unexpected array element type');
    end;
    if bn<>0 then
     begin
      if d=nil then
       begin
        d:=TJSONDocument.Create;
        Result:=d;
       end;
      d.LoadBank(FMemBank,bn);
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.v0(Index: integer): pointer;
begin
  raise EInvalidOp.Create('IJSONDocArray.v0 no longer supported');
end;

procedure TJSONDocArray.Set_Item(Index: integer; const Value: Variant);
var
  b:TJSONMemBank;
  bi,dl:integer;
  m:PJSONMemNode;
  d:IJSONDocument;
  dd:WideString;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FMemBank=nil then
      raise EJSONException.Create('IJSONDocArray currently not connected to an IJSONMemBank');
    if Index<0 then
      raise ERangeError.Create('Index out of range');
    //TODO: keep fast-lookup-array
    b:=FMemBank.Bank as TJSONMemBank;
    {$IFNDEF JSONDOC_THREADSAFE}
    if Index=FCurrentIndex then
      bi:=FCurrentNode
    else
    {$ENDIF}
     begin
      bi:=b.FNodes[FBaseIndex].Child;
      while (bi<>0) and (Index<>0) do
       begin
        bi:=b.FNodes[bi].Next;
        dec(Index);
       end;
     end;
    if bi=0 then
      raise ERangeError.Create('Index out of range');
    m:=@b.FNodes[bi];
    case TVarData(Value).VType of
      varNull:
       begin
        m.ValueIndex:=0;
        m.ValueLength:=0;
        m.F1:=
          (m.F1 and jfk_Mask)//assert jfkArrayIndex
          or jfdNull;
       end;
      varUnknown:
        if (TVarData(Value).VUnknown<>nil) and
          (IUnknown(Value).QueryInterface(IID_IJSONDocument,d)=S_OK) then
         begin
          dd:=d.ToString;
          d:=nil;
          dl:=Length(dd);
          if m.ValueLength>=dl then
           begin
            Move(dd[1],b.json.Data[m.ValueIndex],dl*w2);
            m.ValueLength:=dl;
           end
          else
           begin
            b.json.Append('|');
            m.ValueIndex:=b.json.DataIndex+1;
            m.ValueLength:=dl;
            b.json.Append(dd);
            m.F1:=
              (m.F1 and jfk_Mask)//assert jfkArrayIndex
              or jfdRawJSON;
           end;
         end
        else raise EJSONEncodeException.Create(
          'JSONDocArray.Set_Item requires IJSONDocument instances');
      else raise EJSONEncodeException.Create(
        'JSONDocArray.Set_Item requires IJSONDocument instances');
    end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.ItemsCount: integer;
var
  b:TJSONMemBank;
  bi:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    Result:=0;
    if FMemBank<>nil then
     begin
      b:=FMemBank.Bank as TJSONMemBank;
      bi:=b.FNodes[FBaseIndex].Child;
      while bi<>0 do
       begin
        inc(Result);
        bi:=b.FNodes[bi].Next;
       end;
     end;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.Clear;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    FMemBank:=nil;
    FBaseIndex:=0;

    {$IFNDEF JSONDOC_THREADSAFE}
    FCurrentIndex:=-1;
    FCurrentNode:=0;
    {$ENDIF}

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.Add(const Doc: IJSONDocument): integer;
var
  w:WideString;
  b:TJSONMemBank;
begin
  w:=Doc.AsString;

  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FMemBank=nil then
     begin
      b:=TJSONMemBank.Create;
      FMemBank:=b;
      FBaseIndex:=b.StartArray;
     end
    else
      b:=FMemBank.Bank as TJSONMemBank;

    //TODO: preserve structure when present instead of AsString
    //'CloneBank'?

    Result:=b.Add(FBaseIndex,w,nil);

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.AddJSON(const Data: WideString): integer;
var
  b:TJSONMemBank;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FMemBank=nil then
     begin
      b:=TJSONMemBank.Create;
      FMemBank:=b;
      FBaseIndex:=b.StartArray;
     end
    else
      b:=FMemBank.Bank as TJSONMemBank;

    Result:=b.Add(FBaseIndex,Data,nil);

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.LoadItem(Index: integer; const Doc: IJSONDocument);
var
  b:TJSONMemBank;
  bi,bn:integer;
  m:PJSONMemNode;
  dl:IJSONMemBankLoadable;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FMemBank=nil then
      raise EJSONException.Create('IJSONDocArray currently not connected to an IJSONMemBank');
    b:=FMemBank.Bank as TJSONMemBank;
    bi:=b.FNodes[FBaseIndex].Child;
    bn:=Index;
    if Index<0 then
      raise ERangeError.Create('Out of range');
    while (bn<>0) and (bi<>0) do
     begin
      dec(bn);
      bi:=b.FNodes[bi].Next;
     end;
    if bi=0 then
      raise ERangeError.Create('Out of range');
    m:=@b.FNodes[bi];

    if Doc=nil then
      raise EJSONException.Create('IJSONDocArray.LoadItem called without IJSONDocument instance');
    case m.F1 and jfd_Mask of
      jfdNull:
        Doc.Clear;//?
      jfdObject:
        if Doc.QueryInterface(IID_IJSONMemBankLoadable,dl)=S_OK then
          dl.LoadBank(FMemBank,bi)
        else
          Doc.Parse(Copy(b.json.Data,m.ValueIndex,m.ValueLength));
      jfdRawJSON:
       begin
        if Doc.QueryInterface(IID_IJSONMemBankLoadable,dl)=S_OK then
         begin
          b.Parse(bn,bi);
          dl.LoadBank(FMemBank,bn);
         end
        else
          Doc.Parse(Copy(b.json.Data,m.ValueIndex,m.ValueLength));
       end;
      else
        raise EJSONException.Create('Unexpected array element type');
    end;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.GetJSON(Index: integer): WideString; stdcall;
var
  b:TJSONMemBank;
  bi,bn:integer;
  m:PJSONMemNode;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FMemBank=nil then
      raise EJSONException.Create('IJSONDocArray currently not connected to an IJSONMemBank');
    b:=FMemBank.Bank as TJSONMemBank;
    bi:=b.FNodes[FBaseIndex].Child;
    bn:=Index;
    if Index<0 then
      raise ERangeError.Create('Out of range');
    while (bn<>0) and (bi<>0) do
     begin
      dec(bn);
      bi:=b.FNodes[bi].Next;
     end;
    if bi=0 then
      raise ERangeError.Create('Out of range');
    m:=@b.FNodes[bi];

    {
    case b.FNodes[bi].F1 and jfd_Mask of
      jfdNull:;//?
      jfdRawJSON:Doc.Parse(Copy(b.json.Data,m.ValueIndex,m.ValueLength));
      else
        raise EJSONException.Create('Unexpected node type in document array');
    end;
    }
    //Result:=Copy(b.json.Data,m.ValueIndex,m.ValueLength);
    SetLength(Result,m.ValueLength);
    Move(b.json.Data[m.ValueIndex],Result[1],m.ValueLength*w2);

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.Parse(const JSONData: WideString);
var
  i,l:integer;
begin
  l:=Length(JSONData);
  i:=1;
  while (i<=l) and (JSONData[i]<=' ') do inc(i);
  if (i<=l) and (JSONData[i]='[') then
    JSON(['',Self as IJSONDocArray]).Parse('{"":'+JSONData+'}')
  else
    {$IFDEF JSONDOC_JSON_LOOSE}
    if (i<=l) and (JSONData[i]='{') then //?
      AddJSON(JSONData)
    else
    {$ENDIF}
      raise EJSONDecodeException.Create('JSON doesn''t define an array, "[" expected.');
end;

function TJSONDocArray.JSONToString: WideString;
var
  w:TJSONBuilder;
begin
  if Self=nil then
    Result:='null'
  else
   begin
    w.Clear;
    Build(@w,0);
    Result:=w.Output;
   end;
end;

procedure TJSONDocArray.Build(Builder: pointer; TabIndex: integer);
var
  b:TJSONMemBank;
  w:PJSONBuilder absolute Builder;
  bi,wi:integer;
  l:cardinal;
  m:PJSONMemNode;
  z:WideString;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    if FMemBank=nil then
     begin
      w.Append('[]');
      Exit;
     end;
    b:=FMemBank.Bank as TJSONMemBank;
    bi:=b.FNodes[FBaseIndex].Child;
    if bi=0 then
      w.Append('[]')
    else
     begin

      //first estimate size and reserve builder memory
      l:=TabIndex+1;
      bi:=b.FNodes[FBaseIndex].Child;
      while (bi<>0) do
       begin
        m:=@b.FNodes[bi];
        inc(l,m.ValueLength+TabIndex+1);
        bi:=m.Next;
       end;
      w.Reserve(l);

      //now build array
      wi:=w.DataIndex+1;
      bi:=b.FNodes[FBaseIndex].Child;
      while (bi<>0) do
       begin
        w.Append(',');
        {$IFDEF JSONDOC_STOREINDENTING}
        w.Append(Copy(tabs,1,TabIndex+3));
        {$ENDIF}
        m:=@b.FNodes[bi];
        if (m.F1 and jfd_Mask)=jfdNull then
          w.Append('null')
        else
         begin
          //z:=Copy(b.json.Data,m.ValueIndex,m.ValueLength);
          SetLength(z,m.ValueLength);
          Move(b.json.Data[m.ValueIndex],z[1],m.ValueLength*w2);
          {$IFDEF JSONDOC_STOREINDENTING}
          wr(w,z,#13#10,Copy(tabs,1,TabIndex+3));
          {$ELSE}
          w.Append(z);
          {$ENDIF}
         end;
        bi:=m.Next;
       end;
      w.Data[wi]:='[';
      {$IFDEF JSONDOC_STOREINDENTING}
      w.Append(Copy(tabs,1,TabIndex+2));
      {$ENDIF}
      w.Append(']');
     end;

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.ClearBank(MemBank: IJSONMemBank);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //TODO: if FMemBank<>nil then? if MemBank<>nil then?
    FMemBank:=nil;
    FBaseIndex:=0;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;


procedure TJSONDocArray.LoadBank(Bank: IUnknown; Index: integer);
var
  bb:IJSONMemBank;
  b:TJSONMemBank;
  n:integer;
  function IsHigherIndex:boolean;
  var
    bi,h:integer;
  begin
    Result:=false;
    n:=0;
    h:=0;
    if FMemBank<>nil then
     begin
      b:=FMemBank.Bank as TJSONMemBank;
      bi:=b.FNodes[FBaseIndex].Child;
      while bi<>0 do
       begin
        if h<bi then h:=bi;
        n:=bi;
        bi:=b.FNodes[bi].Next;
       end;
      Result:=h<Index;
     end;
  end;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}

    bb:=Bank as IJSONMemBank;
    //if Allow_Coalesce_DocArrays and LoadIndex=?
    if (FMemBank=bb) and IsHigherIndex then
     begin
      //assert n<>0 and b.FNodes[n].Next=0
      b.FNodes[n].Next:=b.FNodes[Index].Child;
     end
    else
     begin
      FMemBank:=bb;
      FBaseIndex:=Index;
     end;

    //TODO: basic checks?
    //assert (b.FNodes[FBaseIndex].F1 and jfd_Mask)=jfd_Array

  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TJSONDocArrayEnumerator }

constructor TJSONDocArrayEnumerator.Create(const Data: IJSONDocArray);
begin
  inherited Create;
  FDocArray:=Data;
  FIndex:=-1;//before first, see Next
end;

function TJSONDocArrayEnumerator.EOF: boolean;
begin
  if (FIndex=-1) and (FDocArray.Count=0) then
    Result:=true
  else
    Result:=FIndex>=FDocArray.Count;
end;

function TJSONDocArrayEnumerator.Get_DocIndex: integer;
begin
  Result:=FIndex;
end;

function TJSONDocArrayEnumerator.Next: boolean;
begin
  inc(FIndex);
  if FIndex>=FDocArray.Count then
    Result:=false
  else
   begin
    FDocArray.LoadItem(FIndex,Self);
    Result:=true;
   end;
end;

{ JSON }

function JSON:IJSONDocument; //overload;
begin
  Result:=TJSONDocument.Create as IJSONDocument;
end;

function JSON(const x:array of Variant):IJSONDocument; //overload;
var
  i,l,si,sl:integer;
  s:array of TJSONDocument;
  d:TJSONDocument;
  key:WideString;
begin
  d:=TJSONDocument.Create;
  si:=0;
  sl:=0;
  i:=0;
  l:=Length(x);
  while i<l do
   begin
    key:=VarToWideStr(x[i]);
    inc(i);
    if (key<>'') and (key[1]='}') then
     begin
      while (key<>'') and (key[1]='}') do
       begin
        //pop from stack
        if si=0 then
          raise EJSONException.Create('JSON builder: closing more embedded documents than opened #'+IntToStr(i))
        else
         begin
          dec(si);
          d:=s[si];
          s[si]:=nil;
         end;
        key:=Copy(key,2,Length(key)-1);
       end;
      if key<>'' then
        raise EJSONException.Create('JSON builder: "}" not allowed as key prefix #'+IntToStr(i));
     end
    else
      if (key<>'') and (key[Length(key)]='{') then
       begin
        //push on stack
        if si=sl then
         begin
          inc(sl,8);//growstep
          SetLength(s,sl);
         end;
        s[si]:=d;
        d:=TJSONDocument.Create;
        s[si][Copy(key,1,Length(key)-1)]:=d as IJSONDocument;
        inc(si);
       end
      else
        if i=l then
          raise EJSONException.Create('JSON builder: last key is missing value')
        else
         begin
          d[key]:=x[i];
          inc(i);
         end;
   end;
  if si=0 then Result:=d else Result:=s[0];
end;

function JSON(const x: Variant): IJSONDocument; overload;
begin
  case TVarData(x).VType of
    varNull,varEmpty:Result:=nil;//raise?
    varOleStr,varString,$0102:
     begin
      if Result=nil then
        Result:=TJSONDocument.Create as IJSONDocument;
      Result.Parse(VarToWideStr(x));
     end;
    else
      Result:=IUnknown(x) as IJSONDocument;
  end;
end;

function JSON(const x: array of Variant; const d: WideString): IJSONDocument;
begin
  Result:=JSON(x);
  Result.Parse(d);
end;

function JSONEnum(const x: IJSONDocument): IJSONEnumerator;
var
  je:IJSONEnumerable;
begin
  if x=nil then
    Result:=TJSONEnumerator.Create(nil)
  else
    //Result:=(x as IJSONEnumerable).NewEnumerator;
    if x.QueryInterface(IID_IJSONEnumerable,je)=S_OK then
      Result:=je.NewEnumerator
    else
      raise EJSONException.Create('IJSONDocument instance doesn''t implement IJSONEnumerable');
end;

function JSONEnum(const x: Variant): IJSONEnumerator;
var
  vt:TVarType;
  e:IJSONEnumerable;
begin
  vt:=TVarData(x).VType;
  if (vt and varArray)=0 then
    case vt of
      varNull,varEmpty:
        Result:=TJSONEnumerator.Create(nil);//has .EOF=true
      varUnknown:
        if (TVarData(x).VUnknown<>nil) and
          (IUnknown(x).QueryInterface(IID_IJSONEnumerable,e)=S_OK) then
          Result:=e.NewEnumerator
        else
          raise EJSONException.Create('No supported interface found on object');
      else
        raise EJSONException.Create('Unsupported variant type '+IntToHex(vt,4));
    end
  else
    Result:=TVarArrayEnumerator.Create(@x);
end;

function JSON(const x: IJSONEnumerator): IJSONDocument;
begin
  Result:=IUnknown(x.Value) as IJSONDocument;
end;

function JSONEnum(const x: IJSONEnumerator): IJSONEnumerator;
begin
  if (x=nil) or VarIsNull(x.Value) then
    Result:=TJSONEnumerator.Create(nil)
  else
    Result:=(IUnknown(x.Value) as IJSONEnumerable).NewEnumerator;
end;

function JSONEnumSorted(const x: IJSONDocument): IJSONEnumerator;
var
  je:IJSONEnumerableSorted;
begin
  if x=nil then
    Result:=TJSONEnumerator.Create(nil)
  else
    //Result:=(x as IJSONEnumerable).NewEnumerator;
    if x.QueryInterface(IID_IJSONEnumerableSorted,je)=S_OK then
      Result:=je.NewEnumerator
    else
      raise EJSONException.Create('IJSONDocument instance doesn''t implement IJSONEnumerableSorted');
end;

function JSONEnumSorted(const x: Variant): IJSONEnumerator;
var
  vt:TVarType;
  e:IJSONEnumerableSorted;
begin
  vt:=TVarData(x).VType;
  case vt of
    varNull,varEmpty:
      Result:=TJSONEnumerator.Create(nil);//has .EOF=true
    varUnknown:
      if (TVarData(x).VUnknown<>nil) and
        (IUnknown(x).QueryInterface(IID_IJSONEnumerableSorted,e)=S_OK) then
        Result:=e.NewEnumerator
      else
        raise EJSONException.Create('No supported interface found on object');
    else
      raise EJSONException.Create('Unsupported variant type '+IntToHex(vt,4));
  end;
end;

function JSONEnum(const x: IJSONDocArray): IJSONDocArrayEnumerator;
begin
  Result:=TJSONDocArrayEnumerator.Create(x);
end;

function ja(const Items:array of Variant): IJSONArray;
var
  a:TJSONArray;
  i,l:integer;
begin
  l:=Length(Items);
  a:=TJSONArray.Create(l);
  i:=0;
  while i<>l do
   begin
    a.FData[i]:=Items[i];
    inc(i);
   end;
  Result:=a;
end;

function ja(const Item:Variant): IJSONArray;
begin
  if (TVarData(Item).VType=varUnknown) and
    (TVarData(Item).VUnknown<>nil) and
    (IUnknown(Item).QueryInterface(IID_IJSONArray,Result)=S_OK) then
    //ok!
  else
  if (TVarData(Item).VType and varArray)<>0 then
    Result:=TVarJSONArray.Create(Item)
  else
    raise EJSONException.Create('Variant is not IJSONArray');
end;

function JSONDocArray: IJSONDocArray;
begin
  Result:=TJSONDocArray.Create;
end;

function JSONDocArray(const Items:array of IJSONDocument): IJSONDocArray;
var
  i:integer;
begin
  Result:=TJSONDocArray.Create;
  for i:=0 to Length(Items)-1 do Result.Add(Items[i]);
end;

function JSONDocArray(const x: Variant): IJSONDocArray;
var
  vt:TVarType;
  i:integer;
begin
  vt:=TVarData(x).VType;
  if (vt and varArray)=0 then
    case vt of
      varNull,varEmpty:
        Result:=nil;
      varOleStr,varString,$0102:
       begin
        Result:=TJSONDocArray.Create;
        Result.Parse(x);
       end;
      varUnknown:
        if (TVarData(x).VUnknown<>nil) and
          (IUnknown(x).QueryInterface(IID_IJSONDocArray,Result)=S_OK) then
          //Result is an IJSONDocArray
        else
          raise EJSONException.Create('No supported interface found on object');
      else
        raise EJSONException.Create('Unsupported variant type '+IntToHex(vt,4));
    end
  else
   begin
    Result:=TJSONDocArray.Create;
    for i:=VarArrayLowBound(x,1) to VarArrayHighBound(x,1) do
      Result.Add(JSON(x[i]));
   end;
end;

function isJSON(const v: Variant; var d: IJSONDocument): boolean;
begin
  Result:=
    (TVarData(v).VType=varUnknown) and
    (TVarData(v).VUnknown<>nil) and
    (IUnknown(v).QueryInterface(IID_IJSONDocument,d)=S_OK);
end;

function isJSONArray(const v: Variant; var a: IJSONArray): boolean;
begin
  Result:=
    (TVarData(v).VType=varUnknown) and
    (TVarData(v).VUnknown<>nil) and
    (IUnknown(v).QueryInterface(IID_IJSONArray,a)=S_OK);
end;

function isJSONDocArray(const v: Variant; var a: IJSONDocArray): boolean;
begin
  Result:=
    (TVarData(v).VType=varUnknown) and
    (TVarData(v).VUnknown<>nil) and
    (IUnknown(v).QueryInterface(IID_IJSONDocArray,a)=S_OK);
end;

function JSONa: IJSONDocument;
var
  jd:TJSONDocument;
begin
  jd:=TJSONDocument.Create;
  jd.UseIJSONDocArray:=true;
  Result:=jd as IJSONDocument;
end;

function newJSON(var d: IJSONDocument): IJSONDocument;
begin
  d:=TJSONDocument.Create;
  Result:=d;
end;

function newJSONDocArray(var a: IJSONDocArray): IJSONDocArray;
begin
  a:=TJSONDocArray.Create;
  Result:=a;
end;

initialization

  {$IFDEF JSONDOC_DEFAULT_USE_IJSONARRAY}
  JSON_UseIJSONArray:=true;  //default, see TJSONDocument.Create
  {$ELSE}
  JSON_UseIJSONArray:=false; //default, see TJSONDocument.Create
  {$ENDIF}

  {$IFDEF JSONDOC_DEFAULT_USE_IJSONDOCARRAY}
  JSON_UseIJSONDocArray:=true;  //default, see TJSONDocument.Parse
  {$ELSE}
  JSON_UseIJSONDocArray:=false; //default, see TJSONDocument.Parse
  {$ENDIF}

end.




