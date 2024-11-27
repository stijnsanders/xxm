unit xxmCommonUtils;

interface

uses SysUtils, Classes;

function UTCNow:TDateTime;
function RFC822DateGMT(dd: TDateTime): string;
function GetFileModifiedDateTime(const FilePath:AnsiString;
  var FileSize:Int64):TDateTime;
function GetFileSignature(const Path: string): string;
function Base64Encode(const x:AnsiString):AnsiString;
function RawCompare(const a,b: string): integer;

type
  TOwningHandleStream=class(THandleStream)
  public
    destructor Destroy; override;
  end;

  THeapStream=class(TMemoryStream)
  private
    FHeap:THandle;
  protected
    {$IF CompilerVersion<30}
    function Realloc(var NewCapacity: LongInt): Pointer; override;
    {$ELSE}
    function Realloc(var NewCapacity: NativeInt): Pointer; override;
    {$IFEND}
  public
    procedure AfterConstruction; override;
  end;

implementation

uses Windows;

function UTCNow:TDateTime;
var
  st:TSystemTime;
begin
  GetSystemTime(st);
  Result:=
    EncodeDate(st.wYear,st.wMonth,st.wDay)+
    EncodeTime(st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
end;

function RFC822DateGMT(dd: TDateTime): string;
const
  Days:array [1..7] of string=
    ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  Months:array [1..12] of string=
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
//  SignStr:array[boolean] of string=('-','+');
var
  dg:TDateTime;
  y,m,d,wd,th,tm,ts,tms:Word;
  tz:TIME_ZONE_INFORMATION;
begin
  GetTimeZoneInformation(tz);
  dg:=dd+tz.Bias/1440;
  DecodeDateFully(dg,y,m,d,wd);
  DecodeTime(dg,th,tm,ts,tms);
  FmtStr(Result, '%s, %d %s %d %.2d:%.2d:%.2d GMT',
    [Days[wd],d,Months[m],y,th,tm,ts]);
end;

function GetFileModifiedDateTime(const FilePath:AnsiString;
  var FileSize:Int64):TDateTime;
var
  fh:THandle;
  fd:TWin32FindDataA;
  st:TSystemTime;
begin
  if FilePath='' then Result:=0 else
   begin
    fh:=FindFirstFileA(PAnsiChar(FilePath),fd);
    if fh=INVALID_HANDLE_VALUE then Result:=0 else
     begin
      Windows.FindClose(fh);
      FileSize:=fd.nFileSizeHigh shl 32 or fd.nFileSizeLow;
      FileTimeToSystemTime(fd.ftLastWriteTime,st);
      Result:=SystemTimeToDateTime(st);
     end;
   end;
end;

function GetFileSignature(const Path: string): string;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  fh:=FindFirstFile(PChar(Path),fd);
  if fh=INVALID_HANDLE_VALUE then Result:='' else
   begin
    //assert(fd.nFileSizeHigh=0
    Result:=
      IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+'_'+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
end;

{ TOwningHandleStream }

destructor TOwningHandleStream.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

{ THeapStream }

procedure THeapStream.AfterConstruction;
begin
  inherited;
  FHeap:=GetProcessHeap;
end;

{$IF CompilerVersion<30}
function THeapStream.Realloc(var NewCapacity: LongInt): Pointer;
{$ELSE}
function THeapStream.Realloc(var NewCapacity: NativeInt): Pointer;
{$IFEND}
const
  BlockSize=$10000;
begin
  if (NewCapacity>0) and (NewCapacity<>Size) then
    NewCapacity:=(NewCapacity+(BlockSize-1)) and not(BlockSize-1);
  Result:=Memory;
  if NewCapacity<>Capacity then
   begin
    if NewCapacity=0 then
     begin
      HeapFree(FHeap,0,Memory);
      Result:=nil;
     end
    else
     begin
      if Capacity=0 then
        Result:=HeapAlloc(FHeap,0,NewCapacity)
      else
        Result:=HeapReAlloc(FHeap,0,Memory,NewCapacity);
      if Result=nil then EStreamError.Create('HeapAlloc failed');
    end;
   end;
end;

function Base64Encode(const x:AnsiString):AnsiString;
var
  i,j,l:integer;
  a1,a2,a3,b1,b2,b3,b4:byte;
const
  Base64Set:array[0..64] of AnsiChar=
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
begin
  l:=Length(x);
  SetLength(Result,((l+2) div 3)*4);
  i:=1;
  j:=0;
  while i<=l do
   begin
    a1:=byte(x[i]);
    b1:=a1 shr 2;
    inc(i);
    if i<=l then
     begin
      a2:=byte(x[i]);
      b2:=((a1 and $03) shl 4) or (a2 shr 4);
      inc(i);
      if i<=l then
       begin
        a3:=byte(x[i]);
        b3:=((a2 and $0F) shl 2) or (a3 shr 6);
        b4:=a3 and $3F;
        inc(i);
       end
      else
       begin
        b3:=(a2 and $0F) shr 2;
        b4:=64;
       end;
     end
    else
     begin
      b2:=64;
      b3:=64;
      b4:=64;
     end;
    inc(j); Result[j]:=Base64Set[b1];
    inc(j); Result[j]:=Base64Set[b2];
    inc(j); Result[j]:=Base64Set[b3];
    inc(j); Result[j]:=Base64Set[b4];
   end;
  //SetLength(Result,j);
end;

function RawCompare(const a,b: string): integer;
var
  i,al,bl:integer;
begin
  //assert caller already made strings lower-case
  al:=Length(a);
  bl:=Length(b);
  i:=1;
  Result:=0;
  while (Result=0) and (i<=al) and (i<=bl) do
    if a[i]<b[i] then
      Result:=-1
    else
      if a[i]>b[i] then
        Result:=1
      else
        inc(i);
  if Result=0 then
    if (i<=al) then
      Result:=1
    else
      if (i<=bl) then
        Result:=-1;
end;

end.
