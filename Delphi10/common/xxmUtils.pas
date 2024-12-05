unit xxmUtils;

interface

uses System.Classes;

type
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

uses Winapi.Windows;

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
