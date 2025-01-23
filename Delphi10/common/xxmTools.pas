unit xxmTools;

interface

{$D-}
{$Y-}
{$L-}

uses SysUtils, Classes;

type
  TOwningHandleStream=class(THandleStream)
  public
    destructor Destroy; override;
  end;

  THeapStream=class(TMemoryStream)
  private
    FHeap:THandle;
  protected
    function Realloc(var NewCapacity: NativeInt): Pointer; override;
  public
    procedure AfterConstruction; override;
  end;

function GetFileSignature(const Path: string): string;
function UTF8CmpI(a,b:PUTF8Char):NativeInt;
function HTMLEncode(const Data:UTF8String):UTF8String;
function Base64Encode(const x:UTF8String):UTF8String;
function RFC822DateGMT(dd:TDateTime):string;

type
  TKeyValues=record
    Data:UTF8String;
    Keys:array of record
      Key,Value:PUTF8Char;
    end;
    KeysIndex,KeysSize:integer;

    function SplitHeaderValue(Value:PUTF8Char;StartsWithValue:boolean):PUTF8Char;
    function GetValue(Key:PUTF8Char):PUTF8Char;
    property Value[Key:PUTF8Char]:PUTF8Char read GetValue; default;
  end;

implementation

uses Windows;

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

function UTF8CmpI(a,b:PUTF8Char):NativeInt;
begin
  if a=nil then
    if b=nil then
      Result:=0
    else
      Result:=-1
  else
    if b=nil then
      Result:=1
    else
     begin
      while (a^<>#0) and (b^<>#0) and ((a^=b^) or
        ((word(a^) in [$40..$5A]) and (word(b^) in [$60..$7A]) and ((word(a^) and $1F)=(word(b^) and $1F))) or
        ((word(a^) in [$60..$7A]) and (word(b^) in [$40..$5A]) and ((word(a^) and $1F)=(word(b^) and $1F)))) do
       begin
        inc(a);
        inc(b);
       end;
      if a^=b^ then
        Result:=0
      else
        if a^<b^ then
          Result:=-1
        else
          Result:=1;
     end;
end;

function HTMLEncode(const Data:UTF8String):UTF8String;
const
  GrowStep=$100;
var
  di,ri,dl,rl:integer;
  x:UTF8String;
begin
  Result:=Data;
  di:=1;
  dl:=Length(Data);
  while (di<=dl) and not(Data[di] in ['&','<','"','>',#13,#10]) do
    inc(di);
  if di<=dl then
   begin
    ri:=di;
    rl:=((dl div GrowStep)+1)*GrowStep;
    SetLength(Result,rl);
    while (di<=dl) do
     begin
      case Data[di] of
        '&':x:='&amp;';
        '<':x:='&lt;';
        '>':x:='&gt;';
        '"':x:='&quot;';
        #13,#10:
         begin
          if (di<dl) and (Data[di]=#13) and (Data[di+1]=#10) then inc(di);
          x:='<br />'#13#10;
         end;
        else x:=Data[di];
      end;
      if ri+Length(x)>rl then
       begin
        inc(rl,GrowStep);
        SetLength(Result,rl);
       end;
      Move(x[1],Result[ri],Length(x));
      inc(ri,Length(x));
      inc(di);
     end;
    SetLength(Result,ri-1);
   end;
end;

function Base64Encode(const x:UTF8String):UTF8String;
var
  i,j,l:integer;
  a1,a2,a3,b1,b2,b3,b4:byte;
const
  Base64Set:array[0..64] of UTF8Char=
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

function RFC822DateGMT(dd:TDateTime):string;
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

function THeapStream.Realloc(var NewCapacity: NativeInt): Pointer;
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

function TKeyValues.SplitHeaderValue(Value:PUTF8Char;StartsWithValue:boolean):PUTF8Char;
var
  p:PUTF8Char;
begin
  KeysIndex:=0;
  KeysSize:=0;
  Data:=Value;
  Result:=nil;//default
  if Value=nil then Exit;

  p:=PUTF8Char(Data);
  if StartsWithValue then
   begin
    Result:=p;
    while (p^<>#0) and (p^<>';') do inc(p);
    if p^=';' then
     begin
      p^:=#0;
      inc(p);
      while p^ in [#1..' '] do inc(p);//whitespace
     end;
   end;

  while (p^<>#0) do
   begin
    if KeysIndex=KeysSize then
     begin
      inc(KeysSize,$10);//grow step
      SetLength(Keys,KeysSize);
     end;
    //key
    while p^ in [#1..' '] do inc(p);//whitespace
    Keys[KeysIndex].Key:=p;
    while (p^<>#0) and (p^<>'=') do inc(p);
    p^:=#0;
    inc(p);
    //value
    while p^ in [#1..' '] do inc(p);//whitespace
    if p^='"' then
     begin
      inc(p);
      Keys[KeysIndex].Value:=p;
      while (p^<>#0) and (p^<>'"') do inc(p);//TODO: '\'?
      if p^<>#0 then
       begin
        p^:=#0;
        inc(p);
       end;
      if p^=';' then inc(p);
     end
    else
     begin
      Keys[KeysIndex].Value:=p;
      while (p^<>#0) and (p^<>';') do inc(p);
      if p^<>#0 then
       begin
        p^:=#0;
        inc(p);
       end;
     end;
    inc(KeysIndex);
   end;
  //SetLength(Keys,KeysIndex);KeysSize:=KeysIndex;//?
end;

function TKeyValues.GetValue(Key: PUTF8Char): PUTF8Char;
var
  i:integer;
begin
  i:=0;
  while (i<KeysIndex) and (UTF8CmpI(Key,Keys[i].Key)<>0) do inc(i);
  if i<KeysIndex then
    Result:=Keys[i].Value
  else
    Result:=nil;
end;

end.
