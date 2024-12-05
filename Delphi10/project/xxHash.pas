{

  xxHash
  by Stijn Sanders
  http://yoy.be/xxHash
  2016
  v1.0.0

  based on https://github.com/Cyan4973/xxHash/blob/dev/xxhash.c
     Copyright 2012-2016, Yann Collet
     BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

  ATTENTION: simplified by supporting little-endianness only.

}
unit xxHash;

interface

function xxHash32(const s:UTF8String;seed:cardinal):cardinal;
function xxHash64(const s:UTF8String;seed:int64):int64;

implementation

{$Q-}

function xxHash32(const s:UTF8String;seed:cardinal):cardinal;
const
  p1=2654435761;
  p2=2246822519;
  p3=3266489917;
  p4=668265263;
  p5=374761393;
var
  i,j,l,l1,p,q:cardinal;
  v:array[0..3] of cardinal;
begin
  i:=0;
  l:=Length(s);
  if l<16 then Result:=seed+p5+l else
   begin
    v[0]:=seed+p1+p2;
    v[1]:=seed+p2;
    v[2]:=seed;
    v[3]:=seed-p1;
    l1:=l div $10;
    while l1<>0 do
     begin
      for j:=0 to 3 do
       begin
        Move(s[i+1],p,4);
        inc(i,4);
        q:=v[j]+p*p2;
        v[j]:=((q shl 13) or (q shr 19))*p1;
       end;
      dec(l1);
     end;
    Result:=l+
      ((v[0] shl 1) or (v[0] shr 31))+
      ((v[1] shl 7) or (v[1] shr 25))+
      ((v[2] shl 12) or (v[2] shr 20))+
      ((v[3] shl 18) or (v[3] shr 14));
   end;
  while i+4<=l do
   begin
    Move(s[i+1],p,4);
    inc(i,4);
    q:=Result+p*p3;
    Result:=((q shl 17) or (q shr 15))*p4;
   end;
  while i<l do
   begin
    inc(i);
    q:=Result+cardinal(byte(s[i]))*p5;
    Result:=((q shl 11) or (q shr 21))*p1;
   end;
  Result:=(Result xor (Result shr 15))*p2;
  Result:=(Result xor (Result shr 13))*p3;
  Result:=(Result xor (Result shr 16));
end;

function xxHash64(const s:UTF8String;seed:int64):int64;
const
  p1=-7046029288634856825;//11400714785074694791;//$9E3779B185EBCA87;
  p2=-4417276706812531889;//14029467366897019727;//$C2B2AE3D27D4EB4F;
  p3=1609587929392839161;
  p4=-8796714831421723037;//9650029242287828579;//$85EBCA77C2B2AE63;
  p5=2870177450012600261;
var
  i,j,l,l1:cardinal;
  p,q:int64;
  v:array[0..3] of int64;
begin
  l:=Length(s);
  i:=0;
  if l<32 then Result:=seed+p5 else
   begin
    v[0]:=seed+p1+p2;
    v[1]:=seed+p2;
    v[2]:=seed;
    v[3]:=seed-p1;
    l1:=l div $10;
    while l1<>0 do
     begin
      for j:=0 to 3 do
       begin
        Move(s[i+1],p,8);
        inc(i,8);
        q:=v[j]+p*p2;
        v[j]:=((q shl 31) or (q shr 33))*p1;
       end;
      dec(l1);
     end;
    Result:=
      ((v[0] shl 1) or (v[0] shr 63))+
      ((v[1] shl 7) or (v[1] shr 57))+
      ((v[2] shl 12) or (v[2] shr 52))+
      ((v[3] shl 18) or (v[3] shr 46));
    for j:=0 to 3 do
     begin
      q:=v[j]*p2;
      Result:=(Result xor (((q shl 31) or (q shr 33))*p1))*p1+p4;
     end;
   end;
  Result:=Result+l;
  while i+8<=l do
   begin
    Move(s[i+1],p,8);
    inc(i,8);
    q:=Result+p*p2;
    q:=((q shl 31) or (q shr 33))*p1;
    Result:=Result xor q;
    Result:=((Result shl 27) or (Result shr 37))*p1+p4;
   end;
  while i+4<=l do
   begin
    p:=0;
    Move(s[i+1],p,4);
    inc(i,4);
    q:=Result xor (p*p1);
    Result:=((q shl 23) or (q shr 41))*p2+p3;
   end;
  while i<l do
   begin
    inc(i);
    q:=Result xor (byte(s[i])*p5);
    Result:=((q shl 11) or (q shr 53))*p1;
   end;
  Result:=(Result xor (Result shr 33))*p2;
  Result:=(Result xor (Result shr 29))*p3;
  Result:=(Result xor (Result shr 32));
end;

end.
