(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is mozilla.org.
 *
 * The Initial Developer of the Original Code is
 * Netscape Communications Corpotation.
 * Portions created by the Initial Developer are Copyright (C) 1998
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Takanori Itou <necottie@nesitive.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)
unit nsStream;

interface

uses
  nsXPCOM, Classes;

function NS_NewByteArrayInputStream(out stream: nsIInputStream; const Buffer: Pointer; Size: Longword): Longword;
function NS_NewInputStreamFromTStream(input: TStream; own: Boolean=False): nsIInputStream;
function NS_NewOutputStreamFromTStream(output: TStream; own: Boolean=False): nsIOutputStream;

implementation

uses
  Math, nsMemory, nsError, nsTypes, SysUtils;

type
  nsByteArrayInputStream = class(TInterfacedObject,
                                 nsIInputStream)
    FBuffer: PByte;
    FNBytes: Longword;
    FPos: Longword;

    constructor Create(const Buffer: Pointer; nBytes: Longword);
    destructor Destroy; override;

    procedure Close(); safecall;
    function Available(): PRUint32; safecall;
    function Read(aBuf: PAnsiChar; aCount: Longword): PRUint32; safecall;
    function ReadSegments(aWriter: nsWriteSegmentFun; aClosure: Pointer; aCount: Longword): PRUint32; safecall;
    function IsNonBlocking(): PRBool; safecall;
  end;

constructor nsByteArrayInputStream.Create(const Buffer: Pointer; nBytes: Longword);
begin
  inherited Create;
  FBuffer := PByte(Buffer);
  FNBytes := nBytes;
  FPos := 0;
end;

destructor nsByteArrayInputStream.Destroy;
begin
  if Assigned(FBuffer) then
    nsMemory.Free(FBuffer);
  inherited;
end;

function nsByteArrayInputStream.Available(): PRUint32;
begin
  if (FNBytes=0) or not Assigned(FBuffer) then
    Result := 0
  else
    Result := FNBytes - FPos;
end;

procedure CopyMemory(var ADst; const ASrc; ASize: Integer); register; forward;

function nsByteArrayInputStream.Read(aBuf: PAnsiChar;
                                     aCount: Longword): PRUint32;
begin
  if (aCount=0) or (FPos = FNBytes) then
    Result := 0
  else
  begin
    Assert(Assigned(FBuffer), 'Stream buffer has been released - there''s an ownership problem somewhere!');
    if not Assigned(FBuffer) then
      Result := 0
    else
      if aCount > (FNBytes - FPos) then
      begin
        Result := FNBytes - FPos;
        CopyMemory(aBuf^, FBuffer^, Result);
        FPos := FNBytes;
      end else
      begin
        Result := aCount;
        CopyMemory(aBuf^, FBuffer^, Result);
        Inc(FPos, aCount);
      end;
  end;
end;

function nsByteArrayInputStream.ReadSegments(aWriter: nsWriteSegmentFun;
                                             aClosure: Pointer;
                                             aCount: Longword): PRUint32;
var
  readCount: Longword;
  rv: Longword;
  newPtr: PByte;
begin
  if FNBytes=0 then
    raise EInvalidArgument.Create('nsIInputStream.ReadSegments');

  if (aCount=0) or (FNBytes = FPos) then
    Result := 0
  else
  begin
    Assert(Assigned(FBuffer), 'Stream buffer has been released - there''s an ownership problem somewhere!');
    readCount := Min(aCount, FNBytes - FPos);
    if not Assigned(FBuffer) then
      Result := 0
    else
    begin
      newPtr := FBuffer;
      Inc(newPtr, FPos);
      rv := aWriter(Self, aClosure, PChar(newPtr), FPos, readCount, Result);
      if NS_SUCCEEDED(rv) then
        Inc(FPos, readCount);
    end;
  end;
end;

function nsByteArrayInputStream.IsNonBlocking(): PRBool;
begin
  Result := True;
end;

procedure nsByteArrayInputStream.Close;
begin
  if Assigned(FBuffer) then
  begin
    nsMemory.Free(FBuffer);
    FBuffer := nil;
    FNBytes := 0;
  end else
    raise Exception.Create('nsIInputStream.Close')
end;

function NS_NewByteArrayInputStream(out stream: nsIInputStream; const Buffer: Pointer; Size: Longword): Longword;
begin
  try
    stream := nsByteArrayInputStream.Create(Buffer, Size);
  except
    Result := NS_ERROR_OUT_OF_MEMORY;
    Exit;
  end;

  Result := NS_OK;
end;

type
  TInputStream = class(TInterfacedObject,
                        nsIInputStream)
    FStream: TStream;
    FOwn: Boolean;
    constructor Create(stream: TStream; own: Boolean);
    destructor Destroy; override;

    // nsIInputStream
    procedure Close(); safecall;
    function Available(): PRUint32; safecall;
    function Read(aBuf: PAnsiChar; aCount: PRUint32): PRUint32; safecall;
    function ReadSegments(aWriter: nsWriteSegmentFun; aClosure: Pointer; aCount: PRUint32): PRUint32; safecall;
    function IsNonBlocking(): PRBool; safecall;
  end;

constructor TInputStream.Create(stream: TStream; own: Boolean);
begin
  inherited Create;
  FStream := stream;
  FOwn := own;
end;

destructor TInputStream.Destroy;
begin
  if FOwn then
    FStream.Free;
  inherited;
end;

procedure TInputStream.Close;
begin
  if FOwn then
  begin
    FStream.Free;
    FOwn := False;
  end;
  FStream := nil;
end;

function TInputStream.Available: PRUint32;
var
  size, pos: Int64;
begin
  size := FStream.Size;
  pos := FStream.Position;
  if size>0 then
  begin
    Result := PRUint32(size-pos);
  end else
  begin
    Result := High(PRUint32);
  end;
end;

function TInputStream.Read(aBuf: PAnsiChar; aCount: PRUint32): PRUint32;
begin
  Result := FStream.Read(aBuf^, aCount);
end;

function TInputStream.ReadSegments(aWriter: nsWriteSegmentFun;
        aClosure: Pointer; aCount: PRUint32): PRUint32;
type
  nsWriteSegmentFunc = function (aInStream: nsIInputStream;
                                 aClosure: Pointer;
                                 const aFromSegment: Pointer;
                                 aToOffset: PRUint32;
                                 aCount: PRUint32
                                ): PRUint32; safecall;

var
  data: Pointer;
begin
  data := nsMemory.Alloc(aCount);
  try
    aCount := FStream.Read(data^, aCount);
    Result := nsWriteSegmentFunc(aWriter)(
            Self, aClosure, data, FStream.Position, aCount);
  finally
    nsMemory.Free(data);
  end;
end;

function TInputStream.IsNonBlocking: PRBool;
begin
  Result := True;
end;

function NS_NewInputStreamFromTStream(input: TStream; own: Boolean): nsIInputStream;
begin
  Result := TInputStream.Create(input, own);
end;

type
  TOutputStream = class(TInterfacedObject,
                        nsIOutputStream)
    FStream: TStream;
    FOwn: Boolean;

    constructor Create(output: TStream; own: Boolean);
    destructor Destroy; override;

    procedure Close(); safecall;
    procedure Flush(); safecall;
    function Write(const aBuf: PAnsiChar; aCount: PRUint32): PRUint32; safecall;
    function WriteFrom(aFromStream: nsIInputStream; aCount: PRUint32): PRUint32; safecall;
    function WriteSegments(aReader: nsReadSegmentFun; aClosure: Pointer; aCount: PRUint32): PRUint32; safecall;
    function IsNonBlocking(): PRBool; safecall;
  end;

constructor TOutputStream.Create(output: TStream; own: Boolean);
begin
  inherited Create;
  FStream := output;
  FOwn := own;
end;

destructor TOutputStream.Destroy;
begin
  if FOwn then
    FStream.Free;
  inherited;
end;

procedure TOutputStream.Close;
begin
  if FOwn then
  begin
    FStream.Free;
    FOwn := False;
  end;
  FStream := nil;
end;

procedure TOutputStream.Flush;
begin
end;

function TOutputStream.Write(const aBuf: PAnsiChar; aCount: PRUint32): PRUint32;
begin
  Result := FStream.Write(aBuf^, aCount);
end;

function TOutputStream.WriteFrom(aFromStream: nsIInputStream; aCount: PRUint32): PRUint32;
var
  data: Pointer;
begin
  data := nsMemory.Alloc(aCount);
  try
    aCount := aFromStream.Read(data, aCount);
    Result := FStream.Write(data^, aCount);
  finally
    nsMemory.Free(data);
  end;
end;

function TOutputStream.WriteSegments(aReader: nsReadSegmentFun;
        aClosure: Pointer; aCount: PRUint32): PRUint32;
type
  nsReadSegmentFunc = function (aOutStream: nsIOutputStream;
                                aClosure: Pointer;
                                aToSegment: Pointer;
                                aFromOffset: PRUint32;
                                aCount: PRUint32
                               ): PRUint32; safecall;
var
  data: Pointer;
begin
  data := nsMemory.Alloc(aCount);
  try
    aCount := nsReadSegmentFunc(aReader)
            (Self, aClosure, data, FStream.Position, aCount);
    Result := FStream.Write(data^, aCount);
  finally
    nsMemory.Free(data);
  end;
end;

function TOutputStream.IsNonBlocking: PRBool;
begin
  Result := True;
end;

function NS_NewOutputStreamFromTStream(output: TStream; own: Boolean): nsIOutputStream;
begin
  Result := TOutputStream.Create(output, own);
end;


procedure CopyMemory(var ADst; const ASrc; ASize: Integer);
asm
  push edi
  push esi
  pushf

  mov edi, eax
  mov esi, edx
  mov edx, ecx
  shr ecx, 2
  cld
  test ecx, ecx
  jz @@no4

  rep movsd
@@no4:
  and edx, 3
  test edx, edx
  jz @@no1
  mov ecx, edx
  rep movsb
@@no1:
  popf
  pop esi
  pop edi
end;

end.
