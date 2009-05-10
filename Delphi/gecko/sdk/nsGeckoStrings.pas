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
 * The Original Code is GeckoSDK for Delphi.
 *
 * The Initial Developer of the Original Code is Takanori Ito.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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
unit nsGeckoStrings;

interface

uses
  nsConsts, nsTypes;

type

  nsAString = ^nsStringContainer;
  nsString = nsAString;
  nsStringContainer = record
    v: Pointer;
    d1: Pointer;
    d2: Longword;
    d3: Pointer;
  end;

  nsACString = ^nsCStringContainer;
  nsCString = nsACString;
  nsCStringContainer = record
    v: Pointer;
    d1: Pointer;
    d2: Longword;
    d3: Pointer;
  end;

  nsAUTF8String = nsACString;
  nsUTF8String = nsAUTF8String;
  nsUTF8StringContainer = nsCStringContainer;

  IInterfacedString = interface;
  IInterfacedCString = interface;
  IInterfacedUTF8String = interface;

  IInterfacedString = interface
  ['{0FA36E7C-6CA9-4D02-AF23-B49F35047181}']
    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedString); overload;
    procedure Assign(const Source: nsAString); overload;
    procedure Assign(Source: WideString); overload;
    procedure Append(Source: IInterfacedString); overload;
    procedure Append(const Source: nsAString); overload;
    procedure Append(Source: WideString); overload;
    procedure Insert(Source: IInterfacedString; aPosition: Longword); overload;
    procedure Insert(const Source: nsAString; aPosition: Longword); overload;
    procedure Insert(Source: WideString; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsAString); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedString); overload;
    function AString: nsAString;
    function ToString: WideString;
  end;

  IInterfacedCString = interface
  ['{3207A765-52D6-4E1C-B0F1-8EC39DA4D8B4}']
    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedCString); overload;
    procedure Assign(const Source: nsACString); overload;
    procedure Assign(Source: AnsiString); overload;
    procedure Append(Source: IInterfacedCString); overload;
    procedure Append(const Source: nsACString); overload;
    procedure Append(Source: AnsiString); overload;
    procedure Insert(Source: IInterfacedCString; aPosition: Longword); overload;
    procedure Insert(const Source: nsACString; aPosition: Longword); overload;
    procedure Insert(Source: AnsiString; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsACString); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedCString); overload;
    function ACString: nsACString;
    function ToString: AnsiString;
  end;

  IInterfacedUTF8String = interface
  ['{100ABB7D-46A0-4BB2-B5BF-A94F9E53B78C}']
    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedUTF8String); overload;
    procedure Assign(const Source: nsAUTF8String); overload;
    procedure Assign(Source: UTF8String); overload;
    procedure Append(Source: IInterfacedUTF8String); overload;
    procedure Append(const Source: nsAUTF8String); overload;
    procedure Append(Source: UTF8String); overload;
    procedure Insert(Source: IInterfacedUTF8String; aPosition: Longword); overload;
    procedure Insert(const Source: nsAUTF8String; aPosition: Longword); overload;
    procedure Insert(Source: UTF8String; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsAUTF8String); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedUTF8String); overload;
    function AUTF8String: nsAUTF8String;
    function ToString: UTF8String;
  end;

function Compare(const lhs, rhs: nsAString): Integer; overload;
function Compare(const lhs, rhs: nsACString): Integer; overload;

function NewString: IInterfacedString; overload;
function NewString(src: PWideChar): IInterfacedString; overload;
function NewString(src: WideString): IInterfacedString; overload;
function NewString(src: nsAString): IInterfacedString; overload;

function NewCString: IInterfacedCString; overload;
function NewCString(src: PAnsiChar): IInterfacedCString; overload;
function NewCString(src: AnsiString): IInterfacedCString; overload;
function NewCString(src: nsACString): IInterfacedCString; overload;

function NewUTF8String: IInterfacedUTF8String; overload;
function NewUTF8String(src: PAnsiChar): IInterfacedUTF8String; overload;
function NewUTF8String(src: UTF8String): IInterfacedUTF8String; overload;
function NewUTF8String(src: nsAUTF8String): IInterfacedUTF8String; overload;

function RelateString(src: nsAString; own: Boolean=False): IInterfacedString;
function RelateCString(src: nsACString; own: Boolean=False): IInterfacedCString;
function RelateUTF8String(src: nsAUTF8String; own: Boolean=False): IInterfacedUTF8String;

implementation

uses
  nsInit, nsMemory, nsError;

function Compare(const lhs, rhs: nsAString): Integer;
var
  p1, p2: PWideChar;
  l1, l2: Longword;
begin
  l1 := NS_StringGetData(lhs, p1);
  l2 := NS_StringGetData(rhs, p2);

  while (l1>0) and (l2>0) do
  begin
    Result := Ord(p1^) - Ord(p2^);
    if Result <> 0 then Exit;
    Dec(l1);
    Dec(l2);
    Inc(p1);
    Inc(p2);
  end;
  Result := l1 - l2;
end;

function Compare(const lhs, rhs: nsACString): Integer;
var
  p1, p2: PAnsiChar;
  l1, l2: Longword;
begin
  l1 := NS_CStringGetData(lhs, p1);
  l2 := NS_CStringGetData(rhs, p2);

  while (l1>0) and (l2>0) do
  begin
    Result := Ord(p1^) - Ord(p2^);
    if Result <> 0 then Exit;
    Dec(l1);
    Dec(l2);
    Inc(p1);
    Inc(p2);
  end;
  Result := l1 - l2;
end;

type
  TIStringImpl = class(TInterfacedObject, IInterfacedString)
    FContainer: nsStringContainer;
    FString: nsString;
    FOwn: Boolean;
  public
    constructor Create; overload;
    constructor Create(src: PWideChar); overload;
    constructor Create(src: WideString); overload;
    constructor Create(src: nsAString); overload;
    constructor Relate(src: nsAString; own: Boolean);
    destructor Destroy; override;

    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedString); overload;
    procedure Assign(const Source: nsAString); overload;
    procedure Assign(Source: WideString); overload;
    procedure Append(Source: IInterfacedString); overload;
    procedure Append(const Source: nsAString); overload;
    procedure Append(Source: WideString); overload;
    procedure Insert(Source: IInterfacedString; aPosition: Longword); overload;
    procedure Insert(const Source: nsAString; aPosition: Longword); overload;
    procedure Insert(Source: WideString; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsAString); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedString); overload;
    function AString: nsAString;
    function SIToString: WideString;
    function IInterfacedString.ToString = SIToString;
  end;

function NewString: IInterfacedString;
begin
  Result := TIStringImpl.Create;
end;

function NewString(src: PWideChar): IInterfacedString;
begin
  Result := TIStringImpl.Create(src);
end;

function NewString(src: WideString): IInterfacedString;
begin
  Result := TIStringImpl.Create(src);
end;

function NewString(src: nsAString): IInterfacedString;
begin
  Result := TIStringImpl.Create(src);
end;

function RelateString(src: nsAString; own: Boolean): IInterfacedString;
begin
  Result := TIStringImpl.Relate(src, own);
end;

constructor TIStringImpl.Create;
begin
  inherited Create;
  if NS_FAILED(NS_StringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
end;

constructor TIStringImpl.Create(src: PWideChar);
begin
  inherited Create;
  if NS_FAILED(NS_StringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
  //nsMemory.Free(src);
end;

constructor TIStringImpl.Create(src: WideString);
begin
  inherited Create;
  if NS_FAILED(NS_StringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TIStringImpl.Create(src: nsAString);
begin
  inherited Create;
  if NS_FAILED(NS_StringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TIStringImpl.Relate(src: nsAString; own: Boolean);
begin
  inherited Create;
  FString := src;
  FOwn := own;
end;

destructor TIStringImpl.Destroy;
begin
  if FOwn then
    NS_StringContainerFinish(FString^);
  inherited Destroy;
end;

function TIStringImpl.Length: Longword;
var
  temp: PWideChar;
begin
  Result := NS_StringGetData(FString, temp);
  //nsMemory.Free(temp);
end;

procedure TIStringImpl.Cut(cutStart, cutLength: Longword);
begin
  NS_StringCutData(FString, cutStart, cutLength);
end;

procedure TIStringImpl.Assign(Source: IInterfacedString);
begin
  NS_StringCopy(FString, Source.AString);
end;

procedure TIStringImpl.Assign(const Source: nsAString);
begin
  NS_StringCopy(FString, Source);
end;

procedure TIStringImpl.Assign(Source: WideString);
begin
  NS_StringSetData(FString, PWideChar(Source));
end;

procedure TIStringImpl.Append(Source: IInterfacedString);
begin
  NS_StringAppendData(FString, PWideChar(Source.ToString));
end;

procedure TIStringImpl.Append(const Source: nsAString);
var
  src2: IInterfacedString;
begin
  src2 := NewString(Source);
  Append(src2);
end;

procedure TIStringImpl.Append(Source: WideString);
var
  src2: IInterfacedString;
begin
  src2 := NewString(Source);
  Append(src2);
end;

procedure TIStringImpl.Insert(Source: IInterfacedString; aPosition: Longword);
begin
  NS_StringInsertData(FString, aPosition, PWideChar(Source.ToString));
end;

procedure TIStringImpl.Insert(const Source: nsAString; aPosition: Longword);
var
  src2: IInterfacedString;
begin
  src2 := NewString(Source);
  Insert(src2, aPosition);
end;

procedure TIStringImpl.Insert(Source: WideString; aPosition: Longword);
var
  src2: IInterfacedString;
begin
  src2 := NewString(Source);
  Insert(src2, aPosition);
end;

procedure TIStringImpl.Replace(aPosition, aLength: Longword; Source: IInterfacedString);
begin
  NS_StringSetDataRange(FString, aPosition, aLength, PWideChar(Source.ToString));
end;

procedure TIStringImpl.Replace(aPosition, aLength: Longword; const Source: nsAString);
var
  src2: IInterfacedString;
begin
  src2 := NewString(Source);
  Replace(aPosition, aLength, Src2);
end;

function TIStringImpl.AString: nsAString;
begin
  Result := FString;
end;

function TIStringImpl.SIToString: WideString;
var
  p: PWideChar;
  l: Longword;
  i: Longword;
begin
  l := NS_StringGetData(FString, p);
  SetLength(Result, l);
  for i:=1 to l do
  begin
    Result[i] := p^;
    Inc(p);
  end;
end;

type
  TICStringImpl = class(TInterfacedObject, IInterfacedCString)
    FContainer: nsCStringContainer;
    FString: nsCString;
    FOwn: Boolean;

    constructor Create; overload;
    constructor Create(src: AnsiString); overload;
    constructor Create(src: nsACString); overload;
    constructor Relate(src: nsACString; own: Boolean); overload;
    destructor Destroy; override;

    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedCString); overload;
    procedure Assign(const Source: nsACString); overload;
    procedure Assign(Source: AnsiString); overload;
    procedure Append(Source: IInterfacedCString); overload;
    procedure Append(const Source: nsACString); overload;
    procedure Append(Source: AnsiString); overload;
    procedure Insert(Source: IInterfacedCString; aPosition: Longword); overload;
    procedure Insert(const Source: nsACString; aPosition: Longword); overload;
    procedure Insert(Source: AnsiString; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsACString); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedCString); overload;
    function ACString: nsACString;
    function ToAnsiString: AnsiString;
    function IInterfacedCString.ToString = ToAnsiString;
  end;

  TIUTF8StringImpl = class(TInterfacedObject, IInterfacedUTF8String)
    FContainer: nsCStringContainer;
    FString: nsCString;
    FOwn: Boolean;

    constructor Create; overload;
    constructor Create(src: UTF8String); overload;
    constructor Create(src: nsAUTF8String); overload;
    constructor Relate(src: nsAUTF8String; own: Boolean); overload;
    destructor Destroy; override;

    function Length: Longword;
    procedure Cut(cutStart, cutLength: Longword);
    procedure Assign(Source: IInterfacedUTF8String); overload;
    procedure Assign(const Source: nsAUTF8String); overload;
    procedure Assign(Source: UTF8String); overload;
    procedure Append(Source: IInterfacedUTF8String); overload;
    procedure Append(const Source: nsAUTF8String); overload;
    procedure Append(Source: UTF8String); overload;
    procedure Insert(Source: IInterfacedUTF8String; aPosition: Longword); overload;
    procedure Insert(const Source: nsAUTF8String; aPosition: Longword); overload;
    procedure Insert(Source: UTF8String; aPosition: Longword); overload;
    procedure Replace(aPosition, aLength: Longword; const Source: nsAUTF8String); overload;
    procedure Replace(aPosition, aLength: Longword; Source: IInterfacedUTF8String); overload;
    function AUTF8String: nsAUTF8String;
    function ToUTF8String: UTF8String;
    function IInterfacedUTF8String.ToString = ToUTF8String;
  end;

function NewCString: IInterfacedCString;
begin
  Result := TICStringImpl.Create;
end;

function NewCString(src: PAnsiChar): IInterfacedCString;
begin
  Result := TICStringImpl.Create(src);
end;

function NewCString(src: AnsiString): IInterfacedCString;
begin
  Result := TICStringImpl.Create(src);
end;

function NewCString(src: nsACString): IInterfacedCString;
begin
  Result := TICStringImpl.Create(src);
end;

function RelateCString(src: nsACString; own: Boolean): IInterfacedCString;
begin
  Result := TICStringImpl.Relate(src, own);
end;

function NewUTF8String: IInterfacedUTF8String;
begin
  Result := TIUTF8StringImpl.Create;
end;

function NewUTF8String(src: PAnsiChar): IInterfacedUTF8String;
begin
  Result := TIUTF8StringImpl.Create(src);
end;

function NewUTF8String(src: UTF8String): IInterfacedUTF8String;
begin
  Result := TIUTF8StringImpl.Create(src);
end;

function NewUTF8String(src: nsAUTF8String): IInterfacedUTF8String;
begin
  Result := TIUTF8StringImpl.Create(src);
end;

function RelateUTF8String(src: nsAUTF8String; own: Boolean): IInterfacedUTF8String;
begin
  Result := TIUTF8StringImpl.Relate(src, own);
end;

constructor TICStringImpl.Create;
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  inherited Create;
end;

constructor TICStringImpl.Create(src: AnsiString);
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TICStringImpl.Create(src: nsACString);
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TICStringImpl.Relate(src: nsACString; own: Boolean);
begin
  inherited Create;
  FString := src;
  FOwn := own;
end;

destructor TICStringImpl.Destroy;
begin
  if FOwn then
    NS_CStringContainerFinish(FString^);
  inherited Destroy;
end;

function TICStringImpl.Length: Longword;
var
  temp: PAnsiChar;
begin
  Result := NS_CStringGetData(FString, temp);
end;

procedure TICStringImpl.Cut(cutStart, cutLength: Longword);
begin
  NS_CStringCutData(FString, cutStart, cutLength);
end;

procedure TICStringImpl.Assign(Source: IInterfacedCString);
begin
  NS_CStringCopy(FString, Source.ACString);
end;

procedure TICStringImpl.Assign(const Source: nsACString);
begin
  NS_CStringCopy(FString, Source);
end;

procedure TICStringImpl.Assign(Source: AnsiString);
begin
  NS_CStringSetData(FString, PAnsiChar(Source));
end;

procedure TICStringImpl.Append(Source: IInterfacedCString);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source.ACString, src2);
  NS_CStringAppendData(FString, src2);
end;

procedure TICStringImpl.Append(const Source: nsACString);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source, src2);
  NS_CStringAppendData(FString, src2);
end;

procedure TICStringImpl.Append(Source: AnsiString);
begin
  NS_CStringAppendData(FString, PAnsiChar(Source));
end;

procedure TICStringImpl.Insert(Source: IInterfacedCString; aPosition: Longword);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source.ACString, src2);
  NS_CStringInsertData(FString, aPosition, src2);
end;

procedure TICStringImpl.Insert(const Source: nsACString; aPosition: Longword);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source, src2);
  NS_CStringInsertData(FString, aPosition, src2);
end;

procedure TICStringImpl.Insert(Source: AnsiString; aPosition: Longword);
begin
  NS_CStringInsertData(FString, aPosition, PAnsiChar(Source));
end;

procedure TICStringImpl.Replace(aPosition, aLength: Longword; Source: IInterfacedCString);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source.ACString, src2);
  NS_CStringSetDataRange(FString, aPosition, aLength, src2);
end;

procedure TICStringImpl.Replace(aPosition, aLength: Longword; const Source: nsACString);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(Source, src2);
  NS_CStringSetDataRange(FString, aPosition, aLength, src2);
end;

function TICStringImpl.ACString: nsACString;
begin
  Result := FString;
end;

function TICStringImpl.ToAnsiString: AnsiString;
var
  p: PAnsiChar;
  l: Longword;
  i: Longword;
begin
  l := NS_CStringGetData(FString, p);
  SetLength(Result, l);
  for i:=1 to l do
  begin
    Result[i] := p^;
    Inc(p);
  end;
end;

constructor TIUTF8StringImpl.Create;
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  inherited Create;
end;

constructor TIUTF8StringImpl.Create(src: UTF8String);
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TIUTF8StringImpl.Create(src: nsAUTF8String);
begin
  inherited Create;
  if NS_FAILED(NS_CStringContainerInit(FContainer)) then
    Error(reOutOfMemory);
  FOwn := True;
  FString := @FContainer;
  Assign(src);
end;

constructor TIUTF8StringImpl.Relate(src: nsAUTF8String; own: Boolean);
begin
  inherited Create;
  FString := src;
  FOwn := own;
end;

destructor TIUTF8StringImpl.Destroy;
begin
  if FOwn then
    NS_CStringContainerFinish(FString^);
  inherited Destroy;
end;

function TIUTF8StringImpl.Length: Longword;
var
  temp: PAnsiChar;
begin
  Result := NS_CStringGetData(FString, temp);
end;

procedure TIUTF8StringImpl.Cut(cutStart: Cardinal; cutLength: Cardinal);
begin
  NS_CStringCutData(FString, cutStart, cutLength);
end;

procedure TIUTF8StringImpl.Assign(Source: IInterfacedUTF8String);
begin
  NS_CStringCopy(FString, Source.AUTF8String);
end;

procedure TIUTF8StringImpl.Assign(const Source: nsAUTF8String);
begin
  NS_CStringCopy(FString, Source);
end;

procedure TIUTF8StringImpl.Assign(Source: UTF8String);
begin
  NS_CStringSetData(FString, PAnsiChar(Source));
end;

procedure TIUTF8StringImpl.Append(Source: IInterfacedUTF8String);
begin
  NS_CStringAppendData(FString, PAnsiChar(Source.ToString));
end;

procedure TIUTF8StringImpl.Append(const Source: nsAUTF8String);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(source, src2);
  NS_CStringAppendData(FString, src2);
end;

procedure TIUTF8StringImpl.Append(Source: UTF8String);
begin
  NS_CStringAppendData(FString, PAnsiChar(Source));
end;

procedure TIUTF8StringImpl.Insert(Source: IInterfacedUTF8String; aPosition: Longword);
begin
  NS_CStringInsertData(FString, aPosition, PAnsiChar(Source.ToString));
end;

procedure TIUTF8StringImpl.Insert(const Source: nsAUTF8String; aPosition: Longword);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(source, src2);
  NS_CStringInsertData(FString, aPosition, src2);
end;

procedure TIUTF8StringImpl.Insert(Source: UTF8String; aPosition: Longword);
begin
  NS_CStringInsertData(FString, aPosition, PAnsiChar(Source));
end;

procedure TIUTF8StringImpl.Replace(aPosition, aLength: Longword; Source: IInterfacedUTF8String);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(source.AUTF8String, src2);
  NS_CStringSetDataRange(FString, aPosition, aLength, src2);
end;

procedure TIUTF8StringImpl.Replace(aPosition, aLength: Longword; const Source: nsAUTF8String);
var
  src2: PAnsiChar;
begin
  NS_CStringGetData(source, src2);
  NS_CStringSetDataRange(FString, aPosition, aLength, src2);
end;

function TIUTF8StringImpl.AUTF8String: nsAUTF8String;
begin
  Result := FString;
end;

function TIUTF8StringImpl.ToUTF8String:UTF8String;
var
  p: PAnsiChar;
  l: Longword;
  i: Longword;
begin
  l := NS_CStringGetData(FString, p);
  SetLength(Result, l);
  for i:=1 to l do
  begin
    Result[i] := p^;
    Inc(p);
  end;
end;

end.
