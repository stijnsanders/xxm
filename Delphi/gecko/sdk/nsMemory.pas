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
unit nsMemory;

interface

uses
  nsXPCOM;

const
  NS_MEMORY_CONTRACTID = '@mozilla.org/xpcom/memory-service;1';
  NS_MEMORY_CLASSNAME = 'Global Memory Service';
  NS_MEMORY_CID: TGUID = '{30a04e40-38e7-11d4-8cf5-0060b0fc14a3}';

function Alloc(size: Integer): Pointer;
function Realloc(ptr: Pointer; size: Integer): Pointer;
function Free(ptr: Pointer): Integer;
function HeapMinimize(aImmediate: Boolean): Longword;
function Clone(Ptr: Pointer; size: Longword): Pointer;
function GetGlobalMemoryService: nsIMemory;

function GlueStartupMemory: Longword;
procedure GlueShutdownMemory;

procedure SetToMemoryManager;

implementation

uses
  nsError, nsInit;

var
  gMemory: nsIMemory;

procedure FreeGlobalMemory;
begin
  gMemory := nil;
end;

function SetupGlobalMemory: nsIMemory;
begin
  if Assigned(gMemory) then Exit;
  nsInit.NS_GetMemoryManager(gMemory);
  if not Assigned(gMemory) then Exit;
  Result := gMemory;
end;

function GlueStartupMemory: Longword;
begin
  Result := NS_ERROR_FAILURE;
  if Assigned(gMemory) then Exit;
  nsInit.NS_GetMemoryManager(gMemory);
  if not Assigned(gMemory) then Exit;
  Result := NS_OK;
end;

procedure GlueShutdownMemory;
begin
  gMemory := nil;
end;

function ENSURE_ALLOCATOR: Boolean;
begin
  Result := True;
  if not Assigned(gMemory) and not Assigned(SetupGlobalMemory()) then
    Result := False;
end;

function Alloc(size: Integer): Pointer;
begin
  Result := nil;
  if ENSURE_ALLOCATOR then
    Result := gMemory.Alloc(size);
end;

function Realloc(ptr: Pointer; size: Integer): Pointer;
begin
  Result := nil;
  if ENSURE_ALLOCATOR then
    Result := gMemory.Realloc(ptr, size);
end;

function Free(ptr: Pointer): Integer;
begin
  Result := NS_OK;
  if ENSURE_ALLOCATOR then gMemory.Free(ptr)
  else
    Result := Integer(NS_ERROR_UNEXPECTED);
end;

function HeapMinimize(aImmediate: Boolean): Longword;
begin
  Result := NS_ERROR_FAILURE;
  if ENSURE_ALLOCATOR then
  try
    Result := NS_OK;
    gMemory.HeapMinimize(aImmediate);
  except
    Result := NS_ERROR_FAILURE;
  end;
end;

function Clone(ptr: Pointer; size: Longword): Pointer;
begin
  Result := nil;
  if ENSURE_ALLOCATOR then
    Result := Clone(Ptr, size);
end;

function GetGlobalMemoryService: nsIMemory;
begin
  Result := nil;
  if not ENSURE_ALLOCATOR then Exit;
  Result := gMemory;
end;

const
  memmgr: TMemoryManager = (
    GetMem: Alloc;
    FreeMem: Free;
    ReallocMem: Realloc;
  );
procedure SetToMemoryManager;
begin
  SetMemoryManager(memmgr);
end;

end.
