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
 * Portions created by the Initial Developer are Copyright (C) 2003
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
unit nsInit;

interface

uses
  nsXPCOM, nsConsts, nsTypes, nsGeckoStrings;

// XPCOM Functions
function NS_InitXPCOM2(out servMgr: nsIServiceManager; binDir: nsIFile; appFileLocationProvider: nsIDirectoryServiceProvider): nsresult; cdecl;
function NS_ShutdownXPCOM(servMgr: nsIServiceManager): nsresult; cdecl;
function NS_GetServiceManager(out servMgr: nsIServiceManager): nsresult; cdecl;
function NS_GetComponentManager(out compMgr: nsIComponentManager): nsresult; cdecl;
function NS_GetComponentRegistrar(out compReg: nsIComponentRegistrar): nsresult; cdecl;
function NS_GetMemoryManager(out memMgr: nsIMemory): nsresult; cdecl;
function NS_NewLocalFile(const Path: nsAString; FollowLinks: PRBool; out newFile: nsILocalFile): nsresult; cdecl;
function NS_NewNativeLocalFile(const Path: nsACString; FollowLinks: PRBool; out newFile: nsILocalFile): nsresult; cdecl;
function NS_GetDebug(out debug: nsIDebug): nsresult; cdecl;
function NS_GetTraceRefcnt(out traceRefcnt: nsITraceRefcnt): nsresult; cdecl;

type
  PLongBool = ^LongBool;

function NS_StringContainerInit(var aContainer: nsStringContainer): nsresult; cdecl;
procedure NS_StringContainerFinish(var aContainer: nsStringContainer); cdecl;
function NS_StringGetData(const aStr: nsAString; out aData: PWideChar; aTerminated: PLongBool=nil): nsresult; cdecl;
function NS_StringCloneData(const aStr: nsAString): PWideChar; cdecl;
procedure NS_StringSetData(aStr: nsAString; const aData: PWideChar; aDataLength: PRUint32 = High(PRUint32)); cdecl;
procedure NS_StringSetDataRange(aStr: nsAString; aCutOffset, aCutLength: Longword; const aData: PWideChar; aDataLength: PRUint32 = High(PRUint32)); cdecl;
procedure NS_StringCopy(aDestStr: nsAString; const aSrcStr: nsAString); cdecl;
procedure NS_StringAppendData(aStr: nsAString; const aData: PWideChar; aDataLength: PRUint32 = High(PRUint32));
procedure NS_StringInsertData(aStr: nsAString; aOffSet: PRUint32; const aData: PWideChar; aDataLength: PRUint32 = High(PRUint32));
procedure NS_StringCutData(aStr: nsAString; aCutOffset, aCutLength: PRUint32);

function NS_CStringContainerInit(var aContainer: nsCStringContainer): nsresult; cdecl;
procedure NS_CStringContainerFinish(var aContainer: nsCStringContainer); cdecl;
function NS_CStringGetData(const aStr: nsACString; out aData: PAnsiChar; aTerminated: PLongBool=nil): PRUint32; cdecl;
function NS_CStringCloneData(const aStr: nsACString): PAnsiChar; cdecl;
procedure NS_CStringSetData(aStr: nsACString; const aData: PAnsiChar; aDataLength: PRUint32 = High(PRUint32)); cdecl;
procedure NS_CStringSetDataRange(aStr: nsACString; aCutOffset, aCutLength: PRUint32; const aData: PAnsiChar; aDataLength: PRUint32 = High(PRUint32)); cdecl;
procedure NS_CStringCopy(aDestStr: nsACString; const aSrcStr: nsACString); cdecl;
procedure NS_CStringAppendData(aStr: nsACString; const aData: PAnsiChar; aDataLength: Longword = High(PRUint32));
procedure NS_CStringInsertData(aStr: nsACString; aOffSet: PRUint32; const aData: PAnsiChar; aDataLength: PRUint32 = High(PRUint32));
procedure NS_CStringCutData(aStr: nsACString; aCutOffset, aCutLength: PRUint32);

type
  nsSourceEncoding = ( NS_ENCODING_ASCII = 0,
                       NS_ENCODING_UTF8 = 1,
                       NS_ENCODING_NATIVE_FILESYSTEM = 2);

function NS_CStringToUTF16(const aSource: nsACString; aSrcEncoding: nsSourceEncoding; aDest: nsAString): Longword; cdecl;
function NS_UTF16ToCString(const aSource: nsAString; aSrcEncoding: nsSourceEncoding; aDest: nsACString): Longword; cdecl;

// Added for Gecko 1.8
type
  nsGetModuleProc = function (aCompMgr: nsIComponentManager; location: nsIFile; out return_cobj: nsIModule): nsresult; cdecl;

  PStaticModuleInfo = ^nsStaticModuleInfo;
  PStaticModuleInfoArray = ^nsStaticModuleInfoArray;
  nsStaticModuleInfo = record
    name: PChar;
    getModule: nsGetModuleProc;
  end;
  nsStaticModuleInfoArray = array[0..MaxInt div Sizeof(nsStaticModuleInfo)-1] of nsStaticModuleInfo;

function NS_Alloc(size: PRSize): Pointer; cdecl;
function NS_Realloc(ptr: Pointer; size: PRSize): Pointer; cdecl;
procedure NS_Free(ptr: Pointer); cdecl;
function NS_InitXPCOM3(out servMgr: nsIServiceManager; binDir: nsIFile; appFileLocationProvider: nsIDirectoryServiceProvider; staticComponents: nsStaticModuleInfoArray; componentCount: PRint32): nsresult; cdecl;

function NS_StringContainerInit2(var aContainer: nsStringContainer; const aStr: PWideChar; aOffset, aLength: PRUint32): nsresult; cdecl;
procedure NS_StringSetIsVoid(aStr: nsAString; const aIsVoid: PRBool); cdecl;
function NS_StringGetIsVoid(const aStr: nsAString): PRBool; cdecl;

function NS_CStringContainerInit2(var aContainer: nsCStringContainer; const aStr: PAnsiChar; aOffset, aLength: PRUint32): nsresult; cdecl;
procedure NS_CStringSetIsVoid(aStr: nsACString; const aIsVoid: PRBool); cdecl;
function NS_CStringGetIsVoid(const aStr: nsACString): PRBool; cdecl;

// Added for Gecko 1.9
const
  TD_INT8               = 0;
  TD_INT16              = 1;
  TD_INT32              = 2;
  TD_INT64              = 3;
  TD_UINT8              = 4;
  TD_UINT16             = 5;
  TD_UINT32             = 6;
  TD_UINT64             = 7;
  TD_FLOAT              = 8;
  TD_DOUBLE             = 9;
  TD_BOOL               = 10;
  TD_CHAR               = 11;
  TD_WCHAR              = 12;
  TD_VOID               = 13;
  TD_PNSIID             = 14;
  TD_DOMSTRING          = 15;
  TD_PSTRING            = 16;
  TD_PWSTRING           = 17;
  TD_INTERFACE_TYPE     = 18;
  TD_INTERFACE_IS_TYPE  = 19;
  TD_ARRAY              = 20;
  TD_PSTRING_SIZE_IS    = 21;
  TD_PWSTRING_SIZE_IS   = 22;
  TD_UTF8STRING         = 23;
  TD_CSTRING            = 24;
  TS_ASTRING            = 25;

type
  nsXPTType = record
    flags: PRUint8;
  end;

  nsxPTCMiniVariant = record
  case Integer of
  TD_INT8:              (i8: PRInt8);
  TD_INT16:             (i16: PRInt16);
  TD_INT32:             (i32: PRInt32);
  TD_INT64:             (i64: PRInt64);
  TD_UINT8:             (u8: PRUint8);
  TD_UINT16:            (u16: PRUint16);
  TD_UINT32:            (u32: PRUint32);
  TD_UINT64:            (u64: PRUint64);
  TD_FLOAT:             (f: Single);
  TD_DOUBLE:            (d: Double);
  TD_BOOL:              (b: PRBool);
  TD_CHAR:              (c: AnsiChar);
  TD_WCHAR:             (w: WideChar);
  TD_VOID:              (p: Pointer);
  end;

  nsXPTCVariant = record
    val: nsXPTCMiniVariant;
    ptr: Pointer;
    type_: nsXPTType;
    flags: PRUint32;
  end;

  PXPTCVariantArray = ^nsXPTCVariantArray;
  nsXPTCVariantArray = array [0..MaxInt div SizeOf(nsXPTCVariant)-1] of nsXPTCVariant;
  PXPTCMiniVariantArray = ^nsXPTCMiniVariantArray;
  nsXPTCMiniVariantArray = array [0..MaxInt div SizeOf(nsXPTCMiniVariant)-1] of nsXPTCMiniVariant;

  XPTTypeDescriptorPrefix = record
    flags: PRUint8;
  end;

  XPTTypeDescriptor = record
    prefix: XPTTypeDescriptorPrefix;
    argnum: PRUint8;
    argnim2: PRUint8;
    case Integer of
    TD_INTERFACE_TYPE:  (iface: PRUint16);
    TD_ARRAY:           (additional_type: PRUint16);
  end;

  PXPTParamDescriptor = ^XPTParamDescriptor;
  XPTParamDescriptor = record
    flags: PRUint8;
    type_: XPTTypeDescriptor;
  end;
  PXPTParamDescriptorArray = ^XPTParamDescriptorArray;
  XPTParamDescriptorArray = array [0..MaxInt div SizeOf(XPTParamDescriptor)-1] of XPTParamDescriptor;

  XPTMethodDescriptor = record
    name: PAnsiChar;
    params: PXPTParamDescriptorArray;
    result: PXPTParamDescriptor;
    flags: PRUint8;
    num_args: PRUint8;
  end;

  nsIXPTCProxy = interface(nsISupports)
    function CallMethod(aMethodIndex: PRUint16; const aInfo: XPTMethodDescriptor; aParams: PXPTCMiniVariantArray): nsresult; stdcall;
  end;


procedure NS_DebugBreak(aSeverity: PRUint32; aStr: PAnsiChar; aExpr: PAnsiChar; aFile: PAnsiChar; aLine: PRUint32); cdecl;
procedure NS_LogInit(); cdecl;
procedure NS_LogTerm(); cdecl;
procedure NS_LogAddRef(aPtr: Pointer; aNewRefCnt: nsrefcnt; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
procedure NS_LogRelease(aPtr: Pointer; aNewRefCnt: nsrefcnt; aTypeName: PAnsiChar); cdecl;
procedure NS_LogCtor(aPtr: Pointer; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
procedure NS_LogDtor(aPtr: Pointer; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
procedure NS_LogCOMPtrAddRef(aCOMPtr: Pointer; aObject: nsISupports); cdecl;
procedure NS_LogCOMPtrRelease(aCOMPtr: Pointer; aObject: nsISupports); cdecl;

function NS_GetXPTCallStub(aIID: TGUID; aOuter: nsIXPTCProxy; out aStub): nsresult; cdecl;
procedure NS_DestroyXPTCallStub(aStub: nsISupports); cdecl;
function NS_InvokeByIndex(that: nsISupports; methodIndex: PRUint32; paramCount: PRUint32; params: PXPTCVariantArray): nsresult; cdecl;

// GLUE Types
type
  PGREVersionRange = ^TGREVersionRange;
  TGREVersionRange = record
    lower: PAnsiChar;
    lowerInclusive: PRBool;
    upper: PAnsiChar;
    upperInclusive: PRBool;
  end;
  PGREVersionRangeArray = ^TGREVersionRangeArray;
  TGREVersionRangeArray = array [0..MaxInt div SizeOf(TGREVersionRange)-1] of TGREVersionRange;

  PGREProperty = ^TGREProperty;
  TGREProperty = record
    property_: PAnsiChar;
    value: PAnsiChar;
  end;
  PGREPropertyArray = ^TGREPropertyArray;
  TGREPropertyArray = array [0..MaxInt div SizeOf(TGREProperty)-1] of TGREProperty;

  PNSFuncPtr = ^NSFuncPtr;
  NSFuncPtr = procedure (); cdecl;
  PDynamicFunctionLoad = ^TDynamicFunctionLoad;
  TDynamicFunctionLoad = record
    functionName: PAnsiChar;
    function_: Pointer;
  end;
  PDynamicFunctionLoadArray = ^TDynamicFunctionLoadArray;
  TDynamicFunctionLoadArray = array [
    0..MaxInt div SizeOf(TDynamicFunctionLoad)-1
    ] of TDynamicFunctionLoad;

// GLUE Functions
function GRE_GetGREPathWithProperties(
                aVersions: PGREVersionRange;
                versionsLength: PRUint32;
                aProperties: PGREProperty;
                propertiesLength: PRUint32;
                buf: PAnsiChar; buflen: PRUint32): nsresult;

function NS_CompareVersions(lhs, rhs: PAnsiChar): PRInt32;

function XPCOMGlueStartup(xpcomFile: PAnsiChar): nsresult;
function XPCOMGlueShutdown: nsresult;
function XPCOMGlueLoadXULFunctions(aSymbols: PDynamicFunctionLoad): nsresult;

function GRE_Startup: Longword;
function GRE_Shutdown: Longword;


// PChar functions
function NS_StrLen(const Str: PAnsiChar): Cardinal; overload;
function NS_StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; overload;
function NS_StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; maxLen: Cardinal): PAnsiChar; overload;
function NS_StrCat(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; overload;
function NS_StrLCat(Dest: PAnsiChar; const Source: PAnsiChar; maxLen: Cardinal): PAnsiChar; overload;
function NS_StrComp(const Str1, Str2: PAnsiChar): Integer; overload;
function NS_StrRScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; overload;

function NS_StrLen(const Str: PWideChar): Cardinal; overload;
function NS_StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; overload;
function NS_StrLCopy(Dest: PWideChar; const Source: PWideChar; maxLen: Cardinal): PWideChar; overload;
function NS_StrCat(Dest: PWideChar; const Source: PWideChar): PWideChar; overload;
function NS_StrLCat(Dest: PWideChar; const Source: PWideChar; maxLen: Cardinal): PWideChar; overload;
function NS_StrComp(const Str1, Str2: PWideChar): Integer; overload;
function NS_StrRScan(const Str: PWideChar; Chr: WideChar): PWideChar; overload;

function NS_CurrentProcessDirectory(buf: PAnsiChar; bufLen: Cardinal): Boolean;

implementation

uses
  Windows, nsError, nsMemory, SysUtils;

type
  XPCOMExitRoutine = function : Longword; stdcall;

  InitFunc = function(out servMgr: nsIServiceManager; binDir: nsIFile; provider: nsIDirectoryServiceProvider): Longword; cdecl;
  ShutdownFunc = function (servMgr: nsIServiceManager): Longword; cdecl;
  GetServiceManagerFunc = function (out servMgr: nsIServiceManager): Longword; cdecl;
  GetComponentManagerFunc = function (out compMgr: nsIComponentManager): Longword; cdecl;
  GetComponentRegistrarFunc = function (out compReg: nsIComponentRegistrar): Longword; cdecl;
  GetMemoryManagerFunc = function (out memMgr: nsIMemory): Longword; cdecl;
  NewLocalFileFunc = function (const path: nsAString; followLinks: LongBool; out newFile: nsILocalFile): Longword; cdecl;
  NewNativeLocalFileFunc = function (const path: nsACString; followLinks: LongBool; out newFile: nsILocalFile): Longword; cdecl;
  RegisterXPCOMExitRoutineFunc = function (exitRoutine: XPCOMExitRoutine; proproty: Longword): Longword; cdecl;
  UnregisterXPCOMExitRoutineFunc = function (exitRoutine: XPCOMExitRoutine): Longword; cdecl;
  // Added for Gecko 1.8
  AllocFunc = function (size: PRSize): Pointer; cdecl;
  ReallocFunc = function (ptr: Pointer; size: PRSize): Pointer; cdecl;
  FreeFunc = procedure (ptr: Pointer); cdecl;
  Init3Func = function (out servMgr: nsIServiceManager; binDir: nsIFile; provider: nsIDirectoryServiceProvider; const staticComponents: nsStaticModuleInfoArray; componentCount: PRUint32): nsresult; cdecl;

  GetDebugFunc = function (out debug: nsIDebug): Longword; cdecl;
  GetTraceRefcntFunc = function (out traceRefcnt: nsITraceRefcnt): Longword; cdecl;
  // Added for Gecko 1.9
  DebugBreakFunc = procedure (aSeverity: PRUint32; aStr: PAnsiChar; aExpr: PAnsiChar; aFile: PAnsiChar; aLine: PRUint32); cdecl;
  XPCOMVoidFunc = procedure (); cdecl;
  LogAddRefFunc = procedure (var aPtr; refcnt: nsrefcnt; name: PAnsiChar; count: PRUint32); cdecl;
  LogReleaseFunc = procedure (var aPtr; refcnt: nsrefcnt; name: PAnsiChar); cdecl;
  LogCtorFunc = procedure (var aPtr; name: PAnsiChar; ident: PRUint32); cdecl;
  LogCOMPtrFunc = procedure (var aPtr; aIntf: nsISupports); cdecl;
  GetXPTCallStubFunc = function (const guid: TGUID; proxy: nsIXPTCProxy; out aIntf): nsresult; cdecl;
  DestroyXPTCallStubFunc = procedure (aStub: nsISupports); cdecl;
  InvokeByIndexFunc = function (aStub: nsISupports; methodIndex: PRUint32; paramCount: PRUint32; params: PXPTCVariantArray): nsresult; cdecl;
  CycleCollectorFunc = function (aStub: nsISupports): PRBool; cdecl;

  StringContainerInitFunc = function (var container: nsStringContainer): Longword; cdecl;
  StringContainerFinishFunc = procedure (var container: nsStringContainer); cdecl;
  StringGetDataFunc = function (const str: nsAString; out data: PWideChar; aTerminated: PLongBool): Longword; cdecl;
  StringCloneDataFunc = function (const str: nsAString): PWideChar; cdecl;
  StringSetDataFunc = procedure (str: nsAString; const data: PWideChar; length: Longword); cdecl;
  StringSetDataRangeFunc = procedure (str: nsAString; aCutOffset, aCutLength: Longword; const data: PWideChar; length: Longword); cdecl;
  StringCopyFunc = procedure (dst: nsAString; const src: nsAString); cdecl;
  // Added for Gecko 1.8
  StringContainerInit2Func = function (var container: nsStringContainer; const str: PWideChar; offset, length: PRUint32): nsresult; cdecl;
  StringGetMutableDataFunc = function (container: nsAString; aLength: PRUint32; out retval: PWideChar): PRUint32; cdecl;
  // Added for Gecko 1.9
  StringSetIsVoidFunc = procedure (dst: nsAString; const isVoid: PRBool); cdecl;
  StringGetIsVoidFunc = function (src: nsAString): PRBool; cdecl;

  CStringContainerInitFunc = function (var container: nsCStringContainer): Longword; cdecl;
  CStringContainerFinishFunc = procedure (var container: nsCStringContainer); cdecl;
  CStringGetDataFunc = function (const str: nsACString; out data: PAnsiChar; aTerminated: PLongBool): Longword; cdecl;
  CStringCloneDataFunc = function (const str: nsACString): PAnsiChar; cdecl;
  CStringSetDataFunc = procedure (str: nsACString; const data: PAnsiChar; length: Longword); cdecl;
  CStringSetDataRangeFunc = procedure (str: nsACString; aCutOffset, aCutLength: Longword; const data: PAnsiChar; length: Longword); cdecl;
  CStringCopyFunc = procedure (dst: nsACString; const src: nsACString); cdecl;
  // Added for Gecko 1.8
  CStringContainerInit2Func = function (var container: nsCStringContainer; const str: PAnsiChar; offset, length: PRUint32): nsresult; cdecl;
  CStringGetMutableDataFunc = function (container: nsACString; aLength: PRUint32; out retval: PAnsiChar): PRUint32; cdecl;
  // Added for Gecko 1.9
  CStringSetIsVoidFunc = procedure (dst: nsACString; const isVoid: PRBool); cdecl;
  CStringGetIsVoidFunc = function (src: nsACString): PRBool; cdecl;

  CStringToUTF16Func = function (const src: nsACString; encoding: nsSourceEncoding; dst: nsAString): Longword; cdecl;
  UTF16ToCStringFunc = function (const src: nsAString; encoding: nsSourceEncoding; dst: nsACString): Longword; cdecl;

  XPCOMFunctions = record
    version: Longword;
    size: Longword;

    init: InitFunc;
    shutdown: ShutdownFunc;
    getServiceManager: GetServiceManagerFunc;
    getComponentManager: GetComponentManagerFunc;
    getComponentRegistrar: GetComponentRegistrarFunc;
    getMemoryManager: GetMemoryManagerFunc;
    newLocalFile: NewLocalFileFunc;
    newNativeLocalFile: NewNativeLocalFileFunc;

    registerXPCOMExitRoutine: RegisterXPCOMExitRoutineFunc;
    unregisterXPCOMExitRoutine: UnregisterXPCOMExitRoutineFunc;

    // Added for Gecko 1.5
    getDebug: GetDebugFunc;
    getTraceRefCnt: GetTraceRefCntFunc;

    // Added for Gecko 1.7
    stringContainerInit: StringContainerInitFunc;
    stringContainerFinish: StringContainerFinishFunc;
    stringGetData: StringGetDataFunc;
    stringSetData: StringSetDataFunc;
    stringSetDataRange: StringSetDataRangeFunc;
    stringCopy: StringCopyFunc;
    cstringContainerInit: CStringContainerInitFunc;
    cstringContainerFinish: CStringContainerFinishFunc;
    cstringGetData: CStringGetDataFunc;
    cstringSetData: CStringSetDataFunc;
    cstringSetDataRange: CStringSetDataRangeFunc;
    cstringCopy: CStringCopyFunc;
    cstringToUTF16: CStringToUTF16Func;
    UTF16ToCString: UTF16ToCStringFunc;
    stringCloneData: StringCloneDataFunc;
    cstringCloneData: CStringCloneDataFunc;

    // Added for Gecko 1.8
    alloc: AllocFunc;
    realloc: ReallocFunc;
    free: FreeFunc;
    stringContainerInit2: StringContainerInit2Func;
    cstringContainerInit2: CStringContainerInit2Func;
    stringGetMutableData: StringGetMutableDataFunc;
    cstringGetMutableData: CStringGetMutableDataFunc;
    init3: Init3Func;

    // Added for Gecko 1.9
    debugBreak: DebugBreakFunc;
    logInit: xpcomVoidFunc;
    logTerm: xpcomVoidFunc;
    logAddRef: LogAddRefFunc;
    logRelease: LogReleaseFunc;
    logCtor: LogCtorFunc;
    logDtor: LogCtorFunc;
    logCOMPtrAddRef: LogCOMPtrFunc;
    logCOMPtrRelease: LogCOMPtrFunc;
    getXPTCallStub: GetXPTCallStubFunc;
    destroyXPTCallStub: DestroyXPTCallStubFunc;
    invokeByIndex: InvokeByIndexFunc;
    cycleSuspect: CycleCollectorFunc;
    cycleForget: CycleCollectorFunc;
    stringSetIsVoid: StringSetIsVoidFunc;
    stringGetIsVoid: StringGetIsVoidFunc;
    cstringSetIsVoid: CStringSetIsVoidFunc;
    cstringGetIsVoid: CStringGetIsVoidFunc;
  end;

  GetFrozenFunctionsFunc = function(out enrtyPoints: XPCOMFunctions; libraryPath: PAnsiChar): Longword; cdecl;

var
  xpcomFunc: XPCOMFunctions;

function NS_InitXPCOM2(out servMgr: nsIServiceManager;
                       binDir: nsIFile;
                       appFileLocationProvider: nsIDirectoryServiceProvider): Longword;
begin
  if Assigned(xpcomFunc.init) then
    Result := xpcomFunc.init(servMgr, binDir, appFileLocationProvider)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_ShutdownXPCOM(servMgr: nsIServiceManager): Longword;
begin
  if Assigned(xpcomFunc.shutdown) then
    Result := xpcomFunc.shutdown(servMgr)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetServiceManager(out servMgr: nsIServiceManager): Longword;
begin
  if Assigned(xpcomFunc.getServiceManager) then
    Result := xpcomFunc.getServiceManager(servMgr)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetComponentManager(out compMgr: nsIComponentManager): nsresult;
begin
  if Assigned(xpcomFunc.getComponentManager) then
    Result := xpcomFunc.getComponentManager(compMgr)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetComponentRegistrar(out compReg: nsIComponentRegistrar): nsresult;
begin
  if Assigned(xpcomFunc.getComponentRegistrar) then
    Result := xpcomFunc.getComponentRegistrar(compReg)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetMemoryManager(out memMgr: nsIMemory): nsresult;
begin
  if Assigned(xpcomFunc.getMemoryManager) then
    Result := xpcomFunc.getMemoryManager(memMgr)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_NewLocalFile(const path: nsAString;
                         followLinks: PRBool;
                         out newFile: nsILocalFile): nsresult;
begin
  if Assigned(xpcomFunc.newLocalFile) then
    Result := xpcomFunc.newLocalFile(path, followLinks, newFile)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_NewNativeLocalFile(const path: nsACString;
                               followLinks: PRBool;
                               out newFile: nsILocalFile): nsresult;
begin
  if Assigned(xpcomFunc.newNativeLocalFile) then
    Result := xpcomFunc.newNativeLocalFile(path, followLinks, newFile)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_RegisterXPCOMExitRoutine(exitRoutine: XPCOMExitRoutine;
                                     priority: Longword): Longword;
begin
  if Assigned(xpcomFunc.registerXPCOMExitRoutine) then
    Result := xpcomFunc.registerXPCOMExitRoutine(exitRoutine, priority)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_UnregisterXPCOMExitRoutine(exitRoutine: XPCOMExitRoutine): Longword;
begin
  if Assigned(xpcomFunc.unregisterXPCOMExitRoutine) then
    Result := xpcomFunc.unregisterXPCOMExitRoutine(exitRoutine)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetDebug(out debug: nsIDebug): Longword;
begin
  if Assigned(xpcomFunc.getDebug) then
    Result := xpcomFunc.getDebug(debug)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_GetTraceRefcnt(out traceRefcnt: nsITraceRefcnt): Longword;
begin
  if Assigned(xpcomFunc.getTraceRefCnt) then
    Result := xpcomFunc.getTraceRefCnt(traceRefcnt)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_StringContainerInit(var aContainer: nsStringContainer): Longword;
begin
  if Assigned(xpcomFunc.stringContainerInit) then
    Result := xpcomFunc.stringContainerInit(aContainer)
  else
    Result := NS_ERROR_FAILURE;
end;

procedure NS_StringContainerFinish(var aContainer: nsStringContainer);
begin
  if Assigned(xpcomFunc.stringContainerFinish) then
    xpcomFunc.stringContainerFinish(aContainer);
end;

function NS_StringGetData(const aStr: nsAString; out aData: PWideChar; aTerminated: PLongBool): Longword;
begin
  if Assigned(xpcomFunc.stringGetData) then
    Result := xpcomFunc.stringGetData(aStr, aData, aTerminated)
  else
    Result := 0;
end;

procedure NS_StringSetData(aStr: nsAString; const aData: PWideChar; aDataLength: Longword);
begin
  if Assigned(xpcomFunc.stringSetData) then
    xpcomFunc.stringSetData(aStr, aData, aDataLength);
end;

procedure NS_StringSetDataRange(aStr: nsAString; aCutOffset, aCutLength: Longword; const aData: PWideChar; aDataLength: Longword);
begin
  if Assigned(xpcomFunc.stringSetDataRange) then
    xpcomFunc.stringSetDataRange(aStr, aCutOffset, aCutLength, aData, aDataLength);
end;

procedure NS_StringCopy(aDestStr: nsAString; const aSrcStr: nsAString);
begin
  if Assigned(xpcomFunc.stringCopy) then
    xpcomFunc.stringCopy(aDestStr, aSrcStr);
end;

procedure NS_StringAppendData(aStr: nsAString; const aData: PWideChar; aDataLength: Longword);
begin
  NS_StringSetDataRange(aStr, High(Longword), 0, aData, aDataLength);
end;

procedure NS_StringInsertData(aStr: nsAString; aOffSet: Longword; const aData: PWideChar; aDataLength: Longword);
begin
  NS_StringSetDataRange(aStr, aOffset, 0, aData, aDataLength);
end;

procedure NS_StringCutData(aStr: nsAString; aCutOffset, aCutLength: Longword);
begin
  NS_StringSetDataRange(aStr, aCutOffset, aCutLength, nil, 0);
end;

function NS_CStringContainerInit(var aContainer: nsCStringContainer): Longword;
begin
  if Assigned(xpcomFunc.cstringContainerInit) then
    Result := xpcomFunc.cstringContainerInit(aContainer)
  else
    Result := NS_ERROR_FAILURE;
end;

procedure NS_CStringContainerFinish(var aContainer: nsCStringContainer);
begin
  if Assigned(xpcomFunc.cstringContainerFinish) then
    xpcomFunc.cstringContainerFinish(aContainer);
end;

function NS_CStringGetData(const aStr: nsACString; out aData: PAnsiChar; aTerminated: PLongBool): Longword;
begin
  if Assigned(xpcomFunc.cstringGetData) then
    Result := xpcomFunc.cstringGetData(aStr, aData, aTerminated)
  else
    Result := 0;
end;

procedure NS_CStringSetData(aStr: nsACString; const aData: PAnsiChar; aDataLength: Longword);
begin
  if Assigned(xpcomFunc.cstringSetData) then
    xpcomFunc.cstringSetData(aStr, aData, aDataLength);
end;

procedure NS_CStringSetDataRange(aStr: nsACString; aCutOffset, aCutLength: Longword; const aData: PAnsiChar; aDataLength: Longword);
begin
  if Assigned(xpcomFunc.cstringSetDataRange) then
    xpcomFunc.cstringSetDataRange(aStr, aCutOffset, aCutLength, aData, aDataLength);
end;

procedure NS_CStringCopy(aDestStr: nsACString; const aSrcStr: nsACString);
begin
  if Assigned(xpcomFunc.cstringCopy) then
    xpcomFunc.cstringCopy(aDestStr, aSrcStr);
end;

procedure NS_CStringAppendData(aStr: nsACString; const aData: PAnsiChar; aDataLength: Longword);
begin
  NS_CStringSetDataRange(aStr, High(Longword), 0, aData, aDataLength);
end;

procedure NS_CStringInsertData(aStr: nsACString; aOffSet: Longword; const aData: PAnsiChar; aDataLength: Longword);
begin
  NS_CStringSetDataRange(aStr, aOffset, 0, aData, aDataLength);
end;

procedure NS_CStringCutData(aStr: nsACString; aCutOffset, aCutLength: Longword);
begin
  NS_CStringSetDataRange(aStr, aCutOffset, aCutLength, nil, 0);
end;

function NS_CStringToUTF16(const aSource: nsACString; aSrcEncoding: nsSourceEncoding; aDest: nsAString): Longword;
begin
  if Assigned(xpcomFunc.cstringToUTF16) then
    Result := xpcomFunc.cstringToUTF16(aSource, aSrcEncoding, aDest)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_UTF16ToCString(const aSource: nsAString; aSrcEncoding: nsSourceEncoding; aDest: nsACString): Longword;
begin
  if Assigned(xpcomFunc.UTF16ToCString) then
    Result := xpcomFunc.UTF16ToCString(aSource, aSrcEncoding, aDest)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_StringCloneData(const aStr: nsAString): PWideChar;
begin
  if Assigned(xpcomFunc.stringCloneData) then
    Result := xpcomFunc.stringCloneData(aStr)
  else
    Result := nil;
end;

function NS_CStringCloneData(const aStr: nsACString): PAnsiChar;
begin
  if Assigned(xpcomFunc.cstringCloneData) then
    Result := xpcomFunc.cstringCloneData(aStr)
  else
    Result := nil;
end;

// Added for Gecko 1.8
function NS_Alloc(size: PRSize): Pointer; cdecl;
begin
  if Assigned(xpcomFunc.alloc) then
    Result := xpcomFunc.alloc(size)
  else
    Result := nil;
end;

function NS_Realloc(ptr: Pointer; size: PRSize): Pointer; cdecl;
begin
  if Assigned(xpcomFunc.realloc) then
    Result := xpcomFunc.realloc(ptr, size)
  else
    Result := nil;
end;

procedure NS_Free(ptr: Pointer); cdecl;
begin
  if Assigned(xpcomFunc.free) then
    xpcomFunc.free(ptr);
end;

function NS_InitXPCOM3(out servMgr: nsIServiceManager; binDir: nsIFile; appFileLocationProvider: nsIDirectoryServiceProvider; staticComponents: nsStaticModuleInfoArray; componentCount: PRint32): nsresult; cdecl;
begin
  if Assigned(xpcomFunc.init3) then
    Result := xpcomFunc.init3(servMgr, binDir, appFileLocationProvider, staticComponents, componentCount)
  else
    Result := NS_ERROR_FAILURE;
end;

function NS_StringContainerInit2(var aContainer: nsStringContainer; const aStr: PWideChar; aOffset, aLength: PRUint32): nsresult; cdecl;
begin
  if Assigned(xpcomFunc.stringContainerInit2) then
    Result := xpcomFunc.stringContainerInit2(aContainer, aStr, aOffset, aLength)
  else
    Result := NS_ERROR_FAILURE;
end;

procedure NS_StringSetIsVoid(aStr: nsAString; const aIsVoid: PRBool); cdecl;
begin
  if Assigned(xpcomFunc.stringSetIsVoid) then
    xpcomFunc.stringSetIsVoid(aStr, aIsVoid);
end;

function NS_StringGetIsVoid(const aStr: nsAString): PRBool; cdecl;
begin
  if Assigned(xpcomFunc.stringGetIsVoid) then
    Result := xpcomFunc.stringGetIsVoid(aStr)
  else
    Result := false;
end;

function NS_CStringContainerInit2(var aContainer: nsCStringContainer; const aStr: PAnsiChar; aOffset, aLength: PRUint32): nsresult; cdecl;
begin
  if Assigned(xpcomFunc.cstringContainerInit2) then
    Result := xpcomFunc.cstringContainerInit2(aContainer, aStr, aOffset, aLength)
  else
    Result := NS_ERROR_FAILURE;
end;

procedure NS_CStringSetIsVoid(aStr: nsACString; const aIsVoid: PRBool); cdecl;
begin
  if Assigned(xpcomFunc.cstringSetIsVoid) then
    xpcomFunc.cstringSetIsVoid(aStr, aIsVoid);
end;

function NS_CStringGetIsVoid(const aStr: nsACString): PRBool; cdecl;
begin
  if Assigned(xpcomFunc.cstringGetIsVoid) then
    Result := xpcomFunc.cstringGetIsVoid(aStr)
  else
    Result := false;
end;

// Added for Gecko 1.9
procedure NS_DebugBreak(aSeverity: PRUint32; aStr: PAnsiChar; aExpr: PAnsiChar; aFile: PAnsiChar; aLine: PRUint32); cdecl;
begin
  if Assigned(xpcomFunc.debugBreak) then
    xpcomFunc.debugBreak(aSeverity, aStr, aExpr, aFile, aLine);
end;

procedure NS_LogInit(); cdecl;
begin
  if Assigned(xpcomFunc.logInit) then
    xpcomFunc.logInit;
end;

procedure NS_LogTerm(); cdecl;
begin
  if Assigned(xpcomFunc.logTerm) then
    xpcomFunc.logTerm;
end;

procedure NS_LogAddRef(aPtr: Pointer; aNewRefCnt: nsrefcnt; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
begin
  if Assigned(xpcomFunc.logAddRef) then
    xpcomFunc.logAddRef(aPtr, aNewRefCnt, aTypeName, aInstanceSize);
end;

procedure NS_LogRelease(aPtr: Pointer; aNewRefCnt: nsrefcnt; aTypeName: PAnsiChar); cdecl;
begin
  if Assigned(xpcomFunc.logRelease) then
    xpcomFunc.logRelease(aPtr, aNewRefCnt, aTypeName);
end;

procedure NS_LogCtor(aPtr: Pointer; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
begin
  if Assigned(xpcomFunc.logCtor) then
    xpcomFunc.logCtor(aPtr, aTypeName, aInstanceSize);
end;

procedure NS_LogDtor(aPtr: Pointer; aTypeName: PAnsiChar; aInstanceSize: PRUint32); cdecl;
begin
  if Assigned(xpcomFunc.logDtor) then
    xpcomFunc.logDtor(aPtr, aTypeName, aInstanceSize);
end;

procedure NS_LogCOMPtrAddRef(aCOMPtr: Pointer; aObject: nsISupports); cdecl;
begin
  if Assigned(xpcomFunc.logCOMPtrAddRef) then
    xpcomFunc.logCOMPtrAddRef(aCOMPtr, aObject);
end;

procedure NS_LogCOMPtrRelease(aCOMPtr: Pointer; aObject: nsISupports); cdecl;
begin
  if Assigned(xpcomFunc.logCOMPtrRelease) then
    xpcomFunc.logCOMPtrRelease(aCOMPtr, aObject);
end;


function NS_GetXPTCallStub(aIID: TGUID; aOuter: nsIXPTCProxy; out aStub): nsresult; cdecl;
begin
  if Assigned(xpcomFunc.getXPTCallStub) then
    Result := xpcomFunc.getXPTCallStub(aIID, aOuter, aStub)
  else
    Result := NS_ERROR_FAILURE;
end;

procedure NS_DestroyXPTCallStub(aStub: nsISupports); cdecl;
begin
  if Assigned(xpcomFunc.destroyXPTCallStub) then
    xpcomFunc.destroyXPTCallStub(aStub);
end;

function NS_InvokeByIndex(that: nsISupports; methodIndex: PRUint32; paramCount: PRUint32; params: PXPTCVariantArray): nsresult; cdecl;
begin
  if Assigned(xpcomFunc.invokeByIndex) then
    Result := xpcomFunc.invokeByIndex(that, methodIndex, paramCount, params)
  else
    Result := NS_ERROR_FAILURE;
end;

function CheckVersion(toCheck: PAnsiChar;
                      const versions: PGREVersionRangeArray;
                      versionsLength: PRUint32): PRBool; forward;

function GRE_GetPathFromRegKey(
                aRegKey: HKEY;
                versions: PGREVersionRangeArray;
                versionsLength: PRUint32;
                properties: PGREPropertyArray;
                propertiesLength: PRUint32;
                buf: PAnsiChar; buflen: PRUint32): PRBool; forward;

function GRE_GetGREPathWithProperties(
                aVersions: PGREVersionRange;
                versionsLength: PRUint32;
                aProperties: PGREProperty;
                propertiesLength: PRUint32;
                buf: PAnsiChar; buflen: PRUint32): nsresult;
var
  env: array[0..MAX_PATH] of AnsiChar;
  hRegKey: HKEY;
  ok: PRBool;
  versions: PGREVersionRangeArray;
  properties: PGREPropertyArray;
begin
  versions := PGREVersionRangeArray(aVersions);
  properties := PGREPropertyArray(aProperties);
(*
  env := GetEnvironmentVariable('GRE_HOME');
  if Assigned(env) and (env[0] <>#0) then
  begin
    p[0] := #0;
    safe_strncat(p, env, MAX_PATH);
    safe_strncat(p, '\xpcom.dll', MAX_PATH);

  end;
  *)

  if GetEnvironmentVariableA('USE_LOCAL_GRE', env, MAX_PATH)>0 then
  begin
    buf[0] := #0;
    Result := NS_OK;
    Exit;
  end;

  hRegKey := 0;
  if RegOpenKeyEx(HKEY_CURRENT_USER, GRE_WIN_REG_LOC, 0,
                  KEY_READ, hRegKey) = ERROR_SUCCESS then
  begin
    ok := GRE_GetPathFromRegKey(hRegkey,
                                versions, versionsLength,
                                properties, propertiesLength,
                                buf, buflen);
    RegCloseKey(hRegKey);
    if ok then
    begin
      Result := NS_OK;
      Exit;
    end;
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, GRE_WIN_REG_LOC, 0,
                  KEY_READ, hRegKey) = ERROR_SUCCESS then
  begin
    ok := GRE_GetPathFromRegKey(hRegKey,
                                versions, versionsLength,
                                properties, propertiesLength,
                                buf, buflen);
    RegCloseKey(hRegKey);
    if ok then
    begin
      Result := NS_OK;
      Exit;
    end;
  end;

  Result := NS_ERROR_FAILURE;
end;

function CheckVersion(toCheck: PAnsiChar;
                      const versions: PGREVersionRangeArray;
                      versionsLength: PRUint32): PRBool;
var
  i: DWORD;
  c: PRInt32;
begin
  Result := True;
  if versionsLength=0 then
    Exit;
  for i:=0 to versionsLength-1 do
  begin
    c := NS_CompareVersions(toCheck, versions[i].lower);
    if c < 0 then
      Continue;

    if (c=0) and not versions[i].lowerInclusive then
      Continue;

    c := NS_CompareVersions(toCheck, versions[i].upper);
    if c > 0 then
      Continue;

    if (c=0) and not versions[i].upperInclusive then
      Continue;

    Exit;
  end;
  Result := False;
end;

function CopyWithEnvExpansion(
                aDest: PAnsiChar;
                aSource: PAnsiChar;
                aBufLen: PRUint32;
                aType: DWORD): PRBool;
begin
  Result := False;
  case aType of
  REG_SZ:
    begin
      if NS_StrLen(aSource)>=aBufLen then
        Exit;
      NS_StrCopy(aDest, aSource);
      Result := True;
    end;
  REG_EXPAND_SZ:
    begin
      if ExpandEnvironmentStringsA(aSource, aDest, aBufLen) <= aBufLen then
        Result := True;
    end;
  end;
end;

function GRE_GetPathFromRegKey(
                aRegKey: HKEY;
                versions: PGREVersionRangeArray;
                versionsLength: PRUint32;
                properties: PGREPropertyArray;
                propertiesLength: PRUint32;
                buf: PAnsiChar; buflen: PRUint32): PRBool;
const
  XPCOM_DLL = 'xpcom.dll';
var
  i, j: DWORD;
  name: array [0..MAX_PATH] of AnsiChar;
  nameLen: DWORD;
  subKey: HKEY;
  ver: array[0..40] of AnsiChar;
  verLen: DWORD;
  ok: PRBool;
  propbuf: array[0..MAX_PATH] of AnsiChar;
  proplen: DWORD;
  pathtype: DWORD;
begin
  i := 0;
  while True do
  begin
    nameLen := MAX_PATH;
    if RegEnumKeyExA(aRegKey, i, PAnsiChar(@name), nameLen, nil, nil, nil, nil) <>
       ERROR_SUCCESS then
    begin
      break;
    end;

    subKey := 0;
    if RegOpenKeyExA(aRegKey, name, 0, KEY_QUERY_VALUE, subKey) <>
       ERROR_SUCCESS then
    begin
      Continue;
    end;

    ok := False;
    verLen := SizeOf(ver) div SizeOf(ver[0]);
    if (RegQueryValueExA(subKey, 'Version', nil, nil,
                        PByte(@ver), @VerLen)=ERROR_SUCCESS) and
        CheckVersion(ver, versions, versionsLength) then
    begin
      ok := True;
      if propertiesLength>0 then
      begin
        for j:=0 to propertiesLength-1 do
        begin
          proplen := Sizeof(propbuf);
          if (RegQueryValueExA(subKey, properties[i].property_, nil, nil,
              PByte(@propbuf), @proplen)<>ERROR_SUCCESS) or
              (NS_StrComp(propbuf, properties[i].value)<>0) then
          begin
            ok := False;
          end;
        end;
      end;

      proplen := SizeOf(propbuf);
      if ok and
        ((RegQueryValueExA(subKey, 'GreHome', nil, @pathtype,
                          PByte(@propbuf), @proplen)<>ERROR_SUCCESS) or
         (propbuf[0] = #0) or
         not CopyWithEnvExpansion(Buf, propbuf, BufLen, pathtype)) then
      begin
        ok := False;
      end else
      begin
        NS_StrLCat(Buf, '\'+XPCOM_DLL, BufLen);
      end;
    end;
    RegCloseKey(subKey);
    if ok then
    begin
      Result := ok;
      Exit;
    end;
    Inc(i);
  end;
  buf[0] := #0;
  Result := false;
end;

type
  TAnsiCharArray = array [0..High(Word) div SizeOf(AnsiChar)] of AnsiChar;
  PAnsiCharArray = ^TAnsiCharArray;
  TMaxPathChar = array[0..MAX_PATH] of AnsiChar;
  PDependentLib = ^TDependentLib;
  TDependentLib = record
    libHandle: HINST;
    next: PDependentLib;
  end;

const
  XPCOM_DLL = 'xpcom.dll';
  XUL_DLL = 'xul.dll';

var
  sDependentLibs: PDependentLib = nil;
  sXULLibrary: HINST = 0;

type
  TDependentLibsCallback = procedure (aDependentLib: PAnsiChar);

function XPCOMGlueLoad(xpcomFile: PAnsiChar): GetFrozenFunctionsFunc; forward;
procedure XPCOMGlueUnload(); forward;
function fullpath(absPath, relPath: PAnsiChar; maxLength: PRUint32): PAnsiChar; forward;

const
  BUFFEREDFILE_BUFSIZE = 256;
type
  TBufferedFile = record
    f: THandle;
    buf: array [0..BUFFEREDFILE_BUFSIZE-1] of AnsiChar;
    bufPos: Integer;
    bufMax: Integer;
  end;

function BufferedFile_Open(name: PAnsiChar; var ret: TBufferedFile): Boolean;
begin
  Result := False;
  ret.f := CreateFileA(name, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if ret.f <> INVALID_HANDLE_VALUE then
  begin
    Result := True;
  end;
  ret.bufPos := 0;
  ret.bufMax := -1;
end;

procedure BufferedFile_Close(var aFile: TBufferedFile);
begin
  CloseHandle(aFile.f);
  aFile.f := 0;
  aFile.bufPos := 0;
  aFile.bufMax := -1;
end;

procedure BufferedFile_ReadBuffer(var aFile: TBufferedFile);
var
  readSize: DWORD;
begin
  if ReadFile(aFile.f, aFile.buf, BUFFEREDFILE_BUFSIZE, readSize, nil) then
  begin
    aFile.bufPos := 0;
    aFile.bufMax := readSize;
  end;
end;

function BufferedFile_GetString(dest: PAnsiChar; destSize: PRUint32; var buf: TBufferedFile): PRInt32;
var
  i: Cardinal;
  c: AnsiChar;
begin
  i := 0;
  c := #0;
  while (i<destSize-1) do
  begin
    if buf.bufpos>=buf.bufMax then
      BufferedFile_ReadBuffer(buf);
    if buf.bufPos>=buf.bufMax then
      Break;

    c := buf.buf[buf.bufPos];
    dest[i] := c;
    Inc(i);
    Inc(buf.bufPos);
    if (c=#13) or (c=#10) then
    begin
      dest[i] := #10;
      Break;
    end;
  end;
  if (c=#13) and (i<destSize-1) then
  begin
    if buf.bufpos>=buf.bufMax then
      BufferedFile_ReadBuffer(buf);
    if buf.bufPos<buf.bufMax then
    begin
      c := buf.buf[buf.bufPos];
      if c=#10 then
      begin
        Inc(i);
        Inc(buf.bufPos);
      end;
    end;
  end;
  dest[i] := #0;
  Result := i;
end;

procedure XPCOMGlueLoadDependentLibs(xpcomDir: PAnsiChar; cb: TDependentLibsCallback);
var
  buffer: TMaxPathChar;
  buffer2: TMaxPathChar;
  f: TBufferedFile;
  l: Cardinal;
const
  XPCOM_DEPENDENT_LIBS_LIST = 'dependentlibs.list';
begin
  NS_StrCopy(buffer, xpcomDir);
  NS_StrLCat(buffer, '\' + XPCOM_DEPENDENT_LIBS_LIST, MAX_PATH);

  if not BufferedFile_Open(buffer, f) then
  begin
    Exit;
  end;

  try
    while BufferedFile_GetString(buffer, MAX_PATH, f)>0 do
    begin
      l := NS_StrLen(buffer);
      if (buffer[0] = '#') or (l=0) then
        Continue;
      if buffer[l-1] in [#10, #13] then
        buffer[l-1]:= #0;
      NS_StrCopy(buffer2, xpcomdir);
      NS_StrLCat(buffer2, '\', MAX_PATH);
      NS_StrLCat(buffer2, buffer, MAX_PATH);

      cb(buffer2);
    end;
  finally
    BufferedFile_Close(f);
  end;
end;

procedure AppendDependentLib(libHandle: HINST);
var
  d: PDependentLib;
begin
  d := GetMemory(sizeof(TDependentLib));
  if not Assigned(d) then Exit;
  d.next := sDependentLibs;
  d.libHandle := libHandle;

  sDependentLibs := d;
end;

procedure ReadDependentCB(aDependentLib: PAnsiChar);
var
  h: HINST;
begin
  h := LoadLibraryExA(aDependentLib, 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if h <> 0 then
    AppendDependentLib(h);
end;

function XPCOMGlueLoad(xpcomFile: PAnsiChar): GetFrozenFunctionsFunc;
var
  xpcomDir: TMaxPathChar;
  idx: PRInt32;
  h: HINST;
begin
  if (xpcomFile[0]='.') and (xpcomFile[1]=#0) then
  begin
    xpcomFile := XPCOM_DLL;
  end else
  begin
    fullpath(xpcomDir, xpcomFile, sizeof(xpcomDir));
    idx := NS_StrLen(xpcomDir);
    while (idx>=0) and not (xpcomDir[idx] in ['\','/']) do Dec(idx);
    if idx>=0 then
    begin
      xpcomDir[idx] := #0;
      XPCOMGlueLoadDependentLibs(xpcomDir, ReadDependentCB);
      NS_StrLCat(xpcomdir, '\'+xul_dll, sizeof(xpcomdir));
      sXULLibrary := LoadLibraryExA(xpcomdir, 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    end;
  end;

  h := LoadLibraryExA(xpcomFile, 0, LOAD_WITH_ALTERED_SEARCH_PATH);

  AppendDependentLib(h);

  Result := GetFrozenFunctionsFunc(GetProcAddress( h, 'NS_GetFrozenFunctions' ));

  if not Assigned(Result) then
    XPCOMGlueUnload();
end;

procedure XPCOMGlueUnload();
var
  tmp : PDependentLib;
begin
  while Assigned(sDependentLibs) do
  begin
    FreeLibrary(sDependentLibs.libHandle);
    tmp := sDependentLibs;
    sDependentLibs := sDependentLibs.next;
    FreeMemory(tmp);
  end;

  if sXULLibrary<>0 then
  begin
    FreeLibrary(sXULLibrary);
    sXULLibrary := 0;
  end;
end;

function XPCOMGlueStartup(xpcomFile: PAnsiChar): Longword;
const
  XPCOM_DLL = 'xpcom.dll';
  XPCOM_GLUE_VERSION = 1;
var
  func: GetFrozenFunctionsFunc;
begin
  Result := NS_ERROR_FAILURE;

  xpcomFunc.version := XPCOM_GLUE_VERSION;
  xpcomFunc.size := SizeOf(XPCOMFunctions);

  if not Assigned(xpcomFile) then
    xpcomFile := XPCOM_DLL;

  func := XPCOMGlueLoad(xpcomFile);

  if not Assigned(func) then
    Exit;

  Result := func(xpcomFunc, xpcomFile);

  if NS_FAILED(Result) then
    XPCOMGlueUnload();
end;

function XPCOMGlueShutdown: Longword;
begin
  XPCOMGlueUnload();

  FillChar(xpcomFunc, SizeOf(xpcomFunc), Byte(0));

  Result := NS_OK;
end;

function XPCOMGlueLoadXULFunctions(aSymbols: PDynamicFunctionLoad): nsresult;
var
  i: Integer;
  symbols: PDynamicFunctionLoadArray;
type
  PPointer = ^Pointer;
begin
  symbols := PDynamicFunctionLoadArray(aSymbols);

  if sXULLibrary=0 then
  begin
    Result := NS_ERROR_NOT_INITIALIZED;
    Exit;
  end;

  Result := NS_OK;
  i := 0;
  while Assigned(symbols[i].functionName) do
  begin

    PPointer(symbols[i].function_)^ :=
      GetProcAddress(sXULLibrary, symbols[i].functionName);
    if not Assigned(PPointer(symbols[i].function_)^) then
      Result := NS_ERROR_LOSS_OF_SIGNIFICANT_DATA;
    Inc(i);
  end;
end;

type
  TVersionPart = record
    numA: PRInt32;
    strB: PAnsiChar;
    strBLen: PRUint32;
    numC: PRInt32;
    extraD: PAnsiChar;
    extraDlen: PRUint32;
  end;

  TGREVersion = record
    versionStr: PAnsiChar;
    major: TVersionPart;
    minor: TVersionPart;
    revision: TVersionPart;
  end;

function IsNumber(AChar: AnsiChar): Boolean;
begin
  Result := AChar in ['0'..'9'];
end;

function GetGREVersion(verStr: PAnsiChar): TGREVersion;
const
  nullStr: PAnsiChar = '';
  pre = 'pre';
  preLen = Length(pre);
  preStr: PAnsiChar = pre;
var
  idx: Integer;
  workInt: Integer;
  vers: array[0..2] of TVersionPart;
  //verStrPtr: PAnsiCharArray;
  i, j: Integer;
  dot: Integer;
begin
  idx := 0;
  FillChar(vers, sizeof(vers), 0);
  for i:=0 to 2 do
  begin
    dot := idx;
    while (verStr[dot]<>#0) and (verStr[dot]<>'.') do Inc(dot);

    if (dot-idx=1) and (verStr[idx]='*') then
    begin
      vers[i].numA := MaxInt;
      vers[i].strB := nullStr;
    end else
    begin
      workInt := 0;
      while IsNumber(verStr[idx]) do
      begin
        workInt := workInt * 10 + (Ord(verStr[idx]) - Ord('0'));
        Inc(idx);
      end;
      vers[i].numA := workInt;
      vers[i].strB := verStr+idx;
    end;

    if vers[i].strB[0]=#0 then
    begin
      vers[i].strB := nil;
      vers[i].strBlen := 0;
    end else
    if vers[i].strB[0]='+' then
    begin
      Inc(vers[i].numA);
      vers[i].strB := preStr;
      vers[i].strBlen := preLen;
    end else
    begin
      j := idx;
      while (j<dot) and not (verstr[j] in ['0'..'9', '+','-']) do Inc(j);
      vers[i].strBlen := j - idx;
      if (j < dot) then
      begin
        idx := j;
        workInt := 0;
        while IsNumber(verStr[idx]) do
        begin
          workInt := workInt * 10 + (Ord(verStr[idx])-Ord('0'));
          Inc(idx);
        end;
        vers[i].numC := workInt;
        vers[i].extraD := verStr + idx;
        vers[i].extraDlen := dot - idx;
      end;
    end;

    idx := dot;
    if verStr[idx] = #0 then
      Break;
    Inc(idx);
  end;

  Result.versionStr := verStr;
  Result.major := vers[0];
  Result.minor := vers[1];
  Result.revision := vers[2];
end;

function StrNNComp(lhs: PAnsiChar; lhsLen: PRUint32; rhs: PAnsiChar; rhsLen: PRUint32): PRInt32;
begin
  while (lhsLen>0) and (rhsLen>0) do
  begin
    Result := Ord(lhs[0]) - Ord(rhs[0]);
    if Result<>0 then Exit;
    Inc(lhs);
    Dec(lhsLen);
    Inc(rhs);
    Dec(rhsLen);
  end;
  Result := lhsLen - rhsLen;
end;

function GREVersionPartCompare(const lhs, rhs: TVersionPart): Integer;
begin
  Result := lhs.numA - rhs.numA;
  if Result<>0 then Exit;
  Result := strnncomp(lhs.strB, lhs.strBLen, rhs.strB, rhs.strBLen);
  if Result <>0 then Exit;
  Result := lhs.numC - rhs.numC;
  if Result<>0 then Exit;
  Result := strnncomp(lhs.extraD, lhs.extraDLen, rhs.extraD, rhs.extraDLen);
end;

function GREVersionCompare(const lhs, rhs: TGREVersion): Integer;
begin
  Result := GREVersionPartCompare(lhs.major, rhs.major);
  if Result<>0 then Exit;
  Result := GREVersionPartCompare(lhs.minor, rhs.minor);
  if Result<>0 then Exit;
  Result := GREVersionPartCompare(lhs.revision, rhs.revision);
end;

function NS_CompareVersions(lhs, rhs: PAnsiChar): PRInt32;
var
  lhsVer, rhsVer: TGREVersion;
begin
  lhsVer := GetGREVersion(lhs);
  rhsVer := GetGREVersion(rhs);
  Result := GREVersionCompare(lhsVer, rhsVer);
end;

// あるファイルがGREの必要バージョンに達しているかを調べる。
function CheckGeckoVersion(path: PAnsiChar; const reqVer: TGREVersion): Boolean;
const
  BUFSIZE = 4096;
var
  buf: array[0..BUFSIZE-1] of Char;
  fileVer: PAnsiChar;
  dwSize: DWORD;
  dw: DWORD;
  greVer: TGREVersion;
begin
  Result := False;

  dwSize := GetFileVersionInfoSizeA(path, dw);
  if (dwSize<=0) or (dwSize>BUFSIZE) then Exit;

  Result := GetFileVersionInfoA(path, 0, dwSize, @buf);
  if not Result then Exit;

  // バージョン情報の言語IDは決め打ち
  Result := VerQueryValueA(@buf, '\StringFileInfo\000004b0\FileVersion', Pointer(fileVer), dw);
  if not Result then Exit;

  greVer := GetGREVersion(fileVer);

  if GREVersionCompare(greVer, reqVer)>=0 then Result := True;
end;

function IsXpcomDll(const filename: PAnsiChar): Boolean;
var
  module: HMODULE;
  proc: Pointer;
begin
  Result := False;
  module := LoadLibraryA(filename);
  if module=0 then Exit;

  proc := GetProcAddress(module, 'NS_GetFrozenFunctions');
  if Assigned(proc) then Result := True;

  FreeLibrary(module);
end;

function GetLatestGreFromReg(regBase: HKEY; const reqVer: TGREVersion;
  grePath: PAnsiChar; var greVer: TGREVersion): Boolean;
var
  curKey: HKEY;
  dwSize: DWORD;
  i: Integer;
  keyName: TMaxPathChar;
  rv: HRESULT;
  path, dllPath: TMaxPathChar;
  ver: TGREVersion;
begin
  Result := False;
  i := 0;
  dwSize := MAX_PATH;
  rv := RegEnumKeyExA(regBase, i, keyName, dwSize, nil, nil, nil, nil);
  while rv=0 do
  begin
    rv := RegOpenKeyA(regBase, PAnsiChar(@keyName), curKey);
    if rv=0 then
    begin
      dwSize := MAX_PATH;
      rv := RegQueryValueExA(curKey, 'GreHome', nil, nil, PByte(@path), @dwSize);
      if rv=0 then
      begin
        ver := GetGREVersion(keyName);
        if (GREVersionCompare(ver, reqVer)>=0) and
           (GREVersionCompare(ver, greVer)>=0) then
        begin
          dllPath := path;
          NS_StrCat(dllPath, '\xpcom.dll');
          //if IsXpcomDll(dllPath) then
//          if CheckGeckoVersion(dllPath, reqVer) and
//             IsXpcomDll(@dllPath) then
          if CheckGeckoVersion(dllPath, reqVer) then
          begin
            NS_StrCopy(grePath, path);
            greVer := ver;
            Result := True;
          end;
        end;
      end;
      RegCloseKey(curKey);
    end;
    Inc(i);
    dwSize := MAX_PATH;
    rv := RegEnumKeyExA(regBase, i, keyName, dwSize, nil, nil, nil, nil);
  end;
  RegCloseKey(regBase);
end;

function GetLatestGreLocation(buf: PAnsiChar): Boolean;
const
  nameBase: PAnsiChar = 'Software\mozilla.org\GRE';
var
  rv: HRESULT;
  base: HKEY;
  cuPath, lmPath: TMaxPathChar;
  cuVer, lmVer: TGREVersion;
  reqVer: TGREVersion;
  hasLM, hasCU: Boolean;
label
  NoLocalMachine,
  NoCurrentUser;
begin
  FillChar(cuVer, SizeOf(cuVer), 0);
  FillChar(lmVer, SizeOf(lmVer), 0);
  reqVer := GetGREVersion(GRE_BUILD_ID);

  rv := RegOpenKeyA(HKEY_LOCAL_MACHINE, nameBase, base);
  HasLM := (rv=ERROR_SUCCESS) and GetLatestGreFromReg(base, reqVer, lmPath, lmVer);
NoLocalMachine:;
  rv := RegOpenKeyA(HKEY_CURRENT_USER, nameBase, base);
  hasCU := (rv=ERROR_SUCCESS) and GetLatestGreFromReg(base, reqVer, cuPath, cuVer);
NoCurrentUser:;
  Result := hasLM or hasCU;

  if Result then
  begin
    if GREVersionCompare(lmVer,cuVer)>0 then
      //buf := lmPath
      NS_StrCopy(buf, lmPath)
    else
      //buf := cuPath;
      NS_StrCopy(buf, cuPath);
  end;
end;


function GetPathFromConfigDir(dirname: PAnsiChar; buf: PAnsiChar): Boolean;
begin
  //TODO 1: GetPathFromConfigDir の実装
  Result := False;
end;

function GetPathFromConfigFile(const filename: PAnsiChar; buf: PAnsiChar): Boolean;
begin
  //TODO 1: GetPathFromConfigFile の実装
  Result := False;
end;

var
  GRELocation: TMaxPathChar;

function GetGREDirectoryPath(buf: PAnsiChar): Boolean;
const
  GRE_REGISTRY_KEY = GRE_WIN_REG_LOC + GRE_BUILD_ID;
var
  cpd:TMaxPathChar;
  dllPath: TMaxPathChar;
  useLocalGre: TMaxPathChar;
begin
  if NS_StrLen(GreLocation)>0 then
  begin
    NS_StrCopy(buf, GreLocation);
    Result := True;
    Exit;
  end;

  if NS_CurrentProcessDirectory(cpd, MAX_PATH) then
  begin
    dllPath := cpd;
    NS_StrCat(dllPath, '\xpcom.dll');
    if IsXpcomDll(dllPath) then
    begin
      //buf := cpd;
      NS_StrCopy(buf, cpd);
      Result := True;
      Exit;
    end;
  end;

  if GetEnvironmentVariableA('USE_LOCAL_GRE', useLocalGre, MAX_PATH)>0 then
  begin
    Result := False;
    Exit;
  end;

  {if SUCCEEDED(GetEnvironmentVariable('HOME', greConfHomePath, MAX_PATH)) and
     (StrLen(path)>0) then
  begin
    StrCat(greConfHomePath, '\gre.config');
    if GetPathFromConfigFile(greConfHomePath, GRELocation) then
    begin
      buf := GRELocation;
      Result := True;
      Exit;
    end;
  end;}

  {if SUCCEEDED(GetEnvironmentVariable('MOZ_GRE_CONF', path, MAX_PATH)) and
     (StrLen(path)>0) then
  begin
    if GetPathFromConfigFile(path, GRELocation) then
    begin
      buf := GRELocation;
      Result := True;
      Exit;
    end;
  end;}

  // レジストリから探す
  if GetLatestGreLocation(buf) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

type
  nsIDirectoryServiceProvider_stdcall = interface(nsISupports)
  ['{bbf8cab0-d43a-11d3-8cc2-00609792278c}']
    function GetFile(const prop: PAnsiChar; out persistent: PRBool; out AFile: nsIFile): nsresult; stdcall;
  end;

  nsGREDirServiceProvider = class(TInterfacedObject,
                                  nsIDirectoryServiceProvider_stdcall)
  public
    FPathEnvString: TMaxPathChar;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    function GetFile(const prop: PAnsiChar; out persistent: PRBool; out AFile: nsIFile): nsresult; stdcall;
    function GetGreDirectory(out AFile: nsILocalFile): nsresult;
  end;

function nsGREDirServiceProvider.GetGreDirectory(out AFile: nsILocalFile): nsresult;
var
  GreDir: TMaxPathChar;
  tempLocal: nsILocalFile;
  leaf: nsCStringContainer;
begin
  Result := NS_ERROR_FAILURE;
  if not GetGREDirectoryPath(GreDir) then Exit;

  Result := NS_CStringContainerInit(leaf);
  if NS_FAILED(Result) then Exit;
  NS_CStringSetData(@leaf, GreDir);
  Result := NS_NewNativeLocalFile(@leaf, True, tempLocal);

  if NS_SUCCEEDED(Result) then
    Result := tempLocal.QueryInterface(nsILocalFile, AFile);
end;

function nsGREDirServiceProvider.GetFile(const Prop: PAnsiChar; out Persistent: LongBool; out AFile: nsIFile): nsresult;
var
  localFile: nsILocalFile;
const
  NS_GRE_DIR                            = 'GreD';
begin
  persistent := True;

  if NS_StrComp(Prop, NS_GRE_DIR)=0 then
    Result := GetGreDirectory(localFile)
  else
    Result := NS_ERROR_FAILURE;

  if NS_SUCCEEDED(Result) then
    Result := localFile.QueryInterface(nsIFile, AFile);
end;

class function nsGREDirServiceProvider.NewInstance: TObject;
var
  Instance: Pointer;
begin
  Instance := nsMemory.Alloc(InstanceSize);
  Result := InitInstance(Instance);
  nsGREDirServiceProvider(Instance).FRefCount := 1;
end;

procedure nsGREDirServiceProvider.FreeInstance;
begin
  nsMemory.Free(Self);
end;

function GetXPCOMPath(buf: PAnsiChar): Boolean;
var
  grePath: TMaxPathChar;
  greEnv: TMaxPathChar;
begin
  Result := False;
  //if not GetGreDirectoryPath(grePath) then
  if NS_FAILED(GRE_GetGREPathWithProperties(nil, 0, nil, 0, grePath, MAX_PATH)) then
  begin
    if GetEnvironmentVariableA('MOZILLA_FIVE_HOME', greEnv, MAX_PATH)=0 then
      Exit;
    NS_StrCopy(buf, greEnv);
    NS_StrLCat(buf, '\xpcom.dll', MAX_PATH);
  end else
  begin
    NS_StrCopy(buf, grePath);
  end;

  Result := True;
end;

var
  sStartupCount: Integer = 0;

function GRE_Startup: Longword;
var
  xpcomLocation: TMaxPathChar;
  provider: nsGREDirServiceProvider;
  servMgr: nsIServiceManager;
begin
  Result := NS_OK;

  if sStartupCount > 0 then
  begin
    Inc(sStartupCount);
    Exit;
  end;

  GetXPCOMPath(xpcomLocation);

  Result := XPCOMGlueStartup(xpcomLocation);

  if NS_FAILED(Result) then Exit;

  provider := nsGREDirServiceProvider.Create;
  if not Assigned(provider) then
  begin
    Result := NS_ERROR_OUT_OF_MEMORY;
    XPCOMGlueShutdown;
    Exit;
  end;

  provider._AddRef;
  Result := NS_InitXPCOM2(servMgr, nil, provider as nsXPCOM.nsIDirectoryServiceProvider);
  provider._Release;

  if NS_FAILED(Result) then
  begin
    XPCOMGlueShutdown;
    Exit;
  end;
  Inc(sStartupCount);
end;

function GRE_Shutdown: Longword;
begin
  Dec(sStartupCount);

  if sStartupCount=0 then
  begin
    NS_ShutdownXPCOM(nil);
    //XPCOMGlueShutdown;
  end else
  if sStartupCount<0 then sStartupCount := 0;

  Result := NS_OK;
end;

function NS_CompareVersion(A, B: PAnsiChar): PRInt32;
var
  vA, vB: TGREVersion;
begin
  va := GetGREVersion(A);
  vB := GetGREVersion(b);

  Result := GREVersionCompare(vA, vB);
end;

{ PChar routines }

function NS_StrLen(const Str: PAnsiChar): Cardinal;
var
  P: PAnsiChar;
begin
  P := Str;
  Result := 0;
  while P^ <> #0 do
  begin
    Inc(P);
    Inc(Result);
  end;
end;

function NS_StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
var
  D, S: PAnsiChar;
begin
  D := Dest;
  S := Source;
  repeat
    D^ := S^;
    Inc(D);
    Inc(S);
  until D[-1] = #0;
  Result := Dest;
end;

function NS_StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; maxLen: Cardinal): PAnsiChar;
var
  D, S, last: PAnsiChar;
begin
  D := Dest;
  S := Source;
  last := Dest + maxLen;
  while (S^<>#0) and (D < last) do
  begin
    D^ := S^;
    Inc(D);
    Inc(S);
  end;
  D^ := #0;
  Result := Dest;
end;

function NS_StrCat(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
var
  D: PAnsiChar;
begin
  D := Dest;
  while D^ <> #0 do
    Inc(D);
  NS_StrCopy(D, Source);
  Result := Dest;
end;

function NS_StrLCat(Dest: PAnsiChar; const Source: PAnsiChar; maxLen: Cardinal): PAnsiChar;
var
  D, S: PAnsiChar;
  last: PAnsiChar;
begin
  D := Dest + NS_StrLen(Dest);
  S := Source;
  last := Dest + maxLen - 1;
  while (S^ <> #0) and (D < last) do
  begin
    D ^ := S^;
    Inc(D);
    Inc(S);
  end;
  D^ := #0;
  Result := Dest;
end;

function NS_StrComp(const Str1, Str2: PAnsiChar): Integer;
var
  P1, P2: PAnsiChar;
begin
  P1 := Str1;
  P2 := Str2;
  while (P1^<>#0) and (P1^=P2^) do
  begin
    Inc(P1);
    Inc(P2);
  end;
  Result := Ord(P1^) - Ord(P2^);
end;

function NS_StrRScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
var
  P: PAnsiChar;
begin
  P := Str;
  while P^<>#0 do
    Inc(P);
  while (P>=Str) and (P^<>Chr) do
    Dec(P);
  if (P>=Str) then
    Result := P
  else
    Result := nil;
end;

function NS_StrLen(const Str: PWideChar): Cardinal;
var
  P: PWideChar;
begin
  P := Str;
  Result := 0;
  while P^ <> #0 do
  begin
    Inc(P);
    Inc(Result);
  end;
end;

function NS_StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  D, S: PWideChar;
begin
  D := Dest;
  S := Source;
  repeat
    D^ := S^;
    Inc(D);
    Inc(S);
  until D[-1] = #0;
  Result := Dest;
end;

function NS_StrLCopy(Dest: PWideChar; const Source: PWideChar; maxLen: Cardinal): PWideChar;
var
  D, S: PWideChar;
begin
  D := Dest;
  S := Source;
  while (S^<>#0) and (Dest+maxLen<D) do
  begin
    D^ := S^;
    Inc(D);
    Inc(S);
  end;
  D^ := #0;
  Result := Dest;
end;

function NS_StrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  D: PWideChar;
begin
  D := Dest;
  while D^ <> #0 do
    Inc(D);
  NS_StrCopy(D, Source);
  Result := Dest;
end;

function NS_StrLCat(Dest: PWideChar; const Source: PWideChar; maxLen: Cardinal): PWideChar;
var
  D, S: PWideChar;
  last: PWideChar;
begin
  D := Dest + NS_StrLen(Dest);
  S := Source;
  last := Dest + maxLen - 1;
  while (S^ <> #0) and (D < last) do
  begin
    D ^ := S^;
    Inc(D);
    Inc(S);
  end;
  D^ := #0;
  Result := Dest;
end;

function NS_StrComp(const Str1, Str2: PWideChar): Integer;
var
  P1, P2: PWideChar;
begin
  P1 := Str1;
  P2 := Str2;
  while (P1^<>#0) and (P1^=P2^) do
  begin
    Inc(P1);
    Inc(P2);
  end;
  Result := Ord(P1^) - Ord(P2^);
end;

function NS_StrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
var
  P: PWideChar;
begin
  P := Str;
  while P^<>#0 do
    Inc(P);
  while (P>=Str) and (P^<>Chr) do
    Dec(P);
  if (P>=Str) then
    Result := P
  else
    Result := nil;
end;

function safe_strncat(Dest: PAnsiChar; Append: PAnsiChar; count: PRUint32): PRBool;
var
  last: PAnsiChar;
begin
  last := (dest+count-1);
  while dest^ <> #0 do
    Inc(dest);
  while (append^ <> #0) and (Last - Dest > 0) do
  begin
    Dest^ := Append^;
    Inc(dest);
    Inc(Append);
  end;
  Dest^ := #0;
  Result := (Append^ = #0);
end;

function fullpath(absPath, relPath: PAnsiChar; maxLength: PRUint32): PAnsiChar;
var
  filePart: PAnsiChar;
begin
  GetFullPathNameA(relPath, maxLength, absPath, filePart);
  Result := absPath;
end;

function NS_CurrentProcessDirectory(buf: PAnsiChar; bufLen: Cardinal): Boolean;
var
  lastSlash: PAnsiChar;
begin
  Result := False;
  if SUCCEEDED(GetModuleFileNameA(0, buf, bufLen)) then
  begin
    lastSlash := NS_StrRScan(buf, '\');
    if Assigned(lastSlash) then
    begin
      lastSlash^ := #0;
      Result := True;
    end;
  end;
end;


end.
