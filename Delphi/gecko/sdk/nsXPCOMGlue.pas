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
unit nsXPCOMGlue;

interface

uses
  nsXPCOM, nsGeckoStrings, nsConsts, nsTypes, SysUtils;

const
(*
 * Original code: mozilla/xpcom/components/nsIServiceManager.idl
 *)
  NS_XPCOM_STARTUP_OBSERVER_ID = 'xpcom-startup';
  NS_XPCOM_SHUTDOWN_OBSERVER_ID = 'xpcom-shutdown';
  NS_XPCOM_AUTOREGISTRATION_OBSERVER_ID = 'xpcom-autoregistration';

(*
 * Original code: mozilla/xpcom/glue/nsMemory.h
 *)
  NS_MEMORY_CONTRACTID      = '@mozilla.org/xpcom/memory-service;1';
  NS_MEMORY_CLASSNAME       = 'Global Memory Service';
  NS_MEMORY_CID: TGUID      = '{30a04e40-38e7-11d4-8cf5-0060b0fc14a3}';

(*
 * Original code: mozilla/xpcom/io/nsDirectoryServiceDefs.h
 * Rev: 1.27
 *)
(**
 * Defines the property names for directories available from
 * nsIDirectoryService. These dirs are always available even if no
 * nsIDirectoryServiceProviders have been registered with the service.
 * Application level keys are defined in nsAppDirectoryServiceDefs.h.
 *
 * Keys whose definition ends in "DIR" or "FILE" return a single nsIFile (or
 * subclass). Keys whose definition ends in "LIST" return an nsISimpleEnumerator
 * which enumerates a list of file objects.
 *
 * Defines listed in this file are FROZEN.  This list may grow.
 *)
  NS_OS_HOME_DIR                   = 'Home';
  NS_OS_TEMP_DIR                   = 'TmpD';
  NS_OS_CURRENT_WORKING_DIR        = 'CurWorkD';
  NS_OS_DESKTOP_DIR                = 'Desk';
  NS_OS_CURRENT_PROCESS_DIR        = 'CurProcD';
  NS_XPCOM_CURRENT_PROCESS_DIR     = 'XCurProcD';
  NS_XPCOM_COMPONENT_DIR           = 'ComsD';
  NS_XPCOM_COMPONENT_DIR_LIST      = 'ComsDL';
  NS_XPCOM_COMPONENT_REGISTRY_FILE = 'ComRegF';
  NS_XPCOM_XPTI_REGISTRY_FILE      = 'XptiRegF';
  NS_XPCOM_LIBRARY_FILE            = 'XpcomLib';
  NS_GRE_DIR                       = 'GreD';
  NS_GRE_COMPONENT_DIR             = 'GreComsD';
  NS_WIN_WINDOWS_DIR               = 'WinD';
  NS_WIN_HOME_DIR                  = NS_OS_HOME_DIR;
  NS_WIN_DESKTOP_DIR               = 'DeskV';// virtual folder at the root of the namespace
  NS_WIN_PROGRAMS_DIR              = 'Progs';
  NS_WIN_CONTROLS_DIR              = 'Cntls';
  NS_WIN_PRINTERS_DIR              = 'Prnts';
  NS_WIN_PERSONAL_DIR              = 'Pers';
  NS_WIN_FAVORITES_DIR             = 'Favs';
  NS_WIN_STARTUP_DIR               = 'Strt';
  NS_WIN_RECENT_DIR                = 'Rcnt';
  NS_WIN_SEND_TO_DIR               = 'SndTo';
  NS_WIN_BITBUCKET_DIR             = 'Buckt';
  NS_WIN_STARTMENU_DIR             = 'Strt';
  NS_WIN_DESKTOP_DIRECTORY         = 'DeskP';// file sys dir which physically stores objects on desktop
  NS_WIN_DRIVES_DIR                = 'Drivs';
  NS_WIN_NETWORK_DIR               = 'NetW';
  NS_WIN_NETHOOD_DIR               = 'netH';
  NS_WIN_FONTS_DIR                 = 'Fnts';
  NS_WIN_TEMPLATES_DIR             = 'Tmpls';
  NS_WIN_COMMON_STARTMENU_DIR      = 'CmStrt';
  NS_WIN_COMMON_PROGRAMS_DIR       = 'CmPrgs';
  NS_WIN_COMMON_STARTUP_DIR        = 'CmStrt';
  NS_WIN_COMMON_DESKTOP_DIRECTORY  = 'CmDeskP';
  NS_WIN_APPDATA_DIR               = 'AppData';
  NS_WIN_PRINTHOOD                 = 'PrntHd';
  NS_WIN_COOKIES_DIR               = 'CookD';
  (* Deprecated *)
  NS_OS_DRIVE_DIR                  = 'DrvD';

(*
 * Original Code: mozilla/xpcom/io/nsAppDirectoryServiceDefs.h
 * Revision: 1.16
 *)
  NS_APP_APPLICATION_REGISTRY_FILE     = 'AppRegF';
  NS_APP_APPLICATION_REGISTRY_DIR      = 'AppRegD';
  NS_APP_DEFAULTS_50_DIR               = 'DefRt';         // The root dir of all defaults dirs
  NS_APP_PREF_DEFAULTS_50_DIR          = 'PrfDef';
  NS_APP_PROFILE_DEFAULTS_50_DIR       = 'profDef';       // The profile defaults of the "current"
                                                         // locale. Should be first choice.
  NS_APP_PROFILE_DEFAULTS_NLOC_50_DIR  = 'ProfDefNoLoc';  // The profile defaults of the "default"
                                                         // installed locale. Second choice
                                                         // when above is not available.
  NS_APP_USER_PROFILES_ROOT_DIR        = 'DefProfRt';     // The dir where user profile dirs get created.
  NS_APP_RES_DIR                       = 'ARes';
  NS_APP_CHROME_DIR                    = 'AChrom';
  NS_APP_PLUGINS_DIR                   = 'APlugns';       // Deprecated - use NS_APP_PLUGINS_DIR_LIST
  NS_APP_SEARCH_DIR                    = 'SrchPlugns';
  NS_APP_CHROME_DIR_LIST               = 'AChromeDL';
  NS_APP_PLUGINS_DIR_LIST              = 'APluginsDL';
  NS_SHARED                            = 'SHARED';
  NS_APP_PREFS_50_DIR                  = 'PrefD';         // Directory which contains user prefs
  NS_APP_PREFS_50_FILE                 = 'PrefF';
  NS_APP_PREFS_DEFAULTS_DIR_LIST       = 'PrefDL';
  NS_APP_USER_PROFILE_50_DIR           = 'ProfD';
  NS_APP_USER_CHROME_DIR               = 'UChrm';
  NS_APP_LOCALSTORE_50_FILE            = 'LclSt';
  NS_APP_HISTORY_50_FILE               = 'UHist';
  NS_APP_USER_PANELS_50_FILE           = 'UPnls';
  NS_APP_USER_MIMETYPES_50_FILE        = 'UMimTyp';
  NS_APP_CACHE_PARENT_DIR              = 'cachePDir';
  NS_APP_BOOKMARKS_50_FILE             = 'BMarks';
  NS_APP_DOWNLOADS_50_FILE             = 'DLoads';
  NS_APP_SEARCH_50_FILE                = 'SrchF';
  NS_APP_MAIL_50_DIR                   = 'MailD';
  NS_APP_IMAP_MAIL_50_DIR              = 'IMapMD';
  NS_APP_NEWS_50_DIR                   = 'NewsD';
  NS_APP_MESSENGER_FOLDER_CACHE_50_DIR = 'MFCaD';
  NS_APP_INSTALL_CLEANUP_DIR           = 'XPIClnupD';  //location of xpicleanup.dat xpicleanup.exe
  NS_APP_STORAGE_50_FILE               = 'UStor';

procedure NS_CreateInstance(const CID, IID: TGUID; out Intf); overload;
procedure NS_CreateInstance(ContractID: PAnsiChar; const IID: TGUID; out Intf); overload;
function NS_GetWeakReference(Instance: nsISupports): nsIWeakReference;
procedure NS_GetInterface(Source: nsISupports; const IID: TGUID; out Instance);
procedure NS_GetService(const CID, IID: TGUID; out Intf); overload;
procedure NS_GetService(ContractID: PAnsiChar; const IID: TGUID; out Intf); overload;
function NS_GetSpecialDirectory(const specialDirName: PAnsiChar): nsIFile;

const
  NS_PREFLOCALIZEDSTRING_CID: TGUID = '{064d9cee-1dd2-11b2-83e3-d25ab0193c26}';
  NS_PREFLOCALIZEDSTRING_CONTRACTID = '@mozilla.org/pref-localizedstring;1';
  NS_PREFLOCALIZEDSTRING_CLASSNAME  = 'Pref LocalizedString';

  NS_PREFSERVICE_CID: TGUID     = '{1cd91b88-1dd2-11b2-92e1-ed22ed298000}';
  NS_PREFSERVICE_CONTRACTID     = '@mozilla.org/preferences-service;1';
  NS_PREFSERVICE_CLASSNAME      = 'Preferences Server';
  NS_PREFSERVICE_RESET_TOPIC_ID = 'prefservice:before-reset';

  STATUS_UNKNOWN=0;
  STATUS_ACCEPTED=1;
  STATUS_DOWNGRADED=2;
  STATUS_FLAGGED=3;
  STATUS_REJECTED=4;
  POLICY_UNKNOWN=0;
  POLICY_NONE=1;
  POLICY_NO_CONSENT=2;
  POLICY_IMPLICIT_CONSENT=3;
  POLICY_EXPLICIT_CONSENT=4;
  POLICY_NO_II=5;

  NS_COOKIEPROMPTSERVICE_CONTRACTID  = '@mozilla.org/embedcomp/cookieprompt-service;1';

  NS_WINDOWWATCHER_CONTRACTID = '@mozilla.org/embedcomp/window-watcher;1';

  NS_WEBBROWSER_CID: TGUID = '{f1eac761-87e9-11d3-af80-00a024ffc08c}';
  NS_WEBBROWSER_CONTRACTID = '@mozilla.org/embedding/browser/nsWebBrowser;1';

  STATUS_SCRIPT         = $00000001;
  STATUS_SCRIPT_DEFAULT = $00000002;
  STATUS_LINK           = $00000003;
  CHROME_DEFAULT          = $00000001;
  CHROME_WINDOW_BORDERS   = $00000002;
  CHROME_WINDOW_CLOSE     = $00000004;
  CHROME_WINDOW_RESIZE    = $00000008;
  CHROME_MENUBAR          = $00000010;
  CHROME_TOOLBAR          = $00000020;
  CHROME_LOCATIONBAR      = $00000040;
  CHROME_STATUSBAR        = $00000080;
  CHROME_PERSONAL_TOOLBAR = $00000100;
  CHROME_SCROLLBARS       = $00000200;
  CHROME_TITLEBAR         = $00000400;
  CHROME_EXTRA            = $00000800;
  CHROME_WITH_SIZE        = $00001000;
  CHROME_WITH_POSITION    = $00002000;
  CHROME_WINDOW_MIN       = $00004000;
  CHROME_WINDOW_POPUP     = $00008000;
  CHROME_WINDOW_RAISED    = $02000000;
  CHROME_WINDOW_LOWERED   = $04000000;
  CHROME_CENTER_SCREEN    = $08000000;
  CHROME_DEPENDENT        = $10000000;
  CHROME_MODAL            = $20000000;
  CHROME_OPENAS_DIALOG    = $40000000;
  CHROME_OPENAS_CHROME    = $80000000;
  CHROME_ALL              = $00000ffe;

  SETUP_ALLOW_PLUGINS         = 1;
  SETUP_ALLOW_JAVASCRIPT      = 2;
  SETUP_ALLOW_META_REDIRECTS  = 3;
  SETUP_ALLOW_SUBFRAMES       = 4;
  SETUP_ALLOW_IMAGES          = 5;
  SETUP_FOCUS_DOC_BEFORE_CONTENT = 6;
  SETUP_USE_GLOBAL_HISTORY    = 256;
  SETUP_IS_CHROME_WRAPPER     = 7;

  PRINTPREVIEW_GOTO_PAGENUM = 0;
  PRINTPREVIEW_PREV_PAGE    = 1;
  PRINTPREVIEW_NEXT_PAGE    = 2;
  PRINTPREVIEW_HOME         = 3;
  PRINTPREVIEW_END          = 4;

  DIM_FLAGS_POSITION   = 1;
  DIM_FLAGS_SIZE_INNER = 2;
  DIM_FLAGS_SIZE_OUTER = 4;

  CONTEXT_NONE     = 0;
  CONTEXT_LINK     = 1;
  CONTEXT_IMAGE    = 2;
  CONTEXT_DOCUMENT = 4;
  CONTEXT_TEXT     = 8;
  CONTEXT_INPUT    = 16;
  CONTEXT_BACKGROUND_IMAGE = 32;

  STATE_START          = $00000001;
  STATE_REDIRECTING    = $00000002;
  STATE_TRANSFERRING   = $00000004;
  STATE_NEGOTIATING    = $00000008;
  STATE_STOP           = $00000010;
  STATE_IS_REQUEST     = $00010000;
  STATE_IS_DOCUMENT    = $00020000;
  STATE_IS_NETWORK     = $00040000;
  STATE_IS_WINDOW      = $00080000;
  STATE_IS_INSECURE     = $00000004;
  STATE_IS_BROKEN       = $00000001;
  STATE_IS_SECURE       = $00000002;
  STATE_SECURE_HIGH     = $00040000;
  STATE_SECURE_MED      = $00010000;
  STATE_SECURE_LOW      = $00020000;

  NOTIFY_STATE_REQUEST  = $00000001;
  NOTIFY_STATE_DOCUMENT = $00000002;
  NOTIFY_STATE_NETWORK  = $00000004;
  NOTIFY_STATE_WINDOW   = $00000008;
  NOTIFY_STATE_ALL      = $0000000f;
  NOTIFY_PROGRESS       = $00000010;
  NOTIFY_STATUS         = $00000020;
  NOTIFY_SECURITY       = $00000040;
  NOTIFY_LOCATION       = $00000080;
  NOTIFY_ALL            = $000000ff;

type
  TWeakReference = class;
  TSupportsWeakReference = class;

  TWeakReference = class(TInterfacedObject, nsIWeakReference)
  private
    FSupports: TSupportsWeakReference;
    constructor Create(supports: TSupportsWeakReference);
  public
    destructor Destroy; override;
    procedure QueryReferent(const uuid: TGUID; out Intf); safecall;
  end;

  TSupportsWeakReference = class(TInterfacedObject, nsISupportsWeakReference)
  private
    FProxy: TWeakReference;
  public
    destructor Destroy; override;
    function GetWeakReference: nsIWeakReference; safecall;
  end;

  EGeckoError = class(Exception);

function NS_NewSupportsWeakReferenceDelegate(aTarget: nsISupports): nsISupportsWeakReference;

resourcestring
  SGetComponentManagerError = 'Cannot get the Component Manager.';
  SGetServiceManagerError = 'Cannot get the Service Manager.';
  SCreateInstanceError = 'Cannot get the instance of CID ''%s.'' ';
  SGetServiceError = 'Cannot get the service of CID ''%s.'' ';
  SNoSuchSpecialDir = 'Cannot get the Special Directory ''%s.'' ';
  SNoSuchInterface = 'Cannot get the Interface ''%s.'' ';


implementation

uses
  nsMemory, nsError, nsInit, Windows, StrUtils;

var
  sCompMgr: nsIComponentManager = nil;
  sSrvMgr: nsIServiceManager = nil;

procedure NS_CreateInstance(const CID, IID: TGUID; out Intf);
var
  rv: nsresult;
begin
  rv := NS_OK;
  if not Assigned(sCompMgr) then
    rv := NS_GetComponentManager(sCompMgr);
  if NS_FAILED(rv) or not Assigned(sCompMgr) then
    raise EGeckoError.CreateRes(PResStringRec(@SGetComponentManagerError));

  try
    sCompMgr.CreateInstance(CID, nil, IID, Intf);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SCreateInstanceError), [GUIDToString(CID)]);
  end;
end;

procedure NS_CreateInstance(ContractID: PAnsiChar; const IID: TGUID; out Intf);
var
  rv: nsresult;
begin
  rv := NS_OK;
  if not Assigned(sCompMgr) then
    rv := NS_GetComponentManager(sCompMgr);
  if NS_FAILED(rv) or not Assigned(sCompMgr) then
    raise EGeckoError.CreateRes(PResStringRec(@SGetComponentManagerError));

  try
    sCompMgr.CreateInstanceByContractID(ContractID, nil, IID, Intf);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SCreateInstanceError), [String(ContractID)]);
  end;
end;

procedure NS_GetService(const CID, IID: TGUID; out Intf);
var
  rv: nsresult;
begin
  rv := NS_OK;
  if not Assigned(sSrvMgr) then
    rv := NS_GetServiceManager(sSrvMgr);
  if NS_FAILED(rv) or not Assigned(sSrvMgr) then
    raise EGeckoError.CreateRes(PResStringRec(@SGetServiceManagerError));

  try
    sSrvMgr.GetService(CID, IID, Intf);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SGetServiceError), [GUIDToString(CID)]);
  end;
end;

procedure NS_GetService(ContractID: PAnsiChar; const IID: TGUID; out Intf);
var
  rv: nsresult;
begin
  rv := NS_OK;
  if not Assigned(sSrvMgr) then
    rv := NS_GetServiceManager(sSrvMgr);
  if NS_FAILED(rv)  or not Assigned(sSrvMgr) then
    raise EGeckoError.CreateRes(PResStringRec(@SGetServiceManagerError));

  try
    sSrvMgr.GetServiceByContractID(ContractID, IID, Intf);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SGetServiceError), [ContractID]);
  end;
end;

function NS_GetSpecialDirectory(const specialDirName: PAnsiChar): nsIFile;
var
  serv: nsIProperties;
const
  NS_DIRECTORY_SERVICE_CID: TGUID = '{f00152d0-b40b-11d3-8c9c-000064657374}';
begin
  NS_GetService(NS_DIRECTORY_SERVICE_CID, nsIProperties, serv);

  try
    serv.Get(specialDirName, nsIFile, Result);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SNoSuchSpecialDir), [specialDirName]);
  end;
end;

constructor TWeakReference.Create(supports: TSupportsWeakReference);
begin
  inherited Create;
  FSupports := supports;
end;

destructor TWeakReference.Destroy;
begin
  if Assigned(FSupports) then
    FSupports.FProxy := nil;
  inherited;
end;

procedure TWeakReference.QueryReferent(const uuid: TGUID; out Intf);
var
  rv: nsresult;
begin
  rv := FSupports.QueryInterface(uuid, Intf);
  if NS_FAILED(rv) then
    //raise EGeckoError.Create('QueryReference Error');
    System.Error(reIntfCastError);
end;

destructor TSupportsWeakReference.Destroy;
begin
  if Assigned(FProxy) then
    FProxy.FSupports := nil;
  inherited;
end;

function TSupportsWeakReference.GetWeakReference: nsIWeakReference;
begin
  if not Assigned(FProxy) then
    FProxy := TWeakReference.Create(Self);

  if NS_FAILED(FProxy.QueryInterface(nsIWeakReference, Result)) then
    System.Error(reIntfCastError);
end;

function NS_GetWeakReference(Instance: nsISupports): nsIWeakReference;
var
  factory: nsISupportsWeakReference;
begin
  if Assigned(Instance) then
  try
    factory := Instance as nsISupportsWeakReference;
    Result := factory.GetWeakReference();
  except
    Result := nil;
  end;
end;

procedure NS_GetInterface(Source: nsISupports; const IID: TGUID; out Instance);
var
  factory: nsIInterfaceRequestor;
begin
  if Assigned(Source) then
  try
    factory := Source as nsIInterfaceRequestor;
    factory.GetInterface(IID, Instance);
  except
    raise EGeckoError.CreateResFmt(PResStringRec(@SNoSuchInterface), [GUIDToString(IID)]);
  end else
    System.Error(reInvalidPtr);
end;

type
  TSupportsWeakReferenceInternal = class;
  TWeakReferenceInternal = class;

  TSupportsWeakReferenceInternal = class(TInterfacedObject,
                                      nsISupportsWeakReference)
    FTarget: nsISupports;
    FProxy: TWeakReferenceInternal;
    constructor Create(ATarget: nsISupports);
    destructor Destroy; override;
    function getWeakReference: nsIWeakReference; safecall;
  end;

  TWeakReferenceInternal = class(TInterfacedObject,
                              nsIWeakReference)
    FReferent: TSupportsWeakReferenceInternal;
    constructor Create(aReferent: TSupportsWeakReferenceInternal);
    destructor Destroy; override;
    procedure QueryReferent(const iid: TGUID; out Intf); safecall;
  end;

function NS_NewSupportsWeakReferenceDelegate(aTarget: nsISupports): nsISupportsWeakReference;
begin
  Result := TSupportsWeakReferenceInternal.Create(aTarget);
end;

constructor TSupportsWeakReferenceInternal.Create(ATarget: nsISupports);
begin
  inherited Create;
  FTarget := ATarget;
  // FProxy := nil; // non-initialized value set to zero
end;

destructor TSupportsWeakReferenceInternal.Destroy;
begin
  if Assigned(FProxy) then
    FProxy.FReferent := nil;
  inherited;
end;

function TSupportsWeakReferenceInternal.getWeakReference: nsIWeakReference;
begin
  if not Assigned(FProxy) then
  begin
    FProxy := TWeakReferenceInternal.Create(Self);
  end;

  Result := FProxy;
end;

constructor TWeakReferenceInternal.Create(aReferent: TSupportsWeakReferenceInternal);
begin
  inherited Create;

  FReferent := aReferent;
end;

destructor TWeakReferenceInternal.Destroy;
begin
  if Assigned(FReferent) then
    FReferent.FProxy := nil;

  inherited;
end;

procedure TWeakReferenceInternal.QueryReferent(const iid: TGUID; out intf);
begin
  if not Supports(FReferent.FTarget, iid, intf) then
    System.Error(reIntfCastError);
end;

end.
