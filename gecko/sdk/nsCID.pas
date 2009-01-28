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
 * Netscape Communications Corporation.
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
unit nsCID;

interface

const
(*
 * Original code: xpcom/build/nsXPCOMCID.h.
 *)

(**
 * XPCOM Directory Service Contract ID
 *   The directory service provides ways to obtain file system locations. The
 *   directory service is a singleton.
 *
 *   This contract supports the nsIDirectoryService and the nsIProperties
 *   interfaces.
 *
 *)
  NS_DIRECTORY_SERVICE_CONTRACTID = '@mozilla.org/file/directory_service;1';

(**
 * XPCOM File
 *   The file abstraction provides ways to obtain and access files and
 *   directories located on the local system.
 *
 *   This contract supports the nsIFile interface and the nsILocalFile interface.
 *   This contract may also support platform specific interfaces such as
 *   nsILocalFileMac on platforms where additional interfaces are required.
 *
 *)
  NS_LOCAL_FILE_CONTRACTID = '@mozilla.org/file/local;1';

(**
 * XPCOM Category Manager Contract ID
 *   The contract supports the nsICategoryManager interface. The
 *   category manager is a singleton.
 *)
  NS_CATEGORYMANAGER_CONTRACTID = '@mozilla.org/categorymanager;1';

(**
 * XPCOM Properties Object Contract ID
 *   Simple mapping object which supports the nsIProperties interface.
 *)
  NS_PROPERTIES_CONTRACTID = '@mozilla.org/properties;1';

(**
 * XPCOM Array Object ContractID
 * Simple array implementation which supports the nsIArray and
 * nsIMutableArray interfaces.
 *)
  NS_ARRAY_CONTRACTID = '@mozilla.org/array;1';

(**
 * The following are the CIDs and Contract IDs of the nsISupports wrappers for
 * primative types.
 *)
  NS_SUPPORTS_ID_CID: TGUID = '{acf8dc40-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_ID_CONTRACTID = '@mozilla.org/supports-id;1';

  NS_SUPPORTS_CSTRING_CID: TGUID = '{acf8dc41-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_CSTRING_CONTRACTID = '@mozilla.org/supports-cstring;1';

  NS_SUPPORTS_STRING_CID: TGUID = '{acf8dc42-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_STRING_CONTRACTID = '@mozilla.org/supports-string;1';

  NS_SUPPORTS_PRBOOL_CID: TGUID = '{acf8dc43-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRBOOL_CONTRACTID = '@mozilla.org/supports-PRBool;1';

  NS_SUPPORTS_PRUINT8_CID: TGUID = '{acf8dc44-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRUINT8_CONTRACTID = '@mozilla.org/supports-PRUint8;1';

  NS_SUPPORTS_PRUINT16_CID: TGUID = '{acf8dc46-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRUINT16_CONTRACTID = '@mozilla.org/supports-PRUint16;1';

  NS_SUPPORTS_PRUINT32_CID: TGUID = '{acf8dc47-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRUINT32_CONTRACTID = '@mozilla.org/supports-PRUint32;1';

  NS_SUPPORTS_PRUINT64_CID: TGUID = '{acf8dc48-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRUINT64_CONTRACTID = '@mozilla.org/supports-PRUint64;1';

  NS_SUPPORTS_PRTIME_CID: TGUID = '{acf8dc49-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRTIME_CONTRACTID = '@mozilla.org/supports-PRTime;1';

  NS_SUPPORTS_CHAR_CID: TGUID = '{acf8dc4a-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_CHAR_CONTRACTID = '@mozilla.org/supports-char;1';

  NS_SUPPORTS_PRINT16_CID: TGUID = '{acf8dc4b-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRINT16_CONTRACTID = '@mozilla.org/supports-PRInt16;1';

  NS_SUPPORTS_PRINT32_CID: TGUID = '{acf8dc4c-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRINT32_CONTRACTID = '@mozilla.org/supports-PRInt32;1';

  NS_SUPPORTS_PRINT64_CID: TGUID = '{acf8dc4d-4a25-11d3-9890-006008962422}';
  NS_SUPPORTS_PRINT64_CONTRACTID = '@mozilla.org/supports-PRInt64;1';

  NS_SUPPORTS_FLOAT_CID: TGUID = '{cbf86870-4ac0-11d3-baea-00805f8a5dd7}';
  NS_SUPPORTS_FLOAT_CONTRACTID = '@mozilla.org/supports-float;1';

  NS_SUPPORTS_DOUBLE_CID: TGUID = '{cbf86871-4ac0-11d3-baea-00805f8a5dd7}';
  NS_SUPPORTS_DOUBLE_CONTRACTID = '@mozilla.org/supports-double;1';

  NS_SUPPORTS_VOID_CID: TGUID = '{af10f3e0-568d-11d3-baf8-00805f8a5dd7}';
  NS_SUPPORTS_VOID_CONTRACTID = '@mozilla.org/supports-void;1';

  NS_SUPPORTS_INTERFACE_POINTER_CID: TGUID = '{A99FEBBA-1DD1-11B2-A943-B02334A6D083}';
  NS_SUPPORTS_INTERFACE_POINTER_CONTRACTID = '@mozilla.org/supports-interface-pointer;1';

(**
 * nsHashPropertyBag impl of nsIWritablePropertyBag
 *)
  NS_HASH_PROPERTY_BAG_CID: TGUID = '{678c50b8-6bcb-4ad0-b9b8-c81175955199}';
  NS_HASH_PROPERTY_BAG_CONSTRACTID = '@mozilla.org/hash-property-bag;1';

(*
 * Original code: xpcom/glue/nsIGenericFactory.h.
 *)
  // nsIGenericFactory
  NS_GENERICFACTORY_CID: TGUID = '{3bc97f01-ccdf-11d2-bab8-b548654461fc}';
  NS_IGENERICFACTORY_CID: TGUID = '{3bc97f00-ccdf-11d2-bab8-b548654461fc}';
  NS_GENERICFACTORY_CONTRACTID = '@mozilla.org/generic-factory;1';
  NS_GENERICFACTORY_CLASSNAME = 'Generic Factory';

(*
 * Original code: docshell/base/nsIGlobalHistory.idl.
 *)
  // nsIGlobalHistory
  NS_GLOBALHISTORY_CONTRACTID = '@mozilla.org/browser/global-history;1';

(*
 * Original code: modules/libpref/public/nsIPrefBranch.idl.
 *)
  // nsIPrefBranch
  NS_PREFBRANCH_CONTRACTID = '@mozilla.org/preferencesbranch;1';
  NS_PREFBRANCH_CLASSNAME = 'Preferences Branch';

(*
 * Original code: modules/libpref/public/nsIPrefLocalizedString.idl
 *)
  NS_PREFLOCALIZEDSTRING_CID: TGUID ='{064d9cee-1dd2-11b2-83e3-d25ab0193c26}';
  NS_PREFLOCALIZEDSTRING_CONTRACTID = '@mozilla.org/pref-localizedstring;1';
  NS_PREFLOCALIZEDSTRING_CLASSNAME = 'Pref LocalizedString';

(*
 * Original code: modules/libpref/public/nsIPrefService.idl
 *)
  // nsIPrefService
  NS_PREFSERVICE_CID: TGUID ='{1cd91b88-1dd2-11b2-92e1-ed22ed298000}';
  NS_PREFSERVICE_CONTRACTID = '@mozilla.org/preferences-service;1';
  NS_PREFSERVICE_CLASSNAME = 'Preferences Server';
(**
 * Notification sent before reading the default user preferences files.
 *)
  NS_PREFSERVICE_READ_TOPIC_ID = 'prefservice:before-read-userprefs';
(**
 * Notification sent when resetPrefs has been called, but before the actual
 * reset process occurs.
 *)
  NS_PREFSERVICE_RESET_TOPIC_ID = 'prefservice:before-reset';

(*
 * Original code: profile/public/nsIProfile.idl
 *)
  // nsIProfile
  NS_PROFILE_CID: TGUID = '{02b0625b-e7f3-11d2-9f5a-006008a6efe9}';
  NS_PROFILE_CONTRACTID	= '@mozilla.org/profile/manager;1';
  NS_PROFILE_STARTUP_CATEGORY = 'profile-startup-category';

  // nsIProtocolHandler
(**
 * Protocol handlers are registered with XPCOM under the following CONTRACTID prefix:
 *)
  NS_NETWORK_PROTOCOL_CONTRACTID_PREFIX = '@mozilla.org/network/protocol;1?name=';
(**
 * For example, "@mozilla.org/network/protocol;1?name=http"
 *)

(*
 * Original code: xpfe/components/shistory/public/nsISHistory.idl
 *)
  // nsISHistory
  NS_SHISTORY_CID: TGUID = '{7294fe9c-14d8-11d5-9882-00C04fa02f40}';
  NS_SHISTORY_CONTRACTID = '@mozilla.org/browser/shistory;1';

(*
 * Original code: xpfe/components/shistory/public/nsISHistoryListener.idl
 *)
  // nsISHistoryListener
  NS_SHISTORYLISTENER_CONTRACTID = '@mozilla.org/browser/shistorylistener;1';

 (*
  * Original code: embedding/components/windowwatcher/public/nsIWindowWatcher.idl
  *)
  // nsIWindowWatcher
  NS_WINDOWWATCHER_IID: TGUID = '{002286a8-494b-43b3-8ddd-49e3fc50622b}';
  NS_WINDOWWATCHER_CONTRACTID = '@mozilla.org/embedcomp/window-watcher;1';

implementation

end.
