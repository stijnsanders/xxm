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
unit nsError;

interface

uses nsTypes;

const
(*
 * Original code: xpcom/base/nsError.h
 *)

(**
 * @name Standard Module Offset Code. Each Module should identify a unique number
 *       and then all errors associated with that module become offsets from the
 *       base associated with that module id. There are 16 bits of code bits for
 *       each module.
 *)
  NS_ERROR_MODULE_XPCOM      = 1;
  NS_ERROR_MODULE_BASE       = 2;
  NS_ERROR_MODULE_GFX        = 3;
  NS_ERROR_MODULE_WIDGET     = 4;
  NS_ERROR_MODULE_CALENDER   = 5;
  NS_ERROR_MODULE_NETWORK    = 6;
  NS_ERROR_MODULE_PLUGINS    = 7;
  NS_ERROR_MODULE_LAYOUT     = 8;
  NS_ERROR_MODULE_HTMLPARSER = 9;
  NS_ERROR_MODULE_RDF        = 10;
  NS_ERROR_MODULE_UCONV      = 11;
  NS_ERROR_MODULE_REG        = 12;
  NS_ERROR_MODULE_FILES      = 13;
  NS_ERROR_MODULE_DOM        = 14;
  NS_ERROR_MODULE_IMGLIB     = 15;
  NS_ERROR_MODULE_MAILNEWS   = 16;
  NS_ERROR_MODULE_ERITOR     = 17;
  NS_ERROR_MODULE_XPCONNECT  = 18;
  NS_ERROR_MODULE_PROFILE    = 19;
  NS_ERROR_MODULE_LDAP       = 20;
  NS_ERROR_MODULE_SECURITY   = 21;
  NS_ERROR_MODULE_DOM_XPATH  = 22;
  NS_ERROR_MODULE_DOM_RANGE  = 23;
  NS_ERROR_MODULE_URILOADER  = 24;
  NS_ERROR_MODULE_CONTENT    = 25;
  NS_ERROR_MODULE_PYXPCOM    = 26;
  NS_ERROR_MODULE_XSLT       = 27;
  NS_ERROR_MODULE_IPC        = 28;
  NS_ERROR_MODULE_SVG        = 29;
  NS_ERROR_MODULE_STORAGE    = 30;
  NS_ERROR_MODULE_SCHEMA     = 31;

  NS_ERROR_MODULE_GENERAL    = 51;

(**
 * @name Severity Code.  This flag identifies the level of warning
 *)
  NS_ERROR_SEVERITY_SUCCESS = 0;
  NS_ERROR_SEVERITY_ERROR   = 1;

  NS_OK = 0;

  NS_ERROR_BASE                         = nsresult($c1f30000);
  NS_ERROR_NOT_INITIALIZED              = nsresult($c1f30001);
  NS_ERROR_ALREADY_INITIALIZED          = nsresult($c1f30002);
  NS_ERROR_NOT_IMPLEMENTED              = nsresult($80004001);
  NS_ERROR_NOINTERFACE                  = nsresult($80004002);
  NS_ERROR_NO_INTERFACE                 = nsresult($80004002);
  NS_ERROR_INVALID_POINTER              = nsresult($80004003);
  NS_ERROR_NULL_POINTER                 = nsresult($80004003);
  NS_ERROR_ABORT                        = nsresult($80004004);
  NS_ERROR_FAILURE                      = nsresult($80004005);
  NS_ERROR_UNEXPECTED                   = nsresult($8000ffff);
  NS_ERROR_OUT_OF_MEMORY                = nsresult($8007000e);
  NS_ERROR_ILLEGAL_VALUE                = nsresult($80070057);
  NS_ERROR_INVALID_ARG                  = nsresult($80070057);
  NS_ERROR_NO_AGGREGATION               = nsresult($80040110);
  NS_ERROR_NOT_AVAILABLE                = nsresult($80040111);
  NS_ERROR_FACTORY_NOT_REGISTERED       = nsresult($80040154);
  NS_ERROR_FACTORY_REGISTER_AGAIN       = nsresult($80040155);
  NS_ERROR_FACTORY_NOT_LOADED           = nsresult($800401f8);
  NS_ERROR_FACTORY_NO_SIGNATURE_SUPPORT = nsresult($c1f30101);
  NS_ERROR_FACTORY_EXISTS               = nsresult($c1f30100);
  NS_ERROR_PROXY_INVALID_IN_PARAMETER   = nsresult($80010010);
  NS_ERROR_PROXY_INVALID_OUT_PARAMETER  = nsresult($80010011);

  NS_BASE_STREAM_CLOSED         = nsresult($80470002);
  NS_BASE_STREAM_OSERROR        = nsresult($80470003);
  NS_BASE_STREAM_ILLEGAL_ARGS   = nsresult($80470004);
  NS_BASE_STREAM_NO_CONVERTER   = nsresult($80470005);
  NS_BASE_STREAM_BAD_CONVERSION = nsresult($80470006);
  NS_BASE_STREAM_WOULD_BLOCK    = nsresult($80470007);

  NS_ERROR_FILE_UNRECOGNIZED_PATH     = nsresult($80480001);
  NS_ERROR_FILE_UNRESOLVABLE_SYMLINK  = nsresult($80480002);
  NS_ERROR_FILE_EXECUTION_FAILED      = nsresult($80480003);
  NS_ERROR_FILE_UNKNOWN_TYPE          = nsresult($80480004);
  NS_ERROR_FILE_DESTINATION_NOT_DIR   = nsresult($80480005);
  NS_ERROR_FILE_TARGET_DOES_NOT_EXIST = nsresult($80480006);
  NS_ERROR_FILE_COPY_OR_MOVE_FAILED   = nsresult($80480007);
  NS_ERROR_FILE_ALREADY_EXISTS        = nsresult($80480008);
  NS_ERROR_FILE_INVALID_PATH          = nsresult($80480009);
  NS_ERROR_FILE_DISK_FULL             = nsresult($8048000A);
  NS_ERROR_FILE_CORRUPTED             = nsresult($8048000B);
  NS_ERROR_FILE_NOT_DIRECTORY         = nsresult($8048000C);
  NS_ERROR_FILE_IS_DIRECTORY          = nsresult($8048000D);
  NS_ERROR_FILE_IS_LOCKED             = nsresult($8048000E);
  NS_ERROR_FILE_TOO_BIG               = nsresult($8048000F);
  NS_ERROR_FILE_NO_DEVICE_SPACE       = nsresult($80480010);
  NS_ERROR_FILE_NAME_TOO_LONG         = nsresult($80480011);
  NS_ERROR_FILE_NOT_FOUND             = nsresult($80480012);
  NS_ERROR_FILE_READ_ONLY             = nsresult($80480013);
  NS_ERROR_FILE_DIR_NOT_EMPTY         = nsresult($80480014);
  NS_ERROR_FILE_ACCESS_DENIED         = nsresult($80480015);

  NS_ERROR_CANNOT_CONVERT_DATA          = nsresult($80460001);
  NS_ERROR_OBJECT_IS_IMMUTABLE          = nsresult($80460002);
  NS_ERROR_LOSS_OF_SIGNIFICANT_DATA     = nsresult($80460003);
  NS_SUCCESS_LOSS_OF_INSIGNIFICANT_DATA = nsresult($00460003);

(*
 * @name Standard Error Handling Macros
 *)
 
function NS_FAILED(rv: nsresult): Boolean;
function NS_SUCCEEDED(rv: nsresult): Boolean;

implementation

function NS_FAILED(rv: nsresult): Boolean;
begin
  Result := (rv and $80000000) <> 0;
end;

function NS_SUCCEEDED(rv: nsresult): Boolean;
begin
  Result := (rv and $80000000) = 0;
end;

end.
