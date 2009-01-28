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
unit nsTypes;

interface

uses
  nsConsts;

type
  PRBool = LongBool;
  PRUint8 = Byte;
  PRUint16 = Word;
  PRUint32 = Longword;
  PRUint64 = Int64;
  PRTime = Int64;
  PRInt8 = Shortint;
  PRInt16 = Smallint;
  PRInt32 = Longint;
  PRInt64 = Int64;

  nsresult = PRUint32;
  nsrefcnt = PRUint32;

  size_t = PRUint32;

  PRSize = PRUint32;

  PRFileDesc = type Pointer;
  PRLibrary = type Pointer;

  nsPoint = record
    x, y: Longint;
  end;

  nsRect = record
    left, top, right, bottom: Longint;
  end;
  
  nsMargin = record
    left, top, right, bottom: Longint;
  end;

  TGUIDArray = array[0..16383] of TGUID;

  nsHashKey = type Pointer;

  // not FROZEN interfaces
  nsIDOMCounter = interface end;
  nsIDOMRect = interface end;
  nsIDOMRGBColor = interface end;
  nsIPrintSession = interface end;
  nsIAuthPrompt = interface end;
  nsIWidget = interface end;

  PPWideCharArray = ^PWideCharArray;
  PWideCharArray = array[0..16383] of PWideChar;
  PPAnsiCharArray = ^PAnsiCharArray;
  PAnsiCharArray = array[0..16383] of PAnsiChar;

  PGUIDArray = array[0..16383] of PGUID;

  PPRFileDesc = type Pointer;
  PFile = type Pointer;
  PPRLibrary = type Pointer;

  PRUint8Array = ^PRUint8;

  GREVersionRange = record
    lower: PAnsiChar;
    lowerInclusive: PRBool;
    upper: PAnsiChar;
    upperInclusive: PRBool;
  end;
  PGREVersionRangeArray = ^GREVersionRangeArray;
  GREVersionRangeArray = array [0..SizeOf(GREVersionRange)] of GREVersionRange;

  GREProperty = record
    property_: PAnsiChar;
    value: PAnsiChar;
  end;
  PGREPropertyArray = ^GREPropertyArray;
  GREPropertyArray = array[0..SizeOf(GREProperty)] of GREProperty;

implementation

end.
