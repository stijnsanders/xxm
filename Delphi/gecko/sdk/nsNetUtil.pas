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
unit nsNetUtil;

interface

uses
  nsXPCOM, nsConsts, nsTypes, nsGeckoStrings;

function NS_GetIOService: nsIIOService;

function NS_NewURI(const spec: nsAUTF8String;
                   const charset: AnsiString='';
                   baseURI: nsIURI=nil; ioService:
                   nsIIOService=nil): nsIURI; overload;
function NS_NewURI(const spec: UTF8String;
                   baseURI: nsIURI=nil;
                   ioService: nsIIOService=nil): nsIURI; overload;
function NS_NewFileURI(spec: nsIFile; ioService:
                       nsIIOService=nil): nsIURI;
function NS_NewChannel(uri: nsIURI;
                       ioService: nsIIOService=nil;
                       loadGroup: nsILoadGroup=nil;
                       callbacks: nsIInterfaceRequestor=nil;
                       loadFlags: Longword=NS_IREQUEST_LOAD_NORMAL): nsIChannel;
function NS_OpenURI(uri: nsIURI;
                    ioService: nsIIOService=nil;
                    loadGroup: nsILoadGroup=nil;
                    callbacks: nsIInterfaceRequestor=nil;
                    loadFlags: Longword=NS_IREQUEST_LOAD_NORMAL): nsIInputStream; overload;
procedure NS_OpenURI(listener: nsIStreamListener;
                     context: nsISupports; uri:
                     nsIURI; ioService: nsIIOService=nil;
                     loadGroup: nsILoadGroup=nil;
                     callbacks: nsIInterfaceRequestor=nil;
                     loadFlags: Longword=NS_IREQUEST_LOAD_NORMAL); overload;
procedure NS_MakeAbsoluteURI(uri: nsAUTF8String;
                            const spec: nsAUTF8String;
                            baseURI: nsIURI;
                            ioService: nsIIOService=nil); overload;
function NS_MakeAbsoluteURI(const spec: UTF8String;
                            baseURI: nsIURI;
                            ioService: nsIIOService=nil): UTF8String; overload;
function NS_NewLoadGroup(obs: nsIRequestObserver): nsILoadGroup;


function NS_NewURI(const spec: nsAString; const charset: AnsiString=''; baseURI: nsIURI=nil; ioService: nsIIOService=nil): nsIURI; overload;
procedure NS_MakeAbsoluteURI(uri: nsAString; const spec: nsAString; baseURI: nsIURI; ioService: nsIIOService=nil); overload;

resourcestring
  SNewURIError = 'Cannot create instance of nsIURI by URI ''%s.'' ';
  SConversationError = 'Cannot convert strings between UTF8 and UTF16.';
  SNewFileURIError = 'Cannot create instance of nsIURI by file ''%s.''';
  SNewChannelError = 'Cannot create instance of nsIChannel.';
  SOpenURIError = 'Cannot open URI ''%s.'' ';
  SMakeAbsoluteURIError = 'Cannot make the absolute URI.';
  SNewLoadGroupError = 'Cannot create instance of nsILoadGroup.';

implementation

uses
  nsXPCOMGlue, nsError, nsInit;

function NS_GetIOService: nsIIOService;
const
  kIOServiceCID:TGUID = '{9ac9e770-18bc-11d3-9337-00104ba0fd40}';
begin
  NS_GetService(kIOServiceCID, nsIIOService, Result);
end;

function EnsureIOService(ios: nsIIOService): nsIIOService;
begin
  if not Assigned(ios) then
  begin
    Result := NS_GetIOService;
  end else
  begin
    Result := ios;
  end;
end;

function NS_NewURI(const spec: nsAUTF8String; const charset: AnsiString; baseURI: nsIURI; ioService: nsIIOService): nsIURI;
var
  charsetPtr: PAnsiChar;
  grip: nsIIOService;
  str: IInterfacedUTF8String;
begin
  if Length(charset)>0 then
    charsetPtr := PAnsiChar(charset)
  else
    charsetPtr := nil;

  grip := EnsureIOService(ioService);
  try
    Result := grip.NewURI(spec, charsetPtr, baseURI);
  except
    str := RelateUTF8String(spec);
    raise EGeckoError.CreateResFmt(PResStringRec(@SNewURIError), [str.ToString]);
  end;
end;

function NS_NewURI(const spec: nsAString; const charset: AnsiString; baseURI: nsIURI; ioService: nsIIOService): nsIURI;
var
  spec2: IInterfacedUTF8String;
  rv: nsresult;
begin
  spec2 := NewUTF8String;
  rv := NS_UTF16ToCString(spec, NS_ENCODING_UTF8, spec2.AUTF8String);
  if NS_FAILED(rv) then
    raise EGeckoError.CreateRes(PResStringRec(@SConversationError));
  Result := NS_NewURI(spec2.AUTF8String, charset, baseURI, ioService);
end;

function NS_NewURI(const spec: UTF8String; baseURI: nsIURI; ioService: nsIIOService): nsIURI;
var
  spec2: IInterfacedUTF8String;
begin
  spec2 := NewUTF8String(spec);

  Result := NS_NewURI(spec2.AUTF8String, '', baseURI, ioService);
end;

function NS_NewFileURI(spec: nsIFile; ioService:
                       nsIIOService): nsIURI;
var
  grip: nsIIOService;
  str: IInterfacedUTF8String;
begin
  grip := EnsureIOService(ioService);
  if Assigned(grip) then
  try
    Result := grip.NewFileURI(spec);
  except
    str := NewUTF8String;
    spec.GetNativePath(str.AUTF8String);
    raise EGeckoError.CreateResFmt(PResStringRec(@SNewFileURIError), [str.ToString]);
  end;
end;

function NS_NewChannel(uri: nsIURI;
                       ioService: nsIIOService;
                       loadGroup: nsILoadGroup;
                       callbacks: nsIInterfaceRequestor;
                       loadFlags: Longword): nsIChannel;
var
  grip: nsIIOService;
  chan: nsIChannel;
begin
  grip := EnsureIOService(ioService);
  try
    chan := grip.NewChannelFromURI(uri);
    if Assigned(loadGroup) then
      chan.SetLoadGroup(loadGroup);
    if Assigned(callbacks) then
      chan.SetNotificationCallbacks(callbacks);
    if loadFlags <> NS_IREQUEST_LOAD_NORMAL then
      chan.SetLoadFlags(loadFlags);
    Result :=  chan;
  except
    raise EGeckoError.CreateRes(PResStringRec(@SNewChannelError));
  end;
end;

function NS_OpenURI(uri: nsIURI;
                    ioService: nsIIOService;
                    loadGroup: nsILoadGroup;
                    callbacks: nsIInterfaceRequestor;
                    loadFlags: Longword): nsIInputStream;
var
  channel: nsIChannel;
  st: nsIInputStream;
  str: IInterfacedCString;
begin
  try
    channel := NS_NewChannel(uri, ioService, loadGroup, callbacks, loadFlags);
    st := channel.Open;
    Result := st;
  except
    str := NewCString;
    uri.GetSpec(str.ACString);
    raise EGeckoError.CreateResFmt(PResStringRec(@SOpenURIError), [str.ToString]);
  end;
end;

procedure NS_OpenURI(listener: nsIStreamListener;
                    context: nsISupports; uri:
                    nsIURI; ioService: nsIIOService;
                    loadGroup: nsILoadGroup;
                    callbacks: nsIInterfaceRequestor;
                    loadFlags: Longword);
var
  channel: nsIChannel;
  str: IInterfacedCString;
begin
  try
    channel := NS_NewChannel(uri, ioService, loadGroup, callbacks, loadFlags);
    channel.AsyncOpen(listener, context);
  except
    str := NewCString;
    uri.GetSpec(str.ACString);
    raise EGeckoError.CreateResFmt(PResStringRec(@SOpenURIError), [str.ToString]);
  end;
end;

procedure NS_MakeAbsoluteURI(uri: nsAUTF8String;
                             const spec: nsAUTF8String;
                             baseURI: nsIURI;
                             ioService: nsIIOService);
var
  uri2, spec2: IInterfacedUTF8String;
begin
  uri2 := RelateUTF8String(uri);
  spec2 := RelateUTF8String(spec);

  if not Assigned(baseURI) then
  begin
    uri2.Assign(spec2);
  end else
  if uri2.Length()>0 then
  try
    baseURI.Resolve(spec2.AUTF8String, uri2.AUTF8String);
  except
    raise EGeckoError.CreateRes(PResStringRec(@SMakeAbsoluteURIError));
  end else
    raise EGeckoError.CreateRes(PResStringRec(@SMakeAbsoluteURIError));
end;

procedure NS_MakeAbsoluteURI(uri: nsAString; const spec: nsAString; baseURI: nsIURI; ioService: nsIIOService=nil);
var
  uri2, spec2: IInterfacedString;
  buf1, buf2: IInterfacedUTF8String;
  rv: nsresult;
begin
  uri2 := RelateString(uri);
  spec2 := RelateString(spec);
  buf1 := NewUTF8String;
  buf2 := NewUTF8String;

  if not Assigned(baseURI) then
  begin
    uri2.Assign(spec2);
  end else
  try
    if uri2.Length()=0 then
      baseURI.GetSpec(buf1.AUTF8String)
    else
    begin
      NS_UTF16ToCString(spec,NS_ENCODING_UTF8,buf2.AUTF8String);
      baseURI.Resolve(buf2.AUTF8String, buf1.AUTF8String);
    end;
    rv := NS_CStringToUTF16(buf1.AUTF8String, NS_ENCODING_UTF8, uri);
    if NS_FAILED(rv) then
      raise EGeckoError.CreateRes(PResStringRec(@SConversationError));
  except
    on EGeckoError do raise
    else raise EGeckoError.CreateRes(PResStringRec(@SMakeAbsoluteURIError));
  end;
end;

function NS_MakeAbsoluteURI(const spec: UTF8String;
                            baseURI: nsIURI;
                            ioService: nsIIOService): UTF8String;
var
  uri2, spec2: IInterfacedUTF8String;
begin
  uri2 := NewUTF8String();
  spec2 := NewUTF8String(spec);

  NS_MakeAbsoluteURI(uri2.AUTF8String, spec2.AUTF8String, baseURI, ioService);
  Result := uri2.ToString;
end;

function NS_NewLoadGroup(obs: nsIRequestObserver): nsILoadGroup;
const
  kLoadGroupCID: TGUID = '{e1c61582-2a84-11d3-8cce-0060b0fc14a3}';
var
  group: nsILoadGroup;
begin
  try
    NS_CreateInstance(kLoadGroupCID,nsILoadGroup,group);
    group.SetGroupObserver(obs);
    Result := group;
  except
    raise EGeckoError.CreateRes(PResStringRec(@SNewLoadGroupError));
  end;
end;

end.
