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
unit nsProfile;

interface

uses
  nsXPCOM, nsTypes, nsXPCOMGlue;

type
  nsProfileDirServiceProvider = interface(nsIDirectoryServiceProvider)
    procedure SetProfileDir(AProfileDir: nsIFile);
    procedure Register;
    procedure Shutdown;
  end;

  EGeckoProfileError = class(EGeckoError);

function NS_NewProfileDirServiceProvider(aNotifyObservers: PRBool): nsProfileDirServiceProvider;

resourcestring
  SProfileRegisterError = 'Failed to register profile.';
  SProfileShutdownError = 'Failed to shutdown profile.';
  SSetProfileDirError = 'Failed to register profile directory.';
  SNotADirectory = 'The specified file is not a directory.';
  SProfileInitError = 'Failed to initialize profile.';
  SEnsureProfileDirError = 'Cannot ensure profile directory.';

implementation

uses
  Windows, SysUtils, nsConsts, nsError, nsGeckoStrings, nsCID;

type
  TProfileDirLock = class(TObject)
    FHaveLock: Boolean;
    FLockFileHandle: THandle;

    constructor Create; overload;
    constructor Create(src: TProfileDirLock); overload;
    destructor Destroy; override;

    procedure Assign(rhs: TProfileDirLock);
    function Lock(aFile: nsILocalFile): nsresult;
    function Unlock: nsresult;
  end;

  TProfileDirServiceProvider = class(TInterfacedObject,
                                     nsProfileDirServiceProvider,
                                     nsIDirectoryServiceProvider)
    FProfileDir: nsIFile;
    FProfileDirLock: TProfileDirLock;
    FNotifyObservers: PRBool;
    FSharingEnabled: Boolean;
    FNonSharedDirName: IInterfacedString;
    FNonSharedProfileDir: nsIFile;
    function GetFile(const prop: PAnsiChar; out persistent: PRBool): nsIFile; safecall;
    procedure SetProfileDir(AProfileDir: nsIFile);
    procedure Register;
    procedure Shutdown;

    constructor Create(aNotifyObservers: PRBool = True);
    destructor Destroy; override;

    procedure Initialize;
    procedure InitProfileDir(profileDir: nsIFile);
    procedure InitNonSharedProfileDir;
    procedure EnsureProfileFileExists(aFile: nsIFile; destDir: nsIFile);
    procedure UndefineFileLocations;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

constructor TProfileDirLock.Create;
begin
  inherited Create;

  FHaveLock := False;
  FLockFileHandle := INVALID_HANDLE_VALUE;
end;

constructor TProfileDirLock.Create(src: TProfileDirLock);
begin
  inherited Create;

  Assign(src);
end;

procedure TProfileDirLock.Assign(rhs: TProfileDirLock);
begin
  Unlock();

  FHaveLock := rhs.FHaveLock;
  rhs.FHaveLock := False;

  FLockFileHandle := rhs.FLockFileHandle;
  rhs.FLockFileHandle := INVALID_HANDLE_VALUE;
end;

destructor TProfileDirLock.Destroy;
begin
  Unlock();
  inherited;
end;

function TProfileDirLock.Lock(aFile: nsILocalFile): nsresult;
const
  LOCKFILE_NAME = 'parent.lock';
var
  isDir: PRBool;
  lockFile: nsIFile;
  lockFileName: IInterfacedString;
  filePath: IInterfacedCString;
begin
  try
    lockFileName := NewString(LOCKFILE_NAME);
    filePath := NewCString;

    if not FHaveLock then
    begin
      Result := NS_ERROR_UNEXPECTED;
      Exit;
    end;

    isDir := aFile.IsDirectory();

    if not isDir then
    begin
      Result := NS_ERROR_FILE_NOT_DIRECTORY;
      Exit;
    end;

    lockFile := aFile.Clone();
    lockFile.Append(lockFileName.AString);
    lockFile.GetNativePath(filePath.ACString);

    FLockFileHandle := CreateFileA(PAnsiChar(filePath.ToString),
                                  GENERIC_READ or GENERIC_WRITE,
                                  0,
                                  nil,
                                  OPEN_ALWAYS,
                                  FILE_FLAG_DELETE_ON_CLOSE,
                                  0);
    if FLockFileHandle = INVALID_HANDLE_VALUE then
    begin
      Result := NS_ERROR_FILE_ACCESS_DENIED;
      Exit;
    end;
    FHaveLock := True;
    Result := NS_OK;
  except
    on EOutOfMemory do
      Result := NS_ERROR_OUT_OF_MEMORY;
    on ESafeCallException do
      Result := NS_ERROR_FAILURE;
  else
    Result := NS_ERROR_UNEXPECTED;
  end;
end;

function TProfileDirLock.Unlock: nsresult;
begin
  Result := NS_OK;

  if FHaveLock then
  begin
    if FLockFileHandle <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(FLockFileHandle);
      FLockFileHandle := INVALID_HANDLE_VALUE;
    end;
    FHaveLock := False;
  end;
end;

constructor TProfileDirServiceProvider.Create(aNotifyObservers: PRBool);
begin
  inherited Create;
  FNotifyObservers := aNotifyObservers;
end;

destructor TProfileDirServiceProvider.Destroy;
begin
  FProfileDirLock.Free;
  inherited;
end;

procedure TProfileDirServiceProvider.SetProfileDir(aProfileDir: nsIFile);
var
{$IFDEF MOZ_PROFILELOCKING}
  dirToLock: nsILocalFile;
{$ENDIF}
  observerService: nsIObserverService;
begin
  try
    if Assigned(FProfileDir) then
    begin
      if Assigned(aProfileDir) and
         aProfileDir.Equals(FProfileDir) then
        begin
          Exit;
        end;
{$IFDEF MOZ_PROFILELOCKING}
      FProfileDirLock.Unlock();
{$ENDIF}
      UndefineFileLocations;
    end;
    FProfileDir := aProfileDir;
    if not Assigned(FProfileDir) then
    begin
      Exit;
    end;

{$IFDEF MOZ_PROFILELOCKING}
    Result := InitProfileDir(FProfileDir);
    if NS_FAILED(Result) then Exit;

    if FSharingEnabled then
      Result := FNonSharedProfileDir.QueryInterface(nsILocalFile, dirToLock)
    else
      Result := FNonSharedProfileDir.QueryInterface(nsILocalFile, dirToLock);
    if NS_FAILED(Result) then Exit;
    Result := FProfileDirLock.Lock(dirToLock);
    if NS_FAILED(Result) then Exit;
{$ENDIF}

    if FNotifyObservers then
    begin
      NS_GetService('@mozilla.org/observer-service;1', nsIObserverService, observerService);

      observerService.NotifyObservers(nil, 'profile-do-change', 'startup');
      observerService.NotifyObservers(nil, 'profile-after-change', 'startup');
    end;

  except
    raise EGeckoProfileError.CreateRes(PResStringRec(@SSetProfileDirError));
  end;
end;

procedure TProfileDirServiceProvider.Register;
var
  directoryService: nsIDirectoryService;
begin
  NS_GetService(NS_DIRECTORY_SERVICE_CONTRACTID,
                nsIDirectoryService,
                directoryService);
  try
    directoryService.RegisterProvider(Self);
  except
    raise EGeckoProfileError.CreateRes(PResStringRec(@SProfileRegisterError));
  end;
end;

procedure TProfileDirServiceProvider.Shutdown;
var
  observerService: nsIObserverService;
begin
  NS_GetService('@mozilla.org/observer-service;1',
                nsIObserverService,
                observerService);
  try
    observerService.NotifyObservers(nil, 'profile-before-change', 'shutdown-persist');
  except
    raise EGeckoProfileError.CreateRes(PResStringRec(@SProfileShutdownError));
  end;
end;

function TProfileDirServiceProvider.GetFile(const prop: PAnsiChar; out persistent: PRBool): nsIFile;
var
  localFile: nsIFile;
  domainDir: nsIFile;
  appendStr: IInterfacedCString;
const
  PREFS_FILE_50_NAME =' prefs.js';
  USER_CHROME_DIR_50_NAME = 'chrome';
  LOCAL_STORE_FILE_50_NAME = 'localstore.rdf';
  HISTORY_FILE_50_NAME = 'history.dat';
  PANELS_FILE_50_NAME = 'panels.rdf';
  MIME_TYPES_FILE_50_NAME = 'mimeTypes.rdf';
  BOOKMARKS_FILE_50_NAME = 'bookmark.html';
  DOWNLOADS_FILE_50_NAME = 'downloads.rdf';
  SEARCH_FILE_50_NAME = 'search.rdf';
  MAIL_DIR_50_NAME = 'Mail';
  IMAP_MAIL_DIR_50_NAME = 'ImapMail';
  NEWS_DIR_50_NAME = 'News';
  MSG_FOLDER_CACHE_DIR_50_NAME = 'panacea.dat';
begin
  appendStr := NewCString;

  persistent := True;
  domainDir := FProfileDir;

  Assert(Assigned(domainDir));
  
  if prop = NS_APP_PREFS_50_DIR then
  begin
    localFile := domainDir.Clone();
  end else
  if prop = NS_APP_PREFS_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(PREFS_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_USER_PROFILE_50_DIR then
  begin
    localFile := domainDir.Clone();
  end else
  if prop = NS_APP_USER_CHROME_DIR then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(USER_CHROME_DIR_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_LOCALSTORE_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(LOCAL_STORE_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
    EnsureProfileFileExists(localFile, domainDir);
  end else
  if prop = NS_APP_HISTORY_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(HISTORY_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_USER_PANELS_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(PANELS_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
    EnsureProfileFileExists(localFile, domainDir);
  end else
  if prop = NS_APP_USER_MIMETYPES_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(MIME_TYPES_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
    EnsureProfileFileExists(localFile, domainDir);
  end else
  if prop = NS_APP_BOOKMARKS_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(BOOKMARKS_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_DOWNLOADS_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(DOWNLOADS_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_SEARCH_50_FILE then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(SEARCH_FILE_50_NAME);
    localFile.AppendNative(appendStr.ACString);
    EnsureProfileFileExists(localFile, domainDir);
  end else
  if prop = NS_APP_MAIL_50_DIR then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(MAIL_DIR_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_IMAP_MAIL_50_DIR then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(IMAP_MAIL_DIR_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_NEWS_50_DIR then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(NEWS_DIR_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end else
  if prop = NS_APP_MESSENGER_FOLDER_CACHE_50_DIR then
  begin
    localFile := domainDir.Clone();
    appendStr.Assign(MSG_FOLDER_CACHE_DIR_50_NAME);
    localFile.AppendNative(appendStr.ACString);
  end;

  if Assigned(localFile) then
    Result := localFile as nsIFile;
end;

procedure TProfileDirServiceProvider.Initialize;
begin
{$IFDEF MOZ_PROFILELOCKING}
  FProfileDir := TProfileDirServiceProvider.Create(FNotifyObservers);
{$ENDIF}
end;

procedure TProfileDirServiceProvider.InitProfileDir(profileDir: nsIFile);
var
  exists: PRBool;
  profileDefaultsDir: nsIFile;
  profileDirParent: nsIFile;
  profileDirName: IInterfacedCString;
  isDir: PRBool;
begin
  try
    profileDirName := NewCString;

    exists := profileDir.Exists();

    if not exists then
    begin
      profileDirParent := profileDir.Parent;
      profileDir.GetNativeLeafName(profileDirName.ACString);

      try
        profileDefaultsDir := NS_GetSpecialDirectory(NS_APP_PROFILE_DEFAULTS_50_DIR);
      except
        profileDefaultsDir := NS_GetSpecialDirectory(NS_APP_PROFILE_DEFAULTS_NLOC_50_DIR);
      end;
      try
        profileDefaultsDir.CopyToNative(profileDirParent, profileDirName.ACString);
      except
        profileDirParent.AppendNative(profileDirName.ACString);
        profileDirParent.Create(NS_IFILE_DIRECTORY_TYPE, 7 shl 6);
      end;
    end else
    begin
      isDir := profileDir.IsDirectory();
      if not isDir then
        raise EGeckoProfileError.CreateRes(PResStringRec(@SNotADirectory));
    end;

    if FNonSharedDirName.Length > 0 then
      InitNonSharedProfileDir;
  except
    on EGeckoError do raise
    else raise EGeckoProfileError.CreateRes(PResStringRec(@SProfileInitError));
  end;
end;

procedure TProfileDirServiceProvider.InitNonSharedProfileDir;
var
  localDir: nsIFile;
  exists: PRBool;
  isDir: PRBool;
begin
  try
    localDir := FProfileDir.Clone();
    localDir.Append(FNonSharedDirName.AString);
    exists := localDir.Exists();
    if not exists then
    begin
      localDir.Create(NS_IFILE_DIRECTORY_TYPE, 7 shl 6);
    end else
    begin
      isDir := localDir.IsDirectory();
      if not isDir then
        raise EGeckoProfileError.CreateRes(PResStringRec(@SNotADirectory));
    end;
    FNonSharedProfileDir := localDir;
  except
    on EGeckoError do raise
    else raise EGeckoProfileError.CreateRes(PResStringRec(@SProfileInitError));
  end;
end;

procedure TProfileDirServiceProvider.EnsureProfileFileExists(aFile: nsIFile; destDir: nsIFile);
var
  exists: PRBool;
  defaultsFile: nsIFile;
  leafName: IInterfacedCString;
begin
  try
    exists := aFile.Exists;
    if exists then
    begin
      Exit;
    end;

    try
      defaultsFile := NS_GetSpecialDirectory(NS_APP_PROFILE_DEFAULTS_50_DIR);
    except
      defaultsFile := NS_GetSpecialDirectory(NS_APP_PROFILE_DEFAULTS_NLOC_50_DIR);
    end;

    leafName := NewCString;

    aFile.GetNativeLeafName(leafName.ACString);
    defaultsFile.AppendNative(leafName.ACString);

    leafName.Assign('');
    defaultsFile.CopyToNative(destDir, leafName.ACString);
  except
    on EGeckoError do raise
    else raise EGeckoProfileError.CreateRes(PResStringRec(@SEnsureProfileDirError));
  end;
end;

procedure TProfileDirServiceProvider.UndefineFileLocations;
var
  directoryService : nsIProperties;
  i: Integer;
const
  NUM_OF_DIRS = 15;
  Dirs: array [1..15] of PAnsiChar = (
    NS_APP_PREFS_50_DIR,
    NS_APP_PREFS_50_FILE,
    NS_APP_USER_PROFILE_50_DIR,
    NS_APP_USER_CHROME_DIR,
    NS_APP_LOCALSTORE_50_FILE,
    NS_APP_HISTORY_50_FILE,
    NS_APP_USER_PANELS_50_FILE,
    NS_APP_USER_MIMETYPES_50_FILE,
    NS_APP_BOOKMARKS_50_FILE,
    NS_APP_DOWNLOADS_50_FILE,
    NS_APP_SEARCH_50_FILE,
    NS_APP_MAIL_50_DIR,
    NS_APP_IMAP_MAIL_50_DIR,
    NS_APP_NEWS_50_DIR,
    NS_APP_MESSENGER_FOLDER_CACHE_50_DIR
  );
begin
  NS_GetService(NS_DIRECTORY_SERVICE_CONTRACTID,
                nsIProperties,
                directoryService);

  for I:=1 to NUM_OF_DIRS do
    try
      directoryService.Undefine(Dirs[I]);
    except
    end;
end;

function TProfileDirServiceProvider.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Result := HResult(NS_ERROR_FAILURE);
end;

function NS_NewProfileDirServiceProvider(aNotifyObservers: PRBool): nsProfileDirServiceProvider;
var
  prov: TProfileDirServiceProvider;
begin
  prov := TProfileDirServiceProvider.Create(aNotifyObservers);

  prov.Initialize;
  Result := prov;
  prov.FNotifyObservers := aNotifyObservers;
end;

end.
