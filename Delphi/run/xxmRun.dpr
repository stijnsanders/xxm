program xxmRun;

uses
  Windows,
  SysUtils,
  Classes,
  Registry;

{$R *.res}

type
  TActionType=(atCLSID,atXXM,atIE,atPrepare,atCleanup,atApp);
  TRegCall=function: HResult; stdcall;
var
  sl:TStringList;
  Actions:array of record
    ActionType:TActionType;
    Param,FileName:AnsiString;
    Cleanup:boolean;
    Handle:cardinal;
  end;
  i,j,k:integer;
  s:AnsiString;
  r:TRegistry;
  si:TStartupInfo;
  pi:TProcessInformation;
begin
  //load and parse job list
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(ChangeFileExt(ParamStr(0),'.ini'));
    j:=0;
    for i:=0 to sl.Count-1 do
     begin
      s:=sl[i];
      if (s<>'') and not(char(s[1]) in [';','#']) then
       begin
        SetLength(Actions,j+1);
        case char(s[1]) of
          '{':
           begin
            Actions[j].ActionType:=atCLSID;
            Actions[j].Param:=Copy(s,1,38);
            Actions[j].FileName:=Copy(s,39,Length(s)-38);
           end;
          '"':
           begin
            Actions[j].ActionType:=atXXM;
            k:=2;
            while (k<=Length(s)) and (s[k]<>'"') do inc(k);
            Actions[j].Param:=Copy(s,2,k-2);
            Actions[j].FileName:=Copy(s,k+1,Length(s)-k);
           end;
          '!':
           begin
            Actions[j].ActionType:=atPrepare;
            Actions[j].FileName:=Copy(s,2,Length(s)-1);
           end;
          '_':
           begin
            Actions[j].ActionType:=atCleanup;
            Actions[j].FileName:=Copy(s,2,Length(s)-1);
           end;
          '@':
           begin
            Actions[j].ActionType:=atIE;
            Actions[j].Param:=Copy(s,2,Length(s)-1);
           end;
          ' ':
           begin
            Actions[j].ActionType:=atApp;
            Actions[j].FileName:=Copy(s,2,Length(s)-1);
           end;
          else
            raise Exception.Create('Unknown prefix: "'+s+'"');
        end;
        inc(j);
       end;
     end;
  finally
    sl.Free;
  end;

  si.cb:=SizeOf(TStartupInfo);
  ZeroMemory(@si,SizeOf(TStartupInfo));

  //open jobs
  for i:=0 to Length(Actions)-1 do
    case Actions[i].ActionType of
      atCLSID:
       begin
        r:=TRegistry.Create;
        try
          r.RootKey:=HKEY_CLASSES_ROOT;
          if r.OpenKeyReadOnly('\CLSID\'+Actions[i].Param+'\InprocServer32') or
             r.OpenKeyReadOnly('\CLSID\'+Actions[i].Param+'\LocalServer32') then
           begin
            s:=r.ReadString('');
            Actions[i].Cleanup:=not(FileExists(s));
            r.CloseKey;
           end
          else
            Actions[i].Cleanup:=true;
          if Actions[i].Cleanup then
            TRegCall(GetProcAddress(LoadLibrary(PChar(
              Actions[i].FileName)),'DllRegisterServer'));
        finally
          r.Free;
        end;
       end;
      atXXM:
       begin
        r:=TRegistry.Create;
        try
          r.RootKey:=HKEY_CURRENT_USER;
          if r.OpenKey('\SOFTWARE\xxm\local\'+Actions[i].Param,true) then
           begin
            if r.ValueExists('') then s:=r.ReadString('') else s:='';
            //Actions[i].Cleanup:=(s='') or FileExists(s);
            r.WriteString('',ExpandFileName(Actions[i].FileName));
            r.CloseKey;
           end
          else
            raise Exception.Create('Failed to create registry key \xxm\local\'+Actions[i].Param);
          Actions[i].FileName:=s;
        finally
          r.Free;
        end;
       end;
      atPrepare:
       begin
        if not(CreateProcess(nil,PChar(Actions[i].FileName),
          nil,nil,false,0,nil,nil,si,pi)) then RaiseLastOSError;
        CloseHandle(pi.hThread);
        if WaitForSingleObject(pi.hProcess,INFINITE)=WAIT_FAILED then RaiseLastOSError;
        CloseHandle(pi.hProcess);
       end;
      atCleanup:;
      atIE:
       begin
        r:=TRegistry.Create;
        try
          r.RootKey:=HKEY_LOCAL_MACHINE;
          if r.OpenKeyReadOnly('\SOFTWARE\Clients\StartMenuInternet\IEXPLORE.EXE\shell\open\command') or
             r.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE') then
           begin
            s:=r.ReadString('');
            r.CloseKey;
           end
          else
            raise Exception.Create('Internet Explorer not found');
        finally
          r.Free;
        end;
        if not(CreateProcess(nil,PChar('"'+s+'" '+Actions[i].Param),
          nil,nil,false,0,nil,nil,si,pi)) then RaiseLastOSError;
        CloseHandle(pi.hThread);
        Actions[i].Handle:=pi.hProcess;
       end;
      atApp:
       begin
        if not(CreateProcess(nil,PChar(Actions[i].FileName),
          nil,nil,false,0,nil,nil,si,pi)) then RaiseLastOSError;
        CloseHandle(pi.hThread);
        Actions[i].Handle:=pi.hProcess;
       end;
    end;

  //..

  //close jobs
  for i:=Length(Actions)-1 downto 0 do
    case Actions[i].ActionType of
      atCLSID:
        if Actions[i].Cleanup then
          TRegCall(GetProcAddress(LoadLibrary(PChar(
            Actions[i].FileName)),'DllUnregisterServer'));
      atXXM:
       begin
        r:=TRegistry.Create;
        try
          r.RootKey:=HKEY_CURRENT_USER;
          if Actions[i].FileName='' then
            r.DeleteKey('\SOFTWARE\xxm\local\'+Actions[i].Param)
          else
            if r.OpenKey('\SOFTWARE\xxm\local\'+Actions[i].Param,true) then
             begin
              r.WriteString('',Actions[i].FileName);
              r.CloseKey;
             end
            else
              raise Exception.Create('Failed to create registry key \xxm\local\'+Actions[i].Param);
        finally
          r.Free;
        end;
       end;
      atPrepare:;
      atCleanup:
       begin
        if not(CreateProcess(nil,PChar(Actions[i].FileName),
          nil,nil,false,0,nil,nil,si,pi)) then RaiseLastOSError;
        CloseHandle(pi.hThread);
        if WaitForSingleObject(pi.hProcess,INFINITE)=WAIT_FAILED then RaiseLastOSError;
        CloseHandle(pi.hProcess);
       end;
      atIE,atApp:
       begin
        if WaitForSingleObject(Actions[i].Handle,INFINITE)=WAIT_FAILED then RaiseLastOSError;
        CloseHandle(Actions[i].Handle);
       end;
    end;

end.
