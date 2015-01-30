program xxmHttpQuit;

uses
  Windows,
  SysUtils,
  TLHelp32;

{$APPTYPE CONSOLE}

const
  WM_QUIT=$0012;
var
  h:THandle;
  p:TProcessEntry32;
  t:TThreadEntry32;
  i:cardinal;
  b:boolean;
begin
  try
    //TODO: /all /silent ?
    b:=false;//default
    h:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS or TH32CS_SNAPTHREAD,0);
    if h=INVALID_HANDLE_VALUE then RaiseLastOSError;
    p.dwSize:=SizeOf(TProcessEntry32);
    t.dwSize:=SizeOf(TThreadEntry32);
    i:=GetCurrentProcessId;
    if Process32First(h,p) then
      repeat
        if (p.th32ProcessID<>i) and
          (LowerCase(Copy(p.szExeFile,1,7))='xxmhttp') then
         begin
          b:=false;
          if Thread32First(h,t) then
            repeat
              if t.th32OwnerProcessID=p.th32ProcessID then
                if PostThreadMessage(t.th32ThreadID,WM_QUIT,0,0) then
                  b:=true
                else
                  RaiseLastOSError;
            until b or not(Thread32Next(h,t))
          else
            RaiseLastOSError;
         end;
      until not(Process32Next(h,p))
    else
      RaiseLastOSError;
    CloseHandle(h);
    if b then ExitCode:=0 else ExitCode:=1;
  except
    on e:Exception do
     begin
      ExitCode:=1;
      WriteLn(ErrOutput,e.ClassName+': '+e.Message);
     end;
  end;
end.
