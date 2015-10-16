unit xxmLocalOnly;

interface

function XxmGlobalMutex(const ProjectName: string): boolean;
procedure XxmStartURL;

implementation

uses SysUtils, Windows, ShellAPI, xxmHttpMain;

function XxmGlobalMutex(const ProjectName: string): boolean;
var
  h:THandle;
begin
  h:=CreateMutex(nil,true,PChar('Global\xxm_LocalOnly_'+ProjectName));
  Result:=(h<>0) and (GetLastError<>ERROR_ALREADY_EXISTS);
end;

procedure XxmStartURL;
begin
  ShellExecute(GetDesktopWindow,nil,PChar('http://localhost:'+
    //HttpBindIPv4? HttpBindIPv6?
    IntToStr(HttpListenPort)+'/'),nil,nil,SW_NORMAl);
end;

end.
