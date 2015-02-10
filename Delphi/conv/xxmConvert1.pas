unit xxmConvert1;

interface

procedure WelcomeMessage;
procedure RegisterCompileOption;
procedure DoWrite(Msg:AnsiString);

implementation

uses Windows, SysUtils, Classes, Registry, xxmUtilities, xxmWebProject;

procedure WelcomeMessage;
const
  dSize=$1000;
var
  d:array[0..dSize-1] of byte;
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PAnsiChar;
  r:TResourceStream;
  s:AnsiString;
begin
  r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
  try
    r.Read(d[0],dSize);
  finally
    r.Free;
  end;
  if VerQueryValueA(@d[0],'\',pointer(verblock),verlen) then
    s:=
      IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
      IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
      IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
      IntToStr(LoWord(verblock.dwFileVersionLS))
  else
    s:='v???';
  if VerQueryValueA(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
    s:=p+' '+s;
  Writeln(s);
end;

procedure RegisterCompileOption;
var
  r:TRegistry;
begin
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_CLASSES_ROOT;
    r.OpenKey('xxmpfile\Shell\Compile',true);
    r.WriteString('','Compile');
    r.CloseKey;
    r.OpenKey('xxmpfile\Shell\Compile\command',true);
    r.WriteString('','"'+ParamStr(0)+'" /wait "%l"');
    r.CloseKey;
  finally
    r.Free;
  end;
  Writeln('Compile option registered on xxmp filetype');
end;

procedure DoWrite(Msg:AnsiString);
begin
  Write(Msg);//stdout
end;

end.
