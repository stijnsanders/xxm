unit xxmConvert1;

interface

procedure WelcomeMessage;
procedure RegisterCompileOption;
procedure DoWrite(Msg:string);

implementation

uses Windows, SysUtils, Classes, Registry, xxmUtilities, xxmWebProject;

procedure WelcomeMessage;
var
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PChar;
  r:TResourceStream;
  m:TMemoryStream;
  s:string;
begin
  m:=TMemoryStream.Create;
  try
    r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      m.CopyFrom(r,r.Size);
    finally
      r.Free;
    end;
    m.Position:=0;
    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
      s:=
        IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
        IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
        IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
        IntToStr(LoWord(verblock.dwFileVersionLS))
    else
      s:='v???';
    if VerQueryValue(m.Memory,'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
      s:=p+' '+s;
    Writeln(s);
  finally
    m.Free;
  end;
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

procedure DoWrite(Msg:string);
begin
  Write(Msg);//stdout
end;

end.
