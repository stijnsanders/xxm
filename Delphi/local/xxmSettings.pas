unit xxmSettings;

interface

implementation

uses
  Windows, SysUtils, Registry, xxmContext, xxmLoader;

procedure XxmReadSettings;
var
  r:TRegistry;

  function ReadBool(const Key:string;Def:boolean):boolean;
  begin
    if r.ValueExists(Key) then Result:=r.ReadBool(Key) else
     begin
      r.WriteBool(Key,Def);
      Result:=Def;
     end;
  end;

  function ReadInt(const Key:string;Def:integer):integer;
  begin
    if r.ValueExists(Key) then Result:=r.ReadInteger(Key) else
     begin
      r.WriteInteger(Key,Def);
      Result:=Def;
     end;
  end;

  function ReadString(const Key,Def:string):string;
  begin
    if r.ValueExists(Key) then Result:=r.ReadString(Key) else
     begin
      r.WriteString(Key,Def);
      Result:=Def;
     end;
  end;

begin
  //load settings
  r:=TRegistry.Create;
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    if r.OpenKey('\Software\xxm\local',true) then
     begin
      StatusException:=ReadInt('StatusException',200);//500);
      StatusBuildError:=ReadInt('StatusBuildError',200);//503);
      StatusFileNotFound:=ReadInt('StatusFileNotFound',200);//404);
      DefaultProjectName:=ReadString('DefaultProject','xxm');
     end;
  finally
    r.Free;
  end;
end;

initialization
  XxmReadSettings;

end.
