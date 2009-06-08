unit xxmSettings;

interface

implementation

uses
  Windows, SysUtils, Registry, xxmGeckoChannel;

procedure XxmReadSettings;
var
  r:TRegistry;

  function ReadBool(Key:string;Def:boolean):boolean;
  begin
    if r.ValueExists(Key) then Result:=r.ReadBool(Key) else
     begin
      r.WriteBool(Key,Def);
      Result:=Def;
     end;
  end;

  function ReadInt(Key:string;Def:integer):integer;
  begin
    if r.ValueExists(Key) then Result:=r.ReadInteger(Key) else
     begin
      r.WriteInteger(Key,Def);
      Result:=Def;
     end;
  end;

  function ReadString(Key,Def:string):string;
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
    if r.OpenKey('\Software\xxm\gecko',true) then
     begin
      StatusException:=ReadInt('StatusException',500);
      StatusBuildError:=ReadInt('StatusBuildError',503);
      StatusFileNotFound:=ReadInt('StatusFileNotFound',404);
      DefaultProjectName:=ReadString('DefaultProject','xxm');
      //TODO: more settings?
     end;
  finally
    r.Free;
  end;
end;

initialization
  XxmReadSettings;

end.
