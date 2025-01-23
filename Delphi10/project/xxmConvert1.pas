unit xxmConvert1;

interface

procedure xxmConvertProject;

procedure WelcomeMessage;
procedure RegisterCompileOption;

implementation

uses Winapi.Windows, System.SysUtils, System.Classes, xxmProject1,
  System.Win.Registry, xxmPReg;

procedure DoWrite(Subject:TObject;const Msg:AnsiString);
begin
  Write(Msg);//stdout
end;

function GetFolder(var i:integer;const CheckMsg:string):string;
begin
  inc(i);
  Result:=IncludeTrailingPathDelimiter(ParamStr(i));
  if (CheckMsg<>'') and not(DirectoryExists(Result)) then
    raise Exception.Create(CheckMsg+' not found "'+Result+'"');
end;

function GetFile(var i:integer;const CheckMsg:string):string;
begin
  inc(i);
  Result:=ParamStr(i);
  if (CheckMsg<>'') and not(FileExists(Result)) then
    raise Exception.Create(CheckMsg+' not found "'+Result+'"');
end;

procedure xxmConvertProject;
var
  i:integer;
  s,protodir,protodef,srcdir,handlerdir:string;
  wait,rebuild,docompile,dolinemaps,doupdate:boolean;
  p:TXxmProject;
  extra:TStringList;
begin
  WelcomeMessage;

  if (ParamCount=0) or ((ParamCount=1) and (ParamStr(1)='/?')) then
   begin
    Writeln('Usage: ');
    Writeln('  xxmConv [<options>...] <file or dir>...');
    Writeln('    parses and compiles one or more xxm projects');
    Writeln('    /wait           end with "Press enter to continue" message');
    Writeln('    /rebuild        force processing of all files');
    Writeln('    /nocompile      process files only, don''t compile');
    Writeln('    /nolinemaps     don''t generate line map files');
    Writeln('    /noupdate       don''t update files modified data');
    Writeln('    /proto <dir>    use an alternative unit templates folder');
    Writeln('    /protodef <dif> use this default templates folder');
    Writeln('    /src <dir>      use an alternative source output folder');
    Writeln('    /handler <dir>  use an alternative handler path');
    Writeln('    /reg <file>     use project registry overrides');
    Writeln('    /x:XXX          define template value XXX');
    Writeln('  xxmConv /install');
    Writeln('    registers a context-menu compile option on xxmp file type');
    Exit;
   end;

  wait:=false;
  rebuild:=false;
  docompile:=true;
  dolinemaps:=true;
  doupdate:=true;
  protodir:='';
  protodef:='';
  srcdir:='';
  handlerdir:='';
  extra:=TStringList.Create;
  try
    i:=1;
    while i<=ParamCount do
     begin
      s:=ParamStr(i);
      if (s<>'') and (s[1]='/') then
       begin
        if LowerCase(Copy(s,1,3))='/x:' then
         begin
          inc(i);
          extra.Add(Copy(s,4,Length(s)-3)+'='+ParamStr(i));
         end
        else
         begin
          s:=LowerCase(s);
          if s='/install' then RegisterCompileOption else
          if s='/wait' then wait:=true else
          if s='/rebuild' then rebuild:=true else
          if s='/nocompile' then docompile:=false else
          if s='/nolinemaps' then dolinemaps:=false else
          if s='/noupdate' then doupdate:=false else
          if s='/proto' then protodir:=GetFolder(i,'Proto path') else
          if s='/protodef' then protodef:=GetFolder(i,'Default Proto path') else
          if s='/src' then srcdir:=GetFolder(i,'') else //CheckFiles calls ForceDirectories
          if s='/handler' then handlerdir:=GetFolder(i,'') else
          if s='/reg' then XxmProjectRegDataFilePath:=GetFile(i,'Registry data file') else
           begin
            Writeln('Unknown option "'+s+'"');
            Exit;
           end;
         end;
       end
      else
        try
          s:=ExpandFileName(s);
          Writeln('--- '+s);
          p:=TXxmProject.Create(nil,s,handlerdir,protodef,DoWrite,true);
          try
            if protodir<>'' then p.ProtoFolder:=protodir;
            if srcdir<>'' then p.SrcFolder:=srcdir;
            if handlerdir<>'' then p.HandlerPath:=handlerdir;            
            p.LineMaps:=dolinemaps;
            p.CheckFiles(rebuild,extra);
            if docompile then
             begin
              p.Compile;
              if not(rebuild) and doupdate then p.Update;
             end;
          finally
            p.Free;
          end;
        except
          on e:Exception do
           begin
            Writeln('ERROR ('+e.ClassName+')');
            Writeln(e.Message);
           end;
        end;
      inc(i);
     end;

    //setting for ExpandUNCFileName()?
  except
    on e:Exception do
     begin
      Writeln(ErrOutput,'###['+e.ClassName+']'+e.Message);
      ExitCode:=1;
     end;
  end;
  if wait then
   begin
    Write('Press enter to continue...');
    Readln;
   end;
end;

procedure WelcomeMessage;
const
  dSize=$1000;
var
  d:array[0..dSize-1] of byte;
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PChar;
  r:TResourceStream;
  s:string;
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
  if VerQueryValue(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
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

end.

