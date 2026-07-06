program xxmConv;

{$APPTYPE CONSOLE}
{$DEFINE JSONDOC_STOREINDENTING}

{$R 'xxmConv_ver.res' 'xxmConv_ver.rc'}

uses
  SysUtils,
  ActiveX,
  Classes,
  xxmConvert1 in 'xxmConvert1.pas',
  xxmUtilities in 'xxmUtilities.pas',
  xxmWebProject in 'xxmWebProject.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmProtoParse in 'xxmProtoParse.pas',
  xxmPageParse in 'xxmPageParse.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas';

{$R *.res}

var
  i:integer;
  s,protodir,srcdir:string;
  wait,rebuild,docompile,dolinemaps,doupdate,welcomed,silent,compileresult:boolean;
  extra:TStringList;
  wp:TXxmWebProject;
begin
  CoInitialize(nil);
  wait:=false;
  rebuild:=false;
  docompile:=true;
  dolinemaps:=true;
  doupdate:=true;
  welcomed:=false;
  silent:=false;
  protodir:='';
  srcdir:='';
  extra:=TStringList.Create;
  try

    if (ParamCount=0) or ((ParamCount=1) and (ParamStr(1)='/?')) then
     begin
      WelcomeMessage;
      Writeln('Usage: ');
      Writeln('  xxmConv [<options>...] <file or dir>...');
      Writeln('    parses and compiles one or more xxm projects');
      Writeln('    /wait         end with "Press enter to continue" message');
      Writeln('    /rebuild      force processing of all files');
      Writeln('    /nocompile    process files only, don''t compile');
      Writeln('    /nolinemaps   don''t generate line map files');
      Writeln('    /noupdate     don''t update files modified data');
      Writeln('    /proto <dir>  use an alternative unit templates folder');
      Writeln('    /silent       only output error messages');
      Writeln('    /src <dir>    use an alternative source output folder');
      Writeln('    /x:XXX        define template value XXX');
      Writeln('  xxmConv /install');
      Writeln('    registers a context-menu compile option on xxmp file type');
     end;

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
          if s='/silent' then silent:=true else
          if s='/proto' then
           begin
            inc(i);
            protodir:=IncludeTrailingPathDelimiter(ParamStr(i));
            if not DirectoryExists(protodir) then
             begin
              Writeln(ErrOutput,'Proto dir not found "'+protodir+'"');
              ExitCode:=9;
              Exit;
             end;
           end
          else
          if s='/src' then
           begin
            inc(i);
            srcdir:=IncludeTrailingPathDelimiter(ParamStr(i));
            //DirectoryExists? CheckFiles calls ForceDirectories
           end
          else
           begin
            Writeln(ErrOutput,'Unknown option "'+s+'"');
            ExitCode:=8;
            Exit;
           end;
         end;
       end
      else
        try
          if not(welcomed) and not(silent) then
           begin
            WelcomeMessage;
            welcomed:=true;
           end;
          s:=ExpandFileName(s);
          if not(silent) then
            Writeln('--- '+s);
          BuildOutput:=TStringStream.Create('');
          wp:=TXxmWebProject.Create(s,DoBuildOutput,true);
          try
            if protodir<>'' then wp.ProtoFolder:=protodir;
            if srcdir<>'' then wp.SrcFolder:=srcdir;
            wp.LineMaps:=dolinemaps;
            wp.CheckFiles(rebuild,extra);
            if docompile then
             begin

              compileresult:=wp.Compile;
              if not(rebuild) and doupdate then wp.Update;

              if not(silent) then
                Write(wp.ResolveErrorLines(BuildOutput.DataString));

              if not(compileresult) then ExitCode:=3;
             end;
          finally
            wp.Free;
            BuildOutput.Free;
          end;
        except
          on e:Exception do
           begin
            Writeln(ErrOutput,'ERROR ('+e.ClassName+')');
            Writeln(ErrOutput,e.Message);
            ExitCode:=11;
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
end.
