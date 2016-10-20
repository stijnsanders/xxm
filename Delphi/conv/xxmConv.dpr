program xxmConv;

{$APPTYPE CONSOLE}
{$DEFINE JSONDOC_STOREINDENTING}

uses
  SysUtils,
  ActiveX,
  Classes,
  xxmConvert1 in 'xxmConvert1.pas',
  xxmUtilities in 'xxmUtilities.pas',
  xxmWebProject in 'xxmWebProject.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmProtoParse in 'xxmProtoParse.pas',
  xxmPageParse in 'xxmPageParse.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmConvert2 in 'xxmConvert2.pas',
  xxmConvertXML in '..\common\xxmConvertXML.pas';

{$R *.res}

var
  i:integer;
  s,protodir,srcdir:string;
  wait,rebuild,docompile,dolinemaps,doupdate:boolean;
  extra:TStringList;
begin
  CoInitialize(nil);
  WelcomeMessage;
  wait:=false;
  rebuild:=false;
  docompile:=true;
  dolinemaps:=true;
  doupdate:=true;
  protodir:='';
  srcdir:='';
  extra:=TStringList.Create;
  try

    if (ParamCount=0) or ((ParamCount=1) and (ParamStr(1)='/?')) then
     begin
      Writeln('Usage: ');
      Writeln('  xxmConv [<options>...] <file or dir>...');
      Writeln('    parses and compiles one or more xxm projects');
      Writeln('    /wait         end with "Press enter to continue" message');
      Writeln('    /rebuild      force processing of all files');
      Writeln('    /nocompile    process files only, don''t compile');
      Writeln('    /nolinemaps   don''t generate line map files');
      Writeln('    /noupdate     don''t update files modified data');
      Writeln('    /proto <dir>  use an alternative unit templates folder');
      Writeln('    /src <dir>    use an alternative source output folder');
      Writeln('    /x:XXX        define template value XXX');
      Writeln('  xxmConv /install');
      Writeln('    registers a context-menu compile option on xxmp file type');
      Writeln('  xxmConv /xmltojson');
      Writeln('    convert xxm.xml to xxm.json');
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
          if s='/xmltojson' then ConvertProjectReg else
          if s='/wait' then wait:=true else
          if s='/rebuild' then rebuild:=true else
          if s='/nocompile' then docompile:=false else
          if s='/nolinemaps' then dolinemaps:=false else
          if s='/noupdate' then doupdate:=false else
          if s='/proto' then
           begin
            inc(i);
            protodir:=IncludeTrailingPathDelimiter(ParamStr(i));
            if not DirectoryExists(protodir) then
             begin
              Writeln('Proto dir not found "'+protodir+'"');
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
            Writeln('Unknown option "'+s+'"');
            Exit;
           end;
         end;
       end
      else
        try
          s:=ExpandFileName(s);
          Writeln('--- '+s);
          with TXxmWebProject.Create(AnsiString(s),DoWrite,true) do
            try
              if protodir<>'' then ProtoFolder:=AnsiString(protodir);
              if srcdir<>'' then SrcFolder:=AnsiString(srcdir);
              LineMaps:=dolinemaps;
              CheckFiles(rebuild,extra);
              if docompile then
               begin
                Compile;
                if not(rebuild) and doupdate then Update;
               end;
            finally
              Free;
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
end.
