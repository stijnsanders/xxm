program xxmConv;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  ActiveX,
  xxmConvert1 in 'xxmConvert1.pas',
  xxmUtilities in 'xxmUtilities.pas',
  xxmWebProject in 'xxmWebProject.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas',
  xxmProtoParse in 'xxmProtoParse.pas',
  xxmPageParse in 'xxmPageParse.pas';

{$R *.res}

var
  i:integer;
  s,protodir,srcdir:string;
  wait,rebuild,docompile:boolean;
begin
  CoInitialize(nil);
  WelcomeMessage;
  wait:=false;
  rebuild:=false;
  docompile:=true;
  protodir:='';
  srcdir:='';

  if ParamCount=0 then
   begin
    Writeln('Usage: xxmConv <file or dir>');
    Writeln('Usage: xxmConv /install');
    Writeln('  registers a compile option on xxmp file type');
   end;

  i:=1;
  while i<=ParamCount do
   begin
    s:=ParamStr(i);
    if LowerCase(s)='/install' then RegisterCompileOption else
    if LowerCase(s)='/wait' then wait:=true else
    if LowerCase(s)='/rebuild' then rebuild:=true else
    if LowerCase(s)='/nocompile' then docompile:=false else
    if LowerCase(s)='/proto' then
     begin
      inc(i);
      protodir:=ParamStr(i);
     end
    else
    if LowerCase(s)='/src' then
     begin
      inc(i);
      srcdir:=ParamStr(i);
     end
    else
      try
        s:=ExpandFileName(s);
        Writeln('--- '+s);
        with TXxmWebProject.Create(s,DoWrite,true) do
          try
            if protodir<>'' then ProtoFolder:=protodir;
            if srcdir<>'' then SrcFolder:=srcdir;
            CheckFiles(rebuild);
            if docompile then
             begin
              Compile;
              Update;
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

  if wait then
   begin
    Write('Press enter to continue...');
    Readln;
   end;

  //setting for ExpandUNCFileName()?
end.
