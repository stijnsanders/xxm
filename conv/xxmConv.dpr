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
  s:string;
  wait,rebuild:boolean;
begin
  CoInitialize(nil);
  WelcomeMessage;
  wait:=false;
  rebuild:=false;

  if ParamCount=0 then
   begin
    Writeln('Usage: xxmConv <file or dir>');
    Writeln('Usage: xxmConv /install');
    Writeln('  registers a compile option on xxmp file type');
   end;

  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    if LowerCase(s)='/install' then RegisterCompileOption else
    if LowerCase(s)='/wait' then wait:=true else
    if LowerCase(s)='/rebuild' then rebuild:=true else
      try
        s:=ExpandFileName(s);
        Writeln('--- '+s);
        with TXxmWebProject.Create(s,DoWrite,true) do
          try
            CheckFiles(rebuild);
            Compile;
            Update;
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
   end;

  if wait then
   begin
    Write('Press enter to continue...');
    Readln;
   end;

  //setting for ExpandUNCFileName()?
end.
