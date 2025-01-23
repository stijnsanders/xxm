program xxmConv;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  jsonDoc in '..\common\jsonDoc.pas',
  xxmDefs in 'xxmDefs.pas',
  xxmConvert1 in 'xxmConvert1.pas',
  xxmProject1 in 'xxmProject1.pas',
  xxmPageParse in 'xxmPageParse.pas',
  xxmProtoParse in 'xxmProtoParse.pas',
  xxmTools in '..\common\xxmTools.pas',
  xxHash in 'xxHash.pas',
  xxm2 in '..\include\xxm2.pas',
  xxmPReg in 'xxmPReg.pas';

begin
  xxmConvertProject;
end.
