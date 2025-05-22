program xxmHttpDev;

uses
  SysUtils,
  xxm2 in '..\include\xxm2.pas',
  xxmTools in '..\common\xxmTools.pas',
  xxmSock in 'xxmSock.pas',
  xxmThreadPool in 'xxmThreadPool.pas',
  jsonDoc in '..\common\jsonDoc.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHttpRun in 'xxmHttpRun.pas',
  xxmContext in 'xxmContext.pas',
  xxmSChannel in 'xxmSChannel.pas',
  xxmAutoCompile in '..\project\xxmAutoCompile.pas',
  xxmProject1 in '..\project\xxmProject1.pas',
  xxmPageParse in '..\project\xxmPageParse.pas',
  xxmProtoParse in '..\project\xxmProtoParse.pas',
  xxHash in '..\project\xxHash.pas',
  xxmDefs in '..\project\xxmDefs.pas',
  xxmStores in 'xxmStores.pas';

{$R *.res}

begin
  XxmProjectCheckHandler:=ProjectCheck_AutoCompile;
  XxmRunServer;
end.
