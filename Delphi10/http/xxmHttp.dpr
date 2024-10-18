program xxmHttp;

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
  xxmSChannel in 'xxmSChannel.pas';

{$R *.res}

begin
  XxmRunServer;
end.
