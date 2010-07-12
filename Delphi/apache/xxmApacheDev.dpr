library xxmApacheDev;

{$R 'xxmData.res' 'xxmData.rc'}
{$R 'xxmDataDev.res' 'xxmDataDev.rc'}

uses
  HTTPD2 in 'HTTPD2.pas',
  xxm in '..\public\xxm.pas',
  xxmHeaders in '..\public\xxmHeaders.pas',
  xxmParUtils in '..\common\xxmParUtils.pas',
  xxmParams in '..\common\xxmParams.pas',
  xxmApacheModule in 'xxmApacheModule.pas',
  xxmApacheContext in 'xxmApacheContext.pas',
  xxmCommonUtils in '..\common\xxmCommonUtils.pas',
  xxmPReg in '..\common\xxmPReg.pas',
  xxmHttpPReg in '..\http\xxmHttpPReg.pas',
  xxmApacheClientStream in 'xxmApacheClientStream.pas',
  xxmApachePars in 'xxmApachePars.pas',
  xxmContext in '..\common\xxmContext.pas',
  xxmAutoBuild in '..\common\xxmAutoBuild.pas',
  xxmWebProject in '..\conv\xxmWebProject.pas',
  xxmPageParse in '..\conv\xxmPageParse.pas',
  xxmProtoParse in '..\conv\xxmProtoParse.pas',
  xxmUtilities in '..\conv\xxmUtilities.pas',
  MSXML2_TLB in '..\common\MSXML2_TLB.pas';

{$R *.RES}

begin
  XxmAutoBuildHandler:=AutoBuild;
end.
