library xxmApache;

{$R 'xxmData.res' 'xxmData.rc'}

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
  xxmApachePars in 'xxmApachePars.pas';

{$R *.RES}

end.
