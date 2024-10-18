unit xxmPage1;

interface

uses xxm2;

procedure page1(Context:PxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;

implementation

uses xxmProtoMain;

procedure page1(Context:PxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;
begin
  //Context_Include('head.xxmi',['title',1]);
  xxm.Context_SendHTML(Context,'<p><a href="test.xxm">test</a><p>');

  xxm.Context_SendHTML(Context,'<p><b>multipart</b></p>');
  xxm.Context_SendHTML(Context,'<form action="test.xxm?getpar=x" method="post" enctype="multipart/form-data">');
  xxm.Context_SendHTML(Context,'<input type="checkbox" name="iterate" value="1" /><br />');
  xxm.Context_SendHTML(Context,'<input type="text" name="test" /><br />');
  xxm.Context_SendHTML(Context,'<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  xxm.Context_SendHTML(Context,'<input type="file" name="uploadtest" /><br />');
  xxm.Context_SendHTML(Context,'<input type="submit" />');
  xxm.Context_SendHTML(Context,'</form>');

  xxm.Context_SendHTML(Context,'<p><b>post</b></p>');
  xxm.Context_SendHTML(Context,'<form action="test.xxm?getpar=x" method="post">');
  xxm.Context_SendHTML(Context,'<input type="checkbox" name="iterate" value="1" /><br />');
  xxm.Context_SendHTML(Context,'<input type="text" name="test" /><br />');
  xxm.Context_SendHTML(Context,'<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  xxm.Context_SendHTML(Context,'<input type="file" name="uploadtest" /><br />');
  xxm.Context_SendHTML(Context,'<input type="submit" />');
  xxm.Context_SendHTML(Context,'</form>');

  xxm.Context_SendHTML(Context,'<p><b>get</b></p>');
  xxm.Context_SendHTML(Context,'<form action="test.xxm?getpar=x" method="get">');
  xxm.Context_SendHTML(Context,'<input type="checkbox" name="iterate" value="1" /><br />');
  xxm.Context_SendHTML(Context,'<input type="text" name="test" /><br />');
  xxm.Context_SendHTML(Context,'<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  xxm.Context_SendHTML(Context,'<input type="file" name="uploadtest" /><br />');
  xxm.Context_SendHTML(Context,'<input type="submit" />');
  xxm.Context_SendHTML(Context,'</form>');

  xxm.Context_Flush(Context);

  //Context_Include('foot.xxmi');
end;


initialization
  //RegisterPage('/',xxmPage1);
end.
