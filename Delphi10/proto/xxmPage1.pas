unit xxmPage1;

interface

uses xxm2;

procedure page1(Context:CxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;

implementation

uses xxmProtoMain;

procedure page1(Context:CxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;
begin
  //Context_Include('head.xxmi',['title',1]);
  Context.SendHTML('<p><a href="test.xxm">test</a><p>');

  Context.SendHTML('<p><b>multipart</b></p>');
  Context.SendHTML('<form action="test.xxm?getpar=x" method="post" enctype="multipart/form-data">');
  Context.SendHTML('<input type="checkbox" name="iterate" value="1" /><br />');
  Context.SendHTML('<input type="text" name="test" /><br />');
  Context.SendHTML('<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  Context.SendHTML('<input type="file" name="uploadtest" /><br />');
  Context.SendHTML('<input type="submit" />');
  Context.SendHTML('</form>');

  Context.SendHTML('<p><b>post</b></p>');
  Context.SendHTML('<form action="test.xxm?getpar=x" method="post">');
  Context.SendHTML('<input type="checkbox" name="iterate" value="1" /><br />');
  Context.SendHTML('<input type="text" name="test" /><br />');
  Context.SendHTML('<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  Context.SendHTML('<input type="file" name="uploadtest" /><br />');
  Context.SendHTML('<input type="submit" />');
  Context.SendHTML('</form>');

  Context.SendHTML('<p><b>get</b></p>');
  Context.SendHTML('<form action="test.xxm?getpar=x" method="get">');
  Context.SendHTML('<input type="checkbox" name="iterate" value="1" /><br />');
  Context.SendHTML('<input type="text" name="test" /><br />');
  Context.SendHTML('<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  Context.SendHTML('<input type="file" name="uploadtest" /><br />');
  Context.SendHTML('<input type="submit" />');
  Context.SendHTML('</form>');

  Context.Flush;

  //Context.Include('foot.xxmi');
end;


initialization
  //RegisterPage('/',xxmPage1);
end.
