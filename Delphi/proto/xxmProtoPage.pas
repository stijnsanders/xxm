unit xxmProtoPage;

interface

uses xxm;

type
  TXxmProtoPage1=class(TXxmPage)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values:array of OleVariant;
      const Objects: array of TObject); override;
  end;

implementation

uses xxmFReg;

{ TXxmProtoPage1 }

procedure TXxmProtoPage1.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values:array of OleVariant;
  const Objects: array of TObject);
begin
  inherited;
  Context.Include('head.xxmi',['title',1]);

  Context.SendHTML('<p><a href="test.xxm">test</a><p>');

  Context.SendHTML('<p><b>multipart</b></p>');
  Context.SendHTML('<form action="test.xxm?getpar=x" method="post" enctype="multipart/form-data">');
  Context.SendHTML('<input type="checkbox" name="iterate" value="1" /><br />');
  Context.SendHTML('<input type="text" name="test" /><br />');
  Context.SendHTML('<input type="image" src="http://yoy.be/yoy_bg.png" name="testimg" /><br />');
  Context.SendHTML('<input type="file" multiple name="uploadtest" /><br />');
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

  Context.Include('foot.xxmi');
end;

initialization
  XxmFragmentRegistry.RegisterClass('default.xxm',TXxmProtoPage1);

end.
