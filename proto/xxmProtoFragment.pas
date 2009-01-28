unit xxmProtoFragment;

interface

uses xxm;

type
  TXxmFragment1=class(TXxmPage)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant;
      const Objects: array of TObject); override;
  end;

implementation

uses SysUtils, xxmFReg, xxmString;

{ TXxmFragment1 }

procedure TXxmFragment1.Build(const Context: IXxmContext;
  const Caller: IXxmFragment;  const Values:array of OleVariant;
  const Objects: array of TObject);
var
  s:string;
  w:WideString;
  i:integer;
  p,q:IXxmParameter;
  f:IxxmParameterPostFile;
begin
  inherited;
  Context.SendHTML('Hello world<br>'+
    DateTimeToStr(Now)+'<br>'+
    Context.ContextString(csVersion));

  Context.SendHTML('<p><a href="default.xxm"><img src="http://yoy.be/yoy_bg.png"></a></p>');

  s:=Context.ContextString(csQueryString);
  Context.SendHTML('<p><b>[</b>');
  w:=URLDecode(StringReplace(s,'&','&amp;',[rfReplaceAll]));
  Context.SendHTML(w);
  Context.SendHTML('<b>]</b></p>');

  if Context.PostData=nil then
    Context.SendHTML('<p><b>no post data</b></p>')
  else
   begin
    Context.Send(Context.ContextString(csPostMimeType));
    Context.SendHTML('<br /><i>');
    Context.Send(Context.PostData.ClassName);
    Context.SendHTML('</i>---');
    Context.SendHTML('<br /><xmp style="border: 2px solid red;">');
    Context.SendStream(Context.PostData);
    Context.SendHTML('</xmp>');
   end;

  Context.SendHTML('<p style="border: 2px solid blue;">');
  Context.Send(Context['uploadtest'].Value);
  Context.SendHTML('</p>');

  for i:=0 to Context.ParameterCount-1 do
   begin
    try
      p:=Context.Parameter[i];
      s:='?';
      if p.QueryInterface(IID_IXxmParameterGet,q)=S_OK then s:='GET';
      if p.QueryInterface(IID_IXxmParameterPost,q)=S_OK then s:='POST';
      if p.QueryInterface(IID_IXxmParameterPostFile,q)=S_OK then s:='FILE';
      Context.SendHTML('<p><b>'+s+'</b>: '+ p.Name +'</p>');
    except
      on e:Exception do
        Context.SendHTML('<p style="color: red;">'+HTMLEncode(e.Message)+'</p>');
    end;
    p:=nil;
    q:=nil;
   end;

  p:=Context.Parameter['uploadtest'];
  if p.QueryInterface(IID_IXxmParameterPostFile,f)=S_OK then
    f.SaveToFile('C:\temp\temp.dat');

  with TStringContext.Create(Context,Self) do
    try
      Include('foot.xxmi');
      Context.SendHTML('<p style="font-size: 2em;">'+IntToStr(Length(Result))+'</p>');
    finally
      Free;
    end;

end;

initialization
  XxmFragmentRegistry.RegisterClass('test.xxm',TXxmFragment1);

end.
