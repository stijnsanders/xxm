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

uses SysUtils, xxmFReg, xxmString, xxmHeaders;

{ TXxmFragment1 }

procedure TXxmFragment1.Build(const Context: IXxmContext;
  const Caller: IXxmFragment;  const Values:array of OleVariant;
  const Objects: array of TObject);
var
  s:AnsiString;
  w:WideString;
  i,j:integer;
  p,q:IXxmParameter;
  f:IxxmParameterPostFile;
  x:IxxmDictionaryEx;
  y:IxxmDictionary;
begin
  inherited;
  Context.SendHTML('Hello world<br>'+
    DateTimeToStr(Now)+'<br>'+
    Context.ContextString(csVersion)+'<br>'+
    Context.URL);

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
    Context.SendHTML('<br />---');
    //Context.PostData.ClassName ? no longer available since TStream changed to IStream
    Context.SendHTML('<br /><xmp style="border: 2px solid red;">');
    Context.SendStream(Context.PostData);
    Context.SendHTML('</xmp>');
   end;

  Context.SendHTML('<p style="border: 2px solid blue;">');
  Context.Send(Context['uploadtest'].Value);
  Context.SendHTML('</p>');

  Context.SendHTML('<p><u>Parameters</u></p>');

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

  Context.SendHTML('<p><u>RequestHeaders</u></p>');

  x:=(Context as IxxmHttpHeaders).RequestHeaders;
  for i:=0 to x.Count-1 do
    try
      s:=x.Complex(i,y);
      Context.SendHTML('<p><b>'+x.Name[i]+'</b>: '+s+'</p>');
      if y<>nil then
       begin
        Context.SendHTML('<ul>');
        for j:=0 to y.Count-1 do
          Context.SendHTML('<li><b>'+y.Name[j]+'</b>: '+y[j]+'</li>');
        Context.SendHTML('</ul>');
       end;
    except
      on e:Exception do
        Context.SendHTML('<p style="color: red;">'+HTMLEncode(e.Message)+'</p>');
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
