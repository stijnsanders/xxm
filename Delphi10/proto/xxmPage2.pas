unit xxmPage2;

interface

uses xxm2;

procedure page2(Context:CxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;

implementation

uses SysUtils, xxmProtoMain, xxmTools;

procedure page2(Context:CxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;
var
  s:string;
  i:integer;
  p:CXxmParameter;
  p1,p2:UTF8String;
begin
  Context.SendHTML('Hello world<br>'+
    UTF8String(DateTimeToStr(Now))+'<br>'+
    Context.ContextString(csVersion)+'<br>'+
    Context.URL);

  Context.SendHTML('<p><a href="."><img src="http://yoy.be/yoy_bg.png"></a></p>');

  s:=string(Context.ContextString(csQueryString));
  Context.SendHTML('<p><b>[</b>');
  Context.SendHTML(PUTF8Char(UTF8Encode(StringReplace(s,'&','&amp;',[rfReplaceAll]))));
  Context.SendHTML('<b>]</b></p>');

  if xxm.Context_PostData(Context)=nil then
    Context.SendHTML('<p><b>no post data</b></p>')
  else
   begin
    Context.Send(Context.ContextString(csPostMimeType));
    Context.SendHTML('<br />---');
    //Context.PostData.ClassName ?
    Context.SendHTML('<br /><xmp style="border: 2px solid red;">');
    Context.SendStream(xxm.Context_PostData(Context));
    Context.SendHTML('</xmp>');
   end;

  Context.SendHTML('<p style="border: 2px solid blue;">');

  p:=Context['uploadtest'];

  Context.Send(p.Value);
  Context.SendHTML('</p>');

  Context.SendHTML('<p><u>Parameters</u></p>');

  for i:=0 to Context.ParameterCount-1 do
   begin
    try
      p:=Context.GetParameter(i);
      s:=string(p.Origin);
      Context.SendHTML(UTF8Encode(
        '<p><b>'+s+'</b>: '+ string(HTMLEncode(p.Name)) +' = '+string(HTMLEncode(p.Value))+'</p>'));
    except
      on e:Exception do
        Context.SendHTML(UTF8Encode(
          '<p style="color: red;">'+HTMLEncode(UTF8Encode(e.Message))+'</p>'));
    end;
   end;

  Context.SendHTML('<p><u>RequestHeaders</u></p>');

  for i:=0 to Context.RequestHeaderCount-1 do
    try
      Context.GetRequestHeader(i,p1,p2);
      Context.SendHTML(PUTF8Char('<p><b>'+HTMLEncode(p1)+'</b>: '+HTMLEncode(p2)+'</p>'));
      {
      if y<>nil then
       begin
        Context.SendHTML('<ul>');
        for j:=0 to y.Count-1 do
          Context.SendHTML('<li><b>'+y.Name[j]+'</b>: '+y[j]+'</li>');
        Context.SendHTML('</ul>');
       end;
       }
    except
      on e:Exception do
        Context.SendHTML(PUTF8Char('<p style="color: red;">'+HTMLEncode(UTF8Encode(e.Message))+'</p>'));
    end;

  p:=Context.Parameter['uploadtest'];
  if p.Origin='FILE' then
    p.SaveToFile('C:\temp\temp.dat');

{//TODO
  with TStringContext.Create(Context,Self) do
    try
      Include('foot.xxmi');
      Context.SendHTML('<p style="font-size: 2em;">'+IntToStr(Length(Result))+'</p>');
    finally
      Free;
    end;
}
end;

end.
