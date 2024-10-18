unit xxmPage2;

interface

uses xxm2;

procedure page2(Context:PxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;

implementation

uses SysUtils, xxmProtoMain, xxmTools;

procedure page2(Context:PxxmContext;
  const Values:array of Variant;const Objects:array of pointer); stdcall;
var
  s:string;
  i:integer;
  p:PXxmParameter;
  p1,p2:PUTF8Char;
begin
  xxm.Context_SendHTML(Context,PUTF8Char('Hello world<br>'+
    UTF8String(DateTimeToStr(Now))+'<br>'+
    UTF8String(xxm.Context_ContextString(Context,csVersion))+'<br>'+
    UTF8String(xxm.Context_URL(Context))));

  xxm.Context_SendHTML(Context,'<p><a href="."><img src="http://yoy.be/yoy_bg.png"></a></p>');

  s:=string(xxm.Context_ContextString(Context,csQueryString));
  xxm.Context_SendHTML(Context,'<p><b>[</b>');
  xxm.Context_SendHTML(Context,PUTF8Char(UTF8Encode(StringReplace(s,'&','&amp;',[rfReplaceAll]))));
  xxm.Context_SendHTML(Context,'<b>]</b></p>');

  if xxm.Context_PostData(Context)=nil then
    xxm.Context_SendHTML(Context,'<p><b>no post data</b></p>')
  else
   begin
    xxm.Context_Send(Context,xxm.Context_ContextString(Context,csPostMimeType));
    xxm.Context_SendHTML(Context,'<br />---');
    //Context.PostData.ClassName ? no longer available since TStream changed to IStream
    xxm.Context_SendHTML(Context,'<br /><xmp style="border: 2px solid red;">');
    xxm.Context_SendStream(Context,xxm.Context_PostData(Context));
    xxm.Context_SendHTML(Context,'</xmp>');
   end;

  xxm.Context_SendHTML(Context,'<p style="border: 2px solid blue;">');

  p:=xxm.Context_Parameter(Context,'uploadtest');

  xxm.Context_Send(Context,xxm.Parameter_Value(p));
  xxm.Context_SendHTML(Context,'</p>');

  xxm.Context_SendHTML(Context,'<p><u>Parameters</u></p>');

  for i:=0 to xxm.Context_ParameterCount(Context)-1 do
   begin
    try
      p:=xxm.Context_ParameterByIdx(Context,i);
      s:=string(xxm.Parameter_Origin(p));
      xxm.Context_SendHTML(Context,PUTF8Char(UTF8Encode(
        '<p><b>'+s+'</b>: '+ string(xxm.Parameter_Name(p)) +' = '+string(HTMLEncode(xxm.Parameter_Value(p)))+'</p>')));
    except
      on e:Exception do
        xxm.Context_SendHTML(Context,PUTF8Char(UTF8Encode(
          '<p style="color: red;">'+HTMLEncode(UTF8Encode(e.Message))+'</p>')));
    end;
   end;

  xxm.Context_SendHTML(Context,'<p><u>RequestHeaders</u></p>');

  for i:=0 to xxm.Context_RequestHeaderCount(Context)-1 do
    try
      xxm.Context_RequestHeaderByIdx(Context,i,p1,p2);
      xxm.Context_SendHTML(Context,PUTF8Char('<p><b>'+HTMLEncode(p1)+'</b>: '+HTMLEncode(p2)+'</p>'));
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
        xxm.Context_SendHTML(Context,PUTF8Char('<p style="color: red;">'+HTMLEncode(UTF8Encode(e.Message))+'</p>'));
    end;

  p:=xxm.Context_Parameter(Context,'uploadtest');
  if xxm.Parameter_Origin(p)='FILE' then
    xxm.Parameter_SaveToFile(p,'C:\temp\temp.dat');

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
