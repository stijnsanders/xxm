unit xxmConvert2;

interface

//TRANSITIONAL
procedure ConvertProjectReg;

implementation

uses SysUtils, Classes, Variants, MSXML2_TLB, jsonDoc;

const
  Utf8ByteOrderMark=#$EF#$BB#$BF;

procedure ConvertProjectReg;
var
  doc:DOMDocument;
  x,y:IXMLDOMElement;
  xl:IXMLDOMNodeList;
  rj,pl,p:IJSONDocument;
  v:OleVariant;
  w:WideString;
  s:UTF8String;
  f:TFileStream;
begin
  if FileExists('xxm.json') then
    raise Exception.Create('xxm.json detected, not overwriting');
  doc:=CoDOMDocument.Create;
  doc.async:=false;
  if not doc.load('xxm.xml') then
    raise Exception.Create(doc.parseError.reason);

  pl:=JSON;
  rj:=JSON(['projects',pl]);

  v:=doc.documentElement.getAttribute('DefaultProject');
  if not(VarIsNull(v)) then rj['defaultProject']:=v;

  v:=doc.documentElement.getAttribute('SingleProject');
  if not(VarIsNull(v)) then rj['singleProject']:=v;

  xl:=doc.documentElement.selectNodes('Project');
  x:=xl.nextNode as IXMLDOMElement;
  while (x<>nil) do
   begin
    w:=VarToWideStr(x.getAttribute('Name'));
    p:=JSON(pl[w]);
    if p=nil then
     begin
      p:=JSON;
      pl[w]:=p;
     end;

    v:=x.getAttribute('Alias');
    if not(VarIsNull(v)) then p['alias']:=v;

    y:=x.selectSingleNode('ModulePath') as IXMLDOMElement;
    if y<>nil then p['path']:=
      StringReplace(y.text,PathDelim,'/',[rfReplaceAll]);

    v:=x.getAttribute('LoadCopy');
    if not(VarIsNull(v)) then p['loadCopy']:=VarToStr(v)<>'0';

    v:=x.getAttribute('AllowInclude');
    if not(VarIsNull(v)) then p['allowInclude']:=VarToStr(v)<>'0';

    v:=x.getAttribute('Signature');
    if not(VarIsNull(v)) then p['signature']:=v;
    
    v:=x.getAttribute('BufferSize');
    if not(VarIsNull(v)) then p['bufferSize']:=v;

    x:=xl.nextNode as IXMLDOMElement;
   end;

  s:=Utf8ByteOrderMark+UTF8Encode(rj.ToString);
  f:=TFileStream.Create('xxm.json',fmCreate);
  try
    f.Write(s[1],Length(s));
  finally
    f.Free;
  end;
end;

end.
