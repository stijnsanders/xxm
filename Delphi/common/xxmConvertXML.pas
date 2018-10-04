unit xxmConvertXML;

interface

//TRANSITIONAL:
function ConvertProjectFile(const XmlData: string): string;

implementation

uses SysUtils, Variants, jsonDoc, MSXML2_TLB;

function ConvertProjectFile(const XmlData: string): string;
var
  Data:DOMDocument;
  RootNode:IXMLDOMElement;
  xl:IXMLDOMNodeList;
  x:IXMLDOMElement;
  y:IXMLDOMNode;
  d,d1,d2:IJSONDocument;
  v:OleVariant;
  i:integer;

  procedure ConvertTextNodes(const xName,jName:WideString);
  begin
    xl:=RootNode.selectNodes(xName);
    if xl.length<>0 then
     begin
      x:=xl.nextNode as IXMLDOMElement;
      if xl.length=1 then
        d[jName]:=x.text
      else
       begin
        i:=0;
        v:=VarArrayCreate([0,xl.length-1],varVariant);
        while x<>nil do
         begin
          v[i]:=x.text;
          inc(i);
          x:=xl.nextNode as IXMLDOMElement;
         end;
         d[jName]:=v;
       end;
     end;
  end;

  procedure ConvertCommandNodes(const xName,jName:WideString);
  begin
    xl:=RootNode.selectNodes(xName);
    if xl.length<>0 then
     begin
      x:=xl.nextNode as IXMLDOMElement;
      if xl.length=1 then
        d[jName]:=x.text
      else
       begin
        i:=0;
        v:=VarArrayCreate([0,xl.length-1],varVariant);
        while x<>nil do
         begin
          if VarIsNull(x.getAttribute('Path')) then
            v[i]:=x.text
          else
            v[i]:=VarArrayOf([x.text,x.getAttribute('Path')]);
          inc(i);
          x:=xl.nextNode as IXMLDOMElement;
         end;
         d[jName]:=v;
       end;
     end;
  end;

begin
  Data:=CoDOMDocument.Create;
  Data.async:=false;
  Data.preserveWhiteSpace:=true;
  if not(Data.loadXML(string(XmlData))) then
    {
    raise EXxmWebProjectLoad.Create(StringReplace(
      SXxmWebProjectLoad,'__',FRootFolder+DataFileName,[])+
      #13#10+Data.parseError.reason);
    }
    raise Exception.Create(Data.parseError.reason);
  RootNode:=Data.documentElement;
  //DataStartSize:=Length(Data.xml);

  d:=JSON;

  x:=RootNode.selectSingleNode('ProjectName') as IXMLDOMElement;
  if x<>nil then d['name']:=x.text;
  x:=RootNode.selectSingleNode('UUID') as IXMLDOMElement;
  if x<>nil then d['uuid']:=x.text;

  ConvertTextNodes('UsesClause','usesClause');
  ConvertTextNodes('Header','header');
  ConvertTextNodes('Body','body');
  ConvertTextNodes('Switches','switches');

  ConvertCommandNodes('PreCompileCommand','preCompileCommand');
  ConvertCommandNodes('CompileCommand','compileCommand');
  ConvertCommandNodes('PostCompileCommand','postCompileCommand');

  xl:=RootNode.selectNodes('Files/File');
  x:=xl.nextNode as IXMLDOMElement;
  if x<>nil then
   begin
    d1:=JSON;
    d['files']:=d1;
    while x<>nil do
     begin
      d2:=JSON([
        'path',StringReplace(
          x.selectSingleNode('Path').text,PathDelim,'/',[rfReplaceAll])
      ]);
      if not(VarIsNull(x.getAttribute('UnitName'))) then
        d2['unitName']:=StringReplace(
          x.getAttribute('UnitName'),PathDelim,'/',[rfReplaceAll]);
      if not(VarIsNull(x.getAttribute('UnitPath'))) then
        d2['unitPath']:=StringReplace(
          x.getAttribute('UnitPath'),PathDelim,'/',[rfReplaceAll]);
      d1[x.getAttribute('ID')]:=d2;
      x:=xl.nextNode as IXMLDOMElement;
     end;
   end;

  xl:=RootNode.selectNodes('Files/Unit');
  x:=xl.nextNode as IXMLDOMElement;
  if x<>nil then
   begin
    d1:=JSON;
    d['units']:=d1;
    while x<>nil do
     begin
      d2:=JSON;
      if not(VarIsNull(x.getAttribute('UnitPath'))) then
        d2['unitPath']:=StringReplace(
          x.getAttribute('UnitPath'),PathDelim,'/',[rfReplaceAll]);
      d1[x.getAttribute('UnitName')]:=d2;
      x:=xl.nextNode as IXMLDOMElement;
     end;
   end;

  xl:=RootNode.selectNodes('Files/Resource');
  x:=xl.NextNode as IXMLDOMElement;
  if x<>nil then
   begin
    d1:=JSON;
    d['resources']:=d1;
    while x<>nil do
     begin
      d1[StringReplace(
        x.selectSingleNode('Path').text,PathDelim,'/',[rfReplaceAll])]:=JSON;
      x:=xl.nextNode as IXMLDOMElement;
     end;
   end;

  x:=RootNode.selectSingleNode('ParserValues') as IXMLDOMElement;
  if x<>nil then
   begin
    d1:=JSON;
    d['parserValues']:=d1;
    y:=x.firstChild;
    while y<>nil do
     begin
      if y.QueryInterface(IXMLDOMElement,x)=S_OK then
        d1[x.nodeName]:=x.text;
      y:=y.nextSibling;
     end;
   end;

  x:=RootNode.selectSingleNode('LastModified') as IXMLDOMElement;
  if x<>nil then
     d['lastModified']:=x.text;

  Result:=d.ToString;
end;

end.
