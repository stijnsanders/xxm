unit wikiObj;

interface

uses xxm, Contnrs, WikiEngine_TLB, Variants, MSXML2_TLB;

const
  WikiParseXML='wikiparse_pmwiki.xml';
  //include trailing (back)slash!
  WikiDataRoot='https://wikiengine.svn.sourceforge.net/svnroot/wikiengine/WikiEngine/Delphi/bin/WikiLocal/';
  WikiDataUser='';
  WikiDataPwd='';
  WikiDataExt='.wx';
  WikiDataExtLinks='.wxa';
  WikiDataExtBackLinks='.wxb';
  WikiSideBarSuffix='.SideBar';

threadvar
  MainWikiEngine:IEngine;

type
  TWikiViewCheck=class(TInterfacedObject, IWikiPageCheck)
  private
    FReq:IXMLHTTPRequest;
  public
    constructor Create;
    function CheckPage(var Name: WideString; const CurrentGroup: WideString): WordBool; safecall;
  end;

function FileNameSafe(x:string): string;

procedure CheckMainWikiEngine;

function RevFromETag(ETag:WideString):WideString;

implementation

uses Windows, SysUtils;

function FileNameSafe(x:string): string;
var
  i:integer;
begin
  Result:=x;
  for i:=1 to Length(Result) do
    if Result[i] in ['\','/',':','*','?','"','<','>','|'] then Result[i]:='_';
end;

procedure CheckMainWikiEngine;
var
  fn:string;
begin
  if MainWikiEngine=nil then
   begin
    MainWikiEngine:=CoEngine.Create;
    //MainWikiEngine.WikiParseXML:=WikiParseXML;//TODO: get from repository!
	SetLength(fn,$400);
	SetLength(fn,GetModuleFileName(HInstance,PChar(fn),$400));
	fn:=ExtractFilePath(fn);
	if FileExists(fn+WikiParseXML) then fn:='file://'+fn+WikiParseXML else
	  if FileExists(fn+'../'+WikiParseXML) then fn:='file://'+fn+'..\'+WikiParseXML else
	    fn:='file://wikiparse.xml.NOTFOUND';
	MainWikiEngine.WikiParseXML:=fn;
	MainWikiEngine.Groups:=true;
    MainWikiEngine.GroupDelim:='.';
    MainWikiEngine.WikiPageCheck:=TWikiViewCheck.Create as IWikiPageCheck;
   end;
end;

function RevFromETag(ETag:WideString):WideString;
var
  i,j:integer;
begin
  i:=1;
  while (i<=Length(ETag)) and (ETag[i]='"') do inc(i);
  j:=i+1;
  while (j<=Length(ETag)) and not(ETag[j]='/') do inc(j);
  Result:=Copy(ETag,i,j-i);
end;

{ TWikiViewCheck }

constructor TWikiViewCheck.Create;
begin
  inherited Create;
  FReq:=CoXMLHTTP.Create;
end;

function TWikiViewCheck.CheckPage(var Name: WideString;
  const CurrentGroup: WideString): WordBool;
begin
  FReq.open('HEAD',WikiDataRoot+FileNameSafe(Name+WikiDataExt),false,WikiDataUser,WikiDataPwd);
  FReq.send(EmptyParam);
  Result:=FReq.status in [0,200];
end;

end.
