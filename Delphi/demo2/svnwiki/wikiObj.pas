unit wikiObj;

interface

uses xxm, Classes, WikiEngine_TLB, Variants, MSXML2_TLB;

const
  WikiParseXML='wikiparse_pmwiki.xml';
  //include trailing (back)slash!
  
  {$DEFINE FromURL}//FromFile}

  {$IFDEF FromURL}
  WikiDataRoot='https://wikiengine.svn.sourceforge.net/svnroot/wikiengine/WikiEngine/Delphi/bin/WikiLocal/';
  WikiDataUser='';
  WikiDataPwd='';
  {$ENDIF}
  
  {$IFDEF FromFile}  
  WikiDataRoot='C:\Data\WikiEngine\bin\WikiLocal\';
  WikiDataSvnEntries=WikiDataRoot+'.svn\entries';
  {$ENDIF}
  
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

procedure GetItemData(fn,fdefault:WideString;var fdata,finfo:WideString);

procedure GetFullList(sl:TStrings);

implementation

uses Windows, SysUtils, VBScript_RegExp_55_TLB;

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

procedure GetItemData(fn,fdefault:WideString;var fdata,finfo:WideString);
var
  {$IFDEF FromURL}
  rq:IXMLHTTPRequest;
  {$ENDIF}
  {$IFDEF FromFile}
  s:string;
  i,j,k,l,m,fl:integer;
  fi:boolean;
  {$ENDIF}
begin
  fdata:=fdefault;
  finfo:='';
  try
	{$IFDEF FromURL}
	rq:=CoXMLHTTP.Create;
	rq.open('GET',WikiDataRoot+FileNameSafe(fn),false,WikiDataUser,WikiDataPwd);
	rq.send(EmptyParam);
	if rq.status in [0,200] then 
	 begin
	  fdata:=rq.responseText;
	  finfo:='rev:'+RevFromETag(rq.getResponseHeader('ETag'))+' '+rq.getResponseHeader('Last-Modified');
	 end;
	{$ENDIF}
	
	{$IFDEF FromFile}
	with TFileStream.Create(WikiDataRoot+FileNameSafe(fn),fmOpenRead) do
	  try
	    l:=Size;
		SetLength(s,l);
		Read(s[1],l);
	  finally
	    Free;
	  end;
	 fdata:=s;
	with TFileStream.Create(WikiDataSvnEntries,fmOpenRead) do
      try
        l:=Size;
		SetLength(s,l);
		Read(s[1],l);
	  finally
	    Free;
	  end;
	i:=1;
    j:=0;
	k:=0;
	fi:=false;
	fl:=Length(fn);
    //find first FF
    while (i<=l) and (s[i]<>#12) do inc(i);
    while (i<=l) and not(fi and (k=11)) do
     begin
      case s[i] of
        #12:k:=0;
        #10:
         begin
		  if fi then
            case k of
             9:finfo:=Copy(s,j,i-j);//last committed date
             10:finfo:='rev:'+Copy(s,j,i-j)+' '+finfo;
            end
		  else
		   if k=1 then
		    begin
			 //if LowerCase(Copy(s,j,i-j))=LowerCase(fn) then
			 m:=0;
			 while (m<fl) and (UpCase(s[j+m])=UpCase(char(fn[1+m]))) do inc(m);
			 if m=fl then fi:=true;
			end;
          inc(k);
         end;
      end;
      inc(i);
      j:=i;
      while (i<=l) and not(s[i] in [#10,#12]) do inc(i);
     end;
	{$ENDIF}
  except
    //silent
  end;
end;

procedure GetFullList(sl:TStrings);
{$IFDEF FromURL}
var
  rq:IXMLHTTPRequest;
  re1:IRegExp2;
  mc1:IMatchCollection2;
  i:integer;
begin
  re1:=CoRegExp.Create;
  re1.Pattern:='<a href="([^"]*?/)?([^"/]+?)\'+WikiDataExt+'">';//WikiDataExt
  re1.Global:=true;
  re1.IgnoreCase:=true;

  rq:=CoXMLHTTP.Create;
  try
    //get directory listing!
    rq.open('GET',WikiDataRoot,false,WikiDataUser,WikiDataPwd);
    rq.send(EmptyParam);
    if rq.status in [0,200] then
	 begin
	  mc1:=re1.Execute(rq.responseText) as IMatchCollection2;
	  for i:=0 to mc1.Count-1 do
		sl.Add(((mc1[i] as IMatch2).SubMatches as ISubMatches)[1]);//TODO: HTMLDecode?
	 end;
  except
    //silent
  end;
end;
{$ENDIF}
{$IFDEF FromFile}
var
  fd:TWin32FindData;
  fh:THandle;
  fn:string;
begin
  fh:=FindFirstFile(PChar(WikiDataRoot+'*'+WikiDataExt),fd);
  if fh<>INVALID_HANDLE_VALUE then
   begin
    repeat
	  fn:=fd.cFileName;
	  sl.Add(Copy(fn,1,Length(fn)-Length(WikiDataExt)));
	until not FindNextFile(fh,fd);
    Windows.FindClose(fh);
   end;
end;
{$ENDIF}

{ TWikiViewCheck }

constructor TWikiViewCheck.Create;
begin
  inherited Create;
  {$IFDEF FromURL}
  FReq:=CoXMLHTTP.Create;
  {$ENDIF}
end;

function TWikiViewCheck.CheckPage(var Name: WideString;
  const CurrentGroup: WideString): WordBool;
begin
  {$IFDEF FromURL}
  FReq.open('HEAD',WikiDataRoot+FileNameSafe(Name+WikiDataExt),false,WikiDataUser,WikiDataPwd);
  try
    FReq.send(EmptyParam);
    Result:=FReq.status in [0,200];
  except
    Result:=false;
  end;
  {$ENDIF}
  {$IFDEF FromFile}
  Result:=FileExists(WikiDataRoot+FileNameSafe(Name+WikiDataExt));
  {$ENDIF}
end;

end.
