unit FRegRouting;

interface

uses xxm2;

procedure RegisterPage(const Address: UTF8String; Builder: CXxmFragment);
procedure RegisterFragment(const Address: UTF8String; Builder: CXxmFragment);
function GetPageAndParameters(Context: CXxmContext; Address: PUTF8Char): CXxmFragment;
function GetIncludeFragment(Address: PUTF8Char): CXxmFragment;

implementation

uses SysUtils, Classes;

var
  Pages,Fragments:TStringList;

procedure RegisterPage(const Address: UTF8String; Builder: CXxmFragment);
begin
  Pages.AddObject(string(Address),@Builder);
end;

procedure RegisterFragment(const Address: UTF8String; Builder: CXxmFragment);
begin
  Fragments.AddObject(string(Address),@Builder);
end;

function GetPageAndParameters(Context: CXxmContext; Address: PUTF8Char): CXxmFragment;
var
  a,s:UTF8String;
  i,j,l:integer;
begin
  //first handle specific exceptions
  a:=Address;
  if a='readme.html' then
   begin
    //no fragment, let xxm pass the static file
    Result:=nil;
   end
  else
  if Copy(a,1,6)='files/' then
   begin
    //no fragment, let xxm pass the static file
    Result:=nil;
   end
  else
   begin
    //pick up the first part before any slash
    l:=Length(a);
    i:=1;
    while (i<=l) and (a[i]<>'/') do inc(i);
    s:=Copy(a,1,i-1);
    //is there a page by that name?
    j:=Pages.IndexOf(string(s)+'.xxm');
    if j=-1 then
     begin
      //none found? use the default page with an action parameter
      xxm.Context_Add_Parameter(Context,'PATH','action',PUTF8Char(s));
      j:=Pages.IndexOf('Default.xxm');
      //assert j<>-1
     end;
    Result:=CXxmFragment(Pages.Objects[j]);
    //extract any parameters in directory format: /one/two/three
    while i<l do
     begin
      inc(i);
      j:=i;
      while (i<=l) and (a[i]<>'/') do inc(i);
      s:=Copy(a,j,i-j);
      //detect specific forms
      if (s<>'') and (s[1] in ['0'..'9']) then
       begin
        //it's a number
        xxm.Context_Add_Parameter(Context,'PATH','number',PUTF8Char(s));
       end
      else if (s<>'') and (s[1]='x') then
       begin
        //prefix "x"
        xxm.Context_Add_Parameter(Context,'PATH','x',PUTF8Char(Copy(s,2,Length(s)-1)));
       end
      else
       begin
        //pick up next bit as the value: /param/value
        inc(i);
        j:=i;
        while (i<=l) and (a[i]<>'/') do inc(i);
        xxm.Context_Add_Parameter(Context,'PATH',PUTF8Char(s),PUTF8Char(Copy(a,j,i-j)));
       end;
     end;
   end;
end;

function GetIncludeFragment(Address: PUTF8Char): CXxmFragment;
var
  i:integer;
begin
  i:=Fragments.IndexOf(string(Address));
  if i=-1 then Result:=nil else
    Result:=CXxmFragment(Fragments.Objects[i]);
end;

initialization
  Pages:=TStringList.Create;
  Pages.Sorted:=true;
  Pages.Duplicates:=dupIgnore;//dupError?setting?
  Pages.CaseSensitive:=false;//setting?
  Fragments:=TStringList.Create;
  Fragments.Sorted:=true;
  Fragments.Duplicates:=dupIgnore;//dupError?setting?
  Fragments.CaseSensitive:=false;//setting?
finalization
  Pages.Free;
  Fragments.Free;

end.
