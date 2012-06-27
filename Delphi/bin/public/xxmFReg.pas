unit xxmFReg;

interface

uses xxm, Classes;

{

  xxm Fragment Registry

This is a default fragment registry. You are free to change this one  or create a new one for your project.
The TxxmProject (xxmp.pas) calls GetClass with the page section of the URL, or can pre-process the URL.

  $Rev: 220 $ $Date: 2012-06-27 23:14:11 +0200 (wo, 27 jun 2012) $

}

type
  TXxmFragmentClass=class of TXxmFragment;

  TXxmFragmentRegistry=class(TObject)
  private
    Registry:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(FName: AnsiString; FType: TXxmFragmentClass);
    function GetFragment(Project: TxxmProject;
      FName, RelativeTo: AnsiString): IxxmFragment;
  end;

var
  XxmFragmentRegistry:TXxmFragmentRegistry;

const
  XxmDefaultPage:AnsiString='default.xxm';

implementation

uses SysUtils;

{ TXxmFragmentRegistry }

constructor TXxmFragmentRegistry.Create;
begin
  inherited Create;
  Registry:=TStringList.Create;
  Registry.Sorted:=true;
  Registry.Duplicates:=dupIgnore;//dupError?setting?
  Registry.CaseSensitive:=false;//setting?
end;

destructor TXxmFragmentRegistry.Destroy;
begin
  //Registry.Clear;//?
  Registry.Free;
  inherited;
end;

procedure TXxmFragmentRegistry.RegisterClass(FName: AnsiString; FType: TXxmFragmentClass);
begin
  Registry.AddObject(string(FName),TObject(FType));
end;

function TXxmFragmentRegistry.GetFragment(Project: TxxmProject;
  FName, RelativeTo: AnsiString): IxxmFragment;
var
  i,j,l:integer;
  a,b:AnsiString;
  f:TxxmFragment;
begin
  l:=Length(FName);
  if (l<>0) and (FName[1]='/') then
   begin
    //absolute path
    a:=Copy(FName,2,l-1);
   end
  else
   begin
    //expand relative path
    i:=Length(RelativeTo);
    while (i>0) and (RelativeTo[i]<>'/') do dec(i);
    a:=Copy(RelativeTo,1,i);
    i:=1;
    while i<l do
     begin
      j:=i;
      while (i<l) and (FName[i]<>'/') do inc(i);
      inc(i);
      b:=Copy(FName,j,i-j);
      if b='../' then
       begin
        j:=Length(a)-1;
        while (j<>0) and (a[j]<>'/') do dec(j);
        SetLength(a,j);
       end
      else
        if b<>'./' then
          a:=a+b;
     end;
   end;
  //get fragment class
  i:=Registry.IndexOf(a);
  //folder? add index page name
  if i=-1 then
    if (FName='') or (FName[Length(FName)]='/') then 
	    i:=Registry.IndexOf(FName+XxmDefaultPage)
	  else
	    i:=Registry.IndexOf(FName+'/'+XxmDefaultPage);
  if i=-1 then
    Result:=nil
  else
   begin
    f:=TXxmFragmentClass(Registry.Objects[i]).Create(Project);
    //TODO: cache created instance, incease ref count
    f.RelativePath:=a;
    Result:=f;
   end;
end;

initialization
  XxmFragmentRegistry:=TXxmFragmentRegistry.Create;
finalization
  XxmFragmentRegistry.Free;

end.
