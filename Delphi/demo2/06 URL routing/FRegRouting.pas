unit FRegRouting;

interface

uses xxm, Classes;

type
  TXxmFragmentClass=class of TXxmFragment;

  IParameterFromPath=interface(IxxmParameter)
    ['{C70C5470-1D10-47FE-8DE0-41D293900091}']
    //add properties/methods of your own here.
  end;

  TParameterFromPath=class(TInterfacedObject, IxxmParameter, IParameterFromPath)
  private
    FName,FValue:WideString;
  protected
    function GetName:WideString;
    function GetValue:WideString;
    function AsInteger:integer;
    function NextBySameName:IXxmParameter;
  public
    constructor Create(Name,Value:WideString);
    property Name:WideString read GetName;
    property Value:WideString read GetValue;
  end;

const
  IID_IParameterFromPath:TGUID='{C70C5470-1D10-47FE-8DE0-41D293900091}';

procedure RegisterFragmentClass(FName:AnsiString;FType:TXxmFragmentClass);
function GetIncludeFragment(Project:TXxmProject;Address:WideString):IXxmFragment;
function GetPageAndParameters(Project:TXxmProject;Context:IXxmContext;
  Address:WideString):IXxmFragment;

var
  FragmentRegistry:TStringList;

implementation

uses SysUtils, xxmHeaders;

{ TXxmFragmentRegistry }

procedure RegisterFragmentClass(FName:AnsiString;FType:TXxmFragmentClass);
begin
  FragmentRegistry.AddObject(FName,TObject(FType));
end;

function GetIncludeFragment(Project:TXxmProject;Address:WideString):IXxmFragment;
var
  i:integer;
begin
  i:=FragmentRegistry.IndexOf(Address);
  if i=-1 then Result:=nil else
    Result:=TXxmFragmentClass(FragmentRegistry.Objects[i]).Create(Project) as IXxmFragment;
end;

function GetPageAndParameters(Project:TXxmProject;Context:IXxmContext;
  Address:WideString):IXxmFragment;
var
  i,j,l:integer;
  s:WideString;
  pc:IxxmParameterCollection;
begin
  //first handle exceptions
  if Copy(Address,1,6)='files/' then
   begin
    //no fragment, let xxm pass the static file
    Result:=nil;
   end
  else
   begin
    //need this below to add parameters to the context
    pc:=Context as IxxmParameterCollection;
    //pick up the first part before any slash
    l:=Length(Address);
    i:=1;
    while (i<=l) and (Address[i]<>'/') do inc(i);
    s:=Copy(Address,1,i-1);
    //is there a page by that name?
    j:=FragmentRegistry.IndexOf(s+'.xxm');
    if j=-1 then
     begin
      //none found? use the default page with an action parameter
      pc.AddParameter(TParameterFromPath.Create('action',s) as IXxmParameter);
      j:=FragmentRegistry.IndexOf('Default.xxm');
      //assert j<>-1
     end;
    Result:=TXxmFragmentClass(FragmentRegistry.Objects[j]).Create(Project) as IXxmFragment;
    //extract any parameters in directory format: /one/two/three
    while i<l do
     begin
      inc(i);
      j:=i;
      while (i<=l) and (Address[i]<>'/') do inc(i);
      s:=Copy(Address,j,i-j);
      //detect specific forms
      if (s<>'') and (AnsiChar(s[1]) in ['0'..'9']) then
       begin
        //it's a number
        pc.AddParameter(TParameterFromPath.Create('number',s) as IXxmParameter);
       end
      else if (s<>'') and (s[1]='x') then
       begin
        //prefix "x"
        pc.AddParameter(TParameterFromPath.Create('x',Copy(s,2,Length(s)-1)) as IXxmParameter);
       end
      else
       begin
        //pick up next bit as the value: /param/value
        inc(i);
        j:=i;
        while (i<=l) and (Address[i]<>'/') do inc(i);
        pc.AddParameter(TParameterFromPath.Create(s,Copy(Address,j,i-j)) as IXxmParameter);
       end;
     end;
   end;
end;

{ TParameterFromPath }

constructor TParameterFromPath.Create(Name, Value: WideString);
begin
  inherited Create;
  FName:=Name;
  FValue:=Value;
end;

function TParameterFromPath.GetName: WideString;
begin
  Result:=FName;
end;

function TParameterFromPath.GetValue: WideString;
begin
  Result:=FValue;
end;

function TParameterFromPath.AsInteger: integer;
begin
  Result:=StrToIntDef(FValue,0);
end;

function TParameterFromPath.NextBySameName: IXxmParameter;
begin
  raise Exception.Create('TParameterFromPath.NextBySameName not supported');
end;

initialization
  FragmentRegistry:=TStringList.Create;
  FragmentRegistry.Sorted:=true;
  FragmentRegistry.Duplicates:=dupIgnore;//dupError?setting?
  FragmentRegistry.CaseSensitive:=false;//setting?
finalization
  FragmentRegistry.Free;

end.
