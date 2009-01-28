unit xxmParUtils;

interface

uses SysUtils, Classes;

type
  TParamIndex=record
    NameStart,NameLength,ValueStart,ValueLength:integer;
  end;
  TParamIndexes=array of TParamIndex;

  TStreamNozzle=class(TObject)
  private
    FSource: TStream;
    SourceAtEnd: boolean;
    Data: string;
    Size,Index,Done:integer;
    function Ensure(EnsureSize:integer):boolean;
    procedure Flush;
    procedure SkipWhiteSpace;
  public
    constructor Create(Source:TStream);
    procedure CheckBoundary(var Boundary:string);
    function GetHeader(var Params:TParamIndexes):string;
    function GetString(Boundary:string):string;
    procedure GetData(Boundary:string;var Pos:integer;var Len:integer);
    function MultiPartDone:boolean;
  end;

procedure SplitHeader(Value:string; var Params:TParamIndexes);
function SplitHeaderValue(Value:string; var Params:TParamIndexes):string;
function GetParamValue(Data:string; Params:TParamIndexes; Name:string):string;

implementation

procedure SplitHeader(Value:string; var Params:TParamIndexes);
var
  b:boolean;
  p,q,l,r,i:integer;
begin
  q:=1;
  i:=0;
  l:=Length(Value);
  while (q<=l) do
   begin
    p:=q;
    b:=false;
    while (q<=l) and not(b and (Value[q]=#10)) do
     begin
      b:=Value[q]=#13;
      inc(q);
     end;
    inc(q);

    //if not(q-p=2) then
     begin
      SetLength(Params,i+1);
      Params[i].NameStart:=p;
      r:=p;
      while (r<=q) and not(Value[r]=':') do inc(r);
      Params[i].NameLength:=r-p;
      inc(r);
      while (r<=q) and (Value[r] in [#1..#32]) do inc(r);
      Params[i].ValueStart:=r;
      Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
      inc(i);
     end;
   end;
end;

function SplitHeaderValue(Value:string; var Params:TParamIndexes):string;
var
  i,j,l,q:integer;
begin
  l:=Length(Value);
  i:=1;
  while (i<=l) and not(Value[i]=';') do inc(i);
  Result:=Copy(Value,1,i-1);
  if (i<=l) then
   begin
    q:=0;

    while (i<=l) do
     begin
      SetLength(Params,q+1);
      inc(i);
      while (i<=l) and (Value[i] in [#1..#32]) do inc(i);
      Params[q].NameStart:=i;
      j:=i;
      while (j<=l) and not(Value[j]='=') do inc(j);
      Params[q].NameLength:=j-i;
      i:=j+1;
      if (i<=l) and (Value[i]='"') then
       begin
        //in quotes
        inc(i);
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and not(Value[j]='"') do inc(j);
        Params[q].ValueLength:=j-i;
        while (j<=l) and not(Value[j]=';') do inc(j);//ignore
       end
      else
       begin
        //not in quotes
        Params[q].ValueStart:=i;
        j:=i;
        while (j<=l) and not(Value[j]=';') do inc(j);
        Params[q].ValueLength:=j-i;
       end;
      i:=j;
      inc(q);
     end;

   end
  else
    SetLength(Params,0);
end;

function GetParamValue(Data:string; Params:TParamIndexes; Name:string):string;
var
  l,i:integer;
begin
  l:=Length(Params);
  i:=0;
  while (i<l) and not(Copy(Data,Params[i].NameStart,Params[i].NameLength)=Name) do inc(i);//lower?
  if (i<l) then Result:=Copy(Data,Params[i].ValueStart,Params[i].ValueLength) else Result:='';//def?
end;

{ TStreamNozzle }

constructor TStreamNozzle.Create(Source: TStream);
begin
  inherited Create;
  FSource:=Source;
  Size:=0;
  Index:=1;
  Done:=0;
  SourceAtEnd:=false;
end;

function TStreamNozzle.Ensure(EnsureSize: integer):boolean;
var
  i:integer;
const
  GrowStep=$10000;
begin
  //assert EnsureSize<=GrowStep
  if Index+EnsureSize>Size then
   begin
    if SourceAtEnd then Result:=false else
     begin
      i:=GrowStep;
      SetLength(Data,Size+i);
      i:=FSource.Read(Data[Size+1],i);
      inc(Size,i);
      if not(i=GrowStep) then SourceAtEnd:=true;
      Result:=true;//TODO??????????? compare repositories
     end;
   end
  else
    Result:=true;
end;

procedure TStreamNozzle.Flush;
const
  FlushTreshold=$1000;
var
  l:integer;
begin
  if (Index>FlushTreshold) then
   begin
    l:=Size-Index+1;
    Move(Data[Index],Data[1],l);
    SetLength(Data,l);
    Size:=l;
    inc(Done,Index-1);
    Index:=1;
   end;
end;

procedure TStreamNozzle.SkipWhiteSpace;
begin
  //if '--' then multipart done?
  while Ensure(1) and (Data[Index] in [#0..#31]) do inc(Index);
end;

procedure TStreamNozzle.CheckBoundary(var Boundary: string);
var
  bl:integer;
begin
  bl:=Length(Boundary);
  Ensure(bl+5);
  //assert Index=1;
  if not(Copy(Data,3,bl)=Boundary) then
    raise Exception.Create('Multipart data does not start with boundary');
  Index:=bl+3;
  //TODO:detect EOL now?
  SkipWhiteSpace;
  Boundary:=#13#10'--'+Boundary;
  //Flush;?
end;

function TStreamNozzle.GetHeader(var Params: TParamIndexes): string;
const
  GrowStep=$1000;
var
  b:boolean;
  p,q,r,s,i:integer;
begin
  p:=0;
  q:=1;
  i:=0;
  s:=0;
  while Ensure(1) and not(q-p=2) do //2 being Length(EOL)
   begin
    p:=q;
    b:=false;
    while Ensure(1) and not(b and (Data[Index]=#10)) do
     begin
      if q>s then
       begin
        inc(s,GrowStep);
        SetLength(Result,s);
       end;
      Result[q]:=Data[Index];
      b:=Data[Index]=#13;
      inc(Index);
      inc(q);
     end;
    Result[q]:=Data[Index];
    inc(Index);
    inc(q);

    if not(q-p=2) then
     begin
      SetLength(Params,i+1);
      Params[i].NameStart:=p;
      r:=p;
      while (r<=q) and not(Result[r]=':') do inc(r);
      Params[i].NameLength:=r-p;
      inc(r);
      while (r<=q) and (Result[r] in [#1..#32]) do inc(r);
      Params[i].ValueStart:=r;
      Params[i].ValueLength:=q-r-2;//2 from Length(EOL)
      inc(i);
     end;
   end;
  SetLength(Result,q-1);
  Flush;
end;

function TStreamNozzle.GetString(Boundary: string): string;
var
  l,p,q:integer;
begin
  l:=Length(Boundary);
  p:=0;
  q:=Index;
  while Ensure(l) and not(p=l) do
   begin
    p:=0;
    while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
    if not(p=l) then inc(Index);
   end;
  SetLength(Result,Index-q);
  Move(Data[q],Result[1],Index-q);
  inc(Index,l);
  SkipWhiteSpace;
  Flush;
end;

procedure TStreamNozzle.GetData(Boundary: string; var Pos: integer; var Len: integer);
var
  l,p:integer;
begin
  Pos:=Done+Index-1;
  l:=Length(Boundary);
  p:=0;
  while Ensure(l) and not(p=l) do
   begin
    Flush;//depends on flush treshold
    p:=0;
    while (p<l) and (Data[p+Index]=Boundary[p+1]) do inc(p);
    if not(p=l) then inc(Index);
   end;
  Len:=Done+Index-(Pos+1);
  //skip boundary
  inc(Index,l);
  SkipWhiteSpace;
  Flush;
end;

function TStreamNozzle.MultiPartDone: boolean;
begin
  //assert just matched boundary
  Result:=not(Ensure(2)) or ((Data[Index]='-') and (Data[Index+1]='-'));
end;

end.
