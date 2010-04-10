unit xxmProtoParse;

interface

uses SysUtils, Classes;

type
  TXxmProtoParseTag=(
    ptProjectName,
    ptProjectHeader,
    ptProjectBody,
    ptFragmentUnit,
    ptFragmentPath,
    ptFragmentID,
    ptIncludeUnit,
    ptIncludePath,
    ptProtoFile,
    ptUsesClause,
    ptFragmentDefinitions,
    ptFragmentAddress,
    ptFragmentHeader,
    ptFragmentBody,
    ptFragmentFooter,
    
    //add new here

    pt_Iterations,//=1000,
    ptIterateFragment,
    ptIterateInclude,
    //add new iteration starters here
    ptIterateEnd,

    pt_Unknown
  );

  TXxmProtoParser=class(TObject)
  private
    FData:AnsiString;
    FOutput:TMemoryStream;
    PointsCount,PointsSize:integer;
    Points:array of record
      Index,Length:integer;
      Tag:TXxmProtoParseTag;
    end;
    FIndex:integer;
    StackPosition,StackSize:integer;
    Stack:array of integer;
    procedure AddPoint(Index,Length:integer;Tag:TXxmProtoParseTag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Data:AnsiString);
    function GetNext:TXxmProtoParseTag;
    procedure Output(Data:AnsiString);
    procedure IterateBegin(Condition:boolean);
    procedure IterateNext(Condition:boolean);
    function Done:boolean;
    procedure Save(FilePath:AnsiString);
  end;

  EXxmParseUnknownTag=class(Exception);
  EXxmParseIterateSkipFailed=class(Exception);

implementation

const
  ProtoParseTag:array[TXxmProtoParseTag] of AnsiString=(
    'ProjectName',
    'ProjectHeader',
    'ProjectBody',
    'FragmentUnit',
    'FragmentPath',
    'FragmentID',
    'IncludeUnit',
    'IncludePath',
    'ProtoFile',
    'UsesClause',
    'FragmentDefinitions',
    'FragmentAddress',
    'FragmentHeader',
    'FragmentBody',
    'FragmentFooter',

    //add new here

    '@@@',//pt_Iterations: never use this one
    '@Fragment',
    '@Include',
    //add new iteration starters here
    '@',
    ''
  );

const //resourcestring??
  SXxmParseUnknownTag='Unknown parser tag "__"';
  SXxmParseIterateSkipFailed='No matching iteration end found.';

{ TXxmProtoParser }

constructor TXxmProtoParser.Create;
begin
  inherited Create;
  FData:='';
  FOutput:=nil;
  FIndex:=0;
  StackSize:=0;
  StackPosition:=0;
  PointsSize:=0;
  PointsCount:=0;
end;

destructor TXxmProtoParser.Destroy;
begin
  FreeAndNil(FOutput);
  SetLength(Stack,0);
  SetLength(Points,0);
  inherited;
end;

procedure TXxmProtoParser.Parse(Data: AnsiString);
var
  a,i,j,l:integer;
  b:boolean;
  s:AnsiString;
  pt:TXxmProtoParseTag;
begin
  FData:=Data;
  FreeAndNil(FOutput);
  FOutput:=TMemoryStream.Create;
  FIndex:=0;
  StackSize:=0;
  StackPosition:=0;
  PointsSize:=0;
  PointsCount:=0;

  //parse data into points array
  l:=Length(FData);
  i:=1;
  a:=1;
  while (i<=l) do
   begin
    a:=i;
    b:=false;
    while (i<=l) and not(b and (FData[i]='[')) do
     begin
      b:=FData[i]='[';
      inc(i);
     end;
    if b then
     begin
      //may be tag, find end
      j:=i+1;
      b:=false;
      while (j<=l) and not(b and (FData[j]=']')) do
       begin
        b:=FData[j]=']';
        inc(j);
       end;
      if b then
       begin
        //tag found
        s:=Copy(FData,i+1,j-i-2);
        pt:=TXxmProtoParseTag(0);
        while not(pt=pt_Unknown) and not(ProtoParseTag[pt]=s) do inc(pt);
        if pt=pt_Unknown then
          raise EXxmParseUnknownTag.Create(
            StringReplace(SXxmParseUnknownTag,'__',s,[]));
        AddPoint(a,i-a-1,pt);
        i:=j+1;
       end;
     end;
   end;
  AddPoint(a,l-a+1,pt_Unknown);
end;

function TXxmProtoParser.GetNext: TXxmProtoParseTag;
begin
  //assert FIndex<FPointsCount
  Output(Copy(FData,Points[FIndex].Index,Points[FIndex].Length));
  Result:=Points[FIndex].Tag;
  inc(FIndex);
end;

procedure TXxmProtoParser.IterateBegin(Condition:boolean);
var
  i,ic:integer;
begin
  if Condition then
   begin
    if StackPosition=StackSize then
     begin
      inc(StackSize,32);
      SetLength(Stack,StackSize);
     end;
    Stack[StackPosition]:=FIndex;
    inc(StackPosition);
   end
  else
   begin
    //IterateSkip
    i:=FIndex+1;
    ic:=1;
    while (i<PointsCount) and not(ic=0) do
     begin
      case Points[i].Tag of
        pt_Iterations..Pred(ptIterateEnd):inc(ic);
        ptIterateEnd:dec(ic);
      end;
      inc(i);
     end;
    if ic=0 then FIndex:=i else
      raise EXxmParseIterateSkipFailed.Create(SXxmParseIterateSkipFailed);
   end;
end;

procedure TXxmProtoParser.IterateNext(Condition:boolean);
begin
  //assert StackPosition>0
  if Condition then
    FIndex:=Stack[StackPosition-1]
  else
    dec(StackPosition);//IterateEnd
end;

procedure TXxmProtoParser.AddPoint(Index, Length: integer; Tag: TXxmProtoParseTag);
begin
  if PointsCount=PointsSize then
   begin
    inc(PointsSize,32);
    SetLength(Points,PointsSize);
   end;
  Points[PointsCount].Index:=Index;
  Points[PointsCount].Length:=Length;
  Points[PointsCount].Tag:=Tag;
  inc(PointsCount);
end;

procedure TXxmProtoParser.Output(Data: AnsiString);
begin
  FOutput.Write(Data[1],Length(Data));
end;

procedure TXxmProtoParser.Save(FilePath: AnsiString);
begin
  FOutput.Position:=0;
  FOutput.SaveToFile(FilePath);
end;

function TXxmProtoParser.Done: boolean;
begin
  Result:=FIndex>=PointsCount;
end;

end.
