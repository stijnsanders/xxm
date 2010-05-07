unit xxmPageParse;

interface

uses Classes;

type
  TXxmPageSection=(
    psHTML,
    psUses,               // [[@
    psDefinitions,        // [[:
    psHeader,             // [[!
    psBody,               // [[
    psFooter,             // [[_
    psSend,               // [[=
    psSendHTML,           // [[#
    psComment,            // [[/
    psSquareBracketsOpen, // [[[]]
    psSquareBracketsClose // [[]]]
  );

//TODO: dynamic chars for use with AllSections

  TXxmLineNumbersMap=class(TObject)
  private
    LineNrs:array of record
      PasLineNr,XxmLineNr:word;
    end;
    LineNrsIndex,LineNrsSize,PasLineNr:integer;
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MapLine(AdvancePasLines, XxmLineNr:integer);
    procedure Save(FilePath:AnsiString);
    procedure Clear;
    procedure Load(FilePath:AnsiString);
    function GetXxmLines(PasLineNr:integer):AnsiString;
  end;

  TXxmPageParser=class(TObject)
  private
    FData:AnsiString;
    SectionsCount,SectionsSize,TotalLines:integer;
    Sections:array of record
      Index,Length,LineNr:integer;
      SectionType:TXxmPageSection;
    end;
    FIndex:integer;
    procedure AddSection(Index,Length,LineNr:integer;ps:TXxmPageSection);
    function EOLs(Index: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Data:AnsiString);
    //function GetNext:;
    //function Done:boolean;
    function AllSections(ps:TXxmPageSection;map:TXxmLineNumbersMap):AnsiString;
    function BuildBody(map:TXxmLineNumbersMap):AnsiString;
  end;

implementation

uses SysUtils, Windows;

{ TXxmLineNumbersMap }

constructor TXxmLineNumbersMap.Create;
begin
  inherited;
  LineNrsIndex:=0;
  LineNrsSize:=0;
  PasLineNr:=1;
end;

destructor TXxmLineNumbersMap.Destroy;
begin
  SetLength(LineNrs,0);
  inherited;
end;

procedure TXxmLineNumbersMap.Clear;
begin
  LineNrsIndex:=0;
  //LineNrsSize:=0;
  PasLineNr:=1;
end;

procedure TXxmLineNumbersMap.Grow;
begin
  inc(LineNrsSize,$400);
  SetLength(LineNrs,LineNrsSize);
end;

procedure TXxmLineNumbersMap.MapLine(AdvancePasLines, XxmLineNr: integer);
begin
  if XxmLineNr<>0 then
   begin
    if LineNrsIndex=LineNrsSize then Grow;
    LineNrs[LineNrsIndex].PasLineNr:=PasLineNr;
    LineNrs[LineNrsIndex].XxmLineNr:=XxmLineNr;
    inc(LineNrsIndex);
   end;
  inc(PasLineNr,AdvancePasLines);
end;

procedure TXxmLineNumbersMap.Save(FilePath: AnsiString);
var
  f:TFileStream;
begin
  SetFileAttributesA(PAnsiChar(FilePath),0);//strange, hidden files throw exception on overwrite
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(LineNrs[0],LineNrsIndex*4);
  finally
    f.Free;
  end;
  SetFileAttributesA(PAnsiChar(FilePath),FILE_ATTRIBUTE_HIDDEN);// or FILE_ATTRIBUTE_SYSTEM);//?
end;

procedure TXxmLineNumbersMap.Load(FilePath: AnsiString);
var
  f:TFileStream;
  s:Int64;
begin
  PasLineNr:=1;
  f:=TFileStream.Create(FilePath,fmOpenRead);
  try
    s:=f.Size;
    LineNrsIndex:=s div 4;
    while LineNrsSize<LineNrsIndex do Grow;
    f.Read(LineNrs[0],s);
  finally
    f.Free;
  end;
end;

function TXxmLineNumbersMap.GetXxmLines(PasLineNr:integer):AnsiString;
var
  i:integer;
begin
  i:=0;
  Result:='';
  while (i<LineNrsIndex) and (LineNrs[i].PasLineNr<PasLineNr) do inc(i);
  if i<LineNrsIndex then
    if LineNrs[i].PasLineNr=PasLineNr then
      while (i<LineNrsIndex) and (LineNrs[i].PasLineNr=PasLineNr) do
       begin
        if Result<>'' then Result:=Result+',';
        Result:=Result+IntToStr(LineNrs[i].XxmLineNr);
        inc(i);
       end
    else
      if i>0 then
       begin
        dec(i);
        Result:=IntToStr(LineNrs[i].XxmLineNr+PasLineNr-LineNrs[i].PasLineNr);
       end;
end;

{ TXxmPageParser }

constructor TXxmPageParser.Create;
begin
  inherited;
  FData:='';
  SectionsCount:=0;
  SectionsSize:=0;
  FIndex:=0;
  //TODO: flag enable/disable keep line-numbers?
end;

destructor TXxmPageParser.Destroy;
begin
  SetLength(Sections,0);
  inherited;
end;

procedure TXxmPageParser.Parse(Data: AnsiString);
var
  b,instr:boolean;
  a,i,j,l,n1,n2,nx,k,sqd,incom:integer;
  ps:TXxmPageSection;
begin
  SectionsCount:=0;
  SectionsSize:=0;
  //SetLength(Sections,0);
  FData:=Data;
  FIndex:=0;
  l:=Length(FData);
  i:=1;
  a:=1;//counter warning
  n1:=1;
  nx:=1;
  while i<=l do
   begin
    //search for an open tag
    n1:=nx;
    a:=i;
    b:=false;
    while (i<=l) and not(b and (FData[i]='[')) do
     begin
      b:=FData[i]='[';
      if FData[i] in [#13,#10] then
       begin
        inc(nx);
        if (FData[i]=#13) and (i<l) and (FData[i+1]=#10) then inc(i);
       end;
      inc(i);
     end;
    if b then
     begin
      //open tag found, look for close tag (carefully: check strings and opening/closing braces and things)
      j:=i+1;
      b:=false;
      sqd:=0;//square bracket depth
      instr:=false;
      incom:=0;
      n2:=nx;
      while (j<=l) and not(b and (FData[j]=']')) do
       begin
        if instr then
         begin
          if FData[j] in ['''',#13,#10] then instr:=false;
         end
        else
          case FData[j] of
            '''':if incom=0 then instr:=true;
            '[':if incom=0 then inc(sqd);
            ']':if incom=0 then if sqd=0 then b:=true else dec(sqd);
            '{':if incom=0 then incom:=1;
            '}':if incom=1 then incom:=0;
            '(':if (incom=0) and (j<l) and (FData[j+1]='*') then incom:=2;
            ')':if (incom=2) and (j>1) and (FData[j-1]='*') then incom:=0;
          end;
        if FData[j] in [#13,#10] then
         begin
          inc(nx);
          if (FData[j]=#13) and (j<l) and (FData[j+1]=#10) then inc(j);
         end;
        inc(j);
       end;
      if b and (i<l) then
       begin
        //close tag found also
        k:=i+1;
        case FData[k] of
          '[':begin ps:=psSquareBracketsOpen; b:=j-i=2; end;
          ']':begin ps:=psSquareBracketsClose; b:=j-i=2; end;
          '!':ps:=psHeader;
          '@':ps:=psUses;
          ':':ps:=psDefinitions;
          '_':ps:=psFooter;
          '#':ps:=psSendHTML;
          '=':ps:=psSend;
          '/','?':ps:=psComment;
          //'&$%*^+|;.,
          else
           begin
            ps:=psBody;
            dec(k);
           end;
        end;
        if b then
         begin
          AddSection(a,i-a-1,n1,psHTML);
          AddSection(k+1,j-k-2,n2,ps);
          i:=j+1;
         end
        else
          AddSection(a,i-a,n1,psHTML);
        a:=i;
       end
      else
        i:=l+1;
     end;
   end;
  AddSection(a,l-a+1,n1,psHTML);
  TotalLines:=nx;
end;

procedure TXxmPageParser.AddSection(Index, Length, LineNr: integer;
  ps: TXxmPageSection);
begin
  if Length>0 then
   begin
    if SectionsCount=SectionsSize then
     begin
      inc(SectionsSize,16);
      SetLength(Sections,SectionsSize);
     end;
    Sections[SectionsCount].Index:=Index;
    Sections[SectionsCount].Length:=Length;
    Sections[SectionsCount].LineNr:=LineNr;
    Sections[SectionsCount].SectionType:=ps;
    inc(SectionsCount);
   end;
end;

function TXxmPageParser.AllSections(ps:TXxmPageSection;map:TXxmLineNumbersMap): AnsiString;
var
  i,j,l:integer;
begin
  //get total length first
  l:=0;
  for i:=0 to SectionsCount-1 do
    if Sections[i].SectionType=ps then inc(l,Sections[i].Length);
  //allocate all memory needed in one go
  SetLength(Result,l);
  //and fill it
  j:=1;
  for i:=0 to SectionsCount-1 do
    if Sections[i].SectionType=ps then
     begin
      Move(FData[Sections[i].Index],Result[j],Sections[i].Length);
      inc(j,Sections[i].Length);
      map.MapLine(EOLs(i),Sections[i].LineNr);
     end;
end;

function TXxmPageParser.BuildBody(map:TXxmLineNumbersMap): AnsiString;
var
  ss:TStringStream;
  InSend,InString:boolean;
  l:integer;

  procedure DoString(b:boolean);
  begin
    if b then
     begin
      if not(InString) then
       begin
        ss.WriteString('''');
        InString:=true;
        inc(l);
       end;
     end
    else
     begin
      if InString then
       begin
        ss.WriteString('''');
        InString:=false;
        inc(l);
       end;
     end;
  end;

  procedure DoSend(b:boolean);
  begin
    if b then
     begin
      if not(InSend) then
       begin
        ss.WriteString('  Context.SendHTML(');
        InSend:=true;
        InString:=false;//assert already false?
        l:=15;
       end;
     end
    else
     begin
      if InSend then
       begin
        DoString(false);
        ss.WriteString(');'#13#10);
        InSend:=false;
        map.MapLine(1,0);
       end;
     end;
  end;

var
  Section,p,q:integer;
  b:byte;
begin
  ss:=TStringStream.Create('');
  try
    InSend:=false;
    InString:=false;
    l:=0;
    for Section:=0 to SectionsCount-1 do
      case Sections[Section].SectionType of
        psSquareBracketsOpen:
         begin
          DoSend(false);
          ss.WriteString('  Context.SendHTML(''[['');'#13#10);
          map.MapLine(1,Sections[Section].LineNr);//assert EOLs(Section)=0
         end;
        psSquareBracketsClose:
         begin
          DoSend(false);
          ss.WriteString('  Context.SendHTML('']]'');'#13#10);
          map.MapLine(1,Sections[Section].LineNr);//assert EOLs(Section)=0
         end;
        psHTML:
         begin
          //convert into Context.SendHTML(
          p:=Sections[Section].Index;
          q:=p+Sections[Section].Length-1;
          DoSend(true);
          while (p<=q) do
           begin
            if l>=78 then //setting?
             begin
              //DoSend(false);
              DoString(false);
              ss.WriteString('+'#13#10'    ');
              l:=0;
              map.MapLine(1,0);
             end;
            case FData[p] of
              #0..#31:
               begin
                DoString(false);
                b:=byte(FData[p]);
                //setting?
                //ss.WriteString('#$'+IntToHex(b,2));
                case byte(FData[p]) of
                  0..9:
                   begin
                    ss.WriteString('#'+char($30+b));
                    inc(l,2);
                   end;
                  10..99:
                   begin
                    ss.WriteString('#'+char($30+(b div 10))+char($30+(b mod 10)));
                    inc(l,3);
                   end;
                  100..255:
                   begin
                    ss.WriteString('#'+char($30+(b div 100))+char($30+(b div 10 mod 10))+char($30+(b mod 10)));
                    inc(l,4);
                   end;
                end;
               end;
              '''':
               begin
                DoString(true);
                ss.WriteString('''''');
                inc(l,2);
               end;
              else
               begin
                DoString(true);
                ss.Write(FData[p],1);
                inc(l);
               end;
            end;
            inc(p);
           end;
         end;
        psSend:
         begin
          DoSend(false);
          ss.WriteString('  Context.Send(');
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          ss.WriteString(');'#13#10);
          map.MapLine(1+EOLs(Section),Sections[Section].LineNr);
         end;
        psSendHTML:
         begin
          DoSend(false);
          ss.WriteString('  Context.SendHTML(');
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          ss.WriteString(');'#13#10);
          map.MapLine(1+EOLs(Section),Sections[Section].LineNr);
         end;
        psBody:
         begin
          DoSend(false);
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          if (Sections[Section].Length<2) or not(
            (FData[Sections[Section].Index+Sections[Section].Length-2]=#13) and
            (FData[Sections[Section].Index+Sections[Section].Length-1]=#10)) then
           begin
            ss.WriteString(#13#10);
            map.MapLine(1+EOLs(Section),Sections[Section].LineNr);
           end
          else
            map.MapLine(EOLs(Section),Sections[Section].LineNr);
         end;
        //else ingnored
      end;
    DoSend(false);
    Result:=ss.DataString;
  finally
    ss.Free;
  end;
end;

function TXxmPageParser.EOLs(Index: integer): integer;
begin
  if Index=SectionsCount-1 then
    Result:=TotalLines-Sections[Index].LineNr
  else
    Result:=Sections[Index+1].LineNr-Sections[Index].LineNr;
end;

end.
