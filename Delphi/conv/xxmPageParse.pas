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
    psComment,            // [[/ or [[?
    psParserParameter,    // [[*
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

  TXxmPageParserValues=(
    pvSend,
    pvSendClose,
    pvSendHTML,
    pvSendHTMLClose,
    //add new above
    pv_Unknown
  );

  TXxmPageParserValueList=array[TXxmPageParserValues] of record
    Code:AnsiString;
    EOLs:integer;
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
    FParserValues:TXxmPageParserValueList;
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
  if (i<LineNrsIndex) and (LineNrs[i].PasLineNr=PasLineNr) then
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

{ }

const
  DefaultParserValues:TXxmPageParserValueList=(
    (Code:'Context.Send(';EOLs:0),//pvSend
    (Code:');';EOLs:0),//pvSendClose
    (Code:'Context.SendHTML(';EOLs:0),//pvSendHTML
    (Code:');';EOLs:0),//pvSendHTMLClose
    //add new above
    (Code:'';EOLs:0)
  );

//TODO: project defaults (folder defaults?)

{ TXxmPageParser }

constructor TXxmPageParser.Create;
begin
  inherited;
  FData:='';
  SectionsCount:=0;
  SectionsSize:=0;
  FIndex:=0;
  FParserValues:=DefaultParserValues;
  //TODO: flag enable/disable keep line-numbers?
end;

destructor TXxmPageParser.Destroy;
begin
  SetLength(Sections,0);
  inherited;
end;

procedure TXxmPageParser.Parse(Data: AnsiString);
var
  SquareB,AngleB,InString:boolean;
  a1,a2,b1,b2,bx,l,n1,n2,nx,sqd,InComment:integer;
  ps:TXxmPageSection;
begin
  SectionsCount:=0;
  SectionsSize:=0;
  //SetLength(Sections,0);
  FData:=Data;
  FIndex:=0;
  l:=Length(FData);
  a1:=1;//counter warning
  a2:=1;
  n1:=1;
  nx:=1;
  while a2<=l do
   begin
    //search for an open tag
    n1:=nx;
    a1:=a2;
    SquareB:=false;
    AngleB:=false;
    while (a2<=l) and not(SquareB and (FData[a2]='[')) and not(AngleB and (FData[a2]='>')) do
     begin
      SquareB:=FData[a2]='[';
      AngleB:=FData[a2]='>';
      if char(FData[a2]) in [#13,#10] then
       begin
        inc(nx);
        if (FData[a2]=#13) and (a2<l) and (FData[a2+1]=#10) then inc(a2);
       end;
      inc(a2);
     end;
    b1:=a2+1;
    b2:=b1;
    if AngleB then a2:=b1;//inc(a2);
    if SquareB or AngleB then
     begin
      //open tag found, look for close tag (carefully: check strings and opening/closing braces and things)
      SquareB:=false;
      AngleB:=false;
      sqd:=0;//square bracket depth
      InString:=false;
      InComment:=0;
      n2:=nx;
      while (b2<=l) and not(SquareB and (FData[b2]=']')) and not(AngleB and (FData[b2]='<')) do
       begin
        SquareB:=false;
        AngleB:=false;
        if InString then
         begin
          if char(FData[b2]) in ['''',#13,#10] then InString:=false;
         end
        else
          case char(FData[b2]) of
            '''':if InComment=0 then InString:=true;
            '[':if InComment=0 then inc(sqd);
            ']':if InComment=0 then if sqd=0 then SquareB:=true else dec(sqd);
            '{':if InComment=0 then InComment:=1;
            '}':if InComment=1 then InComment:=0;
            '(':if (InComment=0) and (b2<l) and (FData[b2+1]='*') then InComment:=2;
            ')':if (InComment=2) and (b2>1) and (FData[b2-1]='*') then InComment:=0;
            '<':if InComment=0 then AngleB:=true;
          end;
        if char(FData[b2]) in [#13,#10] then
         begin
          inc(nx);
          if (FData[b2]=#13) and (b2<l) and (FData[b2+1]=#10) then inc(b2);
         end;
        inc(b2);
       end;
      if b2>l then
       begin
        //end reached
        AddSection(a1,a2-a1-1,n1,psHTML);
        AddSection(b1,b2-b1,n2,psBody);
        a1:=b2;
        a2:=b2;
       end
      else
        if ((SquareB or AngleB) and (a2<l)) or (b2>l) then
         begin
          //close tag found also
          if AngleB or (b2>l) then
           begin
            AddSection(a1,a2-a1-1,n1,psHTML);
            AddSection(b1,b2-b1-1,n2,psBody);
            a2:=b2;
           end
          else //SquareB
           begin
            bx:=b1;
            if bx=a2 then //opener was AngleB
             begin
              ps:=psBody;
              dec(bx);
             end
            else
              case char(FData[bx]) of
                '[':begin ps:=psSquareBracketsOpen; SquareB:=b2-b1=1; end;
                ']':begin ps:=psSquareBracketsClose; SquareB:=b2-b1=1; end;
                '!':ps:=psHeader;
                '@':ps:=psUses;
                ':':ps:=psDefinitions;
                '_':ps:=psFooter;
                '#':ps:=psSendHTML;
                '=':ps:=psSend;
                '/','?':ps:=psComment;
                '*':ps:=psParserParameter;
                //'&$%^+|;.,
                else
                 begin
                  ps:=psBody;
                  dec(bx);
                 end;
              end;
            if SquareB then
             begin
              AddSection(a1,a2-a1-1,n1,psHTML);
              AddSection(bx+1,b2-bx-2,n2,ps);
              a2:=b2+1;
             end
            else
              AddSection(a1,a2-a1,n1,psHTML);
           end;
          a1:=a2;
         end
        else
          a2:=l+1;
     end;
   end;
  AddSection(a1,l-a1+1,n1,psHTML);
  TotalLines:=nx;
end;

procedure TXxmPageParser.AddSection(Index, Length, LineNr: integer;
  ps: TXxmPageSection);
begin
  if (Length>0) or (ps=psParserParameter) then
   begin
    if SectionsCount=SectionsSize then
     begin
      inc(SectionsSize,$100);
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
  SendCloseCode:AnsiString;
  SendCloseEOLs,l:integer;

  procedure OpenString;
  begin
    if not InString then
     begin
      ss.WriteString('''');
      InString:=true;
      inc(l);
     end;
  end;

  procedure CloseString;
  begin
    if InString then
     begin
      ss.WriteString('''');
      InString:=false;
      inc(l);
     end;
  end;

  procedure OpenSend;
  begin
    if not InSend then
     begin
      ss.WriteString('  '+FParserValues[pvSendHTML].Code);
      map.MapLine(FParserValues[pvSendHTML].EOLs,0);
      InSend:=true;
      SendCloseCode:=FParserValues[pvSendHTMLClose].Code;
      SendCloseEOLs:=FParserValues[pvSendHTMLClose].EOLs;
      InString:=false;//assert already false?
      l:=15;
     end;
  end;

  procedure CloseSend;
  begin
    if InSend then
     begin
      CloseString;
      ss.WriteString(SendCloseCode+#13#10);
      map.MapLine(1+SendCloseEOLs,0);
      InSend:=false;
     end;
  end;

  function CountEOLs(s:AnsiString):integer;
  var
    i,l:integer;
  begin
    Result:=0;
    l:=Length(s);
    for i:=1 to l-1 do if (s[i]=#13) and (s[i+1]=#10) then inc(Result);
  end;

  procedure ParseParserValue(Code: string);
  var
    pv:TXxmPageParserValues;
  begin
    if Code<>'' then
     begin
      pv:=pv_Unknown;
      case Code[1] of
        '=':
         begin
          pv:=pvSend;
          CloseSend;
         end;
        '#':
         begin
          pv:=pvSendHTML;
          CloseSend;
         end;
        else
         begin
          //TODO: parse parameter name
         end;
      end;
      if pv=pv_Unknown then raise Exception.Create('Unknown parse parameter');
      if Length(Code)=1 then
       begin
        FParserValues[pv]:=DefaultParserValues[pv];
        pv:=Succ(pv);
        FParserValues[pv]:=DefaultParserValues[pv];//close
       end
      else
       begin
        case Code[2] of
          '(':;
          ')':pv:=Succ(pv);//close
          else raise Exception.Create('Use "(" or ")" to define opening and closing code');
        end;
        FParserValues[pv].Code:=Copy(Code,3,Length(Code)-2);
        FParserValues[pv].EOLs:=CountEOLs(FParserValues[pv].Code);
       end;
     end;
  end;

  procedure ParseParserValuesMultiLine(Index: integer);
  var
    sl:TStringList;
    i:integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Text:=Copy(FData,Sections[Index].Index,Sections[Index].Length);
      for i:=0 to sl.Count-1 do
        try
          ParseParserValue(Trim(sl[i]));
        except
          on e:Exception do
           begin
            e.Message:='['+IntToStr(Sections[Index].LineNr+i)+']'+e.Message;
            raise;
           end;
        end;
    finally
      sl.Free;
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
          CloseSend;
          ss.WriteString('  '+FParserValues[pvSendHTML].Code+'''[['''+FParserValues[pvSendHTMLClose].Code+#13#10);
          map.MapLine(1+FParserValues[pvSendHTML].EOLs+FParserValues[pvSendHTMLClose].EOLs,
            Sections[Section].LineNr);//assert EOLs(Section)=0
         end;
        psSquareBracketsClose:
         begin
          CloseSend;
          ss.WriteString('  '+FParserValues[pvSendHTML].Code+''']]'''+FParserValues[pvSendHTMLClose].Code+#13#10);
          map.MapLine(1+FParserValues[pvSendHTML].EOLs+FParserValues[pvSendHTMLClose].EOLs,
            Sections[Section].LineNr);//assert EOLs(Section)=0
         end;
        psHTML:
         begin
          //convert into Context.SendHTML(
          p:=Sections[Section].Index;
          q:=p+Sections[Section].Length-1;
          OpenSend;
          while (p<=q) do
           begin
            if l>=78 then //setting?
             begin
              //CloseSend;
              CloseString;
              ss.WriteString('+'#13#10'    ');
              l:=0;
              map.MapLine(1,0);
             end;
            case char(FData[p]) of
              #0..#31:
               begin
                CloseString;
                b:=byte(FData[p]);
                //setting?
                //ss.WriteString('#$'+IntToHex(b,2));
                case byte(FData[p]) of
                  0..9:
                   begin
                    ss.WriteString('#'+AnsiChar($30+b));
                    inc(l,2);
                   end;
                  10..99:
                   begin
                    ss.WriteString('#'+AnsiChar($30+(b div 10))+AnsiChar($30+(b mod 10)));
                    inc(l,3);
                   end;
                  100..255:
                   begin
                    ss.WriteString('#'+AnsiChar($30+(b div 100))+AnsiChar($30+(b div 10 mod 10))+AnsiChar($30+(b mod 10)));
                    inc(l,4);
                   end;
                end;
               end;
              '''':
               begin
                OpenString;
                ss.WriteString('''''');
                inc(l,2);
               end;
              else
               begin
                OpenString;
                ss.Write(FData[p],1);
                inc(l);
               end;
            end;
            inc(p);
           end;
         end;
        psSend:
         begin
          CloseSend;
          ss.WriteString('  '+FParserValues[pvSend].Code);
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          ss.WriteString(FParserValues[pvSendClose].Code+#13#10);
          map.MapLine(1+FParserValues[pvSend].EOLs+FParserValues[pvSendClose].EOLs+EOLs(Section),
            Sections[Section].LineNr);
         end;
        psSendHTML:
         begin
          CloseSend;
          ss.WriteString('  '+FParserValues[pvSendHTML].Code);
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          ss.WriteString(FParserValues[pvSendHTMLClose].Code+#13#10);
          map.MapLine(1+FParserValues[pvSendHTML].EOLs+FParserValues[pvSendHTMLClose].EOLs+EOLs(Section),
            Sections[Section].LineNr);
         end;
        psBody:
         begin
          CloseSend;
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
        psParserParameter:
          if Sections[Section].Length=0 then
           begin
            //no parameters: restore defaults
            FParserValues:=DefaultParserValues;
            CloseSend;
           end
          else
            if (Sections[Section].Length>2) and (char(FData[Sections[Section].Index]) in [#13,' ']) then
              ParseParserValuesMultiLine(Section)
            else
              try
                ParseParserValue(Copy(FData,Sections[Section].Index,Sections[Section].Length));
              except
                on e:Exception do
                 begin
                  e.Message:='['+IntToStr(Sections[Section].LineNr)+']'+e.Message;
                  raise;
                 end;
              end;
        //psComment: ignore
        //else
      end;
    CloseSend;
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
