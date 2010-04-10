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

  TXxmPageParser=class(TObject)
  private
    FData:AnsiString;
    SectionsCount,SectionsSize:integer;
    Sections:array of record
      Index,Length:integer;
      SectionType:TXxmPageSection;
    end;
    FIndex:integer;
    procedure AddSection(Index,Length:integer;ps:TXxmPageSection);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Data:AnsiString);
    //function GetNext:;
    //function Done:boolean;
    function AllSections(ps:TXxmPageSection):AnsiString;
    function BuildBody:AnsiString;
  end;

implementation

uses SysUtils;

{ TXxmPageParser }

constructor TXxmPageParser.Create;
begin
  inherited;
  FData:='';
  SectionsCount:=0;
  SectionsSize:=0;
  FIndex:=0;
end;

destructor TXxmPageParser.Destroy;
begin
  SetLength(Sections,0);
  inherited;
end;

procedure TXxmPageParser.Parse(Data: AnsiString);
var
  b,instr:boolean;
  a,i,j,l,k,sqd,incom:integer;
  ps:TXxmPageSection;
begin
  SectionsCount:=0;
  SectionsSize:=0;
  //SetLength(Sections,0);
  FData:=Data;
  FIndex:=0;
  l:=Length(FData);
  i:=1;
  a:=1;
  while i<=l do
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
      //start tag found
      j:=i+1;
      b:=false;
      sqd:=0;//square bracket depth
      instr:=false;
      incom:=0;
      while (j<=l) and not(b and (FData[j]=']')) do
       begin
        if instr then
          case FData[j] of
            '''':instr:=false;
            #13,#10:instr:=false;
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
        inc(j);
       end;
      if b and (i<l) then
       begin
        //end tag found also
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
          AddSection(a,i-a-1,psHTML);
          AddSection(k+1,j-k-2,ps);
          i:=j+1;
         end
        else
          AddSection(a,i-a,psHTML);
        a:=i;
       end
      else
        i:=l+1;
     end;
   end;
  AddSection(a,l-a+1,psHTML);
end;

procedure TXxmPageParser.AddSection(Index, Length: integer;
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
    Sections[SectionsCount].SectionType:=ps;
    inc(SectionsCount);
   end;
end;

function TXxmPageParser.AllSections(ps: TXxmPageSection): AnsiString;
var
  i,j,l:integer;
begin
  l:=0;
  for i:=0 to SectionsCount-1 do
    if Sections[i].SectionType=ps then inc(l,Sections[i].Length);
  SetLength(Result,l);
  j:=1;
  for i:=0 to SectionsCount-1 do
    if Sections[i].SectionType=ps then
     begin
      Move(FData[Sections[i].Index],Result[j],Sections[i].Length);
      inc(j,Sections[i].Length);
     end;
end;

function TXxmPageParser.BuildBody: AnsiString;
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
         end;
        psSquareBracketsClose:
         begin
          DoSend(false);
          ss.WriteString('  Context.SendHTML('']]'');'#13#10);
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
         end;
        psSendHTML:
         begin
          DoSend(false);
          ss.WriteString('  Context.SendHTML(');
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          ss.WriteString(');'#13#10);
         end;
        psBody:
         begin
          DoSend(false);
          ss.Write(FData[Sections[Section].Index],Sections[Section].Length);
          if (Sections[Section].Length<2) or not(
           (FData[Sections[Section].Index+Sections[Section].Length-2]=#13) and
           (FData[Sections[Section].Index+Sections[Section].Length-1]=#10)) then
            ss.WriteString(#13#10);
         end;
        //else ingnored
      end;
    DoSend(false);
    Result:=ss.DataString;
  finally
    ss.Free;
  end;
end;

end.
