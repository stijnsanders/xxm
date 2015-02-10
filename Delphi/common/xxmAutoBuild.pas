unit xxmAutoBuild;

interface

uses xxm, xxmPReg;

function AutoBuild(Entry: TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString):boolean;

implementation

uses Windows, SysUtils, Classes, Registry, xxmWebProject, xxmUtilities, xxmCommonUtils;

var
  BuildOutput:TStringStream;

procedure DoBuildOutput(Msg:AnsiString);
begin
  BuildOutput.WriteString(Msg);
end;

function AutoBuild(Entry: TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString):boolean;
var
  WebProject:TXxmWebProject;
  wsig,fn:AnsiString;
  b:boolean;
const
  RT_HTML=PAnsiChar(23);//MakeIntResource(23);
  NoNextBuildAfter=5000;//TODO: setting!

  function BuildError(const res, val1, val2: AnsiString): AnsiString;
  var
    i,j,l:integer;
    s:AnsiString;
    r:TResourceStream;
  begin
    r:=TResourceStream.Create(HInstance,res,RT_HTML);
    try
      l:=r.Size;
      SetLength(s,l);
      r.Read(s[1],l);
    finally
      r.Free;
    end;
    //l:=Length(s);
    i:=1;
    Result:='';
    while i<=l do
     begin
      j:=i;
      while (j<l) and (s[j]<>'$') do inc(j);
      if j=l then inc(j);
      Result:=Result+Copy(s,i,j-i);
      if j<l then
       begin
        inc(j);
        case char(s[j]) of
          '1':Result:=Result+HTMLEncode(val1);
          '2':Result:=Result+HTMLEncode(val2);
          'L':Result:=Result+WebProject.ResolveErrorLines(BuildOutput.DataString);
          'N':Result:=Result+DateTimeToStr(Now);
          'P':Result:=Result+HTMLEncode(ProjectName);
          'U':Result:=Result+HTMLEncode(Context.URL);
          //else?
        end;
        inc(j);
       end;
      i:=j;
     end;
  end;


begin
  Result:=true;//default;
  if (GetTickCount-Entry.LastCheck)>NoNextBuildAfter then
   begin
    fn:=Entry.ModulePath;//force get from registry outside of lock
    Entry.Lock;
    try
      //again for those that waited on lock
      if (GetTickCount-Entry.LastCheck)>NoNextBuildAfter then
      try
        //TODO: bidirectional xxl/xxmp mapping?
        BuildOutput:=TStringStream.Create('');
        try
          BuildOutput.WriteString(Context.ContextString(csVersion)+#13#10);
          //CanCreate would disturb standalone xxl
          WebProject:=TXxmWebProject.Create(fn,DoBuildOutput,false);
          try
            b:=WebProject.CheckFiles(false,nil);
            wsig:=GetFileSignature(WebProject.RootFolder+WebProject.ProjectFile);
            if not(b) and (Entry.Signature<>wsig) then
              b:=WebProject.GenerateProjectFiles(false,nil);
            if b or not(FileExists(fn)) then
             begin
              Entry.Release;
              //only compile when changes detected
              //only save when compile success
              if WebProject.Compile then
               begin
                WebProject.Update;
                wsig:=GetFileSignature(WebProject.RootFolder+WebProject.ProjectFile);
                Entry.Signature:=wsig;
                //Entry.LastCheck:=GetTickCount;//moved to finally
               end
              else
               begin
                Result:=false;
                Context.SendHTML(BuildError('bfail','',''));
                //TODO: rig lastcheck to fail waiting threads?
               end;
             end;
          finally
            WebProject.Free;
          end;
        finally
          BuildOutput.Free;
          Entry.LastCheck:=GetTickCount;
        end;
      except
        on e:EXxmWebProjectNotFound do Result:=true;//assert xxl only
        on e:Exception do
         begin
          Result:=false;
          Context.SendHTML(BuildError('berror',e.ClassName,e.Message));
         end;
      end;
    finally
      Entry.Unlock;
    end;
   end;
end;

end.
