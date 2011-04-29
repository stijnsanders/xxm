unit xxmAutoBuild;

interface

uses xxm, xxmPReg;

function AutoBuild(pce:TXxmProjectEntry;
  Context:IXxmContext; ProjectName:WideString):boolean;

implementation

uses Windows, SysUtils, Classes, Registry, xxmWebProject, xxmUtilities;

var
  BuildOutput:TStringStream;
  BuildLock:TRTLCriticalSection;

procedure DoBuildOutput(Msg:AnsiString);
begin
  BuildOutput.WriteString(Msg);
end;

function BuildError(res: AnsiString; vals: array of AnsiString):AnsiString;
var
  i:integer;
  r:TResourceStream;
  l:int64;
const
  RT_HTML = MakeIntResource(23);
begin
  r:=TResourceStream.Create(HInstance,res,RT_HTML);
  try
    l:=r.Size;
    SetLength(Result,l);
    r.Read(Result[1],l);
  finally
    r.Free;
  end;
  for i:=0 to (Length(vals) div 2)-1 do
    Result:=StringReplace(Result,'[['+vals[i*2]+']]',vals[i*2+1],[rfReplaceAll]);
end;

function AutoBuild(pce:TXxmProjectEntry;
  Context:IXxmContext; ProjectName:WideString):boolean;
var
  WebProject:TXxmWebProject;
  wsig,fn:AnsiString;
  b:boolean;
  tc:cardinal;
const
  RT_HTML=PAnsiChar(23);//MakeIntResource(23);
  NoNextBuildAfter=5000;//TODO: setting!
begin
  Result:=true;//default
  tc:=GetTickCount;
  if (tc-pce.LastCheck)>NoNextBuildAfter then
   begin
    fn:=pce.ModulePath;//force get from registry outside of lock
    EnterCriticalSection(BuildLock);
    try
      //again for those that waited on lock
      if (GetTickCount-pce.LastCheck)>NoNextBuildAfter then
      try
        //TODO: bidirectional xxl/xxmp mapping?
        BuildOutput:=TStringStream.Create('');
        try
          BuildOutput.WriteString(Context.ContextString(csVersion));
          BuildOutput.WriteString(#13#10);
          //CanCreate would disturb standalone xxl
          WebProject:=TXxmWebProject.Create(fn,DoBuildOutput,false);
          try
            b:=WebProject.CheckFiles(false);
            wsig:=Signature(WebProject.RootFolder+WebProject.ProjectFile);
            if not(b) and not(pce.Signature=wsig) then
              b:=WebProject.GenerateProjectFiles(false);
            if b or not(FileExists(fn)) then
             begin
              pce.Release;//try? silent?
              //only compile when changes detected
              //only save when compile success
              if WebProject.Compile then
               begin
                WebProject.Update;
                wsig:=Signature(WebProject.RootFolder+WebProject.ProjectFile);
                pce.Signature:=wsig;
                pce.LastCheck:=GetTickCount;
               end
              else
               begin
                Result:=false;
                Context.SendHTML(BuildError('bfail',[
                  'URL',HTMLEncode(Context.URL),
                  'ProjectName',ProjectName,
                  'Log',WebProject.ResolveErrorLines(BuildOutput.DataString),
                  'DateTime',DateTimeToStr(Now)
                ]));
                //TODO: rig lastcheck to fail waiting threads?
               end;
             end
            else
              pce.LastCheck:=GetTickCount;
          finally
            WebProject.Free;
          end;
        finally
          BuildOutput.Free;
        end;
      except
        on e:EXxmWebProjectNotFound do Result:=true;//assert xxl only
        on e:Exception do
         begin
          Result:=false;
          Context.SendHTML(BuildError('berror',[
            'URL',HTMLEncode(Context.URL),
            'ProjectName',ProjectName,
            'ErrorClass',e.ClassName,
            'ErrorMessage',HTMLEncode(e.Message),
            'DateTime',DateTimeToStr(Now)
          ]));
         end;
      end;
    finally
      LeaveCriticalSection(BuildLock);
    end;
   end;
end;

initialization
  InitializeCriticalSection(BuildLock);
finalization
  DeleteCriticalSection(BuildLock);
end.
