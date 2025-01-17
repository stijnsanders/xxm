unit xxmAutoCompile;

interface

uses SysUtils, xxm2, xxmPReg;

function ProjectCheck_AutoCompile(Entry: TProjectEntry;
  Context: CxxmContext; const ProjectName: UTF8String): boolean;

implementation

uses Windows, xxmTools, xxmContext, xxmProject1;

procedure BuildOutput(Subject: TObject; const Msg:AnsiString);
begin
  (Subject as TProjectEntry).LastResult:=
    (Subject as TProjectEntry).LastResult
    +UTF8String(Msg);//UTF8Encode?
end;

function BuildError(const Title,Body,Footer:UTF8String):UTF8String;
begin
  //TODO: from resource? from project data?
  Result:='<!doctype html>'#13#10
    +'<html><head><title>'+Title+'</title></head>'#13#10
    +'<body style="font-family:sans-serif;background-color:white;color:black;margin:0em;">'#13#10
    +'<h1 style="background-color:#0000CC;color:white;margin:0em;padding:0.2em;">'+Title+'</h1>'#13#10
    +Body
    +'<p style="background-color:#0000CC;color:white;font-size:0.8em;margin:0em;padding:0.2em;text-align:right;">'#13#10
    +Footer
    +'<a href="http://yoy.be/xxm/" style="color:white;">xxm</a> '
    +HTMLEncode(UTF8Encode(DateTimeToStr(Now)))+'</p></body></html>'#13#10;
end;

function ProjectCheck_AutoCompile(Entry: TProjectEntry;
  Context: CxxmContext; const ProjectName: UTF8String): boolean;
var
  Project:TXxmProject;
  fn,hp,pp,wsig:string;
  lr:UTF8String;
  b:boolean;
const
  NoNextBuildAfter=2000;//TODO: setting
begin
  lr:=Entry.LastResult;
  Result:=lr='';//default
  if cardinal(GetTickCount-Entry.LastCheck)>NoNextBuildAfter then
   begin
    fn:=Entry.FilePath;
    Entry.Lock;
    try
      //again for those that waited on lock
      if cardinal(GetTickCount-Entry.LastCheck)>NoNextBuildAfter then
       begin
        Entry.LastResult:=Context.ContextString(csVersion)+#13#10;
        hp:=Entry.HandlerPath;
        if hp='' then hp:=XxmProjectRegistry.HandlerPath;
        pp:=Entry.ProtoPath;
        if pp='' then pp:=XxmProjectRegistry.ProtoPath;
        try
          //CanCreate would disturb standalone xxl
          Project:=TXxmProject.Create(Entry,fn,hp,pp,BuildOutput,false);
          try
            try
              b:=Project.CheckFiles(false,nil);
              if not(b) then
               begin
                lr:='';
                Entry.LastResult:='';
                Result:=true;
                wsig:=GetFileSignature(
                  Project.RootFolder+Project.ProjectFile);
                if Entry.Signature<>wsig then
                  b:=Project.GenerateProjectFiles(false,nil);
               end;
              if b or not(FileExists(fn)) then
               begin
                Entry.Release;
                //only compile when changes detected
                //only save when compile success
                if Project.Compile then
                 begin
                  Project.Update;
                  wsig:=GetFileSignature(
                    Project.RootFolder+Project.ProjectFile);
                  Entry.Signature:=wsig;
                  lr:='';
                  Entry.LastResult:='';
                  Result:=true;
                 end
                else
                 begin
                  lr:=BuildError('Build failed: '+HTMLEncode(ProjectName),
                    '<xmp style="margin:0.1em;">'+UTF8Encode(Entry.LastResult)+'</xmp>'#13#10,//<xmp> doesn't need HTMLEncode
                    '<a href="'+HTMLEncode(Context.URL)+'" style="float:left;color:white;">refresh</a>'#13#10);
                  Result:=false;
                 end;
                Entry.LastCheck:=GetTickCount;
               end;
            except
              on e:Exception do
               begin
                lr:=BuildError('Error building: '+HTMLEncode(ProjectName),
                  '<p style="margin:0.1em;">An error occurred while building the module.<br />'#13#10
                  +'<i>'+HTMLEncode(UTF8Encode(e.ClassName))+'</i><br /><b>'+HTMLEncode(UTF8Encode(e.Message))+'</b></p>'#13#10
                  +'</p>','');
                Result:=false;
               end;
            end;
          finally
            Project.Free;
          end;
        except
          on EXxmProjectNotFound do Result:=true;//assert xxl only
        end;
       end
      else
        Result:=Entry.LastResult='';
    finally
      Entry.Unlock;
    end;
   end;
  if not Result then
    if lr='' then
      Context.SendHTML(BuildError('Error building: '+HTMLEncode(ProjectName),
        '<p style="margin:0.1em;">AutoCompile failed in an other worker process.</p>'#13#10,
        //HTMLEncode(lr)?
        ''))
    else
      Context.SendHTML(lr);
end;

end.
