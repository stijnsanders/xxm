unit xxmAutoUpdate;

interface

uses SysUtils, xxmPReg;

function ProjectCheck_AutoUpdate(Entry: TProjectEntry;
  Context: TObject; const ProjectName: UTF8String): boolean;

implementation

uses Windows, xxmTools, xxmContext;

function ProjectCheck_AutoUpdate(Entry: TProjectEntry;
  Context: TObject; const ProjectName: UTF8String): boolean;
var
  fn,fn1,s,s1:string;
  i,lc:integer;
  em:UTF8String;
const
  NoNextUpdateAfter=2500;//TODO: setting!
begin
  //fn:='['+ProjectName+']';
  try
    if cardinal(GetTickCount-Entry.LastCheck)>NoNextUpdateAfter then
     begin
      Entry.LastCheck:=GetTickCount;
      fn:=Entry.FilePath;
      s:=GetFileSignature(fn);
      i:=Length(fn);
      while (i<>0) and (fn[i]<>'.') do dec(i);
      fn1:=Copy(fn,1,i-1)+'.xxu';
      s1:=GetFileSignature(fn1);
      if (s<>Entry.LoadSignature) or (s1<>'') then
       begin
        lc:=Entry.LoadCount;
        Entry.Lock;
        try
          //check for multiple threads that were waiting for lock
          if Entry.LoadCount=lc then
           begin
            Entry.Release;
            if GetFileSignature(fn1)<>'' then
             begin
              //switcheroo
              if not(DeleteFile(PChar(fn))) then MoveFile(PChar(fn),PChar(fn+'.bak'));
              if not(MoveFile(PChar(fn1),PChar(fn))) then
                raise Exception.Create('MoveFile xxu failed: '+SysErrorMessage(GetLastError));
             end;
            //Entry.CheckLibrary will do the re-loading on page load
           end;
        finally
          Entry.Unlock;
        end;
       end;
     end;
    Result:=true;
  except
    on e:Exception do
     begin
      em:=HTMLEncode(UTF8Encode('AutoUpdate failed ['+e.ClassName+'] '+e.Message));
      Context_SendHTML(Context,PUTF8Char(em));
      Result:=false;//caller raises EXxmProjectCheckFailed
     end;
  end;
end;

end.
