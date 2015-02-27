unit xxmAutoUpdate;

interface

uses xxm, xxmPReg;

function AutoUpdate(Entry :TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString): boolean;

implementation

uses Windows, SysUtils, xxmCommonUtils;

function AutoUpdate(Entry: TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString): boolean;
var
  fn,fn1,s,s1:AnsiString;
  i,lc:integer;
const
  NoNextUpdateAfter=5000;//TODO: setting!
begin
  //fn:='['+ProjectName+']';
  try
    if cardinal(GetTickCount-Entry.LastCheck)>NoNextUpdateAfter then
     begin
      Entry.LastCheck:=GetTickCount;
      fn:=Entry.ModulePath;//force get from registry outside of lock
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
              if not(DeleteFileA(PAnsiChar(fn))) then MoveFileA(PAnsiChar(fn),PAnsiChar(fn+'.bak'));
              if not(MoveFileA(PAnsiChar(fn1),PAnsiChar(fn))) then
                raise Exception.Create('MoveFile xxu failed: '+SysErrorMessage(GetLastError));
              //TODO: non-xxmIsapi handlers could still have xxl locked? take xxu as update?
             end;
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
      Context.Send('AutoUpdate failed: '+e.Message);
      //output fn, projectname, selfversion?
      Result:=false;//caller raises EXxmAutoBuildFailed
     end;
  end;
end;

end.
