unit xxmAutoUpdate;

interface

uses xxm, xxmPReg;

function AutoUpdate(Entry :TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString): boolean;

implementation

uses Windows, SysUtils, xxmCommonUtils;

function AutoUpdate(Entry: TXxmProjectEntry; Context: IXxmContext; ProjectName: WideString): boolean;
var
  fn,fn1:AnsiString;
  i:integer;
const
  NoNextUpdateAfter=1000;//TODO: setting!
begin
  try
    if cardinal(GetTickCount-Entry.LastCheck)>NoNextUpdateAfter then
     begin
      fn:=Entry.ModulePath;//force get from registry outside of lock
      i:=Length(fn);
      while (i<>0) and (fn[i]<>'.') do dec(i);
      fn1:=Copy(fn,1,i-1)+'.xxu';
      if (GetFileSignature(fn)<>Entry.LoadSignature) or (GetFileSignature(fn1)<>'') then
       begin
        Entry.Lock;
        try
          //check again for threads that were waiting for lock
          if cardinal(GetTickCount-Entry.LastCheck)>NoNextUpdateAfter then
           begin
            Entry.LastCheck:=GetTickCount;
            Entry.Release;
            if GetFileSignature(fn1)<>'' then
             begin
              //switcheroo
              if not(DeleteFileA(PAnsiChar(fn))) then MoveFileA(PAnsiChar(fn),PAnsiChar(fn+'.bak'));
              if not(MoveFileA(PAnsiChar(fn1),PAnsiChar(fn))) then RaiseLastOSError;
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
      Result:=false;//caller raises EXxmAutoBuildFailed
     end;
  end;
end;

end.
