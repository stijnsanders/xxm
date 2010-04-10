unit xxmAutoUpdate;

interface

uses xxm, xxmPReg;

function AutoUpdate(pce:TXxmProjectEntry;
  Context:IXxmContext; ProjectName:WideString):boolean;

implementation

uses Windows, SysUtils;

var
  UpdateLock:TRTLCriticalSection;

function AutoUpdate(pce:TXxmProjectEntry;
  Context:IXxmContext; ProjectName:WideString):boolean;
var
  fn,fn1:AnsiString;
  tc:cardinal;
  i:integer;
const
  NoNextUpdateAfter=1000;//TODO: setting!
begin
  Result:=true;//default
  tc:=GetTickCount;
  if (tc-pce.LastCheck)>NoNextUpdateAfter then
   begin
    fn:=pce.ModulePath;//force get from registry outside of lock
    EnterCriticalSection(UpdateLock);
    try
      //again for those that waited on lock
      if (GetTickCount-pce.LastCheck)>NoNextUpdateAfter then
       begin
        i:=Length(fn);
        while not(i=0) and not(fn[i]='.') do dec(i);
        fn1:=Copy(fn,1,i-1)+'.xxu';
        if FileExists(fn1) then
         begin
          pce.Release;
          if not(DeleteFileA(PAnsiChar(fn))) then MoveFileA(PAnsiChar(fn),PAnsiChar(fn+'.bak'));
          if not(MoveFileA(PAnsiChar(fn1),PAnsiChar(fn))) then
           begin
            Result:=false;
            Context.Send('AutoUpdate failed "'+fn1+'"'#13#10+
              SysErrorMessage(GetLastError));
           end;
         end;
        pce.LastCheck:=GetTickCount;
       end;
    finally
      LeaveCriticalSection(UpdateLock);
    end;
   end;
end;

initialization
  InitializeCriticalSection(UpdateLock);
finalization
  DeleteCriticalSection(UpdateLock);

end.
