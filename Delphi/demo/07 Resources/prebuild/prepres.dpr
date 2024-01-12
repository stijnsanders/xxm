program prepres;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Classes;

var
  fh:THandle;
  fd:TWin32FindData;
  fn,rn:string;
  sl,sl1:TStringList;
  i,j,k,l:integer;
begin
  try
    //TODO: check for changes only
    //list files to include
    fh:=FindFirstFile('res\*.*',fd);
    if fh=INVALID_HANDLE_VALUE then RaiseLastOSError;
    sl:=TStringList.Create;
    sl1:=TStringList.Create;
    try
      sl1.Sorted:=true;
      repeat
        fn:=fd.cFileName;
        //TODO: list recursively
        if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
         begin
          //build resource name
          j:=1;
          k:=0;
          l:=Length(fn);
          SetLength(rn,l);
          for i:=1 to l do
            if fn[i] in ['0'..'9','A'..'Z','a'..'z'] then
             begin
              rn[j]:=fn[i];
              inc(j);
             end
            else
              inc(k);
          rn:=Copy(rn,1,j-1)+IntToStr(k);
          //TODO: TCompressionStream
          //list file
          sl.Add(rn+' 999 "res\\'+fn+'"');
          sl1.Add(fn+'='+rn);
          Writeln('res\'+fn+': '+IntToStr(l)+' bytes');
         end;
      until not FindNextFile(fh,fd);
      //save file index
      sl1.SaveToFile('Web_rc_index.dat');//TODO: TCompressionStream
      sl.Add('INDEX 998 "Web_rc_index.dat"');
      sl.SaveToFile('Web.rc');
    finally
      Windows.FindClose(fh);
      sl.Free;
      sl1.Free;
    end;
    //TODO: force Web.xxmp "<Switches>{$R" tag?
  except
    on e:Exception do
     begin
      Writeln('Exception occurred: '+e.ClassName);
      Writeln(e.Message);
      ExitCode:=1;
     end;
  end;
end.
