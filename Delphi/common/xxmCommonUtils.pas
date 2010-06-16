unit xxmCommonUtils;

interface

function RFC822DateGMT(dd: TDateTime): AnsiString;
function GetFileModifiedDateTime(FilePath:AnsiString;var FileSize:Int64):TDateTime;

implementation

uses Windows, SysUtils;

function RFC822DateGMT(dd: TDateTime): AnsiString;
const
  Days:array [1..7] of AnsiString=
    ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  Months:array [1..12] of AnsiString=
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
//  SignStr:array[boolean] of AnsiString=('-','+');
var
  dg:TDateTime;
  y,m,d,wd,th,tm,ts,tms:Word;
  tz:TIME_ZONE_INFORMATION;
begin
  GetTimeZoneInformation(tz);
  dg:=dd+tz.Bias/1440;
  DecodeDateFully(dg,y,m,d,wd);
  DecodeTime(dg,th,tm,ts,tms);
  FmtStr(Result, '%s, %d %s %d %.2d:%.2d:%.2d GMT',
    [Days[wd],d,Months[m],y,th,tm,ts]);
end;

function GetFileModifiedDateTime(FilePath:AnsiString;var FileSize:Int64):TDateTime;
var
  fh:THandle;
  fd:TWin32FindDataA;
  st:TSystemTime;
begin
  if FilePath='' then Result:=0 else
   begin
    fh:=FindFirstFileA(PAnsiChar(FilePath),fd);
    if fh=INVALID_HANDLE_VALUE then Result:=0 else
     begin
      Windows.FindClose(fh);
      FileSize:=fd.nFileSizeHigh shl 32 or fd.nFileSizeLow;
      FileTimeToSystemTime(fd.ftLastWriteTime,st);
      Result:=SystemTimeToDateTime(st);
     end;
   end;
end;

end.
 