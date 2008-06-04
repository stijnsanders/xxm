unit xxmCommonUtils;

interface

function RFC822DateGMT(dd: TDateTime): string;
function GetFileModifiedDateTime(FilePath:string):TDateTime;

implementation

uses Windows, SysUtils;

function RFC822DateGMT(dd: TDateTime): string;
const
  Days:array [1..7] of string=
    ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  Months:array [1..12] of string=
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
//  SignStr:array[boolean] of string=('-','+');
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

function GetFileModifiedDateTime(FilePath:string):TDateTime;
var
  fh:THandle;
  fd:TWin32FindData;
  st:TSystemTime;
begin
  if FilePath='' then Result:=0 else
   begin
    fh:=FindFirstFile(PChar(FilePath),fd);
    if fh=INVALID_HANDLE_VALUE then Result:=0 else
     begin
      Windows.FindClose(fh);
      FileTimeToSystemTime(fd.ftLastWriteTime,st);
      Result:=SystemTimeToDateTime(st);
     end;
   end;
end;

end.
 