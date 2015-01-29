unit xxmCommonUtils;

interface

uses SysUtils, Classes;

function RFC822DateGMT(dd: TDateTime): string;
function GetFileModifiedDateTime(const FilePath:AnsiString;
  var FileSize:Int64):TDateTime;
function GetFileSignature(const Path:AnsiString):AnsiString;

type
  TOwningHandleStream=class(THandleStream)
  public
    destructor Destroy; override;
  end;

implementation

uses Windows;

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

function GetFileModifiedDateTime(const FilePath:AnsiString;
  var FileSize:Int64):TDateTime;
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

function GetFileSignature(const Path:AnsiString):AnsiString;
var
  fh:THandle;
  fd:TWin32FindDataA;
begin
  fh:=FindFirstFileA(PAnsiChar(Path),fd);
  if fh=INVALID_HANDLE_VALUE then Result:='' else
   begin
    //assert(fd.nFileSizeHigh=0
    Result:=
      IntToHex(fd.ftLastWriteTime.dwHighDateTime,8)+
      IntToHex(fd.ftLastWriteTime.dwLowDateTime,8)+'_'+
      IntToStr(fd.nFileSizeLow);
    Windows.FindClose(fh);
   end;
end;

{ TOwningHandleStream }

destructor TOwningHandleStream.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

end.
 