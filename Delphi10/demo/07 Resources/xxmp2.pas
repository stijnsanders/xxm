unit xxmp2;

{
  xxm Module Management Unit
  demo: 07 Resources

Please note this xxmp.pas was first created from the xxmp.pas template,
but has been modified to check incoming requests for files that are stored in the  module's resources.
}

interface

uses System.SysUtils, xxm2;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

exports
  XxmInitialize,
  XxmPage,
  XxmFragment;

implementation

uses System.WideStrUtils, WinApi.Windows, System.Classes
  {$I src/xxmFMap0.inc}
  ;

{ xxmFilesFromResIndex }

const
  RT_XXMRESFILE=MakeIntResource(999);
  RT_XXMRESINDEX=MakeIntResource(998);

var
  xxmFilesFromResIndex:TStringList;
  xxmResModSince:UTF8String;

procedure LoadFilesFromResIndex;
var
  r:TResourceStream;
begin
  xxmFilesFromResIndex:=TStringList.Create;
  r:=TResourceStream.Create(HInstance,'INDEX',RT_XXMRESINDEX);
  //TODO: TDecompressionStream
  try
    //into a memory stream first because TStringList.LoadFromStream calls TDecompressionStream.Size
    xxmFilesFromResIndex.LoadFromStream(r);
  finally
    r.Free;
  end;
end;

function UTCNow:TDateTime;
var
  st:TSystemTime;
begin
  GetSystemTime(st);
  Result:=
    EncodeDate(st.wYear,st.wMonth,st.wDay)+
    EncodeTime(st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
end;

function RFC822DateUTC(dd: TDateTime): string;
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
  FmtStr(Result, '%s, %d %s %d %.2d:%.2d:%.2d UTC',
    [Days[wd],d,Months[m],y,th,tm,ts]);
end;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;
begin
  xxm:=xxm2;
  //XxmProjectName:=AProjectName;
  LoadFilesFromResIndex;
  xxmResModSince:=UTF8String(RFC822DateUTC(UTCNow));
  Result:=nil;
end;

procedure XxmNoChange(Context: CxxmContext;
  const Values: array of Variant;
  const Objects: array of pointer); stdcall;
begin
  Context.SetStatus(304,'Not Modified');
  //no content
end;

procedure XxmFileFromRes(Context: CxxmContext;
  const Values: array of Variant;
  const Objects: array of pointer); stdcall;
var
  rn,rt:string;
  r:TResourceStream;
begin
  rn:=string(Context['rn'].Value);
  //TODO: Context.ContentType:= (from resources index?)
  rt:=Copy(rn,Length(rn)-3,3);
  if rt='png' then Context.ContentType:='image/png' else
  if rt='css' then Context.ContentType:='text/css' else
    Context.ContentType:='application/octet-stream';//?
  r:=TResourceStream.Create(HInstance,rn,RT_XXMRESFILE);
  //TODO: TDecompressionStream
  try
    Context.ResponseHeader['Content-Length']:=UTF8String(IntToStr(r.Size));
    Context.SendStream(r);
  finally
    r.Free;
  end;
end;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
var
  a,rn:UTF8String;
  r:pointer;
begin
  //SetSession(Context);
  a:=UTF8LowerCase(Address);

  if Copy(a,1,4)='res/' then
   begin
    rn:=UTF8String(xxmFilesFromResIndex.Values[string(Copy(a,5,Length(a)-4))]);
    if rn='' then
      Result:=nil
    else
      //TODO: LastMod from resource index?
      if Context.RequestHeader['If-Modified-Since']=xxmResModSince then
        Result:=@XxmNoChange
      else
       begin
        Context.ResponseHeader['Last-Modified']:=xxmResModSince;
        xxm.Context_Add_Parameter(Context,'RES','rn',PUTF8Char(rn));
        Result:=@XxmFileFromRes;
       end;
   end
  else
   begin

    {$I src/xxmFMap1.inc}
    Result:=CxxmFragment(r);

   end;
end;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
var
  a:UTF8String;
  r:pointer;
begin
  a:=UTF8LowerCase(Address);
  {$I src/xxmFMap2.inc}
  Result:=CxxmFragment(r);
end;

end.
