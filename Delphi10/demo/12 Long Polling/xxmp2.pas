unit xxmp2;

{
  xxm Module Management Unit

This is a default xxm Module Management Unit.
You are free to change this for your project.
(It's advised to delete this comment block on the generated xxmp2.pas.)
Use XxmPage to process URL's as a request is about to start.
(Be careful with sending content from there though!)
It is advised to link each request to a session here, if you want session management.
(See an example xxmSession.pas in the include folder.)

  $Rev: 530 $ $Date: 2025-01-29 00:41:44 +0100 (wo, 29 jan 2025) $
}

interface

uses System.SysUtils, xxm2;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

procedure XxmClearContext(Project: PxxmProject; Context: CxxmContext); stdcall;

function Demo12CheckEvent(Project: PxxmProject; EventKey: PUTF8Char;
  var CheckIntervalMS: NativeUInt): boolean; stdcall;

type
  TDemo12ContextData=class(TObject)
  public
    Counter:integer;
  end;

exports
  XxmInitialize,
  XxmPage,
  XxmFragment,
  XxmClearContext;

implementation

uses System.WideStrUtils
  {$I src/xxmFMap0.inc}
  ;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;
begin
  xxm:=xxm2;
  //XxmProjectName:=AProjectName;

  //see also Default.xxm
  //xxm.Context_RegisterEvent(nil,'demo12',Demo12CheckEvent,250,1000,'Next.xxmi',[],'Drop.xxmi',[]);

  Result:=nil;
end;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
var
  a:UTF8String;
  r:pointer;
begin
  //SetSession(Context);
  a:=UTF8LowerCase(Address);
  {$I src/xxmFMap1.inc}
  Result:=CxxmFragment(r);
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

procedure XxmClearContext(Project: PxxmProject; Context: CxxmContext); stdcall;
var
  cd:TDemo12ContextData;
begin
  cd:=Context.Data;
  if cd<>nil then cd.Free;
end;

var
  CheckedLast:cardinal;

function Demo12CheckEvent(Project: PxxmProject; EventKey: PUTF8Char;
  var CheckIntervalMS: NativeUInt): boolean; stdcall;
var
  th,tm,ts,tz:word;
  i,j:cardinal;
begin
  //EventKey='demo12' since only using this event key for this demo

  DecodeTime(Now,th,tm,ts,tz);
  i:=th+tm+ts;
  if CheckedLast=i then Result:=false else
   begin
    CheckedLast:=i;
    j:=2;
    //TODO: while (j<=isqtr(j))
    while (j<i) and ((i mod j)<>0) do inc(j);
    Result:=j=i;
   end;

end;

end.
