unit xxmp2;

{
  xxm Module Management Unit
  demo: 03 Data

This file was generated using the xxmp2.pas template, then added as described in "demo/02 Session"
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

uses System.WideStrUtils, xxmSession
  {$I src/xxmFMap0.inc}
  ;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;
begin
  xxm:=xxm2;
  //XxmProjectName:=AProjectName;
  Result:=nil;
end;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
var
  a:UTF8String;
  r:pointer;  
begin
  SetSession(Context);
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

end.
