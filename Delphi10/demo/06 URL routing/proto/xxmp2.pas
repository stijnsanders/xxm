unit xxmp2;

{
  xxm Module Management Unit
  demo: 06 URL routing

This xxmp2.pas template has been adapted to use the FRegRouting unit instead of the default fragment registry.
See GetPageAndParameters for how request addresses are mapped onto page procedures to handle the request.
}

interface

uses System.SysUtils, xxm2;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

var
  XxmProjectName: UTF8String;

exports
  XxmInitialize,
  XxmPage,
  XxmFragment;

implementation

uses FRegRouting;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;
begin
  xxm:=xxm2;
  XxmProjectName:=AProjectName;
  Result:=nil;
end;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
begin
  Result:=GetPageAndParameters(Context,Address);
end;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;
begin
  Result:=GetIncludeFragment(Address);
end;

end.
