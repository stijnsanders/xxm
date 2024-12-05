unit xxmProtoMain;

interface

uses SysUtils, xxm2;

function XxmInitialize(APILevel:NativeUInt;xxm2:Pxxm2;
  AProjectName:PUTF8Char):PxxmProject; stdcall;

function XxmPage(Project:PxxmProject;Context:PxxmContext;
  Address:PUTF8Char):CxxmFragment; stdcall;

exports
  XxmInitialize,
  XxmPage;

implementation

uses xxmPage1, xxmPage2;

function XxmInitialize(APILevel:NativeUInt;xxm2:Pxxm2;
  AProjectName:PUTF8Char):PxxmProject; stdcall;
begin
  xxm:=xxm2;
  //store/check AProjectName?
  Result:=nil;
end;

function XxmPage(Project:PxxmProject;Context:PxxmContext;
  Address:PUTF8Char):CxxmFragment; stdcall;
var
  n:UTF8String;
  p:pointer;
begin
  n:=Address;
  if n='' then p:=@page1 else
  if n='test.xxm' then p:=@page2 else
    p:=nil;
  Result:=CxxmFragment(p);
end;

end.
