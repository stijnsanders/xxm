unit xxmp2;

{
  xxm Module Management Unit
  demo: 02 Session

Please note this xxmp2.pas was first created from the xxmp2.pas template,
but has been modified to include xxmSession in the uses clause (implementation section),
and to call SetSession(Context); in XxmPage, linking this request either to a new session (using Context.SessionID)
or to an existing session that was created for the SessionID.
}

interface

uses System.SysUtils, xxm2;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;

function XxmPage(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

function XxmFragment(Project: PxxmProject; Context: CxxmContext;
  Address: PUTF8Char): CxxmFragment; stdcall;

procedure XxmReleasingContexts(Project: PxxmProject); stdcall;

var
  AbortAll:boolean;

exports
  XxmInitialize,
  XxmPage,
  XxmFragment,
  XxmReleasingContexts;

implementation

uses System.WideStrUtils, xxmSession
  {$I src/xxmFMap0.inc}
  ;

function XxmInitialize(APILevel: NativeUInt; xxm2: Pxxm2;
  AProjectName: PUTF8Char): PxxmProject; stdcall;
begin
  xxm:=xxm2;
  //XxmProjectName:=AProjectName;
  AbortAll:=false;
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

procedure XxmReleasingContexts(Project: PxxmProject); stdcall;
begin
  AbortAll:=true;
end;

end.
