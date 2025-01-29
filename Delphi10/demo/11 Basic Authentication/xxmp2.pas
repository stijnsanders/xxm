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

exports
  XxmInitialize,
  XxmPage,
  XxmFragment;

implementation

uses System.WideStrUtils
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
  //SetSession(Context);

  
  {// to require authentication on the entire site, 
   // create a file "AuthReqMsg.xxm" that contains "<h1>Authentication Required</h1>"
   // and use this code:
  
  if Context.ContextString(csAuthUser)='' then
   begin
    Context.SetStatus(401,'Authentication Required');
    Context.ResponseHeader['WWW-Authenticate']:='Basic realm="demo"';
    Result:=@authreqmsg_xxm1.build;
   end
  else
   begin
  }

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
