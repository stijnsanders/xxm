unit xxmPReg;

{
  ATTENTION: placeholder xxmPReg.pas **only** to define a number of things
  xxmConv doesn't have but xxm*Dev handlers do
}

interface

uses SysUtils, jsonDoc;

type
  EXxmProjectNotFound=class(Exception);

  XxmProjectRegistry=class(TObject)
    class function GetProjectData(const Name:string):IJSONDocument;
  end;

var
  XxmProjectRegDataFilePath: string;

implementation

uses Classes;

//TODO: deduplicate with xxmProject1
function LoadJSON(const FilePath:string):IJSONDocument;
var
  f:TFileStream;
  i:integer;
  s:AnsiString;
  w:WideString;
begin
  Result:=JSON;
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    i:=f.Size;
    SetLength(s,i);
    if f.Read(s[1],i)<>i then RaiseLastOSError;
    if (i>=3) and (s[1]=#$EF) and (s[2]=#$BB) and (s[3]=#$BF) then
      Result.Parse(UTF8ToWideString(Copy(s,4,i-3)))
    else
    if (i>=2) and (s[1]=#$FF) and (s[2]=#$FE) then
     begin
      SetLength(w,(i div 2)-1);
      Move(s[3],w[1],(i*2)-1);
      Result.Parse(w);
     end
    else
      Result.Parse(WideString(s));
  finally
    f.Free;
  end;
end;

{ XxmProjectRegistry }

class function XxmProjectRegistry.GetProjectData(
  const Name: string): IJSONDocument;
begin
  if XxmProjectRegDataFilePath='' then
    Result:=nil
  else
    Result:=LoadJSON(XxmProjectRegDataFilePath);
end;

initialization
  XxmProjectRegDataFilePath:='';
end.
