unit xxmUtilities;

interface

uses Windows, SysUtils, Classes;

procedure ListFilesInPath(FileList:TStringList;Path:string);
function IsReservedWord(x:string):boolean;
function Signature(Path:string):string;
function GetFileSize(Path:string):integer;
function GetSelfPath:string;

const
  Hex:array[0..15] of char='0123456789ABCDEF';
  XxmProjectFileName='Web.xxmp';
  XxmModuleExtension='.xxl';
  XxmProtoExtension='.proto.pas';
  DelphiProjectExtension='.dpr';
  DelphiExtension='.pas';
  ProjectLogExtension='.log';
  SignaturesExtension='.~db';

  ProtoProjectDpr='Web.dpr';
  ProtoProjectMask='Web.*';
  ProtoProjectPas='xxmp.pas';
  ProtoDirectory='proto';
  SourceDirectory='src';

type
  TXxmFileType=(
    ftPage,
    ftInclude,
    ftProject,
    //add new here
    ft_Unknown
  );

const
  XxmFileExtension:array[TXxmFileType] of string=(
    'xxm',
    'xxmi',
    'xxmp',
    //add new here
    ''
  );

implementation

uses Registry;

procedure ListFilesInPath(FileList:TStringList;Path:string);
var
  Dirs:TStringList;
  fh:THandle;
  fd:TWin32FindData;
  CDir,s:string;
  i:integer;
  FirstLevel,AbandonDirectory:boolean;
  ft:TXxmFileType;
begin
  Dirs:=TStringList.Create;
  try
    Dirs.Add('');
    FirstLevel:=true;

    while not(Dirs.Count=0) do
     begin
      CDir:=Dirs[0];
      AbandonDirectory:=false;

      Dirs.Delete(0);
      fh:=FindFirstFile(PChar(Path+CDir+'*.*'),fd);
      if fh=INVALID_HANDLE_VALUE then RaiseLastOSError;
      repeat

       if not(string(fd.cFileName)='.') and not(string(fd.cFileName)='..') then
        begin

         if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
          begin
           s:=fd.cFileName;
           i:=Length(s);
           while not(i=0) and not(s[i]='.') do dec(i);
           s:=LowerCase(Copy(s,i+1,Length(s)-i));

           ft:=TXxmFileType(0);
           while not(ft=ft_Unknown) and not(s=XxmFileExtension[ft]) do inc(ft);

           case ft of

             ftPage,ftInclude:
               FileList.Add(CDir+fd.cFileName);

             ftProject:
               if FirstLevel then
                begin
                 //create TXxmWebProject?
                end
               else
                begin
                 //warning! sub-project, skip directory
                 AbandonDirectory:=true;
                 for i:=Dirs.Count-1 downto 0 do
                   if Copy(Dirs[i],1,Length(CDir))=CDir then Dirs.Delete(i);
                 for i:=FileList.Count-1 downto 0 do
                   if Copy(FileList[i],1,Length(CDir))=CDir then FileList.Delete(i);

                 //TODO: queue secondary process directory?
                end;

             //add new here

             //else?

           end;

          end
         else
           Dirs.Add(CDir+fd.cFileName+PathDelim);

        end;

      until not(FindNextFile(fh,fd)) or AbandonDirectory;
      Windows.FindClose(fh);
      FirstLevel:=false;
     end;


  finally
    Dirs.Free;
  end;
end;


function IsReservedWord(x:string):boolean;
const
  ResWordsCount=65;
  ResWords:array[0..ResWordsCount-1] of string=(
    'and', 'array', 'as', 'asm',
    'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div',
    'do', 'downto', 'else', 'end',
    'except', 'exports', 'file', 'finalization',
    'finally', 'for', 'function', 'goto',
    'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is',
    'label', 'library', 'mod', 'nil',
    'not', 'object', 'of', 'or',
    'out', 'packed', 'procedure', 'program',
    'property', 'raise', 'record', 'repeat',
    'resourcestring', 'set', 'shl', 'shr',
    'string', 'then', 'threadvar', 'to',
    'try', 'type', 'unit', 'until',
    'uses', 'var', 'while', 'with',
    'xor'
  );
  ResWordMaxLength:array['A'..'Z'] of byte=(5,5,11,13,7,12,4,0,14,0,0,7,3,3,6,9,0,14,6,9,5,3,5,3,0,0);
var
  c:char;
  y:string;
  i:integer;
begin
  //assert not(x='')
  c:=UpCase(x[1]);
  //skip anything longer than longest word
  if not(c in ['A'..'B']) or (Length(x)>ResWordMaxLength[c]) then Result:=false else
   begin
    y:=LowerCase(x);
    i:=0;
    while (i<ResWordsCount) and not(y=ResWords[i]) do inc(i);
    Result:=i<ResWordsCount;
   end;
end;

function Signature(Path:string):string;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  fh:=FindFirstFile(PChar(Path),fd);
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

function GetFileSize(Path:string):integer;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  fh:=FindFirstFile(PChar(Path),fd);
  if fh=INVALID_HANDLE_VALUE then Result:=-1 else
   begin
    //assert(fd.nFileSizeHigh=0
    Result:=fd.nFileSizeLow;
    Windows.FindClose(fh);
   end;
end;

function GetSelfPath:string;
var
  i:integer;
begin
  SetLength(Result,$400);
  SetLength(Result,GetModuleFileName(HInstance,PChar(Result),$400));
  i:=Length(Result);
  while not(i=0) and not(Result[i]=PathDelim) do dec(i);
  Result:=Copy(Result,1,i);
  if Copy(Result,1,4)='\\?\' then Result:=Copy(Result,5,Length(Result)-4);
end;

end.
