unit xxmUtilities;

interface

uses Windows, SysUtils, Classes;

procedure ListFilesInPath(FileList:TStringList;Path:AnsiString);
function GetInternalIdentifier(FileName:AnsiString;var cPathIndex,fExtIndex,fPathIndex:integer):AnsiString;
function GetFileSize(Path:AnsiString):integer;
function GetSelfPath:AnsiString;

const
  Hex:array[0..15] of char='0123456789ABCDEF';
  XxmProjectFileName='Web.xxmp';
  XxmModuleExtension='.xxl';
  XxmProtoExtension='.proto.pas';
  DelphiProjectExtension='.dpr';
  DelphiExtension='.pas';
  ProjectLogExtension='.log';
  SignaturesExtension='.~db';
  LinesMapExtension='.~ln';

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
  XxmFileExtension:array[TXxmFileType] of AnsiString=(
    '.xxm',
    '.xxmi',
    '.xxmp',
    //add new here
    ''
  );

implementation

uses Registry;

procedure ListFilesInPath(FileList:TStringList;Path:AnsiString);
var
  Dirs:TStringList;
  fh:THandle;
  fd:TWin32FindDataA;
  CDir,s:AnsiString;
  i:integer;
  FirstLevel,AbandonDirectory:boolean;
  ft:TXxmFileType;                              
begin
  Dirs:=TStringList.Create;
  try
    Dirs.Add('');
    FirstLevel:=true;

    while Dirs.Count<>0 do
     begin
      CDir:=Dirs[0];
      AbandonDirectory:=false;

      Dirs.Delete(0);
      fh:=FindFirstFileA(PAnsiChar(Path+CDir+'*.*'),fd);
      if fh=INVALID_HANDLE_VALUE then RaiseLastOSError;
      repeat

        if (AnsiString(fd.cFileName)<>'.') and (AnsiString(fd.cFileName)<>'..') then
         begin
     
          if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
           begin
            s:=fd.cFileName;
            i:=Length(s);
            while (i<>0) and (s[i]<>'.') do dec(i);
            s:=LowerCase(Copy(s,i,Length(s)-i+1));
     
            ft:=TXxmFileType(0);
            while (ft<>ft_Unknown) and (s<>XxmFileExtension[ft]) do inc(ft);
     
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

      until not(FindNextFileA(fh,fd)) or AbandonDirectory;
      Windows.FindClose(fh);
      FirstLevel:=false;
     end;

  finally
    Dirs.Free;
  end;
end;


function IsReservedWord(x:AnsiString):boolean;
const
  ResWordsCount=65;
  ResWords:array[0..ResWordsCount-1] of AnsiString=(
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
  y:AnsiString;
  i:integer;
begin
  //assert x<>''
  c:=char(UpCase(x[1]));
  //skip anything longer than longest word
  if not(c in ['A'..'B']) or (Length(x)>ResWordMaxLength[c]) then Result:=false else
   begin
    y:=LowerCase(x);
    i:=0;
    while (i<ResWordsCount) and (y<>ResWords[i]) do inc(i);
    Result:=i<ResWordsCount;
   end;
end;

function GetFileSize(Path:AnsiString):integer;
var
  fh:THandle;
  fd:TWin32FindDataA;
begin
  fh:=FindFirstFileA(PAnsiChar(Path),fd);
  if fh=INVALID_HANDLE_VALUE then Result:=-1 else
   begin
    //assert(fd.nFileSizeHigh=0
    Result:=fd.nFileSizeLow;
    Windows.FindClose(fh);
   end;
end;

function GetSelfPath:AnsiString;
var
  i:integer;
begin
  SetLength(Result,$400);
  SetLength(Result,GetModuleFileNameA(HInstance,PAnsiChar(Result),$400));
  i:=Length(Result);
  while (i<>0) and (Result[i]<>PathDelim) do dec(i);
  Result:=Copy(Result,1,i);
  if Copy(Result,1,4)='\\?\' then Result:=Copy(Result,5,Length(Result)-4);
end;

function GetInternalIdentifier(FileName:AnsiString;var cPathIndex,fExtIndex,fPathIndex:integer):AnsiString;
var
  i:integer;
begin
  Result:='';
  cPathIndex:=1;
  fPathIndex:=0;
  fExtIndex:=0;
  for i:=1 to Length(FileName) do
    case FileName[i] of
      '\':
       begin
        Result:=Result+'__';
        cPathIndex:=Length(Result)+1;
        fPathIndex:=i;
       end;
      '.':
       begin
        Result:=Result+'_';
        fExtIndex:=i;
       end;
      'A'..'Z':
        Result:=Result+AnsiChar(byte(FileName[i])+$20);
      '0'..'9','_','a'..'z':
        Result:=Result+FileName[i];
      else
        Result:=Result+'x'+Hex[byte(FileName[i]) shr 4]+Hex[byte(FileName[i]) and $F];
    end;
  //assert CID<>''
  if not(char(Result[1]) in ['A'..'Z','a'..'z']) then Result:='x'+Result;
  if IsReservedWord(Result) then
    if LowerCase(Result)='or' then
      Result:='x_'+Result
    else
      Result:='x'+Result;
end;

end.
