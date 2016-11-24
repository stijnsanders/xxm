unit xxmWebProject;

interface

uses Windows, SysUtils, Classes, jsonDoc, xxmPageParse;

type
  TXxmWebProjectOutput=procedure(const Msg:AnsiString);

  TXxmWebProject=class(TObject)
  private
    Data:IJSONDocument;
    DataStartSize:integer;
    DataFileName,FProjectName,FRootFolder,FSrcFolder,
    FHandlerPath,FProtoPathDef,FProtoPath:AnsiString;
    Modified,DoLineMaps:boolean;
    Signatures:TStringList;
    FOnOutput:TXxmWebProjectOutput;
    FParserValues:TXxmPageParserValueList;

    function KeyText(const Key:WideString):AnsiString;

    function ReadString(const FilePath:AnsiString):AnsiString;
    procedure BuildOutput(const Msg:AnsiString);
  public

    constructor Create(const SourcePath:AnsiString;
      OnOutput:TXxmWebProjectOutput; CanCreate:boolean);
    destructor Destroy; override;

    function CheckFiles(Rebuild:boolean;ExtraFields:TStrings):boolean;
    function GenerateProjectFiles(Rebuild:boolean;ExtraFields:TStrings):boolean;
    function ResolveErrorLines(const BuildOutput:AnsiString):AnsiString;

    function Compile:boolean;
    procedure Update;

    property ProjectName:AnsiString read FProjectName;
    property RootFolder:AnsiString read FRootFolder;
    property ProjectFile:AnsiString read DataFileName;

    property SrcFolder:AnsiString read FSrcFolder write FSrcFolder;
    property ProtoFolder:AnsiString read FProtoPath write FProtoPath;
    property LineMaps:boolean read DoLineMaps write DoLineMaps;
  end;

  EXxmWebProjectNotFound=class(Exception);
  EXxmWebProjectLoad=class(Exception);
  EXxmWebProjectCompile=class(Exception);

implementation

uses Variants, ComObj, xxmUtilities, xxmProtoParse, xxmCommonUtils,
  xxmConvertXML, MSXML2_TLB;

{  }

const
  DefaultParserValues:TXxmPageParserValueList=(
    (Code:'Context.Send(';EOLs:0),//pvSend
    (Code:');';EOLs:0),//pvSendClose
    (Code:'Context.SendHTML(';EOLs:0),//pvSendHTML
    (Code:');';EOLs:0),//pvSendHTMLClose
    (Code:'Context.Send(URLEncode([';EOLs:0),//pvURLEncode
    (Code:']));';EOLs:0),//pvURLEncodeClose
    (Code:'Extra1(';EOLs:0),(Code:');';EOLs:0),//pvExtra1
    (Code:'Extra2(';EOLs:0),(Code:');';EOLs:0),//pvExtra2
    (Code:'Extra3(';EOLs:0),(Code:');';EOLs:0),//pvExtra3
    (Code:'Extra4(';EOLs:0),(Code:');';EOLs:0),//pvExtra4
    (Code:'Extra5(';EOLs:0),(Code:');';EOLs:0),//pvExtra5
    //add new above
    (Code:'';EOLs:0)
  );

  ParserValueElement:array[TXxmPageParserValues] of string=(
    'SendOpen','SendClose',
    'SendHTMLOpen','SendHTMLClose',
    'URLEncodeOpen','URLEncodeClose',
    'Extra1Open','Extra1Close',
    'Extra2Open','Extra2Close',
    'Extra3Open','Extra3Close',
    'Extra4Open','Extra4Close',
    'Extra5Open','Extra5Close',
    ''
  );

//TODO: project defaults (folder defaults?)

{ TXxmWebProject }

const
  SXxmWebProjectNotFound='Web Project File not found for "__"';
  SXxmWebProjectLoad='Could not read "__"';

constructor TXxmWebProject.Create(const SourcePath: AnsiString;
  OnOutput:TXxmWebProjectOutput; CanCreate:boolean);
var
  v:OleVariant;
  i,j,l:integer;
  s:AnsiString;
  f:TFileStream;
  d:IJSONDocument;
  pv:TXxmPageParserValues;
begin
  inherited Create;
  Modified:=false;
  DoLineMaps:=true;
  FOnOutput:=OnOutput;
  FProjectName:='';

  //assert full expanded path
  //SourcePath:=ExpandFileName(SourcePath);

  i:=Length(SourcePath);
  while (i<>0) and (SourcePath[i]<>'.') do dec(i);
  if LowerCase(Copy(SourcePath,i,Length(SourcePath)-i+1))=XxmFileExtension[ftProject] then
   begin
    //project file specified
    while (i<>0) and (SourcePath[i]<>PathDelim) do dec(i);
    FRootFolder:=Copy(SourcePath,1,i);
    DataFileName:=Copy(SourcePath,i+1,Length(SourcePath)-i);
   end
  else
   begin
    //find
    DataFileName:=XxmProjectFileName;
    FRootFolder:=IncludeTrailingPathDelimiter(SourcePath);
    i:=Length(FRootFolder);
    while (i<>0) and not(FileExists(FRootFolder+DataFileName)) do
     begin
      dec(i);
      while (i<>0) and (FRootFolder[i]<>PathDelim) do dec(i);
      SetLength(FRootFolder,i);
     end;
    if i=0 then
      if CanCreate then
       begin
        //create empty project
        if DirectoryExists(SourcePath) then
          FRootFolder:=IncludeTrailingPathDelimiter(SourcePath)
        else
         begin
          i:=Length(SourcePath);
          while (i<>0) and (SourcePath[i]<>PathDelim) do dec(i);
          FRootFolder:=Copy(SourcePath,1,i);
         end;
        i:=Length(FRootFolder)-1;
        while (i<>0) and (FRootFolder[i]<>PathDelim) do dec(i);
        FProjectName:=Copy(FRootFolder,i+1,Length(FRootFolder)-i-1);
        s:='{name:"'+FProjectName+
          '",compileCommand:"dcc32 -U[[HandlerPath]]public'+
          ' -Q [[ProjectName]].dpr"}';
        f:=TFileStream.Create(FRootFolder+DataFileName,fmCreate);
        try
          f.Write(s[1],Length(s));
        finally
          f.Free;
        end;
       end
      else
        raise EXxmWebProjectNotFound.Create(StringReplace(
          SXxmWebProjectNotFound,'__',SourcePath,[]));
   end;

  FHandlerPath:=GetSelfPath;
  FProtoPathDef:=FHandlerPath+ProtoDirectory+PathDelim;
  FSrcFolder:=FRootFolder+SourceDirectory+PathDelim;

  f:=TFileStream.Create(FRootFolder+DataFileName,fmOpenRead);
  try
    DataStartSize:=f.Size;
    SetLength(s,DataStartSize);
    f.Read(s[1],DataStartSize);
  finally
    f.Free;
  end;

  //TRANSITIONAL: convert
  if (s<>'') and (s[1]='<') then
   begin
    s:=ConvertProjectFile(s);
    //CopyFile(,'.bak')?
    if not(CopyFile(PChar(FRootFolder+DataFileName),PChar(FRootFolder+
      StringReplace(DataFileName,'.','_',[rfReplaceAll])+'.bak'),false)) then
      RaiseLastOSError;
    f:=TFileStream.Create(FRootFolder+DataFileName,fmCreate);
    try
      f.Write(s[1],Length(s));
    finally
      f.Free;
    end;

   end;

  Data:=JSON;
  Data.Parse(s);
  v:=Data['uuid'];
  if VarIsNull(v) then Data['uuid']:=CreateClassID;//other random info?

  v:=Data['name'];
  if VarIsNull(v) or (v='') then
   begin
    if FProjectName='' then
     begin
      i:=Length(DataFileName);
      while (i<>0) and (DataFileName[i]<>'.') do dec(i);
      FProjectName:=Copy(DataFileName,1,Length(DataFileName)-i-1);
     end;
    Data['name']:=FProjectName;
   end
  else
    FProjectName:=v;

  if DirectoryExists(FRootFolder+ProtoDirectory) then
    FProtoPath:=FRootFolder+ProtoDirectory+PathDelim
  else
    FProtoPath:=FProtoPathDef;

  FParserValues:=DefaultParserValues;
  d:=JSON(Data['parserValues']);
  if d<>nil then
   begin
    pv:=TXxmPageParserValues(0);
    while (pv<>pv_Unknown) do
     begin
      v:=d[ParserValueElement[pv]];
      if not(VarIsNull(v)) then
       begin
        s:=StringReplace(StringReplace(v,
          '$v',FParserValues[pv].Code,[rfReplaceAll]),
          '$d',FParserValues[pv].Code,[rfReplaceAll]);
        l:=Length(s);
        j:=0;
        for i:=1 to l-1 do if (s[i]=#13) and (s[i+1]=#10) then inc(j);
        FParserValues[pv].Code:=s;
        FParserValues[pv].EOLs:=j;
       end;
      inc(pv);
     end;
   end;
end;

destructor TXxmWebProject.Destroy;
begin
  //Update was here before
  Data:=nil;
  Signatures.Free;
  inherited;
end;

procedure TXxmWebProject.Update;
var
  f:TFileStream;
  fn,s:AnsiString;
begin
  if Modified then
   begin
    s:=AnsiString(Data.ToString);//TODO: UTF8
    //TODO: if Data.Dirty
    if DataStartSize<>Length(s) then
     begin
      Data['lastModified']:=
        FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now);//timezone?
      f:=TFileStream.Create(FRootFolder+DataFileName,fmCreate);
      try
        f.Write(s[1],Length(s));
      finally
        f.Free;
      end;
      Modified:=false;
     end;

    //save signatures
    try
      fn:=FSrcFolder+SignaturesFileName;
      SetFileAttributesA(PAnsiChar(fn),0);
      Signatures.SaveToFile(fn);
      //SetFileAttributesA(PAnsiChar(fn),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
    except
      //silent?
    end;

   end;
end;

function TXxmWebProject.CheckFiles(Rebuild:boolean;ExtraFields:TStrings): boolean;
var
  p:TXxmProtoParser;
  q:TXxmPageParser;
  m:TXxmLineNumbersMap;
  DataFiles,d:IJSONDocument;
  e:IJSONEnumerator;
  v:OleVariant;
  fn,fnu,s,cid,uname,upath,uext:AnsiString;
  sl,sl1:TStringList;
  sl_i,i,cPathIndex,fExtIndex,fPathIndex:integer;
begin
  Result:=false;

  //TODO: setting autoaddfiles
  //TODO: autoremove files?

  Signatures:=TStringList.Create;
  try
    Signatures.LoadFromFile(FSrcFolder+SignaturesFileName);
  except
    //silent
  end;

  DataFiles:=JSON(Data['files']);
  if DataFiles=nil then
   begin
    DataFiles:=JSON;
    Data['files']:=DataFiles;
   end;

  p:=TXxmProtoParser.Create;
  q:=TXxmPageParser.Create(FParserValues);
  m:=TXxmLineNumbersMap.Create;
  try
    sl:=TStringList.Create;
    sl1:=TStringList.Create;
    try
      e:=JSONEnum(DataFiles);
      while e.Next do sl1.Add(e.Key);
      e:=nil;

      ListFilesInPath(sl,FRootFolder);

      for sl_i:=0 to sl.Count-1 do
       begin
        fn:=sl[sl_i];
        cid:=GetInternalIdentifier(fn,cPathIndex,fExtIndex,fPathIndex);
        d:=JSON(DataFiles[cid]);
        if d=nil then
         begin
          d:=JSON;
          DataFiles[cid]:=d;
         end;
        i:=sl1.IndexOf(cid);
        if i<>-1 then sl1.Delete(i);
        //fn fit for URL
        fnu:=StringReplace(fn,PathDelim,'/',[rfReplaceAll]);
        d['path']:=fnu;
        //pascal unit name
        upath:=VarToStr(d['unitPath']);
        uname:=VarToStr(d['unitName']);
        if fExtIndex=0 then uext:='' else
          uext:=Copy(fn,fExtIndex+1,Length(fn)-fExtIndex);

        if uname='' then
         begin
          //unique counter for project
          uname:=Copy(cid,cPathIndex,Length(cid)-cPathIndex+1);
          if not(uname[1] in ['A'..'Z','a'..'z']) then uname:='x'+uname;
          i:=0;
          repeat
            inc(i);
            s:=uname+IntToStr(i);
            v:=DataFiles[s];
          until VarIsNull(v);
          uname:=s;
          d['unitName']:=uname;
          Modified:=true;
         end;
        if upath='' then
         begin
          upath:=Copy(fn,1,fPathIndex);
          d['unitPath']:=upath;
         end;

        //TODO: setting no pas subdirs?

        //TODO: proto signature? (setting?)
        s:=GetFileSignature(FRootFolder+fn);
        if Rebuild or (Signatures.Values[uname]<>s) or not(
          FileExists(FSrcFolder+upath+uname+DelphiExtension)) then
         begin
          Signatures.Values[uname]:=s;
          Modified:=true;
          BuildOutput(':'+FProjectName+':'+fn+':'+uname+':'+cid+#13#10);

          try
            //TODO: alternate proto? either XML tag or default file.
            s:=FRootFolder+fn+XxmProtoExtension;
            if not(FileExists(s)) then s:=FProtoPath+uext+DelphiExtension;
            if not(FileExists(s)) then s:=FProtoPathDef+uext+DelphiExtension;
            p.Parse(ReadString(s),ExtraFields);
            q.Parse(ReadString(FRootFolder+fn));
            m.Clear;
            repeat
              m.MapLine(p.NextEOLs,0);
              case p.GetNext of
                ptProjectName:p.Output(FProjectName);
                ptProjectPath:p.Output(FRootFolder);
                ptProtoFile:p.Output(FProtoPath+uext+DelphiExtension);
                ptFragmentID:p.Output(cid);
                ptFragmentUnit:p.Output(uname);
                ptFragmentAddress:p.Output(fnu);
                ptUsesClause:p.Output(q.AllSectionsCheckComma(psUses,m));
                ptFragmentDefinitions:p.Output(q.AllSections(psDefinitions,m));
                ptFragmentHeader:p.Output(q.AllSections(psHeader,m));
                ptFragmentBody:p.Output(q.BuildBody(m));
                ptFragmentFooter:p.Output(q.AllSections(psFooter,m));
                pt_Unknown:
                  if not p.Done then p.Output(ExtraFields.Values[p.GetTagLabel]);
                //else raise?
              end;
            until p.Done;
            //m.MapLine(0,q.TotalLines);//map EOF?
            ForceDirectories(FSrcFolder+upath);
            p.Save(FSrcFolder+upath+uname+DelphiExtension);
            if DoLineMaps then
              m.Save(FSrcFolder+upath+uname+LinesMapExtension);
            if not Result then
              Signatures.Values[SignaturesUpdateReasonKey]:=uname;
            Result:=true;
          except
            on e:Exception do
             begin
              e.Message:=fn+':'+uname+':'+cid+#13#10+e.Message;
              raise;
             end;
          end;
         end;
       end;

      //delete missing files
      for sl_i:=0 to sl1.Count-1 do
       begin
        cid:=sl1[sl_i];
        d:=JSON(DataFiles[cid]);
        if d<>nil then
         begin
          //TODO: setting keep pas?
          uname:=VarToStr(d['unitName']);
          upath:=VarToStr(d['unitPath']);
          DeleteFile(FSrcFolder+upath+uname+DelphiExtension);
          DeleteFile(FSrcFolder+upath+uname+LinesMapExtension);
          //remove file tag
         end;
        DataFiles.Delete(cid);
        Modified:=true;
        if not Result then
          Signatures.Values[SignaturesUpdateReasonKey]:='<'+uname;
        Result:=true;
       end;
       
    finally
      sl.Free;
      sl1.Free;
    end;

    //check units
    e:=JSONEnum(Data['units']);
    while e.Next do
     begin
      d:=JSON(e.Value);
      uname:=e.Key;//VarToStr(d['unitName']);
      upath:=VarToStr(d['unitPath']);
      fn:=upath+uname+DelphiExtension;
      s:=GetFileSignature(FRootFolder+fn);
      if Signatures.Values[uname]<>s then
       begin
        Signatures.Values[uname]:=s;
        Modified:=true;
        if not Result then
          Signatures.Values[SignaturesUpdateReasonKey]:=uname;
        Result:=true;
       end;
     end;
    //missing? delete?

    //check resource files
    e:=JSONEnum(Data['resources']);
    while e.Next do
     begin
      d:=JSON(e.Value);
      fn:=StringReplace(VarToStr(e.Key),'/',PathDelim,[rfReplaceAll]);
      s:=GetFileSignature(FRootFolder+fn);
      uname:=':'+StringReplace(fn,'=','_',[rfReplaceAll]);
      if Signatures.Values[uname]<>s then
       begin
        Signatures.Values[uname]:=s;
        Modified:=true;
        if not Result then
          Signatures.Values[SignaturesUpdateReasonKey]:=uname;
        Result:=true;
       end;
     end;

    GenerateProjectFiles(Rebuild,ExtraFields);

  finally
    p.Free;
    q.Free;
    m.Free;
  end;
end;

function TXxmWebProject.GenerateProjectFiles(Rebuild:boolean;
  ExtraFields:TStrings):boolean;
var
  p:TXxmProtoParser;
  e:IJSONEnumerator;
  d:IJSONDocument;
  fh:THandle;
  fd:TWin32FindDataA;
  fn1,fn2,s:AnsiString;
  i:integer;
begin
  Result:=false;
  //project files
  fn1:=FSrcFolder+FProjectName+DelphiProjectExtension;
  fn2:=FRootFolder+ProtoProjectPas;
  if Modified or Rebuild or not(FileExists(fn1)) or not(FileExists(fn2)) then
   begin
    p:=TXxmProtoParser.Create;
    try
      //[[ProjectName]].dpr
      BuildOutput(FProjectName+DelphiProjectExtension+#13#10);
      s:=FProtoPath+ProtoProjectDpr;
      if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectDpr;
      p.Parse(ReadString(s),ExtraFields);
      repeat
        case p.GetNext of
          ptProjectName:p.Output(FProjectName);
          ptProjectPath:p.Output(FRootFolder);
          ptProtoFile:p.Output(FProtoPath+ProtoProjectDpr);
          ptIterateFragment:
           begin
            e:=JSONEnum(Data['files']);
            if e.Next then d:=JSON(e.Value) else d:=nil;
            p.IterateBegin(d<>nil);
           end;
          ptIterateInclude:
           begin
            e:=JSONEnum(Data['units']);
            if e.Next then d:=JSON(e.Value) else d:=nil;
            p.IterateBegin(d<>nil);
           end;
          ptFragmentUnit:p.Output(VarToStr(d['unitName']));
          ptFragmentPath:p.Output(StringReplace(
            VarToStr(d['unitPath']),'/',PathDelim,[rfReplaceAll]));
          ptFragmentAddress:p.Output(StringReplace(
            VarToStr(d['path']),'/',PathDelim,[rfReplaceAll]));
          ptIncludeUnit:p.Output(e.Key);
          ptIncludePath:p.Output(StringReplace(
            VarToStr(d['unitPath']),'/',PathDelim,[rfReplaceAll]));
          ptIterateEnd:
           begin
            if e.Next then d:=JSON(e.Value) else d:=nil;
            p.IterateNext(d<>nil);
           end;
          ptUsesClause:     p.Output(KeyText('usesClause'));
          ptProjectHeader:  p.Output(KeyText('header'));
          ptProjectBody:    p.Output(KeyText('body'));
          ptProjectSwitches:p.Output(KeyText('switches'));
          pt_Unknown:
            if not p.Done then p.Output(ExtraFields.Values[p.GetTagLabel]);
        end;
      until p.Done;
      ForceDirectories(FSrcFolder+'dcu');//TODO: setting "create dcu folder"?
      p.Save(fn1);

      //xxmp.pas
      if not(FileExists(fn2)) then
       begin
        BuildOutput(ProtoProjectPas+#13#10);
        s:=FProtoPath+ProtoProjectPas;
        if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectPas;
        p.Parse(ReadString(s),ExtraFields);
        repeat
          case p.GetNext of
            ptProjectName:p.Output(FProjectName);
            ptProjectPath:p.Output(FRootFolder);
            ptProtoFile:p.Output(FProtoPath+ProtoProjectPas);
            pt_Unknown:
              if not p.Done then p.Output(ExtraFields.Values[p.GetTagLabel]);
            //else raise?
          end;
        until p.Done;
        p.Save(fn2);
       end;

      //copy other files the first time (cfg,dof,res...)
      fh:=FindFirstFileA(PAnsiChar(FProtoPath+ProtoProjectMask),fd);
      if fh<>INVALID_HANDLE_VALUE then
       begin
        repeat
          s:=fd.cFileName;
          if s<>ProtoProjectDpr then
           begin
            i:=Length(s);
            while (i<>0) and (s[i]<>'.') do dec(i);
            fn1:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn1)) then
             begin
              BuildOutput(fn1+#13#10);
              CopyFileA(PAnsiChar(FProtoPath+s),PAnsiChar(fn1),false);
             end;
           end;
        until not(FindNextFileA(fh,fd));
        Windows.FindClose(fh);
       end;

      //proto\Web.*
      fh:=FindFirstFileA(PAnsiChar(FProtoPathDef+ProtoProjectMask),fd);
      if fh<>INVALID_HANDLE_VALUE then
       begin
        repeat
          s:=fd.cFileName;
          if s<>ProtoProjectDpr then
           begin
            i:=Length(s);
            while (i<>0) and (s[i]<>'.') do dec(i);
            fn1:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn1)) then
             begin
              BuildOutput(fn1+#13#10);
              CopyFileA(PAnsiChar(FProtoPathDef+s),PAnsiChar(fn1),false);
             end;
           end;
        until not(FindNextFileA(fh,fd));
        Windows.FindClose(fh);
       end;

    finally
      p.Free;
    end;
    Result:=true;
   end;
end;

function TXxmWebProject.ReadString(const FilePath: AnsiString): AnsiString;
var
  f:TFileStream;
  l:int64;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone);
  try
    l:=f.Size;
    SetLength(Result,l);
    f.Read(Result[1],l);
  finally
    f.Free;
  end;
end;

function TXxmWebProject.KeyText(const Key: WideString):AnsiString;
var
  v:OleVariant;
  v1,v2:integer;
  s:TStringStream;
begin
  v:=Data[Key];
  if VarIsNull(v) then
    Result:=''
  else
   begin
    s:=TStringStream.Create('');
    try
      if VarIsArray(v) then
       begin
        v1:=VarArrayLowBound(v,1);
        v2:=VarArrayHighBound(v,1);
        while v1<v2 do
         begin
          s.WriteString(VarToStr(v[v1]));
          inc(v1);
         end;
       end
      else
        s.WriteString(VarToStr(v));
      Result:=
        StringReplace(
        StringReplace(
          s.DataString,
          #13#10,#10,[rfReplaceAll]),
          #10,#13#10,[rfReplaceAll]);
    finally
      s.Free;
    end;
   end;
end;

{$IF not Declared(TStartupInfoA)}
type
  TStartupInfoA=TStartupInfo;
{$IFEND}

function TXxmWebProject.Compile:boolean;
var
  cl:TStringList;
  cli:integer;
  clx,cld,d1:AnsiString;
  pi:TProcessInformation;
  si:TStartupInfoA;
  h1,h2:THandle;
  sa:TSecurityAttributes;
  f:TFileStream;
  d:array[0..$FFF] of AnsiChar;
  procedure GetKeys(const Key: WideString; const Prefix: AnsiString);
  var
    v,vx:OleVariant;
    v1,v2:integer;
  begin
    v:=Data[Key];
    if not(VarIsNull(v)) then
      if VarIsArray(v) then
       begin
        v1:=VarArrayLowBound(v,1);
        v2:=VarArrayHighBound(v,1);
        while v1<v2 do
         begin
          vx:=v[v1];
          if VarIsArray(vx) then
           begin
            cl.Add('4'+VarToStr(vx[1]));//path
            cl.Add('5'+VarToStr(vx[0]));//command
           end
          else
            cl.Add(prefix+VarToStr(vx));
          inc(v2);
         end;
       end
      else
        cl.Add(prefix+VarToStr(v));
  end;
  function DoCommand(cmd,fld:AnsiString):boolean;
  var
    c:cardinal;
    running:boolean;
  begin
    if not(CreateProcessA(nil,PAnsiChar(AnsiString(
      StringReplace(
      StringReplace(
      StringReplace(
      StringReplace(
        cmd,
          '[[ProjectName]]',FProjectName,[rfReplaceAll]),
          '[[SrcPath]]',FSrcFolder,[rfReplaceAll]),
          '[[ProjectPath]]',FRootFolder,[rfReplaceAll]),
          '[[HandlerPath]]',FHandlerPath,[rfReplaceAll])
          //more?
      )),
      nil,nil,true,NORMAL_PRIORITY_CLASS,nil,PAnsiChar(fld),si,pi)) then
      //RaiseLastOSError;
      raise EXxmWebProjectCompile.Create('Error performing'#13#10'"'+cmd+'":'#13#10+SysErrorMessage(GetLastError));
    CloseHandle(pi.hThread);
    try
      running:=true;
      repeat
        if running then
          running:=WaitForSingleObject(pi.hProcess,50)=WAIT_TIMEOUT;
        if not PeekNamedPipe(h1,nil,0,nil,@c,nil) then c:=0;//RaiseLastOSError;
        if c<>0 then
         begin
          if not ReadFile(h1,d[0],$FFF,c,nil) then c:=0;//RaiseLastOSError;
          if c<>0 then
           begin
            f.Write(d[0],c);
            d[c]:=#0;
            BuildOutput(d);
           end;
         end;
      until not(running) and (c=0);
      if GetExitCodeProcess(pi.hProcess,c) then
        if c=0 then
          Result:=true
        else
         begin
          Result:=false;
          BuildOutput('Command "'+cmd+'" failed with code '+IntToStr(integer(c)));
         end
      else
       begin
        Result:=false;
        BuildOutput('GetExitCodeProcess('+cmd+'):'+SysErrorMessage(GetLastError));
       end;
    finally
      CloseHandle(pi.hProcess);
    end;
  end;
begin
  cl:=TStringList.Create;
  try
    GetKeys('preCompileCommand','1');
    GetKeys('compileCommand','2');
    GetKeys('postCompileCommand','3');
    if cl.Count=0 then
      Result:=true
    else
     begin
      d1:=GetCurrentDir;
      f:=TFileStream.Create(FRootFolder+FProjectName+ProjectLogExtension,fmCreate);
      try
        sa.nLength:=SizeOf(TSecurityAttributes);
        sa.lpSecurityDescriptor:=nil;
        sa.bInheritHandle:=true;
        if not(CreatePipe(h1,h2,@sa,$10000)) then RaiseLastOSError;
        ZeroMemory(@si,SizeOf(TStartupInfo));
        si.cb:=SizeOf(TStartupInfo);
        si.dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        si.wShowWindow:=SW_HIDE;
        si.hStdOutput:=h2;
        si.hStdError:=h2;
        Result:=true;//default
        try
          cli:=0;
          while (cli<cl.Count) and Result do
           begin
            clx:=cl[cli];
            inc(cli);
            //TODO: switch for DoOutput(clx)?
            case clx[1] of
              '1','3':cld:=FRootFolder;
              '2':cld:=FSrcFolder;
              '4':cld:=Copy(clx,2,Length(clx)-1);
              '5':;//assert cld set by preceding '4' (see GetNodesText)
            end;
            if clx[1]<>'4' then
             begin
              SetCurrentDir(cld);
              Result:=DoCommand(Copy(clx,2,Length(clx)-1),cld);
             end;
           end;
        finally
          CloseHandle(h1);
          CloseHandle(h2);
        end;
      finally
        f.Free;
        SetCurrentDir(d1);
      end;
     end;
  finally
    cl.Free;
  end;
end;

procedure TXxmWebProject.BuildOutput(const Msg: AnsiString);
begin
  FOnOutput(Msg);
end;

function TXxmWebProject.ResolveErrorLines(
  const BuildOutput: AnsiString): AnsiString;
var
  sl_in,sl_out:TStringList;
  sl_x:integer;
  s,t,u:string;
  i,j,k,l:integer;
  map:TXxmLineNumbersMap;
  d:IJSONDocument;
  e:IJSONEnumerator;
begin
  //TODO: call ResolveErrorLines from xxmConv also
  map:=TXxmLineNumbersMap.Create;
  sl_in:=TStringList.Create;
  sl_out:=TStringList.Create;
  try
    sl_in.Text:=BuildOutput;
    for sl_x:=0 to sl_in.Count-1 do
     begin
      s:=sl_in[sl_x];
      if (s='') or (s[2]=':') or (s[2]='\') then i:=0 else i:=Pos('.pas(',s);
      if i<>0 then
       begin
        k:=i;
        while (k<>0) and (s[k]<>'\') do dec(k);
        inc(i,5);
        j:=i;
        l:=Length(s);
        while (j<=l) and (s[j]<>')') do inc(j);
        try
          t:=Copy(s,1,i-6);
          u:=Copy(s,k+1,i-k-6);
          map.Load(ChangeFileExt(FSrcFolder+t,LinesMapExtension));
          e:=JSONEnum(Data['files']);
          while (e<>nil) and (e.Next) do
           begin
            d:=JSON(e.Value);
            if (VarToStr(d['unitName'])=t) then //and (VarToStr(d['unitPath'])=) then
             begin
              s:=VarToStr(d['path'])+
                '['+map.GetXxmLines(StrToInt(Copy(s,i,j-i)))+
                ']'+Copy(s,j+1,Length(s)-j);
              e:=nil;
             end;
           end;
        except
          //silent
        end;
       end;
      if s<>'' then sl_out.Add(s);
     end;
    Result:=sl_out.Text;
  finally
    sl_in.Free;
    sl_out.Free;
    map.Free;
  end;
end;

end.
