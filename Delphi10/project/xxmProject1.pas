unit xxmProject1;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, jsonDoc, xxmPageParse;

type
  TXxmProjectOutput=procedure(Subject: TObject; const Msg: AnsiString);

  TXxmProject=class(TObject)
  private
    Data:IJSONDocument;
    DataStartSize:integer;
    DataFileName,FProjectName,FRootFolder,FSrcFolder,
    FHandlerPath,FProtoPathDef,FProtoPath:string;
    Modified,DoLineMaps:boolean;
    Signatures:TStringList;
    FSubject:TObject;
    FOnOutput:TXxmProjectOutput;
    FParserValues:TXxmPageParserValueList;

    function KeyText(const Key: WideString): AnsiString;

    function ReadString(const FilePath: string): AnsiString;
    procedure WriteString(const FilePath: string; const FileData: AnsiString);
    procedure BuildOutput(const Msg: AnsiString);
  public

    constructor Create(Subject: TObject; const SourcePath, HandlerPath,
      ProtoPathDef: string; OnOutput: TXxmProjectOutput; CanCreate: boolean);
    destructor Destroy; override;

    function CheckFiles(Rebuild:boolean;ExtraFields:TStrings):boolean;
    function GenerateProjectFiles(Rebuild:boolean;ExtraFields:TStrings):boolean;
    function ResolveErrorLines(const BuildOutput:UTF8String):UTF8string;

    function Compile: boolean;
    procedure Update;

    property ProjectName: string read FProjectName;
    property RootFolder: string read FRootFolder;
    property ProjectFile: string read DataFileName;

    property SrcFolder: string read FSrcFolder write FSrcFolder;
    property ProtoFolder: string read FProtoPath write FProtoPath;
    property HandlerPath: string read FHandlerPath write FHandlerPath;
    property LineMaps: boolean read DoLineMaps write DoLineMaps;
  end;

  EXxmProjectNotFound=class(Exception);
  EXxmProjectLoad=class(Exception);
  EXxmProjectCompile=class(Exception);

implementation

uses System.Variants, System.Win.ComObj, xxmDefs, xxmTools, xxmProtoParse,
  xxHash;

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

  FragmentMapNodeCaseSwitch1=8;
  FragmentMapNodeCaseSwitch2=4;

type
  TFragmentMapNode=class(TObject)
  private
    FList:array[0..FragmentMapNodeCaseSwitch1-1] of record
      Path,UnitName:UTF8String;
    end;
    FListIndex,FMapIndex,FMapCount:integer;
    FMap:array[0..255] of TFragmentMapNode;
    FDetectDefault:boolean;
    FDefaultUnitName,FUnknownUnitName:UTF8String;
  protected
    procedure AddMap(const Path,UnitName:UTF8String;MapIndex:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Path,UnitName:UTF8String);
    function GenerateCode:AnsiString;
  end;

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

function FragmentMapPrefix(const FileName:string):AnsiString;
begin
  Result:='{'#13#10
    +'  '+AnsiString(FileName)+#13#10
    +'  ATTENTION:'#13#10
    +'  This file is re-constructed when the xxm project source changes.'#13#10
    +'  Any changes to this file will be overwritten.'#13#10
    +'}'#13#10;
end;

function XmlEncode(const x:string):AnsiString;
begin
  Result:=AnsiString(
    StringReplace(
    StringReplace(
    StringReplace(
      x
      ,'&','&amp;',[rfReplaceAll])
      ,'<','&lt;',[rfReplaceAll])
      ,'>','&gt;',[rfReplaceAll])
      );
end;

{ TXxmProject }

constructor TXxmProject.Create(Subject: TObject; const SourcePath, HandlerPath,
  ProtoPathDef: string; OnOutput: TXxmProjectOutput; CanCreate: boolean);
var
  v:OleVariant;
  i,j,l:integer;
  s:AnsiString;
  d:IJSONDocument;
  pv:TXxmPageParserValues;
begin
  inherited Create;
  Modified:=false;
  DoLineMaps:=true;
  FSubject:=Subject;
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
        WriteString(FRootFolder+DataFileName,
          '{name:"'+AnsiString(FProjectName)+'",compileCommand:"'+DefaultCompileCommand+'"}');
       end
      else
        raise EXxmProjectNotFound.Create('xxmProject File not found "'+
          SourcePath+'"');
   end;

  if HandlerPath='' then
    FHandlerPath:=GetSelfPath
  else
    FHandlerPath:=IncludeTrailingPathDelimiter(HandlerPath);
  if ProtoPathDef='' then
    FProtoPathDef:=FHandlerPath+'include'+PathDelim+'proto'+PathDelim
  else
    FProtoPathDef:=IncludeTrailingPathDelimiter(ProtoPathDef);
  FSrcFolder:=FRootFolder+SourceDirectory+PathDelim;

  Data:=LoadJSON(FRootFolder+DataFileName);
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

  v:=Data['protopath'];
  if not(VarIsNull(v)) then
    FProtoPath:=IncludeTrailingPathDelimiter(VarToStr(v))
  else
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
        s:=AnsiString(StringReplace(StringReplace(
          v,
          '$v',string(FParserValues[pv].Code),[rfReplaceAll]),
          '$d',string(FParserValues[pv].Code),[rfReplaceAll]));
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

destructor TXxmProject.Destroy;
begin
  //Update was here before
  Data:=nil;
  Signatures.Free;
  inherited;
end;

procedure TXxmProject.Update;
var
  fn:string;
  s:AnsiString;
begin
  if Modified then
   begin
    s:=AnsiString(Data.ToString);//TODO: UTF8
    //TODO: if Data.Dirty
    if DataStartSize<>Length(s) then
     begin
      Data['lastModified']:=
        FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now);//timezone?
      WriteString(FRootFolder+DataFileName,s);
      Modified:=false;
     end;

    //save signatures
    try
      fn:=FSrcFolder+SignaturesFileName;
      SetFileAttributes(PChar(fn),0);
      Signatures.SaveToFile(fn);
      //SetFileAttributes(PChar(fn),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
    except
      //silent?
    end;

   end;
end;

function TXxmProject.CheckFiles(Rebuild:boolean;ExtraFields:TStrings): boolean;
var
  p:TXxmProtoParser;
  q:TXxmPageParser;
  m:TXxmLineNumbersMap;
  DataFiles,d:IJSONDocument;
  e:IJSONEnumerator;
  v:OleVariant;
  fn,fnu,s,cid,uname,upath,uext:string;
  sl,sl1,sl2:TStringList;
  sl_i,i,cPathIndex,fExtIndex,fPathIndex:integer;
  cPathHash:int64;
  FragmentMap1,FragmentMap2:TFragmentMapNode;
  uList:AnsiString;
  f:TFileStream;
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

  if not DirectoryExists(FSrcFolder) then
   begin
    CreateDirectory(PChar(FSrcFolder),nil);
    SetFileAttributes(PChar(FSrcFolder),FILE_ATTRIBUTE_HIDDEN);
   end;

  p:=TXxmProtoParser.Create;
  q:=TXxmPageParser.Create(FParserValues);
  m:=TXxmLineNumbersMap.Create;
  try
    sl:=TStringList.Create;
    sl1:=TStringList.Create;
    sl2:=TStringList.Create;
    try
      sl1.Sorted:=true;
      sl2.Sorted:=true;

      e:=JSONEnum(DataFiles);
      while e.Next do
       begin
        sl1.Add(e.Key);
        v:=JSON(e.Value)['unitName'];
        if not(VarIsNull(v)) then sl2.Add(VarToStr(v));
       end;
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
          if not(AnsiChar(uname[1]) in ['A'..'Z','a'..'z']) then uname:='x'+uname;
          i:=0;
          repeat
            inc(i);
            s:=uname+IntToStr(i);
          until sl2.IndexOf(s)=-1;
          uname:=s;
          d['unitName']:=uname;
          sl2.Add(uname);
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
          BuildOutput(AnsiString(':'+FProjectName+':'+fn+':'+uname+':'+cid+#13#10));

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
                ptProjectName:p.Output(AnsiString(FProjectName));
                ptProjectPath:p.Output(AnsiString(FRootFolder));
                ptProtoFile:p.Output(AnsiString(FProtoPath+uext+DelphiExtension));
                ptFragmentID:p.Output(AnsiString(cid));
                ptFragmentUnit:p.Output(AnsiString(uname));
                ptFragmentAddress:p.Output(AnsiString(fnu));
                ptUsesClause:p.Output(q.AllSectionsCheckComma(psUses,m));
                ptFragmentDefinitions:p.Output(q.AllSections(psDefinitions,m));
                ptFragmentHeader:p.Output(q.AllSections(psHeader,m));
                ptFragmentBody:p.Output(q.BuildBody(m));
                ptFragmentFooter:p.Output(q.AllSections(psFooter,m));
                else//pt_Unknown:
                  if not p.Done then
                    p.Output(AnsiString(ExtraFields.Values[string(p.GetTagLabel)]));
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
      sl2.Free;
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

    //generate fragment map
    //TODO: if GenerateFragmentMap then
    {$Q-}
    cPathHash:=0;
    e:=JSONEnum(DataFiles);
    while e.Next do
     begin
      d:=JSON(e.Value);
      if not(VarIsNull(d['unitName'])) then
        inc(cPathHash,xxHash64(UTF8Encode(LowerCase(VarToStr(d['path']))),11111));
     end;
    s:=IntToHex(cPathHash,16);
    if (Signatures.Values[SignatureFragmentMap]<>s)
      or not(FileExists(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap0))
      or not(FileExists(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap1))
      or not(FileExists(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap2))
      then
     begin
      Signatures.Values[SignatureFragmentMap]:=s;
      BuildOutput(':xxmFMap'#13#10);
      uext:=XxmFileExtension[ftInclude];
      FragmentMap1:=TFragmentMapNode.Create;
      FragmentMap1.FDetectDefault:=true;//TODO: FDetectDefault on paths with '/'...
      FragmentMap2:=TFragmentMapNode.Create;
      try
        uList:='';
        e:=JSONEnum(DataFiles);
        while e.Next do
         begin
          d:=JSON(e.Value);
          v:=d['unitName'];
          if not(VarIsNull(v)) then
           begin
            s:=LowerCase(VarToStr(d['path']));
            if Copy(s,Length(s)-Length(uext)+1,Length(uext))<>uext then
              FragmentMap1.Add(UTF8Encode(s),UTF8Encode(VarToStr(v)))
            else
              FragmentMap2.Add(UTF8Encode(s),UTF8Encode(VarToStr(v)));
            //TODO: additionalPaths array?
            uList:=uList+'  ,'+AnsiString(VarToStr(v))+#13#10;
           end;
         end;
        //TODO: FSrcFolder?
        WriteString(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap0,
          uList);
        WriteString(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap1,
          FragmentMapPrefix(ProtoFragmentMap1)+FragmentMap1.GenerateCode);
        WriteString(FRootFolder+SourceDirectory+PathDelim+ProtoFragmentMap2,
          FragmentMapPrefix(ProtoFragmentMap2)+FragmentMap2.GenerateCode);
      finally
        FragmentMap1.Free;
        FragmentMap2.Free;
      end;
     end;
    e:=nil;

    //project files
    GenerateProjectFiles(Rebuild,ExtraFields);

  finally
    p.Free;
    q.Free;
    m.Free;
  end;
end;

function TXxmProject.GenerateProjectFiles(Rebuild:boolean;
  ExtraFields:TStrings):boolean;
var
  p:TXxmProtoParser;
  e:IJSONEnumerator;
  d:IJSONDocument;
  fh:THandle;
  fd:TWin32FindData;
  fn,fn1,fn2,fn3,s:string;
  i:integer;
begin
  Result:=false;
  //project files
  fn1:=FSrcFolder+FProjectName+'.dpr';
  fn2:=FSrcFolder+FProjectName+'.dproj';
  fn3:=FRootFolder+ProtoProjectPas;
  if Modified or Rebuild
    or not(FileExists(fn1))
    or not(FileExists(fn2))
    or not(FileExists(fn3))
    then
   begin
    p:=TXxmProtoParser.Create;
    try
      //[[ProjectName]].dpr
      BuildOutput(AnsiString(FProjectName+'.dpr'#13#10));
      s:=FProtoPath+ProtoProjectDpr;
      if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectDpr;
      p.Parse(ReadString(s),ExtraFields);
      repeat
        case p.GetNext of
          ptProjectName:p.Output(AnsiString(FProjectName));
          ptProjectPath:p.Output(AnsiString(FRootFolder));
          ptProtoFile:p.Output(AnsiString(FProtoPath+ProtoProjectDpr));
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
          ptFragmentUnit:p.Output(AnsiString(VarToStr(d['unitName'])));
          ptFragmentPath:p.Output(AnsiString(StringReplace(
            VarToStr(d['unitPath']),'/',PathDelim,[rfReplaceAll])));
          ptFragmentAddress:p.Output(AnsiString(StringReplace(
            VarToStr(d['path']),'/',PathDelim,[rfReplaceAll])));
          ptIncludeUnit:p.Output(AnsiString(e.Key));
          ptIncludePath:p.Output(AnsiString(StringReplace(
            VarToStr(d['unitPath']),'/',PathDelim,[rfReplaceAll])));
          ptIterateEnd:
           begin
            if e.Next then d:=JSON(e.Value) else d:=nil;
            p.IterateNext(d<>nil);
           end;
          ptUsesClause:     p.Output(KeyText('usesClause'));
          ptProjectHeader:  p.Output(KeyText('header'));
          ptProjectBody:    p.Output(KeyText('body'));
          ptProjectSwitches:p.Output(KeyText('switches'));
          else//pt_Unknown:
            if not p.Done then
              p.Output(AnsiString(ExtraFields.Values[string(p.GetTagLabel)]));
        end;
      until p.Done;
      ForceDirectories(FSrcFolder+'dcu');//TODO: setting "create dcu folder"?
      p.Save(fn1);

      //[[ProjectName]].dproj
      BuildOutput(AnsiString(FProjectName+'.dproj'#13#10));
      s:=FProtoPath+ProtoProjectDproj;
      if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectDproj;
      p.Parse(ReadString(s),ExtraFields);
      repeat
        case p.GetNext of
          ptProjectName:p.Output(XmlEncode(FProjectName));
          ptProjectUUID:p.Output(XmlEncode(VarToStr(Data['uuid'])));//p.Output(AnsiString(CreateClassID));
          ptHandlerPath:p.Output(XmlEncode(FHandlerPath));
          ptProtoPath:p.Output(XmlEncode(FProtoPath));
          else//pt_Unknown:
            if not p.Done then
              p.Output(XmlEncode(ExtraFields.Values[string(p.GetTagLabel)]));
        end;
      until p.Done;
      ForceDirectories(FSrcFolder+'dcu');//TODO: setting "create dcu folder"?
      p.Save(fn2);

      //xxmp2.pas
      if not(FileExists(fn3)) then //or Rebuild?
       begin
        BuildOutput(ProtoProjectPas+#13#10);
        s:=FProtoPath+ProtoProjectPas;
        if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectPas;
        p.Parse(ReadString(s),ExtraFields);
        repeat
          case p.GetNext of
            ptProjectName:p.Output(AnsiString(FProjectName));
            ptProjectPath:p.Output(AnsiString(FRootFolder));
            ptProtoFile:p.Output(AnsiString(FProtoPath+ProtoProjectPas));
            else//pt_Unknown:
              if not p.Done then
                p.Output(AnsiString(ExtraFields.Values[string(p.GetTagLabel)]));
          end;
        until p.Done;
        p.Save(fn3);
       end;

      //copy other files the first time (cfg,dof,res...)
      fh:=FindFirstFile(PChar(FProtoPath+ProtoProjectMask),fd);
      if fh<>INVALID_HANDLE_VALUE then
       begin
        repeat
          s:=fd.cFileName;
          if s<>ProtoProjectDpr then
           begin
            i:=Length(s);
            while (i<>0) and (s[i]<>'.') do dec(i);
            fn:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn)) then
             begin
              BuildOutput(AnsiString(fn+#13#10));
              CopyFile(PChar(FProtoPath+s),PChar(fn),false);
             end;
           end;
        until not(FindNextFile(fh,fd));
        Winapi.Windows.FindClose(fh);
       end;

      //proto\Web.*
      fh:=FindFirstFile(PChar(FProtoPathDef+ProtoProjectMask),fd);
      if fh<>INVALID_HANDLE_VALUE then
       begin
        repeat
          s:=fd.cFileName;
          if (s<>ProtoProjectDpr) and (s<>ProtoProjectDproj) then
           begin
            i:=Length(s);
            while (i<>0) and (s[i]<>'.') do dec(i);
            fn:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn)) then
             begin
              BuildOutput(AnsiString(fn+#13#10));
              CopyFile(PChar(FProtoPathDef+s),PChar(fn),false);
             end;
           end;
        until not(FindNextFile(fh,fd));
        Winapi.Windows.FindClose(fh);
       end;

    finally
      p.Free;
    end;
    Result:=true;
   end;
end;

function TXxmProject.ReadString(const FilePath: string): AnsiString;
var
  f:TFileStream;
  l:int64;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    l:=f.Size;
    SetLength(Result,l);
    f.Read(Result[1],l);
  finally
    f.Free;
  end;
end;

procedure TXxmProject.WriteString(const FilePath: string;
  const FileData: AnsiString);
var
  f:TFileStream;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(FileData[1],Length(FileData));
  finally
    f.Free;
  end;
end;

function TXxmProject.KeyText(const Key: WideString): AnsiString;
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
        while v1<=v2 do
         begin
          s.WriteString(VarToStr(v[v1]));
          inc(v1);
         end;
       end
      else
        s.WriteString(VarToStr(v));
      Result:=AnsiString(
        StringReplace(
        StringReplace(
          s.DataString,
          #13#10,#10,[rfReplaceAll]),
          #10,#13#10,[rfReplaceAll]))
    finally
      s.Free;
    end;
   end;
end;

{$IF not Declared(TStartupInfoA)}
type
  TStartupInfoA=TStartupInfo;
{$IFEND}

function TXxmProject.Compile:boolean;
const
  dSize=$10000;
var
  cl:TStringList;
  cli:integer;
  clx,cld,d1:string;
  pi:TProcessInformation;
  si:TStartupInfo;
  h1,h2:THandle;
  sa:TSecurityAttributes;
  f:TFileStream;
  d:array[0..dSize-1] of AnsiChar;
  procedure GetKeys(const Key: WideString; const Prefix: string);
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
        while v1<=v2 do
         begin
          vx:=v[v1];
          if VarIsArray(vx) then
           begin
            cl.Add('4'+VarToStr(vx[1]));//path
            cl.Add('5'+VarToStr(vx[0]));//command
           end
          else
            cl.Add(prefix+VarToStr(vx));
          inc(v1);
         end;
       end
      else
        cl.Add(prefix+VarToStr(v));
  end;
  function DoCommand(const cmd,fld:string;ResolveErrors:boolean):boolean;
  var
    c:cardinal;
    running:boolean;
  begin
    if not(CreateProcess(nil,PChar(
      StringReplace(
      StringReplace(
      StringReplace(
      StringReplace(
      StringReplace(
        string(cmd)
          ,'[[ProjectName]]',FProjectName,[rfReplaceAll])
          ,'[[SrcPath]]',FSrcFolder,[rfReplaceAll])
          ,'[[ProjectPath]]',FRootFolder,[rfReplaceAll])
          ,'[[HandlerPath]]',FHandlerPath,[rfReplaceAll])
          ,'[[ProtoPath]]',FProtoPath,[rfReplaceAll])
          //more?
      ),
      nil,nil,true,NORMAL_PRIORITY_CLASS,nil,PChar(fld),si,pi)) then
      //RaiseLastOSError;
      raise EXxmProjectCompile.Create('Error performing'#13#10'"'+string(cmd)
        +'":'#13#10+SysErrorMessage(GetLastError));
    CloseHandle(pi.hThread);
    try
      running:=true;
      repeat
        if running then
          running:=WaitForSingleObject(pi.hProcess,50)=WAIT_TIMEOUT;
        if not PeekNamedPipe(h1,nil,0,nil,@c,nil) then c:=0;//RaiseLastOSError;
        if c<>0 then
         begin
          if not ReadFile(h1,d[0],dSize-1,c,nil) then c:=0;//RaiseLastOSError;
          if c<>0 then
           begin
            f.Write(d[0],c);
            d[c]:=#0;
            if ResolveErrors then
              BuildOutput(AnsiString(ResolveErrorLines(d)))
            else
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
          BuildOutput(AnsiString(
            'Command "'+string(cmd)+'" failed with code '+IntToStr(integer(c))));
         end
      else
       begin
        Result:=false;
        BuildOutput(AnsiString(
          'GetExitCodeProcess('+string(cmd)+'):'+SysErrorMessage(GetLastError)));
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
              Result:=DoCommand(Copy(clx,2,Length(clx)-1),cld,clx[1]='2');
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

procedure TXxmProject.BuildOutput(const Msg: AnsiString);
begin
  FOnOutput(FSubject, Msg);
end;

function TXxmProject.ResolveErrorLines(const BuildOutput: UTF8String): UTF8String;
var
  sl_in,sl_out:TStringList;
  sl_x:integer;
  s,t,u:string;
  i,j,k,l:integer;
  map:TXxmLineNumbersMap;
  d:IJSONDocument;
  e:IJSONEnumerator;
begin
  map:=TXxmLineNumbersMap.Create;
  sl_in:=TStringList.Create;
  sl_out:=TStringList.Create;
  try
    sl_in.Text:=string(BuildOutput);
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
    Result:=UTF8Encode(sl_out.Text);
  finally
    sl_in.Free;
    sl_out.Free;
    map.Free;
  end;
end;

{ TFragmentMapNode }

constructor TFragmentMapNode.Create;
var
  i:integer;
begin
  inherited Create;
  FListIndex:=0;
  FMapIndex:=1;
  FMapCount:=0;
  for i:=0 to 255 do FMap[i]:=nil;
  FDetectDefault:=false;
  FDefaultUnitName:='';
  FUnknownUnitName:='';
end;

destructor TFragmentMapNode.Destroy;
var
  i:integer;
begin
  for i:=0 to 255 do FreeAndNil(FMap[i]);
  inherited;
end;

procedure TFragmentMapNode.Add(const Path, UnitName: UTF8String);
var
  i:integer;
begin
  //assert Path over all calls unique
  if FListIndex<FragmentMapNodeCaseSwitch1 then
   begin
    FList[FListIndex].Path:=Path;
    FList[FListIndex].UnitName:=UnitName;
   end
  else
   begin
    if FListIndex=FragmentMapNodeCaseSwitch1 then
      for i:=0 to FragmentMapNodeCaseSwitch1-1 do
        AddMap(FList[i].Path,FList[i].UnitName,FMapIndex);
    AddMap(Path,UnitName,FMapIndex);
   end;
  inc(FListIndex);
  if FDetectDefault then
   begin
    if (FDefaultUnitName='') and
      ((Path='default.xxm') or (Path='index.xxm')) then
      FDefaultUnitName:=UnitName;
    if (FUnknownUnitName='') and
      (Path='404.xxm') then //or ? 'unknown.xxm'?
      FUnknownUnitName:=UnitName;
   end;
end;

procedure TFragmentMapNode.AddMap(const Path, UnitName: UTF8String;
  MapIndex: integer);
var
  c:byte;
begin
  //assert FListIndex>=FragmentMapNodeCaseSwitch
  if MapIndex>Length(Path) then c:=0 else c:=byte(Path[MapIndex]);
  if FMap[c]=nil then
   begin
    FMap[c]:=TFragmentMapNode.Create;//(MapIndex);
    FMap[c].FMapIndex:=MapIndex+1;
    inc(FMapCount);
   end;
  FMap[c].Add(Path,UnitName);
end;

function TFragmentMapNode.GenerateCode: AnsiString;
var
  i:integer;
  p:AnsiString;
begin
  p:='';
  for i:=0 to FMapIndex-1 do p:=p+'  ';
  if FListIndex>FragmentMapNodeCaseSwitch1 then
   begin
    Result:=p+'if Length(a)<'+AnsiString(IntToStr(FMapIndex))+' then r:=nil else'#13#10;
    if FMapCount>FragmentMapNodeCaseSwitch2 then
     begin
      Result:=Result
        +p+'case a['+AnsiString(IntToStr(FMapIndex))+'] of'#13#10;
      for i:=0 to 255 do
        if FMap[i]<>nil then
         begin
          //Result:=Result=p+'  #'+IntToStr(i)+':'#13#10
          Result:=Result+p+'  ''';
          if i=$27 then Result:=Result+'''''' else Result:=AnsiChar(i);
          Result:=Result+''':'#13#10
            +FMap[i].GenerateCode;
         end;
      Result:=Result
        +p+'  else r:=nil;'#13#10
        +p+'end'#13#10;
     end
    else
     begin
      for i:=0 to 255 do
        if FMap[i]<>nil then
         begin
           Result:=Result+p+'if a['+AnsiString(IntToStr(FMapIndex))+']=';
          //Result:=Result='#'+IntToStr(i)+':'#13#10
          Result:=Result+'''';
          if i=$27 then Result:=Result+'''''' else Result:=AnsiChar(i);
          Result:=Result+''' then'#13#10
            +FMap[i].GenerateCode
            +p+'else'#13#10;
         end;
      Result:=Result
        +p+'  else r:=nil'#13#10;
     end
   end
  else
   begin
    Result:='';
    for i:=0 to FListIndex-1 do
      Result:=Result
        +p+'if a='''
        +AnsiString(StringReplace(string(FList[i].Path),'''','''''',[rfReplaceAll]))
        +''' then r:=@'+AnsiString(FList[i].UnitName)+'.build else'#13#10;
    Result:=Result
      +p+'  r:=nil'#13#10;
   end;
  if FDetectDefault then
   begin
    if FDefaultUnitName<>'' then
      Result:=p+'if a='''' then r:=@'+AnsiString(FDefaultUnitName)
        +'.build else'#13#10+Result;
    if FUnknownUnitName<>'' then
      Result:=p+'begin'#13#10+Result+p+';'#13#10
        +p+'if r=nil then r:=@'+AnsiString(FUnknownUnitName)+'.build;'#13#10
        +p+'end'#13#10;
   end;
  if FMapIndex=1 then Result:=Result+p+';'#13#10;
end;

end.

