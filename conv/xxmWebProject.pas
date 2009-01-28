unit xxmWebProject;

interface

uses Windows, SysUtils, Classes, MSXML2_TLB;

type
  TXxmWebProjectOutput=procedure(Msg:string);

  TXxmWebProject=class(TObject)
  private
    Data:DOMDocument;
    DataStartSize:integer;
    DataFileName,FProjectName,FRootFolder,FSrcFolder,FProtoPathDef,FProtoPath:string;
    RootNode,DataFiles:IXMLDOMElement;
    Modified:boolean;
    Signatures:TStringList;
    FOnOutput:TXxmWebProjectOutput;

    function GetNode(element:IXMLDOMElement;xpath:string):IXMLDOMElement;
    function GetNodeText(element:IXMLDOMElement;xpath:string):string;
    function ForceNode(element:IXMLDOMElement;tagname:string):IXMLDOMElement;
    function ForceNodeID(element:IXMLDOMElement;tagname,id:string):IXMLDOMElement;
    function NodesText(element:IXMLDOMElement;xpath:string):string;

    function ReadString(FilePath:string):string;
    procedure BuildOutput(Msg:string);
  public

    constructor Create(SourcePath:string;
      OnOutput:TXxmWebProjectOutput; CanCreate:boolean);
    destructor Destroy; override;

    function CheckFiles(Rebuild:boolean):boolean;
    function GenerateProjectFiles(Rebuild: boolean):boolean;

    function Compile:boolean;
    procedure Update;

    property ProjectName:string read FProjectName;
    property RootFolder:string read FRootFolder;
    property ProjectFile:string read DataFileName;
  end;

  EXxmWebProjectNotFound=class(Exception);
  EXxmWebProjectLoad=class(Exception);

implementation

uses Variants, ComObj,
  xxmUtilities, xxmProtoParse, xxmPageParse, IniFiles;

{ TXxmWebProject }

const
  SXxmWebProjectNotFound='Web Project File not found for "__"';
  SXxmWebProjectLoad='Could not read "__"';

constructor TXxmWebProject.Create(SourcePath: string;
  OnOutput:TXxmWebProjectOutput; CanCreate:boolean);
var
  x:IXMLDOMElement;
  i:integer;
  s:string;
  f:TFileStream;
begin
  inherited Create;
  Modified:=false;
  FOnOutput:=OnOutput;
  FProjectName:='';

  //assert full expanded path
  //SourcePath:=ExpandFileName(SourcePath);

  i:=Length(SourcePath);
  while not(i=0) and not(SourcePath[i]='.') do dec(i);
  if LowerCase(Copy(SourcePath,i,Length(SourcePath)-i+1))=XxmFileExtension[ftProject] then
   begin
    //project file specified
    while not(i=0) and not(SourcePath[i]=PathDelim) do dec(i);
    FRootFolder:=Copy(SourcePath,1,i);
    DataFileName:=Copy(SourcePath,i+1,Length(SourcePath)-i);
   end
  else
   begin
    //find
    DataFileName:=XxmProjectFileName;
    FRootFolder:=IncludeTrailingPathDelimiter(SourcePath);
    i:=Length(FRootFolder);
    while not(i=0) and not(FileExists(FRootFolder+DataFileName)) do
     begin
      dec(i);
      while not(i=0) and not(FRootFolder[i]=PathDelim) do dec(i);
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
          while not(i=0) and not(SourcePath[i]=PathDelim) do dec(i);
          FRootFolder:=Copy(SourcePath,1,i);
         end;
        i:=Length(FRootFolder)-1;
        while not(i=0) and not(FRootFolder[i]=PathDelim) do dec(i);
        FProjectName:=Copy(FRootFolder,i+1,Length(FRootFolder)-i-1);
        s:='<XxmWebProject>'#13#10#9'<ProjectName></ProjectName>'#13#10#9+
          '<CompileCommand>dcc32 -U..\..\public -Q [[ProjectName]].dpr</CompileCommand>'#13#10'</XxmWebProject>';
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

  FProtoPathDef:=GetSelfPath+ProtoDirectory+PathDelim;
  FSrcFolder:=FRootFolder+SourceDirectory+PathDelim;

  Signatures:=TStringList.Create;
  try
    Signatures.LoadFromFile(FRootFolder+DataFileName+SignaturesExtension);
  except
    //silent
  end;

  Data:=CoDOMDocument.Create;
  Data.async:=false;
  Data.preserveWhiteSpace:=true;
  if not(Data.load(FRootFolder+DataFileName)) then
    raise EXxmWebProjectLoad.Create(StringReplace(
      SXxmWebProjectLoad,'__',FRootFolder+DataFileName,[])+
      #13#10+Data.parseError.reason);
  RootNode:=Data.documentElement;

  DataStartSize:=Length(Data.xml);

  x:=ForceNode(RootNode,' UUID');
  if x.text='' then x.text:=CreateClassID;//other random info?

  x:=ForceNode(RootNode,' ProjectName');
  if x.text='' then
   begin
    if FProjectName='' then
     begin
      i:=Length(DataFileName);
      while not(i=0) and not(DataFileName[i]='.') do dec(i);
      FProjectName:=Copy(DataFileName,1,Length(DataFileName)-i-1);
     end;
    x.text:=FProjectName;
   end
  else
    FProjectName:=x.text;

  DataFiles:=ForceNode(RootNode,' Files');

  //TODO: setting protopath?

  if DirectoryExists(FRootFolder+ProtoDirectory) then
    FProtoPath:=FRootFolder+ProtoDirectory+PathDelim
  else
    FProtoPath:=FProtoPathDef;

  //TODO: setting sourcepath?

  //TODO:

  //Settings/@AutoAddFiles
  //Settings/@AutoRemoveFiles

  //TODO:delphi source in separate buildfolder?
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
  fn:string;
begin
  if Modified then
   begin
    if not(DataStartSize=Length(Data.xml)) then
     begin
      ForceNode(RootNode,' LastModified').text:=
        FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now);//timezone?
      Data.save(FRootFolder+DataFileName);
      Modified:=false;
     end;

    //save signatures
    try
      fn:=FRootFolder+DataFileName+SignaturesExtension;
      SetFileAttributes(PChar(fn),0);
      Signatures.SaveToFile(fn);
      SetFileAttributes(PChar(fn),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
    except
      //silent?
    end;

   end;
end;

function TXxmWebProject.CheckFiles(Rebuild:boolean): boolean;
var
  p:TXxmProtoParser;
  q:TXxmPageParser;
  xl:IXMLDOMNodeList;
  xFile,x:IXMLDOMElement;
  fn,fnu,s,cid,uname,upath,uext:string;
  sl,sl1:TStringList;
  sl_i,i,cPathIndex,fExtIndex,fPathIndex:integer;
begin
  Result:=false;

  //TODO: setting autoaddfiles
  //TODO: autoremove files?

  p:=TXxmProtoParser.Create;
  q:=TXxmPageParser.Create;
  try
    sl:=TStringList.Create;
    sl1:=TStringList.Create;
    try

      xl:=DataFiles.selectNodes('File');
      try
        x:=xl.nextNode as IXMLDOMElement;
        while not(x=nil) do
         begin
          sl1.Add(x.getAttribute('ID'));
          x:=xl.nextNode as IXMLDOMElement;
         end;
      finally
        xl:=nil;
      end;

      ListFilesInPath(sl,FRootFolder);

      for sl_i:=0 to sl.Count-1 do
       begin
        fn:=sl[sl_i];
        cid:=GetInternalIdentifier(fn,cPathIndex,fExtIndex,fPathIndex);

        xFile:=ForceNodeID(DataFiles,' File',cid);
        i:=sl1.IndexOf(cid);
        if not(i=-1) then sl1.Delete(i);

        //fn fit for URL
        fnu:=StringReplace(fn,'\','/',[rfReplaceAll]);
        x:=ForceNode(xFile,'Path');
        x.text:=fnu;

        //BuildFile(xFile,fn,cid,Rebuild);

        //pascal unit name
        upath:=VarToStr(xFile.getAttribute('UnitPath'));
        uname:=VarToStr(xFile.getAttribute('UnitName'));
        if fExtIndex=0 then uext:='' else uext:=Copy(fn,fExtIndex+1,Length(fn)-fExtIndex);

        if uname='' then
         begin
          //unique counter for project
          uname:=Copy(cid,cPathIndex,Length(cid)-cPathIndex+1);
          if not(uname[1] in ['A'..'Z','a'..'z']) then uname:='x'+uname;
          i:=0;
          repeat
            inc(i);
            s:=uname+IntToStr(i);
            x:=DataFiles.selectSingleNode('File[@UnitName="'+s+'"]') as IXMLDOMElement;
          until (x=nil);
          uname:=s;
          xFile.setAttribute('UnitName',uname);
          Modified:=true;
         end;
        if upath='' then
         begin
          upath:=Copy(fn,1,fPathIndex);
          xFile.setAttribute('UnitPath',upath);
         end;

        //TODO: setting no pas subdirs?

        //TODO: proto signature? (setting?)
        s:=Signature(FRootFolder+fn);
        if Rebuild or not(Signatures.Values[uname]=s) or not(
          FileExists(FSrcFolder+upath+uname+DelphiExtension)) then
         begin
          Signatures.Values[uname]:=s;
          Modified:=true;

          BuildOutput(':'+FProjectName+':'+fn+':'+uname+':'+cid+#13#10);

          //TODO: alternate proto? either XML tag or default file.
          s:=FRootFolder+fn+XxmProtoExtension;
          if not(FileExists(s)) then s:=FProtoPath+uext+DelphiExtension;
          if not(FileExists(s)) then s:=FProtoPathDef+uext+DelphiExtension;

          p.Parse(ReadString(s));
          q.Parse(ReadString(FRootFolder+fn));
          repeat
            case p.GetNext of
              ptProjectName:p.Output(FProjectName);
              ptProtoFile:p.Output(FProtoPath+uext+DelphiExtension);
              ptFragmentID:p.Output(cid);
              ptFragmentUnit:p.Output(uname);
              ptFragmentAddress:p.Output(fnu);
              ptUsesClause:p.Output(q.AllSections(psUses));//TODO: check comma's?
              ptFragmentDefinitions:p.Output(q.AllSections(psDefinitions));
              ptFragmentHeader:p.Output(q.AllSections(psHeader));
              ptFragmentBody:p.Output(q.BuildBody);
              ptFragmentFooter:p.Output(q.AllSections(psFooter));
              //else raise?
            end;
          until p.Done;
          ForceDirectories(FSrcFolder+upath);
          p.Save(FSrcFolder+upath+uname+DelphiExtension);

          Result:=true;
         end;


       end;

      //delete missing files
      for sl_i:=0 to sl1.Count-1 do
       begin
        cid:=sl1[sl_i];
        xFile:=ForceNodeID(DataFiles,'File',cid);
        //TODO: setting keep pas?
        uname:=VarToStr(xFile.getAttribute('UnitName'));
        upath:=VarToStr(xFile.getAttribute('UnitPath'));
        DeleteFile(FSrcFolder+upath+uname+DelphiExtension);
        xFile.parentNode.removeChild(xFile);
        Modified:=true;
        Result:=true;
       end;

    finally
      sl.Free;
      sl1.Free;
    end;

    //check units
    xl:=DataFiles.selectNodes('Unit');
    try
      xFile:=xl.nextNode as IXMLDOMElement;
      while not(xFile=nil) and not(Result) do
       begin
        uname:=VarToStr(xFile.getAttribute('UnitName'));
        upath:=VarToStr(xFile.getAttribute('UnitPath'));
        fn:=upath+uname+DelphiExtension;

        s:=Signature(FRootFolder+fn);
        if not(Signatures.Values[uname]=s) then
         begin
          Signatures.Values[uname]:=s;
          Modified:=true;
          Result:=true;
         end;

        xFile:=xl.nextNode as IXMLDOMElement;
       end;
    finally
      xl:=nil;
    end;
    //missing? delete?

    GenerateProjectFiles(Rebuild);

  finally
    p.Free;
    q.Free;
  end;

end;

function TXxmWebProject.GenerateProjectFiles(Rebuild:boolean):boolean;
var
  p:TXxmProtoParser;
  x:IXMLDOMElement;
  xl:IXMLDOMNodeList;
  fh:THandle;
  fd:TWin32FindData;
  fn1,fn2,s:string;
  i:integer;
begin
  Result:=false;
  //project files
  fn1:=FSrcFolder+FProjectName+DelphiProjectExtension;
  fn2:=FRootFolder+ProtoProjectPas;
  if Modified or Rebuild or not(FileExists(fn1)) or not(FileExists(fn2)) then
   begin
    //TODO: flags from project XML?
    //TODO: signatures?

    p:=TXxmProtoParser.Create;
    try

      //[[ProjectName]].dpr
      BuildOutput(FProjectName+DelphiProjectExtension+#13#10);
      s:=FProtoPath+ProtoProjectDpr;
      if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectDpr;
      p.Parse(ReadString(s));
      repeat
        case p.GetNext of
          ptProjectName:p.Output(FProjectName);
          ptProtoFile:p.Output(FProtoPath+ProtoProjectDpr);
          ptIterateFragment:
           begin
            xl:=DataFiles.selectNodes('File');
            x:=xl.nextNode as IXMLDOMElement;
            p.IterateBegin(not(x=nil));
           end;
          ptIterateInclude:
           begin
            xl:=DataFiles.selectNodes('Unit');
            x:=xl.nextNode as IXMLDOMElement;
            p.IterateBegin(not(x=nil));
           end;
          ptFragmentUnit:p.Output(VarToStr(x.getAttribute('UnitName')));
          ptFragmentPath:p.Output(VarToStr(x.getAttribute('UnitPath')));
          ptFragmentAddress:p.Output(GetNode(x,'Path').text);
          ptIncludeUnit:p.Output(VarToStr(x.getAttribute('UnitName')));
          ptIncludePath:p.Output(VarToStr(x.getAttribute('UnitPath')));
          ptIterateEnd:
           begin
            x:=xl.nextNode as IXMLDOMElement;
            p.IterateNext(not(x=nil));
           end;
          ptUsesClause:    p.Output(NodesText(RootNode,'UsesClause'));
          ptProjectHeader: p.Output(NodesText(RootNode,'Header'));
          ptProjectBody:   p.Output(NodesText(RootNode,'Body'));
          //else raise?
        end;
      until p.Done;
      ForceDirectories(FSrcFolder);
      p.Save(fn1);

      //xxmp.pas
      if not(FileExists(fn2)) then
       begin
        BuildOutput(ProtoProjectPas+#13#10);
        s:=FProtoPath+ProtoProjectPas;
        if not(FileExists(s)) then s:=FProtoPathDef+ProtoProjectPas;
        p.Parse(ReadString(s));
        repeat
          case p.GetNext of
            ptProjectName:p.Output(FProjectName);
            ptProtoFile:p.Output(FProtoPath+ProtoProjectPas);
          end;
        until p.Done;
        p.Save(fn2);
       end;

      //copy other files the first time (cfg,dof,res...)
      fh:=FindFirstFile(PChar(FProtoPath+ProtoProjectMask),fd);
      if not(fh=INVALID_HANDLE_VALUE) then
       begin
        repeat
          s:=fd.cFileName;
          if not(s=ProtoProjectDpr) then
           begin
            i:=Length(s);
            while not(i=0) and not(s[i]='.') do dec(i);
            fn1:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn1)) then
             begin
              BuildOutput(fn1+#13#10);
              CopyFile(PChar(FProtoPath+s),PChar(fn1),false);
             end;
           end;
        until not(FindNextFile(fh,fd));
        Windows.FindClose(fh);
       end;

      fh:=FindFirstFile(PChar(FProtoPathDef+ProtoProjectMask),fd);
      if not(fh=INVALID_HANDLE_VALUE) then
       begin
        repeat
          s:=fd.cFileName;
          if not(s=ProtoProjectDpr) then
           begin
            i:=Length(s);
            while not(i=0) and not(s[i]='.') do dec(i);
            fn1:=FSrcFolder+FProjectName+Copy(s,i,Length(s)-i+1);
            if not(FileExists(fn1)) then
             begin
              BuildOutput(fn1+#13#10);
              CopyFile(PChar(FProtoPathDef+s),PChar(fn1),false);
             end;
           end;
        until not(FindNextFile(fh,fd));
        Windows.FindClose(fh);
       end;

    finally
      p.Free;
    end;

    Result:=true;
   end;

end;

function TXxmWebProject.GetNode(element: IXMLDOMElement;
  xpath: string): IXMLDOMElement;
begin
  Result:=element.selectSingleNode(xpath) as IXMLDOMElement;
end;

function TXxmWebProject.GetNodeText(element: IXMLDOMElement;
  xpath: string): string;
var
  x:IXMLDOMNode;
begin
  x:=element.selectSingleNode(xpath);
  if x=nil then Result:='' else Result:=x.text;
end;

function TXxmWebProject.ForceNode(element:IXMLDOMElement;tagname:string): IXMLDOMElement;
begin
  Result:=element.selectSingleNode(tagname) as IXMLDOMElement;
  if Result=nil then
   begin
    if tagname[1]=' ' then
     begin
      element.appendChild(element.ownerDocument.createTextNode(#13#10));
      Result:=element.ownerDocument.createElement(Copy(tagname,2,Length(tagname)-1));
     end
    else
      Result:=element.ownerDocument.createElement(tagname);
    element.appendChild(Result);
    Modified:=true;
   end;
end;

function TXxmWebProject.ForceNodeID(element: IXMLDOMElement; tagname,
  id: string): IXMLDOMElement;
begin
  Result:=element.selectSingleNode(tagname+'[@ID="'+id+'"]') as IXMLDOMElement;
  if Result=nil then
   begin
    if tagname[1]=' ' then
     begin
      element.appendChild(element.ownerDocument.createTextNode(#13#10));
      Result:=element.ownerDocument.createElement(Copy(tagname,2,Length(tagname)-1));
     end
    else
      Result:=element.ownerDocument.createElement(tagname);
    Result.setAttribute('ID',id);
    element.appendChild(Result);
    Modified:=true;
   end;
end;

function TXxmWebProject.ReadString(FilePath: string): string;
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

function TXxmWebProject.NodesText(element: IXMLDOMElement;xpath:string): string;
var
  xl:IXMLDOMNodeList;
  x:IXMLDOMElement;
  s:TStringStream;
begin
  //CDATA? seems to work with .text
  xl:=element.selectNodes(xpath);
  x:=xl.nextNode as IXMLDOMElement;
  s:=TStringStream.Create('');
  try
    while not(x=nil) do
     begin
      s.WriteString(x.text);
      x:=xl.nextNode as IXMLDOMElement;
     end;
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

function TXxmWebProject.Compile:boolean;
var
  cl1,cl2,cl3:string;
  pi:TProcessInformation;
  si:TStartupInfo;
  h1,h2:THandle;
  sa:TSecurityAttributes;
  h:THandleStream;
  f:TFileStream;
  c:cardinal;
  d:array[0..$FFF] of char;
  function DoCommand(cmd:string):boolean;
  begin
    if not(CreateProcess(nil,PChar(
      StringReplace(
        cmd,
          '[[ProjectName]]',FProjectName,[rfReplaceAll])
          //more?
      ),
      nil,nil,true,NORMAL_PRIORITY_CLASS,nil,PChar(FSrcFolder),si,pi)) then
      RaiseLastOSError;
    CloseHandle(pi.hThread);
    try
      while (WaitForSingleObject(pi.hProcess,50)=WAIT_TIMEOUT) or not(h.Size=0) do
       begin
        c:=h.Read(d[0],$1000);
        if not(c=0) then
         begin
          f.Write(d[0],c);
          d[c]:=#0;
          BuildOutput(d);
         end;
       end;
      Result:=GetExitCodeProcess(pi.hProcess,c) and (c=0);
    finally
      CloseHandle(pi.hProcess);
    end;
  end;
begin
  cl1:=GetNodeText(RootNode,'PreCompileCommand');
  cl2:=GetNodeText(RootNode,'CompileCommand');
  cl3:=GetNodeText(RootNode,'PostCompileCommand');
  if (cl1='') and (cl2='') and (cl3='') then Result:=true else
   begin
    //more?
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
      Result:=true;
      h:=THandleStream.Create(h1);
      try
        if Result and not(cl1='') then Result:=DoCommand(cl1);
        if Result and not(cl2='') then Result:=DoCommand(cl2);
        if Result and not(cl3='') then Result:=DoCommand(cl3);
      finally
        h.Free;
        CloseHandle(h1);
        CloseHandle(h2);
      end;
    finally
      f.Free;
    end;
   end;
end;

procedure TXxmWebProject.BuildOutput(Msg: string);
begin
  FOnOutput(Msg);
end;

end.
