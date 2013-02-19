unit xxmEditProject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, MSXML2_TLB, ComCtrls, StdCtrls, Dialogs, ImgList, ActnList;

type
  TEditProjectMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    odOpenProject: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    txtProjectName: TEdit;
    Label2: TLabel;
    txtCompileCommand: TEdit;
    tvFiles: TTreeView;
    Open1: TMenuItem;
    btnRegisterLocal: TButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    Include1: TMenuItem;
    Exclude1: TMenuItem;
    actInclude: TAction;
    actExclude: TAction;
    N2: TMenuItem;
    actDelete: TAction;
    Delete1: TMenuItem;
    ree1: TMenuItem;
    Include2: TMenuItem;
    Exclude2: TMenuItem;
    N3: TMenuItem;
    Delete2: TMenuItem;
    actRefresh: TAction;
    N4: TMenuItem;
    Refresh1: TMenuItem;
    actIncludePas: TAction;
    odIncludeUnit: TOpenDialog;
    Includeunit2: TMenuItem;
    StatusBar1: TStatusBar;
    btnRegisterFile: TButton;
    odXxmXml: TOpenDialog;
    TabSheet3: TTabSheet;
    cbParserValue: TComboBox;
    txtParserValue: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    procedure Exit1Click(Sender: TObject);
    procedure txtChange(Sender: TObject);
    procedure tvFilesCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure New1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure btnRegisterLocalClick(Sender: TObject);
    procedure tvFilesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvFilesCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure tvFilesDblClick(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure tvFilesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure actIncludeExecute(Sender: TObject);
    procedure actExcludeExecute(Sender: TObject);
    procedure tvFilesChange(Sender: TObject; Node: TTreeNode);
    procedure actRefreshExecute(Sender: TObject);
    procedure actIncludePasExecute(Sender: TObject);
    procedure btnRegisterFileClick(Sender: TObject);
    procedure cbParserValueChange(Sender: TObject);
    procedure txtParserValueChange(Sender: TObject);
  private
    Modified:boolean;
    ProjectPath,ProjectFolder:AnsiString;
    ProjectData:DOMDocument;
    LastParserValue:integer;
    function CheckModified:boolean;
    function LoadProject(Path:AnsiString;CreateNew:boolean):boolean;
    procedure SaveProject;
    function GetNode(element:IXMLDOMElement;xpath:WideString):IXMLDOMElement;
    procedure ExpandNode(node:TTreeNode);
    function RootNodeAppend(const nodeName:WideString;
      node:IXMLDOMElement):IXMLDOMElement;
    procedure SaveParserValue;
  protected
    procedure DoCreate; override;
    procedure DoClose(var Action: TCloseAction); override;
  public

  end;

  TFileNode=class(TTreeNode)
  public
    IsDir:boolean;
    ProjectNode:IXMLDOMElement;
  end;

const
  ApplicationTitle='xxm Project Properties';

var
  EditProjectMainForm: TEditProjectMainForm;

implementation

uses DateUtils, xxmUtilities, Registry, ShellAPI, ComObj;

{$R *.dfm}

procedure TEditProjectMainForm.DoCreate;
var
  s:AnsiString;
begin
  inherited;
  ProjectData:=CoDOMDocument.Create;
  if ParamCount=0 then
   begin
    if not(LoadProject('',false)) then Application.Terminate;
   end
  else
   begin
    s:=ParamStr(1);
    if LowerCase(s)='/n' then s:=ExtractFilePath(ParamStr(2))+XxmProjectFileName;
    LoadProject(s,false);
   end;
  //assert Modified=false
  PageControl1.Align:=alClient;//fix!
end;

procedure TEditProjectMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TEditProjectMainForm.CheckModified: boolean;
begin
  Result:=true;
  if Modified then
    case MessageBox(Handle,'Save changes first?',ApplicationTitle,MB_YESNOCANCEL or MB_ICONQUESTION) of
      idYes:SaveProject;
      idNo:;
      idCancel:Result:=false;
    end;
end;

function TEditProjectMainForm.LoadProject(Path: AnsiString;
  CreateNew: boolean): boolean;
var
  fn:AnsiString;
  fe:boolean;
  i,j:integer;
begin
  //assert CheckModified called before

  if Path='' then
   begin
    Result:=CreateNew or odOpenProject.Execute;
    if Result then
     begin
      fn:=odOpenProject.FileName;
      SetForegroundWindow(Handle);
     end;
   end
  else
   begin
    Result:=true;//?
    fn:=Path;
    //Path could be by parameter, so resolve and expand
    if DirectoryExists(fn) then fn:=IncludeTrailingPathDelimiter(fn)+XxmProjectFileName;
   end;

  if Result then
   begin
    fe:=GetFileSize(fn)>0;
    if fe then
     begin
      if not(ProjectData.load(fn)) then
       begin
        MessageBoxW(Handle,PWideChar('Loading project failed:'#13#10+ProjectData.parseError.reason),
          ApplicationTitle,MB_OK or MB_ICONERROR);
        Result:=false;
       end;
     end
    else
     begin
      j:=Length(fn);
      while (j<>0) and (fn[j]<>PathDelim) do dec(j);
      i:=j-1;
      while (i>0) and (fn[i]<>PathDelim) do dec(i);
      ProjectData.loadXML('<XxmWebProject>'#13#10#9'<ProjectName>'+Copy(fn,i+1,j-i-1)+'</ProjectName>'#13#10#9+
        '<CompileCommand>dcc32 -U[[HandlerPath]]public -Q [[ProjectName]].dpr</CompileCommand>'#13#10'</XxmWebProject>');
     end;
    ProjectPath:=fn;
    Caption:='xxm Project - '+fn;
    Application.Title:='xxm Project - '+fn;

    txtProjectName.Text:=GetNode(ProjectData.documentElement,'ProjectName').text;
    txtCompileCommand.Text:=GetNode(ProjectData.documentElement,'CompileCommand').text;
    LastParserValue:=-1;
    cbParserValue.ItemIndex:=-1;

    i:=Length(ProjectPath);
    while (i<>0) and (ProjectPath[i]<>PathDelim) do dec(i);
    ProjectFolder:=Copy(ProjectPath,1,i);

    //load files
    ExpandNode(nil);

    Modified:=not(fe);
   end;
end;

procedure TEditProjectMainForm.SaveProject;
begin
  if txtProjectName.Text='' then raise Exception.Create('Project name required');
  SaveParserValue;
  GetNode(ProjectData.documentElement,'ProjectName').text:=txtProjectName.Text;
  GetNode(ProjectData.documentElement,'CompileCommand').text:=txtCompileCommand.Text;
  RootNodeAppend('LastModified',nil).text:=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now);//timezone?
  //TODO: files?
  ProjectData.save(ProjectPath);
  Modified:=false;
end;

procedure TEditProjectMainForm.txtChange(Sender: TObject);
begin
  Modified:=true;
end;

procedure TEditProjectMainForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if not(CheckModified) then Action:=caNone;
end;

procedure TEditProjectMainForm.tvFilesCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass:=TFileNode;
end;

function TEditProjectMainForm.GetNode(element: IXMLDOMElement;
  xpath: WideString): IXMLDOMElement;
begin
  Result:=element.selectSingleNode(xpath) as IXMLDOMElement;
end;

procedure TEditProjectMainForm.New1Click(Sender: TObject);
begin
  if CheckModified then LoadProject('',true);
end;

procedure TEditProjectMainForm.Save1Click(Sender: TObject);
begin
  SaveProject;
end;

procedure TEditProjectMainForm.Open1Click(Sender: TObject);
begin
  if CheckModified then LoadProject('',false);
end;

procedure TEditProjectMainForm.btnRegisterLocalClick(Sender: TObject);
var
  r:TRegistry;
  s,t,u:AnsiString;
begin
  if CheckModified then
   begin
    t:=txtProjectName.Text;
    if t='' then raise Exception.Create('Project name required');
    s:=ProjectFolder+t+'.xxl';
    r:=TRegistry.Create;
    try
      r.RootKey:=HKEY_CURRENT_USER;//HKEY_LOCAL_MACHINE;
      r.OpenKey('\Software\xxm\local\'+t,true);
      u:=r.ReadString('');
      if (u='') or (u=s) or (MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+
        '" was already registered as'#13#10'  '+u+
        #13#10'Do you want to overwrite this registration?'#13#10'  '+s),
        'xxm Project',MB_OKCANCEL or MB_ICONQUESTION or MB_SYSTEMMODAL)=idOK) then
       begin
        r.WriteString('',s);
        r.DeleteValue('Signature');
        //TODO: default settings?
        MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+'" registered.'),
          'xxm Project',MB_OK or MB_ICONINFORMATION);
       end;
    finally
      r.Free;
    end;
   end;
end;

const
  iiDir=0;
  iiDirIncluded=1;
  iiDirGenerated=2;
  iiFile=3;
  iiFileIncluded=4;
  iiFileGenerated=5;
  iiPas=6;
  iiPasIncluded=7;
  iiPasGenerated=8;
  iiDpr=9;
  iiXxm=10;
  iiXxmi=11;
  iiXxmp=12;
  iiXxl=13;

procedure TEditProjectMainForm.ExpandNode(node: TTreeNode);
var
  fh:THandle;
  fd:TWin32FindDataA;
  d,fn,fe:AnsiString;
  ft:TXxmFileType;
  n:TTreeNode;
  i:integer;
  x:IXMLDOMElement;
begin
  tvFiles.Items.BeginUpdate;
  try
    tvFiles.SortType:=stNone;
    if node=nil then tvFiles.Items.Clear else node.DeleteChildren;
    d:='';
    n:=node;
    while n<>nil do
     begin
      d:=n.Text+PathDelim+d;
      n:=n.Parent;
     end;
    fh:=FindFirstFileA(PAnsiChar(ProjectFolder+d+'*.*'),fd);
    if fh<>INVALID_HANDLE_VALUE then
      try
        repeat
          if ((fd.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)=0) and
             ((fd.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)=0) then
            if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
             begin
              //file
              n:=tvFiles.Items.AddChild(node,fd.cFileName);
              (n as TFileNode).IsDir:=false;
              (n as TFileNode).ProjectNode:=nil;
              fn:=fd.cFileName;
              i:=Length(fn);
              while (i<>0) and (fn[i]<>'.') do dec(i);
              if i=0 then fe:='' else fe:=LowerCase(Copy(fn,i,Length(fn)-i+1));
              if fe=DelphiExtension then //.pas
                if LowerCase(fn)=ProtoProjectPas then //xxmp.pas
                  n.ImageIndex:=iiPasGenerated
                else
                 begin
                  x:=ProjectData.documentElement.selectSingleNode(
                    'Files/Unit[@UnitName="'+Copy(fn,1,i-1)+'"&&@UnitPath="'+
                    StringReplace(d,'\','\\',[rfReplaceAll])+'"]') as IXMLDOMElement;
                  if (x=nil) and (d='') then x:=ProjectData.documentElement.selectSingleNode(
                    'Files/Unit[@UnitName="'+Copy(fn,1,i-1)+'"]') as IXMLDOMElement;
                  if x=nil then
                    n.ImageIndex:=iiPas
                  else
                   begin
                    (n as TFileNode).ProjectNode:=x;
                    n.ImageIndex:=iiPasIncluded;
                   end;
                 end
              else if fe=DelphiProjectExtension then //.dpr
                n.ImageIndex:=iiDpr
              else if (fe='.cfg') or (fe='.dof') then
                n.ImageIndex:=iiFileGenerated
              else
               begin
                ft:=TXxmFileType(0);
                while (ft<>ft_Unknown) and (fe<>XxmFileExtension[ft]) do inc(ft);
                case ft of
                  ftPage,ftInclude:
                   begin
                    x:=ProjectData.documentElement.selectSingleNode(
                      'Files/Unit[Path="'+StringReplace(d,'\','\\',[rfReplaceAll])+Copy(fn,1,i-1)+'"]') as IXMLDOMElement;
                    (n as TFileNode).ProjectNode:=x;
                    if ft=ftPage then
                      n.ImageIndex:=iiXxm
                    else
                      n.ImageIndex:=iiXxmi;
                   end;
                  ftProject://.xxmp
                   begin
                    n.ImageIndex:=iiXxmp;
                    //TODO: invalidate folder since it's another project
                   end;
                  ft_Unknown:
                   begin
                    x:=ProjectData.documentElement.selectSingleNode(
                      'Files/Resource[Path="'+StringReplace(d,'\','\\',[rfReplaceAll])+fn+'"]') as IXMLDOMElement;
                    (n as TFileNode).ProjectNode:=x;
                    if x=nil then
                      n.ImageIndex:=iiFile
                    else
                      n.ImageIndex:=iiFileIncluded;
                   end;
                end;
               end;
              n.SelectedIndex:=n.ImageIndex;
             end
            else
             begin
              //directory
              if (fd.cFileName[0]<>'.') then
               begin
                fn:=fd.cFileName;
                n:=tvFiles.Items.AddChild(node,fn);
                (n as TFileNode).IsDir:=true;
                (n as TFileNode).ProjectNode:=nil;
                if ((node=nil) and ((fn=SourceDirectory) or (fn=ProtoDirectory))) or
                   ((n.Parent<>nil) and (n.Parent.ImageIndex=iiDirGenerated)) then
                 begin
                  n.ImageIndex:=iiDirGenerated;
                  //n.HasChildren:=true;
                  //TODO: map generated pas files on <Unit> tags
                 end
                else
                 begin
                  n.ImageIndex:=iiDir;
                  n.HasChildren:=true;
                 end;
                //ProtoDirectory?
                n.SelectedIndex:=n.ImageIndex;
               end;
             end;
        until not(FindNextFileA(fh,fd));
      finally
        Windows.FindClose(fh);
      end;
    tvFiles.SortType:=stData;
  finally
    tvFiles.Items.EndUpdate;
  end;
  //TODO: merge (missing) XML items?
end;

procedure TEditProjectMainForm.tvFilesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  ExpandNode(Node);
end;

procedure TEditProjectMainForm.tvFilesCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare:=0;
  if (Node1 as TFileNode).IsDir then dec(Compare);
  if (Node2 as TFileNode).IsDir then inc(Compare);
  if Compare=0 then Compare:=AnsiCompareText(Node1.Text,Node2.Text);
end;

procedure TEditProjectMainForm.tvFilesDblClick(Sender: TObject);
begin
  if actInclude.Enabled then actInclude.Execute;
end;

procedure TEditProjectMainForm.actDeleteExecute(Sender: TObject);
var
  so:TSHFileOpStructA;
  n,nx:TTreeNode;
  s:AnsiString;
  x:IXMLDOMElement;
  y:IXMLDOMNode;
begin
  nx:=tvFiles.Selected;
  n:=nx;
  s:='';
  while n<>nil do
   begin
    s:=s+PathDelim+n.Text;
    n:=n.Parent;
   end;
  so.Wnd:=Handle;
  so.wFunc:=FO_DELETE;
  so.pFrom:=PAnsiChar(ProjectFolder+Copy(s,2,Length(s)-1));
  so.pTo:=nil;
  so.fFlags:=FOF_ALLOWUNDO;
  so.fAnyOperationsAborted:=false;
  so.hNameMappings:=nil;
  so.lpszProgressTitle:=nil;
  OleCheck(SHFileOperationA(so));
  if not(so.fAnyOperationsAborted) then
   begin
    x:=(nx as TFileNode).ProjectNode;
    if x<>nil then
     begin
      y:=x.previousSibling;
      if (y<>nil) and (y.nodeType=NODE_TEXT) then x.parentNode.removeChild(y);//whitespace
      x.parentNode.removeChild(x);
      x:=nil;
     end;
    nx.Delete;
   end;
end;

procedure TEditProjectMainForm.tvFilesContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  //odd, RightClickSelect doesn't work...
  //Handled:=false;
  tvFiles.Selected:=tvFiles.GetNodeAt(MousePos.X,MousePos.Y);

  //case (n as TFileNode). of
end;

procedure TEditProjectMainForm.actIncludeExecute(Sender: TObject);
var
  n,nx:TTreeNode;
  x,y:IXMLDOMElement;
  s:AnsiString;
  i,j:integer;
begin
  n:=tvFiles.Selected;
  x:=nil;//default
  nx:=n;
  s:='';
  while nx<>nil do
   begin
    s:=PathDelim+nx.Text+s;
    nx:=nx.Parent;
   end;
  //case (n of TFileNode) of
  case n.ImageIndex of
    iiPas:
     begin
      i:=Length(s);
      while (i<>0) and (s[i]<>'.') do dec(i);
      j:=i;
      while (j<>0) and (s[j]<>PathDelim) do dec(j);
      x:=ProjectData.createElement('Unit');
      x.setAttribute('UnitName',Copy(s,j+1,i-j-1));
      if (j>1) then x.setAttribute('UnitPath',Copy(s,2,j-1));
      n.ImageIndex:=iiPasIncluded;
     end;
    iiFile:
     begin
      x:=ProjectData.createElement('Resource');
      y:=ProjectData.createElement('Path');
      y.text:=Copy(s,2,Length(s));
      x.appendChild(y);
      n.ImageIndex:=iiFileIncluded;
     end;
    //more?
  end;
  if x<>nil then
   begin
    (n as TFileNode).ProjectNode:=x;
    n.SelectedIndex:=n.ImageIndex;
    RootNodeAppend('Files',x);
    Modified:=true;
   end;
  tvFilesChange(tvFiles,n);
end;

procedure TEditProjectMainForm.actExcludeExecute(Sender: TObject);
var
  n:TTreeNode;
  x:IXMLDOMElement;
begin
  n:=tvFiles.Selected;
  //case (n of TFileNode) of
  case n.ImageIndex of
    iiPasIncluded,iiFileIncluded:
     begin
      x:=(n as TFileNode).ProjectNode;
      x.parentNode.removeChild(x);
      n.ImageIndex:=n.ImageIndex-1;
      n.SelectedIndex:=n.ImageIndex;
      (n as TFileNode).ProjectNode:=nil;
      Modified:=true;
     end;
    //more?
  end;
  tvFilesChange(tvFiles,n);
end;

procedure TEditProjectMainForm.tvFilesChange(Sender: TObject;
  Node: TTreeNode);
var
  n:TTreeNode;
  s:string;
begin
  n:=tvFiles.Selected;
  actInclude.Enabled:=(n<>nil) and (n.ImageIndex in [iiPas,iiFile]);
  actExclude.Enabled:=(n<>nil) and (n.ImageIndex in [iiPasIncluded,iiFileIncluded]);
  actDelete.Enabled:=(n<>nil);
  s:='';
  while n<>nil do
   begin
    s:='\'+n.Text+s;
    n:=n.Parent;
   end;
  StatusBar1.Panels[0].Text:=Copy(s,2,Length(s));
end;

procedure TEditProjectMainForm.actRefreshExecute(Sender: TObject);
begin
  ExpandNode(nil);
end;

procedure TEditProjectMainForm.actIncludePasExecute(Sender: TObject);
var
  x:IXMLDOMElement;
  s,t:AnsiString;
  i,j,l,fi,fl,fc:integer;
begin
  if odIncludeUnit.Execute then
   begin
    fc:=0;
    fl:=odIncludeUnit.Files.Count;
    for fi:=0 to fl-1 do
     begin
      s:=odIncludeUnit.Files[fi];
      if LowerCase(Copy(s,1,Length(ProjectFolder)))=LowerCase(ProjectFolder) then
        raise Exception.Create('Use include on a tree node to include a file in the project folder.');//TODO
      //build relative to ProjectFolder
      l:=Length(ProjectFolder);
      j:=Length(s);
      i:=1;
      while (i<=l) and (i<=j) and (UpCase(s[i])=UpCase(ProjectFolder[i])) do inc(i);
      while (i>0) and (s[i]<>PathDelim) do dec(i);
      //assert (i<=l)
      s:=Copy(s,i+1,j-i);
      inc(i);
      while i<=l do
       begin
        if ProjectFolder[i]=PathDelim then s:='..'+PathDelim+s;
        inc(i);
       end;
      //strip extension, path
      i:=Length(s);
      while (i<>0) and (s[i]<>'.') do dec(i);
      j:=i;
      while (j<>0) and (s[j]<>PathDelim) do dec(j);
      t:='@UnitName="'+Copy(s,j+1,i-j-1)+'"';
      if j>0 then t:=t+'&&@UnitPath="'+StringReplace(Copy(s,1,j),'\','\\',[rfReplaceAll])+'"';
      if ProjectData.documentElement.selectSingleNode('Files/Unit['+t+']')=nil then
       begin
        x:=ProjectData.createElement('Unit');
        x.setAttribute('UnitName',Copy(s,j+1,i-j-1));
        if j>1 then x.setAttribute('UnitPath',Copy(s,1,j));
        //(n as TFileNode).ProjectNode:=x;
        RootNodeAppend('Files',x);
        Modified:=true;
        inc(fc);
        if fl=1 then MessageBoxA(Handle,PAnsiChar('Unit "'+s+'" added'),
          'xxm Project',MB_OK or MB_ICONINFORMATION);
       end
      else
        if fl=1 then MessageBoxA(Handle,PAnsiChar(
          'Unit "'+s+'" is aready added to the project'),
          'xxm Project',MB_OK or MB_ICONERROR);
     end;
    if fl>1 then MessageBoxA(Handle,PAnsiChar(IntToStr(fc)+' units added'),
      'xxm Project',MB_OK or MB_ICONINFORMATION);
   end;
end;

function TEditProjectMainForm.RootNodeAppend(const nodeName:WideString;
  node:IXMLDOMElement):IXMLDOMElement;
var
  x:IXMLDOMElement;
  y:IXMLDOMNode;
begin
  x:=ProjectData.documentElement.selectSingleNode(nodeName) as IXMLDOMElement;
  if x=nil then
   begin
    x:=ProjectData.createElement(nodeName);
    ProjectData.documentElement.appendChild(ProjectData.createTextNode(#13#10#9));
    ProjectData.documentElement.appendChild(x);
    ProjectData.documentElement.appendChild(ProjectData.createTextNode(#13#10));
    if node<>nil then x.appendChild(ProjectData.createTextNode(#13#10#9));
   end;
  if node<>nil then
   begin
    y:=x.lastChild;
    if (y=nil) or (y.nodeType<>NODE_TEXT) then
      x.appendChild(ProjectData.createTextNode(#13#10#9#9))
    else
      x.appendChild(ProjectData.createTextNode(#9));
    x.appendChild(node);
    x.appendChild(ProjectData.createTextNode(#13#10#9));
   end;
  Result:=x;
end;

procedure TEditProjectMainForm.btnRegisterFileClick(Sender: TObject);
var
  fn,s,t,u:AnsiString;
  doc:DOMDocument;
  x,y:IXMLDOMElement;
begin
  if CheckModified then
   begin
    t:=txtProjectName.Text;
    if t='' then raise Exception.Create('Project name required');
    s:=ProjectFolder+t+'.xxl';
    if odXxmXml.Execute then
     begin
      fn:=odXxmXml.FileName;
      doc:=CoDOMDocument.Create;
      if FileExists(fn) then
       begin
        if not(doc.load(fn)) then
          raise Exception.Create('Loading xxm.xml failed:'#13#10+doc.parseError.reason);
       end
      else
        doc.loadXML('<ProjectRegistry />');
      x:=doc.documentElement.selectSingleNode('Project[@Name="'+t+'"]') as IXMLDOMElement;
      if x=nil then u:='' else
       begin
        y:=x.selectSingleNode('ModulePath') as IXMLDOMElement;
        if y=nil then u:='[<Project Name="'+t+'"> but no <ModulePath> found]' else u:=y.text;
       end;
      if (u='') or (u=s) or (MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+
        '" was already registered as'#13#10'  '+u+
        #13#10'Do you want to overwrite this registration?'#13#10'  '+s),
        'xxm Project',MB_OKCANCEL or MB_ICONQUESTION or MB_SYSTEMMODAL)=idOK) then
       begin
        if x=nil then
         begin
          x:=doc.createElement('Project');
          x.setAttribute('Name',t);
          doc.documentElement.appendChild(doc.createTextNode(#13#10#9));
          doc.documentElement.appendChild(x);
          doc.documentElement.appendChild(doc.createTextNode(#13#10));
         end
        else
         begin
          x.removeAttribute('Signature');
          x.removeAttribute('Alias');//?
         end;
        if y=nil then
         begin
          y:=doc.createElement('ModulePath');
          x.appendChild(y);
         end;
        y.text:=s;
        doc.save(fn);
        MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+t+'" registered with "'+fn+'".'),
          'xxm Project',MB_OK or MB_ICONINFORMATION);
       end;
     end;
   end;
end;

const
  ParserValueElement:array[0..3] of string=(
    'SendOpen',
    'SendClose',
    'SendHTMLOpen',
    'SendHTMLClose'
  );

procedure TEditProjectMainForm.cbParserValueChange(Sender: TObject);
var
  x:IXMLDOMNode;
begin
  if cbParserValue.ItemIndex=-1 then
    txtParserValue.Text:=''
  else
   begin
    SaveParserValue;
    x:=GetNode(ProjectData.documentElement,'ParserValues/'+
      ParserValueElement[cbParserValue.ItemIndex]);
    if x=nil then
      txtParserValue.Text:=''
    else
      txtParserValue.Text:=x.text;
   end;
  //txtParserValue.Modified:=false;
  LastParserValue:=cbParserValue.ItemIndex;
end;

procedure TEditProjectMainForm.txtParserValueChange(Sender: TObject);
begin
  Modified:=true;
end;

procedure TEditProjectMainForm.SaveParserValue;
var
  x:IXMLDOMElement;
begin
  if LastParserValue<>-1 then
   begin
    x:=GetNode(ProjectData.documentElement,'ParserValues/'+
      ParserValueElement[LastParserValue]);
    if txtParserValue.Text='' then
     begin
      if x<>nil then x.parentNode.removeChild(x);
     end
    else
     begin
      if x=nil then
       begin
        x:=ProjectData.createElement(ParserValueElement[LastParserValue]);
        RootNodeAppend('ParserValues',x);
       end;
      x.text:=txtParserValue.Text;
     end;
   end;
end;

end.
