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
    OpenDialog1: TOpenDialog;
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
  private
    Modified:boolean;
    ProjectPath,ProjectFolder:AnsiString;
    ProjectData:DOMDocument;
    function CheckModified:boolean;
    function LoadProject(Path:AnsiString;CreateNew:boolean):boolean;
    procedure SaveProject;
    function GetNode(element:IXMLDOMElement;xpath:WideString):IXMLDOMElement;
    procedure ExpandNode(node:TTreeNode);
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
    Result:=CreateNew or OpenDialog1.Execute;
    if Result then fn:=OpenDialog1.FileName;
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
      while not(j=0) and not(fn[j]=PathDelim) do dec(j);
      i:=j-1;
      while not(i<=0) and not(fn[i]=PathDelim) do dec(i);
      ProjectData.loadXML('<XxmWebProject>'#13#10#9'<ProjectName>'+Copy(fn,i+1,j-i-1)+'</ProjectName>'#13#10#9+
        '<CompileCommand>dcc32 -U..\..\public -Q [[ProjectName]].dpr</CompileCommand>'#13#10'</XxmWebProject>');
     end;
    ProjectPath:=fn;
    Caption:='xxm Project - '+fn;
    Application.Title:='xxm Project - '+fn;

    txtProjectName.Text:=GetNode(ProjectData.documentElement,'ProjectName').text;
    txtCompileCommand.Text:=GetNode(ProjectData.documentElement,'CompileCommand').text;

    i:=Length(ProjectPath);
    while not(i=0) and not(ProjectPath[i]=PathDelim) do dec(i);
    ProjectFolder:=Copy(ProjectPath,1,i);

    //load files
    ExpandNode(nil);

    Modified:=not(fe);
   end;
end;

procedure TEditProjectMainForm.SaveProject;
begin
  if txtProjectName.Text='' then raise Exception.Create('Project name required');
  GetNode(ProjectData.documentElement,'ProjectName').text:=txtProjectName.Text;
  GetNode(ProjectData.documentElement,'CompileCommand').text:=txtCompileCommand.Text;
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
  s:AnsiString;
begin
  if CheckModified then
   begin
    s:=txtProjectName.Text;
    if s='' then raise Exception.Create('Project name required');
    r:=TRegistry.Create;
    try
      r.RootKey:=HKEY_LOCAL_MACHINE;
      r.OpenKey('\Software\xxm\local\'+s,true);
      r.WriteString('',ProjectFolder+s+'.xxl');
      //TODO: default settings?
    finally
      r.Free;
    end;
    MessageBoxA(GetDesktopWindow,PAnsiChar('Project "'+s+'" registered.'),
      'xxm Project',MB_OK or MB_ICONINFORMATION);
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
    while not(n=nil) do
     begin
      d:=n.Text+PathDelim+d;
      n:=n.Parent;
     end;
    fh:=FindFirstFileA(PAnsiChar(ProjectFolder+d+'*.*'),fd);
    if not(fh=INVALID_HANDLE_VALUE) then
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
              while not(i=0) and not(fn[i]='.') do dec(i);
              if i=0 then fe:='' else fe:=LowerCase(Copy(fn,i,Length(fn)-i+1));
              if fe=DelphiExtension then //.pas
                if LowerCase(fn)=ProtoProjectPas then //xxmp.pas
                 begin
                  n.ImageIndex:=iiPasGenerated;
                 end
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
              else if fe=XxmFileExtension[ftPage] then //.xxm
               begin
                x:=ProjectData.documentElement.selectSingleNode(
                  'Files/Unit[Path="'+StringReplace(d,'\','\\',[rfReplaceAll])+Copy(fn,1,i-1)+'"]') as IXMLDOMElement;
                (n as TFileNode).ProjectNode:=x;
                n.ImageIndex:=iiXxm;
               end
              else if fe=XxmFileExtension[ftInclude] then //.xxmi
               begin
                x:=ProjectData.documentElement.selectSingleNode(
                  'Files/Unit[Path="'+StringReplace(d,'\','\\',[rfReplaceAll])+Copy(fn,1,i-1)+'"]') as IXMLDOMElement;
                (n as TFileNode).ProjectNode:=x;
                n.ImageIndex:=iiXxmi;
               end
              else if fe=XxmFileExtension[ftProject] then //.xxmp
               begin
                n.ImageIndex:=iiXxmp;
                //TODO: invalidate folder since it's another project
               end
              else if fe=DelphiProjectExtension then //.dpr
                n.ImageIndex:=iiDpr
              else if (fe='.cfg') or (fe='.dof') then
                n.ImageIndex:=iiFileGenerated
              else
                n.ImageIndex:=iiFile;
              n.SelectedIndex:=n.ImageIndex;
             end
            else
             begin
              //directory
              if not(fd.cFileName[0]='.') then
               begin
                fn:=fd.cFileName;
                n:=tvFiles.Items.AddChild(node,fn);
                (n as TFileNode).IsDir:=true;
                (n as TFileNode).ProjectNode:=nil;
                if ((node=nil) and (fn=SourceDirectory)) or
                   (not(n.Parent=nil) and (n.Parent.ImageIndex=iiDirGenerated)) then
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
        until not(FindNextFile(fh,fd));
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
begin
  nx:=tvFiles.Selected;
  n:=nx;
  s:='';
  while not(n=nil) do
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
    if not(x=nil) then
     begin
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
  //case (n of TFileNode) of
  case n.ImageIndex of
    iiPas:
     begin
      nx:=n;
      s:='';
      while not(nx=nil) do
       begin
        s:=PathDelim+nx.Text;
        nx:=nx.Parent;
       end;
      i:=Length(s);
      while not(i=0) and not(s[i]='.') do dec(i);
      j:=i;
      while not(j=0) and not(s[j]=PathDelim) do dec(j);
      x:=ProjectData.createElement('Unit');
      x.setAttribute('UnitName',Copy(s,j+1,i-j-1));
      if (j>1) then x.setAttribute('UnitPath',Copy(s,2,j));
      (n as TFileNode).ProjectNode:=x;
      y:=ProjectData.documentElement.selectSingleNode('Files') as IXMLDOMElement;
      if y=nil then
       begin
        y:=ProjectData.createElement('Files');
        ProjectData.documentElement.appendChild(y);
       end;
      y.appendChild(x);
      n.ImageIndex:=iiPasIncluded;
      n.SelectedIndex:=iiPasIncluded;
      Modified:=true;
     end;
    //more?
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
    iiPasIncluded:
     begin
      x:=(n as TFileNode).ProjectNode;
      x.parentNode.removeChild(x);
      n.ImageIndex:=iiPas;
      n.SelectedIndex:=iiPas;
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
begin
  n:=tvFiles.Selected;
  actInclude.Enabled:=not(n=nil) and (n.ImageIndex in [iiPas]);
  actExclude.Enabled:=not(n=nil) and (n.ImageIndex in [iiPasIncluded]);
  actDelete.Enabled:=not(n=nil);
end;

procedure TEditProjectMainForm.actRefreshExecute(Sender: TObject);
begin
  ExpandNode(nil);
end;

end.
