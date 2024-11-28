unit xxmEditProject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Menus, System.ImageList, Vcl.ImgList, System.Actions,
  Vcl.ActnList, jsonDoc, Winapi.ShellAPI, System.Win.ComObj;

type
  TEditProjectForm = class(TForm)
    PageControl1: TPageControl;
    tsProject: TTabSheet;
    tsFiles: TTabSheet;
    tsParserVals: TTabSheet;
    Label2: TLabel;
    txtPreCompCmds: TMemo;
    Label5: TLabel;
    txtCompCmds: TMemo;
    Label6: TLabel;
    txtPostCompCmds: TMemo;
    btnRegisterFile: TButton;
    ActionList1: TActionList;
    actRefresh: TAction;
    actInclude: TAction;
    actExclude: TAction;
    actDelete: TAction;
    actIncludePas: TAction;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ree1: TMenuItem;
    Refresh1: TMenuItem;
    N4: TMenuItem;
    Include2: TMenuItem;
    Exclude2: TMenuItem;
    Includeunit2: TMenuItem;
    N3: TMenuItem;
    Delete2: TMenuItem;
    PopupMenu1: TPopupMenu;
    Include1: TMenuItem;
    Exclude1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    odOpenProject: TOpenDialog;
    odIncludeUnit: TOpenDialog;
    odXxmJson: TOpenDialog;
    tvFiles: TTreeView;
    Label3: TLabel;
    cbParserValue: TComboBox;
    txtParserValue: TMemo;
    Label4: TLabel;
    Label1: TLabel;
    txtProjectName: TEdit;
    procedure txtProjectNameChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure tvFilesCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure tvFilesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvFilesCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure tvFilesDblClick(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actIncludeExecute(Sender: TObject);
    procedure actExcludeExecute(Sender: TObject);
    procedure actIncludePasExecute(Sender: TObject);
    procedure tvFilesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure tvFilesChange(Sender: TObject; Node: TTreeNode);
    procedure btnRegisterFileClick(Sender: TObject);
    procedure cbParserValueChange(Sender: TObject);
    procedure txtParserValueChange(Sender: TObject);
    procedure txtChange(Sender: TObject);
  private
    Modified:boolean;
    ProjectPath,ProjectFolder:string;
    ProjectData:IJSONDocument;
    LastParserValue:integer;
    function CheckModified:boolean;
    function LoadProject(const Path:string;CreateNew:boolean):boolean;
    procedure SaveProject;
    procedure ExpandNode(node:TTreeNode);
    procedure SaveParserValue;
  protected
    procedure DoCreate; override;
    procedure DoClose(var Action: TCloseAction); override;
  end;

  TFileNode=class(TTreeNode)
  public
    IsDir:boolean;
    Col,Key:string;
    Doc:IJSONDocument;
  end;

var
  EditProjectForm: TEditProjectForm;

const
  XxmProjectFileName='Web.xxmp';
  XxmModuleExtension='.xxl';
  XxmProtoExtension='.proto.pas';
  DelphiProjectExtension='.dpr';
  DelphiExtension='.pas';
  ProjectLogExtension='.log';
  SignaturesFileName='xxmp.~db';
  SignaturesUpdateReasonKey='::';
  LinesMapExtension='.~ln';

  ProtoProjectDpr='Web.dpr';
  ProtoProjectMask='Web.*';
  ProtoProjectPas='xxmp.pas';
  ProtoDirectory='proto';
  SourceDirectory='src';

implementation

{$R *.dfm}

function GetFileSize(const Path: string): integer;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  fh:=FindFirstFile(PChar(Path),fd);
  if fh=INVALID_HANDLE_VALUE then Result:=-1 else
   begin
    //assert(fd.nFileSizeHigh=0
    Result:=fd.nFileSizeLow;
    Winapi.Windows.FindClose(fh);
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

procedure TEditProjectForm.DoCreate;
var
  s:string;
begin
  inherited;
  ProjectData:=JSON;
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

procedure TEditProjectForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if not(CheckModified) then Action:=caNone;
end;

procedure TEditProjectForm.actDeleteExecute(Sender: TObject);
var
  so:TSHFileOpStruct;
  n,nx:TTreeNode;
  s:string;
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
  so.pFrom:=PChar(ProjectFolder+Copy(s,2,Length(s)-1));
  so.pTo:=nil;
  so.fFlags:=FOF_ALLOWUNDO;
  so.fAnyOperationsAborted:=false;
  so.hNameMappings:=nil;
  so.lpszProgressTitle:=nil;
  OleCheck(SHFileOperation(so));
  if not(so.fAnyOperationsAborted) then
   begin
    JSON(ProjectData[(nx as TFileNode).Col]).Delete((nx as TFileNode).Key);
    nx.Delete;
   end;
end;

procedure TEditProjectForm.actExcludeExecute(Sender: TObject);
var
  n:TFileNode;
begin
  n:=tvFiles.Selected as TFileNode;
  //case (n of TFileNode) of
  case n.ImageIndex of
    iiPasIncluded,iiFileIncluded:
     begin
      JSON(ProjectData[n.Col]).Delete(n.Key);
      n.Doc:=nil;
      n.ImageIndex:=n.ImageIndex-1;
      n.SelectedIndex:=n.ImageIndex;
      Modified:=true;
     end;
    //more?
  end;
  tvFilesChange(tvFiles,n);
end;

procedure TEditProjectForm.actIncludeExecute(Sender: TObject);
var
  n,nx:TTreeNode;
  nn:TFileNode;
  s:string;
  i,j:integer;
  d:IJSONDocument;
begin
  n:=tvFiles.Selected;
  nx:=n;
  s:='';
  while nx<>nil do
   begin
    s:=PathDelim+nx.Text+s;
    nx:=nx.Parent;
   end;
  nn:=n as TFileNode;
  case n.ImageIndex of
    iiPas:
     begin
      i:=Length(s);
      while (i<>0) and (s[i]<>'.') do dec(i);
      j:=i;
      while (j<>0) and (s[j]<>PathDelim) do dec(j);
      nn.ImageIndex:=iiPasIncluded;
      nn.Col:='units';
      nn.Key:=Copy(s,j+1,i-j-1);
      nn.Doc:=JSON;
      if j>1 then nn.Doc['unitPath']:=
        StringReplace(Copy(s,2,j-1),PathDelim,'/',[rfReplaceAll]);
      d:=JSON(ProjectData[nn.Col]);
      if d=nil then
       begin
        d:=JSON;
        ProjectData[nn.Col]:=d;
       end;
      d[nn.Key]:=nn.Doc;
      Modified:=true;
     end;
    iiFile:
     begin
      nn.ImageIndex:=iiFileIncluded;
      nn.Col:='resources';
      nn.Key:=StringReplace(Copy(s,2,Length(s)),PathDelim,'/',[rfReplaceAll]);
      nn.Doc:=JSON;
      d:=JSON(ProjectData[nn.Col]);
      if d=nil then
       begin
        d:=JSON;
        ProjectData[nn.Col]:=d;
       end;
      d[nn.Key]:=nn.Doc;
      Modified:=true;
     end;
    //more?
  end;
  nn.SelectedIndex:=nn.ImageIndex;
  tvFilesChange(tvFiles,n);
end;

procedure TEditProjectForm.actIncludePasExecute(Sender: TObject);
var
  x,z:IJSONDocument;
  y:IJSONEnumerator;
  s,t,u:string;
  i,j,l,fi,fl,fc:integer;
begin
  odIncludeUnit.InitialDir:=ProjectFolder;
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
      t:=LowerCase(s);
      u:=LowerCase(ProjectFolder);
      i:=1;
      while (i<=l) and (i<=j) and (t[i]=u[i]) do inc(i);
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

      t:=Copy(s,j+1,i-j-1);//unitName
      u:=StringReplace(Copy(s,1,j),PathDelim,'/',[rfReplaceAll]);//unitPath

      x:=nil;
      y:=JSONEnum(ProjectData['units']);
      while (x=nil) and y.Next do
       begin
        x:=JSON(y.Value);
        if (y.Key=t) and ((j=0) or (VarToStr(x['unitPath'])=u)) then
         begin
          if fl=1 then MessageBox(Handle,PChar(
            'Unit "'+s+'" is aready added to the project'),
            'xxm Project',MB_OK or MB_ICONERROR);
         end
        else
          x:=nil;
       end;
      if x=nil then
       begin
        x:=JSON;
        if j<>0 then x['unitPath']:=u;
        //(n as TFileNode).Doc:=x;
        z:=JSON(ProjectData['units']);
        if z=nil then
         begin
          z:=JSON;
          ProjectData['units']:=z;
         end;
        z[t]:=x;
        Modified:=true;
        inc(fc);
        if fl=1 then MessageBox(Handle,PChar('Unit "'+s+'" added'),
          'xxm Project',MB_OK or MB_ICONINFORMATION);
       end;
     end;
    if fl>1 then MessageBox(Handle,PChar(IntToStr(fc)+' units added'),
      'xxm Project',MB_OK or MB_ICONINFORMATION);
   end;
end;

procedure TEditProjectForm.actRefreshExecute(Sender: TObject);
begin
  ExpandNode(nil);
end;

function TEditProjectForm.CheckModified: boolean;
begin
  Result:=true;
  if Modified then
    case MessageBox(Handle,'Save changes first?',PChar(Caption),
      MB_YESNOCANCEL or MB_ICONQUESTION) of
      idYes:SaveProject;
      idNo:;
      idCancel:Result:=false;
    end;
end;

procedure TEditProjectForm.tvFilesChange(Sender: TObject; Node: TTreeNode);
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
    s:=PathDelim+n.Text+s;
    n:=n.Parent;
   end;
  //StatusBar1.Panels[0].Text:=Copy(s,2,Length(s));
end;

procedure TEditProjectForm.tvFilesCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare:=0;
  if (Node1 as TFileNode).IsDir then dec(Compare);
  if (Node2 as TFileNode).IsDir then inc(Compare);
  if Compare=0 then Compare:=AnsiCompareText(Node1.Text,Node2.Text);
end;

procedure TEditProjectForm.tvFilesContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  //odd, RightClickSelect doesn't work...
  //Handled:=false;
  tvFiles.Selected:=tvFiles.GetNodeAt(MousePos.X,MousePos.Y);

  //case (n as TFileNode). of
end;

procedure TEditProjectForm.tvFilesCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass:=TFileNode;
end;

procedure TEditProjectForm.tvFilesDblClick(Sender: TObject);
begin
  if actInclude.Enabled then actInclude.Execute;
end;

procedure TEditProjectForm.tvFilesExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  ExpandNode(Node);
end;

procedure TEditProjectForm.txtParserValueChange(Sender: TObject);
begin
  Modified:=true;
end;

procedure TEditProjectForm.txtChange(Sender: TObject);
begin
  Modified:=true;
end;

procedure TEditProjectForm.txtProjectNameChange(Sender: TObject);
begin
  Caption:='xxm Project - '+txtProjectName.Text;
  Modified:=true;
end;

function TEditProjectForm.LoadProject(const Path: string;
  CreateNew: boolean): boolean;
var
  f:TFileStream;
  fn:string;
  fu:UTF8String;
  fd:WideString;
  fe:boolean;
  i,j:integer;

  procedure LoadMemo(const key: WideString; m: TMemo);
  var
    v:Variant;
    i:integer;
  begin
    v:=ProjectData[key];
    m.Lines.BeginUpdate;
    try
      if VarIsArray(v) then
       begin
        m.Lines.Clear;
        for i:=VarArrayLowBound(v,1) to VarArrayHighBound(v,1) do
          m.Lines.Add(VarToStr(v[i]));
       end
      else
        m.Text:=VarToStr(v);
    finally
      m.Lines.EndUpdate;
    end;
  end;

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
    if DirectoryExists(fn) then
      fn:=IncludeTrailingPathDelimiter(fn)+XxmProjectFileName;
   end;

  if Result then
   begin
    fe:=GetFileSize(fn)>0;
    if fe then
     begin

      f:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
      try
        i:=f.Size;
        SetLength(fu,i);
        if i<>f.Read(fu[1],i) then RaiseLastOSError;
        if (i>=3) and (fu[1]=#$EF) and (fu[2]=#$BB) and (fu[3]=#$BF) then
            fd:=UTF8ToWideString(Copy(fu,4,i-3))
        else
          if (i>=2) and (fu[1]=#$FF) and (fu[2]=#$FE) then
              fd:=PWideChar(@fu[1])
          else
              fd:=WideString(fu);
      finally
        f.Free;
      end;

      ProjectData.Parse(fd);
     end
    else
     begin
      j:=Length(fn);
      while (j<>0) and (fn[j]<>PathDelim) do dec(j);
      i:=j-1;
      while (i>0) and (fn[i]<>PathDelim) do dec(i);
      ProjectData['name']:=Copy(fn,i+1,j-i-1);
      ProjectData['compileCommand']:='dcc32 "-U[[HandlerPath]]include" -Q "[[ProjectName]].dpr"';
      ProjectData['files']:=JSON;
      ProjectData['units']:=JSON;
      ProjectData['resources']:=JSON;
      //'UUID' here?
     end;
    ProjectPath:=fn;

    txtProjectName.Text:=VarToStr(ProjectData['name']);
    LoadMemo('preCompileCommand',txtPreCompCmds);
    LoadMemo('compileCommand',txtCompCmds);
    LoadMemo('postCompileCommand',txtPostCompCmds);
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

procedure TEditProjectForm.New1Click(Sender: TObject);
begin
  if CheckModified then LoadProject('',true);
end;

procedure TEditProjectForm.Open1Click(Sender: TObject);
begin
  if CheckModified then LoadProject('',false);
end;

procedure TEditProjectForm.SaveProject;
var
  s:AnsiString;
  f:TFileStream;

  procedure SaveMemo(const key: WideString; m: TMemo);
  var
    i,l:integer;
    v:Variant;
  begin
    m.Lines.BeginUpdate;
    try
      l:=m.Lines.Count;
      i:=l;
      while i<>0 do
       begin
        dec(i);
        if Trim(m.Lines[i])='' then
         begin
          m.Lines.Delete(i);
          dec(l);
         end;
       end;
      case l of
        0:ProjectData.Delete(key);//ProjectData[key]:=Null;//?
        1:ProjectData[key]:=m.Text;
        else
         begin
          dec(l);
          v:=VarArrayCreate([0,l],varOleStr);
          for i:=0 to l do
            v[i]:=m.Lines[i];
          ProjectData[key]:=v;
         end;
      end;
    finally
      m.Lines.EndUpdate;
    end;
  end;

begin
  if txtProjectName.Text='' then raise Exception.Create('Project name required');
  SaveParserValue;
  ProjectData['name']:=txtProjectName.Text;
  SaveMemo('preCompileCommand',txtPreCompCmds);
  SaveMemo('compileCommand',txtCompCmds);
  SaveMemo('postCompileCommand',txtPostCompCmds);
  ProjectData['lastModified']:=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now);//timezone?
  s:=#$EF#$BB#$BF+UTF8Encode(ProjectData.AsString);
  f:=TFileStream.Create(ProjectPath,fmCreate);
  try
    f.Write(s[1],Length(s));
  finally
    f.Free;
  end;
  Modified:=false;
end;

const
  ParserValueElement:array[0..15] of string=(
    'SendOpen','SendClose',
    'SendHTMLOpen','SendHTMLClose',
    'URLEncodeOpen','URLEncodeClose',
    'Extra1Open','Extra1Close',
    'Extra2Open','Extra2Close',
    'Extra3Open','Extra3Close',
    'Extra4Open','Extra4Close',
    'Extra5Open','Extra5Close'
  );

type
  TXxmFileType=(
    ftPage,
    ftInclude,
    ftProject,
    //add new here above
    ft_Unknown
  );

const
  XxmFileExtension:array[TXxmFileType] of string=(
    '.xxm',
    '.xxmi',
    '.xxmp',
    //add new here above
    ''
  );

procedure TEditProjectForm.Save1Click(Sender: TObject);
begin
  SaveProject;
end;

procedure TEditProjectForm.SaveParserValue;
var
  d:IJSONDocument;
begin
  if LastParserValue<>-1 then
   begin
    d:=JSON(ProjectData['parserValues']);
    if txtParserValue.Text='' then
      d.Delete(ParserValueElement[LastParserValue])
    else
      d[ParserValueElement[LastParserValue]]:=txtParserValue.Text;
   end;
end;

procedure TEditProjectForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TEditProjectForm.ExpandNode(node: TTreeNode);
var
  fh:THandle;
  fd:TWin32FindData;
  d,fn,fe,dx,fx:string;
  ft:TXxmFileType;
  nn:TTreeNode;
  n:TFileNode;
  i:integer;
  x:IJSONDocument;
  y:IJSONEnumerator;
begin
  tvFiles.Items.BeginUpdate;
  try
    tvFiles.SortType:=stNone;
    if node=nil then tvFiles.Items.Clear else node.DeleteChildren;
    d:='';
    nn:=node;
    while nn<>nil do
     begin
      d:=nn.Text+PathDelim+d;
      nn:=nn.Parent;
     end;
    fh:=FindFirstFile(PChar(ProjectFolder+d+'*.*'),fd);
    if fh<>INVALID_HANDLE_VALUE then
      try
        repeat
          if ((fd.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)=0) and
             ((fd.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)=0) then
            if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
             begin
              //file
              n:=tvFiles.Items.AddChild(node,fd.cFileName) as TFileNode;
              n.IsDir:=false;
              n.Col:='';
              n.Key:='';
              n.Doc:=nil;
              fn:=fd.cFileName;
              i:=Length(fn);
              while (i<>0) and (fn[i]<>'.') do dec(i);
              if i=0 then fe:='' else fe:=LowerCase(Copy(fn,i,Length(fn)-i+1));
              if fe=DelphiExtension then //.pas
                if LowerCase(fn)=ProtoProjectPas then //xxmp.pas
                  n.ImageIndex:=iiPasGenerated
                else
                 begin
                  fx:=Copy(fn,1,i-1);
                  dx:=StringReplace(d,PathDelim,'/',[rfReplaceAll]);
                  x:=nil;
                  y:=JSONEnum(ProjectData['units']);
                  while (x=nil) and y.Next do
                   begin
                    x:=JSON(y.Value);
                    if (y.Key=fx) and (VarToStr(x['unitPath'])=dx) then
                     begin
                      n.Col:='units';
                      n.Key:=y.Key;
                      n.Doc:=x;
                      n.ImageIndex:=iiPasIncluded;
                     end
                    else
                      x:=nil;
                   end;
                  if x=nil then n.ImageIndex:=iiPas;
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
                    dx:=StringReplace(d,PathDelim,'/',[rfReplaceAll])+Copy(fn,1,i-1);
                    x:=nil;
                    y:=JSONEnum(ProjectData['files']);
                    while (x=nil) and y.Next do
                     begin
                      x:=JSON(y.Value);
                      if VarToStr(x['path'])=dx then
                       begin
                        n.Col:='files';
                        n.Key:=y.Key;
                        n.Doc:=x;
                       end
                      else
                        x:=nil;
                     end;
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
                    dx:=StringReplace(d,PathDelim,'/',[rfReplaceAll])+fn;
                    x:=JSON(ProjectData['resources']);
                    if x<>nil then x:=JSON(x[dx]);
                    n.Col:='resources';
                    n.Key:=dx;
                    n.Doc:=x;
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
                n:=tvFiles.Items.AddChild(node,fn) as TFileNode;
                n.IsDir:=true;
                n.Col:='';
                n.Key:='';
                n.Doc:=nil;
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
        until not(FindNextFile(fh,fd));
      finally
        Winapi.Windows.FindClose(fh);
      end;
    tvFiles.SortType:=stData;
  finally
    tvFiles.Items.EndUpdate;
  end;
  //TODO: merge (missing) items?
end;

procedure TEditProjectForm.btnRegisterFileClick(Sender: TObject);
var
  fn,s,t,u:string;
  fu:UTF8string;
  fd:WideString;
  i:integer;
  f:TFileStream;
  d,d1:IJSONDocument;
begin
  if CheckModified then
   begin
    t:=txtProjectName.Text;
    if t='' then raise Exception.Create('Project name required');
    s:=ProjectFolder+t+'.xxl';
    if odXxmJson.Execute then
     begin
      fn:=odXxmJson.FileName;

      t:=LowerCase(t);
      d:=JSON;
      if FileExists(fn) then
       begin
        f:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
        try
          i:=f.Size;
          SetLength(fu,i);
          if i<>f.Read(fu[1],i) then RaiseLastOSError;
          if (i>=3) and (fu[1]=#$EF) and (fu[2]=#$BB) and (fu[3]=#$BF) then
            fd:=UTF8ToWideString(Copy(fu,4,i-3))
          else
          if (i>=2) and (fu[1]=#$FF) and (fu[2]=#$FE) then
            fd:=PWideChar(@fu[1])
          else
            fd:=WideString(fu);
        finally
          f.Free;
        end;
        d.Parse(fd);
       end
      else
        d['projects']:=JSON;
      d1:=JSON(JSON(d['projects'])[t]);
      if d1=nil then u:='' else u:=VarToStr(d1['path']);
      if (u='') or (u=s) or (MessageBox(GetDesktopWindow,PChar('Project "'+t+
        '" was already registered as'#13#10'  '+u+
        #13#10'Do you want to overwrite this registration?'#13#10'  '+s),
        'xxm Project',MB_OKCANCEL or MB_ICONQUESTION or MB_SYSTEMMODAL)=idOK) then
       begin
        if d1=nil then
         begin
          d1:=JSON;
          JSON(d['projects'])[t]:=d1;
         end
        else
         begin
          d1.Delete('signature');
          d1.Delete('alias');//?
         end;
        d1['path']:=StringReplace(s,PathDelim,'/',[rfReplaceAll]);
        fu:=#$EF#$BB#$BF+UTF8Encode(d.ToString);
        f:=TFileStream.Create(fn,fmCreate);
        try
          f.Write(fu[1],Length(fu));
        finally
          f.Free;
        end;
        MessageBox(GetDesktopWindow,PChar('Project "'+t+'" registered with "'+fn+'".'),
          'xxm Project',MB_OK or MB_ICONINFORMATION);
       end;
     end;
   end;
end;

procedure TEditProjectForm.cbParserValueChange(Sender: TObject);
var
  d:IJSONDocument;
begin
  if cbParserValue.ItemIndex=-1 then
    txtParserValue.Text:=''
  else
   begin
    SaveParserValue;
    d:=JSON(ProjectData['parserValues']);
    if d=nil then
     begin
      d:=JSON;
      ProjectData['parserValues']:=d;
     end;
    txtParserValue.Text:=VarToStr(
      d[ParserValueElement[cbParserValue.ItemIndex]]);
   end;
  //txtParserValue.Modified:=false;
  LastParserValue:=cbParserValue.ItemIndex;
end;

end.
