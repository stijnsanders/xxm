unit xxmEditProject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, MSXML2_TLB, ComCtrls, StdCtrls, Dialogs;

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
    procedure Exit1Click(Sender: TObject);
    procedure txtChange(Sender: TObject);
    procedure tvFilesCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure New1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure btnRegisterLocalClick(Sender: TObject);
  private
    Modified:boolean;
    ProjectPath:string;
    ProjectData:DOMDocument;
    function CheckModified:boolean;
    function LoadProject(Path:string;CreateNew:boolean):boolean;
    procedure SaveProject;
    function GetNode(element:IXMLDOMElement;xpath:WideString):IXMLDOMElement;
    function GetAttr(element:IXMLDOMElement;name:WideString):WideString;
  protected
    procedure DoCreate; override;
    procedure DoClose(var Action: TCloseAction); override;
  public

  end;

  TFileNode=class(TTreeNode)
  public
    ProjectNode:IXMLDOMElement;
  end;

const
  ApplicationTitle='xxm Project Properties';

var
  EditProjectMainForm: TEditProjectMainForm;

implementation

uses DateUtils, xxmUtilities, Registry;

{$R *.dfm}

procedure TEditProjectMainForm.DoCreate;
begin
  inherited;
  ProjectData:=CoDOMDocument.Create;
  if ParamCount=0 then
   begin
    if not(LoadProject('',false)) then Application.Terminate;
   end
  else
    LoadProject(ParamStr(1),false);
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

function TEditProjectMainForm.LoadProject(Path: string;
  CreateNew: boolean): boolean;
var
  fn:string;
  fe:boolean;
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
    fe:=FileExists(fn);
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
      ProjectData.loadXML('<XxmWebProject>'#13#10#9'<ProjectName></ProjectName>'#13#10#9+
        '<CompileCommand>dcc32 -U..\..\public -Q [[ProjectName]].dpr</CompileCommand>'#13#10'</XxmWebProject>');
    ProjectPath:=fn;
    Caption:='xxm Project - '+fn;
    Application.Title:='xxm Project - '+fn;

    txtProjectName.Text:=GetNode(ProjectData.documentElement,'ProjectName').text;
    txtCompileCommand.Text:=GetNode(ProjectData.documentElement,'CompileCommand').text;

    tvFiles.Items.BeginUpdate;
    try
      tvFiles.Items.Clear;
      //load files
      //TODO:
    finally
      tvFiles.Items.EndUpdate;
    end;

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

function TEditProjectMainForm.GetAttr(element: IXMLDOMElement;
  name: WideString): WideString;
begin
  Result:=VarToWideStr(element.getAttribute(name));
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
  i:integer;
  s,t:string;
begin
  if CheckModified then
   begin
    i:=Length(ProjectPath);
    while not(i=0) and not(ProjectPath[i]=PathDelim) do dec(i);
    s:=Copy(ProjectPath,1,i);
    t:=txtProjectName.Text;
    if t='' then raise Exception.Create('Project name required');
    r:=TRegistry.Create;
    try
      r.RootKey:=HKEY_LOCAL_MACHINE;
      r.OpenKey('\Software\xxm\local\'+t,true);
      r.WriteString('',s+t+'.xxl');
      //TODO: default settings?
    finally
      r.Free;
    end;
    MessageBox(GetDesktopWindow,PChar('Project "'+t+'" registered.'),
      'xxm Project',MB_OK or MB_ICONINFORMATION);
   end;
end;

end.
