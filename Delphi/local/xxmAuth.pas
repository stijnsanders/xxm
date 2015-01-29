unit xxmAuth;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TXxmAuthForm = class(TForm)
    lblURL: TLabel;
    txtURL: TEdit;
    lblRealm: TLabel;
    txtRealm: TEdit;
    lblUserName: TLabel;
    txtUserName: TEdit;
    lblPassword: TLabel;
    txtPassword: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblHoldXToClear: TLabel;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  EXxmUserAuthenticated=class(Exception)
  public
    UserName,Password:string;
    constructor Create(const AUserName,APassword:string);
  end;

procedure XxmAuthenticateUser(const ProjectName,URL,Realm:WideString);

implementation

var
  XxmAuthStore:array of record
    ProjectName,Realm,Usr,Pwd:WideString;
  end;

{$R *.dfm}

procedure XxmAuthenticateUser(const ProjectName,URL,Realm:WideString);
var
  i,l:integer;
  f:TXxmAuthForm;
begin
  //TODO: lock? assert single user
  i:=0;
  l:=Length(XxmAuthStore);

  if (l<>0) and (GetAsyncKeyState(byte('X'))<>0) then
    if MessageBox(GetDesktopWindow,
      '"X" was held while loading the page.'#13#10+
      'Do you want to clear stored authentication credentials?',
      'xxmLocal Authentication',
      MB_OK or MB_ICONQUESTION or MB_OKCANCEL)=idOk then
     begin
      SetLength(XxmAuthStore,0);
      l:=0;
     end;

  while (i<l) and not(
    (ProjectName=XxmAuthStore[i].ProjectName) and
    (Realm=XxmAuthStore[i].Realm)) do inc(i);
  if i=l then
   begin
    f:=TXxmAuthForm.Create(nil);
    try
      f.txtURL.Text:=string(URL);
      f.txtRealm.Text:=string(Realm);
      if f.ShowModal=mrOk then
       begin
        SetLength(XxmAuthStore,i+1);
        XxmAuthStore[i].ProjectName:=ProjectName;
        XxmAuthStore[i].Realm:=Realm;
        XxmAuthStore[i].Usr:=f.txtUserName.Text;
        XxmAuthStore[i].Pwd:=f.txtPassword.Text;
        raise EXxmUserAuthenticated.Create(
          f.txtUserName.Text,f.txtPassword.Text);
       end;
    finally
      f.Free;
    end;
   end
  else
    raise EXxmUserAuthenticated.Create(
      XxmAuthStore[i].Usr,XxmAuthStore[i].Pwd);
end;

{ TXxmAuthForm }

procedure TXxmAuthForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //Params.Style:=
  //Params.ExStyle:=
  Params.WndParent:=GetDesktopWindow;
end;

{ EXxmUserAuthenticated }

constructor EXxmUserAuthenticated.Create(const AUserName,
  APassword: string);
begin
  inherited Create('xxm User Authentication');
  UserName:=AUSerName;
  Password:=APassword;
end;

end.
