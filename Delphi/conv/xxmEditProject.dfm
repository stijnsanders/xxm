object EditProjectMainForm: TEditProjectMainForm
  Left = 457
  Top = 125
  Width = 425
  Height = 469
  Caption = 'xxm Project Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 417
    Height = 415
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      BorderWidth = 4
      Caption = 'Project'
      DesignSize = (
        401
        379)
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 63
        Height = 13
        Caption = 'Project name'
      end
      object Label2: TLabel
        Left = 0
        Top = 40
        Width = 85
        Height = 13
        Caption = 'Compile command'
      end
      object txtProjectName: TEdit
        Left = 0
        Top = 16
        Width = 393
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'txtProjectName'
        OnChange = txtChange
      end
      object txtCompileCommand: TEdit
        Left = 0
        Top = 56
        Width = 393
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'txtCompileCommand'
        OnChange = txtChange
      end
      object btnRegisterLocal: TButton
        Left = 0
        Top = 88
        Width = 153
        Height = 25
        Caption = 'Register for local handler'
        TabOrder = 2
        OnClick = btnRegisterLocalClick
      end
    end
    object TabSheet2: TTabSheet
      BorderWidth = 4
      Caption = 'Files'
      object tvFiles: TTreeView
        Left = 0
        Top = 0
        Width = 401
        Height = 379
        Align = alClient
        Indent = 19
        TabOrder = 0
        OnCreateNodeClass = tvFilesCreateNodeClass
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 328
    Top = 32
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New...'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xxmp'
    FileName = 'Web.xxmp'
    Filter = 
      'xxm Project (Web.xxmp)|web.xxmp|xxm Project (*.xxmp)|*.xxmp|All ' +
      'files (*.*)|*.*'
    Title = 'Open/Create xxm Project'
    Left = 364
    Top = 32
  end
end
