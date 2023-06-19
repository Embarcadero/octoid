object MainView: TMainView
  Left = 0
  Top = 0
  Caption = 'Octoid - Objective-C TranslatOr Into Delphi'
  ClientHeight = 533
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  TextHeight = 13
  object SDKPathLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 782
    Height = 13
    Align = alTop
    Caption = 'SDK Path:'
    ExplicitWidth = 48
  end
  object FrameworksLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 52
    Width = 782
    Height = 13
    Align = alTop
    Caption = 'SDK Frameworks:'
    ExplicitWidth = 84
  end
  object OutputPathLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 426
    Width = 782
    Height = 13
    Align = alBottom
    Caption = 'Output Path:'
    ExplicitWidth = 63
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 472
    Width = 788
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 3
    OnDblClick = BottomPanelDblClick
    object TransformButton: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 105
      Height = 30
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = TransformFrameworkAction
      Align = alLeft
      TabOrder = 0
    end
    object DumpASTButton: TButton
      AlignWithMargins = True
      Left = 375
      Top = 4
      Width = 102
      Height = 30
      Margins.Left = 6
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = DumpASTAction
      Align = alLeft
      TabOrder = 1
      Visible = False
    end
    object ShowOutputButton: TButton
      AlignWithMargins = True
      Left = 115
      Top = 4
      Width = 102
      Height = 30
      Margins.Left = 6
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = ShowOutputAction
      Align = alLeft
      TabOrder = 2
    end
    object OptionsButton: TButton
      AlignWithMargins = True
      Left = 682
      Top = 4
      Width = 102
      Height = 30
      Hint = 'Change options for code generation and the Octoid UI'
      Margins.Left = 6
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = OptionsAction
      Align = alRight
      TabOrder = 3
    end
    object IgnoreUmbrellaCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 229
      Top = 7
      Width = 137
      Height = 24
      Hint = 
        'Some framework "umbrella" headers are known to cause problems. C' +
        'heck this checkbox to ignore it'
      Margins.Left = 12
      Align = alLeft
      Caption = 'Ignore umbrella header'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
  object SDKPathPanel: TPanel
    Left = 0
    Top = 19
    Width = 788
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Caption = 'SDKPathPanel'
    TabOrder = 0
    object SelectSDKPathButton: TSpeedButton
      AlignWithMargins = True
      Left = 761
      Top = 3
      Width = 23
      Height = 24
      Margins.Left = 0
      Margins.Right = 4
      Action = SelectSDKPathAction
      Align = alRight
      ExplicitLeft = 607
      ExplicitTop = 29
      ExplicitHeight = 23
    end
    object SDKPathEdit: TComboBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 757
      Height = 21
      Hint = 'Path to the SDK'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
      OnChange = SDKPathEditChange
      OnCloseUp = SDKPathEditCloseUp
      OnDropDown = SDKPathEditDropDown
    end
  end
  object SDKFrameworksListBox: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 71
    Width = 782
    Height = 349
    Align = alClient
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnClick = SDKFrameworksListBoxClick
  end
  object OutputPathPanel: TPanel
    Left = 0
    Top = 442
    Width = 788
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'SDKPathPanel'
    TabOrder = 2
    object SelectOutputPathButton: TSpeedButton
      AlignWithMargins = True
      Left = 761
      Top = 3
      Width = 23
      Height = 24
      Margins.Left = 0
      Margins.Right = 4
      Action = SelectOutputPathAction
      Align = alRight
      ExplicitLeft = 607
      ExplicitTop = 29
      ExplicitHeight = 23
    end
    object OutputPathEdit: TEdit
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 757
      Height = 22
      Hint = 'Path to the SDK'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
      OnChange = SDKPathEditChange
      ExplicitHeight = 21
    end
  end
  object ProgressBar: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 513
    Width = 782
    Height = 17
    Align = alBottom
    TabOrder = 4
  end
  object SDKPathFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Title = 'Select the SDK folder'
    Left = 68
    Top = 96
  end
  object OutputPathFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Title = 'Select the output folder'
    Left = 68
    Top = 156
  end
  object ActionList: TActionList
    Left = 67
    Top = 223
    object SelectSDKPathAction: TAction
      Caption = '...'
      OnExecute = SelectSDKPathActionExecute
    end
    object SelectOutputPathAction: TAction
      Caption = '...'
      OnExecute = SelectOutputPathActionExecute
    end
    object TransformFrameworkAction: TAction
      Caption = 'Transform'
      OnExecute = TransformFrameworkActionExecute
      OnUpdate = TransformFrameworkActionUpdate
    end
    object DumpASTAction: TAction
      Caption = 'Dump AST'
      OnExecute = DumpASTActionExecute
      OnUpdate = DumpASTActionUpdate
    end
    object ShowOutputAction: TAction
      Caption = 'Show Output'
      OnExecute = ShowOutputActionExecute
    end
    object OptionsAction: TAction
      Caption = 'Options..'
      OnExecute = OptionsActionExecute
    end
  end
  object ImageList: TImageList
    Left = 64
    Top = 292
    Bitmap = {
      494C010102000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BDBDBD00C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000454545004444440000000000000000000000000000000000434343004545
      4500000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C5C5C5000000000000000000C8C8C800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B9B9B900000000004A4A4A0000000000000000004949490000000000B9B9
      B900000000000000000000000000000000000000000000000000000000000000
      000000000000C5C5C50000000000535353005050500000000000C7C7C7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C5C5C50000000000545454005353530000000000C5C5C5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BABABA00000000004949490000000000000000004646460000000000BBBB
      BB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C5C5C5000000000000000000C5C5C500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000434343004343430000000000000000000000000000000000404040004545
      4500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BDBDBD00BDBDBD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000FFFFFE7F00000000
      F3CFFC3F00000000F18FF81F00000000F81FF18F00000000FC3FF3CF00000000
      FE7FFFFF00000000FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object MainMenu: TMainMenu
    Left = 292
    Top = 100
    object FileMenuItem: TMenuItem
      Caption = 'File'
      object FileExitMenuItem: TMenuItem
        Caption = 'E&xit'
      end
    end
    object EditMenuItem: TMenuItem
      Caption = 'Edit'
      object EditOptionsMenuItem: TMenuItem
        Caption = 'Options'
      end
    end
    object HelpMenuItem: TMenuItem
      Caption = 'Help'
      object HelpAboutMenuItem: TMenuItem
        Caption = 'About'
      end
    end
  end
end
