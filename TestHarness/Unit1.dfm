object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Octoid Test Harness'
  ClientHeight = 712
  ClientWidth = 1414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OutputMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 423
    Width = 1408
    Height = 248
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 674
    Width = 1414
    Height = 38
    Align = alBottom
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 3
    object TransformFrameworkButton: TButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 132
      Height = 28
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
      Left = 143
      Top = 5
      Width = 132
      Height = 28
      Margins.Left = 6
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = DumpASTAction
      Align = alLeft
      TabOrder = 1
    end
  end
  object SDKGroupBox: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1408
    Height = 362
    Align = alTop
    Caption = 'SDK'
    TabOrder = 0
    object SDKPathLabel: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 1398
      Height = 13
      Align = alTop
      Caption = 'SDK Path'
      ExplicitWidth = 44
    end
    object FrameworksLabel: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 67
      Width = 1398
      Height = 13
      Align = alTop
      Caption = 'SDK Frameworks'
      ExplicitTop = 64
      ExplicitWidth = 80
    end
    object SDKPathPanel: TPanel
      Left = 2
      Top = 34
      Width = 1404
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      Caption = 'SDKPathPanel'
      TabOrder = 0
      object SelectSDKPathButton: TSpeedButton
        AlignWithMargins = True
        Left = 1377
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
      object SDKPathEdit: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 1373
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
    object SDKFrameworksListBox: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 86
      Width = 1398
      Height = 271
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnClick = SDKFrameworksListBoxClick
    end
  end
  object OutputPathGroupBox: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 371
    Width = 1408
    Height = 46
    Align = alTop
    Caption = 'Output Path'
    TabOrder = 1
    object SelectOutputPathButton: TSpeedButton
      AlignWithMargins = True
      Left = 1379
      Top = 18
      Width = 23
      Height = 23
      Margins.Left = 0
      Margins.Right = 4
      Action = SelectOutputPathAction
      Align = alRight
      ExplicitLeft = 607
      ExplicitTop = 29
    end
    object OutputPathEdit: TEdit
      AlignWithMargins = True
      Left = 6
      Top = 19
      Width = 1373
      Height = 21
      Hint = 'Path to output the result to'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
      OnChange = OutputPathEditChange
    end
  end
  object SDKPathFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Title = 'Select the SDK folder'
    Left = 640
    Top = 80
  end
  object OutputPathFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Title = 'Select the output folder'
    Left = 640
    Top = 140
  end
  object ActionList: TActionList
    Left = 639
    Top = 207
    object SelectSDKPathAction: TAction
      Caption = '...'
      OnExecute = SelectSDKPathActionExecute
    end
    object SelectOutputPathAction: TAction
      Caption = '...'
      OnExecute = SelectOutputPathActionExecute
    end
    object TransformFrameworkAction: TAction
      Caption = 'Transform Framework'
      OnExecute = TransformFrameworkActionExecute
      OnUpdate = TransformFrameworkActionUpdate
    end
    object DumpASTAction: TAction
      Caption = 'Dump AST'
      OnExecute = DumpASTActionExecute
      OnUpdate = DumpASTActionUpdate
    end
  end
end
