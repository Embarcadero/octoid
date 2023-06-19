object OptionsView: TOptionsView
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 379
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 13
  object CommandButtonsPanel: TPanel
    Left = 0
    Top = 345
    Width = 623
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object CancelButton: TButton
      AlignWithMargins = True
      Left = 545
      Top = 3
      Width = 75
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      Default = True
      DoubleBuffered = True
      ModalResult = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = CancelButtonClick
    end
    object OKButton: TButton
      AlignWithMargins = True
      Left = 464
      Top = 3
      Width = 75
      Height = 28
      Action = OKAction
      Align = alRight
      DoubleBuffered = True
      ModalResult = 1
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 623
    Height = 345
    ActivePage = TranslatorTab
    Align = alClient
    TabOrder = 1
    object TranslatorTab: TTabSheet
      Caption = 'Translator Options'
      object ErrorLimitLabel: TLabel
        AlignWithMargins = True
        Left = 7
        Top = 52
        Width = 605
        Height = 13
        Margins.Left = 7
        Align = alTop
        Caption = 'Error Limit:'
        ExplicitWidth = 52
      end
      object AdditionalOptionsLabel: TLabel
        AlignWithMargins = True
        Left = 7
        Top = 3
        Width = 605
        Height = 13
        Margins.Left = 7
        Align = alTop
        Caption = 'Additional Options:'
        ExplicitWidth = 91
      end
      object ErrorLimitEditPanel: TPanel
        Left = 0
        Top = 68
        Width = 615
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object ErrorLimitErrorLabel: TLabel
          AlignWithMargins = True
          Left = 118
          Top = 3
          Width = 3
          Height = 24
          Margins.Left = 8
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object ErrorLimitEdit: TEdit
          AlignWithMargins = True
          Left = 7
          Top = 3
          Width = 100
          Height = 24
          Margins.Left = 7
          Align = alLeft
          NumbersOnly = True
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
      object AdditionalOptionsPanel: TPanel
        Left = 0
        Top = 19
        Width = 615
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object AdditionalOptionsEdit: TEdit
          AlignWithMargins = True
          Left = 7
          Top = 3
          Width = 605
          Height = 24
          Margins.Left = 7
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
      object TodoCommentsCheckbox: TCheckBox
        AlignWithMargins = True
        Left = 7
        Top = 130
        Width = 605
        Height = 17
        Margins.Left = 7
        Margins.Top = 6
        Align = alTop
        Caption = 'Include TODO comments'
        TabOrder = 2
      end
      object ConstTypeCommentsCheckBox: TCheckBox
        AlignWithMargins = True
        Left = 7
        Top = 104
        Width = 605
        Height = 17
        Margins.Left = 7
        Margins.Top = 6
        Align = alTop
        Caption = 'Include unsupported const type comments'
        TabOrder = 3
      end
      object DeprecationCommentsPanel: TPanel
        Left = 0
        Top = 150
        Width = 615
        Height = 55
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        object DeprecationCommentsCheckBox: TCheckBox
          AlignWithMargins = True
          Left = 7
          Top = 6
          Width = 605
          Height = 17
          Hint = 'Includes a comment with the method, regarding deprecation'
          Margins.Left = 7
          Margins.Top = 6
          Align = alTop
          Caption = 'Include deprecation comments'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = DeprecationCommentsCheckBoxClick
          ExplicitLeft = 20
          ExplicitTop = -4
          ExplicitWidth = 592
        end
        object DeprecationCommentFirstCheckBox: TCheckBox
          AlignWithMargins = True
          Left = 20
          Top = 32
          Width = 592
          Height = 17
          Hint = 
            'Otherwise the comment appears on the same line as the method, af' +
            'ter the declaration'
          Margins.Left = 20
          Margins.Top = 6
          Align = alTop
          Caption = 'Before the method (and any method attributes)'
          TabOrder = 1
          ExplicitLeft = 7
          ExplicitTop = 156
          ExplicitWidth = 605
        end
      end
    end
    object BannerTab: TTabSheet
      Caption = 'Banner'
      ImageIndex = 1
      object BannerLabel: TLabel
        AlignWithMargins = True
        Left = 7
        Top = 3
        Width = 605
        Height = 13
        Margins.Left = 7
        Align = alTop
        Caption = 'Banner Text:'
        ExplicitWidth = 63
      end
      object UnitBannerGroupBox: TGroupBox
        Left = 0
        Top = 19
        Width = 615
        Height = 197
        Align = alClient
        Caption = 'Unit Banner'
        TabOrder = 0
      end
      object BannerPositionGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 242
        Width = 609
        Height = 72
        Align = alBottom
        Caption = 'Banner Position'
        TabOrder = 1
        object BannerPositionBeforeUnitRadioButton: TRadioButton
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 599
          Height = 17
          Align = alTop
          Caption = 'Before unit declaration'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object BannerPositionBeforeInterfaceRadioButton: TRadioButton
          AlignWithMargins = True
          Left = 5
          Top = 41
          Width = 599
          Height = 17
          Align = alTop
          Caption = 'Before interface declaration'
          TabOrder = 1
        end
      end
      object BannerTextMemo: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 609
        Height = 191
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object UseBannerAsIsCheckBox: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 219
        Width = 609
        Height = 17
        Align = alBottom
        Caption = 'Insert the banner exactly as is (it is otherwise "decorated")'
        TabOrder = 3
      end
    end
    object UITab: TTabSheet
      Caption = 'UI'
      ImageIndex = 2
      object ThemeLabel: TLabel
        AlignWithMargins = True
        Left = 7
        Top = 3
        Width = 605
        Height = 13
        Margins.Left = 7
        Align = alTop
        Caption = 'Theme:'
        ExplicitWidth = 36
      end
      object ThemeComboBox: TComboBox
        AlignWithMargins = True
        Left = 7
        Top = 22
        Width = 601
        Height = 21
        Margins.Left = 7
        Margins.Right = 7
        Align = alTop
        Style = csDropDownList
        Sorted = True
        TabOrder = 0
        OnCloseUp = ThemeComboBoxCloseUp
      end
    end
  end
  object ActionList: TActionList
    Left = 316
    Top = 136
    object OKAction: TAction
      Caption = 'OK'
      OnExecute = OKActionExecute
      OnUpdate = OKActionUpdate
    end
  end
end
