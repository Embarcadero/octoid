unit Octoid.View.Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnList,
  Octoid.Config;

type
  TOptionsView = class(TForm)
    BannerLabel: TLabel;
    UnitBannerGroupBox: TGroupBox;
    BannerPositionGroupBox: TGroupBox;
    BannerTextMemo: TMemo;
    BannerPositionBeforeUnitRadioButton: TRadioButton;
    BannerPositionBeforeInterfaceRadioButton: TRadioButton;
    CommandButtonsPanel: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    UseBannerAsIsCheckBox: TCheckBox;
    PageControl: TPageControl;
    TranslatorTab: TTabSheet;
    BannerTab: TTabSheet;
    ErrorLimitLabel: TLabel;
    ErrorLimitEdit: TEdit;
    ErrorLimitEditPanel: TPanel;
    AdditionalOptionsLabel: TLabel;
    AdditionalOptionsPanel: TPanel;
    AdditionalOptionsEdit: TEdit;
    TodoCommentsCheckbox: TCheckBox;
    DeprecationCommentsCheckBox: TCheckBox;
    ConstTypeCommentsCheckBox: TCheckBox;
    ActionList: TActionList;
    OKAction: TAction;
    ErrorLimitErrorLabel: TLabel;
    UITab: TTabSheet;
    ThemeLabel: TLabel;
    ThemeComboBox: TComboBox;
    procedure OKActionExecute(Sender: TObject);
    procedure OKActionUpdate(Sender: TObject);
    procedure ThemeComboBoxCloseUp(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FActiveStyleName: string;
    FConfig: TOctoidConfig;
    FBannerFileName: string;
  protected
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  OptionsView: TOptionsView;

implementation

{$R *.dfm}

uses
  System.IOUtils, System.Math,
  Vcl.Themes,
  Octoid.Consts;

{ TOptionsView }

constructor TOptionsView.Create(AOwner: TComponent);
begin
  inherited;
  PageControl.ActivePage := TranslatorTab;
  ThemeComboBox.Items.AddStrings(TStyleManager.StyleNames);
  if TStyleManager.ActiveStyle <> nil then
  begin
    FActiveStyleName := TStyleManager.ActiveStyle.Name;
    ThemeComboBox.ItemIndex := ThemeComboBox.Items.IndexOf(FActiveStyleName);
  end;
end;

procedure TOptionsView.DoShow;
begin
  FConfig := TOctoidConfig.Current;
  FBannerFileName := TPath.Combine(FConfig.GetConfigPath, 'banner.txt');
  UseBannerAsIsCheckBox.Checked := FConfig.UseBannerAsIs;
  if TFile.Exists(FBannerFileName) then
    BannerTextMemo.Text := TFile.ReadAllText(FBannerFileName);
  case FConfig.BannerPosition of
    TBannerPosition.BeforeUnit:
      BannerPositionBeforeUnitRadioButton.Checked := True;
    TBannerPosition.BeforeInterface:
      BannerPositionBeforeInterfaceRadioButton.Checked := True;
  end;
  ConstTypeCommentsCheckBox.Checked := FConfig.IncludeConstTypeComments;
  DeprecationCommentsCheckBox.Checked := FConfig.IncludeDeprecationComments;
  TodoCommentsCheckbox.Checked := FConfig.IncludeTodoComments;
  ErrorLimitEdit.Text := FConfig.ErrorLimit.ToString;
  inherited;
end;

procedure TOptionsView.OKActionExecute(Sender: TObject);
begin
  if TFile.Exists(FBannerFileName) or not string(BannerTextMemo.Text).Trim.IsEmpty then
    TFile.WriteAllText(FBannerFileName, BannerTextMemo.Text);
  if BannerPositionBeforeUnitRadioButton.Checked then
    FConfig.BannerPosition := TBannerPosition.BeforeUnit
  else if BannerPositionBeforeInterfaceRadioButton.Checked then
    FConfig.BannerPosition := TBannerPosition.BeforeInterface;
  if ThemeComboBox.ItemIndex > -1 then
    FConfig.StyleName := ThemeComboBox.Items[ThemeComboBox.ItemIndex];
  FConfig.UseBannerAsIs := UseBannerAsIsCheckBox.Checked;
  FConfig.IncludeConstTypeComments := ConstTypeCommentsCheckBox.Checked;
  FConfig.IncludeDeprecationComments := DeprecationCommentsCheckBox.Checked;
  FConfig.IncludeTodoComments := TodoCommentsCheckbox.Checked;
  FConfig.ErrorLimit := StrToInt(ErrorLimitEdit.Text);
  FConfig.Save;
end;

procedure TOptionsView.OKActionUpdate(Sender: TObject);
var
  LEnable: Boolean;
begin
  LEnable := True;
  if StrToInt(ErrorLimitEdit.Text) < cErrorLimitDefault then
  begin
    ErrorLimitErrorLabel.Caption := 'Error limit should be >= ' + cErrorLimitDefault.ToString;
    LEnable := False;
  end;
  OKAction.Enabled := LEnable;
end;

procedure TOptionsView.ThemeComboBoxCloseUp(Sender: TObject);
begin
  TStyleManager.TrySetStyle(ThemeComboBox.Items[ThemeComboBox.ItemIndex]);
end;

procedure TOptionsView.CancelButtonClick(Sender: TObject);
begin
  if (ThemeComboBox.ItemIndex > -1) and not FActiveStyleName.Equals(ThemeComboBox.Items[ThemeComboBox.ItemIndex]) then
    TStyleManager.TrySetStyle(FActiveStyleName);
end;

end.
