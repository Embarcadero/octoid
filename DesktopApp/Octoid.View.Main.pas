unit Octoid.View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.ImageList, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.Imaging.pngimage, Vcl.ImgList, Vcl.Menus,
  Octoid.Command, Octoid.View.Output, Octoid.Config;

type
  TMainView = class(TForm)
    TransformButton: TButton;
    BottomPanel: TPanel;
    SDKPathFileOpenDialog: TFileOpenDialog;
    SelectSDKPathButton: TSpeedButton;
    SDKPathPanel: TPanel;
    SDKPathLabel: TLabel;
    SDKFrameworksListBox: TListBox;
    FrameworksLabel: TLabel;
    OutputPathFileOpenDialog: TFileOpenDialog;
    ActionList: TActionList;
    SelectSDKPathAction: TAction;
    SelectOutputPathAction: TAction;
    TransformFrameworkAction: TAction;
    DumpASTButton: TButton;
    DumpASTAction: TAction;
    OutputPathLabel: TLabel;
    OutputPathPanel: TPanel;
    SelectOutputPathButton: TSpeedButton;
    OutputPathEdit: TEdit;
    SDKPathEdit: TComboBox;
    ImageList: TImageList;
    ShowOutputButton: TButton;
    ShowOutputAction: TAction;
    ProgressBar: TProgressBar;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    FileExitMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    EditOptionsMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    HelpAboutMenuItem: TMenuItem;
    OptionsButton: TButton;
    OptionsAction: TAction;
    IgnoreUmbrellaCheckBox: TCheckBox;
    procedure SDKPathEditChange(Sender: TObject);
    procedure TransformFrameworkActionExecute(Sender: TObject);
    procedure SelectOutputPathActionExecute(Sender: TObject);
    procedure SelectSDKPathActionExecute(Sender: TObject);
    procedure TransformFrameworkActionUpdate(Sender: TObject);
    procedure OutputPathEditChange(Sender: TObject);
    procedure SDKFrameworksListBoxClick(Sender: TObject);
    procedure DumpASTActionExecute(Sender: TObject);
    procedure DumpASTActionUpdate(Sender: TObject);
    procedure SDKPathEditDropDown(Sender: TObject);
    procedure SDKPathEditCloseUp(Sender: TObject);
    procedure BottomPanelDblClick(Sender: TObject);
    procedure ShowOutputActionExecute(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
  private
    FCommand: TOctoidCommand;
    FConfig: TOctoidConfig;
    FOutput: TOutputView;
    function CanTransform: Boolean;
    procedure CommandComplete;
    procedure DoOutput(const AMsg: string);
    function GetFrameworkDirectory: string;
    procedure TranslatorMessageHandler(const AMsg: string);
    procedure UpdateFrameworks;
    procedure UpdateOutputPath(const APath: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainView: TMainView;

implementation

{$R *.dfm}

uses
  System.IOUtils, System.Types, System.StrUtils, System.UITypes,
  REST.Json,
  Vcl.Themes,
  Neslib.Clang, AstDumper,
  Octoid.CustomTranslator, Octoid.Consts, Octoid.View.Options, Octoid.ObjCHeaderTranslator;

const
  cTranslationSuccess = '%s framework successfully translated into %s';
  cTranslationFail = '%s framework translation failed';

  cProgress20 = 'Analyzing data types...';
  cProgress40 = 'Writing constants...';
  cProgress60 = 'Writing data types...';
  cProgress80 = 'Writing functions...';
  cProgress100 = 'Header conversion completed!';

  cProgressText: array[1..5] of string = (cProgress20, cProgress40, cProgress60, cProgress80, cProgress100);

{ TfrmMain }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  Menu := nil; // Not sure whether this will be used yet
  FOutput := TOutputView.Create(Self);
  FConfig := TOctoidConfig.Current;
  if not FConfig.StyleName.IsEmpty and (IndexText(FConfig.StyleName, TStyleManager.StyleNames) > -1) then
    TStyleManager.TrySetStyle(FConfig.StyleName);
  FCommand := TOctoidCommand.Create;
  FCommand.OnMessage := TranslatorMessageHandler;
  FCommand.Project.TargetPlatform := cTargetPlatformMacOS;
  OutputPathEdit.Text := FConfig.OutputPath;
  SDKPathEdit.Items.AddStrings(FConfig.SDKPathMRU);
  SDKPathEdit.Text := FConfig.SDKPath;
  UpdateFrameworks;
end;

destructor TMainView.Destroy;
begin
  FCommand.Free;
  inherited;
end;

procedure TMainView.SDKFrameworksListBoxClick(Sender: TObject);
begin
  if SDKFrameworksListBox.ItemIndex > -1 then
  begin
    FConfig.SelectedFramework := SDKFrameworksListBox.Items[SDKFrameworksListBox.ItemIndex];
    FConfig.Save;
  end;
end;

procedure TMainView.SDKPathEditChange(Sender: TObject);
begin
  UpdateFrameworks;
end;

procedure TMainView.SDKPathEditCloseUp(Sender: TObject);
var
  LKey: string;
begin
  if (SDKPathEdit.ItemIndex > -1) and (SDKPathEdit.Tag <> SDKPathEdit.ItemIndex) then
  begin
    LKey := SDKPathEdit.Items[SDKPathEdit.ItemIndex];
    SDKPathEdit.Items.Delete(SDKPathEdit.ItemIndex);
    SDKPathEdit.Items.Insert(0, LKey);
    SDKPathEdit.ItemIndex := SDKPathEdit.Items.IndexOf(LKey);
    FConfig.SDKPathMRU := SDKPathEdit.Items.ToStringArray;
    FConfig.Save;
  end;
end;

procedure TMainView.SDKPathEditDropDown(Sender: TObject);
begin
  SDKPathEdit.Tag := SDKPathEdit.Items.IndexOf(SDKPathEdit.Text);
end;

procedure TMainView.TranslatorMessageHandler(const AMsg: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      DoOutput(AMsg);
    end
  );
end;

procedure TMainView.DoOutput(const AMsg: string);
var
  I: Integer;
begin
  for I := Low(cProgressText) to High(cProgressText) do
  begin
    if AMsg.Contains(cProgressText[I]) then
      ProgressBar.Position := I * 20;
  end;
  FOutput.AddMessage(AMsg);
//  if AMsg.EndsWith(' completed!') then
//  begin
//    LDiagsFileName := Format('%s.%s.log', [TPath.GetFileName(FCommand.Project.SdkRoot), FCommand.Project.FrameworkName]);
//    OutputMemo.Lines.SaveToFile(TPath.Combine(TPath.GetDirectoryName(FCommand.Project.TargetPasFile), LDiagsFileName));
//  end;
end;

procedure TMainView.UpdateFrameworks;
var
  LFolders: TStringDynArray;
  LFolder: string;
begin
  FCommand.Project.SdkRoot := '';
  SDKFrameworksListBox.Items.Clear;
  if (SDKPathEdit.Text <> '') and TDirectory.Exists(SDKPathEdit.Text) then
  begin
    FCommand.Project.SdkRoot := SDKPathEdit.Text;
    FConfig.SDKPath := FCommand.Project.SdkRoot;
    LFolders := TDirectory.GetDirectories(TPath.Combine(FCommand.Project.SdkRoot, cSDKFrameworksFolder));
    for LFolder in LFolders do
      SDKFrameworksListBox.Items.Add(TPath.GetFileNameWithoutExtension(LFolder));
    if SDKFrameworksListBox.Items.Count > 0 then
    begin
      if not FConfig.SelectedFramework.IsEmpty then
        SDKFrameworksListBox.ItemIndex := SDKFrameworksListBox.Items.IndexOf(FConfig.SelectedFramework);
      if SDKFrameworksListBox.ItemIndex = -1 then
        SDKFrameworksListBox.ItemIndex := 0;
      FConfig.SelectedFramework := SDKFrameworksListBox.Items[SDKFrameworksListBox.ItemIndex];
    end
    else
      FConfig.SelectedFramework := '';
    FConfig.Save;
  end;
end;

procedure TMainView.UpdateOutputPath(const APath: string);
begin
  if not APath.IsEmpty and TDirectory.Exists(APath) then
  begin
    FConfig.OutputPath := APath;
    FConfig.Save;
  end;
end;

function TMainView.GetFrameworkDirectory: string;
var
  LFramework: string;
begin
  Result := '';
  if not FCommand.Project.SdkRoot.IsEmpty and (SDKFrameworksListBox.ItemIndex > -1) then
  begin
    LFramework := SDKFrameworksListBox.Items[SDKFrameworksListBox.ItemIndex] + '.framework';
    Result := TPath.Combine(TPath.Combine(FCommand.Project.SdkRoot, cSDKFrameworksFolder), LFramework);
  end;
end;

procedure TMainView.OptionsActionExecute(Sender: TObject);
var
  LOptions: TOptionsView;
begin
  LOptions := TOptionsView.Create(nil);
  try
    LOptions.ShowModal;
  finally
    LOptions.Free;
  end;
end;

procedure TMainView.OutputPathEditChange(Sender: TObject);
begin
  UpdateOutputPath(OutputPathEdit.Text);
end;

procedure TMainView.SelectOutputPathActionExecute(Sender: TObject);
begin
  if OutputPathFileOpenDialog.Execute then
  begin
    OutputPathEdit.Text := OutputPathFileOpenDialog.FileName;
    UpdateOutputPath(OutputPathEdit.Text);
  end;
end;

procedure TMainView.SelectSDKPathActionExecute(Sender: TObject);
begin
  SDKPathFileOpenDialog.DefaultFolder := TPath.Combine(TPath.GetDocumentsPath, 'Embarcadero\Studio\SDKs');
  if SDKPathFileOpenDialog.Execute then
  begin
    SDKPathEdit.Text := SDKPathFileOpenDialog.FileName;
    UpdateFrameworks;
  end;
end;

procedure TMainView.BottomPanelDblClick(Sender: TObject);
begin
  DumpASTAction.Visible := not DumpASTAction.Visible;
end;

function TMainView.CanTransform: Boolean;
begin
  Result := not FCommand.Project.SdkRoot.IsEmpty and TDirectory.Exists(FCommand.Project.SdkRoot) and
    (OutputPathEdit.Text <> '') and TDirectory.Exists(OutputPathEdit.Text) and (SDKFrameworksListBox.ItemIndex > -1);
end;

procedure TMainView.TransformFrameworkActionUpdate(Sender: TObject);
begin
  TransformFrameworkAction.Enabled := CanTransform;
end;

procedure TMainView.DumpASTActionUpdate(Sender: TObject);
begin
  DumpASTAction.Enabled := CanTransform;
end;

procedure TMainView.TransformFrameworkActionExecute(Sender: TObject);
var
  LSDKName: string;
  LTranslateOptions: TObjCTranslateOptions;
begin
  ProgressBar.Position := 5;
  if SDKPathEdit.Items.IndexOf(SDKPathEdit.Text) = -1 then
  begin
    SDKPathEdit.Items.Insert(0, SDKPathEdit.Text);
    FConfig.SDKPathMRU := SDKPathEdit.Items.ToStringArray;
    FConfig.Save;
  end;
  FOutput.ClearMessages;
  LSDKName := TPath.GetFileNameWithoutExtension(FCommand.Project.SdkRoot);
  if LSDKName.StartsWith('MAC', True) then
    FCommand.Project.TargetPlatform := 'MACOS'
  else
    FCommand.Project.TargetPlatform := 'IOS';
  FCommand.Project.IgnoreUmbrellaHeader := IgnoreUmbrellaCheckBox.Checked;
  LTranslateOptions := [];
  if FConfig.IncludeConstTypeComments then
    Include(LTranslateOptions, TObjCTranslateOption.UnsupportedConstTypeComments);
  if FConfig.IncludeDeprecationComments then
    Include(LTranslateOptions, TObjCTranslateOption.DeprecationComments);
  if FConfig.DeprecationCommentFirst then
    Include(LTranslateOptions, TObjCTranslateOption.DeprecationCommentFirst);
  if FConfig.IncludeTodoComments then
    Include(LTranslateOptions, TObjCTranslateOption.TodoComments);
  FCommand.Project.ObjCTranslateOptions := LTranslateOptions;
  FCommand.SetExtraSwitches(FConfig.AdditionalOptions);
  FCommand.ErrorLimit := FConfig.ErrorLimit;
  FCommand.Framework := FConfig.SelectedFramework;
  // This is now done automatically, but should check existence etc
  // FCommand.Project.ClangIncludePath := 'C:\Program Files\LLVM\lib\clang\7.0.0\include\';
  FCommand.Project.OutputPath := FConfig.OutputPath;
  TThread.CreateAnonymousThread(
    procedure
    begin
      FCommand.Run;
      TThread.Synchronize(nil, CommandComplete);
    end
  ).Start;
end;

procedure TMainView.CommandComplete;
begin
  if FCommand.Succeeded then
    MessageDlg(Format(cTranslationSuccess, [FCommand.Framework, FCommand.OutputFile]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0)
  else
  begin
    ProgressBar.Position := 0;
    FOutput.Show;
    FOutput.ScrollToBottom;
  end;
end;

procedure TMainView.DumpASTActionExecute(Sender: TObject);
const
  { Whether to include system declarations in the AST }
  INCLUDE_SYSTEM_DECLS = False;
  { Whether source is C++ }
  IS_CPP = True;
  { Additional include directories (comma-seperated) }
  INCLUDE_DIRS = '';
var
  LDumper: TAstDumper;
  LClangIncludePath, LSDKName: string;
begin
  LClangIncludePath := TOctoidCommand.GetClangIncludePath;
  LSDKName := TPath.GetFileNameWithoutExtension(FCommand.Project.SdkRoot);
  FOutput.ClearMessages;
  LDumper := TAstDumper.Create(GetFrameworkDirectory, INCLUDE_SYSTEM_DECLS, IS_CPP, INCLUDE_DIRS);
  try
    LDumper.FileSuffix := Format('%s.%s.', [TPath.GetFileName(FCommand.Project.SdkRoot), LDumper.FrameworkName]);
    LDumper.OutputFolder := OutputPathEdit.Text;
    LDumper.OnMessage := TranslatorMessageHandler;
    LDumper.AddCmdLineArg(Format('-isystem%s', [TPath.Combine(FCommand.Project.SdkRoot, cSDKUserIncludeFolder)]));
    LDumper.AddCmdLineArg(Format('-isystem%s', [LClangIncludePath]));
    LDumper.AddCmdLineArg(Format('-F%s', [TPath.Combine(FCommand.Project.SdkRoot, cSDKFrameworksFolder)]));
    LDumper.AddCmdLineArg('-x');
    LDumper.AddCmdLineArg('objective-c');
    LDumper.AddCmdLineArg('-target');
    if LSDKName.StartsWith('MAC', True) then
      LDumper.AddCmdLineArg('x86_64-apple-darwin10')
    else
      LDumper.AddCmdLineArg('arm-apple-darwin10');
    LDumper.AddCmdLineArg('-ferror-limit=1000');
    LDumper.Run;
    // OutputMemo.Lines.LoadFromFile(TPath.Combine(LDumper.OutputFolder, LDumper.FileSuffix + 'Ast.txt'));
  finally
    LDumper.Free;
  end;
end;

procedure TMainView.ShowOutputActionExecute(Sender: TObject);
begin
  FOutput.Show;
end;

end.
