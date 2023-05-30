unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, System.Actions, Vcl.ActnList,
  Octoid.Command;

type
  TJsonConfig = class(TObject)
  public
    class procedure CreateFromFile<T: TJsonConfig, constructor>(out AObject: T);
    class function GetConfigFileName: string; virtual;
  public
    procedure Save;
  end;

  TOctoidConfig = class(TJsonConfig)
  private
    FSDKPath: string;
    FSelectedFramework: string;
    FOutputPath: string;
  public
    class function GetConfigFileName: string; override;
  public
    property OutputPath: string read FOutputPath write FOutputPath;
    property SDKPath: string read FSDKPath write FSDKPath;
    property SelectedFramework: string read FSelectedFramework write FSelectedFramework;
  end;

  TfrmMain = class(TForm)
    TransformFrameworkButton: TButton;
    OutputMemo: TMemo;
    BottomPanel: TPanel;
    SDKPathFileOpenDialog: TFileOpenDialog;
    SDKGroupBox: TGroupBox;
    SelectSDKPathButton: TSpeedButton;
    SDKPathEdit: TEdit;
    SDKPathPanel: TPanel;
    SDKPathLabel: TLabel;
    SDKFrameworksListBox: TListBox;
    FrameworksLabel: TLabel;
    OutputPathGroupBox: TGroupBox;
    SelectOutputPathButton: TSpeedButton;
    OutputPathEdit: TEdit;
    OutputPathFileOpenDialog: TFileOpenDialog;
    ActionList: TActionList;
    SelectSDKPathAction: TAction;
    SelectOutputPathAction: TAction;
    TransformFrameworkAction: TAction;
    DumpASTButton: TButton;
    DumpASTAction: TAction;
    procedure SDKPathEditChange(Sender: TObject);
    procedure TransformFrameworkActionExecute(Sender: TObject);
    procedure SelectOutputPathActionExecute(Sender: TObject);
    procedure SelectSDKPathActionExecute(Sender: TObject);
    procedure TransformFrameworkActionUpdate(Sender: TObject);
    procedure OutputPathEditChange(Sender: TObject);
    procedure SDKFrameworksListBoxClick(Sender: TObject);
    procedure DumpASTActionExecute(Sender: TObject);
    procedure DumpASTActionUpdate(Sender: TObject);
  private
    FCommand: TOctoidCommand;
    FConfig: TOctoidConfig;
    function CanTransform: Boolean;
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
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.IOUtils, System.Types,
  REST.Json,
  Neslib.Clang, AstDumper,
  Octoid.CustomTranslator, Octoid.Consts;

type
  TJsonHelper = class helper for TJson
  public
    class function FileToObject<T: class, constructor>(out AObject: T; const AFileName: string;
      AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): Boolean; static;
    class procedure SaveToFile(const AObject: TObject; const AFileName: string); static;
  end;

{ TJsonHelper }

class function TJsonHelper.FileToObject<T>(out AObject: T; const AFileName: string;
  AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): Boolean;
var
  LJSON: string;
begin
  Result := False;
  LJSON := '';
  if System.IOUtils.TFile.Exists(AFileName) then
    LJSON := System.IOUtils.TFile.ReadAllText(AFileName);
  if not LJSON.IsEmpty then
  begin
    AObject := JsonToObject<T>(LJSON, AOptions);
    Result := True;
  end;
end;

class procedure TJsonHelper.SaveToFile(const AObject: TObject; const AFileName: string);
var
  LJSON: string;
begin
  if AObject = nil then
    LJSON := '{}'
  else
    LJSON := ObjectToJsonString(AObject);
  System.IOUtils.TFile.WriteAllText(AFileName, LJSON);
end;

{ TJsonConfig }

class procedure TJsonConfig.CreateFromFile<T>(out AObject: T);
begin
  if not TJson.FileToObject(AObject, T.GetConfigFileName) then
    AObject := T.Create;
end;

class function TJsonConfig.GetConfigFileName: string;
begin
  Result := '';
end;

procedure TJsonConfig.Save;
var
  LFileName: string;
begin
  LFileName := GetConfigFileName;
  ForceDirectories(TPath.GetDirectoryName(LFileName));
  TJson.SaveToFile(Self, GetConfigFilename);
end;

{ TOctoidConfig }

class function TOctoidConfig.GetConfigFileName: string;
begin
  Result := TPath.Combine(TPath.Combine(TPath.GetDocumentsPath, 'Octoid'), 'config.json');
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TOctoidConfig.CreateFromFile(FConfig);
  FCommand := TOctoidCommand.Create;
  FCommand.Translator.OnMessage := TranslatorMessageHandler;
  FCommand.Project.TargetPlatform := cTargetPlatformMacOS;
  OutputPathEdit.Text := FConfig.OutputPath;
  SDKPathEdit.Text := FConfig.SDKPath;
  UpdateFrameworks;
end;

destructor TfrmMain.Destroy;
begin
  FCommand.Free;
  FConfig.Free;
  inherited;
end;

procedure TfrmMain.SDKFrameworksListBoxClick(Sender: TObject);
begin
  if SDKFrameworksListBox.ItemIndex > -1 then
  begin
    FConfig.SelectedFramework := SDKFrameworksListBox.Items[SDKFrameworksListBox.ItemIndex];
    FConfig.Save;
  end;
end;

procedure TfrmMain.SDKPathEditChange(Sender: TObject);
begin
  UpdateFrameworks;
end;

procedure TfrmMain.TranslatorMessageHandler(const AMsg: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      DoOutput(AMsg);
    end
  );
end;

procedure TfrmMain.DoOutput(const AMsg: string);
var
  LDiagsFileName: string;
begin
  OutputMemo.Lines.Add(AMsg);
  if AMsg.EndsWith(' completed!') then
  begin
    LDiagsFileName := Format('%s.%s.log', [TPath.GetFileName(FCommand.Project.SdkRoot), FCommand.Project.FrameworkName]);
    OutputMemo.Lines.SaveToFile(TPath.Combine(TPath.GetDirectoryName(FCommand.Project.TargetPasFile), LDiagsFileName));
  end;
end;

procedure TfrmMain.UpdateFrameworks;
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

procedure TfrmMain.UpdateOutputPath(const APath: string);
begin
  if not APath.IsEmpty and TDirectory.Exists(APath) then
  begin
    FConfig.OutputPath := APath;
    FConfig.Save;
  end;
end;

function TfrmMain.GetFrameworkDirectory: string;
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

procedure TfrmMain.OutputPathEditChange(Sender: TObject);
begin
  UpdateOutputPath(OutputPathEdit.Text);
end;

procedure TfrmMain.SelectOutputPathActionExecute(Sender: TObject);
begin
  if OutputPathFileOpenDialog.Execute then
  begin
    OutputPathEdit.Text := OutputPathFileOpenDialog.FileName;
    UpdateOutputPath(OutputPathEdit.Text);
  end;
end;

procedure TfrmMain.SelectSDKPathActionExecute(Sender: TObject);
begin
  SDKPathFileOpenDialog.DefaultFolder := TPath.Combine(TPath.GetDocumentsPath, 'Embarcadero\Studio\SDKs');
  if SDKPathFileOpenDialog.Execute then
  begin
    SDKPathEdit.Text := SDKPathFileOpenDialog.FileName;
    UpdateFrameworks;
  end;
end;

function TfrmMain.CanTransform: Boolean;
begin
  Result := not FCommand.Project.SdkRoot.IsEmpty and TDirectory.Exists(FCommand.Project.SdkRoot) and
    (OutputPathEdit.Text <> '') and TDirectory.Exists(OutputPathEdit.Text) and (SDKFrameworksListBox.ItemIndex > -1);
end;

procedure TfrmMain.TransformFrameworkActionUpdate(Sender: TObject);
begin
  TransformFrameworkAction.Enabled := CanTransform;
end;

procedure TfrmMain.DumpASTActionUpdate(Sender: TObject);
begin
  DumpASTAction.Enabled := CanTransform;
end;

procedure TfrmMain.TransformFrameworkActionExecute(Sender: TObject);
var
  LSDKName: string;
begin
  OutputMemo.Text := '';
  LSDKName := TPath.GetFileNameWithoutExtension(FCommand.Project.SdkRoot);
  if LSDKName.StartsWith('MAC', True) then
    FCommand.Project.TargetPlatform := 'MACOS'
  else
    FCommand.Project.TargetPlatform := 'IOS';
  FCommand.Framework := FConfig.SelectedFramework;
  // Change this to wherever your Clang includes are
  FCommand.Project.ClangIncludePath := 'C:\Program Files\LLVM\lib\clang\7.0.0\include\';
  FCommand.Project.OutputPath := FCOnfig.OutputPath;
  TThread.CreateAnonymousThread(
    procedure
    begin
      FCommand.Run;
    end
  ).Start;
end;

procedure TfrmMain.DumpASTActionExecute(Sender: TObject);
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
  LClangIncludePath := 'C:\Program Files\LLVM\lib\clang\7.0.0\include\';
  LSDKName := TPath.GetFileNameWithoutExtension(FCommand.Project.SdkRoot);
  OutputMemo.Lines.Clear;
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
    OutputMemo.Lines.LoadFromFile(TPath.Combine(LDumper.OutputFolder, LDumper.FileSuffix + 'Ast.txt'));
  finally
    LDumper.Free;
  end;
end;

end.
