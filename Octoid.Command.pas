unit Octoid.Command;

interface

uses
  System.Classes,
  Octoid.CustomTranslator, Octoid.ObjCHeaderTranslator;

type
  /// <summary>
  ///   Performs the work of translating Objective-C headers
  /// </summary>
  /// <remarks>
  ///   Used by both the command-line application and any UI-oriented implementation
  /// </remarks>
  TOctoidCommand = class(TObject)
  private
    FErrorLimit: Integer;
    FExtraSwitches: TStrings;
    FFramework: string;
    FIsDump: Boolean;
    FOnMessage: TMessageEvent;
    FProject: TObjCHeaderProject;
    FNeedsShowUsage: Boolean;
    FSucceeded: Boolean;
    FTranslator: TCustomTranslator;
    function CheckHelpSwitches: Boolean;
    function CheckPathExists(const APath, ASwitch: string; const AIsFile: Boolean = False): Boolean;
    function CheckValidPlatform(const AValue: string): Boolean;
    function FindSwitchValue(const ASwitch: string; var AValue: string): Boolean;
    function GetCmdLine: string;
    function GetOutputFile: string;
    function GetSwitchValue(const ASource: string): string;
    procedure ReadCmdSwitches;
    procedure RunForFramework(const AFrameworkFolder: string);
    procedure ShowUsage;
    procedure TranslatorMessageHandler(const AMsg: string);
    procedure WriteInvalidValueMessage(const AValue, ASwitch: string; const ADefault: string = '');
  public
    /// <summary>
    ///   Returns the path to the latest installed version of the Clang includes
    /// </summary>
    class function GetClangIncludePath: string;
    /// <summary>
    ///   Creates an instance of TOctoidCommand and executes Run
    /// </summary>
    /// <remarks>
    ///   Requires command line switches in order to succeed
    /// </remarks>
    class procedure RunCommand;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Executes translation for the nominated framework. Translates any child frameworks in sub-folders of the framework
    /// </summary>
    procedure Run;
    procedure SetExtraSwitches(const ASwitches: string);
    /// <summary>
    ///   Limit of errors during translation
    /// </summary>
    property ErrorLimit: Integer read FErrorLimit write FErrorLimit;
    /// <summary>
    ///   Name of the framework to translate
    /// </summary>
    property Framework: string read FFramework write FFramework;
    /// <summary>
    ///   Resulting output file
    /// </summary>
    property OutputFile: string read GetOutputFile;
    /// <summary>
    ///   Contains properties of the project, e.g. target platform, SDK root, Clang path etc
    /// </summary>
    property Project: TObjCHeaderProject read FProject;
    /// <summary>
    ///   Indicates whether translation succeeded
    /// </summary>
    property Succeeded: Boolean read FSucceeded;
    /// <summary>
    ///   Does the work of translating the headers
    /// </summary>
    // property Translator: TObjCHeaderTranslator read FTranslator;
    property Translator: TCustomTranslator read FTranslator;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Win.Registry, System.Math,
  Winapi.Windows,
  Octoid.Consts, Octoid.AstDumper;

type
  TOpenTranslator = class(TObjCHeaderTranslator);

{ TOctoidCommand }

constructor TOctoidCommand.Create;
begin
  inherited;
  FErrorLimit := cErrorLimitDefault;
  FExtraSwitches := TStringList.Create;
  FProject := TObjCHeaderProject.Create;
  FProject.ClangIncludePath := GetClangIncludePath;
  FProject.IgnoreParseErrors := True;
  FProject.IncludeSubdirectories := True;
  FProject.EnumHandling := TEnumHandling.ConvertToConst;
  if IsConsole then
  begin
    if not CheckHelpSwitches then
      ReadCmdSwitches;
  end;
end;

destructor TOctoidCommand.Destroy;
begin
  FExtraSwitches.Free;
  FProject.Free;
  FTranslator.Free;
  inherited;
end;

procedure TOctoidCommand.TranslatorMessageHandler(const AMsg: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(AMsg)
  else
    Writeln(AMsg);
end;

procedure TOctoidCommand.SetExtraSwitches(const ASwitches: string);
var
  I: Integer;
  LValues: TArray<string>;
begin
  FExtraSwitches.Clear;
  LValues := ASwitches.Split([' '], '"');
  I := 0;
  while I <= High(LValues) do
  begin
    if LValues[I].StartsWith('-') and not LValues[I].StartsWith('--') then
    begin
      if (I < High(LValues)) and not LValues[I + 1].StartsWith('-') then
      begin
        FExtraSwitches.Add(LValues[I] + ' ' + LValues[I + 1]);
        Inc(I);
      end
      else
        FExtraSwitches.Add(LValues[I]);
    end;
    Inc(I);
  end;
end;

procedure TOctoidCommand.ShowUsage;
begin
  if IsConsole then
  begin
    Writeln(Format('Usage: %s [Options] [Extras]', [TPath.GetFileNameWithoutExtension(ParamStr(0))]));
    Writeln;
    Writeln('Options:');
    Writeln;
    Writeln(Format('%s or %s  - show this help', [cSwitchHelpWord, cSwitchHelpSymbol]));
    Writeln(Format('%s <sdkroot> - root of the target SDK', [cSwitchSdkRoot]));
    Writeln(Format('%s <clanginclude> - path to the Clang include files. May be omitted if a supported version of Clang is installed', [cSwitchClangInclude]));
    Writeln(Format('%s <platform> - the target platform (macOS or iOS)', [cSwitchTargetPlatform]));
    Writeln(Format('%s <framework> - (optional) transform only the framework with the specified name ' +
      '(default is all frameworks in the SDK)', [cSwitchFramework]));
    Writeln(Format('%s <out> - (optional) directory where the output .pas files should be placed ' +
      '(default is current directory)', [cSwitchOutputPath]));
    Writeln(Format('%s <typemap> - (optional) map of types containing equals separated values (can override existing mappings)', [cSwitchTypeMapFile]));
    Writeln(Format('%s <typeunitmap> - (optional) map of types to units containing equals separated values (can override existing mappings)', [cSwitchTypeUnitMapFile]));
    Writeln(Format('%s - dump AST and Types files into the output folder', [cSwitchDump]));
    Writeln;
    Writeln('Extras: additional options to be passed to libClang');
    Writeln;
    ExitCode := 2;
  end;
end;

function TOctoidCommand.CheckHelpSwitches: Boolean;
var
  LValue: string;
begin
  FNeedsShowUsage := FindSwitchValue(cSwitchHelpWord, LValue) or FindSwitchValue(cSwitchHelpSymbol, LValue);
  Result := FNeedsShowUsage;
end;

procedure TOctoidCommand.WriteInvalidValueMessage(const AValue, ASwitch: string; const ADefault: string = '');
begin
  if ADefault.IsEmpty then
    Writeln(Format('Invalid value for %s: %s', [ASwitch, AValue]))
  else
    Writeln(Format('Invalid value for %s: %s, using default of: %s', [ASwitch, AValue, ADefault]));
end;

function TOctoidCommand.CheckPathExists(const APath, ASwitch: string; const AIsFile: Boolean = False): Boolean;
begin
  if not TDirectory.Exists(APath) or (AIsFile and not TFile.Exists(APath)) then
  begin
    WriteInvalidValueMessage(APath, ASwitch);
    Result := False;
  end
  else
    Result := True;
end;

function TOctoidCommand.CheckValidPlatform(const AValue: string): Boolean;
begin
  if not (AValue.ToUpper.Equals(cTargetPlatformIOS) or AValue.ToUpper.Equals(cTargetPlatformMacOS)) then
  begin
    WriteInvalidValueMessage(AValue, cSwitchTargetPlatform);
    Result := False;
  end
  else
    Result := True;
end;

procedure TOctoidCommand.ReadCmdSwitches;
var
  LValue: string;
begin
  if FindSwitchValue(cSwitchSdkRoot, LValue) and CheckPathExists(LValue, cSwitchSdkRoot) then
    FProject.SdkRoot := LValue
  else
    FNeedsShowUsage := True;
  if FindSwitchValue(cSwitchClangInclude, LValue) and CheckPathExists(LValue, cSwitchClangInclude) then
    FProject.ClangIncludePath := LValue
  else if FProject.ClangIncludePath.IsEmpty or not TDirectory.Exists(FProject.ClangIncludePath) then
    FNeedsShowUsage := True;
  if FindSwitchValue(cSwitchTargetPlatform, LValue) and CheckValidPlatform(LValue) then
    FProject.TargetPlatform := LValue
  else
    FNeedsShowUsage := True;
  if FindSwitchValue(cSwitchFramework, LValue) then
    FFramework := LValue;
  if FindSwitchValue(cSwitchTypeMapFile, LValue) and CheckPathExists(LValue, cSwitchTypeMapFile) then
    FTranslator.TypeMapFileName := LValue;
  if FindSwitchValue(cSwitchTypeUnitMapFile, LValue) and CheckPathExists(LValue, cSwitchTypeUnitMapFile) then
    FTranslator.TypeUnitMapFileName := LValue;
  if FindSwitchValue(cSwitchOutputPath, LValue) then
  begin
    if CheckPathExists(LValue, cSwitchOutputPath) then
      FProject.OutputPath := LValue
    else
      FNeedsShowUsage := True;
  end
  else
    FProject.OutputPath := TDirectory.GetCurrentDirectory;
  if FindSwitchValue(cSwitchErrors, LValue) then
  begin
    if not TryStrToInt(LValue, FErrorLimit) or (FErrorLimit < cErrorLimitDefault) then
    begin
      WriteInvalidValueMessage(LValue, cSwitchErrors, cErrorLimitDefault.ToString);
      FErrorLimit := cErrorLimitDefault;
    end;
  end;
  FIsDump := FindSwitchValue(cSwitchDump, LValue);
  if not FNeedsShowUsage then
    SetExtraSwitches(GetCmdLine);
end;

function TOctoidCommand.FindSwitchValue(const ASwitch: string; var AValue: string): Boolean;
var
  I: Integer;
  LInQuote: Boolean;
  LCmdLine, LSource: string;
begin
  Result := False;
  LCmdLine := GetCmdLine;
  LInQuote := False;
  for I := 0 to Length(LCmdLine) - 1 do
  begin
    if not LInQuote and (LCmdLine.Chars[I] = '"') then
      LInQuote := True
    else if not LInQuote then
    begin
      LSource := LCmdLine.Substring(I, Length(ASwitch));
      if SameText(LSource, ASwitch) then
      begin
        AValue := AnsiDequotedStr(GetSwitchValue(LCmdLine.Substring(I + Length(LSource)).Trim), '"');
        Result := True;
        Break;
      end;
    end
    else if LInQuote and (LCmdLine.Chars[I] = '"') then
      LInQuote := False;
  end;
end;

class function TOctoidCommand.GetClangIncludePath: string;
var
  LRegistry: TRegistry;
  LClangPath, LLLVMPath: string;
  LVersionFolders: TArray<string>;
  I: Integer;
begin
  Result := '';
  LRegistry := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if LRegistry.OpenKey(cLLVMRegistryPath, False) then
    try
      LLLVMPath := LRegistry.ReadString('');
      if not LLLVMPath.IsEmpty and TDirectory.Exists(LLLVMPath) then
      begin
        LClangPath := TPath.Combine(LLLVMPath, cLLVMClangFolder);
        if TDirectory.Exists(LClangPath) then
        begin
          LVersionFolders := TDirectory.GetDirectories(LClangPath);
          for I := Length(LVersionFolders) - 1 downto 0 do
          begin
            LClangPath := TPath.Combine(LVersionFolders[I], cLLVMClangIncludeFolder);
            if TDirectory.Exists(LClangPath) then
            begin
              Result := LClangPath;
              Break;
            end;
          end;
        end;
      end;
    finally
      LRegistry.CloseKey;
    end;
  finally
    LRegistry.Free;
  end;
end;

function TOctoidCommand.GetCmdLine: string;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := CmdLine;
  {$WARN SYMBOL_PLATFORM ON}
end;

function TOctoidCommand.GetOutputFile: string;
begin
  Result := FProject.TargetPasFile;
end;

function TOctoidCommand.GetSwitchValue(const ASource: string): string;
var
  I: Integer;
  LIsQuoted: Boolean;
begin
  Result := '';
  if not ASource.IsEmpty then
  begin
    LIsQuoted := ASource.Chars[0] = '"';
    for I := 0 to Length(GetCmdLine) - 1 do
    begin
      if LIsQuoted and (I > 0) and (ASource.Chars[I] = '"') then
        Exit(ASource.Substring(0, I + 1)) // <======
      else if not LIsQuoted and (ASource.Chars[I] = ' ') then
        Exit(ASource.Substring(0, I)); // <======
    end;
    Result := ASource;
  end;
end;

procedure TOctoidCommand.Run;
var
  LFrameworkFolders: TArray<string>;
  LFolder, LFramework, LArg: string;
  I: Integer;
begin
  FSucceeded := False;
  if not FNeedsShowUsage then
  begin
    FProject.ClearCmdLineArgs;
    FProject.AddCmdLineArg(Format('-isystem%s', [TPath.Combine(FProject.SdkRoot, cSDKUserIncludeFolder)]));
    if not FProject.ClangIncludePath.IsEmpty then
      FProject.AddCmdLineArg(Format('-isystem%s', [FProject.ClangIncludePath]))
    else
      FProject.AddCmdLineArg(Format('-isystem%s', [TPath.Combine(FProject.SdkRoot, cSDKUserLibClangIncludePath)]));
    FProject.AddCmdLineArg(Format('-F%s', [TPath.Combine(FProject.SdkRoot, cSDKFrameworksFolder)]));
    FProject.AddCmdLineArg(Format('-F%s', [TPath.Combine(FProject.SdkRoot, cSDKFrameworksFolder)]));
    FProject.AddCmdLineArg('-x');
    FProject.AddCmdLineArg('objective-c');
    FProject.AddCmdLineArg('-ferror-limit=' + Max(FErrorLimit, cErrorLimitDefault).ToString); //!!!!
    FProject.AddCmdLineArg('-target');
    if FProject.TargetPlatform.Equals(cTargetPlatformMacOS) then
      FProject.AddCmdLineArg('x86_64-apple-darwin10')
    else if FProject.TargetPlatform.Equals(cTargetPlatformIOS) then
    begin
      FProject.AddCmdLineArg('arm-apple-darwin10');
      FProject.AddCmdLineArg('-DTARGET_OS_IPHONE=1');
    end;
    for I := 0 to FExtraSwitches.Count - 1 do
      FProject.AddCmdLineArg(FExtraSwitches[I]);

    FTranslator.Free;
    if not FIsDump then
      FTranslator := TObjCHeaderTranslator.Create(FProject)
    else
      FTranslator := TAstDumper.Create(FProject);
    FTranslator.OnMessage := TranslatorMessageHandler;
    TOpenTranslator(FTranslator).DoMessage('Using arguments:');
    for LArg in FProject.CmdLineArgs do
      TOpenTranslator(FTranslator).DoMessage(LArg);
    LFrameworkFolders := TDirectory.GetDirectories(TPath.Combine(FProject.SdkRoot, cSDKFrameworksFolder));
    FSucceeded := True;
    for LFolder in LFrameworkFolders do
    begin
      // If a framework name is supplied that does not exist, it just won't be transformed
      LFramework := TPath.GetFileNameWithoutExtension(LFolder);
      if LFolder.EndsWith('.framework', True) and (FFramework.IsEmpty or SameText(LFramework, FFramework)) then
        RunForFramework(LFolder);
    end;
    ExitCode := Ord(not FSucceeded); // i.e. exit code of 0 = success
  end
  else
    ShowUsage;
end;

procedure TOctoidCommand.RunForFramework(const AFrameworkFolder: string);
var
  LTargetFileName: string;
begin
  FProject.FrameworkDirectory := AFrameworkFolder;
  LTargetFileName := TPath.ChangeExtension(TPath.GetFileName(AFrameworkFolder), '.pas');
  if FProject.TargetPlatform.Equals(cTargetPlatformMacOS) then
    LTargetFileName := 'Macapi.' + LTargetFileName
  else if FProject.TargetPlatform.Equals(cTargetPlatformIOS) then
    LTargetFileName := 'iOSapi.' + LTargetFileName;
  FProject.TargetPasFile := TPath.Combine(FProject.OutputPath, LTargetFileName);
  if not FTranslator.Run then
    FSucceeded := False;
end;

class procedure TOctoidCommand.RunCommand;
var
  LCommand: TOctoidCommand;
begin
  LCommand := TOctoidCommand.Create;
  try
    LCommand.Run;
  finally
    LCommand.Free;
  end;
end;

end.
