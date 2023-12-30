unit Octoid.CustomTranslator;

// Based on these units:
//   https://github.com/neslib/Chet/blob/master/Classes/Chet.HeaderTranslator.pas
//   https://github.com/neslib/Chet/blob/master/Classes/Chet.Project.pas

// The author Erik van Bilsen has indicated that users are free to do whatever they like with his work as long as he is not held accountable :-)

interface

uses
  System.Classes, System.Generics.Collections,
  Neslib.Clang,
  Chet.SourceWriter,
  Octoid.SourceReader, Octoid.Config;

type
  TReservedWordHandling = (PrefixAmpersand, PrefixUnderscore, PostfixUnderscore);

  TUnconvertibleHandling = (
    { Write a TODO comment and comment-out the declaration }
    WriteToDo,
    { Comment-out the declaration }
    CommentOut,
    { Ignore (remove) the declaration }
    Ignore
  );

  TCallConv = (
    { Use "cdecl" calling convention }
    Cdecl,
    { Use "stdcall" calling convention }
    StdCall
  );

  TEnumHandling = (
    { Convert to Delphi-style enumerated types }
    ConvertToEnum,
    { Convert to integer type, with each value converted to a constant }
    ConvertToConst
  );

  TCustomTranslatorProject = class(TObject)
  private
    FCallConv: TCallConv;
    FCmdLineArgs: TStrings;
    FEnumHandling: TEnumHandling;
    FIgnoreParseErrors: Boolean;
    FReservedWordHandling: TReservedWordHandling;
    FTargetPasFile: string;
    FTreatDirectivesAsReservedWords: Boolean;
    FUnconvertibleHandling: TUnconvertibleHandling;
    function GetCmdLineArgs: TArray<String>;
  protected
    function GetLibraryConstant: string; virtual;
    function GetLibraryConstantDeclaration: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCmdLineArg(const AArg: string);
    procedure ClearCmdLineArgs;
    property CallConv: TCallConv read FCallConv write FCallConv;
    property CmdLineArgs: TArray<String> read GetCmdLineArgs;
    property EnumHandling: TEnumHandling read FEnumHandling write FEnumHandling;
    property IgnoreParseErrors: Boolean read FIgnoreParseErrors write FIgnoreParseErrors;
    property ReservedWordHandling: TReservedWordHandling read FReservedWordHandling write FReservedWordHandling;
    property TargetPasFile: string read FTargetPasFile write FTargetPasFile;
    property TreatDirectivesAsReservedWords: Boolean read FTreatDirectivesAsReservedWords write FTreatDirectivesAsReservedWords;
    property UnconvertibleHandling: TUnconvertibleHandling read FUnconvertibleHandling write FUnconvertibleHandling;
  end;

  TMacroInfo = record
  public
    Cursor: TCursor;
    Visited: Boolean;
  public
    constructor Create(const ACursor: TCursor);
  end;

  TMessageEvent = procedure(const Msg: string) of object;

  TBuiltinTypes = array[Low(TTypeKind)..TTypeKind.LongDouble] of string;

  TCursorKinds = TArray<TCursorKind>;

  TCursorKindsHelper = record helper for TCursorKinds
  public
    function Contains(const AKind: TCursorKind): Boolean;
  end;

  /// <summary>
  ///   Generic C/C++ translator (or at least might be able to be used as one)
  /// </summary>
  TCustomTranslator = class(TObject)
  private
    class function ConvertEscapeSequences(const ASource: string): string; static;
    class function GenerateProcTypeNameForArg(const AFuncCursor, AArgCursor: TCursor): string; static;
    class function GetIndirectionCount(var AType: TType): Integer; static;
    class function IsEnumTypeDef(const ACursor: TCursor): Boolean; static;
    class function IsMostlyLowerCase(const AStr: string): Boolean; static;
    class function IsSystemCursor(const ACursor: TCursor): Boolean; static;
  private
    FAnonymousTypes: TDictionary<string, Integer>;
    FBuiltinTypes: TBuiltinTypes;
    FConfig: TOctoidConfig;
    FConsts: TList<TCursor>;
    FDeclaredTypes: TList<TCursor>;
    FEnumConsts: TDictionary<string, TCursor>;
    FHasClosing: Boolean;
    FIndex: IIndex;
    FInvalidConstants: TDictionary<string, Integer>;
    FMacros: TDictionary<string, TMacroInfo>;
    FMaxIndirectionCount: TDictionary<string, Integer>;
    FProject: TCustomTranslatorProject;
    FReservedWords: TDictionary<string, Integer>;
    FSourceReader: TSourceReader;
    FSymbolsToIgnore: TDictionary<string, Integer>;
    FTokenMap: TDictionary<string, string>;
    FTranslationUnit: ITranslationUnit;
    FTypeMap: TDictionary<string, string>;
    FTypeUnitMap: TDictionary<string, string>;
    FTypeMapFileName: string;
    FTypeUnitMapFileName: string;
    FTypes: TList<TCursor>;
    FUnitName: string;
    FVisitedTypes: TDictionary<TCursor, Integer>;
    FWriter: TSourceWriter;
    FOnMessage: TMessageEvent;
    procedure AnalyzeDeclaredTypes;
    procedure AnalyzeTypes;
    function GetArrayTypeName(const AType: TType): string;
    function GetBinaryOperand(const ASource: string): string;
    procedure LoadDictionaryFromFile(const AFileName: string; const ADictionary: TDictionary<string, string>);
    procedure Prepare;
    procedure SetupBuiltinTypes;
    procedure SetupReservedWords;
    procedure SetupTypeMap;
    procedure SetupTypeUnitMap;
    procedure TrackIndirections(const AType: TType);
    function VisitConstantsFirstPass(const ACursor, AParent: TCursor): TChildVisitResult;
    function VisitConstantsSecondPass(const ACursor, AParent: TCursor): TChildVisitResult;
    function VisitFunctions(const ACursor, AParent: TCursor): TChildVisitResult;
    function VisitTypes(const ACursor, AParent: TCursor): TChildVisitResult;
    procedure WriteCommentedOutOriginalSource(const ACursor: TCursor);
    procedure WriteConstant(const ACursor: TCursor);
    procedure WriteConstants;
    procedure WriteCopyrightHeader;
    procedure WriteDelphiSource;
    procedure WriteEnumType(const ACursor: TCursor);
    procedure WriteEnumTypeConst(const ACursor: TCursor);
    procedure WriteEnumTypeConstValue(const ACursor: TCursor);
    procedure WriteEnumTypeConstValueExpr(const ACursor: TCursor);
    procedure WriteEnumTypeEnum(const ACursor: TCursor);
    // procedure WriteEnumTypes;
    procedure WriteForwardTypeDeclarations;
    procedure WriteFunction(const ACursor: TCursor);
    procedure WriteFunctions;
    procedure WriteIndirections(ATypeName: string; const ADelphiTypeName: string = '');
    procedure WriteProceduralType(const ACursor: TCursor; const AType: TType; const ANamePrefix: string = '');
    procedure WriteStructType(const ACursor: TCursor; const AIsUnion: Boolean);
    procedure WriteStructTypeField(const ACursor: TCursor; const AIndex: Integer);
    procedure WriteStructTypeFieldElaborate(const ACursor: TCursor; const AIndex: Integer);
    procedure WriteStructTypeFieldStruct(const ACursor: TCursor);
    procedure WriteStructTypeUnion(const ACursor: TCursor);
    procedure WriteToDo(const AText: string);
    procedure WriteType(const ACursor: TCursor);
    procedure WriteTypedefType(const ACursor: TCursor);
    procedure WriteTypes;
  protected
    class function IsProceduralType(const AType: TType; out APointee: TType): Boolean; static;
  protected
    function CanIncludeCursor(const ACursor: TCursor): Boolean; virtual;
    function CanWriteToDo: Boolean; virtual;
    function GetBinaryOperatorTokens(const ASource: string): TArray<string>; virtual;
    function GetCursorFileName(const ACursor: TCursor): string;
    function GetUnderlyingType(const AType: TType): TType;
    procedure DebugMessage(const AMessage: string);
    procedure DoMessage(const AMessage: string); overload;
    procedure DoMessage(const AMessage: string; const AArgs: array of const); overload;
    procedure DoPrepare; virtual;
    procedure DoSetupTypeMap; virtual;
    procedure DoSetupTypeUnitMap; virtual;
    function FindFieldUnion(const ACursor: TCursor; out AUnionCursor: TCursor): Boolean;
    function GenerateAnonymousTypeName(const AName: string): string; virtual;
    function GetCursorSource(const ACursor: TCursor; const ADiagnostic: Boolean = False): string;
    function GetDelphiTypeName(AType: TType; const AInParamDecl: Boolean = False; const AOutIsAnonymous: PBoolean = nil): string; virtual;
    function GetUnexposedType(const AType: TType): string; virtual;
    function GetValidIdentifier(const AValue: string): string;
    function HandleTypeDeclaration(const ACursor: TCursor): Boolean; virtual;
    function HandleTypeDefinition(const ACursor: TCursor): Boolean; virtual;
    procedure HandleWriteType(const ACursor: TCursor); virtual;
    function IsAnonymous(const AName: string): Boolean; virtual;
    function IsConstIdentifier(const AName: string): Boolean; virtual;
    function PerformTranslation: Boolean;
    function Translate: ITranslationUnit; virtual;
    function RemoveQualifiers(const ATypeName: string): string; virtual;
    procedure SetupTokenMap; virtual;
    procedure WriteClasses; virtual;
    procedure WriteEnumTypeDecl(const ACursor: TCursor); virtual;
    procedure WriteForwardClasses; virtual;
    procedure WriteFunctionProto(const ACursor: TCursor; const AType: TType; const AFunctionName: string; const AInStruct: Boolean = False);
    procedure WriteImplementationContent; virtual;
    procedure WriteImplementationUsesClause; virtual;
    procedure WriteInterfaceUsesClause; virtual;
    property BuiltinTypes: TBuiltinTypes read FBuiltinTypes write FBuiltinTypes;
    property Consts: TList<TCursor> read FConsts;
    property DeclaredTypes: TList<TCursor> read FDeclaredTypes;
    property SourceReader: TSourceReader read FSourceReader;
    property SourceUnitName: string read FUnitName;
    property TranslationIndex: IIndex read FIndex;
    property TranslationUnit: ITranslationUnit read FTranslationUnit;
    property TokenMap: TDictionary<string, string> read FTokenMap;
    property TypeMap: TDictionary<string, string> read FTypeMap;
    property TypeUnitMap: TDictionary<string, string> read FTypeUnitMap;
    property Types: TList<TCursor> read FTypes;
    property Writer: TSourceWriter read FWriter;
  public
    constructor Create(const AProject: TCustomTranslatorProject); virtual;
    destructor Destroy; override;
    function Run: Boolean; virtual;
    property TypeMapFileName: string read FTypeMapFileName write FTypeMapFileName;
    property TypeUnitMapFileName: string read FTypeUnitMapFileName write FTypeUnitMapFileName;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils, System.IOUtils, System.Generics.Defaults, System.DateUtils, System.StrUtils,
  Neslib.Clang.Api;

function NormalizeFileName(const AFileName: string): string;
begin
  Result := StringReplace(AFileName, '/', '\', [rfReplaceAll]);
end;

{ TCustomTranslatorProject }

constructor TCustomTranslatorProject.Create;
begin
  inherited;
  FCmdLineArgs := TStringList.Create;
end;

destructor TCustomTranslatorProject.Destroy;
begin
  FCmdLineArgs.Free;
  inherited;
end;

procedure TCustomTranslatorProject.ClearCmdLineArgs;
begin
  FCmdLineArgs.Clear;
end;

function TCustomTranslatorProject.GetCmdLineArgs: TArray<String>;
begin
  Result := FCmdLineArgs.ToStringArray;
end;

function TCustomTranslatorProject.GetLibraryConstant: string;
begin
  Result := '';
end;

function TCustomTranslatorProject.GetLibraryConstantDeclaration: string;
begin
  Result := '';
end;

procedure TCustomTranslatorProject.AddCmdLineArg(const AArg: string);
begin
  FCmdLineArgs.Add(AArg);
end;

{ TMacroInfo }

constructor TMacroInfo.Create(const ACursor: TCursor);
begin
  Cursor := ACursor;
  Visited := False;
end;

{ TCursorKindsHelper }

function TCursorKindsHelper.Contains(const AKind: TCursorKind): Boolean;
var
  I: Integer;
begin
  for I := Low(Self) to High(Self) do
  begin
    if Self[I] = AKind then
      Exit(True);
  end;
  Result := False;
end;

{ TCustomTranslator }

constructor TCustomTranslator.Create(const AProject: TCustomTranslatorProject);
begin
  inherited Create;
  FConfig := TOctoidConfig.Current;
  FProject := AProject;
  FSourceReader := TSourceReader.Create;
  FIndex := TIndex.Create(False, False);
  FConsts := TList<TCursor>.Create;
  FEnumConsts := TDictionary<string, TCursor>.Create;
  FTypes := TList<TCursor>.Create;
  FDeclaredTypes := TList<TCursor>.Create;
  FVisitedTypes := TDictionary<TCursor, Integer>.Create(TEqualityComparer<TCursor>.Construct(
    function(const ALeft, ARight: TCursor): Boolean
    begin
      Result := (ALeft = ARight);
    end,

    function(const AValue: TCursor): Integer
    begin
      Result := AValue.Hash;
    end)
  );
  FReservedWords := TDictionary<string, Integer>.Create(TIStringComparer.Ordinal);
  FTypeMap := TDictionary<string, string>.Create;
  FTypeUnitMap := TDictionary<string, string>.Create;
  FTokenMap := TDictionary<string, string>.Create;
  FMacros := TDictionary<string, TMacroInfo>.Create;
  FMaxIndirectionCount := TDictionary<string, Integer>.Create;
  FSymbolsToIgnore := TDictionary<string, Integer>.Create;
  FAnonymousTypes := TDictionary<string, Integer>.Create;
  SetupBuiltinTypes;
  SetupReservedWords;
  SetupTypeMap;
  SetupTypeUnitMap;
  SetupTokenMap;
end;

destructor TCustomTranslator.Destroy;
begin
  FSourceReader.Free;
  FAnonymousTypes.Free;
  FSymbolsToIgnore.Free;
  FMaxIndirectionCount.Free;
  FTokenMap.Free;
  FTypeMap.Free;
  FTypeUnitMap.Free;
  FReservedWords.Free;
  FVisitedTypes.Free;
  FDeclaredTypes.Free;
  FTypes.Free;
  FConsts.Free;
  FEnumConsts.Free;
  FMacros.Free;
  inherited;
end;

procedure TCustomTranslator.DebugMessage(const AMessage: string);
begin
  OutputDebugString(PChar(AMessage));
end;

procedure TCustomTranslator.Prepare;
begin
  FConsts.Clear;
  FTypes.Clear;
  FDeclaredTypes.Clear;
  FVisitedTypes.Clear;
  DoPrepare;
end;

procedure TCustomTranslator.DoPrepare;
begin
  // Override in descendant
end;

procedure TCustomTranslator.DoMessage(const AMessage: string; const AArgs: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Format(AMessage, AArgs));
end;

procedure TCustomTranslator.DoMessage(const AMessage: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(AMessage);
end;

function TCustomTranslator.GetCursorFileName(const ACursor: TCursor): string;
var
  LFile: Neslib.Clang.TFile;
  LLine, LColumn, LOffset: Integer;
begin
  ACursor.Location.GetFileLocation(LFile, LLine, LColumn, LOffset);
  Result := NormalizeFileName(LFile.Filename);
end;

function TCustomTranslator.GetCursorSource(const ACursor: TCursor; const ADiagnostic: Boolean = False): string;
var
  LLineStart, LLineEnd, LColStart, LColEnd, LOffsetStart, LOffsetEnd, I, LTextColStart, LTextColEnd: Integer;
  LFile: Neslib.Clang.TFile;
  LReference: string;
begin
  // https://github.com/Embarcadero/octoid/issues/18
  //   Previously this routine would read only one line. Now it extracts the source across multiple lines, where needed
  ACursor.Extent.First.GetFileLocation(LFile, LLineStart, LColStart, LOffsetStart);
  ACursor.Extent.Last.GetFileLocation(LFile, LLineEnd, LColEnd, LOffsetEnd);
  LReference := string.Empty;
  if ADiagnostic then
  begin
    LReference := Format(' (%s - %s) (%s - %d:%d to %d:%d)',
      [ACursor.Kind.Spelling, ACursor.CursorType.KindSpelling, TPath.GetFileName(LFile.Filename.Replace('/', '\')),
        LLineStart, LColStart, LLineEnd, LColEnd]);
  end;
  for I := LLineStart to LLineEnd do
  begin
    LTextColEnd := 0;
    LTextColStart := 1;
    if I = LLineStart then
      LTextColStart := LColStart;
    if I = LLineEnd then
      LTextColEnd := LColEnd;
    if Result <> string.Empty then
      Result := Result + ' ';
    Result := Result + FSourceReader.ReadSubstring(LFile.Filename, I, LTextColStart, LTextColEnd - 1).Trim;
  end;
  if not LReference.IsEmpty then
    Result := Result + #13#10 + LReference;
end;

procedure TCustomTranslator.SetupBuiltinTypes;
begin
  FBuiltinTypes[TTypeKind.Bool] := 'Boolean';
  FBuiltinTypes[TTypeKind.Char_U] := 'Byte';
  FBuiltinTypes[TTypeKind.UChar] := 'Byte';
  FBuiltinTypes[TTypeKind.Char16] := 'WideChar';
  FBuiltinTypes[TTypeKind.Char32] := 'UCS4Char';
  FBuiltinTypes[TTypeKind.UShort] := 'Word';
  FBuiltinTypes[TTypeKind.UInt] := 'Cardinal';
  FBuiltinTypes[TTypeKind.ULong] := 'LongWord';
  FBuiltinTypes[TTypeKind.ULongLong] := 'UInt64';
  FBuiltinTypes[TTypeKind.Char_S] := 'AnsiChar'; // Certainly is in Objective-C
{
  case FProject.CharConvert of
    TCharConvert.UTF8Char:
      FBuiltinTypes[TTypeKind.Char_S] := 'UTF8Char';
    TCharConvert.Shortint:
      FBuiltinTypes[TTypeKind.Char_S] := 'Shortint';
  else
    FBuiltinTypes[TTypeKind.Char_S] := 'Byte';
  end;
}
  FBuiltinTypes[TTypeKind.SChar] := 'Shortint';
  FBuiltinTypes[TTypeKind.WChar] := 'WideChar';
  FBuiltinTypes[TTypeKind.Short] := 'Smallint';
  FBuiltinTypes[TTypeKind.Int] := 'Integer';
  FBuiltinTypes[TTypeKind.Long] := 'LongInt';
  FBuiltinTypes[TTypeKind.LongLong] := 'Int64';
  FBuiltinTypes[TTypeKind.Float] := 'Single';
  FBuiltinTypes[TTypeKind.Double] := 'Double';
  FBuiltinTypes[TTypeKind.LongDouble] := 'Extended';
end;

procedure TCustomTranslator.SetupReservedWords;
begin
  FReservedWords.Add('and', 0);
  FReservedWords.Add('end', 0);
  FReservedWords.Add('interface', 0);
  FReservedWords.Add('record', 0);
  FReservedWords.Add('var', 0);
  FReservedWords.Add('array', 0);
  FReservedWords.Add('except', 0);
  FReservedWords.Add('is', 0);
  FReservedWords.Add('repeat', 0);
  FReservedWords.Add('while', 0);
  FReservedWords.Add('as', 0);
  FReservedWords.Add('exports', 0);
  FReservedWords.Add('label', 0);
  FReservedWords.Add('resourcestring', 0);
  FReservedWords.Add('with', 0);
  FReservedWords.Add('asm', 0);
  FReservedWords.Add('file', 0);
  FReservedWords.Add('library', 0);
  FReservedWords.Add('set', 0);
  FReservedWords.Add('xor', 0);
  FReservedWords.Add('begin', 0);
  FReservedWords.Add('finalization', 0);
  FReservedWords.Add('mod', 0);
  FReservedWords.Add('shl', 0);
  FReservedWords.Add('case', 0);
  FReservedWords.Add('finally', 0);
  FReservedWords.Add('nil', 0);
  FReservedWords.Add('shr', 0);
  FReservedWords.Add('class', 0);
  FReservedWords.Add('for', 0);
  FReservedWords.Add('not', 0);
  FReservedWords.Add('string', 0);
  FReservedWords.Add('const', 0);
  FReservedWords.Add('function', 0);
  FReservedWords.Add('object', 0);
  FReservedWords.Add('then', 0);
  FReservedWords.Add('constructor', 0);
  FReservedWords.Add('goto', 0);
  FReservedWords.Add('of', 0);
  FReservedWords.Add('threadvar', 0);
  FReservedWords.Add('destructor', 0);
  FReservedWords.Add('if', 0);
  FReservedWords.Add('or', 0);
  FReservedWords.Add('to', 0);
  FReservedWords.Add('dispinterface', 0);
  FReservedWords.Add('implementation', 0);
  FReservedWords.Add('packed', 0);
  FReservedWords.Add('try', 0);
  FReservedWords.Add('div', 0);
  FReservedWords.Add('in', 0);
  FReservedWords.Add('procedure', 0);
  FReservedWords.Add('type', 0);
  FReservedWords.Add('do', 0);
  FReservedWords.Add('inherited', 0);
  FReservedWords.Add('program', 0);
  FReservedWords.Add('unit', 0);
  FReservedWords.Add('downto', 0);
  FReservedWords.Add('initialization', 0);
  FReservedWords.Add('property', 0);
  FReservedWords.Add('until', 0);
  FReservedWords.Add('else', 0);
  FReservedWords.Add('inline', 0);
  FReservedWords.Add('raise', 0);
  FReservedWords.Add('uses', 0);

  { Always treat built-in data types as reserved words. }
  FReservedWords.Add('byte', 0);
  FReservedWords.Add('shortint', 0);
  FReservedWords.Add('smallint', 0);
  FReservedWords.Add('word', 0);
  FReservedWords.Add('cardinal', 0);
  FReservedWords.Add('integer', 0);

  if FProject.TreatDirectivesAsReservedWords then
  begin
    FReservedWords.Add('absolute', 0);
    FReservedWords.Add('export', 0);
    FReservedWords.Add('public', 0);
    FReservedWords.Add('stdcall', 0);
    FReservedWords.Add('abstract', 0);
    FReservedWords.Add('external', 0);
    FReservedWords.Add('near', 0);
    FReservedWords.Add('published', 0);
    FReservedWords.Add('strict', 0);
    FReservedWords.Add('assembler', 0);
    FReservedWords.Add('far', 0);
    FReservedWords.Add('automated', 0);
    FReservedWords.Add('final', 0);
    FReservedWords.Add('operator', 0);
    FReservedWords.Add('unsafe', 0);
    FReservedWords.Add('cdecl', 0);
    FReservedWords.Add('forward', 0);
    FReservedWords.Add('out', 0);
    FReservedWords.Add('varargs', 0);
    FReservedWords.Add('overload', 0);
    FReservedWords.Add('register', 0);
    FReservedWords.Add('virtual', 0);
    FReservedWords.Add('override', 0);
    FReservedWords.Add('reintroduce', 0);
    FReservedWords.Add('deprecated', 0);
    FReservedWords.Add('pascal', 0);
    FReservedWords.Add('dispid', 0);
    FReservedWords.Add('platform', 0);
    FReservedWords.Add('safecall', 0);
    FReservedWords.Add('dynamic', 0);
    FReservedWords.Add('private', 0);
    FReservedWords.Add('sealed', 0);
    FReservedWords.Add('experimental', 0);
    FReservedWords.Add('message', 0);
    FReservedWords.Add('protected', 0);
    FReservedWords.Add('static', 0);

    { These are also directives, but the Delphi IDE doesn't highlight them as
      special keywords, so we don't regard them as "special":
        name, nodefault, read, stored, readonly, reference, contains, helper,
        default, implements, winapi, delayed, index, package, requires, write,
        resident, writeonly, local }
  end;
end;

procedure TCustomTranslator.SetupTokenMap;
begin
  // Override in descendant
end;

procedure TCustomTranslator.DoSetupTypeMap;
begin
  // Override in descendant
end;

procedure TCustomTranslator.DoSetupTypeUnitMap;
begin
  // Override in descendant
end;

procedure TCustomTranslator.LoadDictionaryFromFile(const AFileName: string; const ADictionary: TDictionary<string, string>);
var
  LTypes: TStrings;
  I: Integer;
begin
  if TFile.Exists(AFileName) then
  begin
    LTypes := TStringList.Create;
    try
      LTypes.LoadFromFile(AFileName);
      for I := 0 to LTypes.Count - 1 do
        ADictionary.AddOrSetValue(LTypes.Names[I], LTypes.ValueFromIndex[I]);
    finally
      LTypes.Free;
    end;
  end;
end;

procedure TCustomTranslator.SetupTypeMap;
begin
  // "Standard" mapping
  DoSetupTypeMap;
  // Allow file-based overrides
  LoadDictionaryFromFile(FTypeMapFileName, FTypeMap);
end;

procedure TCustomTranslator.SetupTypeUnitMap;
begin
  // "Standard" mapping
  DoSetupTypeUnitMap;
  // Allow file-based overrides
  LoadDictionaryFromFile(FTypeUnitMapFileName, FTypeUnitMap);
end;

function TCustomTranslator.GetValidIdentifier(const AValue: string): string;
var
  LColonPos: Integer;
begin
  Result := AValue;
  LColonPos := Pos(':', Result);
  if LColonPos > 0 then
    Delete(Result, LColonPos, Length(Result));
  if FReservedWords.ContainsKey(Result) then
  begin
    case FProject.ReservedWordHandling of
      TReservedWordHandling.PrefixUnderscore:
        Result := '_' + Result;
      TReservedWordHandling.PostfixUnderscore:
        Result := Result + '_';
    else
      Result := '&' + Result;
    end;
  end;
end;

class function TCustomTranslator.IsMostlyLowerCase(const AStr: string): Boolean;
var
  C: Char;
  UpCount, LoCount: Integer;
begin
  if (AStr = '') then
    Exit(False);

  C := AStr.Chars[0];
  if (C < 'a') or (C > 'z') then
    Exit(False);

  UpCount := 0;
  LoCount := 0;
  for C in AStr do
  begin
    case C of
      'a'..'z':
        Inc(LoCount);
      'A'..'Z':
        Inc(UpCount);
    end;
  end;

  Result := (LoCount > UpCount);
end;

function TCustomTranslator.IsAnonymous(const AName: string): Boolean;
begin
  // Override in descendant
  Result := False;
end;

function TCustomTranslator.IsConstIdentifier(const AName: string): Boolean;
begin
  Result := False;
end;

class function TCustomTranslator.IsEnumTypeDef(const ACursor: TCursor): Boolean;
var
  LIsEnum: Boolean;
begin
  LIsEnum := False;
  if ACursor.Kind = TCursorKind.TypedefDecl then
  begin
    ACursor.VisitChildren(
      function(const ACursor, AParent: TCursor): TChildVisitResult
      begin
        LIsEnum := ACursor.Spelling.StartsWith('enum ', True);
        Result := TChildVisitResult.Break;
      end
    );
  end;
  Result := LIsEnum;
end;

class function TCustomTranslator.ConvertEscapeSequences(const ASource: string): string;
var
  Index, ReplaceCount: Integer;
  C: Char;
  S: string;
  B, Val: Byte;
  HasReplacements: Boolean;
begin
  Result := ASource;
  HasReplacements := False;
  Index := 0;
  while True do
  begin
    Index := Result.IndexOf('\', Index);
    if (Index < 0) or (Index >= (Result.Length - 1)) then
      Break;

    ReplaceCount := 2;
    C := Result.Chars[Index + 1];
    case C of
      'a':
        S := '''#7''';
      'b':
        S := '''#8''';
      'f':
        S := '''#12''';
      'n':
        S := '''#10''';
      'r':
        S := '''#13''';
      't':
        S := '''#9''';
      'v':
        S := '''#11''';
      '''':
        S := '''''';
      'x':
        begin
          { Convert hex byte }
          if ((Index + 3) >= Result.Length) then
            Break;
          B := StrToIntDef('$' + Result.Chars[Index + 2] + Result.Chars[Index + 3], 32);
          S := '''#' + B.ToString + '''';
          ReplaceCount := 4;
        end;
      '0'..'7':
        begin
          { Convert to octal byte }
          Val := Ord(C) - Ord('0');
          if ((Index + 2) < Result.Length) then
          begin
            C := Result.Chars[Index + 2];
            if (C >= '0') and (C <= '7') then
            begin
              Inc(ReplaceCount);
              Val := (Val shl 3) or (Ord(C) - Ord('0'));

              if ((Index + 3) < Result.Length) then
              begin
                C := Result.Chars[Index + 3];
                if (C >= '0') and (C <= '7') then
                begin
                  Inc(ReplaceCount);
                  Val := (Val shl 3) or (Ord(C) - Ord('0'));
                end;
              end;
            end;
          end;
          S := '''#' + Val.ToString + '''';
        end
    else
      { Uncommon escape sequences we currently don't support:
        * "\e": non-standard escape character
        * "\Uhhhhhhhh": 32-bit Unicode codepoint
        * "\uhhhh": 16-bit Unicode codepoint }
      S := C;
    end;

    Result := Result.Remove(Index, ReplaceCount).Insert(Index, S);
    HasReplacements := True;
    Inc(Index, S.Length);
  end;

  if (HasReplacements) then
  begin
    if (Result.Length > 3) and (Result.StartsWith('''''')) and (Result.Chars[2] <> '''') then
      Result := Result.Remove(0, 2);

    if (Result.Length > 3) and (Result.EndsWith('''''')) and (Result.Chars[Result.Length - 3] <> '''') then
      SetLength(Result, Result.Length - 2);
  end;
end;

function TCustomTranslator.GenerateAnonymousTypeName(const AName: string): string;
begin
  // Override in descendant
  Result := '';
end;

class function TCustomTranslator.GenerateProcTypeNameForArg(const AFuncCursor, AArgCursor: TCursor): string;
begin
  Result := AFuncCursor.Spelling + '_' + AArgCursor.Spelling;
end;

function TCustomTranslator.RemoveQualifiers(const ATypeName: string): string;
begin
  // Override in descendant
  Result := '';
end;

function TCustomTranslator.GetArrayTypeName(const AType: TType): string;
begin
  Result := '';
  if AType.Kind = TTypeKind.ConstantArray then
    Result := Format('array [0..%d] of %s', [AType.ArraySize - 1, GetDelphiTypeName(AType.ArrayElementType)])
  else if AType.Kind = TTypeKind.Vector then
    Result := Format('array [0..%d] of %s', [AType.ElementCount - 1, GetDelphiTypeName(AType.ElementType)]);
end;

function TCustomTranslator.GetUnderlyingType(const AType: TType): TType;
begin
  Result := AType;
  if AType.Kind = TTypeKind.Typedef then
    Result := AType.Declaration.TypedefDeclUnderlyingType;
end;

function TCustomTranslator.GetUnexposedType(const AType: TType): string;
begin
  Result := '';
end;

function TCustomTranslator.GetDelphiTypeName(AType: TType; const AInParamDecl: Boolean; const AOutIsAnonymous: PBoolean): string;
var
  OrigType: TType;
  Kind: TTypeKind;
  StarCount: Integer;
  Conv: string;
begin
  if Assigned(AOutIsAnonymous) then
    AOutIsAnonymous^ := False;

  OrigType := AType;
  Kind := AType.Kind;
  if (Kind in [TTypeKind.ConstantArray, TTypeKind.Vector]) then
  begin
    if (AInParamDecl) then
      { Cannot have inline array declarations in a parameter. Treat these as
        pointer type instead. }
      Exit('P' + GetDelphiTypeName(AType.ArrayElementType))
    else
      Exit(GetArrayTypeName(AType));
  end
  else if (Kind = TTypeKind.IncompleteArray) then
  begin
    { Incomplete arrays are defined like:
        int foo[];
      Convert these to a pointer type. }
    Exit('P' + GetDelphiTypeName(AType.ArrayElementType));
  end;

  StarCount := 0;
  while (Kind = TTypeKind.Pointer) do
  begin
    Inc(StarCount);
    AType := AType.PointeeType;
    Kind := AType.Kind;
  end;

  if Kind = TTypeKind.Unexposed then
  begin
    Result := GetUnexposedType(OrigType);
    if not Result.IsEmpty then
      Exit // <======
    else
      { We cannot handle unexposed typed. }
      Exit('Integer { TODO : Cannot convert original type "' + OrigType.Spelling + '" }');
  end;

  if (Kind <= TTypeKind.LongDouble) then
  begin
    Result := FBuiltinTypes[Kind];
    if (Result <> '') then
    begin
      if (StarCount > 0) then
        Result := string.Create('P', StarCount) + Result;
      Exit;
    end;
  end;

  Result := RemoveQualifiers(AType.Spelling);
  if IsAnonymous(Result) then
  begin
    Result := GenerateAnonymousTypeName(Result);
    if Assigned(AOutIsAnonymous) then
      AOutIsAnonymous^ := True;
  end;

  if (FTypeMap.TryGetValue(Result, Conv)) then
    Result := Conv;

  if (StarCount = 0) then
    Result := GetValidIdentifier(Result)
  else if (Kind = TTypeKind.Void) then
  begin
    if (StarCount = 1) then
      Result := 'Pointer'
    else
      Result := string.Create('P', StarCount - 1) + 'Pointer';
  end
  else
    Result := string.Create('P', StarCount) + Result;
end;

procedure TCustomTranslator.AnalyzeDeclaredTypes;
var
  TypeNames: TDictionary<string, Integer>;
  Cursor: TCursor;
  I: Integer;
  S: string;
begin
  { Check the cursors in FDeclaredTypes. These contain structs that were
    declared but not defined. Check if they got defined later, that is, if they
    are in the FTypes list as well. Since the cursors will be different, we
    have to compare by name. }
  if (FDeclaredTypes.Count = 0) then
    Exit;

  TypeNames := TDictionary<string, Integer>.Create;
  try
    for Cursor in FTypes do
      TypeNames.AddOrSetValue(Cursor.Spelling, 0);

    for I := FDeclaredTypes.Count - 1 downto 0 do
    begin
      if TypeNames.ContainsKey(FDeclaredTypes[I].Spelling) then
        { Type was later defined. Remove it. }
        FDeclaredTypes.Delete(I);
    end;

    { Finally, there may be duplicate declared types. Remove these. }
    TypeNames.Clear;
    for I := FDeclaredTypes.Count - 1 downto 0 do
    begin
      S := FDeclaredTypes[I].Spelling;
      if TypeNames.ContainsKey(S) then
        FDeclaredTypes.Delete(I)
      else
        TypeNames.Add(S, 0);
    end;
  finally
    TypeNames.Free;
  end;
end;

procedure TCustomTranslator.AnalyzeTypes;
begin
  DoMessage('Analyzing data types...');
  { Walk the AST to discover all used types. When a type depends on other types
    (eg. fields in a struct), then add those dependent types to the list BEFORE
    the type, so we can avoid having forward type declarations. }
  FTranslationUnit.Cursor.VisitChildren(VisitTypes);
  AnalyzeDeclaredTypes;
end;

class function TCustomTranslator.GetIndirectionCount(var AType: TType): Integer;
var
  T: TType;
begin
  Result := 0;
  T := AType;
  while (T.Kind = TTypeKind.Pointer) do
  begin
    Inc(Result);
    T := T.PointeeType;
  end;
  AType := T;
end;

procedure TCustomTranslator.TrackIndirections(const AType: TType);
var
  T: TType;
  TypeName: string;
  IndirectionCount, MaxIndirectionCount: Integer;
begin
  T := AType;
  IndirectionCount := GetIndirectionCount(T);
  if (IndirectionCount >= 1) then
  begin
    { This is a pointer-to-a-pointer(to-a-pointer*) type. We need to create
      additional forward declarations for there, so keep track of them. }
    TypeName := RemoveQualifiers(T.Spelling);
    if (FMaxIndirectionCount.TryGetValue(TypeName, MaxIndirectionCount)) then
    begin
      if (IndirectionCount > MaxIndirectionCount) then
        FMaxIndirectionCount[TypeName] := IndirectionCount;
    end
    else
      FMaxIndirectionCount.Add(TypeName, IndirectionCount);
  end;
end;

class function TCustomTranslator.IsSystemCursor(const ACursor: TCursor): Boolean;
begin
  //!!!!! Might need an "exception" list
  Result := { not ACursor.Spelling.Equals('NSObject') and } ACursor.Location.IsInSystemHeader;
end;

class function TCustomTranslator.IsProceduralType(const AType: TType; out APointee: TType): Boolean;
var
  LTypeKind: TTypeKind;
begin
  LTypeKind := AType.Kind;
  if LTypeKind in [TTypeKind.Pointer, TTypeKind.BlockPointer] then // LTypeKind = TTypeKind.Pointer then
  begin
    APointee := AType.PointeeType;
    if (APointee.Kind = TTypeKind.Unexposed) then
      APointee := AType.CanonicalType.PointeeType;

    if (APointee.Kind in [TTypeKind.FunctionProto, TTypeKind.FunctionNoProto]) then
      Exit(True);
  end;

  Result := False;
end;

function TCustomTranslator.CanIncludeCursor(const ACursor: TCursor): Boolean;
begin
  Result := True;
end;

function TCustomTranslator.CanWriteToDo: Boolean;
begin
  Result := True;
end;

function TCustomTranslator.VisitConstantsFirstPass(const ACursor, AParent: TCursor): TChildVisitResult;
begin
  if CanIncludeCursor(ACursor) and (ACursor.Kind = TCursorKind.MacroDefinition) then
    { Record all #defines first, so we can reorder later based on dependencies }
    FMacros.AddOrSetValue(ACursor.Spelling, TMacroInfo.Create(ACursor));

  { Do not recurse. Only handle top-level #defines }
  Result := TChildVisitResult.Continue;
end;

function TCustomTranslator.VisitConstantsSecondPass(const ACursor, AParent: TCursor): TChildVisitResult;
begin
  if not IsSystemCursor(ACursor) and CanIncludeCursor(ACursor) and (ACursor.Kind = TCursorKind.MacroDefinition) then
    WriteConstant(ACursor);

  { Do not recurse. Only handle top-level #defines }
  Result := TChildVisitResult.Continue;
end;

function TCustomTranslator.VisitFunctions(const ACursor, AParent: TCursor): TChildVisitResult;
begin
  // Inlined functions are never exported
  if not IsSystemCursor(ACursor) and CanIncludeCursor(ACursor) and (ACursor.Kind = TCursorKind.FunctionDecl) and not ACursor.IsFunctionInlined then
    WriteFunction(ACursor);

  { Do not recurse. Only handle top-level functions }
  Result := TChildVisitResult.Continue;
end;


function TCustomTranslator.VisitTypes(const ACursor, AParent: TCursor): TChildVisitResult;
//var
//  LSpelling: string;
begin
  // "System" types are not visited, because it is assumed they are already implemented by the language requiring the translation
  // CanIncludeCursor is used to filter types from non-target libraries
  if not IsSystemCursor(ACursor) and CanIncludeCursor(ACursor) then
  begin
    // LSpelling := ACursor.Spelling;
    { Visit children BEFORE adding this type to the list }
    ACursor.VisitChildren(VisitTypes);

    if not FVisitedTypes.ContainsKey(ACursor) then
    begin
      TrackIndirections(ACursor.CursorType);
      TrackIndirections(ACursor.ResultType);

      if ACursor.IsDefinition then
      begin
        if ACursor.Kind in [TCursorKind.StructDecl, TCursorKind.UnionDecl, TCursorKind.EnumDecl, TCursorKind.TypedefDecl, TCursorKind.ParmDecl] then
        begin
          // DPN
          if (ACursor.Kind = TCursorKind.EnumDecl) and (FProject.EnumHandling = TEnumHandling.ConvertToConst) then
          begin
            FConsts.Add(ACursor);
            if not ACursor.Spelling.IsEmpty then
              FEnumConsts.Add(ACursor.Spelling, ACursor);
          end
          else if not ACursor.IsAnonymous and not IsEnumTypeDef(ACursor) then
            FTypes.Add(ACursor);
          FVisitedTypes.Add(ACursor, 0);
        end
        // E.G:  static NSString *const kFIREventAddPaymentInfo NS_SWIFT_NAME(AnalyticsEventAddPaymentInfo) = @"add_payment_info";
        // static [type] const [name] is a definition (ACursor.IsDefinition) of ACursor.Kind: TCursorKind.VarDecl
        // Needs to be done like exported consts, except like this:
        //
        // function kFIREventAddPaymentInfo: NSString;
        // begin
        //   Result := StrToNSStr('add_payment_info');
        // end;
        //
        // This is how it comes out of the AST:
        //
        // kFIREventAddPaymentInfo: VarDecl(T1) [IsDeclaration, IsDefinition, HasAttributes] @Headers/FIREventNames.h (17:24)
        //   (unnamed): UnexposedAttr [IsAttribute, IsUnexposed, HasAttributes] @Headers/FIREventNames.h (17:48)
        //   NSString: ObjCClassRef(T2) [IsReference] @Headers/FIREventNames.h (17:8)
        //   "add_payment_info": ObjCStringLiteral(T3) [IsExpression, HasAttributes] @Headers/FIREventNames.h (18:5)
        //     "add_payment_info": StringLiteral(T4) [IsExpression] @Headers/FIREventNames.h (18:6)
        else if ACursor.Kind = TCursorKind.VarDecl then
          HandleTypeDeclaration(ACursor)
        else if not HandleTypeDefinition(ACursor) then ;
          // DoMessage('Unhandled type definition: ' + ACursor.Spelling);
      end
      else if ACursor.IsDeclaration then
      begin
        { TCursorKind.StructDecl is a struct type that is declared but not defined, as in:
            struct Foo;
          The struct MAY be defined later, but not always. If it is not defined
          later, then it is only used in a pointer reference (*Foo) and we treat
          as an opaque type later. }
        if ACursor.Kind = TCursorKind.StructDecl then
          FDeclaredTypes.Add(ACursor)
        else if not HandleTypeDeclaration(ACursor) then
        begin
//          if not LSpelling.IsEmpty then
//            DoMessage('Unhandled type declaration: ' + LSpelling);
        end;
      end;
    end;
  end;

  Result := TChildVisitResult.Continue;
end;

procedure TCustomTranslator.WriteClasses;
begin
  //
end;

procedure TCustomTranslator.WriteCommentedOutOriginalSource(const ACursor: TCursor);
var
  Range: TSourceRange;
  Tokens: ITokenList;
  I: Integer;
  S: string;
begin
  if (FProject.UnconvertibleHandling = TUnconvertibleHandling.Ignore) then
    Exit;

  Range := ACursor.Extent;
  if (Range.IsNull) then
    Exit;

  Tokens := FTranslationUnit.Tokenize(Range);
  if (Tokens = nil) or (Tokens.Count = 0) then
    Exit;

  FWriter.Write('(*');
  for I := 0 to Tokens.Count - 1 do
  begin
    S := FTranslationUnit.GetTokenSpelling(Tokens[I]);
    FWriter.Write(' ');
    FWriter.Write(S);
  end;
  FWriter.WriteLn(' *)');
end;

procedure TCustomTranslator.WriteConstant(const ACursor: TCursor);
const
  SUFFICES: array[0..10] of string = ('i8', 'i16', 'i32', 'i64', 'ui8', 'ui16', 'ui32', 'ui64', 'u', 'l', 'f');
var
  Range: TSourceRange;
  TokenList: ITokenList;
  Tokens: TArray<string>;
  S, Conv: string;
  C: Char;
  I, J, MaxSuffix: Integer;
  Info: TMacroInfo;
  HasSuffix, HasFloatToken, IsString: Boolean;
  LValue: TStringStream;
begin
  Range := ACursor.Extent;
  if (Range.IsNull) then
    Exit;

  S := ACursor.Spelling;
  if FMacros.TryGetValue(S, Info) then
  begin
    if Info.Visited then
      Exit;
  end;
  Info.Cursor := ACursor;
  Info.Visited := True;
  FMacros.AddOrSetValue(S, Info);

  if ACursor.IsMacroFunctionLike then
  begin
    { This a a #define that looks like a function, as in:
        #define foo(x) (x < 0) ? -x : x
      We cannot convert these. }
    WriteToDo('Unable to convert function-like macro:');
    FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
    if CanWriteToDo then
      WriteCommentedOutOriginalSource(ACursor);
    Exit;
  end;

  { Tokenize the macro (#define) definition. This gives us a list of tokens
    that make up the definition (including the original macro name). We try to
    convert these tokens to Delphi. }

  TokenList := FTranslationUnit.Tokenize(Range);
  if (TokenList = nil) or (TokenList.Count < 2) then
    Exit;

  { Check any TokenList for symbols we do not support }
  HasFloatToken := False;
  SetLength(Tokens, TokenList.Count);
  for I := 0 to TokenList.Count - 1 do
  begin
    S := FTranslationUnit.GetTokenSpelling(TokenList[I]);
    if (S = '{') or (S = '?') or (S = ',') then
    begin
      WriteToDo('Unable to convert macro:');
      FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
      if CanWriteToDo then
        WriteCommentedOutOriginalSource(ACursor);
      Exit;
    end
    else if (S = '*') and ((I = 0) or (I = (TokenList.Count - 1))) then
    begin
      { '*' at begin or end }
      WriteToDo('Unable to convert macro:');
      FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
      if CanWriteToDo then
        WriteCommentedOutOriginalSource(ACursor);
      Exit;
    end
    else if (S.StartsWith('__')) then
    begin
      WriteToDo(Format('Macro refers to system symbol "%s":', [S]));
      FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
      if CanWriteToDo then
        WriteCommentedOutOriginalSource(ACursor);
      Exit;
    end
    else if IsMostlyLowerCase(S) then
    begin
      WriteToDo(Format('Macro probably use invalid symbol "%s":', [S]));
      FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
      if CanWriteToDo then
        WriteCommentedOutOriginalSource(ACursor);
      Exit;
    end
    else if FInvalidConstants.ContainsKey(S) then
    begin
      WriteToDo(Format('Macro uses commented-out symbol "%s":', [S]));
      FInvalidConstants.AddOrSetValue(ACursor.Spelling, 0);
      if CanWriteToDo then
        WriteCommentedOutOriginalSource(ACursor);
      Exit;
    end;

    if FMacros.TryGetValue(S, Info) then
    begin
      if (not Info.Visited) then
        { Write dependent macro first }
        WriteConstant(Info.Cursor);
    end;

    if (S.IndexOf('.') >= 0) then
      HasFloatToken := True;
    Tokens[I] := S;
  end;

  if (FSymbolsToIgnore.ContainsKey(Tokens[0])) then
    Exit;

  { Seems that libclang does not provide comments for #defines. Call WriteComment
    anyway in case future libclang versions do. }
  //!!!!! FCommentWriter.WriteComment(ACursor);

  LValue := TStringStream.Create;
  try

    for I := 1 to TokenList.Count - 1 do
    begin
      S := Tokens[I];
      // TODO: Should do other checks, here
      if S.StartsWith('_Pragma') then
        Break;
      IsString := False;

      { Convert commonly used tokens }
      if (FTokenMap.TryGetValue(S, Conv)) then
        S := Conv
      else if (S = '/') then
      begin
        { Could be "/" or "div". If we found a token with a period (.) then
          assume "/", otherwise "div". }
        if (not HasFloatToken) then
          S := ' div ';
      end
      else if (S.StartsWith('.')) then
        { Floating-point values in Delphi cannot start with a period }
        S := '0' + S
      else if (S.StartsWith('0x', True)) then
        { Convert hex value }
        S := '$' + S.Substring(2)
      else if (S.StartsWith('"') and S.EndsWith('"')) then
      begin
        { Convert to single quotes and convert escape sequences }
        S := ConvertEscapeSequences('''' + S.Substring(1, S.Length - 2) + '''');
        IsString := True;
      end
      else if (S.StartsWith('''') and S.EndsWith('''')) then
      begin
        { Convert escape sequences }
        S := ConvertEscapeSequences(S);
        IsString := True;
      end;

      { Check for type suffixes (as in 123LL) and remove those }
      C := S.Chars[0];
      if ((C >= '0') and (C <= '9')) or (C = '$') then
      begin
        if (C <> '$') and (S.IndexOf('.') > 0) then
          { 'f' is only a valid suffix for floating-point values }
          MaxSuffix := LENGTH(SUFFICES) - 1
        else
          MaxSuffix := LENGTH(SUFFICES) - 2;

        repeat
          HasSuffix := False;
          for J := 0 to MaxSuffix do
          begin
            if (S.EndsWith(SUFFICES[J], True)) then
            begin
              HasSuffix := True;
              SetLength(S, S.Length - SUFFICES[J].Length);
            end;
          end;
        until (not HasSuffix);
      end;

      if IsString then
      begin
        { In macros, strings can be concatenated like this:
            FOO "str" BAR
          We need to put '+' inbetween }
        if (I > 1) and (Tokens[I - 1] <> '+') then
          // FWriter.Write('+');
          LValue.WriteString('+');

        // FWriter.Write(S);
        LValue.WriteString(S);

        if (I < (TokenList.Count - 1)) and (Tokens[I + 1] <> '+') then
          // FWriter.Write('+');
          LValue.WriteString('+');
      end
      else
        // FWriter.Write(S);
        LValue.WriteString(S);
    end;
    if LValue.Size > 0 then
    begin
      { First token is macro (constant) name }
      FWriter.Write(Tokens[0]);
      FWriter.Write(' = ');
      FWriter.Write(LValue.DataString);
      FWriter.WriteLn(';');
    end;
  finally
    LValue.Free;
  end;
end;

procedure TCustomTranslator.WriteConstants;
var
  LCursor: TCursor;
begin
  DoMessage('Writing constants...');
  FWriter.StartSection('const');

  FTranslationUnit.Cursor.VisitChildren(VisitConstantsFirstPass);
  FInvalidConstants := TDictionary<string, Integer>.Create;
  try
    FTranslationUnit.Cursor.VisitChildren(VisitConstantsSecondPass);
  finally
    FInvalidConstants.Free;
  end;

  for LCursor in FConsts do
  begin
    if LCursor.Kind = TCursorKind.EnumDecl then
      WriteEnumTypeConst(LCursor);
  end;
  FWriter.EndSection;
end;

procedure TCustomTranslator.WriteCopyrightHeader;
var
  LLength, LLongest, I: Integer;
  LPaddingLeft, LPaddingRight: string;
  LLines: TStrings;
begin
  if TFile.Exists(FConfig.GetBannerFileName) then
  begin
    LLines := TStringList.Create;
    try
      LLines.Text := TFile.ReadAllText(FConfig.GetBannerFileName);
      if LLines.Count > 0 then
      begin
        if FConfig.UseBannerAsIs then
        begin
          for I := 0 to LLines.Count - 1 do
            FWriter.WriteLn(LLines[I]);
        end
        else
        begin
          LLongest := 0;
          for I := 0 to LLines.Count - 1 do
          begin
            if Length(LLines[I]) > LLongest then
              LLongest := Length(LLines[I]);
          end;
          FWriter.WriteLn('{' + StringOfChar('*', LLongest + 2) + '}');
          FWriter.WriteLn('{' + StringOfChar(' ', LLongest + 2) + '}');
          for I := 0 to LLines.Count - 1 do
          begin
            LLength := Length(LLines[I]);
            LPaddingLeft := StringOfChar(' ', (LLongest - LLength) div 2);
            LPaddingRight := StringOfChar(' ', LLongest - LLength - LPaddingLeft.Length);
            FWriter.WriteLn('{ ' + LPaddingLeft + LLines[I] + LPaddingRight + ' }');
          end;
          FWriter.WriteLn('{' + StringOfChar(' ', LLongest + 2) + '}');
          FWriter.WriteLn('{' + StringOfChar('*', LLongest + 2) + '}');
        end;
      end;
    finally
      LLines.Free;
    end;
  end;
end;

procedure TCustomTranslator.WriteEnumType(const ACursor: TCursor);
begin
  if ACursor.IsDeclaration then
    WriteEnumTypeDecl(ACursor)
  else if (FProject.EnumHandling = TEnumHandling.ConvertToEnum) then
    WriteEnumTypeEnum(ACursor)
  else
    WriteEnumTypeConst(ACursor);
end;

procedure TCustomTranslator.WriteEnumTypeConst(const ACursor: TCursor);
var
  LNames: TArray<string>;
  LSpelling: string;
begin
  LNames := [];
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      LSpelling := ACursor.Spelling;
      if not LSpelling.IsEmpty and (IndexStr(LSpelling.ToLower, LNames) = -1) then
      begin
        FWriter.Write('%s = ', [LSpelling]);
        WriteEnumTypeConstValue(ACursor);
        FWriter.WriteLn(';');
        LNames := LNames + [LSpelling.ToLower];
      end;
      { NOTE: Enum values usually have children of an expression type. However,
        we don't translate expressions. Instead, we use de constant value that
        libclang provides. }
      Result := TChildVisitResult.Continue;
    end
  );
end;

procedure TCustomTranslator.WriteEnumTypeConstValue(const ACursor: TCursor);
var
  LSource: string;
  LHandled: Boolean;
begin
  LHandled := False;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      case ACursor.Kind of
        TCursorKind.UnexposedExpr:
        begin
          //!!!!!!!!!!!! Could actually check one more level deep, here
          FWriter.Write(AParent.EnumConstantDeclValue.ToString);
          LHandled := True;
        end;
        TCursorKind.ParenExpr:
        begin
          WriteEnumTypeConstValueExpr(ACursor);
          LHandled := True;
        end;
        TCursorKind.DeclRefExpr:
        begin
          FWriter.Write(ACursor.Spelling);
          LHandled := True;
        end;
        TCursorKind.IntegerLiteral:
        begin
          // AST does not give the underlying type?
          LSource := GetCursorSource(ACursor);
          if IsConstIdentifier(LSource) then
            FWriter.Write(LSource)
          else
            FWriter.Write(AParent.EnumConstantDeclValue.ToString);
          LHandled := True;
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
  if not LHandled then
    FWriter.Write(ACursor.EnumConstantDeclValue.ToString);
end;

procedure TCustomTranslator.WriteEnumTypeConstValueExpr(const ACursor: TCursor);
var
  LTokens: TArray<string>;
  LOperator: string;
begin
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    var
      I: Integer;
    begin
      case ACursor.Kind of
        TCursorKind.BinaryOperator:
        begin
          // Parsing the source since libClang does not provide this info
          LTokens := GetBinaryOperatorTokens(GetCursorSource(ACursor));
          for I := 0 to Length(LTokens) - 1 do
          begin
            if (I + 1) mod 2 = 0  then
            begin
              if not FTokenMap.TryGetValue(LTokens[I], LOperator) then
                LOperator := 'unknown';
              FWriter.Write(' %s ', [LOperator.Trim]);
            end
            else
              FWriter.Write('%s', [GetBinaryOperand(LTokens[I]).Trim]);
          end;
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
end;

function TCustomTranslator.GetBinaryOperand(const ASource: string): string;
const
  cNumeric = ['0'..'9'];
  cNumericDot = cNumeric + ['.'];
var
  LChar: Char;
begin
  Result := '';
  for LChar in ASource do
  begin
    if CharInSet(LChar, cNumericDot) then
      Result := Result + LChar;
  end;
  // If it's not a number, assume it's an identifier
  if Result.IsEmpty then
  begin
    Result := ASource;
    if Result.StartsWith('~') then
      Result := 'not ' + Result.Substring(1);
  end;
end;

function TCustomTranslator.GetBinaryOperatorTokens(const ASource: string): TArray<string>;
begin
  Result := [];
end;

procedure TCustomTranslator.WriteEnumTypeDecl(const ACursor: TCursor);
begin
  //
end;

procedure TCustomTranslator.WriteEnumTypeEnum(const ACursor: TCursor);
var
  T: TType;
  TypeName: string;
  FirstChild: Boolean;
  IsUnsigned: Boolean;
begin
  T := ACursor.CursorType;
  TypeName := GetDelphiTypeName(T);
  FWriter.WriteLn('%s = (', [TypeName]);
  FWriter.Indent;

  { Determine whether enum values are signed or unsigned. }
  T := ACursor.EnumDeclIntegerType;
  IsUnsigned := (T.Kind in [TTypeKind.Char_U, TTypeKind.UChar, TTypeKind.UShort, TTypeKind.UInt, TTypeKind.ULong, TTypeKind.ULongLong]);

  FirstChild := True;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      if (not FirstChild) then
        FWriter.WriteLn(',');

      //!!!!! FCommentWriter.WriteComment(ACursor);
      FWriter.Write('%s = ', [ACursor.Spelling]);
      if IsUnsigned then
        FWriter.Write(UIntToStr(ACursor.EnumConstantDeclUnsignedValue))
      else
        FWriter.Write(IntToStr(ACursor.EnumConstantDeclValue));

      FirstChild := False;

      { NOTE: Enum values usually have children of an expression type. However,
        we don't translate expressions. Instead, we use de constant value that
        libclang provides. }
          Result := TChildVisitResult.Continue;
    end);

  FWriter.WriteLn(');');
  FWriter.Outdent;
  FWriter.WriteLn('P%s = ^%0:s;', [TypeName]);
end;

{
procedure TCustomTranslator.WriteEnumTypes;
var
  Cursor: TCursor;
begin
  FWriter.StartSection('type');
  for Cursor in FTypes do
  begin
    if (Cursor.Kind = TCursorKind.EnumDecl) then
      WriteEnumType(Cursor);
  end;
  FWriter.EndSection;
end;
}

procedure TCustomTranslator.WriteForwardClasses;
begin
  //
end;

procedure TCustomTranslator.WriteForwardTypeDeclarations;
var
  Cursor: TCursor;
  S: string;
  First: Boolean;
  I, IndirectionCount: Integer;
  P: TPair<string, string>;

{
  procedure CheckFirst;
  begin
    if (First) then
    begin
      FWriter.WriteLn('// Forward declarations');
      First := False;
    end;
  end;
}

  procedure CheckIndirection(const ACNames: array of string; const APasName: string; const AStart: Integer = 2);
  var
    I, IndirectionCount, MaxIndirectionCount: Integer;
    S, CName: string;
  begin
    MaxIndirectionCount := 0;
    for CName in ACNames do
    begin
      if (FMaxIndirectionCount.TryGetValue(CName, IndirectionCount)) then
      begin
        if (IndirectionCount > MaxIndirectionCount) then
          MaxIndirectionCount := IndirectionCount;
      end;
    end;

    if (MaxIndirectionCount >= AStart) then
    begin
      // CheckFirst;
      S := APasName;

      if (AStart = 1) then
        FWriter.WriteLn('P%s = ^%0:s;', [S]);

      for I := 2 to MaxIndirectionCount do
      begin
        S := 'P' + S;
        FWriter.WriteLn('P%s = ^%0:s;', [S]);
      end;
    end;
  end;

begin
  First := True;
  WriteForwardClasses;
  //!!!!! This is C-specific - turn into virtual
(*
  { Check if we need pointer-to-pointer declarations for some builtin types. }
  case FProject.CharConvert of
    TCharConvert.UTF8Char:
      begin
        CheckIndirection(['char'], 'UTF8Char');
        CheckIndirection(['unsigned char'], 'Byte');
      end;
    TCharConvert.Shortint:
      begin
        CheckIndirection(['char', 'signed char'], 'Shortint');
        CheckIndirection(['unsigned char'], 'Byte');
      end
  else
    CheckIndirection(['char', 'unsigned char'], 'Byte');
  end;
  CheckIndirection(['short', 'short int'], 'Smallint');
  CheckIndirection(['unsigned short int'], 'Word');
  CheckIndirection(['long', 'int', 'long int'], 'Integer');
  CheckIndirection(['unsigned int', 'unsigned long int'], 'Cardinal');
  CheckIndirection(['long long int'], 'Int64');
  CheckIndirection(['unsigned long long int'], 'UInt64');
  CheckIndirection(['float'], 'Single');
  CheckIndirection(['double'], 'Double');
  CheckIndirection(['long double'], 'Extended');
*)

  for P in FTypeMap do
    CheckIndirection([P.Key], P.Value, 1);

  { Check any types that were declared, but not defined, as in:
      struct Foo;
    These are only used in pointer references (*Foo) and treated as opaque
    types. }
  for Cursor in FDeclaredTypes do
  begin
    S := Cursor.Spelling;
    if (S <> '') and (not FSymbolsToIgnore.ContainsKey(S)) then
    begin
      // CheckFirst;
      First := False;
      FWriter.WriteLn('P%s = Pointer;', [S]);
      FWriter.WriteLn('PP%s = ^P%0:s;', [S]);
    end;
  end;

  for Cursor in FTypes do
  begin
    case Cursor.Kind of
      TCursorKind.StructDecl:
        begin
          S := Cursor.Spelling;
          if (S = '') then
          begin
            (*For typedef structs without a name:

                typedef struct {...} Alias;

              The spelling will be empty. Use the type name.*)
            S := Cursor.CursorType.Spelling;
            if (IsAnonymous(S)) then
              S := ''
            else
              S := RemoveQualifiers(S);
          end;

          if (S <> '') and (not FSymbolsToIgnore.ContainsKey(S)) then
          begin
            // CheckFirst;
            First := False;
            FWriter.WriteLn('P%s = ^%0:s;', [S]);

            { Check if we need additional pointer-to-pointer declarations }
            if (FMaxIndirectionCount.TryGetValue(S, IndirectionCount)) then
            begin
              for I := 2 to IndirectionCount do
              begin
                S := 'P' + S;
                FWriter.WriteLn('P%s = ^%0:s;', [S]);
              end;
            end;
          end;
        end;
    end;
  end;
  if (not First) then
    FWriter.WriteLn;
end;

procedure TCustomTranslator.WriteFunction(const ACursor: TCursor);
var
  FuncCursor: TCursor;
  Name: string;
begin
  Name := ACursor.Spelling;
  FWriter.WriteLn;
  { Check if any of the function parameters is a procedural type. Delphi
    doesn't support inline procedural types, so we need to create declarations
    for those. }
  FuncCursor := ACursor;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    var
      CursorType, PointeeType: TType;
    begin
      if (ACursor.Kind = TCursorKind.ParmDecl) then
      begin
        CursorType := ACursor.CursorType;
        if IsProceduralType(CursorType, PointeeType) then
        begin
          FWriter.StartSection('type');
          { Make up name for this parameter type }
          FWriter.Write(GenerateProcTypeNameForArg(FuncCursor, ACursor));
          FWriter.Write(' = ');
          WriteFunctionProto(ACursor, PointeeType, '');
          FWriter.WriteLn(';');
          FWriter.EndSection;
          FWriter.WriteLn;
        end;
      end;

      Result := TChildVisitResult.Continue;
    end);

  //!!!! FCommentWriter.WriteComment(ACursor);
  WriteFunctionProto(ACursor, ACursor.CursorType, GetValidIdentifier(Name));
  FWriter.WriteLn(';');
  FWriter.WriteLn('  external %s name _PU + ''%s'';', [FProject.GetLibraryConstant, Name]);
end;

procedure TCustomTranslator.WriteFunctionProto(const ACursor: TCursor; const AType: TType; const AFunctionName: string; const AInStruct: Boolean);
var
  ProtoCursor: TCursor;
  ResType, ProtoType: TType;
  ArgIndex, ArgCount: Integer;
  HasResult, IsBlock: Boolean;
begin
  { AType is the function proto type (of kind FunctionProto or FunctionNoProto).
    Use its ResultType and ArgTypes properties for parameter type information.
    However, these do not provide info about the names of the parameters. Those
    are children of ACursor. Parameters do not have to have names. In that case,
    they are still children of ACursor, but Spelling will be empty.  }
  ResType := AType.ResultType;
  IsBlock := ACursor.TypedefDeclUnderlyingType.Kind = TTypeKind.BlockPointer;
  HasResult := (not (ResType.Kind in [TTypeKind.Invalid, TTypeKind.Void]));
  if (HasResult) then
    FWriter.Write('function')
  else
    FWriter.Write('procedure');

  { AFunctionName is empty for procedural types }
  if (AFunctionName <> '') then
  begin
    FWriter.Write(' ');
    FWriter.Write(AFunctionName);
  end;

  ProtoCursor := ACursor;
  ProtoType := AType;
  ArgIndex := 0;
  ArgCount := AType.ArgTypeCount;
  if ArgCount > 0 then
    FWriter.Write('(');
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    var
      CursorType, PointeeType: TType;
      ArgName: string;
    begin
      if (ACursor.Kind = TCursorKind.ParmDecl) then
      begin
        ArgName := GetValidIdentifier(ACursor.Spelling);
        if (ArgName = '') then
          ArgName := 'p' + (ArgIndex + 1).ToString;
        FWriter.Write(ArgName + ': ');
        CursorType := ACursor.CursorType;
        if (IsProceduralType(CursorType, PointeeType)) then
        begin
          { The argument is a procedural type. If this is a top-level function
            (AInStruct=False), then a procedural type declaration has been
            created earlier, and we can use it here. If this function is the
            type of a field in a recurd (AInStruct=True), then we cannot create
            a procedural type declaration on-the-fly. Instead, we write a TODO
            and use a pointer as parameter type. }
          if (AInStruct) then
          begin
            WriteToDo('Create a procedural type for this parameter');
            FWriter.Write('Pointer');
          end
          else
            FWriter.Write(GenerateProcTypeNameForArg(ProtoCursor, ACursor))
        end
        else
          FWriter.Write(GetDelphiTypeName(ACursor.CursorType, True));

        if (ArgIndex < (ArgCount - 1)) then
          FWriter.Write('; ');

        Inc(ArgIndex);
      end;

      Result := TChildVisitResult.Continue;
    end);

  if ArgCount > 0 then
    FWriter.Write(')');
  if (HasResult) then
  begin
    FWriter.Write(': ');
    FWriter.Write(GetDelphiTypeName(ResType));
  end;

  if (AType.Kind = TTypeKind.FunctionProto) and (AType.IsFunctionVariadic) then
    FWriter.Write(' varargs');

  if not IsBlock then
    FWriter.Write(';');

  if IsBlock then
    FWriter.Write(' of object')
  else if (FProject.CallConv = TCallConv.stdcall) then
    FWriter.Write(' stdcall')
  else
    FWriter.Write(' cdecl');
end;

procedure TCustomTranslator.WriteFunctions;
var
  LDeclaration: string;
begin
  DoMessage('Writing functions...');
  LDeclaration := FProject.GetLibraryConstantDeclaration;
  if not LDeclaration.IsEmpty then
  begin
    FWriter.WriteLn('const');
    FWriter.WriteLn('  ' + LDeclaration);
  end;
  FTranslationUnit.Cursor.VisitChildren(VisitFunctions);
  FWriter.WriteLn;
end;

procedure TCustomTranslator.WriteImplementationContent;
begin
  //
end;

procedure TCustomTranslator.WriteImplementationUsesClause;
begin
  //
end;

procedure TCustomTranslator.WriteIndirections(ATypeName: string; const ADelphiTypeName: string);
var
  I, IndirectionCount: Integer;
begin
  if (FMaxIndirectionCount.TryGetValue(ATypeName, IndirectionCount)) then
  begin
    if (IndirectionCount > 0) then
    begin
      { Special case for UTF8Char. We need to convert the first indirection to
        PUTF8Char so we can use it to pass string literals in code. }
      if (ADelphiTypeName = 'UTF8Char') then
        FWriter.WriteLn('P%s = PUTF8Char;', [ATypeName])
      else
        FWriter.WriteLn('P%s = ^%s;', [ATypeName, GetValidIdentifier(ATypeName)]);

      for I := 1 to IndirectionCount - 1 do
      begin
        ATypeName := 'P' + ATypeName;
        FWriter.WriteLn('P%s = ^%0:s;', [ATypeName]);
      end;
    end;
  end;
end;

procedure TCustomTranslator.WriteProceduralType(const ACursor: TCursor; const AType: TType; const ANamePrefix: string);
var
  LIdentifier: string;
begin
  FWriter.WriteLn;
  LIdentifier := GetValidIdentifier(ACursor.Spelling);
  FWriter.Write('%s%s = ', [ANamePrefix, LIdentifier]);
  WriteFunctionProto(ACursor, AType, '');
  FWriter.WriteLn(';');
end;

function TCustomTranslator.FindFieldUnion(const ACursor: TCursor; out AUnionCursor: TCursor): Boolean;
var
  LCount: Integer;
  LHasUnion: Boolean;
  LCursor: TCursor;
begin
  Result := False;
  LHasUnion := False;
  if ACursor.Kind = TCursorKind.FieldDecl then
  begin
    LCount := 0;
    ACursor.VisitChildren(
      function(const ACursor, AParent: TCursor): TChildVisitResult
      begin
        if ACursor.Kind = TCursorKind.UnionDecl then
        begin
          LCursor := ACursor;
          LHasUnion := True;
        end;
        Inc(LCount);
        Result := TChildVisitResult.Continue;
      end
    );
  end;
  if (LCount = 1) and LHasUnion then
  begin
    AUnionCursor := LCursor;
    Result := True;
  end;
end;

procedure TCustomTranslator.WriteStructTypeFieldElaborate(const ACursor: TCursor; const AIndex: Integer);
var
  LUnionCursor: TCursor;
begin
  // Elaborate will be different if first child type is a union
  if FindFieldUnion(ACursor, LUnionCursor) then
    WriteStructTypeUnion(LUnionCursor)
  else
    WriteStructTypeFieldStruct(ACursor);
end;

procedure TCustomTranslator.WriteStructTypeFieldStruct(const ACursor: TCursor);
var
  LIndex: Integer;
begin
  LIndex := 0;
  ACursor.CursorType.VisitFields(
    function(const ACursor: TCursor): TVisitorResult
    begin
      if LIndex > 0 then
        FWriter.WriteLn(';');
      WriteStructTypeField(ACursor, LIndex);
      Inc(LIndex);
      Result := TVisitorResult.Continue;
    end
  );
  // Unions/union fields will already have a terminating semicolon
  if not FHasClosing and not FWriter.NeedIndent then // and (ACursor.Kind <> TCursorKind.UnionDecl) then
    FWriter.Write(';');
end;

procedure TCustomTranslator.WriteStructTypeField(const ACursor: TCursor; const AIndex: Integer);
var
  LPointeeType: TType;
  LFieldName: string;
  LSpelling: string;
  LTypeKind: TTypeKind;
  LCursorKind: TCursorKind;
  LIsElaborate: Boolean;
begin
  LCursorKind := ACursor.Kind;
  LTypeKind := ACursor.CursorType.Kind;
  LSpelling := ACursor.Spelling;
  LFieldName := GetValidIdentifier(ACursor.Spelling);
  if (LFieldName = '') then
    LFieldName := 'f' + (AIndex + 1).ToString;
  LIsElaborate := LTypeKind in [TTypeKind.Rec, TTypeKind.Elaborated];
  if not LIsElaborate then
  begin
    FWriter.Write(LFieldName);
    FWriter.Write(': ');
  end;
  if LIsElaborate then
    WriteStructTypeFieldElaborate(ACursor, AIndex)
  else if LCursorKind = TCursorKind.UnionDecl then
    WriteStructTypeUnion(ACursor)
  else if (IsProceduralType(ACursor.CursorType, LPointeeType)) then
    WriteFunctionProto(ACursor, LPointeeType, '', True)
  else
    FWriter.Write(GetDelphiTypeName(ACursor.CursorType));
end;

procedure TCustomTranslator.WriteStructTypeUnion(const ACursor: TCursor);
var
  LIndex: Integer;
begin
  LIndex := 0;
  FWriter.WriteLn('case Integer of');
  FWriter.Indent;
  ACursor.CursorType.VisitFields(
    function(const AFieldCursor: TCursor): TVisitorResult
    begin
      FWriter.WriteLn('%d:', [LIndex]);
      FWriter.Indent;
      FWriter.Write('(');
      FHasClosing := True;
      WriteStructTypeField(AFieldCursor, LIndex);
      FHasClosing := False;
      FWriter.WriteLn(');');
      FWriter.Outdent;
      Inc(LIndex);
      Result := TVisitorResult.Continue;
    end
  );
  FWriter.Outdent;
end;

procedure TCustomTranslator.WriteStructType(const ACursor: TCursor; const AIsUnion: Boolean);
var
  LCursorType: TType;
  LStructName: string;
  LIsAnonymous: Boolean;
begin
  LCursorType := ACursor.CursorType;
  LStructName := GetDelphiTypeName(LCursorType, False, @LIsAnonymous);
  if not FWriter.IsAtSectionStart then
    FWriter.WriteLn;
  FWriter.WriteLn('%s = record', [LStructName]);
  FWriter.Indent;
  if AIsUnion then
    WriteStructTypeUnion(ACursor)
  else
  begin
    WriteStructTypeFieldStruct(ACursor);
    // If not already on a new line, finish the current one
    if not FWriter.NeedIndent then
      FWriter.WriteLn('');
  end;
  FWriter.Outdent;
  FWriter.WriteLn('end;');
  FWriter.WriteLn;
end;

procedure TCustomTranslator.WriteToDo(const AText: string);
begin
  if (FProject.UnconvertibleHandling = TUnconvertibleHandling.WriteToDo) and CanWriteToDo then
    FWriter.WriteLn('{ TODO : %s }', [AText]);
end;

procedure TCustomTranslator.WriteType(const ACursor: TCursor);
begin
  if not FSymbolsToIgnore.ContainsKey(ACursor.Spelling) then
  begin
    case ACursor.Kind of
      TCursorKind.StructDecl:
        WriteStructType(ACursor, False);

      TCursorKind.UnionDecl:
        WriteStructType(ACursor, True);

      TCursorKind.TypedefDecl:
        WriteTypedefType(ACursor);

      TCursorKind.EnumDecl:
        WriteEnumType(ACursor);
    else
      HandleWriteType(ACursor);
    end;
  end;
end;

function TCustomTranslator.HandleTypeDeclaration(const ACursor: TCursor): Boolean;
begin
  // Override in descendant
  Result := False;
end;

function TCustomTranslator.HandleTypeDefinition(const ACursor: TCursor): Boolean;
begin
  // Override in descendant
  Result := False;
end;

procedure TCustomTranslator.HandleWriteType(const ACursor: TCursor);
begin
  // Override in descendant
end;

procedure TCustomTranslator.WriteTypedefType(const ACursor: TCursor);
var
  SrcName, DstName, LValidIdent: string;
  T, Pointee, CanonType: TType;
  LEnumConstCursor: TCursor;
begin
  DstName := ACursor.Spelling;
  T := ACursor.TypedefDeclUnderlyingType;
  SrcName := GetDelphiTypeName(T);

  { There are three ways to create procedural types. The most common one:

      typedef int (*SomeProc)(int SomeParam);

    This gets handled by IsProcedural type below. There is also a variation
    without a '*':

      typedef int (SomeProc)(int SomeParam);

    In this case, libclang returns this as an "unexposed" type and its
    canonical type will be a function proto. But it can later only be
    referenced as *SomeProc, so we need to prefix with a 'P'.

    Finally there is this version without parenthesis around the name:

      typedef int SomeProc(int SomeParam);

    In this case, the type is Function(No)Proto, and again can only be used as
    a reference. }
  if (T.Kind = TTypeKind.Unexposed) then
  begin
    T := T.CanonicalType;
    if (T.Kind in [TTypeKind.FunctionNoProto, TTypeKind.FunctionProto]) then
    begin
      WriteProceduralType(ACursor, T, 'P');
      Exit;
    end;
  end
  else if (T.Kind in [TTypeKind.FunctionNoProto, TTypeKind.FunctionProto]) then
  begin
    WriteProceduralType(ACursor, T, 'P');
    Exit;
  end;

  if (IsProceduralType(T, Pointee)) then
  begin
    { This is a procedural type }
    WriteProceduralType(ACursor, Pointee);
    WriteIndirections(ACursor.Spelling);
    Exit;
  end;

  { Check for "opaque" types, as in:
      typedef struct _Foo Foo;
    Here, Foo is never used as-is, but only as "*Foo".
    Seems like we can detect these by checking IsPodType }
  CanonType := T.CanonicalType;
  if (CanonType.Kind = TTypeKind.Rec) and (not CanonType.IsPodType) then
  begin
    FWriter.WriteLn('P%s = Pointer;', [DstName]);
    FWriter.WriteLn('PP%s = ^P%0:s;', [DstName]);
    Exit;
  end;

  { Another common declaration for an opaque type is:
      typedef struct _Foo *Foo
    Here Foo itself is a pointer type, and the pointee again is not a POD type. }
  if (CanonType.Kind = TTypeKind.Pointer) then
  begin
    Pointee := CanonType.PointeeType;
    if (Pointee.Kind = TTypeKind.Rec) and (not Pointee.IsPodType) then
    begin
      FWriter.WriteLn('%s = Pointer;', [DstName]);
      FWriter.WriteLn('P%s = ^%0:s;', [DstName]);
      Exit;
    end;
  end;

  if (CanonType.Kind = TTypeKind.Enum) and FEnumConsts.TryGetValue(DstName, LEnumConstCursor) then
  begin
    FWriter.WriteLn('%s = %s;', [DstName, GetDelphiTypeName(LEnumConstCursor.EnumDeclIntegerType)]);
    Exit;
  end;

  { Check for typedefs to "void", as in:
      typedef void Foo;
    These only make sense when user later as a pointer reference (*Foo) }
  if (T.Kind = TTypeKind.Void) then
  begin
    FWriter.WriteLn('P%s = Pointer;', [DstName]);
    FWriter.WriteLn('PP%s = ^P%0:s;', [DstName]);
    Exit;
  end;

  (*Do NOT convert typedef if the name of the typedef is the same as name
    of the structure, eg.:
      typedef struct Foo {
        ..
      } Foo;
  *)
  if DstName = SrcName then
    Exit;

  // https://github.com/Embarcadero/octoid/issues/21
  LValidIdent := GetValidIdentifier(DstName);
  if not SameText(LValidIdent, SrcName) then
    FWriter.WriteLn('%s = %s;', [LValidIdent, SrcName]);
  WriteIndirections(DstName, SrcName);
end;

procedure TCustomTranslator.WriteTypes;
var
  Cursor: TCursor;
begin
  DoMessage('Writing data types...');

// DPN - not doing it this way
//  if (FProject.EnumHandling = TEnumHandling.ConvertToConst) then
//    { This mixes type- and const-sections, so must be handled separately. }
//    WriteEnumTypes;

  FWriter.StartSection('type');

  { Forward pointer-to-record declarations }
  WriteForwardTypeDeclarations;

  for Cursor in FTypes do
    WriteType(Cursor);

  WriteClasses;

  FWriter.EndSection;
end;

procedure TCustomTranslator.WriteInterfaceUsesClause;
begin
  //
end;

procedure TCustomTranslator.WriteDelphiSource;
begin
  FUnitName := TPath.GetFileNameWithoutExtension(FProject.TargetPasFile);
  FWriter := TSourceWriter.Create(FProject.TargetPasFile);
  try
    if FConfig.BannerPosition = TBannerPosition.BeforeUnit then
    begin
      WriteCopyrightHeader;
      FWriter.WriteLn;
    end;
    FWriter.WriteLn('unit %s;', [FUnitName]);
    if FConfig.BannerPosition = TBannerPosition.BeforeInterface then
    begin
      FWriter.WriteLn;
      WriteCopyrightHeader;
    end;
    FWriter.WriteLn;
    FWriter.WriteLn('interface');
    FWriter.WriteLn;
    WriteInterfaceUsesClause;
    WriteConstants;
    WriteTypes;
    WriteFunctions;
    FWriter.WriteLn('implementation');
    WriteImplementationUsesClause;
    WriteImplementationContent;
    FWriter.WriteLn;
    FWriter.Write('end.');
  finally
    FWriter.Free;
  end;
end;

function TCustomTranslator.PerformTranslation: Boolean;
begin
  FTranslationUnit := Translate;
  Result := FTranslationUnit <> nil;
end;

function TCustomTranslator.Translate: ITranslationUnit;
begin
  Result := nil;
end;

function TCustomTranslator.Run: Boolean;
begin
  Result := False;
  Prepare;
  if PerformTranslation then
  begin
    AnalyzeTypes;
    WriteDelphiSource;
    DoMessage('Header conversion completed!');
    Result := True; // Might need to check for errors :-P
  end
  else
    DoMessage('**** Header conversion failed ***** - Please refer to the reported fatal errors');
end;

end.
