unit Octoid.AstDumper;

interface

uses
  System.Classes, System.Generics.Collections, System.Hash, System.IOUtils,
  Neslib.Clang,
  Octoid.CustomTranslator, Octoid.ObjCHeaderTranslator, Octoid.SourceReader;

type
  TAstDumper = class(TCustomTranslator)
  private
    FCombinedHeaderFileName: string;
    FIncludeSystemDecls: Boolean;
    FProject: TObjCHeaderProject;
    FSourceReader: TSourceReader;
    FTypes: TDictionary<TType, Integer>;
    FWriter: TStreamWriter;
    function CanInclude(const ACursor: TCursor): Boolean;
    procedure CheckUnexposedAttr(const ACursor: TCursor);
    function CreateCombinedHeaderFile: Boolean;
    function GetFileSuffix: string;
    function GetHeaderDirectory: string;
    procedure PerformDiagnostics;
    function TypeIndex(const AType: TType): Integer;
    procedure WriteAst;
    procedure WriteCursor(const ACursor: TCursor; const AIndent: String);
    procedure WriteType(const AType: TType; const AIndex: Integer);
    procedure WriteTypes;
  protected
    function Translate: ITranslationUnit; override;
  public
    constructor Create(const AProject: TCustomTranslatorProject); override;
    destructor Destroy; override;
    function Run: Boolean; override;
  end;

implementation

uses
  System.Generics.Defaults, System.SysUtils,
  Neslib.Clang.Api;

const
  cCombinedHeaderFileName = '_combinedheaders_.h';

function TemplateArgumentKindToString(const AKind: TTemplateArgumentKind): String;
begin
  case AKind of
    TTemplateArgumentKind.Null: Result := 'Null';
    TTemplateArgumentKind.Typ: Result := 'Typ';
    TTemplateArgumentKind.Declaration: Result := 'Declaration';
    TTemplateArgumentKind.NullPtr: Result := 'NullPtr';
    TTemplateArgumentKind.Integral: Result := 'Integral';
    TTemplateArgumentKind.Template: Result := 'Template';
    TTemplateArgumentKind.TemplateExpansion: Result := 'TemplateExpansion';
    TTemplateArgumentKind.Expression: Result := 'Expression';
    TTemplateArgumentKind.Pack: Result := 'Pack';
  else
    Result := 'Invalid';
  end;
end;

function CxxAccessSpecifierToString(const AAccess: TCxxAccessSpecifier): String;
begin
  case AAccess of
    TCxxAccessSpecifier.Public: Result := 'Public';
    TCxxAccessSpecifier.Protected: Result := 'Protected';
    TCxxAccessSpecifier.Private: Result := 'Private';
  else
    Result := 'Invalid';
  end;
end;

function CallingConvToString(const AConv: TCallingConv): String;
begin
  case AConv of
    TCallingConv.Default: Result := 'Default';
    TCallingConv.C: Result := 'C';
    TCallingConv.X86StdCall: Result := 'X86StdCall';
    TCallingConv.X86FastCall: Result := 'X86FastCall';
    TCallingConv.X86ThisCall: Result := 'X86ThisCall';
    TCallingConv.X86Pascal: Result := 'X86Pascal';
    TCallingConv.AAPCS: Result := 'AAPCS';
    TCallingConv.AAPCS_VFP: Result := 'AAPCS_VFP';
    TCallingConv.X86RegCall: Result := 'X86RegCall';
    TCallingConv.IntelOclBicc: Result := 'IntelOclBicc';
    TCallingConv.Win64: Result := 'Win64';
//    TCallingConv.X86_64Win64: Result := 'X86_64Win64';
    TCallingConv.X86_64SysV: Result := 'X86_64SysV';
    TCallingConv.X86VectorCall: Result := 'X86VectorCall';
    TCallingConv.Swift: Result := 'Swift';
    TCallingConv.PreserveMost: Result := 'PreserveMost';
    TCallingConv.PreserveAll: Result := 'PreserveAll';
    TCallingConv.Unexposed: Result := 'Unexposed';
  else
    Result := 'Invalid';
  end;
end;

{ TAstDumper }

constructor TAstDumper.Create(const AProject: TCustomTranslatorProject);
begin
  inherited; // Create(AProject as TCustomTranslatorProject);
  FProject := TObjCHeaderProject(AProject);
  FSourceReader := TSourceReader.Create;
  FTypes := TDictionary<TType, Integer>.Create(TEqualityComparer<TType>.Construct(
    function(const ALeft, ARight: TType): Boolean
    begin
      Result := (ALeft = ARight);
    end,

    function(const AValue: TType): Integer
    var
      Src, Dst: TCXType;
    begin
      Src := AValue.Handle;
      PInt64(@Dst.kind)^ := 0;
      Dst.kind := Src.kind;
      Dst.data[0] := Src.data[0];
      Dst.data[1] := Src.data[1];
      Result := THashBobJenkins.GetHashValue(Dst, SizeOf(Dst));
    end));
end;

destructor TAstDumper.Destroy;
begin
  FSourceReader.Free;
  FTypes.Free;
  inherited;
end;

// TODO: Refactor this into TObjCHeaderProject
function TAstDumper.CreateCombinedHeaderFile: Boolean;
var
  Option: TSearchOption;
  Writer: TStreamWriter;
  HeaderFiles: TArray<string>;
  HeaderPath, HeaderFile: string;
  I: Integer;
begin
  if FProject.IncludeSubdirectories then
    Option := TSearchOption.soAllDirectories
  else
    Option := TSearchOption.soTopDirectoryOnly;
  HeaderPath := IncludeTrailingPathDelimiter(FProject.FrameworkDirectory);
  HeaderFiles := TDirectory.GetFiles(HeaderPath, '*.h', Option);
  if Length(HeaderFiles) = 0 then
  begin
    DoMessage('No header files found in directory "%s".', [HeaderPath]);
    Exit(False); // <======
  end;
  for I := 0 to Length(HeaderFiles) - 1 do
  begin
    HeaderFile := HeaderFiles[I];
    // "Umbrella" frameworks have a header file with the same name as the framework, with an .h extension
    if not FProject.FrameworkName.Equals(TPath.GetFileNameWithoutExtension(TPath.GetFileName(HeaderFile))) then
      HeaderFile := string.Empty
    else
      Break;
  end;
  if HeaderFile.IsEmpty or FProject.IgnoreUmbrellaHeader then
  begin
    FCombinedHeaderFileName := TPath.Combine(TPath.GetTempPath, cCombinedHeaderFileName);
    Writer := TStreamWriter.Create(FCombinedHeaderFileName);
    try
      for I := 0 to Length(HeaderFiles) - 1 do
      begin
        HeaderFile := HeaderFiles[I];
        if HeaderFile.StartsWith(HeaderPath, True) then
          HeaderFile := HeaderFile.Substring(HeaderPath.Length);
        Writer.WriteLine('#include "%s"', [HeaderFile]);
      end;
    finally
      Writer.Free;
    end;
  end
  else
    FCombinedHeaderFileName := HeaderFile;
  Result := True;
end;

function TAstDumper.GetFileSuffix: string;
begin
  Result := Format('%s.%s.', [TPath.GetFileName(FProject.SdkRoot), FProject.FrameworkName]);
end;

function TAstDumper.GetHeaderDirectory: string;
begin
  Result := FProject.FrameworkDirectory;
end;

procedure TAstDumper.PerformDiagnostics;
var
  DiagOpts: TDiagnosticDisplayOptions;
  Diag: IDiagnostic;
  I, ErrorCount: Integer;
begin
  DiagOpts := GetDefaultDiagnosticDisplayOptions;
  ErrorCount := 0;
  for I := 0 to TranslationUnit.DiagnosticCount - 1 do
  begin
    Diag := TranslationUnit.Diagnostics[I];
    if (Diag.Severity >= TDiagnosticSeverity.Warning) then // .Error) then
    begin
      DoMessage(Diag.Format(DiagOpts));
      Inc(ErrorCount);
    end;
  end;
  if (ErrorCount = 0) then
    DoMessage('Parsed header files without errors')
  else
    DoMessage(Format('Parsed header files with %d error(s)', [ErrorCount]));
end;

function TAstDumper.Translate: ITranslationUnit;
var
  Args: TArray<string>;
  LPath: string;
begin
  Result := nil;
  if CreateCombinedHeaderFile then
  begin
    DoMessage('Parsing header files...');
    Args := FProject.CmdLineArgs;
    Args := Args + ['-I' + FProject.FrameworkDirectory];
    for LPath in TDirectory.GetDirectories(FProject.FrameworkDirectory, '*', TSearchOption.soAllDirectories) do
      Args := Args + ['-I' + LPath];
    Result := TranslationIndex.ParseTranslationUnit(FCombinedHeaderFileName, Args, [], [TTranslationUnitFlag.DetailedPreprocessingRecord,
      TTranslationUnitFlag.SkipFunctionBodies, TTranslationUnitFlag.KeepGoing]);
  end;
end;

function TAstDumper.Run: Boolean;
begin
  Result := False;
  if PerformTranslation then
  begin
    PerformDiagnostics;
    WriteAst;
    WriteTypes;
    DoMessage('Done!');
    Result := True;
  end;
end;

function TAstDumper.TypeIndex(const AType: TType): Integer;

  procedure Check(const AType: TType);
  begin
    if (AType.Kind <> TTypeKind.Invalid) then
      TypeIndex(AType);
  end;

var
  I: Integer;
begin
  if (not FTypes.TryGetValue(AType, Result)) then
  begin
    Result := FTypes.Count;
    FTypes.Add(AType, Result);

    Check(AType.CanonicalType);
    Check(AType.PointeeType);
    Check(AType.ResultType);

    for I := 0 to AType.ArgTypeCount - 1 do
      TypeIndex(AType.ArgTypes[I]);

    Check(AType.ElementType);
    Check(AType.ArrayElementType);
    Check(AType.NamedType);
    Check(AType.ClassType);

    for I := 0 to AType.TemplateArgumentCount - 1 do
      TypeIndex(AType.TemplateArgumentTypes[I]);

    AType.VisitFields(
      function(const ACursor: TCursor): TVisitorResult
      var
        T: TType;
      begin
        T := ACursor.CursorType;
        if (T.Kind <> TTypeKind.Invalid) then
          TypeIndex(T);
        Result := TVisitorResult.Continue;
      end);
  end;
end;

procedure TAstDumper.WriteAst;
begin
  DoMessage('Writing AST..');
  FWriter := TStreamWriter.Create(TPath.Combine(FProject.OutputPath, GetFileSuffix + 'Ast.txt'));
  try
    WriteCursor(TranslationUnit.Cursor, '');
  finally
    FWriter.Free;
  end;
end;

function TAstDumper.CanInclude(const ACursor: TCursor): Boolean;
var
  LFile: Neslib.Clang.TFile;
  LLine, LColumn, LOffset: Integer;
  LFileName: string;
begin
  ACursor.Location.GetFileLocation(LFile, LLine, LColumn, LOffset);
  LFileName := StringReplace(LFile.Filename, '/', '\', [rfReplaceAll]);
  Result := not ACursor.IsPreprocessing and (LFileName.IsEmpty or LFilename.StartsWith(GetHeaderDirectory));
end;

procedure TAstDumper.CheckUnexposedAttr(const ACursor: TCursor);
var
  LLineStart, LLineEnd, LColStart, LColEnd, LOffsetStart, LOffsetEnd: Integer;
  LFile: TFile;
  LLine: string;
begin
  ACursor.Extent.First.GetFileLocation(LFile, LLineStart, LColStart, LOffsetStart);
  ACursor.Extent.Last.GetFileLocation(LFile, LLineEnd, LColEnd, LOffsetEnd);
  // Not doing multiple lines
  if LLineStart <> LLineEnd then
    Exit; // <======
  LLine := FSourceReader.ReadSubstring(LFile.Filename, LLineStart, LColStart, LColEnd - 1);
  if LLine.StartsWith('API_DEPRECATED') then
    FWriter.Write(Format(' [%s]', [LLine.Trim]));
end;

procedure TAstDumper.WriteCursor(const ACursor: TCursor; const AIndent: String);
var
  S, Flags, LFileName: String;
  T: TType;
  F: TFile;
  I, Count, Line, Column, Offset: Integer;
  Val: Int64;
  Range: TSourceRange;
  Tokens: ITokenList;
  IsSystemDecl: Boolean;

  procedure AddFlag(const AFlag: String; const AEnable: Boolean);
  begin
    if (AEnable) then
    begin
      if (Flags <> '') then
        Flags := Flags + ', ';
      Flags := Flags + AFlag;
    end;
  end;

  procedure CheckType(const AType: TType; const AKind: String);
  begin
    if (AType.Kind <> TTypeKind.Invalid) then
    begin
      FWriter.Write(' ');
      FWriter.Write(AKind);
      FWriter.Write(': T');
      FWriter.Write(TypeIndex(AType));
    end;
  end;

begin
  IsSystemDecl := ACursor.Location.IsInSystemHeader;
  if not CanInclude(ACursor) or (IsSystemDecl and (not FIncludeSystemDecls)) then
    Exit;

  if (ACursor.Kind = TCursorKind.MacroExpansion) then
    Exit;

  FWriter.Write(AIndent);

  // Reference, e.g. in ObjC it might be:
  //   CLLocation: ObjCInterfaceDecl         or
  //     NSObject: ObjCSuperClassRef
  S := ACursor.Spelling;
  if (S = '') then
    S := '(unnamed)';
  FWriter.Write(S);
  FWriter.Write(': ');
  FWriter.Write(ACursor.Kind.Spelling);

  T := ACursor.CursorType;
  if (T.Kind <> TTypeKind.Invalid) then
  begin
    FWriter.Write('(T');
    FWriter.Write(TypeIndex(T));
    FWriter.Write(')');
  end;

  CheckType(ACursor.TypedefDeclUnderlyingType, 'UnderlyingType');
  CheckType(ACursor.EnumDeclIntegerType, 'EnumValueType');
  CheckType(ACursor.ResultType, 'ResultType');

  Flags := '';
  AddFlag('IsNull', ACursor.IsNull);
  AddFlag('IsDeclaration', ACursor.IsDeclaration);
  AddFlag('IsDefinition', ACursor.IsDefinition);
  AddFlag('IsReference', ACursor.IsReference);
  AddFlag('IsExpression', ACursor.IsExpression);
  AddFlag('IsStatement', ACursor.IsStatement);
  AddFlag('IsAttribute', ACursor.IsAttribute);
  AddFlag('IsInvalid', ACursor.IsInvalid);
  AddFlag('IsTranslationUnit', ACursor.IsTranslationUnit);
  AddFlag('IsPreprocessing', ACursor.IsPreprocessing);
  AddFlag('IsUnexposed', ACursor.IsUnexposed);
  AddFlag('IsMacroFunctionLike', ACursor.IsMacroFunctionLike);
  AddFlag('IsMacroBuiltin', ACursor.IsMacroBuiltin);
  AddFlag('IsFunctionInlined', ACursor.IsFunctionInlined);
  AddFlag('IsAnonymous', ACursor.IsAnonymous);
  AddFlag('IsBitField', ACursor.IsBitField);
  AddFlag('IsVirtualBase', ACursor.IsVirtualBase);
  AddFlag('IsDynamicCall', ACursor.IsDynamicCall);
  AddFlag('IsVariadic', ACursor.IsVariadic);
  AddFlag('CxxConstructorIsConvertingConstructor', ACursor.CxxConstructorIsConvertingConstructor);
  AddFlag('CxxConstructorIsCopyConstructor', ACursor.CxxConstructorIsCopyConstructor);
  AddFlag('CxxConstructorIsDefaultConstructor', ACursor.CxxConstructorIsDefaultConstructor);
  AddFlag('CxxConstructorIsMoveConstructor', ACursor.CxxConstructorIsMoveConstructor);
  AddFlag('CxxFieldIsMutable', ACursor.CxxFieldIsMutable);
  AddFlag('CxxMethodIsDefaulted', ACursor.CxxMethodIsDefaulted);
  AddFlag('CxxMethodIsPureVirtual', ACursor.CxxMethodIsPureVirtual);
  AddFlag('CxxMethodIsStatic', ACursor.CxxMethodIsStatic);
  AddFlag('CxxMethodIsVirtual', ACursor.CxxMethodIsVirtual);
  AddFlag('CxxRecordIsAbstract', ACursor.CxxRecordIsAbstract);
  AddFlag('EnumDeclIsScoped', ACursor.EnumDeclIsScoped);
  AddFlag('CxxMethodIsConst', ACursor.CxxMethodIsConst);
  AddFlag('HasAttributes', ACursor.HasAttributes);
  AddFlag('InSystemHeader', IsSystemDecl);
  if (Flags <> '') then
  begin
    FWriter.Write(' [');
    FWriter.Write(Flags);
    FWriter.Write(']');
  end;

  Val := ACursor.EnumConstantDeclValue;
  if (Val <> 0) and (Val <> Int64.MinValue) then
  begin
    FWriter.Write(' EnumValue: ');
    FWriter.Write(Val);
  end;

  I := ACursor.FieldDeclBitWidth;
  if (I >= 0) then
  begin
    FWriter.Write(' FieldBitWidth: ');
    FWriter.Write(I);
  end;

  Val := ACursor.OffsetOfField;
  if (Val <> -1) then
  begin
    FWriter.Write(' OffsetOfField: ');
    FWriter.Write(Val);
  end;

  if (ACursor.CxxAccessSpecifier <> TCxxAccessSpecifier.Invalid) then
  begin
    FWriter.Write(' Access: ');
    FWriter.Write(CxxAccessSpecifierToString(ACursor.CxxAccessSpecifier));
  end;

  I := ACursor.OverloadedDeclCount;
  if (I > 0) then
  begin
    FWriter.Write(' OverloadedCount: ');
    FWriter.Write(I);
  end;

  I := ACursor.ArgumentCount;
  if (I >= 0) then
  begin
    FWriter.Write(' ArgCount: ');
    FWriter.Write(I);
  end;

  if (ACursor.TemplateCursorKind <> TCursorKind.NoDeclFound) then
  begin
    FWriter.Write(' TemplateCursorKind: ');
    FWriter.Write(ACursor.TemplateCursorKind.Spelling);
  end;

  Count := ACursor.TemplateArgumentCount;
  if (Count > 0) then
  begin
    FWriter.Write(' TemplateArgs: [');
    for I := 0 to Count - 1 do
    begin
      FWriter.Write('{');
      FWriter.Write(TemplateArgumentKindToString(ACursor.TemplateArgumentKind[I]));

      T := ACursor.TemplateArgumentType[I];
      if (T.Kind <> TTypeKind.Invalid) then
      begin
        FWriter.Write(' T');
        FWriter.Write(TypeIndex(T));
      end;

      FWriter.Write(' ');
      FWriter.Write(ACursor.TemplateArgumentValue[I]);

      FWriter.Write('}');
      if (I < (Count - 1)) then
        FWriter.Write(', ');
    end;
    FWriter.Write(']');
  end;

  ACursor.Location.GetFileLocation(F, Line, Column, Offset);
  if (not F.IsNull) then
  begin
    LFileName := Copy(F.Filename, Length(GetHeaderDirectory) + 1, Length(F.Filename));
    FWriter.Write(' @%s (%d:%d)', [LFilename, Line, Column]);
  end;

  if (ACursor.Kind = TCursorKind.MacroDefinition) then
  begin
    Range := ACursor.Extent;
    if (not Range.IsNull) then
    begin
      FWriter.Write(' #');
      Tokens := TranslationUnit.Tokenize(Range);
      for I := 0 to Tokens.Count - 1 do
      begin
        FWriter.Write(TranslationUnit.GetTokenSpelling(Tokens[I]));
        FWriter.Write(' ');
      end;
    end;
  end;

  //!!!!! DPN
  if ACursor.Kind = TCursorKind.UnexposedAttr then
    CheckUnexposedAttr(ACursor);

  FWriter.WriteLine;

  S := AIndent + '  ';
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      WriteCursor(ACursor, S);
      Result := TChildVisitResult.Continue;
    end);
end;

procedure TAstDumper.WriteType(const AType: TType; const AIndex: Integer);
var
  S, Flags: String;
  I, J, Count: Integer;

  procedure AddFlag(const AFlag: String; const AEnable: Boolean);
  begin
    if (AEnable) then
    begin
      if (Flags <> '') then
        Flags := Flags + ', ';
      Flags := Flags + AFlag;
    end;
  end;

  procedure Check(const ASubType: TType; const AKind: String);
  var
    I: Integer;
  begin
    if (ASubType <> AType) and FTypes.TryGetValue(ASubType, I) then
    begin
      FWriter.Write(' ');
      FWriter.Write(AKind);
      FWriter.Write(': T');
      FWriter.Write(I);
    end;
  end;

begin
  FWriter.Write('T');
  FWriter.Write(AIndex);
  FWriter.Write(': ');
  FWriter.Write(AType.Spelling);
  FWriter.Write(' (');
  FWriter.Write(AType.KindSpelling);
  FWriter.Write(')');

  Flags := '';
  AddFlag('IsConstQualified', AType.IsConstQualified);
  AddFlag('IsVolatileQualified', AType.IsVolatileQualified);
  AddFlag('IsRestrictQualified', AType.IsRestrictQualified);
  AddFlag('IsFunctionVariadic', AType.IsFunctionVariadic);
  AddFlag('IsPodType', AType.IsPodType);
  AddFlag('IsTransparentTagTypedef', AType.IsTransparentTagTypedef);
  if (Flags <> '') then
  begin
    FWriter.Write(' [');
    FWriter.Write(Flags);
    FWriter.Write(']');
  end;

  Check(AType.CanonicalType, 'Canonical');
  Check(AType.PointeeType, 'Pointee');
  Check(AType.ResultType, 'Result');
  Check(AType.ElementType, 'Element');
  Check(AType.ArrayElementType, 'ArrayElement');
  Check(AType.NamedType, 'Named');
  Check(AType.ClassType, 'Class');

  S := AType.TypedefName;
  if (S <> '') then
  begin
    FWriter.Write(' TypedefName: ');
    FWriter.Write(S);
  end;

  if (AType.FunctionCallingConv <> TCallingConv.Invalid) then
  begin
    FWriter.Write(' CallConv: ');
    FWriter.Write(CallingConvToString(AType.FunctionCallingConv));
  end;

  if (AType.ElementCount > 0) then
  begin
    FWriter.Write(' ElementCount: ');
    FWriter.Write(AType.ElementCount);
  end;

  if (AType.ArraySize > 0) then
  begin
    FWriter.Write(' ArraySize: ');
    FWriter.Write(AType.ArraySize);
  end;

  if (AType.AlignOf <> TYPE_LAYOUT_ERROR_INVALID) then
  begin
    FWriter.Write(' AlignOf: ');
    FWriter.Write(AType.AlignOf);
  end;

  if (AType.SizeOf <> TYPE_LAYOUT_ERROR_INVALID) then
  begin
    FWriter.Write(' SizeOf: ');
    FWriter.Write(AType.SizeOf);
  end;

  Count := AType.ArgTypeCount;
  if (Count > 0) then
  begin
    FWriter.Write(' ArgTypes: [');
    for I := 0 to Count - 1 do
    begin
      if (not FTypes.TryGetValue(AType.ArgTypes[I], J)) then
        J := -1;
      FWriter.Write('T');
      FWriter.Write(J);
      if (I < (Count - 1)) then
        FWriter.Write(', ');
    end;
    FWriter.Write(']');
  end;

  Count := AType.TemplateArgumentCount;
  if (Count > 0) then
  begin
    FWriter.Write(' TemplateArgTypes: [');
    for I := 0 to Count - 1 do
    begin
      if (not FTypes.TryGetValue(AType.TemplateArgumentTypes[I], J)) then
        J := -1;
      FWriter.Write('T');
      FWriter.Write(J);
      if (I < (Count - 1)) then
        FWriter.Write(', ');
    end;
    FWriter.Write(']');
  end;

  case AType.CxxRefQualifier of
    TRefQualifierKind.LValue:
      FWriter.Write(' RefQualifier: LValue');

    TRefQualifierKind.RValue:
      FWriter.Write(' RefQualifier: RValue');
  end;

  FWriter.WriteLine;
end;

procedure TAstDumper.WriteTypes;
var
  Pairs: TArray<TPair<TType, Integer>>;
  I: Integer;
begin
  DoMessage('Writing types...');

  Pairs := FTypes.ToArray;
  TArray.Sort<TPair<TType, Integer>>(Pairs, TComparer<TPair<TType, Integer>>.Construct(
    function(const Left, Right: TPair<TType, Integer>): Integer
    begin
      Result := Left.Value - Right.Value;
    end));

  FWriter := TStreamWriter.Create(TPath.Combine(FProject.OutputPath, GetFileSuffix + 'Types.txt'));
  try
    for I := 0 to Length(Pairs) - 1 do
      WriteType(Pairs[I].Key, I);
  finally
    FWriter.Free;
  end;
end;

end.
