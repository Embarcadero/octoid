unit Octoid.ObjCHeaderTranslator;

// Based on this unit:
//   https://github.com/neslib/Chet/blob/master/Classes/Chet.HeaderTranslator.pas

// The author Erik van Bilsen has indicated that users are free to do whatever they like with his work as long as he is not held accountable :-)

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections, System.Generics.Defaults,
  Neslib.Clang,
  Octoid.CustomTranslator;

type
  TParamTypeNames = TStringDynArray;
  TCursorArray = TArray<TCursor>;

  TDelphiMethod = record
    DeclarationFormat: string;
    Deprecation: string;
    Index: Integer;
    IsAvailable: Boolean;
    IsMismatched: Boolean;
    IsOverload: Boolean;
    MethodName: string; // Delphi
    MethodNamePartsCount: Integer;
    ObjCMethodName: string; // Fully qualified
    ObjCMethodNameParts: TArray<string>;
    ParamTypeNames: TParamTypeNames; // Type names of parameters of the method
    ResultTypeName: string;
    function GetDeclaration: string;
    function Equals(const AMethod: TDelphiMethod): Boolean;
    procedure ExpandMethodName;
    procedure ExpandObjCMethodName;
    function IsNameMismatched: Boolean;
  end;

  TDelphiMethods = class(TList<TDelphiMethod>)
  private
    class var FComparer: IComparer<TDelphiMethod>;
  public
    function AddMethod(AMethod: TDelphiMethod): Integer;
    function MethodExists(const AIndex: Integer): Boolean;
    function ResolveCollisions(const AMethod: TDelphiMethod): Boolean;
    procedure ResolveMethods;
    procedure ResolveOverloads(const AIndex: Integer);
  end;

  TBlockMethod = record
    Declaration: string;  // e.g. TMTLDeviceCompletionHandler1 = procedure(Param1: NSUInteger) of object;
    ObjCClassName: string; // e.g. MTLDevice
    ObjCMethodName: string; // if the declaration applies to a method result type
    ObjCMethodNameParts: TArray<string>;
    TypeName: string; // e.g. TMTLDeviceBlockMethod1, so this can be used when the method declaration in the class is emitted
    procedure ExpandObjCMethodName;
  end;

  TBlockMethods = class(TList<TBlockMethod>)
  private
    class var FComparer: IComparer<TBlockMethod>;
  public
    function AddBlockMethod(const AClassName, ADeclaration: string; const AMethodName: string = ''): Integer;
    function ClassBlockMethodCount(const AClassName: string): Integer;
    function FindMethod(const AClassName, ADeclaration: string; out AMethod: TBlockMethod; const AMethodName: string = ''): Integer;
    procedure SortMethods;
  end;

  TObjCTranslateOption = (UnsupportedConstTypeComments, DeprecationComments, DeprecationCommentFirst, TodoComments);
  TObjCTranslateOptions = set of TObjCTranslateOption;
  TObjCTranslateOptionsHelper = record helper for TObjCTranslateOptions
    function WantDeprecationComments: Boolean;
    function WantDeprecationCommentFirst: Boolean;
    function WantDeprecationCommentSameLine: Boolean;
  end;

  TObjCHeaderProject = class(TCustomTranslatorProject)
  private
    FClangIncludePath: string;
    FFrameworkDirectory: string;
    FFrameworkRoot: string;
    FIgnoreUmbrellaHeader: Boolean;
    FIncludeSubdirectories: Boolean;
    FObjCTranslateOptions: TObjCTranslateOptions;
    FOutputPath: string;
    FSdkRoot: string;
    FTargetPlatform: string;
    function GetFrameworkName: string;
    function GetLibraryPath: string;
    procedure SetSdkRoot(const Value: string);
    procedure SetTargetPlatform(const Value: string);
  protected
    function GetLibraryConstant: string; override;
    function GetLibraryConstantDeclaration: string; override;
    property FrameworkRoot: string read FFrameworkRoot;
  public
    property ClangIncludePath: string read FClangIncludePath write FClangIncludePath;
    property FrameworkName: string read GetFrameworkName;
    property FrameworkDirectory: string read FFrameworkDirectory write FFrameworkDirectory;
    property IgnoreUmbrellaHeader: Boolean read FIgnoreUmbrellaHeader write FIgnoreUmbrellaHeader;
    property IncludeSubdirectories: Boolean read FIncludeSubdirectories write FIncludeSubdirectories;
    property OutputPath: string read FOutputPath write FOutputPath;
    property ObjCTranslateOptions: TObjCTranslateOptions read FObjCTranslateOptions write FObjCTranslateOptions;
    property SdkRoot: string read FSdkRoot write SetSdkRoot;
    property TargetPlatform: string read FTargetPlatform write SetTargetPlatform;
  end;

  TObjCHeaderTranslator = class(TCustomTranslator)
  private
    const cExportedConstTypeNames: array[0..3] of string = ('NSString', 'Integer', 'Pointer', 'Double');
  private
    FBlockMethods: TBlockMethods;
    FCategories: TList<TCursor>;
    FClasses: TList<TCursor>;
    FCombinedHeaderFileName: string;
    FExportedConsts: TList<TCursor>;
    FIncludedFrameworks: TStrings;
    FInterfaceUnits: TStrings;
    FMethods: TDelphiMethods;
    FNeedsMacapiHelpers: Boolean;
    FProject: TObjCHeaderProject;
    procedure CheckTypeMaps(const ACursor: TCursor);
    procedure CheckTypeUnitMap(const ATypeName: string);
    function CreateCombinedHeaderFile: Boolean;
    procedure DiscoverBlockMethods(const ACursor: TCursor; const AClassName: string = '');
    procedure DiscoverBlockMethodParams(const ACursor: TCursor; const AClassName: string);
    function GetBlockMethodDeclaration(const ACursor: TCursor; const AClassName: string): string;
    function GetBlockMethodTypeDeclaration(const AType: TType): string;
    function GetCategoryClassName(const ACursor: TCursor): string;
    function GetClassCategories(const AClassName: string): TCursorArray;
    function GetDelphiMethod(const ACursor: TCursor; const AClassName: string; const AIsClassMethod: Boolean; const AIsClass: Boolean): TDelphiMethod;
    procedure GetDelphiMethodChildren(const ACursor: TCursor; const ABuilder: TStringBuilder; const AClassName: string; const AIsClass: Boolean;
      var AMethod: TDelphiMethod);
    function GetIsAvailable(const ASource: string): Boolean;
    // function HasObjCClassRef(const ACursor: TCursor): Boolean;
    function IsPlatformIOS: Boolean;
    function IsPlatformMacOS: Boolean;
    function IsSupportedDefinedConst(const ACursor: TCursor): Boolean;
    function IsSupportedExportedConst(const AType: TType): Boolean;
    procedure WriteDelphiMethods;
    procedure WriteExportedConstCGFloat(const AConstName: string);
    procedure WriteExportedConstFunctions;
    procedure WriteExportedConstProtos;
    procedure WriteInterfaceType(const ACursor: TCursor);
    function WriteInterfaceClassType(const ACursor: TCursor): Boolean;
    procedure WriteInterfaceDeclarationHeader(const AClassName, AClassRef: string; const AIsClass: Boolean);
    procedure WriteInterfaceInstanceType(const ACursor: TCursor);
  protected
    function CanIncludeCursor(const ACursor: TCursor): Boolean; override;
    function CanWriteToDo: Boolean; override;
    procedure DoPrepare; override;
    procedure DoSetupTypeMap; override;
    procedure DoSetupTypeUnitMap; override;
    function GenerateAnonymousTypeName(const AName: string): string; override;
    function GetBinaryOperatorTokens(const ASource: string): TArray<string>; override;
    function GetDelphiTypeName(AType: TType; const AInParamDecl: Boolean = False; const AOutIsAnonymous: PBoolean = nil): string; override;
    function GetUnexposedType(const AType: TType): string; override;
    function HandleTypeDeclaration(const ACursor: TCursor): Boolean; override;
    function HandleTypeDefinition(const ACursor: TCursor): Boolean; override;
    procedure HandleWriteType(const ACursor: TCursor); override;
    function IsAnonymous(const AName: string): Boolean; override;
    function IsConstIdentifier(const AName: string): Boolean; override;
    procedure ProcessErrors(const AErrors: TArray<string>);
    function Translate: ITranslationUnit; override;
    function RemoveQualifiers(const ATypeName: string): string; override;
    procedure SetupTokenMap; override;
    procedure WriteClasses; override;
    procedure WriteEnumTypeDecl(const ACursor: TCursor); override;
    procedure WriteForwardClasses; override;
    procedure WriteImplementationContent; override;
    procedure WriteImplementationUsesClause; override;
    procedure WriteInterfaceUsesClause; override;
  public
    constructor Create(const AProject: TCustomTranslatorProject); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, System.StrUtils, System.Character,
  Neslib.Clang.Api,
  Octoid.Consts;

type
  TParamTypeNamesHelper = record helper for TParamTypeNames
  public
    procedure Add(const AParamType: string);
    procedure Clear;
    function Count: Integer;
    function Equals(const AParamTypeNames: TParamTypeNames): Boolean;
  end;

  TCursorArrayHelper = record helper for TCursorArray
  public
    procedure Add(const ACursor: TCursor);
  end;

  TDelphiMethodComparerByMethodName = class (TInterfacedObject, IComparer<TDelphiMethod>)
  protected
    { IComparer<T> }
    function Compare(const Left, Right: TDelphiMethod): Integer;
  end;

  TBlockMethodComparerByTypeName = class (TInterfacedObject, IComparer<TBlockMethod>)
  protected
    { IComparer<T> }
    function Compare(const Left, Right: TBlockMethod): Integer;
  end;

const
  cCombinedHeaderFileName = '_combinedheaders_.h';

{ TObjCTranslateOptionsHelper }

function TObjCTranslateOptionsHelper.WantDeprecationCommentFirst: Boolean;
begin
  Result := WantDeprecationComments and (TObjCTranslateOption.DeprecationCommentFirst in Self);
end;

function TObjCTranslateOptionsHelper.WantDeprecationComments: Boolean;
begin
  Result := TObjCTranslateOption.DeprecationComments in Self;
end;

function TObjCTranslateOptionsHelper.WantDeprecationCommentSameLine: Boolean;
begin
  Result := WantDeprecationComments and not (TObjCTranslateOption.DeprecationCommentFirst in Self);
end;

{ TParamTypeNamesHelper }

procedure TParamTypeNamesHelper.Add(const AParamType: string);
begin
  SetLength(Self, Count + 1);
  Self[Count - 1] := AParamType;
end;

procedure TParamTypeNamesHelper.Clear;
begin
  SetLength(Self, 0);
end;

function TParamTypeNamesHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TParamTypeNamesHelper.Equals(const AParamTypeNames: TParamTypeNames): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count = AParamTypeNames.Count then
  begin
    for I := 0 to Count - 1 do
    begin
      if not Self[I].Equals(AParamTypeNames[I]) then
        Exit(False); // <======
    end;
    Result := True;
  end;
end;

{ TCursorArrayHelper }

procedure TCursorArrayHelper.Add(const ACursor: TCursor);
begin
  SetLength(Self, Length(Self) + 1);
  Self[Length(Self) - 1] := ACursor;
end;

{ TDelphiMethodComparerByMethodName }

function TDelphiMethodComparerByMethodName.Compare(const Left, Right: TDelphiMethod): Integer;
begin
  Result := string.CompareText(Left.MethodName, Right.MethodName);
end;

{ TBlockMethodComparerByTypeName }

function TBlockMethodComparerByTypeName.Compare(const Left, Right: TBlockMethod): Integer;
begin
  Result := string.CompareText(Left.TypeName, Right.TypeName);
end;

{ TDelphiMethod }

function TDelphiMethod.Equals(const AMethod: TDelphiMethod): Boolean;
begin
  Result := (Index <> AMethod.Index) and AMethod.MethodName.Equals(MethodName) and AMethod.ParamTypeNames.Equals(ParamTypeNames);
end;

procedure TDelphiMethod.ExpandMethodName;
var
  I: Integer;
  LPart: string;
begin
  if (Length(ObjCMethodNameParts) > 1) and (MethodNamePartsCount < Length(ObjCMethodNameParts) - 1) then
  begin
    Inc(MethodNamePartsCount);
    MethodName := ObjCMethodNameParts[0];
    for I := 1 to MethodNamePartsCount do
    begin
      LPart := ObjCMethodNameParts[I];
      if not LPart.IsEmpty then
        MethodName := MethodName + UpCase(ObjCMethodNameParts[I].Chars[0]) + ObjCMethodNameParts[I].Substring(1);
    end;
  end;
end;

procedure TDelphiMethod.ExpandObjCMethodName;
begin
  ObjCMethodNameParts := ObjCMethodName.Split([':']);
end;

function TDelphiMethod.GetDeclaration: string;
begin
  Result := Format(DeclarationFormat, [MethodName]);
end;

function TDelphiMethod.IsNameMismatched: Boolean;
var
  LMethodName: string;
begin
  LMethodName := MethodName;
  // Ignore escaping
  if LMethodName.StartsWith('&') then
    LMethodName := LMethodName.Substring(1);
  Result := IsMismatched or ((Length(ObjCMethodNameParts) > 0) and not LMethodName.Equals(ObjCMethodNameParts[0]));
end;

{ TBlockMethod }

procedure TBlockMethod.ExpandObjCMethodName;
begin
  ObjCMethodNameParts := ObjCMethodName.Split([':']);
end;

{ TDelphiMethods }

function TDelphiMethods.AddMethod(AMethod: TDelphiMethod): Integer;
begin
  if AMethod.IsAvailable then
  begin
    AMethod.ExpandObjCMethodName;
    AMethod.Index := Count;
    Result := Add(AMethod);
  end
  else
    Result := -1;
end;

function TDelphiMethods.MethodExists(const AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if (AIndex <> I) and Items[I].Equals(Items[AIndex]) then
      Exit(True); // <======
  end;
end;

procedure TDelphiMethods.ResolveMethods;
var
  I: Integer;
  LHasCollisions: Boolean;
begin
  // Resolve collisions
  repeat
    LHasCollisions := False;
    for I := 0 to Count - 1 do
    begin
      if MethodExists(I) then
      begin
        if ResolveCollisions(Items[I]) then
          LHasCollisions := True;
      end;
    end;
  until not LHasCollisions;
  // Resolve overloads
  for I := 0 to Count - 1 do
    ResolveOverloads(I);
  if FComparer = nil then
    FComparer := TDelphiMethodComparerByMethodName.Create;
  Sort(FComparer);
end;

function TDelphiMethods.ResolveCollisions(const AMethod: TDelphiMethod): Boolean;
var
  I: Integer;
  LMethod: TDelphiMethod;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    LMethod := Items[I];
    if LMethod.Equals(AMethod) then
    begin
      LMethod.ExpandMethodName;
      Result := True;
      Items[I] := LMethod;
    end;
  end;
end;

procedure TDelphiMethods.ResolveOverloads(const AIndex: Integer);
var
  I: Integer;
  LMethod: TDelphiMethod;
begin
  for I := 0 to Count - 1 do
  begin
    LMethod := Items[I];
    if (I <> AIndex) and LMethod.MethodName.Equals(Items[AIndex].MethodName) then
    begin
      LMethod.IsOverload := True;
      Items[I] := LMethod;
    end;
  end;
end;

{ TBlockMethods }

function TBlockMethods.AddBlockMethod(const AClassName, ADeclaration: string; const AMethodName: string = ''): Integer;
var
  LBlockMethod: TBlockMethod;
begin
  Result := FindMethod(AClassName, ADeclaration, LBlockMethod, AMethodName);
  if Result = -1 then
  begin
    LBlockMethod.ObjCClassName := AClassName;
    LBlockMethod.ObjCMethodName := AMethodName;
    LBlockMethod.ExpandObjCMethodName;
    LBlockMethod.TypeName := Format('T%sBlockMethod%d', [AClassName, ClassBlockMethodCount(AClassName) + 1]);  // e.g. TMTLDeviceBlockMethod1
    LBlockMethod.Declaration := ADeclaration;
    Add(LBlockMethod);
    Result := Count - 1;
  end;
end;

function TBlockMethods.ClassBlockMethodCount(const AClassName: string): Integer;
var
  LMethod: TBlockMethod;
begin
  Result := 0;
  for LMethod in Self do
  begin
    if LMethod.ObjCClassName.Equals(AClassName) then
      Inc(Result);
  end;
end;

function TBlockMethods.FindMethod(const AClassName, ADeclaration: string; out AMethod: TBlockMethod; const AMethodName: string = ''): Integer;
var
  I: Integer;
  LMethod: TBlockMethod;
begin
  for I := 0 to Count - 1 do
  begin
    LMethod := Items[I];
    if LMethod.ObjCClassName.Equals(AClassName) then
    begin
      if (AMethodName.IsEmpty and LMethod.Declaration.Equals(ADeclaration))
        or (not AMethodName.IsEmpty and LMethod.ObjCMethodName.Equals(AMethodName)) then
      begin
        AMethod := LMethod;
        Exit(I); // <======
      end;
    end;
  end;
  Result := -1;
end;

procedure TBlockMethods.SortMethods;
begin
  if FComparer = nil then
    FComparer := TBlockMethodComparerByTypeName.Create;
  Sort(FComparer);
end;

{ TObjCHeaderProject }

function TObjCHeaderProject.GetFrameworkName: string;
begin
  Result := TPath.GetFileNameWithoutExtension(FFrameworkDirectory);
end;

function TObjCHeaderProject.GetLibraryConstant: string;
begin
  Result := 'lib' + GetFrameworkName;
end;

function TObjCHeaderProject.GetLibraryConstantDeclaration: string;
begin
  Result := Format('%s = ''%s'';', [GetLibraryConstant, GetLibraryPath]);
end;

function TObjCHeaderProject.GetLibraryPath: string;
begin
  Result := TPath.Combine(FFrameworkDirectory, GetFrameworkName);
  Result := Copy(Result, Length(FSdkRoot) + 1, Length(Result));
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

procedure TObjCHeaderProject.SetSdkRoot(const Value: string);
begin
  FSdkRoot := Value;
  FFrameworkRoot := TPath.Combine(FSdkRoot, cSDKFrameworksFolder);
end;

procedure TObjCHeaderProject.SetTargetPlatform(const Value: string);
begin
  FTargetPlatform := Value.ToUpper;
end;

{ TObjCHeaderTranslator }

constructor TObjCHeaderTranslator.Create(const AProject: TCustomTranslatorProject);
begin
  inherited;
  FProject := TObjCHeaderProject(AProject);
  FIncludedFrameworks := TStringList.Create;
  FInterfaceUnits := TStringList.Create;
  FBlockMethods := TBlockMethods.Create;
  FMethods := TDelphiMethods.Create;
  FClasses := TList<TCursor>.Create;
  FCategories := TList<TCursor>.Create;
  FExportedConsts := TList<TCursor>.Create;
end;

destructor TObjCHeaderTranslator.Destroy;
begin
  FIncludedFrameworks.Free;
  FInterfaceUnits.Free;
  FBlockMethods.Free;
  FMethods.Free;
  FClasses.Free;
  FCategories.Free;
  FExportedConsts.Free;
  inherited;
end;

procedure TObjCHeaderTranslator.DoPrepare;
begin
  FNeedsMacapiHelpers := False;
  FClasses.Clear;
  FExportedConsts.Clear;
  FBlockMethods.Clear;
  FIncludedFrameworks.Clear;
  FInterfaceUnits.Clear;
end;

procedure TObjCHeaderTranslator.SetupTokenMap;
begin
  inherited;
  { C-to-Delphi translations for common tokens.
    These are only used when manually converting #define macros. }
  { Surround with spaces to avoid concatenation with other tokens. }
  TokenMap.Add('<<', ' shl ');
  TokenMap.Add('>>', ' shr ');
  TokenMap.Add('&', ' and ');
  TokenMap.Add('|', ' or ');
  TokenMap.Add('^', ' xor ');
  TokenMap.Add('~', ' not ');
  TokenMap.Add('&&', ' and ');
  TokenMap.Add('||', ' or ');
  TokenMap.Add('!', ' not ');
  TokenMap.Add('%', ' mod ');
  TokenMap.Add('==', '=');
  TokenMap.Add('!=', '<>');
end;

procedure TObjCHeaderTranslator.DoSetupTypeMap;
begin
  TypeMap.Add('size_t', 'NativeUInt');
  TypeMap.Add('intptr_t', 'IntPtr');
  TypeMap.Add('uintptr_t', 'UIntPtr');
  TypeMap.Add('ptrdiff_t', 'NativeInt');
  TypeMap.Add('wchar_t', 'WideChar');
  TypeMap.Add('time_t', 'Longint');

  // From stdint.h
  TypeMap.Add('int8_t', 'Int8');
  TypeMap.Add('int16_t', 'Int16');
  TypeMap.Add('int32_t', 'Int32');
  TypeMap.Add('int64_t', 'Int64');
  TypeMap.Add('uint8_t', 'UInt8');
  TypeMap.Add('uint16_t', 'UInt16');
  TypeMap.Add('uint32_t', 'UInt32');
  TypeMap.Add('uint64_t', 'UInt64');

  TypeMap.Add('int_least8_t', 'Int8');
  TypeMap.Add('int_least16_t', 'Int16');
  TypeMap.Add('int_least32_t', 'Int32');
  TypeMap.Add('int_least64_t', 'Int64');
  TypeMap.Add('uint_least8_t', 'UInt8');
  TypeMap.Add('uint_least16_t', 'UInt16');
  TypeMap.Add('uint_least32_t', 'UInt32');
  TypeMap.Add('uint_least64_t', 'UInt64');

  TypeMap.Add('int_fast8_t', 'Int8');
  TypeMap.Add('int_fast16_t', 'Int32');
  TypeMap.Add('int_fast32_t', 'Int32');
  TypeMap.Add('int_fast64_t', 'Int64');
  TypeMap.Add('uint_fast8_t', 'UInt8');
  TypeMap.Add('uint_fast16_t', 'UInt32');
  TypeMap.Add('uint_fast32_t', 'UInt32');
  TypeMap.Add('uint_fast64_t', 'UInt64');

  TypeMap.Add('intmax_t', 'Int64');
  TypeMap.Add('uintmax_t', 'UInt64');

  // C functions can have a parameter of type "va_list", containing a variable
  // number of arguments. We cannot really use these functions in Delphi (but
  // we CAN use "varargs" functions). We set the "va_list" type to a pointer,
  // but should probably ignore functions that use it.
  TypeMap.Add('va_list', 'Pointer');

  // Type FILE the cannot be used in Delphi, so we convert to a Pointer.
  TypeMap.Add('FILE', 'Pointer');

  TypeMap.Add('ULONG', 'UInt32');
  TypeMap.Add('LPVOID', 'Pointer');
  TypeMap.Add('Class', 'Pointer');
  TypeMap.Add('Protocol', 'Pointer');
  TypeMap.Add('BOOL', 'Boolean');
  TypeMap.Add('PBOOL', 'PBoolean');
  TypeMap.Add('id', 'Pointer');
  TypeMap.Add('instancetype', 'Pointer');
  // Unexposed types
  TypeMap.Add('ObjectType', 'Pointer');
  TypeMap.Add('ValueType', 'Pointer');
end;

procedure TObjCHeaderTranslator.DoSetupTypeUnitMap;
begin
  TypeUnitMap.Add('id', 'Macapi.ObjCRuntime');
  TypeUnitMap.Add('dispatch_queue_t', 'Macapi.Dispatch');
  TypeUnitMap.Add('CGSize', 'Macapi.CocoaTypes|iOSapi.CoreGraphics');
end;

function TObjCHeaderTranslator.GenerateAnonymousTypeName(const AName: string): string;
begin
  Result := ''; // Todo?
end;

function TObjCHeaderTranslator.GetCategoryClassName(const ACursor: TCursor): string;
var
  LClassName: string;
begin
  LClassName := '';
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      if ACursor.Kind = TCursorKind.ObjCClassRef then
      begin
        LClassName := ACursor.Spelling;
        Result := TChildVisitResult.Break;
      end
      else
        Result := TChildVisitResult.Continue;
    end
  );
  Result := LClassName;
end;

function TObjCHeaderTranslator.GetClassCategories(const AClassName: string): TCursorArray;
var
  LCursor: TCursor;
begin
  for LCursor in FCategories do
  begin
    if GetCategoryClassName(LCursor).Equals(AClassName) then
      Result.Add(LCursor);
  end;
end;

function TObjCHeaderTranslator.CanWriteToDo: Boolean;
begin
  Result := TObjCTranslateOption.TodoComments in FProject.ObjCTranslateOptions;
end;

procedure TObjCHeaderTranslator.CheckTypeMaps(const ACursor: TCursor);
var
  LTypeName: string;
begin
  case ACursor.Kind of
    TCursorKind.ParmDecl, TCursorKind.FieldDecl:
    begin
      LTypeName := GetDelphiTypeName(ACursor.CursorType, True);
      CheckTypeUnitMap(LTypeName);
    end;
  end;
end;

procedure TObjCHeaderTranslator.CheckTypeUnitMap(const ATypeName: string);
var
  LUnitName: string;
  LParts: TArray<string>;
begin
  if TypeUnitMap.TryGetValue(ATypeName, LUnitName) then
  begin
    LParts := LUnitName.Split(['|']);
    if Length(LParts) > 1 then
    begin
      if SameText(FProject.TargetPlatform, cTargetPlatformMacOS) then
        LUnitName := LParts[0]
      else
        LUnitName := LParts[1];
    end;
    if FInterfaceUnits.IndexOf(LUnitName) = -1 then
      FInterfaceUnits.Add(LUnitName);
  end;
end;

procedure TObjCHeaderTranslator.DiscoverBlockMethodParams(const ACursor: TCursor; const AClassName: string);
var
  LMethodName: string;
  LClassName: string;
  LIsProperty: Boolean;
begin
  LMethodName := ACursor.Spelling;
  LClassName := AClassName;
  LIsProperty := ACursor.Kind = TCursorKind.ObjCPropertyDecl;
  // Has a block pointer as a result type, so add that block method
  if not LIsProperty and (ACursor.ResultType.Kind = TTypeKind.BlockPointer) then
    FBlockMethods.AddBlockMethod(AClassName, GetBlockMethodTypeDeclaration(ACursor.ResultType.PointeeType), LMethodName)
//  else if LIsProperty and (ACursor.CursorType.Kind = TTypeKind.BlockPointer) then
//  // This is to allow property declarations (ObjCPropertyDecl) to determine block method defs
//    FBlockMethods.AddBlockMethod(AClassName, GetBlockMethodDeclaration(ACursor, AClassName), LMethodName);
  else if LIsProperty then
    Exit;
  // Discover block methods in parameters for this method
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      if ACursor.Kind = TCursorKind.ParmDecl then
      begin
        if ACursor.CursorType.Kind = TTypeKind.BlockPointer then
          FBlockMethods.AddBlockMethod(AClassName, GetBlockMethodDeclaration(ACursor, AClassName));
      end;
      Result := TChildVisitResult.Continue;
    end
  );
end;

procedure TObjCHeaderTranslator.DiscoverBlockMethods(const ACursor: TCursor; const AClassName: string = '');
var
  LStructName: string;
  LIsAnonymousStruct: Boolean;
begin
  if AClassName.IsEmpty then
  begin
    case ACursor.Kind of
      TCursorKind.ObjCInterfaceDecl:
        LStructName := GetDelphiTypeName(ACursor.CursorType, False, @LIsAnonymousStruct);
      TCursorKind.ObjCCategoryDecl:
        LStructName := GetCategoryClassName(ACursor);
    else
      LStructName := ACursor.Spelling;
    end;
  end
  else
    LStructName := AClassName;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      case ACursor.Kind of
        TCursorKind.ObjCClassMethodDecl, TCursorKind.ObjCInstanceMethodDecl, TCursorKind.ObjCPropertyDecl:
        begin
          DiscoverBlockMethodParams(ACursor, LStructName);
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
end;

function TObjCHeaderTranslator.HandleTypeDeclaration(const ACursor: TCursor): Boolean;
begin
  Result := True;
  case ACursor.Kind of
    TCursorKind.ObjCInterfaceDecl:
    begin
      FClasses.Add(ACursor);
      // Methods need to be iterated to determine if there are any unnamed block method types
      // Each type is added to a separate list which is emitted after the regular types, but before the protocols and classes
      DiscoverBlockMethods(ACursor);
    end;
    TCursorKind.ObjCCategoryDecl:
    begin
      FCategories.Add(ACursor);
      // Methods need to be iterated to determine if there are any unnamed block method types
      // Each type is added to a separate list which is emitted after the regular types, but before the protocols and classes
      DiscoverBlockMethods(ACursor);
    end;
    TCursorKind.EnumDecl:
    begin
      Types.Add(ACursor);
    end;
    // Includes exported consts
    TCursorKind.VarDecl:
    begin
      FExportedConsts.Add(ACursor);
    end;
  else
    Result := False;
  end;
end;

function TObjCHeaderTranslator.HandleTypeDefinition(const ACursor: TCursor): Boolean;
begin
  Result := True;
  case ACursor.Kind of
    TCursorKind.ObjCProtocolDecl:
    begin
      FClasses.Add(ACursor);
      DiscoverBlockMethods(ACursor);
    end;
  else
    Result := False;
  end;
end;

procedure TObjCHeaderTranslator.HandleWriteType(const ACursor: TCursor);
begin
  case ACursor.Kind of
    TCursorKind.ObjCInterfaceDecl:
      WriteInterfaceType(ACursor);
    TCursorKind.ObjCProtocolDecl:
      // For now, WriteInterfaceClassType will also handle protocol declarations
      WriteInterfaceClassType(ACursor);
  end;
end;

function TObjCHeaderTranslator.GetBlockMethodTypeDeclaration(const AType: TType): string;
var
  LHasResult: Boolean;
  LArgument, LTypeName: string;
  LBuilder: TStringBuilder;
  I: Integer;
begin
  LHasResult := not (AType.ResultType.Kind in [TTypeKind.Invalid, TTypeKind.Void]);
  LBuilder := TStringBuilder.Create;
  try
    if LHasResult then
      LBuilder.Append('function')
    else
      LBuilder.Append('procedure');
    if AType.ArgTypeCount > 0 then
      LBuilder.Append('(');
    for I := 0 to AType.ArgTypeCount - 1 do
    begin
      LTypeName := GetDelphiTypeName(AType.ArgTypes[I], True);
      LArgument := Format('param%d: %s', [I + 1, LTypeName]);
      LBuilder.Append(LArgument);
      if I < AType.ArgTypeCount - 1 then
        LBuilder.Append('; ');
    end;
    if AType.ArgTypeCount > 0 then
      LBuilder.Append(')');
    // Add the result type, if it's a function
    if LHasResult then
    begin
      LBuilder.Append(': ');
      LBuilder.Append(GetDelphiTypeName(AType.ResultType));
    end;
    LBuilder.Append(' of object;');
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TObjCHeaderTranslator.GetBinaryOperatorTokens(const ASource: string): TArray<string>;
const
  cBinaryOperators: array[0..7] of string = ('<<', '>>', '&&', '||', '%', '^', '|', '&'); // *** Put double character operators first! ***
var
  LOperator, LSource, LToken: string;
  I, LCount: Integer;
begin
  SetLength(Result, 0);
  LSource := ASource;
  LCount := 0;
  I := 0;
  while not LSource.IsEmpty and (I < Length(LSource)) do
  begin
    for LOperator in cBinaryOperators do
    begin
      LToken := LSource.Substring(I, Length(LOperator));
      if LToken.Equals(LOperator) then
      begin
        Inc(LCount, 2);
        SetLength(Result, LCount);
        Result[LCount - 2] := LSource.Substring(0, I).Trim;
        Result[LCount - 1] := LToken.Trim;
        LSource := LSource.Substring(I + Length(LToken));
        I := -1;
        Break;
      end;
      //
    end;
    Inc(I);
  end;
  if not LSource.IsEmpty then
  begin
    SetLength(Result, LCount + 1);
    Result[LCount] := LSource.Trim;
  end;
end;

function TObjCHeaderTranslator.GetBlockMethodDeclaration(const ACursor: TCursor; const AClassName: string): string;
var
  LHasResult: Boolean;
  LArgCount, LArgIndex: Integer;
  LPointeeType: TType;
  LBuilder: TStringBuilder;
  LMethodName, LSpelling: string;
  LHasArgs: Boolean;
begin
  LMethodName := ACursor.Spelling;
  IsProceduralType(ACursor.CursorType, LPointeeType);
  LHasResult := not (ACursor.ResultType.Kind in [TTypeKind.Invalid, TTypeKind.Void]);
  LBuilder := TStringBuilder.Create;
  try
    if LHasResult then
      LBuilder.Append('function')
    else
      LBuilder.Append('procedure');
    LArgCount := LPointeeType.ArgTypeCount;
    LHasArgs := False;
    // Pre-check
    ACursor.VisitChildren(
      function(const ACursor, AParent: TCursor): TChildVisitResult
      begin
        if ACursor.Kind = TCursorKind.ParmDecl then
        begin
          LHasArgs := True;
          Result := TChildVisitResult.Break;
        end
        else
          Result := TChildVisitResult.Continue;
      end
    );
    if LHasArgs then
    begin
      LBuilder.Append('(');
      LArgIndex := 0;
      ACursor.VisitChildren(
        function(const ACursor, AParent: TCursor): TChildVisitResult
        var
          LArgName, LTypeName: string;
          LIndex: Integer;
        begin
          LSpelling := ACursor.Spelling;
          if ACursor.Kind = TCursorKind.ParmDecl then
          begin
            LArgName := GetValidIdentifier(ACursor.Spelling);
            if LArgName = '' then
              LArgName := 'param' + (LArgIndex + 1).ToString;
            LTypeName := 'Unknown';
            case ACursor.CursorType.Kind of
              TTypeKind.BlockPointer:
              begin
                LIndex := FBlockMethods.AddBlockMethod(AClassName, GetBlockMethodTypeDeclaration(ACursor.ResultType.PointeeType), ACursor.Spelling);
                if LIndex > -1 then
                  LTypeName := FBlockMethods.Items[LIndex].TypeName;
              end;
            else
              LTypeName := GetDelphiTypeName(ACursor.CursorType, True);
            end;
            LBuilder.Append(LArgName + ': ' + LTypeName);
            Inc(LArgIndex);
            if LArgIndex < LArgCount then
              LBuilder.Append('; ');
          end;
          Result := TChildVisitResult.Continue;
        end
      );
      LBuilder.Append(')');
    end;
    // Add the result type, if it's a function
    if LHasResult then
    begin
      LBuilder.Append(': ');
      LBuilder.Append(GetDelphiTypeName(ACursor.ResultType));
    end;
    LBuilder.Append(' of object;');
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

procedure TObjCHeaderTranslator.GetDelphiMethodChildren(const ACursor: TCursor; const ABuilder: TStringBuilder; const AClassName: string;
  const AIsClass: Boolean; var AMethod: TDelphiMethod);
var
  LArgCount, LArgIndex: Integer;
  LParamTypeNames: TParamTypeNames;
  LSource, LDeprecation: string;
  LIsAvailable, LIsMismatched: Boolean;
  LMethod: TDelphiMethod;
  LParamNames: TArray<string>;
begin
  LDeprecation := '';
  LArgCount := ACursor.ArgumentCount;
  if LArgCount > 0 then
    ABuilder.Append('(');
  LArgIndex := 0;
  LIsAvailable := True;
  LMethod := AMethod;
  LParamNames := [];
  LIsMismatched := False;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    var
      LBlockMethod: TBlockMethod;
      LArgName, LTypeName, LBlockDeclaration, LSpelling: string;
      LParamExists: Boolean;
    begin
      case ACursor.Kind of
        TCursorKind.ParmDecl:
        begin
          LSpelling := ACursor.Spelling;
          LArgName := '';
          if (LArgIndex > 0) and (Length(LMethod.ObjCMethodNameParts) > LArgIndex) then
            LArgName := GetValidIdentifier(LMethod.ObjCMethodNameParts[LArgIndex]);
          LParamExists := IndexStr(LArgName, LParamNames) > -1;
          if LParamExists then
            LIsMismatched := True;
          if LArgName.IsEmpty or LParamExists then
            LArgName := GetValidIdentifier(ACursor.Spelling);
          if LArgName.IsEmpty then
            LArgName := 'param' + (LArgIndex + 1).ToString;
          LParamNames := LParamNames + [LArgName];
          LTypeName := 'Unknown';
          if ACursor.CursorType.Kind = TTypeKind.BlockPointer then
          begin
            if AIsClass then
            begin
              LBlockDeclaration := GetBlockMethodDeclaration(ACursor, AClassName);
              if FBlockMethods.FindMethod(AClassName, LBlockDeclaration, LBlockMethod) > -1 then
                LTypeName := LBlockMethod.TypeName;
            end
            else
              // Block result type in "protocols" are always Pointer type
              LTypeName := 'Pointer';
          end
          else
            LTypeName := GetDelphiTypeName(ACursor.CursorType, True);
          ABuilder.Append(LArgName + ': ' + LTypeName);
          LParamTypeNames.Add(LTypeName);
          Inc(LArgIndex);
          if LArgIndex < LArgCount then
            ABuilder.Append('; ');
        end;
        TCursorKind.UnexposedAttr:
        begin
          LSource := GetCursorSource(ACursor);
          if LSource.StartsWith(cAPIDeprecatedStartsWith) then
            LDeprecation := LSource
          else if LSource.StartsWith(cAPINSAvailableStartsWith) then
            LIsAvailable := GetIsAvailable(LSource);
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
  AMethod.IsMismatched := LIsMismatched;
  AMethod.ParamTypeNames := LParamTypeNames;
  AMethod.Deprecation := LDeprecation;
  AMethod.IsAvailable := LIsAvailable;
  if LArgCount > 0 then
    ABuilder.Append(')');
end;

function TObjCHeaderTranslator.GetIsAvailable(const ASource: string): Boolean;
var
  LParts: TStringDynArray;
  LSegment: string;
begin
  // ASource should be e.g. NS_AVAILABLE(NA, 4_0)
  Result := True;
  LSegment := ASource.Substring(ASource.IndexOf('(') + 1); // 'NA, 4_0)'
  LParts := LSegment.Substring(0, LSegment.IndexOf(')')).Split([',']);  // ['NA', ' 4_0']
  if IsPlatformMacOS and (Length(LParts) > 0) and LParts[0].Trim.Equals('NA') then
    Result := False
  else if IsPlatformIOS and (Length(LParts) > 1) and LParts[1].Trim.Equals('NA') then
    Result := False;
end;

function TObjCHeaderTranslator.GetDelphiMethod(const ACursor: TCursor; const AClassName: string; const AIsClassMethod: Boolean;
  const AIsClass: Boolean): TDelphiMethod;
var
  LBlockMethod: TBlockMethod;
  LHasResult: Boolean;
  LBlockDeclaration: string;
  LBuilder: TStringBuilder;
begin
  Result.ObjCMethodName := ACursor.Spelling;
  Result.ExpandObjCMethodName;
  Result.MethodName := GetValidIdentifier(ACursor.Spelling);
  LHasResult := not (ACursor.ResultType.Kind in [TTypeKind.Invalid, TTypeKind.Void]);
  LBuilder := TStringBuilder.Create;
  try
    if AIsClassMethod then
      LBuilder.Append('{class} ');
    if LHasResult then
      LBuilder.Append('function ')
    else
      LBuilder.Append('procedure ');
    LBuilder.Append('%s'); // To be replaced by the method name
    // Add the params
    GetDelphiMethodChildren(ACursor, LBuilder, AClassName, AIsClass, Result);
    // Add the result type, if it's a function
    if LHasResult then
    begin
      LBuilder.Append(': ');
      Result.ResultTypeName := 'Unknown';
      if ACursor.ResultType.Kind = TTypeKind.BlockPointer then
      begin
        LBlockDeclaration := GetBlockMethodTypeDeclaration(ACursor.ResultType.PointeeType);
        if FBlockMethods.FindMethod(AClassName, LBlockDeclaration, LBlockMethod) > -1 then
          Result.ResultTypeName := LBlockMethod.TypeName;
      end
      else
        Result.ResultTypeName := GetDelphiTypeName(ACursor.ResultType);
      LBuilder.Append(Result.ResultTypeName);
    end;
    Result.DeclarationFormat := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TObjCHeaderTranslator.GetDelphiTypeName(AType: TType; const AInParamDecl: Boolean; const AOutIsAnonymous: PBoolean): string;
var
  LIndex: Integer;
  LConverted: string;
begin
  Result := inherited GetDelphiTypeName(AType, AInParamDecl, AOutIsAnonymous);
  // Remove generic type from the declaration
  LIndex := Result.IndexOf('<');
  if LIndex > 0 then
    Delete(Result, LIndex + 1, Length(Result))
  else
  begin
    LIndex := Pos(' *', Result);
    if LIndex > 0 then
      Delete(Result, LIndex, Length(Result));
  end;
  if TypeMap.TryGetValue(Result, LConverted) then
    Result := LConverted;
  // Slight "hack"
  if Result.Equals('PNSError') then
    Result := 'PPointer';
  if Result.StartsWith('__kindof') then
    Result := Result.Substring(Result.IndexOf(' ') + 1);
end;

function TObjCHeaderTranslator.GetUnexposedType(const AType: TType): string;
begin
  // Unexposed types are added to the type map
  TypeMap.TryGetValue(AType.Spelling, Result);
end;

procedure TObjCHeaderTranslator.WriteClasses;
var
  LBlockMethod: TBlockMethod;
  LCursor: TCursor;
begin
  // Write block method types
  for LBlockMethod in FBlockMethods do
    Writer.WriteLn('%s = %s', [LBlockMethod.TypeName, LBlockMethod.Declaration]);
  if FBlockMethods.Count > 0 then
    Writer.WriteLn;
  // Write classes/delegates
  for LCursor in FClasses do
    HandleWriteType(LCursor);
  Writer.Outdent;
  Writer.WriteLn;
  WriteExportedConstProtos;
end;

procedure TObjCHeaderTranslator.WriteDelphiMethods;
var
  LMethod: TDelphiMethod;
begin
  FMethods.ResolveMethods;
  for LMethod in FMethods do
  begin
    // if LMethod.ObjCMethodName.CountChar(':') > 1 then
    if not LMethod.Deprecation.IsEmpty and FProject.ObjCTranslateOptions.WantDeprecationCommentFirst then
      Writer.WriteLn('// %s', [LMethod.Deprecation]);
    if LMethod.IsNameMismatched then
      Writer.WriteLn('[MethodName(''%s'')]', [LMethod.ObjCMethodName]);
    Writer.Write(LMethod.GetDeclaration);
    if LMethod.IsOverload then
      Writer.Write('; overload');
    Writer.Write('; cdecl;');
    if not LMethod.Deprecation.IsEmpty and FProject.ObjCTranslateOptions.WantDeprecationCommentSameLine then
      Writer.Write(' // %s', [LMethod.Deprecation]);
    Writer.WriteLn('');
  end;
end;

procedure TObjCHeaderTranslator.WriteEnumTypeDecl(const ACursor: TCursor);
begin
  Writer.WriteLn('%s = %s;', [GetValidIdentifier(ACursor.Spelling), 'NSInteger']);
end;

function TObjCHeaderTranslator.IsSupportedDefinedConst(const ACursor: TCursor): Boolean;
var
  LIsSupported: Boolean;
begin
  if ACursor.IsExpression then
  begin
    // This is the goal:
    // Result := ACursor.Kind in [TCursorKind.IntegerLiteral, TCursorKind.FloatingLiteral, TCursorKind.StringLiteral, TCursorKind.CharacterLiteral,
    //   TCursorKind.ObjCStringLiteral, TCursorKind.ObjCBoolLiteralExpr];
    // This is what has been implemented so far:
    Result := ACursor.Kind in [TCursorKind.ObjCStringLiteral];
  end
  else
  begin
    LIsSupported := False;
    ACursor.VisitChildren(
      function(const ACursor, AParent: TCursor): TChildVisitResult
      begin
        if ACursor.IsExpression and IsSupportedDefinedConst(ACursor) then
        begin
          LIsSupported := True;
          Result := TChildVisitResult.Break;
        end
        else
          Result := TChildVisitResult.Continue;
      end
    );
    Result := LIsSupported;
  end;
end;

function TObjCHeaderTranslator.IsSupportedExportedConst(const AType: TType): Boolean;
var
  I: Integer;
  LUnderlyingType: TType;
  LTypeName: string;
begin
  LUnderlyingType := GetUnderlyingType(AType);
  Result := LUnderlyingType.Kind in [TTypeKind.TypeDef, TTypeKind.Pointer];
  if not Result then
  begin
    LTypeName := GetDelphiTypeName(LUnderlyingType);
    for I := Low(cExportedConstTypeNames) to High(cExportedConstTypeNames) do
    begin
      if LTypeName.Equals(cExportedConstTypeNames[I]) then
        Exit(True); // <======
    end;
  end;
end;

// Defined constant example:
// function kFIREventAddPaymentInfo: NSString;
// begin
//   Result := StrToNSStr('add_payment_info');
// end;

//  { An integer literal. }
//  IntegerLiteral = CXCursor_IntegerLiteral,
//
//  { A floating point number literal. }
//  FloatingLiteral = CXCursor_FloatingLiteral,
//
//  { An imaginary number literal. }
//  ImaginaryLiteral = CXCursor_ImaginaryLiteral,
//
//  { A string literal. }
//  StringLiteral = CXCursor_StringLiteral,
//
//  { A character literal. }
//  CharacterLiteral = CXCursor_CharacterLiteral,
//
//  { [C99 6.5.2.5] }
//  CompoundLiteralExpr = CXCursor_CompoundLiteralExpr,
//
//  { [C++ 2.13.5] C++ Boolean Literal. }
//  CXXBoolLiteralExpr = CXCursor_CXXBoolLiteralExpr,
//
//  { [C++0x 2.14.7] C++ Pointer Literal. }
//  CXXNullPtrLiteralExpr = CXCursor_CXXNullPtrLiteralExpr,
//
//  { An Objective-C string literal i.e. @@"foo". }
//  ObjCStringLiteral = CXCursor_ObjCStringLiteral,
//
//  { Objective-c Boolean Literal. }
//  ObjCBoolLiteralExpr = CXCursor_ObjCBoolLiteralExpr,

procedure TObjCHeaderTranslator.WriteExportedConstFunctions;
var
  LCursor: TCursor;
  LUnderlyingType: TType;
  LTypeName, LUnderylingTypeName, LLiteral: string;
  LLiteralKind: TCursorKind;
  LUnderlyingTypeKind: TTypeKind;
begin
  for LCursor in FExportedConsts do
  begin
    if IsSupportedExportedConst(LCursor.CursorType) and not (LCursor.IsDefinition and not IsSupportedDefinedConst(LCursor)) then
    begin
      LUnderlyingType := GetUnderlyingType(LCursor.CursorType);
      LUnderlyingTypeKind := LUnderlyingType.Kind;
      LUnderylingTypeName := GetDelphiTypeName(LUnderlyingType);
      // https://github.com/Embarcadero/octoid/issues/20
      if LUnderylingTypeName.Equals('P__CFString') or LUnderylingTypeName.Equals('CFStringRef') then
      begin
        LUnderlyingTypeKind := TTypeKind.ObjCObjectPointer;
        LUnderylingTypeName := 'NSString';
      end;
      LTypeName := GetDelphiTypeName(LCursor.CursorType);
      // https://github.com/Embarcadero/octoid/issues/20
      if (LTypeName.Equals('CFStringRef') or (LUnderylingTypeName.Equals('NSString') and not LTypeName.Equals('NSErrorDomain'))) then
        LTypeName := 'NSString';
      Writer.WriteLn('function %s: %s;', [GetValidIdentifier(LCursor.Spelling), LTypeName]);
      Writer.WriteLn('begin');
      Writer.Indent;
      if LCursor.IsDefinition and IsSupportedDefinedConst(LCursor) then
      begin
        // kFIREventAddPaymentInfo: VarDecl(T1) [IsDeclaration, IsDefinition, HasAttributes] @Headers/FIREventNames.h (17:24)
        //   (unnamed): UnexposedAttr [IsAttribute, IsUnexposed, HasAttributes] @Headers/FIREventNames.h (17:48)
        //   NSString: ObjCClassRef(T2) [IsReference] @Headers/FIREventNames.h (17:8)
        //   "add_payment_info": ObjCStringLiteral(T3) [IsExpression, HasAttributes] @Headers/FIREventNames.h (18:5)
        //     "add_payment_info": StringLiteral(T4) [IsExpression] @Headers/FIREventNames.h (18:6)
        LLiteral := '';
        LLiteralKind := TCursorKind.UnexposedDecl;
        LCursor.VisitChildren(
          function(const ACursor, AParent: TCursor): TChildVisitResult
          begin
            if IsSupportedDefinedConst(ACursor) then
            begin
              LLiteralKind := ACursor.Kind;
              LLiteral := ACursor.Spelling.DeQuotedString('"');
              Result := TChildVisitResult.Break;
            end
            else
              Result := TChildVisitResult.Continue;
          end
        );
        if not LLiteral.IsEmpty then
        begin
          case LLiteralKind of
            TCursorKind.ObjCStringLiteral:
              Writer.WriteLn('Result := StrToNSStr(''%s'');', [LLiteral]);
          else
            Writer.WriteLn('Result := OctoidDeveloperError');
          end;
        end
        else
          Writer.WriteLn('Result := CouldNotFindConstantLiteral');
      end
      else if LCursor.IsDeclaration then
      begin
        // Special case for CGFloat
        if LTypeName.Equals('CGFloat') then
          WriteExportedConstCGFloat(LCursor.Spelling)
        else if LUnderlyingTypeKind in [TTypeKind.TypeDef, TTypeKind.Pointer] then
        begin
          if LTypeName.EndsWith('Ref') then
            Writer.WriteLn('Result := %s(CocoaPointerConst(%s, ''%s''));', [LTypeName, FProject.GetLibraryConstant, LCursor.Spelling])
          else
            Writer.WriteLn('Result := P%s(CocoaPointerConst(%s, ''%s''))^;', [LUnderylingTypeName, FProject.GetLibraryConstant, LCursor.Spelling]);
        end
        else
          Writer.WriteLn('Result := Cocoa%sConst(%s, ''%s'');', [LUnderylingTypeName, FProject.GetLibraryConstant, LCursor.Spelling]);
      end;
      Writer.Outdent;
      Writer.WriteLn('end;');
      Writer.WriteLn;
    end;
  end;
end;

procedure TObjCHeaderTranslator.WriteExportedConstCGFloat(const AConstName: string);
begin
  Writer.WriteLn('{$IF Defined(CPU32BITS)}');
  Writer.WriteLn('Result := CocoaSingleConst(%s, ''%s'');', [FProject.GetLibraryConstant, AConstName]);
  Writer.WriteLn('{$ELSEIF Defined(CPU64BITS)}');
  Writer.WriteLn('Result := CocoaDoubleConst(%s, ''%s'');', [FProject.GetLibraryConstant, AConstName]);
  Writer.WriteLn('{$ENDIF}');
end;

procedure TObjCHeaderTranslator.WriteExportedConstProtos;
var
  LCursor: TCursor;
  LTypeName, LConstType, LUnderylingTypeName: string;
  LUnderlyingType: TType;
begin
  for LCursor in FExportedConsts do
  begin
    if LCursor.IsDefinition then
      LConstType := 'Defined'
    else
      LConstType := 'Exported';
    if IsSupportedExportedConst(LCursor.CursorType) and not (LCursor.IsDefinition and not IsSupportedDefinedConst(LCursor)) then
    begin
      LTypeName := GetDelphiTypeName(LCursor.CursorType);
      LUnderlyingType := GetUnderlyingType(LCursor.CursorType);
      LUnderylingTypeName := GetDelphiTypeName(LUnderlyingType);
      // https://github.com/Embarcadero/octoid/issues/20
      if LUnderylingTypeName.Equals('P__CFString') or LUnderylingTypeName.Equals('CFStringRef') then
        LTypeName := 'NSString';
      // https://github.com/Embarcadero/octoid/issues/13
      if LCursor.IsDefinition and (LTypeName.Equals('NSString') or LTypeName.Equals('NSErrorDomain')) then
        FNeedsMacapiHelpers := True;
      Writer.WriteLn('function %s: %s;', [GetValidIdentifier(LCursor.Spelling), LTypeName]);
    end
    else if TObjCTranslateOption.UnsupportedConstTypeComments in FProject.ObjCTranslateOptions then
      Writer.WriteLn('// %s const %s has an unsupported type: %s', [LConstType, LCursor.Spelling, LCursor.CursorType.Spelling]);
  end;
  if FExportedConsts.Count > 0 then
    Writer.WriteLn;
end;

procedure TObjCHeaderTranslator.WriteForwardClasses;
var
  LCursor: TCursor;
begin
  for LCursor in FClasses do
    Writer.WriteLn('%s = interface;', [LCursor.Spelling]);
  if FClasses.Count > 0 then
    Writer.WriteLn;
end;

function TObjCHeaderTranslator.IsPlatformIOS: Boolean;
begin
  Result := FProject.TargetPlatform.Equals(cTargetPlatformIOS);
end;

function TObjCHeaderTranslator.IsPlatformMacOS: Boolean;
begin
  Result := FProject.TargetPlatform.Equals(cTargetPlatformMacOS);
end;

procedure TObjCHeaderTranslator.WriteImplementationUsesClause;
begin
  Writer.WriteLn;
  Writer.WriteLn('uses');
  Writer.Indent;
  if FNeedsMacapiHelpers then
    Writer.WriteLn('Macapi.Helpers,');
  if IsPlatformIOS then
    Writer.WriteLn('Posix.Dlfcn;');
  if IsPlatformMacOS then
    Writer.WriteLn('System.SysUtils;');
  Writer.Outdent;
  Writer.WriteLn;
  Writer.WriteLn('var');
  Writer.Indent;
  Writer.WriteLn('%sModule: THandle;', [FProject.GetFrameworkName]);
  Writer.Outdent;
  Writer.WriteLn;
end;

procedure TObjCHeaderTranslator.WriteImplementationContent;
begin
  WriteExportedConstFunctions;
  Writer.WriteLn('initialization');
  Writer.Indent;
  if IsPlatformIOS then
    Writer.WriteLn('%sModule := dlopen(MarshaledAString(%s), RTLD_LAZY);', [FProject.GetFrameworkName, FProject.GetLibraryConstant]);
  if IsPlatformMacOS then
    Writer.WriteLn('%sModule := LoadLibrary(%s);', [FProject.GetFrameworkName, FProject.GetLibraryConstant]);
  Writer.Outdent;
  Writer.WriteLn;
  Writer.WriteLn('finalization');
  Writer.Indent;
  if IsPlatformIOS then
    Writer.WriteLn('dlclose(%sModule);',  [FProject.GetFrameworkName]);
  if IsPlatformMacOS then
  begin
    Writer.WriteLn('if %sModule <> 0 then', [FProject.GetFrameworkName]);
    Writer.Indent;
    Writer.WriteLn('FreeLibrary(%sModule);', [FProject.GetFrameworkName]);
    Writer.Outdent;
  end;
  Writer.Outdent;
end;

procedure TObjCHeaderTranslator.WriteInterfaceDeclarationHeader(const AClassName, AClassRef: string; const AIsClass: Boolean);
const
  cClassSuffix: array[Boolean] of string = ('', 'Class');
begin
  Writer.WriteLn('%s%s = interface(%s%s)', [AClassName, cClassSuffix[AIsClass], AClassRef, cClassSuffix[AIsClass]]);
  Writer.Indent;
  Writer.WriteLn('[''%s'']', [TGUID.NewGuid.ToString]);
end;

function TObjCHeaderTranslator.WriteInterfaceClassType(const ACursor: TCursor): Boolean;
var
  LIsClass, LIsAnonymousStruct, LHasClassRef: Boolean;
  LStructName: string;
  LCategory: TCursor;
begin
  LHasClassRef := False;
  if not Writer.IsAtSectionStart then
    Writer.WriteLn;
  LIsClass := ACursor.Kind = TCursorKind.ObjCInterfaceDecl;
  if LIsClass then
    LStructName := GetDelphiTypeName(ACursor.CursorType, False, @LIsAnonymousStruct)
  else
  begin
    LHasClassRef := True;
    LStructName := ACursor.Spelling;
    Writer.WriteLn('%s = interface(IObjectiveC)', [LStructName]);
    Writer.Indent;
    Writer.WriteLn('[''%s'']', [TGUID.NewGuid.ToString]);
  end;
  FMethods.Clear;
  // Only for when generating import for Foundation frameworks
  // if LIsClass and not HasObjCClassRef(ACursor) then
  //   WriteInterfaceDeclarationHeader(LStructName, 'NSObject', True);
  ACursor.VisitChildren(
    function(const AChildCursor, AParent: TCursor): TChildVisitResult
    var
      LKind: TCursorKind;
      LChildSpelling: string;
    begin
      LKind := AChildCursor.Kind;
      LChildSpelling := AChildCursor.Spelling;
      case LKind of
        TCursorKind.ObjCClassRef:
        begin
          if LIsClass and not LStructName.Equals(LChildSpelling) then
          begin
            LHasClassRef := True;
            WriteInterfaceDeclarationHeader(LStructName, LChildSpelling, True);
          end;
        end;
        TCursorKind.ObjCProtocolRef:
        begin
          //
        end;
        TCursorKind.ObjCClassMethodDecl:
        begin
          FMethods.AddMethod(GetDelphiMethod(AChildCursor, LStructName, True, LIsClass));
        end;
        TCursorKind.ObjCInstanceMethodDecl:
        begin
          if not LIsClass then
            FMethods.AddMethod(GetDelphiMethod(AChildCursor, LStructName, False, LIsClass));
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
  if LHasClassRef then
  begin
    // Obtain class methods from any categories
    for LCategory in GetClassCategories(LStructName) do
    begin
      LCategory.VisitChildren(
        function(const ACursor, AParent: TCursor): TChildVisitResult
        begin
          case ACursor.Kind of
            TCursorKind.ObjCClassMethodDecl:
            begin
              FMethods.AddMethod(GetDelphiMethod(ACursor, LStructName, True, LIsClass));
            end;
          end;
          Result := TChildVisitResult.Continue;
        end
      );
    end;
    WriteDelphiMethods;
    Writer.Outdent;
    Writer.WriteLn('end;');
    Writer.WriteLn;
  end
  else
    Writer.WriteLn('// ***** Could not find class reference for declaration: ' + GetCursorSource(ACursor, True));
  Result := LHasClassRef;
end;

procedure TObjCHeaderTranslator.WriteInterfaceInstanceType(const ACursor: TCursor);
var
  LStructName: string;
  LIsAnonymousStruct: Boolean;
  LCategory: TCursor;
  LHasClassRef: Boolean;
begin
  LHasClassRef := False;
  LStructName := GetDelphiTypeName(ACursor.CursorType, False, @LIsAnonymousStruct);
  FMethods.Clear;
  // Only for when generating import for Foundation frameworks
  // if not HasObjCClassRef(ACursor) then
  //   WriteInterfaceDeclarationHeader(LStructName, 'NSObject', False);
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      case ACursor.Kind of
        TCursorKind.ObjCSuperClassRef:
        begin
          LHasClassRef := True;
          WriteInterfaceDeclarationHeader(LStructName, ACursor.Spelling, False);
        end;
        TCursorKind.ObjCInstanceMethodDecl:
        begin
          if not ACursor.Spelling.Equals('init') then
            FMethods.AddMethod(GetDelphiMethod(ACursor, LStructName, False, True));
        end;
      end;
      Result := TChildVisitResult.Continue;
    end
  );
  if LHasClassRef then
  begin
    // Obtain instance methods from any categories
    for LCategory in GetClassCategories(LStructName) do
    begin
      LCategory.VisitChildren(
        function(const ACursor, AParent: TCursor): TChildVisitResult
        begin
          case ACursor.Kind of
            TCursorKind.ObjCInstanceMethodDecl:
            begin
              FMethods.AddMethod(GetDelphiMethod(ACursor, LStructName, False, True));
            end;
          end;
          Result := TChildVisitResult.Continue;
        end
      );
    end;
    WriteDelphiMethods;
    Writer.Outdent;
    Writer.WriteLn('end;');
    Writer.WriteLn('T%s = class(TOCGenericImport<%sClass, %s>) end;', [LStructName, LStructName, LStructName]);
  end;
end;

(*
function TObjCHeaderTranslator.HasObjCClassRef(const ACursor: TCursor): Boolean;
var
  LResult: Boolean;
begin
  LResult := False;
  ACursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      if ACursor.Kind = TCursorKind.ObjCClassRef then
        LResult := True;
      Result := TChildVisitResult.Continue;
    end
  );
  Result := LResult;
end;
*)

procedure TObjCHeaderTranslator.WriteInterfaceType(const ACursor: TCursor);
begin
  if not Writer.IsAtSectionStart then
    Writer.WriteLn;
  if WriteInterfaceClassType(ACursor) then
    WriteInterfaceInstanceType(ACursor);
  Writer.WriteLn;
end;

procedure TObjCHeaderTranslator.WriteInterfaceUsesClause;
var
  I: Integer;
  LNamespace: string;
begin
  if FProject.TargetPlatform.Equals(cTargetPlatformMacOS) then
    LNamespace := 'Macapi'
  else
    LNamespace := 'iOSapi';
  Writer.StartSection('uses');
  Writer.Indent;
  if SourceUnitName.Equals('Macapi.CoreFoundation') then
    Writer.Write('Macapi.ObjectiveC')
  else
    Writer.Write('Macapi.ObjectiveC, Macapi.CoreFoundation');
  if not SourceUnitName.EndsWith('.Foundation') and (FInterfaceUnits.IndexOf(LNamespace + '.Foundation') = -1) and (FIncludedFrameworks.IndexOf('Foundation') = -1) then
    Writer.Write(', %s.Foundation', [LNamespace]);
  for I := 0 to FInterfaceUnits.Count - 1 do
  begin
    Writer.Write(', ');
    Writer.Write(FInterfaceUnits[I]);
  end;
  Writer.Write(', %s.CocoaTypes', [LNamespace]);
  for I := 0 to FIncludedFrameworks.Count - 1 do
  begin
    // CoreFoundation already included
    if not FIncludedFrameworks[I].Equals('CoreFoundation') then
    begin
      Writer.Write(', ');
      Writer.Write('%s.%s', [LNamespace, FIncludedFrameworks[I]]);
    end;
  end;
  Writer.WriteLn(';');
  Writer.Outdent;
  Writer.EndSection;
end;

function TObjCHeaderTranslator.IsAnonymous(const AName: string): Boolean;
begin
  Result := (AName.IndexOf('(anonymous ') >= 0);
end;

function TObjCHeaderTranslator.IsConstIdentifier(const AName: string): Boolean;
const
  cConstIdentifiers: array[0..0] of string = ('NSIntegerMax');
begin
  Result := MatchText(AName, cConstIdentifiers);
end;

function TObjCHeaderTranslator.RemoveQualifiers(const ATypeName: string): string;
var
  HasPrefix: Boolean;
begin
  { Remove leading "struct", "const" or "volatile" }
  Result := ATypeName;
  repeat
    HasPrefix := False;

    if (Result.StartsWith('struct ')) then
    begin
      Result := Result.Substring(7);
      HasPrefix := True;
    end;

    if (Result.StartsWith('union ')) then
    begin
      Result := Result.Substring(6);
      HasPrefix := True;
    end;

    if (Result.StartsWith('enum ')) then
    begin
      Result := Result.Substring(5);
      HasPrefix := True;
    end;

    if (Result.StartsWith('const ')) then
    begin
      Result := Result.Substring(6);
      HasPrefix := True;
    end;

    if (Result.StartsWith('volatile ')) then
    begin
      Result := Result.Substring(9);
      HasPrefix := True;
    end;
  until (not HasPrefix);
end;

function TObjCHeaderTranslator.CanIncludeCursor(const ACursor: TCursor): Boolean;
var
  LFileName, LFramework, LSpelling: string;
begin
  CheckTypeMaps(ACursor);
  LFileName := GetCursorFileName(ACursor);
  // Is from the target framework..
  Result := LFileName.StartsWith(FProject.FrameworkDirectory) or LFileName.Contains('usr\include');
  if (ACursor.Kind = TCursorKind.InclusionDirective) and LFileName.StartsWith(FProject.FrameworkDirectory) then
  begin
    LSpelling := ACursor.Spelling;
    LFramework := LSpelling.Substring(0, Pos('/', LSpelling) - 1);
    if not LFramework.IsEmpty and LFramework.Chars[0].IsUpper and not SameText(LFramework, FProject.FrameworkName)
      and (FIncludedFrameworks.IndexOf(LFramework) = -1) then
      FIncludedFrameworks.Add(LFramework);
  end;
end;

function TObjCHeaderTranslator.CreateCombinedHeaderFile: Boolean;
var
  Option: TSearchOption;
  Writer: TStreamWriter;
  HeaderFiles: TStringDynArray;
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

function TObjCHeaderTranslator.Translate: ITranslationUnit;
var
  Args: TArray<string>;
  Options: TTranslationUnitFlags;
  DiagOpts: TDiagnosticDisplayOptions;
  Diag: IDiagnostic;
  I, ErrorCount: Integer;
  LMsg, LPath: string;
  LMissing, LParts, LErrors: TArray<string>;
begin
  if not CreateCombinedHeaderFile then
    Exit(nil); // <======
  DoMessage('Parsing header files...');
  Options := [TTranslationUnitFlag.DetailedPreprocessingRecord, TTranslationUnitFlag.SkipFunctionBodies];
  if (FProject.IgnoreParseErrors) then
    Include(Options, TTranslationUnitFlag.KeepGoing);
  Args := FProject.CmdLineArgs;
  Args := Args + ['-I' + FProject.FrameworkDirectory];
  for LPath in TDirectory.GetDirectories(FProject.FrameworkDirectory, '*', TSearchOption.soAllDirectories) do
    Args := Args + ['-I' + LPath];
  Result := TranslationIndex.ParseTranslationUnit(FCombinedHeaderFilename, Args, [], Options);
  if Result = nil then
    Exit; // <======
  DiagOpts := GetDefaultDiagnosticDisplayOptions;
  ErrorCount := 0;
  for I := 0 to Result.DiagnosticCount - 1 do
  begin
    Diag := Result.Diagnostics[I];
    if (Diag.Severity >= TDiagnosticSeverity.Error) then
    begin
      Inc(ErrorCount);
      LMsg :=  Diag.Format(DiagOpts);
      // Interested in errors for the target framework only
      // For "file not found", the translation may succeed anyway, so perhaps have an option to ignore this error
      if { not FProject.FrameworkName.Contains('Foundation') and LMsg.Contains(FProject.FrameworkName + '.framework') and} LMsg.Contains('file not found') then
        LMissing := LMissing + [LMsg]
      else
        DoMessage(StringReplace(LMsg, FProject.SdkRoot, '<SdkRoot>', [rfIgnoreCase]));
    end;
  end;
  if (ErrorCount = 0) then
    DoMessage('Parsed header files without errors')
  else
    DoMessage('Parsed header files with %d error(s)', [ErrorCount]);
  if Length(LMissing) > 0 then
  begin
    Result := nil;
    DoMessage(#13#10'FATAL errors:');
    for LMsg in LMissing do
    begin
      DoMessage(LMsg);
      LParts := LMsg.Split(['error: ']);
      if Length(LParts) > 1 then
        LErrors := LErrors + [LParts[1]];
    end;
    if Length(LErrors) > 0 then
      ProcessErrors(LErrors);
  end;
end;

procedure TObjCHeaderTranslator.ProcessErrors(const AErrors: TArray<string>);
var
  LError, LFileName, LFilePath, LFolderPath, LFrameworkName, LHeaderName: string;
  LParts, LFrameworks, LFolders: TArray<string>;
begin
  DoMessage('');
  for LError in AErrors do
  begin
    if LError.EndsWith('file not found') then
    begin
      LParts := LError.Split(['''']);
      if Length(LParts) > 1 then
      begin
        // e.g. ModelIO/ModelIO.h
        // e.g. cups/ppd.h
        LFilePath := LParts[1];
        LParts := LFilePath.Split(['/']);
        if Length(LParts) > 1 then
        begin
          LFileName := LParts[Length(LParts) - 1];
          // e.g. ModelIO.h will become ModelIO
          LHeaderName := TPath.GetFileNameWithoutExtension(LFileName);
          // If the missing header has a capital for the first letter, it's likely to be a missing framework
          LFrameworkName := LParts[Length(LParts) - 2];
          if LFrameworkName.Chars[0].IsUpper then
          begin
            if IndexStr(LFrameworkName, LFrameworks) = -1 then
              LFrameworks := LFrameworks + [LFrameworkName];
          end
          else
          begin
            Delete(LParts, Length(LParts) - 1, 1);
            LFolderPath := string.Join('/', LParts);
            if IndexStr(LFolderPath, LFolders) = -1 then
              LFolders := LFolders + [LFolderPath];
          end;
        end;
      end;
    end;
  end;
  if Length(LFrameworks) > 0 then
  begin
    DoMessage('The following appear to be frameworks not imported into the SDK, or are dependent frameworks that are missing:');
    DoMessage(string.Join(', ', LFrameworks));
  end;
  if Length(LFolders) > 0 then
  begin
    DoMessage('The following appear to be folders missing from below \usr\include in the SDK:');
    DoMessage(string.Join(#13#10, LFolders));
  end;
  if (Length(LFrameworks) > 0) or (Length(LFolders) > 0) then
    DoMessage('');
end;

end.

