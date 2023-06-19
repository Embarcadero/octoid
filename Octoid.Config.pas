unit Octoid.Config;

interface

type
  TBannerPosition = (BeforeUnit, BeforeInterface);

  TJsonConfig = class(TObject)
  public
    class procedure CreateFromFile<T: TJsonConfig, constructor>(out AObject: T);
    class function GetConfigFileName: string; virtual;
  public
    procedure Save;
  end;

  TSDKPathMRU = TArray<string>;

  TOctoidConfig = class(TJsonConfig)
  private
    class var FCurrent: TOctoidConfig;
    class destructor DestroyClass;
    class function GetCurrent: TOctoidConfig; static;
  private
    FAdditionalOptions: string;
    FBannerPosition: TBannerPosition;
    FDeprecationCommentFirst: Boolean;
    FErrorLimit: Integer;
    FIncludeConstTypeComments: Boolean;
    FIncludeDeprecationComments: Boolean;
    FIncludeTodoComments: Boolean;
    FOutputPath: string;
    FSDKPathMRU: TSDKPathMRU;
    FSDKPath: string;
    FSelectedFramework: string;
    FStyleName: string;
    FUseBannerAsIs: Boolean;
    procedure CheckSDKPathMRU;
  public
    class function GetBannerFileName: string;
    class function GetConfigFileName: string; override;
    class function GetConfigPath: string;
    class property Current: TOctoidConfig read GetCurrent;
  public
    property AdditionalOptions: string read FAdditionalOptions write FAdditionalOptions;
    property BannerPosition: TBannerPosition read FBannerPosition write FBannerPosition;
    property DeprecationCommentFirst: Boolean read FDeprecationCommentFirst write FDeprecationCommentFirst;
    property ErrorLimit: Integer read FErrorLimit write FErrorLimit;
    property IncludeConstTypeComments: Boolean read FIncludeConstTypeComments write FIncludeConstTypeComments;
    property IncludeDeprecationComments: Boolean read FIncludeDeprecationComments write FIncludeDeprecationComments;
    property IncludeTodoComments: Boolean read FIncludeTodoComments write FIncludeTodoComments;
    property SDKPathMRU: TSDKPathMRU read FSDKPathMRU write FSDKPathMRU;
    property OutputPath: string read FOutputPath write FOutputPath;
    property SDKPath: string read FSDKPath write FSDKPath;
    property SelectedFramework: string read FSelectedFramework write FSelectedFramework;
    property StyleName: string read FStyleName write FStyleName;
    property UseBannerAsIs: Boolean read FUseBannerAsIs write FUseBannerAsIs;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, REST.Json, System.Math,
  Octoid.Consts;

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

class destructor TOctoidConfig.DestroyClass;
begin
  FCurrent.Free;
end;

class function TOctoidConfig.GetBannerFileName: string;
begin
  Result := TPath.Combine(GetConfigPath, 'banner.txt');
end;

class function TOctoidConfig.GetConfigFileName: string;
begin
  Result := TPath.Combine(GetConfigPath, 'config.json');
end;

class function TOctoidConfig.GetConfigPath: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'Octoid')
end;

class function TOctoidConfig.GetCurrent: TOctoidConfig;
begin
  if FCurrent = nil then
    CreateFromFile<TOctoidConfig>(FCurrent);
  FCurrent.ErrorLimit := Max(cErrorLimitDefault, FCurrent.ErrorLimit);
  FCurrent.CheckSDKPathMRU;
  Result := FCurrent;
end;

procedure TOctoidConfig.CheckSDKPathMRU;
var
  I: Integer;
begin
  for I := Length(FSDKPathMRU) - 1 downto 0 do
  begin
    if not TDirectory.Exists(FSDKPathMRU[I]) then
      Delete(FSDKPathMRU, I, 1);
  end;
end;

end.
