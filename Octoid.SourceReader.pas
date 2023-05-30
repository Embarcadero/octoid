unit Octoid.SourceReader;

interface

uses
  System.Classes;

type
  TSourceReader = class(TObject)
  private
    FCurrentLine: Integer;
    FCurrentText: string;
    FFileName: string;
    FReader: TStreamReader;
    procedure CheckReader(const AFileName: string);
    function ReadLine(const ALine: Integer; var AText: string): Boolean;
  public
    destructor Destroy; override;
    function ReadSubstring(const AFileName: string; const ALine, AColStart, AColEnd: Integer): string;
  end;

implementation

uses
  System.SysUtils;

{ TSourceReader }

procedure TSourceReader.CheckReader(const AFileName: string);
begin
  if not SameText(AFileName, FFileName) then
  begin
    FReader.Free;
    FReader := TStreamReader.Create(AFileName, TEncoding.ANSI);
    FReader.Rewind;
    FFileName := AFileName;
    FCurrentLine := 0;
  end;
end;

destructor TSourceReader.Destroy;
begin
  FReader.Free;
  inherited;
end;

function TSourceReader.ReadLine(const ALine: Integer; var AText: string): Boolean;
var
  I: Integer;
  LText: string;
begin
  if ALine > FCurrentLine then
  begin
    for I := 1 to (ALine - FCurrentLine) do
    begin
      if not FReader.EndOfStream then
      begin
        LText := FReader.ReadLine;
        Inc(FCurrentLine);
      end
      else
        Exit(False); // <======
    end;
    AText := LText;
    FCurrentText := AText;
  end
  else if ALine < FCurrentLine then
  begin
    FReader.Rewind;
    FCurrentLine := 0;
    ReadLine(ALine, AText);
  end
  else
    AText := FCurrentText;
  Result := True;
end;

function TSourceReader.ReadSubstring(const AFileName: string; const ALine, AColStart, AColEnd: Integer): string;
var
  LText: string;
begin
  Result := '';
  CheckReader(AFileName);
  if ReadLine(ALine, LText) then
  begin
    if AColEnd > 0 then
      Result := LText.Substring(AColStart - 1, AColEnd - AColStart + 1)
    else
      Result := LText.Substring(AColStart - 1);
  end;
end;

end.
