program Octoid;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Octoid.Command in '..\Octoid.Command.pas',
  Octoid.CustomTranslator in '..\Octoid.CustomTranslator.pas',
  Octoid.ObjCHeaderTranslator in '..\Octoid.ObjCHeaderTranslator.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    TOctoidCommand.RunCommand;
    if DebugHook > 0 then
      Readln;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
