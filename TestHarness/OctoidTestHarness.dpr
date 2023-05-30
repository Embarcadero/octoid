program OctoidTestHarness;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  Octoid.CustomTranslator in '..\Octoid.CustomTranslator.pas',
  Octoid.ObjCHeaderTranslator in '..\Octoid.ObjCHeaderTranslator.pas',
  Octoid.Consts in '..\Octoid.Consts.pas',
  Octoid.SourceReader in '..\Octoid.SourceReader.pas',
  Octoid.Command in '..\Octoid.Command.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
