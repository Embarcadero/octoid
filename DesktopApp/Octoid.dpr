program Octoid;

{$I Octoid.Debug.inc}

uses
  Octoid.Include,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Octoid.View.Main in 'Octoid.View.Main.pas' {MainView},
  Octoid.View.Output in 'Octoid.View.Output.pas' {OutputView},
  Octoid.View.Options in 'Octoid.View.Options.pas' {OptionsView};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Onyx Blue');
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
