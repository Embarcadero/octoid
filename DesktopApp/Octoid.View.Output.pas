unit Octoid.View.Output;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TOutputView = class(TForm)
    OutputMemo: TMemo;
    BottomPanel: TPanel;
    CloseButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AddMessage(const AMsg: string);
    procedure ClearMessages;
    procedure ScrollToBottom;
  end;

var
  OutputView: TOutputView;

implementation

{$R *.dfm}

{ TOutputView }

procedure TOutputView.AddMessage(const AMsg: string);
begin
  OutputMemo.Lines.Add(AMsg);
end;

procedure TOutputView.ClearMessages;
begin
  OutputMemo.Lines.Clear;
end;

procedure TOutputView.CloseButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TOutputView.ScrollToBottom;
begin
  SendMessage(OutputMemo.Handle, EM_LINESCROLL, 0, OutputMemo.Lines.Count);
end;

end.
