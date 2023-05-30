unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    MessageMemo: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    FolderFileOpenDialog: TFileOpenDialog;
    OutputMemo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure DumperMessageHandler(const AMsg: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  AstDumper;

const
  { Whether to include system declarations in the AST }
  INCLUDE_SYSTEM_DECLS = False;

const
  { Whether source is C++ }
  IS_CPP = True;

const
  { Additional include directories (comma-seperated) }
  INCLUDE_DIRS = '';

const
  cSDKUserIncludeFolder = 'usr\include\';
  cSDKUserIncludeTypesFolder = 'usr\include\_types\';
  cSDKFrameworksFolder = 'System\Library\Frameworks\';

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  LDumper: TAstDumper;
  LClangIncludePath, LSdkRoot: string;
begin
  if not FolderFileOpenDialog.Execute then
    Exit; // <======
  LClangIncludePath := 'C:\Program Files\LLVM\lib\clang\7.0.0\include\';
  LSdkRoot := 'C:\Users\Dubten\Documents\Embarcadero\Studio\SDKs\MacOSX10.14.sdk';
  MessageMemo.Lines.Clear;
  LDumper := TAstDumper.Create(FolderFileOpenDialog.FileName, INCLUDE_SYSTEM_DECLS, IS_CPP, INCLUDE_DIRS);
  try
    LDumper.OutputFolder := 'Z:\Temp\AstDump';
    LDumper.OnMessage := DumperMessageHandler;
    LDumper.AddCmdLineArg(Format('-isystem%s', [TPath.Combine(LSdkRoot, cSDKUserIncludeFolder)]));
    LDumper.AddCmdLineArg(Format('-isystem%s', [LClangIncludePath]));
    LDumper.AddCmdLineArg(Format('-F%s', [TPath.Combine(LSdkRoot, cSDKFrameworksFolder)]));
    LDumper.AddCmdLineArg('-x');
    LDumper.AddCmdLineArg('objective-c');
    LDumper.AddCmdLineArg('-target');
    LDumper.AddCmdLineArg('x86_64-apple-darwin10');
    LDumper.AddCmdLineArg('-ferror-limit=1000');
    LDumper.Run;
    OutputMemo.Lines.LoadFromFile(TPath.Combine(LDumper.OutputFolder, 'Ast.txt'));
  finally
    LDumper.Free;
  end;
end;

procedure TForm1.DumperMessageHandler(const AMsg: string);
begin
  MessageMemo.Lines.Add(AMsg);
end;

end.
