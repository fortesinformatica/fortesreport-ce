unit ufrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmPrincipal = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ufrmReport;

{$R *.dfm}

procedure TfrmPrincipal.Button1Click(Sender: TObject);
var
  value : string;
begin
  repeat
    value := inputbox('Texto para inserir', 'Por favor informe algo', 'Linha x');
  until value.Trim <> EmptyStr;

  ListBox1.AddItem(value, Button1);
end;

procedure TfrmPrincipal.Button2Click(Sender: TObject);
begin
  frmReport.RLReport1.Preview();
end;

end.
