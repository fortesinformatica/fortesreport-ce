unit ufrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs;

type
  TfrmPrincipal = class(TForm)
    btnRelatorioTexto: TButton;
    OpenDialog1: TOpenDialog;
    btnRelatorioImagem: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure btnRelatorioTextoClick(Sender: TObject);
    procedure btnRelatorioImagemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ufrmReport, ufrmReportImagem;

{$R *.dfm}

procedure TfrmPrincipal.btnRelatorioImagemClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    frmReportImagem.RLImage1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    frmReportImagem.RLReport1.Preview();
  end;
end;

procedure TfrmPrincipal.btnRelatorioTextoClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    frmReport.rlmmTexto.Lines.LoadFromFile(OpenDialog1.FileName);
    frmreport.RLReport1.Preview();
  end;
end;

end.
