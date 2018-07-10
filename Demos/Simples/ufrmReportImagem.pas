unit ufrmReportImagem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RLReport;

type
  TfrmReportImagem = class(TForm)
    RLReport1: TRLReport;
    RLBand1: TRLBand;
    RLLabel1: TRLLabel;
    RLBand2: TRLBand;
    RLImage1: TRLImage;
    procedure RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FMoreData : Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReportImagem: TfrmReportImagem;

implementation

{$R *.dfm}

procedure TfrmReportImagem.FormCreate(Sender: TObject);
begin
  FMoreData := true;
  RLImage1.AutoSize := true;
end;

procedure TfrmReportImagem.RLReport1NeedData(Sender: TObject;
  var MoreData: Boolean);
begin
  MoreData := FMoreData;
  FMoreData := False;
end;

end.
