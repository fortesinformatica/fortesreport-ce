unit ufrmReport;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RLReport;

type
  TfrmReport = class(TForm)
    RLReport1: TRLReport;
    RLBand1: TRLBand;
    RLLabel1: TRLLabel;
    RLBand2: TRLBand;
    rlmmTexto: TRLMemo;
    RLBand3: TRLBand;
    RLSystemInfo1: TRLSystemInfo;
    RLSystemInfo2: TRLSystemInfo;
    procedure FormCreate(Sender: TObject);
    procedure RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
  private
    FMoreData : Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReport: TfrmReport;

implementation

{$R *.dfm}

procedure TfrmReport.FormCreate(Sender: TObject);
begin
  FMoreData := true;
end;

procedure TfrmReport.RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  MoreData := FMoreData;
  FMoreData := false;
end;

end.
