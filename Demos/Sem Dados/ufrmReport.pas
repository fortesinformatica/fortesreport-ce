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
    RLMemo1: TRLMemo;
    procedure RLMemo1BeforePrint(Sender: TObject; var AText: string;
      var PrintIt: Boolean);
    procedure RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FCount : integer;
    FMoreData : boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReport: TfrmReport;

implementation

uses
  ufrmPrincipal;

{$R *.dfm}

procedure TfrmReport.FormCreate(Sender: TObject);
begin
  FCount := -1;
  FMoreData := true;
end;

procedure TfrmReport.RLMemo1BeforePrint(Sender: TObject; var AText: string;
  var PrintIt: Boolean);
begin
  if FCount < frmPrincipal.ListBox1.Count  then
    AText := frmPrincipal.ListBox1.Items[FCount];
end;

procedure TfrmReport.RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  MoreData := FMoreData;
  inc(FCount);
  FMoreData := FCount < frmPrincipal.ListBox1.Count ;
end;

end.
