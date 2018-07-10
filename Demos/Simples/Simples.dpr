program Simples;

uses
  Vcl.Forms,
  ufrmPrincipal in 'ufrmPrincipal.pas' {frmPrincipal},
  ufrmReport in 'ufrmReport.pas' {frmReport},
  ufrmReportImagem in 'ufrmReportImagem.pas' {frmReportImagem};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TfrmReport, frmReport);
  Application.CreateForm(TfrmReportImagem, frmReportImagem);
  Application.Run;
end.
