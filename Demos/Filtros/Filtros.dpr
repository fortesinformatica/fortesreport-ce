program Filtros;

uses
  Vcl.Forms,
  ufrmPrincipal in 'ufrmPrincipal.pas' {frmPrincipal},
  udmdados in 'udmdados.pas' {dmdados: TDataModule},
  ufrmReport in 'ufrmReport.pas' {frmReport};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(Tdmdados, dmdados);
  Application.CreateForm(TfrmReport, frmReport);
  Application.Run;
end.
