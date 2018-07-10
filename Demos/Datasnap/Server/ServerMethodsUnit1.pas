unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  DataSnap.DSServer, DataSnap.DSAuth;

type
  TServerMethods1 = class(TDSServerModule)
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function RelatorioEmpregados(out size: int64): TStream;
  end;

implementation

{$R *.dfm}

uses System.StrUtils, ufrmReport;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.RelatorioEmpregados(out size: int64): TStream;
var
  ArquivoPDF: string;
begin
  frmReport.RLReport1.Prepare;

  ArquivoPDF := ExtractFilePath(ParamStr(0)) + 'relatorio.pdf';
  frmReport.RLPDFFilter1.ShowProgress := false;
  frmReport.RLPDFFilter1.FileName := ArquivoPDF;
  frmReport.RLPDFFilter1.FilterPages(frmReport.RLReport1.Pages);

  Result := TFileStream.Create(ArquivoPDF, fmOpenRead or fmShareDenyNone);
  size := Result.size;
  Result.Position := 0;
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.
