unit ufrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmPrincipal = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ClientModuleUnit1, Winapi.ShellAPI;

{$R *.dfm}

procedure TfrmPrincipal.Button1Click(Sender: TObject);
var
  RelatorioStream: TStream;

  memoria: TMemoryStream;
  Buffer: PByte;
  BufSize: integer;
  BytesLido: integer;

  tamanho: int64;
  RelatorioPDF : string;
begin
  RelatorioPDF := ExtractFilePath(ParamStr(0)) + 'relatorio.pdf';
  if FileExists(RelatorioPDF) then
    DeleteFile(RelatorioPDF);
  BufSize := 1024;
  try
    memoria := TMemoryStream.Create;
    GetMem(Buffer, BufSize);

    RelatorioStream := ClientModule1.ServerMethods1Client.
      RelatorioEmpregados(tamanho);
    RelatorioStream.Position := 0;
    if tamanho <> 0 then
    begin
      repeat
        BytesLido := RelatorioStream.Read(pointer(Buffer)^, BufSize);
        if BytesLido > 0 then
          memoria.WriteBuffer(pointer(Buffer)^, BufSize);

        Application.ProcessMessages;
      until (BytesLido < BufSize);

      memoria.SaveToFile(RelatorioPDF);
    end;

  finally
    FreeMem(Buffer,BufSize);
    FreeAndNil(memoria);
  end;

  if FileExists(RelatorioPDF) then
    ShellExecute(Handle, nil, PChar(RelatorioPDF), nil,  nil, SW_SHOWNORMAL);

end;

end.
