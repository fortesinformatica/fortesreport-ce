{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              Márcio Martins                                  }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informações você pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descrição...
******************************************************************************}

unit uPrincipal;

interface

uses
  JclIDEUtils, JclCompilerUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, pngimage, ShlObj,
  JvExControls, JvAnimatedImage, JvGIFCtrl, JvWizard, JvWizardRouteMapNodes,
  JvExComCtrls, JvComCtrls, JvCheckTreeView, System.IOUtils,
  System.Types, Vcl.Imaging.jpeg, Vcl.CheckLst,
  uPlataformaAlvo;

type
  // Severidade das mensagens mostradas na lista de andamento da instalação.
  // Vai gravada no Objects[] de cada item e decide a cor no desenho.
  TNivelMensagem = (nmInfo, nmDestaque, nmSucesso, nmAviso, nmErro);

  TfrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizMapa: TJvWizardRouteMapNodes;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizPgObterFontes: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    wizPgFinalizar: TJvWizardInteriorPage;
    wizPgInicio: TJvWizardWelcomePage;
    Label4: TLabel;
    lstAlvos: TCheckListBox;
    btnMarcarTodos: TSpeedButton;
    btnDesmarcarTodos: TSpeedButton;
    chkSomenteLib: TCheckBox;
    chkInstalarIDE64: TCheckBox;
    Label2: TLabel;
    edtDirDestino: TEdit;
    Label6: TLabel;
    Label1: TLabel;
    edtURL: TEdit;
    lblInfoObterFontes: TLabel;
    lstMsgInstalacao: TListBox;
    pnlTopo: TPanel;
    Label9: TLabel;
    btnSelecDirInstall: TSpeedButton;
    Label3: TLabel;
    pgbInstalacao: TProgressBar;
    lblUrlForum1: TLabel;
    lblUrlfrce1: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    btnSVNCheckoutUpdate: TSpeedButton;
    btnInstalarfrce: TSpeedButton;
    ckbFecharTortoise: TCheckBox;
    btnVisualizarLogCompilacao: TSpeedButton;
    pnlInfoCompilador: TPanel;
    lbInfo: TListBox;
    Image1: TImage;
    Label7: TLabel;
    Label8: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure wizPgInicioNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure URLClick(Sender: TObject);
    procedure btnSelecDirInstallClick(Sender: TObject);
    procedure wizPrincipalCancelButtonClick(Sender: TObject);
    procedure wizPrincipalFinishButtonClick(Sender: TObject);
    procedure wizPgConfiguracaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnSVNCheckoutUpdateClick(Sender: TObject);
    procedure wizPgObterFontesEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure btnInstalarfrceClick(Sender: TObject);
    procedure wizPgInstalacaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnVisualizarLogCompilacaoClick(Sender: TObject);
    procedure wizPgInstalacaoEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure btnMarcarTodosClick(Sender: TObject);
    procedure btnDesmarcarTodosClick(Sender: TObject);
    procedure lstMsgInstalacaoDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FCountErros: Integer;
    FListaAlvos: TListaAlvos;
    // alvo (IDE + plataforma) e modo de compilação da vez
    FAlvo: TPlataformaAlvo;
    FModoAtual: TModoCompilacao;
    FPacoteAtual: String;
    FArquivoLog: String;
    FDirRoot: String;
    // IDEs já limpas nesta execução, pela chave de configuração
    FIDEsLimpasNestaExecucao: TStringList;
    // diretórios de instalação a expurgar dos paths da IDE: o atual e o que
    // estava gravado no .ini, para o caso de a pasta ter sido movida
    FTextosLimpeza: TStringList;
    FQtdeOrfaosRemovidos: Integer;

    // -- opções
    function ModosCompilacao: TModosCompilacao;
    function QuantidadeModosCompilacao: Integer;
    function QuantidadeAlvosMarcados: Integer;
    function ModoParaInstalacao: TModoCompilacao;
    function DirLibraryAtual: String;
    function PodeInstalarPacotesNaIDE: Boolean;

    // -- utilitários
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog(const ANomeAlvo: String): String;
    procedure WriteToTXT(const ArqTXT, AString: AnsiString;
      const AppendIfExists: Boolean = True; AddLineBreak: Boolean = True);
    procedure FazLog(const ATexto: String);
    procedure InformaSituacao(const AMensagem: String;
      const ANivel: TNivelMensagem = nmInfo);
    procedure InformaProgresso;
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;
    procedure PopularListaAlvos;
    function IsCheckOutJaFeito(const ADiretorio: String): Boolean;

    // -- pacotes
    function DiretorioDoPacote(const ANomePacote: String): String;
    procedure CompilarPacote(const ANomePacote: String);
    function RegistrarPacoteNaIDE(const AArquivoPacote: String): Boolean;
    function DependenciaRunTimeFaltando: String;

    // -- etapas da instalação
    procedure RemoverRegistrosOrfaosDeTodasIDEs;
    function LimpezaDaIDEJaFoiFeita: Boolean;
    procedure RemoverPacotesAntigos;
    procedure RemoverDiretoriosFRCEDoPath;
    procedure LimparArtefatosDaRaizDaPlataforma;
    procedure ApagarOutrosArquivosDaPastaLibrary;
    procedure CopiarOutrosArquivosParaPastaLibrary;
    procedure ColetarDiretoriosDeFontes(const ADirRaiz: String; ALista: TStrings);
    procedure AddLibrarySearchPath;
    procedure DeixarSomenteLib;
    procedure AdicionarPastaDosBplsNoPathDaIDE;
    procedure InstalarAlvo;
    procedure CompilarEInstalarPacotes;
    procedure InstalarOutrosRequisitos;

    // -- compilador
    procedure ConfiguraMetodosCompiladores;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure OutputCallLine(const Text: string);
    function RemoverPathsDuplicados(const APaths: String): String;
    procedure UsarResponseFileSeNecessario(Sender: TJclBorlandCommandLineTool);
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  SVN_Class,
  {$WARNINGS off} FileCtrl, {$WARNINGS on} ShellApi, IniFiles, StrUtils, Math;

const
  cPacoteRunTime = 'frce.dpk';
  cPacoteDesign  = 'dclfrce.dpk';

  // separador dos alvos marcados no arquivo .ini
  cSeparadorAlvos = '|';

{$R *.dfm}

procedure TfrmPrincipal.WriteToTXT(const ArqTXT, AString : AnsiString ;
   const AppendIfExists: Boolean; AddLineBreak : Boolean) ;
var
  FS : TFileStream ;
  LineBreak : AnsiString ;
begin
  if ArqTXT = '' then
    Exit;

  FS := TFileStream.Create( ArqTXT,
               IfThen( AppendIfExists and FileExists(ArqTXT),
                       Integer(fmOpenReadWrite), Integer(fmCreate)) or fmShareDenyWrite );
  try
     FS.Seek(0, soFromEnd);  // vai para EOF
     FS.Write(Pointer(AString)^,Length(AString));

     if AddLineBreak then
     begin
        LineBreak := sLineBreak;
        FS.Write(Pointer(LineBreak)^,Length(LineBreak));
     end ;
  finally
     FS.Free ;
  end;
end;

procedure TfrmPrincipal.FazLog(const ATexto: String);
begin
  WriteToTXT(AnsiString(FArquivoLog), AnsiString(ATexto));
end;

// mostra a mensagem na tela e grava no log do alvo que está sendo instalado
procedure TfrmPrincipal.InformaSituacao(const AMensagem: String;
  const ANivel: TNivelMensagem);
var
  LLinhas: TStringList;
  I: Integer;
begin
  FazLog(AMensagem);

  // Algumas mensagens trazem quebras de linha (mensagens de erro). Como a lista
  // é desenhada manualmente, cada linha vira um item para não aparecer o
  // caractere de controle no meio do texto.
  LLinhas := TStringList.Create;
  try
    LLinhas.Text := AMensagem;
    if LLinhas.Count = 0 then
      LLinhas.Add('');

    for I := 0 to LLinhas.Count - 1 do
      lstMsgInstalacao.Items.AddObject(LLinhas[I], TObject(NativeInt(Ord(ANivel))));
  finally
    LLinhas.Free;
  end;

  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  Application.ProcessMessages;
end;

// Cores por severidade, no estilo dos logs de console: verde para sucesso,
// vermelho para erro, laranja para aviso e azul para as demais informações.
procedure TfrmPrincipal.lstMsgInstalacaoDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  clLaranja     = TColor($000080FF);
  clVerdeEscuro = TColor($00107000);
var
  LLista: TListBox;
  LNivel: TNivelMensagem;
begin
  LLista := TListBox(Control);

  if (Index < 0) or (Index >= LLista.Items.Count) then
    Exit;

  LNivel := TNivelMensagem(NativeInt(LLista.Items.Objects[Index]));

  // A lista só exibe o andamento; a seleção é ignorada de propósito para que a
  // última linha (sempre selecionada, para rolar sozinha) mantenha a cor.
  LLista.Canvas.Brush.Color := clWindow;
  LLista.Canvas.FillRect(Rect);

  LLista.Canvas.Font := LLista.Font;
  case LNivel of
    nmDestaque:
      begin
        LLista.Canvas.Font.Color := clBlack;
        LLista.Canvas.Font.Style := [fsBold];
      end;
    nmSucesso:
      LLista.Canvas.Font.Color := clVerdeEscuro;
    nmAviso:
      LLista.Canvas.Font.Color := clLaranja;
    nmErro:
      begin
        LLista.Canvas.Font.Color := clRed;
        LLista.Canvas.Font.Style := [fsBold];
      end;
  else
    LLista.Canvas.Font.Color := clNavy;
  end;

  LLista.Canvas.TextOut(Rect.Left + 4, Rect.Top + 1, LLista.Items[Index]);
end;

procedure TfrmPrincipal.InformaProgresso;
begin
  if pgbInstalacao.Position < pgbInstalacao.Max then
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
  Application.ProcessMessages;
end;

// Modos de compilação. Não é uma escolha à parte: decorre de "Deixar somente a
// pasta Lib no Library Path".
//
//   marcada    -> Release e Debug. Os projetos do usuário compilam contra os
//                 .dcu prontos, então precisam do Release para o build normal
//                 e do Debug para quem marca "use debug .dcus".
//
//   desmarcada -> somente Release. Os fontes ficam no Library Path e cada
//                 projeto recompila o FortesReport com as opções dele,
//                 ignorando os .dcu daqui. Sobra como consumidor só a IDE, que
//                 carrega os BPL de Release. Compilar Debug nesse caso seria
//                 dobrar o tempo de instalação para gerar algo que ninguém abre.
function TfrmPrincipal.ModosCompilacao: TModosCompilacao;
begin
  if chkSomenteLib.Checked then
    Result := [mcRelease, mcDebug]
  else
    Result := [mcRelease];
end;

function TfrmPrincipal.QuantidadeModosCompilacao: Integer;
var
  LModo: TModoCompilacao;
begin
  Result := 0;
  for LModo := Low(TModoCompilacao) to High(TModoCompilacao) do
  begin
    if (LModo in ModosCompilacao) then
      Inc(Result);
  end;
end;

function TfrmPrincipal.QuantidadeAlvosMarcados: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to lstAlvos.Count - 1 do
  begin
    if lstAlvos.Checked[I] then
      Inc(Result);
  end;
end;

// Os BPL registrados na IDE são sempre os de Release, e o Release é compilado
// em qualquer configuração (veja ModosCompilacao).
function TfrmPrincipal.ModoParaInstalacao: TModoCompilacao;
begin
  Result := mcRelease;
end;

function TfrmPrincipal.DirLibraryAtual: String;
begin
  Result := FAlvo.DirLibraryPorModo(FModoAtual);
end;

function TfrmPrincipal.PodeInstalarPacotesNaIDE: Boolean;
begin
  Result := FAlvo.SuportaPacotesDesignTime;

  // instalar na IDE de 64 bits é opcional
  if Result and (FAlvo.Plataforma = bpWin64) then
    Result := chkInstalarIDE64.Checked;
end;

// retornar o path do aplicativo
function TfrmPrincipal.PathApp: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

// retornar o caminho completo para o arquivo .ini de configurações
function TfrmPrincipal.PathArquivoIni: String;
var
  NomeApp: String;
begin
  NomeApp := ExtractFileName(ParamStr(0));
  Result := PathApp + ChangeFileExt(NomeApp, '.ini');
end;

// O log é por alvo: cada combinação IDE + plataforma tem o seu arquivo, senão
// a instalação em várias IDEs escreveria tudo no mesmo lugar.
function TfrmPrincipal.PathArquivoLog(const ANomeAlvo: String): String;
begin
  Result := PathApp +
    'log_' + StringReplace(ANomeAlvo, ' ', '_', [rfReplaceAll]) + '.txt';
end;

// verificar se no caminho informado já existe o .svn indicando que o
// checkout já foi feito no diretorio
function TfrmPrincipal.IsCheckOutJaFeito(const ADiretorio: String): Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(ADiretorio) + '.svn')
end;

procedure TfrmPrincipal.PopularListaAlvos;
var
  I: Integer;
begin
  lstAlvos.Items.BeginUpdate;
  try
    lstAlvos.Items.Clear;
    for I := 0 to FListaAlvos.Count - 1 do
    begin
      // as versões que o FortesReport não suporta nem aparecem na lista
      if not FListaAlvos[I].EhSuportado then
        Continue;

      lstAlvos.Items.AddObject(FListaAlvos[I].NomeAlvo, FListaAlvos[I]);
    end;
  finally
    lstAlvos.Items.EndUpdate;
  end;
end;

// ler o arquivo .ini de configurações e setar os campos com os valores lidos
procedure TfrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  LMarcados: TStringList;
  LDirAnterior, LAlvoAntigo: String;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  LMarcados := TStringList.Create;
  try
    LDirAnterior := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', '');

    edtDirDestino.Text        := StrUtils.IfThen(LDirAnterior <> '', LDirAnterior,
                                        ExtractFilePath(ParamStr(0)));
    ckbFecharTortoise.Checked := ArqIni.ReadBool('CONFIG', 'FecharTortoise', True);
    chkSomenteLib.Checked     := ArqIni.ReadBool('CONFIG', 'DeixarSomentePastaLib', True);
    chkInstalarIDE64.Checked  := ArqIni.ReadBool('CONFIG', 'InstalarNaIDE64Bits', True);

    // se a instalação anterior foi feita em outra pasta, os paths dela também
    // precisam sair da configuração da IDE
    if (LDirAnterior <> '') then
      FTextosLimpeza.Add(IncludeTrailingPathDelimiter(LDirAnterior));

    LMarcados.Delimiter := cSeparadorAlvos;
    LMarcados.StrictDelimiter := True;
    LMarcados.DelimitedText := ArqIni.ReadString('CONFIG', 'AlvosSelecionados', '');

    // .ini gravado pelas versões anteriores, que guardavam uma versão e uma
    // plataforma únicas
    if (LMarcados.Count = 0) then
    begin
      LAlvoAntigo := Trim(ArqIni.ReadString('CONFIG', 'DelphiVersao', '') + ' ' +
                          ArqIni.ReadString('CONFIG', 'Plataforma', 'Win32'));
      if LAlvoAntigo <> '' then
        LMarcados.Add(LAlvoAntigo);
    end;

    for I := 0 to lstAlvos.Count - 1 do
      lstAlvos.Checked[I] := (LMarcados.IndexOf(lstAlvos.Items[I]) >= 0);
  finally
    LMarcados.Free;
    ArqIni.Free;
  end;
end;

// gravar as configurações efetuadas pelo usuário
procedure TfrmPrincipal.GravarConfiguracoes;
var
  ArqIni: TIniFile;
  LMarcados: TStringList;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  LMarcados := TStringList.Create;
  try
    LMarcados.Delimiter := cSeparadorAlvos;
    LMarcados.StrictDelimiter := True;

    for I := 0 to lstAlvos.Count - 1 do
    begin
      if lstAlvos.Checked[I] then
        LMarcados.Add(lstAlvos.Items[I]);
    end;

    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteString('CONFIG', 'AlvosSelecionados', LMarcados.DelimitedText);
    ArqIni.WriteBool('CONFIG', 'FecharTortoise', ckbFecharTortoise.Checked);
    ArqIni.WriteBool('CONFIG', 'DeixarSomentePastaLib', chkSomenteLib.Checked);
    ArqIni.WriteBool('CONFIG', 'InstalarNaIDE64Bits', chkInstalarIDE64.Checked);

    // chaves das versões anteriores, que davam a entender uma seleção única
    ArqIni.DeleteKey('CONFIG', 'DelphiVersao');
    ArqIni.DeleteKey('CONFIG', 'Plataforma');
  finally
    LMarcados.Free;
    ArqIni.Free;
  end;
end;

// Todos os diretórios de fontes, incluindo a raiz: o FortesReport tem .pas
// direto em "Source", e não só nas subpastas.
procedure TfrmPrincipal.ColetarDiretoriosDeFontes(const ADirRaiz: String;
  ALista: TStrings);
var
  oDirList: TSearchRec;
  LDirRaiz, LSubDir: String;
begin
  LDirRaiz := IncludeTrailingPathDelimiter(ADirRaiz);
  if not DirectoryExists(LDirRaiz) then
    Exit;

  if ALista.IndexOf(ExcludeTrailingPathDelimiter(LDirRaiz)) < 0 then
    ALista.Add(ExcludeTrailingPathDelimiter(LDirRaiz));

  if FindFirst(LDirRaiz + '*.*', faDirectory, oDirList) <> 0 then
    Exit;

  try
    repeat
      if ((oDirList.Attr and faDirectory) = 0) or
         (oDirList.Name = '.') or (oDirList.Name = '..') or
         (oDirList.Name = '__history') or (oDirList.Name = '__recovery') then
        Continue;

      LSubDir := LDirRaiz + oDirList.Name;
      ColetarDiretoriosDeFontes(LSubDir, ALista);
    until FindNext(oDirList) <> 0;
  finally
    SysUtils.FindClose(oDirList);
  end;
end;

// Procura o .dpk dentro de "Packages", retornando o diretório com a barra
// final, ou vazio se não encontrar.
function TfrmPrincipal.DiretorioDoPacote(const ANomePacote: String): String;

  function Procurar(const ADir: String): String;
  var
    oDirList: TSearchRec;
    LDir: String;
  begin
    Result := '';
    LDir := IncludeTrailingPathDelimiter(ADir);

    if FileExists(LDir + ANomePacote) then
    begin
      Result := LDir;
      Exit;
    end;

    if FindFirst(LDir + '*.*', faDirectory, oDirList) <> 0 then
      Exit;

    try
      repeat
        if ((oDirList.Attr and faDirectory) = 0) or
           (oDirList.Name = '.') or (oDirList.Name = '..') or
           (oDirList.Name = '__history') then
          Continue;

        Result := Procurar(LDir + oDirList.Name);
      until (Result <> '') or (FindNext(oDirList) <> 0);
    finally
      SysUtils.FindClose(oDirList);
    end;
  end;

begin
  Result := Procurar(FDirRoot + 'Packages');
end;

// Remove diretorios repetidos e vazios de uma lista de paths separada por ";".
// A Library Path do Delphi costuma ter muitas entradas duplicadas, o que
// aumenta desnecessariamente o tamanho da linha de comando do compilador.
function TfrmPrincipal.RemoverPathsDuplicados(const APaths: String): String;
var
  slOrigem, slDestino, slIndice: TStringList;
  iFor: Integer;
  sPath: String;
begin
  slOrigem  := TStringList.Create;
  slDestino := TStringList.Create;
  slIndice  := TStringList.Create;
  try
    slOrigem.StrictDelimiter := True;
    slOrigem.Delimiter := ';';
    slOrigem.DelimitedText := APaths;

    slIndice.CaseSensitive := False;
    slIndice.Sorted := True;
    slIndice.Duplicates := dupIgnore;

    for iFor := 0 to slOrigem.Count - 1 do
    begin
      sPath := ExcludeTrailingPathDelimiter(Trim(slOrigem[iFor]));

      if (sPath = '') or (slIndice.IndexOf(sPath) >= 0) then
        Continue;

      slIndice.Add(sPath);
      slDestino.Add(sPath);
    end;

    slDestino.StrictDelimiter := True;
    slDestino.Delimiter := ';';
    Result := slDestino.DelimitedText;
  finally
    slIndice.Free;
    slDestino.Free;
    slOrigem.Free;
  end;
end;

// A API CreateProcess do Windows limita a linha de comando a 32767 caracteres.
// Quando a Library Path do Delphi e muito grande (muitos componentes instalados
// ou diretorios com nomes longos) o dcc32/dcc64 nem chega a ser executado e a
// compilacao falha sem nenhuma mensagem de erro. Nesse caso as opcoes sao
// gravadas em um "response file" (@arquivo), suportado pelo compilador, o que
// mantem a linha de comando pequena.
procedure TfrmPrincipal.UsarResponseFileSeNecessario(Sender: TJclBorlandCommandLineTool);
const
  // 32767 do Windows menos uma margem para o nome do compilador e do pacote
  CLimiteLinhaComando = 30000;
var
  iFor, iTamanho: Integer;
  sArquivoResposta: String;
  slOpcoes: TStringList;
begin
  iTamanho := Length(Sender.FileName) + Length(FPacoteAtual) + 64;

  for iFor := 0 to Sender.Options.Count - 1 do
    Inc(iTamanho, Length(Sender.Options[iFor]) + 1);

  if iTamanho <= CLimiteLinhaComando then
    Exit;

  sArquivoResposta := IncludeTrailingPathDelimiter(DirLibraryAtual) + 'frce_dcc.rsp';

  slOpcoes := TStringList.Create;
  try
    slOpcoes.Assign(Sender.Options);

    // --no-config precisa continuar na linha de comando
    iFor := slOpcoes.IndexOf('--no-config');
    if iFor >= 0 then
      slOpcoes.Delete(iFor);

    // gravar em ANSI, o compilador nao interpreta BOM/UTF-8 no response file
    slOpcoes.SaveToFile(sArquivoResposta, TEncoding.ANSI);

    Sender.Options.Clear;

    if FAlvo.Instalacao.SupportsNoConfig then
      Sender.Options.Add('--no-config');

    // as aspas envolvem tambem o "@" para suportar diretorios com espacos
    Sender.Options.Add('"@' + sArquivoResposta + '"');

    FazLog(Format('Linha de comando muito grande (%d caracteres). ' +
                  'Usando response file: %s', [iTamanho, sArquivoResposta]));
    FazLog(slOpcoes.Text);
  finally
    slOpcoes.Free;
  end;
end;

// Evento disparado a cada saída do compilador
procedure TfrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
    FazLog(Text);
end;

procedure TfrmPrincipal.ConfiguraMetodosCompiladores;
begin
  // -- Evento disparado antes de iniciar a execução do processo
  FAlvo.Instalacao.DCC32.OnBeforeExecute := BeforeExecute;

  // -- o mesmo tratamento para o compilador de 64 bits, quando existir
  if (FAlvo.Instalacao is TJclBDSInstallation) and
     (clDcc64 in FAlvo.Instalacao.CommandLineTools) then
    (FAlvo.Instalacao as TJclBDSInstallation).DCC64.OnBeforeExecute := BeforeExecute;

  // -- Evento para saidas de mensagens
  FAlvo.Instalacao.OutputCallback := OutputCallLine;
end;

// evento para setar os parâmetros do compilador antes de compilar
procedure TfrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
const
  NamespacesBase    = 'System;Xml;Data;Datasnap;Web;Soap;';
  NamespacesWindows = 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;';
  NamespacesVCL     = 'Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;';
var
  sLibraryPath, sNamespaces, sDirLibrary: String;
begin
  sDirLibrary := DirLibraryAtual;

  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if FAlvo.Instalacao.SupportsNoConfig then
    Sender.Options.Add('--no-config');

  // -B = Build all units. O -M (make, só o que mudou) era enviado junto, e um
  // anula o outro: com o -B o compilador refaz tudo de qualquer jeito. Ficou só
  // o -B, que também é o único seguro aqui, porque na compilação Debug o -U
  // abaixo inclui a pasta do Release e o -M poderia dar por bom um .dcu da
  // outra configuração.
  Sender.Options.Add('-B');
  // -Q = Quiet compile
  Sender.Options.Add('-Q');
  // -H- = não mostrar hints
  Sender.Options.Add('-H-');
  // -W- = não mostrar warnings
  Sender.Options.Add('-W-');

  // As chaves abaixo são sempre informadas explicitamente nos dois modos, para
  // que o resultado não dependa nem do padrão do dcc32 nem do que está escrito
  // dentro do .dpk (o frce.dpk, por exemplo, traz {$DEBUGINFO ON}).
  if (FModoAtual = mcDebug) then
  begin
    // O- = Otimização desligada (código na ordem do fonte, facilita o passo a passo)
    Sender.Options.Add('-$O-');
    // W+ = Gera stack frames (pilha de chamadas confiável no depurador)
    Sender.Options.Add('-$W+');
    // D+ = Informação de depuração
    Sender.Options.Add('-$D+');
    // L+ = Símbolos locais
    Sender.Options.Add('-$L+');
    // Y+ = Informação de referência de símbolos
    Sender.Options.Add('-$Y+');
    // C+ = Assertions ligadas
    Sender.Options.Add('-$C+');
    // -V = Informações de depuração no binário gerado
    Sender.Options.Add('-V');
    // -D<syms> = Define conditionals
    Sender.Options.Add('-DDEBUG');
  end
  else
  begin
    // O+ = Otimização ligada
    Sender.Options.Add('-$O+');
    // W- = Não gera stack frames
    Sender.Options.Add('-$W-');
    // D- = Sem informação de depuração
    Sender.Options.Add('-$D-');
    // L- = Sem símbolos locais
    Sender.Options.Add('-$L-');
    // Y- = Sem informação de referência de símbolos
    Sender.Options.Add('-$Y-');
    // C- = Assertions desligadas
    Sender.Options.Add('-$C-');
    // -D<syms> = Define conditionals
    Sender.Options.Add('-DRELEASE');
  end;

  // Q (overflow) e R (range) ficam desligados nos DOIS modos, que é o padrão do
  // dcc32. Ligá-los no Debug faria o RLCRC32, que conta com o estouro de
  // inteiro, passar a levantar exceção.
  Sender.Options.Add('-$Q-');
  Sender.Options.Add('-$R-');

  sLibraryPath := RemoverPathsDuplicados(
                    FAlvo.Instalacao.LibrarySearchPath[FAlvo.Plataforma]);

  // -U<paths> = Unit directories
  Sender.AddPathOption('U', FAlvo.Instalacao.LibFolderName[FAlvo.Plataforma]);
  Sender.AddPathOption('U', sLibraryPath);
  Sender.AddPathOption('U', sDirLibrary);
  // no Debug os .dcu de Release servem ao que não for recompilado
  if (FModoAtual = mcDebug) then
    Sender.AddPathOption('U', FAlvo.DirLibraryPorModo(mcRelease));
  // -I<paths> = Include directories
  Sender.AddPathOption('I', sLibraryPath);
  // -R<paths> = Resource directories
  Sender.AddPathOption('R', sLibraryPath);
  // -N0<path> = unit .dcu output directory
  Sender.AddPathOption('N0', sDirLibrary);
  Sender.AddPathOption('LE', sDirLibrary);
  Sender.AddPathOption('LN', sDirLibrary);

  // -- Path para instalar os pacotes do Rave no D7, nas demais versões
  // -- o path existe.
  if FAlvo.Instalacao.VersionNumberStr = 'd7' then
    Sender.AddPathOption('U', FAlvo.Instalacao.RootDir + '\Rave5\Lib');

  // -- A partir do XE2 (pacotes 16) os nomes das units são qualificados por
  // -- namespace, e é preciso dizer ao compilador quais procurar.
  if (FAlvo.Instalacao.IDEPackageVersionNumber >= 16) then
  begin
    sNamespaces := NamespacesBase + NamespacesWindows + NamespacesVCL;

    // a BDE só existe para Win32
    if (FAlvo.Plataforma = bpWin32) then
      sNamespaces := sNamespaces + 'Bde;';

    Sender.Options.Add('-NS' + sNamespaces);
  end;

  // usar response file quando a linha de comando estourar o limite do Windows
  UsarResponseFileSeNecessario(Sender);
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  FCountErros    := 0;
  FDirRoot       := '';
  FArquivoLog    := '';
  FPacoteAtual   := '';
  FAlvo          := nil;
  FModoAtual     := mcRelease;
  FQtdeOrfaosRemovidos := 0;

  FIDEsLimpasNestaExecucao := TStringList.Create;
  FIDEsLimpasNestaExecucao.Sorted := True;
  FIDEsLimpasNestaExecucao.Duplicates := dupIgnore;

  FTextosLimpeza := TStringList.Create;
  FTextosLimpeza.Sorted := True;
  FTextosLimpeza.Duplicates := dupIgnore;
  FTextosLimpeza.CaseSensitive := False;
  // as entradas gravadas com macro nao trazem o caminho literal da
  // instalacao, entao a marca do projeto tambem entra na limpeza
  FTextosLimpeza.Add(cIdentificacaoProjeto);

  // uma entrada por IDE + plataforma encontrada na máquina
  FListaAlvos := GeraListaAlvos;
  PopularListaAlvos;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FListaAlvos.Free;
  FTextosLimpeza.Free;
  FIDEsLimpasNestaExecucao.Free;
end;

procedure TfrmPrincipal.btnMarcarTodosClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lstAlvos.Count - 1 do
    lstAlvos.Checked[I] := True;
end;

procedure TfrmPrincipal.btnDesmarcarTodosClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lstAlvos.Count - 1 do
    lstAlvos.Checked[I] := False;
end;

// Registros apontando para BPLs inexistentes fazem a IDE exibir "Can't load
// package" ao abrir. A limpeza cobre TODAS as IDEs detectadas, inclusive as que
// não estão marcadas nesta execução: o registro quebrado pode ter ficado de uma
// instalação anterior em outra IDE ou em outra pasta.
procedure TfrmPrincipal.RemoverRegistrosOrfaosDeTodasIDEs;
var
  I: Integer;
  LIDEs: TStringList;
  LChave: String;
begin
  FQtdeOrfaosRemovidos := 0;

  LIDEs := TStringList.Create;
  try
    LIDEs.Sorted := True;
    LIDEs.Duplicates := dupIgnore;

    for I := 0 to FListaAlvos.Count - 1 do
    begin
      LChave := AnsiUpperCase(FListaAlvos[I].Instalacao.ConfigDataLocation);
      if LIDEs.IndexOf(LChave) >= 0 then
        Continue;
      LIDEs.Add(LChave);

      try
        Inc(FQtdeOrfaosRemovidos,
            FListaAlvos[I].RemoverPacotesFRCEDaIDE(cSecaoKnownPackages, True));
        Inc(FQtdeOrfaosRemovidos,
            FListaAlvos[I].RemoverPacotesFRCEDaIDE(cSecaoKnownPackagesX64, True));
      except
        on E: Exception do
        begin
          // O registro do Windows costuma guardar IDEs que já foram
          // desinstaladas; nenhuma delas pode impedir a instalação.
          InformaSituacao('Aviso: não foi possível limpar registros órfãos de ' +
                          LChave + ': ' + E.Message, nmAviso);
        end;
      end;
    end;
  finally
    LIDEs.Free;
  end;
end;

// A limpeza vale para a IDE inteira, então só pode acontecer uma vez por
// execução. Repetir a cada plataforma apagaria o que a plataforma anterior
// acabou de registrar nesta mesma execução.
function TfrmPrincipal.LimpezaDaIDEJaFoiFeita: Boolean;
var
  LChave: String;
begin
  LChave := AnsiUpperCase(FAlvo.Instalacao.ConfigDataLocation);
  Result := (FIDEsLimpasNestaExecucao.IndexOf(LChave) >= 0);
  if not Result then
    FIDEsLimpasNestaExecucao.Add(LChave);
end;

// Remove os registros do FortesReport desta IDE, nas duas listas (32 e 64
// bits), independente da plataforma que está sendo instalada agora.
procedure TfrmPrincipal.RemoverPacotesAntigos;
var
  I, LRemovidos: Integer;
begin
  // Lista de 32 bits: pela JCL, para manter o cache interno dela consistente
  with FAlvo.Instalacao do
  begin
    for I := IdePackages.Count[False] - 1 downto 0 do
    begin
      if Pos(cIdentificacaoPacotes,
             AnsiUpperCase(IdePackages.PackageFileNames[I, False])) > 0 then
        IdePackages.RemovePackage(IdePackages.PackageFileNames[I, False], False);
    end;
  end;

  // Lista de 64 bits: a JCL não a conhece
  LRemovidos := FAlvo.RemoverPacotesFRCEDaIDE(cSecaoKnownPackagesX64, False);
  if LRemovidos > 0 then
    FazLog(Format('Removidos %d registros do FortesReport de "%s".',
                  [LRemovidos, cSecaoKnownPackagesX64]));
end;

procedure TfrmPrincipal.RemoverDiretoriosFRCEDoPath;
var
  I: Integer;
begin
  for I := 0 to FTextosLimpeza.Count - 1 do
  begin
    // Search Path, Browsing Path e Debug DCU Path
    FAlvo.RemoverDosPathsDeBiblioteca(FTextosLimpeza[I]);
    // caminho de procura dos BPLs
    FAlvo.RemoverDoPackageSearchPath(FTextosLimpeza[I]);
  end;
end;

// Versões anteriores do instalador gravavam os artefatos direto na pasta da
// versão do Delphi. Agora cada plataforma e configuração tem a sua subpasta,
// então o que estiver solto na raiz é sobra de instalação antiga e pode fazer o
// compilador ou a IDE pegarem um artefato de outra configuração.
procedure TfrmPrincipal.LimparArtefatosDaRaizDaPlataforma;
var
  LArquivos: TStringDynArray;
  I, LRemovidos: Integer;
begin
  if not DirectoryExists(FAlvo.DirLibraryRaiz) then
    Exit;

  LRemovidos := 0;
  LArquivos := TDirectory.GetFiles(
                 IncludeTrailingPathDelimiter(FAlvo.DirLibraryRaiz),
                 '*.*', TSearchOption.soTopDirectoryOnly);

  for I := Low(LArquivos) to High(LArquivos) do
  begin
    if DeleteFile(PWideChar(LArquivos[I])) then
      Inc(LRemovidos);
  end;

  if LRemovidos > 0 then
    InformaSituacao(Format('Removidos %d arquivos soltos na raiz de "%s" (layout antigo).',
                           [LRemovidos, FAlvo.DirLibraryRaiz]), nmAviso);
end;

procedure TfrmPrincipal.ApagarOutrosArquivosDaPastaLibrary;

  procedure Apagar(const AMascara: String);
  var
    ListArquivos: TStringDynArray;
    I: Integer;
  begin
    ListArquivos := TDirectory.GetFiles(
                      IncludeTrailingPathDelimiter(FAlvo.DirLibraryRaiz),
                      AMascara, TSearchOption.soAllDirectories);
    for I := Low(ListArquivos) to High(ListArquivos) do
      DeleteFile(PWideChar(ListArquivos[I]));
  end;

begin
  if not DirectoryExists(FAlvo.DirLibraryRaiz) then
    Exit;

  Apagar('*.dcr');
  Apagar('*.res');
  Apagar('*.dfm');
  Apagar('*.ini');
  Apagar('*.inc');
end;

procedure TfrmPrincipal.CopiarOutrosArquivosParaPastaLibrary;

  procedure Copiar(const AMascara: String);
  var
    ListArquivos: TStringDynArray;
    LArquivo: String;
    I: Integer;
  begin
    ListArquivos := TDirectory.GetFiles(FDirRoot + 'Source', AMascara,
                                        TSearchOption.soAllDirectories);
    for I := Low(ListArquivos) to High(ListArquivos) do
    begin
      LArquivo := ExtractFileName(ListArquivos[I]);
      CopyFile(PWideChar(ListArquivos[I]),
               PWideChar(IncludeTrailingPathDelimiter(
                           FAlvo.DirLibraryPorModo(ModoParaInstalacao)) + LArquivo),
               False);
    end;
  end;

begin
  Copiar('*.dcr');
  Copiar('*.res');
  Copiar('*.dfm');
  Copiar('*.ini');
  Copiar('*.inc');
end;

// adicionar os paths ao library path do delphi
procedure TfrmPrincipal.AddLibrarySearchPath;
var
  LDirLibrary: String;
  LDiretorios: TStringList;
begin
  LDirLibrary := FAlvo.DirLibraryPorModo(ModoParaInstalacao);

  LDiretorios := TStringList.Create;
  try
    LDiretorios.Add(LDirLibrary);
    ColetarDiretoriosDeFontes(FDirRoot + 'Source', LDiretorios);

    // uma única gravação por path, em vez de uma por diretório: a JCL regrava
    // o EnvOptions.proj a cada chamada
    FAlvo.AdicionarNosPathsDeBiblioteca(LDiretorios);
    FazLog(Format('Adicionados %d diretórios ao Library Path de %s.',
                  [LDiretorios.Count, FAlvo.NomePlataforma]));
  finally
    LDiretorios.Free;
  end;

  // os .dcu com informações de depuração ficam na pasta do modo Debug
  if (mcDebug in ModosCompilacao) then
    FAlvo.AdicionarNoDebugDCUPath(FAlvo.DirLibraryPorModo(mcDebug))
  else
    FAlvo.AdicionarNoDebugDCUPath(LDirLibrary);

  // caminho onde a IDE (32 ou 64 bits) procura os BPLs do FortesReport
  if PodeInstalarPacotesNaIDE then
    FAlvo.AdicionarPackageSearchPath(LDirLibrary);
end;

procedure TfrmPrincipal.DeixarSomenteLib;
var
  LDiretorios: TStringList;
begin
  // remove do Library Search Path as pastas de fontes, deixando apenas a pasta
  // da combinação instalada. O Browsing Path é mantido, para o Ctrl+Click
  // continuar abrindo os fontes.
  LDiretorios := TStringList.Create;
  try
    ColetarDiretoriosDeFontes(FDirRoot + 'Source', LDiretorios);
    FAlvo.RemoverDoLibrarySearchPath(LDiretorios);
  finally
    LDiretorios.Free;
  end;
end;

// É por aqui que a IDE encontra os BPLs de runtime de que os pacotes de design
// dependem. O Package Search Path serve para os projetos, e não para o
// carregamento dos pacotes da própria IDE.
procedure TfrmPrincipal.AdicionarPastaDosBplsNoPathDaIDE;
begin
  FAlvo.AjustarEnvironmentPath(
    FAlvo.DirLibraryPorModo(ModoParaInstalacao), '', False);
end;

procedure TfrmPrincipal.CompilarPacote(const ANomePacote: String);
var
  LDirPacote, LDirLibrary: String;
begin
  LDirPacote := DiretorioDoPacote(ANomePacote);
  if LDirPacote = '' then
  begin
    Inc(FCountErros);
    InformaSituacao(Format('Pacote "%s" não localizado em "%sPackages".',
                           [ANomePacote, FDirRoot]), nmErro);
    Exit;
  end;

  FPacoteAtual := LDirPacote + ANomePacote;

  if not IsDelphiPackage(FPacoteAtual) then
  begin
    Inc(FCountErros);
    InformaSituacao(Format('"%s" não é um pacote Delphi.', [ANomePacote]), nmErro);
    Exit;
  end;

  LDirLibrary := DirLibraryAtual;

  if FAlvo.Instalacao.RadToolKind = brBorlandDevStudio then
    FAlvo.LimparPackageCache(BinaryFileName(LDirLibrary, FPacoteAtual));

  FazLog('');
  if FAlvo.Instalacao.CompilePackage(FPacoteAtual, LDirLibrary, LDirLibrary) then
    InformaSituacao(Format('Pacote "%s" compilado com sucesso.', [ANomePacote]), nmSucesso)
  else
  begin
    Inc(FCountErros);
    InformaSituacao(Format('Erro ao compilar o pacote "%s".', [ANomePacote]), nmErro);
  end;
end;

// O dclfrce depende do frce em tempo de execução. Se os dois não estiverem na
// MESMA pasta (mesma versão do Delphi, plataforma e configuração) a IDE não
// consegue carregar o pacote de design.
function TfrmPrincipal.DependenciaRunTimeFaltando: String;
var
  LDirPacote, LArquivoBpl: String;
begin
  Result := '';

  LDirPacote := DiretorioDoPacote(cPacoteRunTime);
  if LDirPacote = '' then
    Exit;

  LArquivoBpl := BinaryFileName(DirLibraryAtual, LDirPacote + cPacoteRunTime);
  if not FileExists(LArquivoBpl) then
    Result := ExtractFileName(LArquivoBpl);
end;

function TfrmPrincipal.RegistrarPacoteNaIDE(const AArquivoPacote: String): Boolean;
var
  LRunOnly: Boolean;
  LNaoUsado, LDescricao, LArquivoBpl: String;
begin
  // O BPL já foi gerado na etapa de compilação; aqui ele é apenas registrado na
  // IDE. Usar o InstallPackage da JCL recompilaria tudo de novo.
  GetDPKFileInfo(AArquivoPacote, LRunOnly, @LNaoUsado, @LDescricao);
  LArquivoBpl := BinaryFileName(DirLibraryAtual, AArquivoPacote);

  Result := FileExists(LArquivoBpl);
  if not Result then
  begin
    InformaSituacao(Format('BPL não encontrado para registrar na IDE: "%s"',
                           [LArquivoBpl]), nmErro);
    Exit;
  end;

  if (FAlvo.Plataforma = bpWin64) then
    // Grava direto na lista "Known Packages x64". As versões antigas da JCL,
    // que o instalador ainda precisa suportar, não conhecem essa lista.
    FAlvo.RegistrarPacoteNaIDE(LArquivoBpl, LDescricao)
  else
    Result := FAlvo.Instalacao.RegisterPackage(LArquivoBpl, LDescricao);
end;

procedure TfrmPrincipal.CompilarEInstalarPacotes;
var
  LModo: TModoCompilacao;
  LArquivoDesign, LFaltando: String;
begin
  FAlvo.ConfigurarDCC;

  // -- Compila em todos os modos marcados (Release e/ou Debug)
  for LModo := Low(TModoCompilacao) to High(TModoCompilacao) do
  begin
    if not (LModo in ModosCompilacao) then
      Continue;

    FModoAtual := LModo;
    ForceDirectories(DirLibraryAtual);

    InformaSituacao('');
    InformaSituacao('COMPILANDO OS PACOTES ' + FAlvo.NomePlataforma + ' em ' +
                    cNomeModoCompilacao[LModo] + '...', nmDestaque);

    CompilarPacote(cPacoteRunTime);

    // o pacote de design só é compilado onde existe IDE para carregá-lo
    if (FCountErros = 0) and FAlvo.SuportaPacotesDesignTime then
      CompilarPacote(cPacoteDesign);

    InformaProgresso;

    if (FCountErros > 0) then
    begin
      InformaSituacao('Abortando... Ocorreram erros na compilação dos pacotes.', nmErro);
      Exit;
    end;
  end;

  // os BPL registrados na IDE são sempre os de Release
  FModoAtual := ModoParaInstalacao;

  if not PodeInstalarPacotesNaIDE then
  begin
    InformaSituacao('');
    if (FAlvo.Plataforma = bpWin64) and (not FAlvo.SuportaIDE64Bits) then
      InformaSituacao('Esta versão do Delphi não possui IDE de 64 bits; ' +
                      'os pacotes Win64 foram somente compilados.', nmAviso)
    else if (FAlvo.Plataforma = bpWin64) then
      InformaSituacao('Instalação na IDE de 64 bits não marcada; ' +
                      'os pacotes Win64 foram somente compilados.', nmAviso)
    else
      InformaSituacao('Para a plataforma ' + FAlvo.NomePlataforma +
                      ' os pacotes são somente compilados.', nmAviso);
    InformaProgresso;
    Exit;
  end;

  InformaSituacao('');
  InformaSituacao('INSTALANDO OS PACOTES NA IDE ' + FAlvo.NomePlataforma + '...', nmDestaque);

  LArquivoDesign := DiretorioDoPacote(cPacoteDesign);
  if LArquivoDesign = '' then
  begin
    Inc(FCountErros);
    InformaSituacao(Format('Pacote de design "%s" não localizado.', [cPacoteDesign]), nmErro);
    Exit;
  end;
  LArquivoDesign := LArquivoDesign + cPacoteDesign;

  LFaltando := DependenciaRunTimeFaltando;
  if LFaltando <> '' then
    InformaSituacao(Format('AVISO: o pacote "%s" depende de "%s", que não está ' +
                           'em "%s". A IDE não conseguirá carregá-lo.',
                           [cPacoteDesign, LFaltando, DirLibraryAtual]), nmAviso);

  if RegistrarPacoteNaIDE(LArquivoDesign) then
    InformaSituacao(Format('Pacote "%s" instalado com sucesso.', [cPacoteDesign]), nmSucesso)
  else
  begin
    Inc(FCountErros);
    InformaSituacao(Format('Ocorreu um erro ao instalar o pacote "%s".', [cPacoteDesign]), nmErro);
  end;

  InformaProgresso;
end;

procedure TfrmPrincipal.InstalarOutrosRequisitos;
begin
  // Deixar somente a pasta Lib no Library Path vale para Win32 e Win64
  // igualmente: nas duas os pacotes são compilados e os .dcu ficam prontos.
  if not chkSomenteLib.Checked then
    Exit;

  InformaSituacao('');
  InformaSituacao('INSTALANDO OUTROS REQUISITOS...', nmDestaque);

  try
    DeixarSomenteLib;
    InformaSituacao('Limpeza do library path feita com sucesso.', nmSucesso);
  except
    on E: Exception do
      InformaSituacao('Ocorreu erro ao limpar o path: ' + sLineBreak + E.Message, nmErro);
  end;

  try
    CopiarOutrosArquivosParaPastaLibrary;
    InformaSituacao('Cópia dos arquivos necessários feita com sucesso para: ' +
                    FAlvo.DirLibraryPorModo(ModoParaInstalacao), nmSucesso);
  except
    on E: Exception do
      InformaSituacao('Ocorreu erro ao copiar arquivos para: ' +
                      FAlvo.DirLibraryPorModo(ModoParaInstalacao) + sLineBreak +
                      'Erro: ' + E.Message, nmErro);
  end;
end;

procedure TfrmPrincipal.InstalarAlvo;
var
  I: Integer;
  LModo: TModoCompilacao;
  LCabecalho, LModos: String;
begin
  FAlvo.DirLibraryRaiz := FDirRoot + FAlvo.SubDirLibrary;
  FArquivoLog := PathArquivoLog(FAlvo.NomeAlvo);

  LModos := '';
  for LModo := Low(TModoCompilacao) to High(TModoCompilacao) do
  begin
    if (LModo in ModosCompilacao) then
    begin
      if LModos <> '' then
        LModos := LModos + ', ';
      LModos := LModos + cNomeModoCompilacao[LModo];
    end;
  end;

  LCabecalho :=
    'Executado em       : ' + DateTimeToStr(Now) + sLineBreak +
    'Versão do delphi   : ' + FAlvo.NomeAlvo + sLineBreak +
    'Dir. Instalação    : ' + FDirRoot + sLineBreak +
    'Dir. Bibliotecas   : ' + FAlvo.DirLibraryRaiz + '\<Configuração>' + sLineBreak +
    'Modos de compilação: ' + LModos + sLineBreak +
    'Registros órfãos removidos: ' + IntToStr(FQtdeOrfaosRemovidos) + sLineBreak +
    'Instala pacotes na IDE: ' + BoolToStr(PodeInstalarPacotesNaIDE, True) + sLineBreak +
    StringOfChar('=', 80);

  // cada alvo começa o seu próprio arquivo de log
  WriteToTXT(AnsiString(FArquivoLog), AnsiString(LCabecalho), False);

  InformaSituacao('');
  InformaSituacao('===== ' + FAlvo.NomeAlvo + ' =====', nmDestaque);

  ConfiguraMetodosCompiladores;

  InformaSituacao('Removendo library paths da instalação anterior...');
  RemoverDiretoriosFRCEDoPath;
  InformaSituacao('...OK', nmSucesso);

  if not LimpezaDaIDEJaFoiFeita then
  begin
    InformaSituacao('Removendo pacotes da instalação anterior na IDE...');
    RemoverPacotesAntigos;
    // uma vez por IDE, antes de qualquer plataforma acrescentar a sua pasta
    for I := 0 to FTextosLimpeza.Count - 1 do
      FAlvo.AjustarEnvironmentPath('', FTextosLimpeza[I], False);
    InformaSituacao('...OK', nmSucesso);
  end;
  InformaProgresso;

  InformaSituacao('Criando diretórios de bibliotecas para ' + FAlvo.NomePlataforma + '...');
  ForceDirectories(FAlvo.DirLibraryRaiz);
  LimparArtefatosDaRaizDaPlataforma;
  for LModo := Low(TModoCompilacao) to High(TModoCompilacao) do
  begin
    if (LModo in ModosCompilacao) then
      ForceDirectories(FAlvo.DirLibraryPorModo(LModo));
  end;
  ApagarOutrosArquivosDaPastaLibrary;
  InformaSituacao('...OK', nmSucesso);
  InformaProgresso;

  InformaSituacao('Adicionando library paths para ' + FAlvo.NomePlataforma + '...');
  AddLibrarySearchPath;
  InformaSituacao('...OK', nmSucesso);
  InformaProgresso;

  InformaSituacao('Adicionando a pasta dos BPLs ' + FAlvo.NomePlataforma +
                  ' ao PATH do Delphi...');
  AdicionarPastaDosBplsNoPathDaIDE;
  InformaSituacao('...OK', nmSucesso);
  InformaProgresso;

  CompilarEInstalarPacotes;
end;

// botão de compilação e instalação dos alvos marcados
procedure TfrmPrincipal.btnInstalarfrceClick(Sender: TObject);
var
  I: Integer;
  LAlvoComErro: String;
begin
  FCountErros := 0;
  LAlvoComErro := '';
  // as mensagens anteriores ao primeiro alvo ainda não têm log de destino
  FArquivoLog := '';
  FDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);
  FTextosLimpeza.Add(FDirRoot);

  btnInstalarfrce.Enabled := False;
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
  try
    lstMsgInstalacao.Clear;
    FIDEsLimpasNestaExecucao.Clear;

    pgbInstalacao.Position := 0;
    pgbInstalacao.Max := Max(1, QuantidadeAlvosMarcados *
                                (6 + QuantidadeModosCompilacao));

    // antes de qualquer coisa, tirar do caminho registros quebrados de
    // execuções anteriores, inclusive de IDEs que não fazem parte desta
    // instalação
    RemoverRegistrosOrfaosDeTodasIDEs;

    for I := 0 to lstAlvos.Count - 1 do
    begin
      if not lstAlvos.Checked[I] then
        Continue;

      FAlvo := TPlataformaAlvo(lstAlvos.Items.Objects[I]);
      InstalarAlvo;

      // Um alvo que falha normalmente indica um problema que vai se repetir nos
      // demais (fonte quebrado, define errado, dependência faltando). Seguir a
      // fila só enterraria o erro no meio de um log enorme, então para aqui.
      if (FCountErros > 0) then
      begin
        LAlvoComErro := FAlvo.NomeAlvo;
        Break;
      end;

      InstalarOutrosRequisitos;
      InformaProgresso;
    end;
  finally
    btnInstalarfrce.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, FCountErros = 0);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;

  if FCountErros = 0 then
  begin
    pgbInstalacao.Position := pgbInstalacao.Max;
    Application.MessageBox(
      PWideChar(
        'Pacotes compilados e instalados com sucesso! '+sLineBreak+
        'Clique em "Próximo" para finalizar a instalação.'
      ),
      'Instalação',
      MB_ICONINFORMATION + MB_OK
    );
  end
  else
  begin
    if Application.MessageBox(
      PWideChar(
        Format('A instalação foi interrompida: o alvo "%s" terminou com erros.',
               [LAlvoComErro]) + sLineBreak +
        'Para maiores informações verifique o arquivo de log gerado.' + sLineBreak + sLineBreak +
        'Deseja visualizar o arquivo de log gerado?'
      ),
      'Instalação',
      MB_ICONQUESTION + MB_YESNO
    ) = ID_YES then
    begin
      btnVisualizarLogCompilacao.Click;
    end;
  end;
end;

// chama a caixa de dialogo para selecionar o diretório de instalação
procedure TfrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir, [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

// quando clicar em alguma das urls chamar o link mostrado no caption
procedure TfrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TfrmPrincipal.wizPgInicioNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // Verificar se o delphi está aberto
  {$IFNDEF DEBUG}
  if FListaAlvos.Instalacoes.AnyInstanceRunning then
  begin
    Stop := True;
    Application.MessageBox(
      'Feche a IDE do delphi antes de continuar.',
      PWideChar(Application.Title),
      MB_ICONERROR + MB_OK
    );
  end;
  {$ENDIF}

  // Verificar se o tortoise está instalado, se não estiver, não mostrar a aba de atualização
  // o usuário deve utilizar software proprio e fazer manualmente
  // pedido do forum
  wizPgObterFontes.Visible := TSVN_Class.SVNInstalled;
end;

procedure TfrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
var
  I: Integer;
  LAlvo: TPlataformaAlvo;
  LModos: String;
  LModo: TModoCompilacao;
begin
  lstMsgInstalacao.Clear;
  pgbInstalacao.Position := 0;

  FDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);

  LModos := '';
  for LModo := Low(TModoCompilacao) to High(TModoCompilacao) do
  begin
    if (LModo in ModosCompilacao) then
    begin
      if LModos <> '' then
        LModos := LModos + ', ';
      LModos := LModos + cNomeModoCompilacao[LModo];
    end;
  end;

  // mostrar ao usuário as informações de compilação
  with lbInfo.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add('Dir. Instalação : ' + edtDirDestino.Text);
      Add('Compilação      : ' + LModos);

      for I := 0 to lstAlvos.Count - 1 do
      begin
        if not lstAlvos.Checked[I] then
          Continue;

        LAlvo := TPlataformaAlvo(lstAlvos.Items.Objects[I]);
        Add(lstAlvos.Items[I] + '  ->  ' +
            FDirRoot + LAlvo.SubDirLibrary + '\<Configuração>');
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmPrincipal.wizPgInstalacaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if (lstMsgInstalacao.Count <= 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Clique no botão instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if (FCountErros > 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Ocorreram erros durante a compilação e instalação dos pacotes, verifique.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;
end;

procedure TfrmPrincipal.wizPgConfiguracaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
var
  I: Integer;
  LAlvo: TPlataformaAlvo;
  LAntigas: String;
begin
  // verificar se foi informado o diretório
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diretório de instalação não foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
    Exit;
  end;

  if not DirectoryExists(IncludeTrailingPathDelimiter(edtDirDestino.Text) + 'Packages') then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      PWideChar('O diretório informado não parece ser o do FortesReport: ' +
                'não foi encontrada a pasta "Packages".'),
      'Erro.',
      MB_OK + MB_ICONERROR
    );
    Exit;
  end;

  // prevenir nenhuma IDE marcada
  if QuantidadeAlvosMarcados = 0 then
  begin
    Stop := True;
    lstAlvos.SetFocus;
    Application.MessageBox(
      'Marque ao menos uma IDE/plataforma para instalar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
    Exit;
  end;

  // O aviso sai uma única vez, depois que o usuário conclui a escolha, listando
  // todas as IDEs antigas marcadas.
  LAntigas := '';
  for I := 0 to lstAlvos.Count - 1 do
  begin
    if not lstAlvos.Checked[I] then
      Continue;

    LAlvo := TPlataformaAlvo(lstAlvos.Items.Objects[I]);
    if MatchText(LAlvo.Instalacao.VersionNumberStr, ['d6', 'd7', 'd9', 'd10', 'd11']) then
      LAntigas := LAntigas + sLineBreak + '  - ' + lstAlvos.Items[I];
  end;

  if LAntigas <> '' then
    Application.MessageBox(
      PWideChar('As versões abaixo são muito antigas e podem apresentar falhas ' +
                'na compilação dos pacotes:' + sLineBreak + LAntigas),
      'Atenção',
      MB_OK + MB_ICONWARNING
    );

  // Gravar as configurações em um .ini para utilizar depois
  GravarConfiguracoes;
end;

procedure TfrmPrincipal.wizPgObterFontesEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
  // verificar se o checkout já foi feito se sim, atualizar
  // se não fazer o checkout
  if IsCheckOutJaFeito(edtDirDestino.Text) then
  begin
    lblInfoObterFontes.Caption := 'Clique em "Atualizar" para efetuar a atualização do repositório FRCE.';
    btnSVNCheckoutUpdate.Caption := 'Atualizar...';
    btnSVNCheckoutUpdate.Tag := -1;
  end
  else
  begin
    lblInfoObterFontes.Caption := 'Clique em "Download" para efetuar o download do repositório FRCE.';
    btnSVNCheckoutUpdate.Caption := 'Download...';
    btnSVNCheckoutUpdate.Tag := 1;
  end;
end;

procedure TfrmPrincipal.btnSVNCheckoutUpdateClick(Sender: TObject);
begin
  // chamar o método de update ou checkout conforme a necessidade
  if TSpeedButton(Sender).Tag > 0 then
  begin
    // criar o diretório onde será baixado o repositório
    if not DirectoryExists(edtDirDestino.Text) then
    begin
      if not ForceDirectories(edtDirDestino.Text) then
      begin
        raise EDirectoryNotFoundException.Create(
          'Ocorreu o seguinte erro ao criar o diretório' + sLineBreak +
            SysErrorMessage(GetLastError));
      end;
    end;

    // checkout
    TSVN_Class.SVNTortoise_CheckOut(edtURL.Text, edtDirDestino.Text, ckbFecharTortoise.Checked );
  end
  else
  begin
    // update
    TSVN_Class.SVNTortoise_Update(edtDirDestino.Text, ckbFecharTortoise.Checked);
  end;
end;

// Abre o log do último alvo instalado. Como agora existe um log por alvo, sem
// nenhuma instalação feita o que se abre é a pasta com todos eles.
procedure TfrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  if (FArquivoLog <> '') and FileExists(FArquivoLog) then
    ShellExecute(Handle, 'open', PWideChar(FArquivoLog), '', '', 1)
  else
    ShellExecute(Handle, 'open', PWideChar(PathApp), '', '', 1);
end;

procedure TfrmPrincipal.wizPrincipalCancelButtonClick(Sender: TObject);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instalação?',
    'Fechar',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_YES then
  begin
    Self.Close;
  end;
end;

procedure TfrmPrincipal.wizPrincipalFinishButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.
