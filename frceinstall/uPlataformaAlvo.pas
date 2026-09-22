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
|* 22/09/2026:  Victor H. Gonzales "Panda"
|* - Criação da unit, no mesmo desenho usado no instalador do ACBr.
******************************************************************************}

unit uPlataformaAlvo;

{
  Alvos de instalação.

  Um alvo é a combinação de uma IDE do Delphi instalada na máquina com uma
  plataforma de compilação (Win32 ou Win64). O instalador trabalha sobre uma
  lista de alvos, e não mais sobre "a versão selecionada", o que permite
  instalar em várias IDEs e nas duas plataformas numa única execução.

  Cada alvo conhece as suas próprias pastas de saída, separadas por modo de
  compilação:

    <raiz>\Binary\Lib<versao>\<Plataforma>\<Configuracao>

  e sabe registrar os pacotes na lista certa da IDE: "Known Packages" para a
  IDE de 32 bits e "Known Packages x64" para a de 64 bits, que a JCL não
  conhece.
}

interface

uses
  Windows, SysUtils, Classes, StrUtils, Generics.Collections,
  JclIDEUtils, JclCompilerUtils;

const
  // Plataformas para as quais os pacotes são compilados
  PlataformasSuportadas = [bpWin32, bpWin64];

  // Seções e valores da configuração da IDE (registro do Windows)
  cSecaoKnownPackages     = 'Known Packages';
  cSecaoKnownPackagesX64  = 'Known Packages x64';
  cSecaoPackageCache      = 'Package Cache';
  cSecaoPackageCacheX64   = 'Package Cache x64';
  cSecaoLibrary           = 'Library';
  cSecaoEnvironmentVars   = 'Environment Variables';
  cValorPackageSearchPath = 'Package Search Path';

  // Marca que identifica os BPLs do FortesReport no registro da IDE
  // (frce.bpl, dclfrce.bpl e os equivalentes com LIBSUFFIX)
  cIdentificacaoPacotes = 'FRCE';

  // Marca que identifica os diretórios do projeto na Library Path da IDE.
  // Complementa a limpeza pelo diretório de instalação, que não alcança as
  // entradas gravadas com macro, que nao trazem o caminho literal.
  cIdentificacaoProjeto = 'fortesreport';

type
  // Modos de compilação dos pacotes (BPL/DCU) suportados pelo instalador
  TModoCompilacao = (mcRelease, mcDebug);
  TModosCompilacao = set of TModoCompilacao;

const
  cNomeModoCompilacao: array[TModoCompilacao] of string = ('Release', 'Debug');

type
  TPlataformaAlvo = class(TObject)
  private
    function EhIDEComSuporteAPlataformas: Boolean;
    function GetPackageSearchPath: String;
    procedure SetPackageSearchPath(const AValor: String);
    procedure CarregarListaDePath(const AValor: String; ALista: TStrings);
    function MontarPathDaLista(ALista: TStrings): String;
  public
    Instalacao: TJclBorRADToolInstallation;
    Plataforma: TJclBDSPlatform;
    NomePlataforma: String;
    // Raiz da plataforma. Nada é gravado nela: os artefatos ficam nas subpastas
    // de configuração (veja DirLibraryPorModo).
    DirLibraryRaiz: String;

    constructor Create(AInstalacao: TJclBorRADToolInstallation;
      APlataforma: TJclBDSPlatform; const ANomePlataforma: String);

    function NomeIDE: String;
    function NomeAlvo: String;
    function SubDirLibrary: String;
    function EhSuportado: Boolean;
    procedure ConfigurarDCC;

    // -- Suporte a Release/Debug --------------------------------------------
    // Cada combinação IDE + Plataforma + Configuração tem a sua própria pasta,
    // e todos os artefatos dela (BPL, DCP, DCU) ficam juntos ali. Sem isso a
    // troca de configuração mudaria o caminho do BPL e deixaria o registro
    // anterior da IDE apontando para o lugar errado.
    function DirLibraryPorModo(const AModo: TModoCompilacao): String;

    // -- Suporte a IDE de 32 e 64 bits --------------------------------------
    // A IDE de 64 bits (bin64\bds.exe) e os pacotes de design para Win64
    // (designide) só existem nas versões mais recentes do Delphi.
    function SuportaIDE64Bits: Boolean;
    function SuportaPacotesDesignTime: Boolean;

    function SecaoKnownPackages: String;
    function SecaoLibraryDaPlataforma: String;

    procedure LimparPackageCache(const ANomeArquivoBpl: String);
    procedure RegistrarPacoteNaIDE(const ANomeArquivoBpl, ADescricao: String);
    procedure RemoverPacoteDaIDE(const ANomeArquivoBpl: String);
    // Remove os registros do FortesReport de uma das listas de pacotes da IDE.
    // ApenasOrfaos = True remove somente os que apontam para arquivos que não
    // existem mais. Retorna a quantidade removida.
    function RemoverPacotesFRCEDaIDE(const ASecao: String;
      const ApenasOrfaos: Boolean): Integer;

    // Alterações de Library/Browsing Path em lote: a JCL regrava o
    // EnvOptions.proj a cada diretório, o que fica lento com centenas deles.
    procedure AdicionarNosPathsDeBiblioteca(ADiretorios: TStrings);
    procedure RemoverDoLibrarySearchPath(ADiretorios: TStrings);
    // Remove de Search/Browsing/Debug DCU Path tudo que contenha o texto
    // informado, com uma única gravação por path.
    procedure RemoverDosPathsDeBiblioteca(const ATextoProcurar: String);
    procedure AdicionarNoDebugDCUPath(const ADiretorio: String);

    // Caminho onde a IDE procura os BPLs exigidos pelos projetos. É mantido por
    // plataforma, evitando conflito entre Win32 e Win64.
    procedure AdicionarPackageSearchPath(const ADiretorio: String);
    procedure RemoverDoPackageSearchPath(const ATextoProcurar: String);

    // Ajusta o PATH que a IDE usa para localizar os BPLs de runtime exigidos
    // pelos pacotes de design. O PATH é único para as IDEs de 32 e 64 bits, mas
    // o loader do Windows ignora a DLL de arquitetura errada e continua
    // procurando nas pastas seguintes -- a própria Delphi conta com isso ao
    // deixar Bpl e Bpl\Win64 juntos no PATH do sistema. Por isso as pastas das
    // duas plataformas podem conviver aqui.
    // ATextoRemover vazio = não remove nada; ADiretorio vazio = não acrescenta.
    procedure AjustarEnvironmentPath(const ADiretorio, ATextoRemover: String;
      const ExpandirPathSeNecessario: Boolean);
  end;

  TListaAlvos = class(TObjectList<TPlataformaAlvo>)
  private
    FInstalacoes: TJclBorRADToolInstallations;
  public
    constructor Create;
    destructor Destroy; override;
    property Instalacoes: TJclBorRADToolInstallations read FInstalacoes;
  end;

// Monta a lista com todos os alvos da máquina: uma entrada por IDE + plataforma.
function GeraListaAlvos: TListaAlvos;

implementation

// Nome usado na tela e no arquivo de log. As versões não mapeadas caem no nome
// que a própria JCL informa, para que um Delphi novo apareça na lista sem
// depender de alteração aqui.
function NomeAmigavelIDE(AInstalacao: TJclBorRADToolInstallation): String;
begin
  if      AInstalacao.VersionNumberStr = 'd3'  then Result := 'Delphi 3'
  else if AInstalacao.VersionNumberStr = 'd4'  then Result := 'Delphi 4'
  else if AInstalacao.VersionNumberStr = 'd5'  then Result := 'Delphi 5'
  else if AInstalacao.VersionNumberStr = 'd6'  then Result := 'Delphi 6'
  else if AInstalacao.VersionNumberStr = 'd7'  then Result := 'Delphi 7'
  else if AInstalacao.VersionNumberStr = 'd9'  then Result := 'Delphi 2005'
  else if AInstalacao.VersionNumberStr = 'd10' then Result := 'Delphi 2006'
  else if AInstalacao.VersionNumberStr = 'd11' then Result := 'Delphi 2007'
  else if AInstalacao.VersionNumberStr = 'd12' then Result := 'Delphi 2009'
  else if AInstalacao.VersionNumberStr = 'd14' then Result := 'Delphi 2010'
  else if AInstalacao.VersionNumberStr = 'd15' then Result := 'Delphi XE'
  else if AInstalacao.VersionNumberStr = 'd16' then Result := 'Delphi XE2'
  else if AInstalacao.VersionNumberStr = 'd17' then Result := 'Delphi XE3'
  else if AInstalacao.VersionNumberStr = 'd18' then Result := 'Delphi XE4'
  else if AInstalacao.VersionNumberStr = 'd19' then Result := 'Delphi XE5'
  else if AInstalacao.VersionNumberStr = 'd20' then Result := 'Delphi XE6'
  else if AInstalacao.VersionNumberStr = 'd21' then Result := 'Delphi XE7'
  else if AInstalacao.VersionNumberStr = 'd22' then Result := 'Delphi XE8'
  else if AInstalacao.VersionNumberStr = 'd23' then Result := 'Delphi 10 Seattle'
  else if AInstalacao.VersionNumberStr = 'd24' then Result := 'Delphi 10.1 Berlin'
  else if AInstalacao.VersionNumberStr = 'd25' then Result := 'Delphi 10.2 Tokyo'
  else if AInstalacao.VersionNumberStr = 'd26' then Result := 'Delphi 10.3 Rio'
  else if AInstalacao.VersionNumberStr = 'd27' then Result := 'Delphi 10.4 Sydney'
  else if AInstalacao.VersionNumberStr = 'd28' then Result := 'Delphi 11 Alexandria'
  else if AInstalacao.VersionNumberStr = 'd29' then Result := 'Delphi 12 Athens'
  else if AInstalacao.VersionNumberStr = 'd37' then Result := 'Delphi 13 Florence'
  else Result := AInstalacao.Name;
end;

// A partir do BDS 9 (XE2) o Delphi passou a compilar para mais de uma
// plataforma e a separar as configurações por plataforma.
function PossuiOutrasPlataformas(AInstalacao: TJclBorRADToolInstallation): Boolean;
begin
  Result := (AInstalacao is TJclBDSInstallation) and
            (AInstalacao.IDEVersionNumber >= 9) and
            (not AInstalacao.IsTurboExplorer);
end;

function GeraListaAlvos: TListaAlvos;
var
  I: Integer;
  LInstalacao: TJclBorRADToolInstallation;
begin
  Result := TListaAlvos.Create;
  for I := 0 to Result.Instalacoes.Count - 1 do
  begin
    LInstalacao := Result.Instalacoes.Installations[I];

    // Win32 sempre existe
    Result.Add(TPlataformaAlvo.Create(LInstalacao, bpWin32, BDSPlatformWin32));

    if PossuiOutrasPlataformas(LInstalacao) and
       (bpDelphi64 in LInstalacao.Personalities) then
      Result.Add(TPlataformaAlvo.Create(LInstalacao, bpWin64, BDSPlatformWin64));
  end;
end;

{ TListaAlvos }

constructor TListaAlvos.Create;
begin
  inherited Create(True);
  FInstalacoes := TJclBorRADToolInstallations.Create;
end;

destructor TListaAlvos.Destroy;
begin
  // os alvos apontam para as instalações, então saem antes delas
  Clear;
  FInstalacoes.Free;
  inherited;
end;

{ TPlataformaAlvo }

constructor TPlataformaAlvo.Create(AInstalacao: TJclBorRADToolInstallation;
  APlataforma: TJclBDSPlatform; const ANomePlataforma: String);
begin
  inherited Create;
  Instalacao     := AInstalacao;
  Plataforma     := APlataforma;
  NomePlataforma := ANomePlataforma;
  DirLibraryRaiz := '';
end;

function TPlataformaAlvo.NomeIDE: String;
begin
  Result := NomeAmigavelIDE(Instalacao);
end;

function TPlataformaAlvo.NomeAlvo: String;
begin
  Result := NomeIDE + ' ' + NomePlataforma;
end;

function TPlataformaAlvo.SubDirLibrary: String;
begin
  Result := 'Binary\Lib' + AnsiUpperCase(Instalacao.VersionNumberStr) +
            '\' + NomePlataforma;
end;

function TPlataformaAlvo.EhSuportado: Boolean;
begin
  Result := (not MatchText(Instalacao.VersionNumberStr, ['d3', 'd4', 'd5'])) and
            (Plataforma in PlataformasSuportadas);
end;

procedure TPlataformaAlvo.ConfigurarDCC;
begin
  if (Plataforma = bpWin64) and (Instalacao is TJclBDSInstallation) then
    Instalacao.DCC := (Instalacao as TJclBDSInstallation).DCC64
  else
    Instalacao.DCC := Instalacao.DCC32;
end;

function TPlataformaAlvo.DirLibraryPorModo(const AModo: TModoCompilacao): String;
begin
  Result := ExcludeTrailingPathDelimiter(DirLibraryRaiz) + '\' +
            cNomeModoCompilacao[AModo];
end;

function TPlataformaAlvo.EhIDEComSuporteAPlataformas: Boolean;
begin
  Result := (Instalacao is TJclBDSInstallation) and
            (Instalacao.IDEVersionNumber >= 9);
end;

function TPlataformaAlvo.SuportaIDE64Bits: Boolean;
var
  LRaiz: String;
begin
  Result := False;
  if not (Instalacao is TJclBDSInstallation) then
    Exit;

  if not (bpDelphi64 in Instalacao.Personalities) then
    Exit;

  LRaiz := IncludeTrailingPathDelimiter(Instalacao.RootDir);

  // Detectado pelos arquivos e não pelo número da versão, assim novas versões
  // do Delphi passam a ser suportadas sem alterar o instalador.
  Result := FileExists(LRaiz + 'bin64\bds.exe') and
            FileExists(LRaiz + 'lib\win64\release\designide.dcp');
end;

function TPlataformaAlvo.SuportaPacotesDesignTime: Boolean;
begin
  Result := (Plataforma = bpWin32) or
            ((Plataforma = bpWin64) and SuportaIDE64Bits);
end;

function TPlataformaAlvo.SecaoKnownPackages: String;
begin
  if (Plataforma = bpWin64) then
    Result := cSecaoKnownPackagesX64
  else
    Result := cSecaoKnownPackages;
end;

function TPlataformaAlvo.SecaoLibraryDaPlataforma: String;
begin
  if EhIDEComSuporteAPlataformas then
    Result := cSecaoLibrary + '\' + NomePlataforma
  else
    Result := cSecaoLibrary;
end;

procedure TPlataformaAlvo.LimparPackageCache(const ANomeArquivoBpl: String);
var
  LNomeArquivo: String;
begin
  if not (Instalacao is TJclBDSInstallation) then
    Exit;

  LNomeArquivo := ExtractFileName(ANomeArquivoBpl);
  try
    Instalacao.ConfigData.EraseSection(cSecaoPackageCache + '\' + LNomeArquivo);
    Instalacao.ConfigData.EraseSection(cSecaoPackageCacheX64 + '\' + LNomeArquivo);
  except
    // o cache pode não existir, o que não é um problema
  end;
end;

procedure TPlataformaAlvo.RegistrarPacoteNaIDE(const ANomeArquivoBpl, ADescricao: String);
var
  LDescricao: String;
begin
  LDescricao := Trim(ADescricao);
  if LDescricao = '' then
    LDescricao := ChangeFileExt(ExtractFileName(ANomeArquivoBpl), '');

  LimparPackageCache(ANomeArquivoBpl);
  Instalacao.ConfigData.WriteString(SecaoKnownPackages, ANomeArquivoBpl, LDescricao);
end;

procedure TPlataformaAlvo.RemoverPacoteDaIDE(const ANomeArquivoBpl: String);
begin
  LimparPackageCache(ANomeArquivoBpl);
  Instalacao.ConfigData.DeleteKey(SecaoKnownPackages, ANomeArquivoBpl);
end;

function TPlataformaAlvo.RemoverPacotesFRCEDaIDE(const ASecao: String;
  const ApenasOrfaos: Boolean): Integer;
var
  LPacotes: TStringList;
  I: Integer;
  LArquivo: String;
begin
  Result := 0;
  LPacotes := TStringList.Create;
  try
    Instalacao.ConfigData.ReadSection(ASecao, LPacotes);
    for I := LPacotes.Count - 1 downto 0 do
    begin
      LArquivo := LPacotes[I];

      if Pos(cIdentificacaoPacotes, AnsiUpperCase(LArquivo)) <= 0 then
        Continue;

      // caminhos com macro da IDE ($(BDSBIN)...) são dos pacotes dela
      if Pos('$(', LArquivo) > 0 then
        Continue;

      if ApenasOrfaos and FileExists(LArquivo) then
        Continue;

      LimparPackageCache(LArquivo);
      Instalacao.ConfigData.DeleteKey(ASecao, LArquivo);
      Inc(Result);
    end;
  finally
    LPacotes.Free;
  end;
end;

procedure TPlataformaAlvo.CarregarListaDePath(const AValor: String; ALista: TStrings);
var
  I: Integer;
begin
  // Não usamos DelimitedText para não inserir aspas em caminhos com espaços
  ALista.Text := StringReplace(AValor, ';', sLineBreak, [rfReplaceAll]);
  for I := ALista.Count - 1 downto 0 do
  begin
    ALista[I] := Trim(ALista[I]);
    if ALista[I] = '' then
      ALista.Delete(I);
  end;
end;

function TPlataformaAlvo.MontarPathDaLista(ALista: TStrings): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ALista.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + ALista[I];
  end;
end;

procedure TPlataformaAlvo.AdicionarNosPathsDeBiblioteca(ADiretorios: TStrings);

  procedure Acrescentar(const AValorAtual: String; out ANovoValor: String);
  var
    LLista: TStringList;
    I: Integer;
  begin
    LLista := TStringList.Create;
    try
      CarregarListaDePath(AValorAtual, LLista);
      for I := 0 to ADiretorios.Count - 1 do
      begin
        if LLista.IndexOf(ADiretorios[I]) < 0 then
          LLista.Add(ADiretorios[I]);
      end;
      ANovoValor := MontarPathDaLista(LLista);
    finally
      LLista.Free;
    end;
  end;

var
  LNovo: String;
begin
  if (ADiretorios = nil) or (ADiretorios.Count = 0) then
    Exit;

  Acrescentar(Instalacao.RawLibrarySearchPath[Plataforma], LNovo);
  Instalacao.RawLibrarySearchPath[Plataforma] := LNovo;

  Acrescentar(Instalacao.RawLibraryBrowsingPath[Plataforma], LNovo);
  Instalacao.RawLibraryBrowsingPath[Plataforma] := LNovo;
end;

procedure TPlataformaAlvo.RemoverDoLibrarySearchPath(ADiretorios: TStrings);
var
  LLista: TStringList;
  I, LIndice: Integer;
begin
  if (ADiretorios = nil) or (ADiretorios.Count = 0) then
    Exit;

  LLista := TStringList.Create;
  try
    CarregarListaDePath(Instalacao.RawLibrarySearchPath[Plataforma], LLista);
    for I := 0 to ADiretorios.Count - 1 do
    begin
      LIndice := LLista.IndexOf(ADiretorios[I]);
      if LIndice >= 0 then
        LLista.Delete(LIndice);
    end;
    Instalacao.RawLibrarySearchPath[Plataforma] := MontarPathDaLista(LLista);
  finally
    LLista.Free;
  end;
end;

procedure TPlataformaAlvo.RemoverDosPathsDeBiblioteca(const ATextoProcurar: String);

  function SemOcorrencias(const AValor: String): String;
  var
    LLista: TStringList;
    I: Integer;
  begin
    LLista := TStringList.Create;
    try
      CarregarListaDePath(AValor, LLista);
      for I := LLista.Count - 1 downto 0 do
      begin
        if Pos(AnsiUpperCase(ATextoProcurar), AnsiUpperCase(LLista[I])) > 0 then
          LLista.Delete(I);
      end;
      Result := MontarPathDaLista(LLista);
    finally
      LLista.Free;
    end;
  end;

begin
  if Trim(ATextoProcurar) = '' then
    Exit;

  Instalacao.RawLibrarySearchPath[Plataforma] :=
    SemOcorrencias(Instalacao.RawLibrarySearchPath[Plataforma]);
  Instalacao.RawLibraryBrowsingPath[Plataforma] :=
    SemOcorrencias(Instalacao.RawLibraryBrowsingPath[Plataforma]);
  Instalacao.RawDebugDCUPath[Plataforma] :=
    SemOcorrencias(Instalacao.RawDebugDCUPath[Plataforma]);
end;

procedure TPlataformaAlvo.AdicionarNoDebugDCUPath(const ADiretorio: String);
var
  LLista: TStringList;
begin
  LLista := TStringList.Create;
  try
    CarregarListaDePath(Instalacao.RawDebugDCUPath[Plataforma], LLista);
    if LLista.IndexOf(ADiretorio) < 0 then
    begin
      LLista.Insert(0, ADiretorio);
      Instalacao.RawDebugDCUPath[Plataforma] := MontarPathDaLista(LLista);
    end;
  finally
    LLista.Free;
  end;
end;

function TPlataformaAlvo.GetPackageSearchPath: String;
begin
  Result := Instalacao.ConfigData.ReadString(SecaoLibraryDaPlataforma,
                                             cValorPackageSearchPath, '');
end;

procedure TPlataformaAlvo.SetPackageSearchPath(const AValor: String);
begin
  Instalacao.ConfigData.WriteString(SecaoLibraryDaPlataforma,
                                    cValorPackageSearchPath, AValor);
end;

procedure TPlataformaAlvo.AdicionarPackageSearchPath(const ADiretorio: String);
var
  LLista: TStringList;
begin
  if not EhIDEComSuporteAPlataformas then
    Exit;

  LLista := TStringList.Create;
  try
    CarregarListaDePath(GetPackageSearchPath, LLista);
    if LLista.IndexOf(ADiretorio) < 0 then
    begin
      LLista.Insert(0, ADiretorio);
      SetPackageSearchPath(MontarPathDaLista(LLista));
    end;
  finally
    LLista.Free;
  end;
end;

procedure TPlataformaAlvo.RemoverDoPackageSearchPath(const ATextoProcurar: String);
var
  LLista: TStringList;
  I: Integer;
  LAlterou: Boolean;
begin
  if (not EhIDEComSuporteAPlataformas) or (Trim(ATextoProcurar) = '') then
    Exit;

  LAlterou := False;
  LLista := TStringList.Create;
  try
    CarregarListaDePath(GetPackageSearchPath, LLista);
    for I := LLista.Count - 1 downto 0 do
    begin
      if Pos(AnsiUpperCase(ATextoProcurar), AnsiUpperCase(LLista[I])) > 0 then
      begin
        LLista.Delete(I);
        LAlterou := True;
      end;
    end;
    if LAlterou then
      SetPackageSearchPath(MontarPathDaLista(LLista));
  finally
    LLista.Free;
  end;
end;

procedure TPlataformaAlvo.AjustarEnvironmentPath(const ADiretorio, ATextoRemover: String;
  const ExpandirPathSeNecessario: Boolean);
var
  PathsAtuais: String;
  ListaPaths: TStringList;
  I: Integer;
begin
  PathsAtuais := Instalacao.ConfigData.ReadString(cSecaoEnvironmentVars, 'PATH', '$(PATH)');

  if ExpandirPathSeNecessario then
  begin
    // tentar ler o path configurado na ide do delphi, se não existir ler
    // a atual para complementar e fazer o override
    if PathsAtuais = '$(PATH)' then
      PathsAtuais := Trim(Instalacao.EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');
  end;

  ListaPaths := TStringList.Create;
  try
    ListaPaths.Delimiter := ';';
    ListaPaths.StrictDelimiter := True;
    ListaPaths.DelimitedText := PathsAtuais;

    // remover as entradas antigas do FortesReport, de qualquer plataforma
    if (Trim(ATextoRemover) <> '') then
    begin
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos(AnsiUpperCase(ATextoRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
    end;

    // adicionar a pasta da biblioteca desta plataforma, sem duplicar
    if (Trim(ADiretorio) <> '') and (ListaPaths.IndexOf(ADiretorio) < 0) then
      ListaPaths.Insert(0, ADiretorio);

    Instalacao.ConfigData.WriteString(cSecaoEnvironmentVars, 'PATH', ListaPaths.DelimitedText);
  finally
    ListaPaths.Free;
  end;
end;

end.
