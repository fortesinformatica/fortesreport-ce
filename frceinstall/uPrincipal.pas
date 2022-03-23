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
  System.Types, Vcl.Imaging.jpeg;

type
  TDestino = (tdSystem, tdDelphi, tdNone);

  TfrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizMapa: TJvWizardRouteMapNodes;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizPgObterFontes: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    wizPgFinalizar: TJvWizardInteriorPage;
    wizPgInicio: TJvWizardWelcomePage;
    Label4: TLabel;
    Label5: TLabel;
    edtDelphiVersion: TComboBox;
    edtPlatform: TComboBox;
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
    procedure edtDelphiVersionChange(Sender: TObject);
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
  private
    FCountErros: Integer;
    oFRCE: TJclBorRADToolInstallations;
    iVersion: Integer;
    tPlatform: TJclBDSPlatform;
    sDirRoot: string;
    sDirLibrary: string;
    sDirPackage: string;
    sDestino   : TDestino;
    sPathBin   : String;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure AddLibrarySearchPath;
    procedure OutputCallLine(const Text: string);
    procedure SetPlatformSelected;
    function IsCheckOutJaFeito(const ADiretorio: String): Boolean;
    procedure CreateDirectoryLibrarysNotExist;
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog: String;
    procedure ExtrairDiretorioPacote(NomePacote: string);
    procedure WriteToTXT( const ArqTXT, AString : AnsiString;
      const AppendIfExists : Boolean = True; AddLineBreak : Boolean = True );
    procedure AddLibraryPathToDelphiPath(const APath, AProcurarRemover: String);
    procedure FindDirs(ADirRoot: String; bAdicionar: Boolean = True);
    procedure CopiarArquivosToLib;
    procedure InstalarOPacoteNoDelphi(const NomePacote: String);
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  SVN_Class,
  {$WARNINGS off} FileCtrl, {$WARNINGS on} ShellApi, IniFiles, StrUtils, Math,
  Registry;

{$R *.dfm}

procedure TfrmPrincipal.WriteToTXT(const ArqTXT, AString : AnsiString ;
   const AppendIfExists: Boolean; AddLineBreak : Boolean) ;
var
  FS : TFileStream ;
  LineBreak : AnsiString ;
begin
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

procedure TfrmPrincipal.ExtrairDiretorioPacote(NomePacote: string);

  procedure FindDirPackage(sDir, sPacote: String);
  var
    oDirList: TSearchRec;
    iRet: Integer;
    sDirDpk: string;
  begin
    sDir := IncludeTrailingPathDelimiter(sDir);
    if not DirectoryExists(sDir) then
       Exit;

    iRet := FindFirst(sDir + '*.*', faAnyFile, oDirList);
    try
      while (iRet = 0) do
      begin
        iRet := FindNext(oDirList);
        if (oDirList.Name = '.')  or (oDirList.Name = '..') or (oDirList.Name = '__history') then
          Continue;

        //if oDirList.Attr = faDirectory then
        if DirectoryExists(sDir + oDirList.Name) then
          FindDirPackage(sDir + oDirList.Name, sPacote)
        else
        begin
          if oDirList.Name = sPacote then
            sDirPackage := IncludeTrailingPathDelimiter(sDir);
        end;
      end;
    finally
      SysUtils.FindClose(oDirList);
    end;
  end;

begin
   sDirPackage := '';
   FindDirPackage(sDirRoot + 'Packages', NomePacote);
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

// retornar o caminho completo para o arquivo de logs
function TfrmPrincipal.PathArquivoLog: String;
begin
  Result := PathApp +
    'log_' + StringReplace(edtDelphiVersion.Text, ' ', '_', [rfReplaceAll]) + '.txt';
end;

// verificar se no caminho informado já existe o .svn indicando que o
// checkout já foi feito no diretorio
function TfrmPrincipal.IsCheckOutJaFeito(const ADiretorio: String): Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(ADiretorio) + '.svn')
end;

// ler o arquivo .ini de configurações e setar os campos com os valores lidos
procedure TfrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    edtDirDestino.Text          := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', ExtractFilePath(ParamStr(0)));
    edtPlatform.ItemIndex       := edtPlatform.Items.IndexOf(ArqIni.ReadString('CONFIG', 'Plataforma', 'Win32'));
    edtDelphiVersion.ItemIndex  := edtDelphiVersion.Items.IndexOf(ArqIni.ReadString('CONFIG', 'DelphiVersao', ''));
    ckbFecharTortoise.Checked   := ArqIni.ReadBool('CONFIG', 'FecharTortoise', True);

    if Trim(edtDelphiVersion.Text) = '' then
      edtDelphiVersion.ItemIndex := 0;

    edtDelphiVersionChange(edtDelphiVersion);

  finally
    ArqIni.Free;
  end;
end;

// gravar as configurações efetuadas pelo usuário
procedure TfrmPrincipal.GravarConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteString('CONFIG', 'DelphiVersao', edtDelphiVersion.Text);
    ArqIni.WriteString('CONFIG', 'Plataforma', edtPlatform.Text);
    ArqIni.WriteBool('CONFIG', 'FecharTortoise', ckbFecharTortoise.Checked);
  finally
    ArqIni.Free;
  end;
end;

// criação dos diretórios necessários
procedure TfrmPrincipal.CreateDirectoryLibrarysNotExist;
begin
  // Checa se existe diretório da plataforma
  if not DirectoryExists(sDirLibrary) then
    ForceDirectories(sDirLibrary);
end;

procedure TfrmPrincipal.CopiarArquivosToLib;
  procedure Copiar(const Extensao : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(IncludeTrailingPathDelimiter(sDirRoot) + 'Source', Extensao ,TSearchOption.soAllDirectories ) ;
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo :=  ExtractFileName(ListArquivos[i]);
      CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(sDirLibrary)+ Arquivo) ,True);
    end;
  end;
begin
  // remover os path com o segundo parametro
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Source', False);

  Copiar('*.dcr');
  Copiar('*.res');
  Copiar('*.dfm');
  Copiar('*.ini');
	Copiar('*.inc');
end;

procedure TfrmPrincipal.AddLibraryPathToDelphiPath(const APath: String; const AProcurarRemover: String);
const
  cs: PChar = 'Environment Variables';
var
  lParam, wParam: Integer;
  aResult: Cardinal;
  ListaPaths: TStringList;
  I: Integer;
  PathsAtuais: String;
  PathFonte: string;
begin
  with oFRCE.Installations[iVersion] do
  begin
    // tentar ler o path configurado na ide do delphi, se não existir ler
    // a atual para complementar e fazer o override
    PathsAtuais := Trim(EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');

    // manipular as strings
    ListaPaths := TStringList.Create;
    try
      ListaPaths.Delimiter       := ';';
      ListaPaths.StrictDelimiter := True;
      ListaPaths.DelimitedText   := PathsAtuais;

      // verificar se existe algo do Fortes e remover
      if Trim(AProcurarRemover) <> '' then
      begin
        for I := ListaPaths.Count - 1 downto 0 do
        begin
         if Pos(AnsiUpperCase(AProcurarRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
           ListaPaths.Delete(I);
        end;
      end;

      // adicionar o path
      ListaPaths.Add(APath);

      // escrever a variavel no override da ide
      ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);

      // enviar um broadcast de atualização para o windows
      wParam := 0;
      lParam := LongInt(cs);
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, aResult);
      if aResult <> 0 then
        raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(aResult));
    finally
      ListaPaths.Free;
    end;
  end;


end;

procedure TfrmPrincipal.FindDirs(ADirRoot: String; bAdicionar: Boolean = True);
var
  oDirList: TSearchRec;
  iRet: Integer;
begin
  ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

  iRet := FindFirst(ADirRoot + '*.*', faDirectory, oDirList);
  if iRet = 0 then
  begin
     try
       repeat
          if ((oDirList.Attr and faDirectory) <> 0) and
              (oDirList.Name <> '.')                and
              (oDirList.Name <> '..')               and
              (oDirList.Name <> '__history')        then
          begin
             with oFRCE.Installations[iVersion] do
             begin
               if bAdicionar then
               begin
                  AddToLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
                  AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, tPlatform);
               end
               else
                  RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
             end;
             //-- Procura subpastas
             FindDirs(ADirRoot + oDirList.Name, bAdicionar);
          end;
          iRet := FindNext(oDirList);
       until iRet <> 0;
     finally
       SysUtils.FindClose(oDirList)
     end;
  end;
end;

// adicionar o paths ao library path do delphi
procedure TfrmPrincipal.AddLibrarySearchPath;
begin
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Source');

  // --
  with oFRCE.Installations[iVersion] do
  begin
    AddToLibraryBrowsingPath(sDirLibrary, tPlatform);
    AddToLibrarySearchPath(sDirLibrary, tPlatform);
    AddToDebugDCUPath(sDirLibrary, tPlatform);
  end;

  // -- adicionar a library path ao path do windows
  AddLibraryPathToDelphiPath(sDirLibrary, 'frce');

end;

// setar a plataforma de compilação
procedure TfrmPrincipal.SetPlatformSelected;
var
  sVersao: String;
  sTipo: String;
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sVersao  := AnsiUpperCase(oFRCE.Installations[iVersion].VersionNumberStr);
  sDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);

  sTipo := 'Binary\';

  if edtPlatform.ItemIndex = 0 then // Win32
  begin
    tPlatform   := bpWin32;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao;
  end
  else
  if edtPlatform.ItemIndex = 1 then // Win64
  begin
    tPlatform   := bpWin64;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao + 'x64';
  end;
end;

// Evento disparado a cada ação do instalador
procedure TfrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(Text));
end;

// evento para setar os parâmetros do compilador antes de compilar
procedure TfrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
begin
  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if oFRCE.Installations[iVersion].SupportsNoConfig then
    Sender.Options.Add('--no-config');

  // -B = Build all units
  Sender.Options.Add('-B');
  // O+ = Optimization
  Sender.Options.Add('-$O-');
  // W- = Generate stack frames
  Sender.Options.Add('-$W+');
  // Y+ = Symbol reference info
  Sender.Options.Add('-$Y-');
  // -M = Make modified units
  Sender.Options.Add('-M');
  // -Q = Quiet compile
  Sender.Options.Add('-Q');
  // não mostrar warnings
  Sender.Options.Add('-H-');
  // não mostrar hints
  Sender.Options.Add('-W-');
  // -D<syms> = Define conditionals
  Sender.Options.Add('-DRELEASE');
  // -U<paths> = Unit directories
  Sender.AddPathOption('U', oFRCE.Installations[iVersion].LibFolderName[tPlatform]);
  Sender.AddPathOption('U', oFRCE.Installations[iVersion].LibrarySearchPath[tPlatform]);
  Sender.AddPathOption('U', sDirLibrary);
  // -I<paths> = Include directories
  Sender.AddPathOption('I', oFRCE.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -R<paths> = Resource directories
  Sender.AddPathOption('R', oFRCE.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -N0<path> = unit .dcu output directory
  Sender.AddPathOption('N0', sDirLibrary);
  Sender.AddPathOption('LE', sDirLibrary);
  Sender.AddPathOption('LN', sDirLibrary);

  //
  with oFRCE.Installations[iVersion] do
  begin
     // -- Path para instalar os pacotes do Rave no D7, nas demais versões
     // -- o path existe.
     if VersionNumberStr = 'd7' then
        Sender.AddPathOption('U', oFRCE.Installations[iVersion].RootDir + '\Rave5\Lib');

     // -- Na versão XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
     // -- os prefixos dos nomes, para identificar se será compilado para VCL ou FMX
     if VersionNumberStr = 'd16' then
        Sender.Options.Add('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

     if MatchText(VersionNumberStr, ['d17','d18','d19','d20','d21','d22','d23','d24','d25','d26','d27','d28']) then
        Sender.Options.Add('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');

  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  iFor: Integer;
begin
  iVersion    := -1;
  sDirRoot    := '';
  sDirLibrary := '';
  sDirPackage := '';

  oFRCE := TJclBorRADToolInstallations.Create;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to oFRCE.Count - 1 do
  begin
    if      oFRCE.Installations[iFor].VersionNumberStr = 'd3' then
      edtDelphiVersion.Items.Add('Delphi 3')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd4' then
      edtDelphiVersion.Items.Add('Delphi 4')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd5' then
      edtDelphiVersion.Items.Add('Delphi 5')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd6' then
      edtDelphiVersion.Items.Add('Delphi 6')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd7' then
      edtDelphiVersion.Items.Add('Delphi 7')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd9' then
      edtDelphiVersion.Items.Add('Delphi 2005')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd10' then
      edtDelphiVersion.Items.Add('Delphi 2006')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd11' then
      edtDelphiVersion.Items.Add('Delphi 2007')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd12' then
      edtDelphiVersion.Items.Add('Delphi 2009')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd14' then
      edtDelphiVersion.Items.Add('Delphi 2010')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd15' then
      edtDelphiVersion.Items.Add('Delphi XE')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd16' then
      edtDelphiVersion.Items.Add('Delphi XE2')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd17' then
      edtDelphiVersion.Items.Add('Delphi XE3')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd18' then
      edtDelphiVersion.Items.Add('Delphi XE4')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd19' then
      edtDelphiVersion.Items.Add('Delphi XE5')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd20' then
      edtDelphiVersion.Items.Add('Delphi XE6')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd21' then
      edtDelphiVersion.Items.Add('Delphi XE7')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd22' then
      edtDelphiVersion.Items.Add('Delphi XE8')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd23' then
      edtDelphiVersion.Items.Add('Delphi 10 Seattle')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd24' then
      edtDelphiVersion.Items.Add('Delphi 10.1 Berlin')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd25' then
      edtDelphiVersion.Items.Add('Delphi 10.2 Tokyo')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd26' then
      edtDelphiVersion.Items.Add('Delphi 10.3 Rio')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd27' then
      edtDelphiVersion.Items.Add('Delphi 10.4 Sydney')
    else if oFRCE.Installations[iFor].VersionNumberStr = 'd28' then
      edtDelphiVersion.Items.Add('Delphi 11 Alexandria');

    // -- Evento disparado antes de iniciar a execução do processo.
    oFRCE.Installations[iFor].DCC32.OnBeforeExecute := BeforeExecute;

    // -- Evento para saidas de mensagens.
    oFRCE.Installations[iFor].OutputCallback := OutputCallLine;
  end;

  if edtDelphiVersion.Items.Count > 0 then
  begin
    edtDelphiVersion.ItemIndex := 0;
    iVersion := 0;
  end;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oFRCE.Free;
end;

// botão de compilação e instalação dos pacotes selecionados no treeview
procedure TfrmPrincipal.btnInstalarfrceClick(Sender: TObject);
var
//  iDpk: Integer;
  bRunOnly: Boolean;
  NomePacote: String;
  Cabecalho: String;
  function ExistePacoteDesingtimeParaPacote(NomeArquivoPacoteDCL: string): boolean;
  var
    bRunOnlyLocal:Boolean;
  begin
    Result := False;
    if not FileExists(NomeArquivoPacoteDCL) then
    begin
      Exit;
    end;
    if not IsDelphiPackage(NomeArquivoPacoteDCL) then
    begin
      Exit;
    end;
    GetDPKFileInfo(NomeArquivoPacoteDCL, bRunOnlyLocal);
    Result := not bRunOnlyLocal;
  end;

  procedure MostrarMensagemInstalado(const aMensagem: String; const aErro: String = '');
  var
    Msg: String;
  begin

    if Trim(aErro) = EmptyStr then
    begin
      case sDestino of
//        tdSystem: Msg := Format(aMensagem + ' em "%s"', [PathSystem]);
        tdDelphi: Msg := Format(aMensagem + ' em "%s"', [sPathBin]);
        tdNone:   Msg := 'Tipo de destino "nenhum" não aceito!';
      end;
    end
    else
    begin
      Inc(FCountErros);

      case sDestino of
//        tdSystem: Msg := Format(aMensagem + ' em "%s": "%s"', [PathSystem, aErro]);
        tdDelphi: Msg := Format(aMensagem + ' em "%s": "%s"', [sPathBin, aErro]);
        tdNone:   Msg := 'Tipo de destino "nenhum" não aceito!';
      end;
    end;

    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));
    WriteToTXT(AnsiString(PathArquivoLog), Msg);

    lstMsgInstalacao.Items.Add(Msg);
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  end;

begin
  FCountErros := 0;

  btnInstalarfrce.Enabled := False;
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
  try
    Cabecalho := 'Caminho: ' + edtDirDestino.Text + sLineBreak +
                 'Versão do delphi: ' + edtDelphiVersion.Text + ' (' + IntToStr(iVersion)+ ')' + sLineBreak +
                 'Plataforma: ' + edtPlatform.Text + '(' + IntToStr(Integer(tPlatform)) + ')' + sLineBreak +
                 StringOfChar('=', 80);

    // limpar o log
    lstMsgInstalacao.Clear;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(Cabecalho), False);

    // setar barra de progresso
    pgbInstalacao.Position := 0;
    pgbInstalacao.Max := 5;

    // Seta a plataforna selecionada
    SetPlatformSelected;
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    lstMsgInstalacao.Items.Add('Setando parâmetros de plataforma...');
    Application.ProcessMessages;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString('Setando parâmetros de plataforma...'));

    // Cria diretório de biblioteca da versão do delphi selecionada,
    // só será criado se não existir
    CreateDirectoryLibrarysNotExist;
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    lstMsgInstalacao.Items.Add('Criando diretórios de bibliotecas...');
    Application.ProcessMessages;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString('Criando diretórios de bibliotecas...'));

    // Adiciona os paths dos fontes na versão do delphi selecionada
    AddLibrarySearchPath;
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    lstMsgInstalacao.Items.Add('Adicionando library paths...');
    Application.ProcessMessages;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString('Adicionando library paths...'));

    // compilar os pacotes primeiramente
    lstMsgInstalacao.Items.Add('');
    lstMsgInstalacao.Items.Add('COMPILANDO OS PACOTES...');

    NomePacote := 'frce.dpk';
    // Busca diretório do pacote
    ExtrairDiretorioPacote(NomePacote);

    if (IsDelphiPackage(NomePacote)) then
    begin
      WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));

      if oFRCE.Installations[iVersion].CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
      begin
        lstMsgInstalacao.Items.Add(Format('Pacote "%s" compilado com sucesso.', [NomePacote]));
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
      end
      else
      begin
        Inc(FCountErros);
        lstMsgInstalacao.Items.Add(Format('Erro ao compilar o pacote "%s".', [NomePacote]));
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
      end;
    end;

    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    Application.ProcessMessages;


    // instalar os pacotes somente se não ocorreu erro na compilação e plataforma for Win32
    if (edtPlatform.ItemIndex = 0) then
    begin
      if (FCountErros <= 0) then
      begin
        lstMsgInstalacao.Items.Add('');
        lstMsgInstalacao.Items.Add('INSTALANDO OS PACOTES...');
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;

        // Busca diretório do pacote
        ExtrairDiretorioPacote(NomePacote);

        if IsDelphiPackage(NomePacote) then
        begin
          // instalar somente os pacotes de designtime
          GetDPKFileInfo(sDirPackage + NomePacote, bRunOnly);
          if not bRunOnly then
          begin
            InstalarOPacoteNoDelphi(NomePacote);
          end
          else
          begin
            //procura um pacote desingtime para o pacote RunOnly
            if ExistePacoteDesingtimeParaPacote(sDirPackage + 'dcl'+ NomePacote) then
              InstalarOPacoteNoDelphi('dcl'+ NomePacote);
          end;
        end;
        pgbInstalacao.Position := pgbInstalacao.Position + 1;
        Application.ProcessMessages;

        CopiarArquivosToLib;
      end
      else
      begin
        lstMsgInstalacao.Items.Add('');
        lstMsgInstalacao.Items.Add('Abortando... Ocorreram erros na compilação dos pacotes.');
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
      end;
    end
    else
    begin
      lstMsgInstalacao.Items.Add('');
      lstMsgInstalacao.Items.Add('Para a plataforma de 64 bits os pacotes são somente compilados.');
      lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
    end;

  finally
    btnInstalarfrce.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, FCountErros = 0);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;

  if FCountErros = 0 then
  begin
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
        'Ocorreram erros durante o processo de instalação, '+sLineBreak+
        'para maiores informações verifique o arquivo de log gerado.'+sLineBreak+sLineBreak+
        'Deseja visualizar o arquivo de log gerado?'
      ),
      'Instalação',
      MB_ICONQUESTION + MB_YESNO
    ) = ID_YES then
    begin
      btnVisualizarLogCompilacao.Click;
    end;
  end;
  //-- Copiar todas as BPLs para a pasta SYSTEM do windows, isso é necessário
  //-- por motivo do delphi ao iniciar buscar as BPLs nas pastas SYSTEM
  //-- se não tiver na poasta padrão dele.
//  CopiarArquivosBPLsSystem;
end;

// chama a caixa de dialogo para selecionar o diretório de instalação
// seria bom que a caixa fosse aquele que possui o botão de criar pasta
procedure TfrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir, [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

// quando trocar a versão verificar se libera ou não o combo
// da plataforma de compilação
procedure TfrmPrincipal.edtDelphiVersionChange(Sender: TObject);
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sPathBin := IncludeTrailingPathDelimiter(oFRCE.Installations[iVersion].BinFolderName);
  // -- Plataforma só habilita para Delphi XE2
  // -- Desabilita para versão diferente de Delphi XE2
  edtPlatform.Enabled := oFRCE.Installations[iVersion].VersionNumber >= 9;
  if oFRCE.Installations[iVersion].VersionNumber < 9 then
    edtPlatform.ItemIndex := 0;

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
  if oFRCE.AnyInstanceRunning then
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
begin
  SetPlatformSelected;
  lstMsgInstalacao.Clear;
  pgbInstalacao.Position := 0;

  // para 64 bit somente compilar
  if tPlatform = bpWin32 then // Win32
    btnInstalarfrce.Caption := 'Instalar'
  else // win64
    btnInstalarfrce.Caption := 'Compilar';

  // mostrar ao usuário as informações de compilação
  with lbInfo.Items do
  begin
    Clear;
    Add(edtDelphiVersion.Text + ' ' + edtPlatform.Text);
    Add('Dir. Instalação  : ' + edtDirDestino.Text);
    Add('Dir. Bibliotecas : ' + sDirLibrary);
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
begin
  if Pos(oFRCE.Installations[iVersion].VersionNumberStr, 'd3, d4, d5') > 0 then
  begin
    Stop := True;
    edtDelphiVersion.SetFocus;
    Application.MessageBox(
      'Versão do delphi não suportada pelo FRCE.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

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
  end;

  // prevenir versão do delphi em branco
  if Trim(edtDelphiVersion.Text) = '' then
  begin
    Stop := True;
    edtDelphiVersion.SetFocus;
    Application.MessageBox(
      'Versão do delphi não foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // prevenir plataforma em branco
  if Trim(edtPlatform.Text) = '' then
  begin
    Stop := True;
    edtPlatform.SetFocus;
    Application.MessageBox(
      'Plataforma de compilação não foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

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
  if TButton(Sender).Tag > 0 then
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

procedure TfrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(PathArquivoLog), '', '', 1);
end;

procedure TfrmPrincipal.InstalarOPacoteNoDelphi(const NomePacote: String);
begin
  WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));

  if oFRCE.Installations[iVersion].InstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
  begin
    lstMsgInstalacao.Items.Add(Format('Pacote "%s" instalado com sucesso.', [NomePacote]));
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  end
  else
  begin
    Inc(FCountErros);
    lstMsgInstalacao.Items.Add(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  end;
//  else
//  begin
//    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));
//
//    if oFRCE.Installations[iVersion].UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
//    begin
//      lstMsgInstalacao.Items.Add(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
//      lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
//    end;
//  end;
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
