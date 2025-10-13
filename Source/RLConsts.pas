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

{$I RLReport.inc}

{@unit RLConsts - Variáveis de internacionalização e variáveis de configuração. }

unit RLConsts;

interface

uses
  Classes,
  {$IfDef CLX}
   QDialogs,
  {$Else}
   Dialogs,
  {$EndIf}
  SysUtils;

const
  {@const ScreenPPI - Resolução do monitor em pixels por polegada.
   Representa a quantidade de pixels por polegada do vídeo. O valor real varia de monitor para monitor mas,
   para facilitar cálculos e tornar os projetos independentes do terminal, essa valor é assumido como sendo 96. :/}
//  ScreenPPI = 96;

  {@const InchAsMM - Fator de conversão de polegada para milímetros.
   Este fator é utilizado em diversos pontos para conversões de coordenadas. :/}
  InchAsMM = 254 / 10;

  {@const MMAsPixels - Fator de conversão de milímetros para pixels de tela.
   @links ScreenPPI, InchAsMM. :/}
//  MMAsPixels = ScreenPPI / InchAsMM;

  MaxPageNo = 999999;

  HARD_LINEBREAK = [#13, #10];  // CR, LF
  SOFT_LINEBREAK = [#9,' ','-','.',',']; // TAB

  ReportFileExt = '.rpf';

  { constantes para exibição na inicialização e no sobre do delphi a partir da versão 2009 }
  cRLSobreDialogoTitulo = 'FortesReport Community Edition';
  cRLSobreTitulo = 'FortesReport Community Edition VCL';
  cRLSobreDescricao = 'FortesReport Community Edition VCL ' + sLineBreak +
                      'http://www.fortesreport.com.br' + sLineBreak +
                      'https://github.com/fortesinformatica/fortesreport-ce' + sLineBreak +
                      'Componentes para Geração de Relatórios' + sLineBreak +
                      'Lesser General Public License version 2.0';
  cRLSobreLicencaStatus = 'LGPLv2';
  
  {****                                  *}	

const
  CS_CopyrightStr = 'Copyright © 1999-2021 Fortes Informática';
  CS_ProductTitleStr = 'FortesReport Community Edition';
  CS_URLStr = 'http://www.fortesreport.com.br';
  CS_URLGitHubStr = 'https://github.com/fortesinformatica/fortesreport-ce';
  CS_AuthorNameStr = 'Ronaldo Moreira' + sLineBreak +
                     'Márcio Martins' + sLineBreak +
					 'Daniel Simões de Almeida' + sLineBreak +
                     'Régys Borges da Silveira' + sLineBreak +
                     'Juliomar Marchetti';
  CS_Version = '4.0.1.2';

type
  TRLLocaleStrings = record
    {@var LocaleStrings.LS_PrintingInProgressStr - Variável de internacionalização para "Imprimindo o relatório..." :/}
    LS_PrintingInProgressStr: string;
    {@var LS_FilterInProgressStr - Variável de internacionalização para "Salvando o relatório..." :/}
    LS_FilterInProgressStr: string;
    {@var LS_PreparingReportStr - Variável de internacionalização para "Preparando o relatório..." :/}
    LS_PreparingReportStr: string;
    {@var LS_PrinterNotFoundStr - Variável de internacionalização para "Nenhuma impressora encontrada" :/}
    LS_PrinterNotFoundStr: string;
    {@var LS_NoPrinterSelected - Variável de internacionalização para "Nenhuma impressora selecionada" :/}
    LS_NoPrinterSelected: string;
    {@var LS_NoPathToPrinterStr - Variável de internacionalização para "Caminho inválido para a impressora" :/}
    LS_NoPathToPrinterStr: string;
    {@var LS_LoadDefaultConfigStr - Variável de internacionalização para "Será carregada a configuração padrão" :/}
    LS_LoadDefaultConfigStr: string;
    {@var LS_PrinterDriverErrorStr - Variável de internacionalização para "Erro no driver da impressora" :/}
    LS_PrinterDriverErrorStr: string;
    {@var LS_PageStr - Variável de internacionalização para "Página" :/}
    LS_PageStr: string;
    {@var LS_PrepareErrorStr - Variável de internacionalização para "Erro durante a preparação do relatório" :/}
    LS_PrepareErrorStr: string;
    {@var LS_PageBreakStr - Variável de internacionalização para "Continua..." :/}
    LS_PageBreakStr: string;
    {@var LS_PageMendStr - Variável de internacionalização para "Continuação" :/}
    LS_PageMendStr: string;
    {@var LS_ReportEndStr - Variável de internacionalização para "Fim" :/}
    LS_ReportEndStr: string;
    {@var LS_FileExists - Translation variable for "File already exists. Overwrite?". :/}
    LS_FileExists: string;
    {@var LS_FileNotFoundStr - Variável de internacionalização para "Arquivo não encontrado" :/}
    LS_FileNotFoundStr: string;
    {@var LS_FileNameIsEmpty - Variável de internacionalização para "Nome do arquivo não especificado" :/}
    LS_FileNameIsEmpty: string;
    {@var LS_FileNameStr - Variável de internacionalização para "Nome do arquivo" :/}
    LS_FileNameStr: string;
    {@var LS_AllFileTypesStr - Variável de internacionalização para "Todos os arquivos" :/}
    LS_AllFileTypesStr: string;
    {@var LS_LoadReportStr - Variável de internacionalização para "Carregar relatório" :/}
    LS_LoadReportStr: string;
    {@var LS_NotFoundStr - Variável de internacionalização para "Não encontrado" :/}
    LS_NotFoundStr: string;
    {@var LS_WaitStr - Variável de internacionalização para "Aguarde..." :/}
    LS_WaitStr: string;
    {@var LS_FinishedStr - Variável de internacionalização para "Concluído" :/}
    LS_FinishedStr: string;
    {@var LS_CancelStr - Variável de internacionalização para "Cancelar" :/}
    LS_CancelStr: string;
    {@var LS_CloseStr - Variável de internacionalização para "Fechar" :/}
    LS_CloseStr: string;
    {@var LS_SaveStr - Variável de internacionalização para "Salvar" :/}
    LS_SaveStr: string;
    {@var LS_SendStr - Variável de internacionalização para "Enviar" :/}
    LS_SendStr: string;
    {@var LS_PrintStr - Variável de internacionalização para "Imprimir" :/}
    LS_PrintStr: string;
    {@var LS_AboutTheStr - Variável de internacionalização para "Sobre o" :/}
    LS_AboutTheStr: string;
    {@var LS_PreviewStr - Variável de internacionalização para "Pré-visualização" :/}
    LS_PreviewStr: string;
    {@var LS_OfStr - Variável de internacionalização para "de" :/}
    LS_OfStr: string;
    {@var LS_ZoomStr - Variável de internacionalização para "Zoom" :/}
    LS_ZoomStr: string;
    {@var LS_FirstPageStr - Variável de internacionalização para "Primeira página" :/}
    LS_FirstPageStr: string;
    {@var LS_PriorPageStr - Variável de internacionalização para "Página anterior" :/}
    LS_PriorPageStr: string;
    {@var LS_NextPageStr - Variável de internacionalização para "Próxima página" :/}
    LS_NextPageStr: string;
    {@var LS_LastPageStr - Variável de internacionalização para "Última página" :/}
    LS_LastPageStr: string;
    {@var LS_EntirePageStr - Variável de internacionalização para "Página inteira" :/}
    LS_EntirePageStr: string;
    {@var LS_EntireWidthStr - Variável de internacionalização para "Largura da página" :/}
    LS_EntireWidthStr: string;
    {@var LS_MultiplePagesStr - Variável de internacionalização para "Várias páginas" :/}
    LS_MultiplePagesStr: string;
    {@var LS_ConfigPrinterStr - Variável de internacionalização para "Configurar impressora" :/}
    LS_ConfigPrinterStr: string;
    {@var LS_SaveToFileStr - Variável de internacionalização para "Salvar em disco" :/}
    LS_SaveToFileStr: string;
    {@var LS_SendToStr - Variável de internacionalização para "Enviar para" :/}
    LS_SendToStr: string;
    {@var LS_PrinterStr - Variável de internacionalização para "Impressora" :/}
    LS_PrinterStr: string;
    {@var LS_NameStr - Variável de internacionalização para "Nome" :/}
    LS_NameStr: string;
    {@var LS_PrintToFileStr - Variável de internacionalização para "Imprimir em arquivo" :/}
    LS_PrintToFileStr: string;
    {@var LS_PrintInBackgroundStr - Variável de internacionalização para "Imprimir em segundo plano" :/}
    LS_PrintInBackgroundStr: string;
    {@var LS_OptionsStr - Variável de internacionalização para "Opções" de filtragem. :/}
    LS_OptionsStr: string;
    {@var LS_SaveInBackground - Variável de internacionalização para "Salvar em segundo plano" :/}
    LS_SaveInBackground: string;
    {@var LS_PageRangeStr - Variável de internacionalização para "Intervalo de páginas" :/}
    LS_PageRangeStr: string;
    {@var LS_CopyAsImageStr - Variável de internacionalização para "Copiar como Bitmap" :/}
    LS_CopyAsImageStr: string;
    {@var LS_CopyAsMetafile - Variável de internacionalização para "Copiar como Metafile" :/}
    LS_CopyAsMetafileStr: string;
    {@var LS_RangeFromStr - Variável de internacionalização para "de" :/}
    LS_RangeFromStr: string;
    {@var LS_RangeToStr - Variável de internacionalização para "até" :/}
    LS_RangeToStr: string;
    {@var LS_AllStr - Variável de internacionalização para "Tudo" :/}
    LS_AllStr: string;
    {@var LS_PagesStr - Variável de internacionalização para "Páginas" :/}
    LS_PagesStr: string;
    {@var LS_SelectionStr - Variável de internacionalização para "Seleção" :/}
    LS_SelectionStr: string;
    {@var LS_CopiesStr - Variável de internacionalização para "Cópias" :/}
    LS_CopiesStr: string;
    {@var LS_NumberOfCopiesStr - Variável de internacionalização para "Número de cópias" :/}
    LS_NumberOfCopiesStr: string;
    {@var LS_OkStr - Variável de internacionalização para "Ok" :/}
    LS_OkStr: string;
    {@var LS_DivideScreenStr - Variável de internacionalização para "Dividir a tela" :/}
    LS_DivideScreenStr: string;
    {@var LS_InvalidNameStr - Variável de internacionalização para "Nome inválido" :/}
    LS_InvalidNameStr: string;
    {@var LS_DuplicateNameStr - Variável de internacionalização para "Nome já utilizado" :/}
    LS_DuplicateNameStr: string;
    {@var LS_UseFilterStr - Variável de internacionalização para "Usar Filtro" :/}
    LS_UseFilterStr: string;
    {@var LS_WebPageStr - Variável de internacionalização para "Página da Web" :/}
    LS_WebPageStr: string;
    {@var LS_RichFormatStr - Variável de internacionalização para "Formato RichText" :/}
    LS_RichFormatStr: string;
    {@var LS_PDFFormatStr - Variável de internacionalização para "Documento PDF" :/}
    LS_PDFFormatStr: string;
    {@var LS_XLSFormatStr97-2013 - Variável de internacionalização para "Planilha Excel 97-2013" :/}
    LS_XLSFormatStr97_2013: string;
    {@var LS_XLSFormatStr - Variável de internacionalização para "Planilha Excel" :/}
    LS_XLSFormatStr: string;
    {@var LS_AtStr - Variável de internacionalização para "em" :/}
    LS_AtStr: string;
    {@var LS_FormStr - Variável de internacionalização para "Formulário" :/}
    LS_FormStr: string;
    {@var LS_DefaultStr - Variável de internacionalização para "Padrão" :/}
    LS_DefaultStr: string;
    {@var LS_ZoomInStr - Variável de internacionalização para "Aumentar o zoom" :/}
    LS_ZoomInStr: string;
    {@var LS_ZoomOutStr - Variável de internacionalização para "Diminuir o zoom" :/}
    LS_ZoomOutStr: string;
    {@var LS_CopyStr - Variável de internacionalização para "Copiar" :/}
    LS_CopyStr: string;
    {@var LS_EditStr - Variável de internacionalização para "Editar" :/}
    LS_EditStr: string;
    {@var LS_FindCaptionStr - Variável de internacionalização para "Procurar" :/}
    LS_FindCaptionStr: string;
    {@var LS_TextToFindStr - Variável de internacionalização para "Te&xto" :/}
    LS_TextToFindStr: string;
    {@var LS_FindNextStr - &Variável de internacionalização para "Próxima" :/}
    LS_FindNextStr: string;
    {@var LS_WholeWordsStr - Variável de internacionalização para "Palavras &inteiras" :/}
    LS_WholeWordsStr: string;
    {@var LS_MatchCaseStr - Variável de internacionalização para "Diferenciar &maiúsculas de minúsculas" :/}
    LS_MatchCaseStr: string;
    {@var LS_DirectionUpStr - Variável de internacionalização para "A&cima" :/}
    LS_DirectionUpStr: string;
    {@var LS_DirectionDownStr - Variável de internacionalização para "A&baixo" :/}
    LS_DirectionDownStr: string;
    {@var LS_DirectionCaptionStr - Variável de internacionalização para "Direção" :/}
    LS_DirectionCaptionStr: string;
    {@var LS_ColumnsStr - Variável de internacionalização para "Colunas". :/}
    LS_ColumnsStr: string;
    {@var LS_SetupStr - Variável de internacionalização para "Configuração". :/}
    LS_SetupStr: string;
    {@var LS_FontSizeError - Variável de internacionalização para "Erro no cálculo das fontes". :/}
    LS_FontSizeError: string;
    {@var LS_OddPages - Variável de internacionalização para "Ímpares". :/}
    LS_OddPages: string;
    {@var LS_EvenPages - Variável de internacionalização para "Pares". :/}
    LS_EvenPages: string;
    {@var LS_OddPagesOnly - Variável de internacionalização para "Ímpares somente". :/}
    LS_OddPagesOnly: string;
    {@var LS_EvenPagesOnly - Variável de internacionalização para "Pares somente". :/}
    LS_EvenPagesOnly: string;
    {@var LS_AllOddAndEven - Variável de internacionalização para "Todas". :/}
    LS_AllOddAndEven: string;
    {@var LS_PrintDialogError - Variável de internacionalização para "Problemas com o diálogo da impressora". :/}
    LS_PrintDialogError: string;
    {@var LS_PageSelectionHint - Variável de internacionalização para "Separe com ponto-e-vírgula os números ou intervalos de páginas a imprimir. Ex.: 1;3;5-12;4". :/}
    LS_PageSelectionHint: string;
    {@var LS_DefaultJobTitle - Variável de internacionalização para "Relatório %s". :/}
    LS_DefaultJobTitle: string;
    {@var LS_ZoomHint - Variável de internacionalização para "Diminuir o zoom" :/}
    LS_ZoomHint: string;
    {@var Ls_Aplicar - Variável de internacionalização para "Aplicar". :/}
    Ls_Aplicar: String;
    {@var Ls_Propriedades - Variável de internacionalização para "Propriedades". :/}
    Ls_Propriedades: string;
    {@var Ls_Salvar_Como - Variável de internacionalização para "Salvar como". :/}
    Ls_Salvar_Como: string;
    {@var LS_FileCorrupted - Variável de internacionalização para "Arquivo corrompido". :/}
    LS_FileCorrupted: String;
    {@var LS_FileCorruptedHeader - Translation variable for "Corrupted file header "%s"!". :/}
    LS_FileCorruptedHeader: string;
    {@var LS_FileVersion - Variável de internacionalização para "Versão de Arquivo inválido". :/}
    LS_FileVersion: String;
    {@var Ls_PageSetings - Variável de internacionalização para "Configuração da Página". :/}
    LS_PageSettings: String;
    {@var Ls_Page_margins - Variável de internacionalização para "Margem da Página". :/}
    LS_PageMargins: String;
    {@var Ls_PageMarginsTop - Variável de internacionalização para "Margem Superior". :/}
    LS_PageMarginsTop: String;
    {@var Ls_PageMarginsBottom - Variável de internacionalização para "Margem Inferior". :/}
    LS_PageMarginsBottom: String;
    {@var LS_PageMarginsRigth - Variável de internacionalização para "Margem direita". :/}
    LS_PageMarginsRigth: String;
    {@var Ls_PageLeftBottom - Variável de internacionalização para "Margem equerda". :/}
    LS_PageMarginsLeft: String;
    {@var LS_PageMarginsPaper - Variável de internacionalização para "Margem do Papel". :/}
    LS_PageMarginsPaper: String;
    {@var LS_PagePaper - Variável de internacionalização para "Papel". :/}
    LS_PagePaper: String;
    {@var LS_PaperSize - Variável de internacionalização para "Tamanho do Papel". :/}
    LS_PaperSize: String;
    {@var LS_PaperWidth - Variável de internacionalização para "Largura do Papel". :/}
    LS_PaperSizeWidth: String;
    {@var LS_PaperSizeHeigth - Variável de internacionalização para "Altura do Papel". :/}
    LS_PaperSizeHeigth: String;
    {@var LS_PaperOrientation - Variável de internacionalização para "Orientação do Papel". :/}
    LS_PaperOrientation: String;
    {@var LS_PaperOrientationLandscape - Variável de internacionalização para "Orientação da página em retrato". :/}
    LS_PaperOrientationLandscape: String;
    {@var LS_PaperOrientationPortrait - Variável de internacionalização para "Orientação da página em paisagem". :/}
    LS_PaperOrientationPortrait: String;
    {@var LS_Duplex - Translation variable for "Automatic two-sided printing". :/}
    LS_Duplex: String;
    {@var LS_OnlyOneInstance - Translation variable for "Only one instance of %s is allowed!". :/}
    LS_OnlyOneInstance: String;
    {@var LS_NotImplemented - Translation variable for "%s is not yet implemented for this platform!". :/}
    LS_NotImplemented: String;
    {@var LS_NoHandle - Translation variable for "Handle not available!". :/}
    LS_NoHandle: String;

    LS_LastFooMsg: string;
  end;

var
  LocaleStrings: TRLLocaleStrings;
  ReportServiceMode: Boolean = False;

procedure DetectLocale;

{/@unit}


implementation

var
  KnownCommercialVersion: Integer = 0;
  KnownReleaseVersion: Integer = 0;
  KnownCommentVersion: string = '';

var
  EnglishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Printing in progress...';
    LS_FilterInProgressStr: 'Saving report...';
    LS_PreparingReportStr: 'Preparing report...';
    LS_PrinterNotFoundStr: 'Printer not found';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Invalid printer path';
    LS_LoadDefaultConfigStr: 'Load default configuration';
    LS_PrinterDriverErrorStr: 'Printer driver error';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Error while preparing report';
    LS_PageBreakStr: 'Continues...';
    LS_PageMendStr: 'Continuation';
    LS_ReportEndStr: 'End';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'File not found';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'File Name';
    LS_AllFileTypesStr: 'All files';
    LS_LoadReportStr: 'Load report';
    LS_NotFoundStr: 'Not found';
    LS_WaitStr: 'Wait...';
    LS_FinishedStr: 'Finished';
    LS_CancelStr: 'Cancel';
    LS_CloseStr: 'Close';
    LS_SaveStr: 'Save';
    LS_SendStr: 'Send';
    LS_PrintStr: 'Print';
    LS_AboutTheStr: 'About';
    LS_PreviewStr: 'Preview';
    LS_OfStr: 'of';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'First page';
    LS_PriorPageStr: 'Prior page';
    LS_NextPageStr: 'Next page';
    LS_LastPageStr: 'Last page';
    LS_EntirePageStr: 'Entire page';
    LS_EntireWidthStr: 'Entire width';
    LS_MultiplePagesStr: 'Multiple pages';
    LS_ConfigPrinterStr: 'Configure printer';
    LS_SaveToFileStr: 'Save to file';
    LS_SendToStr: 'Send to';
    LS_PrinterStr: 'Printer';
    LS_NameStr: 'Name';
    LS_PrintToFileStr: 'Print to file';
    LS_PrintInBackgroundStr: 'Print in background';
    LS_OptionsStr: 'Options';
    LS_SaveInBackground: 'Save in background';
    LS_PageRangeStr: 'Page range';
    LS_CopyAsImageStr: 'Copy as Bitmap';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'from';
    LS_RangeToStr: 'to';
    LS_AllStr: 'All';
    LS_PagesStr: 'Pages';
    LS_SelectionStr: 'Selection';
    LS_CopiesStr: 'Copies';
    LS_NumberOfCopiesStr: 'Number of copies';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Divide the screen';
    LS_InvalidNameStr: 'Invalid name';
    LS_DuplicateNameStr: 'Name already in use';
    LS_UseFilterStr: 'Use filter';
    LS_WebPageStr: 'Web page';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
    LS_XLSFormatStr97_2013: 'Excel spreadsheet 97-2013';
    LS_XLSFormatStr: 'Excel spreadsheet';
    LS_AtStr: 'at';
    LS_FormStr: 'Form';
    LS_DefaultStr: 'Default';
    LS_ZoomInStr: 'Increase zoom';
    LS_ZoomOutStr: 'Decrease zoom';
    LS_CopyStr: 'Copy';
    LS_EditStr: 'Edit';
    LS_FindCaptionStr: 'Find';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Find &Next';
    LS_WholeWordsStr: '&Whole words only';
    LS_MatchCaseStr: '&Match Case';
    LS_DirectionUpStr: '&Up';
    LS_DirectionDownStr: '&Down';
    LS_DirectionCaptionStr: 'Direction';
    LS_ColumnsStr: 'Columns';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Font size error';
    LS_OddPages: 'Odd';
    LS_EvenPages: 'Even';
    LS_OddPagesOnly: 'Odd pages only';
    LS_EvenPagesOnly: 'Even pages only';
    LS_AllOddAndEven: 'All';
    LS_PrintDialogError: 'Problems with print dialog';
    LS_PageSelectionHint: 'Separate page numbers or page intervals with ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Report "%s"';
    LS_ZoomHint: 'Change zoom level';
    Ls_Aplicar: 'Use';
    LS_Propriedades: 'Settings';
    LS_Salvar_Como: 'Save as';
    LS_FileCorrupted: 'File is corrupted!';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: 'Invalid file version %d!';
    LS_PageSettings: 'Page Configuration';
    LS_PageMargins: 'Margins';
    LS_PageMarginsTop: 'Top';
    LS_PageMarginsBottom: 'Bottom';
    LS_PageMarginsRigth: 'Right';
    LS_PageMarginsLeft: 'Left';
    LS_PageMarginsPaper: 'Paper Margins';
    LS_PagePaper: 'Paper';
    LS_PaperSize: 'Paper Size';
    LS_PaperSizeWidth: 'Width';
    LS_PaperSizeHeigth: 'Height';
    LS_PaperOrientation: 'Orientation';
    LS_PaperOrientationLandscape: 'Landscape';
    LS_PaperOrientationPortrait: 'Portrait';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  PortugueseStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Imprimindo o relatório...';
    LS_FilterInProgressStr: 'Salvando o relatório...';
    LS_PreparingReportStr: 'Preparando o relatório...';
    LS_PrinterNotFoundStr: 'Nenhuma impressora encontrada';
    LS_NoPrinterSelected: 'Nenhuma impressora selecionada.';
    LS_NoPathToPrinterStr: 'Caminho inválido para a impressora';
    LS_LoadDefaultConfigStr: 'Será carregada a configuração padrão';
    LS_PrinterDriverErrorStr: 'Erro no driver da impressora';
    LS_PageStr: 'Página';
    LS_PrepareErrorStr: 'Erro durante a preparação do relatório';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuação';
    LS_ReportEndStr: 'Fim';
    LS_FileExists: 'Arquivo já existe. Sobreescrever?';
    LS_FileNotFoundStr: 'Arquivo não encontrado';
    LS_FileNameIsEmpty: 'Nome do arquivo não especificado';
    LS_FileNameStr: 'Nome do arquivo';
    LS_AllFileTypesStr: 'Todos os arquivos';
    LS_LoadReportStr: 'Carregar relatório';
    LS_NotFoundStr: 'Não encontrado';
    LS_WaitStr: 'Aguarde...';
    LS_FinishedStr: 'Concluído';
    LS_CancelStr: 'Cancelar';
    LS_CloseStr: 'Fechar';
    LS_SaveStr: 'Salvar';
    LS_SendStr: 'Enviar';
    LS_PrintStr: 'Imprimir';
    LS_AboutTheStr: 'Sobre o';
    LS_PreviewStr: 'Pré-visualização';
    LS_OfStr: 'de';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Primeira página';
    LS_PriorPageStr: 'Página anterior';
    LS_NextPageStr: 'Próxima página';
    LS_LastPageStr: 'Última página';
    LS_EntirePageStr: 'Página inteira';
    LS_EntireWidthStr: 'Largura da página';
    LS_MultiplePagesStr: 'Várias páginas';
    LS_ConfigPrinterStr: 'Configurar impressora';
    LS_SaveToFileStr: 'Salvar em disco';
    LS_SendToStr: 'Enviar para';
    LS_PrinterStr: 'Impressora';
    LS_NameStr: 'Nome';
    LS_PrintToFileStr: 'Imprimir em arquivo';
    LS_PrintInBackgroundStr: 'Imprimir em segundo plano';
    LS_OptionsStr: 'Opções';
    LS_SaveInBackground: 'Salvar em segundo plano';
    LS_PageRangeStr: 'Intervalo de páginas';
    LS_CopyAsImageStr: 'Copiar como Bitmap';
    LS_CopyAsMetafileStr: 'Copiar como Metafile';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'até';
    LS_AllStr: 'Tudo';
    LS_PagesStr: 'Páginas';
    LS_SelectionStr: 'Seleção';
    LS_CopiesStr: 'Cópias';
    LS_NumberOfCopiesStr: 'Número de cópias';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Dividir a tela';
    LS_InvalidNameStr: 'Nome inválido';
    LS_DuplicateNameStr: 'Nome já utilizado';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'Página da Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr97_2013: 'Planilha Excel 97-2013';
    LS_XLSFormatStr: 'Planilha Excel';
    LS_AtStr: 'em';
    LS_FormStr: 'Formulário';
    LS_DefaultStr: 'Padrão';
    LS_ZoomInStr: 'Aumentar o zoom';
    LS_ZoomOutStr: 'Diminuir o zoom';
    LS_CopyStr: 'Copiar';
    LS_EditStr: 'Editar';
    LS_FindCaptionStr: 'Procurar';
    LS_TextToFindStr: 'Te&xto';
    LS_FindNextStr: '&Próxima';
    LS_WholeWordsStr: 'Palavras &inteiras';
    LS_MatchCaseStr: 'Diferenciar &maiúsculas de minúsculas';
    LS_DirectionUpStr: 'A&cima';
    LS_DirectionDownStr: 'A&baixo';
    LS_DirectionCaptionStr: 'Direção';
    LS_ColumnsStr: 'Colunas';
    LS_SetupStr: 'Configuração';
    LS_FontSizeError: 'Erro no cálculo das fontes';
    LS_OddPages: 'Ímpares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'Somente Ímpares';
    LS_EvenPagesOnly: 'Somente Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas com o diálogo da impressora';
    LS_PageSelectionHint: 'Separe com ponto-e-vírgula os números ou intervalos de páginas a imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Relatório "%s"';
    LS_ZoomHint: 'Você também pode aumentar ou reduzir o zoom do relatório' + sLineBreak + 'pressionando "Ctrl" e usando a rolagem do mouse.';
    LS_Aplicar: 'Aplicar';
    LS_Propriedades: 'Propriedades';
    LS_Salvar_Como: 'Salvar Como';
    LS_FileCorrupted: 'Arquivo Corrompido';
    LS_FileCorruptedHeader: 'Cabeçalho de arquivo corrompido "%s"!';
    LS_FileVersion: 'Versão de arquivo inválida %d!';
    LS_PageSettings: 'Configurações da página';
    LS_PageMargins: 'Margem';
    LS_PageMarginsTop: 'Superior';
    LS_PageMarginsBottom: 'Inferior';
    LS_PageMarginsRigth: 'Direita';
    LS_PageMarginsLeft: 'Esquerda';
    LS_PageMarginsPaper: 'Margsss';
    LS_PagePaper: 'Papel';
    LS_PaperSize: 'Tamanho do Papel';
    LS_PaperSizeWidth: 'Largura';
    LS_PaperSizeHeigth: 'Altura';
    LS_PaperOrientation: 'Orienta Ô úo';
    LS_PaperOrientationLandscape: 'Paisagem';
    LS_PaperOrientationPortrait: 'Retrato';
    LS_Duplex: 'Impressão frente e verso';
    LS_OnlyOneInstance: 'Apenas uma instância de %s é permitida!';
    LS_NotImplemented: '%s ainda não foi implementado para esta plataforma!';
    LS_NoHandle: 'Handle não disponível!';

    LS_LastFooMsg: '';
  );

var
  FrenchStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Préparation du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouvée';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Chemin d''imprimante non valide';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la préparation du rapport';
    LS_PageBreakStr: 'Suite...';
    LS_PageMendStr: 'A suivre';
    LS_ReportEndStr: 'Fin';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'Fichier non trouvé';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Nom de Fichier';
    LS_AllFileTypesStr: 'Tous les fichiers';
    LS_LoadReportStr: 'Ouvrir rapport';
    LS_NotFoundStr: 'Non trouvé';
    LS_WaitStr: 'Patientez...';
    LS_FinishedStr: 'Fini';
    LS_CancelStr: 'Annuler';
    LS_CloseStr: 'Fermer';
    LS_SaveStr: 'Sauver';
    LS_SendStr: 'Envoyer';
    LS_PrintStr: 'Imprimer';
    LS_AboutTheStr: 'A propos de';
    LS_PreviewStr: 'Aperçu avant impression';
    LS_OfStr: 'de';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Première page';
    LS_PriorPageStr: 'Page précédente';
    LS_NextPageStr: 'Page suivante';
    LS_LastPageStr: 'Dernière page';
    LS_EntirePageStr: 'Page entière';
    LS_EntireWidthStr: 'Pleine largeur';
    LS_MultiplePagesStr: 'Plusieurs pages';
    LS_ConfigPrinterStr: 'Configuration de l''imprimante';
    LS_SaveToFileStr: 'Enregistrer dans un fichier';
    LS_SendToStr: 'Envoyer à...';
    LS_PrinterStr: 'Imprimante';
    LS_NameStr: 'Nom';
    LS_PrintToFileStr: 'Imprimer dans un fichier';
    LS_PrintInBackgroundStr: 'Imprimer dans background';
    LS_OptionsStr: 'Options';
    LS_SaveInBackground: 'Enregistrer dans background';
    LS_PageRangeStr: 'Intervalle de pages';
    LS_CopyAsImageStr: 'Copie sous forme d''image';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'à';
    LS_AllStr: 'Tout';
    LS_PagesStr: 'Pages';
    LS_SelectionStr: 'Sélection';
    LS_CopiesStr: 'Copies';
    LS_NumberOfCopiesStr: 'Nombre de copies';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Écran divisé';
    LS_InvalidNameStr: 'Nom invalide';
    LS_DuplicateNameStr: 'Nom répété';
    LS_UseFilterStr: 'Utiliser un filtre';
    LS_WebPageStr: 'Page Web';
    LS_RichFormatStr: 'Format RichText';
    LS_PDFFormatStr: 'Document en PDF';
    LS_XLSFormatStr97_2013: 'Feuille de calcul Excel 97-2013';
    LS_XLSFormatStr: 'Feuille de calcul Excel';
    LS_AtStr: 'à';
    LS_FormStr: 'Formulaire';
    LS_DefaultStr: 'Défaut';
    LS_ZoomInStr: 'Grandir zoom';
    LS_ZoomOutStr: 'Réduire zoom';
    LS_CopyStr: 'Copier';
    LS_EditStr: 'Éditer';
    LS_FindCaptionStr: 'Trouvaille';
    LS_TextToFindStr: 'Te&xte';
    LS_FindNextStr: 'A&près';
    LS_WholeWordsStr: 'Mots &entiers seulement';
    LS_MatchCaseStr: 'Cas d''allu&mette';
    LS_DirectionUpStr: 'Le &Haut';
    LS_DirectionDownStr: 'Le &bas';
    LS_DirectionCaptionStr: 'Direction';
    LS_ColumnsStr: 'Cols';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Erreur dans la taille de police';
    LS_OddPages: 'Impair';
    LS_EvenPages: 'Pair';
    LS_OddPagesOnly: 'Pages impaires seulement';
    LS_EvenPagesOnly: 'Pages paires seulement';
    LS_AllOddAndEven: 'Toutes les pages';
    LS_PrintDialogError: 'Erreur dans la boîte de dialogue de l''imprimante';
    LS_PageSelectionHint: 'Séparé des numéros de page ou des intervalles avec ";". p.ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Rapport "%s"';
    LS_ZoomHint: '';
    LS_Aplicar: 'Appliquer';
    LS_Propriedades: '';
    LS_Salvar_Como: '';
    LS_FileCorrupted: '';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: 'Invalid file version %d!';
    LS_PageSettings: '';
    LS_PageMargins: '';
    LS_PageMarginsTop: '';
    LS_PageMarginsBottom: '';
    LS_PageMarginsRigth: '';
    LS_PageMarginsLeft: '';
    LS_PageMarginsPaper: '';
    LS_PagePaper: '';
    LS_PaperSize: '';
    LS_PaperSizeWidth: '';
    LS_PaperSizeHeigth: '';
    LS_PaperOrientation: '';
    LS_PaperOrientationLandscape: '';
    LS_PaperOrientationPortrait: '';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not avaible!';

    LS_LastFooMsg: '';
  );

var
  SpanishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impresión en marcha...';
    LS_FilterInProgressStr: 'Guardando el informe...';
    LS_PreparingReportStr: 'Preparación del informe...';
    LS_PrinterNotFoundStr: 'Impresora no encontrada';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Camino de la impresora no es válido';
    LS_LoadDefaultConfigStr: 'Cargar la configuración estándar';
    LS_PrinterDriverErrorStr: 'Error en driver de la impresora';
    LS_PageStr: 'Página';
    LS_PrepareErrorStr: 'Un error ocurrió mientras se preparaba el informe';
    LS_PageBreakStr: 'Continúa...';
    LS_PageMendStr: 'Continuación';
    LS_ReportEndStr: 'Extremo';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'Archivo no encontrado';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Nombre del Archivo';
    LS_AllFileTypesStr: 'Todos los archivos';
    LS_LoadReportStr: 'Cargar el informe';
    LS_NotFoundStr: 'No encontrado';
    LS_WaitStr: 'Espera...';
    LS_FinishedStr: 'Finalizado';
    LS_CancelStr: 'Cancelar';
    LS_CloseStr: 'Cerrar';
    LS_SaveStr: 'Guardar';
    LS_SendStr: 'Enviar';
    LS_PrintStr: 'Imprimir';
    LS_AboutTheStr: 'Sobre';
    LS_PreviewStr: 'Ver';
    LS_OfStr: 'de';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Primera página';
    LS_PriorPageStr: 'Página anterior';
    LS_NextPageStr: 'Página siguiente';
    LS_LastPageStr: 'Última página';
    LS_EntirePageStr: 'Página entera';
    LS_EntireWidthStr: 'Ancho completo';
    LS_MultiplePagesStr: 'Varias páginas';
    LS_ConfigPrinterStr: 'Configurar la impresora';
    LS_SaveToFileStr: 'Guardar en un archivo';
    LS_SendToStr: 'Envíar a';
    LS_PrinterStr: 'Impresora';
    LS_NameStr: 'Nombre';
    LS_PrintToFileStr: 'Imprimir a un archivo';
    LS_PrintInBackgroundStr: 'Imprimir en background';
    LS_OptionsStr: 'Opciones';
    LS_SaveInBackground: 'Guardar en background';
    LS_PageRangeStr: 'Intervalo de páginas';
    LS_CopyAsImageStr: 'Copiar como imagen';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'a';
    LS_AllStr: 'Todas';
    LS_PagesStr: 'Páginas';
    LS_SelectionStr: 'Selección';
    LS_CopiesStr: 'Copias';
    LS_NumberOfCopiesStr: 'Número de copias';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Dividir la pantalla';
    LS_InvalidNameStr: 'Nombre inválido';
    LS_DuplicateNameStr: 'Nombre ya en uso';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'Página Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr97_2013: 'Hoja de cálculo Excel 97-2013';
    LS_XLSFormatStr: 'Hoja de cálculo Excel';
    LS_AtStr: 'en';
    LS_FormStr: 'Formulario';
    LS_DefaultStr: 'Estándar';
    LS_ZoomInStr: 'Aumentar zoom';
    LS_ZoomOutStr: 'Disminuir zoom';
    LS_CopyStr: 'Copiar';
    LS_EditStr: 'Editar';
    LS_FindCaptionStr: 'Buscar';
    LS_TextToFindStr: 'Te&xto';
    LS_FindNextStr: '&Siguiente';
    LS_WholeWordsStr: 'Palabras &completas sólamente';
    LS_MatchCaseStr: 'Diferenciar &mayúsculas y minúsculas';
    LS_DirectionUpStr: 'En&cima';
    LS_DirectionDownStr: '&Abajo';
    LS_DirectionCaptionStr: 'Dirección';
    LS_ColumnsStr: 'Cols';
    LS_SetupStr: 'Configuración...';
    LS_FontSizeError: 'Error en el cálculo de las fuentes';
    LS_OddPages: 'Impares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'Sólo Impares';
    LS_EvenPagesOnly: 'Sólo Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas con el diálogo de la impresora';
    LS_PageSelectionHint: 'Separar con ";" los números o rangos de páginas para imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Impresión "%s"';
    LS_ZoomHint: '';
    LS_Aplicar: '';
    LS_Propriedades: '';
    LS_Salvar_Como: '';
    LS_FileCorrupted: '';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: 'Invalid file version %d!';
    LS_PageSettings: '';
    LS_PageMargins: '';
    LS_PageMarginsTop: '';
    LS_PageMarginsBottom: '';
    LS_PageMarginsRigth: '';
    LS_PageMarginsLeft: '';
    LS_PageMarginsPaper: '';
    LS_PagePaper: '';
    LS_PaperSize: '';
    LS_PaperSizeWidth: '';
    LS_PaperSizeHeigth: '';
    LS_PaperOrientation: '';
    LS_PaperOrientationLandscape: '';
    LS_PaperOrientationPortrait: '';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  ItalianStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Stampa in corso...';
    LS_FilterInProgressStr: 'Salvataggio stampa...';
    LS_PreparingReportStr: 'Preparazione stampa...';
    LS_PrinterNotFoundStr: 'Stampante non trovata';
    LS_NoPrinterSelected: 'Nessuna stampante selezionata.';
    LS_NoPathToPrinterStr: 'Percorso stampante non valido';
    LS_LoadDefaultConfigStr: 'Carica configurazione standard';
    LS_PrinterDriverErrorStr: 'Errore Driver stampane';
    LS_PageStr: 'Pagina';
    LS_PrepareErrorStr: 'Errore durante la preparazione della stampa';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuazione';
    LS_ReportEndStr: 'Fine';
    LS_FileExists: 'Il File esiste. Sovrascrivo?';
    LS_FileNotFoundStr: 'File non trovato';
    LS_FileNameIsEmpty: 'Nome File non indicato';
    LS_FileNameStr: 'Nome del file';
    LS_AllFileTypesStr: 'Tutti i file';
    LS_LoadReportStr: 'Carica stampa';
    LS_NotFoundStr: 'Non trovato';
    LS_WaitStr: 'Attendi...';
    LS_FinishedStr: 'Finito';
    LS_CancelStr: 'Annulla';
    LS_CloseStr: 'Chiudi';
    LS_SaveStr: 'Salva';
    LS_SendStr: 'Invia';
    LS_PrintStr: 'Stampa';
    LS_AboutTheStr: 'About';
    LS_PreviewStr: 'Anteprima';
    LS_OfStr: 'di';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Prima pagina';
    LS_PriorPageStr: 'Pagina precedente';
    LS_NextPageStr: 'Prossima pagina';
    LS_LastPageStr: 'Ultima pagina';
    LS_EntirePageStr: 'Pagina intera';
    LS_EntireWidthStr: 'Ampiezza intera';
    LS_MultiplePagesStr: 'Più pagine';
    LS_ConfigPrinterStr: 'Configura stampante';
    LS_SaveToFileStr: 'Salva su File';
    LS_SendToStr: 'Invia a';
    LS_PrinterStr: 'Stampante';
    LS_NameStr: 'Nome';
    LS_PrintToFileStr: 'Stampa su File';
    LS_PrintInBackgroundStr: 'Stampa in background';
    LS_OptionsStr: 'Opzioni';
    LS_SaveInBackground: 'Salva in background';
    LS_PageRangeStr: 'Intervallo di pagine';
    LS_CopyAsImageStr: 'Copia come Immagine';
    LS_CopyAsMetafileStr: 'Copia come Metafile';
    LS_RangeFromStr: 'da';
    LS_RangeToStr: 'a';
    LS_AllStr: 'Tutte';
    LS_PagesStr: 'Pagine';
    LS_SelectionStr: 'Selezione';
    LS_CopiesStr: 'Copie';
    LS_NumberOfCopiesStr: 'Numero di copie';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Dividi lo schermo';
    LS_InvalidNameStr: 'Nome non valido';
    LS_DuplicateNameStr: 'Nome già presente';
    LS_UseFilterStr: 'Usa filtro';
    LS_WebPageStr: 'Pagina Web';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
    LS_XLSFormatStr97_2013: 'Microsoft Excel 97-2013';
    LS_XLSFormatStr: 'Microsoft Excel';
    LS_AtStr: 'a';
    LS_FormStr: 'Form';
    LS_DefaultStr: 'Default';
    LS_ZoomInStr: 'Ingrandisci';
    LS_ZoomOutStr: 'Rimpicciolisci';
    LS_CopyStr: 'Copia';
    LS_EditStr: 'Modifica';
    LS_FindCaptionStr: 'Trova';
    LS_TextToFindStr: 'Testo da cercare';
    LS_FindNextStr: 'Trova &prossimo';
    LS_WholeWordsStr: 'Solo parole intere';
    LS_MatchCaseStr: 'Considera Maius/Minus';
    LS_DirectionUpStr: 'Su';
    LS_DirectionDownStr: 'In giù';
    LS_DirectionCaptionStr: 'Direzione';
    LS_ColumnsStr: 'Colonne';
    LS_SetupStr: 'Configura...';
    LS_FontSizeError: 'Errore dimensione carattere';
    LS_OddPages: 'Dispari';
    LS_EvenPages: 'Pari';
    LS_OddPagesOnly: 'Solo pagine dispari';
    LS_EvenPagesOnly: 'Solo pagine pari';
    LS_AllOddAndEven: 'Tutte';
    LS_PrintDialogError: 'Problemi con la finestra di stampa';
    LS_PageSelectionHint: 'Separa numeri di pagina o intervallo con ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Stampa "%s"';
    LS_ZoomHint: 'Cambia livello di zoom';
    LS_Aplicar: 'Applica';
    LS_Propriedades: 'Impostazioni';
    LS_Salvar_Como: 'Salva come';
    LS_FileCorrupted: 'File corrotto';
    LS_FileCorruptedHeader: 'Intestazione file corrotta "%s"!';
    LS_FileVersion: 'Versione file non valida %d!';
    LS_PageSettings: 'Configura pagina';
    LS_PageMargins: 'Margini';
    LS_PageMarginsTop: 'Alto';
    LS_PageMarginsBottom: 'Basso';
    LS_PageMarginsRigth: 'Destra';
    LS_PageMarginsLeft: 'Sinistra';
    LS_PageMarginsPaper: 'Margini Foglio';
    LS_PagePaper: 'Foglio';
    LS_PaperSize: 'Dimensione Foglio';
    LS_PaperSizeWidth: '';
    LS_PaperSizeHeigth: '';
    LS_PaperOrientation: '';
    LS_PaperOrientationLandscape: '';
    LS_PaperOrientationPortrait: '';
    LS_Duplex: 'Stampa fronte-retro';
    LS_OnlyOneInstance: 'Una sola istanza di %s è ammessa!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  SwedishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Das Drucken im Gange...';
    LS_FilterInProgressStr: 'Das Sparen von Bericht  ...';
    LS_PreparingReportStr: 'Das Vorbereiten von Berich...';
    LS_PrinterNotFoundStr: 'Drucker fand nicht';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Ungültiger Druckerpfad';
    LS_LoadDefaultConfigStr: 'Laden Sie Standardkonfiguration';
    LS_PrinterDriverErrorStr: 'Druckerfahrer Fehler';
    LS_PageStr: 'Seite';
    LS_PrepareErrorStr: 'Fehler, während das Vorbereiten von Bericht';
    LS_PageBreakStr: 'Setzt fort...';
    LS_PageMendStr: 'Fortsetzung';
    LS_ReportEndStr: 'Ende';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'File fand nicht';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Akte Name';
    LS_AllFileTypesStr: 'Alles legt ab';
    LS_LoadReportStr: 'Lastbericht';
    LS_NotFoundStr: 'Finden Sie nicht';
    LS_WaitStr: 'Wartezeit...';
    LS_FinishedStr: 'Beendet';
    LS_CancelStr: 'Sagen Sie ab';
    LS_CloseStr: 'Ende';
    LS_SaveStr: 'Ballabwehr';
    LS_SendStr: 'Schicken Sie';
    LS_PrintStr: 'Druck';
    LS_AboutTheStr: 'Über';
    LS_PreviewStr: 'Vorschau';
    LS_OfStr: 'von';
    LS_ZoomStr: 'Gummilinse';
    LS_FirstPageStr: 'Erste Seite';
    LS_PriorPageStr: 'Vorausgehende Seite';
    LS_NextPageStr: 'Danach Seite';
    LS_LastPageStr: 'Letzte Seite';
    LS_EntirePageStr: 'Ganze Seite';
    LS_EntireWidthStr: 'Ganze Weite';
    LS_MultiplePagesStr: 'Mehrfache Seiten';
    LS_ConfigPrinterStr: 'Konfigurieren Sie Drucker';
    LS_SaveToFileStr: 'Ballabwehr, um abzulegen';
    LS_SendToStr: 'Schicken Sie dazu';
    LS_PrinterStr: 'Drucker';
    LS_NameStr: 'Name';
    LS_PrintToFileStr: 'Drucker, um abzulegen';
    LS_PrintInBackgroundStr: 'Druck in Hintergrund ';
    LS_OptionsStr: 'Möglichkeiten';
    LS_SaveInBackground: 'Ballabwehr in Hintergrund';
    LS_PageRangeStr: 'Rufen Sie Auswahl aus';
    LS_CopyAsImageStr: 'Copy as Bitmap';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'von';
    LS_RangeToStr: 'zu';
    LS_AllStr: 'Alles';
    LS_PagesStr: 'Seiten';
    LS_SelectionStr: 'Auswahl';
    LS_CopiesStr: 'Kopien';
    LS_NumberOfCopiesStr: 'Anzahl von Kopien';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Teilen Sie den Bildschirm';
    LS_InvalidNameStr: 'Ungültiger Name';
    LS_DuplicateNameStr: 'Nennen Sie schon im Gebrauch';
    LS_UseFilterStr: 'Verwendungsfilter';
    LS_WebPageStr: 'Webseite';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
    LS_XLSFormatStr97_2013: 'Zeichnen Sie Tabelle aus 97-2013';
    LS_XLSFormatStr: 'Zeichnen Sie Tabelle aus';
    LS_AtStr: 'bei';
    LS_FormStr: 'Form';
    LS_DefaultStr: 'Versäumnis';
    LS_ZoomInStr: 'Zunahmegummilinse';
    LS_ZoomOutStr: 'Abnahmengummilinse';
    LS_CopyStr: 'Kopie';
    LS_EditStr: 'Bearbeiten Sie';
    LS_FindCaptionStr: 'Fund';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Finden Sie danach';
    LS_WholeWordsStr: '&Ganze Wörter nur';
    LS_MatchCaseStr: '&Passen Sie Fall zusammen';
    LS_DirectionUpStr: '&Auf';
    LS_DirectionDownStr: '&Entlang';
    LS_DirectionCaptionStr: 'Richtung';
    LS_ColumnsStr: 'Säule';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Font size error';
    LS_OddPages: 'Odd';
    LS_EvenPages: 'Even';
    LS_OddPagesOnly: 'Odd pages only';
    LS_EvenPagesOnly: 'Even pages only';
    LS_AllOddAndEven: 'All';
    LS_PrintDialogError: 'Problems calling the printer dialog';
    LS_PageSelectionHint: 'Separate page numbers or page intervals with ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Das Drucken "%s"';
    LS_ZoomHint: '';
    LS_Aplicar: '';
    LS_Propriedades: '';
    LS_Salvar_Como: '';
    LS_FileCorrupted: '';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: 'Invalid file version %d!';
    LS_PageSettings: '';
    LS_PageMargins: '';
    LS_PageMarginsTop: '';
    LS_PageMarginsBottom: '';
    LS_PageMarginsRigth: '';
    LS_PageMarginsLeft: '';
    LS_PageMarginsPaper: '';
    LS_PagePaper: '';
    LS_PaperSize: '';
    LS_PaperSizeWidth: '';
    LS_PaperSizeHeigth: '';
    LS_PaperOrientation: '';
    LS_PaperOrientationLandscape: '';
    LS_PaperOrientationPortrait: '';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  RussianStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Ð?ÐµÑ?Ð°Ñ?Ñ?...';
    LS_FilterInProgressStr: 'Ð¡Ð¾Ñ?Ñ?Ð°Ð½ÐµÐ½Ð¸Ðµ Ð¾Ñ?Ñ?ÐµÑ?Ð°...';
    LS_PreparingReportStr: 'Ð?Ð¾Ð´Ð³Ð¾Ñ?Ð¾Ð²ÐºÐ° Ð¾Ñ?Ñ?ÐµÑ?Ð°...';
    LS_PrinterNotFoundStr: 'Ð?Ñ?Ð¸Ð½Ñ?ÐµÑ? Ð½Ðµ Ð½Ð°Ð¹Ð´ÐµÐ½';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Ð?ÐµÐ¿Ñ?Ð°Ð²Ð¸Ð»Ñ?Ð½Ñ?Ð¹ Ð¿Ñ?Ñ?Ñ? Ð¿Ñ?Ð¸Ð½Ñ?ÐµÑ?Ð°';
    LS_LoadDefaultConfigStr: 'Ð?Ð°Ð³Ñ?Ñ?Ð·Ð¸Ñ?Ñ? ÐºÐ¾Ð½Ñ?Ð¸Ð³Ñ?Ñ?Ð°Ñ?Ð¸Ñ? Ð¿Ð¾ Ñ?Ð¼Ð¾Ð»Ñ?Ð°Ð½Ð¸Ñ?';
    LS_PrinterDriverErrorStr: 'Ð?Ñ?Ð¸Ð±ÐºÐ° Ð´Ñ?Ð°Ð¹Ð²ÐµÑ?Ð° Ð¿Ñ?Ð¸Ð½Ñ?ÐµÑ?Ð°';
    LS_PageStr: 'Ð¡Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_PrepareErrorStr: 'Ð?Ñ?Ð¸Ð±ÐºÐ° Ð¿Ñ?Ð¸ Ð¿Ð¾Ð´Ð³Ð¾Ñ?Ð¾Ð²ÐºÐµ Ð¾Ñ?Ñ?ÐµÑ?Ð°';
    LS_PageBreakStr: 'Ð?Ñ?Ð¾Ð´Ð¾Ð»Ð¶Ð°ÐµÑ?Ñ?Ñ?...';
    LS_PageMendStr: 'Ð?Ñ?Ð¾Ð´Ð¾Ð»Ð¶ÐµÐ½Ð¸Ðµ';
    LS_ReportEndStr: 'Ð?Ð¾Ð½ÐµÑ?';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'Ð¤Ð°Ð¹Ð» Ð½Ðµ Ð½Ð°Ð¹Ð´ÐµÐ½';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Ð?Ð¼Ñ? Ñ?Ð°Ð¹Ð»Ð°';
    LS_AllFileTypesStr: 'Ð?Ñ?Ðµ Ñ?Ð°Ð¹Ð»Ñ?';
    LS_LoadReportStr: 'Ð?Ð°Ð³Ñ?Ñ?Ð·Ð¸Ñ?Ñ? Ð¾Ñ?Ñ?ÐµÑ?';
    LS_NotFoundStr: 'Ð?Ðµ Ð½Ð°Ð¹Ð´ÐµÐ½';
    LS_WaitStr: 'Ð?Ð¾Ð´Ð¾Ð¶Ð´Ð¸Ñ?Ðµ...';
    LS_FinishedStr: 'Ð?Ð°Ð²ÐµÑ?Ñ?ÐµÐ½Ð¾';
    LS_CancelStr: 'Ð?Ñ?Ð¼ÐµÐ½Ð°';
    LS_CloseStr: 'Ð?Ð°ÐºÑ?Ñ?Ñ?Ñ?';
    LS_SaveStr: 'Ð¡Ð¾Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ñ?';
    LS_SendStr: 'Ð?Ñ?Ð¿Ñ?Ð°Ð²Ð¸Ñ?Ñ?';
    LS_PrintStr: 'Ð?ÐµÑ?Ð°Ñ?Ñ?';
    LS_AboutTheStr: 'Ð? Ð?Ñ?Ð¾Ð³Ñ?Ð°Ð¼Ð¼Ðµ';
    LS_PreviewStr: 'Ð?Ñ?ÐµÐ´Ð²Ð°Ñ?Ð¸Ñ?ÐµÐ»Ñ?Ð½Ñ?Ð¹ Ð¿Ñ?Ð¾Ñ?Ð¼Ð¾Ñ?Ñ?';
    LS_OfStr: 'Ð¸Ð·';
    LS_ZoomStr: 'Ð?Ð°Ñ?Ñ?Ð°Ð±';
    LS_FirstPageStr: 'Ð?ÐµÑ?Ð²Ð°Ñ? Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_PriorPageStr: 'Ð?Ñ?ÐµÐ´Ñ?Ð´Ñ?Ñ?Ð°Ñ? Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_NextPageStr: 'Ð¡Ð»ÐµÐ´Ñ?Ñ?Ñ?Ð°Ñ? Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_LastPageStr: 'Ð?Ð¾Ñ?Ð»ÐµÐ´Ð½Ñ?Ñ? Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_EntirePageStr: 'Ð?Ñ?Ñ? Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_EntireWidthStr: 'Ð?Ñ?Ñ? Ñ?Ð¸Ñ?Ð¸Ð½Ð°';
    LS_MultiplePagesStr: 'Ð?ÐµÑ?ÐºÐ¾Ð»Ñ?ÐºÐ¾ Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?';
    LS_ConfigPrinterStr: 'Ð?Ð°Ñ?Ñ?Ñ?Ð¾Ð¹ÐºÐ° Ð¿Ñ?Ð¸Ð½Ñ?ÐµÑ?Ð°';
    LS_SaveToFileStr: 'Ð¡Ð¾Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ñ? Ð² Ñ?Ð°Ð¹Ð»';
    LS_SendToStr: 'Ð?Ñ?Ð¿Ñ?Ð°Ð²Ð¸Ñ?Ñ? Ð²';
    LS_PrinterStr: 'Ð?Ñ?Ð¸Ð½Ñ?ÐµÑ?';
    LS_NameStr: 'Ð?Ð¼Ñ?';
    LS_PrintToFileStr: 'Ð?ÐµÑ?Ð°Ñ?Ñ? Ð² Ñ?Ð°Ð¹Ð»';
    LS_PrintInBackgroundStr: 'Ð?ÐµÑ?Ð°Ñ?Ñ? Ð² Ñ?Ð¾Ð½Ðµ';
    LS_OptionsStr: 'Ð?Ð°Ñ?ÑÑÐ?Ð?ÐºÐ¸';
    LS_SaveInBackground: 'Ð¡Ð¾Ñ?Ñ?Ð°Ð½ÐµÐ½Ð¸Ðµ Ð² Ñ?Ð¾Ð½Ðµ';
    LS_PageRangeStr: 'Ð?Ð¸Ð°Ð¿Ð°Ð·Ð¾Ð½ Ð¿ÐµÑ?Ð°Ñ?Ð¸';
    LS_CopyAsImageStr: 'Copy as Bitmap';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'Ñ?';
    LS_RangeToStr: 'Ð¿Ð¾';
    LS_AllStr: 'Ð?Ñ?Ðµ';
    LS_PagesStr: 'Ð¡Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ñ?';
    LS_SelectionStr: 'Ð?Ñ?Ð´ÐµÐ»ÐµÐ½Ð¸Ðµ';
    LS_CopiesStr: 'Ð?Ð¾Ð¿Ð¸Ð¸';
    LS_NumberOfCopiesStr: 'Ð?Ð¾Ð»-Ð²Ð¾ ÐºÐ¾Ð¿Ð¸Ð¹';
    LS_OkStr: 'Ð¥Ð¾Ñ?Ð¾Ñ?Ð¾';
    LS_DivideScreenStr: 'Ð Ð°Ð·Ð´ÐµÐ»Ð¸Ñ?Ñ? Ñ?ÐºÑ?Ð°Ð½';
    LS_InvalidNameStr: 'Ð?ÐµÐ¿Ñ?Ð°Ð²Ð¸Ð»Ñ?Ð½Ð¾Ðµ Ð¸Ð¼Ñ?';
    LS_DuplicateNameStr: 'Ð?Ð¼Ñ? Ñ?Ð¶Ðµ Ð¸Ñ?Ð¿Ð¾Ð»Ñ?Ð·Ñ?ÐµÑ?Ñ?Ñ?';
    LS_UseFilterStr: 'Ð¤Ð¸Ð»Ñ?Ñ?Ñ?';
    LS_WebPageStr: 'Ð?ÐµÐ± Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ð°';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Ð?Ð¾ÐºÑ?Ð¼ÐµÐ½Ñ?';
    LS_XLSFormatStr97_2013: '';
    LS_XLSFormatStr: 'Ð¢Ð°Ð±Ð»Ð¸Ñ?Ð° Excel';
    LS_AtStr: 'Ð²';
    LS_FormStr: 'Ð¤Ð¾Ñ?Ð¼Ð°';
    LS_DefaultStr: 'Ð?Ð¾ Ñ?Ð¼Ð¾Ð»Ñ?Ð°Ð½Ð¸Ñ?';
    LS_ZoomInStr: 'Ð£Ð²ÐµÐ»Ð¸Ñ?Ð¸Ñ?Ñ?';
    LS_ZoomOutStr: 'Ð£Ð¼ÐµÐ½Ñ?Ñ?Ð¸Ñ?Ñ?';
    LS_CopyStr: 'Ð?Ð¾Ð¿Ð¸Ñ?Ð¾Ð²Ð°Ñ?Ñ?';
    LS_EditStr: 'Ð ÐµÐ´Ð°ÐºÑ?Ð¸Ñ?Ð¾Ð²Ð°Ñ?Ñ?';
    LS_FindCaptionStr: 'Ð?Ð°Ð¹Ñ?Ð¸';
    LS_TextToFindStr: 'Ð¢Ðµ&ÐºÑ?Ñ?';
    LS_FindNextStr: 'Ð?Ð°Ð¹Ñ?Ð¸ &Ð´Ð°Ð»Ñ?Ñ?Ðµ';
    LS_WholeWordsStr: '&Ð¢Ð¾Ð»Ñ?ÐºÐ¾ Ñ?Ð»Ð¾Ð²Ð¾ Ñ?ÐµÐ»Ð¸ÐºÐ¾Ð¼';
    LS_MatchCaseStr: '&Ð£Ñ?Ð¸Ñ?Ñ?Ð²Ð°Ñ?Ñ? Ñ?ÐµÐ³Ð¸Ñ?Ñ?Ñ?';
    LS_DirectionUpStr: '&Ð?Ð²ÐµÑ?Ñ?';
    LS_DirectionDownStr: 'Ð?&Ð½Ð¸Ð·';
    LS_DirectionCaptionStr: 'Ð?Ð°Ð¿Ñ?Ð°Ð²Ð»ÐµÐ½Ð¸Ðµ';
    LS_ColumnsStr: 'Ð¡Ñ?Ð¾Ð»Ð±Ñ?Ñ?';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Font size error';
    LS_OddPages: 'Odd';
    LS_EvenPages: 'Even';
    LS_OddPagesOnly: 'Odd pages only';
    LS_EvenPagesOnly: 'Even pages only';
    LS_AllOddAndEven: 'All';
    LS_PrintDialogError: 'Problems with print dialog';
    LS_PageSelectionHint: 'Separate page numbers or page intervals with ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Report "%s"';
    LS_ZoomHint: '';
    LS_Aplicar: 'Ð?Ñ?Ð¸Ð¼ÐµÐ½Ð¸Ñ?Ñ?';
    LS_Propriedades: 'Ð?Ð°Ñ?Ñ?Ñ?Ð¾Ð¹ÐºÐ¸';
    LS_Salvar_Como: 'Ð¡Ð¾Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ñ? ÐºÐ°Ðº';
    LS_FileCorrupted: 'Ð¤Ð°Ð¹Ð» Ð¿Ð¾Ð²Ñ?ÐµÐ¶Ð´ÐµÐ½!';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: 'Ð?ÐµÐ¿Ñ?Ð°Ð²Ð¸Ð»Ñ?Ð½Ð°Ñ? Ð²ÐµÑ?Ñ?Ð¸Ñ? Ñ?Ð°Ð¹Ð»Ð° %d!';
    LS_PageSettings: 'Ð?Ð°Ñ?Ñ?Ñ?Ð¾Ð¹ÐºÐ° Ñ?Ñ?Ñ?Ð°Ð½Ð¸Ñ?Ñ?';
    LS_PageMargins: 'Ð?Ð¾Ð»Ñ?';
    LS_PageMarginsTop: 'Ð?ÐµÑ?Ñ?';
    LS_PageMarginsBottom: 'Ð?Ð¸Ñ?';
    LS_PageMarginsRigth: 'Ð?Ñ?Ð°Ð²Ð¾';
    LS_PageMarginsLeft: 'Ð?ÐµÐ²Ð¾';
    LS_PageMarginsPaper: '';
    LS_PagePaper: 'Ð?Ñ?Ð¼Ð°Ð³Ð°';
    LS_PaperSize: 'Ð Ð°Ð·Ð¼ÐµÑ? Ð±Ñ?Ð¼Ð°Ð³Ð¸';
    LS_PaperSizeWidth: 'Ð¨Ð¸Ñ?Ð¸Ð½Ð°';
    LS_PaperSizeHeigth: 'Ð?Ñ?Ñ?Ð¾Ñ?Ð°';
    LS_PaperOrientation: 'Ð?Ñ?Ð¸ÐµÐ½Ñ?Ð°Ñ?Ð¸Ñ?';
    LS_PaperOrientationLandscape: 'Ð?Ð°Ð½Ð´Ñ?Ð°Ñ?Ñ?Ð½Ð°Ñ?';
    LS_PaperOrientationPortrait: 'Ð?Ð¾Ñ?Ñ?Ñ?ÐµÑ?Ð½Ð°Ñ?';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  CzechStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Probíhá tisk...';
    LS_FilterInProgressStr: 'Ukládám report...';
    LS_PreparingReportStr: 'Pøipravuji report...';
    LS_PrinterNotFoundStr: 'Tiskárna nenalezena';
    LS_NoPrinterSelected: 'Není vybrána ?ádná tiskárna.';
    LS_NoPathToPrinterStr: 'Chybná cesta k tiskárnì';
    LS_LoadDefaultConfigStr: 'Naèíst implicitní nastavení';
    LS_PrinterDriverErrorStr: 'Chyba ovladaèe tisku';
    LS_PageStr: 'Stránka';
    LS_PrepareErrorStr: 'Chyba bìhem pøípravy reportu';
    LS_PageBreakStr: 'Pokraèuje...';
    LS_PageMendStr: 'Pokraèování';
    LS_ReportEndStr: 'Konec';
    LS_FileExists: 'Soubor ji? existuje. Pøepsat?';
    LS_FileNotFoundStr: 'Soubor nenalezen';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Název souboru';
    LS_AllFileTypesStr: 'V?echny soubory';
    LS_LoadReportStr: 'Naèíst report';
    LS_NotFoundStr: 'Nenalezeno';
    LS_WaitStr: 'Èekejte...';
    LS_FinishedStr: 'Ukonèeno';
    LS_CancelStr: 'Zru?it';
    LS_CloseStr: 'Zavøít';
    LS_SaveStr: 'Ulo?it';
    LS_SendStr: 'Poslat';
    LS_PrintStr: 'Tisk';
    LS_AboutTheStr: 'O aplikaci';
    LS_PreviewStr: 'Náhled';
    LS_OfStr: 'z';
    LS_ZoomStr: 'Pøiblí?ení';
    LS_FirstPageStr: 'První stránka';
    LS_PriorPageStr: 'Pøedchozí stránka';
    LS_NextPageStr: 'Dal?í stránka';
    LS_LastPageStr: 'Poslední stránka';
    LS_EntirePageStr: 'Celá stránka';
    LS_EntireWidthStr: 'Na ?íøku';
    LS_MultiplePagesStr: 'Více stránek';
    LS_ConfigPrinterStr: 'Konfigurace tiskárny';
    LS_SaveToFileStr: 'Ulo?it do souboru';
    LS_SendToStr: 'Odeslat';
    LS_PrinterStr: 'Tiskárna';
    LS_NameStr: 'Název';
    LS_PrintToFileStr: 'Tisk do souboru';
    LS_PrintInBackgroundStr: 'Tisk na pozadí';
    LS_OptionsStr: 'Volby';
    LS_SaveInBackground: 'Ulo?it na pozadí';
    LS_PageRangeStr: 'Rozsah stránek';
    LS_CopyAsImageStr: 'Kopírovat jako bitmapu';
    LS_CopyAsMetafileStr: 'Kopírovat jako metasoubor';
    LS_RangeFromStr: 'od';
    LS_RangeToStr: 'do';
    LS_AllStr: 'V?echny';
    LS_PagesStr: 'Stránky';
    LS_SelectionStr: 'Výbìr';
    LS_CopiesStr: 'Kopie';
    LS_NumberOfCopiesStr: 'Poèet kopií';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Rozdìlit obrazovku';
    LS_InvalidNameStr: 'Chybný název';
    LS_DuplicateNameStr: 'Název se ji? pou?ívá';
    LS_UseFilterStr: 'Pou?ít filtr';
    LS_WebPageStr: 'Webová stránka';
    LS_RichFormatStr: 'Formát RichText';
    LS_PDFFormatStr: 'PDF dokument';
    LS_XLSFormatStr97_2013: 'Tabulka Excel 97-2013';
    LS_XLSFormatStr: 'Tabulka Excel';
    LS_AtStr: 'na';
    LS_FormStr: 'Formuláø';
    LS_DefaultStr: 'Default';
    LS_ZoomInStr: 'Pøiblí?it';
    LS_ZoomOutStr: 'Oddálit';
    LS_CopyStr: 'Kopírovat';
    LS_EditStr: 'Upravit';
    LS_FindCaptionStr: 'Najít';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Najít &dal?í';
    LS_WholeWordsStr: '&Pouze celá slova';
    LS_MatchCaseStr: '&Rozli?ovat velikost písmen';
    LS_DirectionUpStr: '&Nahoru';
    LS_DirectionDownStr: '&Dolù';
    LS_DirectionCaptionStr: 'Orientace';
    LS_ColumnsStr: 'Sloupce';
    LS_SetupStr: 'Nastavení...';
    LS_FontSizeError: 'Chyba velikosti textu';
    LS_OddPages: 'Sudé';
    LS_EvenPages: 'Liché';
    LS_OddPagesOnly: 'Pouze sudé stránky';
    LS_EvenPagesOnly: 'Pouze liché stránky';
    LS_AllOddAndEven: 'V?echny';
    LS_PrintDialogError: 'Problém s dialogem tisku';
    LS_PageSelectionHint: 'Oddìlené èísla stránek støedníkem nebo rozmezí stránek s pomlèkou. tj.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Report "%s"';
    LS_ZoomHint: 'Upravit pøiblí?ení.';
    LS_Aplicar: 'Pou?ít';
    LS_Propriedades: 'Nastavení';
    LS_Salvar_Como: 'Ulo?it jako';
    LS_FileCorrupted: 'Soubor je po?kozen!';
    LS_FileCorruptedHeader: 'Po?kozená hlavièka souboru "%s"!';
    LS_FileVersion: 'Chybný typ souboru %d!';
    LS_PageSettings: 'Nastavení stránky';
    LS_PageMargins: 'Okraje';
    LS_PageMarginsTop: 'Nahoøe';
    LS_PageMarginsBottom: 'Dole';
    LS_PageMarginsRigth: 'Vpravo';
    LS_PageMarginsLeft: 'Vlevo';
    LS_PageMarginsPaper: 'Okraje papíru';
    LS_PagePaper: 'Papír';
    LS_PaperSize: 'Velikost papíru';
    LS_PaperSizeWidth: '?íøka';
    LS_PaperSizeHeigth: 'Vý?ka';
    LS_PaperOrientation: 'Orientace';
    LS_PaperOrientationLandscape: 'Na ?íøku';
    LS_PaperOrientationPortrait: 'Na vý?ku';
    LS_Duplex: 'Automatický oboustranný tisk';
    LS_OnlyOneInstance: 'Je povolena pouze jedna instance %s!';
    LS_NotImplemented: '%s není implementováno na této platformì!';
    LS_NoHandle: 'Handle není dostupný!';

    LS_LastFooMsg: '';
  );
  
  GermanStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Drucke...';
    LS_FilterInProgressStr: 'Speichere Bericht...';
    LS_PreparingReportStr: 'Bereite den Bericht vor...';
    LS_PrinterNotFoundStr: 'Drucker nicht gefunden';
    LS_NoPrinterSelected: 'Es ist kein Drucker ausgewählt.';	
    LS_NoPathToPrinterStr: 'Ungültiger Druckerpfad';
    LS_LoadDefaultConfigStr: 'Standardkonfiguration laden';
    LS_PrinterDriverErrorStr: 'Druckertreiberfehler';
    LS_PageStr: 'Seite';
    LS_PrepareErrorStr: 'Fehler beim vorbereiten des Berichts';
    LS_PageBreakStr: 'Setze fort...';
    LS_PageMendStr: 'Fortsetzung';
    LS_ReportEndStr: 'Ende';
	LS_FileExists: 'Die Datei existiert bereits. Überschreiben?';
    LS_FileNotFoundStr: 'Datei nicht gefunden';
	LS_FileNameIsEmpty: 'Kein Dateiname angegeben';
    LS_FileNameStr: 'Dateiname';
    LS_AllFileTypesStr: 'Alle Dateien';
    LS_LoadReportStr: 'Bericht laden';
    LS_NotFoundStr: 'Nicht gefunden';
    LS_WaitStr: 'Bitte warten...';
    LS_FinishedStr: 'Fertig';
    LS_CancelStr: 'Abbrechen';
    LS_CloseStr: 'Schliessen';
    LS_SaveStr: 'Speichern';
    LS_SendStr: 'Senden';
    LS_PrintStr: 'Drucken';
    LS_AboutTheStr: 'Über';
    LS_PreviewStr: 'Vorschau';
    LS_OfStr: 'von';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Erste Seite';
    LS_PriorPageStr: 'Vorherige Seite';
    LS_NextPageStr: 'Nächste Seite';
    LS_LastPageStr: 'Letzte Seite';
    LS_EntirePageStr: 'Gesamte Seite';
    LS_EntireWidthStr: 'Gesamte Breite';
    LS_MultiplePagesStr: 'Mehrere Seiten';
    LS_ConfigPrinterStr: 'Drucker konfigurieren';
    LS_SaveToFileStr: 'Als Datei speichern';
    LS_SendToStr: 'Senden an';
    LS_PrinterStr: 'Drucker';
    LS_NameStr: 'Name';
    LS_PrintToFileStr: 'In Datei drucken';
    LS_PrintInBackgroundStr: 'Im Hintergrund drucken';
    LS_OptionsStr: 'Optionen';
    LS_SaveInBackground: 'Im Hintergrund speichern';
    LS_PageRangeStr: 'Seitenbereich';
    LS_CopyAsImageStr: 'Als Bild kopieren';
	LS_CopyAsMetafileStr: 'Als Metadatei kopieren';
    LS_RangeFromStr: 'von';
    LS_RangeToStr: 'bis';
    LS_AllStr: 'Alle';
    LS_PagesStr: 'Seiten';
    LS_SelectionStr: 'Auswahl';
    LS_CopiesStr: 'Kopien';
    LS_NumberOfCopiesStr: 'Kopienanzahl';
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Bildschirm teilen';
    LS_InvalidNameStr: 'Ungültiger Name';
    LS_DuplicateNameStr: 'Name bereits in Benutzung';
    LS_UseFilterStr: 'Filter';
    LS_WebPageStr: 'Webseite';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Dokument';
    LS_XLSFormatStr97_2013: 'Exceltabelle 97-2013';
	LS_XLSFormatStr: 'Exceltabelle';
    LS_AtStr: 'at';
    LS_FormStr: 'Formular';
    LS_DefaultStr: 'Standard';
    LS_ZoomInStr: 'Zoom erhöhen';
    LS_ZoomOutStr: 'Zoom verringern';
    LS_CopyStr: 'Kopieren';
    LS_EditStr: 'Bearbeiten';
    LS_FindCaptionStr: 'Suchen';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: '&Weitersuchen';
    LS_WholeWordsStr: '&Nur ganze Worte';
    LS_MatchCaseStr: '&Groß- / Kleinschreibung';
    LS_DirectionUpStr: '&Hoch';
    LS_DirectionDownStr: '&Runter';
    LS_DirectionCaptionStr: 'Richtung';
    LS_ColumnsStr: 'Spalten';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Schriftgrößenfehler';
    LS_OddPages: 'Ungerade';
    LS_EvenPages: 'Gerade';
    LS_OddPagesOnly: 'Nur ungerade Seiten';
    LS_EvenPagesOnly: 'Nur gerade Seiten';
    LS_AllOddAndEven: 'Alle';
    LS_PrintDialogError: 'Probleme mit dem Druckerdialog';
    LS_PageSelectionHint: 'Seiten oder Seitenbereiche mit ";" trennen. Z.B.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Bericht "%s"';
    LS_ZoomHint: 'Zoom ändern';
    Ls_Aplicar: 'Übernehmen';
    LS_Propriedades: 'Einstellungen';
    LS_Salvar_Como: 'Speichern unter';
    LS_FileCorrupted: 'Die Datei ist beschädigt!';
    LS_FileCorruptedHeader: 'Beschädigter file header "%s"!';
    LS_FileVersion: 'ungültige Dateiversion %d!';
    LS_PageSettings: 'Seiteneinstellungen';
    LS_PageMargins: 'Ränder';
    LS_PageMarginsTop: 'Oben';
    LS_PageMarginsBottom: 'Unten';
    LS_PageMarginsRigth: 'Rechts';
    LS_PageMarginsLeft: 'Links';
    LS_PageMarginsPaper: 'Seitenränder';
    LS_PagePaper: 'Papier';
    LS_PaperSize: 'Papiergröße';
    LS_PaperSizeWidth: 'Breite';
    LS_PaperSizeHeigth: 'Höhe';
    LS_PaperOrientation: 'Ausrichtung';
    LS_PaperOrientationLandscape: 'Querformat';
    LS_PaperOrientationPortrait: 'Hochformat';
    LS_Duplex: 'automatischer, doppelseitiger Druck';
    LS_OnlyOneInstance: 'Es ist nur eine Instanz von %s erlaubt!';
    LS_NotImplemented: '%s ist für diese Platform noch nicht implementiert!';
    LS_NoHandle: 'Handle nicht verfügbar!';	
	
    LS_LastFooMsg: '';	
  );

procedure DetectLocale;
{$IfNDef MSWINDOWS}
var
  dlct: string;
{$EndIf}
begin
{$IfNDef MSWINDOWS}
  dlct := AnsiUpperCase(Copy(GetEnvironmentVariable('LANG'), 1, 2));
  if dlct = 'EN' then
    LocaleStrings := EnglishStrings
  else if dlct = 'PT' then
    LocaleStrings := PortugueseStrings
  else if dlct = 'ES' then
    LocaleStrings := SpanishStrings
  else if dlct = 'FR' then
    LocaleStrings := FrenchStrings
  else if dlct = 'IT' then
    LocaleStrings := ItalianStrings
  else if dlct = 'SW' then
    LocaleStrings := SwedishStrings
  else if dlct = 'RU' then
    LocaleStrings := RussianStrings
  else if dlct = 'CS' then
    LocaleStrings := CzechStrings
  else if dlct = 'DE' then
    LocaleStrings := GermanStrings
  else
    LocaleStrings := EnglishStrings;
{$Else}
  case SysLocale.PriLangID of
    $09 {LANG_ENGLISH}: LocaleStrings := EnglishStrings;
    $16 {LANG_PORTUGUESE}: LocaleStrings := PortugueseStrings;
    $0a {LANG_SPANISH}: LocaleStrings := SpanishStrings;
    $0c {LANG_FRENCH}: LocaleStrings := FrenchStrings;
    $10 {LANG_ITALIAN}: LocaleStrings := ItalianStrings;
    $1d {LANG_SWEDISH}: LocaleStrings := SwedishStrings;
    $19 {LANG_RUSSIAN}: LocaleStrings := RussianStrings;
    $05 {LANG_CZECH}: LocaleStrings := CzechStrings;
    $07 {LANG_GERMAN}: LocaleStrings := GermanStrings;
  else
    LocaleStrings := EnglishStrings;
  end;
{$EndIf}
end;

initialization
  DetectLocale;

end.
