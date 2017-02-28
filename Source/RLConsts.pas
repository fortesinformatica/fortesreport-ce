{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios săo constituídos por bandas que  }
{ tęm funçőes específicas no fluxo de impressăo. Vocę definir agrupamentos     }
{ subníveis e totais simplesmente pela relaçăo hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              Márcio Martins                                  }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Vocę pode obter a última versăo desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informaçőes vocę pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca é software livre; vocę pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versăo 2.1 da Licença, ou (a seu critério) }
{ qualquer versăo posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇĂO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Vocę deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se năo, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Vocę também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descriçăo...
******************************************************************************}

{$I RLReport.inc}

{@unit RLConsts - Variáveis de internacionalizaçăo e variáveis de configuraçăo. }
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
  {@const ScreenPPI - Resoluçăo do monitor em pixels por polegada.
   Representa a quantidade de pixels por polegada do vídeo. O valor real varia de monitor para monitor mas,
   para facilitar cálculos e tornar os projetos independentes do terminal, essa valor é assumido como sendo 96. :/}
  ScreenPPI = 96;

  {@const InchAsMM - Fator de conversăo de polegada para milímetros.
   Este fator é utilizado em diversos pontos para conversőes de coordenadas. :/}
  InchAsMM = 254 / 10;

  {@const MMAsPixels - Fator de conversăo de milímetros para pixels de tela.
   @links ScreenPPI, InchAsMM. :/}
  MMAsPixels = ScreenPPI / InchAsMM;

  MaxPageNo = 999999;

  ReportFileExt = '.rpf';

  { constantes para exibiçăo na inicializaçăo e no sobre do delphi a partir da versăo 2009 }
  cRLSobreDialogoTitulo = 'FortesReport Community Edition';
  cRLSobreTitulo = 'FortesReport Community Edition VCL';
  cRLSobreDescricao = 'FortesReport Community Edition VCL ' + sLineBreak +
                      'http://www.fortesreport.com.br' + sLineBreak +
                      'https://github.com/fortesinformatica/fortesreport-ce' + sLineBreak +
                      'Componentes para Geraçăo de Relatórios' + sLineBreak +
                      'Lesser General Public License version 2.0';
  cRLSobreLicencaStatus = 'LGPLv2';
  
  {****                                  *}	

const
  CS_CopyrightStr = 'Copyright © 1999-2016 Fortes Informática';
  CS_ProductTitleStr = 'FortesReport Community Edition';
  CS_URLStr = 'http://www.fortesreport.com.br';
  CS_AuthorNameStr = 'Ronaldo Moreira';
  CS_Version = '4.0';

type
  TRLLocaleStrings = record
    {@var LocaleStrings.LS_PrintingInProgressStr - Variável de internacionalizaçăo para "Imprimindo o relatório..." :/}
    LS_PrintingInProgressStr: string;
    {@var LS_FilterInProgressStr - Variável de internacionalizaçăo para "Salvando o relatório..." :/}
    LS_FilterInProgressStr: string;
    {@var LS_PreparingReportStr - Variável de internacionalizaçăo para "Preparando o relatório..." :/}
    LS_PreparingReportStr: string;
    {@var LS_PrinterNotFoundStr - Variável de internacionalizaçăo para "Nenhuma impressora encontrada" :/}
    LS_PrinterNotFoundStr: string;
    {@var LS_NoPathToPrinterStr - Variável de internacionalizaçăo para "Caminho inválido para a impressora" :/}
    LS_NoPathToPrinterStr: string;
    {@var LS_LoadDefaultConfigStr - Variável de internacionalizaçăo para "Será carregada a configuraçăo padrăo" :/}
    LS_LoadDefaultConfigStr: string;
    {@var LS_PrinterDriverErrorStr - Variável de internacionalizaçăo para "Erro no driver da impressora" :/}
    LS_PrinterDriverErrorStr: string;
    {@var LS_PageStr - Variável de internacionalizaçăo para "Página" :/}
    LS_PageStr: string;
    {@var LS_PrepareErrorStr - Variável de internacionalizaçăo para "Erro durante a preparaçăo do relatório" :/}
    LS_PrepareErrorStr: string;
    {@var LS_PageBreakStr - Variável de internacionalizaçăo para "Continua..." :/}
    LS_PageBreakStr: string;
    {@var LS_PageMendStr - Variável de internacionalizaçăo para "Continuaçăo" :/}
    LS_PageMendStr: string;
    {@var LS_ReportEndStr - Variável de internacionalizaçăo para "Fim" :/}
    LS_ReportEndStr: string;
    {@var LS_FileNotFoundStr - Variável de internacionalizaçăo para "Arquivo năo encontrado" :/}
    LS_FileNotFoundStr: string;
    {@var LS_FileNameStr - Variável de internacionalizaçăo para "Nome do arquivo" :/}
    LS_FileNameStr: string;
    {@var LS_AllFileTypesStr - Variável de internacionalizaçăo para "Todos os arquivos" :/}
    LS_AllFileTypesStr: string;
    {@var LS_LoadReportStr - Variável de internacionalizaçăo para "Carregar relatório" :/}
    LS_LoadReportStr: string;
    {@var LS_NotFoundStr - Variável de internacionalizaçăo para "Năo encontrado" :/}
    LS_NotFoundStr: string;
    {@var LS_WaitStr - Variável de internacionalizaçăo para "Aguarde..." :/}
    LS_WaitStr: string;
    {@var LS_FinishedStr - Variável de internacionalizaçăo para "Concluído" :/}
    LS_FinishedStr: string;
    {@var LS_CancelStr - Variável de internacionalizaçăo para "Cancelar" :/}
    LS_CancelStr: string;
    {@var LS_CloseStr - Variável de internacionalizaçăo para "Fechar" :/}
    LS_CloseStr: string;
    {@var LS_SaveStr - Variável de internacionalizaçăo para "Salvar" :/}
    LS_SaveStr: string;
    {@var LS_SendStr - Variável de internacionalizaçăo para "Enviar" :/}
    LS_SendStr: string;
    {@var LS_PrintStr - Variável de internacionalizaçăo para "Imprimir" :/}
    LS_PrintStr: string;
    {@var LS_AboutTheStr - Variável de internacionalizaçăo para "Sobre o" :/}
    LS_AboutTheStr: string;
    {@var LS_PreviewStr - Variável de internacionalizaçăo para "Pré-visualizaçăo" :/}
    LS_PreviewStr: string;
    {@var LS_OfStr - Variável de internacionalizaçăo para "de" :/}
    LS_OfStr: string;
    {@var LS_ZoomStr - Variável de internacionalizaçăo para "Zoom" :/}
    LS_ZoomStr: string;
    {@var LS_FirstPageStr - Variável de internacionalizaçăo para "Primeira página" :/}
    LS_FirstPageStr: string;
    {@var LS_PriorPageStr - Variável de internacionalizaçăo para "Página anterior" :/}
    LS_PriorPageStr: string;
    {@var LS_NextPageStr - Variável de internacionalizaçăo para "Próxima página" :/}
    LS_NextPageStr: string;
    {@var LS_LastPageStr - Variável de internacionalizaçăo para "Última página" :/}
    LS_LastPageStr: string;
    {@var LS_EntirePageStr - Variável de internacionalizaçăo para "Página inteira" :/}
    LS_EntirePageStr: string;
    {@var LS_EntireWidthStr - Variável de internacionalizaçăo para "Largura da página" :/}
    LS_EntireWidthStr: string;
    {@var LS_MultiplePagesStr - Variável de internacionalizaçăo para "Várias páginas" :/}
    LS_MultiplePagesStr: string;
    {@var LS_ConfigPrinterStr - Variável de internacionalizaçăo para "Configurar impressora" :/}
    LS_ConfigPrinterStr: string;
    {@var LS_SaveToFileStr - Variável de internacionalizaçăo para "Salvar em disco" :/}
    LS_SaveToFileStr: string;
    {@var LS_SendToStr - Variável de internacionalizaçăo para "Enviar para" :/}
    LS_SendToStr: string;
    {@var LS_PrinterStr - Variável de internacionalizaçăo para "Impressora" :/}
    LS_PrinterStr: string;
    {@var LS_NameStr - Variável de internacionalizaçăo para "Nome" :/}
    LS_NameStr: string;
    {@var LS_PrintToFileStr - Variável de internacionalizaçăo para "Imprimir em arquivo" :/}
    LS_PrintToFileStr: string;
    {@var LS_PrintInBackgroundStr - Variável de internacionalizaçăo para "Imprimir em segundo plano" :/}
    LS_PrintInBackgroundStr: string;
    {@var LS_OptionsStr - Variável de internacionalizaçăo para "Opçőes" de filtragem. :/}
    LS_OptionsStr: string;
    {@var LS_SaveInBackground - Variável de internacionalizaçăo para "Salvar em segundo plano" :/}
    LS_SaveInBackground: string;
    {@var LS_PageRangeStr - Variável de internacionalizaçăo para "Intervalo de páginas" :/}
    LS_PageRangeStr: string;
    {@var LS_CopyAsImageStr - Variável de internacionalizaçăo para "Copiar como imagem" :/}
    LS_CopyAsImageStr: string;
    {@var LS_RangeFromStr - Variável de internacionalizaçăo para "de" :/}
    LS_RangeFromStr: string;
    {@var LS_RangeToStr - Variável de internacionalizaçăo para "até" :/}
    LS_RangeToStr: string;
    {@var LS_AllStr - Variável de internacionalizaçăo para "Tudo" :/}
    LS_AllStr: string;
    {@var LS_PagesStr - Variável de internacionalizaçăo para "Páginas" :/}
    LS_PagesStr: string;
    {@var LS_SelectionStr - Variável de internacionalizaçăo para "Seleçăo" :/}
    LS_SelectionStr: string;
    {@var LS_CopiesStr - Variável de internacionalizaçăo para "Cópias" :/}
    LS_CopiesStr: string;
    {@var LS_NumberOfCopiesStr - Variável de internacionalizaçăo para "Número de cópias" :/}
    LS_NumberOfCopiesStr: string;
    {@var LS_OkStr - Variável de internacionalizaçăo para "Ok" :/}
    LS_OkStr: string;
    {@var LS_DivideScreenStr - Variável de internacionalizaçăo para "Dividir a tela" :/}
    LS_DivideScreenStr: string;
    {@var LS_InvalidNameStr - Variável de internacionalizaçăo para "Nome inválido" :/}
    LS_InvalidNameStr: string;
    {@var LS_DuplicateNameStr - Variável de internacionalizaçăo para "Nome já utilizado" :/}
    LS_DuplicateNameStr: string;
    {@var LS_UseFilterStr - Variável de internacionalizaçăo para "Usar Filtro" :/}
    LS_UseFilterStr: string;
    {@var LS_WebPageStr - Variável de internacionalizaçăo para "Página da Web" :/}
    LS_WebPageStr: string;
    {@var LS_RichFormatStr - Variável de internacionalizaçăo para "Formato RichText" :/}
    LS_RichFormatStr: string;
    {@var LS_PDFFormatStr - Variável de internacionalizaçăo para "Documento PDF" :/}
    LS_PDFFormatStr: string;
    {@var LS_XLSFormatStr97-2013 - Variável de internacionalizaçăo para "Planilha Excel 97-2013" :/}
    LS_XLSFormatStr97_2013: string;
    {@var LS_XLSFormatStr - Variável de internacionalizaçăo para "Planilha Excel" :/}
    LS_XLSFormatStr: string;
    {@var LS_AtStr - Variável de internacionalizaçăo para "em" :/}
    LS_AtStr: string;
    {@var LS_FormStr - Variável de internacionalizaçăo para "Formulário" :/}
    LS_FormStr: string;
    {@var LS_DefaultStr - Variável de internacionalizaçăo para "Padrăo" :/}
    LS_DefaultStr: string;
    {@var LS_ZoomInStr - Variável de internacionalizaçăo para "Aumentar o zoom" :/}
    LS_ZoomInStr: string;
    {@var LS_ZoomOutStr - Variável de internacionalizaçăo para "Diminuir o zoom" :/}
    LS_ZoomOutStr: string;
    {@var LS_CopyStr - Variável de internacionalizaçăo para "Copiar" :/}
    LS_CopyStr: string;
    {@var LS_EditStr - Variável de internacionalizaçăo para "Editar" :/}
    LS_EditStr: string;
    {@var LS_FindCaptionStr - Variável de internacionalizaçăo para "Procurar" :/}
    LS_FindCaptionStr: string;
    {@var LS_TextToFindStr - Variável de internacionalizaçăo para "Te&xto" :/}
    LS_TextToFindStr: string;
    {@var LS_FindNextStr - &Variável de internacionalizaçăo para "Próxima" :/}
    LS_FindNextStr: string;
    {@var LS_WholeWordsStr - Variável de internacionalizaçăo para "Palavras &inteiras" :/}
    LS_WholeWordsStr: string;
    {@var LS_MatchCaseStr - Variável de internacionalizaçăo para "Diferenciar &maiúsculas de minúsculas" :/}
    LS_MatchCaseStr: string;
    {@var LS_DirectionUpStr - Variável de internacionalizaçăo para "A&cima" :/}
    LS_DirectionUpStr: string;
    {@var LS_DirectionDownStr - Variável de internacionalizaçăo para "A&baixo" :/}
    LS_DirectionDownStr: string;
    {@var LS_DirectionCaptionStr - Variável de internacionalizaçăo para "Direçăo" :/}
    LS_DirectionCaptionStr: string;
    {@var LS_ColumnsStr - Variável de internacionalizaçăo para "Colunas". :/}
    LS_ColumnsStr: string;
    {@var LS_SetupStr - Variável de internacionalizaçăo para "Configuraçăo". :/}
    LS_SetupStr: string;
    {@var LS_FontSizeError - Variável de internacionalizaçăo para "Erro no cálculo das fontes". :/}
    LS_FontSizeError: string;
    {@var LS_OddPages - Variável de internacionalizaçăo para "Ímpares". :/}
    LS_OddPages: string;
    {@var LS_EvenPages - Variável de internacionalizaçăo para "Pares". :/}
    LS_EvenPages: string;
    {@var LS_OddPagesOnly - Variável de internacionalizaçăo para "Ímpares somente". :/}
    LS_OddPagesOnly: string;
    {@var LS_EvenPagesOnly - Variável de internacionalizaçăo para "Pares somente". :/}
    LS_EvenPagesOnly: string;
    {@var LS_AllOddAndEven - Variável de internacionalizaçăo para "Todas". :/}
    LS_AllOddAndEven: string;
    {@var LS_PrintDialogError - Variável de internacionalizaçăo para "Problemas com o diálogo da impressora". :/}
    LS_PrintDialogError: string;
    {@var LS_PageSelectionHint - Variável de internacionalizaçăo para "Separe com ponto-e-vírgula os números ou intervalos de páginas a imprimir. Ex.: 1;3;5-12;4". :/}
    LS_PageSelectionHint: string;
    {@var LS_DefaultJobTitle - Variável de internacionalizaçăo para "Relatório %s". :/}
    LS_DefaultJobTitle: string;
    {@var LS_ZoomHint - Variável de internacionalizaçăo para "Diminuir o zoom" :/}
    LS_ZoomHint: string;
    {@var Ls_Aplicar - Variável de internacionalizaçăo para "Aplicar". :/}
    Ls_Aplicar: String;
    {@var Ls_Propriedades - Variável de internacionalizaçăo para "Propriedades". :/}
    Ls_Propriedades: string;
    {@var Ls_Salvar_Como - Variável de internacionalizaçăo para "Salvar como". :/}
    Ls_Salvar_Como: string;
    {@var Ls_Nome_Arquivo - Variável de internacionalizaçăo para "Nome do Arquivo". :/}
    Ls_Nome_Arquivo: string;
    {@var LS_FileCorrupted - Variável de internacionalizaçăo para "Arquivo corrompido". :/}
    LS_FileCorrupted: String;
    {@var LS_FileVersion - Variável de internacionalizaçăo para "Versăo de Arquivo inválido". :/}
    LS_FileVersion: String;
    {@var Ls_PageSetings - Variável de internacionalizaçăo para "Configuraçăo da Página". :/}
    LS_PageSettings: String;
    {@var Ls_Page_margins - Variável de internacionalizaçăo para "Margem da Página". :/}
    LS_PageMargins: String;
    {@var Ls_PageMarginsTop - Variável de internacionalizaçăo para "Margem Superior". :/}
    LS_PageMarginsTop: String;
    {@var Ls_PageMarginsBottom - Variável de internacionalizaçăo para "Margem Inferior". :/}
    LS_PageMarginsBottom: String;
    {@var LS_PageMarginsRigth - Variável de internacionalizaçăo para "Margem direita". :/}
    LS_PageMarginsRigth: String;
    {@var Ls_PageLeftBottom - Variável de internacionalizaçăo para "Margem equerda". :/}
    LS_PageMarginsLeft: String;
    {@var LS_PageMarginsPaper - Variável de internacionalizaçăo para "Margem do Papel". :/}
    LS_PageMarginsPaper: String;
    {@var LS_PagePaper - Variável de internacionalizaçăo para "Papel". :/}
    LS_PagePaper: String;
    {@var LS_PaperSize - Variável de internacionalizaçăo para "Tamanho do Papel". :/}
    LS_PaperSize: String;
    {@var LS_PaperWidth - Variável de internacionalizaçăo para "Largura do Papel". :/}
    LS_PaperSizeWidth: String;
    {@var LS_PaperSizeHeigth - Variável de internacionalizaçăo para "Altura do Papel". :/}
    LS_PaperSizeHeigth: String;
    {@var LS_PaperOrientation - Variável de internacionalizaçăo para "Orientaçăo do Papel". :/}
    LS_PaperOrientation: String;
    {@var LS_PaperOrientationLandscape - Variável de internacionalizaçăo para "Orientaçăo da página em retrato". :/}
    LS_PaperOrientationLandscape: String;
    {@var LS_PaperOrientationPortrait - Variável de internacionalizaçăo para "Orientaçăo da página em paisagem". :/}
    LS_PaperOrientationPortrait: String;

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
    LS_NoPathToPrinterStr: 'Invalid printer path';
    LS_LoadDefaultConfigStr: 'Load default configuration';
    LS_PrinterDriverErrorStr: 'Printer driver error';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Error while preparing report';
    LS_PageBreakStr: 'Continues...';
    LS_PageMendStr: 'Continuation';
    LS_ReportEndStr: 'End';
    LS_FileNotFoundStr: 'File not found';
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
    LS_CopyAsImageStr: 'Copy as image';
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
    LS_Nome_Arquivo: 'File Name:';
    LS_FileCorrupted: 'File is corrupted!';
    LS_FileVersion: 'Invalid file version!';
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

    LS_LastFooMsg: '';
  );

var
  PortugueseStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Imprimindo o relatório...';
    LS_FilterInProgressStr: 'Salvando o relatório...';
    LS_PreparingReportStr: 'Preparando o relatório...';
    LS_PrinterNotFoundStr: 'Nenhuma impressora encontrada';
    LS_NoPathToPrinterStr: 'Caminho inválido para a impressora';
    LS_LoadDefaultConfigStr: 'Será carregada a configuraçăo padrăo';
    LS_PrinterDriverErrorStr: 'Erro no driver da impressora';
    LS_PageStr: 'Página';
    LS_PrepareErrorStr: 'Erro durante a preparaçăo do relatório';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuaçăo';
    LS_ReportEndStr: 'Fim';
    LS_FileNotFoundStr: 'Arquivo năo encontrado';
    LS_FileNameStr: 'Nome do arquivo';
    LS_AllFileTypesStr: 'Todos os arquivos';
    LS_LoadReportStr: 'Carregar relatório';
    LS_NotFoundStr: 'Năo encontrado';
    LS_WaitStr: 'Aguarde...';
    LS_FinishedStr: 'Concluído';
    LS_CancelStr: 'Cancelar';
    LS_CloseStr: 'Fechar';
    LS_SaveStr: 'Salvar';
    LS_SendStr: 'Enviar';
    LS_PrintStr: 'Imprimir';
    LS_AboutTheStr: 'Sobre o';
    LS_PreviewStr: 'Pré-visualizaçăo';
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
    LS_OptionsStr: 'Opçőes';
    LS_SaveInBackground: 'Salvar em segundo plano';
    LS_PageRangeStr: 'Intervalo de páginas';
    LS_CopyAsImageStr: 'Copiar como imagem';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'até';
    LS_AllStr: 'Tudo';
    LS_PagesStr: 'Páginas';
    LS_SelectionStr: 'Seleçăo';
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
    LS_DefaultStr: 'Padrăo';
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
    LS_DirectionCaptionStr: 'Direçăo';
    LS_ColumnsStr: 'Colunas';
    LS_SetupStr: 'Configuraçăo';
    LS_FontSizeError: 'Erro no cálculo das fontes';
    LS_OddPages: 'Ímpares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'Somente Ímpares';
    LS_EvenPagesOnly: 'Somente Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas com o diálogo da impressora';
    LS_PageSelectionHint: 'Separe com ponto-e-vírgula os números ou intervalos de páginas a imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Relatório "%s"';
    LS_ZoomHint: 'Vocę também pode aumentar ou reduzir o zoom do relatório' + sLineBreak + 'pressionando "Ctrl" e usando a rolagem do mouse.';
    LS_Aplicar: 'Aplicar';
    LS_Propriedades: 'Propriedades';
    LS_Salvar_Como: 'Salvar Como';
    LS_Nome_Arquivo: 'Nome do Arquivo:';
    LS_FileCorrupted: 'Arquivo Corrompido';
    LS_FileVersion: 'Vers úo de arquivo inv ílido!';
    LS_PageSettings: 'Configura Ô  es da p ígina';
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

    LS_LastFooMsg: '';
  );

var
  FrenchStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Préparation du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouvée';
    LS_NoPathToPrinterStr: 'Chemin d''imprimante non valide';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la préparation du rapport';
    LS_PageBreakStr: 'Suite...';
    LS_PageMendStr: 'A suivre';
    LS_ReportEndStr: 'Fin';
    LS_FileNotFoundStr: 'Fichier non trouvé';
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
    LS_FirstPageStr: 'Premičre page';
    LS_PriorPageStr: 'Page précédente';
    LS_NextPageStr: 'Page suivante';
    LS_LastPageStr: 'Derničre page';
    LS_EntirePageStr: 'Page entičre';
    LS_EntireWidthStr: 'Pleine largeur';
    LS_MultiplePagesStr: 'Plusieurs pages';
    LS_ConfigPrinterStr: 'Configuration de l''imprimante';
    LS_SaveToFileStr: 'Enregistrer dans un fichier';
    LS_SendToStr: 'Envoyer ŕ...';
    LS_PrinterStr: 'Imprimante';
    LS_NameStr: 'Nom';
    LS_PrintToFileStr: 'Imprimer dans un fichier';
    LS_PrintInBackgroundStr: 'Imprimer dans background';
    LS_OptionsStr: 'Options';
    LS_SaveInBackground: 'Enregistrer dans background';
    LS_PageRangeStr: 'Intervalle de pages';
    LS_CopyAsImageStr: 'Copie sous forme d''image';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'ŕ';
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
    LS_AtStr: 'ŕ';
    LS_FormStr: 'Formulaire';
    LS_DefaultStr: 'Défaut';
    LS_ZoomInStr: 'Grandir zoom';
    LS_ZoomOutStr: 'Réduire zoom';
    LS_CopyStr: 'Copier';
    LS_EditStr: 'Éditer';
    LS_FindCaptionStr: 'Trouvaille';
    LS_TextToFindStr: 'Te&xte';
    LS_FindNextStr: 'A&prčs';
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
    LS_Aplicar: 'Appliquer';
  );

var
  SpanishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impresión en marcha...';
    LS_FilterInProgressStr: 'Guardando el informe...';
    LS_PreparingReportStr: 'Preparación del informe...';
    LS_PrinterNotFoundStr: 'Impresora no encontrada';
    LS_NoPathToPrinterStr: 'Camino de la impresora no es válido';
    LS_LoadDefaultConfigStr: 'Cargar la configuración estándar';
    LS_PrinterDriverErrorStr: 'Error en driver de la impresora';
    LS_PageStr: 'Página';
    LS_PrepareErrorStr: 'Un error ocurrió mientras se preparaba el informe';
    LS_PageBreakStr: 'Continúa...';
    LS_PageMendStr: 'Continuación';
    LS_ReportEndStr: 'Extremo';
    LS_FileNotFoundStr: 'Archivo no encontrado';
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
  );

var
  ItalianStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Préparation du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouvée';
    LS_NoPathToPrinterStr: 'Invalid printer path';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la prépartaion du rapport';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuazione';
    LS_ReportEndStr: 'Fine';
    LS_FileNotFoundStr: 'Archivio non fondň';
    LS_FileNameStr: 'Nome di file';
    LS_AllFileTypesStr: 'Tutti archiviano';
    LS_LoadReportStr: 'Rapporto di carico';
    LS_NotFoundStr: 'Non fondato';
    LS_WaitStr: 'Attesa...';
    LS_FinishedStr: 'Finito';
    LS_CancelStr: 'Annulli';
    LS_CloseStr: 'Vicino';
    LS_SaveStr: 'Salvataggio';
    LS_SendStr: 'Spinta dellonda';
    LS_PrintStr: 'Stampa';
    LS_AboutTheStr: 'Circa';
    LS_PreviewStr: 'Anteprima';
    LS_OfStr: 'di';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Prima la pagina';
    LS_PriorPageStr: 'Antecedente la pagina';
    LS_NextPageStr: 'Prossimo pagina';
    LS_LastPageStr: 'Ultima pagina';
    LS_EntirePageStr: 'Pagina intera';
    LS_EntireWidthStr: 'Ampiezza intera';
    LS_MultiplePagesStr: 'Pagine multiple';
    LS_ConfigPrinterStr: 'Configuri stampante';
    LS_SaveToFileStr: 'Salvi archiviare';
    LS_SendToStr: 'Spedisca a';
    LS_PrinterStr: 'Stampante';
    LS_NameStr: 'Nome';
    LS_PrintToFileStr: 'Stampi archiviare';
    LS_PrintInBackgroundStr: 'Stampi in sfondo';
    LS_OptionsStr: 'Opzioni';
    LS_SaveInBackground: 'Salvi in sfondo';
    LS_PageRangeStr: 'Serie di pagina';
    LS_CopyAsImageStr: 'Copy as image';
    LS_RangeFromStr: 'da';
    LS_RangeToStr: 'a';
    LS_AllStr: 'Tutti';
    LS_PagesStr: 'Pagine';
    LS_SelectionStr: 'Selezione';
    LS_CopiesStr: 'Copie';
    LS_NumberOfCopiesStr: 'Numero di copie';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Divida lo schermo';
    LS_InvalidNameStr: 'Nome nullo';
    LS_DuplicateNameStr: 'Giŕ chiami in uso';
    LS_UseFilterStr: 'Filtro di uso';
    LS_WebPageStr: 'Pagina di Web';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
    LS_XLSFormatStr97_2013: 'Eccella foglio di calcolo elettronico 97-2013';
    LS_XLSFormatStr: 'Eccella foglio di calcolo elettronico';
    LS_AtStr: 'a';
    LS_FormStr: 'Forma';
    LS_DefaultStr: 'Contumacia';
    LS_ZoomInStr: 'Zoom di aumento';
    LS_ZoomOutStr: 'Zoom di calo';
    LS_CopyStr: 'Copia';
    LS_EditStr: 'Compili';
    LS_FindCaptionStr: 'Trouvaille';
    LS_TextToFindStr: 'Testo';
    LS_FindNextStr: 'Trovi Prossimo';
    LS_WholeWordsStr: 'Parole intere solamente';
    LS_MatchCaseStr: 'Accoppi Caso';
    LS_DirectionUpStr: 'Su';
    LS_DirectionDownStr: 'In giů';
    LS_DirectionCaptionStr: 'Direzione';
    LS_ColumnsStr: 'Colonnas';
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Font size error';
    LS_OddPages: 'Odd';
    LS_EvenPages: 'Even';
    LS_OddPagesOnly: 'Odd pages only';
    LS_EvenPagesOnly: 'Even pages only';
    LS_AllOddAndEven: 'All';
    LS_PrintDialogError: 'Problems calling the printer dialog';
    LS_PageSelectionHint: 'Separate page numbers or page intervals with ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Rapporto "%s"';
  );

var
  SwedishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Das Drucken im Gange...';
    LS_FilterInProgressStr: 'Das Sparen von Bericht  ...';
    LS_PreparingReportStr: 'Das Vorbereiten von Berich...';
    LS_PrinterNotFoundStr: 'Drucker fand nicht';
    LS_NoPathToPrinterStr: 'Ungültiger Druckerpfad';
    LS_LoadDefaultConfigStr: 'Laden Sie Standardkonfiguration';
    LS_PrinterDriverErrorStr: 'Druckerfahrer Fehler';
    LS_PageStr: 'Seite';
    LS_PrepareErrorStr: 'Fehler, während das Vorbereiten von Bericht';
    LS_PageBreakStr: 'Setzt fort...';
    LS_PageMendStr: 'Fortsetzung';
    LS_ReportEndStr: 'Ende';
    LS_FileNotFoundStr: 'File fand nicht';
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
    LS_CopyAsImageStr: 'Copy as image';
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
  );

var
  RussianStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'ĐźĐµŃ‡Đ°Ń‚ŃŚ...';
    LS_FilterInProgressStr: 'ĐˇĐľŃ…Ń€Đ°Đ˝ĐµĐ˝Đ¸Đµ ĐľŃ‚Ń‡ĐµŃ‚Đ°...';
    LS_PreparingReportStr: 'ĐźĐľĐ´ĐłĐľŃ‚ĐľĐ˛ĐşĐ° ĐľŃ‚Ń‡ĐµŃ‚Đ°...';
    LS_PrinterNotFoundStr: 'ĐźŃ€Đ¸Đ˝Ń‚ĐµŃ€ Đ˝Đµ Đ˝Đ°ĐąĐ´ĐµĐ˝';
    LS_NoPathToPrinterStr: 'Đ?ĐµĐżŃ€Đ°Đ˛Đ¸Đ»ŃŚĐ˝Ń‹Đą ĐżŃŃ‚ŃŚ ĐżŃ€Đ¸Đ˝Ń‚ĐµŃ€Đ°';
    LS_LoadDefaultConfigStr: 'Đ—Đ°ĐłŃ€ŃĐ·Đ¸Ń‚ŃŚ ĐşĐľĐ˝Ń„Đ¸ĐłŃŃ€Đ°Ń†Đ¸Ń? ĐżĐľ ŃĐĽĐľĐ»Ń‡Đ°Đ˝Đ¸ŃŽ';
    LS_PrinterDriverErrorStr: 'ĐžŃĐ¸Đ±ĐşĐ° Đ´Ń€Đ°ĐąĐ˛ĐµŃ€Đ° ĐżŃ€Đ¸Đ˝Ń‚ĐµŃ€Đ°';
    LS_PageStr: 'ĐˇŃ‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_PrepareErrorStr: 'ĐžŃĐ¸Đ±ĐşĐ° ĐżŃ€Đ¸ ĐżĐľĐ´ĐłĐľŃ‚ĐľĐ˛ĐşĐµ ĐľŃ‚Ń‡ĐµŃ‚Đ°';
    LS_PageBreakStr: 'ĐźŃ€ĐľĐ´ĐľĐ»Đ¶Đ°ĐµŃ‚Ń?Ń?...';
    LS_PageMendStr: 'ĐźŃ€ĐľĐ´ĐľĐ»Đ¶ĐµĐ˝Đ¸Đµ';
    LS_ReportEndStr: 'ĐšĐľĐ˝ĐµŃ†';
    LS_FileNotFoundStr: 'Đ¤Đ°ĐąĐ» Đ˝Đµ Đ˝Đ°ĐąĐ´ĐµĐ˝';
    LS_FileNameStr: 'ĐĐĽŃ? Ń„Đ°ĐąĐ»Đ°';
    LS_AllFileTypesStr: 'Đ’Ń?Đµ Ń„Đ°ĐąĐ»Ń‹';
    LS_LoadReportStr: 'Đ—Đ°ĐłŃ€ŃĐ·Đ¸Ń‚ŃŚ ĐľŃ‚Ń‡ĐµŃ‚';
    LS_NotFoundStr: 'Đ?Đµ Đ˝Đ°ĐąĐ´ĐµĐ˝';
    LS_WaitStr: 'ĐźĐľĐ´ĐľĐ¶Đ´Đ¸Ń‚Đµ...';
    LS_FinishedStr: 'Đ—Đ°Đ˛ĐµŃ€ŃĐµĐ˝Đľ';
    LS_CancelStr: 'ĐžŃ‚ĐĽĐµĐ˝Đ°';
    LS_CloseStr: 'Đ—Đ°ĐşŃ€Ń‹Ń‚ŃŚ';
    LS_SaveStr: 'ĐˇĐľŃ…Ń€Đ°Đ˝Đ¸Ń‚ŃŚ';
    LS_SendStr: 'ĐžŃ‚ĐżŃ€Đ°Đ˛Đ¸Ń‚ŃŚ';
    LS_PrintStr: 'ĐźĐµŃ‡Đ°Ń‚ŃŚ';
    LS_AboutTheStr: 'Đž ĐźŃ€ĐľĐłŃ€Đ°ĐĽĐĽĐµ';
    LS_PreviewStr: 'ĐźŃ€ĐµĐ´Đ˛Đ°Ń€Đ¸Ń‚ĐµĐ»ŃŚĐ˝Ń‹Đą ĐżŃ€ĐľŃ?ĐĽĐľŃ‚Ń€';
    LS_OfStr: 'Đ¸Đ·';
    LS_ZoomStr: 'ĐśĐ°ŃŃ‚Đ°Đ±';
    LS_FirstPageStr: 'ĐźĐµŃ€Đ˛Đ°Ń? Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_PriorPageStr: 'ĐźŃ€ĐµĐ´Ń‹Đ´ŃŃ‰Đ°Ń? Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_NextPageStr: 'ĐˇĐ»ĐµĐ´ŃŃŽŃ‰Đ°Ń? Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_LastPageStr: 'ĐźĐľŃ?Đ»ĐµĐ´Đ˝Ń?Ń? Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_EntirePageStr: 'Đ’Ń?Ń? Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_EntireWidthStr: 'Đ’Ń?Ń? ŃĐ¸Ń€Đ¸Đ˝Đ°';
    LS_MultiplePagesStr: 'Đ?ĐµŃ?ĐşĐľĐ»ŃŚĐşĐľ Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†';
    LS_ConfigPrinterStr: 'Đ?Đ°Ń?Ń‚Ń€ĐľĐąĐşĐ° ĐżŃ€Đ¸Đ˝Ń‚ĐµŃ€Đ°';
    LS_SaveToFileStr: 'ĐˇĐľŃ…Ń€Đ°Đ˝Đ¸Ń‚ŃŚ Đ˛ Ń„Đ°ĐąĐ»';
    LS_SendToStr: 'ĐžŃ‚ĐżŃ€Đ°Đ˛Đ¸Ń‚ŃŚ Đ˛';
    LS_PrinterStr: 'ĐźŃ€Đ¸Đ˝Ń‚ĐµŃ€';
    LS_NameStr: 'ĐĐĽŃ?';
    LS_PrintToFileStr: 'ĐźĐµŃ‡Đ°Ń‚ŃŚ Đ˛ Ń„Đ°ĐąĐ»';
    LS_PrintInBackgroundStr: 'ĐźĐµŃ‡Đ°Ń‚ŃŚ Đ˛ Ń„ĐľĐ˝Đµ';
    LS_SaveInBackground: 'ĐˇĐľŃ…Ń€Đ°Đ˝ĐµĐ˝Đ¸Đµ Đ˛ Ń„ĐľĐ˝Đµ';
    LS_PageRangeStr: 'Đ”Đ¸Đ°ĐżĐ°Đ·ĐľĐ˝ ĐżĐµŃ‡Đ°Ń‚Đ¸';
    LS_RangeFromStr: 'Ń?';
    LS_RangeToStr: 'ĐżĐľ';
    LS_AllStr: 'Đ’Ń?Đµ';
    LS_PagesStr: 'ĐˇŃ‚Ń€Đ°Đ˝Đ¸Ń†Ń‹';
    LS_SelectionStr: 'Đ’Ń‹Đ´ĐµĐ»ĐµĐ˝Đ¸Đµ';
    LS_CopiesStr: 'ĐšĐľĐżĐ¸Đ¸';
    LS_NumberOfCopiesStr: 'ĐšĐľĐ»-Đ˛Đľ ĐşĐľĐżĐ¸Đą';
    LS_OkStr: 'ĐĄĐľŃ€ĐľŃĐľ';
    LS_DivideScreenStr: 'Đ Đ°Đ·Đ´ĐµĐ»Đ¸Ń‚ŃŚ Ń?ĐşŃ€Đ°Đ˝';
    LS_InvalidNameStr: 'Đ?ĐµĐżŃ€Đ°Đ˛Đ¸Đ»ŃŚĐ˝ĐľĐµ Đ¸ĐĽŃ?';
    LS_DuplicateNameStr: 'ĐĐĽŃ? ŃĐ¶Đµ Đ¸Ń?ĐżĐľĐ»ŃŚĐ·ŃĐµŃ‚Ń?Ń?';
    LS_UseFilterStr: 'Đ¤Đ¸Đ»ŃŚŃ‚Ń€';
    LS_WebPageStr: 'Đ’ĐµĐ± Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Đ°';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Đ”ĐľĐşŃĐĽĐµĐ˝Ń‚';
    LS_XLSFormatStr: 'Đ˘Đ°Đ±Đ»Đ¸Ń†Đ° Excel';
    LS_AtStr: 'Đ˛';
    LS_FormStr: 'Đ¤ĐľŃ€ĐĽĐ°';
    LS_DefaultStr: 'ĐźĐľ ŃĐĽĐľĐ»Ń‡Đ°Đ˝Đ¸ŃŽ';
    //LS_ColsStr: 'ĐˇŃ‚ĐľĐ»Đ±.';
    LS_ZoomInStr: 'ĐŁĐ˛ĐµĐ»Đ¸Ń‡Đ¸Ń‚ŃŚ';
    LS_ZoomOutStr: 'ĐŁĐĽĐµĐ˝ŃŚŃĐ¸Ń‚ŃŚ';
    LS_CopyStr: 'ĐšĐľĐżĐ¸Ń€ĐľĐ˛Đ°Ń‚ŃŚ';
    LS_EditStr: 'Đ ĐµĐ´Đ°ĐşŃ‚Đ¸Ń€ĐľĐ˛Đ°Ń‚ŃŚ';
    LS_FindCaptionStr: 'Đ?Đ°ĐąŃ‚Đ¸';
    LS_TextToFindStr: 'Đ˘Đµ&ĐşŃ?Ń‚';
    LS_FindNextStr: 'Đ?Đ°ĐąŃ‚Đ¸ &Đ´Đ°Đ»ŃŚŃĐµ';
    LS_WholeWordsStr: '&Đ˘ĐľĐ»ŃŚĐşĐľ Ń?Đ»ĐľĐ˛Đľ Ń†ĐµĐ»Đ¸ĐşĐľĐĽ';
    LS_MatchCaseStr: '&ĐŁŃ‡Đ¸Ń‚Ń‹Đ˛Đ°Ń‚ŃŚ Ń€ĐµĐłĐ¸Ń?Ń‚Ń€';
    LS_DirectionUpStr: '&Đ’Đ˛ĐµŃ€Ń…';
    LS_DirectionDownStr: 'Đ’&Đ˝Đ¸Đ·';
    LS_DirectionCaptionStr: 'Đ?Đ°ĐżŃ€Đ°Đ˛Đ»ĐµĐ˝Đ¸Đµ';
    //Ls_GetOutlineTextMetrics: 'GetOutlineTextMetrics Ń?Đ»ĐľĐĽĐ°Đ»ĐľŃ?ŃŚ';
    //LS_OptionsStr: 'Đ?Đ°Ń?Ń‚Ń€ĐľĐąĐşĐ¸';
    LS_ColumnsStr: 'ĐˇŃ‚ĐľĐ»Đ±Ń†Ń‹';
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
    //Ls_Progresso: 'ĐźŃ€ĐľĐłŃ€ĐµŃ?Ń?';
    Ls_Aplicar: 'ĐźŃ€Đ¸ĐĽĐµĐ˝Đ¸Ń‚ŃŚ';
    LS_Propriedades: 'Đ?Đ°Ń?Ń‚Ń€ĐľĐąĐşĐ¸';
    LS_Salvar_Como: 'ĐˇĐľŃ…Ń€Đ°Đ˝Đ¸Ń‚ŃŚ ĐşĐ°Đş';
    LS_Nome_Arquivo: 'ĐĐĽŃ? Ń„Đ°ĐąĐ»Đ°:';
    LS_FileCorrupted: 'Đ¤Đ°ĐąĐ» ĐżĐľĐ˛Ń€ĐµĐ¶Đ´ĐµĐ˝!';
    LS_FileVersion: 'Đ?ĐµĐżŃ€Đ°Đ˛Đ¸Đ»ŃŚĐ˝Đ°Ń? Đ˛ĐµŃ€Ń?Đ¸Ń? Ń„Đ°ĐąĐ»Đ°!';
    LS_PageSettings: 'Đ?Đ°Ń?Ń‚Ń€ĐľĐąĐşĐ° Ń?Ń‚Ń€Đ°Đ˝Đ¸Ń†Ń‹';
    LS_PageMargins: 'ĐźĐľĐ»Ń?';
    LS_PageMarginsTop: 'Đ’ĐµŃ€Ń…';
    LS_PageMarginsBottom: 'Đ?Đ¸Ń…';
    LS_PageMarginsRigth: 'ĐźŃ€Đ°Đ˛Đľ';
    LS_PageMarginsLeft: 'Đ›ĐµĐ˛Đľ';
    LS_PageMarginsPaper: '';
    LS_PagePaper: 'Đ‘ŃĐĽĐ°ĐłĐ°';
    LS_PaperSize: 'Đ Đ°Đ·ĐĽĐµŃ€ Đ±ŃĐĽĐ°ĐłĐ¸';    
    LS_PaperSizeWidth: 'Đ¨Đ¸Ń€Đ¸Đ˝Đ°';
    LS_PaperSizeHeigth: 'Đ’Ń‹Ń?ĐľŃ‚Đ°'; 
    LS_PaperOrientation: 'ĐžŃ€Đ¸ĐµĐ˝Ń‚Đ°Ń†Đ¸Ń?';
    LS_PaperOrientationLandscape: 'Đ›Đ°Đ˝Đ´ŃĐ°Ń„Ń‚Đ˝Đ°Ń?';
    LS_PaperOrientationPortrait: 'ĐźĐľŃ€Ń‚Ń€ĐµŃ‚Đ˝Đ°Ń?';
  );

var
  CzechStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Probíhá tisk...';
    LS_FilterInProgressStr: 'Ukládám report...';
    LS_PreparingReportStr: 'Připravuji report...';
    LS_PrinterNotFoundStr: 'Tiskárna nenalezena';
    LS_NoPathToPrinterStr: 'Chybná cesta k tiskárně';
    LS_LoadDefaultConfigStr: 'Načíst implicitní nastavení';
    LS_PrinterDriverErrorStr: 'Chyba ovladače tisku';
    LS_PageStr: 'Stránka';
    LS_PrepareErrorStr: 'Chyba během přípravy reportu';
    LS_PageBreakStr: 'Pokračuje...';
    LS_PageMendStr: 'Pokračování';
    LS_ReportEndStr: 'Konec';
    LS_FileNotFoundStr: 'Soubor nenalezen';
    LS_FileNameStr: 'Název souboru';
    LS_AllFileTypesStr: 'Všechny soubory';
    LS_LoadReportStr: 'Načíst report';
    LS_NotFoundStr: 'Nenalezeno';
    LS_WaitStr: 'Čekejte...';
    LS_FinishedStr: 'Ukončeno';
    LS_CancelStr: 'Zrušit';
    LS_CloseStr: 'Zavřít';
    LS_SaveStr: 'Uložit';
    LS_SendStr: 'Poslat';
    LS_PrintStr: 'Tisk';
    LS_AboutTheStr: 'O aplikaci';
    LS_PreviewStr: 'Náhled';
    LS_OfStr: 'z';
    LS_ZoomStr: 'Přiblížení';
    LS_FirstPageStr: 'První stránka';
    LS_PriorPageStr: 'Předchozí stránka';
    LS_NextPageStr: 'Další stránka';
    LS_LastPageStr: 'Poslední stránka';
    LS_EntirePageStr: 'Celá stránka';
    LS_EntireWidthStr: 'Na šířku';
    LS_MultiplePagesStr: 'Více stránek';
    LS_ConfigPrinterStr: 'Konfigurace tiskárny';
    LS_SaveToFileStr: 'Uložit do souboru';
    LS_SendToStr: 'Odeslat';
    LS_PrinterStr: 'Tiskárna';
    LS_NameStr: 'Název';
    LS_PrintToFileStr: 'Tisk do souboru';
    LS_PrintInBackgroundStr: 'Tisk na pozadí';
    LS_OptionsStr: 'Volby';
    LS_SaveInBackground: 'Uložit na pozadí';
    LS_PageRangeStr: 'Page range';
    LS_CopyAsImageStr: 'Copy as image';
    LS_RangeFromStr: 'od';
    LS_RangeToStr: 'do';
    LS_AllStr: 'Všechny';
    LS_PagesStr: 'Stránky';
    LS_SelectionStr: 'Výběr';
    LS_CopiesStr: 'Kopie';
    LS_NumberOfCopiesStr: 'Počet kopií';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Rozdělit obrazovku';
    LS_InvalidNameStr: 'Chybný název';
    LS_DuplicateNameStr: 'Název se již používá';
    LS_UseFilterStr: 'Použít filtr';
    LS_WebPageStr: 'Webová stránka';
    LS_RichFormatStr: 'Formát RichText';
    LS_PDFFormatStr: 'PDF dokument';
    LS_XLSFormatStr97_2013: 'Tabulka Excel 97-2013';
    LS_XLSFormatStr: 'Tabulka Excel';
    LS_AtStr: 'na';
    LS_FormStr: 'Formulář';
    LS_DefaultStr: 'Default';
    LS_ZoomInStr: 'Přiblížit';
    LS_ZoomOutStr: 'Oddálit';
    LS_CopyStr: 'Kopírovat';
    LS_EditStr: 'Upravit';
    LS_FindCaptionStr: 'Najít';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Najít &další';
    LS_WholeWordsStr: '&Pouze celá slova';
    LS_MatchCaseStr: '&Rozlišovat velikost písmen';
    LS_DirectionUpStr: '&Nahoru';
    LS_DirectionDownStr: '&Dolů';
    LS_DirectionCaptionStr: 'Orientace';
    LS_ColumnsStr: 'Sloupce';
    LS_SetupStr: 'Nastavení...';
    LS_FontSizeError: 'Chyba velikosti textu';
    LS_OddPages: 'Sudé';
    LS_EvenPages: 'Liché';
    LS_OddPagesOnly: 'Pouze sudé stránky';
    LS_EvenPagesOnly: 'Pouze liché stránky';
    LS_AllOddAndEven: 'Všechny';
    LS_PrintDialogError: 'Problém s dialogem tisku';
    LS_PageSelectionHint: 'Oddělené čísla stránek nebo rozmezí stránek s ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Report "%s"';
    LS_Propriedades: 'Nastavení';
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
  else
    LocaleStrings := EnglishStrings;
  end;
{$EndIf}
end;

initialization
  DetectLocale;

end.

