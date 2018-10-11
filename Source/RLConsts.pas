{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ � um poderoso gerador de relat�rios dispon�vel como um pacote de componentes }
{ para Delphi. Em FortesReport, os relat�rios s�o constitu�dos por bandas que  }
{ t�m fun��es espec�ficas no fluxo de impress�o. Voc� definir agrupamentos     }
{ subn�veis e totais simplesmente pela rela��o hier�rquica entre as bandas.    }
{ Al�m disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright � 1999-2015 Fortes Inform�tica     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              M�rcio Martins                                  }
{                              R�gys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informa��es voc� pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descri��o...
******************************************************************************}

{$I RLReport.inc}

{@unit RLConsts - Vari�veis de internacionaliza��o e vari�veis de configura��o. }
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
  {@const ScreenPPI - Resolu��o do monitor em pixels por polegada.
   Representa a quantidade de pixels por polegada do v�deo. O valor real varia de monitor para monitor mas,
   para facilitar c�lculos e tornar os projetos independentes do terminal, essa valor � assumido como sendo 96. :/}
  ScreenPPI = 96;

  {@const InchAsMM - Fator de convers�o de polegada para mil�metros.
   Este fator � utilizado em diversos pontos para convers�es de coordenadas. :/}
  InchAsMM = 254 / 10;

  {@const MMAsPixels - Fator de convers�o de mil�metros para pixels de tela.
   @links ScreenPPI, InchAsMM. :/}
  MMAsPixels = ScreenPPI / InchAsMM;

  MaxPageNo = 999999;

  ReportFileExt = '.rpf';

  { constantes para exibi��o na inicializa��o e no sobre do delphi a partir da vers�o 2009 }
  cRLSobreDialogoTitulo = 'FortesReport Community Edition';
  cRLSobreTitulo = 'FortesReport Community Edition VCL';
  cRLSobreDescricao = 'FortesReport Community Edition VCL ' + sLineBreak +
                      'http://www.fortesreport.com.br' + sLineBreak +
                      'https://github.com/fortesinformatica/fortesreport-ce' + sLineBreak +
                      'Componentes para Gera��o de Relat�rios' + sLineBreak +
                      'Lesser General Public License version 2.0';
  cRLSobreLicencaStatus = 'LGPLv2';
  
  {****                                  *}	

const
  CS_CopyrightStr = 'Copyright � 1999-2016 Fortes Inform�tica';
  CS_ProductTitleStr = 'FortesReport Community Edition';
  CS_URLStr = 'http://www.fortesreport.com.br';
  CS_AuthorNameStr = 'Ronaldo Moreira';
  CS_Version = '4.0';

type
  TRLLocaleStrings = record
    {@var LocaleStrings.LS_PrintingInProgressStr - Vari�vel de internacionaliza��o para "Imprimindo o relat�rio..." :/}
    LS_PrintingInProgressStr: string;
    {@var LS_FilterInProgressStr - Vari�vel de internacionaliza��o para "Salvando o relat�rio..." :/}
    LS_FilterInProgressStr: string;
    {@var LS_PreparingReportStr - Vari�vel de internacionaliza��o para "Preparando o relat�rio..." :/}
    LS_PreparingReportStr: string;
    {@var LS_PrinterNotFoundStr - Vari�vel de internacionaliza��o para "Nenhuma impressora encontrada" :/}
    LS_PrinterNotFoundStr: string;
    {@var LS_NoPrinterSelected - Vari�vel de internacionaliza��o para "Nenhuma impressora selecionada" :/}
    LS_NoPrinterSelected: string;
    {@var LS_NoPathToPrinterStr - Vari�vel de internacionaliza��o para "Caminho inv�lido para a impressora" :/}
    LS_NoPathToPrinterStr: string;
    {@var LS_LoadDefaultConfigStr - Vari�vel de internacionaliza��o para "Ser� carregada a configura��o padr�o" :/}
    LS_LoadDefaultConfigStr: string;
    {@var LS_PrinterDriverErrorStr - Vari�vel de internacionaliza��o para "Erro no driver da impressora" :/}
    LS_PrinterDriverErrorStr: string;
    {@var LS_PageStr - Vari�vel de internacionaliza��o para "P�gina" :/}
    LS_PageStr: string;
    {@var LS_PrepareErrorStr - Vari�vel de internacionaliza��o para "Erro durante a prepara��o do relat�rio" :/}
    LS_PrepareErrorStr: string;
    {@var LS_PageBreakStr - Vari�vel de internacionaliza��o para "Continua..." :/}
    LS_PageBreakStr: string;
    {@var LS_PageMendStr - Vari�vel de internacionaliza��o para "Continua��o" :/}
    LS_PageMendStr: string;
    {@var LS_ReportEndStr - Vari�vel de internacionaliza��o para "Fim" :/}
    LS_ReportEndStr: string;
    {@var LS_FileExists - Translation variable for "File already exists. Overwrite?". :/}
    LS_FileExists: string;
    {@var LS_FileNotFoundStr - Vari�vel de internacionaliza��o para "Arquivo n�o encontrado" :/}
    LS_FileNotFoundStr: string;
    {@var LS_FileNameIsEmpty - Vari�vel de internacionaliza��o para "Nome do arquivo n�o especificado" :/}
    LS_FileNameIsEmpty: string;
    {@var LS_FileNameStr - Vari�vel de internacionaliza��o para "Nome do arquivo" :/}
    LS_FileNameStr: string;
    {@var LS_AllFileTypesStr - Vari�vel de internacionaliza��o para "Todos os arquivos" :/}
    LS_AllFileTypesStr: string;
    {@var LS_LoadReportStr - Vari�vel de internacionaliza��o para "Carregar relat�rio" :/}
    LS_LoadReportStr: string;
    {@var LS_NotFoundStr - Vari�vel de internacionaliza��o para "N�o encontrado" :/}
    LS_NotFoundStr: string;
    {@var LS_WaitStr - Vari�vel de internacionaliza��o para "Aguarde..." :/}
    LS_WaitStr: string;
    {@var LS_FinishedStr - Vari�vel de internacionaliza��o para "Conclu�do" :/}
    LS_FinishedStr: string;
    {@var LS_CancelStr - Vari�vel de internacionaliza��o para "Cancelar" :/}
    LS_CancelStr: string;
    {@var LS_CloseStr - Vari�vel de internacionaliza��o para "Fechar" :/}
    LS_CloseStr: string;
    {@var LS_SaveStr - Vari�vel de internacionaliza��o para "Salvar" :/}
    LS_SaveStr: string;
    {@var LS_SendStr - Vari�vel de internacionaliza��o para "Enviar" :/}
    LS_SendStr: string;
    {@var LS_PrintStr - Vari�vel de internacionaliza��o para "Imprimir" :/}
    LS_PrintStr: string;
    {@var LS_AboutTheStr - Vari�vel de internacionaliza��o para "Sobre o" :/}
    LS_AboutTheStr: string;
    {@var LS_PreviewStr - Vari�vel de internacionaliza��o para "Pr�-visualiza��o" :/}
    LS_PreviewStr: string;
    {@var LS_OfStr - Vari�vel de internacionaliza��o para "de" :/}
    LS_OfStr: string;
    {@var LS_ZoomStr - Vari�vel de internacionaliza��o para "Zoom" :/}
    LS_ZoomStr: string;
    {@var LS_FirstPageStr - Vari�vel de internacionaliza��o para "Primeira p�gina" :/}
    LS_FirstPageStr: string;
    {@var LS_PriorPageStr - Vari�vel de internacionaliza��o para "P�gina anterior" :/}
    LS_PriorPageStr: string;
    {@var LS_NextPageStr - Vari�vel de internacionaliza��o para "Pr�xima p�gina" :/}
    LS_NextPageStr: string;
    {@var LS_LastPageStr - Vari�vel de internacionaliza��o para "�ltima p�gina" :/}
    LS_LastPageStr: string;
    {@var LS_EntirePageStr - Vari�vel de internacionaliza��o para "P�gina inteira" :/}
    LS_EntirePageStr: string;
    {@var LS_EntireWidthStr - Vari�vel de internacionaliza��o para "Largura da p�gina" :/}
    LS_EntireWidthStr: string;
    {@var LS_MultiplePagesStr - Vari�vel de internacionaliza��o para "V�rias p�ginas" :/}
    LS_MultiplePagesStr: string;
    {@var LS_ConfigPrinterStr - Vari�vel de internacionaliza��o para "Configurar impressora" :/}
    LS_ConfigPrinterStr: string;
    {@var LS_SaveToFileStr - Vari�vel de internacionaliza��o para "Salvar em disco" :/}
    LS_SaveToFileStr: string;
    {@var LS_SendToStr - Vari�vel de internacionaliza��o para "Enviar para" :/}
    LS_SendToStr: string;
    {@var LS_PrinterStr - Vari�vel de internacionaliza��o para "Impressora" :/}
    LS_PrinterStr: string;
    {@var LS_NameStr - Vari�vel de internacionaliza��o para "Nome" :/}
    LS_NameStr: string;
    {@var LS_PrintToFileStr - Vari�vel de internacionaliza��o para "Imprimir em arquivo" :/}
    LS_PrintToFileStr: string;
    {@var LS_PrintInBackgroundStr - Vari�vel de internacionaliza��o para "Imprimir em segundo plano" :/}
    LS_PrintInBackgroundStr: string;
    {@var LS_OptionsStr - Vari�vel de internacionaliza��o para "Op��es" de filtragem. :/}
    LS_OptionsStr: string;
    {@var LS_SaveInBackground - Vari�vel de internacionaliza��o para "Salvar em segundo plano" :/}
    LS_SaveInBackground: string;
    {@var LS_PageRangeStr - Vari�vel de internacionaliza��o para "Intervalo de p�ginas" :/}
    LS_PageRangeStr: string;
    {@var LS_CopyAsImageStr - Vari�vel de internacionaliza��o para "Copiar como Bitmap" :/}
    LS_CopyAsImageStr: string;
    {@var LS_CopyAsMetafile - Vari�vel de internacionaliza��o para "Copiar como Metafile" :/}
    LS_CopyAsMetafileStr: string;
    {@var LS_RangeFromStr - Vari�vel de internacionaliza��o para "de" :/}
    LS_RangeFromStr: string;
    {@var LS_RangeToStr - Vari�vel de internacionaliza��o para "at�" :/}
    LS_RangeToStr: string;
    {@var LS_AllStr - Vari�vel de internacionaliza��o para "Tudo" :/}
    LS_AllStr: string;
    {@var LS_PagesStr - Vari�vel de internacionaliza��o para "P�ginas" :/}
    LS_PagesStr: string;
    {@var LS_SelectionStr - Vari�vel de internacionaliza��o para "Sele��o" :/}
    LS_SelectionStr: string;
    {@var LS_CopiesStr - Vari�vel de internacionaliza��o para "C�pias" :/}
    LS_CopiesStr: string;
    {@var LS_NumberOfCopiesStr - Vari�vel de internacionaliza��o para "N�mero de c�pias" :/}
    LS_NumberOfCopiesStr: string;
    {@var LS_OkStr - Vari�vel de internacionaliza��o para "Ok" :/}
    LS_OkStr: string;
    {@var LS_DivideScreenStr - Vari�vel de internacionaliza��o para "Dividir a tela" :/}
    LS_DivideScreenStr: string;
    {@var LS_InvalidNameStr - Vari�vel de internacionaliza��o para "Nome inv�lido" :/}
    LS_InvalidNameStr: string;
    {@var LS_DuplicateNameStr - Vari�vel de internacionaliza��o para "Nome j� utilizado" :/}
    LS_DuplicateNameStr: string;
    {@var LS_UseFilterStr - Vari�vel de internacionaliza��o para "Usar Filtro" :/}
    LS_UseFilterStr: string;
    {@var LS_WebPageStr - Vari�vel de internacionaliza��o para "P�gina da Web" :/}
    LS_WebPageStr: string;
    {@var LS_RichFormatStr - Vari�vel de internacionaliza��o para "Formato RichText" :/}
    LS_RichFormatStr: string;
    {@var LS_PDFFormatStr - Vari�vel de internacionaliza��o para "Documento PDF" :/}
    LS_PDFFormatStr: string;
    {@var LS_XLSFormatStr97-2013 - Vari�vel de internacionaliza��o para "Planilha Excel 97-2013" :/}
    LS_XLSFormatStr97_2013: string;
    {@var LS_XLSFormatStr - Vari�vel de internacionaliza��o para "Planilha Excel" :/}
    LS_XLSFormatStr: string;
    {@var LS_AtStr - Vari�vel de internacionaliza��o para "em" :/}
    LS_AtStr: string;
    {@var LS_FormStr - Vari�vel de internacionaliza��o para "Formul�rio" :/}
    LS_FormStr: string;
    {@var LS_DefaultStr - Vari�vel de internacionaliza��o para "Padr�o" :/}
    LS_DefaultStr: string;
    {@var LS_ZoomInStr - Vari�vel de internacionaliza��o para "Aumentar o zoom" :/}
    LS_ZoomInStr: string;
    {@var LS_ZoomOutStr - Vari�vel de internacionaliza��o para "Diminuir o zoom" :/}
    LS_ZoomOutStr: string;
    {@var LS_CopyStr - Vari�vel de internacionaliza��o para "Copiar" :/}
    LS_CopyStr: string;
    {@var LS_EditStr - Vari�vel de internacionaliza��o para "Editar" :/}
    LS_EditStr: string;
    {@var LS_FindCaptionStr - Vari�vel de internacionaliza��o para "Procurar" :/}
    LS_FindCaptionStr: string;
    {@var LS_TextToFindStr - Vari�vel de internacionaliza��o para "Te&xto" :/}
    LS_TextToFindStr: string;
    {@var LS_FindNextStr - &Vari�vel de internacionaliza��o para "Pr�xima" :/}
    LS_FindNextStr: string;
    {@var LS_WholeWordsStr - Vari�vel de internacionaliza��o para "Palavras &inteiras" :/}
    LS_WholeWordsStr: string;
    {@var LS_MatchCaseStr - Vari�vel de internacionaliza��o para "Diferenciar &mai�sculas de min�sculas" :/}
    LS_MatchCaseStr: string;
    {@var LS_DirectionUpStr - Vari�vel de internacionaliza��o para "A&cima" :/}
    LS_DirectionUpStr: string;
    {@var LS_DirectionDownStr - Vari�vel de internacionaliza��o para "A&baixo" :/}
    LS_DirectionDownStr: string;
    {@var LS_DirectionCaptionStr - Vari�vel de internacionaliza��o para "Dire��o" :/}
    LS_DirectionCaptionStr: string;
    {@var LS_ColumnsStr - Vari�vel de internacionaliza��o para "Colunas". :/}
    LS_ColumnsStr: string;
    {@var LS_SetupStr - Vari�vel de internacionaliza��o para "Configura��o". :/}
    LS_SetupStr: string;
    {@var LS_FontSizeError - Vari�vel de internacionaliza��o para "Erro no c�lculo das fontes". :/}
    LS_FontSizeError: string;
    {@var LS_OddPages - Vari�vel de internacionaliza��o para "�mpares". :/}
    LS_OddPages: string;
    {@var LS_EvenPages - Vari�vel de internacionaliza��o para "Pares". :/}
    LS_EvenPages: string;
    {@var LS_OddPagesOnly - Vari�vel de internacionaliza��o para "�mpares somente". :/}
    LS_OddPagesOnly: string;
    {@var LS_EvenPagesOnly - Vari�vel de internacionaliza��o para "Pares somente". :/}
    LS_EvenPagesOnly: string;
    {@var LS_AllOddAndEven - Vari�vel de internacionaliza��o para "Todas". :/}
    LS_AllOddAndEven: string;
    {@var LS_PrintDialogError - Vari�vel de internacionaliza��o para "Problemas com o di�logo da impressora". :/}
    LS_PrintDialogError: string;
    {@var LS_PageSelectionHint - Vari�vel de internacionaliza��o para "Separe com ponto-e-v�rgula os n�meros ou intervalos de p�ginas a imprimir. Ex.: 1;3;5-12;4". :/}
    LS_PageSelectionHint: string;
    {@var LS_DefaultJobTitle - Vari�vel de internacionaliza��o para "Relat�rio %s". :/}
    LS_DefaultJobTitle: string;
    {@var LS_ZoomHint - Vari�vel de internacionaliza��o para "Diminuir o zoom" :/}
    LS_ZoomHint: string;
    {@var Ls_Aplicar - Vari�vel de internacionaliza��o para "Aplicar". :/}
    Ls_Aplicar: String;
    {@var Ls_Propriedades - Vari�vel de internacionaliza��o para "Propriedades". :/}
    Ls_Propriedades: string;
    {@var Ls_Salvar_Como - Vari�vel de internacionaliza��o para "Salvar como". :/}
    Ls_Salvar_Como: string;
    {@var LS_FileCorrupted - Vari�vel de internacionaliza��o para "Arquivo corrompido". :/}
    LS_FileCorrupted: String;
    {@var LS_FileCorruptedHeader - Translation variable for "Corrupted file header "%s"!". :/}
    LS_FileCorruptedHeader: string;
    {@var LS_FileVersion - Vari�vel de internacionaliza��o para "Vers�o de Arquivo inv�lido". :/}
    LS_FileVersion: String;
    {@var Ls_PageSetings - Vari�vel de internacionaliza��o para "Configura��o da P�gina". :/}
    LS_PageSettings: String;
    {@var Ls_Page_margins - Vari�vel de internacionaliza��o para "Margem da P�gina". :/}
    LS_PageMargins: String;
    {@var Ls_PageMarginsTop - Vari�vel de internacionaliza��o para "Margem Superior". :/}
    LS_PageMarginsTop: String;
    {@var Ls_PageMarginsBottom - Vari�vel de internacionaliza��o para "Margem Inferior". :/}
    LS_PageMarginsBottom: String;
    {@var LS_PageMarginsRigth - Vari�vel de internacionaliza��o para "Margem direita". :/}
    LS_PageMarginsRigth: String;
    {@var Ls_PageLeftBottom - Vari�vel de internacionaliza��o para "Margem equerda". :/}
    LS_PageMarginsLeft: String;
    {@var LS_PageMarginsPaper - Vari�vel de internacionaliza��o para "Margem do Papel". :/}
    LS_PageMarginsPaper: String;
    {@var LS_PagePaper - Vari�vel de internacionaliza��o para "Papel". :/}
    LS_PagePaper: String;
    {@var LS_PaperSize - Vari�vel de internacionaliza��o para "Tamanho do Papel". :/}
    LS_PaperSize: String;
    {@var LS_PaperWidth - Vari�vel de internacionaliza��o para "Largura do Papel". :/}
    LS_PaperSizeWidth: String;
    {@var LS_PaperSizeHeigth - Vari�vel de internacionaliza��o para "Altura do Papel". :/}
    LS_PaperSizeHeigth: String;
    {@var LS_PaperOrientation - Vari�vel de internacionaliza��o para "Orienta��o do Papel". :/}
    LS_PaperOrientation: String;
    {@var LS_PaperOrientationLandscape - Vari�vel de internacionaliza��o para "Orienta��o da p�gina em retrato". :/}
    LS_PaperOrientationLandscape: String;
    {@var LS_PaperOrientationPortrait - Vari�vel de internacionaliza��o para "Orienta��o da p�gina em paisagem". :/}
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
    LS_PrintingInProgressStr: 'Imprimindo o relat�rio...';
    LS_FilterInProgressStr: 'Salvando o relat�rio...';
    LS_PreparingReportStr: 'Preparando o relat�rio...';
    LS_PrinterNotFoundStr: 'Nenhuma impressora encontrada';
    LS_NoPrinterSelected: 'Nenhuma impressora selecionada.';
    LS_NoPathToPrinterStr: 'Caminho inv�lido para a impressora';
    LS_LoadDefaultConfigStr: 'Ser� carregada a configura��o padr�o';
    LS_PrinterDriverErrorStr: 'Erro no driver da impressora';
    LS_PageStr: 'P�gina';
    LS_PrepareErrorStr: 'Erro durante a prepara��o do relat�rio';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continua��o';
    LS_ReportEndStr: 'Fim';
    LS_FileExists: 'Arquivo j� existe. Sobreescrever?';
    LS_FileNotFoundStr: 'Arquivo n�o encontrado';
    LS_FileNameIsEmpty: 'Nome do arquivo n�o especificado';
    LS_FileNameStr: 'Nome do arquivo';
    LS_AllFileTypesStr: 'Todos os arquivos';
    LS_LoadReportStr: 'Carregar relat�rio';
    LS_NotFoundStr: 'N�o encontrado';
    LS_WaitStr: 'Aguarde...';
    LS_FinishedStr: 'Conclu�do';
    LS_CancelStr: 'Cancelar';
    LS_CloseStr: 'Fechar';
    LS_SaveStr: 'Salvar';
    LS_SendStr: 'Enviar';
    LS_PrintStr: 'Imprimir';
    LS_AboutTheStr: 'Sobre o';
    LS_PreviewStr: 'Pr�-visualiza��o';
    LS_OfStr: 'de';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Primeira p�gina';
    LS_PriorPageStr: 'P�gina anterior';
    LS_NextPageStr: 'Pr�xima p�gina';
    LS_LastPageStr: '�ltima p�gina';
    LS_EntirePageStr: 'P�gina inteira';
    LS_EntireWidthStr: 'Largura da p�gina';
    LS_MultiplePagesStr: 'V�rias p�ginas';
    LS_ConfigPrinterStr: 'Configurar impressora';
    LS_SaveToFileStr: 'Salvar em disco';
    LS_SendToStr: 'Enviar para';
    LS_PrinterStr: 'Impressora';
    LS_NameStr: 'Nome';
    LS_PrintToFileStr: 'Imprimir em arquivo';
    LS_PrintInBackgroundStr: 'Imprimir em segundo plano';
    LS_OptionsStr: 'Op��es';
    LS_SaveInBackground: 'Salvar em segundo plano';
    LS_PageRangeStr: 'Intervalo de p�ginas';
    LS_CopyAsImageStr: 'Copiar como Bitmap';
    LS_CopyAsMetafileStr: 'Copiar como Metafile';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'at�';
    LS_AllStr: 'Tudo';
    LS_PagesStr: 'P�ginas';
    LS_SelectionStr: 'Sele��o';
    LS_CopiesStr: 'C�pias';
    LS_NumberOfCopiesStr: 'N�mero de c�pias';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Dividir a tela';
    LS_InvalidNameStr: 'Nome inv�lido';
    LS_DuplicateNameStr: 'Nome j� utilizado';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'P�gina da Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr97_2013: 'Planilha Excel 97-2013';
    LS_XLSFormatStr: 'Planilha Excel';
    LS_AtStr: 'em';
    LS_FormStr: 'Formul�rio';
    LS_DefaultStr: 'Padr�o';
    LS_ZoomInStr: 'Aumentar o zoom';
    LS_ZoomOutStr: 'Diminuir o zoom';
    LS_CopyStr: 'Copiar';
    LS_EditStr: 'Editar';
    LS_FindCaptionStr: 'Procurar';
    LS_TextToFindStr: 'Te&xto';
    LS_FindNextStr: '&Pr�xima';
    LS_WholeWordsStr: 'Palavras &inteiras';
    LS_MatchCaseStr: 'Diferenciar &mai�sculas de min�sculas';
    LS_DirectionUpStr: 'A&cima';
    LS_DirectionDownStr: 'A&baixo';
    LS_DirectionCaptionStr: 'Dire��o';
    LS_ColumnsStr: 'Colunas';
    LS_SetupStr: 'Configura��o';
    LS_FontSizeError: 'Erro no c�lculo das fontes';
    LS_OddPages: '�mpares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'Somente �mpares';
    LS_EvenPagesOnly: 'Somente Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas com o di�logo da impressora';
    LS_PageSelectionHint: 'Separe com ponto-e-v�rgula os n�meros ou intervalos de p�ginas a imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Relat�rio "%s"';
    LS_ZoomHint: 'Voc� tamb�m pode aumentar ou reduzir o zoom do relat�rio' + sLineBreak + 'pressionando "Ctrl" e usando a rolagem do mouse.';
    LS_Aplicar: 'Aplicar';
    LS_Propriedades: 'Propriedades';
    LS_Salvar_Como: 'Salvar Como';
    LS_FileCorrupted: 'Arquivo Corrompido';
    LS_FileCorruptedHeader: 'Cabe�alho de arquivo corrompido "%s"!';
    LS_FileVersion: 'Vers�o de arquivo inv�lida %d!';
    LS_PageSettings: 'Configura��es da p�gina';
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
    LS_PaperOrientation: 'Orienta � �o';
    LS_PaperOrientationLandscape: 'Paisagem';
    LS_PaperOrientationPortrait: 'Retrato';
    LS_Duplex: 'Impress�o frente e verso';
    LS_OnlyOneInstance: 'Apenas uma inst�ncia de %s � permitida!';
    LS_NotImplemented: '%s ainda n�o foi implementado para esta plataforma!';
    LS_NoHandle: 'Handle n�o dispon�vel!';

    LS_LastFooMsg: '';
  );

var
  FrenchStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Pr�paration du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouv�e';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Chemin d''imprimante non valide';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la pr�paration du rapport';
    LS_PageBreakStr: 'Suite...';
    LS_PageMendStr: 'A suivre';
    LS_ReportEndStr: 'Fin';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'Fichier non trouv�';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Nom de Fichier';
    LS_AllFileTypesStr: 'Tous les fichiers';
    LS_LoadReportStr: 'Ouvrir rapport';
    LS_NotFoundStr: 'Non trouv�';
    LS_WaitStr: 'Patientez...';
    LS_FinishedStr: 'Fini';
    LS_CancelStr: 'Annuler';
    LS_CloseStr: 'Fermer';
    LS_SaveStr: 'Sauver';
    LS_SendStr: 'Envoyer';
    LS_PrintStr: 'Imprimer';
    LS_AboutTheStr: 'A propos de';
    LS_PreviewStr: 'Aper�u avant impression';
    LS_OfStr: 'de';
    LS_ZoomStr: 'Zoom';
    LS_FirstPageStr: 'Premi�re page';
    LS_PriorPageStr: 'Page pr�c�dente';
    LS_NextPageStr: 'Page suivante';
    LS_LastPageStr: 'Derni�re page';
    LS_EntirePageStr: 'Page enti�re';
    LS_EntireWidthStr: 'Pleine largeur';
    LS_MultiplePagesStr: 'Plusieurs pages';
    LS_ConfigPrinterStr: 'Configuration de l''imprimante';
    LS_SaveToFileStr: 'Enregistrer dans un fichier';
    LS_SendToStr: 'Envoyer �...';
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
    LS_RangeToStr: '�';
    LS_AllStr: 'Tout';
    LS_PagesStr: 'Pages';
    LS_SelectionStr: 'S�lection';
    LS_CopiesStr: 'Copies';
    LS_NumberOfCopiesStr: 'Nombre de copies';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: '�cran divis�';
    LS_InvalidNameStr: 'Nom invalide';
    LS_DuplicateNameStr: 'Nom r�p�t�';
    LS_UseFilterStr: 'Utiliser un filtre';
    LS_WebPageStr: 'Page Web';
    LS_RichFormatStr: 'Format RichText';
    LS_PDFFormatStr: 'Document en PDF';
    LS_XLSFormatStr97_2013: 'Feuille de calcul Excel 97-2013';
    LS_XLSFormatStr: 'Feuille de calcul Excel';
    LS_AtStr: '�';
    LS_FormStr: 'Formulaire';
    LS_DefaultStr: 'D�faut';
    LS_ZoomInStr: 'Grandir zoom';
    LS_ZoomOutStr: 'R�duire zoom';
    LS_CopyStr: 'Copier';
    LS_EditStr: '�diter';
    LS_FindCaptionStr: 'Trouvaille';
    LS_TextToFindStr: 'Te&xte';
    LS_FindNextStr: 'A&pr�s';
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
    LS_PrintDialogError: 'Erreur dans la bo�te de dialogue de l''imprimante';
    LS_PageSelectionHint: 'S�par� des num�ros de page ou des intervalles avec ";". p.ex.: 1;3;5-12;4';
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
    LS_PrintingInProgressStr: 'Impresi�n en marcha...';
    LS_FilterInProgressStr: 'Guardando el informe...';
    LS_PreparingReportStr: 'Preparaci�n del informe...';
    LS_PrinterNotFoundStr: 'Impresora no encontrada';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: 'Camino de la impresora no es v�lido';
    LS_LoadDefaultConfigStr: 'Cargar la configuraci�n est�ndar';
    LS_PrinterDriverErrorStr: 'Error en driver de la impresora';
    LS_PageStr: 'P�gina';
    LS_PrepareErrorStr: 'Un error ocurri� mientras se preparaba el informe';
    LS_PageBreakStr: 'Contin�a...';
    LS_PageMendStr: 'Continuaci�n';
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
    LS_FirstPageStr: 'Primera p�gina';
    LS_PriorPageStr: 'P�gina anterior';
    LS_NextPageStr: 'P�gina siguiente';
    LS_LastPageStr: '�ltima p�gina';
    LS_EntirePageStr: 'P�gina entera';
    LS_EntireWidthStr: 'Ancho completo';
    LS_MultiplePagesStr: 'Varias p�ginas';
    LS_ConfigPrinterStr: 'Configurar la impresora';
    LS_SaveToFileStr: 'Guardar en un archivo';
    LS_SendToStr: 'Env�ar a';
    LS_PrinterStr: 'Impresora';
    LS_NameStr: 'Nombre';
    LS_PrintToFileStr: 'Imprimir a un archivo';
    LS_PrintInBackgroundStr: 'Imprimir en background';
    LS_OptionsStr: 'Opciones';
    LS_SaveInBackground: 'Guardar en background';
    LS_PageRangeStr: 'Intervalo de p�ginas';
    LS_CopyAsImageStr: 'Copiar como imagen';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'a';
    LS_AllStr: 'Todas';
    LS_PagesStr: 'P�ginas';
    LS_SelectionStr: 'Selecci�n';
    LS_CopiesStr: 'Copias';
    LS_NumberOfCopiesStr: 'N�mero de copias';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Dividir la pantalla';
    LS_InvalidNameStr: 'Nombre inv�lido';
    LS_DuplicateNameStr: 'Nombre ya en uso';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'P�gina Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr97_2013: 'Hoja de c�lculo Excel 97-2013';
    LS_XLSFormatStr: 'Hoja de c�lculo Excel';
    LS_AtStr: 'en';
    LS_FormStr: 'Formulario';
    LS_DefaultStr: 'Est�ndar';
    LS_ZoomInStr: 'Aumentar zoom';
    LS_ZoomOutStr: 'Disminuir zoom';
    LS_CopyStr: 'Copiar';
    LS_EditStr: 'Editar';
    LS_FindCaptionStr: 'Buscar';
    LS_TextToFindStr: 'Te&xto';
    LS_FindNextStr: '&Siguiente';
    LS_WholeWordsStr: 'Palabras &completas s�lamente';
    LS_MatchCaseStr: 'Diferenciar &may�sculas y min�sculas';
    LS_DirectionUpStr: 'En&cima';
    LS_DirectionDownStr: '&Abajo';
    LS_DirectionCaptionStr: 'Direcci�n';
    LS_ColumnsStr: 'Cols';
    LS_SetupStr: 'Configuraci�n...';
    LS_FontSizeError: 'Error en el c�lculo de las fuentes';
    LS_OddPages: 'Impares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'S�lo Impares';
    LS_EvenPagesOnly: 'S�lo Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas con el di�logo de la impresora';
    LS_PageSelectionHint: 'Separar con ";" los n�meros o rangos de p�ginas para imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Impresi�n "%s"';
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
    LS_MultiplePagesStr: 'Pi� pagine';
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
    LS_DuplicateNameStr: 'Nome gi� presente';
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
    LS_DirectionDownStr: 'In gi�';
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
    LS_OnlyOneInstance: 'Una sola istanza di %s � ammessa!';
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
    LS_NoPathToPrinterStr: 'Ung�ltiger Druckerpfad';
    LS_LoadDefaultConfigStr: 'Laden Sie Standardkonfiguration';
    LS_PrinterDriverErrorStr: 'Druckerfahrer Fehler';
    LS_PageStr: 'Seite';
    LS_PrepareErrorStr: 'Fehler, w�hrend das Vorbereiten von Bericht';
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
    LS_AboutTheStr: '�ber';
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
    LS_OptionsStr: 'M�glichkeiten';
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
    LS_InvalidNameStr: 'Ung�ltiger Name';
    LS_DuplicateNameStr: 'Nennen Sie schon im Gebrauch';
    LS_UseFilterStr: 'Verwendungsfilter';
    LS_WebPageStr: 'Webseite';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
    LS_XLSFormatStr97_2013: 'Zeichnen Sie Tabelle aus 97-2013';
    LS_XLSFormatStr: 'Zeichnen Sie Tabelle aus';
    LS_AtStr: 'bei';
    LS_FormStr: 'Form';
    LS_DefaultStr: 'Vers�umnis';
    LS_ZoomInStr: 'Zunahmegummilinse';
    LS_ZoomOutStr: 'Abnahmengummilinse';
    LS_CopyStr: 'Kopie';
    LS_EditStr: 'Bearbeiten Sie';
    LS_FindCaptionStr: 'Fund';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Finden Sie danach';
    LS_WholeWordsStr: '&Ganze W�rter nur';
    LS_MatchCaseStr: '&Passen Sie Fall zusammen';
    LS_DirectionUpStr: '&Auf';
    LS_DirectionDownStr: '&Entlang';
    LS_DirectionCaptionStr: 'Richtung';
    LS_ColumnsStr: 'S�ule';
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
    LS_PrintingInProgressStr: 'Печать...';
    LS_FilterInProgressStr: 'Сохранение отчета...';
    LS_PreparingReportStr: 'Подготовка отчета...';
    LS_PrinterNotFoundStr: 'Принтер не найден';
    LS_NoPrinterSelected: 'No printer selected.';
    LS_NoPathToPrinterStr: '�?еправильный путь принтера';
    LS_LoadDefaultConfigStr: 'Загрузить конфигураци�? по умолчанию';
    LS_PrinterDriverErrorStr: 'Ошибка драйвера принтера';
    LS_PageStr: 'Страница';
    LS_PrepareErrorStr: 'Ошибка при подготовке отчета';
    LS_PageBreakStr: 'Продолжает�?�?...';
    LS_PageMendStr: 'Продолжение';
    LS_ReportEndStr: 'Конец';
    LS_FileExists: 'File already exists. Overwrite?';
    LS_FileNotFoundStr: 'Файл не найден';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'Им�? файла';
    LS_AllFileTypesStr: 'В�?е файлы';
    LS_LoadReportStr: 'Загрузить отчет';
    LS_NotFoundStr: '�?е найден';
    LS_WaitStr: 'Подождите...';
    LS_FinishedStr: 'Завершено';
    LS_CancelStr: 'Отмена';
    LS_CloseStr: 'Закрыть';
    LS_SaveStr: 'Сохранить';
    LS_SendStr: 'Отправить';
    LS_PrintStr: 'Печать';
    LS_AboutTheStr: 'О Программе';
    LS_PreviewStr: 'Предварительный про�?мотр';
    LS_OfStr: 'из';
    LS_ZoomStr: 'Маштаб';
    LS_FirstPageStr: 'Перва�? �?траница';
    LS_PriorPageStr: 'Предыдуща�? �?траница';
    LS_NextPageStr: 'Следующа�? �?траница';
    LS_LastPageStr: 'По�?ледн�?�? �?траница';
    LS_EntirePageStr: 'В�?�? �?траница';
    LS_EntireWidthStr: 'В�?�? ширина';
    LS_MultiplePagesStr: '�?е�?колько �?траниц';
    LS_ConfigPrinterStr: '�?а�?тройка принтера';
    LS_SaveToFileStr: 'Сохранить в файл';
    LS_SendToStr: 'Отправить в';
    LS_PrinterStr: 'Принтер';
    LS_NameStr: 'Им�?';
    LS_PrintToFileStr: 'Печать в файл';
    LS_PrintInBackgroundStr: 'Печать в фоне';
    LS_OptionsStr: '�?а�?��ОКки';
    LS_SaveInBackground: 'Сохранение в фоне';
    LS_PageRangeStr: 'Диапазон печати';
    LS_CopyAsImageStr: 'Copy as Bitmap';
    LS_CopyAsMetafileStr: 'Copy as Metafile';
    LS_RangeFromStr: '�?';
    LS_RangeToStr: 'по';
    LS_AllStr: 'В�?е';
    LS_PagesStr: 'Страницы';
    LS_SelectionStr: 'Выделение';
    LS_CopiesStr: 'Копии';
    LS_NumberOfCopiesStr: 'Кол-во копий';
    LS_OkStr: 'Хорошо';
    LS_DivideScreenStr: 'Разделить �?кран';
    LS_InvalidNameStr: '�?еправильное им�?';
    LS_DuplicateNameStr: 'Им�? уже и�?пользует�?�?';
    LS_UseFilterStr: 'Фильтр';
    LS_WebPageStr: 'Веб �?траница';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Документ';
    LS_XLSFormatStr97_2013: '';
    LS_XLSFormatStr: 'Таблица Excel';
    LS_AtStr: 'в';
    LS_FormStr: 'Форма';
    LS_DefaultStr: 'По умолчанию';
    LS_ZoomInStr: 'Увеличить';
    LS_ZoomOutStr: 'Уменьшить';
    LS_CopyStr: 'Копировать';
    LS_EditStr: 'Редактировать';
    LS_FindCaptionStr: '�?айти';
    LS_TextToFindStr: 'Те&к�?т';
    LS_FindNextStr: '�?айти &дальше';
    LS_WholeWordsStr: '&Только �?лово целиком';
    LS_MatchCaseStr: '&Учитывать реги�?тр';
    LS_DirectionUpStr: '&Вверх';
    LS_DirectionDownStr: 'В&низ';
    LS_DirectionCaptionStr: '�?аправление';
    LS_ColumnsStr: 'Столбцы';
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
    LS_Aplicar: 'Применить';
    LS_Propriedades: '�?а�?тройки';
    LS_Salvar_Como: 'Сохранить как';
    LS_FileCorrupted: 'Файл поврежден!';
    LS_FileCorruptedHeader: 'Corrupted file header "%s"!';
    LS_FileVersion: '�?еправильна�? вер�?и�? файла %d!';
    LS_PageSettings: '�?а�?тройка �?траницы';
    LS_PageMargins: 'Пол�?';
    LS_PageMarginsTop: 'Верх';
    LS_PageMarginsBottom: '�?их';
    LS_PageMarginsRigth: 'Право';
    LS_PageMarginsLeft: 'Лево';
    LS_PageMarginsPaper: '';
    LS_PagePaper: 'Бумага';
    LS_PaperSize: 'Размер бумаги';
    LS_PaperSizeWidth: 'Ширина';
    LS_PaperSizeHeigth: 'Вы�?ота';
    LS_PaperOrientation: 'Ориентаци�?';
    LS_PaperOrientationLandscape: 'Ландшафтна�?';
    LS_PaperOrientationPortrait: 'Портретна�?';
    LS_Duplex: 'Automatic two-sided printing';
    LS_OnlyOneInstance: 'Only one instance of %s is allowed!';
    LS_NotImplemented: '%s is not yet implemented for this platform!';
    LS_NoHandle: 'Handle not available!';

    LS_LastFooMsg: '';
  );

var
  CzechStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Prob�h� tisk...';
    LS_FilterInProgressStr: 'Ukl�d�m report...';
    LS_PreparingReportStr: 'P�ipravuji report...';
    LS_PrinterNotFoundStr: 'Tisk�rna nenalezena';
    LS_NoPrinterSelected: 'Nen� vybr�na ��dn� tisk�rna.';
    LS_NoPathToPrinterStr: 'Chybn� cesta k tisk�rn�';
    LS_LoadDefaultConfigStr: 'Na��st implicitn� nastaven�';
    LS_PrinterDriverErrorStr: 'Chyba ovlada�e tisku';
    LS_PageStr: 'Str�nka';
    LS_PrepareErrorStr: 'Chyba b�hem p��pravy reportu';
    LS_PageBreakStr: 'Pokra�uje...';
    LS_PageMendStr: 'Pokra�ov�n�';
    LS_ReportEndStr: 'Konec';
    LS_FileExists: 'Soubor ji� existuje. P�epsat?';
    LS_FileNotFoundStr: 'Soubor nenalezen';
    LS_FileNameIsEmpty: 'File name not Specified';
    LS_FileNameStr: 'N�zev souboru';
    LS_AllFileTypesStr: 'V�echny soubory';
    LS_LoadReportStr: 'Na��st report';
    LS_NotFoundStr: 'Nenalezeno';
    LS_WaitStr: '�ekejte...';
    LS_FinishedStr: 'Ukon�eno';
    LS_CancelStr: 'Zru�it';
    LS_CloseStr: 'Zav��t';
    LS_SaveStr: 'Ulo�it';
    LS_SendStr: 'Poslat';
    LS_PrintStr: 'Tisk';
    LS_AboutTheStr: 'O aplikaci';
    LS_PreviewStr: 'N�hled';
    LS_OfStr: 'z';
    LS_ZoomStr: 'P�ibl�en�';
    LS_FirstPageStr: 'Prvn� str�nka';
    LS_PriorPageStr: 'P�edchoz� str�nka';
    LS_NextPageStr: 'Dal�� str�nka';
    LS_LastPageStr: 'Posledn� str�nka';
    LS_EntirePageStr: 'Cel� str�nka';
    LS_EntireWidthStr: 'Na ���ku';
    LS_MultiplePagesStr: 'V�ce str�nek';
    LS_ConfigPrinterStr: 'Konfigurace tisk�rny';
    LS_SaveToFileStr: 'Ulo�it do souboru';
    LS_SendToStr: 'Odeslat';
    LS_PrinterStr: 'Tisk�rna';
    LS_NameStr: 'N�zev';
    LS_PrintToFileStr: 'Tisk do souboru';
    LS_PrintInBackgroundStr: 'Tisk na pozad�';
    LS_OptionsStr: 'Volby';
    LS_SaveInBackground: 'Ulo�it na pozad�';
    LS_PageRangeStr: 'Rozsah str�nek';
    LS_CopyAsImageStr: 'Kop�rovat jako bitmapu';
    LS_CopyAsMetafileStr: 'Kop�rovat jako metasoubor';
    LS_RangeFromStr: 'od';
    LS_RangeToStr: 'do';
    LS_AllStr: 'V�echny';
    LS_PagesStr: 'Str�nky';
    LS_SelectionStr: 'V�b�r';
    LS_CopiesStr: 'Kopie';
    LS_NumberOfCopiesStr: 'Po�et kopi�';
    LS_OkStr: 'OK';
    LS_DivideScreenStr: 'Rozd�lit obrazovku';
    LS_InvalidNameStr: 'Chybn� n�zev';
    LS_DuplicateNameStr: 'N�zev se ji� pou��v�';
    LS_UseFilterStr: 'Pou��t filtr';
    LS_WebPageStr: 'Webov� str�nka';
    LS_RichFormatStr: 'Form�t RichText';
    LS_PDFFormatStr: 'PDF dokument';
    LS_XLSFormatStr97_2013: 'Tabulka Excel 97-2013';
    LS_XLSFormatStr: 'Tabulka Excel';
    LS_AtStr: 'na';
    LS_FormStr: 'Formul��';
    LS_DefaultStr: 'Default';
    LS_ZoomInStr: 'P�ibl�it';
    LS_ZoomOutStr: 'Odd�lit';
    LS_CopyStr: 'Kop�rovat';
    LS_EditStr: 'Upravit';
    LS_FindCaptionStr: 'Naj�t';
    LS_TextToFindStr: 'Te&xt';
    LS_FindNextStr: 'Naj�t &dal��';
    LS_WholeWordsStr: '&Pouze cel� slova';
    LS_MatchCaseStr: '&Rozli�ovat velikost p�smen';
    LS_DirectionUpStr: '&Nahoru';
    LS_DirectionDownStr: '&Dol�';
    LS_DirectionCaptionStr: 'Orientace';
    LS_ColumnsStr: 'Sloupce';
    LS_SetupStr: 'Nastaven�...';
    LS_FontSizeError: 'Chyba velikosti textu';
    LS_OddPages: 'Sud�';
    LS_EvenPages: 'Lich�';
    LS_OddPagesOnly: 'Pouze sud� str�nky';
    LS_EvenPagesOnly: 'Pouze lich� str�nky';
    LS_AllOddAndEven: 'V�echny';
    LS_PrintDialogError: 'Probl�m s dialogem tisku';
    LS_PageSelectionHint: 'Odd�len� ��sla str�nek st�edn�kem nebo rozmez� str�nek s poml�kou. tj.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Report "%s"';
    LS_ZoomHint: 'Upravit p�ibl�en�.';
    LS_Aplicar: 'Pou��t';
    LS_Propriedades: 'Nastaven�';
    LS_Salvar_Como: 'Ulo�it jako';
    LS_FileCorrupted: 'Soubor je po�kozen!';
    LS_FileCorruptedHeader: 'Po�kozen� hlavi�ka souboru "%s"!';
    LS_FileVersion: 'Chybn� typ souboru %d!';
    LS_PageSettings: 'Nastaven� str�nky';
    LS_PageMargins: 'Okraje';
    LS_PageMarginsTop: 'Naho�e';
    LS_PageMarginsBottom: 'Dole';
    LS_PageMarginsRigth: 'Vpravo';
    LS_PageMarginsLeft: 'Vlevo';
    LS_PageMarginsPaper: 'Okraje pap�ru';
    LS_PagePaper: 'Pap�r';
    LS_PaperSize: 'Velikost pap�ru';
    LS_PaperSizeWidth: '���ka';
    LS_PaperSizeHeigth: 'V��ka';
    LS_PaperOrientation: 'Orientace';
    LS_PaperOrientationLandscape: 'Na ���ku';
    LS_PaperOrientationPortrait: 'Na v��ku';
    LS_Duplex: 'Automatick� oboustrann� tisk';
    LS_OnlyOneInstance: 'Je povolena pouze jedna instance %s!';
    LS_NotImplemented: '%s nen� implementov�no na t�to platform�!';
    LS_NoHandle: 'Handle nen� dostupn�!';

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
