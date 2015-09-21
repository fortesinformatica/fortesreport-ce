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

unit RLConsts;

interface

uses
  SysUtils, Classes, Dialogs;

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
  cRLSobreDescricao = 'FortesReport Community Edition VCL ' + #13#10 +
                      'http://www.fortesreport.com.br' + #13#10 +
                      'https://github.com/fortesinformatica/fortesreport-ce' + #13#10 +
                      'Componentes para Gera��o de Relat�rios' + #13#10 +
                      'Lesser General Public License version 2.0';
  cRLSobreLicencaStatus = 'LGPLv2';
  
  {****                                  *}	

const
  CS_CopyrightStr = 'Copyright � 1999-2015 Fortes Inform�tica';
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
    {@var LS_FileNotFoundStr - Vari�vel de internacionaliza��o para "Arquivo n�o encontrado" :/}
    LS_FileNotFoundStr: string;
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
    {@var LS_CopyAsImageStr - Vari�vel de internacionaliza��o para "Copiar como imagem" :/}
    LS_CopyAsImageStr: string;
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
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Divide the screen';
    LS_InvalidNameStr: 'Invalid name';
    LS_DuplicateNameStr: 'Name already in use';
    LS_UseFilterStr: 'Use filter';
    LS_WebPageStr: 'Web page';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
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
  );

var
  PortugueseStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Imprimindo o relat�rio...';
    LS_FilterInProgressStr: 'Salvando o relat�rio...';
    LS_PreparingReportStr: 'Preparando o relat�rio...';
    LS_PrinterNotFoundStr: 'Nenhuma impressora encontrada';
    LS_NoPathToPrinterStr: 'Caminho inv�lido para a impressora';
    LS_LoadDefaultConfigStr: 'Ser� carregada a configura��o padr�o';
    LS_PrinterDriverErrorStr: 'Erro no driver da impressora';
    LS_PageStr: 'P�gina';
    LS_PrepareErrorStr: 'Erro durante a prepara��o do relat�rio';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continua��o';
    LS_ReportEndStr: 'Fim';
    LS_FileNotFoundStr: 'Arquivo n�o encontrado';
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
    LS_CopyAsImageStr: 'Copiar como imagem';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'at�';
    LS_AllStr: 'Tudo';
    LS_PagesStr: 'P�ginas';
    LS_SelectionStr: 'Sele��o';
    LS_CopiesStr: 'C�pias';
    LS_NumberOfCopiesStr: 'N�mero de c�pias';
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Dividir a tela';
    LS_InvalidNameStr: 'Nome inv�lido';
    LS_DuplicateNameStr: 'Nome j� utilizado';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'P�gina da Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
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
  );

var
  FrenchStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Pr�paration du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouv�e';
    LS_NoPathToPrinterStr: 'Invalid printer path';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la pr�partaion du rapport';
    LS_PageBreakStr: 'Saut de page...';
    LS_PageMendStr: 'A suivre';
    LS_ReportEndStr: 'Fin';
    LS_FileNotFoundStr: 'Fichier non trouv�';
    LS_FileNameStr: 'Nom de Fichier';
    LS_AllFileTypesStr: 'Tous les fichiers';
    LS_LoadReportStr: 'Ouvrir rapport';
    LS_NotFoundStr: 'Non trouv�';
    LS_WaitStr: 'Patientez...';
    LS_FinishedStr: 'Fini';
    LS_CancelStr: 'Annul�';
    LS_CloseStr: 'Ferm�';
    LS_SaveStr: 'Sauver';
    LS_SendStr: 'Envoyez';
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
    LS_SaveToFileStr: 'Enregistrer sous';
    LS_SendToStr: 'Envoyez �';
    LS_PrinterStr: 'Imprimante';
    LS_NameStr: 'Nom';
    LS_PrintToFileStr: 'Imprimer dans un fichier';
    LS_PrintInBackgroundStr: 'Imprimer dans background';
    LS_OptionsStr: 'Opciones';
    LS_SaveInBackground: 'Enregistrer dans background';
    LS_PageRangeStr: 'Intervalle de pages';
    LS_CopyAsImageStr: 'Copy as image';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: '�';
    LS_AllStr: 'Tout';
    LS_PagesStr: 'Pages';
    LS_SelectionStr: 'S�lection';
    LS_CopiesStr: 'Copies';
    LS_NumberOfCopiesStr: 'Nombre de copies';
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Dividir a tela';
    LS_InvalidNameStr: 'Nom inadmissible';
    LS_DuplicateNameStr: 'Nom reproduit';
    LS_UseFilterStr: 'Use filter';
    LS_WebPageStr: 'Page de Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr: 'Excel tableur';
    LS_AtStr: '�';
    LS_FormStr: 'Formulaire';
    LS_DefaultStr: 'D�faut';
    LS_ZoomInStr: 'Grandir zoom';
    LS_ZoomOutStr: 'R�duire zoom';
    LS_CopyStr: 'Copier';
    LS_EditStr: 'Edit';
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
    LS_FontSizeError: 'Font size error';
    LS_OddPages: 'Odd';
    LS_EvenPages: 'Even';
    LS_OddPagesOnly: 'Odd pages only';
    LS_EvenPagesOnly: 'Even pages only';
    LS_AllOddAndEven: 'All';
    LS_PrintDialogError: 'Problems calling the printer dialog';
    LS_PageSelectionHint: 'Separate page numbers or page intervals with ";". i.e.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Rapport "%s"';
  );

var
  SpanishStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impresi�n en marcha...';
    LS_FilterInProgressStr: 'Guardando el informe...';
    LS_PreparingReportStr: 'Preparaci�n del informe...';
    LS_PrinterNotFoundStr: 'Impresora no encontrada';
    LS_NoPathToPrinterStr: 'Caminho inv�lido para a impressora';
    LS_LoadDefaultConfigStr: 'Cargar la configuraci�n est�ndar';
    LS_PrinterDriverErrorStr: 'Error en driver de la impresora';
    LS_PageStr: 'P�gina';
    LS_PrepareErrorStr: 'Un error ocurri� mientras se preparaba el informe';
    LS_PageBreakStr: 'Contin�a...';
    LS_PageMendStr: 'Continuaci�n';
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
    LS_OptionsStr: 'Opziones';
    LS_SaveInBackground: 'Guardar en background';
    LS_PageRangeStr: 'Intervalo de p�ginas';
    LS_CopyAsImageStr: 'Copy as image';
    LS_RangeFromStr: 'de';
    LS_RangeToStr: 'a';
    LS_AllStr: 'Todas';
    LS_PagesStr: 'P�ginas';
    LS_SelectionStr: 'Selecci�n';
    LS_CopiesStr: 'Copias';
    LS_NumberOfCopiesStr: 'N�mero de copias';
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Dividir la pantalla';
    LS_InvalidNameStr: 'Nombre inv�lido';
    LS_DuplicateNameStr: 'Nombre ya en uso';
    LS_UseFilterStr: 'Usar Filtro';
    LS_WebPageStr: 'P�gina Web';
    LS_RichFormatStr: 'Formato RichText';
    LS_PDFFormatStr: 'Documento PDF';
    LS_XLSFormatStr: 'Planilha Excel';
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
    LS_SetupStr: 'Setup...';
    LS_FontSizeError: 'Erro no c�lculo das fontes';
    LS_OddPages: '�mpares';
    LS_EvenPages: 'Pares';
    LS_OddPagesOnly: 'Somente �mpares';
    LS_EvenPagesOnly: 'Somente Pares';
    LS_AllOddAndEven: 'Todas';
    LS_PrintDialogError: 'Problemas com o di�logo da impressora';
    LS_PageSelectionHint: 'Separe com ponto-e-v�rgula os n�meros ou intervalos de p�ginas a imprimir. Ex.: 1;3;5-12;4';
    LS_DefaultJobTitle: 'Impresi�n "%s"';
  );

var
  ItalianStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Impression du rapport...';
    LS_FilterInProgressStr: 'Sauver le rapport...';
    LS_PreparingReportStr: 'Pr�paration du rapport...';
    LS_PrinterNotFoundStr: 'Imprimante non trouv�e';
    LS_NoPathToPrinterStr: 'Invalid printer path';
    LS_LoadDefaultConfigStr: 'Chargement de la configuration standard';
    LS_PrinterDriverErrorStr: 'Erreur dans le driver d''impression';
    LS_PageStr: 'Page';
    LS_PrepareErrorStr: 'Erreur durant la pr�partaion du rapport';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuazione';
    LS_ReportEndStr: 'Fine';
    LS_FileNotFoundStr: 'Archivio non fond�';
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
    LS_OptionsStr: 'Opciones';
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
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Divida lo schermo';
    LS_InvalidNameStr: 'Nome nullo';
    LS_DuplicateNameStr: 'Gi� chiami in uso';
    LS_UseFilterStr: 'Filtro di uso';
    LS_WebPageStr: 'Pagina di Web';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
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
    LS_DirectionDownStr: 'In gi�';
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
    LS_NoPathToPrinterStr: 'Ung�ltiger Druckerpfad';
    LS_LoadDefaultConfigStr: 'Laden Sie Standardkonfiguration';
    LS_PrinterDriverErrorStr: 'Druckerfahrer Fehler';
    LS_PageStr: 'Seite';
    LS_PrepareErrorStr: 'Fehler, w�hrend das Vorbereiten von Bericht';
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
    LS_CopyAsImageStr: 'Copy as image';
    LS_RangeFromStr: 'von';
    LS_RangeToStr: 'zu';
    LS_AllStr: 'Alles';
    LS_PagesStr: 'Seiten';
    LS_SelectionStr: 'Auswahl';
    LS_CopiesStr: 'Kopien';
    LS_NumberOfCopiesStr: 'Anzahl von Kopien';
    LS_OkStr: 'Ok';
    LS_DivideScreenStr: 'Teilen Sie den Bildschirm';
    LS_InvalidNameStr: 'Ung�ltiger Name';
    LS_DuplicateNameStr: 'Nennen Sie schon im Gebrauch';
    LS_UseFilterStr: 'Verwendungsfilter';
    LS_WebPageStr: 'Webseite';
    LS_RichFormatStr: 'RichText Format';
    LS_PDFFormatStr: 'PDF Document';
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
  );

procedure DetectLocale;
{$ifdef LINUX}
var
  dlct: string;
{$endif}
begin
{$ifdef LINUX}
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
  else
    LocaleStrings := EnglishStrings;
{$else}
  case SysLocale.PriLangID of
    $09 {LANG_ENGLISH}: LocaleStrings := EnglishStrings;
    $16 {LANG_PORTUGUESE}: LocaleStrings := PortugueseStrings;
    $0a {LANG_SPANISH}: LocaleStrings := SpanishStrings;
    $0c {LANG_FRENCH}: LocaleStrings := FrenchStrings;
    $10 {LANG_ITALIAN}: LocaleStrings := ItalianStrings;
    $1d {LANG_SWEDISH}: LocaleStrings := SwedishStrings;
  else
    LocaleStrings := EnglishStrings;
  end;
{$endif}
end;

initialization
  DetectLocale;

end.

