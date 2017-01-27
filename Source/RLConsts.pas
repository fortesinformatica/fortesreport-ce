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
  SysUtils ;

const
  {@const ScreenPPI - Resolução do monitor em pixels por polegada.
   Representa a quantidade de pixels por polegada do vídeo. O valor real varia de monitor para monitor mas,
   para facilitar cálculos e tornar os projetos independentes do terminal, essa valor é assumido como sendo 96. :/}
  ScreenPPI = 96;

  {@const InchAsMM - Fator de conversão de polegada para milímetros.
   Este fator é utilizado em diversos pontos para conversões de coordenadas. :/}
  InchAsMM = 254 / 10;

  {@const MMAsPixels - Fator de conversão de milímetros para pixels de tela.
   @links ScreenPPI, InchAsMM. :/}
  MMAsPixels = ScreenPPI / InchAsMM;

  MaxPageNo = 999999;

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
  CS_CopyrightStr = 'Copyright © 1999-2016 Fortes Informática';
  CS_ProductTitleStr = 'FortesReport Community Edition';
  CS_URLStr = 'http://www.fortesreport.com.br';
  CS_AuthorNameStr = 'Ronaldo Moreira';
  CS_Version = '4.0';

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
    {@var LS_FileNotFoundStr - Variável de internacionalização para "Arquivo não encontrado" :/}
    LS_FileNotFoundStr: string;
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
    {@var LS_CopyAsImageStr - Variável de internacionalização para "Copiar como imagem" :/}
    LS_CopyAsImageStr: string;
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
    {@var Ls_Nome_Arquivo - Variável de internacionalização para "Aplicar". :/}
    Ls_Aplicar: String;
    {@var Ls_Nome_Arquivo - Variável de internacionalização para "Propriedades". :/}
    Ls_Propriedades: String;
    {@var Ls_Salvar_Como - Variável de internacionalização para "Salvar como". :/}
    Ls_Salvar_Como: String;
    {@var Ls_Nome_Arquivo - Variável de internacionalização para "Nome do Arquivo". :/}
    Ls_Nome_Arquivo: String;
    {@var Ls_File_corrupted - Variável de internacionalização para "Arquivo corrompido". :/}
    Ls_File_corrupted: String;
    {@var Ls_File_corrupted - Variável de internacionalização para "Versão de Arquivo inválido". :/}
    Ls_File_version: String;
    {@var Ls_Page_setings - Variável de internacionalização para "Configuração da Página". :/}
    Ls_Page_settings: String;
    {@var Ls_Page_margins - Variável de internacionalização para "Margem da Página". :/}
    Ls_Page_margins: String;
    {@var Ls_Page_margins_top - Variável de internacionalização para "Margem Superior". :/}
    Ls_Page_margins_top: String;
    {@var Ls_Page_margins_bottom - Variável de internacionalização para "Margem Inferior". :/}
    Ls_Page_margins_bottom: String;
    {@var Ls_Page_margins_rigth - Variável de internacionalização para "Margem direita". :/}
    Ls_Page_margins_rigth: String;
    {@var Ls_Page_left_bottom - Variável de internacionalização para "Margem equerda". :/}
    Ls_Page_margins_left: String;
    {@var Ls_Page_margins_paper - Variável de internacionalização para "Margem do Papel". :/}
    Ls_Page_margins_paper: String;
    {@var Ls_Page_paper - Variável de internacionalização para "Papel". :/}
    Ls_Page_paper: String;
    {@var Ls_Paper_Size - Variável de internacionalização para "Tamanho do Papel". :/}
    Ls_Paper_Size: String;
    {@var Ls_Paper_Width - Variável de internacionalização para "Largura do Papel". :/}
    Ls_Paper_Size_Width: String;
    {@var Ls_Paper_Size_Heigth - Variável de internacionalização para "Altura do Papel". :/}
    Ls_Paper_Size_Heigth: String;
    {@var Ls_Paper_Orientation - Variável de internacionalização para "Orientação do Papel". :/}
    Ls_Paper_Orientation: String;
    {@var Ls_Paper_Orientation_Landscape - Variável de internacionalização para "Orientação da página em retrato". :/}
    Ls_Paper_Orientation_Landscape: String;
    {@var Ls_Paper_Orientation_Portrait - Variável de internacionalização para "Orientação da página em paisagem". :/}
    Ls_Paper_Orientation_Portrait: String;

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
    LS_Propriedades: 'Settings';
  );

var
  PortugueseStrings: TRLLocaleStrings = (
    LS_PrintingInProgressStr: 'Imprimindo o relatório...';
    LS_FilterInProgressStr: 'Salvando o relatório...';
    LS_PreparingReportStr: 'Preparando o relatório...';
    LS_PrinterNotFoundStr: 'Nenhuma impressora encontrada';
    LS_NoPathToPrinterStr: 'Caminho inválido para a impressora';
    LS_LoadDefaultConfigStr: 'Será carregada a configuração padrão';
    LS_PrinterDriverErrorStr: 'Erro no driver da impressora';
    LS_PageStr: 'Página';
    LS_PrepareErrorStr: 'Erro durante a preparação do relatório';
    LS_PageBreakStr: 'Continua...';
    LS_PageMendStr: 'Continuação';
    LS_ReportEndStr: 'Fim';
    LS_FileNotFoundStr: 'Arquivo não encontrado';
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
    LS_CopyAsImageStr: 'Copiar como imagem';
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
    Ls_Aplicar: 'Aplicar';
    Ls_Propriedades: 'Propriedades';
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
    LS_FirstPageStr: 'Première page';
    LS_PriorPageStr: 'Page précédente';
    LS_NextPageStr: 'Page suivante';
    LS_LastPageStr: 'Dernière page';
    LS_EntirePageStr: 'Page entière';
    LS_EntireWidthStr: 'Pleine largeur';
    LS_MultiplePagesStr: 'Plusieurs pages';
    LS_ConfigPrinterStr: 'Configuration de l''imprimante';
    LS_SaveToFileStr: 'Enregistrer dans un fichier';
    LS_SendToStr: 'Envoyer à...;
    LS_PrinterStr: 'Imprimante';
    LS_NameStr: 'Nom';
    LS_PrintToFileStr: 'Imprimer dans un fichier';
    LS_PrintInBackgroundStr: 'Imprimer dans background';
    LS_OptionsStr: 'Options';
    LS_SaveInBackground: 'Enregistrer dans background';
    LS_PageRangeStr: 'Intervalle de pages';
    LS_CopyAsImageStr: 'Copie sous forme d''image';
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
    Ls_Aplicar: 'Appliquer';
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
    LS_FileNotFoundStr: 'Archivio non fondò';
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
    LS_DuplicateNameStr: 'Già chiami in uso';
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
    LS_DirectionDownStr: 'In giù';
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
  else
    LocaleStrings := EnglishStrings;
  end;
{$EndIf}
end;

initialization
  DetectLocale;

end.

