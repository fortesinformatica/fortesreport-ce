{Projeto: FortesReport Community Edition                                      }
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

{@unit RLReport - Implementação dos principais componentes e tipos do FortesReport. }
unit RLReport;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Messages, DB, Classes, SysUtils, Math, Contnrs, TypInfo,
  {$IfDef CLX}
   QTypes, QButtons, QGraphics, QControls, QDialogs, QForms, QExtCtrls,
   QDBCtrls, QMask, RLMetaCLX,
  {$Else}
   Types, Buttons, Graphics, Controls, Dialogs, Forms, ExtCtrls, DBCtrls,
   RLMetaVCL, StdCtrls,
   {$IfDef FPC}
    LMessages,
   {$Else}
    Mask, RlCompilerConsts,
   {$EndIf}
  {$EndIf}
  {$IfDef SUPPORTS_VARIANT}
   variants,
  {$EndIf}
  maskutils,
  RLMetaFile, RLFeedBack, RLParser, RLFilters, RLConsts, RLUtils,
  RLPrintDialog, RLPreviewForm, RLPreview,
  RLTypes, RLPrinters;

type

  // CLASSES

  TRLCustomControl = class; // tcontrol base para todos os outros
  TRLCustomDBControl = class; // tcontrol base com acesso a banco de dados
  TRLCustomLabel = class; // tcontrol base com texto de uma linha
  TRLLabel = class; // componente label
  TRLCustomDBText = class; // label basico de campo de arquivo
  TRLDBText = class; // label de campo de arquivo
  TRLCustomDBResult = class; // label de operacoes com campos
  TRLDBResult = class;
  TRLCustomSystemInfo = class; // label com informacoes de sistema (nr.pag,totais,data&hora)
  TRLSystemInfo = class;
  TRLCustomAngleLabel = class;
  TRLAngleLabel = class;
  TRLCustomMultiLine = class; // tcontrol base com texto de varias linhas
  TRLCustomMemo = class; // componente memo
  TRLMemo = class;
  TRLCustomDBMemo = class; // memo de campo de arquivo
  TRLDBMemo = class;
  TRLCustomImage = class; // componente imagem
  TRLImage = class;
  TRLCustomDBImage = class; // imagem de campo de arquivo
  TRLDBImage = class;
  TRLCustomDraw = class; // canvas de figuras
  TRLDraw = class;
  TRLCustomSite = class; // controle base para todas as janelas de impressao
  TRLCustomPanel = class;
  TRLPanel = class; // componente de design para fazer containers alinhados
  TRLCustomBandSet = class;
  TRLCustomBand = class; // banda de impressao que pode ser: header, detail ou footer
  TRLBand = class;
  TRLCustomDetailGrid = class; // banda de colunas de detalhe
  TRLDetailGrid = class;
  TRLCustomPager = class; // painel paginador de tamanho variavel com header, detail e footer
  TRLCustomGroup = class; // paginador com escopo de registros
  TRLGroup = class;
  TRLCustomSkipper = class; // paginador controlado por base de dados
  TRLCustomSubDetail = class; // controlador de base masterizada
  TRLSubDetail = class;
  TRLCustomReport = class;
  TRLReport = class; // controlador principal

  TRLBorders = class;
  TRLMargins = class;
  TRLPageSetup = class;
  TRLRealBounds = class;
  TRLBackground = class;
  TRLDegradeEffect = class;
  TRLSortedBands = class;

  // CLASS TYPES

  TRLPagerClassType = class of TRLCustomPager;

  // PROPERTY TYPES

  {@type TRLDataFieldProperty - Tipo de propriedade para nome de campo de um dataset. :/}
  TRLDataFieldProperty = type String;

  {@type TRLDataFieldsProperty - Tipo de propriedade para lista de nomes de campos de um dataset.
   Os nomes devem ser separados por ponto-e-vírgula (";"). :/}
  TRLDataFieldsProperty = type String;

  {@type TRLRecordRange - Faixa de registros a processar.
   Pode assumir um dos seguintes valores:
   rrAllRecords - Processa desde o primeiro registro até o fim (default);
   rrCurrentOnly - Processa apenas o registro corrente;
   rrUntilEof - Processa a partir do registro corrente até o fim;
   rrNextN - Processa a partir do registro corrente (inclusive) N registros. Informe N na prop RangeCount.
   @links TRLCustomSkipper.RecordRange, TRLCustomSkipper.RangeCount. :/}
  TRLRecordRange = (rrAllRecords, rrCurrentOnly, rrUntilEof, rrNextN);

  // EVENT TYPES

  {@type TRLRecordAction - Ação tomada a cada registro processado.
   Pode assumir um dos seguintes valores:
   raUseIt - Utilizar o registro e processar o próximo;
   raIgnoreIt - Não utilizar o registro e processar o próximo;
   raUseAndRetain - Utilizar o registro mais de uma vez;
   raBlankAndRetain - Imprimir band em branco e utilizá o registro mais uma vez.
   @links TRLCustomSkipper.DataFirst, TRLCustomSkipper.DataNext. :/}
  TRLRecordAction = (raUseIt, raIgnoreIt, raUseAndRetain, raBlankAndRetain);

  {@type TRLAfterPrintEvent - Após a impressão de um controle.
   Implemente um evento TRLAfterPrintEvent para tomar atitudes logo após a impressão
   de um controle.
   @links TRLBeforePrintEvent, TRLBeforeTextEvent. :/}
  TRLAfterPrintEvent = procedure(Sender: TObject) of object;

  {@type TRLBeforePrintEvent - Antes da impressão de um controle.
   Implemente um evento TRLBeforePrintEvent para decidir se o controle será impresso,
   através do parâmetro PrintIt, ou para alterar as suas características, como por
   exemplo: dimensões, cor, bordas etc.
   @links TRLAfterPrintEvent, TRLBeforeTextEvent. :/}
  TRLBeforePrintEvent = procedure(Sender: TObject; var PrintIt: Boolean) of object;

  {@type TRLBeforeTextEvent - Antes da impressão de uma caixa de texto.
   Implemente um evento TRLBeforeTextEvent para decidir se o controle será impresso,
   através do parâmetro PrintIt, para alterar o texto a imprimir ou para alterar as
   suas características, como por exemplo: dimensões, cor, bordas etc.
   Nota: Este evento aparecerá como o nome BeforePrint nas caixas de texto.
   @links TRLAfterPrintEvent, TRLBeforePrintEvent, TRLCustomLabel. :/}
  TRLBeforeTextEvent = procedure(Sender: TObject; var AText: string;
    var PrintIt: Boolean) of object;

  {@type TRLOnComputeEvent - Ao computar um valor durante iterações para estatística dos acumuladores.
   Altere o parâmetro Value e/ou Text para modificar o valor processado pelos acumuladores,
   ou altere o parâmetro ComputeIt para indicar se o valor deve ser computado ou ignorado.
   @links TRLDBResult. :/}
  TRLOnComputeEvent = procedure(Sender: TObject; var Value: Variant;
    var AText: string; var ComputeIt: Boolean) of object;

  {@type TRLOnDetailComputeEvent - Ao computar uma band detail durante iterações para estatística dos acumuladores.
   Altere o parâmetro ComputeIt para indicar se os valores correspondentes à band devem ser computados ou ignorados.
   @links TRLDBResult. :/}
  TRLOnDetailComputeEvent = procedure(Sender: TObject; var ComputeIt: Boolean) of object;

  {@type TRLOnGetBreakEvent - Nas verificações de quebra de grupo de registros não automática.
   Altere o parâmetro BreakIt para indicar a quebra de sequência de registros num grupo.
   @links TRLGroup. :/}
  TRLOnGetBreakEvent = procedure(Sender: TObject; var BreakIt: Boolean) of object;

  {@type TRLOnDataCountEvent - Ao calcular a quantidade de registros.
   Implemente este evento para informar ao gerador a quantidade de registros a serem processados.
   Isto ajuda a projetar as expectativas de término, e também elimina a consulta à prop RecordCount
   do DataSet que estiver associado pela prop DataSource.
   @links TRLCustomSkipper. :/}
  TRLOnDataCountEvent = procedure(Sender: TObject; var DataCount: Integer) of object;

  {@type TRLOnNeedDataEvent - Alimentação de registros sem dataset.
   Implemente este evento para fornecer registros de dados a um RLReport ou RLSubDetail.
   Indique o final dos registros alterando o parâmetro MoreData para False.
   @links TRLCustomSkipper. :/}
  TRLOnNeedDataEvent = procedure(Sender: TObject; var MoreData: Boolean) of object;

  {@type TRLOnDataRecordEvent - Ao processar um registro.
   Este evento é disparado todas as vezes que um registro é processado, quer tenha sido
   obtido de um DataSet ou fornecido pelo evento OnNeedData.
   O parâmetro RecNo representa o número sequencial do registro corrente.
   O parâmetro CopyNo é o número de cópia do registro, caso ele esteja sendo repetido.
   O parâmetro Eof indica ou atribui o fim dos dados. É semelhante ao MoreData do evento OnNeedData.
   O parâmetro RecordAction indica a próxima ação a ser tomada com o registro.
   Obs.: Um registro pode ser repetido se a ação do último evento foi raUseAndRetain e, neste caso, CopyNo será maior que 1.
   @links TRLCustomSkipper. :/}
  TRLOnDataRecordEvent = procedure(Sender: TObject; RecNo: Integer;
    CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction) of object;

  {@type TRLOnDrawEvent - Na hora de desenhar o fundo do controle.
   Implemente este evento para desenhar aleatóriamente no fundo de uma controle.
   Surface representa a superfície de desenho (Canvas) do controle em questão.
   Rect é o retângulo da área cliente do controle, aonde é possível desenhar.
   @links TRLCustomSite. :/}
  TRLOnDrawEvent = procedure(Sender: TObject; Surface: TRLGraphicSurface;
    Rect: TRect) of object;

  {@type TRLPrepareErrorEvent - Ao ocorrer qualquer erro no método Prepare. :/}
  TRLPrepareErrorEvent = procedure(Sender: TObject; Error: Exception) of object;

  // ENUMERATED TYPES

  {@type TRLBandType - Tipo de banda.
   O tipo indica o comportamento que a band deverá assumir durante a listagem.
   Pode assumir um dos seguintes valores:
   btHeader - Cabeçalho. Imprime uma vez na primeira página e sempre que houver quebra de página ou de sequência
   de dados. Útil para exibir número de página e nome do relatório ou informações sobre a sequência de dados atual;
   btTitle - Título. Imprime apenas na primeira página ou no ínicio de uma sequência de dados abaixo do header. Útil
   para mostrar descrição prévia do relatório;
   btColumnHeader - Cabeçalho de colunas. Mesmo comportamento do header exceto por seu posicionamento após o title;
   btDetail - Detalhe. Imprime uma vez para cada registro de dados;
   btColumnFooter - Rodapé de colunas. Mesmo comportamento do rodapé exceto por seu posicionamento antes do summary;
   btSummary - Sumário. Imprime ao final do relatório ou da sequência de dados antes do footer. Útil para mostrar
   resumos, somatórios e informações estatísticas;
   btFooter - Rodapé. Imprime uma vez na última página e sempre após quebra de página ou de sequência de dados.
   @links TRLBand. :/}
  TRLBandType = (btHeader, btTitle, btColumnHeader, btDetail,
    btColumnFooter, btSummary, btFooter);

  {@type TRLCompletionType - Tipo de preenchimento de página.
   Indica como a página deve ser preenchida após o último registro ser impresso.
   Pode assumir um dos seguintes valores:
   ctNone - Não completa página;
   ctFullPage - Completa com bands em branco até o fim da página;
   ctMaxBands - Completa com bands em branco até o número máximo de bands do ParentPager;
   ctMinBands - Completa com bands em branco até o número mínimo de bands do ParentPager.
   @links TRLBand, MaxBands. :/}
  TRLCompletionType = (ctNone, ctFullPage, ctMaxBands, ctMinBands);

  {@type TRLImageArrange - Tipo de arranjo de imagem.
   Indica como uma imagem deve ser distribuída no fundo do controle.
   Pode assumir um dos seguintes valores:
   baAligned - A imagem deve ser alinhada no fundo de acordo com a prop Align;
   baSidebySide - A imagem deve ser distribuída pelo fundo horizontalmente;
   baCenter - A imagem deve ser centralizada;
   baDistributed - A imagem deve ser distribuída como numa parede de tijolos.
   @links TRLBackground. :/}
  TRLImageArrange = (baAligned, baSidebySide, baCenter, baDistributed);

  {@type TRLReportState - Estado atual do relatório.
   Indica como está o relatório em relação à sua preparação.
   Pode assumir um dos seguintes valores:
   rsAbout - Ainda não foi preparado;
   rsWriting - Está sendo preparado;
   rsClosing - Não há mais páginas a preparar e vai finalizar;
   rsReady - Foi preparado e está pronto para imprimir.
   @links TRLReport. :/}
  TRLReportState = (rsAbout, rsWriting, rsClosing, rsReady);

  {@type TRLDegradeDirection - Direção do efeito degradê.
  Indica a direção do efeito de transição de cores (degradê) no fundo do controle.
  Pode assumir um dos seguintes valores:
  ddNone - Nenhuma efeito;
  ddHorizontal - Efeito horizontal da esquerda para a direita, de Color para OppositeColor;
  ddVertical - Efeito vertical de cima para baixo, de Color para OppositeColor.
   @links TRLDegradeEffect. :/}
  TRLDegradeDirection = (ddNone, ddHorizontal, ddVertical);

  {@type TRLInfoType - Tipo de informação de sistema.
   Indica que informações de sistema devem ser exibidas pelo controle TRLSystemInfo.
   Pode assumir um dos seguintes valores:
   itCarbonCopy - Número da cópia de imagem da band;
   itDate - Data da impressão (ver prop TRLReport.ReportDateTime);
   itDetailCount - Quantidade de detalhes já impressos;
   itFullDate - Data e hora da impressão no formato LongDateFormat;
   itHour - Hora da impressão (ver prop TRLReport.ReportDateTime);
   itJunction - Flag de junção de páginas. Indica se o relatório continua nas próximas páginas;
   itLastPageNumber - Número da última página do relatório;
   itMend - Flag de junção de páginas. Indica se a página atual é a continuação de páginas anteriores;
   itNow - Data e hora da impressão no formato ShotDateFormat;
   itPageNumber - Número da página atual;
   itPagePreview - Número da página atual e total de páginas do relatório;
   itTitle - Título do relatório obtido da prop Title do TRLReport;
   itRecNo - Número sequencial do registro corrente;
   itCopyNo - Número sequencial da cópia do registro.
   @links TRLSystemInfo, TRLReport, TRLReport.ReportDateTime, TRLCustomBand.CarbonCopies. :}
  TRLInfoType = (itCarbonCopy, itDate, itDetailCount, itFullDate,
    itHour, itJunction, itLastPageNumber, itMend, itNow,
    itPageNumber, itPagePreview, itTitle, itRecNo, itCopyNo);
  {/@type}

  {@type TRLResultInfo - Tipo de informação estatística.
   Indica que informações estatísticas devem ser exibidas pelo controle TRLDBResult.
   Pode assumir um dos seguintes valores:
   riAverage - Média aritmética dos valores impressos;
   riCount - Número de ocorrências dos valores impressos; 
   riFirst - Primeiro valor impresso;
   riLast - Último valor impresso;
   riMax - Maior dos valores impressos;
   riMin - Menor dos valores impressos;
   riSum - Somátório de todos os valores impressos;
   riFirstText - Primeiro texto impresso;
   riLastText - Último texto impresso;
   riSimple - Útil para a resolução de fórmulas com funções built-in. 
   @links TRLDBResult. :/}
  TRLResultInfo = (riAverage, riCount, riFirst, riLast, riMax, riMin,
    riSum, riFirstText, riLastText, riSimple);

  {@type TRLControlAlign - Alinhamento melhorado.
   Especifica como um controle deve ser posicionado em relação ao controle pai.
   Pode assumir um dos seguintes valores:
   faNone - Nenhum alinhamento. Nenhuma alteração automática no posicionamento ou dimensão do controle;
   faLeft - Alinhado à esquerda. A largura é mantida e a altura se ajusta ao máximo disponível no controle pai;
   faTop - Alinhado acima. A altura é mantida e a largura se ajusta ao máximo disponível no controle pai;
   faRight - Alinhado à direita. A largura é mantida e a altura se ajusta ao máximo disponível no controle pai;
   faBottom - Alinhado abaixo. A altura é mantida e a largura se ajusta ao máximo disponível no controle pai;
   faLeftMost - Alinhado à esquerda com prioridade. Mesmo que faLeft com prioridade sobre os alinhamentos verticais;
   faRightMost - Alinhado à direita com prioridade. Mesmo que faRight com prioridade sobre os alinhamentos verticais;
   faClient - Alinhado à área cliente. O controle se ajusta à área que sobrou no controle pai;
   faLeftTop - Alinhado à esquerda e acima. O controle mantém suas dimensões e suas coordenadas são (0,0);
   faRightTop - Alinhado à direita e acima. O controle mantém suas dimensões e suas coordenadas são (-Width,0);
   faLeftBottom - Alinhado à esquerda e abaixo. O controle mantém suas dimensões e suas coordenadas são (0,-Height);
   faRightBottom - Alinhado à direita e abaixo. O controle mantém suas dimensões e suas coordenadas são (-Width,-Height);
   faCenter - Alinhado ao centro. O controle mantém suas dimensões;
   faCenterLeft - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faCenterTop - Alinhado ao centro e acima. O controle mantém suas dimensões;
   faCenterRight - Alinhado ao centro e à direita. O controle mantém suas dimensões;
   faCenterBottom - Alinhado ao centro e abaixo. O controle mantém suas dimensões;
   faClientLeft - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faClientTop - Alinhado ao centro e acima. O controle mantém suas dimensões;
   faClientRight - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faClientBottom - Alinhado ao centro e abaixo. O controle mantém suas dimensões;
   faHeight - Alinhado pela altura. O controle mantém a sua largura e expande a sua altura de modo a se acomodar no controle pai;
   faWidth - Alinhado pela largura. O controle mantém a sua altura e expande a sua largura de modo a se acomodar no controle pai;
   faLeftOnly - Alinhado à esquerda somente. O controle tem sua coordenada esquerda igual a 0;
   faRightOnly - Alinhado à direita somente. O controle tem sua coordenada direita igual a 0;
   faTopOnly - Alinhado acima somente. O controle tem sua coordenada de topo igual a 0;
   faBottomOnly - Alinhado abaixo somente. O controle tem sua coordenada abaixo igual a -Height.
   @links TRLCustomControl. :}
  TRLControlAlign = (faNone, faLeft, faTop, faRight, faBottom,
    faLeftMost, faRightMost, faClient,
    faLeftTop, faRightTop, faLeftBottom, faRightBottom,
    faCenter, faCenterLeft, faCenterTop, faCenterRight, faCenterBottom,
    faClientLeft, faClientTop, faClientRight, faClientBottom,
    faHeight, faWidth,
    faLeftOnly, faRightOnly, faTopOnly, faBottomOnly);
  {/@type}

  {@type TRLTextAlignment - Alinhamento melhorado de texto dentro do controle.
   Especifica como o texto deve ser posicionado dentro de um controle de texto.
   Pode assumir um dos seguintes valores:
   taLeftJustify - Alinhado à esquerda (padrão);
   taRightJustify - Alinhado à direita;
   taCenter - Alinhado ao centro do controle;
   taJustify - Espaços são inseridos entre as palavras de modo que o texto ocupe toda a largura do controle.
   @links TRLCustomLabel, TRLCustomMemo. :/}
  TRLTextAlignment = (taLeftJustify, taRightJustify, taCenter, taJustify);

  {@type TRLTextLayout - Alinhamento vertical melhorado de texto dentro do controle.
   Especifica como o texto deve ser posicionado dentro de um controle de texto na vertical.
   Pode assumir um dos seguintes valores:
   tlTop - Alinhado ao topo (padrão);
   tlCenter - Alinhado ao centro do controle;
   tlBottom - Alinhado ao fundo;
   tlJustify - Linhas em branco são inseridas entre as linhas de texto de modo que o texto ocupe toda a largura do controle.
   @links TRLCustomLabel, TRLCustomMemo. :/}
  TRLTextLayout = (tlTop, tlCenter, tlBottom, tlJustify);

  {@type TRLPageBreaking - Quebra de página forçada.
   Especifica se e como a band ou Pager irá forçar a quebra de página.
   Pode assumir um dos seguintes valores:
   pbNone - Não deve haver quebra de página forçada;
   pbBeforePrint - A quebra de página será verificada sempre antes da impressão do controle;
   pbAfterPrint - A quebra de página será verificada sempre após da impressão do controle.
   @links PageBreaking. :/}
  TRLPageBreaking = (pbNone, pbBeforePrint, pbAfterPrint);

  {@type TRLPrintBandResults - Resultado da impressão de bands.
   Pode assumir um dos seguintes valores:
   brNoBands - Nenhuma band foi impressa;
   brPrinted - Ao menos uma band foi impressa;
   brStackExit - Saída forçada por salto de página.
   @links TRLCustomPager.PrintBands. :/}
  TRLPrintBandResults = (brNoBands, brPrinted, brStackExit);

  {@type TRLHoldStyle - Estilo de ancoragem relativa.
   Pode assumir um dos seguintes valores:
   hsAsColumn - O controle segue a posição horizontal do Holder e copia a sua largura, desde que a propriedade AutoSize seja False;
   hsHorizontally - O controle apenas segue a posição horizontal do Holder;
   hsVertically - O controle apenas segue a posição vertical do Holder;
   hsRelatively - O controle mantém a posição horizontal e vertical em relação ao Holder.
   @links TRLCustomControl.Holder, TRLCustomControl.SecondHolder. :/}
  TRLHoldStyle = (hsAsColumn, hsHorizontally, hsVertically, hsRelatively,
    hsCopyWidth, hsCopyHeight, hsCopySize);

  {@type TRLPrintQuality - Qualidade de impressão.
   Configura os elementos gráficos que devem ser impressos.
   Pode assumir um dos seguintes valores:
   pqFullFeature - Todos os recursos gráficos serão preservados;
   pqFixedOnly - Imprimir apenas bordas fixadas. Dispensa características gráficas especiais.
   @links TRLReport. :/}
  TRLPrintQuality = (pqFullFeature, pqFixedOnly);

  {@type TRLControlAnchorsType - Âncoras melhoradas.
   Indica os lados aos quais o controle será ancorado.
   Pode assumir um dos seguintes valores:
   fkLeft - À esquerda;
   fkTop - Ao topo;
   fkRight - À direita;
   fkBottom - À base.
   @links TRLCustomControl. :/}
  TRLControlAnchorsType = (fkLeft, fkTop, fkRight, fkBottom);

  {@type TRLControlStateType - Status de um controle FortesReport.
   Indica o estado atual do controle quanto à impressão, alinhamento e dimensões.
   Pode assumir um dos seguintes valores:
   stPrinting - Sendo impresso;
   stAligningControls - Alinhando seus controles internos;
   stAdjustingHoldeds - Ajustando os controles dos quais é o Holder;
   stAdjustingBounds - Ajustando as suas dimensões;
   stExpandingParent - Ajustando o controle pai que é AutoSize e AutoExpand;
   stRestoringBounds - Restaurando suas dimensões após a sua impressão;
   stMeasuringHeights - Antecipando a sua própria medição.
   @links TRLCustomControl.ControlState, TRLCustomControl.Holder, TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLControlStateType = (stPrinting, stAligningControls, stAdjustingHoldeds,
    stAdjustingBounds, stExpandingParent, stRestoringBounds, stMeasuringHeights);

  {@type TRLAutoSizeDirType - Direção do redimensionamento automático.
   Determina a direção do redimensionamento automático.
   Pode assumir um dos seguintes valores:
   asWidthDir - Redimensionamento pela largura;
   asHeightDir - Redimensionamento pela altura.
   @links TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLAutoSizeDirType = (asWidthDir, asHeightDir);

  {@type TRLControlBehaviorType - Comportamento do controle.
   Determina como o controle deve se comportar sob diversos aspectos.
   Pode assumir um dos seguintes valores:
   beSiteExpander - Ao sofrer redimensionamento o controle deve expandir o seu controle pai.
   @links TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLControlBehaviorType = (beSiteExpander);

  {@type TRLFooterMeasuring - Momento de medição.
   Determina em que momento deve ser efetuada uma antecipação na medição de bands como btFooter ou btSummary.
   Pode assumir um dos seguintes valores:
   fmNone - Nenhuma antecipação é feita;
   fmAfterHeader - Sempre após a impressão dos cabeçalhos;
   fmBeforeDetail - Antes de cada band de detalhe.
   @links TRLCustomBand.BandType. :/}
  TRLFooterMeasuring = (fmNone, fmAfterHeader, fmBeforeDetail);

  // SETS

  {@type TRLControlAnchors - Conjunto de âncoras.
   Indica os lados aos quais o controle será ancorado.
   @links TRLControlAnchorsType, TRLCustomControl.Anchors. :/}
  TRLControlAnchors = set of TRLControlAnchorsType;

  {@type TRLAllowedBands - Tipos de band inseridos.
   Determina que tipos de band inicialmente serão inseridos sobre o Pager.
   Nota: Este recurso é mantido para fins de compatibilidade, pois o FortesReport permite mais de uma band
   do mesmo tipo por relatório.
   @links TRLBandType, TRLCustomPager.AllowedBands. :/}
  TRLAllowedBands = set of TRLBandType;

  {@type TRLControlState - Status do controle.
   Indica o estado atual do controle.
   @links TRLControlStateType. :/}
  TRLControlState = set of TRLControlStateType;

  {@type TRLControlBehavior - Comportamento do controle.
   Determina características de comportamento do controle.
   @links TRLControlBehaviorType, TRLCustomControl.Behavior. :/}
  TRLControlBehavior = set of TRLControlBehaviorType;

  {@type TRLAutoSizeDirSet - Direções de redimensionamento.
   Determina as direções do redimensionamento automático.
   @links TRLAutoSizeDirType, TRLCustomControl.AutoSizeDir, TRLCustomControl.ExpandParentSite. :/}
  TRLAutoSizeDirSet = set of TRLAutoSizeDirType;

  // OBJECT PROPERTIES

  {@type TRLBorderSides - Configuração rápida de bordas.
   Pode assumir um dos seguintes valores:
   sdCustom - As bordas devem ser indicadas pela propriedade Borders;
   sdNone - O controle não deve exibir bordas;
   sdAll - Todas as bordas acionadas.
   @links TRLBorders. :/}
  TRLBorderSides = (sdCustom, sdNone, sdAll);

  {@class TRLBorders - Propriedades para as bordas de um TRLCustomControl.
   Determina que lados serão desenhados, a largura, estilo, cor e espessura das linhas. Possui propriedade ParentControl
   para determinar o controle onde se deve desenhar. Possui método AdjustParent que chama o AdjustBounds do ParentControl
   sempre que forem alteradas as propriedades que afetam o tamanho. Invoca o Invalidate do ParentControl sempre que houver
   alteração na cor e estilo. Este objeto não é responsável pelo Paint do ParentControl, este é que deve faze-lo no seu
   Paint de acordo com as propriedades de borda.
   @links TRLCustomControl.Borders. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLBorders = class(TPersistent)
  private

    // variables

    FParentControl: TRLCustomControl;
    FDrawLeft: Boolean;
    FDrawTop: Boolean;
    FDrawRight: Boolean;
    FDrawBottom: Boolean;
    FWidth: Integer;
    FColor: TColor;
    FStyle: TBrushStyle;
    FSides: TRLBorderSides;
    FFixedLeft: Boolean;
    FFixedTop: Boolean;
    FFixedRight: Boolean;
    FFixedBottom: Boolean;

    // assign methods

    procedure SetSides(const AValue: TRLBorderSides);
    procedure SetDrawLeft(const AValue: Boolean);
    procedure SetDrawTop(const AValue: Boolean);
    procedure SetDrawRight(const AValue: Boolean);
    procedure SetDrawBottom(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
    procedure SetColor(const AValue: TColor);
    procedure SetStyle(const AValue: TBrushStyle);
    procedure SetParentControl(const AValue: TRLCustomControl);
    procedure SetFixedLeft(const AValue: Boolean);
    procedure SetFixedTop(const AValue: Boolean);
    procedure SetFixedRight(const AValue: Boolean);
    procedure SetFixedBottom(const AValue: Boolean);

    // custom methods

    procedure AdjustParent;
    procedure CheckSides;
    function IsCustom: Boolean;

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomControl);
    destructor Destroy; override;

    // custom methods

    {@method PaintTo - Desenha as bordas em um canvas delimitado por um retângulo. :}
    procedure PaintTo(ACanvas: TCanvas; ARect: TRect); overload;
    procedure PaintTo(ASurface: TRLGraphicSurface; ARect: TRect); overload;
    {/@method}

    {@method CanDrawLeft - Indica se é pérmitido desenhar a borda esquerda. :/}
    function CanDrawLeft: Boolean;

    {@method CanDrawTop - Indica se é pérmitido desenhar a borda superior. :/}
    function CanDrawTop: Boolean;

    {@method CanDrawRight - Indica se é pérmitido desenhar a borda direita. :/}
    function CanDrawRight: Boolean;

    {@method CanDrawBottom - Indica se é pérmitido desenhar a borda inferior. :/}
    function CanDrawBottom: Boolean;

    // custom properties

    {@prop ParentControl - Controle sobre o qual as bordas serão desenhadas.
     @links TRLCustomControl. :/}
    property ParentControl: TRLCustomControl read FParentControl write SetParentControl;

  published

    // custom properties

    {@prop Sides - Configuração instantânea das bordas.
     @links TRLBorderSides. :/}
    property Sides: TRLBorderSides read FSides write SetSides default sdNone;

    {@prop DrawLeft - Desenhar borda esquerda. :/}
    property DrawLeft: Boolean read FDrawLeft write SetDrawLeft stored IsCustom;

    {@prop DrawTop - Desenhar borda superior. :/}
    property DrawTop: Boolean read FDrawTop write SetDrawTop stored IsCustom;

    {@prop DrawRight - Desenhar borda direita. :/}
    property DrawRight: Boolean read FDrawRight write SetDrawRight stored IsCustom;

    {@prop DrawBottom - Desenhar borda inferior. :/}
    property DrawBottom: Boolean read FDrawBottom write SetDrawBottom stored IsCustom;

    {@prop Width - Largura da borda. :/}
    property Width: Integer read FWidth write SetWidth default 1;

    {@prop Color - Cor da borda. :/}
    property Color: TColor read FColor write SetColor default clBlack;

    {@prop Style - Estilo da borda. :/}
    property Style: TBrushStyle read FStyle write SetStyle default bsSolid;

    {@prop FixedLeft - Desenhar borda esquerda fixa. :/}
    property FixedLeft: Boolean read FFixedLeft write SetFixedLeft default False;

    {@prop FixedTop - Desenhar borda superior fixa. :/}
    property FixedTop: Boolean read FFixedTop write SetFixedTop default False;

    {@prop FixedRight - Desenhar borda direita fixa. :/}
    property FixedRight: Boolean read FFixedRight write SetFixedRight default False;

    {@prop FixedBottom - Desenhar borda inferior fixa. :/}
    property FixedBottom: Boolean read FFixedBottom write SetFixedBottom default False;
  end;

  {/@class}


  {@class TRLMargins - Propriedades para as margens internas de alinhamento de um CustomPanel.
   Determina largura das margens: superior, inferior e laterais em MM. Possui método AdjustParent que chama o AdjustBounds
   do ParentControl sempre que forem alteradas as propriedades que afetam o tamanho.
   @links TRLCustomSite.Margins, TRLCustomSite.InsideMargins. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLMargins = class(TPersistent)
  private

    // variables

    FParentControl: TRLCustomControl;
    FLeftMargin: Double;
    FTopMargin: Double;
    FRightMargin: Double;
    FBottomMargin: Double;
    FDefaultLeftMargin: Double;
    FDefaultTopMargin: Double;
    FDefaultRightMargin: Double;
    FDefaultBottomMargin: Double;

    // assign methods

    procedure SetLeftMargin(const AValue: Double);
    procedure SetRightMargin(const AValue: Double);
    procedure SetTopMargin(const AValue: Double);
    procedure SetBottomMargin(const AValue: Double);

    procedure ReadLeftMargin(Reader: TReader);
    procedure WriteLeftMargin(Writer: TWriter);
    procedure ReadTopMargin(Reader: TReader);
    procedure WriteTopMargin(Writer: TWriter);
    procedure ReadRightMargin(Reader: TReader);
    procedure WriteRightMargin(Writer: TWriter);
    procedure ReadBottomMargin(Reader: TReader);
    procedure WriteBottomMargin(Writer: TWriter);

    // custom methods

    procedure AdjustParent;

  protected

    // override

    procedure DefineProperties(Filer: TFiler); override;

    // custom methods

    procedure SetDefaults(ALeft, ATop, ARight, ABottom: Double);

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomControl);
    destructor Destroy; override;

    // custom properties

    {@prop ParentControl - Referência ao controle.
     @links TRLCustomControl. :/}
    property ParentControl: TRLCustomControl read FParentControl write FParentControl;

    // override

    procedure Assign(Source: TPersistent); override;

  published

    // custom properties

    {@prop LeftMargin - Margem esquerda em milímetros. :/}
    property LeftMargin: Double read FLeftMargin write SetLeftMargin stored False;

    {@prop TopMargin - Margem superior em milímetros. :/}
    property TopMargin: Double read FTopMargin write SetTopMargin stored False;

    {@prop RightMargin - Margem direita em milímetros. :/}
    property RightMargin: Double read FRightMargin write SetRightMargin stored False;

    {@prop BottomMargin - Margem inferior em milímetros. :/}
    property BottomMargin: Double read FBottomMargin write SetBottomMargin stored False;
  end;

  {/@class}


  {@class TRLPageSetup - Propriedades para configuração de página.
                         Determina a largura e altura do papel em MM, o tipo de papel utilizado e a orientação.
   @links TRLCustomReport.PageSetup. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLPageSetup = class(TPersistent)
  private

    // variables

    FParentReport: TRLCustomReport;
    FPaperHeight: Double;
    FPaperWidth: Double;
    FPaperSize: TRLPaperSize;
    FOrientation: TRLPageOrientation;
    FForceEmulation: Boolean;

    // assign methods

    function GetOrientedWidth: Double;
    function GetOrientedHeight: Double;
    procedure SetOrientedHeight(const AValue: Double);
    procedure SetOrientedWidth(const AValue: Double);
    procedure SetPaperSize(const AValue: TRLPaperSize);
    procedure SetPaperHeight(const AValue: Double);
    procedure SetPaperWidth(const AValue: Double);
    procedure SetOrientation(const AValue: TRLPageOrientation);

    // custom methods

    procedure AdjustParent;
    function IsCustomPaperSize: Boolean;

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomReport);
    destructor Destroy; override;

    // custom methods

    {@method Assign - Inicializa propriedades a partir de um outro objeto. :/}
    procedure Assign(Source: TRLPageSetup); reintroduce;

    {@prop ParentReport - Referência ao objeto relatório.
     @links TRLCustomReport. :/}
    property ParentReport: TRLCustomReport read FParentReport write FParentReport;

    {@prop OrientedWidth - Largura orientada do papel em milímetros. :/}
    property OrientedWidth: Double read GetOrientedWidth write SetOrientedWidth;

    {@prop OrientedHeight - Altura orientada do papel em milímetros. :/}
    property OrientedHeight: Double read GetOrientedHeight write SetOrientedHeight;

  published

    // custom properties

    {@prop PaperSize - Tamanho do papel.
     @links TRLPaperSize. :/}
    property PaperSize: TRLPaperSize read FPaperSize write SetPaperSize default fpA4;

    {@prop Orientation - Orientação do papel.
     @links TRLPageOrientation. :/}
    property Orientation: TRLPageOrientation
      read FOrientation write SetOrientation default poPortrait;

    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property PaperWidth: Double
      read FPaperWidth write SetPaperWidth stored IsCustomPaperSize;

    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property PaperHeight: Double
      read FPaperHeight write SetPaperHeight stored IsCustomPaperSize;

    {@prop ForceEmulation - Emulação forçada. :/}
    property ForceEmulation: Boolean
      read FForceEmulation write FForceEmulation default False;
  end;

  {/@class}


  {@type TRLRealBoundsUnit - Unidades de medida para o dimensionamento real de um controle.
                             Nota: Esta funcionalizade não está implementada. :/}
  TRLRealBoundsUnit = (buNone, buMilimeters, buInches);

  {@class TRLRealBounds - Configuração do tamanho real de um controle em milímetros ou polegadas.
                          Nota: Esta funcionalizade não está implementada.
   @links TRLCustomControl.RealBounds. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLRealBounds = class(TPersistent)
  private

    // variables

    FParentControl: TRLCustomControl;
    FUsedUnit: TRLRealBoundsUnit;
    FLeft: Double;
    FTop: Double;
    FHeight: Double;
    FWidth: Double;

    // assign methods

    procedure SetLeft(const AValue: Double);
    procedure SetTop(const AValue: Double);
    procedure SetHeight(const AValue: Double);
    procedure SetWidth(const AValue: Double);
    procedure SetUsedUnit(const AValue: TRLRealBoundsUnit);

    // custom methods

    procedure AdjustParent;

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomControl);

    // custom methods

    {@prop ParentReport - Referência ao controle.
     @links TRLCustomControl. :/}
    property ParentControl: TRLCustomControl read FParentControl write FParentControl;
    destructor Destroy; override;

  published

    // custom properties

    {@prop UsedUnit - Unidade de medida utilizada.
     @links TRLRealBoundsUnit. :/}
    property UsedUnit: TRLRealBoundsUnit read FUsedUnit write SetUsedUnit default buNone;

    {@prop Left - Coordenada esquerda em milímetros. :/}
    property Left: Double read FLeft write SetLeft;

    {@prop Top - Coordenada superior em milímetros. :/}
    property Top: Double read FTop write SetTop;

    {@prop Width - Largura em milímetros. :/}
    property Width: Double read FWidth write SetWidth;

    {@prop Height - Altura em milímetros. :/}
    property Height: Double read FHeight write SetHeight;
  end;

  {/@class}


  {@class TRLBackground - Propriedades para uma figura a ser desenhada no fundo de um site.
                          Determina o posicionamento ou forma de distribuicao, e o tamanho da figura no parentsite.
   @links TRLCustomSite.Background, TRLDegradeEffect. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLBackground = class(TPersistent)
  private

    // variables

    FParentSite: TRLCustomSite;
    FAlign: TRLControlAlign;
    FArrange: TRLImageArrange;
    FAutoSize: Boolean;
    FHeight: Integer;
    FPicture: TPicture;
    FStretch: Boolean;
    FWidth: Integer;

    // assign methods

    procedure SetAlign(const AValue: TRLControlAlign);
    procedure SetArrange(const AValue: TRLImageArrange);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetHeight(const AValue: Integer);
    procedure SetPicture(const AValue: TPicture);
    procedure SetStretch(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomSite);
    destructor Destroy; override;

    // custom methods

    {@method PaintTo - Desenha em outra superfície. :}
    procedure PaintTo(ACanvas: TCanvas; ARect: TRect); overload;
    procedure PaintTo(ASurface: TRLGraphicSurface; ARect: TRect); overload;
    {/@method}

    {@method AdjustSize - Ajusta tamanho de acordo com a imagem. :/}
    procedure AdjustSize;

    // custom properties

    {@prop ParentSite - Referência ao site sobre o qual o fundo será desenhado.
     @links TRLCustomSite. :/}
    property ParentSite: TRLCustomSite read FParentSite write FParentSite;

  published

    // custom properties

    {@prop Align - Alinhamento da imagem.
     @links TRLControlAlign. :/}
    property Align: TRLControlAlign read FAlign write SetAlign default faClient;

    {@prop Arrange - Arranjo da imagem.
     @links TRLImageArrange. :/}
    property Arrange: TRLImageArrange read FArrange write SetArrange default baAligned;

    {@prop AutoSize - Redimensionamento automático da imagem. :/}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;

    {@prop Height - Altura da imagem. :/}
    property Height: Integer read FHeight write SetHeight default 40;

    {@prop Stretch - Esticamento da imagem. :/}
    property Stretch: Boolean read FStretch write SetStretch default False;

    {@prop Width - Largura da imagem. :/}
    property Width: Integer read FWidth write SetWidth default 40;

    {@prop Picture - Imagem de fundo. :/}
    property Picture: TPicture read FPicture write SetPicture;
  end;

  {/@class}


  {@class TRLDegradeEffect - Efeito de transição de cores no fundo de um site.
                             Determina as cores origem e destino e a direção do efeito.
   @links TRLCustomSite.Degrade, TRLBackground. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDegradeEffect = class(TPersistent)
  private

    // variables

    FParentSite: TRLCustomSite;
    FOppositeColor: TColor;
    FDirection: TRLDegradeDirection;
    FGranularity: Integer;

    // assign methods

    procedure SetDirection(const AValue: TRLDegradeDirection);
    procedure SetOppositeColor(const AValue: TColor);
    procedure SetGranularity(const AValue: Integer);

    // custom methods

    procedure PaintTo(ACanvas: TCanvas; ARect: TRect; AColor: TColor); overload;
    procedure PaintTo(ASurface: TRLGraphicSurface; ARect: TRect;
      AColor: TColor); overload;

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomSite);


    {@prop ParentSite - Referência ao site sobre o qual o efeito será desenhado.
     @links TRLCustomSite. :/}
    property ParentSite: TRLCustomSite read FParentSite;
    destructor Destroy; override;

  published

    // custom properties

    {@prop Direction - Direção do efeito.
     @links TRLDegradeDirection. :/}
    property Direction: TRLDegradeDirection
      read FDirection write SetDirection default ddNone;

    {@prop OppositeColor - Cor oposta. :/}
    property OppositeColor: TColor
      read FOppositeColor write SetOppositeColor default clBlack;

    {@prop Granularity - Distância entre os tons do efeito. :/}
    property Granularity: Integer read FGranularity write SetGranularity default 1;
  end;

  {/@class}


  {@type TRLSortedBandTypes - Tipos das bands sortedadas.
   @links TRLSortedBands. :}
  TRLSortedBandTypes = array[btHeader..btFooter] of record
    List: TList;
    Printed: Boolean;
  end;
  {/@type}

  {@class TRLSortedBands - Propriedades para atribuição de Bands a CustomSkippers.
                           Determina as Bands incluidas pelos seus tipos bem como controla os tipos de Bands já
                           impressos no ParentSkipper.
   @links TRLCustomPager.SortedBands, TRLSortedBandTypes. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLSortedBands = class(TPersistent)
  private

    // variables

    FTypes: TRLSortedBandTypes;

    // assign methods

    function GetList(AType: TRLBandType): TList;
    function GetPrinted(AType: TRLBandType): Boolean;
    procedure SetPrinted(AType: TRLBandType; AValue: Boolean);

  public

    // constructors & destructors

    constructor Create;
    destructor Destroy; override;

    // custom methods

    {@method Add - Adiciona banda ou controle semelhante.
     @links TRLCustomSite. :/}
    procedure Add(ABand: TRLCustomSite);

    {@method Clear - Limpa a lista. :/}
    procedure Clear;

    {@method ResetPage - Reseta os flags de impresso para bands não title. :/}
    procedure ResetPage;

    {@method ResetAll - Reseta os flags de impresso para todas as bands. :/}
    procedure ResetAll;

    // custom properties

    {@prop List - Referência para lista de bands do tipo informado.
     @links TRLBandType. :/}
    property List[AType: TRLBandType]: TList read GetList;

    {@prop Printed - Flag de impresso para bands do tipo informado.
     @links TRLBandType. :/}
    property Printed[AType: TRLBandType]: Boolean read GetPrinted write SetPrinted;
  end;

  {/@class}
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCompositeOptions = class(TPersistent)
  private
    FParentReport: TRLCustomReport;
    FResetPageNumber: Boolean;
    FNumberPages: Boolean;
  public
    constructor Create(AOwner: TRLCustomReport);
    procedure Assign(Source: TRLCompositeOptions); reintroduce;
  published
    property ResetPageNumber: Boolean read FResetPageNumber
      write FResetPageNumber default False;
    property NumberPages: Boolean read FNumberPages write FNumberPages default True;
  end;

  { TRLPreviewOptions }

  {@type TRLPreviewOptionsDefaults - Uso dos defaults no preview padrão.
   Pode assumir um dos seguintes valores:
   pdUseDefaults - Utilizar as mesmas opções deixadas pelo último preview;
   pdIgnoreDefaults - Utilizar as opções definidas na prop PreviewOptions.
   @links TRLPreviewOptions. :/}
  TRLPreviewOptionsDefaults = (pdUseDefaults, pdIgnoreDefaults);

  {@class TRLPreviewOptions - Opções do form de preview padrão para um componente TRLReport em particular.
   @links TRLCustomReport.PreviewOptions, TRLPreviewOptionsDefaults. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLPreviewOptions = class(TPersistent)
  private

    // variables

    FParentReport: TRLCustomReport;
    FDefaults: TRLPreviewOptionsDefaults;
    FShowModal: Boolean;
    FFormStyle: TFormStyle;
    FPosition: TPosition;
    FWindowState: TWindowState;
    FBorderIcons: TBorderIcons;
    FHelpFile: string;
    FHelpContext: Integer;
    FCaption: TCaption;

    // assign methods

    function IsCaption: Boolean;

  public

    // constructors & destructors

    constructor Create(AOwner: TRLCustomReport);

    {@method Assign - Inicializa propriedades a partir de um outro objeto. :/}
    procedure Assign(Source: TRLPreviewOptions); reintroduce;

    // custom properties

    {@prop ParentReport - Referência ao report.
     @links TRLCustomReport. :/}
    property ParentReport: TRLCustomReport read FParentReport write FParentReport;

  published

    // custom properties

    {@prop WindowState - Indica o estado inicial da janela de preview. :/}
    property WindowState: TWindowState
      read FWindowState write FWindowState default wsMaximized;

    {@prop Position - Indica a posição da janela de preview. :/}
    property Position: TPosition read FPosition write FPosition default poScreenCenter;

    {@prop FormStyle - Indica o estilo da janela de preview. :/}
    property FormStyle: TFormStyle read FFormStyle write FFormStyle default fsNormal;

    {@prop ShowModal - Indica se a janela de preview será modal. :/}
    property ShowModal: Boolean read FShowModal write FShowModal default False;

    {@prop BorderIcons - Seleciona os botões da janela de preview. :/}
    property BorderIcons: TBorderIcons
      read FBorderIcons write FBorderIcons default [biSystemMenu, biMinimize, biMaximize];

    {@prop HelpFile - Nome do arquivo de help para a janela preview, se houver. :/}
    property HelpFile: string read FHelpFile write FHelpFile;

    {@prop HelpContext - Contexto de help para a janela preview, se houver. :/}
    property HelpContext: Integer read FHelpContext write FHelpContext default 0;

    {@prop Caption - Título da janela de preview. :/}
    property Caption: TCaption read FCaption write FCaption stored IsCaption;

    {@prop Defaults - Indica como estas configurações serão utilizadas pelo form de preview.
     @links TRLPreviewOptionsDefaults. :/}
    property Defaults: TRLPreviewOptionsDefaults
      read FDefaults write FDefaults default pdUseDefaults;
  end;

  {/@class}


  // CUSTOM Components

  { TRLCustomControl }

  {@class TRLCustomControl - Super classe da qual derivam todos os controles do FortesReport.
   @ancestor TCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomControl = class(TCustomControl)
  private

    // variables

    FPreparingCaption: TCaption;

    // property variables

    FAfterPrint: TRLAfterPrintEvent;
    FAlign: TRLControlAlign;
    FAlignment: TRLTextAlignment;
    FAutoSize: Boolean;
    FAutoSizeDir: TRLAutoSizeDirSet;
    FAutoExpand: Boolean;
    FAutoTrunc: Boolean;
    FAnchors: TRLControlAnchors;
    FBorders: TRLBorders;
    FHolder: TRLCustomControl;
    FHoldStyle: TRLHoldStyle;
    FHolderOffset: TPoint;
    FSecondHolder: TRLCustomControl;
    FSecondHoldStyle: TRLHoldStyle;
    FSecondHolderOffset: TPoint;
    FHoldeds: TList;
    FPeekBoundsRect: TRect;
    FRealBounds: TRLRealBounds;
    FCaption: TCaption;
    FLayout: TRLTextLayout;
    FControlState: TRLControlState;
    FBehavior: TRLControlBehavior;
    FTransparent: Boolean;
    FOldBoundsRect: TRect;
    FFixedSize: TPoint;
    FSizeFixed: Boolean;
    FFriendlyName: string;
    FCouldPrint: Boolean;

    // property variables

    FOnMeasureHeight: TNotifyEvent;

    // assign methods

    procedure SetAlign(const AValue: TRLControlAlign);
    procedure SetAnchors(const AValue: TRLControlAnchors);
    procedure SetAutoExpand(const AValue: Boolean);
    procedure SetHolder(const AValue: TRLCustomControl);
    procedure SetHoldStyle(const AValue: TRLHoldStyle);
    procedure SetSecondHolder(const AValue: TRLCustomControl);
    procedure SetSecondHoldStyle(const AValue: TRLHoldStyle);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetCaption(const AValue: TCaption);
    procedure SetAlignment(const AValue: TRLTextAlignment);
    procedure SetAutoTrunc(const AValue: Boolean);
    procedure SetLayout(const AValue: TRLTextLayout);
    procedure SetBorders(const AValue: TRLBorders);
    procedure SetRealBounds(const AValue: TRLRealBounds);
    procedure SetClientHeight(const Value: Integer);
    procedure SetClientWidth(const Value: Integer);
    procedure SetClientSize(const Value: TPoint);
    procedure SetFriendlyName(const Value: string);
    function GetDefaultCaption: TCaption;

    // custom methods

    function IsFriendlyName: Boolean;
    function IsCaption: Boolean;

  protected

    // property variables

    FBeforeText: TRLBeforeTextEvent;
    FBeforePrint: TRLBeforePrintEvent;

    {@method CustomControlPrint - Imprimir como TRLCustomControl. :/}
    procedure CustomControlPrint;

    {@method CustomControlPaint - Desenha como TRLCustomControl. :/}
    procedure CustomControlPaint;

    // override & reintroduce

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RequestAlign; override;
    function GetClientRect: TRect; override;
    procedure SetName(const Value: TComponentName); override;
{$ifdef CLX}
    procedure SetParent(const AParent: TWidgetControl); override;
{$else}
    procedure SetParent(AParent: TWinControl); override;
{$endif}
    procedure Paint; override;

    {@method SetAutoSize - SetAutoSize estendido. :/}
    procedure SetAutoSize(const AValue: Boolean); reintroduce;

    // assign methods

    {@method GetCaption - Retorna o caption dependendo do estado do relatório. :/}
    function GetCaption: TCaption;

    {@method GetMasterReport - Retorna referência ao relatório principal da cadeia após busca recursiva através das props Parent e PriorReport.
     Se não encontrar, retorna nil.
     @links TRLCustomReport, TRLCustomReport.NextReport, TRLCustomReport.PriorReport. :/}
    function GetMasterReport: TRLCustomReport;
    function GetLastReport: TRLCustomReport;

    {@method GetClientHeight - Retorna a altura da área cliente.
     @links ClientRect. :/}
    function GetClientHeight: Integer;

    {@method GetClientWidth - Retorna a largura da área cliente.
     @links ClientRect. :/}
    function GetClientWidth: Integer;

    // static methods

    {@method AdjustToParentFrame - Ajusta as dimensões do controle pai, se este for um TFrame. :/}
    procedure AdjustToParentFrame(var ALeft, ATop, AWidth, AHeight: Integer);

    {@method AdjustToFixedSize - Ajusta coordenadas de acordo com as dimensões estabelecidas no método CalcSize.
     @links CalcSize. :/}
    procedure AdjustToFixedSize(var ALeft, ATop, AWidth, AHeight: Integer);

    {@method AdjustToHolder - Ajusta coordenadas de acordo com o holder.
     @links TRLCustomControl, Holder, SecondHolder. :/}
    procedure AdjustToHolder(AHolder: TRLCustomControl;
      var ALeft, ATop, AWidth, AHeight: Integer);

    {@method OriginalSetBounds - Corresponde ao método SetBounds original sem efeitos colaterais de alinhamento estendido. :/}
    procedure OriginalSetBounds(ALeft, ATop, AWidth, AHeight: Integer);

    {@method CanSetWidth - Indica se é possível para o usuário determinar uma largura aleatória para o controle.
     Em determinadas circunstâncias dependendo do alinhamento, autosize ou holder, não é possível modificar as dimensões.
     @links AutoSize, Align, Holder. :/}
    function CanSetWidth: Boolean;

    {@method CanSetHeight - Indica se é possível para o usuário determinar uma altura aleatória para o controle.
     Em determinadas circunstâncias dependendo do alinhamento, autosize ou holder, não é possível modificar as dimensões.
     @links AutoSize, Align, Holder. :/}
    function CanSetHeight: Boolean;

    {@method ExpandParentSite - Ajusta as dimensões do controle pai. :/}
    procedure ExpandParentSite;

    {@method AdjustAlignment - Ajusta as dimensões do controle respeitando o seu alinhamento. :/}
    procedure AdjustAlignment(var ARect: TRect);

    {@method DoAfterPrint - Invoca o evento AfterPrint.
                            Não utilize o método diretamente. Ele invoca o evento AfterPrint do controle após a sua impressão. :/}
    procedure DoAfterPrint;

    {@method DoBeforePrint - Invoca o evento BeforePrint.
                             Não utilize o método diretamente. Ele invoca o evento BeforePrint do controle antes da sua impressão. :/}
    procedure DoBeforePrint(var APrintIt: Boolean);

    {@method DoBeforeText - Invoca o evento BeforePrint.
                            Não utilize o método diretamente. Ele invoca o evento BeforePrint do controle antes da sua impressão. :/}
    procedure DoBeforeText(var AText: string; var APrintIt: Boolean);

    {@method DoOnMeasureHeight - Invoca o evento OnMeasureHeight.
                                 Não utilize o método diretamente. Ele invoca o evento OnMeasureHeight do controle na horas das medições de página. :/}
    procedure DoOnMeasureHeight;

    {@method GetMadeCaption - Produz e retorna o Caption.
     @links MakeCaption. :/}
    function GetMadeCaption: string;

    {@method MakeCaption - Produz o Caption.
     @links GetMadeCaption. :/}
    procedure MakeCaption;

    {@method RealignHoldeds - Ajusta cotroles "agarrados".
     @links Hold, Holdeds. :/}
    procedure RealignHoldeds;

    {@method Hold - Agarra controle. :/}
    procedure Hold(AControl: TRLCustomControl; APlace: Integer);

    {@method Unhold - Libera controle agarrado. :/}
    procedure Unhold(AControl: TRLCustomControl);
    procedure CheckParent(var AControl: TWinControl);
    function IsPreparing: Boolean;

    {@method IsBallast - O controle está sendo impresso como um lastro.
                         Quando o parentpager está imprimindo bands em branco para preencher o espaço da página, ou
                         quando o parentskipper foi instruído a saltar um registro, o controle é dito lastro. :/}
    function IsBallast: Boolean;

    // dynamic methods
    {@method CanPrint - Intervenção antes da impressão.
     Não utilize CanPrint diretamente. Este método é disparado automaticamente pelo painel sempre antes de
     sua impressão. Este método invoca o evento BeforePrint, dentro do qual se pode mudar características
     do painel como: tamanho, cor, etc., além de decidir se ele será impresso ou não.
     Nota: Paineis não visíveis ou desabilitados não dispararão este método.
     O tamanho do painel será restaurado automaticamente após a sua impressão. :/}
    function CanPrint: Boolean; dynamic;

    {@method CalcSize - Cacula o tamanho do controle. :/}
    procedure CalcSize(var ASize: TPoint); dynamic;

    {@method DrawBounds - Desenha bordas.
     Não utilize este método diretamente. Ele é disparado automaticamente para que sejam impressas as bordas
     ao redor do panel. :/}
    procedure DrawBounds; dynamic;

    {@method CalcWastedPixels - Margens dispensadas do controle. :/}
    function CalcWastedPixels: TRect; dynamic;

    {@method CalcPrintClientRect - Retângulo com coordenadas relativas à linha corrente da página. :/}
    function CalcPrintClientRect: TRect; dynamic;

    {@method CalcPrintBoundsRect - Retângulo com coordenadas relativas ao parentreport. :/}
    function CalcPrintBoundsRect: TRect; dynamic;

    {@method CalcPrintSizeRect - Cacula o tamanho do controle para fins de impressão. :/}
    function CalcPrintSizeRect: TRect; dynamic;

    {@method CalcSizeRect - Cacula o tamanho do controle. :/}
    function CalcSizeRect: TRect; dynamic;

    {@method SetClientRect - Estabelece as dimensões do controle descontando margens etc. :/}
    procedure SetClientRect(const AValue: TRect); virtual;

    {@method InternalMakeCaption - Produz Caption. :/}
    function InternalMakeCaption: string; dynamic;

    {@method Initialize - Inicializa os acumuladores internos.
     Inicializa os acumuladores internos do controle em questão e de seus controles filhos.
     Estes acumuladores podem ser contadores de registros, totalizadores de campos numéricos
     e informações de estatística. :/}
    procedure Initialize; dynamic;

    {@method ComputeDetail - Computar novo Detail.
     Não utilize este método diretamente. Ele é invocado sempre que uma band de detalhe é impressa para
     que controles de contabilidade e estatística possam computar seus valores. O controle repassa a chamada
     para seus controles filhos em cascata. :/}
    procedure ComputeDetail(ACaller: TObject); dynamic;

    {@method InternalPrint - Processa o controle para impressão. :/}
    procedure InternalPrint; dynamic;

    {@method InternalMeasureHeight - Mede a altura da band de acordo com seu conteúdo, tentando predizer as
     quebras de folha que virão. :/}
    procedure InternalMeasureHeight; dynamic;

    {@method GetAttribute - Devolve o valor do controle como um Variant.
     Este valor é arbitrário e depende da classe que implementa o método. :/}
    function GetAttribute(const AName: string): Variant; virtual;

    {@method SetAttribute - Modifica o valor do controle. :/}
    function SetAttribute(const AName: string; AValue: Variant): Boolean; virtual;

{$ifdef CLX}
    procedure ColorChanged; override;
    procedure FontChanged; override;
{$else}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
{$endif}

    {@method PrepareStatics - Prepara os controles filhos do painel antes de imprimí-los.
     Esta operação consiste em invocar os eventos BeforePrint de cada controle, dando oportunidade para o
     redimensionamento antes de renderizar todos os controles. :/}
    procedure PrepareStatics;

    {@method PrintStatics - Desenha os controles filhos do painel sobre a sua superfície. :/}
    procedure PrintStatics;

    property CouldPrint: Boolean read FCouldPrint write FCouldPrint;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // override methods

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // static methods

    {@method IsMeasurable - Indica se o controle pode sofrer predicção sobre a sua altura. :/}
    function IsMeasurable: Boolean;

    {@method MeasureHeight - Mede a altura do controle. :/}
    procedure MeasureHeight;

    {@method PushBoundsRect - Guarda as dimensões do controle. :/}
    procedure PushBoundsRect;

    {@method PopBoundsRect - Restaura as dimensões do controle. :/}
    procedure PopBoundsRect;

    {@method Print - Gera imagem do controle para impressão.
     Gera imagem do controle junto com seus controles filhos e dispara os eventos BeforePrint e AfterPrint. :/}
    procedure Print;

    {@method FindParentSite - Referência ao site pai. Retorna referência ao site pai após busca dinâmica pela prop Parent.
     @links TRLCustomSite. :/}
    function FindParentSite: TRLCustomSite;

    {@method FindParentBand - Referência à band pai. Retorna referência à band pai após busca dinâmica pela prop Parent.
     @links TRLCustomBand. :/}
    function FindParentBand: TRLCustomBand;

    {@method FindParentGroup - Referência ao grupo pai. Retorna referência ao grupo pai após busca dinâmica pela prop Parent.
     @links TRLCustomGroup. :/}
    function FindParentGroup: TRLCustomGroup;

    {@method FindParentPager - Referência ao Pager pai. Retorna referência ao parentpager pai após busca dinâmica pela prop Parent.
     @links TRLCustomPager. :/}
    function FindParentPager: TRLCustomPager;

    {@method FindParentSkipper - Referência à skipper pai. Retorna referência ao skipper pai após busca dinâmica pela prop Parent.
     @links TRLCustomSkipper. :/}
    function FindParentSkipper: TRLCustomSkipper;

    {@method FindParentReport - Referência ao relatório pai. Retorna referência ao relatório pai após busca dinâmica pela prop Parent.
     @links TRLCustomReport. :/}
    function FindParentReport: TRLCustomReport;

    {@method RequestParentPager - Referência ao Pager pai. Gera exceção se não encontrar.
     @links TRLCustomPager. :/}
    function RequestParentPager: TRLCustomPager;

    {@method RequestParentSkipper - Referência à skipper pai. Gera exceção se não encontrar.
     @links TRLCustomSkipper. :/}
    function RequestParentSkipper: TRLCustomSkipper;

    {@method RequestParentSurface - Referência à skipper pai. Gera exceção se não encontrar.
     @links TRLGraphicSurface. :/}
    function RequestParentSurface: TRLGraphicSurface;

    {@method RequestParentReport - Referência ao report pai. Gera exceção se não encontrar.
     @links TRLCustomReport. :/}
    function RequestParentReport: TRLCustomReport;

    {@method Realign - Força o realinhamento do controle dentro de seu control pai. :/}
    procedure Realign; reintroduce;

    {@method RealignControls - Realinha os controles dentro deste de acordo com suas props. :/}
    procedure RealignControls; dynamic;

    {@method AdjustBounds - Ajusta coordenadas e tamanho. :/}
    procedure AdjustBounds;

    // dynamic methods

    {@method FindParentSurface - Superfície de desenho pai.
     Referência à superfície de desenho do painel pai.
     @links TRLGraphicSurface. :/}
    function FindParentSurface: TRLGraphicSurface; dynamic;

    // custom properties

    {@prop Anchors - Ancoramento estendido. Propriedade estendida de ancoragem de controles.
     @links TRLControlAnchors. :/}
    property Anchors: TRLControlAnchors read FAnchors write SetAnchors default [];

    {@prop Align - Alinhamento estendido. Propriedade estendida de alinhamento de controles.
     @links TRLControlAlign. :/}
    property Align: TRLControlAlign read FAlign write SetAlign default faNone;

    {@prop Alignment - Especifica como o texto deve ser alinhado dentro de um controle.
     @links TRLTextAlignment. :/}
    property Alignment: TRLTextAlignment
      read FAlignment write SetAlignment default taLeftJustify;

    {@prop AutoSize - Redimensionamento automático. Determina se o controle irá se
     redimensionar automaticamente de acordo com o seu conteúdo. :/}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;

    {@prop AutoSizeDir - Determina em que direções o controle poderá efetuar o redimensionamento automático.
     @links TRLAutoSizeDirSet. :/}
    property AutoSizeDir: TRLAutoSizeDirSet
      read FAutoSizeDir write FAutoSizeDir default [];

    {@prop AutoExpand - Determina se o controle fará a expansão de acordo com o seu conteúdo. :/}
    property AutoExpand: Boolean read FAutoExpand write SetAutoExpand default False;

    {@prop AutoTrunc - Determina se o tamanho do controle depende do conteúdo impresso. :/}
    property AutoTrunc: Boolean read FAutoTrunc write SetAutoTrunc default False;

    {@prop Behavior - Comportamento do controle. Utilize Behavior para definir o comportamento do controle sob diversos aspectos.
     @links TRLControlBehavior. :/}
    property Behavior: TRLControlBehavior read FBehavior write FBehavior default [];

    {@prop Caption - Texto a imprimir. :/}
    property Caption: TCaption read GetCaption write SetCaption stored IsCaption;

    {@prop FriendlyName - Nome amigável para uso com o ExpressionParser e interface com o usuário final. :/}
    property FriendlyName: string
      read FFriendlyName write SetFriendlyName stored IsFriendlyName;

    {@prop HoldStyle - Estilo de ancoragem. Define as regras de ancoragem entre dois controles.
     @links TRLHoldStyle. :/}
    property HoldStyle: TRLHoldStyle read FHoldStyle write SetHoldStyle default
      hsAsColumn;

    {@prop HolderOffset - Distância do ancoradouro. :/}
    property HolderOffset: TPoint read FHolderOffset write FHolderOffset;

    {@prop Layout - Layout do texto. Define o posicionamento vertical do texto no controle.
     @links TRLTextLayout. :/}
    property Layout: TRLTextLayout read FLayout write SetLayout default tlTop;

    {@prop SecondHoldStyle - Estilo de ancoragem ao segundo ancoradouro.
     @links TRLHoldStyle. :/}
    property SecondHoldStyle: TRLHoldStyle read FSecondHoldStyle
      write SetSecondHoldStyle default hsAsColumn;

    {@prop SecondHolderOffset - Distância ao segundo ancoradouro. :/}
    property SecondHolderOffset: TPoint read FSecondHolderOffset
      write FSecondHolderOffset;

    {@prop Transparent - Transparência do controle em tempo de impressão.
     Utilize Transparent quando for necessário imprimir apenas o conteúdo do painel.
     Um painel normalmente sobrepõe qualquer imagem ou efeito que estiver por trás dele.
     Quando o painel é transparente não possui uma cor de preenchimento, preservando a
     imagem ou efeitos desenhados no painel pai. :/}
    property Transparent: Boolean read FTransparent write SetTransparent default True;

    // internal custom properties

    {@prop ControlState - Estado do controle dentre as diversas atividades.
     @links TRLControlState. :/}
    property ControlState: TRLControlState read FControlState write FControlState;

    {@prop OldBoundsRect - Contém as últimas dimensões do controle antes da última alteração. :/}
    property OldBoundsRect: TRect read FOldBoundsRect write FOldBoundsRect;

    {@prop PeekBoundsRect - Contém as dimensões originais do controle salvas antes da sua impressão. :/}
    property PeekBoundsRect: TRect read FPeekBoundsRect write FPeekBoundsRect;

    // indirections

    {@prop ClientHeight - Determina ou indica a altura da área cliente. :/}
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;

    {@prop ClientRect - Retângulo da área cliente.
     Retorna retângulo contendo as coordenadas da área cliente do controle.
     A área cliente corresponde ao retângulo Rect(0,0,Width,Height), deduzido das bordas. :/}
    property ClientRect: TRect read GetClientRect write SetClientRect;

    {@prop ClientWidth - Determina ou indica a largura da área cliente. :/}
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;

    // links

    {@prop Holder - Controle referência para ancoragem.
     O mecanismo por trás da prop holder é um dos recursos mais interessantes do FortesReport. Esta prop
     aponta para um controle que servirá como âncora, como referência de posicionamento.
     É possível informar para um RLDBText de uma band detalhe que sua posição horizontal deve se
     mantêr sempre igual ao RLLabel correspondente no cabeçalho, indicando RLDBText.Holder:=RLLabel.
     Deste modo, ao mover o label do cabeçalho, em tempo de design ou impressão, o RLDBText será
     movido junto com ele.
     Há várias opções de ancoragem e também há a possibilidade de um controle possuir dois
     holders: um para referência horizontal e outro para vertical, por exemplo.
     @links TRLCustomControl, HoldStyle, SecondHolder. :/}
    property Holder: TRLCustomControl read FHolder write SetHolder;

    {@prop SecondHolder - Segundo controle referência de ancoragem. Define um outro controle para referência de ancoragem.
     @links TRLCustomControl, SecondHoldStyle, Holder. :/}
    property SecondHolder: TRLCustomControl read FSecondHolder write SetSecondHolder;

    // agregates

    {@prop Borders - Bordas ao redor do controle.
     Utilize Borders para exibir bordas ao redor do painel. As bordas serão exibidas entre as margens
     exteriores e interiores do painel. É possível informar que lados serão exibidos, a largura das linhas,
     o nível de qualidade, a cor e etc.
     @links TRLBorders. :/}
    property Borders: TRLBorders read FBorders write SetBorders;

    {@prop RealBounds - Configuração do tamanho real de um controle em milímetros ou polegadas.
     Nota: Esta funcionalizade não está implementada.
     @links TRLRealBounds. :/}
    property RealBounds: TRLRealBounds read FRealBounds write SetRealBounds;

    // readonly

    {@prop Holdeds - Lista de controles "agarrados". Contém a lista dos controles que orientam suas posições relativamente às coordenadas deste. :/}
    property Holdeds: TList read FHoldeds;

    {@prop MasterReport - Relatório mestre.
     Retorna referência ao componente TRLReport do relatório mestre ao qual o painel pertence.
     A pesquisa é feita dinamicamente a cada chamada e utiliza a propriedade Parent.
     Nota: O FortesReport permite a composição de relatórios através de concatenação.
     Esta propriedade deve retornar uma referência ao primeiro relatório da composição, do
     qual se pode extrair informações comuns a todos os relatórios, como: número de páginas,
     tamanho do papel, etc.
     @links TRLCustomReport. :/}
    property MasterReport: TRLCustomReport read GetMasterReport;
    property LastReport: TRLCustomReport read GetLastReport;

    // events

    {@event AfterPrint - Após a impressão. Ocorre exatamente após o controle ter sua imagem impressa no relatório.
     @links TRLAfterPrintEvent. :/}
    property AfterPrint: TRLAfterPrintEvent read FAfterPrint write FAfterPrint;

    {@event OnMeasureHeight - Na hora de medir a altura. :/}
    property OnMeasureHeight: TNotifyEvent read FOnMeasureHeight write FOnMeasureHeight;

    // standard properties

    {@prop ParentColor - Herança de cor. Define se o controle deve herdar a cor do controle pai. :/}
    property ParentColor default True;

    {@prop ParentFont - Herança de fonte. Define se o controle deve herdar a fonte do controle pai. :/}
    property ParentFont default True;

    {@prop Visible - Determina se o controle será visível em tempo de impressão.
     Com esta propriedade configurada para False o controle será ignorado em tempo de impressão e nenhum evento
     ligado a ele será disparado. :/}
    property Visible;

    {@prop Color - Cor do controle. Define a cor de fundo do controle. :/}
    property Color;

    {@prop Font - Fonte do controle. Define a fonte do controle. :/}
    property Font;
  end;

  {/@class}


  { TRLCustomDBControl }

  {@class TRLCustomDBControl - Classe base da qual se pode derivar controles de impressão dataware.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDBControl = class(TRLCustomControl)
  private

    // variables

    FDataField: TRLDataFieldProperty;
    FDataSource: TDataSource;

    // assign methods

    function GetField: TField;
    function GetDataSet: TDataSet;
    procedure SetDataField(const AValue: TRLDataFieldProperty);
    procedure SetDataSource(const AValue: TDataSource);

  protected

    // override methods

    function InternalMakeCaption: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom properties

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property DataField: TRLDataFieldProperty read FDataField write SetDataField;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;

    // readonly
    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property Field: TField read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property DataSet: TDataSet read GetDataSet;
  end;

  {/@class}


  { TRLCustomLabel }

  {@class TRLCustomLabel - Classe base da qual derivam todas as caixas de texto.
   Utilize descendentes do TRLCustomLabel para imprimir textos estáticos ou dinâmicos sobre o relatório.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomLabel = class(TRLCustomControl)
  protected

    // override methods

    procedure CalcSize(var ASize: TPoint); override;
    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // override methods

    procedure Paint; override;

    // custom properties

    {@prop AutoSize - Redimensionamento automático.
     Determina se a label irá se redimensionar automaticamente de acordo com o tamanho do seu Caption. :/}
    property AutoSize default True;

    {@prop Caption - Texto a ser impresso no corpo do label. :/}
    property Caption;

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou
     suspender a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property BeforePrint: TRLBeforeTextEvent read FBeforeText write FBeforeText;
  end;

  {/@class}


  { TRLCustomAngleLabel }

  {@class TRLCustomAngleLabel - Caixa de texto com rotação por ângulo.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomAngleLabel = class(TRLCustomControl)
  private

    // variables

    FAngle: Double;
    FAngleBorders: Boolean;

    // assign methods

    procedure SetAngle(const AValue: Double);
    procedure SetAngleBorders(const AValue: Boolean);
    function IsAngle: Boolean;

  protected

    // override methods

    procedure CalcSize(var ASize: TPoint); override;
    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // override methods

    procedure Paint; override;

    // custom properties

    {@prop Angle - Ângulo de inclinação.
     Determina o ângulo de inclinação no desenho do texto. :/}
    property Angle: Double read FAngle write SetAngle stored IsAngle;

    {@prop AngleBorders - Funcionalidade não implementada. :/}
    property AngleBorders: Boolean
      read FAngleBorders write SetAngleBorders default False;

    // events

    {@event BeforePrint - Antes da impressão.
     Ocorre antes da impressão do controle para alterar o texto ou suspender a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property BeforePrint: TRLBeforeTextEvent read FBeforeText write FBeforeText;

    {@prop AutoSize - Redimensionamento automático.
     Determina se a label irá se redimensionar automaticamente de acordo com o tamanho do seu Caption. :/}
    property AutoSize default True;
  end;

  {/@class}


  { TRLCustomDBText }

  {@class TRLCustomDBText - Classe base da qual podem derivar caixas de texto dataware.
   @ancestor TRLCustomLabel. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDBText = class(TRLCustomLabel)
  private

    // variables

    FText: TCaption;
    FDataField: TRLDataFieldProperty;
    FDataFormula: string;
    FDataSource: TDataSource;
    FDisplayMask: string;

    // assign methods

    function GetField: TField;
    function GetFieldLabel: string;
    function GetDataSet: TDataSet;
    procedure SetDataField(const AValue: TRLDataFieldProperty);
    procedure SetDataFormula(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);

    // custom methods

    function ApplyMask(const AValue: Variant): string;

  protected

    // override & reintroduce

    function InternalMakeCaption: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetText(const AValue: TCaption); reintroduce;

    // dynamic

    function GetFieldText: string; dynamic;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom properties

    {@prop Text - Texto auxiliar. Este texto deverá ser impresso junto com o conteúdo do campo. :/}
    property Text: TCaption read FText write SetText;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property DataField: TRLDataFieldProperty read FDataField write SetDataField;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property DataFormula: string read FDataFormula write SetDataFormula;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;

    {@prop DisplayMask - Mascara de formatação. :/}
    property DisplayMask: string read FDisplayMask write FDisplayMask;

    // readonly

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property Field: TField read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property DataSet: TDataSet read GetDataSet;
  end;

  {/@class}


  { TRLCustomDBResult }

  TRLDBResultBuiltIn = class
  public
    Id: Integer;
    Count: Integer;
    Max: Variant;
    Min: Variant;
    Sum: Double;
    First: Variant;
    Last: Variant;
  end;

  {@class TRLCustomDBResult - Caixa de texto para resultado de cáculos matemáticos com campos de um dataset.
   @ancestor TRLCustomDBText. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDBResult = class(TRLCustomDBText)
  private

    // variables

    FCount: Integer;
    FMax: Variant;
    FMin: Variant;
    FSum: Double;
    FInfo: TRLResultInfo;
    FFirst: Variant;
    FLast: Variant;
    FFirstText: string;
    FLastText: string;
    FSimple: Variant;
    FOnCompute: TRLOnComputeEvent;
    FNullValue: Variant;
    FResetAfterPrint: Boolean;
    FMustResetValue: Boolean;
    FBuiltInRegs: TObjectList;
    FComputeNulls: Boolean;

    // assign methods

    procedure SetInfo(const AValue: TRLResultInfo);
    function GetValue: Variant;

    // builtin methods

    function BuiltIn(AId: Integer; ACanCreate: Boolean = True): TRLDBResultBuiltIn;
    function BuiltInCount(AId: Integer): Variant;
    function BuiltInSum(AId: Integer; AValue: Variant): Variant;
    function BuiltInMin(AId: Integer; AValue: Variant): Variant;
    function BuiltInMax(AId: Integer; AValue: Variant): Variant;
    function BuiltInAvg(AId: Integer; AValue: Variant): Variant;
    function BuiltInFirst(AId: Integer; AValue: Variant): Variant;
    function BuiltInLast(AId: Integer; AValue: Variant): Variant;

    procedure Evaluate(var FieldText: string; var FieldValue: Variant);
    procedure InitializeRegs;

  protected

    // override methods

    function GetFieldText: string; override;
    procedure Initialize; override;
    procedure ComputeDetail(ACaller: TObject); override;
    procedure InternalPrint; override;
    function GetAttribute(const AName: string): Variant; override;

    {@method Resolve - Avalia uma função built-in. :/}
    function Resolve(Sender: TObject; const Identifier: string;
      Params: Variant): Variant;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // properties

    {@prop Info - Tipo de informação.
     @links TRLResultInfo. :/}
    property Info: TRLResultInfo read FInfo write SetInfo default riSimple;

    {@prop ResetAfterPrint - Zerar os acumuladores após a impressão. :/}
    property ResetAfterPrint: Boolean read FResetAfterPrint
      write FResetAfterPrint default False;

    // readonly

    {@prop Value - Valor parcial. :/}
    property Value: Variant read GetValue;

    {@prop ComputeNulls - Indica se campos com valor nulo serão computados. :/}
    property ComputeNulls: Boolean read FComputeNulls write FComputeNulls default True;

    // events

    {@event OnCompute - Ocorre durante os cálculos estatísticos para validação do valor a ser computado.
     @links TRLOnComputeEvent. :/}
    property OnCompute: TRLOnComputeEvent read FOnCompute write FOnCompute;
  end;

  {/@class}


  { TRLCustomSystemInfo }

  {@class TRLCustomSystemInfo - Caixa de texto com informações de sistema.
   @ancestor TRLCustomLabel. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomSystemInfo = class(TRLCustomLabel)
  private

    // variables

    FInfoType: TRLInfoType;
    FText: TCaption;
    fPrintEndTextOnNextReport: Boolean;

    // assign methods

    procedure SetInfoType(const AValue: TRLInfoType);
    function JunctionStr: string;
    function MendStr: string;

  protected

    // override & reintroduce

    function InternalMakeCaption: string; override;
    procedure SetText(const AValue: TCaption); reintroduce;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom properties

    {@prop Info - Tipo de informação.
     @links TRLInfoType. :/}
    property Info: TRLInfoType read FInfoType write SetInfoType default itDate;

    {@prop Text - Texto auxiliar. :/}
    property Text: TCaption read FText write SetText;
  published
    property PrintEndTextOnNextReport: Boolean read fPrintEndTextOnNextReport write fPrintEndTextOnNextReport default false;
  end;

  {/@class}


  { TRLCustomMultiLine }

  {@class TRLCustomMultiLine - Classe base para controles multilinhas.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomMultiLine = class(TRLCustomControl)
  private
    FWordWrap: Boolean;
    FIntegralHeight: Boolean;
    procedure SetWordWrap(const AValue: Boolean);
  protected
    procedure CalcSize(var ASize: TPoint); override;
    procedure InternalPrint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    {@prop WordWrap - Quebra automática de linha.
     Determina se quebras automáticas de linha deverão ser inseridas de modo a encaixar o texto de acordo com a
     largura do controle. :/}
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    {@prop IntegralHeight - Altura integral das linhas.
     Determina se as linhas que excederem a área cliente do controle serão exibidas. :/}
    property IntegralHeight: Boolean
      read FIntegralHeight write FIntegralHeight default False;
    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou suspender
     a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property BeforePrint: TRLBeforeTextEvent read FBeforeText write FBeforeText;
    {@prop AutoSize - Redimensionamento automático. Determina se o memo irá se redimensionar automaticamente de
     acordo com o tamanho do seu texto. :/}
    property AutoSize default True;
  end;

  {/@class}


  { TRLCustomMemo }

  {@class TRLCustomMemo - Classe base para caixa de texto multilinhas.
   @ancestor TRLCustomMultiLine. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomMemo = class(TRLCustomMultiLine)
  private

    // variables

    FLines: TStrings;

    // assign methods

    procedure SetLines(const AValue: TStrings);

    // event handlers

    procedure TreatOnChange(Sender: TObject);

  protected

    // override methods

    function InternalMakeCaption: string; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // custom properties

    {@prop Lines - Lista contendo as linhas de texto do memo. :/}
    property Lines: TStrings read FLines write SetLines;
  end;

  {/@class}


  { TRLCustomDBMemo }

  {@class TRLCustomDBMemo - Classe base para caixa de texto multilinhas ligado a campo de dataset.
   @ancestor TRLCustomMultiLine. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDBMemo = class(TRLCustomMultiLine)
  private

    // variables

    FDataField: TRLDataFieldProperty;
    FDataFormula: string;
    FDataSource: TDataSource;

    // assign methods

    function GetField: TField;
    function GetFieldLabel: string;
    function GetDataSet: TDataSet;
    procedure SetDataField(const AValue: TRLDataFieldProperty);
    procedure SetDataFormula(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);

  protected

    // override methods

    function InternalMakeCaption: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetFieldText: string;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom properties

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property Field: TField read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property DataSet: TDataSet read GetDataSet;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property DataField: TRLDataFieldProperty read FDataField write SetDataField;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property DataFormula: string read FDataFormula write SetDataFormula;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

  {/@class}


  { TRLCustomImage }

  {@class TRLCustomImage - Classe base para caixa de imagem.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomImage = class(TRLCustomControl)
  private

    // variables

    FPicture: TPicture;
    FStretch: Boolean;
    FCenter: Boolean;
    FScaled: Boolean;

    // assign methods

    procedure SetCenter(const AValue: Boolean);
    procedure SetPicture(const AValue: TPicture);
    procedure SetStretch(const AValue: Boolean);
    procedure SetScaled(const AValue: Boolean);

    // custom methods

    procedure PictureChanged(Sender: TObject);

  protected

    // event handlers

    procedure CalcSize(var ASize: TPoint); override;
    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // override methods

    procedure Paint; override;

    // custom properties

    {@prop Center - Centralização da imagem.
     Determina se a imagem deve ser posicionada ao centro da área cliente. :/}
    property Center: Boolean read FCenter write SetCenter default False;

    {@prop Stretch - Esticamento da imagem.
     Indica se a imagem deve ser esticada de modo a preencher totalmente a área cliente do controle. :/}
    property Stretch: Boolean read FStretch write SetStretch default False;

    {@prop Scaled - Esticamento proporcional.
     Indica se a imagem deve ser esticada de modo a preencher área cliente do controle mantendo a mesma proporção
     de altura e largura. :/}
    property Scaled: Boolean read FScaled write SetScaled default False;

    // objects

    {@prop Picture - Representa a imagem que aparece no fundo do controle. :/}
    property Picture: TPicture read FPicture write SetPicture;

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificar a imagem ou
     suspender a sua impressão.
     @links TRLBeforePrintEvent. :/}
    property BeforePrint: TRLBeforePrintEvent read FBeforePrint write FBeforePrint;
  end;

  {/@class}


  { TRLCustomDBImage }

  {@class TRLCustomDBImage - Classe base para caixa de imagem ligada a campo de dataset.
   @ancestor TRLCustomImage. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDBImage = class(TRLCustomImage)
  private

    // variables

    FDataField: TRLDataFieldProperty;
    FDataSource: TDataSource;

    // assign methods

    function GetField: TField;
    function GetDataSet: TDataSet;
    procedure SetDataField(const AValue: TRLDataFieldProperty);
    procedure SetDataSource(const AValue: TDataSource);

    // custom methods

    procedure LoadPicture;

  protected

    // override methods

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom properties

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property Field: TField read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property DataSet: TDataSet read GetDataSet;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property DataField: TRLDataFieldProperty read FDataField write SetDataField;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

  {/@class}


  { TRLCustomDraw }

  {@type TRLDrawKind - Tipo de figura geométrica para o componente TRLDraw.
   Pode assumir um dos seguintes valores:
   dkRectangle - Desenha um retângulo ou um quadrado;
   dkLine - Desenha uma linha reta;
   dkTriangle - Desenha um triângulo;
   dkElipse - Desenha uma elipse ou um círculo;
   dkArrow - Desenha uma seta simples;
   dkCustom - Desenha um polígono cujos pontos são definidos na prop DrawData.
   @links TRLDraw, TRLCustomDraw.DrawData. :/}
  TRLDrawKind = (dkRectangle, dkLine, dkTriangle, dkElipse, dkArrow, dkCustom);

  {@type TRLDrawOptions - Opções para desenho de figuras do RLDraw.
   Pode ser um conjunto dos seguintes valores:
   doKeepAspectRatio - A relação entre largura e altura da figura deve ser mantida;
   doKeepSize - O tamanho da figura será o original (mesmo do ângulo zero) para qualquer ângulo escolhido;
   doKeepVisible - A figura terá um tamanho que permita que ela seja vista inteira em qualquer ângulo escolhido.
   @links TRLDraw, TRLCustomDraw.DrawData. :}
  TRLDrawOption = (doKeepAspectRatio, doKeepSize, doKeepVisible);
  TRLDrawOptions = set of TRLDrawOption;
  {/@type}

  {@class TRLCustomDraw - Classe base para caixa de desenho de figuras geométricas.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDraw = class(TRLCustomControl)
  private

    // variables

    FAngle: Double;
    FBrush: TBrush;
    FDrawKind: TRLDrawKind;
    FPen: TPen;
    FDrawData: TStrings;
    FCenter: Boolean;
    FDrawWidth: Integer;
    FDrawHeight: Integer;
    FOptions: TRLDrawOptions;

    // assign methods

    procedure SetAngle(const AValue: Double);
    procedure SetBrush(const AValue: TBrush);
    procedure SetDrawKind(const AValue: TRLDrawKind);
    procedure SetPen(const AValue: TPen);
    procedure SetDrawData(const Value: TStrings);
    procedure SetCenter(const Value: Boolean);
    procedure SetDrawHeight(const Value: Integer);
    procedure SetDrawWidth(const Value: Integer);
    procedure SetOptions(const Value: TRLDrawOptions);

    procedure ReadKind(Reader: TReader);

    // event handlers

    procedure ChangeResponse(Sender: TObject);

    // custom methods

    function IsAngle: Boolean;
    function IsDrawData: Boolean;
    function IsDrawSize: Boolean;
    procedure ProducePoints(var ADest: TPointArray);
    procedure ScaleToFit(var APoints: TPointArray; const ARect: TRect);

  protected

    // override methods

    procedure InternalPrint; override;
    procedure DefineProperties(Filer: TFiler); override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // override methods

    procedure Paint; override;

    // custom properties

    {@prop Angle - Ângulo de rotação da figura. :/}
    property Angle: Double read FAngle write SetAngle stored IsAngle;

    {@prop DrawKind - Tipo de figura geométrica.
     @links TRLDrawKind. :/}
    property DrawKind: TRLDrawKind read FDrawKind write SetDrawKind default dkRectangle;

    // agregates

    {@prop Brush - Cor e padrão de preenchimento da figura. :/}
    property Brush: TBrush read FBrush write SetBrush;

    {@prop Pen - Cor e estilo dos traçoes usados no desenho da figura. :/}
    property Pen: TPen read FPen write SetPen;

    {@prop DrawData - Lista de coordenadas para desenho do polígono.
     A lista é uma sequência de números inteiros separados por espaços. Cada par de números
     representa a coordenada absoluta de um ponto do polígono. Todos os pontos serão
     ligados. O primeiro e o último fecharão o polígono. O polígono será desenhado e preenchido
     de acordo com as props Pen e Brush. :/}
    property DrawData: TStrings read FDrawData write SetDrawData stored IsDrawData;

    {@prop Center - A figura deve ser centralizada na área cliente. :/}
    property Center: Boolean read FCenter write SetCenter default True;

    {@prop DrawWidth - Largura da figura em pixels. Quando não informada, fica valendo a largura do componente. :/}
    property DrawWidth: Integer read FDrawWidth write SetDrawWidth stored IsDrawSize;

    {@prop DrawHeight - Altura da figura em pixels. Quando não informada, fica valendo a altura do componente. :/}
    property DrawHeight: Integer read FDrawHeight write SetDrawHeight stored IsDrawSize;

    {@prop Options - Determina várias opções de desenho da figura. @links TRLDrawOptions. :/}
    property Options: TRLDrawOptions read FOptions write SetOptions default [];

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificar a imagem
                          ou suspender a sua impressão.
     @links TRLBeforePrintEvent. :/}
    property BeforePrint: TRLBeforePrintEvent read FBeforePrint write FBeforePrint;
  end;

  {/@class}


  { TRLCustomSite }

  {@class TRLCustomSite - Classe base da qual derivam todos os paineis de impressão como: TRLBand, TRLPanel,
   TRLGroup e o próprio TRLReport. Derive a partir do TRLCustomSite para criar qualquer painel customizado.
   Nota: Descendentes do TRLCustomSite podem conter controles e outros paineis.
   @links TRLPanel, TRLBand, TRLGroup, TRLReport.
   @ancestor TRLCustomControl. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomSite = class(TRLCustomControl)
  private

    // variables

    FBackground: TRLBackground;
    FDegrade: TRLDegradeEffect;
    FInsideMargins: TRLMargins;
    FMargins: TRLMargins;
    FSurface: TRLGraphicSurface;
    FPrintPosition: TPoint;
    FPrintSize: TPoint;

    // events

    FOnDraw: TRLOnDrawEvent;

    // assign methods

    procedure SetBackground(const AValue: TRLBackground);
    procedure SetDegrade(const AValue: TRLDegradeEffect);
    procedure SetInsideMargins(const AValue: TRLMargins);
    procedure SetMargins(const AValue: TRLMargins);

    procedure DrawFrame(Rect: TRect; AColor: TColor; ARound: Boolean);
    procedure DrawTracks;
    procedure DrawUnusedRect(Rect: TRect);
    procedure InvalidateAll;
    procedure Signup(const ASignature: string; ABig: Boolean = False);
    // calc
    function CalcClientPixels: TRect;
    function CalcBordersPixels: TRect;
    function CalcBordersRect: TRect;
    function CalcMarginalRect: TRect;
    function CalcPrintBordersRect: TRect;
    function CalcPrintClientPixels: TRect;
    function CalcPrintMarginalRect: TRect;
    function CalcPrintWastedPixels: TRect;
    function CalcPrintWastedPixelsSum: TRect;
    function CalcGlobalPrintPosition: TPoint;
    function CalcPrintBordersPixels: TRect;
    function CalcPrintMarginalPixels: TRect;
  protected

    // override & reintroduce

    procedure Loaded; override;
    procedure CalcSize(var ASize: TPoint); override;

    {@method GetClientRect - Margens externas do painel.
     Retorna retângulo contendo as coordenadas da área cliente do painel.
     A área cliente corresponde ao retângulo (0,0,Width,Height), deduzido
     das margens externas, internas e das bordas. :/}
    function GetClientRect: TRect; override;

    function CanPrint: Boolean; override;
    function CalcWastedPixels: TRect; override;
    function CalcPrintClientRect: TRect; override;
    function CalcPrintSizeRect: TRect; override;
    function CalcPrintBoundsRect: TRect; override;
    procedure SetClientRect(const AValue: TRect); override;
    procedure DrawBounds; override;
    procedure InternalPrint; override;
    procedure InternalMeasureHeight; override;

    {@method AlignControls - Alinha os controles filhos. Não utilize este método diretamente.
     Ele provoca o alinhamento os controles filhos do panel segundo a propriedade estendida Align de cada controle
     através do método AlignControls e prossegue recursivamente. :}
    procedure AlignControls(ARect: TRect); reintroduce; overload;
    procedure AlignControls(AControl: TControl; var Rect: TRect); overload; override;
    {/@method}

    {@method DoOnDraw - Invoca o evento OnDraw. Não utilize este método diretamente.
     Ele é invocado durante a impressão do panel para permitir que um desenho qualquer seja feito em sua superfície. :/}
    procedure DoOnDraw(ASurface: TRLGraphicSurface; ARect: TRect);

    // dynamic methods

    {@method SurfaceOpening - Uma nova superfície de impressão está sendo aberta.
     Local ideal para inicializações relativas à página ou sequência de dados. :/}
    procedure SurfaceOpening; dynamic;

    {@method SurfaceBeginDraw - Os controles estão sendo desenhados na nova superfície de desenho. :/}
    procedure SurfaceBeginDraw; dynamic;

    {@method SurfaceOpened - A superfície de impressão foi aberta e os controles estáticos já foram desenhados. :/}
    procedure SurfaceOpened; dynamic;

    {@method WriteSurface - A superfície de impressão está pronta para a rotina de trabalho, se houver. :/}
    procedure WriteSurface; dynamic;

    {@method SurfaceEndDraw - Os controles estáticos que dependem do tamanho do site e os de finalização estão sendo desenhados. :/}
    procedure SurfaceEndDraw; dynamic;

    {@method SurfaceClosed - A superfície já foi fechada e agora deverá ser acumulada na superfície do controle pai. :/}
    procedure SurfaceClosed; dynamic;

    {@method TruncateSurface - O desenho da superfície já foi terminado e sua altura definitiva deve ser determinada. :/}
    procedure TruncateSurface; dynamic;

    {@method MarkPrintPosition - Primeira marcação da linha/coluna e dimensões de impressão. :/}
    procedure MarkPrintPosition; dynamic;

    {@method ThrowSurface - Procede a transferência e posicionamento da superfície de impressão sobre a superfície do controle pai. :/}
    procedure ThrowSurface; dynamic;

    {@method PrepareBackgroundSurface - Prepara a superfície de desenho do controle pai antes da relocação.
     Neste momento o controle está ciente do sua posição e tamanho finais e deve providenciar a preparação da
     superfície do controle pai.
     @links TRLGraphicSurface. :/}
    procedure PrepareBackgroundSurface(ABackgroundSurface: TRLGraphicSurface;
      const ARect: TRect); dynamic;

    procedure DrawClient; dynamic;

    {@method DrawBackground - Desenha imagem de fundo.
     Não utilize este método diretamente. Ele desenha a imagem definida em Background no fundo do painel. :/}
    procedure DrawBackground(const ARect: TRect); dynamic;

    function CalcEffectiveRect: TRect; dynamic;
    function CalcMarginalPixels: TRect; dynamic;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // override methods

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Paint; override;
    procedure Initialize; override;
    procedure ComputeDetail(ACaller: TObject); override;

    // static methods

    {@method OpenSurface - Cria uma nova superfície de desenho e inicializa-a.
     Não utilize este método diretamente. Esté método é invocado pelo o método Print. :/}
    procedure OpenSurface;

    {@method CloseSurface - Fecha superfície de desenho e envia-a para o panel pai.
     Não utilize este método diretamente. Esté método é invocado após o método Print. Ele fecha a superfície de
     desenho e a repassa para o panel pai para ser devidamente posicionada. :/}
    procedure CloseSurface;

    procedure RealignControls; override;

    // agregates

    {@prop Background - Imagem para o fundo do painel.
     Utilize Background para colocar uma imagem no fundo do painel. A imagem deve ser um bitmap ou icone e pode ser
     disposta de várias formas de acordo com a propriedade Arrange.
     @links TRLBackground. :/}
    property Background: TRLBackground read FBackground write SetBackground;

    {@prop Degrade - Efeito de transição de cores no fundo do painel.
     Utilize Degrade para produzir o efeito de transição de cores no fundo do painel. Pode-se configurar as cores
     origem e destino, bem como a direção e a qualidade do efeito.
     @links TRLDegradeEffect. :/}
    property Degrade: TRLDegradeEffect read FDegrade write SetDegrade;

    {@prop InsideMargins - Margens internas do painel.
     Utilize InsideMargins quando for necessário posicionar os controles dentro do painel com um afastamento lateral
     dentro do retângulo definido por Margins e Borders.
     @links TRLMargins. :/}
    property InsideMargins: TRLMargins read FInsideMargins write SetInsideMargins;

    {@prop Margins - Margens externas do painel.
     Utilize Margins quando for necessário posicionar os controles dentro do painel com um afastamento lateral ou para
     reduzir o retângulo das bordas.
     @links TRLMargins. :/}
    property Margins: TRLMargins read FMargins write SetMargins;

    // events

    {@event OnDraw - Na hora de desenhar o fundo do site.
     @links TRLOnDrawEvent. :/}
    property OnDraw: TRLOnDrawEvent read FOnDraw write FOnDraw;

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificá-lo ou suspender
     sua impressão.
     @links TRLBeforePrintEvent. :/}
    property BeforePrint: TRLBeforePrintEvent read FBeforePrint write FBeforePrint;

    // readonly

    {@prop Surface - Superfície de desenho.
     @links TRLGraphicSurface. :/}
    property Surface: TRLGraphicSurface read FSurface;

    // standard properties

    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
  end;

  {/@class}


  { TRLCustomPanel }

  {@class TRLCustomPanel - Classe base para containers de controles.
   Utilize um TRLCustomPanel como container para controles ou outros paineis.
   @ancestor TRLCustomSite. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomPanel = class(TRLCustomSite)
  protected

    // override methods

    procedure DrawBounds; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
  end;

  {/@class}


  { TRLCustomBandSet }

  {@class TRLCustomBandSet - Classe base para criação de bands.
   @ancestor TRLCustomSite. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomBandSet = class(TRLCustomSite)
  private
    FBandSets: TList;
    FIntegralHeight: Boolean;
    function FindParentBandSet: TRLCustomBandSet;
  protected
    procedure SurfaceOpened; override;
    procedure SurfaceClosed; override;
    procedure SurfaceBeginDraw; override;
    procedure AddBandSet(ABandSet: TRLCustomBandSet);
    function CountBandSet(ABandSet: TRLCustomBandSet): Integer;
    property IntegralHeight: Boolean
      read FIntegralHeight write FIntegralHeight default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsFirstBandSet: Boolean;
    function BandSetCount: Integer;
  end;

  {/@class}

  {@type TRLBandOption - Opção para formatação e comportamento de uma band.
   Pode assumir um dos seguintes valores:
   boOptimisticPageBreak - Quebra de página otimista. O cálculo de espaço para
   forçar a quebra de página é feito somente após a renderização da band. Assim,
   o usuário pode modificar a altura da band e interferir na decisão da quebra.
   @links TRLBand.Options. :/}
  TRLBandOption = (boOptimisticPageBreak);

  {@type TRLBandOptions - Conjunto de opções para formatação e comportamento de uma band.
   @links TRLBandOption. :/}
  TRLBandOptions = set of TRLBandOption;

  { TRLCustomBand }

  {@class TRLCustomBand - Classe base da qual derivam as bandas de impressão.
   Derive a partir da TRLCustomBand para criar bandas de impressão de dados.
   As bandas de impressão formam a base do algorítmo de paginação do FortesReport.
   @links TRLBand, TRLDetailGrid.
   @ancestor TRLCustomBandSet. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomBand = class(TRLCustomBandSet)
  private
    FBandType: TRLBandType;
    FComputable: Boolean;
    FPageBreaking: TRLPageBreaking;
    FCompletion: TRLCompletionType;
    FAlignToBottom: Boolean;
    FCarbonCopies: Integer;
    FCarbonIndex: Integer;
    FGroupIndex: Integer;
    FIntegralHeight: Boolean;
    FOptions: TRLBandOptions;
    FOnCompute: TRLOnDetailComputeEvent;
    FGreenBarPrint: Boolean;
    FGreenBarColor: TColor;
    procedure SetBandType(const AValue: TRLBandType);
    procedure SetCarbonCopies(const AValue: Integer);
    procedure SetGroupIndex(const AValue: Integer);
    procedure AdjustCarbonGroup;
    procedure AdjustFromCarbonGroup;
    procedure NotifyDataBandPrinted;
    function GetCompleting: Boolean;
    procedure CheckPageBreak;
    function MaxBandsReached: Boolean;
  protected
    procedure SurfaceClosed; override;
    {@method ThrowSurface - Procede a transferência e posicionamento da superfície de impressão sobre a superfície
     do controle pai.
     Determina a posição e as dimensões de impressão antes da relocação para o controle pai. :/}
    procedure ThrowSurface; override;
    {@method VerticalExceeded - O limite vertical foi excedido e uma atitude deve ser tomada.
     No caso das bands simples, a impressão para para uma nova página. :/}
    procedure VerticalExceeded; dynamic;
    procedure MarkPrintPosition; override;
    procedure InternalPrint; override;
    {@method HeightFits - A band cabe na página atual. Se não couber, aAvailableHeight representará o espaço disponível
     em pixels. :/}
    function HeightFits(AHeight: Integer; var AAvailable: Integer): Boolean; dynamic;
    {@method SkipToNextPosition - Move o cursor do parentpager para a posição da próxima band. :/}
    procedure SkipToNextPosition(AWidth, AHeight: Integer); dynamic;
    function GetBandTypeName: string; dynamic;
    {@method IsDataBand - Indica se a band é uma band de dados.
     Se o tipo da band é btDetail ou btSummary e ela não está sendo impressa como um lastro, então ela é uma band
     de dados.
     @links IsBallast, BandType. :/}
    function IsDataBand: Boolean;
    function CanCompute: Boolean;
  public
    FGreenBarFlag: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    {@prop Completing - Indica se a band está sendo impressa após o fim dos dados para completar o espaço que sobrou. :/}
    property Completing: Boolean read GetCompleting;
    {@prop CarbonIndex - Número da cópia da band. :/}
    property CarbonIndex: Integer read FCarbonIndex write FCarbonIndex;
    {@prop AlignToBottom - Alinhado a parte inferior da página.
     Força a band a ser impressa na parte inferior da página como se fosse um btFooter. :/}
    property AlignToBottom: Boolean
      read FAlignToBottom write FAlignToBottom default False;
    {@prop BandType - Define o comportamento da banda.
     Utilize a propriedade BandType para definir o comportamento da banda em relação aos dados impressos.
     @links TRLBandType. :/}
    property BandType: TRLBandType read FBandType write SetBandType default btDetail;
    {@prop CarbonCopies - Número de cópias da band. :/}
    property CarbonCopies: Integer read FCarbonCopies write SetCarbonCopies default 1;
    {@prop Completion - Tipo de preenchimento de página.
     @links TRLCompletionType. :/}
    property Completion: TRLCompletionType
      read FCompletion write FCompletion default ctNone;
    {@prop Computable - Indica se a band é válida para estatísticas. :/}
    property Computable: Boolean read FComputable write FComputable default True;
    {@prop GroupIndex - Agrupamento de bands. :/}
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    {@prop PageBreaking - Quebra de página.
     @links TRLPageBreaking. :/}
    property PageBreaking: TRLPageBreaking read FPageBreaking
      write FPageBreaking default pbNone;
    {@prop IntegralHeight - Determina se a band poderá ser exibida parcialmente.
     Se a band com o seu conteúdo não couber na página, a band poderá ser dividida em partes por página. :/}
    property IntegralHeight: Boolean
      read FIntegralHeight write FIntegralHeight default True;
    {@prop Options - Opções diversas de formatação e comportamento da band.
     @links TRLBandOptions. :/}
    property Options: TRLBandOptions read FOptions write FOptions default [];
    property OnCompute: TRLOnDetailComputeEvent read FOnCompute write FOnCompute;
    {@prop AutoExpand - Expansão automática de acordo com crescimento do conteúdo. :/}
    property AutoExpand default True;
    property GreenBarPrint:Boolean read FGreenBarPrint write FGreenBarPrint default False;
    property GreenBarColor: TColor read FGreenBarColor write FGreenBarColor default $00E5E5E5;
  end;

  {/@class}


  { TRLCustomDetailGrid }

  {@type TRLDetailGridOrganization - Organização para impressão das bandas.
   Pode assumir um dos seguintes valores:
   goInRows - Todas as bandas de uma linha são impressas antes de passar para a linha seguinte (padrão);
   goInColumns - As bandas são impressas verticalmente em coluna até o fim da página e então a impressão passa para
   o topo da próxima coluna.
   @links TRLDetailGrid. :/}
  TRLDetailGridOrganization = (goInRows, goInColumns);

  {@class TRLCustomDetailGrid - Classe base para bandas de detalhe multi-colunas.
   Banda de tipo fixado em btDetail. Ideal para a impressão de etiquetas e relatórios em colunas.
   @ancestor TRLCustomBand. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomDetailGrid = class(TRLCustomBand)
  private

    // variables

    FColIndex: Integer;
    FColCount: Integer;
    FColSpacing: Double;
    FColWidth: Double;
    FRowIndex: Integer;
    FTopRow: Integer;
    FBottomRow: Integer;
    FOrganization: TRLDetailGridOrganization;

    // assign methods

    procedure SetColCount(const AValue: Integer);
    procedure SetColSpacing(const AValue: Double);
    procedure SetColWidth(const AValue: Double);
    function GetClientCellRect(AColIndex, ARowIndex: Integer): TRect;



    function IsManyCols: Boolean;

  protected

    // override methods

    function GetBandTypeName: string; override;
    function CalcEffectiveRect: TRect; override;
    procedure MarkPrintPosition; override;
    procedure SurfaceOpening; override;
    procedure SurfaceClosed; override;

    {@method VerticalExceeded - O limite vertical foi excedido e uma atitude deve ser tomada.
     No caso do detailgrid, se a orientação for colbycol, então a impressão deve passar para uma nova coluna. :/}
    procedure VerticalExceeded; override;

    {@method HeightFits - A band cabe na página atual.
     Se não couber, aAvailableHeight representará o espaço disponível em pixels. :/}
    function HeightFits(AHeight: Integer; var AAvailable: Integer): Boolean; override;

    {@method SkipToNextPosition - Move o cursor do parentpager para a posição da próxima band na coluna à direita
     ou abaixo. :/}
    procedure SkipToNextPosition(AWidth, AHeight: Integer); override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // override methods

    procedure DrawClient; override;

    // custom methods

    procedure Initialize; override;

    // internal custom properties

    {@prop ColIndex - Índice da coluna imprimindo. :/}
    property ColIndex: Integer read FColIndex;

    {@prop ColCount - Total de colunas da grid. :/}
    property ColCount: Integer read FColCount write SetColCount default 1;

    {@prop ColSpacing - Espaço entre as colunas em milímetros. :/}
    property ColSpacing: Double read FColSpacing write SetColSpacing stored IsManyCols;

    {@prop ColWidth - Largura das colunas em milímetros. :/}
    property ColWidth: Double read FColWidth write SetColWidth stored IsManyCols;

    {@prop RowIndex - Índice da linha imprimindo. :/}
    property RowIndex: Integer read FRowIndex;

    {@prop Organization - Determina a direção para a impressão das bandas.
     @links TRLDetailGridOrganization. :/}
    property Organization: TRLDetailGridOrganization
      read FOrganization write FOrganization default goInRows;
  end;

  {/@class}


  { TRLCustomPager }

  {@type TRLPagerStatusType - Estado do Pager.
   Indica os estados que o Pager pode assumir.
   Pode ser um dos seguintes valores:
   psCompleting - Está completando a página com bands em branco. :/}
  TRLPagerStatusType = (psCompleting);

  {@type TRLPagerStatus - Conjunto de estados do Pager.
   Indica os trabalhos que o Pager está executando. :/}
  TRLPagerStatus = set of TRLPagerStatusType;

  {@class TRLCustomPager - Classe base para paginadores.
   Derive a partir da TRLCustomPager para criar controles de quebra de página.
   Os paginadores são containers para as bandas de impressão e controlam a quantidade de bandas que podem ser
   impressas por página.
   @links TRLReport, TRLSubDetail, TRLGroup.
   @ancestor TRLCustomBandSet. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomPager = class(TRLCustomBandSet)
  private
    FAllowedBands: TRLAllowedBands;
    FDetailCount: Integer;
    FSortedBands: TRLSortedBands;
    FMaxBands: Integer;
    FMinBands: Integer;
    FRelativePagerRow: Integer;
    FDetailsInSurface: Integer;
    FNewPageNeeded: Boolean;
    FPageBreaking: TRLPageBreaking;
    FJumpPending: Boolean;
    FJumpLength: Integer;
    FNewPageCaller: TObject;
    FForceMinBands: Boolean;
    FFooterMeasuring: TRLFooterMeasuring;
    FDataBandPrinted: Integer;
    FPagerStatus: TRLPagerStatus;
    function GetSummaryHeight: Integer;
    function GetSummaryHeightSum: Integer;
    function GetFooterHeight: Integer;
    function GetFooterHeightSum: Integer;
    function GetAlignedSummaryHeight: Integer;
    function GetColumnFooterHeight: Integer;
    function GetColumnFooterHeightSum: Integer;
    function GetAlignedSummaryHeightSum: Integer;
    function GetWastedBottomSum: Integer;
    function GetNewPageNeeded: Boolean;
    procedure SetAllowedBands(const AValue: TRLAllowedBands);
    function CreateChild(AType: TRLBandType): TRLCustomBand;
    function FindChild(AType: TRLBandType): TRLCustomBand;
    procedure KillChild(AType: TRLBandType);
    procedure SortBands;
    function IsSatisfied: Boolean;
    procedure InitializePageInfo;
  protected
    procedure SurfaceOpening; override;
    procedure TruncateSurface; override;
    procedure SurfaceClosed; override;
    procedure MarkPrintPosition; override;
    procedure SurfaceBeginDraw; override;
    procedure SurfaceEndDraw; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalBeginDoc; dynamic;
    procedure InternalEndDoc; dynamic;
    procedure InternalNewPage(ACaller: TObject; AMoveOnly: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure ComputeDetail(ACaller: TObject); override;
    function PrintBands(AType: TRLBandType): TRLPrintBandResults;
    procedure PrintBand(ABand: TRLCustomBand);
    procedure PrintDetails;
    procedure PrintHeaders;
    procedure PrintFooters(ASummarize: Boolean = False);
    procedure PrintCompletion;
    procedure PrintSite(ASite: TRLCustomSite);
    procedure PrintPagers(AClass: TRLPagerClassType);
    procedure MeasureFooters;
    function GetRelativeFooterRow(AConsiderAligned: Boolean): Integer;
    function GoFooterRow: Boolean;
    function GetRelativeSummaryRow(AConsiderAligned: Boolean): Integer;
    function GoSummaryRow: Boolean;
    function GetRelativeColumnFooterRowNoSummary(AConsiderAligned: Boolean): Integer;
    function GoColumnFooterRow: Boolean;
    procedure InvalidatePage;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    {@prop MaxBands - Número máximo de bands para o Pager. :/}
    property MaxBands: Integer read FMaxBands write FMaxBands default 0;
    {@prop MinBands - Número mínimo de bands para o Pager. :/}
    property MinBands: Integer read FMinBands write FMinBands default 0;
    {@prop PageBreaking - Quebra de página do Pager.
     @links TRLPageBreaking. :/}
    property PageBreaking: TRLPageBreaking read FPageBreaking
      write FPageBreaking default pbNone;
    {@prop AllowedBands - Tipos de bands inseridas.
     @links TRLAllowedBands. :/}
    property AllowedBands: TRLAllowedBands read FAllowedBands
      write SetAllowedBands default [];
    {@prop ForceMinBands - Forçar a quantidade mínima de bands. :/}
    property ForceMinBands: Boolean
      read FForceMinBands write FForceMinBands default False;
    {@prop FooterMeasuring - Antecipação do cálculo da altura dos rodapés.
     @links TRLFooterMeasuring. :/}
    property FooterMeasuring: TRLFooterMeasuring
      read FFooterMeasuring write FFooterMeasuring default fmNone;
    {@prop RelativePagerRow - Número da linha atual relativa ao Pager. :/}
    property RelativePagerRow: Integer read FRelativePagerRow write FRelativePagerRow;
    {@prop DetailsInSurface - Quantidade de detalhes impressos na página atual. :/}
    property DetailsInSurface: Integer read FDetailsInSurface write FDetailsInSurface;
    {@prop NewPageNeeded - Indica a necessidade de salto de página. :/}
    property NewPageNeeded: Boolean read GetNewPageNeeded write FNewPageNeeded;
    {@prop DataBandPrinted - Indica se alguma band de dados já foi impressa na página atual. :/}
    property DataBandPrinted: Integer read FDataBandPrinted write FDataBandPrinted;
    {@prop DetailCount - Número de bands de detalhe impressas desde o início da impressão. :/}
    property DetailCount: Integer read FDetailCount;
    {@prop SortedBands - Lista de bands agrupadas pelo tipo.
     @links TRLSortedBands. :/}
    property SortedBands: TRLSortedBands read FSortedBands;
    {@prop PagerStatus - Estado do Pager.
     Indica se o Pager está completando a página com bands vazias após o término dos dados.
     @links TRLPagerStatus. :/}
    property PagerStatus: TRLPagerStatus read FPagerStatus;
  end;

  {/@class}


  { TRLCustomGroup }

  {@class TRLCustomGroup - Classe base para sequências de registros de dados.
   Utilize descendentes do TRLCustomGroup para imprimir sequências de registros de dados.
   @ancestor TRLCustomPager. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomGroup = class(TRLCustomPager)
  private

    // variables

    FOnGetBreak: TRLOnGetBreakEvent;
    FDataFields: TRLDataFieldsProperty;
    FDataFormula: string;
    FLastKey: string;
    FBroken: Boolean;

    // assign methods

    function GetKey: string;
    function CheckBreak: Boolean;
    procedure SetDataFields(const Value: TRLDataFieldsProperty);
    procedure SetDataFormula(const Value: string);

  protected

    // override methods

    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // override methods

    procedure ComputeDetail(ACaller: TObject); override;
    procedure Paint; override;

    // custom properties

    {@prop DataFields - Campo ou conjunto de campos que determinam a quebra de sequência de registros.
     Informe os campos determinantes da quebra de sequência de registros. Os campos devem ser separados por
     ponto-e-vírgula ";". A quebra automatica é detectada através da comparação no conteúdo dos campos do último
     registro impresso com o atual.
     @links TRLDataFieldsProperty. :/}
    property DataFields: TRLDataFieldsProperty read FDataFields write SetDataFields;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. @links DataFields. :/}
    property DataFormula: string read FDataFormula write SetDataFormula;

    {@prop Enabled - Quebra de registros habilitada.
     Quando setada para False, esta propriedade desativa as quebras de sequência do grupo, porém sem interferir
     nos controles e grupos internos, que são impressos normalmente. :/}
    property Enabled;

    // events

    {@event OnGetBreak - Evento que determina da quebra de sequência de registros.
     Informe na implementação do evento OnGetBreak quando a quebra de sequência deverá ser efetuada. Sender é uma
     referência ao componente de grupo que originou a chamada. O parâmetro BreakIt deverá ser setado para True para
     que a quebra aconteça.
     Nota: Este evento é chamado a partir do segundo registro da sequência a ser impresso.
     @links TRLOnGetBreakEvent. :/}
    property OnGetBreak: TRLOnGetBreakEvent read FOnGetBreak write FOnGetBreak;
  end;

  {/@class}


  { TRLCustomSkipper }

  {@class TRLCustomSkipper - Classe base para Pager com fontes de dados.
   Derive a partir da TRLCustomSkipper para criar fontes de dados para as bandas.
   As fontes de dados, além de acumularem a função de paginadores, controlam a sequência de dados, automaticamente
   quando a fonte é uma DataSource, ou através de eventos de interação.
   @links TRLReport, TRLSubDetail.
   @ancestor TRLCustomPager. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomSkipper = class(TRLCustomPager)
  private

    // variables

    FRecordAction: TRLRecordAction;
    FDataSource: TDataSource;
    FOnDataCount: TRLOnDataCountEvent;
    FOnDataRecord: TRLOnDataRecordEvent;
    FOnNeedData: TRLOnNeedDataEvent;
    FDataEof: Boolean;
    FRecNo: Integer;
    FCopyNo: Integer;
    FRecordMoved: Boolean;
    FRecordRange: TRLRecordRange;
    FRangeCount: Integer;

    function IsNextNRecordRange: Boolean;

    // assign methods

    procedure SetDataSource(const AValue: TDataSource);

  protected

    // override methods

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // custom methods

    function DataCount: Integer; dynamic;
    procedure DataFirst; dynamic;
    procedure DataNext; dynamic;

    // custom properties

    {@prop DataSource - Referência ao DataSource de onde os registros serão obtidos. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;

    {@prop RecordRange - Indica a faixa de registros a processar. @links TRLRecordRange, RangeCount. :/}
    property RecordRange: TRLRecordRange
      read FRecordRange write FRecordRange default rrAllRecords;

    {@prop RangeCount - Indica a quantidade de registros a processar a partir do atual se a prop RecordRange for rrNextN. @links RecordRange. :/}
    property RangeCount: Integer
      read FRangeCount write FRangeCount stored IsNextNRecordRange;

    // internal custom properties

    {@prop RecordMoved - Indice se o registro foi movido por algum processo subsequente. :/}
    property RecordMoved: Boolean read FRecordMoved write FRecordMoved;

    // events

    {@event OnDataCount - Ao solicitar a quantidade de registros.
     @links TRLOnDataCountEvent. :/}
    property OnDataCount: TRLOnDataCountEvent read FOnDataCount write FOnDataCount;

    {@event OnDataRecord - Ao selecionar um registro a imprimir.
     @links TRLOnDataRecordEvent. :/}
    property OnDataRecord: TRLOnDataRecordEvent read FOnDataRecord write FOnDataRecord;

    {@event OnNeedData - Ao solicitar novos registros.
     @links TRLOnNeedDataEvent. :/}
    property OnNeedData: TRLOnNeedDataEvent read FOnNeedData write FOnNeedData;

    // readonly

    {@prop DataEof - Indica o final dos dados de entrada. :/}
    property DataEof: Boolean read FDataEof;

    {@prop RecordAction - Ação tomada para último registro.
     @links TRLRecordAction. :/}
    property RecordAction: TRLRecordAction read FRecordAction;

    {@prop RecNo - Número do registro atual. :/}
    property RecNo: Integer read FRecNo;

    {@prop CopyNo - Número da cópia da band atual. :/}
    property CopyNo: Integer read FCopyNo;
  end;

  {/@class}


  { TRLCustomSubDetail }

  {@class TRLCustomSubDetail - Mini relatório para relacionamentos tipo master/detail.
   Utilize os descendentes do TRLCustomSubDetail para imprimir registros ou sequências de dados relacionadas com
   os registros da fontes de dados principal. O controle de sub-detalhe é especialmente útil quando se quer listar
   registros de uma base que possui registros filhos ou relacionados (Master/Detail), aonde um TRLReport responderia
   pelos registros principais e o TRLSubDetail pelos registros filhos.
   @links TRLSubDetail.
   @ancestor TRLCustomSkipper. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomSubDetail = class(TRLCustomSkipper)
  private

    // variables

    FPositioning: TRLBandType;

    // assign methods

    procedure SetPositioning(const Value: TRLBandType);

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // override methods

    procedure Paint; override;

    // custom properties

    {@prop Positioning - Posicionamento do subdetail. Equivalente à prop BandType da TRLBand.
     @links TRLBandType. :/}
    property Positioning: TRLBandType
      read FPositioning write SetPositioning default btDetail;
  end;

  {/@class}

  {@type TRLPrintDialogParams - Intervalo de páginas a passar como default para o diálogo de impressão. :}
  TRLPrintDialogParams = record
    FromPage1: Integer;
    ToPage1: Integer;
    Selection: string;
  end;
  {/@type}

  { TRLCustomReport }

  {@class TRLCustomReport - Componente principal na confecção de relatórios.
   Utilize os descendentes do TRLCustomReport como ponto de partida na confecção de qualquer relatório com o
   FortesReport. Um componente TRLCustomReport pode listar registros de uma fonte de dados, solicitar os dados
   através de eventos em tempo de execução ou apenas imprimir páginas confeccionadas com os componentes da biblioteca.
   @links TRLReport.
   @ancestor TRLCustomSkipper. }
  TNextReportState=(nrStart,nrNext,nrEnd);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLCustomReport = class(TRLCustomSkipper)
  private

    // variables

    FParseInvoker: TObject;

    // property variables

    FOnPageEnding: TNotifyEvent;
    FOnPageStarting: TNotifyEvent;
    FCanceled: Boolean;
    FPages: TRLGraphicStorage;
    FPageSurface: TRLGraphicSurface;
    FNextReport: TRLCustomReport;
    FPriorReport: TRLCustomReport;
    FFirstPageNumber: Integer;
    FCurrentPageNumber: Integer;
    FPageIndex: Integer;
    FPageSetup: TRLPageSetup;
    FPrintDialog: Boolean;
    FPrintEmpty: Boolean;
    FPrinterMetrics: TRLPrinterMetrics;
    FReportState: TRLReportState;
    FShowDesigners: Boolean;
    FShowTracks: Boolean;
    FShowExplosion: Boolean;
    FTitle: string;
    FJobTitle: string;
    FReportDateTime: TDateTime;
    FDefaultFilter: TRLCustomPrintFilter;
    FExpressionParser: TRLExpressionParser;
    FShowProgress: Boolean;
    FPrintQuality: TRLPrintQuality;
    FOnFilterText: TRLBeforeTextEvent;
    FAdjustableMargins: Boolean;
    FPreviewOptions: TRLPreviewOptions;
    FForcePrepare: Boolean;
    FCompositeOptions: TRLCompositeOptions;
    FNextReportState: TNextReportState;
    FOnPrepareError: TRLPrepareErrorEvent;

    // Bobina
    FUnlimitedHeight: Boolean;

    // assign methods

    function GetPageNumber: Integer;
    procedure SetPriorReport(const AValue: TRLCustomReport);
    procedure SetNextReport(const AValue: TRLCustomReport);
    procedure SetShowDesigners(const AValue: Boolean);
    procedure SetShowTracks(const AValue: Boolean);
    procedure SetShowExplosion(const AValue: Boolean);
    procedure SetPrintQuality(const AValue: TRLPrintQuality);
    procedure SetDefaultFilter(const AValue: TRLCustomPrintFilter);
    procedure SetExpressionParser(const AValue: TRLExpressionParser);
    procedure SetAdjustableMargins(const AValue: Boolean);
    procedure SetPageSetup(const Value: TRLPageSetup);
    procedure SetPreviewOptions(const Value: TRLPreviewOptions);
    procedure SetCompositeOptions(const Value: TRLCompositeOptions);

    // custom events

    procedure ParserResource(Sender: TObject; const AIdentifier: string;
      AParams: Variant; var AResult: Variant);
    procedure ParserTokener(Sender: TObject; var AToken: string;
      var AKind: TRLParserTokenKind);
    procedure ParserFindAgregate(Sender: TObject; AOwner: TPersistent;
      const AName: string; var AAgregate: TPersistent);
    procedure ParserGetAttribute(Sender: TObject; AOwner: TPersistent;
      const AName: string; var AValue: Variant);
    procedure ParserSetAttribute(Sender: TObject; AOwner: TPersistent;
      const AName: string; const AValue: Variant; var AHandled: Boolean);

    // custom methods

    function GetOrientedUnprintablePixels: TRect;
    function GetOrientedUnprintableRect: TRect;
    procedure CreateProgress;
    procedure DestroyProgress;
    procedure ProgressCanceled(Sender: TObject; var CancelIt: Boolean);
    procedure DoPageStarting;
    procedure DoPageEnding;
    procedure DoFilterText(var AText: string; var APrintIt: Boolean);
    procedure CheckCloseSurface;
    procedure UpdateMacros;
    function CompositeIndex: Integer;
    procedure SetTitle(const Value: string);
    function IsJobTitle: Boolean;
    procedure SetJobTitle(const Value: string);
    procedure SetUnlimitedHeight(const Value: boolean);

  protected

    // override methods

    function CalcSizeRect: TRect; override;
    procedure SurfaceOpening; override;
    procedure SurfaceBeginDraw; override;
    procedure SurfaceEndDraw; override;
    procedure SurfaceClosed; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawBackground(const ARect: TRect); override;
    procedure CalcSize(var ASize: TPoint); override;
    function CalcMarginalPixels: TRect; override;
    procedure InternalPrint; override;

    // custom methods

    procedure BeforeSave;
    procedure AfterLoad;
    procedure ReloadPrinter;

  public

    // variables

    ProgressForm: TfrmRLFeedBack;
    PreviewClosed: Boolean;
    DialogParams: TRLPrintDialogParams;

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    // override methods

    procedure DataFirst; override;
    procedure DataNext; override;
    procedure Paint; override;
    function FindParentSurface: TRLGraphicSurface; override;

    // custom methods

    procedure Clear;
    function ShowPrintDialog: Boolean;
    function Prepare: Boolean;
    function Preview(Dest: TRLPreview = nil): Boolean;
    function PreviewModal: Boolean;
    procedure ClosePreview;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure Cancel;
    procedure SetProgressPhase(const APhase: string);
    procedure SetProgressSize(AMax: Integer);
    procedure StepProgress(AStep: Integer = 1);
    function Parse(Sender: TObject; const AExpression: string): Variant;

    // custom properties

    {@prop AdjustableMargins - Determina se as margens poderão ser aumentadas de acordo com a área não imprimível da
     impressora. :/}
    property AdjustableMargins: Boolean read FAdjustableMargins
      write SetAdjustableMargins default False;

    {@prop FirstPageNumber - Númeração para a primeira página. :/}
    property FirstPageNumber: Integer read FFirstPageNumber
      write FFirstPageNumber default 1;

    {@prop ForcePrepare - Indica se o relatório deve ser sempre preparado antes de imprimir ou visualizar.
     @links Prepare. :/}
    property ForcePrepare: Boolean read FForcePrepare write FForcePrepare default True;

    {@prop PrintDialog - Indica se um diálogo de seleção será exibido antes da impressão. :/}
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog default True;

    {@prop PrintEmpty - Indica se o relatório deve ser gerado e impresso mesmo que não haja registros a imprimir. :/}
    property PrintEmpty: Boolean read FPrintEmpty write FPrintEmpty default True;

    {@prop ShowDesigners - Exibir régua e delineadores dos controles em tempo de design. :/}
    property ShowDesigners: Boolean
      read FShowDesigners write SetShowDesigners default True;

    {@prop ShowTracks - Exibir régua em tempo de design. :/}
    property ShowTracks: Boolean read FShowTracks write SetShowTracks default True;

    {@prop ShowExplosion - Não implementada. :/}
    property ShowExplosion: Boolean
      read FShowExplosion write SetShowExplosion default False;

    {@prop UnlimitedHeight - Impressão sem quebra de pagina. :/}
    property UnlimitedHeight: Boolean
      read FUnlimitedHeight write SetUnlimitedHeight default False;

    {@prop Title - Título do relatório.
     Pode ser recuperado pelo componente TRLSystemInfo. :/}
    property Title: string read FTitle write SetTitle;

    {@prop JobTitle - Título do trabalho de impressão. Aparece na lista de trabalhos de impressão do sistema operacional. :/}
    property JobTitle: string read FJobTitle write SetJobTitle stored IsJobTitle;

    {@prop ShowProgress - Exibir barra de progresso. :/}
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;

    {@prop PrintQuality - Qualidade de impressão.
     @links TRLPrintQuality. :/}
    property PrintQuality: TRLPrintQuality read FPrintQuality
      write SetPrintQuality default pqFullFeature;

    {@prop ReportDateTime - Data e hora de impressão do relatório. :/}
    property ReportDateTime: TDateTime read FReportDateTime write FReportDateTime;

    // external

    {@prop DefaultFilter - Filtro padrão de impressão.
     @links TRLCustomPrintFilter. :/}
    property DefaultFilter: TRLCustomPrintFilter
      read FDefaultFilter write SetDefaultFilter;

    {@prop ExpressionParser - Referência para um objeto avaliador de expressões matemáticas.
     @links TRLExpressionParser. :/}
    property ExpressionParser: TRLExpressionParser
      read FExpressionParser write SetExpressionParser;

    {@prop PriorReport - Relatório anterior da composição.
     @links TRLCustomReport. :/}
    property PriorReport: TRLCustomReport read FPriorReport write SetPriorReport;

    {@prop NextReport - Relatório seguinte da composição.
     @links TRLCustomReport. :/}
    property NextReport: TRLCustomReport read FNextReport write SetNextReport;

    // internal custom properties

    {@prop PageIndex - Índice da página atual. :/}
    property PageIndex: Integer read FPageIndex;

    {@prop PageNumber - Número da página atual (FirstPageNumber+PageIndex). :/}
    property PageNumber: Integer read GetPageNumber;

    {@prop ReportState - Estado da preparação do relatório.
     @links TRLReportState. :/}
    property ReportState: TRLReportState read FReportState;

    // readonly

    {@prop Canceled - Indica se o relatório foi cancelado durante a preparação. :/}
    property Canceled: Boolean read FCanceled;

    // agregates

    {@prop PrinterMetrics - Dimensões do papel na impressora.
     @links TRLPrinterMetrics. :/}
    property PrinterMetrics: TRLPrinterMetrics read FPrinterMetrics;

    {@prop Pages - Lista de páginas preparadas.
     @links TRLGraphicStorage. :/}
    property Pages: TRLGraphicStorage read FPages;

    {@prop PageSetup - Configuração do papel.
     @links TRLPageSetup. :/}
    property PageSetup: TRLPageSetup read FPageSetup write SetPageSetup;

    {@prop PreviewOptions - Opções de pré-visualização.
     @links TRLPreviewOptions. :/}
    property PreviewOptions: TRLPreviewOptions
      read FPreviewOptions write SetPreviewOptions;

    property CompositeOptions: TRLCompositeOptions
      read FCompositeOptions write SetCompositeOptions;

    // events

    {@event OnPageEnding - Ao terminar uma página. :/}
    property OnPageEnding: TNotifyEvent read FOnPageEnding write FOnPageEnding;

    {@event OnPageStarting - No início de cada página. :/}
    property OnPageStarting: TNotifyEvent read FOnPageStarting write FOnPageStarting;

    {@event OnFilterText - Ao imprimir qualquer texto.
     Captura de textos antes do envio para a impressora.
     @links TRLBeforeTextEvent. :/}
    property OnFilterText: TRLBeforeTextEvent read FOnFilterText write FOnFilterText;

    {@event OnPrepareError - Ao ocorrer qualquer erro no método Prepare. :/}
    property OnPrepareError: TRLPrepareErrorEvent read FOnPrepareError write FOnPrepareError;

    // standard

    property ParentFont default False;
    property ParentColor default False;
    property Color default clWhite;

    property NextReportState: TNextReportState read FNextReportState;

  end;

  {/@class}


  // FINAL COMPONENTS

  { TRLLabel }

  {@class TRLLabel - Caixa de texto padrão.
   Utilize o TRLLabel para imprimir textos estáticos sobre o relatório.
   @icon TRLLabel.jpg
   @ancestor TRLCustomLabel.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLLabel = class(TRLCustomLabel)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Caption = ancestor /}
    property Caption;
    {@prop Color = ancestor /}
    property Color;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLAngleLabel }

  {@class TRLAngleLabel - Caixa de texto de com rotação por ângulo.
   @icon TRLAngleLabel.jpg
   @ancestor TRLCustomAngleLabel.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLAngleLabel = class(TRLCustomAngleLabel)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop Angle = ancestor /}
    property Angle;
    {@prop AngleBorders = ancestor /}
    property AngleBorders;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Caption = ancestor /}
    property Caption;
    {@prop Color = ancestor /}
    property Color;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDBText }

  {@class TRLDBText - Caixa de texto ligada a campo de dataset.
   @icon TRLDBText.jpg
   @ancestor TRLCustomDBText.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDBText = class(TRLCustomDBText)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop DataField = ancestor /}
    property DataField;
    {@prop DataFormula = ancestor /}
    property DataFormula;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop DisplayMask = ancestor /}
    property DisplayMask;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Text = ancestor /}
    property Text;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDBResult }

  {@class TRLDBResult - Caixa de texto de resultado de operações matemáticas ou estatíticas com campos de dataset.
   @icon TRLDBResult.jpg
   @ancestor TRLCustomDBResult.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDBResult = class(TRLCustomDBResult)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop ComputeNulls = ancestor /}
    property ComputeNulls;
    {@prop DataField = ancestor /}
    property DataField;
    {@prop DataFormula = ancestor /}
    property DataFormula;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop DisplayMask = ancestor /}
    property DisplayMask;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Info = ancestor /}
    property Info;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop ResetAfterPrint = ancestor /}
    property ResetAfterPrint;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Text = ancestor /}
    property Text;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnCompute = ancestor /}
    property OnCompute;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLSystemInfo }

  {@class TRLSystemInfo - Caixa de texto de com informações do sistema.
   @icon TRLSystemInfo.jpg
   @ancestor TRLCustomSystemInfo.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLSystemInfo = class(TRLCustomSystemInfo)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Info = ancestor /}
    property Info;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Text = ancestor /}
    property Text;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLMemo }

  {@class TRLMemo - Caixa de texto multilinhas.
   @icon TRLMemo.jpg
   @ancestor TRLCustomMemo.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLMemo = class(TRLCustomMemo)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop Lines = ancestor /}
    property Lines;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop WordWrap = ancestor /}
    property WordWrap;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDBMemo }

  {@class TRLDBMemo - Caixa de texto multilinhas ligada a campo de dataset.
   @icon TRLDBMemo.jpg
   @ancestor TRLCustomDBMemo.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDBMemo = class(TRLCustomDBMemo)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop DataField = ancestor /}
    property DataField;
    {@prop DataFormula = ancestor /}
    property DataFormula;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop WordWrap = ancestor /}
    property WordWrap;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLImage }

  {@class TRLImage - Caixa de imagem.
   @icon TRLImage.jpg
   @ancestor TRLCustomImage.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLImage = class(TRLCustomImage)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Center = ancestor /}
    property Center;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Picture = ancestor /}
    property Picture;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop Scaled = ancestor /}
    property Scaled;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property Stretch;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDBImage }

  {@class TRLDBImage - Caixa de imagem ligada a campo de dataset.
   @icon TRLDBImage.jpg
   @ancestor TRLCustomDBImage.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDBImage = class(TRLCustomDBImage)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Center = ancestor /}
    property Center;
    {@prop DataField = ancestor /}
    property DataField;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop Scaled = ancestor /}
    property Scaled;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property Stretch;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDraw }

  {@class TRLDraw - Caixa de desenho para figuras geométricas.
   As figuras podem ser de um tipo pré-determinado ou customizado pelo usuário.
   @icon TRLDraw.jpg
   @ancestor TRLCustomDraw.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDraw = class(TRLCustomDraw)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop Angle = ancestor /}
    property Angle;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Brush = ancestor /}
    property Brush;
    {@prop Center = ancestor /}
    property Center;
    {@prop Color = ancestor /}
    property Color;
    {@prop DrawData = ancestor /}
    property DrawData;
    {@prop DrawHeight = ancestor /}
    property DrawHeight;
    {@prop DrawKind = ancestor /}
    property DrawKind;
    {@prop DrawWidth = ancestor /}
    property DrawWidth;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop Options = ancestor /}
    property Options;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop Pen = ancestor /}
    property Pen;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLPanel }

  {@class TRLPanel - Container para controles.
                     Utilize o TRLPanel como container para controles ou outros paineis.
   @icon TRLPanel.jpg
   @ancestor TRLCustomPanel.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLPanel = class(TRLCustomPanel)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoExpand = ancestor /}
    property AutoExpand;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Background = ancestor /}
    property Background;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop Holder = ancestor /}
    property Holder;
    {@prop HoldStyle = ancestor /}
    property HoldStyle;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnDraw = ancestor /}
    property OnDraw;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLBand }

  {@class TRLBand - Banda de impressão.
   Utilize a banda de impressão para representar registros de dados ou quebras de sequências de dados. Ela deve ser
   colocada dentro de um Report, Group ou SubDetail.
   O comportamento da banda é controlado através da propriedade BandType.
   @icon TRLBand.jpg
   @ancestor TRLCustomBand.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLBand = class(TRLCustomBand)
  published
    property GreenBarPrint;
    property GreenBarColor;


    // properties

    {@prop AlignToBottom = ancestor /}
    property AlignToBottom;
    {@prop AutoExpand = ancestor /}
    property AutoExpand;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Background = ancestor /}
    property Background;
    {@prop BandType = ancestor /}
    property BandType;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop CarbonCopies = ancestor /}
    property CarbonCopies;
    {@prop Color = ancestor /}
    property Color;
    {@prop Completion = ancestor /}
    property Completion;
    {@prop Computable = ancestor /}
    property Computable;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop GroupIndex = ancestor /}
    property GroupIndex;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop Options = ancestor /}
    property Options;
    {@prop PageBreaking = ancestor /}
    property PageBreaking;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;


    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;

  {/@class}


  { TRLDetailGrid }

  {@class TRLDetailGrid - Banda de detalhe multi-colunas.
   Banda de tipo fixo btDetail. Ideal para a impressão de etiquetas e relatórios em colunas.
   @icon TRLDetailGrid.jpg
   @ancestor TRLCustomDetailGrid.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLDetailGrid = class(TRLCustomDetailGrid)
  published

    // properties

    {@prop AutoExpand = ancestor /}
    property AutoExpand;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Background = ancestor /}
    property Background;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop ColCount = ancestor /}
    property ColCount;
    {@prop Color = ancestor /}
    property Color;
    {@prop ColSpacing = ancestor /}
    property ColSpacing;
    {@prop ColWidth = ancestor /}
    property ColWidth;
    {@prop Completion = ancestor /}
    property Completion;
    {@prop Computable = ancestor /}
    property Computable;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop Font = ancestor /}
    property Font;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop GroupIndex = ancestor /}
    property GroupIndex;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop Organization = ancestor /}
    property Organization;
    {@prop PageBreaking = ancestor /}
    property PageBreaking;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
  end;

  {/@class}


  { TRLGroup }

  {@class TRLGroup - Sequência de registros de dados.
   Insira bands sobre um componente de grupo para imprimir sequências de registros de dados.
   A quebra de sequência dos registros será detectada automaticamente se for indicado um campo ou conjunto de campos
   através da propriedade DataFields, ou ainda pela expressão contida em DataFormula. A quebra também poderá ser feita
   interativamente durante as chamadas ao evento OnGetBreak. Um componente de grupo deve conter pelo menos uma band de
   detalhe para imprimir os registros da sequência. Adicionalmente, podem ser inseridos quaisquer outros tipos de band
   como, por exemplo: btSummary para mostrar somatórios e estatísticas ao final da sequência, ou btHeader para mostrar
   cabeçalhos. Grupos podem ser inseridos recursivamente dentro de outros grupos formando uma cadeia de sequências
   hierárquicas. Subdetalhes também podem ser inseridos dentro de grupos e vice-versa. Um grupo pode ser desativado
   sem no entanto influenciar na impressão dos seus controles através da propriedade Enabled.
   @links TRLSubDetail.
   @icon TRLGroup.jpg
   @ancestor TRLCustomGroup.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLGroup = class(TRLCustomGroup)
  published

    // properties

    {@prop AllowedBands = ancestor /}
    property AllowedBands;
    {@prop Background = ancestor /}
    property Background;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop DataFields = ancestor /}
    property DataFields;
    {@prop DataFormula = ancestor /}
    property DataFormula;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop Enabled = ancestor /}
    property Enabled;
    {@prop Font = ancestor /}
    property Font;
    {@prop FooterMeasuring = ancestor /}
    property FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property ForceMinBands;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop MaxBands = ancestor /}
    property MaxBands;
    {@prop MinBands = ancestor /}
    property MinBands;
    {@prop PageBreaking = ancestor /}
    property PageBreaking;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnGetBreak = ancestor /}
    property OnGetBreak;
  end;

  {/@class}


  { TRLSubDetail }

  {@class TRLSubDetail - Sub-relatório.
   Utilize o TRLSubDetail para imprimir registros ou sequências de dados relacionadas com os registros da fontes de
   dados principal. O controle de sub-detalhe é especialmente útil quando se quer listar registros de uma base que
   possui registros filhos ou relacionados (Master/Detail), aonde um TRLReport responderia pelos registros principais
   e o TRLSubDetail pelos registros filhos.
   @links TRLGroup.
   @icon TRLSubDetail.jpg
   @ancestor TRLCustomSubDetail.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLSubDetail = class(TRLCustomSubDetail)
  published
    {@prop AllowedBands = ancestor /}
    property AllowedBands;
    {@prop Background = ancestor /}
    property Background;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop Font = ancestor /}
    property Font;
    {@prop FooterMeasuring = ancestor /}
    property FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property ForceMinBands;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop MaxBands = ancestor /}
    property MaxBands;
    {@prop MinBands = ancestor /}
    property MinBands;
    {@prop PageBreaking = ancestor /}
    property PageBreaking;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop Positioning = ancestor /}
    property Positioning;
    {@prop RangeCount = ancestor /}
    property RangeCount;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop RecordRange = ancestor /}
    property RecordRange;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnDataCount = ancestor /}
    property OnDataCount;
    {@event OnDataRecord = ancestor /}
    property OnDataRecord;
    {@event OnNeedData = ancestor /}
    property OnNeedData;
  end;

  {/@class}


  { TRLReport }

  {@class TRLReport - Componente principal na construção de relatórios.
   Utilize o TRLReport como ponto de partida na confecção de qualquer relatório com o FortesReport. Um componente
   TRLReport pode listar registros de uma fonte de dados, solicitar os dados através de eventos em tempo de execução
   ou apenas imprimir páginas confeccionadas com os componentes da biblioteca.
   @icon TRLReport.jpg
   @ancestor TRLCustomReport.
   @pub }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLReport = class(TRLCustomReport)
  published

    // properties

    {@prop AllowedBands = ancestor /}
    property AllowedBands;
    {@prop AdjustableMargins = ancestor /}
    property AdjustableMargins;
    {@prop Background = ancestor /}
    property Background;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Color = ancestor /}
    property Color;
    {@prop CompositeOptions = ancestor /}
    property CompositeOptions;
    {@prop DataSource = ancestor /}
    property DataSource;
    {@prop DefaultFilter = ancestor /}
    property DefaultFilter;
    {@prop Degrade = ancestor /}
    property Degrade;
    {@prop FirstPageNumber = ancestor /}
    property FirstPageNumber;
    {@prop Font = ancestor /}
    property Font;
    {@prop FooterMeasuring = ancestor /}
    property FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property ForceMinBands;
    {@prop ForcePrepare = ancestor /}
    property ForcePrepare;
    {@prop FriendlyName = ancestor /}
    property FriendlyName;
    {@prop InsideMargins = ancestor /}
    property InsideMargins;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop MaxBands = ancestor /}
    property MaxBands;
    {@prop MinBands = ancestor /}
    property MinBands;
    {@prop NextReport = ancestor /}
    property NextReport;
    {@prop PageSetup = ancestor /}
    property PageSetup;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop PreviewOptions = ancestor /}
    property PreviewOptions;
    {@prop PrintDialog = ancestor /}
    property PrintDialog;
    {@prop PrintEmpty = ancestor /}
    property PrintEmpty;
    {@prop PrintQuality = ancestor /}
    property PrintQuality;
    {@prop RangeCount = ancestor /}
    property RangeCount;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop RecordRange = ancestor /}
    property RecordRange;
    {@prop ShowDesigners = ancestor /}
    property ShowDesigners;
    {@prop ShowExplosion = ancestor /}
    property ShowExplosion;
    {@prop ShowProgress = ancestor /}
    property ShowProgress;
    {@prop ShowTracks = ancestor /}
    property ShowTracks;
    {@prop Title = ancestor /}
    property Title;
    {@prop JobTitle = ancestor /}
    property JobTitle;
    {@prop ExpressionParser = ancestor /}
    property ExpressionParser;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop UnlimitedHeight  = ancestor /}
    property UnlimitedHeight ;

    // events

    {@event AfterPrint = ancestor /}
    property AfterPrint;
    {@event BeforePrint = ancestor /}
    property BeforePrint;
    {@event OnDataCount = ancestor /}
    property OnDataCount;
    {@event OnDataRecord = ancestor /}
    property OnDataRecord;
    {@event OnFilterText = ancestor /}
    property OnFilterText;
    {@event OnNeedData = ancestor /}
    property OnNeedData;
    {@event OnPageEnding = ancestor /}
    property OnPageEnding;
    {@event OnPageStarting = ancestor /}
    property OnPageStarting;
  end;

{/@class}

{@proc LoadReportDialog - Carrega e pré-visualiza relatório com diálogo.
 Exibe um diálogo para carga de relatório salvo em disco e em seguida chama o preview padrão. :/}
procedure LoadReportDialog;

{@proc LoadReportFromFile - Carrega e pré-visualiza relatório a partir de um arquivo. :/}
procedure LoadReportFromFile(const AFileName: string);

// ronaldo 20100415 - para nao prejudicar outras aplicacoes
var NewAlignedSummaryBehavior: Boolean = False;

// ronaldo 20101007 - flag criado por requisito do danfe, para nao prejudicar outras aplicacoes
var FineTuneAngleLabels: Boolean = False;

{/@unit}

implementation

uses
  RLSpoolFilter;

const
  faSlaveLeftSet = [faLeft, faTop, faBottom, faLeftMost, faClient,
    faLeftTop, faLeftBottom, faCenterLeft, faClientLeft, faClientTop,
    faClientBottom, faWidth, faLeftOnly];
  faSlaveTopSet = [faLeft, faTop, faRight, faLeftMost, faRightMost,
    faClient, faLeftTop, faRightTop, faCenterTop, faClientLeft,
    faClientTop, faClientRight, faHeight, faTopOnly];
  faSlaveRightSet = [faTop, faRight, faBottom, faRightMost, faClient,
    faRightTop, faRightBottom, faCenterRight, faClientTop, faClientRight,
    faClientBottom, faWidth, faRightOnly];
  faSlaveBottomSet = [faLeft, faRight, faBottom, faLeftMost, faRightMost,
    faClient, faLeftBottom, faRightBottom, faCenterBottom, faClientLeft,
    faClientRight, faClientBottom, faHeight, faBottomOnly];
  faLeftSet = [faLeft, faLeftMost, faLeftTop, faLeftBottom, faCenterLeft,
    faClientLeft, faLeftOnly];
  faTopSet = [faTop, faLeftTop, faRightTop, faCenterTop, faClientTop, faTopOnly];
  faRightSet = [faRight, faRightMost, faRightTop, faRightBottom,
    faCenterRight, faClientRight, faRightOnly];
  faBottomSet = [faBottom, faLeftBottom, faRightBottom, faCenterBottom,
    faClientBottom, faBottomOnly];

  faSlaveWidthSet = faSlaveLeftSet * faSlaveRightSet;
  faSlaveHeightSet = faSlaveTopSet * faSlaveBottomSet;
  faFreeLeftSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveLeftSet;
  faFreeTopSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveTopSet;
  faFreeRightSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveRightSet;
  faFreeBottomSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveBottomSet;
  faFreeWidthSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveWidthSet;
  faFreeHeightSet = [Low(TRLControlAlign)..High(TRLControlAlign)] - faSlaveHeightSet;

const
  BandTypeNames: array[TRLBandType] of string = ('Header', 'Title', 'ColumnHeader',
    'Detail', 'ColumnFooter', 'Summary', 'Footer');
  InfoTypeNames: array[TRLInfoType] of string = ('CarbonCopy', 'Date', 'DetailCount',
    'FullDate', 'Hour', 'Junction', 'LastPageNumber', 'Mend', 'Now', 'PageNumber',
    'PagePreview', 'Title', 'RecNo', 'CopyNo');
  faFromAlign: array[TAlign] of TRLControlAlign = (
    faNone, faTop, faBottom, faLeft, faRight, faClient
{$ifndef DELPHI5}
    , faNone
{$endif}
    );
  fkFromAnchor: array[TAnchorKind] of TRLControlAnchorsType =
    (fkLeft, fkTop, fkRight, fkBottom);

procedure LoadReportFromFile(const AFileName: string);
var
  form: TForm;
  Report: TRLCustomReport;
  savecursor: TCursor;
begin
  if not FileExists(AFileName) then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_FileNotFoundStr + ' "' + AFileName + '"'));

  savecursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    form := TForm.Create(nil);
    try
      Report := TRLCustomReport.Create(form);
      Report.LoadFromFile(AFileName);
      Screen.Cursor := savecursor;
      Report.Preview;
    finally
      FreeObj(form);
    end;
  except
    Screen.Cursor := savecursor;
    raise;
  end;
end;

procedure LoadReportDialog;
var
  dialog: TOpenDialog;
begin
  dialog := TOpenDialog.Create(nil);
  try
    dialog.DefaultExt := FormatFileExt(ReportFileExt);
    dialog.Filter := AddFileFilter('', CS_ProductTitleStr, ReportFileExt);
    dialog.FilterIndex := 1;
    dialog.Title := GetLocalizeStr(LocaleStrings.LS_LoadReportStr);
    if dialog.Execute then
      LoadReportFromFile(dialog.FileName);
  finally
    FreeObj(dialog);
  end;
end;

// controle dentro de frame
function ControlWithin(AControl: TControl): TControl;
begin
  if (AControl is TCustomFrame) and (TCustomFrame(AControl).ControlCount > 0) then
    Result := ControlWithin(TCustomFrame(AControl).Controls[0])
  else
    Result := AControl;
end;

function IsStaticCustomControl(AControl: TControl): Boolean;
begin
  Result := ((AControl is TRLCustomControl) and not (AControl is TRLCustomSite)) or
    (AControl is TRLCustomPanel);
end;

function IsTransparent(AControl: TRLCustomControl): Boolean;
begin
  Result := AControl.Transparent;
end;

// alinhamento de controle
function GetControlAlignOf(AControl: TControl): TRLControlAlign;
begin
  AControl := ControlWithin(AControl);
  if AControl is TRLCustomControl then
    Result := TRLCustomControl(AControl).Align
  else if AControl is TControl then
    Result := faFromAlign[TControl(AControl).Align]
  else
    Result := faNone;
end;

function GetControlAnchorsOf(AControl: TControl): TRLControlAnchors;
var
  I: TAnchorKind;
begin
  if AControl is TRLCustomControl then
    Result := TRLCustomControl(AControl).Anchors
  else if AControl is TControl then
  begin
    Result := [];
    for I := Low(TAnchorKind) to High(TAnchorKind) do
      if I in TControl(AControl).Anchors then
        Result := Result + [fkFromAnchor[I]];
  end
  else
    Result := [];
end;

function GetScreenLeft(AControl: TControl): Integer; overload;
begin
  Result := AControl.Left;
  if AControl.Parent <> nil then
    Inc(Result, GetScreenLeft(AControl.Parent));
end;

function GetScreenLeft(AControl: TControl; ALeft: Integer): Integer; overload;
begin
  Result := ALeft;
  if AControl.Parent <> nil then
    Inc(Result, GetScreenLeft(AControl.Parent));
end;

procedure SetScreenLeft(AControl: TControl; ALeft: Integer); overload;
begin
  AControl.Left := AControl.Left + ALeft - GetScreenLeft(AControl);
end;

procedure SetScreenLeft(AControl: TControl; ALeft: Integer;
  var AResult: Integer); overload;
begin
  AResult := AControl.Left + ALeft - GetScreenLeft(AControl);
end;

function GetScreenTop(AControl: TControl): Integer; overload;
begin
  Result := AControl.Top;
  if AControl.Parent <> nil then
    Inc(Result, GetScreenTop(AControl.Parent));
end;

function GetScreenTop(AControl: TControl; ATop: Integer): Integer; overload;
begin
  Result := ATop;
  if AControl.Parent <> nil then
    Inc(Result, GetScreenTop(AControl.Parent));
end;

procedure SetScreenTop(AControl: TControl; ATop: Integer); overload;
begin
  AControl.Top := AControl.Top + ATop - GetScreenTop(AControl);
end;

procedure SetScreenTop(AControl: TControl; ATop: Integer; var AResult: Integer);
  overload;
begin
  AResult := AControl.Top + ATop - GetScreenTop(AControl);
end;

function GetScreenPos(AControl: TControl): TPoint; overload;
begin
  Result := Point(AControl.Left, AControl.Top);
  if AControl.Parent <> nil then
    with GetScreenPos(AControl.Parent) do
    begin
      Inc(Result.X, X);
      Inc(Result.Y, Y);
    end;
end;

function GetScreenPos(AControl: TControl; APos: TPoint): TPoint; overload;
begin
  Result := APos;
  if AControl.Parent <> nil then
    with GetScreenPos(AControl.Parent) do
    begin
      Inc(Result.X, X);
      Inc(Result.Y, Y);
    end;
end;

procedure SetScreenPos(AControl: TControl; APos: TPoint); overload;
var
  P: TPoint;
begin
  P := GetScreenPos(AControl);
  with AControl do
    SetBounds(Left + APos.X - P.X, Top + APos.Y - P.Y, Width, Height);
end;

procedure SetScreenPos(AControl: TControl; APos: TPoint; var AResult: TPoint); overload;
var
  P: TPoint;
begin
  P := GetScreenPos(AControl);
  AResult.X := AControl.Left + APos.X - P.X;
  AResult.Y := AControl.Top + APos.Y - P.Y;
end;

function ReportOrNIL(ASource: TObject): TRLCustomReport;
begin
  if Assigned(ASource) and (ASource is TRLCustomReport) then
    Result := TRLCustomReport(ASource)
  else
    Result := nil;
end;

{ TRLBorders }

constructor TRLBorders.Create(AOwner: TRLCustomControl);
begin
  // variables
  FParentControl := AOwner;
  FSides := sdNone;
  FDrawLeft := False;
  FDrawTop := False;
  FDrawRight := False;
  FDrawBottom := False;
  FWidth := 1;
  FColor := clBlack;
  FStyle := bsSolid;
  FFixedLeft := False;
  FFixedTop := False;
  FFixedRight := False;
  FFixedBottom := False;

  inherited Create;
end;

procedure TRLBorders.AdjustParent;
begin
  with ParentControl do
  begin
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLBorders.PaintTo(ACanvas: TCanvas; ARect: TRect);
var
  W: Integer;
begin
  if Width > 0 then
  begin
    W := Self.Width;
    ACanvas.Pen.Color := Self.Color;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Mode := pmCopy;
    ACanvas.Pen.Width := 1;
    ACanvas.Brush.Color := Self.Color;
    ACanvas.Brush.Style := bsSolid;
    if CanDrawLeft then
      ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Left + W, ARect.Bottom);
    if CanDrawTop then
      ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Top + W);
    if CanDrawRight then
      ACanvas.Rectangle(ARect.Right - W, ARect.Top, ARect.Right, ARect.Bottom);
    if CanDrawBottom then
      ACanvas.Rectangle(ARect.Left, ARect.Bottom - W, ARect.Right, ARect.Bottom);
  end;
end;

procedure TRLBorders.PaintTo(ASurface: TRLGraphicSurface; ARect: TRect);
var
  W: Integer;
begin
  if Width > 0 then
  begin
    W := Self.Width;
    ASurface.Pen.Color := Self.Color;
    ASurface.Pen.Style := psSolid;
    ASurface.Pen.Mode := pmCopy;
    ASurface.Pen.Width := 1;
    ASurface.Brush.Color := Self.Color;
    ASurface.Brush.Style := bsSolid;
    if CanDrawLeft then
      ASurface.Rectangle(ARect.Left, ARect.Top, ARect.Left + W, ARect.Bottom);
    if CanDrawTop then
      ASurface.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Top + W);
    if CanDrawRight then
      ASurface.Rectangle(ARect.Right - W, ARect.Top, ARect.Right, ARect.Bottom);
    if CanDrawBottom then
      ASurface.Rectangle(ARect.Left, ARect.Bottom - W, ARect.Right, ARect.Bottom);
  end;
end;

procedure TRLBorders.CheckSides;
begin
  if FDrawLeft and FDrawTop and FDrawRight and FDrawBottom and (FSides = sdAll) then
  else if not FDrawLeft and not FDrawTop and not FDrawRight and not
    FDrawBottom and (FSides = sdNone) then
  else
    FSides := sdCustom;
end;

procedure TRLBorders.SetDrawLeft(const AValue: Boolean);
begin
  if AValue = FDrawLeft then
    Exit;
  FDrawLeft := AValue;
  if not FDrawLeft then
    FFixedLeft := False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawTop(const AValue: Boolean);
begin
  if AValue = FDrawTop then
    Exit;
  FDrawTop := AValue;
  if not FDrawTop then
    FFixedTop := False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawRight(const AValue: Boolean);
begin
  if AValue = FDrawRight then
    Exit;
  FDrawRight := AValue;
  if not FDrawRight then
    FFixedRight := False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawBottom(const AValue: Boolean);
begin
  if AValue = FDrawBottom then
    Exit;
  FDrawBottom := AValue;
  if not FDrawBottom then
    FFixedBottom := False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetWidth(const AValue: Integer);
begin
  if AValue = FWidth then
    Exit;
  if AValue < 0 then
    Exit;
  FWidth := AValue;
  AdjustParent;
end;

procedure TRLBorders.SetColor(const AValue: TColor);
begin
  if AValue = FColor then
    Exit;
  FColor := AValue;
  ParentControl.Invalidate;
end;

procedure TRLBorders.SetStyle(const AValue: TBrushStyle);
begin
  if AValue = FStyle then
    Exit;
  FStyle := AValue;
  ParentControl.Invalidate;
end;

procedure TRLBorders.SetParentControl(const AValue: TRLCustomControl);
begin
  if AValue = FParentControl then
    Exit;
  FParentControl := AValue;
  AdjustParent;
end;

procedure TRLBorders.SetSides(const AValue: TRLBorderSides);
begin
  if AValue = FSides then
    Exit;
  FSides := AValue;
  case FSides of
    sdNone:
    begin
      FDrawLeft := False;
      FDrawTop := False;
      FDrawRight := False;
      FDrawBottom := False;
      FFixedLeft := False;
      FFixedTop := False;
      FFixedRight := False;
      FFixedBottom := False;
    end;
    sdAll:
    begin
      FDrawLeft := True;
      FDrawTop := True;
      FDrawRight := True;
      FDrawBottom := True;
    end;
  else
    Exit;
  end;
  AdjustParent;
end;

procedure TRLBorders.SetFixedLeft(const AValue: Boolean);
begin
  if AValue = FFixedLeft then
    Exit;
  FFixedLeft := AValue;
  if AValue and not DrawLeft then
    DrawLeft := True;
end;

procedure TRLBorders.SetFixedTop(const AValue: Boolean);
begin
  if AValue = FFixedTop then
    Exit;
  FFixedTop := AValue;
  if AValue and not DrawTop then
    DrawTop := True;
end;

procedure TRLBorders.SetFixedRight(const AValue: Boolean);
begin
  if AValue = FFixedRight then
    Exit;
  FFixedRight := AValue;
  if AValue and not DrawRight then
    DrawRight := True;
end;

procedure TRLBorders.SetFixedBottom(const AValue: Boolean);
begin
  if AValue = FFixedBottom then
    Exit;
  FFixedBottom := AValue;
  if AValue and not FDrawBottom then
    DrawBottom := True;
end;

function TRLBorders.CanDrawLeft: Boolean;
begin
  Result := DrawLeft and (FixedLeft or ((ParentControl.MasterReport <> nil) and
    (ParentControl.MasterReport.PrintQuality = pqFullFeature)));
end;

function TRLBorders.CanDrawTop: Boolean;
begin
  Result := DrawTop and (FixedTop or ((ParentControl.MasterReport <> nil) and
    (ParentControl.MasterReport.PrintQuality = pqFullFeature)));
end;

function TRLBorders.CanDrawRight: Boolean;
begin
  Result := DrawRight and (FixedRight or ((ParentControl.MasterReport <> nil) and
    (ParentControl.MasterReport.PrintQuality = pqFullFeature)));
end;

function TRLBorders.CanDrawBottom: Boolean;
begin
  Result := DrawBottom and (FixedBottom or
    ((ParentControl.MasterReport <> nil) and
    (ParentControl.MasterReport.PrintQuality = pqFullFeature)));
end;

function TRLBorders.IsCustom: Boolean;
begin
  Result := (FSides = sdCustom);
end;

destructor TRLBorders.Destroy;
begin
  inherited;
end;

{ TRLMargins }

constructor TRLMargins.Create(AOwner: TRLCustomControl);
begin
  // variables
  FParentControl := AOwner;
  FLeftMargin := 0;
  FTopMargin := 0;
  FRightMargin := 0;
  FBottomMargin := 0;
  FDefaultLeftMargin := 0;
  FDefaultTopMargin := 0;
  FDefaultRightMargin := 0;
  FDefaultBottomMargin := 0;

  inherited Create;
end;

procedure TRLMargins.ReadLeftMargin(Reader: TReader);
begin
  FLeftMargin := Reader.ReadFloat;
end;

procedure TRLMargins.WriteLeftMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FLeftMargin);
end;

procedure TRLMargins.ReadTopMargin(Reader: TReader);
begin
  FTopMargin := Reader.ReadFloat;
end;

procedure TRLMargins.WriteTopMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FTopMargin);
end;

procedure TRLMargins.ReadRightMargin(Reader: TReader);
begin
  FRightMargin := Reader.ReadFloat;
end;

procedure TRLMargins.WriteRightMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FRightMargin);
end;

procedure TRLMargins.ReadBottomMargin(Reader: TReader);
begin
  FBottomMargin := Reader.ReadFloat;
end;

procedure TRLMargins.WriteBottomMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FBottomMargin);
end;

procedure TRLMargins.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('LeftMargin', ReadLeftMargin, WriteLeftMargin,
    FLeftMargin <> FDefaultLeftMargin);
  Filer.DefineProperty('TopMargin', ReadTopMargin, WriteTopMargin,
    FTopMargin <> FDefaultTopMargin);
  Filer.DefineProperty('RightMargin', ReadRightMargin, WriteRightMargin,
    FRightMargin <> FDefaultRightMargin);
  Filer.DefineProperty('BottomMargin', ReadBottomMargin, WriteBottomMargin,
    FBottomMargin <> FDefaultBottomMargin);
end;

procedure TRLMargins.SetDefaults(ALeft, ATop, ARight, ABottom: Double);
begin
  FDefaultLeftMargin := ALeft;
  FDefaultTopMargin := ATop;
  FDefaultRightMargin := ARight;
  FDefaultBottomMargin := ABottom;
  FLeftMargin := ALeft;
  FTopMargin := ATop;
  FRightMargin := ARight;
  FBottomMargin := ABottom;
end;

procedure TRLMargins.AdjustParent;
begin
  with ParentControl do
  begin
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLMargins.SetLeftMargin(const AValue: Double);
begin
  if AValue = FLeftMargin then
    Exit;
  FLeftMargin := AValue;
  AdjustParent;
end;

procedure TRLMargins.SetRightMargin(const AValue: Double);
begin
  if AValue = FRightMargin then
    Exit;
  FRightMargin := AValue;
  AdjustParent;
end;

procedure TRLMargins.SetTopMargin(const AValue: Double);
begin
  if AValue = FTopMargin then
    Exit;
  FTopMargin := AValue;
  AdjustParent;
end;

procedure TRLMargins.SetBottomMargin(const AValue: Double);
begin
  if AValue = FBottomMargin then
    Exit;
  FBottomMargin := AValue;
  AdjustParent;
end;

procedure TRLMargins.Assign(Source: TPersistent);
begin
  if Source is TRLMargins then
    with TRLMargins(Source) do
    begin
      Self.LeftMargin := LeftMargin;
      Self.TopMargin := TopMargin;
      Self.RightMargin := RightMargin;
      Self.BottomMargin := BottomMargin;
    end
  else
    inherited;
end;

destructor TRLMargins.Destroy;
begin
  inherited;
end;

{ TRLPageSetup }

constructor TRLPageSetup.Create(AOwner: TRLCustomReport);
begin
  // variables
  FParentReport := ReportOrNIL(AOwner);
  FPaperSize := fpA4;
  FPaperWidth := PaperInfo[FPaperSize].Width;
  FPaperHeight := PaperInfo[FPaperSize].Height;
  FOrientation := poPortrait;
  FForceEmulation := False;

  inherited Create;
end;

procedure TRLPageSetup.AdjustParent;
begin
  with ParentReport do
  begin
    ReloadPrinter;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLPageSetup.SetPaperSize(const AValue: TRLPaperSize);
begin
  if AValue = FPaperSize then
    Exit;
  FPaperSize := AValue;
  if FPaperSize <> fpCustom then
  begin
    FPaperWidth := PaperInfo[FPaperSize].Width;
    FPaperHeight := PaperInfo[FPaperSize].Height;
  end;
  AdjustParent;
end;

procedure TRLPageSetup.SetPaperHeight(const AValue: Double);
begin
  if AValue = FPaperHeight then
    Exit;
  if (FPaperSize <> fpCustom) or (AValue = 0) then
    Exit;
  if Assigned(FParentReport) and
     FParentReport.UnlimitedHeight and
     (not (csDesigning in FParentReport.ComponentState)) and
     (not (FParentReport.ReportState in ([rsClosing, rsWriting]))) then
  begin
    Exit;
  end;

  FPaperHeight := AValue;
  AdjustParent;
end;

procedure TRLPageSetup.SetPaperWidth(const AValue: Double);
begin
  if AValue = FPaperWidth then
    Exit;
  if (FPaperSize <> fpCustom) or (AValue = 0) then
    Exit;
  FPaperWidth := AValue;
  AdjustParent;
end;

procedure TRLPageSetup.SetOrientation(const AValue: TRLPageOrientation);
begin
  if AValue = FOrientation then
    Exit;
  if Assigned(FParentReport) and (FParentReport.UnlimitedHeight) and (AValue = poLandscape) then
     Exit;

  FOrientation := AValue;
  AdjustParent;
end;

function TRLPageSetup.GetOrientedWidth: Double;
begin
  if FOrientation = poPortrait then
    Result := FPaperWidth
  else
    Result := FPaperHeight;
end;

function TRLPageSetup.GetOrientedHeight: Double;
begin
  if FOrientation = poPortrait then
    Result := FPaperHeight
  else
    Result := FPaperWidth;
end;

procedure TRLPageSetup.SetOrientedHeight(const AValue: Double);
begin
  if FOrientation = poPortrait then
    FPaperHeight := AValue
  else
    FPaperWidth := AValue;
end;

procedure TRLPageSetup.SetOrientedWidth(const AValue: Double);
begin
  if FOrientation = poPortrait then
    FPaperWidth := AValue
  else
    FPaperHeight := AValue;
end;

function TRLPageSetup.IsCustomPaperSize: Boolean;
begin
  Result := (FPaperSize = fpCustom);
end;

procedure TRLPageSetup.Assign(Source: TRLPageSetup);
begin
  PaperSize := Source.PaperSize;
  Orientation := Source.Orientation;
  PaperWidth := Source.PaperWidth;
  PaperHeight := Source.PaperHeight;
  ForceEmulation := Source.ForceEmulation;
end;

destructor TRLPageSetup.Destroy;
begin
  inherited;
end;

{ TRLBackground }

constructor TRLBackground.Create(AOwner: TRLCustomSite);
begin
  // variables
  FParentSite := AOwner;
  FAlign := faClient;
  FArrange := baAligned;
  FAutoSize := True;
  FHeight := 40;
  FStretch := False;
  FWidth := 40;
  // objects
  FPicture := TPicture.Create;

  inherited Create;
end;

destructor TRLBackground.Destroy;
begin
  FreeObj(FPicture);

  inherited;
end;

procedure TRLBackground.AdjustSize;
begin
  if (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty then
  begin
    FWidth := FPicture.Width;
    FHeight := FPicture.Height;
  end;
end;

procedure TRLBackground.PaintTo(ACanvas: TCanvas; ARect: TRect);
var
  X, Y, D: Integer;
  R: TRect;
  B: TBitmap;
begin
  if (FPicture.Graphic = nil) or FPicture.Graphic.Empty then
    Exit;
  case FArrange of
    baAligned:
    begin
      case FAlign of
        faNone: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Top + FHeight);
        faLeft,
        faLeftMost: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Bottom);
        faTop: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top + FHeight);
        faRight,
        faRightMost: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Top, ARect.Right, ARect.Bottom);
        faBottom: R :=
            Classes.Rect(ARect.Left, ARect.Bottom - FHeight, ARect.Right, ARect.Bottom);
        faClient: R := ARect;
        faLeftTop: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Top + FHeight);
        faRightTop: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Top, ARect.Right, ARect.Top + FHeight);
        faLeftBottom: R :=
            Classes.Rect(ARect.Left, ARect.Bottom - FHeight, ARect.Left + FWidth, ARect.Bottom);
        faRightBottom: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Bottom - FHeight, ARect.Right, ARect.Bottom);
        faCenter:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(X, Y, X + FWidth, Y + FHeight);
        end;
        faCenterLeft,
        faClientLeft:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Left + FWidth, Y + FHeight);
        end;
        faCenterTop,
        faClientTop:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Top + FHeight);
        end;
        faCenterRight,
        faClientRight:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Right - FWidth, Y, ARect.Right, Y + FHeight);
        end;
        faCenterBottom,
        faClientBottom:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Bottom - FHeight, X + FWidth, ARect.Bottom);
        end;
        faHeight:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Bottom);
        end;
        faWidth:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Right, Y + FHeight);
        end;
        faLeftOnly:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Left + FWidth, Y + FHeight);
        end;
        faRightOnly:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Right - FWidth, Y, ARect.Right, Y + FHeight);
        end;
        faTopOnly:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Top + FHeight);
        end;
        faBottomOnly:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Bottom - FHeight, X + FWidth, ARect.Bottom);
        end;
      end;
      if FStretch then
      else
      begin
        R.Right := R.Left + FPicture.Width;
        R.Bottom := R.Top + FPicture.Height;
      end;
      ACanvas.StretchDraw(R, FPicture.Graphic);
    end;
    baCenter:
    begin
      R.Left := (ARect.Left + ARect.Right - FWidth) div 2;
      R.Top := (ARect.Top + ARect.Bottom - FHeight) div 2;
      if FStretch then
      begin
        R.Right := R.Left + FWidth;
        R.Bottom := R.Top + FHeight;
      end
      else
      begin
        R.Right := R.Left + FPicture.Width;
        R.Bottom := R.Top + FPicture.Height;
      end;
      ACanvas.StretchDraw(R, FPicture.Graphic);
    end;
    baSidebySide,
    baDistributed:
    begin
      B := NeedAuxBitmap;
      B.PixelFormat := pf32bit;
      B.Width := RectWidth(ARect);
      B.Height := RectHeight(ARect);
      D := 0;
      Y := 0;
      repeat
        X := -D * FWidth div 2;
        repeat
          R.Left := X;
          R.Top := Y;
          if FStretch then
          begin
            R.Right := R.Left + FWidth;
            R.Bottom := R.Top + FHeight;
          end
          else
          begin
            R.Right := R.Left + FPicture.Width;
            R.Bottom := R.Top + FPicture.Height;
          end;
          B.Canvas.StretchDraw(R, FPicture.Graphic);
          Inc(X, FWidth);
        until X > B.Width;
        if FArrange = baDistributed then
          D := 1 - D;
        Inc(Y, FHeight);
      until Y > B.Height;
      ACanvas.StretchDraw(ARect, B);
    end;
  end;
end;

procedure TRLBackground.PaintTo(ASurface: TRLGraphicSurface; ARect: TRect);
var
  X, Y, D: Integer;
  R: TRect;
  B: TBitmap;
begin
  if (FPicture.Graphic = nil) or FPicture.Graphic.Empty then
    Exit;
  case FArrange of
    baAligned:
    begin
      case FAlign of
        faNone: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Top + FHeight);
        faLeft,
        faLeftMost: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Bottom);
        faTop: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top + FHeight);
        faRight,
        faRightMost: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Top, ARect.Right, ARect.Bottom);
        faBottom: R :=
            Classes.Rect(ARect.Left, ARect.Bottom - FHeight, ARect.Right, ARect.Bottom);
        faClient: R := ARect;
        faLeftTop: R :=
            Classes.Rect(ARect.Left, ARect.Top, ARect.Left + FWidth, ARect.Top + FHeight);
        faRightTop: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Top, ARect.Right, ARect.Top + FHeight);
        faLeftBottom: R :=
            Classes.Rect(ARect.Left, ARect.Bottom - FHeight, ARect.Left + FWidth, ARect.Bottom);
        faRightBottom: R :=
            Classes.Rect(ARect.Right - FWidth, ARect.Bottom - FHeight, ARect.Right, ARect.Bottom);
        faCenter:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(X, Y, X + FWidth, Y + FHeight);
        end;
        faCenterLeft,
        faClientLeft:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Left + FWidth, Y + FHeight);
        end;
        faCenterTop,
        faClientTop:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Top + FHeight);
        end;
        faCenterRight,
        faClientRight:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Right - FWidth, Y, ARect.Right, Y + FHeight);
        end;
        faCenterBottom,
        faClientBottom:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Bottom - FHeight, X + FWidth, ARect.Bottom);
        end;
        faHeight:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Bottom);
        end;
        faWidth:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Right, Y + FHeight);
        end;
        faLeftOnly:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Left, Y, ARect.Left + FWidth, Y + FHeight);
        end;
        faRightOnly:
        begin
          Y := (ARect.Top + ARect.Bottom - FHeight) div 2;
          R :=
            Classes.Rect(ARect.Right - FWidth, Y, ARect.Right, Y + FHeight);
        end;
        faTopOnly:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Top, X + FWidth, ARect.Top + FHeight);
        end;
        faBottomOnly:
        begin
          X := (ARect.Left + ARect.Right - FWidth) div 2;
          R :=
            Classes.Rect(X, ARect.Bottom - FHeight, X + FWidth, ARect.Bottom);
        end;
      end;
      if FStretch then
      else
      begin
        R.Right := R.Left + FPicture.Width;
        R.Bottom := R.Top + FPicture.Height;
      end;
      ASurface.StretchDraw(R, FPicture.Graphic);
    end;
    baCenter:
    begin
      R.Left := (ARect.Left + ARect.Right - FWidth) div 2;
      R.Top := (ARect.Top + ARect.Bottom - FHeight) div 2;
      if FStretch then
      begin
        R.Right := R.Left + FWidth;
        R.Bottom := R.Top + FHeight;
      end
      else
      begin
        R.Right := R.Left + FPicture.Width;
        R.Bottom := R.Top + FPicture.Height;
      end;
      ASurface.StretchDraw(R, FPicture.Graphic);
    end;
    baSidebySide,
    baDistributed:
    begin
      B := NeedAuxBitmap;
      B.PixelFormat := pf32bit;
      B.Width := RectWidth(ARect);
      B.Height := RectHeight(ARect);
      D := 0;
      Y := 0;
      repeat
        X := -D * FWidth div 2;
        repeat
          R.Left := X;
          R.Top := Y;
          if FStretch then
          begin
            R.Right := R.Left + FWidth;
            R.Bottom := R.Top + FHeight;
          end
          else
          begin
            R.Right := R.Left + FPicture.Width;
            R.Bottom := R.Top + FPicture.Height;
          end;
          B.Canvas.StretchDraw(R, FPicture.Graphic);
          Inc(X, FWidth);
        until X > B.Width;
        if FArrange = baDistributed then
          D := 1 - D;
        Inc(Y, FHeight);
      until Y > B.Height;
      ASurface.StretchDraw(ARect, B);
    end;
  end;
end;

procedure TRLBackground.SetAlign(const AValue: TRLControlAlign);
begin
  if AValue = FAlign then
    Exit;
  FAlign := AValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetArrange(const AValue: TRLImageArrange);
begin
  if AValue = FArrange then
    Exit;
  FArrange := AValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetAutoSize(const AValue: Boolean);
begin
  if AValue = FAutoSize then
    Exit;
  FAutoSize := AValue;
  if AValue then
    AdjustSize;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetHeight(const AValue: Integer);
begin
  if AValue = FHeight then
    Exit;
  FHeight := AValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetPicture(const AValue: TPicture);
begin
  FPicture.Assign(AValue);
  if FAutoSize then
    AdjustSize;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetStretch(const AValue: Boolean);
begin
  if AValue = FStretch then
    Exit;
  FStretch := AValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetWidth(const AValue: Integer);
begin
  if AValue = FWidth then
    Exit;
  FWidth := AValue;
  ParentSite.Invalidate;
end;

{ TRLDegradeEffect }

constructor TRLDegradeEffect.Create(AOwner: TRLCustomSite);
begin
  // variables
  FParentSite := AOwner;
  FDirection := ddNone;
  FOppositeColor := clBlack;
  FGranularity := 1;

  inherited Create;
end;

procedure TRLDegradeEffect.PaintTo(ACanvas: TCanvas; ARect: TRect; AColor: TColor);
type
  TRGBInfo = record
    red, green, blue, pallete: Byte;
  end;
var
  I, barcount, barwidth, totalwidth: Integer;
  P: Double;
  R: TRect;
  cl1, cl2: TRGBInfo;

  function RGBInfo(Color: TColor): TRGBInfo;
  var
    L: Integer;
  begin
    L := ColorToRGB(Color);
    Move(L, Result, 4);
  end;

begin
  cl1 := RGBInfo(FParentSite.Color);
  cl2 := RGBInfo(FOppositeColor);
  if FDirection = ddVertical then
    totalwidth := RectHeight(ARect)
  else
    totalwidth := RectWidth(ARect);
  barwidth := FGranularity;
  barcount := (totalwidth + barwidth - 1) div barwidth;

  for I := 0 to barcount - 1 do
  begin
    P := I / barcount;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := Round(P * cl2.Blue) * 65536 +
      (cl1.Blue - Round(P * cl1.Blue)) * 65536 + Round(P *
      cl2.Green) * 256 + (cl1.Green - Round(P * cl1.Green)) * 256 +
      Round(P * cl2.Red) * 1 + (cl1.Red - Round(P * cl1.Red)) * 1;
    if FDirection = ddVertical then
    begin
      R.Left := ARect.Left;
      R.Top := ARect.Top + I * barwidth;
      R.Right := ARect.Right;
      R.Bottom := R.Top + barwidth;
      if R.Bottom > ARect.Bottom then
        R.Bottom := ARect.Bottom;
    end
    else
    begin
      R.Left := ARect.Left + I * barwidth;
      R.Top := ARect.Top;
      R.Right := R.Left + barwidth;
      R.Bottom := ARect.Bottom;
      if R.Right > ARect.Right then
        R.Right := ARect.Right;
    end;
    ACanvas.FillRect(R);
  end;
end;

destructor TRLDegradeEffect.Destroy;
begin
  inherited;
end;

procedure TRLDegradeEffect.PaintTo(ASurface: TRLGraphicSurface;
  ARect: TRect; AColor: TColor);
type
  TRGBInfo = record
    red, green, blue, pallete: Byte;
  end;
var
  I, barcount, barwidth, totalwidth: Integer;
  P: Double;
  R: TRect;
  cl1, cl2: TRGBInfo;

  function RGBInfo(Color: TColor): TRGBInfo;
  var
    L: Integer;
  begin
    L := ColorToRGB(Color);
    Move(L, Result, 4);
  end;

begin
  cl1 := RGBInfo(FParentSite.Color);
  cl2 := RGBInfo(FOppositeColor);
  if FDirection = ddVertical then
    totalwidth := RectHeight(ARect)
  else
    totalwidth := RectWidth(ARect);
  barwidth := FGranularity;
  barcount := (totalwidth + barwidth - 1) div barwidth;

  for I := 0 to barcount - 1 do
  begin
    P := I / barcount;
    ASurface.Brush.Style := bsSolid;
    ASurface.Brush.Color := Round(P * cl2.Blue) * 65536 +
      (cl1.Blue - Round(P * cl1.Blue)) * 65536 + Round(P *
      cl2.Green) * 256 + (cl1.Green - Round(P * cl1.Green)) * 256 +
      Round(P * cl2.Red) * 1 + (cl1.Red - Round(P * cl1.Red)) * 1;
    if FDirection = ddVertical then
    begin
      R.Left := ARect.Left;
      R.Top := ARect.Top + I * barwidth;
      R.Right := ARect.Right;
      R.Bottom := R.Top + barwidth;
      if R.Bottom > ARect.Bottom then
        R.Bottom := ARect.Bottom;
    end
    else
    begin
      R.Left := ARect.Left + I * barwidth;
      R.Top := ARect.Top;
      R.Right := R.Left + barwidth;
      R.Bottom := ARect.Bottom;
      if R.Right > ARect.Right then
        R.Right := ARect.Right;
    end;
    ASurface.FillRect(R);
  end;
end;

procedure TRLDegradeEffect.SetDirection(const AValue: TRLDegradeDirection);
begin
  if AValue = FDirection then
    Exit;
  FDirection := AValue;
  FParentSite.Invalidate;
end;

procedure TRLDegradeEffect.SetGranularity(const AValue: Integer);
begin
  if AValue = FGranularity then
    Exit;
  FGranularity := AValue;
  FParentSite.Invalidate;
end;

procedure TRLDegradeEffect.SetOppositeColor(const AValue: TColor);
begin
  if AValue = FOppositeColor then
    Exit;
  FOppositeColor := AValue;
  FParentSite.Invalidate;
end;

{ TRLSortedBands }

constructor TRLSortedBands.Create;
var
  I: TRLBandType;
begin
  for I := Low(TRLBandType) to High(TRLBandType) do
    with FTypes[I] do
    begin
      List := TList.Create;
      Printed := False;
    end;

  inherited Create;
end;

destructor TRLSortedBands.Destroy;
var
  I: TRLBandType;
begin
  for I := Low(TRLBandType) to High(TRLBandType) do
    with FTypes[I] do
      FreeObj(List);

  inherited;
end;

procedure TRLSortedBands.Add(ABand: TRLCustomSite);
var
  I: Integer;
  T: TRLBandType;
begin
{$ifdef CLX}
  T := btDetail;
{$endif}
  if ABand is TRLCustomBand then
    T := TRLCustomBand(ABand).BandType
  else if ABand is TRLCustomSubDetail then
    T := TRLCustomSubDetail(ABand).Positioning
  else
    Exit;
  with FTypes[T].List do
  begin
    I := 0;
    while (I <= Count - 1) and (ABand.Top >= TRLCustomSite(Items[I]).Top) do
      Inc(I);
    if I > Count - 1 then
      Add(ABand)
    else
      Insert(I, ABand);
  end;
end;

procedure TRLSortedBands.Clear;
var
  I: TRLBandType;
begin
  for I := Low(TRLBandType) to High(TRLBandType) do
    FTypes[I].List.Clear;
end;

procedure TRLSortedBands.ResetAll;
var
  I: TRLBandType;
begin
  for I := Low(TRLBandType) to High(TRLBandType) do
    FTypes[I].Printed := False;
end;

procedure TRLSortedBands.ResetPage;
var
  I: TRLBandType;
begin
  for I := Low(TRLBandType) to High(TRLBandType) do
    if I in [btTitle] then
    else
      FTypes[I].Printed := False;
end;

function TRLSortedBands.GetList(AType: TRLBandType): TList;
begin
  Result := FTypes[AType].List;
end;

function TRLSortedBands.GetPrinted(AType: TRLBandType): Boolean;
begin
  Result := FTypes[AType].Printed;
end;

procedure TRLSortedBands.SetPrinted(AType: TRLBandType; AValue: Boolean);
begin
  FTypes[AType].Printed := AValue;
end;

{ TRLRealBounds }

constructor TRLRealBounds.Create(AOwner: TRLCustomControl);
begin
  FParentControl := AOwner;
  FUsedUnit := buNone;
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;

  inherited Create;
end;

procedure TRLRealBounds.AdjustParent;
begin
end;

procedure TRLRealBounds.SetUsedUnit(const AValue: TRLRealBoundsUnit);
begin
  FUsedUnit := AValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetWidth(const AValue: Double);
begin
  FWidth := AValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetHeight(const AValue: Double);
begin
  FHeight := AValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetLeft(const AValue: Double);
begin
  FLeft := AValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetTop(const AValue: Double);
begin
  FTop := AValue;
  AdjustParent;
end;

destructor TRLRealBounds.Destroy;
begin
  inherited;
end;

{ TRLCustomControl }

constructor TRLCustomControl.Create(AOwner: TComponent);
begin
  // initialization
  FAfterPrint := nil;
  FAlign := faNone;
  FAnchors := [];
  FBeforePrint := nil;
  FBeforeText := nil;
  FHolder := nil;
  FHoldStyle := hsAsColumn;
  FSecondHolder := nil;
  FSecondHoldStyle := hsAsColumn;
  FRealBounds := nil;
  FAlignment := taLeftJustify;
  FAutoSize := False;
  FAutoSizeDir := [];
  FAutoExpand := False;
  FAutoTrunc := False;
  FLayout := tlTop;
  FControlState := [];
  FBehavior := [];
  FTransparent := True;
  FSizeFixed := False;
  FFriendlyName := '';
  FOnMeasureHeight := nil;
  // objects
  FBorders := TRLBorders.Create(Self);
  FHoldeds := TList.Create;
  FRealBounds := TRLRealBounds.Create(Self);

  inherited Create(AOwner);
  // customization
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  MakeCaption;
end;

destructor TRLCustomControl.Destroy;
begin
  FreeObj(FRealBounds);
  FreeObj(FHoldeds);
  FreeObj(FBorders);

  inherited;
end;

procedure TRLCustomControl.ComputeDetail(ACaller: TObject);
begin
end;

procedure TRLCustomControl.Initialize;
begin
end;

// invoca evento apos a impressao
procedure TRLCustomControl.DoAfterPrint;
begin
  if Assigned(FAfterPrint) then
    FAfterPrint(Self);
end;

procedure TRLCustomControl.DrawBounds;
var
  R: TRect;
begin
  R := CalcSizeRect;
  with Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    MoveTo(R.Left, R.Top + 5);
    LineTo(R.Left, R.Top);
    LineTo(R.Left + 5, R.Top);
    MoveTo(R.Right - 5, R.Top);
    LineTo(R.Right - 1, R.Top);
    LineTo(R.Right - 1, R.Top + 5);
    MoveTo(R.Right - 1, R.Bottom - 5);
    LineTo(R.Right - 1, R.Bottom - 1);
    LineTo(R.Right - 5, R.Bottom - 1);
    MoveTo(R.Left + 5, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 5);
  end;
end;

procedure TRLCustomControl.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FHolder then
      FHolder := nil;
    if AComponent = FSecondHolder then
      FSecondHolder := nil;
    if FHoldeds <> nil then
    begin
      I := FHoldeds.IndexOf(AComponent);
      if I <> -1 then
        FHoldeds.Delete(I);
    end;
  end;
end;

// anula alinhamento natural do delphi
procedure TRLCustomControl.RequestAlign;
begin
end;

procedure TRLCustomControl.CustomControlPaint;
var
  R: TRect;
begin
  R := CalcSizeRect;
  with Canvas do
  begin
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    FillRect(R);
  end;
  Borders.PaintTo(Canvas, R);
end;

procedure TRLCustomControl.Paint;
begin
  CustomControlPaint;
end;

function TRLCustomControl.IsFriendlyName: Boolean;
begin
  Result := (FFriendlyName <> '') and (FFriendlyName <> Name);
end;

function TRLCustomControl.IsCaption: Boolean;
begin
  Result := (FCaption <> '') and (FCaption <> GetDefaultCaption);
end;

procedure TRLCustomControl.CustomControlPrint;
var
  R: TRect;
  S: TRLGraphicSurface;
begin
  R := CalcPrintBoundsRect;
  S := RequestParentSurface;
  S.GeneratorId := PtrInt(Self);
  NewGroupId;
  if not IsTransparent(Self) then
  begin
    S.Brush.Color := Self.Color;
    S.Brush.Style := bsSolid;
    S.FillRect(R);
  end;
  Borders.PaintTo(S, R);
end;

procedure TRLCustomControl.InternalPrint;
begin
  CustomControlPrint;
end;

procedure TRLCustomControl.Print;
begin
  Include(FControlState, stPrinting);
  try
    InternalPrint;
  finally
    Exclude(FControlState, stPrinting);
  end;
end;

procedure TRLCustomControl.CalcSize(var ASize: TPoint);
begin
  ASize := Point(Width, Height);
end;

procedure TRLCustomControl.AdjustAlignment(var ARect: TRect);
var
  newwidth, newheight: Integer;
begin
  newwidth := RectWidth(ARect);
  case Alignment of
    taLeftJustify: ARect.Right := Left + newwidth;
    taCenter:
    begin
      if Odd(newwidth) then
        Inc(newwidth);
      ARect.Left := Left + (Width - newwidth) div 2;
      ARect.Right := ARect.Left + newwidth;
    end;
    taRightJustify:
    begin
      ARect.Right := Left + Width;
      ARect.Left := ARect.Right - newwidth;
    end;
    taJustify: ARect.Right := Left + newwidth;
  end;
  newheight := RectHeight(ARect);
  case Layout of
    tlTop: ARect.Bottom := Top + newheight;
    tlCenter:
    begin
      if Odd(newheight) then
        Inc(newheight);
      ARect.Top := Top + (Height - newheight) div 2;
      ARect.Bottom := ARect.Top + newheight;
    end;
    tlBottom:
    begin
      ARect.Bottom := Top + Height;
      ARect.Top := ARect.Bottom - newheight;
    end;
  end;
end;

procedure TRLCustomControl.AdjustBounds;
var
  P: TPoint;
  R: TRect;
begin
  if csLoading in ComponentState then
    Exit;
  if stAdjustingBounds in FControlState then
    Exit;
  Include(FControlState, stAdjustingBounds);
  try
    CalcSize(P);
    if csDesigning in ComponentState then
    begin
      if P.X = 0 then
        P.X := 1;
      if P.Y = 0 then
        P.Y := 1;
    end;
    R.Left := Left;
    R.Top := Top;
    R.Right := R.Left + P.X;
    R.Bottom := R.Top + P.Y;
    AdjustAlignment(R);
    BoundsRect := R;
  finally
    Exclude(FControlState, stAdjustingBounds);
  end;
end;

{$ifdef CLX}

procedure TRLCustomControl.ColorChanged;
begin
  if not (csLoading in ComponentState) and (Color <> clWhite) then
    FTransparent := False;

  inherited;
end;

procedure TRLCustomControl.FontChanged;
begin
  AdjustBounds;
  Invalidate;

  inherited;
end;

{$else}

procedure TRLCustomControl.CMColorChanged(var Message: TMessage);
begin
  if not (csLoading in ComponentState) and (Color <> clWhite) then
    FTransparent := False;
  //
  inherited;
end;
procedure TRLCustomControl.CMFontChanged(var Message: TMessage);
begin
  AdjustBounds;
  Invalidate;
  //
  inherited;
end;

{$endif}

type
  TFriendControl = class(TControl)
  end;

function GetControlImage(AControl: TControl; var ABitmap: TBitmap): Boolean;
{$ifdef CLX}
var C: TFriendControl;
{$ifndef CPP}
var H: QPaintDeviceH;
{$endif}
{$endif}
begin
  Result := False;
  ABitmap := TRLBitmap.Create;
  try
    ABitmap.Width := AControl.Width;
    ABitmap.Height := AControl.Height;
{$ifdef CLX}
    C := TFriendControl(AControl);
{$ifndef CPP}
    H := C.GetPaintDevice;
    QPainter_redirect(H, ABitmap.Handle);
{$endif}
    C.Repaint;
{$ifndef CPP}
    QPainter_redirect(H, nil);
{$endif}
{$else}
    if AControl is TWinControl then
      TWinControl(AControl).PaintTo(ABitmap.Canvas.Handle, 0, 0)
    else if AControl is TControl then
      AControl.Perform(WM_PAINT, ABitmap.Canvas.Handle, 0)
    else
      Abort;
{$endif}
    Result := True;
  except
    ABitmap.Free;
    ABitmap := nil;
  end;
end;

procedure GetControlsByPrintOrder(AParent: TWinControl; AList: TList);
var
  I, J: Integer;
  C: TControl;

  function IsPriorPrintOrder(ACtrl, ARef: TControl): Boolean;
  begin
    Result := (ACtrl.ComponentIndex < ARef.ComponentIndex);
  end;

begin
  for I := 0 to AParent.ControlCount - 1 do
  begin
    C := AParent.Controls[I];
    J := 0;
    while (J < AList.Count) and IsPriorPrintOrder(TControl(AList[J]), C) do
      Inc(J);
    AList.Insert(J, C);
  end;
end;

procedure PrepareStaticsAllFrom(AParent: TWinControl);
var
  I: Integer;
  C: TControl;
  L: TList;
begin
  L := TList.Create;
  try
    // monta lista sorteada por creation order
    GetControlsByPrintOrder(AParent, L);
    for I := 0 to L.Count - 1 do
    begin
      C := TControl(L[I]);
      // panel ou control não site
      if IsStaticCustomControl(C) then
        with TRLCustomControl(C) do
          if CanPrint then
          begin
            AdjustBounds;
            PrepareStatics;
          end
          else
      else if C is TCustomFrame then
        if TCustomFrame(C).Visible then
          PrepareStaticsAllFrom(TCustomFrame(C));
    end;
  finally
    L.Free;
  end;
end;

procedure PrintNonNative(AParent: TWinControl; AControl: TControl);
var
  site: TWinControl;
  offs: TPoint;
  bmp: TBitmap;
begin
  // procura o parentsite para pegar o surface de desenho e as coordenadas relativas do controle aControl
  site := AParent;
  offs := Point(AControl.Left, AControl.Top);
  while (site <> nil) and not (site is TRLCustomSite) do
  begin
    Inc(offs.X, site.Left);
    Inc(offs.Y, site.Top);
    site := site.Parent;
  end;

  if site <> nil then
    if GetControlImage(AControl, bmp) then
      try
        TRLCustomSite(site).Surface.Draw(offs.X, offs.Y, bmp);
      finally
        bmp.Free;
      end;
end;

procedure PrintStaticsAllFrom(AParent: TWinControl);
var
  I: Integer;
  C: TControl;
  L: TList;
begin
  L := TList.Create;
  try
    // monta lista sorteada por creation order
    GetControlsByPrintOrder(AParent, L);
    for I := 0 to L.Count - 1 do
    begin
      C := TControl(L[I]);
      // panel ou control não site
      if IsStaticCustomControl(C) then
        with TRLCustomControl(C) do
          if CouldPrint then
          begin
            Print;
            DoAfterPrint;
          end
          else
      else if C is TCustomFrame then
        if TCustomFrame(C).Visible then
          PrintStaticsAllFrom(TCustomFrame(C))
        else
      else if not (C is TRLCustomControl) then
        if C.Visible then
          PrintNonNative(AParent, C);
    end;
  finally
    L.Free;
  end;
end;

procedure TRLCustomControl.PrepareStatics;
begin
  if Enabled then
    PrepareStaticsAllFrom(Self);
end;

procedure TRLCustomControl.PrintStatics;
begin
  if Enabled then
    PrintStaticsAllFrom(Self);
end;

function TRLCustomControl.IsBallast: Boolean;
var
  P: TRLCustomPager;
  S: TRLCustomSkipper;
begin
  S := FindParentSkipper;
  P := FindParentPager;
  if (P <> nil) and (psCompleting in P.PagerStatus) then
    Result := True
  else if (S <> nil) and (S.RecordAction = raBlankAndRetain) then
    Result := True
  else
    Result := False;
end;

procedure TRLCustomControl.MakeCaption;
begin
  Caption := GetMadeCaption;
end;

function TRLCustomControl.GetMadeCaption: string;
begin
  if IsBallast then
    Result := ''
  else
    Result := InternalMakeCaption;
end;

function TRLCustomControl.InternalMakeCaption: string;
begin
  Result := GetCaption;
end;

function TRLCustomControl.GetDefaultCaption: TCaption;
begin
  if FFriendlyName <> '' then
    Result := FFriendlyName
  else
    Result := Name;
end;

// relatorio anterior
function TRLCustomControl.FindParentReport: TRLCustomReport;
var
  W: TControl;
begin
  W := Self;
  while (W <> nil) and not (W is TRLCustomReport) do
    W := W.Parent;
  Result := TRLCustomReport(W);
end;

// relatorio mestre, o primeiro da lista
function TRLCustomControl.GetMasterReport: TRLCustomReport;
begin
  Result := FindParentReport;
  if Result <> nil then
    if Result.PriorReport <> nil then
      Result := Result.PriorReport.MasterReport;
end;

function TRLCustomControl.GetLastReport: TRLCustomReport;
begin
  Result := FindParentReport;
  if Result <> nil then
    while Result.NextReport <> nil do
      Result := Result.NextReport;
end;

// site anterior
function TRLCustomControl.FindParentSite: TRLCustomSite;
var
  W: TControl;
begin
  W := Parent;
  while (W <> nil) and not (W is TRLCustomSite) do
    W := W.Parent;
  Result := TRLCustomSite(W);
end;

// band anterior 
function TRLCustomControl.FindParentBand: TRLCustomBand;
var
  W: TControl;
begin
  W := Parent;
  while (W <> nil) and not (W is TRLCustomBand) do
    W := W.Parent;
  Result := TRLCustomBand(W);
end;

function TRLCustomControl.FindParentGroup: TRLCustomGroup;
var
  W: TControl;
begin
  Result := nil;
  W := Parent;
  while (W <> nil) and not (W is TRLCustomGroup) do
    if W is TRLCustomPager then
      Exit
    else
      W := W.Parent;
  Result := TRLCustomGroup(W);
end;

// controlador de registros atual ou anterior (DlReport ou subdetail)
function TRLCustomControl.FindParentSkipper: TRLCustomSkipper;
var
  W: TControl;
begin
  W := Self;
  while (W <> nil) and not (W is TRLCustomSkipper) do
    W := W.Parent;  
  
  if Assigned(W) then
    Result := TRLCustomSkipper(W)
  else
    Result := nil;   
end;

function TRLCustomControl.FindParentPager: TRLCustomPager;
var
  W: TControl;
begin
  W := Parent;
  while (W <> nil) and not (W is TRLCustomPager) do
    W := W.Parent;  
  
  if Assigned(W) then
    Result := TRLCustomPager(W)
  else
    Result := nil;  
end;

function TRLCustomControl.FindParentSurface: TRLGraphicSurface;
var
  P: TRLCustomSite;
begin
  P := FindParentSite;
  if Assigned(P) then
    Result := P.Surface
  else
    Result := nil;
end;

// Pager atual ou anterior (DlReport, subdetail ou group)
function TRLCustomControl.RequestParentPager: TRLCustomPager;
begin
  Result := FindParentPager;
  if Result = nil then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr + ': ' + Name + '.ParentPager'));
end;

function TRLCustomControl.RequestParentSkipper: TRLCustomSkipper;
begin
  Result := FindParentSkipper;
  if Result = nil then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr + ': ' +
      Name + '.ParentSkipper'));
end;

function TRLCustomControl.RequestParentReport: TRLCustomReport;
begin
  Result := FindParentReport;
  if Result = nil then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr + ': ' + Name + '.ParentReport'));
end;

function TRLCustomControl.RequestParentSurface: TRLGraphicSurface;
begin
  Result := FindParentSurface;
  if Result = nil then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr + ': ' +
      Name + '.ParentSurface'));
end;

procedure TRLCustomControl.DoBeforePrint(var APrintIt: Boolean);
begin
  if Assigned(FBeforePrint) then
    FBeforePrint(Self, APrintIt);
end;

procedure TRLCustomControl.DoBeforeText(var AText: string; var APrintIt: Boolean);
begin
  if Assigned(FBeforeText) then
    FBeforeText(Self, AText, APrintIt);
end;

procedure TRLCustomControl.DoOnMeasureHeight;
begin
  if Assigned(FOnMeasureHeight) then
    FOnMeasureHeight(Self);
end;

function TRLCustomControl.CanPrint: Boolean;
var
  S: string;
begin
  FCouldPrint := Visible;
  if FCouldPrint then
  begin
    DoBeforePrint(FCouldPrint);
    if FCouldPrint then
    begin
      S := GetMadeCaption;
      DoBeforeText(S, FCouldPrint);
      if FCouldPrint then
      begin
        FindParentReport.DoFilterText(S, FCouldPrint);
        if FCouldPrint then
          Caption := S;
      end;
    end;
  end;
  Result := FCouldPrint;
end;

function TRLCustomControl.CalcSizeRect: TRect;
begin
  Result := BoundsRect;
  MoveRect(Result, 0, 0);
end;

function TRLCustomControl.GetClientRect: TRect;
var
  W: Integer;
begin
  Result := CalcSizeRect;
  W := FBorders.Width;
  if W > 0 then
  begin
    Inc(W);
    if FBorders.CanDrawLeft then
      Inc(Result.Left, W);
    if FBorders.CanDrawTop then
      Inc(Result.Top, W);
    if FBorders.CanDrawRight then
      Dec(Result.Right, W);
    if FBorders.CanDrawBottom then
      Dec(Result.Bottom, W);
  end;
end;

function TRLCustomControl.CalcPrintBoundsRect: TRect;
var
  P: TControl;
begin
  Result := BoundsRect;

  P := Parent;
  while (P <> nil) and not (P is TRLCustomSite) do
  begin
    OffsetRect(Result, P.Left, P.Top);
    P := P.Parent;
  end;
end;

function TRLCustomControl.CalcPrintSizeRect: TRect;
begin
  Result := BoundsRect;
  MoveRect(Result, 0, 0);
end;

function TRLCustomControl.CalcPrintClientRect: TRect;
var
  W, H: Integer;
begin
  Result := CalcPrintBoundsRect;
  if FBorders.Width > 0 then
  begin
    W := FBorders.Width;
    H := FBorders.Width;
    if FBorders.CanDrawLeft then
      Inc(Result.Left, W);
    if FBorders.CanDrawTop then
      Inc(Result.Top, H);
    if FBorders.CanDrawRight then
      Dec(Result.Right, W);
    if FBorders.CanDrawBottom then
      Dec(Result.Bottom, H);
  end;
end;

function TRLCustomControl.CalcWastedPixels: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FBorders.Width > 0 then
  begin
    if FBorders.CanDrawLeft then
      Inc(Result.Left, FBorders.Width);
    if FBorders.CanDrawTop then
      Inc(Result.Top, FBorders.Width);
    if FBorders.CanDrawRight then
      Inc(Result.Right, FBorders.Width);
    if FBorders.CanDrawBottom then
      Inc(Result.Bottom, FBorders.Width);
  end;
end;

procedure TRLCustomControl.SetClientRect(const AValue: TRect);
var
  rWasted: TRect;
  newRect: TRect;
begin
  rWasted := CalcWastedPixels;
  newRect := AValue;
  Dec(newRect.Left, rWasted.Left);
  Dec(newRect.Top, rWasted.Top);
  Inc(newRect.Right, rWasted.Right);
  Inc(newRect.Bottom, rWasted.Bottom);
  BoundsRect := newRect;
end;

procedure TRLCustomControl.SetAlign(const AValue: TRLControlAlign);
var
  old: TRLControlAlign;
begin
  old := FAlign;
  if AValue = old then
    Exit;
  if AValue <> faNone then
    FAnchors := [];
  FAlign := AValue;

  if ((old in faFreeHeightSet) and (AValue in faFreeWidthSet)) or
    ((old in faFreeWidthSet) and (AValue in faFreeHeightSet)) then
  begin
    FAlign := faNone;
    AdjustBounds;
    FAlign := AValue;
  end;
  Realign;
end;

procedure TRLCustomControl.SetAnchors(const AValue: TRLControlAnchors);
begin
  if AValue <> [] then
    FAlign := faNone;
  FAnchors := AValue;
end;

procedure TRLCustomControl.Hold(AControl: TRLCustomControl; APlace: Integer);
var
  M, N, P: TPoint;
begin
  if AControl = Self then
    Exit;
  if FHoldeds.IndexOf(AControl) = -1 then
    FHoldeds.Add(AControl);
  // guarda posição relativa
  M := GetScreenPos(Self);
  N := GetScreenPos(AControl);
  P.X := N.X - M.X;
  P.Y := N.Y - M.Y;
  case APlace of
    0: AControl.HolderOffset := P;
    1: AControl.SecondHolderOffset := P;
  end;
  // ajusta posição do controle amarrado
  AControl.AdjustBounds;
end;

procedure TRLCustomControl.Unhold(AControl: TRLCustomControl);
var
  I: Integer;
begin
  I := FHoldeds.IndexOf(AControl);
  if I <> -1 then
    FHoldeds.Delete(I);
end;

procedure TRLCustomControl.SetHolder(const AValue: TRLCustomControl);
var
  old: TRLCustomControl;
begin
  old := FHolder;
  if AValue = old then
    Exit;
  FHolder := AValue;
  if old <> nil then
    old.Unhold(Self);
  if AValue <> nil then
    AValue.Hold(Self, 0);
end;

procedure TRLCustomControl.SetHoldStyle(const AValue: TRLHoldStyle);
begin
  if AValue = FHoldStyle then
    Exit;
  FHoldStyle := AValue;
  if FHolder <> nil then
  begin
    FHolder.Unhold(Self);
    FHolder.Hold(Self, 0);
  end;
end;

procedure TRLCustomControl.SetSecondHolder(const AValue: TRLCustomControl);
var
  old: TRLCustomControl;
begin
  old := FSecondHolder;
  if AValue = old then
    Exit;
  FSecondHolder := AValue;
  if old <> nil then
    old.Unhold(Self);
  if AValue <> nil then
    AValue.Hold(Self, 1);
end;

procedure TRLCustomControl.SetSecondHoldStyle(const AValue: TRLHoldStyle);
begin
  if AValue = FSecondHoldStyle then
    Exit;
  FSecondHoldStyle := AValue;
  if FSecondHolder <> nil then
  begin
    FSecondHolder.Unhold(Self);
    FSecondHolder.Hold(Self, 1);
  end;
end;

// ajusta a altura do controle pai para comportar este controle
procedure TRLCustomControl.ExpandParentSite;
var
  W, H: Integer;
  S: TRLCustomSite;
begin
  if csLoading in ComponentState then
    Exit;
  if stRestoringBounds in FControlState then
    Exit;
  if stExpandingParent in FControlState then
    Exit;
  Include(FControlState, stExpandingParent);
  try
    W := (BoundsRect.Right - BoundsRect.Left) - (OldBoundsRect.Right -
      OldBoundsRect.Left);
    H := (BoundsRect.Bottom - BoundsRect.Top) -
      (OldBoundsRect.Bottom - OldBoundsRect.Top);
    // detecta controle escravo de largura
    if Align in faFreeWidthSet then
    begin
      // procura o site pai não escravo de largura
      S := FindParentSite;
      while (S <> nil) and not (S.Align in faFreeWidthSet) do
        S := S.FindParentSite;
      if (S <> nil) and (asWidthDir in S.AutoSizeDir) then
        if S.AutoSize then
          S.AdjustBounds
        else if S.AutoExpand and (beSiteExpander in Behavior) then
          S.Width := S.Width + W;
    end;
    // detecta controle escravo de altura
    if Align in faFreeHeightSet then
    begin
      // procura o site pai não escravo de altura
      S := FindParentSite;
      while (S <> nil) and not (S.Align in faFreeHeightSet) do
        S := S.FindParentSite;
      if (S <> nil) and (asHeightDir in S.AutoSizeDir) then
        if S.AutoSize then
          S.AdjustBounds
        else if S.AutoExpand and (beSiteExpander in Behavior) then
          S.Height := S.Height + H;
    end;
  finally
    Exclude(FControlState, stExpandingParent);
  end;
end;

procedure TRLCustomControl.Realign;
var
  P: TRLCustomSite;
begin
  if csLoading in ComponentState then
    Exit;
  P := FindParentSite;
  if P <> nil then
    P.RealignControls;
end;

procedure TRLCustomControl.RealignControls;
begin
end;

function TRLCustomControl.CanSetWidth: Boolean;
begin
  Result := (Align in faFreeWidthSet) and not ((asWidthDir in AutoSizeDir) and AutoSize);
end;

function TRLCustomControl.CanSetHeight: Boolean;
begin
  Result := (Align in faFreeHeightSet) and not
    ((asHeightDir in AutoSizeDir) and AutoSize);
end;

procedure TRLCustomControl.RealignHoldeds;
var
  I: Integer;
begin
  if stAdjustingHoldeds in FControlState then
    Exit;
  Include(FControlState, stAdjustingHoldeds);
  try
    for I := 0 to FHoldeds.Count - 1 do
      TRLCustomControl(FHoldeds[I]).AdjustBounds;
  finally
    Exclude(FControlState, stAdjustingHoldeds);
  end;
end;

procedure TRLCustomControl.OriginalSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;


procedure TRLCustomControl.AdjustToFixedSize(var ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FSizeFixed then
  begin
    AWidth := FFixedSize.X;
    AHeight := FFixedSize.Y;
  end;
end;

// adequa o frame que contém o control
procedure TRLCustomControl.AdjustToParentFrame(
  var ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if (Parent <> nil) and (Parent is TCustomFrame) then
  begin
    R.Left := Parent.Left + ALeft;
    R.Top := Parent.Top + ATop;
    R.Right := R.Left + AWidth;
    R.Bottom := R.Top + AHeight;
    ALeft := 0;
    ATop := 0;
    Parent.BoundsRect := R;
  end;
end;


procedure TRLCustomControl.AdjustToHolder(AHolder: TRLCustomControl;
  var ALeft, ATop, AWidth, AHeight: Integer);
var
  M, P, R, holdofs: TPoint;
  ox, oy: Integer;
  holdst: TRLHoldStyle;
begin
{$ifdef CLX}
  holdofs := Point(0, 0);
  holdst := hsAsColumn;
{$endif}
  if AHolder = nil then
    Exit
  else if AHolder = FHolder then
  begin
    holdofs := HolderOffset;
    holdst := HoldStyle;
  end
  else if AHolder = FSecondHolder then
  begin
    holdofs := SecondHolderOffset;
    holdst := SecondHoldStyle;
  end
  else
    Exit;

  case Alignment of
    taLeftJustify: ox := 0;
    taCenter: ox := (AHolder.Width - AWidth) div 2;
    taRightJustify: ox := AHolder.Width - AWidth;
    taJustify: ox := 0;
  else
    ox := 0;
  end;

  case Layout of
    tlTop: oy := 0;
    tlCenter: oy := (AHolder.Height - AHeight) div 2;
    tlBottom: oy := AHolder.Height - AHeight;
    tlJustify: oy := 0;
  else
    oy := 0;
  end;

  case holdst of
    hsAsColumn:
    begin
      SetScreenLeft(Self, GetScreenLeft(AHolder) + ox, ALeft);
      if CanSetWidth then
        AWidth := AHolder.Width;
    end;
    hsHorizontally: SetScreenLeft(Self, GetScreenLeft(AHolder) + holdofs.X + ox, ALeft);
    hsVertically: SetScreenTop(Self, GetScreenTop(AHolder) + holdofs.Y + oy, ATop);
    hsRelatively:
    begin
      M := GetScreenPos(AHolder);
      P := Point(M.X + holdofs.X + ox, M.Y + holdofs.Y + oy);
      SetScreenPos(Self, P, R);
      ALeft := R.X;
      ATop := R.Y;
    end;
    hsCopyWidth: if CanSetWidth then
        AWidth := AHolder.Width;
    hsCopyHeight: if CanSetHeight then
        AHeight := AHolder.Height;
    hsCopySize:
    begin
      if CanSetWidth then
        AWidth := AHolder.Width;
      if CanSetHeight then
        AHeight := AHolder.Height;
    end;
  end;
end;

procedure TRLCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  SavedBoundsRect: TRect;
begin
  SavedBoundsRect := BoundsRect;
  OldBoundsRect := SavedBoundsRect;

  AdjustToFixedSize(ALeft, ATop, AWidth, AHeight);
  AdjustToParentFrame(ALeft, ATop, AWidth, AHeight);
  AdjustToHolder(FHolder, ALeft, ATop, AWidth, AHeight);
  AdjustToHolder(FSecondHolder, ALeft, ATop, AWidth, AHeight);
  // se as coordenadas mudaram em relação ao controle pai...
  if (OldBoundsRect.Left <> ALeft) or (OldBoundsRect.Top <> ATop) or
    (RectWidth(OldBoundsRect) <> AWidth) or (RectHeight(OldBoundsRect) <>
    AHeight) then
  begin
    OriginalSetBounds(ALeft, ATop, AWidth, AHeight);
    OldBoundsRect := SavedBoundsRect;
    ExpandParentSite;
    Realign;
  end;
  // incondicionalmente realinha os controles internos e "agarrados"
  RealignControls;
  RealignHoldeds;
end;

procedure TRLCustomControl.SetTransparent(const AValue: Boolean);
begin
  if AValue = FTransparent then
    Exit;
  if AValue then
    if Self is TRLCustomReport then
      Color := clWhite
    else
      ParentColor := True;
  FTransparent := AValue;
end;

procedure TRLCustomControl.SetAlignment(const AValue: TRLTextAlignment);
begin
  if AValue = FAlignment then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoSize(const AValue: Boolean);
begin
  if AValue = FAutoSize then
    Exit;
  FAutoSize := AValue;
  if AValue then
    AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoExpand(const AValue: Boolean);
begin
  if AValue = FAutoExpand then
    Exit;
  FAutoExpand := AValue;
  if AValue then
    AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoTrunc(const AValue: Boolean);
begin
  if AValue = FAutoTrunc then
    Exit;
  FAutoTrunc := AValue;
  if AValue then
    AdjustBounds;
end;

function TRLCustomControl.GetCaption: TCaption;
begin
  if IsPreparing then
    Result := FPreparingCaption
  else if (FCaption = '') and (csDesigning in ComponentState) then
    Result := GetDefaultCaption
  else
    Result := FCaption;
end;

procedure TRLCustomControl.SetCaption(const AValue: TCaption);
begin
  // fPreparingCaption é o caption para efeitos de impressão, e é descartado quando o relatório termina
  // fCaption contém o texto oficial do caption, que deve ser gravado do dfm
  FPreparingCaption := AValue;
  if IsPreparing then
  else if (AValue = GetDefaultCaption) and (csDesigning in ComponentState) then
    FCaption := ''
  else
    FCaption := AValue;

  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetLayout(const AValue: TRLTextLayout);
begin
  if AValue = FLayout then
    Exit;
  FLayout := AValue;
  Invalidate;
end;

procedure PushBoundsAllFrom(AParent: TWinControl);
var
  I: Integer;
  C: TControl;
begin
  for I := 0 to AParent.ControlCount - 1 do
  begin
    C := AParent.Controls[I];
    if C is TRLCustomControl then
    begin
      TRLCustomControl(C).PushBoundsRect;
      if C is TRLCustomSite then
        PushBoundsAllFrom(TRLCustomSite(C));
    end
    else if C is TCustomFrame then
      PushBoundsAllFrom(TCustomFrame(C));
  end;
end;

procedure PopBoundsAllFrom(AParent: TWinControl);
var
  I: Integer;
  C: TControl;
begin
  for I := 0 to AParent.ControlCount - 1 do
  begin
    C := AParent.Controls[I];
    if C is TRLCustomControl then
    begin
      TRLCustomControl(C).PopBoundsRect;
      if C is TRLCustomSite then
        PopBoundsAllFrom(TRLCustomSite(C));
    end
    else if C is TCustomFrame then
      PopBoundsAllFrom(TCustomFrame(C));
  end;
end;

procedure TRLCustomControl.PushBoundsRect;
begin
  FPeekBoundsRect := BoundsRect;
end;

procedure TRLCustomControl.PopBoundsRect;
begin
  Include(FControlState, stRestoringBounds);
  try
    BoundsRect := FPeekBoundsRect;
  finally
    Exclude(FControlState, stRestoringBounds);
  end;
end;

procedure TRLCustomControl.SetBorders(const AValue: TRLBorders);
begin
  FBorders := AValue;
  FBorders.ParentControl := Self;
  Invalidate;
end;

procedure TRLCustomControl.SetRealBounds(const AValue: TRLRealBounds);
begin
  FRealBounds := AValue;
end;

function TRLCustomControl.IsPreparing: Boolean;
var
  R: TRLCustomReport;
begin
  R := FindParentReport;
  Result := Assigned(R) and (R.ReportState in [rsWriting, rsClosing]);
end;

procedure TRLCustomControl.CheckParent(var AControl: TWinControl);
begin
  // uma band não pode conter outras
  if (Self is TRLCustomBand) and (AControl is TRLCustomBand) then
    AControl := AControl.Parent;
  // um panel não pode conter bands ou paginadores
  if (Self is TRLCustomBand) or (Self is TRLCustomPager) then
    while (AControl <> nil) and (AControl is TRLCustomPanel) do
      AControl := AControl.Parent;
end;

procedure TRLCustomControl.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);

  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetFriendlyName(const Value: string);
var
  I: Integer;
begin
  if Value = FFriendlyName then
    Exit;

  if (Value = '') or (Value = Name) then
    FFriendlyName := ''
  else if not IsValidIdent(Value) then
    if csLoading in ComponentState then
      FFriendlyName := ''
    else
      raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_InvalidNameStr + ' "' + Value + '"'))
  else
  begin
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TRLCustomControl then
        if AnsiSameText(Value, TRLCustomControl(Owner.Components[I]).FriendlyName) then
          raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_DuplicateNameStr + ' "' + Value + '"'));
    FFriendlyName := Value;
  end;

  AdjustBounds;
  Invalidate;
end;

function TRLCustomControl.IsMeasurable: Boolean;
begin
  Result := AutoSize or AutoExpand or Assigned(FOnMeasureHeight);
end;

// try to Measure controls height before it be printed
procedure TRLCustomControl.MeasureHeight;
begin
  if not Visible then
    Exit;
  if not IsMeasurable then
    Exit;
  if csLoading in ComponentState then
    Exit;
  if stMeasuringHeights in FControlState then
    Exit;
  Include(FControlState, stMeasuringHeights);
  try
    InternalMeasureHeight;
    DoOnMeasureHeight;
  finally
    Exclude(FControlState, stMeasuringHeights);
  end;
  AdjustBounds;
end;

procedure TRLCustomControl.InternalMeasureHeight;
begin
  Caption := GetMadeCaption;
end;

function TRLCustomControl.GetAttribute(const AName: string): Variant;
begin
  Result := Caption;
end;

function TRLCustomControl.SetAttribute(const AName: string; AValue: Variant): Boolean;
begin
  Result := False;
end;

{$ifdef CLX}
procedure TRLCustomControl.SetParent(const AParent: TWidgetControl);
var
  P: TWidgetControl;
{$else}
procedure TRLCustomControl.SetParent(AParent: TWinControl);
var
  P: TWinControl;
{$endif}
begin
  P := AParent;
  if P <> nil then
    CheckParent(P);

  inherited SetParent(P);

  if P <> nil then
  begin
    AdjustBounds;
    Realign;
  end;
end;

function TRLCustomControl.GetClientHeight: Integer;
begin
  Result := RectHeight(ClientRect);
end;

function TRLCustomControl.GetClientWidth: Integer;
begin
  Result := RectWidth(ClientRect);
end;

procedure TRLCustomControl.SetClientSize(const Value: TPoint);
var
  R: TRect;
begin
  R := GetClientRect;
  SetBounds(Left, Top, Width - RectWidth(R) + Value.X, Height - RectHeight(R) + Value.Y);
end;

procedure TRLCustomControl.SetClientHeight(const Value: Integer);
begin
  SetClientSize(Point(Width, Value));
end;

procedure TRLCustomControl.SetClientWidth(const Value: Integer);
begin
  SetClientSize(Point(Value, Height));
end;

{ TRLCustomDBControl }

constructor TRLCustomDBControl.Create(AOwner: TComponent);
begin
  FDataField := '';
  FDataSource := nil;

  inherited Create(AOwner);
  // customization
  Width := 65;
  Height := 17;
end;

procedure TRLCustomDBControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil;
end;

procedure TRLCustomDBControl.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  Invalidate;
end;

procedure TRLCustomDBControl.SetDataField(const AValue: TRLDataFieldProperty);
begin
  if AValue = FDataField then
    Exit;
  FDataField := AValue;
  MakeCaption;
end;

function TRLCustomDBControl.GetDataSet: TDataSet;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

function TRLCustomDBControl.GetField: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
  begin
    Result := DataSet.FindField(FDataField);
{Fred/Ronaldo/Tiago - 10/05/2010 - Comentado para não dar erro na DLIB2
    if Result = nil then
      raise Exception.Create(LocaleStrings.LS_NotFoundStr + ': ' +
        Name + '.DataField "' + FDataField + '"');}
  end
  else
    Result := nil;
end;

function TRLCustomDBControl.InternalMakeCaption: string;
var
  F: TField;
begin
  if not IsPreparing then
    if FFriendlyName <> '' then
      Result := FFriendlyName
    else if FDataField <> '' then
      Result := FDataField
    else
      Result := Name
  else
  begin
    F := GetField;
    if (F <> nil) and F.DataSet.Active and not F.DataSet.Eof then
      Result := SmartGetFieldDisplayText(F)
    else
      Result := '';
  end;
end;

{ TRLCustomLabel }

constructor TRLCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // customization
  Width := 65;
  Height := 17;
  AutoSizeDir := [asWidthDir];
  AutoSize := True;
end;

procedure TRLCustomLabel.Paint;
var
  S: string;
  R: TRect;
  P: TRLCustomReport;
begin
  inherited;

  S := Caption;
  if (S = '') and not IsPreparing then
    S := Name;
  R := GetClientRect;
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
  end;
  CanvasTextRectEx(Canvas, R, R.Left, R.Top, S,
    ToMetaTextAlignment(TAlignment(Alignment)),
    ToMetaTextLayout(TTextLayout(Layout)), MetaTextFlagAutoSize or
    MetaTextFlagIntegralHeight);

  P := FindParentReport;
  if not Assigned(P) or P.ShowDesigners then
    DrawBounds;
end;

procedure TRLCustomLabel.InternalPrint;
var
  W, O, H, T, L: Integer;
  R: TRect;
  S: AnsiString;
  F: TRLMetaTextFlags;
begin
  inherited;

  R := CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId := PtrInt(Self);
    NewGroupId;
    Font := Self.Font;
    S := AnsiString(Caption);
    O := TextWidth(' ') div 2;
    W := TextWidth(S + ' ');
    H := TextHeight(S + ' ');
    case Alignment of
      taCenter: L := (R.Left + R.Right - W) div 2 + O;
      taRightJustify: L := R.Right - W + O;
    else
      L := R.Left + O;
    end;
    case Layout of
      tlCenter: T := (R.Top + R.Bottom - H) div 2;
      tlBottom: T := R.Bottom - H;
    else
      T := R.Top;
    end;
    Brush.Style := bsClear;
    F := MetaTextFlagIntegralHeight;
    if AutoSize then
      F := F or MetaTextFlagAutoSize;
    TextRectEx(R, L, T, S, ToMetaTextAlignment(TAlignment(Alignment)),
      ToMetaTextLayout(TTextLayout(Layout)), F);
  end;
end;

procedure TRLCustomLabel.CalcSize(var ASize: TPoint);
var
  W: Integer;
  C: string;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  C := Caption;
  if (C = '') and not IsPreparing then
    C := Name;
  // dimensões do texto
  ASize.X := 0;
  ASize.Y := 0;
  with TextBounds(C + ' ', Self.Font, 0) do
  begin
    Inc(ASize.X, X);
    Inc(ASize.Y, Y);
  end;
  // adicional das bordas
  W := FBorders.Width;
  if W > 0 then
  begin
    Inc(W);
    if FBorders.CanDrawLeft then
      Inc(ASize.X, W);
    if FBorders.CanDrawTop then
      Inc(ASize.Y, W);
    if FBorders.CanDrawRight then
      Inc(ASize.X, W);
    if FBorders.CanDrawBottom then
      Inc(ASize.Y, W);
  end;
end;

{ TRLCustomAngleLabel }

constructor TRLCustomAngleLabel.Create(AOwner: TComponent);
begin
  FAngle := 0;
  FAngleBorders := False;

  inherited Create(AOwner);
  // customization
  Width := 65;
  Height := 17;
  AutoSizeDir := [asWidthDir];
  AutoSize := True;
end;

procedure TRLCustomAngleLabel.Paint;
var
  W, O, H, T, L: Integer;
  S: string;
  R: TRect;
  P: TRLCustomReport;
  M, A: TBitmap;
begin
  inherited;

  S := Caption;
  if (S = '') and not IsPreparing then
    S := Name;
  R := GetClientRect;
  with Canvas do
  begin
    Font := Self.Font;
    O := 0;
    W := TextWidth(S + ' ');
    H := TextHeight(S + ' ');

    M := TRLBitmap.Create;
    try
      M.PixelFormat := pf32bit;
      M.Width := W;
      M.Height := H;
      M.Transparent := Self.Transparent;
      M.TransparentColor := Self.Color;
      M.TransparentMode := tmFixed;
      M.Canvas.Font := Self.Font;
      M.Canvas.Brush.Color := Self.Color;
      M.Canvas.Brush.Style := bsSolid;
      M.Canvas.Pen.Style := psClear;
      M.Canvas.Rectangle(0, 0, M.Width + 1, M.Height + 1);
      M.Canvas.TextOut(1, -1, S);

      A := RotatedBitmap(M, FAngle);
      try
        case Alignment of
          taCenter: L := (R.Left + R.Right - A.Width) div 2 + O;
          taRightJustify: L := R.Right - A.Width + O;
        else
          L := R.Left + O;
        end;
        case Layout of
          tlCenter: T := (R.Top + R.Bottom - A.Height) div 2;
          tlBottom: T := R.Bottom - A.Height;
        else
          T := R.Top;
        end;
        Draw(L, T, A);
      finally
        A.Free;
      end;
    finally
      M.Free;
    end;
  end;
  P := FindParentReport;
  if not Assigned(P) or P.ShowDesigners then
    DrawBounds;
end;

procedure TRLCustomAngleLabel.InternalPrint;
var
  W, O, H, T, L: Integer;
  S: string;
  R: TRect;
  M, A: TBitmap;
  Surf: TRLGraphicSurface;
begin
  inherited;

  S := Caption;
  R := CalcPrintClientRect;
  Surf := RequestParentSurface;
  with Surf do
  begin
    GeneratorId := PtrInt(Self);
    NewGroupId;
    Surf.Font := Self.Font;

    if FineTuneAngleLabels then
      Surf.Font.Size := Surf.Font.Size * 4;

    O := 0; //TextWidth(' ') div 2;
    W := TextWidth(Caption + ' ');
    H := TextHeight(Caption + ' ');

    M := TRLBitmap.Create;
    try
      M.PixelFormat := pf32bit;
      M.Width := W;
      M.Height := H;
      M.Transparent := Self.Transparent;
      M.TransparentColor := Self.Color;
      M.TransparentMode := tmFixed;
      M.Canvas.Font := Surf.Font;
      M.Canvas.Brush.Color := Self.Color;
      M.Canvas.Brush.Style := bsSolid;
      M.Canvas.Pen.Style := psClear;
      M.Canvas.Rectangle(0, 0, M.Width + 1, M.Height + 1);

      if FineTuneAngleLabels then
        M.Canvas.TextOut(1, 1, S)
      else
        M.Canvas.TextOut(1, -1, S);

      A := RotatedBitmap(M, FAngle);
      try
        case Alignment of
          taCenter: L := (R.Left + R.Right - A.Width) div 2 + O;
          taRightJustify: L := R.Right - A.Width + O;
        else
          L := R.Left + O;
        end;
        case Layout of
          tlCenter: T := (R.Top + R.Bottom - A.Height) div 2;
          tlBottom: T := R.Bottom - A.Height;
        else
          T := R.Top;
        end;

        if FineTuneAngleLabels then
        begin
          R := ScaleRect(Rect(0, 0, A.Width, A.Height), R, True);
          Surf.StretchDraw(R, A);
        end
        else
          Surf.Draw(L, T, A);

      finally
        A.Free;
      end;
    finally
      M.Free;
    end;
  end;
end;

procedure TRLCustomAngleLabel.CalcSize(var ASize: TPoint);
var
  W: Integer;
  C: string;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  C := Caption;
  if (C = '') and not IsPreparing then
    C := Name;
  // dimensões do texto
  ASize.X := 0;
  ASize.Y := 0;
  with TextBounds(C + ' ', Self.Font, FAngle) do
  begin
    Inc(ASize.X, X);
    Inc(ASize.Y, Y);
  end;
  // adicional das bordas
  W := FBorders.Width;
  if W > 0 then
  begin
    Inc(W);
    if FBorders.CanDrawLeft then
      Inc(ASize.X, W);
    if FBorders.CanDrawTop then
      Inc(ASize.Y, W);
    if FBorders.CanDrawRight then
      Inc(ASize.X, W);
    if FBorders.CanDrawBottom then
      Inc(ASize.Y, W);
  end;
end;

procedure TRLCustomAngleLabel.SetAngle(const AValue: Double);
begin
  if AValue = FAngle then
    Exit;
  FAngle := AValue;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomAngleLabel.SetAngleBorders(const AValue: Boolean);
begin
  if AValue = FAngleBorders then
    Exit;
  FAngleBorders := AValue;
  Invalidate;
end;

function TRLCustomAngleLabel.IsAngle: Boolean;
begin
  Result := (abs(FAngle - Round(FAngle)) < 1 / 10);
end;

{ TRLCustomDBText }

constructor TRLCustomDBText.Create(AOwner: TComponent);
begin
  FText := '';
  FDataField := '';
  FDataFormula := '';
  FDataSource := nil;
  FDisplayMask := '';

  inherited Create(AOwner);
end;

procedure TRLCustomDBText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil;
end;

procedure TRLCustomDBText.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBText.SetDataField(const AValue: TRLDataFieldProperty);
begin
  if AValue = FDataField then
    Exit;
  if AValue <> '' then
    FDataFormula := '';
  FDataField := AValue;
  MakeCaption;
end;

procedure TRLCustomDBText.SetDataFormula(const AValue: string);
begin
  if AValue = FDataFormula then
    Exit;
  if AValue <> '' then
    FDataField := '';
  FDataFormula := AValue;
  MakeCaption;
end;

function TRLCustomDBText.GetDataSet: TDataSet;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

function TRLCustomDBText.GetField: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
  begin
    Result := DataSet.FindField(FDataField);
{Fred/Ronaldo - 14/01/2009 - Comentado para não dar erro na DLIB2
    if Result = nil then
      raise Exception.Create(LocaleStrings.LS_NotFoundStr + ': ' +
        Name + '.DataField "' + FDataField + '"');}
  end
  else
    Result := nil;
end;

function TRLCustomDBText.ApplyMask(const AValue: Variant): string;
var
  M: string;
  P: TFloatFormat;
  C: Integer;
  V: Double;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Result := ''
  else if FDisplayMask = '' then
    if Field <> nil then
      if Field is TNumericField then
        with TNumericField(Field) do
        begin
          if EditFormat = '' then
            M := DisplayFormat
          else
            M := EditFormat;
          V := AValue;
          if M <> '' then
            Result := FormatFloat(M, V)
          else
          begin
            if (Field is TFloatField) and TFloatField(Field).Currency then
            begin
              P := ffCurrency;
              {$ifdef HAS_FORMATSETTINGS}
              C := FormatSettings.CurrencyDecimals;
              {$else}
              C := CurrencyDecimals;
              {$endif}
            end
            else
            begin
              P := ffGeneral;
              C := 0;
            end;
            if Field is TFloatField then
              Result := FloatToStrF(V, P, TFloatField(Field).Precision, C)
            else
              Result := FloatToStrF(V, P, 0, C);
          end;
        end
      else
        Result := AValue
    else
      Result := AValue
  else if VarType(AValue) in [varSmallint, varInteger, varSingle,
    varDouble, varCurrency] then
    Result := FormatFloat(FDisplayMask, AValue)
  else
    Result := FormatMaskText(FDisplayMask, AValue);
end;

function TRLCustomDBText.GetFieldText: string;
var
  D: TDataSet;
  F: TField;
  P: TRLCustomReport;
begin
  P := FindParentReport;
  if not IsPreparing then
    if FFriendlyName <> '' then
      Result := FFriendlyName
    else if FDataField <> '' then
      Result := GetFieldLabel
    else if FDataFormula <> '' then
      Result := '(' + FDataFormula + ')'
    else
      Result := Name
  else
  begin
    D := GetDataSet;
    if Assigned(D) and D.Active and not D.Eof then
    begin
      F := GetField;
      if F <> nil then
        Result := SmartGetFieldDisplayText(F, FDisplayMask)
      else if FDataFormula <> '' then
        Result := ApplyMask(P.Parse(Self, FDataFormula))
      else
        Result := '';
    end    
    else
      Result := '';
  end;
end;

function TRLCustomDBText.InternalMakeCaption: string;
var
  I: Integer;
begin
  Result := GetFieldText;
  if FText <> '' then
  begin
    I := Pos('#', FText);
    if I > 0 then
      Result := Copy(FText, 1, I - 1) + Result + Copy(FText, I + 1, Length(FText))
    else
      Result := FText + Result;
  end;
end;

procedure TRLCustomDBText.SetText(const AValue: TCaption);
begin
  if AValue = FText then
    Exit;
  FText := AValue;
  MakeCaption;
end;

function TRLCustomDBText.GetFieldLabel: string;
var
  F: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
    F := DataSet.FindField(FDataField)
  else
    F := nil;
  if F <> nil then
    Result := F.DisplayLabel
  else
    Result := FDataField;
end;

function GetNullValue(AField: TField): Variant;
begin
  if AField <> nil then
    if AField is TNumericField then
      Result := 0
    else if AField is TBooleanField then
      Result := False
    else if AField is TStringField then
      Result := ''
    else if AField is TDateTimeField then
      Result := 0
    else if AField is TBinaryField then
      Result := ''
    else if AField is TBlobField then
      Result := ''
    {$ifndef FPC}
    else if AField is TObjectField then
      Result := 0
    {$endif}
    else if AField is TVariantField then
      Result := 0
    {$ifndef FPC}
    else if AField is TInterfaceField then
      Result := ''
    {$endif}
    else
      Result := Null
  else
    Result := Null;
end;

{ TRLCustomDBResult }

constructor TRLCustomDBResult.Create(AOwner: TComponent);
begin
  FInfo := riSimple;
  FResetAfterPrint := False;
  FMustResetValue := False;
  FBuiltInRegs := nil;
  FComputeNulls := True;

  FBuiltInRegs := TObjectList.Create;

  inherited Create(AOwner);

  Initialize;
end;

destructor TRLCustomDBResult.Destroy;
begin
  inherited;

  if Assigned(FBuiltInRegs) then
    FBuiltInRegs.Free;
end;

procedure TRLCustomDBResult.InternalPrint;
begin
  inherited;

  if FResetAfterPrint then
    FMustResetValue := True;
end;

procedure TRLCustomDBResult.Evaluate(var FieldText: string; var FieldValue: Variant);
var
  DatasetRef: TDataSet;
  FieldRef: TField;
begin
  DatasetRef := DataSet;
  FieldRef := Field;
  FieldValue := Null;
  FieldText := '';
  if FDataFormula <> '' then
  begin
    FieldValue := RequestParentReport.Parse(Self, FDataFormula);
    if (VarIsNull(FieldValue) or VarIsEmpty(FieldValue)) and FComputeNulls then
      FieldValue := GetNullValue(Field);
    FieldText := VarToStr(FieldValue);
  end
  else if Assigned(DatasetRef) and DatasetRef.Active and not
    DatasetRef.Eof and (FieldRef <> nil) then
  begin
    FieldValue := FieldRef.Value;
    FieldText := VarToStr(SmartGetFieldDisplayText(FieldRef, FDisplayMask));
  end;
end;

procedure TRLCustomDBResult.InitializeRegs;
var
  I: Integer;
begin
  for I := 0 to FBuiltInRegs.Count - 1 do
    with TRLDBResultBuiltIn(FBuiltInRegs[I]) do
    begin
      Count := 0;
      Sum := 0;
    end;
end;

procedure TRLCustomDBResult.Initialize;
begin
  FNullValue := GetNullValue(Field);
  FCount := 0;
  FSum := 0;
  FMax := FNullValue;
  FMin := FNullValue;
  FLast := FNullValue;
  FFirst := FNullValue;
  FLastText := '';
  FFirstText := '';
  FSimple := FNullValue;
  FBuiltInRegs.Clear;

  Evaluate(FFirstText, FFirst);
  FMin := FFirst;
  FMax := FFirst;
  FLast := FFirst;
  FLastText := FFirstText;
  FSimple := FFirst;
  InitializeRegs;
end;

procedure TRLCustomDBResult.ComputeDetail(ACaller: TObject);
var
  fieldvalue: Variant;
  fieldtext: string;
  computeit: Boolean;
begin
  inherited;
  if FMustResetValue then
  begin
    Initialize;
    FMustResetValue := False;
  end;
  Evaluate(fieldtext, fieldvalue);
  computeit := True;
  if Assigned(FOnCompute) then
    FOnCompute(Self, fieldvalue, fieldtext, computeit);
  if not computeit then
    Exit;
  if VarIsNull(fieldvalue) or VarIsEmpty(fieldvalue) then
    if FComputeNulls then
      fieldvalue := GetNullValue(Field)
    else
      Exit;
  FSimple := fieldvalue;
  Inc(FCount);
  if FCount = 1 then
  begin
    FFirst := fieldvalue;
    FFirstText := fieldtext;
  end;
  FLast := fieldvalue;
  FLastText := fieldtext;
{$ifdef SUPPORTS_VARIANT}
  if VarType(fieldvalue) in [varSmallint, varInteger, varSingle, varDouble, varCurrency] then
    FSum := FSum + fieldvalue;
{$else}
  if VarIsNumeric(fieldvalue) then
    FSum := FSum + fieldvalue;
{$endif}
  if (FCount = 1) or (fieldvalue > FMax) then
    FMax := fieldvalue;
  if (FCount = 1) or (fieldvalue < FMin) then
    FMin := fieldvalue;
end;

function TRLCustomDBResult.GetFieldText: string;
var
  N: string;
begin
  if not IsPreparing then
  begin
    if FFriendlyName <> '' then
      N := FFriendlyName
    else if FDataField <> '' then
      N := GetFieldLabel
    else if FDataFormula <> '' then
      N := '(' + FDataFormula + ')'
    else
      N := '';
    case FInfo of
      riAverage: Result := '(Average ' + N + ')';
      riCount: Result := '(Count)';
      riFirst: Result := '(First ' + N + ')';
      riLast: Result := '(Last ' + N + ')';
      riMax: Result := '(Max ' + N + ')';
      riMin: Result := '(Min ' + N + ')';
      riSum: Result := '(Sum ' + N + ')';
      riFirstText: Result := '(FirstText ' + N + ')';
      riLastText: Result := '(LastText ' + N + ')';
      riSimple: Result := '(Simple ' + N + ')';
    end;
  end
  else if VarIsNull(Self.Value) or VarIsEmpty(Self.Value) then
    Result := ''
  else
    case FInfo of
      riCount: Result := Self.Value;
      riFirstText,
      riLastText: Result := Self.Value;
    else
      Result := ApplyMask(Self.Value);
    end;
end;

function TRLCustomDBResult.GetValue: Variant;
begin
  case FInfo of
    riAverage: if FCount = 0 then
        Result := 0
      else
        Result := FSum / FCount;
    riCount: Result := FCount;
    riMax: Result := FMax;
    riMin: Result := FMin;
    riSum: Result := FSum;
    riFirst: Result := FFirst;
    riLast: Result := FLast;
    riFirstText: Result := FFirstText;
    riLastText: Result := FLastText;
    riSimple: Result := FSimple;
  else
    Result := GetNullValue(Field);
  end;
end;

function TRLCustomDBResult.GetAttribute(const AName: string): Variant;
begin
  Result := GetValue;
end;

function TRLCustomDBResult.Resolve(Sender: TObject; const Identifier: string;
  Params: Variant): Variant;
var
  id: Integer;
begin
  id := TRLExpressionParser(Sender).IdentifierId;
  if AnsiSameText(Identifier, 'count') then
    Result := BuiltInCount(id)
  else if AnsiSameText(Identifier, 'sum') then
    Result := BuiltInSum(id, Params[0])
  else if AnsiSameText(Identifier, 'min') then
    Result := BuiltInMin(id, Params[0])
  else if AnsiSameText(Identifier, 'max') then
    Result := BuiltInMax(id, Params[0])
  else if AnsiSameText(Identifier, 'avg') then
    Result := BuiltInAvg(id, Params[0])
  else if AnsiSameText(Identifier, 'first') then
    Result := BuiltInFirst(id, Params[0])
  else if AnsiSameText(Identifier, 'last') then
    Result := BuiltInLast(id, Params[0])
  else
    Result := Unassigned;
end;

procedure TRLCustomDBResult.SetInfo(const AValue: TRLResultInfo);
begin
  if AValue = FInfo then
    Exit;
  FInfo := AValue;
  MakeCaption;
end;

function TRLCustomDBResult.BuiltIn(AId: Integer;
  ACanCreate: Boolean = True): TRLDBResultBuiltIn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FBuiltInRegs.Count - 1 do
    if TRLDBResultBuiltIn(FBuiltInRegs[I]).Id = AId then
    begin
      Result := TRLDBResultBuiltIn(FBuiltInRegs[I]);
      Break;
    end;
  if (Result = nil) and ACanCreate then
  begin
    Result := TRLDBResultBuiltIn.Create;
    Result.Id := AId;
    Result.Count := 0;
    Result.Max := Null;
    Result.Min := Null;
    Result.Sum := 0;
    Result.First := Null;
    Result.Last := Null;
    FBuiltInRegs.Add(Result);
  end;
end;

function TRLCustomDBResult.BuiltInCount(AId: Integer): Variant;
begin
  with BuiltIn(AId) do
  begin
    Inc(Count);
    Result := Count;
  end;
end;

function TRLCustomDBResult.BuiltInSum(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    Sum := Sum + AValue;
    Result := Sum;
  end;
end;

function TRLCustomDBResult.BuiltInMin(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    if VarIsNull(Min) or VarIsEmpty(Min) or (AValue < Min) then
      Min := AValue;
    Result := Min;
  end;
end;

function TRLCustomDBResult.BuiltInMax(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    if VarIsNull(Max) or VarIsEmpty(Max) or (AValue < Max) then
      Max := AValue;
    Result := Max;
  end;
end;

function TRLCustomDBResult.BuiltInAvg(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    Inc(Count);
    Sum := Sum + AValue;
    Result := Sum / Count;
  end;
end;

function TRLCustomDBResult.BuiltInFirst(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    Inc(Count);
    if Count = 1 then
      First := AValue;
    Result := First;
  end;
end;

function TRLCustomDBResult.BuiltInLast(AId: Integer; AValue: Variant): Variant;
begin
  with BuiltIn(AId) do
  begin
    Last := AValue;
    Result := Last;
  end;
end;

{ RunMemo }

function CanvasTextWidth(Canvas: TObject; const AText: string): Integer;
begin
  if Canvas is TRLGraphicSurface then
    Result := (Canvas as TRLGraphicSurface).TextWidth(AText)
  else if Canvas is TCanvas then
    Result := (Canvas as TCanvas).TextWidth(AText)
  else
    Result := 0;
end;

function CanvasTextHeight(Canvas: TObject; const AText: string): Integer;
begin
  if Canvas is TRLGraphicSurface then
    Result := (Canvas as TRLGraphicSurface).TextHeight(AText)
  else if Canvas is TCanvas then
    Result := (Canvas as TCanvas).TextHeight(AText)
  else
    Result := 0;
end;

function NextLine(const Buffer: string; var Pos: Integer; var Wrapped: Boolean;
  var LineWidth: Integer; Canvas: TObject; MaxWidth: Integer): string;
var
  Pos0, PosAux: Integer;
  HasText: Boolean;

  function GetALine: String;
  begin
    Result := Copy(Buffer, Pos0, Pos - Pos0)
  end;

begin
  Wrapped := False;
  HasText := False;

  Pos0 := Pos;
  while True do
    if Pos <= Length(Buffer) then
      if CharInSet(Buffer[Pos], [#9, #32]) then
      begin
        LineWidth :=  CanvasTextWidth(Canvas, GetALine);
        if (LineWidth > MaxWidth) and HasText then
        begin
          Wrapped := True;
          Result := GetALine;
          while (Pos <= Length(Buffer)) and CharInSet(Buffer[Pos], [#9, #32]) do
            Inc(Pos);
          Break;
        end;
        Inc(Pos);
      end
      else if CharInSet(Buffer[Pos], [#13, #10]) then
      begin
        Result := GetALine;
        Inc(Pos);
        if (Pos <= Length(Buffer)) and CharInSet(Buffer[Pos], [#10]) then
          Inc(Pos);
        Break;
      end
      else
      begin
        PosAux := Pos;
        while (Pos <= Length(Buffer)) and not CharInSet(Buffer[Pos], [#9, #32, #13, #10]) do
          Inc(Pos);
        LineWidth := CanvasTextWidth(Canvas, GetALine);
        if (LineWidth > MaxWidth) and HasText then
        begin
          Pos := PosAux;
          Wrapped := True;
          Result := GetALine;
          while (Pos <= Length(Buffer)) and CharInSet(Buffer[Pos], [#9, #32]) do
            Inc(Pos);
          Break;
        end;
        HasText := True;
      end
    else
    begin
      Result := GetALine;
      Break;
    end;
end;

procedure CanvasTextRect(Canvas: TObject; const ARect: TRect;
  const AText: string; Alignment: TRLTextAlignment = taLeftJustify);
var
  X, Y: Integer;
begin
  if Canvas is TRLGraphicSurface then
  begin
    case Alignment of
      taCenter: X := (ARect.Left + ARect.Right -
          (Canvas as TRLGraphicSurface).TextWidth(AText)) div 2;
      taRightJustify: X := ARect.Right - (Canvas as TRLGraphicSurface).TextWidth(AText);
      taJustify: X := ARect.Left;
    else // taLeftJustify
      X := ARect.Left;
    end;
    Y := ARect.Top;
    (Canvas as TRLGraphicSurface).TextRectEx(ARect, X, Y, AText,
      ToMetaTextAlignment(TAlignment(Alignment)), ToMetaTextLayout(TTextLayout(tlTop)),
      MetaTextFlagIntegralHeight or MetaTextFlagAutoSize);
  end
  else if Canvas is TCanvas then
    CanvasTextRectEx(Canvas as TCanvas, ARect, ARect.Left, ARect.Top,
      AText, ToMetaTextAlignment(TAlignment(Alignment)), ToMetaTextLayout(TTextLayout(tlTop)),
      MetaTextFlagIntegralHeight);
end;

procedure RunMemo(const Buffer: string; Canvas: TObject;
  Alignment: TRLTextAlignment; const ARect: TRect; MaxWidth, MaxHeight: Integer;
  var Size: TPoint);
var
  LineHeight: Integer;
  LineWidth: Integer;
  LineOffset: Integer;
  Pos: Integer;
  Wrapped: Boolean;
  TextLn: string;
  Aux: TRect;
  LineAlign: TRLTextAlignment;
begin
  Size.X := 0;
  LineHeight := CanvasTextHeight(Canvas, 'A');
  LineOffset := 0;
  Pos := 1;
  while Pos <= Length(Buffer) do
  begin
    TextLn := NextLine(Buffer, Pos, Wrapped, LineWidth, Canvas, MaxWidth);
    if LineWidth > Size.X then
      Size.X := LineWidth;
    Aux := Rect(ARect.Left + 0, ARect.Top + LineOffset, ARect.Left +
      MaxWidth, ARect.Top + LineOffset + LineHeight);
    if Aux.Bottom > ARect.Bottom then
      Aux.Bottom := ARect.Bottom;
    LineAlign := Alignment;
    if (LineAlign = taJustify) and not Wrapped then
      LineAlign := taLeftJustify;
    CanvasTextRect(Canvas, Aux, TextLn, LineAlign);
    Inc(LineOffset, LineHeight);
    if LineOffset > MaxHeight then
      Break;
  end;
  Size.Y := LineOffset;
end;

function MemoSize(const Buffer: string; Font: TFont; MaxWidth: Integer): TPoint;
var
  AuxBitmap: TBitmap;
begin
  AuxBitmap := NeedAuxBitmap;
  AuxBitmap.Canvas.Font.Assign(Font);
  RunMemo(Buffer, AuxBitmap.Canvas, taLeftJustify, Rect(0, 0, MaxWidth, MaxInt),
    MaxWidth, MaxInt, Result);
end;

procedure MemoDraw(const Buffer: string; Canvas: TObject;
  Alignment: TRLTextAlignment; const ARect: TRect; WordWrap: Boolean);
const
  {Fred/Ronaldo - 19/07/2012 - Tamanho máximo de texto possível para não quebrar linha,
   Isto foi necessário para corrigir bug nas funções que usavam MaxWidth somado com outros valores
   e extrapolavam o MaxInt}
  MaxTextWidth = MaxInt div 2;
var
  FooSize: TPoint;
  MaxWidth: Integer;
  MaxHeight: Integer;
begin
  if WordWrap then
    MaxWidth := ARect.Right - ARect.Left
  else
    MaxWidth := MaxTextWidth;
  MaxHeight := ARect.Bottom - ARect.Top;
  RunMemo(Buffer, Canvas, Alignment, ARect, MaxWidth, MaxHeight, FooSize);
end;

{ TRLCustomMultiLine }

constructor TRLCustomMultiLine.Create(AOwner: TComponent);
begin
  FWordWrap := True;
  FIntegralHeight := False;
  inherited Create(AOwner);
  // customization
  Width := 185;
  Height := 89;
  Behavior := Behavior + [beSiteExpander];
  AutoSizeDir := [asHeightDir];
  AutoSize := True;
end;

procedure TRLCustomMultiLine.Paint;
var
  R: TRect;
  S: string;
  P: TRLCustomReport;
begin
  inherited;

  S := Caption;
  if (S = '') and not IsPreparing then
    S := Name;
  R := GetClientRect;
  with Canvas do
  begin
    Font := Self.Font;
    if IsTransparent(Self) then
      Brush.Style := bsClear
    else
    begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Color;
    end;
    MemoDraw(S, Self.Canvas, Self.Alignment, R, Self.WordWrap);
  end;
  P := FindParentReport;
  if not Assigned(P) or P.ShowDesigners then
    DrawBounds;
end;

procedure TRLCustomMultiLine.InternalPrint;
var
  R: TRect;
begin
  inherited;

  R := CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId := PtrInt(Self);
    NewGroupId;
    Font := Self.Font;
    if IsTransparent(Self) then
      Brush.Style := bsClear
    else
    begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Color;
    end;
    MemoDraw(Caption, Self.RequestParentSurface, Self.Alignment, R, Self.WordWrap);
  end;
end;

procedure TRLCustomMultiLine.CalcSize(var ASize: TPoint);
var
  W: Integer;
  aux: TPoint;
  C: string;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  C := Caption;
  if (C = '') and not IsPreparing then
    C := Name;
  // dimensões do texto
  ASize.Y := 0;
  aux := MemoSize(C, Self.Font, ASize.X);
  if aux.Y = 0 then
    Inc(ASize.Y, TextBounds(' ', Self.Font, 0).Y)
  else
    Inc(ASize.Y, aux.Y);
  // adicional das bordas
  W := FBorders.Width;
  if W > 0 then
  begin
    Inc(W);
    if FBorders.CanDrawTop then
      Inc(ASize.Y, W);
    if FBorders.CanDrawBottom then
      Inc(ASize.Y, W);
  end;
end;

procedure TRLCustomMultiLine.SetWordWrap(const AValue: Boolean);
begin
  if AValue = FWordWrap then
    Exit;
  FWordWrap := AValue;
  MakeCaption;
end;

{ TRLCustomMemo }

constructor TRLCustomMemo.Create(AOwner: TComponent);
begin
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := TreatOnChange;

  inherited Create(AOwner);
end;

destructor TRLCustomMemo.Destroy;
begin
  FreeObj(FLines);

  inherited;
end;

function TRLCustomMemo.InternalMakeCaption: string;
begin
  Result := FLines.Text;
  if not IsPreparing and (Trim(Result) = '') then
    Result := GetDefaultCaption;
end;

procedure TRLCustomMemo.SetLines(const AValue: TStrings);
begin
  FLines.Assign(AValue);
end;

procedure TRLCustomMemo.TreatOnChange(Sender: TObject);
begin
  MakeCaption;
end;

{ TRLCustomDBMemo }

constructor TRLCustomDBMemo.Create(AOwner: TComponent);
begin
  FDataField := '';
  FDataSource := nil;

  inherited Create(AOwner);
end;

procedure TRLCustomDBMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil;
end;

procedure TRLCustomDBMemo.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBMemo.SetDataField(const AValue: TRLDataFieldProperty);
begin
  if AValue = FDataField then
    Exit;
  if AValue <> '' then
    FDataFormula := '';
  FDataField := AValue;
  MakeCaption;
end;

procedure TRLCustomDBMemo.SetDataFormula(const AValue: string);
begin
  if AValue = FDataFormula then
    Exit;
  if AValue <> '' then
    FDataField := '';
  FDataFormula := AValue;
  MakeCaption;
end;

function TRLCustomDBMemo.GetDataSet: TDataSet;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

function TRLCustomDBMemo.GetField: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
  begin
    Result := DataSet.FindField(FDataField);
{Fred/Ronaldo/Tiago - 10/05/2010 - Comentado para não dar erro na DLIB2
    if Result = nil then
      raise Exception.Create(LocaleStrings.LS_NotFoundStr + ': ' +
        Name + '.DataField "' + FDataField + '"');}
  end
  else
    Result := nil;
end;

function TRLCustomDBMemo.GetFieldText: string;
var
  D: TDataSet;
  F: TField;
  P: TRLCustomReport;
begin
  P := FindParentReport;
  if not IsPreparing then
    if FFriendlyName <> '' then
      Result := FFriendlyName
    else if FDataField <> '' then
      Result := GetFieldLabel
    else if FDataFormula <> '' then
      Result := '(' + FDataFormula + ')'
    else
      Result := Name
  else
  begin
    D := GetDataSet;
    F := GetField;
    if Assigned(D) and D.Active and not D.Eof then
      if F <> nil then
        Result := SmartGetFieldDisplayText(F)
      else if FDataFormula <> '' then
        Result := P.Parse(Self, FDataFormula)
      else
        Result := ''
    else
      Result := '';
  end;
end;

function TRLCustomDBMemo.InternalMakeCaption: string;
begin
  Result := GetFieldText;
end;

function TRLCustomDBMemo.GetFieldLabel: string;
var
  F: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
    F := DataSet.FindField(FDataField)
  else
    F := nil;
  if F <> nil then
    Result := F.DisplayLabel
  else
    Result := FDataField;
end;

{ TRLCustomImage }

constructor TRLCustomImage.Create(AOwner: TComponent);
begin
  // variables
  FStretch := False;
  FCenter := False;
  FScaled := False;
  // objects
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  inherited Create(AOwner);
  // customization
  Height := 105;
  Width := 105;
  AutoSizeDir := [asWidthDir, asHeightDir];
end;

destructor TRLCustomImage.Destroy;
begin
  FreeObj(FPicture);

  inherited;
end;

procedure TRLCustomImage.CalcSize(var ASize: TPoint);
var
  W: Integer;
begin
  ASize := Point(Width, Height);
  if (FPicture = nil) or not AutoSize then
    Exit;
  // pega size da imagem
  ASize.X := FPicture.Width;
  ASize.Y := FPicture.Height;
  // adicional das bordas
  W := FBorders.Width;
  if W > 0 then
  begin
    Inc(W);
    if FBorders.CanDrawLeft then
      Inc(ASize.X, W);
    if FBorders.CanDrawTop then
      Inc(ASize.Y, W);
    if FBorders.CanDrawRight then
      Inc(ASize.X, W);
    if FBorders.CanDrawBottom then
      Inc(ASize.Y, W);
  end;
end;

procedure TRLCustomImage.Paint;
var
  P: TRLCustomReport;
  R: TRect;
  B: TBitmap;
begin
  inherited;

  R := GetClientRect;
  if (FPicture <> nil) and (FPicture.Graphic <> nil) then
    if FScaled then
    begin
      R := ScaleRect(Rect(0, 0, FPicture.Graphic.Width, FPicture.Graphic.Height),
        R, FCenter);
      Canvas.StretchDraw(R, FPicture.Graphic);
    end
    else if FStretch then
      Canvas.StretchDraw(R, FPicture.Graphic)
    else
    begin
      B := ClipGraphic(FPicture.Graphic, R, FCenter);
      try
        Canvas.StretchDraw(R, B);
      finally
        B.Free;
      end;
    end;

  P := FindParentReport;
  if not Assigned(P) or P.ShowDesigners then
    DrawBounds;
end;

procedure TRLCustomImage.PictureChanged(Sender: TObject);
begin
  if AutoSize and (FPicture.Width > 0) and (FPicture.Height > 0) then
    BoundsRect := Rect(Left, Top, Left + FPicture.Width, Top + FPicture.Height);
end;

procedure TRLCustomImage.InternalPrint;
var
  R: TRect;
begin
  inherited;

  R := CalcPrintClientRect;
  if (FPicture <> nil) and (FPicture.Graphic <> nil) then
    with RequestParentSurface do
    begin
      GeneratorId := PtrInt(Self);
      NewGroupId;
      if FScaled then
        ScaleDraw(R, FPicture.Graphic, FCenter)
      else if FStretch then
        StretchDraw(R, FPicture.Graphic)
      else
        ClipDraw(R, FPicture.Graphic, FCenter);
    end;
end;

procedure TRLCustomImage.SetCenter(const AValue: Boolean);
begin
  if AValue = FCenter then
    Exit;
  FCenter := AValue;
  Invalidate;
end;

procedure TRLCustomImage.SetPicture(const AValue: TPicture);
begin
  FPicture.Assign(AValue);
  Invalidate;
end;

procedure TRLCustomImage.SetStretch(const AValue: Boolean);
begin
  if AValue = FStretch then
    Exit;
  FStretch := AValue;
  if FStretch then
    FScaled := False;
  Invalidate;
end;

procedure TRLCustomImage.SetScaled(const AValue: Boolean);
begin
  if AValue = FScaled then
    Exit;
  FScaled := AValue;
  if FScaled then
    FStretch := False;
  Invalidate;
end;

{ TRLCustomDBImage }

constructor TRLCustomDBImage.Create(AOwner: TComponent);
begin
  FDataField := '';
  FDataSource := nil;

  inherited Create(AOwner);
end;

procedure TRLCustomDBImage.LoadPicture;
var
  F: TField;
begin
  FPicture.Graphic := nil;
  F := GetField;
  if (F <> nil) and F.DataSet.Active and not F.DataSet.Eof then
    FPicture.Assign(F);
end;

procedure TRLCustomDBImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
    begin
      LoadPicture;
      Invalidate;
      FDataSource := nil;
    end;
end;

procedure TRLCustomDBImage.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  LoadPicture;
  Invalidate;
end;

procedure TRLCustomDBImage.SetDataField(const AValue: TRLDataFieldProperty);
begin
  if AValue = FDataField then
    Exit;
  FDataField := AValue;
  LoadPicture;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomDBImage.GetDataSet: TDataSet;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

function TRLCustomDBImage.GetField: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
  begin
    Result := DataSet.FindField(FDataField);
{Fred/Ronaldo/Tiago - 10/05/2010 - Comentado para não dar erro na DLIB2
    if Result = nil then
      raise Exception.Create(LocaleStrings.LS_NotFoundStr + ': ' +
        Name + '.DataField "' + FDataField + '"');}
  end
  else
    Result := nil;
end;

procedure TRLCustomDBImage.InternalPrint;
begin
  LoadPicture;

  inherited;
end;

{ TRLCustomSystemInfo }

constructor TRLCustomSystemInfo.Create(AOwner: TComponent);
begin
  FInfoType := itDate;
  FText := '';
  inherited Create(AOwner);
end;

function TRLCustomSystemInfo.JunctionStr: string;
var
  S, t1, t2: string;
  I: Integer;
  R: TRLReportState;
  P: TRLCustomReport;
  ParentRLReport: TRLCustomReport;
begin
  // a propriedade TEXT pode vir na seguinte forma: "Esta página # o relatório\|continua;encerra";
  t1 := '';
  t2 := '';
  S := FText;
  I := Pos('|', S);
  if I > 0 then
  begin
    // elimina os textos antes da barra e após a segunda barra, se houver
    Delete(S, 1, I);
    I := Pos('|', S);
    if I > 0 then
      S := Copy(S, 1, I - 1);
    // primeiro e segundo parâmetros
    I := Pos(';', S);
    if I = 0 then
      I := Length(S) + 1;
    t1 := Copy(S, 1, I - 1);
    t2 := Copy(S, I + 1, MaxInt);
  end;
  if t1 = '' then
    t1 := GetLocalizeStr(LocaleStrings.LS_PageBreakStr);
  if t2 = '' then
    t2 := GetLocalizeStr(LocaleStrings.LS_ReportEndStr);

  if fPrintEndTextOnNextReport then
  begin
    ParentRLReport:=FindParentReport;
    if ParentRLReport.NextReportState <> nrStart then
      Result:=t2
    else
      Result:=t1;
  end
  else
  begin
    P := LastReport;
    if Assigned(P) then
      R := P.ReportState
    else
      R := rsAbout;
    if (R = rsClosing) then
      Result := t2
    else
      Result := t1;
  end;
end;

const
  CLEARCONST = '^CLEAR';

function TRLCustomSystemInfo.MendStr: string;
var
  S, t1, t2: string;
  I, Q: Integer;
begin
  // a propriedade TEXT pode vir na seguinte forma: "Esta página é a # o relatório\|continuação;";
  t1 := '';
  t2 := '';
  S := FText;
  I := Pos('|', S);
  if I > 0 then
  begin
    // elimina os textos antes da barra e após a segunda barra, se houver
    Delete(S, 1, I);
    I := Pos('|', S);
    if I > 0 then
      S := Copy(S, 1, I - 1);
    // primeiro e segundo parâmetros
    I := Pos(';', S);
    if I = 0 then
      I := Length(S) + 1;
    t1 := Copy(S, 1, I - 1);
    t2 := Copy(S, I + 1, MaxInt);
  end;
  if t1 = '' then
    t1 := GetLocalizeStr(LocaleStrings.LS_PageMendStr);
  if t2 = '' then
    t2 := CLEARCONST;

  Q := RequestParentPager.DetailCount;
  if Q > 0 then
    Result := t1
  else
    Result := t2;
end;

function LastPageNumberMacroName(R: TRLCustomReport): string;
var
  I: Integer;
begin
  I := R.CompositeIndex;
  if I = 0 then
    Result := 'LastPageNumber'
  else
    Result := 'LastPageNumber_' + IntToStr(I);
end;

function TRLCustomSystemInfo.InternalMakeCaption: string;
var
  I, P: Integer;
  S, V, lp: string;
  R, M: TRLCustomReport;
begin
  R := FindParentReport;
  M := MasterReport;

  if not IsPreparing then
    if FFriendlyName <> '' then
      S := FFriendlyName
    else if FInfoType = itPagePreview then
      S := '(' + InfoTypeNames[itPageNumber] + ')' + #9 + '(' +
        InfoTypeNames[itLastPageNumber] + ')'
    else
      S := '(' + InfoTypeNames[FInfoType] + ')'
  else
  begin
    lp := LastPageNumberMacroName(R);
    case FInfoType of
      itCarbonCopy: S := IntToStr(FindParentBand.CarbonIndex + 1);
      itDate: S := DateToStr(M.ReportDateTime);
      itDetailCount: S := IntToStr(FindParentPager.DetailCount);
      {$ifdef HAS_FORMATSETTINGS}
      itFullDate: S := FormatDateTime(FormatSettings.LongDateFormat, M.ReportDateTime);
      {$else}
      itFullDate: S := FormatDateTime(LongDateFormat, M.ReportDateTime);
      {$endif}
      itHour: S := TimeToStr(M.ReportDateTime);
      itJunction: S := JunctionStr;
      itLastPageNumber: S := '{' + lp + '}';
      itMend: S := MendStr;
      itNow: S := DateTimeToStr(M.ReportDateTime);
      itPageNumber: S := IntToStr(R.PageNumber);
      itPagePreview: S := IntToStr(R.PageNumber) + #9 + '{' + lp + '}';
      itTitle: S := R.Title;
      itRecNo: S := IntToStr(FindParentSkipper.RecNo);
      itCopyNo: S := IntToStr(FindParentSkipper.CopyNo);
    end;
  end;
  // brecha para eliminar o texto
  if Pos(CLEARCONST, S) > 0 then
    Result := ''
  else
  begin
    // elimina opções embutidas em TEXT
    Result := FText;
    I := Pos('|', Result);
    if I > 0 then
      Result := Copy(Result, 1, I - 1);
    // substitui parâmetros em TEXT
    repeat
      // próximo parâmetro de S
      P := Pos(#9, S);
      if P = 0 then
        P := Length(S) + 1;
      V := Copy(S, 1, P - 1);
      Delete(S, 1, P);
      // próximo lugar em Result
      I := Pos('#', Result);
      if I = 0 then
        Result := Result + V
      else
      begin
        Delete(Result, I, 1);
        Insert(V, Result, I);
      end;
    until S = '';
  end;
end;

procedure TRLCustomSystemInfo.SetInfoType(const AValue: TRLInfoType);
begin
  if AValue = FInfoType then
    Exit;
  FInfoType := AValue;
  MakeCaption;
end;

procedure TRLCustomSystemInfo.SetText(const AValue: TCaption);
begin
  if AValue = FText then
    Exit;
  FText := AValue;
  MakeCaption;
end;

{ TRLCustomDraw }

constructor TRLCustomDraw.Create(AOwner: TComponent);
begin
  // initialization
  FAngle := 0;
  FDrawKind := dkRectangle;
  FCenter := True;
  FDrawData := nil;
  FDrawWidth := 0;
  FDrawHeight := 0;
  FOptions := [];
  // objects
  FBrush := TBrush.Create;
  FBrush.OnChange := ChangeResponse;
  FPen := TPen.Create;
  FPen.OnChange := ChangeResponse;
  FDrawData := TStringList.Create;

  inherited Create(AOwner);
  // customization
  Width := 48;
  Height := 48;
end;

destructor TRLCustomDraw.Destroy;
begin
  FreeObj(FDrawData);
  FreeObj(FBrush);
  FreeObj(FPen);

  inherited;
end;

procedure PointArray(APoints: array of TPoint; var ADest: TPointArray);
var
  I: Integer;
begin
  SetLength(ADest, High(APoints) + 1);
  for I := 0 to High(APoints) do
    ADest[I] := APoints[I];
end;

procedure RectToPoints(const ARect: TRect; var APoints: TPointArray);
begin
  SetLength(APoints, 4);
  APoints[0].X := ARect.Left;
  APoints[0].Y := ARect.Top;
  APoints[1].X := ARect.Right;
  APoints[1].Y := ARect.Top;
  APoints[2].X := ARect.Right;
  APoints[2].Y := ARect.Bottom;
  APoints[3].X := ARect.Left;
  APoints[3].Y := ARect.Bottom;
end;

procedure ProduceRectanglePoints(var ADest: TPointArray);
begin
  PointArray([Point(0, 0), Point(1, 0), Point(1, 1), Point(0, 1)], ADest);
end;

procedure ProduceElipsePoints(var ADest: TPointArray);
const
  MaxPoints = 36;
  Axis = 1000;
var
  D, S, C: Double;
  I: Integer;
begin
  SetLength(ADest, MaxPoints);
  I := 0;
  while I < MaxPoints do
  begin
    D := 2 * Pi * I / MaxPoints;
    S := Sin(D);
    C := Cos(D);
    ADest[I] := Point(Round(Axis + C * Axis), Round(Axis + S * Axis));
    Inc(I);
  end;
end;

procedure ProduceLinePoints(var ADest: TPointArray);
begin
  PointArray([Point(0, 0), Point(1, 0)], ADest);
end;

procedure ProduceTriaglePoints(var ADest: TPointArray);
begin
  PointArray([Point(0, 87), Point(50, 0), Point(100, 87)], ADest);
end;

procedure ProduceArrowPoints(var ADest: TPointArray);
begin
  PointArray([Point(0, 2), Point(5, 2), Point(5, 0), Point(8, 3),
    Point(5, 6), Point(5, 4), Point(0, 4)], ADest);
end;

function PointsToStr(const APoints: TPointArray): string;
var
  len, I: Integer;
begin
  Result := '';
  len := High(APoints) + 1;
  for I := 0 to len - 1 do
  begin
    if I > 0 then
      Result := Result + sLineBreak;
    Result := Result + IntToStr(APoints[I].X) + ' ' + IntToStr(APoints[I].Y);
  end;
end;

procedure ProducePolygonPoints(var ADest: TPointArray; const APoints: string);

  function NextInt(var I, N: Integer): Boolean;
  const
    SpaceSet = [#32, #9, #13, #10, #26];
    NumSet = ['0'..'9'];
  var
    M: Integer;
  begin
    Result := False;
    while (I <= Length(APoints)) and CharInSet(APoints[I], SpaceSet) do
      Inc(I);
    M := I;
    while (I <= Length(APoints)) and CharInSet(APoints[I], NumSet) do
      Inc(I);
    if I - M > 0 then
    begin
      N := StrToIntDef(Copy(APoints, M, I - M), 0);
      Result := True;
    end;
  end;

var
  I, Q: Integer;
  P: TPoint;
begin
  // conta os pontos
  Q := 0;
  I := 1;
  while NextInt(I, P.X) do
    if NextInt(I, P.Y) then
      Inc(Q);
  // popula
  SetLength(ADest, Q);
  Q := 0;
  I := 1;
  while NextInt(I, P.X) do
    if NextInt(I, P.Y) then
    begin
      ADest[Q] := P;
      Inc(Q);
    end;
end;

procedure TRLCustomDraw.ProducePoints(var ADest: TPointArray);
begin
  case FDrawKind of
    dkRectangle: ProduceRectanglePoints(ADest);
    dkTriangle: ProduceTriaglePoints(ADest);
    dkLine: ProduceLinePoints(ADest);
    dkElipse: ProduceElipsePoints(ADest);
    dkArrow: ProduceArrowPoints(ADest);
    dkCustom: ProducePolygonPoints(ADest, FDrawData.Text);
  else
    SetLength(ADest, 0);
  end;
end;

procedure TRLCustomDraw.ScaleToFit(var APoints: TPointArray; const ARect: TRect);
var
  R: TRect;
  N: Integer;
begin
  R := ARect;
  if FDrawWidth <> 0 then
    R.Right := R.Left + FDrawWidth;
  if FDrawHeight <> 0 then
    R.Bottom := R.Top + FDrawHeight;
  if doKeepVisible in FOptions then
  begin
    N := Math.Min(R.Right - R.Left, R.Bottom - R.Top);
    R.Right := R.Left + N;
    R.Bottom := R.Top + N;
  end;
  if doKeepAspectRatio in FOptions then
    ScalePoints(APoints, R)
  else
    StretchPoints(APoints, R);
end;

procedure TRLCustomDraw.Paint;
var
  R: TRect;
  P: TPointArray;
begin
  inherited;

  R := GetClientRect;
  with Canvas do
  begin
    Brush := Self.Brush;
    Pen := Self.Pen;

    Dec(R.Right);
    Dec(R.Bottom);
    if Pen.Width > 1 then
    begin
      Inc(R.Left, Pen.Width div 2);
      Inc(R.Top, Pen.Width div 2);
      Dec(R.Right, (Pen.Width - 1) div 2);
      Dec(R.Bottom, (Pen.Width - 1) div 2);
    end;

    ProducePoints(P);
    ScaleToFit(P, R);
    RotatePoints(P, FAngle);
    if doKeepSize in FOptions then
    else
      ScaleToFit(P, R);
    if FCenter then
      CenterPoints(P, R);
    Polygon(P);
  end;
end;

procedure TRLCustomDraw.InternalPrint;
var
  R: TRect;
  P: TPointArray;
begin
  inherited;

  R := CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId := PtrInt(Self);
    NewGroupId;
    Brush := Self.Brush;
    Pen := Self.Pen;

    Dec(R.Right);
    Dec(R.Bottom);
    if Pen.Width > 1 then
    begin
      Inc(R.Left, Pen.Width div 2);
      Inc(R.Top, Pen.Width div 2);
      Dec(R.Right, (Pen.Width - 1) div 2);
      Dec(R.Bottom, (Pen.Width - 1) div 2);
    end;

    ProducePoints(P);
    ScaleToFit(P, R);
    RotatePoints(P, FAngle);
    if doKeepSize in FOptions then
    else
      ScaleToFit(P, R);
    if FCenter then
      CenterPoints(P, R);
    Polygon(P);
  end;
end;

procedure TRLCustomDraw.ChangeResponse(Sender: TObject);
begin
  Invalidate;
end;

procedure TRLCustomDraw.SetAngle(const AValue: Double);
begin
  if AValue = FAngle then
    Exit;
  FAngle := AValue;
  Invalidate;
end;

procedure TRLCustomDraw.SetBrush(const AValue: TBrush);
begin
  FBrush.Assign(AValue);
  Invalidate;
end;

procedure TRLCustomDraw.SetDrawKind(const AValue: TRLDrawKind);
var
  P: TPointArray;
begin
  if AValue = FDrawKind then
    Exit;
  FDrawKind := AValue;
  if FDrawKind <> dkCustom then
  begin
    ProducePoints(P);
    FDrawData.Text := PointsToStr(P);
  end;
  Invalidate;
end;

procedure TRLCustomDraw.SetPen(const AValue: TPen);
begin
  FPen.Assign(AValue);
  Invalidate;
end;

function TRLCustomDraw.IsAngle: Boolean;
begin
  Result := (Abs(FAngle - Round(FAngle)) < 1 / 10);
end;

function TRLCustomDraw.IsDrawData: Boolean;
begin
  Result := (FDrawKind in [dkCustom]);
end;

procedure TRLCustomDraw.SetDrawData(const Value: TStrings);
begin
  if Value.Text = FDrawData.Text then
    Exit;
  FDrawData.Assign(Value);
  FDrawKind := dkCustom;
  Invalidate;
end;

procedure TRLCustomDraw.SetCenter(const Value: Boolean);
begin
  if FCenter = Value then
    Exit;
  FCenter := Value;
  Invalidate;
end;

procedure TRLCustomDraw.ReadKind(Reader: TReader);
var
  kindname: string;
begin
  kindname := Reader.ReadIdent;
  if AnsiSameText(kindname, 'dkRectangle') then
  begin
    FDrawKind := dkRectangle;
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkLine') then
  begin
    FDrawKind := dkCustom;
    FDrawData.Text := '0 0 1 1';
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkTriangle') then
  begin
    FDrawKind := dkTriangle;
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkElipse') then
  begin
    FDrawKind := dkElipse;
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkArrow') then
  begin
    FDrawKind := dkArrow;
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkCircle') then
  begin
    FDrawKind := dkElipse;
    FOptions := [doKeepAspectRatio];
  end
  else if AnsiSameText(kindname, 'dkHorzLine') then
  begin
    FDrawKind := dkLine;
    FOptions := [doKeepSize];
  end
  else if AnsiSameText(kindname, 'dkVertLine') then
  begin
    FDrawKind := dkLine;
    FAngle := FAngle + 90;
    FOptions := [doKeepSize, doKeepVisible];
  end
  else if AnsiSameText(kindname, 'dkReverseLine') then
  begin
    FDrawKind := dkCustom;
    FDrawData.Text := '1 0 0 1';
    FOptions := [doKeepSize];
  end;
end;

procedure TRLCustomDraw.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Kind', ReadKind, nil, False);
end;

procedure TRLCustomDraw.SetDrawHeight(const Value: Integer);
begin
  if Value = FDrawHeight then
    Exit;
  FDrawHeight := Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomDraw.SetDrawWidth(const Value: Integer);
begin
  if Value = FDrawWidth then
    Exit;
  FDrawWidth := Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomDraw.SetOptions(const Value: TRLDrawOptions);
begin
  if Value = FOptions then
    Exit;
  FOptions := Value;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomDraw.IsDrawSize: Boolean;
begin
  Result := (FDrawWidth <> 0) or (FDrawHeight <> 0);
end;

{ TRLCustomSite }

constructor TRLCustomSite.Create(AOwner: TComponent);
begin
  // initialization
  FOnDraw := nil;
  // objects
  FBackground := TRLBackground.Create(Self);
  FDegrade := TRLDegradeEffect.Create(Self);
  FInsideMargins := TRLMargins.Create(Self);
  FMargins := TRLMargins.Create(Self);
  FSurface := TRLGraphicSurface.Create;

  inherited Create(AOwner);
  // customization
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csOpaque, csDoubleClicks, csReplicatable];
end;

destructor TRLCustomSite.Destroy;
begin
  FreeObj(FSurface);
  FreeObj(FMargins);
  FreeObj(FInsideMargins);
  FreeObj(FDegrade);
  FreeObj(FBackground);

  inherited;
end;

// anula alinhamento natural do delphi
procedure TRLCustomSite.AlignControls(AControl: TControl; var Rect: TRect);
begin
end;

// novo alinhamento de controles
procedure TRLCustomSite.AlignControls(ARect: TRect);
type
  TAlignControlArray = array[TRLControlAlign] of TList;
var
  alignarray: TAlignControlArray;
  align: TRLControlAlign;
  control: TControl;
  anchors: TRLControlAnchors;
  alignrect: TRect;
  auxrect: TRect;
  leftrect: TRect;
  rightrect: TRect;
  L: TList;
  I, J, W, H: Integer;

  // retorna TRUE se os controles na ordem correta segundo o alinhamento e suas posições
  function IsOrdered(AControl1, AControl2: TControl; AAlign: TRLControlAlign): Boolean;
  begin
    case AAlign of
      faTop: Result := (AControl1.Top < AControl2.Top);
      faBottom: Result := (AControl1.Top > AControl2.Top);
      faLeft: Result := (AControl1.Left < AControl2.Left);
      faLeftMost: Result := (AControl1.Left < AControl2.Left);
      faClient: Result := (AControl1.Left < AControl2.Left);
      faRight: Result := (AControl1.Left > AControl2.Left);
      faRightMost: Result := (AControl1.Left > AControl2.Left);
      faLeftTop: Result := (AControl1.Left < AControl2.Left) and
          (AControl1.Top < AControl2.Top);
      faRightTop: Result := (AControl1.Left > AControl2.Left) and
          (AControl1.Top < AControl2.Top);
      faLeftBottom: Result := (AControl1.Left < AControl2.Left) and
          (AControl1.Top > AControl2.Top);
      faRightBottom: Result :=
          (AControl1.Left > AControl2.Left) and (AControl1.Top > AControl2.Top);
      faCenter: Result := (AControl1.Left < AControl2.Left);
      faCenterLeft: Result := (AControl1.Top < AControl2.Top);
      faCenterTop: Result := (AControl1.Left < AControl2.Left);
      faCenterRight: Result := (AControl1.Top < AControl2.Top);
      faCenterBottom: Result := (AControl1.Left > AControl2.Left);
      faClientLeft: Result := (AControl1.Top < AControl2.Top);
      faClientTop: Result := (AControl1.Left < AControl2.Left);
      faClientRight: Result := (AControl1.Top < AControl2.Top);
      faClientBottom: Result := (AControl1.Left > AControl2.Left);
      faHeight: Result := (AControl1.Top < AControl2.Top);
      faWidth: Result := (AControl1.Left < AControl2.Left);
      faLeftOnly: Result := (AControl1.Left < AControl2.Left);
      faRightOnly: Result := (AControl1.Left > AControl2.Left);
      faTopOnly: Result := (AControl1.Top < AControl2.Top);
      faBottomOnly: Result := (AControl1.Top > AControl2.Top);
    else
      Result := True;
    end;
  end;

  // retorna nível de alinhamento (prioridade)
  function AlignPriority(AControl: TControl): Integer;
  begin
    if IsStaticCustomControl(AControl) then
      Result := 0
    else if AControl is TRLCustomBand then
      case TRLCustomBand(AControl).BandType of
        btHeader: Result := 10;
        btTitle: Result := 20;
        btColumnHeader: Result := 30;
        btDetail: Result := 40;
        btColumnFooter: Result := 50;
        btSummary: Result := 60;
        btFooter: Result := 70;
      else
        Result := 10;
      end
    else if AControl is TRLCustomSubDetail then
      case TRLCustomSubDetail(AControl).Positioning of
        btHeader: Result := 11;
        btTitle: Result := 21;
        btColumnHeader: Result := 31;
        btDetail: Result := 41;
        btColumnFooter: Result := 51;
        btSummary: Result := 61;
        btFooter: Result := 71;
      else
        Result := 100;
      end
    else if AControl is TRLCustomGroup then
      Result := 32
    else
      Result := 100;
  end;

  // retorna ID do grupo de controles
  function AlignGroup(AControl: TControl): Integer;
  begin
    if AControl is TRLCustomBand then
      Result := TRLCustomBand(AControl).GroupIndex
    else
      Result := 0;
  end;

  // retorna TRUE se os controles na ordem correta segundo grupos, níveis e suas posições perante a um alinhamento
  function IsPrior(AControl1, AControl2: TControl; AAlign: TRLControlAlign): Boolean;
  var
    prio1, prio2, group1, group2: Integer;
    ctrl1, ctrl2: TControl;
  begin
    ctrl1 := ControlWithin(AControl1);
    ctrl2 := ControlWithin(AControl2);
    prio1 := AlignPriority(ctrl1);
    prio2 := AlignPriority(ctrl2);
    if prio1 = prio2 then
      if (ctrl1 is TRLCustomBand) and (ctrl2 is TRLCustomBand) then
      begin
        group1 := AlignGroup(ctrl1);
        group2 := AlignGroup(ctrl2);
        if group1 = group2 then
          Result := IsOrdered(AControl1, AControl2, AAlign)
        else
          Result := (group1 < group2);
      end
      else
        Result := IsOrdered(AControl1, AControl2, AAlign)
    else
      Result := (prio1 < prio2);
  end;

  // adiciona controle numa lista na posição ideal para o alinhamento
  procedure AddToList(AControl: TControl; var AArray: TAlignControlArray);
  var
    A: TRLControlAlign;
    I: Integer;
  begin
    A := GetControlAlignOf(AControl);
    if A = faNone then
      I := AArray[A].Count
    else
    begin
      I := 0;
      while (I <= AArray[A].Count - 1) and not IsPrior(AControl,
          TControl(AArray[A][I]), A) do
        Inc(I);
    end;
    if I = AArray[A].Count then
      AArray[A].Add(AControl)
    else
      AArray[A].Insert(I, AControl);
  end;

  procedure SetControlBoundsRect(AControl: TControl; ABoundsRect: TRect);
  var
    ctrl: TControl;
  begin
    ctrl := ControlWithin(AControl);
    if ctrl <> AControl then
    begin
      OffsetRect(ABoundsRect, -AControl.Left, -AControl.Top);
      AControl := ctrl;
    end;
    AControl.BoundsRect := ABoundsRect;
  end;

begin
  // limpa vetor de listas
  for align := Low(TRLControlAlign) to High(TRLControlAlign) do
    alignarray[align] := nil;
  try
    // criar listas de alinhamento
    for align := Low(TRLControlAlign) to High(TRLControlAlign) do
      alignarray[align] := TList.Create;
    // adiciona controles às listas de alinhamento
    for I := 0 to ControlCount - 1 do
    begin
      control := Controls[I];
      if not (csDesigning in ComponentState) and not control.Visible then
        Continue;
      AddToList(control, alignarray);
    end;

    // retângulo de alinhamento
    alignrect := ARect;

    // alinhamentos de alta prioridade: leftmost, rightmost
    L := alignarray[faLeftMost];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Left,
        alignrect.Top, alignrect.Left + control.Width, alignrect.Bottom));
      Inc(alignrect.Left, control.Width);
    end;
    L := alignarray[faRightMost];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Right -
        control.Width, alignrect.Top, alignrect.Right, alignrect.Bottom));
      Dec(alignrect.Right, control.Width);
    end;

    // alinhamentos de média prioridade: top,bottom
    L := alignarray[faTop];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Left,
        alignrect.Top, alignrect.Right, alignrect.Top + control.Height));
      Inc(alignrect.Top, control.Height);
    end;
    L := alignarray[faBottom];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Left,
        alignrect.Bottom - control.Height, alignrect.Right, alignrect.Bottom));
      Dec(alignrect.Bottom, control.Height);
    end;

    // alinhamentos de baixa prioridade: left,right
    L := alignarray[faLeft];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Left,
        alignrect.Top, alignrect.Left + control.Width, alignrect.Bottom));
      Inc(alignrect.Left, control.Width);
    end;
    L := alignarray[faRight];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(alignrect.Right -
        control.Width, alignrect.Top, alignrect.Right, alignrect.Bottom));
      Dec(alignrect.Right, control.Width);
    end;

    // alinhamento pela sobra de espaço: client
    auxrect := alignrect;
    L := alignarray[faClient];
    if L.Count > 0 then
    begin
      W := RectWidth(auxrect) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := RectWidth(auxrect);
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Top, auxrect.Left + W, auxrect.Bottom));
        Inc(auxrect.Left, control.Width);
      end;
    end;

    // outros alinhamentos que pegam a mesma sobra de espaço 
    leftrect := alignrect;
    rightrect := alignrect;

    // sobras ao topo
    auxrect := alignrect;
    L := alignarray[faLeftTop];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Left, auxrect.Top,
        auxrect.Left + control.Width, auxrect.Top + control.Height));
      if control.BoundsRect.Bottom > leftrect.Top then
        leftrect.Top := control.BoundsRect.Bottom;
      Inc(auxrect.Left, control.Width);
    end;
    L := alignarray[faRightTop];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Right -
        control.Width, auxrect.Top, auxrect.Right, auxrect.Top + control.Height));
      if control.BoundsRect.Bottom > rightrect.Top then
        rightrect.Top := control.BoundsRect.Bottom;
      Dec(auxrect.Right, control.Width);
    end;
    L := alignarray[faClientTop];
    if L.Count > 0 then
    begin
      H := (auxrect.Right - auxrect.Left) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Right - auxrect.Left
        else
          W := H;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Top, auxrect.Left + W, auxrect.Top + control.Height));
        Inc(auxrect.Left, control.Width);
      end;
    end;

    // sobras à base
    auxrect := alignrect;
    L := alignarray[faLeftBottom];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
        auxrect.Bottom - control.Height, auxrect.Left + control.Width, auxrect.Bottom));
      if control.BoundsRect.Top < leftrect.Bottom then
        leftrect.Bottom := control.BoundsRect.Top;
      Inc(auxrect.Left, control.Width);
    end;
    L := alignarray[faRightBottom];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Right -
        control.Width, auxrect.Bottom - control.Height, auxrect.Right, auxrect.Bottom));
      if control.BoundsRect.Top < rightrect.Bottom then
        rightrect.Bottom := control.BoundsRect.Top;
      Dec(auxrect.Right, control.Width);
    end;
    L := alignarray[faClientBottom];
    if L.Count > 0 then
    begin
      H := (auxrect.Right - auxrect.Left) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Right - auxrect.Left
        else
          W := H;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Bottom - control.Height, auxrect.Left + W, auxrect.Bottom));
        Inc(auxrect.Left, control.Width);
      end;
    end;

    // sobras à esquerda
    auxrect := leftrect;
    L := alignarray[faClientLeft];
    if L.Count > 0 then
    begin
      H := (auxrect.Bottom - auxrect.Top) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Bottom - auxrect.Top
        else
          W := H;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Top, auxrect.Left + control.Width, auxrect.Top + W));
        Inc(auxrect.Top, W);
      end;
    end;

    // sobras à direita
    auxrect := rightrect;
    L := alignarray[faClientRight];
    if L.Count > 0 then
    begin
      H := (auxrect.Bottom - auxrect.Top) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Bottom - auxrect.Top
        else
          W := H;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Right -
          control.Width, auxrect.Top, auxrect.Right, auxrect.Top + W));
        Inc(auxrect.Top, control.Height);
      end;
    end;

    // alinhamentos parciais
    auxrect := alignrect;

    // somente à esquerda
    L := alignarray[faLeftOnly];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Left, control.Top,
        auxrect.Left + control.Width, control.Top + control.Height));
    end;
    // somente à direita
    L := alignarray[faRightOnly];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Right -
        control.Width, control.Top, auxrect.Right, control.Top + control.Height));
    end;
    // somente ao topo
    L := alignarray[faTopOnly];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(control.Left, auxrect.Top,
        control.Left + control.Width, auxrect.Top + control.Height));
    end;
    // somente à base
    L := alignarray[faBottomOnly];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(control.Left,
        auxrect.Bottom - control.Height, control.Left + control.Width, auxrect.Bottom));
    end;
    // somente à altura
    L := alignarray[faHeight];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(control.Left, auxrect.Top,
        control.Left + control.Width, auxrect.Bottom));
    end;
    // somente à largura
    L := alignarray[faWidth];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      SetControlBoundsRect(control, Classes.Rect(auxrect.Left, control.Top,
        auxrect.Right, control.Top + control.Height));
    end;

    // alinhamentos aos centros

    // centro à esquerda
    auxrect := alignrect;
    L := alignarray[faCenterLeft];
    if L.Count > 0 then
    begin
      H := (auxrect.Bottom - auxrect.Top) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Bottom - auxrect.Top
        else
          W := H;
        J := (W - control.Height) div 2;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Top + J, auxrect.Left + control.Width, auxrect.Top + J + control.Height));
        Inc(auxrect.Top, W);
      end;
    end;
    // centro ao topo
    auxrect := alignrect;
    L := alignarray[faCenterTop];
    if L.Count > 0 then
    begin
      H := (auxrect.Right - auxrect.Left) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Right - auxrect.Left
        else
          W := H;
        J := (W - control.Width) div 2;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left + J,
          auxrect.Top, auxrect.Left + J + control.Width, auxrect.Top + control.Height));
        Inc(auxrect.Left, W);
      end;
    end;
    // centro à direita
    auxrect := alignrect;
    L := alignarray[faCenterRight];
    if L.Count > 0 then
    begin
      H := (auxrect.Bottom - auxrect.Top) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Bottom - auxrect.Top
        else
          W := H;
        J := (W - control.Height) div 2;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Right -
          control.Width, auxrect.Top + J, auxrect.Right, auxrect.Top + J + control.Height));
        Inc(auxrect.Top, W);
      end;
    end;
    // centro à base
    auxrect := alignrect;
    L := alignarray[faCenterBottom];
    if L.Count > 0 then
    begin
      H := (auxrect.Right - auxrect.Left) div L.Count;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        if I = L.Count - 1 then
          W := auxrect.Right - auxrect.Left
        else
          W := H;
        J := (W - control.Width) div 2;
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left + J,
          auxrect.Bottom - control.Height, auxrect.Left + J + control.Width, auxrect.Bottom));
        Inc(auxrect.Left, W);
      end;
    end;
    // centro
    auxrect := alignrect;
    L := alignarray[faCenter];
    if L.Count > 0 then
    begin
      H := 0;
      W := 0;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        Inc(H, control.Height);
        Inc(W, control.Width);
      end;
      auxrect.Top := (auxrect.Top + auxrect.Bottom - H) div 2;
      auxrect.Left := (auxrect.Left + auxrect.Right - W) div 2;
      for I := 0 to L.Count - 1 do
      begin
        control := TControl(L[I]);
        SetControlBoundsRect(control, Classes.Rect(auxrect.Left,
          auxrect.Top, auxrect.Left + control.Width, auxrect.Top + control.Height));
        Inc(auxrect.Left, control.Width);
      end;
    end;

    // ajusta controles ancorados
    W := RectWidth(OldBoundsRect);
    H := RectHeight(OldBoundsRect);
    L := alignarray[faNone];
    for I := 0 to L.Count - 1 do
    begin
      control := TControl(L[I]);
      alignrect := control.BoundsRect;
      anchors := GetControlAnchorsOf(control);
      if fkRight in anchors then
        if fkLeft in anchors then
          Inc(alignrect.Right, Width - W)
        else
          OffsetRect(alignrect, Width - W, 0)
      else if (fkLeft in anchors) and (anchors * [fkTop, fkBottom] = []) then
        OffsetRect(alignrect, 0, Round(alignrect.Top * Height / H));
      if fkBottom in anchors then
        if fkTop in anchors then
          Inc(alignrect.Bottom, Height - H)
        else
          OffsetRect(alignrect, 0, Height - H)
      else if (fkTop in anchors) and (anchors * [fkLeft, fkRight] = []) then
        OffsetRect(alignrect, Round(alignrect.Left * Width / W), 0);
      SetControlBoundsRect(control, alignrect);
    end;

  finally
    for align := Low(TRLControlAlign) to High(TRLControlAlign) do
      FreeObj(alignarray[align]);
  end;
end;

// alinha os controles do panel e dos panels parentizados
procedure TRLCustomSite.RealignControls;
var
  I: Integer;
  C: TControl;
begin
  if csLoading in ComponentState then
    Exit;
  if stAligningControls in FControlState then
    Exit;
  Include(FControlState, stAligningControls);
  try
    AlignControls(ClientRect);

    for I := 0 to ControlCount - 1 do
    begin
      C := ControlWithin(Controls[I]);
      if C is TRLCustomSite then
        TRLCustomSite(C).RealignControls;
    end;
  finally
    Exclude(FControlState, stAligningControls);
  end;
end;

procedure TRLCustomSite.DrawClient;
begin
  DrawFrame(GetClientRect, clGray, True);
end;

// desenha frames delimitadores
procedure TRLCustomSite.DrawBounds;
begin
  DrawFrame(CalcSizeRect, clBlue, False);
end;

// desenha uma frame colorida e com cantos arredondados
procedure TRLCustomSite.DrawFrame(Rect: TRect; AColor: TColor; ARound: Boolean);
var
  curv: Integer;
begin
  with Canvas do
  begin
    Pen.Color := AColor;
    Pen.Style := psDot;
    Pen.Mode := pmCopy;
    Brush.Style := bsClear;
    if ARound then
    begin
      {$ifdef CLX}
       curv := 2;
      {$else}
       curv := 6;
      {$endif}
      RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, curv, curv);
    end
    else
      Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  end;
end;

// desenha regua 
procedure TRLCustomSite.DrawTracks;
const
  clCm = $00DFDFDF;
  clHalf = $00F1F1F1;
var
  X, Y: Integer;
  cm, F: Double;
  bCm, num: Boolean;
  R: TRect;
begin
  num := True; //Self is TRLCustomReport;
  R := CalcSizeRect;
  F := ScreenPPI / (InchAsMM / 10);
  with Canvas do
  begin
    if num then
    begin
      Font.Name := 'Small Fonts';
      Font.Size := 6;
      Font.Style := [];
      Font.Color := clBlack;
    end;
    Pen.Color := clAqua;
    Pen.Mode := pmCopy;
    Brush.Style := bsClear;
    bCm := False;
    cm := 1 / 2;
    repeat
      Y := R.Top + Round(cm * F);
      if Y > R.Bottom then
        Break;
      if bCm then
      begin
        if num then
          TextOut(R.Left + 1, Y + 1, IntToStr(Round(cm)));
        Pen.Style := psSolid;
      end
      else
        Pen.Style := psDot;
      MoveTo(R.Left, Y);
      LineTo(R.Right, Y);
      cm := cm + 1 / 2;
      bCm := not bCm;
    until False;
    bCm := False;
    cm := 1 / 2;
    repeat
      X := R.Left + Round(cm * F);
      if X > R.Right then
        Break;
      if bCm then
      begin
        if num then
          TextOut(X + 1, R.Top + 1, IntToStr(Round(cm)));
        Pen.Style := psSolid;
      end
      else
        Pen.Style := psDot;
      MoveTo(X, R.Top);
      LineTo(X, R.Bottom);
      cm := cm + 1 / 2;
      bCm := not bCm;
    until False;
  end;
end;

// preenche regiao nao utilizada com barras 
procedure TRLCustomSite.DrawUnusedRect(Rect: TRect);
const
  clDarkness = $00F4F4F4;
begin
  with Canvas do
  begin
    Pen.Color := clDarkness;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Brush.Color := clDarkness;
    Brush.Style := bsSolid;
    Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  end;
end;

// zera contadores 
procedure InitializeAllFrom(AParent: TWinControl);
var
  I: Integer;
begin
  for I := 0 to AParent.ControlCount - 1 do
    if AParent.Controls[I] is TRLCustomControl then
      TRLCustomControl(AParent.Controls[I]).Initialize
    else if AParent.Controls[I] is TCustomFrame then
      InitializeAllFrom(TCustomFrame(AParent.Controls[I]));
end;

procedure TRLCustomSite.Initialize;
begin
  InitializeAllFrom(Self);
end;

// incrementa contadores 
procedure ComputeDetailAllFrom(AParent: TWinControl; ACaller: TObject);
var
  I: Integer;
begin
  for I := 0 to AParent.ControlCount - 1 do
    if AParent.Controls[I] <> ACaller then
      if IsStaticCustomControl(AParent.Controls[I]) or
        (AParent.Controls[I] is TRLCustomBand) then
        TRLCustomControl(AParent.Controls[I]).ComputeDetail(ACaller)
      else if AParent.Controls[I] is TCustomFrame then
        ComputeDetailAllFrom(TCustomFrame(AParent.Controls[I]), ACaller);
end;

procedure TRLCustomSite.ComputeDetail(ACaller: TObject);
begin
  ComputeDetailAllFrom(Self, ACaller);
end;

// forca o redesenho do panel e dos panels filhos
procedure InvalidateAllFrom(AParent: TWinControl);
var
  I: Integer;
begin
  AParent.Invalidate;
  for I := 0 to AParent.ControlCount - 1 do
    if AParent.Controls[I] is TRLCustomSite then
      TRLCustomSite(AParent.Controls[I]).InvalidateAll
    else if AParent.Controls[I] is TRLCustomControl then
      TRLCustomControl(AParent.Controls[I]).Invalidate
    else if AParent.Controls[I] is TCustomFrame then
      InvalidateAllFrom(TCustomFrame(AParent.Controls[I]));
end;

procedure TRLCustomSite.InvalidateAll;
begin
  InvalidateAllFrom(Self);
end;

// invoca evento durante a impressão 
procedure TRLCustomSite.DoOnDraw(ASurface: TRLGraphicSurface; ARect: TRect);
var
  R: TRect;
begin
  if Assigned(FOnDraw) then
  begin
    R := GetClientRect;
    OffsetRect(R, ARect.Left, ARect.Top);
    FOnDraw(Self, ASurface, R);
  end;
end;

procedure TRLCustomSite.OpenSurface;
var
  S: TRLCustomSite;
begin
  if Surface.Opened then
    Exit;
  // por precaução, abre o canvas do controle pai antes
  S := FindParentSite;
  if (S <> nil) and not S.Surface.Opened then
    S.OpenSurface;

  SurfaceOpening;
  MarkPrintPosition;
  Surface.Open;
  Surface.Clear;
  Surface.Margins := ClientRect;
  if Enabled then
    SurfaceBeginDraw;
  SurfaceOpened;
end;

procedure TRLCustomSite.CloseSurface;
begin
  if not Surface.Opened then
    Exit;

  if Enabled then
    SurfaceEndDraw;
  TruncateSurface;
  ThrowSurface;
  Surface.Close;
  SurfaceClosed;
end;

procedure TRLCustomSite.ThrowSurface;
var
  DestSurface: TRLGraphicSurface;
  DestRect: TRect;
begin
  DestSurface := RequestParentSurface;
  DestRect := CalcPrintBoundsRect;

  if Enabled then
    PrepareBackgroundSurface(DestSurface, DestRect);
  DestSurface.Draw(DestRect.Left, DestRect.Top, Surface);
end;

procedure TRLCustomSite.PrepareBackgroundSurface(ABackgroundSurface: TRLGraphicSurface;
  const ARect: TRect);
var
  M: TRect;
begin
  ABackgroundSurface.GeneratorId := PtrInt(Self);
  NewGroupId;
  if (Degrade.Direction <> ddNone) and (Degrade.OppositeColor <> Color) then
    Degrade.PaintTo(ABackgroundSurface, ARect, Color)
  else if not IsTransparent(Self) then
  begin
    ABackgroundSurface.Brush.Color := Self.Color;
    ABackgroundSurface.Brush.Style := bsSolid;
    ABackgroundSurface.FillRect(ARect);
  end;
  Background.PaintTo(ABackgroundSurface, ARect);
  M := CalcPrintMarginalRect;
  OffsetRect(M, ARect.Left, ARect.Top);
  DoOnDraw(ABackgroundSurface, M);
  Borders.PaintTo(ABackgroundSurface, M);
end;

procedure TRLCustomSite.SurfaceOpening;
begin
end;

procedure TRLCustomSite.SurfaceBeginDraw;
begin
  PrintStatics;
end;

procedure TRLCustomSite.SurfaceOpened;
begin
end;

procedure TRLCustomSite.WriteSurface;
begin
end;

procedure TRLCustomSite.SurfaceEndDraw;
begin
end;

procedure TRLCustomSite.SurfaceClosed;
begin
end;

procedure TRLCustomSite.TruncateSurface;
begin
end;

procedure TRLCustomSite.MarkPrintPosition;
var
  P: TWinControl;
begin
  FPrintPosition.X := Left;
  FPrintPosition.Y := Top;
  FPrintSize.X := Width;
  FPrintSize.Y := Height;

  P := Parent;
  while (P <> nil) and not (P is TRLCustomSite) do
  begin
    Inc(FPrintPosition.X, P.Left);
    Inc(FPrintPosition.Y, P.Top);
    P := P.Parent;
  end;
end;

procedure TRLCustomSite.DrawBackground(const ARect: TRect);
begin
  Background.PaintTo(Canvas, ARect);
end;

function TRLCustomSite.CalcEffectiveRect: TRect;
begin
  Result := CalcSizeRect;
end;

procedure TRLCustomSite.Signup(const ASignature: string; ABig: Boolean = False);
var
  W, H: Integer;
  T: TRect;
  S: string;
begin
  with Canvas do
  begin
    if ABig then
    begin
      Font.Name := 'MS Sans Serif';
      Font.Size := 8;
    end
    else
    begin
      Font.Name := 'Small Fonts';
      Font.Size := 6;
    end;
    Font.Style := [];
    Font.Color := clWhite;
    S := ' ' + ASignature + ' ';
    W := TextWidth(S);
    H := TextHeight(S);
    T.Left := 1;
    T.Top := 1;
    T.Right := T.Left + W;
    T.Bottom := T.Top + H;
    Pen.Color := clBlue;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Brush.Color := clBlue;
    Brush.Style := bsSolid;
    RoundRect(T.Left, T.Top, T.Right, T.Bottom, 5, 5);
    Brush.Style := bsClear;
    TextRect(T, T.Left, T.Top, S);
  end;
end;

// desenha o panel em tela
procedure TRLCustomSite.Paint;
var
  Z, S, E, R: TRect;
  P: TRLCustomReport;
begin
  Z := CalcSizeRect;
  S := Z;
  E := CalcEffectiveRect;
  // pinta fundo
  with Canvas do
  begin
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    FillRect(Z);
  end;
  // preenche espaços não client
  if E.Top > S.Top then
  begin
    R := S;
    R.Bottom := E.Top;
    DrawUnusedRect(R);
    S.Top := E.Top;
  end;
  if E.Bottom < S.Bottom then
  begin
    R := S;
    R.Top := E.Bottom;
    DrawUnusedRect(R);
    S.Bottom := E.Bottom;
  end;
  if E.Left > S.Left then
  begin
    R := S;
    R.Right := E.Left;
    DrawUnusedRect(R);
    S.Left := E.Left;
  end;
  if E.Right < S.Right then
  begin
    R := S;
    R.Left := E.Right;
    DrawUnusedRect(R);
    S.Right := E.Right;
  end;

  if (Degrade.Direction <> ddNone) and (Degrade.OppositeColor <> Color) then
    Degrade.PaintTo(Canvas, E, Color);
  DrawBackground(E);
  P := FindParentReport;
  if not Assigned(P) or (P.ShowDesigners and P.ShowTracks) then
    DrawTracks;
  Borders.PaintTo(Canvas, CalcMarginalRect);
  if not Assigned(P) or P.ShowDesigners then
  begin
    DrawClient;
    DrawBounds;
  end;
end;

procedure TRLCustomSite.InternalPrint;
begin
  OpenSurface;
  WriteSurface;
  CloseSurface;
end;

procedure TRLCustomSite.CalcSize(var ASize: TPoint);
var
  I, totalwidth, totalheight, maxright, maxbottom, maxwidth, maxheight: Integer;
  control: TControl;
  ctrlalign: TRLControlAlign;
  clirect: TRect;
  ctrlsize: TPoint;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  // totaliza tamanho dos controles
  totalwidth := 0;
  totalheight := 0;
  maxright := 0;
  maxbottom := 0;
  maxwidth := 0;
  maxheight := 0;
  for I := 0 to ControlCount - 1 do
  begin
    control := Controls[I];
    if not control.Visible then
      Continue;
    ctrlalign := GetControlAlignOf(control);
    ctrlsize := Point(control.Width, control.Height);
    if ctrlalign = faNone then
    begin
      maxright := Math.Max(maxright, control.Left + ctrlsize.X);
      maxbottom := Math.Max(maxbottom, control.Top + ctrlsize.Y);
    end
    else
    begin
      if ctrlalign in faFreeWidthSet then
        if ctrlalign in [faClientLeft, faClientRight, faCenterLeft, faCenterRight, faLeftOnly, faRightOnly] then
          maxwidth := Math.Max(maxwidth, ctrlsize.X)
        else
          Inc(totalwidth, ctrlsize.X);
      if ctrlalign in faFreeHeightSet then
        if ctrlalign in [faClientTop, faClientBottom, faCenterTop, faCenterBottom, faTopOnly, faBottomOnly] then
          maxheight := Math.Max(maxheight, ctrlsize.Y)
        else
          Inc(totalheight, ctrlsize.Y);
    end;
  end;

  clirect := GetClientRect;
  Dec(maxright, clirect.Left);
  Dec(maxbottom, clirect.Top);
  totalwidth := Math.Max(Math.Max(totalwidth, maxright), maxwidth);
  totalheight := Math.Max(Math.Max(totalheight, maxbottom), maxheight);

  if (Align in faSlaveWidthSet) or (totalwidth = 0) then
    totalwidth := RectWidth(clirect);
  if (Align in faSlaveHeightSet) or (totalheight = 0) then
    totalheight := RectHeight(clirect);
  // incremento das bordas, margens e etc.
  ASize.X := (Width - RectWidth(clirect)) + totalwidth;
  ASize.Y := (Height - RectHeight(clirect)) + totalheight;
end;

// margens externas em pixels
function TRLCustomSite.CalcMarginalPixels: TRect;
begin
  Result.Left := Round(ScreenPPI * FMargins.LeftMargin / InchAsMM);
  Result.Top := Round(ScreenPPI * FMargins.TopMargin / InchAsMM);
  Result.Right := Round(ScreenPPI * FMargins.RightMargin / InchAsMM);
  Result.Bottom := Round(ScreenPPI * FMargins.BottomMargin / InchAsMM);
end;

// retangulo interno as margens
function TRLCustomSite.CalcMarginalRect: TRect;
begin
  Result := ReduceRect(CalcEffectiveRect, CalcMarginalPixels);
end;

function TRLCustomSite.CalcBordersPixels: TRect;
var
  W, H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if FBorders.Width > 0 then
  begin
    W := FBorders.Width;
    H := FBorders.Width;
    if FBorders.CanDrawLeft then
      Inc(Result.Left, W);
    if FBorders.CanDrawTop then
      Inc(Result.Top, H);
    if FBorders.CanDrawRight then
      Inc(Result.Right, W);
    if FBorders.CanDrawBottom then
      Inc(Result.Bottom, H);
  end;
end;

// retangulo interno as bordas 
function TRLCustomSite.CalcBordersRect: TRect;
begin
  Result := ReduceRect(CalcMarginalRect, CalcBordersPixels);
end;

function TRLCustomSite.CalcClientPixels: TRect;
begin
  Result.Left := Round(ScreenPPI * FInsideMargins.LeftMargin / InchAsMM);
  Result.Top := Round(ScreenPPI * FInsideMargins.TopMargin / InchAsMM);
  Result.Right := Round(ScreenPPI * FInsideMargins.RightMargin / InchAsMM);
  Result.Bottom := Round(ScreenPPI * FInsideMargins.BottomMargin / InchAsMM);
end;

// retangulo livre de bordas e margens para desenho interno ou posicionamento de controls
function TRLCustomSite.GetClientRect: TRect;
begin
  Result := ReduceRect(CalcBordersRect, CalcClientPixels);
end;

function TRLCustomSite.CalcPrintBoundsRect: TRect;
begin
  Result := Rect(FPrintPosition.X, FPrintPosition.Y, FPrintPosition.X +
    FPrintSize.X, FPrintPosition.Y + FPrintSize.Y);
end;

function TRLCustomSite.CalcPrintSizeRect: TRect;
begin
  Result := CalcPrintBoundsRect;
  MoveRect(Result, 0, 0);
end;

function TRLCustomSite.CalcPrintWastedPixels: TRect;
begin
  Result := DiffRect(CalcPrintSizeRect, CalcPrintClientRect);
end;

function TRLCustomSite.CalcPrintWastedPixelsSum: TRect;
var
  P: TRLCustomPager;
  W: TRect;
begin
  Result := CalcPrintWastedPixels;
  P := FindParentPager;
  if P <> nil then
  begin
    W := P.CalcPrintWastedPixelsSum;
    Inc(Result.Left, W.Left);
    Inc(Result.Top, W.Top);
    Inc(Result.Right, W.Right);
    Inc(Result.Bottom, W.Bottom);
  end;
end;

// espacos perdidos em pixels de tela 
function TRLCustomSite.CalcWastedPixels: TRect;
begin
  Result := DiffRect(CalcSizeRect, GetClientRect);
end;

function TRLCustomSite.CanPrint: Boolean;
begin
  FCouldPrint := Visible and not (stPrinting in FControlState);
  if FCouldPrint then
    DoBeforePrint(FCouldPrint);
  Result := FCouldPrint;
end;

function TRLCustomSite.CalcPrintMarginalPixels: TRect;
begin
  Result := CalcMarginalPixels;
end;

function TRLCustomSite.CalcPrintMarginalRect: TRect;
var
  M: TRect;
begin
  Result := CalcPrintSizeRect;
  M := CalcPrintMarginalPixels;
  Inc(Result.Left, M.Left);
  Inc(Result.Top, M.Top);
  Dec(Result.Right, M.Right);
  Dec(Result.Bottom, M.Bottom);
end;

function TRLCustomSite.CalcPrintBordersPixels: TRect;
begin
  Result := CalcBordersPixels;
end;

function TRLCustomSite.CalcPrintBordersRect: TRect;
begin
  Result := ReduceRect(CalcPrintMarginalRect, CalcPrintBordersPixels);
end;

function TRLCustomSite.CalcPrintClientPixels: TRect;
begin
  Result := CalcClientPixels;
end;

function TRLCustomSite.CalcPrintClientRect: TRect;
begin
  Result := ReduceRect(CalcPrintBordersRect, CalcPrintClientPixels);
end;

function TRLCustomSite.CalcGlobalPrintPosition: TPoint;
var
  P: TRLCustomSite;
begin
  Result := FPrintPosition;
  P := FindParentSite;
  if P <> nil then
    with P.CalcGlobalPrintPosition do
    begin
      Inc(Result.X, X);
      Inc(Result.Y, Y);
    end;
end;

procedure TRLCustomSite.SetClientRect(const AValue: TRect);
begin
  BoundsRect := IncreaseRect(AValue, CalcWastedPixels);
end;

procedure TRLCustomSite.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  FPrintSize.X := Width;
  FPrintSize.Y := Height;
end;

procedure TRLCustomSite.SetBackground(const AValue: TRLBackground);
begin
  FBackground := AValue;
  FBackground.ParentSite := Self;
  Invalidate;
end;

procedure TRLCustomSite.SetDegrade(const AValue: TRLDegradeEffect);
begin
  FDegrade := AValue;
  Invalidate;
end;

procedure TRLCustomSite.SetInsideMargins(const AValue: TRLMargins);
begin
  FInsideMargins.Assign(AValue);
  Invalidate;
end;

procedure TRLCustomSite.SetMargins(const AValue: TRLMargins);
begin
  FMargins.Assign(AValue);
  Invalidate;
end;

procedure TRLCustomSite.Loaded;
begin
  inherited;

  AdjustBounds;
  AlignControls(ClientRect);
end;

procedure TRLCustomSite.InternalMeasureHeight;
var
  C: TControl;
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    C := ControlWithin(Controls[I]);
    if C is TRLCustomControl then
      TRLCustomControl(C).MeasureHeight;
  end;

  inherited;
end;

{ TRLCustomPanel }

constructor TRLCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // customization
  Width := 64;
  Height := 32;
  AutoSizeDir := [asWidthDir, asHeightDir];
end;

procedure TRLCustomPanel.DrawBounds;
var
  R: TRect;
begin
  R := CalcSizeRect;
  with Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    MoveTo(R.Left, R.Top + 5);
    LineTo(R.Left, R.Top);
    LineTo(R.Left + 5, R.Top);
    MoveTo(R.Right - 5, R.Top);
    LineTo(R.Right - 1, R.Top);
    LineTo(R.Right - 1, R.Top + 5);
    MoveTo(R.Right - 1, R.Bottom - 5);
    LineTo(R.Right - 1, R.Bottom - 1);
    LineTo(R.Right - 5, R.Bottom - 1);
    MoveTo(R.Left + 5, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 5);
  end;
end;

{ TRLCustomBandSet }

constructor TRLCustomBandSet.Create(AOwner: TComponent);
begin
  FBandSets := nil;
  FIntegralHeight := False;
  FBandSets := TList.Create;
  inherited;
end;

destructor TRLCustomBandSet.Destroy;
begin
  FreeObj(FBandSets);
  inherited;
end;

procedure TRLCustomBandSet.SurfaceOpened;
var
  P: TRLCustomBandSet;
begin
  inherited;

  FBandSets.Clear;
  P := FindParentBandSet;
  if Assigned(P) then
    P.AddBandSet(Self);
end;

procedure TRLCustomBandSet.SurfaceClosed;
begin
  inherited;

  FBandSets.Clear;
end;

procedure TRLCustomBandSet.SurfaceBeginDraw;
begin
  PrepareStatics;
  PrintStatics;
end;

function TRLCustomBandSet.FindParentBandSet: TRLCustomBandSet;
var
  W: TControl;
begin
  W := Parent;
  while (W <> nil) and not (W is TRLCustomBandSet) do
    W := W.Parent;
  Result := TRLCustomBandSet(W);
end;

function TRLCustomBandSet.CountBandSet(ABandSet: TRLCustomBandSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBandSets.Count - 1 do
    if FBandSets[I] = ABandSet then
      Inc(Result);
end;

procedure TRLCustomBandSet.AddBandSet(ABandSet: TRLCustomBandSet);
begin
  FBandSets.Add(ABandSet);
end;

function TRLCustomBandSet.BandSetCount: Integer;
var
  P: TRLCustomBandSet;
begin
  P := FindParentBandSet;
  if Assigned(P) then
    Result := P.CountBandSet(Self)
  else
    Result := 0;
end;

function TRLCustomBandSet.IsFirstBandSet: Boolean;
begin
  Result := (BandSetCount = 1);
end;

{ TRLCustomBand }

constructor TRLCustomBand.Create(AOwner: TComponent);
begin
  // initialization
  FBandType := btDetail;
  FComputable := True;
  FPageBreaking := pbNone;
  FCompletion := ctNone;
  FAlignToBottom := False;
  FCarbonCopies := 1;
  FCarbonIndex := 0;
  FGroupIndex := 0;
  FIntegralHeight := True;
  FOnCompute := nil;
  // objects
  inherited Create(AOwner);
  // customization
  FAlign := faTop;
  FAutoExpand := True;
  AutoSizeDir := [asHeightDir];

  Height := 16;
  Width := 185;
  FGreenBarFlag := False;
  FGreenBarColor := $00E5E5E5;
end;

function TRLCustomBand.HeightFits(AHeight: Integer; var AAvailable: Integer): Boolean;
var
  Pager: TRLCustomPager;
  footr: Integer;
  pgrow: Integer;
begin
  Pager := RequestParentPager;
  // excedeu a última linha para bands de dados?
  if BandType = btSummary then
    footr := Pager.GetRelativeFooterRow(False)
  else
    footr := Pager.GetRelativeColumnFooterRowNoSummary(False);
  pgrow := Pager.RelativePagerRow;
  AAvailable := footr - pgrow;
  Result := (AHeight <= AAvailable);
end;

function TRLCustomBand.MaxBandsReached: Boolean;
var
  Pager: TRLCustomPager;
begin
  Pager := Self.RequestParentPager;
  Result := (Pager.MaxBands > 0) and (Pager.DetailsInSurface + 1 > Pager.MaxBands) and
    not Self.Completing;
end;

procedure TRLCustomBand.ThrowSurface;
var
  DestSurface: TRLGraphicSurface;
  DestRect: TRect;
  SrcRect: TRect;
  FullRect: TRect;
  Report: TRLCustomReport;
  Pager: TRLCustomPager;
  VertSpace: Integer;
  TotalCut: Integer;
  CutHeight: Integer;
  CutWidth: Integer;
  FreeRow: Integer;
begin
  Report := RequestParentReport;
  Pager := RequestParentPager;
  DestSurface := RequestParentSurface;

  // checa se é preciso saltar a página antes de imprimir esta band
  if IsDataBand and Report.NewPageNeeded or
    // se o último controle impresso recomendou que o salto fosse feito na próxima band de dados
    (PageBreaking = pbBeforePrint) and (Report.DataBandPrinted > 0) or
    // se a quebra deve ser feita antes desta band e já foi impresso algum detalhe
    (BandType = btDetail) and MaxBandsReached then
    // se esta band excede o máximo previsto pelo seu pager
    Pager.InternalNewPage(Self, not Pager.IsSatisfied);

  // bands alinhadas ao rodapé da página (footers são sempre alinhados)
  if AlignToBottom or (BandType in [btFooter]) then
    case BandType of
      btFooter: Pager.GoFooterRow;
      btSummary:
        if not Pager.GoSummaryRow then
        begin
          Pager.InternalNewPage(Self, not Pager.IsSatisfied);
          Pager.GoSummaryRow;
        end;
      btColumnFooter: Pager.GoColumnFooterRow;
    end;
  MarkPrintPosition;

  TotalCut := 0;
  CutHeight := FPrintSize.Y;
  CutWidth := FPrintSize.X;
  while TotalCut < FPrintSize.Y do
  begin
    CutHeight := FPrintSize.Y - TotalCut;
    // Se relatório é de altura inifinita...
    if Report.UnlimitedHeight then
    // se a band tem obrigatoriamente que ser impressa nesta página...
    else if BandType in [btFooter, btColumnFooter] then
    // se a band (ou pedaço) couber na página...
    else if HeightFits(CutHeight, VertSpace) then
    // se não puder dividir a band ou o pedaço que couber for menor que o tamanho mínimo...
    else if IntegralHeight or (VertSpace < Constraints.MinHeight) then
      VerticalExceeded
    else if not IntegralHeight then
      if Surface.FindFreeRow(TotalCut + VertSpace, FreeRow) and
        (FreeRow >= Constraints.MinHeight) and (FreeRow > TotalCut) then
        CutHeight := FreeRow - TotalCut
      else
        VerticalExceeded
    else
      CutHeight := VertSpace;
    // tamanho da band descontando o pedaço já impresso
    SrcRect := Rect(0, TotalCut, FPrintSize.X, TotalCut + CutHeight);
    DestRect := SrcRect;
    MoveRect(DestRect, FPrintPosition.X, FPrintPosition.Y);

    if Enabled then
    begin
      DestSurface.SetClipRect(DestRect);
      try
        FullRect := Rect(FPrintPosition.X, FPrintPosition.Y - TotalCut,
          FPrintPosition.X + FPrintSize.X, FPrintPosition.Y - TotalCut + FPrintSize.Y);
        PrepareBackgroundSurface(DestSurface, FullRect);
      finally
        DestSurface.ResetClipRect;
      end;
    end;
    DestSurface.CopyRect(DestRect, Surface, SrcRect);

    Inc(TotalCut, RectHeight(SrcRect));
    if TotalCut < FPrintSize.Y then
      VerticalExceeded;
  end;

  SkipToNextPosition(CutWidth, CutHeight);
end;

procedure TRLCustomBand.VerticalExceeded;
begin
  // move para a próxima página
  RequestParentPager.InternalNewPage(Self, False);
  MarkPrintPosition;
end;

procedure TRLCustomBand.SkipToNextPosition(AWidth, AHeight: Integer);
var
  Report: TRLCustomReport;
  Pager: TRLCustomPager;
  PrintedMM: Double;
begin
  Pager := RequestParentPager;
  Pager.RelativePagerRow := Pager.RelativePagerRow + AHeight;

  Report := RequestParentReport;
  if Assigned(Report) and Report.UnlimitedHeight then
  begin
    PrintedMM :=  (Pager.RelativePagerRow * InchAsMM) / ScreenPPI;
    if (PrintedMM > Report.PageSetup.PaperHeight) then
      Report.PageSetup.PaperHeight := PrintedMM;
  end;
end;

procedure TRLCustomBand.CheckPageBreak;
var
  VertSpace: Integer;
  R: TRLCustomReport;
begin
  R := FindParentReport;
  // Se relatório é de altura inifinita...
  if Assigned(R) and R.UnlimitedHeight then
  // se a band tem obrigatoriamente que ser impressa nesta página...
  else if BandType in [btFooter, btColumnFooter] then
  // se a band couber na página...
  else if HeightFits(FPrintSize.Y, VertSpace) then
  // se não puder dividir a band ou o pedaço que couber for menor que o tamanho mínimo...
  else if IntegralHeight or (VertSpace < Constraints.MinHeight) then
    VerticalExceeded;
end;

procedure TRLCustomBand.SurfaceClosed;
begin
  inherited;

  if (BandType = btDetail) and CanCompute then
    with RequestParentPager do
      DetailsInSurface := DetailsInSurface + 1;
  if PageBreaking = pbAfterPrint then
    RequestParentReport.InvalidatePage;
end;

function TRLCustomBand.GetBandTypeName: string;
begin
  Result := BandTypeNames[FBandType];
end;

procedure TRLCustomBand.Paint;
var
  R: TRLCustomReport;
begin
  inherited;

  R := FindParentReport;
  if not Assigned(R) or R.ShowDesigners then
    Signup(GetBandTypeName + ' ' + Name);
end;

procedure TRLCustomBand.NotifyDataBandPrinted;
var
  P: TRLCustomPager;
begin
  P := FindParentPager;
  while P <> nil do
  begin
    Inc(P.FDataBandPrinted);
    P := P.FindParentPager;
  end;
end;

function TRLCustomBand.IsDataBand: Boolean;
begin
  Result := (BandType in [btDetail, btSummary]) and not IsBallast;
end;

function TRLCustomBand.CanCompute: Boolean;
begin
  Result := FComputable;
  if Assigned(FOnCompute) then
    FOnCompute(Self, Result);
end;

procedure TRLCustomBand.InternalPrint;
var
  DoCompute: Boolean;
  IsDataLike: Boolean;
  BackupColor: TColor;
  RestoreColor: Boolean;
begin
  RestoreColor := False;
  BackupColor := clNone;
  try
    if GreenBarPrint then
    begin
      if FGreenBarFlag then
      begin
        BackupColor := Self.Color;
        RestoreColor := True;
        Self.Color:= GreenBarColor;
      end;
      FGreenBarFlag := not FGreenBarFlag;
    end;
    // se for detail computável deve computar o registro
    DoCompute := (BandType = btDetail) and CanCompute and not IsBallast;
    // se for band de dados, deve setar o flag de dados impressos
    IsDataLike := DoCompute or (BandType = btSummary);
    // computa o registro nos controles da própria band
    if DoCompute then
      Self.ComputeDetail(Self);
    inherited;
    // computa o registro para o Pager
    if DoCompute then
      RequestParentPager.ComputeDetail(Self);
    // seta flag de dados impressos
    if IsDataLike then
      NotifyDataBandPrinted;
  finally
    if RestoreColor then
      Self.Color:=BackupColor;
  end;
end;

procedure TRLCustomBand.MarkPrintPosition;
begin
  FPrintPosition.X := Left;
  FPrintPosition.Y := RequestParentPager.RelativePagerRow;
  FPrintSize.X := Width;
  FPrintSize.Y := Height;
end;

procedure TRLCustomBand.SetBandType(const AValue: TRLBandType);
begin
  if AValue = FBandType then
    Exit;
  FBandType := AValue;

  Realign;
  Invalidate;
end;

procedure TRLCustomBand.AdjustCarbonGroup;
var
  P: TRLCustomSite;
  B: TControl;
  I: Integer;
begin
  if FGroupIndex > 0 then
  begin
    P := FindParentSite;
    if P = nil then
      Exit;
    for I := 0 to P.ControlCount - 1 do
    begin
      B := P.Controls[I];
      if (B is TRLCustomBand) and not (B = Self) and
        (TRLCustomBand(B).GroupIndex = FGroupIndex) then
        TRLCustomBand(B).FCarbonCopies := FCarbonCopies;
    end;
  end;
end;

procedure TRLCustomBand.AdjustFromCarbonGroup;
var
  P: TRLCustomSite;
  B: TControl;
  I: Integer;
begin
  if FGroupIndex > 0 then
  begin
    P := FindParentSite;
    if P = nil then
      Exit;
    for I := 0 to P.ControlCount - 1 do
    begin
      B := P.Controls[I];
      if (B is TRLCustomBand) and not (B = Self) and
        (TRLCustomBand(B).GroupIndex = FGroupIndex) then
      begin
        FCarbonCopies := TRLCustomBand(B).CarbonCopies;
        Break;
      end;
    end;
  end;
end;

procedure TRLCustomBand.SetCarbonCopies(const AValue: Integer);
begin
  if AValue = FCarbonCopies then
    Exit;
  if AValue < 1 then
    FCarbonCopies := 1
  else
    FCarbonCopies := AValue;
  AdjustCarbonGroup;
end;

procedure TRLCustomBand.SetGroupIndex(const AValue: Integer);
begin
  if AValue = FGroupIndex then
    Exit;
  if AValue < 0 then
    FGroupIndex := 0
  else
    FGroupIndex := AValue;
  AdjustFromCarbonGroup;
end;

function TRLCustomBand.GetCompleting: Boolean;
var
  Pager: TRLCustomPager;
begin
  Pager := FindParentPager;
  Result := (Pager <> nil) and (psCompleting in Pager.PagerStatus);
end;

{ TRLCustomDetailGrid }

constructor TRLCustomDetailGrid.Create(AOwner: TComponent);
begin
  FBandType := btDetail;
  FColIndex := 0;
  FColCount := 1;
  FColSpacing := 0;
  FColWidth := 0;
  FRowIndex := 0;
  FOrganization := goInRows;

  inherited Create(AOwner);
end;

procedure TRLCustomDetailGrid.Initialize;
begin
  inherited;

  FColIndex := 0;
  FRowIndex := 0;
end;

function TRLCustomDetailGrid.GetClientCellRect(AColIndex, ARowIndex: Integer): TRect;
var
  W, ws, H: Integer;
  R: TRect;
begin
  R := CalcSizeRect;

  ws := Round(FColSpacing * ScreenPPI / InchAsMM);
  if FColCount > 0 then
    if FColWidth > 0 then
      W := Round(FColWidth * ScreenPPI / InchAsMM)
    else
      W := Round((RectWidth(R) - (FColCount - 1) * ws) / FColCount)
  else
    W := R.Right - R.Left;

  H := R.Bottom - R.Top;

  Result.Left := AColIndex * (W + ws);
  Result.Top := ARowIndex * H;
  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;
end;

procedure TRLCustomDetailGrid.DrawClient;

  procedure DrawBall(ARect: TRect; const ACaption: string);
  var
    tw, th, D: Integer;
    T: TRect;
  begin
    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Color := clBlue;
      Pen.Mode := pmCopy;
      Brush.Style := bsClear;
      with Font do
      begin
        Name := 'Small Fonts';
        Size := 6;
        Style := [];
        Color := clBlue;
      end;
      tw := TextWidth(' ' + ACaption + ' ');
      th := TextHeight(ACaption);
      if tw > th then
        D := tw
      else
        D := th;
      // text rect
      T := ARect;
      T.Right := T.Left + D + 2;
      T.Top := T.Bottom - D - 2;
      TextOut((T.Left + T.Right - tw) div 2, (T.Top + T.Bottom - th) div 2, ACaption);
      Ellipse(T.Left, T.Top, T.Right, T.Bottom);
    end;
  end;

var
  X: Integer;
  R: TRect;
begin
  inherited;

  X := 0;
  repeat
    R := GetClientCellRect(X, 0);
    DrawFrame(R, clGray, True);
    DrawBall(R, IntToStr(X + 1));
    Inc(X);
  until not (X < FColCount);
end;

function TRLCustomDetailGrid.CalcEffectiveRect: TRect;
begin
  Result := CalcSizeRect;
  with GetClientCellRect(0, 0) do
  begin
    Result.Right := Result.Left + (Right - Left);
    Result.Bottom := Result.Top + (Bottom - Top);
  end;
end;

procedure TRLCustomDetailGrid.VerticalExceeded;
begin
  // se a organização é em colunas, passa para a próxima coluna
  if FOrganization = goInColumns then
  begin
    Inc(FColIndex);
    if FColIndex > FColCount - 1 then
    begin
      with RequestParentPager do
      begin
        InternalNewPage(Self, False);
        FTopRow := RelativePagerRow;
        FBottomRow := FTopRow;
      end;
      FColIndex := 0;
    end;
  end
  else
    with RequestParentPager do
    begin
      InternalNewPage(Self, False);
      FTopRow := RelativePagerRow;
      FBottomRow := FTopRow;
    end;
  FRowIndex := 0;

  MarkPrintPosition;
end;

procedure TRLCustomDetailGrid.SkipToNextPosition(AWidth, AHeight: Integer);
begin
  case FOrganization of
    goInRows:
    begin
      Inc(FColIndex);
      if FColIndex > FColCount - 1 then
      begin
        Inc(FRowIndex);
        FColIndex := 0;
      end;
    end;
    goInColumns: Inc(FRowIndex);
  end;
end;

procedure TRLCustomDetailGrid.SurfaceOpening;
begin
  if (FColIndex = 0) and (FRowIndex = 0) then
  begin
    FTopRow := RequestParentPager.RelativePagerRow;
    FBottomRow := FTopRow;
  end;
end;

procedure TRLCustomDetailGrid.SurfaceClosed;
begin
  inherited;

  RequestParentPager.RelativePagerRow := FBottomRow;
end;

procedure TRLCustomDetailGrid.MarkPrintPosition;
var
  cellrect: TRect;
  avail: Integer;
begin
  cellrect := GetClientCellRect(FColIndex, FRowIndex);
  FPrintPosition.X := Left + cellrect.Left;
  FPrintPosition.Y := FTopRow + cellrect.Top;
  FPrintSize.X := RectWidth(cellrect);
  FPrintSize.Y := RectHeight(cellrect);

  if HeightFits(FPrintSize.Y, avail) then
    FBottomRow := Math.Max(FBottomRow, FTopRow + cellrect.Bottom)
  else if not IntegralHeight then
    FBottomRow := Math.Max(FBottomRow, FTopRow + cellrect.Top + avail);
end;

function TRLCustomDetailGrid.HeightFits(AHeight: Integer;
  var AAvailable: Integer): Boolean;
var
  pagerrow: Integer;
  Pager: TRLCustomPager;
begin
  Pager := RequestParentPager;
  // excedeu a última linha para bands de dados?
  pagerrow := FTopRow + GetClientCellRect(FColIndex, FRowIndex).Top;
  AAvailable := Pager.GetRelativeColumnFooterRowNoSummary(False) - pagerrow;
  Result := (AHeight <= AAvailable);
end;

function TRLCustomDetailGrid.GetBandTypeName: string;
begin
  Result := 'DetailGrid';
end;

procedure TRLCustomDetailGrid.SetColCount(const AValue: Integer);
begin
  if AValue = FColCount then
    Exit;
  if AValue < 1 then
    FColCount := 1
  else
    FColCount := AValue;
  RealignControls;
  Invalidate;
end;

procedure TRLCustomDetailGrid.SetColSpacing(const AValue: Double);
begin
  if AValue = FColSpacing then
    Exit;
  FColSpacing := AValue;
  RealignControls;
  Invalidate;
end;

procedure TRLCustomDetailGrid.SetColWidth(const AValue: Double);
begin
  if AValue = FColWidth then
    Exit;
  FColWidth := AValue;
  RealignControls;
  Invalidate;
end;

function TRLCustomDetailGrid.IsManyCols: Boolean;
begin
  Result := (FColCount > 1);
end;

{ TRLCustomPager }

constructor TRLCustomPager.Create(AOwner: TComponent);
begin
  // initialization
  FAllowedBands := [];
  FDetailCount := 0;
  FMaxBands := 0;
  FMinBands := 0;
  FRelativePagerRow := 0;
  FDetailsInSurface := 0;
  FNewPageNeeded := False;
  FPageBreaking := pbNone;
  FJumpPending := False;
  FJumpLength := 0;
  FNewPageCaller := nil;
  FForceMinBands := False;
  FFooterMeasuring := fmNone;
  FDataBandPrinted := 0;
  FPagerStatus := [];

  // objects
  FSortedBands := TRLSortedBands.Create;

  inherited Create(AOwner);
  // customization
  FAlign := faTop;
  AutoSizeDir := [asHeightDir];
end;

destructor TRLCustomPager.Destroy;
begin
  FreeObj(FSortedBands);

  inherited;
end;

function TRLCustomPager.CreateChild(AType: TRLBandType): TRLCustomBand;
begin
  Result := FindChild(AType);
  if Result <> nil then
    Exit;
  Result := TRLBand.Create(Owner);
  with Result do
  begin
    Parent := Self;
    if Self is TRLCustomReport then
      Align := faTop
    else
      case AType of
        btHeader,
        btTitle,
        btColumnHeader: Align := faTop;
        btDetail: Align := faClient;
        btSummary,
        btColumnFooter,
        btFooter: Align := faBottom;
      end;
    BandType := AType;
    Height := 20;
    Name := NewComponentName(Result);
  end;
end;

function TRLCustomPager.FindChild(AType: TRLBandType): TRLCustomBand;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if (Controls[I] is TRLCustomBand) and
      (TRLCustomBand(Controls[I]).BandType = AType) then
    begin
      Result := TRLCustomBand(Controls[I]);
      Break;
    end;
end;

procedure TRLCustomPager.KillChild(AType: TRLBandType);
var
  B: TRLCustomBand;
begin
  B := FindChild(AType);
  FreeObj(B);
end;

procedure TRLCustomPager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent is TRLCustomBand) and (TRLCustomBand(AComponent).Parent = Self) then
    case Operation of
      opInsert: Include(FAllowedBands, TRLCustomBand(AComponent).BandType);
      opRemove: Exclude(FAllowedBands, TRLCustomBand(AComponent).BandType);
    end;
end;

procedure TRLCustomPager.InternalNewPage(ACaller: TObject; AMoveOnly: Boolean = False);
var
  SavedCaller: TObject;
  ParentPager: TRLCustomPager;
  ParentReport: TRLCustomReport;
begin
  SavedCaller := FNewPageCaller;
  FNewPageCaller := ACaller;
  try
    ParentReport := FindParentReport;
    if Assigned(ParentReport) and ParentReport.UnlimitedHeight then
      Exit;

    ParentPager := FindParentPager;
    // moveonly=True significa que o Pager não vai se dividir entre a página atual e a próxima
    if IntegralHeight and (ParentPager <> nil) and
      (ParentPager.DataBandPrinted > Self.DataBandPrinted) then
      AMoveOnly := True;
    if not AMoveOnly then
      CloseSurface;
    if ParentPager <> nil then
      ParentPager.InternalNewPage(ACaller);
    if AMoveOnly then
      MarkPrintPosition
    else
      OpenSurface;
  finally
    FNewPageCaller := SavedCaller;
  end;
end;

procedure TRLCustomPager.SurfaceOpening;
begin
  inherited;

  PushBoundsRect;
  InitializePageInfo;
  SortedBands.ResetPage;
end;

procedure TRLCustomPager.SurfaceBeginDraw;
begin
  inherited;

  PrintHeaders;
end;

procedure TRLCustomPager.SurfaceEndDraw;
begin
  PrintFooters;

  inherited;
end;

procedure TRLCustomPager.TruncateSurface;
begin
  Inc(FRelativePagerRow, CalcPrintWastedPixels.Bottom);
  if AutoTrunc then
    FPrintSize.Y := FRelativePagerRow;
end;

procedure TRLCustomPager.SurfaceClosed;
var
  P: TRLCustomPager;
begin
  inherited;

  P := FindParentPager;
  if P <> nil then
    P.RelativePagerRow := P.RelativePagerRow + RectHeight(CalcPrintBoundsRect);

  PopBoundsRect;
end;

function TRLCustomPager.PrintBands(AType: TRLBandType): TRLPrintBandResults;
var
  I, icc, qcc, savei, savedgroup: Integer;
  E: TRLCustomSite;
  L: TList;
begin
  L := SortedBands.List[AType];
  if L.Count > 0 then
  begin
    I := 0;
    while I < L.Count do
    begin
      if L.Items[I] = FNewPageCaller then
      begin
        Result := brStackExit;
        Exit;
      end;
      E := TRLCustomSite(L.Items[I]);
      if E is TRLCustomBand then
      begin
        savedgroup := TRLCustomBand(L.Items[I]).GroupIndex;
        savei := I;
        qcc := TRLCustomBand(L.Items[I]).CarbonCopies;
        icc := 0;
        while icc < qcc do
        begin
          I := savei;
          while (I < L.Count) and (TRLCustomBand(L.Items[I]).GroupIndex = savedgroup) do
          begin
            TRLCustomBand(L.Items[I]).CarbonIndex := icc;
            PrintBand(TRLCustomBand(L.Items[I]));
            Inc(I);
            if savedgroup = 0 then
              Break;
          end;
          Inc(icc);
        end;
      end
      else if E is TRLCustomSubDetail then
      begin
        PrintSite(E);
        Inc(I);
      end;
    end;
    SortedBands.Printed[AType] := True;
    Result := brPrinted;
  end
  else
    Result := brNoBands;
end;

procedure TRLCustomPager.PrintBand(ABand: TRLCustomBand);
begin
  with ABand do
  begin
    // save all bounds
    PushBoundsRect;
    PushBoundsAllFrom(ABand);

    if CanPrint then
    begin
      if not (boOptimisticPageBreak in Options) then
        CheckPageBreak;
      AdjustBounds;
      Print;
      DoAfterPrint;
    end;
    // restore all bounds
    PopBoundsAllFrom(ABand);
    PopBoundsRect;
  end;
end;

procedure TRLCustomPager.PrintDetails;
begin
  if FFooterMeasuring = fmBeforeDetail then
    MeasureFooters;
  PrintPagers(TRLCustomGroup);
  if PrintBands(btDetail) = brStackExit then
    Exit;
end;

procedure TRLCustomPager.PrintSite(ASite: TRLCustomSite);
begin
  with ASite do
    if CanPrint then
    begin
      AdjustBounds;
      Print;
      DoAfterPrint;
    end;
end;

procedure TRLCustomPager.PrintPagers(AClass: TRLPagerClassType);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is AClass then
      PrintSite(TRLCustomSite(Controls[I]));
end;

procedure TRLCustomPager.PrintHeaders;
begin
  if not Enabled then
    Exit;
  if not SortedBands.Printed[btHeader] then
    if PrintBands(btHeader) = brStackExit then
      Exit;
  if not SortedBands.Printed[btTitle] then
    if PrintBands(btTitle) = brStackExit then
      Exit;
  if not SortedBands.Printed[btColumnHeader] then
    if PrintBands(btColumnHeader) = brStackExit then
      Exit;
  if FFooterMeasuring = fmAfterHeader then
    MeasureFooters;
end;

procedure TRLCustomPager.PrintFooters(ASummarize: Boolean = False);
begin
  if not Enabled then
    Exit;
  if not SortedBands.Printed[btColumnFooter] then
    if PrintBands(btColumnFooter) = brStackExit then
      Exit;
  if ASummarize then
    if not SortedBands.Printed[btSummary] then
      if PrintBands(btSummary) = brStackExit then
        Exit;
  if not SortedBands.Printed[btFooter] then
    if PrintBands(btFooter) = brStackExit then
      Exit;
end;

procedure TRLCustomPager.PrintCompletion;
var
  I, iHeight, iLast: Integer;
  L: TList;
  B: TRLCustomBand;
begin
  Include(FPagerStatus, psCompleting);
  try
    L := SortedBands.List[btDetail];
    if L.Count = 0 then
      Exit;
    // encontra a band que será utilizada para completar a página  
    B := nil;
    for I := 0 to L.Count - 1 do
      if TObject(L.Items[I]) is TRLCustomBand then
        with TRLCustomBand(L.Items[I]) do
          if (Completion <> ctNone) and CanCompute then
          begin
            B := TRLCustomBand(L.Items[I]);
            Break;
          end;
    if B = nil then
      Exit;
    case B.Completion of
      ctMinBands: while FDetailsInSurface < FMinBands do
          PrintBand(B);
      ctMaxBands: while FDetailsInSurface < FMaxBands do
          PrintBand(B);
      ctFullPage:
      begin
        iHeight := RectHeight(B.CalcPrintBoundsRect);
        iLast := GetRelativeColumnFooterRowNoSummary(True);
        while not (FRelativePagerRow + iHeight >= iLast) do
          if (FMaxBands > 0) and not (FDetailsInSurface < FMaxBands) then
            Break
          else
            PrintBand(B);
      end;
    end;
  finally
    Exclude(FPagerStatus, psCompleting);
  end;
end;

procedure TRLCustomPager.SortBands;
var
  I: Integer;
  C: TControl;
begin
  SortedBands.Clear;
  for I := 0 to ControlCount - 1 do
  begin
    C := ControlWithin(Controls[I]);
    if C is TRLCustomBand then
      SortedBands.Add(TRLCustomBand(C))
    else
    begin
      if C is TRLCustomSubDetail then
        SortedBands.Add(TRLCustomSubDetail(C));
      if C is TRLCustomPager then
        TRLCustomPager(C).SortBands;
    end;
  end;
end;

function TRLCustomPager.GoFooterRow: Boolean;
var
  R: Integer;
begin
  if not AutoTrunc then
  begin
    R := GetRelativeFooterRow(True);
    Result := (FRelativePagerRow < R);
    if Result then
      FRelativePagerRow := R;
  end
  else
    Result := False;
end;

function TRLCustomPager.GoSummaryRow: Boolean;
var
  R: Integer;
begin
  R := GetRelativeSummaryRow(True);
  Result := FRelativePagerRow < R;
  if Result then
    FRelativePagerRow := R;
end;

function TRLCustomPager.GoColumnFooterRow: Boolean;
var
  R: Integer;
begin
  R := GetRelativeColumnFooterRowNoSummary(True);
  Result := FRelativePagerRow < R;
  if Result then
    FRelativePagerRow := R;
end;

procedure TRLCustomPager.MeasureFooters;
var
  L: TList;
  I: Integer;
begin
  L := SortedBands.List[btFooter];
  for I := 0 to L.Count - 1 do
    if TObject(L.Items[I]) is TRLCustomBand then
      TRLCustomBand(L.Items[I]).MeasureHeight;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    TRLCustomPager(Parent).MeasureFooters;
end;

function TRLCustomPager.GetFooterHeight: Integer;
var
  L: TList;
  I: Integer;
begin
  Result := 0;
  L := SortedBands.List[btFooter];
  for I := 0 to L.Count - 1 do
    if TObject(L.Items[I]) is TRLCustomBand then
      with TRLCustomBand(L.Items[I]) do
        if Visible then
          Inc(Result, Height);
end;

function TRLCustomPager.GetFooterHeightSum: Integer;
begin
  Result := GetFooterHeight;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    Inc(Result, TRLCustomPager(Parent).GetFooterHeightSum);
end;

function TRLCustomPager.GetAlignedSummaryHeight: Integer;
var
  I: Integer;
  L: TList;
begin
  Result := 0;
  L := SortedBands.List[btSummary];
  for I := 0 to L.Count - 1 do
    if TObject(L.Items[I]) is TRLCustomBand then
      with TRLCustomBand(L.Items[I]) do
        if AlignToBottom and Visible then
          Inc(Result, Height);
end;

function TRLCustomPager.GetColumnFooterHeight: Integer;
var
  I: Integer;
  L: TList;
begin
  Result := 0;
  L := SortedBands.List[btColumnFooter];
  for I := 0 to L.Count - 1 do
    if TObject(L.Items[I]) is TRLCustomBand then
      with TRLCustomBand(L.Items[I]) do
        if Visible then
          Inc(Result, Height);
end;

function TRLCustomPager.GetColumnFooterHeightSum: Integer;
begin
  Result := GetColumnFooterHeight;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    Inc(Result, TRLCustomPager(Parent).GetColumnFooterHeightSum);
end;

function TRLCustomPager.GetAlignedSummaryHeightSum: Integer;
begin
  Result := GetAlignedSummaryHeight;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    Inc(Result, TRLCustomPager(Parent).GetAlignedSummaryHeightSum);
end;

function TRLCustomPager.GetSummaryHeightSum: Integer;
begin
  Result := GetSummaryHeight;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    Inc(Result, TRLCustomPager(Parent).GetSummaryHeightSum);
end;

function TRLCustomPager.GetSummaryHeight: Integer;
var
  I: Integer;
  L: TList;
begin
  Result := 0;
  L := SortedBands.List[btSummary];
  for I := 0 to L.Count - 1 do
    if TObject(L.Items[I]) is TRLCustomBand then
      with TRLCustomBand(L.Items[I]) do
        if AlignToBottom and Visible then
          Inc(Result, Height);
end;

function TRLCustomPager.GetWastedBottomSum: Integer;
begin
  Result := CalcWastedPixels.Bottom;
  if (Parent <> nil) and (Parent is TRLCustomPager) then
    Inc(Result, TRLCustomPager(Parent).GetWastedBottomSum);
end;

function TRLCustomPager.GetRelativeFooterRow(AConsiderAligned: Boolean): Integer;
var
  Report: TRLCustomReport;
  printr: TRect;
  globxy: TPoint;
  wasted: Integer;
  footer: Integer;
  ppager: TRLCustomPager;
begin
  Report := RequestParentReport;
  printr := Report.CalcPrintBoundsRect;
  globxy := CalcGlobalPrintPosition;
  wasted := GetWastedBottomSum;

  // a linha de impressao do footer é calculada pelo tamanho da pagina menos a
  // altura dos footers e columnfooters abaixo segundo a hierarquia de pagers
  // ainda subtrai, é claro, a altura dos footers deste pager
  footer := GetFooterHeightSum;
  ppager := FindParentPager;
  if ppager <> nil then
  begin
    Inc(footer, ppager.GetColumnFooterHeightSum);
    if AConsiderAligned and NewAlignedSummaryBehavior then
      Inc(footer, ppager.GetAlignedSummaryHeightSum);
  end;
  Result := printr.Bottom - globxy.Y - wasted - footer;
end;

function TRLCustomPager.GetRelativeSummaryRow(AConsiderAligned: Boolean): Integer;
begin
  // a linha do primeiro sumario deste pager é a linha do primeiro rodape deste pager (footer)
  // menos a altura dos sumarios deste pager
  Result := GetRelativeFooterRow(AConsiderAligned) - GetSummaryHeight;
end;

{function TRLCustomPager.GetRelativeColumnFooterRow: Integer;
begin
  // a linha do primeiro columnfooter deste pager é a linha do primeiro sumario
  // deste pager menos a altura dos columnfooters deste pager
  Result := GetRelativeSummaryRow - GetColumnFooterHeight;
end;}///

function TRLCustomPager.GetRelativeColumnFooterRowNoSummary(AConsiderAligned: Boolean): Integer;
begin
  // neste caso, nao levo em consideracao os sumarios, pois o cara que me pergunta
  // nao pretende imprimi-los.
  Result := GetRelativeFooterRow(AConsiderAligned) - GetColumnFooterHeight;
end;

procedure TRLCustomPager.MarkPrintPosition;
var
  P: TRLCustomPager;
begin
  P := FindParentPager;
  if Assigned(P) then
  begin
    FPrintPosition.X := Left;
    FPrintPosition.Y := P.RelativePagerRow;
    FPrintSize.X := Width;
    if AutoTrunc then
      FPrintSize.Y := (RequestParentReport.CalcPrintBoundsRect.Bottom -
        CalcGlobalPrintPosition.Y) - P.CalcPrintWastedPixelsSum.Bottom - P.GetFooterHeightSum
    else
      FPrintSize.Y := Height;
  end
  else
  begin
    FPrintPosition.X := 0;
    FPrintPosition.Y := 0;
    FPrintSize.X := Width;
    FPrintSize.Y := Height;
  end;
end;

procedure TRLCustomPager.SetAllowedBands(const AValue: TRLAllowedBands);
var
  I: TRLBandType;
begin
  if AValue = FAllowedBands then
    Exit;
  FAllowedBands := AValue;
  if csLoading in ComponentState then
    Exit;

  if csDesigning in ComponentState then
    for I := Low(TRLBandType) to High(TRLBandType) do
      if (I in AValue) xor (I in FAllowedBands) then
        if I in AValue then
          CreateChild(I)
        else
          KillChild(I);
end;

procedure TRLCustomPager.Initialize;
begin
  inherited;
  FDetailCount := 0;
  InitializePageInfo;
end;

procedure TRLCustomPager.InitializePageInfo;
begin
  FDataBandPrinted := 0;
  FDetailsInSurface := 0;
  FNewPageNeeded := False;
  FJumpPending := False;
  FJumpLength := 0;
  FRelativePagerRow := CalcPrintWastedPixels.Top;
end;

procedure TRLCustomPager.ComputeDetail(ACaller: TObject);
begin
  inherited;
  Inc(FDetailCount);
end;

procedure TRLCustomPager.InternalBeginDoc;
begin
  SortedBands.ResetAll;
  Initialize;
end;

procedure TRLCustomPager.InvalidatePage;
begin
  FNewPageNeeded := True;
end;

function TRLCustomPager.GetNewPageNeeded: Boolean;
begin
  Result := FNewPageNeeded;
  FNewPageNeeded := False;
end;

procedure TRLCustomPager.InternalEndDoc;
begin
  PrintCompletion;
  PrintFooters(True);
  CloseSurface;
end;

procedure TRLCustomPager.BeginDoc;
begin
  InternalBeginDoc;
  MarkPrintPosition;
end;

procedure TRLCustomPager.EndDoc;
begin
  InternalEndDoc;
end;

procedure TRLCustomPager.NewPage;
begin
  InternalNewPage(nil);
end;

function TRLCustomPager.IsSatisfied: Boolean;
begin
  Result := True;
  if ForceMinBands and (MinBands > 0) and (DetailsInSurface < MinBands) then
    Result := False;
  if DataBandPrinted = 0 then
    Result := False;
end;

{ TRLCustomGroup }

constructor TRLCustomGroup.Create(AOwner: TComponent);
begin
  // initialization
  FOnGetBreak := nil;
  FBroken := False;
  FDataFields := '';
  FDataFormula := '';

  // objects
  inherited Create(AOwner);
  // customization
  FAutoTrunc := True;

  Height := 64;
end;

procedure TRLCustomGroup.Paint;
var
  R: TRLCustomReport;
begin
  inherited;

  R := FindParentReport;
  if not Assigned(R) or R.ShowDesigners then
    Signup('Group ' + Name);
end;

procedure TRLCustomGroup.InternalPrint;
var
  B: Boolean;
  R: TRLCustomReport;
  P: TRLCustomPager;
  S: TRLCustomSkipper;
begin
  R := RequestParentReport;
  P := RequestParentPager;
  S := RequestParentSkipper;

  if (PageBreaking = pbBeforePrint) and (R.DataBandPrinted > 0) then
  begin
    if Assigned(P) then
      P.InternalNewPage(Self, not P.IsSatisfied);
    MarkPrintPosition;
  end;

  BeginDoc;

  B := True; // flag de primeira quebra
  FLastKey := GetKey;
  while not S.DataEof and not R.Canceled do
  begin
    FBroken := False;
    if B then
      B := False
    else if CheckBreak then
      Break;
    S.RecordMoved := False;
    PrintDetails;
    if FBroken then
      Break;
    if not S.RecordMoved then
      S.DataNext;
  end;

  EndDoc;

  if PageBreaking = pbAfterPrint then
    R.InvalidatePage;
end;

function TRLCustomGroup.GetKey: string;
var
  I: Integer;
  N, K: string;
  F: tfield;
  S: TRLCustomSkipper;
begin
  S := RequestParentSkipper;
  if Assigned(S.DataSource) and S.DataSource.DataSet.Active then
    if FDataFormula <> '' then
      Result := FindParentReport.Parse(Self, FDataFormula)
    else if FDataFields <> '' then
    begin
      Result := '';
      N := FDataFields;
      repeat
        I := Pos(';', N);
        if I = 0 then
          I := Length(N) + 1;
        K := Copy(N, 1, I - 1);
        if K <> '' then
        begin
          F := S.DataSource.DataSet.FindField(K);
          if F = nil then
            raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr +
              ': ' + Name + '.DataField "' + K + '"'));
          Result := Result + F.AsString;
        end;
        Delete(N, 1, I);
      until N = '';
    end
    else
      Result := ''
  else
    Result := '';
end;

function TRLCustomGroup.CheckBreak: Boolean;
var
  key: string;
  grp: TRLCustomGroup;
begin
  Result := False;
  if Enabled then
    if Assigned(FOnGetBreak) then
      FOnGetBreak(Self, Result)
    else
    begin
      key := GetKey;
      Result := key <> FLastKey;
      FLastKey := key;
    end
  else
  begin
    grp := FindParentGroup;
    if grp <> nil then
      Result := grp.CheckBreak;
  end;
  FBroken := Result;
end;

procedure TRLCustomGroup.ComputeDetail(ACaller: TObject);
var
  P: TRLCustomPager;
begin
  inherited;
  P := FindParentPager;
  if P <> nil then
    P.ComputeDetail(ACaller);
end;

procedure TRLCustomGroup.SetDataFields(const Value: TRLDataFieldsProperty);
begin
  FDataFields := Value;
  if FDataFields <> '' then
    FDataFormula := '';
end;

procedure TRLCustomGroup.SetDataFormula(const Value: string);
begin
  FDataFormula := Value;
  if FDataFormula <> '' then
    FDataFields := '';
end;

{ TRLCustomSkipper }

constructor TRLCustomSkipper.Create(AOwner: TComponent);
begin
  // initialization
  FRecordAction := raUseIt;
  FDataSource := nil;
  FOnNeedData := nil;
  FOnDataCount := nil;
  FOnDataRecord := nil;
  FRecordMoved := False;
  FDataEof := True;
  FRecNo := 0;
  FCopyNo := 0;
  FRecordRange := rrAllRecords;
  FRangeCount := 0;

  // objects
  inherited Create(AOwner);
  // customization
end;

function TRLCustomSkipper.DataCount: Integer;
begin
  if Assigned(FOnDataCount) then
    FOnDataCount(Self, Result)
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and
    DataSource.DataSet.Active then
    Result := DataSource.DataSet.RecordCount
  else
    Result := 0;
end;

procedure TRLCustomSkipper.DataFirst;
var
  KeepOn: Boolean;
begin
  FRecNo := 1;
  FCopyNo := 1;
  repeat
    if Assigned(DataSource) then
      if Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
      begin
        if FRecordRange = rrAllRecords then
          DataSource.DataSet.First;
        KeepOn := not DataSource.DataSet.Eof;
      end
      else
        KeepOn := False
    else if Assigned(FOnNeedData) then
    begin
      KeepOn := False;
      FOnNeedData(Self, KeepOn);
    end
    else
      KeepOn := False;
    if (FRecordRange = rrNextN) and (FRecNo > FRangeCount) then
      KeepOn := False;

    FDataEof := not KeepOn;
    FRecordAction := raUseIt;
    if Assigned(FOnDataRecord) then
      FOnDataRecord(Self, FRecNo, FCopyNo, FDataEof, FRecordAction);
  until FDataEof or not (FRecordAction in [raIgnoreIt]);
  FRecordMoved := False;
end;

procedure TRLCustomSkipper.DataNext;
var
  KeepOn: Boolean;
begin
  if FRecordAction in [raUseAndRetain] then
    Inc(FCopyNo)
  else
    FCopyNo := 1;
  if FRecordAction in [raUseIt] then
    Inc(FRecNo);
  repeat
    if FRecordAction in [raUseIt, raIgnoreIt] then
    begin
      if Assigned(DataSource) then
        if Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
        begin
          DataSource.DataSet.Next;
          KeepOn := not DataSource.DataSet.Eof;
        end
        else
          KeepOn := False
      else if Assigned(FOnNeedData) then
      begin
        KeepOn := False;
        FOnNeedData(Self, KeepOn);
      end
      else
        KeepOn := False;
    end
    else
      KeepOn := True;
    if FRecordRange = rrCurrentOnly then
      KeepOn := False
    else if (FRecordRange = rrNextN) and (FRecNo > FRangeCount) then
      KeepOn := False;

    FDataEof := not KeepOn;
    FRecordAction := raUseIt;
    if Assigned(FOnDataRecord) then
      FOnDataRecord(Self, FRecNo, FCopyNo, FDataEof, FRecordAction);
  until FDataEof or not (FRecordAction in [raIgnoreIt]);
  FRecordMoved := True;
end;

procedure TRLCustomSkipper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil;
end;

procedure TRLCustomSkipper.InternalPrint;
var
  R: TRLCustomReport;
  P: TRLCustomPager;
begin
  R := RequestParentReport;
  P := RequestParentPager;

  if (PageBreaking = pbBeforePrint) and Assigned(R) and (R.DataBandPrinted > 0) then
  begin
    if Assigned(P) then
      P.InternalNewPage(Self, not P.IsSatisfied);
    MarkPrintPosition;
  end;

  BeginDoc;

  DataFirst;
  while not DataEof and not R.Canceled do
  begin
    FRecordMoved := False;
    PrintDetails;
    if not FRecordMoved then
      DataNext;
  end;

  EndDoc;

  if PageBreaking = pbAfterPrint then
    R.InvalidatePage;
end;

procedure TRLCustomSkipper.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function TRLCustomSkipper.IsNextNRecordRange: Boolean;
begin
  Result := (RecordRange = rrNextN);
end;

{ TRLCustomSubDetail }

constructor TRLCustomSubDetail.Create(AOwner: TComponent);
begin
  // initialization
  // objects
  inherited Create(AOwner);
  // customization
  FPositioning := btDetail;
  FAutoTrunc := True;

  Height := 64;
end;

procedure TRLCustomSubDetail.Paint;
var
  R: TRLCustomReport;
begin
  inherited;

  R := FindParentReport;
  if not Assigned(R) or R.ShowDesigners then
    Signup('SubDetail ' + Name);
end;

procedure TRLCustomSubDetail.SetPositioning(const Value: TRLBandType);
begin
  if Value = FPositioning then
    Exit;
  FPositioning := Value;

  Realign;
  Invalidate;
end;

{TRLCustomReport}

constructor TRLCustomReport.Create(AOwner: TComponent);
begin
  // initialization
  FShowProgress := True;
  FDefaultFilter := nil;
  FExpressionParser := nil;
  FOnPageEnding := nil;
  FOnPageStarting := nil;
  FCanceled := False;
  FNextReport := nil;
  FPriorReport := nil;
  FFirstPageNumber := 1;
  FCurrentPageNumber := 0;
  FPageIndex := 0;
  FPrintDialog := True;
  FPrintEmpty := True;
  FReportState := rsAbout;
  FShowDesigners := True;
  FShowTracks := True;
  FShowExplosion := False;
  FUnlimitedHeight := False;
  FTitle := '';
  FJobTitle := '';
  FPrintQuality := pqFullFeature;
  FPageSurface := nil;
  FOnFilterText := nil;
  ProgressForm := nil;
  FParseInvoker := nil;
  FAdjustableMargins := False;
  FPreviewOptions := nil;
  FCompositeOptions := nil;
  FForcePrepare := True;

  FillChar(DialogParams, SizeOf(DialogParams), 0);
  FillChar(FPrinterMetrics, SizeOf(FPrinterMetrics), 0);
  // objects
  FPages := TRLGraphicStorage.Create(Self);
  FPageSetup := TRLPageSetup.Create(Self);
  FPreviewOptions := TRLPreviewOptions.Create(Self);
  FCompositeOptions := TRLCompositeOptions.Create(Self);

  inherited Create(AOwner);
  // customization
  FMargins.SetDefaults(10, 10, 10, 10);

  ParentFont := False;
  Font.Name := 'Arial';
  Font.Size := 10;
  Font.Style := [];
  Font.Color := clBlack;
  ParentColor := False;
  Color := clWhite;

  ReloadPrinter;
end;

destructor TRLCustomReport.Destroy;
begin
  if Assigned(FPages) then
    FPages.Unlink(Self);
  FreeObj(FPageSurface);
  FreeObj(FPageSetup);
  FreeObj(FPreviewOptions);
  FreeObj(FCompositeOptions);

  inherited;
end;

procedure TRLCustomReport.ReloadPrinter;
begin
  RLPrinter.LoadMetrics(FPrinterMetrics);
end;

procedure TRLCustomReport.CalcSize(var ASize: TPoint);
begin
  ASize := Point(Round(ScreenPPI * FPageSetup.OrientedWidth / InchAsMM),
    Round(ScreenPPI * FPageSetup.OrientedHeight / InchAsMM));
  FFixedSize := ASize;
  FSizeFixed := True;
end;

procedure TRLCustomReport.CreateProgress;
var
  LevelCount: Integer;
  Master: TRLCustomReport;
  Aux: TRLCustomReport;
begin
  Master := MasterReport;
  if Master = Self then
  begin
    LevelCount := 1;
    Aux := Self;
    while Aux.NextReport <> nil do
    begin
      Aux := Aux.NextReport;
      Inc(LevelCount);
    end;
    Master.ProgressForm := TfrmRLFeedBack.Create(GetLocalizeStr(LocaleStrings.LS_PrintingInProgressStr),
      LevelCount);
    Master.ProgressForm.Show;
    Master.ProgressForm.SetFocus;
    Master.ProgressForm.OnCancel := ProgressCanceled;
  end
  else if Master.ProgressForm <> nil then
    Master.ProgressForm.NextLevel;
end;

procedure TRLCustomReport.SetProgressSize(AMax: Integer);
begin
  with MasterReport do
    if Assigned(ProgressForm) then
      ProgressForm.SetMax(AMax);
end;

procedure TRLCustomReport.SetProgressPhase(const APhase: string);
begin
  with MasterReport do
    if Assigned(ProgressForm) then
      ProgressForm.StepCaption(APhase);
end;

procedure TRLCustomReport.StepProgress(AStep: Integer = 1);
begin
  with MasterReport do
    if Assigned(ProgressForm) then
      ProgressForm.Tick;
end;

procedure TRLCustomReport.DestroyProgress;
begin
  FreeObj(ProgressForm);
end;

procedure TRLCustomReport.Cancel;
begin
  FCanceled := True;
end;

procedure TRLCustomReport.ProgressCanceled(Sender: TObject; var CancelIt: Boolean);
begin
  Cancel;
end;

procedure TRLCustomReport.Clear;
begin
  FreeObj(FPageSurface);
  FPages.Clear;
  FReportState := rsAbout;
  FCanceled := False;
  FPageIndex := -1;
  FCurrentPageNumber := FFirstPageNumber - 1;
end;

procedure TRLCustomReport.CheckCloseSurface;
begin
  if FPrintEmpty and (FDetailCount = 0) then
    OpenSurface;
end;

procedure TRLCustomReport.SurfaceOpening;
var
  Master: TRLCustomReport;
begin
  inherited;
  Master := MasterReport;
  Inc(Master.FPageIndex);
  if CompositeOptions.FNumberPages then
    Inc(Master.FCurrentPageNumber);
end;

procedure TRLCustomReport.SurfaceBeginDraw;
begin
  inherited;
  DoPageStarting;
end;

procedure TRLCustomReport.SurfaceEndDraw;
begin
  DoPageEnding;

  inherited;
end;

procedure TRLCustomReport.SurfaceClosed;
begin
  inherited;
  FPageSurface.Macros.Values['PageNo'] := IntToStr(PageNumber);
  FPageSurface.Macros.Values['CompositeIndex'] := IntToStr(CompositeIndex);
  MasterReport.Pages.Add(FPageSurface);
  FPageSurface := nil;
end;

procedure TRLCustomReport.DoPageStarting;
begin
  if Assigned(FOnPageStarting) then
    FOnPageStarting(Self);
end;

procedure TRLCustomReport.DoPageEnding;
begin
  if Assigned(FOnPageEnding) then
    FOnPageEnding(Self);
end;

procedure TRLCustomReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil
    else if AComponent = FNextReport then
      FNextReport := nil
    else if AComponent = FPriorReport then
      FPriorReport := nil
    else if AComponent = FDefaultFilter then
      FDefaultFilter := nil
    else if AComponent = FExpressionParser then
      FExpressionParser := nil;
end;

procedure TRLCustomReport.DataFirst;
begin
  SetProgressSize(DataCount);

  inherited;
end;

procedure TRLCustomReport.DataNext;
begin
  StepProgress;
  SetProgressPhase(GetLocalizeStr(LocaleStrings.LS_PageStr + ' ' + IntToStr(MasterReport.PageNumber)));
  inherited;
end;

function TRLCustomReport.GetOrientedUnprintablePixels: TRect;
var
  R: TRect;
begin
  if FPrinterMetrics.PPIX * FPrinterMetrics.PPIY = 0 then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    with FPrinterMetrics do
      R := Rect(Round(MarginLeft * ScreenPPI / PPIX),
        Round(MarginTop * ScreenPPI / PPIY),
        Round(MarginRight * ScreenPPI / PPIX),
        Round(MarginBottom * ScreenPPI / PPIY));
    if FPageSetup.Orientation = poPortrait then
      Result := R
    else
    begin
      // as landscape the margins are turned anti clockwise
      Result.Left := R.Top;
      Result.Top := R.Right;
      Result.Right := R.Bottom;
      Result.Bottom := R.Left;
    end;
  end;
end;

function TRLCustomReport.GetOrientedUnprintableRect: TRect;
var
  R: TRect;
begin
  R := GetOrientedUnprintablePixels;
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := Round(FPageSetup.OrientedWidth * ScreenPPI / InchAsMM) - R.Right;
  Result.Bottom := Round(FPageSetup.OrientedHeight * ScreenPPI / InchAsMM) - R.Bottom;
end;

// unprintable area
procedure TRLCustomReport.DrawBackground(const ARect: TRect);
var
  R: TRect;
begin
  inherited;

  with Canvas do
  begin
    R := GetOrientedUnprintableRect;
    DrawUnusedRect(Rect(0, 0, R.Left, Height)); // left
    DrawUnusedRect(Rect(R.Left, 0, Width, R.Top)); // top
    DrawUnusedRect(Rect(R.Right, R.Top, Width, Height)); // right
    DrawUnusedRect(Rect(R.Left, R.Bottom, R.Right, Height)); // bottom
  end;
end;

procedure TRLCustomReport.Paint;
begin
  inherited;

  if FShowDesigners then
    Signup('Report ' + Name, True);
end;

function TRLCustomReport.Prepare: Boolean;
var
  KeepOn: Boolean;
  Rep: TRLCustomReport;
  OldPaperHeight: Double;
begin
  Result := False;
  OldPaperHeight := 0;

  try
    Clear;
    FReportState := rsWriting;
    FReportDateTime := Now;

    if UnlimitedHeight then
    begin
      if (csDesigning in ComponentState) then
        OldPaperHeight := PageSetup.PaperHeight;

      PageSetup.PaperHeight := 10;
    end;

    PushBoundsRect;
    try
      if CompositeOptions.ResetPageNumber or (Self = MasterReport) then
        MasterReport.FCurrentPageNumber := Self.FFirstPageNumber - 1;
      if MasterReport.ShowProgress then
        CreateProgress;
      SetProgressPhase(GetLocalizeStr(LocaleStrings.LS_PreparingReportStr));
      KeepOn := True;
      DoBeforePrint(KeepOn);
      if not KeepOn then
        Exit;
      if CompositeOptions.ResetPageNumber or (Self = MasterReport) then
        MasterReport.FCurrentPageNumber := Self.FFirstPageNumber - 1;
      SortBands;
      ReloadPrinter;

      BeginDoc;

      DataFirst;
      FNextReportState:=nrStart;
      while not DataEof and not MasterReport.Canceled do
      begin
        FRecordMoved := False;
        PrintDetails;
        if not FRecordMoved then
          DataNext;
      end;

      if FNextReport = nil then
      begin
        FReportState := rsClosing;
        FNextReportState:=nrEnd;
      end
      else
        FNextReportState:=nrNext;
      CheckCloseSurface;

      EndDoc;
      // verifica cancelamento
      if MasterReport.Canceled then
        Abort;

      DoAfterPrint;

      // atualiza numero da ultima pagina previamente
      MasterReport.Pages.Macros.Values[LastPageNumberMacroName(Self)] :=
        IntToStr(PageNumber);
      // atualiza o numero da ultima pagina para os relatorios anteriores que são da mesma sessão
      Rep := Self;
      while not Rep.CompositeOptions.ResetPageNumber and (Rep.PriorReport <> nil) do
      begin
        MasterReport.Pages.Macros.Values[LastPageNumberMacroName(Rep.PriorReport)] :=
          IntToStr(PageNumber);
        Rep := Rep.PriorReport;
      end;

      // prepara o próximo relatório
      if (FNextReport <> nil) and not FNextReport.Prepare then
        Exit;
      // depois disso o relatorio esta pronto
      UpdateMacros;

      FReportState := rsReady;
      Result := True;
    except
      on E: Exception do
      begin
        FReportState := rsAbout;
        if FPriorReport <> nil then
          raise
        else if not (E is EAbort) then
          if ReportServiceMode then
            Log(Name + ': ' + GetLocalizeStr(LocaleStrings.LS_PrepareErrorStr) + sLineBreak +
              E.ClassName + '(' + E.Message + ')')
          else
            if Assigned(OnPrepareError) then
              OnPrepareError(Self, E)
            else
              ShowMessage(GetLocalizeStr(Name + ': ' + LocaleStrings.LS_PrepareErrorStr +
                          sLineBreak + E.ClassName + '(' + E.Message + ')'));
      end;
    end;
  finally
    DestroyProgress;
    PopBoundsRect;
    if UnlimitedHeight and (csDesigning in ComponentState) then
      PageSetup.PaperHeight := OldPaperHeight;
  end;
end;

function TRLCustomReport.PreviewModal: Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) or FForcePrepare then
    Clear;
  if Assigned(DefaultFilter) then
    SelectedFilter := DefaultFilter;
  if FReportState = rsAbout then
    if not Prepare then
      Exit;
  with PreviewOptions do
    if Defaults = pdUseDefaults then
      PreviewPagesWithOptions(Self.Pages, True, DefaultFormStyle,
        DefaultPosition, DefaultWindowState, DefaultBorderIcons, DefaultHelpFile,
        DefaultHelpContext, DefaultCaption)
    else
      PreviewPagesWithOptions(Self.Pages, True, FormStyle, Position,
        WindowState, BorderIcons, HelpFile, HelpContext, Caption);
  Result := True;
end;

function TRLCustomReport.Preview(Dest: TRLPreview = nil): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) or FForcePrepare then
    Clear;
  if Assigned(DefaultFilter) then
    SelectedFilter := DefaultFilter;
  if FReportState = rsAbout then
    if not Prepare then
      Exit;
  if Assigned(Dest) then
    Dest.Pages := Self.Pages
  else
    with PreviewOptions do
      if Defaults = pdUseDefaults then
        PreviewPagesWithOptions(Self.Pages, DefaultShowModal,
          DefaultFormStyle, DefaultPosition, DefaultWindowState, DefaultBorderIcons,
          DefaultHelpFile, DefaultHelpContext, DefaultCaption)
      else
        PreviewPagesWithOptions(Self.Pages, ShowModal, FormStyle,
          Position, WindowState, BorderIcons, HelpFile, HelpContext, Caption);
  Result := True;
end;

procedure TRLCustomReport.ClosePreview;
begin
  PreviewClosed := True;
end;

function TRLCustomReport.ShowPrintDialog: Boolean;
var
  Dialog: TRLPrintDialog;
begin
  Dialog := TRLPrintDialog.CreateNew(nil);
  try
    Dialog.HelpContext := Self.HelpContext;
    if Dialog.HelpContext <> 0 then
      Dialog.Options := Dialog.Options + [rpoHelp];
    if Self.ReportState = rsReady then
      Dialog.MaxPage := Self.Pages.PageCount;
    Dialog.Orientation := Self.PageSetup.Orientation;
    Dialog.Copies := RLPrinter.Copies;
    Result := Dialog.Execute;
    if Result then
    begin
      Self.DialogParams.FromPage1 := Dialog.FromPage;
      Self.DialogParams.ToPage1 := Dialog.ToPage;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TRLCustomReport.InternalPrint;
var
  filter: TRLCustomFilter;
  oddp: Integer;
begin
  RLPrinter.SetPaperSize(FPageSetup.PaperWidth, FPageSetup.PaperHeight,
    FPageSetup.Orientation = poLandscape, FPageSetup.ForceEmulation, True);
  if Assigned(DefaultFilter) then
    SelectedFilter := DefaultFilter;

  DialogParams.FromPage1 := 1;
  DialogParams.ToPage1 := MaxInt;
  DialogParams.Selection := '';
  RLPrinter.OddEven := odAllPages;
  if RLPrinter.Copies < 1 then
    RLPrinter.Copies := 1;

  if FPrintDialog then
    if not ShowPrintDialog then
    begin
      FCanceled := True;
      SysUtils.Abort;
    end;
  if FReportState = rsAbout then
    if not Prepare then
      Exit;

  DialogParams.FromPage1 := Math.Max(DialogParams.FromPage1, 1);
  DialogParams.ToPage1 := Math.Min(DialogParams.ToPage1, Pages.PageCount);

  filter := SelectedFilter;
  if not Assigned(filter) then
    filter := SpoolFilter;
  filter.ShowProgress := Self.ShowProgress;
  case RLPrinter.OddEven of
    odOddPagesOnly: oddp := PrintOddPagesOnly;
    odEvenPagesOnly: oddp := PrintEvenPagesOnly;
  else
    oddp := PrintOddAndEvenPages;
  end;
  FilterPages(Pages, filter, DialogParams.FromPage1, DialogParams.ToPage1,
    DialogParams.Selection, oddp);
end;

function TRLCustomReport.CompositeIndex: Integer;
var
  Master: TRLCustomReport;
begin
  Result := 0;
  Master := MasterReport;
  while (Master <> nil) and (Master <> Self) do
  begin
    Master := Master.NextReport;
    Inc(Result);
  end;
end;

procedure TRLCustomReport.UpdateMacros;
var
  Aux: string;
begin
  // atualiza símbolos
  Pages.FirstPageNumber := FirstPageNumber;
  ///Pages.LastPageNumber :=PageNumber;

  Pages.Title := Title;

  Aux := JobTitle;
  if Aux = '' then
    Aux := Format(LocaleStrings.LS_DefaultJobTitle,
      [ParamStr(0) + ': ' + Owner.Name + '.' + Self.Name]);
  Pages.JobTitle := GetLocalizeStr(Aux);

  if PageSetup.Orientation = poLandscape then
    Pages.Orientation := MetaOrientationLandscape
  else
    Pages.Orientation := MetaOrientationPortrait;
  Pages.PaperWidth := PageSetup.PaperWidth;
  Pages.PaperHeight := PageSetup.PaperHeight;
end;

function TRLCustomReport.FindParentSurface: TRLGraphicSurface;
begin
  if not Assigned(FPageSurface) then
  begin
    FPageSurface := TRLGraphicSurface.Create;
    with FPageSurface do
    begin
      Width := Self.Width;
      Height := Self.Height;
      if Self.PageSetup.Orientation = poLandscape then
        Orientation := MetaOrientationLandscape
      else
        Orientation := MetaOrientationPortrait;
      PaperWidth := PageSetup.PaperWidth;
      PaperHeight := PageSetup.PaperHeight;
      Open;
    end;
  end;

  Result := FPageSurface;
end;

function TRLCustomReport.CalcSizeRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  CalcSize(Result.BottomRight);
end;

procedure TRLCustomReport.SetPriorReport(const AValue: TRLCustomReport);
var
  old: TRLCustomReport;
begin
  old := FPriorReport;
  if (AValue = old) or (AValue = Self) then
    Exit;
  FPriorReport := AValue;
  if old <> nil then
    old.NextReport := nil;
  if AValue <> nil then
  begin
    AValue.NextReport := Self;
    AValue.FreeNotification(Self);
  end;
end;

procedure TRLCustomReport.SetNextReport(const AValue: TRLCustomReport);
var
  old: TRLCustomReport;
begin
  old := FNextReport;
  if (AValue = old) or (AValue = Self) then
    Exit;
  FNextReport := AValue;
  if old <> nil then
    old.PriorReport := nil;
  if AValue <> nil then
  begin
    AValue.PriorReport := Self;
    AValue.FreeNotification(Self);
  end;
end;

function TRLCustomReport.GetPageNumber;
begin
  Result := MasterReport.FCurrentPageNumber;
end;

procedure TRLCustomReport.SetShowDesigners(const AValue: Boolean);
begin
  if AValue = FShowDesigners then
    Exit;
  FShowDesigners := AValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetShowTracks(const AValue: Boolean);
begin
  if AValue = FShowTracks then
    Exit;
  FShowTracks := AValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetShowExplosion(const AValue: Boolean);
begin
  if AValue = FShowExplosion then
    Exit;
  FShowExplosion := AValue;
  InvalidateAll;
end;

procedure TRLCustomReport.BeforeSave;
begin
  // prepara antes de gravar
  if FReportState = rsAbout then
    if not Prepare then
      Exit;
end;

procedure TRLCustomReport.AfterConstruction;
begin
  //
end;

procedure TRLCustomReport.AfterLoad;
begin
  FirstPageNumber := Pages.FirstPageNumber;
  Title := Pages.Title;
  JobTitle := Pages.JobTitle;
  PageSetup.PaperSize := PaperSizeBySize(Pages.PaperWidth, Pages.PaperHeight);
  if Pages.Orientation = MetaOrientationLandscape then
    PageSetup.Orientation := poLandscape
  else
    PageSetup.Orientation := poPortrait;
  PageSetup.PaperWidth := Pages.PaperWidth;
  PageSetup.PaperHeight := Pages.PaperHeight;

  FReportState := rsReady;
end;

procedure TRLCustomReport.SaveToStream(AStream: TStream);
begin
  BeforeSave;
  Pages.SaveToStream(AStream);
end;

procedure TRLCustomReport.LoadFromStream(AStream: TStream);
begin
  Clear;
  Pages.LoadFromStream(AStream);
  AfterLoad;
end;

procedure TRLCustomReport.SaveToFile(const AFileName: string);
var
  F: TRLCustomSaveFilter;
begin
  BeforeSave;
  F := SaveFilterByFileName(AFileName);
  if F <> nil then
  begin
    F.FileName := AFileName;
    FilterPages(Pages, F, 1, MaxInt, '', PrintOddAndEvenPages);
  end
  else
    Pages.SaveToFile(AFileName);
end;

procedure TRLCustomReport.LoadFromFile(const AFileName: string);
begin
  Clear;
  Pages.LoadFromFile(AFileName);
  AfterLoad;
end;

procedure TRLCustomReport.SetPrintQuality(const AValue: TRLPrintQuality);
begin
  if AValue = FPrintQuality then
    Exit;
  FPrintQuality := AValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetDefaultFilter(const AValue: TRLCustomPrintFilter);
begin
  FDefaultFilter := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function GetDataSourceOf(AControl: TControl): TDataSource;
begin
  if AControl is TRLCustomDBControl then
    Result := TRLCustomDBControl(AControl).DataSource
  else if AControl is TRLCustomDBText then
    Result := TRLCustomDBText(AControl).DataSource
  else if AControl is TRLCustomDBMemo then
    Result := TRLCustomDBMemo(AControl).DataSource
  else if AControl is TRLCustomSkipper then
    Result := TRLCustomSkipper(AControl).DataSource
  else
    Result := nil;
end;

procedure TRLCustomReport.ParserResource(Sender: TObject;
  const AIdentifier: string; AParams: Variant; var AResult: Variant);
var
  src: TDataSource;
  fld: TField;
  ctr: TWinControl;
begin
  if Assigned(FParseInvoker) and (FParseInvoker is TRLCustomDBResult) then
  begin
    AResult := TRLCustomDBResult(FParseInvoker).Resolve(Sender, AIdentifier, AParams);
    if not VarIsEmpty(AResult) then
      Exit;
  end;

  ctr := TWinControl(FParseInvoker);
  while ctr <> nil do
  begin
    src := GetDataSourceOf(ctr);
    if Assigned(src) and Assigned(src.DataSet) then
    begin
      fld := src.DataSet.FindField(AIdentifier);
      if Assigned(fld) then
      begin
        // considerar valores nulos
        if fld.IsNull and Assigned(FParseInvoker) and
          (FParseInvoker is TRLCustomDBResult) and
          TRLCustomDBResult(FParseInvoker).ComputeNulls then
          AResult := GetNullValue(fld)
        else
          AResult := fld.Value;
        Break;
      end;
    end;
    ctr := ctr.Parent;
  end;
end;

procedure TRLCustomReport.ParserTokener(Sender: TObject; var AToken: string;
  var AKind: TRLParserTokenKind);
begin
end;

function MatchFriendlyName(AComp: TComponent; const AName: string): Boolean;
begin
  Result := AnsiSameText(AComp.Name, AName) or (AComp is TRLCustomControl) and
    AnsiSameText(TRLCustomControl(AComp).FriendlyName, AName);
end;

procedure TRLCustomReport.ParserFindAgregate(Sender: TObject;
  AOwner: TPersistent; const AName: string; var AAgregate: TPersistent);
var
  I: Integer;
begin
  if AOwner is TComponent then
    with TComponent(AOwner) do
    begin
      I := 0;
      while (I < ComponentCount) and not MatchFriendlyName(Components[I], AName) do
        Inc(I);
      if I < ComponentCount then
        AAgregate := Components[I];
    end;
end;

procedure TRLCustomReport.ParserGetAttribute(Sender: TObject;
  AOwner: TPersistent; const AName: string; var AValue: Variant);
var
  fld: TField;
begin
  if (AOwner is TDataSource) and Assigned(TDataSource(AOwner).DataSet) then
    AOwner := TDataSource(AOwner).DataSet;
  if AOwner is TDataSet then
  begin
    fld := TDataSet(AOwner).FindField(Name);
    if Assigned(fld) then
      AValue := fld.Value;
  end
  else if AOwner is TRLCustomControl then
    AValue := TRLCustomControl(AOwner).GetAttribute(AName)
  else if AOwner is TFont then
    with TFont(AOwner) do
      AValue := Name + ' ' + IntToStr(Size);
end;

procedure TRLCustomReport.ParserSetAttribute(Sender: TObject;
  AOwner: TPersistent; const AName: string; const AValue: Variant; var AHandled: Boolean);
begin
  if AOwner is TRLCustomControl then
    AHandled := TRLCustomControl(AOwner).SetAttribute(AName, AValue);
end;

procedure TRLCustomReport.SetExpressionParser(const AValue: TRLExpressionParser);
var
  old: TRLExpressionParser;
begin
  old := FExpressionParser;
  if AValue = old then
    Exit;
  if old <> nil then
  begin
    old.ResourceProc := nil;
    old.TokenProc := nil;
    old.FindAgregateProc := nil;
    old.SetAttributeProc := nil;
    old.GetAttributeProc := nil;
  end;
  FExpressionParser := AValue;
  if AValue <> nil then
  begin
    AValue.ResourceProc := Self.ParserResource;
    AValue.TokenProc := Self.ParserTokener;
    AValue.FindAgregateProc := Self.ParserFindAgregate;
    AValue.SetAttributeProc := Self.ParserSetAttribute;
    AValue.GetAttributeProc := Self.ParserGetAttribute;
    AValue.FreeNotification(Self);
  end;
end;

procedure TRLCustomReport.DoFilterText(var AText: string; var APrintIt: Boolean);
begin
  if Assigned(FOnFilterText) then
    FOnFilterText(Self, AText, APrintIt);
end;

function TRLCustomReport.Parse(Sender: TObject; const AExpression: string): Variant;
begin
  if Assigned(FExpressionParser) then
  begin
    FParseInvoker := Sender;
    try
      Result := FExpressionParser.Evaluate(AExpression);
    finally
      FParseInvoker := nil;
    end;
  end
  else
    Result := Unassigned;
end;

function TRLCustomReport.CalcMarginalPixels: TRect;
var
  U: TRect;
begin
  Result := inherited CalcMarginalPixels;
  if FAdjustableMargins then
  begin
    U := GetOrientedUnprintablePixels;
    Result.Left := Math.Max(Result.Left, U.Left);
    Result.Top := Math.Max(Result.Top, U.Top);
    Result.Right := Math.Max(Result.Right, U.Right);
    Result.Bottom := Math.Max(Result.Bottom, U.Bottom);
  end;
end;

procedure TRLCustomReport.SetAdjustableMargins(const AValue: Boolean);
begin
  if AValue = FAdjustableMargins then
    Exit;
  FAdjustableMargins := AValue;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomReport.SetPageSetup(const Value: TRLPageSetup);
begin
  FPageSetup.Assign(Value);
end;

procedure TRLCustomReport.SetPreviewOptions(const Value: TRLPreviewOptions);
begin
  FPreviewOptions.Assign(Value);
end;

procedure TRLCustomReport.SetCompositeOptions(const Value: TRLCompositeOptions);
begin
  FCompositeOptions.Assign(Value);
end;

procedure TRLCustomReport.SetTitle(const Value: string);
var
  WasEqual: Boolean;
begin
  if Value = FTitle then
    Exit;
  WasEqual := FJobTitle = FTitle;
  FTitle := Value;
  if WasEqual then
    FJobTitle := FTitle;
end;

function TRLCustomReport.IsJobTitle: Boolean;
begin
  Result := (FJobTitle <> '') and (FJobTitle <> FTitle);
end;

procedure TRLCustomReport.SetJobTitle(const Value: string);
begin
  if Value = FJobTitle then
    Exit;
  if Value <> '' then
    FJobTitle := Value
  else
    FJobTitle := Title;
end;

procedure TRLCustomReport.SetUnlimitedHeight(const Value: boolean);
begin
  if Value = FUnlimitedHeight then
    Exit;

  if Value then
  begin
    FPageSetup.Orientation := poPortrait;
    FPageSetup.PaperSize := fpCustom;
  end;

  FUnlimitedHeight := Value;
end;

{ TRLPreviewOptions }

constructor TRLPreviewOptions.Create(AOwner: TRLCustomReport);
begin
  FParentReport := AOwner;
  FShowModal := False;
  FFormStyle := fsNormal;
  FPosition := poScreenCenter;
  FWindowState := wsMaximized;
  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FHelpFile := '';
  FHelpContext := 0;
  FCaption := GetLocalizeStr(LocaleStrings.LS_PreviewStr);

  inherited Create;
end;

procedure TRLPreviewOptions.Assign(Source: TRLPreviewOptions);
begin
  WindowState := Source.WindowState;
  Position := Source.Position;
  FormStyle := Source.FormStyle;
  ShowModal := Source.ShowModal;
  BorderIcons := Source.BorderIcons;
  HelpFile := Source.HelpFile;
  HelpContext := Source.HelpContext;
  Caption := Source.Caption;
  Defaults := Source.Defaults;
end;

function TRLPreviewOptions.IsCaption: Boolean;
begin
  Result := (FCaption <> GetLocalizeStr(LocaleStrings.LS_PreviewStr));
end;

{ TRLCompositeOptions }

constructor TRLCompositeOptions.Create(AOwner: TRLCustomReport);
begin
  FParentReport := AOwner;
  FResetPageNumber := False;
  FNumberPages := True;
  inherited Create;
end;

procedure TRLCompositeOptions.Assign(Source: TRLCompositeOptions);
begin
  Self.ResetPageNumber := Source.ResetPageNumber;
end;

end.
