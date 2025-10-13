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

{@unit RLBarcode - Implementação dos componentes para código de barras. }
unit RLBarcode;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils, DB,
  {$IfDef CLX}
   QTypes, QGraphics, QDialogs,
  {$Else}
   Types, Graphics, Dialogs,
  {$EndIf}
  {$IfDef FPC}
   LCLIntf,
  {$EndIf}
  RLReport, RLConsts;

type
  {@type TRLBarcodeType - Padrão de codificação para o código de barras.
  Pode ser um dos seguintes valores:
  bcCode2OF5Interleaved - Código 25, também conhecido como "Código 2 de 5". É
    utilizado sobretudo no manuseio de inventários, em fichas de compensação
    bancária, na identificação de envelopes de acabamento de fotografias, em
    passagens aéreas, no manuseio de bagagens e cargas e em dezenas de outras
    aplicações. É um formato de código distinto, de comprimento variável e
    consiste em duas barras espessas em um total de cinco barras para cada
    caractere codificado. O código deve ter comprimento par;
  bcCode2OF5Industry - ITF ou "Entrelaçado de 2 de 5". Esse código de barras é um
    dos formatos mais populares utilizados pelas indústrias de transporte e de
    armazenamento e foi desenvolvido com base no Código 25. Ambos os formatos
    utilizam as mesmas técnicas de codificação, exceto que, no formato ITF,
    tanto as barras quanto os espaços transportam dados. Os dígitos de posição
    ímpar são codificados nas barras e os dígitos de posição par são codificados
    nos espaços. O ITF é um formato de alta densidade, de comprimento variável,
    exclusivamente numérico;
  bcCode2OF5Matrix - ver bcCode2OF5Industry;
  bcCode39 - Código 39, também conhecido como "Código 3 de 9", é o formato mais
    popular utilizado em inventário e controle não varejista. O formato consiste
    em três elementos espessos (barras ou espaços) em um totalizado em manufatura,
    aplicações militares e de saúde. O formato distinto de comprimento variável
    aceita os 44 caracteres seguintes: 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.*$/+%. O
    asterisco (*) é utilizado como caractere de início/parada, não podendo ser
    utilizado no corpo da mensagem. Você também pode adicionar um dígito de verificação
    que ajude a garantir a segurança do código de barras. O Código 39 suporta os
    formatos de dígito de verificação Módulo 43 e xxx-nnnnnnn-c utilizados pela
    alfândega dos E.U.A. para remessas de importação/exportação e em dezenas de
    outras aplicações;
  bcCode39Extended - O código extendido 39 foi desenvolvido para proporcionar
    meios de codificar os caracteres adicionais que não são normalmente parte
    do conjunto de caracteres do código 39 (caracteres minúsculos e símbolos). Os
    caracteres extendidos são codificados por um par de caracteres normais do
    código 39; por exemplo, uma minúscula "a" (que não faz parte do conjunto de
    caracteres do código 39) pode ser codificado pelo par "+A". Um código de controle
    de retorno do carro pode ser codificado pelo par "#";
  bcCode128A - Código 128 é um formato alfanumérico de alta densidade e comprimento
    variável utilizado na indústria de transporte e etiquetagem. Esse código possui
    106 padrões de barras e espaços. Cada padrão pode ter três significados, dependendo
    de qual dos três conjuntos de caracteres é empregado. Um conjunto de caracteres
    codifica todos os caracteres de controle ASCII e maiúsculos, um outro codifica
    todos os caracteres maiúsculos e minúsculos e o terceiro conjunto codifica os
    pares de dígitos numéricos de 00 a 99. O conjunto de caracteres utilizado é
    determinado pelo caractere inicial. O Código 128 também permite codificar quatro
    códigos de função:
    FNC1 - reservado para uso em EAN (European Article Numbering);
    FNC2 - utilizado para instruir o leitor de código de barras na concatenação da
      mensagem em um símbolo de código de barras com a mensagem no símbolo de texto;
    FNC3 - utilizado para instruir o leitor de código de barras a efetuar uma redefinição;
    FNC4 - utilizado em aplicações de sistemas fechados.
    Uma variação do formato Código 128 é o EAN 128. Esse símbolo utiliza o mesmo
    conjunto de códigos que o Código 128, mas os códigos de função de FNC2 a FNC4
    não podem ser utilizados e FNC1 é utilizado como parte do código inicial;
  bcCode128B - ver bcCode128A;
  bcCode128C - ver bcCode128A;
  bcCode93 - O código 93 é uma versão mais compacta do código 39. Codifica
    exatamente os mesmos caracteres que o código 39, mas utiliza 9 elementos de
    barra por caractere ao invés de 15. O dígito verificador o dígito verificador
    módulo 43 é opcional, como no código 39;
  bcCode93Extended - ver bcCode93;
  bcMSI - O código de barras MSI Plessey é utilizado principalmente em bibliotecas e
    em etiquetagem de prateleiras de lojas. O MSI Plessey é um formato de comprimento
    variável que permite codificar os 10 caracteres seguintes: 0123456789. Cada caractere
    consiste em oito elementos: quatro barras e quatro espaços;
  bcPostNet - Os códigos de barras POSTNET (Postal Numeric Encoding Technique) são
    utilizados para codificar códigos de endereçamento postal no correio dos
    E.U.A. O processo de manuseio de correspondência do Serviço postal foi
    desenvolvido para ser totalmente automatizado e os códigos de barras POSTNET
    alimentam o equipamento automatizado. O POSTNET difere dos outros formatos em
    que a altura das barras varia, e não a largura das barras. Cada número é
    representado por um padrão de cinco barras. Uma única barra alta é utilizada
    para as barras de início e parada. O POSTNET pode ser utilizado como código
    de barras de ponto de entrega de cinco dígitos, de nove dígitos e de 11
    dígitos. Esses códigos são freqüentemente utilizados em conjunto com as barras
    FIM que se encontram no canto superior direito de uma correspondência, como
    cartões-resposta comerciais;
  bcCodaBar - O CodBar é utilizado freqüentemente em bibliotecas, bancos de
    sangue e na atividade de encomendas aéreas. O formato de comprimento variável
    permite a codificação dos 20 caracteres seguintes: 0123456789-$:/.+ABCD. Os
    caracteres de início e de parada de uma mensagem CodBar precisam ser A, B, C ou D;
  bcEAN8 - O sistema EAN (European Article Numbering) é uma versão européia do
    código UPC (Universal Product Code). Atualmente, esse código é denominado
    International Article Number, mas a abreviação EAN permanece. Os códigos
    EAN encontram-se em itens de varejo na Europa. Esse número é apropriado para uso
    em publicações e periódicos, aparecendo como um código de barras adicional
    no lado direito do código de barras principal. É a versão simplificada do padrão
    EAN-13 para aplicação em produtos onde a etiqueta no padrão EAN-13 fique muito
    grande. O EAN-8 codifica até oito dígitos, consistindo em dois dígitos do código
    do país, cinco dígitos de dados e um dígito de verificação. Um número opcional de
    dois ou cinco dígitos pode ser acrescentado ao código de barras principal;
  bcEAN13 - O EAN-13 é a versão européia do UPC (A) (Universal Product Code). É o
    padrão adotado pela ABAC (EAN Brasil) para codificação de produtos em
    supermercados. Também é designado para uso em publicações e periódicos, aparecendo
    como um código de barras adicional no lado direito do código de barras principal. Permite
    a codificação de até 13 dígitos numéricos. A diferença entre o EAN-13 e o
    UPC (A) é que o EAN-13 codifica um 13° dígito no padrão de paridade dos seis dígitos
    da esquerda de um símbolo UPC (A). Esse 13° dígito, combinado com o 12°, representa um
    código de país. Um número opcional de dois ou cinco dígitos pode ser acrescentado ao
    código de barras principal;
  bcUPC_A - Os símbolos UPC (Universal Product Code) são usados em aplicações de
    varejo nos Estados Unidos e no Canadá. O UPC(A) é um formato de 12
    dígitos. O símbolo consiste em 11 dígitos de dados e um dígito de
    verificação. Normalmente, o primeiro dígito representa o tipo de produto
    sendo identificado. Os cinco dígitos seguintes são um código de fabricante
    e os cinco dígitos seguintes são utilizados para identificar um produto específico;
  bcUPC_E0 - Como o UPC(A), o UPC(E) é utilizado em aplicações de varejo, no entanto,
    como o código de barras é menor, ele é mais adequado para itens menores. Esse formato
    também é chamado de "zero suprimido" porque o UPC(E) compacta um código de 12 dígitos
    UPC(A) em um código de seis dígitos. O UPC(E) suprime o dígito de sistema numérico,
    os dígitos finais no código de fabricante e os zeros iniciais na parte de identificação
    de produto do código. Um número opcional de dois ou cinco dígitos pode ser adicionado
    ao do código de barras UPC(A) e UPC(E) principal. Esse número é designado para uso em
    publicações e periódicos, aparecendo como um código de barras adicional no lado direito
    do código de barras principal;
  bcUPC_E1 - ver bcUPC_E0;
  bcUPC_Supp2 - ver bcUPC_Supp;
  bcUPC_Supp5 - ver bcUPC_Supp;
  bcEAN128A - Mais abrangente que os demais códigos, o UCC/EAN-128 é complementar,
    baseado em Identificadores de Aplicação (AI), identificando o significado e o
    formato de dados. O UCC/EAN-128 pode, inclusive, ser aplicado em unidades de
    distribuição, permitindo a identificação do número de lote, série, data de
    fabricação, validade, textos livres e outros dados. A utilização do UCC/EAN-128
    é múltipla, podendo ser aplicado na logística e automação de vários setores
    produtivos e comerciais, como o ramo alimentício, farmacêutico, vestuário e
    de papel, entre outros. Além disso, pode ser usado na distribuição, armazenamento,
    inventários e gestão de estoque, proporcionando agilidade na captura de informações,
    com menor margem de erros. Trata-se de um sistema que possui abrangência necessária
    para a obtenção de grandes ganhos na cadeia distributiva, sempre objetivando a
    otimizar e a maximizar, por meio da informação rápida e precisa;
  bcEAN128B - ver bcEAN128A;
  bcEAN128C - ver bcEAN128A.
  :}
  TRLBarcodeType = (bcCode2OF5Interleaved, bcCode2OF5Industry, bcCode2OF5Matrix, 
    bcCode39, bcCode39Extended, bcCode128A, bcCode128B, bcCode128C, 
    bcCode93, bcCode93Extended, bcMSI, bcPostNet, bcCodaBar, bcEAN8, 
    bcEAN13, bcUPC_A, bcUPC_E0, bcUPC_E1, bcUPC_Supp2, bcUPC_Supp5, 
    bcEAN128A, bcEAN128B, bcEAN128C);
  {/@type}

  // para uso interno somente
  // blHalfFilled significa uma linha preta com altura de 2/5 (used for PostNet)
  TRLBarcodeLineType = (blFilled, blNotFilled, blHalfFilled);

  TRLBarcodeBarWidth = (bw100, bw100Ratio, bw150Ratio, bw200Ratio);

  // quais textos mostrar
  TRLBarcodeTextOption = (boNone, boCode, boType, boBoth);

  TRLBarcodeCheckSumMethod = (cmNone, cmModule10);

  {@type TRLBarcodeOrientation - Orientação do desenho das barras.
   Pode ser um dos seguintes valores:
   boLeftToRight - Da esquerda para a direita;
   boBottomToTop - De baixo para cima;
   boTopToBottom - De cima para baixo. :}
  TRLBarcodeOrientation = (boLeftToRight, boBottomToTop, boTopToBottom);
  {/@type}

  {@type TRLBarcodeInvalidCode - O que deve ser exibido se o código contiver erros.
   Pode ser um dos seguintes valores:
   icEmptyRect - Apresenta um retângulo vazio;
   icCrossOut - Apresenta o código de barras rasurado com uma cruz vermelha;
   icDrawAnyway - Desenha as barras extraindo os dígitos inválidos.:}
  TRLBarcodeInvalidCode = (icEmptyRect, icCrossOut, icDrawAnyway);
  {/@type}

  { TRLCustomBarcode }

  {@class TRLCustomBarcode - Classe base da qual podem derivar componentes para códigos de barras. @ancestor TRLCustomControl. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLCustomBarcode = class(TRLCustomControl)
  private
    FBeforeText: TRLBeforeTextEvent;
    FBarColor: TColor;
    FShowText: TRLBarcodeTextOption;
    FOrientation: TRLBarcodeOrientation;
    FMargins: TRLMargins;
    FModule: Integer;
    FRatio: Double;
    FBarcodeType: TRLBarcodeType;
    FCheckSum: Boolean;
    FCheckSumMethod: TRLBarcodeCheckSumMethod;
    FModules: array[TRLBarcodeBarWidth] of ShortInt;
    FInvalid: Boolean;
    FInvalidCode: TRLBarcodeInvalidCode;
    procedure GetBarInfo(AChar: Char; var ABarWidth: Integer;
      var ALineType: TRLBarcodeLineType);
    function GetTypeText: string;
    function GetImageWidth(const ABarData: string): Integer;
    function GetBarData(const AText: string): string;
    procedure SetModule(Value: Integer);
    procedure SetBarColor(const Value: TColor);
    procedure SetShowText(const Value: TRLBarcodeTextOption);
    procedure SetBarcodeType(const Value: TRLBarcodeType);
    procedure SetRatio(const Value: Double);
    procedure SetOrientation(const Value: TRLBarcodeOrientation);
    procedure SetMargins(const AValue: TRLMargins);
    procedure SetInvalidCode(const Value: TRLBarcodeInvalidCode);
    procedure SetCheckSum(const Value: Boolean);
    procedure SetCheckSumMethod(const Value: TRLBarcodeCheckSumMethod);
    function GetAs2OF5Interleaved(const AText: string): string;
    function GetAs2OF5Industry(const AText: string): string;
    function GetAs2OF5Matrix(const AText: string): string;
    function GetAs39(const AText: string): string;
    function GetAs39Extended(const AText: string): string;
    function GetAs128(const AText: string): string;
    function GetAs93(const AText: string): string;
    function GetAs93Extended(const AText: string): string;
    function GetAsMSI(const AText: string): string;
    function GetAsPostNet(const AText: string): string;
    function GetAsCodaBar(const AText: string): string;
    function GetAsEAN8(const AText: string): string;
    function GetAsEAN13(const AText: string): string;
    function GetAsUPC_A(const AText: string): string;
    function GetAsUPC_E0(const AText: string): string;
    function GetAsUPC_E1(const AText: string): string;
    function GetAsUPC_Supp5(const AText: string): string;
    function GetAsUPC_Supp2(const AText: string): string;
    procedure MakeModules;
    function DoCheckSumming(const AData: string): string;
    function CreateBitmap(const AText: string; AWidth, AHeight: Integer): TBitmap;
    function IsRatio: Boolean;
    function CalcMarginalPixels: TRect;
  protected
    procedure CalcSize(var ASize: TPoint); override;
    procedure InternalPrint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    {@prop AutoSize - Redimensionamento automático. Determina se o controle irá se redimensionar automaticamente de acordo com o tamanho do seu conteúdo. :/}
    property AutoSize default True;
    {@prop Caption - Texto a ser impresso como código de barras. :/}
    property Caption;
    {@prop BarColor - Cor das barras. Determina a cor das barras cheias. :/}
    property BarColor: TColor read FBarColor write SetBarColor default clBlack;
    {@prop ShowText - Determina se e como serão exibidas as informações junto com as barras.
     Pode ser um dos seguintes valores:
     boNone - Nenhum texto é exibido;
     boCode - Apenas o valor do código de barras;
     boType - Apenas o tipo de código de barras utilizado;
     boBoth - Ambos o valor e o tipo de código. :/}
    property ShowText: TRLBarcodeTextOption read FShowText write SetShowText default boNone;
    {@prop Module - Fator de ampliação da largura das barras. :/}
    property Module: Integer read FModule write SetModule default 1;
    {@prop Ratio - Razão entre as larguras das barras. :/}
    property Ratio: Double read FRatio write SetRatio stored IsRatio;
    {@prop BarcodeType - Padrão de código de barras. @links TRLBarcodeType. :/}
    property BarcodeType: TRLBarcodeType read FBarcodeType write SetBarcodeType default bcCode2of5Interleaved;
    {@prop Orientation - Orientação da leitura das barras. :/}
    property Orientation: TRLBarcodeOrientation read FOrientation write SetOrientation default boLeftToRight;
    {@prop Margins - Margens externas do código de barras. @links TRLMargins. :/}
    property Margins: TRLMargins read FMargins write SetMargins;
    {@prop InvalidCode - Determina o que deve ser exibido se o código tiver algum erro. @links TRLBarcodeInvalidCode. :/}
    property InvalidCode: TRLBarcodeInvalidCode read FInvalidCode write SetInvalidCode default icEmptyRect;
    property CheckSum: Boolean read FCheckSum write SetCheckSum default False;
    property CheckSumMethod: TRLBarcodeCheckSumMethod read FCheckSumMethod write SetCheckSumMethod default cmModule10;
    {@prop BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou anular a sua impressão. :/}
    property BeforePrint: TRLBeforeTextEvent read FBeforeText write FBeforeText;
  end;
  {/@class}
  

  { TRLCustomDBBarcode }

  {@class TRLCustomDBBarcode - Classe base da qual podem derivar componentes para códigos de barras dataware. @ancestor TRLCustomBarcode.}
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLCustomDBBarcode = class(TRLCustomBarcode)
  private
    FDataField: TRLDataFieldProperty;
    FDataFormula: string;
    FDataSource: TDataSource;
    function GetField: TField;
    function GetFieldLabel: string;
    function GetDataSet: TDataSet;
    procedure SetDataField(const AValue: TRLDataFieldProperty);
    procedure SetDataFormula(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
  protected
    function InternalMakeCaption: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetFieldText: string; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    {@prop DataField - Nome do campo associado. :/}
    property DataField: TRLDataFieldProperty read FDataField write SetDataField;
    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property DataFormula: string read FDataFormula write SetDataFormula;
    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property DataSource: TDataSource read FDataSource write SetDataSource;
    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property Field: TField read GetField;
    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property DataSet: TDataSet read GetDataSet;
  end;
  {/@class}


  { TRLBarcode }

  {@class TRLBarcode - Componente para códigos de barras. @pub. @ancestor TRLCustomBarcode. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLBarcode = class(TRLCustomBarcode)
  published
    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop BarcodeType = ancestor /}
    property BarcodeType;
    {@prop BarColor = ancestor /}
    property BarColor;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop Caption = ancestor /}
    property Caption;
    {@prop CheckSum = ancestor /}
    property CheckSum;
    {@prop CheckSumMethod = ancestor /}
    property CheckSumMethod;
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
    {@prop InvalidCode = ancestor /}
    property InvalidCode;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop Module = ancestor /}
    property Module;
    {@prop Orientation = ancestor /}
    property Orientation;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop Ratio = ancestor /}
    property Ratio;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop ShowText = ancestor /}
    property ShowText;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop AfterPrint = ancestor /}
    property AfterPrint;
    {@prop BeforePrint = ancestor /}
    property BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;
  {/@class}


  {@class TRLDBBarcode - Componente para códigos de barras dataware. @pub. @ancestor TRLCustomDBBarcode. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLDBBarcode = class(TRLCustomDBBarcode)
  published
    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop BarcodeType = ancestor /}
    property BarcodeType;
    {@prop BarColor = ancestor /}
    property BarColor;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
    {@prop CheckSum = ancestor /}
    property CheckSum;
    {@prop CheckSumMethod = ancestor /}
    property CheckSumMethod;
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
    {@prop InvalidCode = ancestor /}
    property InvalidCode;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop Margins = ancestor /}
    property Margins;
    {@prop Module = ancestor /}
    property Module;
    {@prop Orientation = ancestor /}
    property Orientation;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop Ratio = ancestor /}
    property Ratio;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop ShowText = ancestor /}
    property ShowText;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop AfterPrint = ancestor /}
    property AfterPrint;
    {@prop BeforePrint = ancestor /}
    property BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;
  {/@class}

{/@unit}

implementation

uses
  RLUtils;

type
  TRLBarcodeTypeInfo = record
    Name: string; // name of barcode
    DigitsOnly: Boolean; // numeric data only
  end;

var
  BarcodeTypeInfo: array[TRLBarcodeType] of TRLBarcodeTypeInfo = (
    (Name: '2OF5 Interleaved'; DigitsOnly: True), 
    (Name: '2OF5 Industrial'; DigitsOnly: True), 
    (Name: '2OF5 Matrix'; DigitsOnly: True), 
    (Name: 'Code 39'; DigitsOnly: False), 
    (Name: 'Code 39 Extended'; DigitsOnly: False), 
    (Name: 'Code 128A'; DigitsOnly: False), 
    (Name: 'Code 128B'; DigitsOnly: False), 
    (Name: 'Code 128C'; DigitsOnly: True), 
    (Name: 'Code 93'; DigitsOnly: False), 
    (Name: 'Code 93 Extended'; DigitsOnly: False), 
    (Name: 'MSI'; DigitsOnly: True), 
    (Name: 'PostNet'; DigitsOnly: True), 
    (Name: 'CodaBar'; DigitsOnly: False), 
    (Name: 'EAN8'; DigitsOnly: True), 
    (Name: 'EAN13'; DigitsOnly: True), 
    (Name: 'UPC A'; DigitsOnly: True), 
    (Name: 'UPC E0'; DigitsOnly: True), 
    (Name: 'UPC E1'; DigitsOnly: True), 
    (Name: 'UPC Supp2'; DigitsOnly: True), 
    (Name: 'UPC Supp5'; DigitsOnly: True), 
    (Name: 'EAN 128A'; DigitsOnly: False), 
    (Name: 'EAN 128B'; DigitsOnly: False), 
    (Name: 'EAN 128C'; DigitsOnly: True));

// UTILS

function CheckSumModule10(const AData: string): string;
var
  I, InverseI, Sum: Integer;
begin
  Sum := 0;
  InverseI := Length(AData);
  for I := 1 to Length(AData) do
  begin
    if (InverseI mod 2) = 0 then
      Inc(Sum, StrToInt(AData[I]) * 1)
    else
      Inc(Sum, StrToInt(AData[I]) * 3);
    Dec(InverseI);
  end;
  if (Sum mod 10) = 0 then
    Result := AData + '0'
  else
    Result := AData + IntToStr(10 - (Sum mod 10));
end;

// converts a string from '321' to the internal representation '715'
// i need this function because some pattern tables have a different
// format :
// '00111'
// converts to '05161'
function Convert(const Str: string): string;
var
  I, V: Integer;
begin
  Result := '';
  for I := 1 to Length(Str) do
  begin
    V := Ord(Str[I]) - 1;
    if Odd(I) then
      Inc(V, 5);
    Result := Result + Char(V);
  end;
end;

function PadZ(const AText: string; AWidth: Integer): string;
begin
  Result := AText;
  while Length(Result) < AWidth do
    Result := '0' + Result;
end;

function EvenZ(const AText: string): string;
begin
  Result := AText;
  if Odd(Length(Result)) then
    Result := '0' + Result;
end;

{ TRLCustomBarcode }

constructor TRLCustomBarcode.Create(AOwner: TComponent);
var
  SelfSize: TPoint;
begin
  FShowText := boNone;
  FBarColor := clBlack;
  FOrientation := boLeftToRight;
  FRatio := 2;
  FModule := 1;
  FBarcodeType := bcCode2of5Interleaved;
  FCheckSum := False;
  FCheckSumMethod := cmModule10;
  FInvalidCode := icEmptyRect;
  FMargins := TRLMargins.Create(Self);
  inherited;
  CalcSize(SelfSize);
  Width := SelfSize.X;
  Height := 34;
  AutoSizeDir := [asWidthDir];
  AutoSize := True;
  with FMargins do
  begin
    LeftMargin := 1;
    TopMargin := 0;
    RightMargin := 1;
    BottomMargin := 0;
  end;
end;

destructor TRLCustomBarcode.Destroy;
begin
  FreeObj(FMargins);
  inherited;
end;

function TRLCustomBarcode.IsRatio: Boolean;
begin
  Result := (FRatio <> 2);
end;

// margens em pixels
function TRLCustomBarcode.CalcMarginalPixels: TRect;
begin
  Result.Left := Round(ScreenPPI * FMargins.LeftMargin / InchAsMM);
  Result.Top := Round(ScreenPPI * FMargins.TopMargin / InchAsMM);
  Result.Right := Round(ScreenPPI * FMargins.RightMargin / InchAsMM);
  Result.Bottom := Round(ScreenPPI * FMargins.BottomMargin / InchAsMM);
end;

procedure TRLCustomBarcode.CalcSize(var ASize: TPoint);
var
  ImageWidth, BorderWidth: Integer;
  MarginalPixels: TRect;
  SizeAxis: ^Integer;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  ImageWidth := GetImageWidth(GetBarData(Caption)) + 1;
  if FOrientation = boLeftToRight then
    SizeAxis := @ASize.X
  else
    SizeAxis := @ASize.Y;
  SizeAxis^ := ImageWidth;
  MarginalPixels := CalcMarginalPixels;
  Inc(SizeAxis^, MarginalPixels.Left + MarginalPixels.Right);
  // adicional das bordas
  BorderWidth := Self.Borders.Width;
  if BorderWidth > 0 then
  begin
    Inc(BorderWidth);
    if Self.Borders.CanDrawLeft then
      Inc(SizeAxis^, BorderWidth);
    if Self.Borders.CanDrawRight then
      Inc(SizeAxis^, BorderWidth);
  end;
end;

function TRLCustomBarcode.CreateBitmap(const AText: string; AWidth, AHeight: Integer): TBitmap;
var
  BarWidth: Integer;
  LineType: TRLBarcodeLineType;
  BarData, S: string;
  PaintRect: TRect;
  MarginalRect: TRect;
  FooRect: TRect;
  I: Integer;
  StrWidth, StrHeight: Integer;
begin
  Result := NewBitmap(AWidth, AHeight);
  try
    BarData := GetBarData(AText);
    // desenha o código de barras
    PaintRect := Rect(0, 0, AWidth, AHeight);
    MarginalRect := PaintRect;
    with Result.Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Color;
      FillRect(PaintRect);
      Pen.Style := psSolid;
      Pen.Width := 1;
      // examine the pattern string
      if FShowText <> boNone then
      begin
        Font.Assign(Self.Font);
        // reserva uma linha no topo para o tipo
        if FShowText in [boType, boBoth] then
          Inc(MarginalRect.Top, TextHeight(' '));
        // reserva meia linha em baixo para o código (postnet é um linha)
        if FShowText in [boCode, boBoth] then
          if FBarcodeType = bcPostNet then
            Dec(MarginalRect.Bottom, TextHeight(' '))
          else
            Dec(MarginalRect.Bottom, TextHeight(' ') div 2);
      end;
      // centraliza a imagem das barras
      MarginalRect.Right := MarginalRect.Left + GetImageWidth(BarData);
      case Alignment of
        taCenter: OffsetRect(MarginalRect, (RectWidth(PaintRect) - RectWidth(MarginalRect)) div 2, 0);
        taRightJustify: OffsetRect(MarginalRect, RectWidth(PaintRect) - RectWidth(MarginalRect), 0);
      end;
      // desenha as barras
      if (FInvalid and (FInvalidCode = icEmptyRect)) or (Trim(AText) = '') then
      else
      begin
        FooRect := MarginalRect;
        for I := 1 to Length(BarData) do
        begin
          GetBarInfo(BarData[I], BarWidth, LineType);
          // determina a cor da barra
          if LineType in [blNotFilled, blHalfFilled] then
            Brush.Color := Self.BarColor
          else
            Brush.Color := Self.Color;
          if LineType = blHalfFilled then
            FooRect.Top := MarginalRect.Bottom - (MarginalRect.Bottom - MarginalRect.Top) * 2 div 5
          else
            FooRect.Top := MarginalRect.Top;
          FooRect.Right := FooRect.Left + BarWidth;
          // draw the rectangle
          FillRect(FooRect);
          // step it
          Inc(FooRect.Left, BarWidth);
        end;
        if FInvalid and (InvalidCode = icCrossOut) then
        begin
          Pen.Width := 4;
          Pen.Color := clRed;
          MoveTo(MarginalRect.Left + Pen.Width, MarginalRect.Top + Pen.Width);
          LineTo(MarginalRect.Right - Pen.Width - 1, MarginalRect.Bottom - Pen.Width - 1);
          MoveTo(MarginalRect.Right - Pen.Width - 1, MarginalRect.Top + Pen.Width);
          LineTo(MarginalRect.Left + Pen.Width, MarginalRect.Bottom - Pen.Width - 1);
        end;
      end;
      // desenha o texto
      if FShowText <> boNone then
      begin
        Font.Assign(Self.Font);
        Brush.Style := bsSolid;
        Brush.Color := Self.Color;
        if FShowText in [boType, boBoth] then
        begin
          S := GetTypeText;
          StrWidth := TextWidth(S);
          StrHeight := TextHeight(S + ' ');
          FooRect.Left := (MarginalRect.Left + MarginalRect.Right - StrWidth) div 2;
          FooRect.Top := PaintRect.Top;
          FooRect.Right := FooRect.Left + StrWidth;
          FooRect.Bottom := FooRect.Top + StrHeight;
          FillRect(FooRect);
          TextRect(FooRect, FooRect.Left, FooRect.Top, S);
        end;
        if FShowText in [boCode, boBoth] then
        begin
          S := Self.Caption;
          StrWidth := TextWidth(S);
          StrHeight := TextHeight(S + ' ');
          FooRect.Left := (MarginalRect.Left + MarginalRect.Right - StrWidth) div 2;
          FooRect.Top := PaintRect.Bottom - StrHeight;
          FooRect.Right := FooRect.Left + StrWidth;
          FooRect.Bottom := FooRect.Top + StrHeight;
          FillRect(FooRect);
          TextRect(FooRect, FooRect.Left, FooRect.Top, S);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRLCustomBarcode.Paint;
var
  NormalImage, RotatedImage: TBitmap;
  OrientationAngle: Double;
  HorzOffset, VertOffset: Integer;
  ImageWidth, ImageHeight: Integer;
  SwapAux: Integer;
  MarginalRect: TRect;
begin
  CustomControlPaint;
  MarginalRect := ReduceRect(GetClientRect, CalcMarginalPixels);
  ImageWidth := RectWidth(MarginalRect);
  ImageHeight := RectHeight(MarginalRect);
  if FOrientation in [boBottomToTop, boTopToBottom] then
  begin
    SwapAux := ImageWidth;
    ImageWidth := ImageHeight;
    ImageHeight := SwapAux;
  end;
  if (ImageWidth > 0) and (ImageHeight > 0) then
  begin
    NormalImage := CreateBitmap(Caption, ImageWidth, ImageHeight);
    try
      case FOrientation of
        boBottomToTop: OrientationAngle := 90;
        boTopToBottom: OrientationAngle := -90;
      else // boLeftToRight
        OrientationAngle := 0;
      end;
      RotatedImage := RotatedBitmap(NormalImage, OrientationAngle);
      try
        case Alignment of
          taCenter: HorzOffset := (MarginalRect.Left + MarginalRect.Right - RotatedImage.Width) div 2;
          taRightJustify: HorzOffset := MarginalRect.Right - RotatedImage.Width;
        else
          HorzOffset := MarginalRect.Left;
        end;
        case Layout of
          tlCenter: VertOffset := (MarginalRect.Top + MarginalRect.Bottom - RotatedImage.Height) div 2;
          tlBottom: VertOffset := MarginalRect.Bottom - RotatedImage.Height;
        else
          VertOffset := MarginalRect.Top;
        end;
        Canvas.Draw(HorzOffset, VertOffset, RotatedImage);
      finally
        RotatedImage.Free;
      end;
    finally
      NormalImage.Free;
    end;
  end;
end;


procedure TRLCustomBarcode.InternalPrint;
var
  NormalImage, RotatedImage: TBitmap;
  OrientationAngle: Double;
  HorzOffset, VertOffset: Integer;
  ImageWidth, ImageHeight: Integer;
  SwapAux: Integer;
  MarginalRect: TRect;
begin
  inherited;
  MarginalRect := ReduceRect(CalcPrintClientRect, CalcMarginalPixels);
  ImageWidth := RectWidth(MarginalRect);
  ImageHeight := RectHeight(MarginalRect);
  if FOrientation in [boBottomToTop, boTopToBottom] then
  begin
    SwapAux := ImageWidth;
    ImageWidth := ImageHeight;
    ImageHeight := SwapAux;
  end;
  if (ImageWidth > 0) and (ImageHeight > 0) then
  begin
    NormalImage := CreateBitmap(Caption, ImageWidth, ImageHeight);
    try
      case FOrientation of
        boBottomToTop: OrientationAngle := 90;
        boTopToBottom: OrientationAngle := -90;
      else // boLeftToRight
        OrientationAngle := 0;
      end;
      RotatedImage := RotatedBitmap(NormalImage, OrientationAngle);
      try
        case Alignment of
          taCenter: HorzOffset := (MarginalRect.Left + MarginalRect.Right - RotatedImage.Width) div 2;
          taRightJustify: HorzOffset := MarginalRect.Right - RotatedImage.Width;
        else
          HorzOffset := MarginalRect.Left;
        end;
        case Layout of
          tlCenter: VertOffset := (MarginalRect.Top + MarginalRect.Bottom - RotatedImage.Height) div 2;
          tlBottom: VertOffset := MarginalRect.Bottom - RotatedImage.Height;
        else
          VertOffset := MarginalRect.Top;
        end;
        RequestParentSurface.Draw(HorzOffset, VertOffset, RotatedImage);
      finally
        RotatedImage.Free;
      end;
    finally
      NormalImage.Free;
    end;
  end;
end;

procedure TRLCustomBarcode.SetBarColor(const Value: TColor);
begin
  if FBarColor = Value then
    Exit;
  FBarColor := Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetShowText(const Value: TRLBarcodeTextOption);
begin
  if FShowText = Value then
    Exit;
  FShowText := Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetBarcodeType(const Value: TRLBarcodeType);
begin
  if FBarcodeType = Value then
    Exit;
  FBarcodeType := Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomBarcode.SetRatio(const Value: Double);
begin
  if FRatio = Value then
    Exit;
  FRatio := Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomBarcode.SetOrientation(const Value: TRLBarcodeOrientation);
var
  SwapAux: Integer;
begin
  if FOrientation = Value then
    Exit;
  if (Value in [boLeftToRight]) <> (FOrientation in [boLeftToRight]) then
  begin
    SwapAux := Width;
    Width := Height;
    Height := SwapAux;
  end;
  FOrientation := Value;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomBarcode.GetTypeText: string;
begin
  Result := BarcodeTypeInfo[FBarcodeType].Name;
end;

procedure TRLCustomBarcode.SetModule(Value: Integer);
begin
  if FModule = Value then
    Exit;
  if (Value < 1) or (Value >= 50) then
    Exit;
  FModule := Value;
  AdjustBounds;
  Invalidate;
end;

// calculate the width and the LineType of a sigle bar
procedure TRLCustomBarcode.GetBarInfo(AChar: Char; var ABarWidth: Integer;
  var ALineType: TRLBarcodeLineType);
begin
  {
  Code Color Width      Height
  -----------------------------------
  '0'  white 100%       full
  '1'  white 100%*Ratio full
  '2'  white 150%*Ratio full
  '3'  white 200%*Ratio full
  '5'  black 100%       full
  '6'  black 100%*Ratio full
  '7'  black 150%*Ratio full
  '8'  black 200%*Ratio full
  'A'  black 100%       2/5 (PostNet)
  'B'  black 100%*Ratio 2/5 (PostNet)
  'C'  black 150%*Ratio 2/5 (PostNet)
  'D'  black 200%*Ratio 2/5 (PostNet)
  }
  if CharInSet(AChar, ['0', '5', 'A']) then
    ABarWidth := FModules[bw100]
  else if CharInSet(AChar, ['1', '6', 'B']) then
    ABarWidth := FModules[bw100Ratio]
  else if CharInSet(AChar, ['2', '7', 'C']) then
    ABarWidth := FModules[bw150Ratio]
  else if CharInSet(AChar, ['3', '8', 'D']) then
    ABarWidth := FModules[bw200Ratio]
  else
    ABarWidth := FModules[bw100];
  if CharInSet(AChar, ['0', '1', '2', '3']) then
    ALineType := blFilled
  else if CharInSet(AChar, ['5', '6', '7', '8']) then
    ALineType := blNotFilled
  else if CharInSet(AChar, ['A', 'B', 'C', 'D']) then
    ALineType := blHalfFilled
  else
    ALineType := blNotFilled;
end;

procedure TRLCustomBarcode.MakeModules;
begin
  case BarcodeType of
    bcCode2OF5Interleaved, 
    bcCode2OF5Industry, 
    bcCode39, 
    bcEAN8, 
    bcEAN13, 
    bcCode39Extended, 
    bcCodaBar, 
    bcUPC_A, 
    bcUPC_E0, 
    bcUPC_E1, 
    bcUPC_Supp2, 
    bcUPC_Supp5: 
    if FRatio < 2 then
      FRatio := 2
    else if FRatio > 3 then
      FRatio := 3;
    bcCode2OF5Matrix: 
    if FRatio < 2.25 then
      FRatio := 2.25
    else if FRatio > 3 then
      FRatio := 3;
    bcCode128A, 
    bcCode128B, 
    bcCode128C, 
    bcCode93, 
    bcCode93Extended, 
    bcMSI, 
    bcPostNet: ;
  end;
  FModules[bw100] := FModule;
  FModules[bw100Ratio] := Round(FModule * FRatio);
  FModules[bw150Ratio] := FModules[bw100Ratio] * 3 div 2;
  FModules[bw200Ratio] := FModules[bw100Ratio] * 2;
end;

function TRLCustomBarcode.GetBarData(const AText: string): string;
var
  PureBarText: string;
  I: Integer;
begin
  FInvalid := False;
  // calculate the with of the different lines (fModules)
  MakeModules;
  // numeric barcode type?
  PureBarText := Caption;
  if BarcodeTypeInfo[BarcodeType].DigitsOnly then
  begin
    PureBarText := Trim(PureBarText);
    for I := 1 to Length(PureBarText) do
      if not CharInSet(PureBarText[I], ['0'..'9']) then
      begin
        PureBarText[I] := '0';
        FInvalid := True;
      end;
  end;
  // get the pattern of the barcode
  case BarcodeType of
    bcCode2of5Interleaved: Result := GetAs2OF5Interleaved(PureBarText);
    bcCode2OF5Industry: Result := GetAs2OF5Industry(PureBarText);
    bcCode2OF5Matrix: Result := GetAs2OF5Matrix(PureBarText);
    bcCode39: Result := GetAs39(PureBarText);
    bcCode39Extended: Result := GetAs39Extended(PureBarText);
    bcCode128A, 
    bcCode128B, 
    bcCode128C, 
    bcEAN128A, 
    bcEAN128B, 
    bcEAN128C: Result := GetAs128(PureBarText);
    bcCode93: Result := GetAs93(PureBarText);
    bcCode93Extended: Result := GetAs93Extended(PureBarText);
    bcMSI: Result := GetAsMSI(PureBarText);
    bcPostNet: Result := GetAsPostNet(PureBarText);
    bcCodaBar: Result := GetAsCodaBar(PureBarText);
    bcEAN8: Result := GetAsEAN8(PureBarText);
    bcEAN13: Result := GetAsEAN13(PureBarText);
    bcUPC_A: Result := GetAsUPC_A(PureBarText);
    bcUPC_E0: Result := GetAsUPC_E0(PureBarText);
    bcUPC_E1: Result := GetAsUPC_E1(PureBarText);
    bcUPC_Supp2: Result := GetAsUPC_Supp2(PureBarText);
    bcUPC_Supp5: Result := GetAsUPC_Supp5(PureBarText);
  else
    // raise Exception.Create(Self.Name+': Wrong barcode type');
    Result := '';
  end;
end;

function TRLCustomBarcode.GetImageWidth(const ABarData: string): Integer;
var
  LineType: TRLBarcodeLineType;
  BarWidth: Integer;
  I: Integer;
begin
  // examine the pattern string
  Result := 0;
  for I := 1 to Length(ABarData) do
  begin
    GetBarInfo(ABarData[I], BarWidth, LineType);
    Inc(Result, BarWidth);
  end;
end;

function TRLCustomBarcode.DoCheckSumming(const AData: string): string;
begin
  if FCheckSum then
    case FCheckSumMethod of
      cmModule10: Result := CheckSumModule10(AData);
    else
      Result := AData;
    end
  else
    Result := AData;
end;

// Pattern for Barcode EAN Charset A (L1   S1   L2   S2)
const
  Table_EAN_A: array['0'..'9'] of string = 
    (('2605'), // 0
     ('1615'), // 1
     ('1516'), // 2
     ('0805'), // 3
     ('0526'), // 4
     ('0625'), // 5
     ('0508'), // 6
     ('0706'), // 7
     ('0607'), // 8
     ('2506')); // 9

// Pattern for Barcode EAN Charset C (S1   L1   S2   L2)
const
  Table_EAN_C: array['0'..'9'] of string = 
    (('7150'), // 0
     ('6160'), // 1
     ('6061'), // 2
     ('5350'), // 3
     ('5071'), // 4
     ('5170'), // 5
     ('5053'), // 6
     ('5251'), // 7
     ('5152'), // 8
     ('7051')); // 9

function TRLCustomBarcode.GetAsEAN8(const AText: string): string;
var
  PureBarText: string;
  I: Integer;
begin
  if FCheckSum then
  begin
    PureBarText := PadZ(AText, 7);
    PureBarText := DoCheckSumming(Copy(PureBarText, Length(PureBarText) - 6, 7));
  end
  else
    PureBarText := PadZ(AText, 8);
  Result := '';
  if Length(PureBarText) = 8 then
  begin
    // startcode
    Result := '505';
    for I := 1 to 4 do
      Result := Result + Table_EAN_A[PureBarText[I]];
    // center guard pattern
    Result := Result + '05050';
    for I := 5 to 8 do
      Result := Result + Table_EAN_C[PureBarText[I]];
    // stopcode
    Result := Result + '505';
  end
  else
    FInvalid := True;
end;

// Pattern for Barcode EAN Zeichensatz B {L1   S1   L2   S2}
const
  Table_EAN_B: array['0'..'9'] of string = 
    (('0517'), // 0
     ('0616'), // 1
     ('1606'), // 2
     ('0535'), // 3
     ('1705'), // 4
     ('0715'), // 5
     ('3505'), // 6
     ('1525'), // 7
     ('2515'), // 8
     ('1507')); // 9

// Zuordung der Paraitaetsfolgen für EAN13
const
  Table_ParityEAN13: array[0..9, 1..6] of Char = 
    (('A', 'A', 'A', 'A', 'A', 'A'), // 0
     ('A', 'A', 'B', 'A', 'B', 'B'), // 1
     ('A', 'A', 'B', 'B', 'A', 'B'), // 2
     ('A', 'A', 'B', 'B', 'B', 'A'), // 3
     ('A', 'B', 'A', 'A', 'B', 'B'), // 4
     ('A', 'B', 'B', 'A', 'A', 'B'), // 5
     ('A', 'B', 'B', 'B', 'A', 'A'), // 6
     ('A', 'B', 'A', 'B', 'A', 'B'), // 7
     ('A', 'B', 'A', 'B', 'B', 'A'), // 8
     ('A', 'B', 'B', 'A', 'B', 'A')); // 9

function TRLCustomBarcode.GetAsEAN13(const AText: string): string;
var
  I, BarDigitAsc: Integer;
  PureBarText: string;
begin
  if FCheckSum then
  begin
    PureBarText := PadZ(AText, 12);
    PureBarText := DoCheckSumming(PureBarText);
  end
  else
    PureBarText := PadZ(AText, 13);
  Result := '';
  if Length(PureBarText) = 13 then
  begin
    BarDigitAsc := StrToInt(PureBarText[1]);
    PureBarText := Copy(PureBarText, 2, 12);
    // startcode
    Result := '505';
    for I := 1 to 6 do
      case Table_ParityEAN13[BarDigitAsc, I] of
        'A': Result := Result + Table_EAN_A[PureBarText[I]];
        'B': Result := Result + Table_EAN_B[PureBarText[I]];
        'C': Result := Result + Table_EAN_C[PureBarText[I]];
      end;
    // center guard pattern}
    Result := Result + '05050';
    for I := 7 to 12 do
      Result := Result + Table_EAN_C[PureBarText[I]];
    // stopcode
    Result := Result + '505';
  end
  else
    FInvalid := True;
end;

// pattern for barcode 2 of 5
const
  Table_2_5: array['0'..'9', 1..5] of Char = 
    (('0', '0', '1', '1', '0'), {'0'}
     ('1', '0', '0', '0', '1'), {'1'}
     ('0', '1', '0', '0', '1'), {'2'}
     ('1', '1', '0', '0', '0'), {'3'}
     ('0', '0', '1', '0', '1'), {'4'}
     ('1', '0', '1', '0', '0'), {'5'}
     ('0', '1', '1', '0', '0'), {'6'}
     ('0', '0', '0', '1', '1'), {'7'}
     ('1', '0', '0', '1', '0'), {'8'}
     ('0', '1', '0', '1', '0')); {'9'}

function TRLCustomBarcode.GetAs2OF5Interleaved(const AText: string): string;
var
  I, J: Integer;
  C: Char;
begin
  // startcode
  Result := '5050';
  for I := 1 to Length(AText) div 2 do
    for J := 1 to 5 do
    begin
      if Table_2_5[AText[I * 2 - 1], J] = '1' then
        C := '6'
      else
        C := '5';
      Result := Result + C;
      if Table_2_5[AText[I * 2], J] = '1' then
        C := '1'
      else
        C := '0';
      Result := Result + C;
    end;
  // stopcode
  Result := Result + '605';
end;

function TRLCustomBarcode.GetAs2OF5Industry(const AText: string): string;
var
  I, J: Integer;
begin
  // startcode
  Result := '606050';
  for I := 1 to Length(AText) do
    for J := 1 to 5 do
      if Table_2_5[AText[I], J] = '1' then
        Result := Result + '60'
      else
        Result := Result + '50';
  // stopcode
  Result := Result + '605060';
end;

function TRLCustomBarcode.GetAs2OF5Matrix(const AText: string): string;
var
  I, J: Integer;
  BarChar: Char;
begin
  // startcode
  Result := '705050';
  for I := 1 to Length(AText) do
  begin
    for J := 1 to 5 do
    begin
      if Table_2_5[AText[I], J] = '1' then
        BarChar := '1'
      else
        BarChar := '0';
      if Odd(J) then
        BarChar := Char(Ord(BarChar) + 5);
      Result := Result + BarChar;
    end;
    Result := Result + '0';
  end;
  // stopcode
  Result := Result + '70505';
end;


type
  TCode39 = record
    Ch: Char;
    Data: array[0..9] of Char;
    Checkup: ShortInt;
  end;
          
const
  Table_39: array[0..43] of TCode39 = (
     (Ch: '0'; Data: '505160605'; Checkup: 0), 
     (Ch: '1'; Data: '605150506'; Checkup: 1), 
     (Ch: '2'; Data: '506150506'; Checkup: 2), 
     (Ch: '3'; Data: '606150505'; Checkup: 3), 
     (Ch: '4'; Data: '505160506'; Checkup: 4), 
     (Ch: '5'; Data: '605160505'; Checkup: 5), 
     (Ch: '6'; Data: '506160505'; Checkup: 6), 
     (Ch: '7'; Data: '505150606'; Checkup: 7), 
     (Ch: '8'; Data: '605150605'; Checkup: 8), 
     (Ch: '9'; Data: '506150605'; Checkup: 9), 
     (Ch: 'A'; Data: '605051506'; Checkup: 10), 
     (Ch: 'B'; Data: '506051506'; Checkup: 11), 
     (Ch: 'C'; Data: '606051505'; Checkup: 12), 
     (Ch: 'D'; Data: '505061506'; Checkup: 13), 
     (Ch: 'E'; Data: '605061505'; Checkup: 14), 
     (Ch: 'F'; Data: '506061505'; Checkup: 15), 
     (Ch: 'G'; Data: '505051606'; Checkup: 16), 
     (Ch: 'H'; Data: '605051605'; Checkup: 17), 
     (Ch: 'I'; Data: '506051605'; Checkup: 18), 
     (Ch: 'J'; Data: '505061605'; Checkup: 19), 
     (Ch: 'K'; Data: '605050516'; Checkup: 20), 
     (Ch: 'L'; Data: '506050516'; Checkup: 21), 
     (Ch: 'M'; Data: '606050515'; Checkup: 22), 
     (Ch: 'N'; Data: '505060516'; Checkup: 23), 
     (Ch: 'O'; Data: '605060515'; Checkup: 24), 
     (Ch: 'P'; Data: '506060515'; Checkup: 25), 
     (Ch: 'Q'; Data: '505050616'; Checkup: 26), 
     (Ch: 'R'; Data: '605050615'; Checkup: 27), 
     (Ch: 'S'; Data: '506050615'; Checkup: 28), 
     (Ch: 'T'; Data: '505060615'; Checkup: 29), 
     (Ch: 'U'; Data: '615050506'; Checkup: 30), 
     (Ch: 'V'; Data: '516050506'; Checkup: 31), 
     (Ch: 'W'; Data: '616050505'; Checkup: 32), 
     (Ch: 'X'; Data: '515060506'; Checkup: 33), 
     (Ch: 'Y'; Data: '615060505'; Checkup: 34), 
     (Ch: 'Z'; Data: '516060505'; Checkup: 35), 
     (Ch: '-'; Data: '515050606'; Checkup: 36), 
     (Ch: '.'; Data: '615050605'; Checkup: 37), 
     (Ch: ' '; Data: '516050605'; Checkup: 38), 
     (Ch: '*'; Data: '515060605'; Checkup: 0), 
     (Ch: '$'; Data: '515151505'; Checkup: 39), 
     (Ch: '/'; Data: '515150515'; Checkup: 40), 
     (Ch: '+'; Data: '515051515'; Checkup: 41), 
     (Ch: '%'; Data: '505151515'; Checkup: 42));

function FindIdx(Z: Char): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Table_39) do
    if Table_39[I].Ch = Z then
    begin
      Result := I;
      Break;
    end;
end;

function TRLCustomBarcode.GetAs39(const AText: string): string;
var
  CheckSum, I, ChIndex: Integer;
begin
  CheckSum := 0;
  // startcode
  Result := string(Table_39[FindIdx('*')].Data) + '0';
  for I := 1 to Length(AText) do
  begin
    ChIndex := FindIdx(AText[I]);
    if ChIndex < 0 then
      FInvalid := True
    else
    begin
      Result := Result + Table_39[ChIndex].Data + '0';
      Inc(CheckSum, Table_39[ChIndex].Checkup);
    end;
  end;
  // calculate CheckSum Data
  if FCheckSum then
  begin
    CheckSum := CheckSum mod 43;
    for I := 0 to High(Table_39) do
      if CheckSum = Table_39[I].Checkup then
      begin
        Result := Result + Table_39[I].Data + '0';
        Break;
      end;
  end;
  // stopcode
  Result := Result + Table_39[FindIdx('*')].Data;
end;

const
  Code39x: array[0..127] of string[2] = (
    ('%U'), ('$A'), ('$B'), ('$C'), ('$D'), ('$E'), ('$F'), ('$G'), 
    ('$H'), ('$I'), ('$J'), ('$K'), ('$L'), ('$M'), ('$N'), ('$O'), 
    ('$P'), ('$Q'), ('$R'), ('$S'), ('$T'), ('$U'), ('$V'), ('$W'), 
    ('$X'), ('$Y'), ('$Z'), ('%A'), ('%B'), ('%C'), ('%D'), ('%E'), 
    (' '), ('/A'), ('/B'), ('/C'), ('/D'), ('/E'), ('/F'), ('/G'), 
    ('/H'), ('/I'), ('/J'), ('/K'), ('/L'), ('/M'), ('/N'), ('/O'), 
    ('0'), ('1'), ('2'), ('3'), ('4'), ('5'), ('6'), ('7'), 
    ('8'), ('9'), ('/Z'), ('%F'), ('%G'), ('%H'), ('%I'), ('%J'), 
    ('%V'), ('A'), ('B'), ('C'), ('D'), ('E'), ('F'), ('G'), 
    ('H'), ('I'), ('J'), ('K'), ('L'), ('M'), ('N'), ('O'), 
    ('P'), ('Q'), ('R'), ('S'), ('T'), ('U'), ('V'), ('W'), 
    ('X'), ('Y'), ('Z'), ('%K'), ('%L'), ('%M'), ('%N'), ('%O'), 
    ('%W'), ('+A'), ('+B'), ('+C'), ('+D'), ('+E'), ('+F'), ('+G'), 
    ('+H'), ('+I'), ('+J'), ('+K'), ('+L'), ('+M'), ('+N'), ('+O'), 
    ('+P'), ('+Q'), ('+R'), ('+S'), ('+T'), ('+U'), ('+V'), ('+W'), 
    ('+X'), ('+Y'), ('+Z'), ('%P'), ('%Q'), ('%R'), ('%S'), ('%T')
);
    
function TRLCustomBarcode.GetAs39Extended(const AText: string): string;
var
  FooStr: string;
  I: Integer;
begin
  FooStr := '';
  for I := 1 to Length(AText) do
    if Ord(AText[I]) <= 127 then
      FooStr := FooStr + Code39x[Ord(AText[I])]
    else
      FInvalid := True;
  Result := GetAs39(FooStr);
end;

function TRLCustomBarcode.GetAs128(const AText: string): string;
const
  StartA = '211412';
  StartB = '211214';
  StartC = '211232';
  Stop = '2331112';
type
  TCode128 = record
    A, B: Char;
    C: string[2];
    Data: string[6]; /// parada no refactoring
  end;
const
  Table_128: array[0..102] of TCode128 = (
    (A: ' '; B: ' '; C: '00'; Data: '212222'), 
    (A: '!'; B: '!'; C: '01'; Data: '222122'), 
    (A: '"'; B: '"'; C: '02'; Data: '222221'), 
    (A: '#'; B: '#'; C: '03'; Data: '121223'), 
    (A: '$'; B: '$'; C: '04'; Data: '121322'), 
    (A: '%'; B: '%'; C: '05'; Data: '131222'), 
    (A: '&'; B: '&'; C: '06'; Data: '122213'), 
    (A: ''''; B: ''''; C: '07'; Data: '122312'), 
    (A: '('; B: '('; C: '08'; Data: '132212'), 
    (A: ')'; B: ')'; C: '09'; Data: '221213'), 
    (A: '*'; B: '*'; C: '10'; Data: '221312'), 
    (A: '+'; B: '+'; C: '11'; Data: '231212'), 
    (A: '´'; B: '´'; C: '12'; Data: '112232'), 
    (A: '-'; B: '-'; C: '13'; Data: '122132'), 
    (A: '.'; B: '.'; C: '14'; Data: '122231'), 
    (A: '/'; B: '/'; C: '15'; Data: '113222'), 
    (A: '0'; B: '0'; C: '16'; Data: '123122'), 
    (A: '1'; B: '1'; C: '17'; Data: '123221'), 
    (A: '2'; B: '2'; C: '18'; Data: '223211'), 
    (A: '3'; B: '3'; C: '19'; Data: '221132'), 
    (A: '4'; B: '4'; C: '20'; Data: '221231'), 
    (A: '5'; B: '5'; C: '21'; Data: '213212'), 
    (A: '6'; B: '6'; C: '22'; Data: '223112'), 
    (A: '7'; B: '7'; C: '23'; Data: '312131'), 
    (A: '8'; B: '8'; C: '24'; Data: '311222'), 
    (A: '9'; B: '9'; C: '25'; Data: '321122'), 
    (A: ':'; B: ':'; C: '26'; Data: '321221'), 
    (A: ';'; B: ';'; C: '27'; Data: '312212'), 
    (A: '<'; B: '<'; C: '28'; Data: '322112'), 
    (A: '='; B: '='; C: '29'; Data: '322211'), 
    (A: '>'; B: '>'; C: '30'; Data: '212123'), 
    (A: '?'; B: '?'; C: '31'; Data: '212321'), 
    (A: '@'; B: '@'; C: '32'; Data: '232121'), 
    (A: 'A'; B: 'A'; C: '33'; Data: '111323'), 
    (A: 'B'; B: 'B'; C: '34'; Data: '131123'), 
    (A: 'C'; B: 'C'; C: '35'; Data: '131321'), 
    (A: 'D'; B: 'D'; C: '36'; Data: '112313'), 
    (A: 'E'; B: 'E'; C: '37'; Data: '132113'), 
    (A: 'F'; B: 'F'; C: '38'; Data: '132311'), 
    (A: 'G'; B: 'G'; C: '39'; Data: '211313'), 
    (A: 'H'; B: 'H'; C: '40'; Data: '231113'), 
    (A: 'I'; B: 'I'; C: '41'; Data: '231311'), 
    (A: 'J'; B: 'J'; C: '42'; Data: '112133'), 
    (A: 'K'; B: 'K'; C: '43'; Data: '112331'), 
    (A: 'L'; B: 'L'; C: '44'; Data: '132131'), 
    (A: 'M'; B: 'M'; C: '45'; Data: '113123'), 
    (A: 'N'; B: 'N'; C: '46'; Data: '113321'), 
    (A: 'O'; B: 'O'; C: '47'; Data: '133121'), 
    (A: 'P'; B: 'P'; C: '48'; Data: '313121'), 
    (A: 'Q'; B: 'Q'; C: '49'; Data: '211331'), 
    (A: 'R'; B: 'R'; C: '50'; Data: '231131'), 
    (A: 'S'; B: 'S'; C: '51'; Data: '213113'), 
    (A: 'T'; B: 'T'; C: '52'; Data: '213311'), 
    (A: 'U'; B: 'U'; C: '53'; Data: '213131'), 
    (A: 'V'; B: 'V'; C: '54'; Data: '311123'), 
    (A: 'W'; B: 'W'; C: '55'; Data: '311321'), 
    (A: 'X'; B: 'X'; C: '56'; Data: '331121'), 
    (A: 'Y'; B: 'Y'; C: '57'; Data: '312113'), 
    (A: 'Z'; B: 'Z'; C: '58'; Data: '312311'), 
    (A: '['; B: '['; C: '59'; Data: '332111'), 
    (A: '\'; B: '\'; C: '60'; Data: '314111'), 
    (A: ']'; B: ']'; C: '61'; Data: '221411'), 
    (A: '^'; B: '^'; C: '62'; Data: '431111'), 
    (A: '_'; B: '_'; C: '63'; Data: '111224'), 
    (A: ' '; B: '`'; C: '64'; Data: '111422'), 
    (A: ' '; B: 'a'; C: '65'; Data: '121124'), 
    (A: ' '; B: 'b'; C: '66'; Data: '121421'), 
    (A: ' '; B: 'c'; C: '67'; Data: '141122'), 
    (A: ' '; B: 'd'; C: '68'; Data: '141221'), 
    (A: ' '; B: 'e'; C: '69'; Data: '112214'), 
    (A: ' '; B: 'f'; C: '70'; Data: '112412'), 
    (A: ' '; B: 'g'; C: '71'; Data: '122114'), 
    (A: ' '; B: 'h'; C: '72'; Data: '122411'), 
    (A: ' '; B: 'i'; C: '73'; Data: '142112'), 
    (A: ' '; B: 'j'; C: '74'; Data: '142211'), 
    (A: ' '; B: 'k'; C: '75'; Data: '241211'), 
    (A: ' '; B: 'l'; C: '76'; Data: '221114'), 
    (A: ' '; B: 'm'; C: '77'; Data: '413111'), 
    (A: ' '; B: 'n'; C: '78'; Data: '241112'), 
    (A: ' '; B: 'o'; C: '79'; Data: '134111'), 
    (A: ' '; B: 'p'; C: '80'; Data: '111242'), 
    (A: ' '; B: 'q'; C: '81'; Data: '121142'), 
    (A: ' '; B: 'r'; C: '82'; Data: '121241'), 
    (A: ' '; B: 's'; C: '83'; Data: '114212'), 
    (A: ' '; B: 't'; C: '84'; Data: '124112'), 
    (A: ' '; B: 'u'; C: '85'; Data: '124211'), 
    (A: ' '; B: 'v'; C: '86'; Data: '411212'), 
    (A: ' '; B: 'w'; C: '87'; Data: '421112'), 
    (A: ' '; B: 'x'; C: '88'; Data: '421211'), 
    (A: ' '; B: 'y'; C: '89'; Data: '212141'), 
    (A: ' '; B: 'z'; C: '90'; Data: '214121'), 
    (A: ' '; B: '{'; C: '91'; Data: '412121'), 
    (A: ' '; B: '|'; C: '92'; Data: '111143'), 
    (A: ' '; B: '}'; C: '93'; Data: '111341'), 
    (A: ' '; B: '~'; C: '94'; Data: '131141'), 
    (A: ' '; B: ' '; C: '95'; Data: '114113'), 
    (A: ' '; B: ' '; C: '96'; Data: '114311'), 
    (A: ' '; B: ' '; C: '97'; Data: '411113'), 
    (A: ' '; B: ' '; C: '98'; Data: '411311'), 
    (A: ' '; B: ' '; C: '99'; Data: '113141'), 
    (A: ' '; B: ' '; C: '  '; Data: '114131'), 
    (A: ' '; B: ' '; C: '  '; Data: '311141'), 
    (A: ' '; B: ' '; C: '  '; Data: '411131'));
  // find code 128 codeset a or b
  function Find_Code128AB(C: Char): Integer;
  var
    I: Integer;
    V: Char;
  begin
    Result := -1;
    for I := 0 to High(Table_128) do
    begin
      if FBarcodeType = bcCode128A then
        V := Table_128[I].A
      else
        V := Table_128[I].B;
      if C = V then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
  // find code 128 codeset c
  function Find_Code128C(C: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to High(Table_128) do
      if Table_128[I].C = C then
      begin
        Result := I;
        Break;
      end;
  end;
var
  I, J, idx: Integer;
  startcode: string;
  checksum: Integer;
  codeword_pos: Integer;
  text: string;
begin
  text := AText;
  case FBarcodeType of
    bcCode128A,
    bcEAN128A: begin
                 checksum := 103;
                 startcode := StartA;
               end;
    bcCode128B,
    bcEAN128B: begin
                 checksum := 104;
                 startcode := StartB;
               end;
    bcCode128C,
    bcEAN128C: begin
                 checksum := 105;
                 startcode := StartC;
               end;
  else
    checksum := 0;
  end;

  // startcode
  Result := Convert(startcode);
  codeword_pos := 1;

  case FBarcodeType of
    bcEAN128A, 
    bcEAN128B, 
    bcEAN128C: begin // special identifier FNC1 = function code 1 for EAN 128 barcodes
                 Result := Result + Convert(Table_128[102].Data);
                 Inc(checksum, 102 * codeword_pos);
                 Inc(codeword_pos);
                 // if there is no checksum at the end of the string the EAN128 needs one (modulo 10)
                 if FCheckSum then
                   text := DoCheckSumming(text);
               end;
  end;
  if (FBarcodeType = bcCode128C) or (FBarcodeType = bcEAN128C) then
  begin
    if Length(text) mod 2 <> 0 then
      text := '0' + text;
    for I := 1 to Length(text) div 2 do
    begin
      J := (I - 1) * 2 + 1;
      idx := Find_Code128C(Copy(text, J, 2));
      if idx < 0 then
        idx := Find_Code128C('00');
      Result := Result + Convert(Table_128[idx].Data);
      Inc(checksum, idx * codeword_pos);
      Inc(codeword_pos);
    end;
  end
  else
    for I := 1 to Length(text) do
    begin
      idx := Find_Code128AB(text[I]);
      if idx < 0 then
        idx := Find_Code128AB(' ');
      Result := Result + Convert(Table_128[idx].Data);
      Inc(checksum, idx * codeword_pos);
      Inc(codeword_pos);
    end;
  checksum := checksum mod 103;
  Result := Result + Convert(Table_128[checksum].Data);
  // stopcode
  Result := Result + Convert(Stop);
end;

type
  TCode93 = record
            C: Char;
            Data: array[0..5] of Char;
          end;

const
  Table_93: array[0..46] of TCode93 = (
    (C: '0'; Data: '131112'), 
    (C: '1'; Data: '111213'), 
    (C: '2'; Data: '111312'), 
    (C: '3'; Data: '111411'), 
    (C: '4'; Data: '121113'), 
    (C: '5'; Data: '121212'), 
    (C: '6'; Data: '121311'), 
    (C: '7'; Data: '111114'), 
    (C: '8'; Data: '131211'), 
    (C: '9'; Data: '141111'), 
    (C: 'A'; Data: '211113'), 
    (C: 'B'; Data: '211212'), 
    (C: 'C'; Data: '211311'), 
    (C: 'D'; Data: '221112'), 
    (C: 'E'; Data: '221211'), 
    (C: 'F'; Data: '231111'), 
    (C: 'G'; Data: '112113'), 
    (C: 'H'; Data: '112212'), 
    (C: 'I'; Data: '112311'), 
    (C: 'J'; Data: '122112'), 
    (C: 'K'; Data: '132111'), 
    (C: 'L'; Data: '111123'), 
    (C: 'M'; Data: '111222'), 
    (C: 'N'; Data: '111321'), 
    (C: 'O'; Data: '121122'), 
    (C: 'P'; Data: '131121'), 
    (C: 'Q'; Data: '212112'), 
    (C: 'R'; Data: '212211'), 
    (C: 'S'; Data: '211122'), 
    (C: 'T'; Data: '211221'), 
    (C: 'U'; Data: '221121'), 
    (C: 'V'; Data: '222111'), 
    (C: 'W'; Data: '112122'), 
    (C: 'X'; Data: '112221'), 
    (C: 'Y'; Data: '122121'), 
    (C: 'Z'; Data: '123111'), 
    (C: '-'; Data: '121131'), 
    (C: '.'; Data: '311112'), 
    (C: ' '; Data: '311211'), 
    (C: '$'; Data: '321111'), 
    (C: '/'; Data: '112131'), 
    (C: '+'; Data: '113121'), 
    (C: '%'; Data: '211131'), 
    (C: '['; Data: '121221'), {only used for Extended Code 93}
    (C: ']'; Data: '312111'), {only used for Extended Code 93}
    (C: '{'; Data: '311121'), {only used for Extended Code 93}
    (C: '}'; Data: '122211')); {only used for Extended Code 93}

function FindCode93(C: Char): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Table_93) do
    if C = Table_93[I].C then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TRLCustomBarcode.GetAs93(const AText: string): string;
var
  checkC, checkK, weightC, weightK, I, idx: Integer;
begin
  // startcode
  Result := Convert('111141');
  for I := 1 to Length(AText) do
  begin
    idx := FindCode93(AText[I]);
    if idx < 0 then
      FInvalid := True
    else
      Result := Result + Convert(Table_93[idx].Data);
  end;

  checkC := 0;
  checkK := 0;
  weightC := 1;
  weightK := 2;

  for I := Length(AText) downto 1 do
  begin
    idx := FindCode93(AText[I]);
    Inc(checkC, idx * weightC);
    Inc(checkK, idx * weightK);
    Inc(weightC);
    if weightC > 20 then
      weightC := 1;
    Inc(weightK);
    if weightK > 15 then
      weightC := 1;
  end;

  Inc(checkK, checkC);
  checkC := checkC mod 47;
  checkK := checkK mod 47;

  Result := Result + Convert(Table_93[checkC].Data) + 
  Convert(Table_93[checkK].Data);
  // stopcode
  Result := Result + Convert('1111411');
end;

const
  Code93x: array[0..127] of string[2] = (
    (']U'), ('[A'), ('[B'), ('[C'), ('[D'), ('[E'), ('[F'), ('[G'), 
    ('[H'), ('[I'), ('[J'), ('[K'), ('[L'), ('[M'), ('[N'), ('[O'), 
    ('[P'), ('[Q'), ('[R'), ('[S'), ('[T'), ('[U'), ('[V'), ('[W'), 
    ('[X'), ('[Y'), ('[Z'), (']A'), (']B'), (']C'), (']D'), (']E'), 
    (' '), ('{A'), ('{B'), ('{C'), ('{D'), ('{E'), ('{F'), ('{G'), 
    ('{H'), ('{I'), ('{J'), ('{K'), ('{L'), ('{M'), ('{N'), ('{O'), 
    ('0'), ('1'), ('2'), ('3'), ('4'), ('5'), ('6'), ('7'), 
    ('8'), ('9'), ('{Z'), (']F'), (']G'), (']H'), (']I'), (']J'), 
    (']V'), ('A'), ('B'), ('C'), ('D'), ('E'), ('F'), ('G'), 
    ('H'), ('I'), ('J'), ('K'), ('L'), ('M'), ('N'), ('O'), 
    ('P'), ('Q'), ('R'), ('S'), ('T'), ('U'), ('V'), ('W'), 
    ('X'), ('Y'), ('Z'), (']K'), (']L'), (']M'), (']N'), (']O'), 
    (']W'), ('}A'), ('}B'), ('}C'), ('}D'), ('}E'), ('}F'), ('}G'), 
    ('}H'), ('}I'), ('}J'), ('}K'), ('}L'), ('}M'), ('}N'), ('}O'), 
    ('}P'), ('}Q'), ('}R'), ('}S'), ('}T'), ('}U'), ('}V'), ('}W'), 
    ('}X'), ('}Y'), ('}Z'), (']P'), (']Q'), (']R'), (']S'), (']T')
);
    
function TRLCustomBarcode.GetAs93Extended(const AText: string): string;
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to Length(AText) - 1 do
    if Ord(AText[I]) <= 127 then
      S := S + Code93x[Ord(AText[I])]
    else
      FInvalid := True;
  Result := GetAs93(S);
end;

const
  Table_MSI: array['0'..'9'] of string[8] = (
    ('51515151'), {'0'}
    ('51515160'), {'1'}
    ('51516051'), {'2'}
    ('51516060'), {'3'}
    ('51605151'), {'4'}
    ('51605160'), {'5'}
    ('51606051'), {'6'}
    ('51606060'), {'7'}
    ('60515151'), {'8'}
    ('60515160') {'9'}
);

function Quersumme(X: Integer): Integer;
begin
  Result := 0;
  while X > 0 do
  begin
    Inc(Result, X mod 10);
    X := X div 10;
  end;
end;

function TRLCustomBarcode.GetAsMSI(const AText: string): string;
var
  check_even, check_odd, checksum: Integer;
  I: Integer;
begin
  // startcode
  Result := '60';
  check_even := 0;
  check_odd := 0;

  for I := 1 to Length(AText) do
  begin
    if Odd(I - 1) then
      check_odd := check_odd * 10 + Ord(AText[I])
    else
      check_even := check_even + Ord(AText[I]);
    Result := Result + Table_MSI[AText[I]];
  end;

  checksum := Quersumme(check_odd * 2) + check_even;

  checksum := checksum mod 10;
  if checksum > 0 then
    checksum := 10 - checksum;

  Result := Result + Table_MSI[Char(Ord('0') + checksum)];
  // stopcode
  Result := Result + '515';
end;

const
  Table_PostNet: array['0'..'9'] of string[10] = (
    ('5151A1A1A1'), {'0'}
    ('A1A1A15151'), {'1'}
    ('A1A151A151'), {'2'}
    ('A1A15151A1'), {'3'}
    ('A151A1A151'), {'4'}
    ('A151A151A1'), {'5'}
    ('A15151A1A1'), {'6'}
    ('51A1A1A151'), {'7'}
    ('51A1A151A1'), {'8'}
    ('51A151A1A1') {'9'}
);
    
function TRLCustomBarcode.GetAsPostNet(const AText: string): string;
var
  I: Integer;
begin
  Result := '51';
  for I := 1 to Length(AText) do
    Result := Result + Table_PostNet[AText[I]];
  Result := Result + '5';
end;

type
  TCodabar = record
             C: Char;
             Data: array[0..6] of Char;
           end;
           
const
  Table_cb: array[0..19] of TCodabar = (
    (C: '1'; Data: '5050615'), 
    (C: '2'; Data: '5051506'), 
    (C: '3'; Data: '6150505'), 
    (C: '4'; Data: '5060515'), 
    (C: '5'; Data: '6050515'), 
    (C: '6'; Data: '5150506'), 
    (C: '7'; Data: '5150605'), 
    (C: '8'; Data: '5160505'), 
    (C: '9'; Data: '6051505'), 
    (C: '0'; Data: '5050516'), 
    (C: '-'; Data: '5051605'), 
    (C: '$'; Data: '5061505'), 
    (C: ':'; Data: '6050606'), 
    (C: '/'; Data: '6060506'), 
    (C: '.'; Data: '6060605'), 
    (C: '+'; Data: '5060606'), 
    (C: 'A'; Data: '5061515'), 
    (C: 'B'; Data: '5151506'), 
    (C: 'C'; Data: '5051516'), 
    (C: 'D'; Data: '5051615')
);
    
function FindCodabar(C: Char): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Table_cb) do
    if C = Table_cb[I].C then
    begin
      Result := I;
      Break;
    end;
end;

function TRLCustomBarcode.GetAsCodaBar(const AText: string): string;
var
  I, idx: Integer;
begin
  Result := string(Table_cb[FindCodabar('A')].Data) + '0';
  for I := 1 to Length(AText) do
  begin
    idx := FindCodabar(AText[I]);
    if idx < 0 then
      FInvalid := True
    else
      Result := Result + Table_cb[idx].Data + '0';
  end;
  Result := Result + Table_cb[FindCodabar('B')].Data;
end;

function TRLCustomBarcode.GetAsUPC_A(const AText: string): string;
var
  tmp: string;
  I: Integer;
begin
  tmp := PadZ(AText, 12);
  if FCheckSum then
    tmp := DoCheckSumming(Copy(tmp, 1, 11));
  // startcode
  Result := '505';
  for I := 1 to 6 do
    Result := Result + Table_EAN_A[tmp[I]];
  Result := Result + '05050'; {Trennzeichen}
  for I := 7 to 12 do
    Result := Result + Table_EAN_C[tmp[I]];
  // stopcode
  Result := Result + '505';
end;

{UPC E Parity Pattern Table, Number System 0}
const
  Table_UPC_E0: array['0'..'9', 1..6] of Char = (
    ('E', 'E', 'E', 'o', 'o', 'o'), { 0 }
    ('E', 'E', 'o', 'E', 'o', 'o'), { 1 }
    ('E', 'E', 'o', 'o', 'E', 'o'), { 2 }
    ('E', 'E', 'o', 'o', 'o', 'E'), { 3 }
    ('E', 'o', 'E', 'E', 'o', 'o'), { 4 }
    ('E', 'o', 'o', 'E', 'E', 'o'), { 5 }
    ('E', 'o', 'o', 'o', 'E', 'E'), { 6 }
    ('E', 'o', 'E', 'o', 'E', 'o'), { 7 }
    ('E', 'o', 'E', 'o', 'o', 'E'), { 8 }
    ('E', 'o', 'o', 'E', 'o', 'E') { 9 }
);
function TRLCustomBarcode.GetAsUPC_E0(const AText: string): string;
var
  I, J: Integer;
  tmp: string;
  C: Char;
begin
  tmp := PadZ(AText, 7);
  tmp := DoCheckSumming(Copy(tmp, 1, 6));
  C := tmp[7];
  // startcode
  Result := '505';
  for I := 1 to 6 do
    if Table_UPC_E0[C, I] = 'E' then
      for J := 1 to 4 do
        Result := Result + Table_EAN_C[tmp[I], 5 - J]
    else
      Result := Result + Table_EAN_A[tmp[I]];
  // stopcode
  Result := Result + '05050';
end;

function TRLCustomBarcode.GetAsUPC_E1(const AText: string): string;
var
  I, J: Integer;
  tmp: string;
  C: Char;
begin
  tmp := PadZ(AText, 7);
  tmp := DoCheckSumming(Copy(tmp, 1, 6));
  C := tmp[7];
  // startcode
  Result := '505';
  for I := 1 to 6 do
    if Table_UPC_E0[C, I] = 'E' then
      Result := Result + Table_EAN_A[tmp[I]]
    else
      for J := 1 to 4 do
        Result := Result + Table_EAN_C[tmp[I], 5 - J];
  // stopcode
  Result := Result + '05050';
end;

function GetSupp(const ANumber: string): string;
var
  I, InverseI, Sum: Integer;
  tmp: string;
begin
  Sum := 0;
  tmp := Copy(ANumber, 1, Length(ANumber) - 1);
  InverseI := Length(tmp);
  for I := 1 to Length(tmp) do
  begin
    if (InverseI mod 2) = 0 then
      Sum := Sum + (StrToInt(tmp[I]) * 9)
    else
      Sum := Sum + (StrToInt(tmp[I]) * 3);
    Dec(InverseI);
  end;
  Sum := ((Sum mod 10) mod 10) mod 10;
  Result := tmp + IntToStr(Sum);
end;

function TRLCustomBarcode.GetAsUPC_Supp5(const AText: string): string;
var
  I, J: Integer;
  tmp: string;
  C: Char;
begin
  tmp := PadZ(AText, 5);
  tmp := GetSupp(Copy(tmp, 1, 5) + '0');
  C := tmp[6];
  // startcode
  Result := '506';
  for I := 1 to 5 do
  begin
    if Table_UPC_E0[C, (6 - 5) + I] = 'E' then
      for J := 1 to 4 do
        Result := Result + Table_EAN_C[tmp[I], 5 - J]
    else
      Result := Result + Table_EAN_A[tmp[I]];
    // character delineator
    if I < 5 then
      Result := Result + '05';
  end;
end;

function TRLCustomBarcode.GetAsUPC_Supp2(const AText: string): string;
var
  tmp, mS: string;
  I, J: Integer;
begin
  tmp := PadZ(AText, 2);
  I := StrToInt(tmp);
  case I mod 4 of
    3: mS := 'EE';
    2: mS := 'Eo';
    1: mS := 'oE';
    0: mS := 'oo';
  end;
  tmp := GetSupp(Copy(tmp, 1, 5) + '0');
  // startcode
  Result := '506';
  for I := 1 to 2 do
  begin
    if mS[I] = 'E' then
      for J := 1 to 4 do
        Result := Result + Table_EAN_C[tmp[I], 5 - J]
    else
      Result := Result + Table_EAN_A[tmp[I]];
    if I < 2 then
      Result := Result + '05'; // character delineator
  end;
end;

procedure TRLCustomBarcode.SetMargins(const AValue: TRLMargins);
begin
  FMargins.Assign(AValue);
  Invalidate;
end;

procedure TRLCustomBarcode.SetInvalidCode(const Value: TRLBarcodeInvalidCode);
begin
  if Value = FInvalidCode then
    Exit;
  FInvalidCode := Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetCheckSum(const Value: Boolean);
begin
  if Value = FCheckSum then
    Exit;
  AdjustBounds;
  Invalidate;
  FCheckSum := Value;
end;

procedure TRLCustomBarcode.SetCheckSumMethod(
  const Value: TRLBarcodeCheckSumMethod);
begin
  if Value = FCheckSumMethod then
    Exit;
  AdjustBounds;
  Invalidate;
  FCheckSumMethod := Value;
end;

{ TRLCustomDBBarcode }

constructor TRLCustomDBBarcode.Create(AOwner: TComponent);
begin
  FDataField := '';
  FDataFormula := '';
  FDataSource := nil;
  inherited Create(AOwner);
end;

procedure TRLCustomDBBarcode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil;
end;

procedure TRLCustomDBBarcode.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDataSource then
    Exit;
  FDataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBBarcode.SetDataField(const AValue: TRLDataFieldProperty);
begin
  if AValue = FDataField then
    Exit;
  if AValue <> '' then
    FDataFormula := '';
  FDataField := AValue;
  MakeCaption;
end;

procedure TRLCustomDBBarcode.SetDataFormula(const AValue: string);
begin
  if AValue = FDataFormula then
    Exit;
  if AValue <> '' then
    FDataField := '';
  FDataFormula := AValue;
  MakeCaption;
end;

function TRLCustomDBBarcode.GetDataSet: TDataSet;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

function TRLCustomDBBarcode.GetField: TField;
begin
  if (DataSet <> nil) and (FDataField <> '') then
  begin
    Result := DataSet.FindField(FDataField);
    if Result = nil then
      raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr + ': ' + Name + '.DataField "' + FDataField + '"'));
  end
  else
    Result := nil;
end;

function TRLCustomDBBarcode.GetFieldText: string;
var
  D: TDataSet;
  F: TField;
  P: TRLCustomReport;
begin
  P := FindParentReport;
  if not IsPreparing then
    if FriendlyName <> '' then
      Result := FriendlyName
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

function TRLCustomDBBarcode.InternalMakeCaption: string;
begin
  Result := GetFieldText;
end;

function TRLCustomDBBarcode.GetFieldLabel: string;
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

end.

