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

{@unit RLDraftFilter - Implementação do filtro de impressão draft. }
unit RLDraftFilter;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  SysUtils, Classes, Math, Contnrs,
  {$IfDef FPC}
   LCLIntf, LCLType, IntfGraphics, FPImage, FileUtil, Process,
   {$IfDef MSWINDOWS} WinUtilPrn, {$EndIf}
  {$Else}
   WinSpool, ShellApi,
  {$EndIf}
  {$IfDef CLX}
   QGraphics, RLMetaCLX,
  {$Else}
   Graphics,
   RLMetaVCL,
   {$IfNDef FPC}
    RlCompilerConsts,
   {$EndIf}
  {$EndIf}
  RLMetaFile, RLConsts, RLUtils, RLFilters, RLTypes, RLPrinters;

type
  {@type TRLDraftAccentMethod - Comportamento do filtro em relação aos caracteres acentuados.
   Algumas impressoras não suportam ou podem não estar adequadamente configuradas para imprimir
   caracteres acentuados.
   O método de acentuação do filtro pode ser:
   amOverwrite - O filtro provoca o retorno do carro para imprimir o caractere de acento sobre a letra;
   amTakeOut - O caractere é impresso sem o acento;
   amSustain - O caractere é enviado sem modificações (exige configuração da impressora). :/}
  TRLDraftAccentMethod = (amOverwrite, amTakeOut, amSustain);

  {@type TRLDraftEjectMethod - Comportamento do filtro em relação ao salto de página.
   Quando se trabalha com um tamanho específico de formulário, pode ser necessário modificar a maneira
   como o filtro efetua os saltos de página.
   O método de salto do filtro pode ser:
   ejCompletePage - Envia códigos de salto de linha até completar a página. A distância entre os páginas pode
   dilatar ou contrair ao longo da impressão;
   ejForceWithCode - Envia código de salto de página para a impressora. Este é o melhor método para formulários
   contínuos padrão, porém pode não servir para formulários de tamanho customizado;
   ejLeavePage - Deixa o carro da impressora na posição aonde parou. Pode ser utilizado com formulários sem picote
   como rolos por ex, ou quando se deseja economizar papel com testes. :/}
  TRLDraftEjectMethod = (ejCompletePage, ejForceWithCode, ejLeavePage);

  {@type TRLPrinterFamily - Família de impressoras.
   Além dos códigos de programação, pode ser interessante conhecer algumas particularidades da impressora a
   ser utilizada e que não podem ser informadas somente com comandos, como a impressão de gráficos, por exemplo.
   A família pode ser:
   fmCustom - Códigos de programação informados pelo usuário na prop Commands;
   fmEpsonLike - Códigos de programação da especificação EPSON/FX;
   fmESCP2 - Códigos de programação da especificação EPSON ESC/P2. :/}
  TRLPrinterFamily = (fmCustom, fmEpsonLike, fmESCP2);

  {@type TRLDeviceKind - Tipo de dispositivo de impressão.
   Indica como o relatório deve ser despachado.
   Pode assumir um dos seguintes valores:
   dkPrinter - Utilizar o nome da impressora;
   dkPrinterPort - A prop DevicePath será preenchida em runtime de acordo com a impressora selecionada no
   diálogo de impressão. Este é o padrão mais recomendado para o Windows, pois o próprio sistema se
   encarregará de descobrir o caminho para o dispositivo através de informações do SO (default);
   dkProgram - A prop DevicePath indica o nome de um programa spooler. Padrão recomendado para o Linux;
   dkFileName - A prop DevicePath aponta para um nome de arquivo. Este arquivo poderá ser copiado para
   outra impressora, ou ser utilizado como debug. :/}
  TRLDeviceKind = (dkPrinter, dkPrinterPort, dkProgram, dkFileName);

  {@type TRLDitheringMethod - Método para impressão de imagens.
   Esta propriedade indica que técnica deve ser utilizada para transformar imagens coloridas em pontos preto e branco.
   Pode ser um dos seguintes valores:
   dmNone - Indica que nenhuma imagem será impressa;
   dmColorTable - Tabela de associação de cores. Este método geralmente apresenta os melhores resultados;
   dmErrorDiffusion - Difusão de erros. :/}
  TRLDitheringMethod = (dmNone, dmColorTable, dmErrorDiffusion);

  {@type TRLLineDrawMethod - Método para impressão de linhas e traços.
   Para imprimir linhas retas em modo texto, é necessário informar o conjunto de caracteres a utilizar.
   Pode ser um dos seguintes valores:
   ldNone - Nenhuma linha ou traço será impresso;
   ldMinusAndPipe - Linhas horizontais como sinal negativo "-" e verticais como pipes "|";
   ldGraphicCharset - Utilizar os conectores gráficos do padrão ProPrinter (exige configuração da impressora). :/}
  TRLLineDrawMethod = (ldNone, ldMinusAndPipe, ldGraphicCharset);

  {@type TRLFillArtMethod - Método para preenchimento de áreas.
   Define o método para representação de retângulos sólidos ou linhas grossas em impressoras matriciais.
   Pode ser um dos seguintes valores:
   fmNone - Nenhum preenchimento será impresso;
   fmLetterX - A letra X será utilizada ;
   fmGraphicCharset - Utilizar os caracteres gráficos do padrão ProPrinter (exige configuração da impressora). :/}
  TRLFillArtMethod = (fmNone, fmLetterX, fmGraphicCharset);

  {@type TRLFormSelection - Seleção da largura do formulário contínuo.
   Indica como será escolhido formulário em relação à largura.  
   Pode ser um dos seguintes valores:
   fsNone - Nenhuma adaptação é feita e nenhum diálogo é exibido;
   fsAccordingToOrientation - O diálogo apresentará as opções de 80cols para Portrait e 132cols para Landscape,
   e o default será de acordo com a orientação do relatório;
   fs80Cols - O default será o formulário de 80cols;
   fs132Cols - O default será o formulário de 132cols. :/}
  TRLFormSelection = (fsNone, fsAccordingToOrientation, fs80Cols, fs132Cols);

  {@type TRLStretchCharWidth - Método de adaptação do tamanho das fontes para o formulário escolhido.
   Pode ser um dos seguintes valores:
   scNone - Nenhuma adaptação será feita;
   scEnlargementsOnly - A fonte deverá ser aumentada quando o formulário for maior;
   scShrinksOnly - A fonte deverá ser encolhida quando o formulário for menor;
   scAlways - A fonte deverá ser aumentada ou encolhida de acordo com a escolha do formulário. :/}
  TRLStretchCharWidth = (scNone, scEnlargementsOnly, scShrinksOnly, scAlways);

  {@type TRLDraftTextDecoration - Define como os efeitos de fonte serão implementados.
   Pode ser um dos seguintes valores:
   ddIncludeNone - Nenhum efeito é realizado;
   ddIncludeAll - Todos os efeitos são realizados;
   ddCustomized - Somente os efeitos indicados na prop TextStyles. :/}
  TRLDraftTextDecoration = (ddIncludeNone, ddIncludeAll, ddCustomized);

  {@type TRLDraftTextStyles - Indica que efeitos de fonte devem ser realizados ou ignorados pelo filtro.
   Pode ser nenhum ou uma combinação dos seguintes valores:
   tsItalic - Efeito itálico (fonte inclinada);
   tsBold - Efeito negrito (passada dupla);
   tsUnderline - Efeito sublinhado. :/}
  TRLDraftTextStyles = set of (tsItalic, tsBold, tsUnderline);

  {@type TRLCPPSelection - Indica a política de compressão dos caracteres, e pode ser fixa ou variável.
   Pode ser um dos seguintes valores:
   csAutomatic - A compressão varia de acordo com a fonte de cada label;
   csFixed5CPP - Compressão fixa em 5cpp;
   csFixed10CPP - Compressão fixa em 10cpp;
   csFixed12CPP - Compressão fixa em 12cpp;
   csFixed17CPP - Compressão fixa em 17cpp;
   csFixed20CPP - Fixa Compressão fm 20cpp. :/}
  TRLCPPSelection = (csAutomatic, csFixed5CPP, csFixed10CPP, csFixed12CPP, csFixed17CPP, csFixed20CPP);

  TDraftObj = class;
  TDraftText = class;
  TDraftImage = class;

  { TRLDraftFilter }

  {@class TRLDraftFilter - Filtro de impressão para impressoras matriciais.
   Este filtro age substituindo os comandos gráficos que seriam enviados ao driver da impressora por códigos de
   programação, os quais são enviados diretamente para o dispositivo de impressão ou programa spooler. Com isso se
   consegue imprimir o mesmo relatório em impressoras de tecnologias diferentes, mantendo-se o design gráfico original
   com impressoras a jato e a laser, e alta velocidade em uma matricial.
   Há várias propriedades e maneiras de se conseguir bons resultados, equilibrando velocidade e qualidade de impressão.   
   Nota: O algorítmo do filtro conseguirá fazer uma melhor aproximação de fontes TrueType variáveis do que de fontes
   fixas. Portanto, não é necessário desenhar os relatórios em uma fonte com pitch fixo, como: Courier ou Terminal.
   @ancestor TRLCustomPrintFilter.
   @links TRLHTMLFilter, TRLRichFilter.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLDraftFilter = class(TRLCustomPrintFilter)
  private

    // state variables

    FDeviceHandle: file;
    FPrinterHandle: Cardinal;
    FSendFirstReset: Boolean;
    FDeviceFileName: String;
    FPrintCut: TPoint;
    FPrintSize: TPoint;
    FCurrentPrintPos: TPoint;
    FCurrentCharWidth: Integer;
    FCurrentCharHeight: Integer;
    FCurrentLPP: Integer;
    FCurrentCPP: Integer;
    FCurrentBoldState: Boolean;
    FCurrentItalicState: Boolean;
    FCurrentUnderlineState: Boolean;

    // property variables
    
    FDriverName: String;
    FCommands: TStrings;
    FDeviceKind: TRLDeviceKind;
    FDevicePath: String;
    FAccentMethod: TRLDraftAccentMethod;
    FEjectMethod: TRLDraftEjectMethod;
    FPrinterFamily: TRLPrinterFamily;
    FDitheringMethod: TRLDitheringMethod;
    FLineDrawMethod: TRLLineDrawMethod;
    FFillArtMethod: TRLFillArtMethod;
    FFormSelection: TRLFormSelection;
    FStretchCharWidth: TRLStretchCharWidth;
    FOrientation: TRLPageOrientation;
    FOptionIndex: Integer;
    FOptions: TStrings;
    FTextDecoration: TRLDraftTextDecoration;
    FTextStyles: TRLDraftTextStyles;
    FCPPSelection: TRLCPPSelection;

    // assign methods
    
    procedure SetCommands(const Value: TStrings);

    // custom methods

    procedure GetObjList(APage: TRLGraphicSurface; AList: TObjectList);
    procedure AddObj(AObj: TDraftObj; AList: TObjectList);
    procedure AddSimpleText(const AText: AnsiString; ACPP, AStyle, APinX, APinY: Integer; AList: TObjectList);
    procedure AddText(AObj: TRLTextObject; AList: TObjectList);
    procedure AddImage(AObj: TRLImageObject; AList: TObjectList);
    procedure AddFillRect(AObj: TRLFillRectObject; AList: TObjectList);
    procedure AddLine(AObj: TRLLineObject; AList: TObjectList);
    procedure AddRectangle(AObj: TRLRectangleObject; AList: TObjectList);
    //
    procedure ResetPage;
    procedure SetPrintPos(AY, AX: Integer);
    procedure SetPrintCompression(ACPP: Integer; AForce: Boolean = False);
    procedure SetPrintStyle(AStyle: Byte);
    function PixelToPinX(X: Integer): Integer;
    function PixelToPinY(Y: Integer): Integer;
    function PrintCode(const ACommand: String; AParameter: Integer = 0): AnsiString;
    procedure SetPrinterFamily(const Value: TRLPrinterFamily);
    procedure DeviceWrite(const AData: AnsiString);
    procedure SetFormSelection(const Value: TRLFormSelection);
    function FormFactorX: Double;
    function SelectFontCPP(const AFontName: String; ASize: Integer): Integer;
    function FontSizeToCPP(const AFontName: String; ASize: Integer): Integer;
    function CPPSelectionToCPP(ACPP: TRLCPPSelection): Integer;
    function StretchFontSize(ASize: Integer): Integer;
    procedure SetTextDecoration(const Value: TRLDraftTextDecoration);
    procedure SetTextStyle(const Value: TRLDraftTextStyles);

    // misc

    function IsCustomPrinterFamily: Boolean;
    function IsCustomDevice: Boolean;
    function IsCustomTextStyle: Boolean;
    procedure GetProgrammingCodes(APrinterFamily: TRLPrinterFamily; ADest: TStrings);

  protected

    // override methods

    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalDrawPage(APage: TRLGraphicSurface); override;
    procedure InternalNewPage; override;
    //
    procedure Loaded; override;
    //
    function GetOptionsLabel: String; override;
    function GetOptions: TStrings; override;
    function GetOptionIndex: Integer; override;
    procedure SetOptionIndex(const Value: Integer); override;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {@method DefaultCommands - Preenche lista de códigos de programação de acordo com a família de impressoras
     escolhida. :/}
    procedure DefaultCommands;

  published
  
    {@method SetOrientation - Seleciona a orientação de papel padrão.
     @links TRLPageOrientation. :/}
    procedure SetOrientation(AOrientation: TRLPageOrientation); override;

    {@prop Commands - Códigos de programação para a impressora.
     Esta propriedade especifica os códigos de compressão, efeitos de fonte e controle de página nativos da impressora.
     Os códigos da configuração default são para impressoras do tipo EPSON. É possível porém adaptá-los a praticamente
     qualquer impressora matricial.
     Cada linha deve indicar o comando e a sequencia de bytes na seguinte forma: "NOME=Asc1,'Chr1',Asc2,'ChrN',Asc2".
     Pode conter as seguintes variáveis: 
     CR - Retorno do carro (ex.: CR=13);
     LF - Avanço de linha (ex.: LF=10);
     BS - Retrocesso de 1 caractere (ex.: BS=8);
     FF - Avanço de página (ex.: FF=12);
     Reset - Inicialização da impressora (ex.: RESET=27,'@');
     MicroOn - Micro salto de N/12 de linha (ex.: MicroOn=27,'A',#);
     MicroOff - Volta ao salto normal de linha (ex.: MicroOff=27,'2');
     Space - O caractere de espaço (ex.: Space=32);
     CPP10 - Compressão a 10 caracteres/polegada (ex.: CPP10=27,'P',18);
     CPP12 - Compressão a 12 caracteres/polegada (ex.: CPP12=27,'M',18);
     CPP17 - Compressão a 17 caracteres/polegada (ex.: CPP17=27,'P',15);
     CPP20 - Compressão a 20 caracteres/polegada (ex.: CPP20=27,'M',15);
     ExpandOn - Liga modo expandido (ex.: ExpandOn=27,'W',1);
     ExpandOff - Modo expandido desligado (ex.: ExpandOff=27,'W',0);
     BoldOn - Liga modo negrito (ex.: BoldOn=27,'G');
     BoldOff - Modo negrito desligado (ex.: BoldOff=27,'H');
     ItalicOn - Liga modo itálico (ex.: ItalicOn=27,'4');
     ItalicOff - Modo itálico desligado (ex.: ItalicOff=27,'5');
     UnderlineOn - Liga modo itálico (ex.: UnderlineOn=27,'-1');
     UnderlineOff - Modo itálico desligado (ex.: UnderlineOff=27,'-0');
     RAW - Envio de sequência de bytes para impressão de gráficos (ex.: RAW=27,'L',#l,#h).
     @links DefaultCommands, PrinterFamily. :/}
    property Commands: TStrings read FCommands write SetCommands stored IsCustomPrinterFamily;

    {@prop DriverName - Nome de arquivo "driver" contendo os códigos de programação da impressora.
     Utilize esta propriedade quando quiser manter os códigos de programação da impressora
     em um arquivo texto externo ao programa. A sintaxe do arquivo é a mesma da propriedade
     Commands.
     @links Commands, DefaultCommands. :/}
    property DriverName: String read FDriverName write FDriverName;

    {@prop DeviceKind - Tipo de dispositivo de impressão.
     Indica que tipo de dispositivo está identificado na propriedade DevicePath.
     @links TRLDeviceKind, DevicePath. :/}
    property DeviceKind: TRLDeviceKind read FDeviceKind write FDeviceKind stored IsCustomDevice;

    {@prop DevicePath - Caminho para o dispositivo de impressão.
     Esta propriedade indica o nome ou o caminho do dispositivo de impressão ou programa spooler.
     No Windows pode-se utilizar um dos dispositivos padrões: PRN, LPT1, LPT2 etc, ou um caminho de
     rede no formato "\\computador\impressora". O dispositivo PRN é especialmente interessante, pois
     sempre representa a impressora atualmente selecionada pelo sistema.
     No Linux pode-se informar tanto um caminho para um dispositivo (ex.: "/dev/lp0") como para um
     programa de controle de spool como o lpr (ex.: "lpr -P%p %f"). Neste último caso, "%p" representa
     o nome de uma impressora válida cadastrada pelo linuxconf, e "%f" o nome de um arquivo temporário
     gerado pelo FR.
     @links DeviceKind. :/}
    property DevicePath: String read FDevicePath write FDevicePath stored IsCustomDevice;

    {@prop AccentMethod - Comportamento do filtro em relação a caracteres acentuados.
     Algumas impressoras não suportam ou podem não estar adequadamente configuradas para imprimir
     caracteres acentuados.
     @links TRLDraftAccentMethod. :/}
    property AccentMethod: TRLDraftAccentMethod read FAccentMethod write FAccentMethod default amOverwrite;

    {@prop EjectMethod - Comportamento do filtro em relação aos saltos de páginas.
     Em alguns casos quando se tem um tamanho específico de formulário pode ser necessário
     modificar a maneira como o filtro efetua os saltos de página.
     @links TRLDraftEjectMethod, Commands. :/}
    property EjectMethod: TRLDraftEjectMethod read FEjectMethod write FEjectMethod default ejCompletePage;

    {@prop PrinterFamily - Família de impressoras.
     Às vezes é desejável conhecer algumas particularidades da impressora a ser utilizada e que
     não podem ser informadas somente com comandos, como a impressão de gráficos.
     @links TRLPrinterFamily. :/}
    property PrinterFamily: TRLPrinterFamily read FPrinterFamily write SetPrinterFamily default fmEpsonLike;

    {@prop DitheringMethod - Método para impressão de imagens.
     Esta propriedade indica que método deve ser utilizado para transformar imagens coloridas
     em preto e branco.
     @links TRLDitheringMethod. :/}
    property DitheringMethod: TRLDitheringMethod read FDitheringMethod write FDitheringMethod default dmColorTable;

    {@prop LineDrawMethod - Método para impressão de linhas.
     Esta propriedade indica que método deve ser utilizado para desenhar linhas numa impressora
     matricial.
     @links TRLLineDrawMethod. :/}
    property LineDrawMethod: TRLLineDrawMethod read FLineDrawMethod write FLineDrawMethod default ldMinusAndPipe;

    {@prop FillArtMethod - Método para o preenchimento de retângulos ou traços grossos. :/}
    property FillArtMethod: TRLFillArtMethod read FFillArtMethod write FFillArtMethod default fmNone;

    {@prop FormSelection - Política de seleção de tamanho para formulários contínuos.
     @links TRLFormSelection. :/}
    property FormSelection: TRLFormSelection read FFormSelection write SetFormSelection default fsAccordingToOrientation;

    {@prop StretchCharWidth - Método de adaptação do tamanho das fontes.
     @links TRLStretchCharWidth. :/}
    property StretchCharWidth: TRLStretchCharWidth read FStretchCharWidth write FStretchCharWidth default scShrinksOnly;

    {@prop TextDecoration - Decoração do texto.
     @links TRLDraftTextDecoration. :/}
    property TextDecoration: TRLDraftTextDecoration read FTextDecoration write SetTextDecoration default ddIncludeAll;
    
    {@prop TextStyles - Estilos de texto.
     @links TRLDraftTextStyles. :/}
    property TextStyles: TRLDraftTextStyles read FTextStyles write SetTextStyle stored IsCustomTextStyle;

    {@prop CPPSelection - Fixa uma compressão padrão para todo o relatório.
     @links Commands, TRLCPPSelection. :/}
    property CPPSelection: TRLCPPSelection read FCPPSelection write FCPPSelection default csAutomatic;

    property DisplayName;
  end;
  {/@class}

  { TDraftObj }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TDraftObj = class
  public
    PinBounds: TRect;
  end;

  { TDraftText }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TDraftText = class(TDraftObj)
  public
    CPP: Integer;
    Style: Byte;
    Text: AnsiString;
  end;

  { TDraftImage }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TDraftImage = class(TDraftObj)
  public
    Bitmap: TBitmap;
    //
    constructor Create;
    destructor Destroy; override;
  end;

{/@unit}

implementation

const
  AspectratioX = 126 / 100;
  AspectratioY = 72 / 100;
  //
  StandardLPP = Trunc(66 / 11); // linhas por polegada (66 lins / 11 pol = 6 lpp)
  StandardCPP = Trunc(80 / 8); // colunas por polegada (80 cols / 8 pol = 10 cpp)
  DefaultFontName = 'Arial';
  DefaultFontSize = 6;
  //
  AccentLetters = 'áàãâäÁÀÃÂÄéèêëÉÈÊËíìîïÍÌÎÏóòõôöÓÒÕÔÖúùûüÚÙÛÜçÇºª';
  NormalLetters = 'aaaaaAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUcCoa';
  AccentChars = '''`~^¨''`~^¨''`^¨''`^¨''`^¨''`^¨''`~^¨''`~^¨''`^¨''`^¨,,__';

var
  PinsPerRow: Integer = 12;
  PinsPerCol: Integer = 12;

procedure DoColorTable(ASource, ADest: TBitmap; AContrast: Double = 2);
const
  MatrixSize = 4;
  TheMatrix: array[0..MatrixSize - 1, 0..MatrixSize - 1] of Byte = ((0, 192, 48, 240), 
                                                             (128, 64, 176, 112), 
                                                             (32, 224, 16, 208), 
                                                             (160, 96, 144, 80));
  PlotColors: array[Boolean] of TColor = (clWhite, clBlack);
var
  X, Y, I: Integer;
  plot: Boolean;
  srcln: PRGBArray;
  dstln: PRGBArray;
begin
  ASource.PixelFormat := pf32bit;
  ADest.PixelFormat := pf32bit;
  ADest.Width := ASource.Width;
  ADest.Height := ASource.Height;
  for Y := 0 to ASource.Height - 1 do
  begin
    srcln := ASource.ScanLine[Y];
    dstln := ADest.ScanLine[Y];
    for X := 0 to ASource.Width - 1 do
    begin
      with srcln[X] do
        I := (rgbRed + rgbGreen + rgbBlue) div 3;
      I := Trunc(((I - 128) * AContrast + 128));
      plot := (I < TheMatrix[Y mod MatrixSize, X mod MatrixSize]);
      with dstln[X] do
      begin
        rgbRed := Byte(not plot) * 255;
        rgbGreen := Byte(not plot) * 255;
        rgbBlue := Byte(not plot) * 255;
      end;
    end;
  end;
end;

procedure DoErrorDiffusion(ASource, ADest: TBitmap; AContrast: Double = 2);
const
  PlotColors: array[Boolean] of TColor = (clWhite, clBlack);
var
  X, Y, I: Integer;
  plot: Boolean;
  srcln: PRGBArray;
  dstln: PRGBArray;
begin
  ASource.PixelFormat := pf32bit;
  ADest.PixelFormat := pf32bit;
  ADest.Width := ASource.Width;
  ADest.Height := ASource.Height;
  for Y := 0 to ASource.Height - 1 do
  begin
    srcln := ASource.ScanLine[Y];
    dstln := ADest.ScanLine[Y];
    for X := 0 to ASource.Width - 1 do
    begin
      with srcln[X] do
        I := (rgbRed + rgbGreen + rgbBlue) div 3;
      I := Trunc(((I - 128) * AContrast + 128));
      plot := (I < Random(256));
      with dstln[X] do
      begin
        rgbRed := Byte(not plot) * 255;
        rgbGreen := Byte(not plot) * 255;
        rgbBlue := Byte(not plot) * 255;
      end;
    end;
  end;
end;

function TranslateAccents(const AString, ABS: AnsiString): AnsiString;
var
  I, P: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    P := Pos(AString[I], AccentLetters);
    if P > 0 then
      Result := Result + Copy(AccentChars, P, 1) + ABS + Copy(NormalLetters, P, 1)
    else
      Result := Result + AString[I];
  end;
end;

function RemoveAccents(const AString: AnsiString): AnsiString;
var
  I, P: Integer;
  Aux: AnsiString;
begin
  Aux := NormalLetters;
  Result := AString;
  for I := 1 to Length(AString) do
  begin
    P := Pos(AString[I], AccentLetters);
    if P > 0 then
      Result[I] := Aux[P];
  end;
end;

function CPPPins(ACPP: Integer): Integer;
begin
  Result := Round(PinsPerCol * StandardCPP / ACPP);
end;

function LPPPins(ALPP: Integer): Integer;
begin
  Result := Round(PinsPerRow * StandardLPP / ALPP);
end;

function GetBitmapPixel(ABitmap: TBitmap; AX, AY: Integer; ADefault: TColor): TColor;
begin
  {$ifndef FPC}
  if AY < ABitmap.Height then
    with TRGBArray(ABitmap.ScanLine[AY]^)[AX] do
      Result := RGB(rgbRed, rgbGreen, rgbBlue)
  {$else}
  if AY < ABitmap.Height then
    Result := aBitmap.Canvas.Pixels[aX,aY]
  {$endif}
  else
    Result := ADefault;
end;

{$IfDef MSWINDOWS}
{$IfDef DELPHIXE2_UP}
 function LinePrinterStart(const PrnName, DocName: String): NativeUInt;
{$else}
 function LinePrinterStart(const PrnName, DocName: String): Cardinal;
{$EndIf}
var
  di: TDocInfo1;
begin
  FillChar(di, SizeOf(di), 0);
  di.pDocName := PChar(DocName);
  di.pOutputFile := nil;
  di.pDatatype := 'RAW';
  {$IfDef UNICODE}
  OpenPrinterW(PWideChar(PrnName), {$IfDef FPC}@{$EndIf}Result, nil);
  {$Else}
  OpenPrinter(PChar(PrnName), {$IfDef FPC}@{$EndIf}Result, nil);
  {$EndIf}
  StartDocPrinter(Result, 1, @di);
  StartPagePrinter(Result);
end;

procedure LinePrinterWrite(PrnHandle: Cardinal; const Text: AnsiString);
var
  Len: Cardinal;
begin
  Len := Length(Text);
  if Len > 0 then
  begin
    {$ifdef FPC}
    WritePrinter(PrnHandle, @Text[1], Len, PDword(Len));
    {$else}
    WritePrinter(PrnHandle, @Text[1], Len, Len);
    {$endif}
  end;
end;

procedure LinePrinterEnd(PrnHandle: Cardinal);
begin
  EndPagePrinter(PrnHandle);
  EndDocPrinter(PrnHandle);
  ClosePrinter(PrnHandle);
end;
{$EndIf}

{ TDraftImage }

constructor TDraftImage.Create;
begin
  Bitmap := nil;
  //
  inherited;
end;

destructor TDraftImage.Destroy;
begin
  inherited;
  //
  if Assigned(Bitmap) then
    Bitmap.Free;
end;

{ TRLDraftFilter }

constructor TRLDraftFilter.Create(AOwner: TComponent);
begin
  FDriverName := '';
  FCommands := nil;
  FAccentMethod := amOverwrite;
  FEjectMethod := ejCompletePage;
  FSendFirstReset := False;
{$ifndef LINUX}
  FDeviceKind := dkPrinterPort;
  FDevicePath := 'prn';
{$else}
  FDeviceKind := dkProgram;
  FDevicePath := 'lpr -P%p %f';
{$endif};
  FCommands := nil;
  FPrinterFamily := fmEpsonLike;
  FDitheringMethod := dmColorTable;
  FLineDrawMethod := ldMinusAndPipe;
  FFillArtMethod := fmNone;
  FFormSelection := fsAccordingToOrientation;
  FOrientation := poPortrait;
  FOptionIndex := 0;
  FOptions := nil;
  FStretchCharWidth := scShrinksOnly;
  FTextDecoration := ddIncludeAll;
  FTextStyles := [tsItalic, tsBold, tsUnderline];
  FCPPSelection := csAutomatic;
  //
  inherited;
  //
  FCommands := TStringList.Create;
  DefaultCommands;
  //
  ClassOptions := ClassOptions + [foEmulateCopies];
end;

destructor TRLDraftFilter.Destroy;
begin
  if Assigned(FCommands) then
    FreeAndNil(FCommands);
  if Assigned(FOptions) then
    FreeAndNil(FOptions);
  //
  inherited;
end;

procedure TRLDraftFilter.InternalBeginDoc;
var
  metrics: TRLPrinterMetrics;
begin
  RLPrinter.SetPaperSize(Pages.PaperWidth, Pages.PaperHeight,
    Pages.Orientation = MetaOrientationLandscape, False, False);
  RLPrinter.LoadMetrics(metrics);
  //
  FPrintCut.X := Round(metrics.MarginLeft * Pages.OrientedWidth / metrics.PhysicalWidth);
  FPrintCut.Y := Round(metrics.MarginTop * Pages.OrientedHeight / metrics.PhysicalHeight);
  FPrintSize.X := Pages.OrientedWidth;
  FPrintSize.Y := Pages.OrientedHeight;
  //
  FSendFirstReset := True;
  // carrega comandos
  if (FDriverName <> '') and FileExists(FDriverName) then
    FCommands.LoadFromFile(FDriverName);
  if FCommands.Count = 0 then
    DefaultCommands;
  if FPrinterFamily = fmESCP2 then
    PinsPerRow := 60 // 0..255 360/6LPP
  else
    PinsPerRow := 12; // 0..85 60/6LPP
  // medidas padrão papel carta
  FCurrentLPP := StandardLPP;
  FCurrentCPP := StandardCPP;
  FCurrentBoldState := False;
  FCurrentItalicState := False;
  FCurrentUnderlineState := False;
  FCurrentCharWidth := CPPPins(FCurrentCPP);
  FCurrentCharHeight := LPPPins(FCurrentLPP);
  //

  {$IfDef MSWINDOWS}
  if FDeviceKind = dkPrinter then
    FPrinterHandle := LinePrinterStart(RLPrinter.PrinterName, 'FortesReport')
  else
  {$EndIf}
  begin
    case FDeviceKind of
      dkPrinterPort: FDeviceFileName := RLPrinter.PrinterPort;
      dkProgram: begin
                       FDeviceFileName := GetTempFileName;
                       RegisterTempFile(FDeviceFileName);
                     end;
      dkFileName: FDeviceFileName := FDevicePath;
    else
      FDeviceFileName := FDevicePath;
    end;

    //
    AssignFile(FDeviceHandle, FDeviceFileName);
    Rewrite(FDeviceHandle, 1);
  end;
  //
  ResetPage;
end;

procedure TRLDraftFilter.InternalEndDoc;
var
  cmd: String;
  par:string;
  i  :integer;
  {$ifdef FPC}
   VProcess: TProcess;
  {$endif}
begin
  NewPage;
  //
  {$IfDef MSWINDOWS}
   if FDeviceKind = dkPrinter then
     LinePrinterEnd(FPrinterHandle)
   else
  {$EndIf}
  begin
    CloseFile(FDeviceHandle);
    case FDeviceKind of
      dkPrinterPort: ;
      dkProgram: 
      begin
        cmd := FDevicePath;
        cmd := StringReplace(cmd, '%p', RLPrinter.PrinterName, [rfReplaceAll, rfIgnoreCase]);
        cmd := StringReplace(cmd, '%f', FDeviceFileName, [rfReplaceAll, rfIgnoreCase]);

        {$IfDef FPC}
         VProcess := TProcess.Create(nil);
         try
           begin
             VProcess.Options := [poNoConsole];
             VProcess.CommandLine := Cmd;
             VProcess.Execute;
             end;
         finally
           VProcess.Free;
         end;
        {$else}
         {$IfDef MSWINDOWS}
          I := Pos(' ', cmd);
          if I = 0 then
            I := Length(cmd) + 1;
          par := Copy(cmd, I + 1, Length(cmd));
          cmd := Copy(cmd, 1, I - 1);
          ShellExecute(0, 'open', PChar(cmd), PChar(par), nil, SW_SHOWNORMAL);
         {$Else}
          Libc.system(PChar(cmd));
         {$endif};
        {$endif}
      end;
      dkFileName: ;
    end;
  end;
end;

procedure TRLDraftFilter.InternalNewPage;
begin
  case FEjectMethod of
    ejCompletePage: SetPrintPos(PixelToPinY(FPrintSize.Y), PixelToPinX(0)); // posicionamento vertical da proxima página em agulhas
    ejForceWithCode: DeviceWrite(PrintCode('FF'));
    ejLeavePage: SetPrintPos(FCurrentPrintPos.Y + PinsPerRow, PixelToPinX(0)); // apenas salta uma linha
  end;
  ResetPage;
end;

procedure TRLDraftFilter.ResetPage;
begin
  FCurrentPrintPos.X := 0;
  FCurrentPrintPos.Y := 0;
end;

procedure TRLDraftFilter.GetProgrammingCodes(APrinterFamily: TRLPrinterFamily; ADest: TStrings);
begin
  case APrinterFamily of
    fmEpsonLike: with ADest do
                 begin
                   Values['CR'] := '13';
                   Values['LF'] := '10';
                   Values['BS'] := '8';
                   Values['FF'] := '12';
                   Values['Reset'] := '27,''@'',27,''x0''';
                   Values['MicroOn'] := '27,''A'',#';
                   Values['MicroOff'] := '27,''2''';
                   Values['Space'] := '32';
                   Values['CPP10'] := '27,''P'',18';
                   Values['CPP12'] := '27,''M'',18';
                   Values['CPP17'] := '27,''P'',15';
                   Values['CPP20'] := '27,''M'',15';
                   Values['ExpandOn'] := '27,''W'',1';
                   Values['ExpandOff'] := '27,''W'',0';
                   Values['BoldOn'] := '27,''G''';
                   Values['BoldOff'] := '27,''H''';
                   Values['ItalicOn'] := '27,''4''';
                   Values['ItalicOff'] := '27,''5''';
                   Values['UnderlineOn'] := '27,''-1''';
                   Values['UnderlineOff'] := '27,''-0''';
                   Values['RAW'] := '27,''L'',#l,#h';
                 end;
    fmESCP2: begin
                   GetProgrammingCodes(fmEpsonLike, ADest);
                   with ADest do
                     Values['MicroOn'] := '27,''+'',#';
                 end;
  end;
end;

procedure TRLDraftFilter.DefaultCommands;
begin
  if FPrinterFamily = fmCustom then
    Exit;
  FCommands.Clear;
  GetProgrammingCodes(FPrinterFamily, FCommands);
end;

function TRLDraftFilter.PrintCode(const ACommand: String; AParameter: Integer = 0): AnsiString;
var
  S, AChr: String;
  I: Integer;
begin
  S := FCommands.Values[ACommand];
  Result := '';
  if S = '' then
    Exit;
  I := 0;
  repeat
    Inc(I);
    AChr := Token(S, I, ',');
    if AChr = '' then
      Break
    else if (AChr[1] = '''') and (AChr[Length(AChr)] = '''') then
      Result := Result + Copy(AChr, 2, Length(AChr) - 2)
    else if AChr = '#' then
      Result := Result + Char(AParameter)
    else if LowerCase(AChr) = '#l' then
      Result := Result + Char(AParameter mod 256)
    else if LowerCase(AChr) = '#h' then
      Result := Result + Char(AParameter div 256)
    else if AChr = '$' then
      Result := Result + IntToStr(AParameter)
    else
      Result := Result + AnsiChar(chr(strtoint(AChr)));
  until False;
end;

procedure TRLDraftFilter.DeviceWrite(const AData: AnsiString);
begin
  if AData <> '' then
    {$IfDef MSWINDOWS}
     if FDeviceKind = dkPrinter then
       LinePrinterWrite(FPrinterHandle, AData)
     else
    {$EndIf}
    begin
      BlockWrite(FDeviceHandle, AData[1], Length(AData));
    end;
end;

function TRLDraftFilter.FormFactorX: Double;
begin
  if FOrientation = poPortrait then
    if FOptionIndex = 2 then
      Result := 132 / 80
    else
      Result := 1
  else if FOrientation = poLandscape then
    if FOptionIndex = 1 then
      Result := 80 / 132
    else
      Result := 1
  else
    Result := 1; 
end;

function TRLDraftFilter.PixelToPinX(X: Integer): Integer;
var
  qpol, qcol: Double;
begin
  Dec(X, FPrintCut.X);
  qpol := (X * FormFactorX) / ScreenPPI; // transforma X em polegadas
  qcol := qpol * StandardCPP; // transforma polegadas em colunas de acordo com o cpp padrão
  Result := Trunc(qcol * PinsPerCol); // transforma colunas em pinos
end;

function TRLDraftFilter.PixelToPinY(Y: Integer): Integer;
var
  qpol, qlin: Double;
begin
  Dec(Y, FPrintCut.Y);
  qpol := Y / ScreenPPI; // transforma Y em polegadas
  qlin := qpol * StandardLPP; // transforma polegadas em linhas de acordo com o lpp padrão
  Result := Trunc(qlin * PinsPerRow); // transforma linhas em pinos
end;

procedure TRLDraftFilter.SetPrintPos(AY, AX: Integer);
var
  dist: Integer;
begin
  // posicionamento vertical
  while FCurrentPrintPos.Y + FCurrentCharHeight <= AY do
  begin
    DeviceWrite(PrintCode('CR'));
    DeviceWrite(PrintCode('LF'));
    Inc(FCurrentPrintPos.Y, FCurrentCharHeight);
    FCurrentPrintPos.X := 0;
  end;
  if FCurrentPrintPos.Y < AY then
  begin
    dist := AY - FCurrentPrintPos.Y;
    if dist > 0 then
    begin
      DeviceWrite(PrintCode('MicroOn', dist));
      DeviceWrite(PrintCode('CR'));
      DeviceWrite(PrintCode('LF'));
      DeviceWrite(PrintCode('MicroOff'));
      FCurrentPrintPos.X := 0;
    end;
    FCurrentPrintPos.Y := AY;
  end;
  // posicionamento horizontal
  if AX < FCurrentPrintPos.X then
  begin
    DeviceWrite(PrintCode('CR'));
    FCurrentPrintPos.X := 0;
  end;
  while FCurrentPrintPos.X + FCurrentCharWidth <= AX do
  begin
    DeviceWrite(PrintCode('Space'));
    Inc(FCurrentPrintPos.X, FCurrentCharWidth);
  end;
end;

function TRLDraftFilter.StretchFontSize(ASize: Integer): Integer;
var
  F: Double;
begin
  F := FormFactorX;
  case FStretchCharWidth of
    scEnlargementsOnly: if F > 1 then
                          Result := Round(ASize * F)
                        else
                          Result := ASize;
    scShrinksOnly: if F < 1 then
                          Result := Round(ASize * F)
                        else
                          Result := ASize;
    scAlways: Result := Round(ASize * F);
  else
    Result := ASize;
  end;
end;

function TRLDraftFilter.SelectFontCPP(const AFontName: String; ASize: Integer): Integer;
begin
  if FCPPSelection = csAutomatic then
    Result := FontSizeToCPP(AFontName, StretchFontSize(ASize))
  else
    Result := CPPSelectionToCPP(FCPPSelection);
end;

function TRLDraftFilter.FontSizeToCPP(const AFontName: String; ASize: Integer): Integer;
const
  TextSample = 'In addition, a check is made to see if the screen or printer is a palette device, and if so, palette handling for the device is enabled.';
  MagicDelta = 14 / 10;
var
  OneCharWidth: Double;
  CharsPerInch: Double;
  FontSizeBitmap: TBitmap;
begin
  FontSizeBitmap := NeedAuxBitmap;
  FontSizeBitmap.Canvas.Font.Name := AFontName;
  FontSizeBitmap.Canvas.Font.Size := ASize;
  // a largura média de um caractere em pixels é dada pela largura da amostra em
  // pixels dividida pela largura da amostra em caracteres com a fonte indicada
  // um fator "mágico" foi calculado através de testes para chegar ao valor ideal  
  OneCharWidth := MagicDelta * FontSizeBitmap.Canvas.TextWidth(TextSample) / Length(TextSample);
  CharsPerInch := ScreenPPI / OneCharWidth;
  //
  if CharsPerInch <= 5 then
    Result := 5
  else if CharsPerInch <= 10 then
    Result := 10
  else if CharsPerInch <= 12 then
    Result := 12
  else if CharsPerInch <= 17 then
    Result := 17
  else
    Result := 20;
end;

function TRLDraftFilter.CPPSelectionToCPP(ACPP: TRLCPPSelection): Integer;
begin
  case ACPP of
    csFixed5CPP: Result := 5;
    csFixed10CPP: Result := 10;
    csFixed12CPP: Result := 12;
    csFixed17CPP: Result := 17;
    csFixed20CPP: Result := 20;
  else
    Result := FCurrentCPP;
  end;
end;

// ajusta fonte da impressora
procedure TRLDraftFilter.SetPrintCompression(ACPP: Integer; AForce: Boolean = False);
begin
  // verifica a real necessidade
  if (ACPP = FCurrentCPP) and not AForce then
    Exit;
  if FCurrentCPP = 5 then
    DeviceWrite(PrintCode('ExpandOff'));
  case ACPP of
    5: begin
          DeviceWrite(PrintCode('CPP10'));
          DeviceWrite(PrintCode('ExpandOn')); // CPP10 + expandido
        end;
    10: DeviceWrite(PrintCode('CPP10'));
    12: DeviceWrite(PrintCode('CPP12'));
    17: DeviceWrite(PrintCode('CPP17'));
    20: DeviceWrite(PrintCode('CPP20'));
  end;
  FCurrentCPP := ACPP;
  FCurrentCharWidth := Trunc(PinsPerCol * StandardCPP / FCurrentCPP);
  // força o reposicionamento do carro para a nova compressão
  with FCurrentPrintPos do
    SetPrintPos(Y, 0);
end;

procedure TRLDraftFilter.SetPrintStyle(AStyle: Byte);
var
  newbold: Boolean;
  newitalic: Boolean;
  newunderline: Boolean;
begin
  // negrito
  newbold := ((AStyle and MetaFontStyleBold) = MetaFontStyleBold) and (tsBold in FTextStyles);
  if newbold <> FCurrentBoldState then
  begin
    if newbold then
      DeviceWrite(PrintCode('BoldOn'))
    else
      DeviceWrite(PrintCode('BoldOff'));
    FCurrentBoldState := newbold;
  end;
  // itálico
  newitalic := ((AStyle and MetaFontStyleItalic) = MetaFontStyleItalic) and (tsItalic in FTextStyles);
  if newitalic <> FCurrentItalicState then
  begin
    if newitalic then
      DeviceWrite(PrintCode('ItalicOn'))
    else
      DeviceWrite(PrintCode('ItalicOff'));
    FCurrentItalicState := newitalic;
  end;
  // underline
  newunderline := ((AStyle and MetaFontStyleUnderline) = MetaFontStyleUnderline) and (tsUnderline in FTextStyles);
  if newunderline <> FCurrentUnderlineState then
  begin
    if newunderline then
      DeviceWrite(PrintCode('UnderlineOn'))
    else
      DeviceWrite(PrintCode('UnderlineOff'));
    FCurrentUnderlineState := newunderline;
  end;
end;

function IsText(AObj: TObject): Boolean;
begin
  Result := (AObj is TDraftText);
end;

function IsSameFont(AObj1, AObj2: TObject): Boolean;
begin
  Result := (TDraftText(AObj1).CPP = TDraftText(AObj2).CPP) and (TDraftText(AObj1).Style = TDraftText(AObj2).Style);
end;

procedure TRLDraftFilter.AddObj(AObj: TDraftObj; AList: TObjectList);
var
  I: Integer;
  O: TDraftObj;
begin
  // posicionamento na lista
  I := 0;
  while I < AList.Count do
  begin
    O := TDraftObj(AList[I]);
    if AObj.PinBounds.Top < O.PinBounds.Top then
      Break
    else if AObj.PinBounds.Top = O.PinBounds.Top then
      if IsText(AObj) then
        if not IsText(O) then
          Break
        else if IsSameFont(AObj, O) then
          if AObj.PinBounds.Left < O.PinBounds.Left then
            Break
          else
        else
      else if AObj.PinBounds.Left < O.PinBounds.Left then
        Break;
    Inc(I);
  end;
  AList.Insert(I, AObj);
end;

procedure TRLDraftFilter.AddSimpleText(const AText: AnsiString; ACPP, AStyle, APinX, APinY: Integer; AList: TObjectList);
var
  ob: TDraftText;
begin
  ob := TDraftText.Create;
  ob.CPP := ACPP;
  ob.Style := AStyle;
  ob.PinBounds.Left := APinX;
  ob.PinBounds.Top := APinY;
  ob.PinBounds.Right := APinX + Length(AText) * CPPPins(ob.CPP);
  ob.PinBounds.Bottom := APinY + LPPPins(FCurrentLPP);
  ob.Text := AText;
  AddObj(ob, AList);
end;

procedure TRLDraftFilter.AddText(AObj: TRLTextObject; AList: TObjectList);
var
  W, I: Integer;
  ob: TDraftText;
  R: TRect;
  S: AnsiString;
begin
  ob := TDraftText.Create;
  ob.CPP := SelectFontCPP(AObj.Font.Name, AObj.Font.Size);
  ob.Style := AObj.Font.Style;
  R.Left := AObj.BoundsRect.Left;
  R.Top := AObj.BoundsRect.Top;
  R.Right := AObj.BoundsRect.Right;
  R.Bottom := AObj.BoundsRect.Bottom;
  S := GetAnsiStr(AObj.DisplayText);
  if (AObj.TextFlags and MetaTextFlagAutoSize) = 0 then
  begin
    // corta o texto
    W := (PixelToPinX(R.Right) - PixelToPinX(R.Left)) div CPPPins(ob.CPP) + 1;
    case AObj.Alignment of
      MetaTextAlignmentLeft: S := Copy(S, 1, W);
      MetaTextAlignmentRight: S := Copy(S, Max(1, Length(S) - W + 1), W);
      MetaTextAlignmentCenter: S := Copy(S, Max(1, (Length(S) - W) div 2), W);
      MetaTextAlignmentJustify: begin
                                 S := Copy(S, 1, W);
                                 I := Length(S);
                                 while (Length(S) < W) and IterateJustification(S, I) do;
                               end;
    end;
  end;
  // posicao esquerda
  W := Length(S) * CPPPins(ob.CPP);
  case AObj.Alignment of
    MetaTextAlignmentLeft: ob.PinBounds.Left := PixelToPinX(AObj.Origin.X);
    MetaTextAlignmentRight: ob.PinBounds.Left := PixelToPinX(R.Right) - W;
    MetaTextAlignmentCenter: ob.PinBounds.Left := (PixelToPinX(R.Left) + PixelToPinX(R.Right) - W) div 2;
    MetaTextAlignmentJustify: ob.PinBounds.Left := PixelToPinX(AObj.Origin.X);
  end;
  //
  ob.PinBounds.Top := PixelToPinY(AObj.Origin.Y);
  ob.PinBounds.Right := ob.PinBounds.Left + W;
  ob.PinBounds.Bottom := ob.PinBounds.Top + LPPPins(FCurrentLPP);
  ob.Text := S;
  AddObj(ob, AList);
end;

procedure TRLDraftFilter.AddImage(AObj: TRLImageObject; AList: TObjectList);
var
  thegraphic: TGraphic;
  asbitmap: TBitmap;
  tempbmp: TBitmap;
  Y, ystep: Integer;
  ob: TDraftImage;
begin
  if FDitheringMethod = dmNone then
    Exit;
  asbitmap := TRLBitmap.Create;
  try
    thegraphic := FromMetaGraphic(AObj.Data);
    try
      tempbmp := TRLBitmap.Create;
      try
        tempbmp.PixelFormat := pf32bit;
        tempbmp.Width := Round((AObj.BoundsRect.Right - AObj.BoundsRect.Left) * AspectratioX);
        tempbmp.Height := Round((AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) * AspectratioY);
        {$ifdef FPC}
        tempbmp.Canvas.StretchDraw(Bounds(0, 0, tempbmp.Width, tempbmp.Height), thegraphic);
        {$else}
        tempbmp.Canvas.StretchDraw(Rect(0, 0, tempbmp.Width, tempbmp.Height), thegraphic);
        {$endif}
        asbitmap.Width := tempbmp.Width;
        asbitmap.Height := tempbmp.Height;
        case FDitheringMethod of
          dmNone: ;
          dmColorTable: DoColorTable(tempbmp, asbitmap);
          dmErrorDiffusion: DoErrorDiffusion(tempbmp, asbitmap);
        end;
      finally
        tempbmp.Free;
      end;
    finally
      thegraphic.Free;
    end;
    //
    Y := 0;
    while Y < asbitmap.Height do
    begin
      ystep := asbitmap.Height - Y;
      if ystep > 8 then
        ystep := 8;
      ob := TDraftImage.Create;
      try
        ob.Bitmap := TRLBitmap.Create;
        ob.Bitmap.PixelFormat := pf32bit;
        ob.Bitmap.Width := asbitmap.Width;
        ob.Bitmap.Height := ystep;
        ob.Bitmap.Canvas.Draw(0, - Y, asbitmap);
        ob.PinBounds.Left := PixelToPinX(AObj.BoundsRect.Left);
        ob.PinBounds.Top := PixelToPinY(AObj.BoundsRect.Top) + Y;
        ob.PinBounds.Right := PixelToPinX(AObj.BoundsRect.Right);
        ob.PinBounds.Bottom := ob.PinBounds.Top + ystep;
        AddObj(ob, AList);
      except
        ob.Free;
        raise;
      end;
      Inc(Y, ystep);
    end;
  finally
    asbitmap.Free;
  end;
end;

procedure TRLDraftFilter.AddRectangle(AObj: TRLRectangleObject; AList: TObjectList);
var
  RectHorzLength, RectVertLength: Integer;
  OneLetterWidthInPins, OneLetterHeightInPins: Integer;
  VertStep, TraceCPP, I: Integer;
  ReplCh: AnsiChar;
  aux: AnsiString;
begin
  if FLineDrawMethod = ldNone then
    Exit;
  // se não for sólido
  if AObj.Pen.Style <> MetaPenStyleSolid then
    Exit;
  // se for de cor clara
  with AObj.Pen.Color do
    if (Red + Green + Blue) / 3 > 127 then
      Exit;
  // escolhe o cpp default para o retângulo
  TraceCPP := SelectFontCPP(DefaultFontName, DefaultFontSize);
  // tamanho de um caractere em agulhas no cpp padrão
  OneLetterWidthInPins := CPPPins(TraceCPP);
  OneLetterHeightInPins := LPPPins(StandardLPP);
  // calcula as dimensões do retangulo em caracteres
  RectHorzLength := (PixelToPinX(AObj.BoundsRect.Right) - PixelToPinX(AObj.BoundsRect.Left)) div OneLetterWidthInPins;
  RectVertLength := (PixelToPinY(AObj.BoundsRect.Bottom) - PixelToPinY(AObj.BoundsRect.Top)) div OneLetterHeightInPins;
  //
  if RectHorzLength > 0 then
  begin
    case FLineDrawMethod of
      ldMinusAndPipe: if AObj.Pen.Width > 1 then
                          ReplCh := '='
                        else
                          ReplCh := '-';
      ldGraphicCharset: if AObj.Pen.Width > 1 then
                          ReplCh := #205
                        else
                          ReplCh := #196;
    else
      ReplCh := ' ';
    end;
    SetLength(aux, RectHorzLength);
    FillChar(aux[1], RectHorzLength, ReplCh);
    AddSimpleText(aux, TraceCPP, 0, PixelToPinX(AObj.BoundsRect.Left), PixelToPinY(AObj.BoundsRect.Top) - OneLetterHeightInPins div 2, AList);
    if RectVertLength > 1 then
      AddSimpleText(aux, TraceCPP, 0, PixelToPinX(AObj.BoundsRect.Left), PixelToPinY(AObj.BoundsRect.Bottom) - OneLetterHeightInPins div 2, AList);
  end;
  if RectVertLength > 0 then
  begin
    case FLineDrawMethod of
      ldMinusAndPipe: ReplCh := '|';
      ldGraphicCharset: if AObj.Pen.Width > 1 then
                          ReplCh := #186
                        else
                          ReplCh := #179;
    else
      ReplCh := ' ';
    end;
    VertStep := (AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) div RectVertLength;
    for I := 0 to RectVertLength - 1 do
    begin
      AddSimpleText(ReplCh, TraceCPP, 0, PixelToPinX(AObj.BoundsRect.Left) - OneLetterWidthInPins div 2, PixelToPinY(AObj.BoundsRect.Top + I * VertStep), AList);
      if RectHorzLength > 1 then
        AddSimpleText(ReplCh, TraceCPP, 0, PixelToPinX(AObj.BoundsRect.Right) - OneLetterWidthInPins div 2, PixelToPinY(AObj.BoundsRect.Top + I * VertStep), AList);
    end;
  end;
end;

procedure TRLDraftFilter.AddFillRect(AObj: TRLFillRectObject; AList: TObjectList);
var
  W, H, I, gh, chw, chh, cpp: Integer;
  gch: Ansichar;
  S: AnsiString;
begin
  if FFillArtMethod = fmNone then
    Exit;
  // se não for sólido  
  if AObj.Brush.Style <> MetaBrushStyleSolid then
    Exit;
  // se for de cor clara  
  with AObj.Brush.Color do
    if (Red + Green + Blue) / 3 > 127 then
      Exit;
  //    
  cpp := SelectFontCPP(DefaultFontName, DefaultFontSize);
  chw := CPPPins(cpp);
  chh := LPPPins(StandardLPP);
  W := (PixelToPinX(AObj.BoundsRect.Right) - PixelToPinX(AObj.BoundsRect.Left)) div chw;
  H := (PixelToPinY(AObj.BoundsRect.Bottom) - PixelToPinX(AObj.BoundsRect.Top)) div chh;
  if W > 0 then
  begin
    case FFillArtMethod of
      fmLetterX: gch := 'x';
      fmGraphicCharset: gch := #219;
    else
      gch := ' ';
    end;
    SetLength(S, W);
    FillChar(S[1], W, gch);
    AddSimpleText(S, cpp, 0, PixelToPinX(AObj.BoundsRect.Left), PixelToPinY(AObj.BoundsRect.Top - chh div 2), AList);
  end
  else if H > 0 then
  begin
    case FFillArtMethod of
      fmLetterX: gch := 'x';
      fmGraphicCharset: gch := #219;
    else
      gch := ' ';
    end;
    gh := (AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) div H;
    for I := 0 to H - 1 do
      AddSimpleText(gch, cpp, 0, PixelToPinX(AObj.BoundsRect.Left - chw div 2), PixelToPinY(AObj.BoundsRect.Top + I * gh), AList);
  end;
end;

procedure TRLDraftFilter.AddLine(AObj: TRLLineObject; AList: TObjectList);
begin
end;

// seleciona objetos interessantes e os ordena
procedure TRLDraftFilter.GetObjList(APage: TRLGraphicSurface; AList: TObjectList);
var
  obj: TRLGraphicObject;
  I: Integer;
begin
  AList.Clear;
  for I := 0 to APage.ObjectCount - 1 do
  begin
    obj := APage.Objects[I];
    if obj is TRLPixelObject then
      ///AddPixel(TRLPixelObject(obj),aList)
    else if obj is TRLLineObject then
      AddLine(TRLLineObject(obj), AList)
    else if obj is TRLRectangleObject then
      AddRectangle(TRLRectangleObject(obj), AList)
    else if obj is TRLTextObject then
      AddText(TRLTextObject(obj), AList)
    else if obj is TRLFillRectObject then
      AddFillRect(TRLFillRectObject(obj), AList)
    else if obj is TRLEllipseObject then
      ///AddEllipse(TRLEllipseObject(obj),aList)
    else if obj is TRLPolygonObject then
      ///AddPolygon(TRLPolygonObject(obj),aList)
    else if obj is TRLPolylineObject then
      ///AddPolyline(TRLPolylineObject(obj),aList)
    else if obj is TRLImageObject then
      AddImage(TRLImageObject(obj), AList)
    else if obj is TRLSetClipRectObject then
      ///AddSetClipRect(TRLSetClipRectObject(obj),aList)
    else if obj is TRLResetClipRectObject then
      ///AddResetClipRect(TRLResetClipRectObject(obj),aList);
  end;
end;

procedure TRLDraftFilter.InternalDrawPage(APage: TRLGraphicSurface);
type
  TRGB4 = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
var
  objlist: TObjectList;
  obj: TDraftObj;
  objpos: TRect;
  B: TBitmap;
  I, X: Integer;
  S: String;
begin
  objlist := TObjectList.Create;
  try
    // seleciona textos
    GetObjList(APage, objlist);
    // inicialização da impressora
    if FSendFirstReset then
      DeviceWrite(PrintCode('Reset'));
    FSendFirstReset := False;
    // reseta fonte
    SetPrintCompression(10, True);
    SetPrintStyle(0);
    // imprime textos e imagens
    for I := 0 to objlist.Count - 1 do
    begin
      obj := TDraftObj(objlist[I]);
      // posição do objeto em agulhas
      objpos := obj.PinBounds;
      // não pode imprimir antes da linha atual (ou fora da área imprimível)
      if objpos.Top < FCurrentPrintPos.Y then
        objpos.Top := FCurrentPrintPos.Y;
      //
      if obj is TDraftText then
      begin
        // compressão
        SetPrintCompression(TDraftText(obj).CPP);
        // posicionamento
        SetPrintPos(objpos.Top, objpos.Left);
        // acentos
        case FAccentMethod of
          amOverwrite: S := TranslateAccents(TDraftText(obj).Text, PrintCode('BS'));
          amTakeOut: S := RemoveAccents(TDraftText(obj).Text);
        else
          S := TDraftText(obj).Text;
        end;
        // envolve o texto com o estilo
        SetPrintStyle(TDraftText(obj).Style);
        DeviceWrite(S);
        SetPrintStyle(0);
        //
        Inc(FCurrentPrintPos.X, Length(TDraftText(obj).Text) * FCurrentCharWidth);
      end
      else if obj is TDraftImage then
      begin
        // posicionamento
        SetPrintPos(objpos.Top, objpos.Left);
        //
        B := TDraftImage(obj).Bitmap;
        DeviceWrite(PrintCode('RAW', B.Width));
        for X := 0 to B.Width - 1 do
          DeviceWrite(Char(Byte(GetBitmapPixel(B, X, 0, clWhite) = clBlack) * 128 + 
                           Byte(GetBitmapPixel(B, X, 1, clWhite) = clBlack) * 64 + 
                           Byte(GetBitmapPixel(B, X, 2, clWhite) = clBlack) * 32 + 
                           Byte(GetBitmapPixel(B, X, 3, clWhite) = clBlack) * 16 + 
                           Byte(GetBitmapPixel(B, X, 4, clWhite) = clBlack) * 8 + 
                           Byte(GetBitmapPixel(B, X, 5, clWhite) = clBlack) * 4 + 
                           Byte(GetBitmapPixel(B, X, 6, clWhite) = clBlack) * 2 + 
                           Byte(GetBitmapPixel(B, X, 7, clWhite) = clBlack) * 1));
        //                   
        FCurrentPrintPos.X := PixelToPinX(obj.PinBounds.Right);
      end;
    end;
    // reseta fonte
    SetPrintCompression(10);
  finally
    objlist.Free;
  end;
end;

procedure TRLDraftFilter.SetCommands(const Value: TStrings);
begin
  FCommands.Assign(Value);
end;

procedure TRLDraftFilter.SetPrinterFamily(const Value: TRLPrinterFamily);
begin
  if Value = FPrinterFamily then
    Exit;
  FPrinterFamily := Value;
  //
  DefaultCommands;
end;

procedure TRLDraftFilter.Loaded;
begin
  inherited;
  //
{$IfNDef MSWINDOWS}
  if (FDeviceKind = dkProgram) and (Copy(FDevicePath, 1, 5) = '/dev/') then
    FDevicePath := 'lpr -P%p %f';
{$endif};
end;

function TRLDraftFilter.GetOptionsLabel: String;
begin
  Result := GetLocalizeStr(LocaleStrings.LS_FormStr);
end;

function TRLDraftFilter.GetOptionIndex: Integer;
begin
  Result := FOptionIndex;
end;

function TRLDraftFilter.GetOptions: TStrings;
begin
  if FOptions = nil then
  begin
    FOptions := TStringList.Create;
    FOptions.Add(GetLocalizeStr(LocaleStrings.LS_DefaultStr));
    FOptions.Add(GetLocalizeStr('80 ' + LocaleStrings.LS_ColumnsStr));
    FOptions.Add(GetLocalizeStr('132 ' + LocaleStrings.LS_ColumnsStr));
  end;
  //
  Result := FOptions;
end;

procedure TRLDraftFilter.SetOptionIndex(const Value: Integer);
begin
  FOptionIndex := Value;
end;

procedure TRLDraftFilter.SetOrientation(AOrientation: TRLPageOrientation);
begin
  FOrientation := AOrientation;
  if FFormSelection = fsAccordingToOrientation then
    case FOrientation of
      poPortrait: FOptionIndex := 1;
      poLandscape: FOptionIndex := 2;
    end;
end;

procedure TRLDraftFilter.SetFormSelection(const Value: TRLFormSelection);
begin
  FFormSelection := Value;
  case FFormSelection of
    fsNone: ;
    fsAccordingToOrientation: case FOrientation of
                                poPortrait: FOptionIndex := 1;
                                poLandscape: FOptionIndex := 2;
                              end;
    fs80Cols: FOptionIndex := 1;
    fs132Cols: FOptionIndex := 2;
  end;
end;

function TRLDraftFilter.IsCustomPrinterFamily: Boolean;
begin
  Result := (FPrinterFamily = fmCustom);
end;

function TRLDraftFilter.IsCustomDevice: Boolean;
begin
{$ifndef LINUX}
  Result := (FDeviceKind <> dkPrinterPort);
{$else}
  Result := (FDeviceKind <> dkProgram) or (FDevicePath <> 'lpr -P%p %f');
{$endif};
end;

function TRLDraftFilter.IsCustomTextStyle: Boolean;
begin
  Result := (FTextDecoration = ddCustomized);
end;

procedure TRLDraftFilter.SetTextDecoration(const Value: TRLDraftTextDecoration);
begin
  if FTextDecoration = Value then
    Exit;
  FTextDecoration := Value;
  //
  case FTextDecoration of
    ddIncludeNone: FTextStyles := [];
    ddIncludeAll: FTextStyles := [tsItalic, tsBold, tsUnderline];
    ddCustomized: ;
  end;
end;

procedure TRLDraftFilter.SetTextStyle(const Value: TRLDraftTextStyles);
begin
  if FTextStyles = Value then
    Exit;
  FTextStyles := Value;
  //
  FTextDecoration := ddCustomized;
end;

end.

