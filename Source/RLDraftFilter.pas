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

{@unit RLDraftFilter - Implementa��o do filtro de impress�o draft. }
unit RLDraftFilter;

interface

uses
  SysUtils, Classes, Math, Contnrs, 
{$ifndef LINUX}
  Windows, WinSpool, ShellApi, 
{$else}
  Types, Libc, 
{$endif}
{$ifdef VCL}
  Graphics, RLMetaVCL, 
{$else}
  QGraphics, RLMetaCLX, 
{$endif}
  RLMetaFile, RLConsts, RLUtils, RLFilters, RLTypes, RLPrinters,
  RlCompilerConsts;

type
  {@type TRLDraftAccentMethod - Comportamento do filtro em rela��o aos caracteres acentuados.
   Algumas impressoras n�o suportam ou podem n�o estar adequadamente configuradas para imprimir
   caracteres acentuados.
   O m�todo de acentua��o do filtro pode ser:
   amOverwrite - O filtro provoca o retorno do carro para imprimir o caractere de acento sobre a letra;
   amTakeOut - O caractere � impresso sem o acento;
   amSustain - O caractere � enviado sem modifica��es (exige configura��o da impressora). :/}
  TRLDraftAccentMethod = (amOverwrite, amTakeOut, amSustain);

  {@type TRLDraftEjectMethod - Comportamento do filtro em rela��o ao salto de p�gina.
   Quando se trabalha com um tamanho espec�fico de formul�rio, pode ser necess�rio modificar a maneira
   como o filtro efetua os saltos de p�gina.
   O m�todo de salto do filtro pode ser:
   ejCompletePage - Envia c�digos de salto de linha at� completar a p�gina. A dist�ncia entre os p�ginas pode
   dilatar ou contrair ao longo da impress�o;
   ejForceWithCode - Envia c�digo de salto de p�gina para a impressora. Este � o melhor m�todo para formul�rios
   cont�nuos padr�o, por�m pode n�o servir para formul�rios de tamanho customizado;
   ejLeavePage - Deixa o carro da impressora na posi��o aonde parou. Pode ser utilizado com formul�rios sem picote
   como rolos por ex, ou quando se deseja economizar papel com testes. :/}
  TRLDraftEjectMethod = (ejCompletePage, ejForceWithCode, ejLeavePage);

  {@type TRLPrinterFamily - Fam�lia de impressoras.
   Al�m dos c�digos de programa��o, pode ser interessante conhecer algumas particularidades da impressora a
   ser utilizada e que n�o podem ser informadas somente com comandos, como a impress�o de gr�ficos, por exemplo.
   A fam�lia pode ser:
   fmCustom - C�digos de programa��o informados pelo usu�rio na prop Commands;
   fmEpsonLike - C�digos de programa��o da especifica��o EPSON/FX;
   fmESCP2 - C�digos de programa��o da especifica��o EPSON ESC/P2. :/}
  TRLPrinterFamily = (fmCustom, fmEpsonLike, fmESCP2);

  {@type TRLDeviceKind - Tipo de dispositivo de impress�o.
   Indica como o relat�rio deve ser despachado.
   Pode assumir um dos seguintes valores:
   dkPrinter - Utilizar o nome da impressora;
   dkPrinterPort - A prop DevicePath ser� preenchida em runtime de acordo com a impressora selecionada no
   di�logo de impress�o. Este � o padr�o mais recomendado para o Windows, pois o pr�prio sistema se
   encarregar� de descobrir o caminho para o dispositivo atrav�s de informa��es do SO (default);
   dkProgram - A prop DevicePath indica o nome de um programa spooler. Padr�o recomendado para o Linux;
   dkFileName - A prop DevicePath aponta para um nome de arquivo. Este arquivo poder� ser copiado para
   outra impressora, ou ser utilizado como debug. :/}
  TRLDeviceKind = (dkPrinter, dkPrinterPort, dkProgram, dkFileName);

  {@type TRLDitheringMethod - M�todo para impress�o de imagens.
   Esta propriedade indica que t�cnica deve ser utilizada para transformar imagens coloridas em pontos preto e branco.
   Pode ser um dos seguintes valores:
   dmNone - Indica que nenhuma imagem ser� impressa;
   dmColorTable - Tabela de associa��o de cores. Este m�todo geralmente apresenta os melhores resultados;
   dmErrorDiffusion - Difus�o de erros. :/}
  TRLDitheringMethod = (dmNone, dmColorTable, dmErrorDiffusion);

  {@type TRLLineDrawMethod - M�todo para impress�o de linhas e tra�os.
   Para imprimir linhas retas em modo texto, � necess�rio informar o conjunto de caracteres a utilizar.
   Pode ser um dos seguintes valores:
   ldNone - Nenhuma linha ou tra�o ser� impresso;
   ldMinusAndPipe - Linhas horizontais como sinal negativo "-" e verticais como pipes "|";
   ldGraphicCharset - Utilizar os conectores gr�ficos do padr�o ProPrinter (exige configura��o da impressora). :/}
  TRLLineDrawMethod = (ldNone, ldMinusAndPipe, ldGraphicCharset);

  {@type TRLFillArtMethod - M�todo para preenchimento de �reas.
   Define o m�todo para representa��o de ret�ngulos s�lidos ou linhas grossas em impressoras matriciais.
   Pode ser um dos seguintes valores:
   fmNone - Nenhum preenchimento ser� impresso;
   fmLetterX - A letra X ser� utilizada ;
   fmGraphicCharset - Utilizar os caracteres gr�ficos do padr�o ProPrinter (exige configura��o da impressora). :/}
  TRLFillArtMethod = (fmNone, fmLetterX, fmGraphicCharset);

  {@type TRLFormSelection - Sele��o da largura do formul�rio cont�nuo.
   Indica como ser� escolhido formul�rio em rela��o � largura.  
   Pode ser um dos seguintes valores:
   fsNone - Nenhuma adapta��o � feita e nenhum di�logo � exibido;
   fsAccordingToOrientation - O di�logo apresentar� as op��es de 80cols para Portrait e 132cols para Landscape,
   e o default ser� de acordo com a orienta��o do relat�rio;
   fs80Cols - O default ser� o formul�rio de 80cols;
   fs132Cols - O default ser� o formul�rio de 132cols. :/}
  TRLFormSelection = (fsNone, fsAccordingToOrientation, fs80Cols, fs132Cols);

  {@type TRLStretchCharWidth - M�todo de adapta��o do tamanho das fontes para o formul�rio escolhido.
   Pode ser um dos seguintes valores:
   scNone - Nenhuma adapta��o ser� feita;
   scEnlargementsOnly - A fonte dever� ser aumentada quando o formul�rio for maior;
   scShrinksOnly - A fonte dever� ser encolhida quando o formul�rio for menor;
   scAlways - A fonte dever� ser aumentada ou encolhida de acordo com a escolha do formul�rio. :/}
  TRLStretchCharWidth = (scNone, scEnlargementsOnly, scShrinksOnly, scAlways);

  {@type TRLDraftTextDecoration - Define como os efeitos de fonte ser�o implementados.
   Pode ser um dos seguintes valores:
   ddIncludeNone - Nenhum efeito � realizado;
   ddIncludeAll - Todos os efeitos s�o realizados;
   ddCustomized - Somente os efeitos indicados na prop TextStyles. :/}
  TRLDraftTextDecoration = (ddIncludeNone, ddIncludeAll, ddCustomized);

  {@type TRLDraftTextStyles - Indica que efeitos de fonte devem ser realizados ou ignorados pelo filtro.
   Pode ser nenhum ou uma combina��o dos seguintes valores:
   tsItalic - Efeito it�lico (fonte inclinada);
   tsBold - Efeito negrito (passada dupla);
   tsUnderline - Efeito sublinhado. :/}
  TRLDraftTextStyles = set of (tsItalic, tsBold, tsUnderline);

  {@type TRLCPPSelection - Indica a pol�tica de compress�o dos caracteres, e pode ser fixa ou vari�vel.
   Pode ser um dos seguintes valores:
   csAutomatic - A compress�o varia de acordo com a fonte de cada label;
   csFixed5CPP - Compress�o fixa em 5cpp;
   csFixed10CPP - Compress�o fixa em 10cpp;
   csFixed12CPP - Compress�o fixa em 12cpp;
   csFixed17CPP - Compress�o fixa em 17cpp;
   csFixed20CPP - Fixa Compress�o fm 20cpp. :/}
  TRLCPPSelection = (csAutomatic, csFixed5CPP, csFixed10CPP, csFixed12CPP, csFixed17CPP, csFixed20CPP);

  TDraftObj = class;
  TDraftText = class;
  TDraftImage = class;

  { TRLDraftFilter }

  {@class TRLDraftFilter - Filtro de impress�o para impressoras matriciais.
   Este filtro age substituindo os comandos gr�ficos que seriam enviados ao driver da impressora por c�digos de
   programa��o, os quais s�o enviados diretamente para o dispositivo de impress�o ou programa spooler. Com isso se
   consegue imprimir o mesmo relat�rio em impressoras de tecnologias diferentes, mantendo-se o design gr�fico original
   com impressoras a jato e a laser, e alta velocidade em uma matricial.
   H� v�rias propriedades e maneiras de se conseguir bons resultados, equilibrando velocidade e qualidade de impress�o.   
   Nota: O algor�tmo do filtro conseguir� fazer uma melhor aproxima��o de fontes TrueType vari�veis do que de fontes
   fixas. Portanto, n�o � necess�rio desenhar os relat�rios em uma fonte com pitch fixo, como: Courier ou Terminal.
   @ancestor TRLCustomPrintFilter.
   @links TRLHTMLFilter, TRLRichFilter.
   @pub }
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
    function PrintCode(const ACommand: String; AParameter: Integer = 0): String;
    procedure SetPrinterFamily(const Value: TRLPrinterFamily);
    procedure DeviceWrite(const AData: String);
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

    {@method DefaultCommands - Preenche lista de c�digos de programa��o de acordo com a fam�lia de impressoras
     escolhida. :/}
    procedure DefaultCommands;

  published
  
    {@method SetOrientation - Seleciona a orienta��o de papel padr�o.
     @links TRLPageOrientation. :/}
    procedure SetOrientation(AOrientation: TRLPageOrientation); override;

    {@prop Commands - C�digos de programa��o para a impressora.
     Esta propriedade especifica os c�digos de compress�o, efeitos de fonte e controle de p�gina nativos da impressora.
     Os c�digos da configura��o default s�o para impressoras do tipo EPSON. � poss�vel por�m adapt�-los a praticamente
     qualquer impressora matricial.
     Cada linha deve indicar o comando e a sequencia de bytes na seguinte forma: "NOME=Asc1,'Chr1',Asc2,'ChrN',Asc2".
     Pode conter as seguintes vari�veis: 
     CR - Retorno do carro (ex.: CR=13);
     LF - Avan�o de linha (ex.: LF=10);
     BS - Retrocesso de 1 caractere (ex.: BS=8);
     FF - Avan�o de p�gina (ex.: FF=12);
     Reset - Inicializa��o da impressora (ex.: RESET=27,'@');
     MicroOn - Micro salto de N/12 de linha (ex.: MicroOn=27,'A',#);
     MicroOff - Volta ao salto normal de linha (ex.: MicroOff=27,'2');
     Space - O caractere de espa�o (ex.: Space=32);
     CPP10 - Compress�o a 10 caracteres/polegada (ex.: CPP10=27,'P',18);
     CPP12 - Compress�o a 12 caracteres/polegada (ex.: CPP12=27,'M',18);
     CPP17 - Compress�o a 17 caracteres/polegada (ex.: CPP17=27,'P',15);
     CPP20 - Compress�o a 20 caracteres/polegada (ex.: CPP20=27,'M',15);
     ExpandOn - Liga modo expandido (ex.: ExpandOn=27,'W',1);
     ExpandOff - Modo expandido desligado (ex.: ExpandOff=27,'W',0);
     BoldOn - Liga modo negrito (ex.: BoldOn=27,'G');
     BoldOff - Modo negrito desligado (ex.: BoldOff=27,'H');
     ItalicOn - Liga modo it�lico (ex.: ItalicOn=27,'4');
     ItalicOff - Modo it�lico desligado (ex.: ItalicOff=27,'5');
     UnderlineOn - Liga modo it�lico (ex.: UnderlineOn=27,'-1');
     UnderlineOff - Modo it�lico desligado (ex.: UnderlineOff=27,'-0');
     RAW - Envio de sequ�ncia de bytes para impress�o de gr�ficos (ex.: RAW=27,'L',#l,#h).
     @links DefaultCommands, PrinterFamily. :/}
    property Commands: TStrings read FCommands write SetCommands stored IsCustomPrinterFamily;

    {@prop DriverName - Nome de arquivo "driver" contendo os c�digos de programa��o da impressora.
     Utilize esta propriedade quando quiser manter os c�digos de programa��o da impressora
     em um arquivo texto externo ao programa. A sintaxe do arquivo � a mesma da propriedade
     Commands.
     @links Commands, DefaultCommands. :/}
    property DriverName: String read FDriverName write FDriverName;

    {@prop DeviceKind - Tipo de dispositivo de impress�o.
     Indica que tipo de dispositivo est� identificado na propriedade DevicePath.
     @links TRLDeviceKind, DevicePath. :/}
    property DeviceKind: TRLDeviceKind read FDeviceKind write FDeviceKind stored IsCustomDevice;

    {@prop DevicePath - Caminho para o dispositivo de impress�o.
     Esta propriedade indica o nome ou o caminho do dispositivo de impress�o ou programa spooler.
     No Windows pode-se utilizar um dos dispositivos padr�es: PRN, LPT1, LPT2 etc, ou um caminho de
     rede no formato "\\computador\impressora". O dispositivo PRN � especialmente interessante, pois
     sempre representa a impressora atualmente selecionada pelo sistema.
     No Linux pode-se informar tanto um caminho para um dispositivo (ex.: "/dev/lp0") como para um
     programa de controle de spool como o lpr (ex.: "lpr -P%p %f"). Neste �ltimo caso, "%p" representa
     o nome de uma impressora v�lida cadastrada pelo linuxconf, e "%f" o nome de um arquivo tempor�rio
     gerado pelo FR.
     @links DeviceKind. :/}
    property DevicePath: String read FDevicePath write FDevicePath stored IsCustomDevice;

    {@prop AccentMethod - Comportamento do filtro em rela��o a caracteres acentuados.
     Algumas impressoras n�o suportam ou podem n�o estar adequadamente configuradas para imprimir
     caracteres acentuados.
     @links TRLDraftAccentMethod. :/}
    property AccentMethod: TRLDraftAccentMethod read FAccentMethod write FAccentMethod default amOverwrite;

    {@prop EjectMethod - Comportamento do filtro em rela��o aos saltos de p�ginas.
     Em alguns casos quando se tem um tamanho espec�fico de formul�rio pode ser necess�rio
     modificar a maneira como o filtro efetua os saltos de p�gina.
     @links TRLDraftEjectMethod, Commands. :/}
    property EjectMethod: TRLDraftEjectMethod read FEjectMethod write FEjectMethod default ejCompletePage;

    {@prop PrinterFamily - Fam�lia de impressoras.
     �s vezes � desej�vel conhecer algumas particularidades da impressora a ser utilizada e que
     n�o podem ser informadas somente com comandos, como a impress�o de gr�ficos.
     @links TRLPrinterFamily. :/}
    property PrinterFamily: TRLPrinterFamily read FPrinterFamily write SetPrinterFamily default fmEpsonLike;

    {@prop DitheringMethod - M�todo para impress�o de imagens.
     Esta propriedade indica que m�todo deve ser utilizado para transformar imagens coloridas
     em preto e branco.
     @links TRLDitheringMethod. :/}
    property DitheringMethod: TRLDitheringMethod read FDitheringMethod write FDitheringMethod default dmColorTable;

    {@prop LineDrawMethod - M�todo para impress�o de linhas.
     Esta propriedade indica que m�todo deve ser utilizado para desenhar linhas numa impressora
     matricial.
     @links TRLLineDrawMethod. :/}
    property LineDrawMethod: TRLLineDrawMethod read FLineDrawMethod write FLineDrawMethod default ldMinusAndPipe;

    {@prop FillArtMethod - M�todo para o preenchimento de ret�ngulos ou tra�os grossos. :/}
    property FillArtMethod: TRLFillArtMethod read FFillArtMethod write FFillArtMethod default fmNone;

    {@prop FormSelection - Pol�tica de sele��o de tamanho para formul�rios cont�nuos.
     @links TRLFormSelection. :/}
    property FormSelection: TRLFormSelection read FFormSelection write SetFormSelection default fsAccordingToOrientation;

    {@prop StretchCharWidth - M�todo de adapta��o do tamanho das fontes.
     @links TRLStretchCharWidth. :/}
    property StretchCharWidth: TRLStretchCharWidth read FStretchCharWidth write FStretchCharWidth default scShrinksOnly;

    {@prop TextDecoration - Decora��o do texto.
     @links TRLDraftTextDecoration. :/}
    property TextDecoration: TRLDraftTextDecoration read FTextDecoration write SetTextDecoration default ddIncludeAll;
    
    {@prop TextStyles - Estilos de texto.
     @links TRLDraftTextStyles. :/}
    property TextStyles: TRLDraftTextStyles read FTextStyles write SetTextStyle stored IsCustomTextStyle;

    {@prop CPPSelection - Fixa uma compress�o padr�o para todo o relat�rio.
     @links Commands, TRLCPPSelection. :/}
    property CPPSelection: TRLCPPSelection read FCPPSelection write FCPPSelection default csAutomatic;

    property DisplayName;
  end;
  {/@class}

  { TDraftObj }

  TDraftObj = class
  public
    PinBounds: TRect;
  end;

  { TDraftText }

  TDraftText = class(TDraftObj)
  public
    CPP: Integer;
    Style: Byte;
    Text: AnsiString;
  end;

  { TDraftImage }

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
  AccentLetters = '���������������������������������������������Ǻ�';
  NormalLetters = 'aaaaaAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUcCoa';
  AccentChars = '''`~^�''`~^�''`^�''`^�''`^�''`^�''`~^�''`~^�''`^�''`^�,,__';

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

function TranslateAccents(const AString, ABS: String): String;
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

function RemoveAccents(const AString: String): String;
var
  I, P: Integer;
begin
  Result := AString;
  for I := 1 to Length(AString) do
  begin
    P := Pos(AString[I], AccentLetters);
    if P > 0 then
      Result[I] := NormalLetters[P];
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
  if AY < ABitmap.Height then
    with TRGBArray(ABitmap.ScanLine[AY]^)[AX] do
      Result := RGB(rgbRed, rgbGreen, rgbBlue)
  else
    Result := ADefault;
end;
{$IFDEF DELPHIXE2_UP}
function LinePrinterStart(const PrnName, DocName: String): NativeUInt;
{$ELSE}
function LinePrinterStart(const PrnName, DocName: String): Cardinal;
{$ENDIF}
var
  di: TDocInfo1;
begin
  FillChar(di, SizeOf(di), 0);
  di.pDocName := PChar(DocName);
  di.pOutputFile := nil;
  di.pDatatype := 'RAW';
  OpenPrinter(PChar(PrnName), Result, nil);
  StartDocPrinter(Result, 1, @di);
  StartPagePrinter(Result);
end;

procedure LinePrinterWrite(PrnHandle: Cardinal; const Text: String);
var
  Len: Cardinal;
  Aux: AnsiString;
begin
  Aux:=Ansistring(Text);
  Len := Length(Aux);
  if Len > 0 then
  begin
    WritePrinter(PrnHandle, @Aux[1], Len, Len);
  end;
end;

procedure LinePrinterEnd(PrnHandle: Cardinal);
begin
  EndPagePrinter(PrnHandle);
  EndDocPrinter(PrnHandle);
  ClosePrinter(PrnHandle);
end;

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
  // medidas padr�o papel carta
  FCurrentLPP := StandardLPP;
  FCurrentCPP := StandardCPP;
  FCurrentBoldState := False;
  FCurrentItalicState := False;
  FCurrentUnderlineState := False;
  FCurrentCharWidth := CPPPins(FCurrentCPP);
  FCurrentCharHeight := LPPPins(FCurrentLPP);
  //
  if FDeviceKind = dkPrinter then
    FPrinterHandle := LinePrinterStart(RLPrinter.PrinterName, 'FortesReport')
  else
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
{$ifndef LINUX}
var
  par: String;
  I: Integer;
{$endif}
begin
  NewPage;
  //
  if FDeviceKind = dkPrinter then
    LinePrinterEnd(FPrinterHandle)
  else
  begin
    CloseFile(FDeviceHandle);
    case FDeviceKind of
      dkPrinterPort: ;
      dkProgram: 
      begin
        cmd := FDevicePath;
        cmd := StringReplace(cmd, '%p', RLPrinter.PrinterName, [rfReplaceAll, rfIgnoreCase]);
        cmd := StringReplace(cmd, '%f', FDeviceFileName, [rfReplaceAll, rfIgnoreCase]);
{$ifndef LINUX}
        I := Pos(' ', cmd);
        if I = 0 then
          I := Length(cmd) + 1;
        par := Copy(cmd, I + 1, Length(cmd));
        cmd := Copy(cmd, 1, I - 1);
        ShellExecute(0, 'open', PChar(cmd), PChar(par), nil, SW_SHOWNORMAL);
{$else}
        Libc.system(PChar(cmd));
{$endif};
      end;
      dkFileName: ;
    end;
  end;
end;

procedure TRLDraftFilter.InternalNewPage;
begin
  case FEjectMethod of
    ejCompletePage: SetPrintPos(PixelToPinY(FPrintSize.Y), PixelToPinX(0)); // posicionamento vertical da proxima p�gina em agulhas
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

function TRLDraftFilter.PrintCode(const ACommand: String; AParameter: Integer = 0): String;
var
  S, chr: String;
  I: Integer;
begin
  S := FCommands.Values[ACommand];
  Result := '';
  if S = '' then
    Exit;
  I := 0;
  repeat
    Inc(I);
    chr := Token(S, I, ',');
    if chr = '' then
      Break
    else if (chr[1] = '''') and (chr[Length(chr)] = '''') then
      Result := Result + Copy(chr, 2, Length(chr) - 2)
    else if chr = '#' then
      Result := Result + Char(AParameter)
    else if LowerCase(chr) = '#l' then
      Result := Result + Char(AParameter mod 256)
    else if LowerCase(chr) = '#h' then
      Result := Result + Char(AParameter div 256)
    else if chr = '$' then
      Result := Result + IntToStr(AParameter)
    else
      Result := Result + Char(strtoint(chr));
  until False;
end;

procedure TRLDraftFilter.DeviceWrite(const AData: String);
var
  Aux: AnsiString;
begin
  if AData <> '' then
    if FDeviceKind = dkPrinter then
      LinePrinterWrite(FPrinterHandle, AData)
    else
    begin
      Aux := AnsiString(AData);
      BlockWrite(FDeviceHandle, Aux[1], Length(Aux));
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
  qcol := qpol * StandardCPP; // transforma polegadas em colunas de acordo com o cpp padr�o
  Result := Trunc(qcol * PinsPerCol); // transforma colunas em pinos
end;

function TRLDraftFilter.PixelToPinY(Y: Integer): Integer;
var
  qpol, qlin: Double;
begin
  Dec(Y, FPrintCut.Y);
  qpol := Y / ScreenPPI; // transforma Y em polegadas
  qlin := qpol * StandardLPP; // transforma polegadas em linhas de acordo com o lpp padr�o
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
  // a largura m�dia de um caractere em pixels � dada pela largura da amostra em
  // pixels dividida pela largura da amostra em caracteres com a fonte indicada
  // um fator "m�gico" foi calculado atrav�s de testes para chegar ao valor ideal  
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
  // for�a o reposicionamento do carro para a nova compress�o
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
  // it�lico
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
  S := AObj.DisplayText;
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
        tempbmp.Canvas.StretchDraw(Rect(0, 0, tempbmp.Width, tempbmp.Height), thegraphic);
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
  // se n�o for s�lido
  if AObj.Pen.Style <> MetaPenStyleSolid then
    Exit;
  // se for de cor clara
  with AObj.Pen.Color do
    if (Red + Green + Blue) / 3 > 127 then
      Exit;
  // escolhe o cpp default para o ret�ngulo
  TraceCPP := SelectFontCPP(DefaultFontName, DefaultFontSize);
  // tamanho de um caractere em agulhas no cpp padr�o
  OneLetterWidthInPins := CPPPins(TraceCPP);
  OneLetterHeightInPins := LPPPins(StandardLPP);
  // calcula as dimens�es do retangulo em caracteres
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
  // se n�o for s�lido  
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
    // inicializa��o da impressora
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
      // posi��o do objeto em agulhas
      objpos := obj.PinBounds;
      // n�o pode imprimir antes da linha atual (ou fora da �rea imprim�vel)
      if objpos.Top < FCurrentPrintPos.Y then
        objpos.Top := FCurrentPrintPos.Y;
      //
      if obj is TDraftText then
      begin
        // compress�o
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
{$ifdef LINUX}
  if (FDeviceKind = dkProgram) and (Copy(FDevicePath, 1, 5) = '/dev/') then
    FDevicePath := 'lpr -P%p %f';
{$endif};
end;

function TRLDraftFilter.GetOptionsLabel: String;
begin
  Result := LocaleStrings.LS_FormStr;
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
    FOptions.Add(LocaleStrings.LS_DefaultStr);
    FOptions.Add('80 ' + LocaleStrings.LS_ColumnsStr);
    FOptions.Add('132 ' + LocaleStrings.LS_ColumnsStr);
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

