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

{@unit RLXLSFilter - Implementação do filtro para criação de planilhas do Excel. }
unit RLXLSFilter;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  SysUtils, Classes, Contnrs,
  {$IfDef FPC}
   LCLIntf, LCLType,
  {$EndIf}
  {$IfDef CLX}
   QTypes, QGraphics, RLMetaCLX,
  {$Else}
   Types, Graphics, RLMetaVCL,
   {$IfNDef FPC}
    RlCompilerConsts,
   {$EndIf}
  {$EndIf}
  {$IfDef NATIVEEXCEL}
   nExcel,
  {$EndIf}
  Math, DateUtils, StrUtils,
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFilters;

const
  XLSMaxDefaultColors = 16;
  XLSMaxRowsInSheet = 65536;
  XLSMaxRowsInBlock = 32;
  XLSMaxCellsInRow = 256;
  XLSMaxColorsInPalette = 56;

const
  XLSDefaultColorPalette: packed array[0..XLSMaxColorsInPalette - 1] of TColor = (
    $000000, $FFFFFF, $0000FF, $00FF00, $FF0000, $00FFFF, $FF00FF, $FFFF00, $000080, 
    $008000, $800000, $008080, $800080, $808000, $C0C0C0, $808080, $FF9999, $663399, 
    $CCFFFF, $FFFFCC, $660066, $8080FF, $CC6600, $FFCCCC, $800000, $FF00FF, $00FFFF, 
    $FFFF00, $800080, $000080, $808000, $FF0000, $FFCC00, $FFFFCC, $CCFFCC, $99FFFF, 
    $FFCC99, $CC99FF, $FF99CC, $99CCFF, $FF6633, $CCCC33, $00CC99, $00CCFF, $0099FF, 
    $0066FF, $996666, $969696, $663300, $669933, $003300, $003333, $003399, $663399, 
    $993333, $333333);

  XLSDefaultColors: array[0..XLSMaxDefaultColors - 1] of Integer = (
    clWhite, clBlack, clSilver, clGray, clRed, clMaroon, clYellow, clOlive,
    clLime, clGreen, clAqua, clTeal, clBlue, clNavy, clFuchsia, clPurple);
  
const
  SheetTabsIncrement = 10 * 1024;
  
type
  TRLXLSBiff8BOF = packed record
    vers: Word;
    dt: Word;
    rupBuild: Word;
    rupYear: Word;
    bfh: Cardinal;
    sfo: Cardinal;
  end;

  TRLXLSBiff8ColumnInfo = packed record
    colFirst: Word;
    colLast: Word;
    coldx: Word;
    ixfe: Word;
    grbit: Word;
    res1: Byte;
  end;

  TRLXLSBiff8XF = packed record
    ifnt: Word;
    ifmt: Word;
    Opt1: Word;
    Opt2: Byte;
    trot: Byte;
    Opt3: Word;
    Borders1: Word;
    Borders2: Word;
    Borders3: Cardinal;
    Colors: Word;
  end;

  TRLXLSBiff8Dimensions = packed record
    rwMic: Cardinal;
    rwMac: Cardinal;
    colMic: Word;
    colMac: Word;
    Res1: Word;
  end;

  TRLXLSBiff8Row = packed record
    rw: Word;
    colMic: Word;
    colMac: Word;
    miyRw: Word;
    irwMac: Word;
    Res1: Word;
    grbit: Word;
    ixfe: Word;
  end;

  TRLXLSBiff8InterfaceHeader = packed record
    cv: Word;
  end;

  TRLXLSBiff8MMS = packed record
    caitm: Byte;
    cditm: Byte;
  end;

  TRLXLSBiff8CodePage = packed record
    cv: Word;
  end;

  TRLXLSBiff8FNGroupCount = packed record
    cFnGroup: Word;
  end;

  TRLXLSBiff8WindowProtect = packed record
    FLockWn: Word;
  end;

  TRLXLSBiff8Protect = packed record
    FLock: Word;
  end;

  TRLXLSBiff8Password = packed record
    wPassword: Word;
  end;

  TRLXLSBiff8BACKUP = packed record
    FBackupFile: Word;
  end;

  TRLXLSBiff8HIDEOBJ = packed record
    FHideObj: Word;
  end;

  TRLXLSBiff81904 = packed record
    f1904: Word;
  end;

  TRLXLSBiff8PRECISION = packed record
    FFullPrec: Word;
  end;

  TRLXLSBiff8BOOKBOOL = packed record
    FNoSaveSupp: Word;
  end;

  TRLXLSBiff8FONT = packed record
    dyHeight: Word;
    grbit: Word;
    icv: Word;
    bls: Word;
    sss: Word;
    uls: Byte;
    bFamily: Byte;
    bCharSet: Byte;
    Res1: Byte;
    cch: Byte;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8FONT = ^TRLXLSBiff8FONT;

  TRLXLSBiff8FORMAT = packed record
    ifmt: Word;
    cch: Word;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8FORMAT = ^TRLXLSBiff8FORMAT;

  TRLXLSBiff8COUNTRY = packed record
    iCountryDef: Word;
    iCountryWinIni: Word;
  end;

  TRLXLSBiff8INDEX = packed record
    Res1: Cardinal;
    rwMic: Cardinal;
    rwMac: Cardinal;
    Res2: Cardinal;
  end;
  PRLXLSBiff8INDEX = ^TRLXLSBiff8INDEX;

  TRLXLSBiff8CALCMODE = packed record
    FAutoRecalc: Word;
  end;

  TRLXLSBiff8CALCCOUNT = packed record
    cIter: Word;
  end;

  TRLXLSBiff8REFMODE = packed record
    FRefA1: Word;
  end;

  TRLXLSBiff8ITERATION = packed record
    FIter: Word;
  end;

  TRLXLSBiff8DELTA = packed record
    numDelta: Int64; 
  end;

  TRLXLSBiff8SAVERECALC = packed record
    FSaveRecalc: Word;
  end;

  TRLXLSBiff8PRINTHEADERS = packed record
    FPrintRwCol: Word;
  end;

  TRLXLSBiff8PRINTGRIDLINES = packed record
    FPrintGrid: Word;
  end;

  TRLXLSBiff8GRIDSET = packed record
    FGridSet: Word;
  end;

  TRLXLSBiff8GUTS = packed record
    dxRwGut: Word;
    dyColGut: Word;
    iLevelRwMac: Word;
    iLevelColMac: Word;
  end;

  TRLXLSBiff8DEFAULTROWHEIGHT = packed record
    grbit: Word;
    miyRw: Word;
  end;

  TRLXLSBiff8WSBOOL = packed record
    grbit: Word;
  end;

  TRLXLSBiff8HEADER = packed record
    cch: Word;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8HEADER = ^TRLXLSBiff8HEADER;

  TRLXLSBiff8FOOTER = packed record
    cch: Word;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8FOOTER = ^TRLXLSBiff8FOOTER;

  TRLXLSBiff8HCENTER = packed record
    FHCenter: Word;
  end;

  TRLXLSBiff8VCENTER = packed record
    FVCenter: Word;
  end;

  TRLXLSBiff8DEFCOLWIDTH = packed record
    cchdefColWidth: Word;
  end;

  TRLXLSBiff8WRITEACCESS = packed record
    stName: array[0..111] of Byte;
  end;

  TRLXLSBiff8DOUBLESTREAMFILE = packed record
    FDSF: Word;
  end;

  TRLXLSBiff8PROT4REV = packed record
    FRevLock: Word;
  end;

  TRLXLSBiff8PROT4REVPASS = packed record
    wRevPass: Word;
  end;

  TRLXLSBiff8WINDOW1 = packed record
    xWn: Word;
    yWn: Word;
    dxWn: Word;
    dyWn: Word;
    grbit: Word;
    itabCur: Word;
    itabFirst: Word;
    ctabSel: Word;
    wTabRatio: Word;
  end;

  TRLXLSBiff8REFRESHALL = packed record
    FRefreshAll: Word;
  end;

  TRLXLSBiff8USESELFS = packed record
    FUsesElfs: Word;
  end;

  TRLXLSBiff8PALETTE = packed record
    ccv: Word;
    colors: array[0..XLSMaxColorsInPalette - 1] of Cardinal;
  end;

  TRLXLSBiff8BOUNDSHEET = packed record
    lbPlyPos: Cardinal;
    grbit: Word;
    cch: Byte;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8BOUNDSHEET = ^TRLXLSBiff8BOUNDSHEET;

  TRLXLSBiff8WINDOW2 = packed record
    grbit: Word;
    rwTop: Word;
    colLeft: Word;
    icvHdr: Cardinal;
    wScaleSLV: Word;
    wScaleNormal: Word;
    Res1: Cardinal;
  end;

  TRLXLSBiff8SELECTION = packed record
    pnn: Byte;
    rwAct: Word;
    colAct: Word;
    irefAct: Word;
    cref: Word;
  end;
  PRLXLSBiff8SELECTION = ^TRLXLSBiff8SELECTION;

  TRLXLSBiff8DBCELL = packed record
    dbRtrw: Cardinal;
  end;

  TRLXLSBiff8DBCELLCellsOffsArray = array[0..XLSMaxCellsInRow - 1] of Word;

  TRLXLSBiff8DBCELLfull = packed record
    dbRtrw: Cardinal;
    cellsOffs: TRLXLSBiff8DBCELLCellsOffsArray;
  end;

  TRLXLSBiff8MERGErec = packed record
    top: Word;
    bottom: Word;
    left: Word;
    right: Word;
  end;
  PRLXLSBiff8MERGErec = ^TRLXLSBiff8MERGErec;

  TRLXLSBiff8MERGE = packed record
    cnt: Word; 
  end;
  PRLXLSBiff8MERGE = ^TRLXLSBiff8MERGE;

  TRLXLSBiff8LABEL = packed record
    rw: Word;
    col: Word;
    ixfe: Word;
    cch: Word;
    cchgrbit: Byte;
  end;
  PRLXLSBiff8LABEL = ^TRLXLSBiff8LABEL;

  TRLXLSBiff8BLANK = packed record
    rw: Word;
    col: Word;
    ixfe: Word;
  end;

  TRLXLSBiff8MULBLANK = packed record
    rw: Word;
    colFirst: Word;
  end;
  PRLXLSBiff8MULBLANK = ^TRLXLSBiff8MULBLANK;

  TRLXLSBiff8SETUP = packed record
    iPaperSize: Word;
    iScale: Word;
    iPageStart: Word;
    iFitWidth: Word;
    iFitHeight: Word;
    grbit: Word;
    iRes: Word;
    iVRes: Word;
    numHdr: Double;
    numFtr: Double;
    iCopies: Word;
  end;

  TRLXLSBiff8SST = packed record
    cstTotal: Cardinal;
    cstUnique: Cardinal;
  end;
  PRLXLSBiff8SST = ^TRLXLSBiff8SST;

  TRLXLSBiff8EXTSST = packed record
    Dsst: Word;
  end;
  PRLXLSBiff8EXTSST = ^TRLXLSBiff8EXTSST;

  TRLXLSBiff8ISSTINF = packed record
    ib: Cardinal;
    cb: Word;
    res1: Word;
  end;
  PRLXLSBiff8ISSTINF = ^TRLXLSBiff8ISSTINF;

  TRLXLSBiff8LABELSST = packed record
    rw: Word;
    col: Word;
    ixfe: Word;
    isst: Cardinal;
  end;

  TRLXLSBiff8LEFTMARGIN = packed record
    num: Double;
  end;

  TRLXLSBiff8RIGHTMARGIN = packed record
    num: Double;
  end;

  TRLXLSBiff8TOPMARGIN = packed record
    num: Double;
  end;

  TRLXLSBiff8BOTTOMMARGIN = packed record
    num: Double;
  end;

  TRLXLSBiff8NUMBER = packed record
    rw: Word;
    col: Word;
    ixfe: Word;
    num: Double;
  end;

type
  TRLXLSWorkbook = class;
  TRLXLSWorksheet = class;
  TRLXLSRow = class;
  TRLXLSCol = class;
  TRLXLSFilter = class;

  {@type TRLXLSCellDataType - Tipo de dado de uma célula ou faixa de células.
   Pode ser:
   ctNumber - A célula é um valor e pode ser envolvido em cálculos;
   ctString - O conteúdo da célula é um texto. :/}
  TRLXLSCellDataType = (ctNumber, ctString, ctDate, ctTime);

  TRLXLSLineStyleType = (lsNone, lsThin, lsMedium, lsDashed, lsDotted, lsThick, lsDouble, 
    lsHair, lsMediumDashed, lsDashDot, lsMediumDashDot, lsDashDotDot, 
    lsMediumDashDotDot, lsSlantedDashDot);

  TRLXLSWeightType = (weHairline, weThin, weMedium, weThick);

  TRLXLSBorderType = (bdDiagonalDown, bdDiagonalUp, bdEdgeBottom, bdEdgeLeft, 
    bdEdgeRight, bdEdgeTop);

  TRLXLSHorizontalAlignmentType = (haGeneral, haLeft, haCenter, haRight, haFill,
    haJustify, haCenterAcrossSelection);

  TRLXLSVerticalAlignmentType = (vaTop, vaCenter, vaBottom, vaJustify);

  TRLXLSOrderType = (odDownThenOver, odOverThenDown);

  TRLXLSOrientationType = (orPortrait, orLandscape);

  TRLXLSPrintErrorsType = (peBlank, peDash, peDisplayed, peNA);

  TRLXLSFillPattern = (fpNone, fpAutomatic, fpChecker, fpCrissCross, fpDown, fpGray8, 
    fpGray16, fpGray25, fpGray50, fpGray75, fpGrid, fpHorizontal, 
    fpLightDown, fpLightHorizontal, fpLightUp, fpLightVertical, 
    fpSemiGray75, fpSolid, fpUp, fpVertical);
  
  TRLXLSPaperSizeType = (
    szPaperOther, 
    szPaperLetter, {8 1/2 x 11"}
    szPaperLetterSmall, {8 1/2 x 11"}
    szPaperTabloid, {11 x 17"}
    szPaperLedger, {17 x 11"}
    szPaperLegal, {8 1/2 x 14"}
    szPaperStatement, {5 1/2 x 8 1/2"}
    szPaperExecutive, {7 1/4 x 10 1/2"}
    szPaperA3, {297 x 420 ìì}
    szPaperA4, {210 x 297 ìì}
    szPaperA4SmallSheet, {210 x 297 ìì}
    szPaperA5, {148 x 210 ìì}
    szPaperB4, {250 x 354 ìì}
    szPaperB5, {182 x 257 ìì}
    szPaperFolio, {8 1/2 x 13"}
    szPaperQuartoSheet, {215 x 275 ìì}
    szPaper10x14, {10 x 14"}
    szPaper11x17, {11 x 17"}
    szPaperNote, {8 1/2 x 11"}
    szPaper9Envelope, {3 7/8 x 8 7/8"}
    szPaper10Envelope, {4 1/8  x 9 1/2"}
    szPaper11Envelope, {4 1/2 x 10 3/8"}
    szPaper12Envelope, {4 3/4 x 11"}
    szPaper14Envelope, {5 x 11 1/2"}
    szPaperCSheet, {17 x 22"}
    szPaperDSheet, {22 x 34"}
    szPaperESheet, {34 x 44"}
    szPaperDLEnvelope, {110 x 220 ìì}
    szPaperC5Envelope, {162 x 229 ìì}
    szPaperC3Envelope, {324 x 458 ìì}
    szPaperC4Envelope, {229 x 324 ìì}
    szPaperC6Envelope, {114 x 162 ìì}
    szPaperC65Envelope, {114 x 229 ìì}
    szPaperB4Envelope, {250 x 353 ìì}
    szPaperB5Envelope, {176 x 250 ìì}
    szPaperB6Envelope, {176 x 125 ìì}
    szPaperItalyEnvelope, {110 x 230 ìì}
    szPaperMonarchEnvelope, {3 7/8 x 7 1/2"}
    szPaper63_4Envelope, {3 5/8 x 6 1/2"}
    szPaperUSStdFanfold, {14 7/8 x 11"}
    szPaperGermanStdFanfold, {8 1/2 x 12"}
    szPaperGermanLegalFanfold, {8 1/2 x 13"}
    szPaperB4_ISO, {250 x 353 ìì}
    szPaperJapanesePostcard, {100 x 148 ìì}
    szPaper9x11, {9 x 11"}
    szPaper10x11, {10 x 11"}
    szPaper15x11, {15 x 11"}
    szPaperEnvelopeInvite, {220 x 220 ìì}
    szPaperLetterExtra, {9 \ 275 x 12"}
    szPaperLegalExtra, {9 \275 x 15"}
    szPaperTabloidExtra, {11.69 x 18"}
    szPaperA4Extra, {9.27 x 12.69"}
    szPaperLetterTransverse, {8 \275 x 11"}
    szPaperA4Transverse, {210 x 297 ìì}
    szPaperLetterExtraTransverse, {9\275 x 12"}
    szPaperSuperASuperAA4, {227 x 356 ìì}
    szPaperSuperBSuperBA3, {305 x 487 ìì}
    szPaperLetterPlus, {8.5 x 12.69"}
    szPaperA4Plus, {210 x 330 ìì}
    szPaperA5Transverse, {148 x 210 ìì}
    szPaperB5_JIS_Transverse, {182 x 257 ìì}
    szPaperA3Extra, {322 x 445 ìì}
    szPaperA5Extra, {174 x 235 ìì}
    szPaperB5_ISO_Extra, {201 x 276 ìì}
    szPaperA2, {420 x 594 ìì}
    szPaperA3Transverse, {297 x 420 ìì}
    szPaperA3ExtraTransverse {322 x 445 ìì});

  { TRLXLSBorder }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLXLSBorder = class
  private
    FColor: TColor;
    FLineStyle: TRLXLSLineStyleType;
    FWeight: TRLXLSWeightType;
  public
    constructor Create;
    property Color: TColor read FColor write FColor;
    property LineStyle: TRLXLSLineStyleType read FLineStyle write FLineStyle;
    property Weight: TRLXLSWeightType read FWeight write FWeight;
  end;

  { TRLXLSBorders }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLXLSBorders = class
  private
    FBorders: array[TRLXLSBorderType] of TRLXLSBorder;
    function GetItem(I: TRLXLSBorderType): TRLXLSBorder;
  public
    constructor Create;
    destructor Destroy; override;
    property Borders[I: TRLXLSBorderType]: TRLXLSBorder read GetItem; default;
  end;

  TRLXLSCellChoords = record
    X, Y: Integer;
  end;

  TRLXLSCellArea = record
    X0, Y0, X1, Y1: Integer;
  end;

  { TRLXLSRange }

  {@class TRLXLSRange - Representa uma faixa de células de uma planilha.
   Uma faixa (range) é o meio para se acessar ou modificar o conteúdo e as características das células. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLXLSRange = class
  private
    FWorksheet: TRLXLSWorksheet;
    FCellArea: TRLXLSCellArea;
    FBorders: TRLXLSBorders;
    FFont: TFont;
    FHorizontalAlignment: TRLXLSHorizontalAlignmentType;
    FVerticalAlignment: TRLXLSVerticalAlignmentType;
    FWrapText: Boolean;
    FRotation: Byte;
    FFormat: AnsiString;
    FValue: AnsiString;
    FDataType: TRLXLSCellDataType;
    FFillPattern: TRLXLSFillPattern;
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
    FExportData: Pointer;
    function GetWorkbook: TRLXLSWorkbook;
    procedure SetValue(const Value: AnsiString);
    function IsValue(const Str: AnsiString; var ValueText: AnsiString; var Value: Double): Boolean;
    function IsDate(const Str: AnsiString; var DateText: AnsiString; var DateValue: TDateTime): Boolean;
///    function IsTime(const Str: AnsiString; var TimeText: AnsiString; var TimeValue: TDateTime): Boolean;
    property Borders: TRLXLSBorders read FBorders;
    property HorizontalAlignment: TRLXLSHorizontalAlignmentType read FHorizontalAlignment write FHorizontalAlignment;
    property VerticalAlignment: TRLXLSVerticalAlignmentType read FVerticalAlignment write FVerticalAlignment;
    property WrapText: Boolean read FWrapText write FWrapText;
    property Rotation: Byte read FRotation write FRotation;
    property Format: AnsiString read FFormat write FFormat;
    property FillPattern: TRLXLSFillPattern read FFillPattern write FFillPattern;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property ExportData: Pointer read FExportData write FExportData;
  public
    constructor Create;
    destructor Destroy; override;
    {@prop Worksheet - Referência à aba da planilha a qual pertence esta faixa de células. @links TRLXLSWorksheet. :/}
    property Worksheet: TRLXLSWorksheet read FWorksheet;
    {@prop Workbook - Referência à planilha a qual pertence esta faixa de células. @links TRLXLSWorkbook. :/}
    property Workbook: TRLXLSWorkbook read GetWorkbook;
    {@prop Font - Configuração de fonte das células. :/}
    property Font: TFont read FFont;
    {@prop CellArea - Faixa de células compreendidas pela faixa. :/}
    property CellArea: TRLXLSCellArea read FCellArea write FCellArea;
    {@prop Value - Valor das células como AnsiString. :/}
    property Value: AnsiString read FValue write SetValue;
    {@prop DataType - Tipo de dado das celulas. @links TRLXLSCellDataType. :/}
    property DataType: TRLXLSCellDataType read FDataType write FDataType;
  end;
  {/@class}

  { TRLXLSRow }

  {@class TRLXLSRow - Representa uma linha de uma planilha. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSRow = class
  private
    FIndex: Integer;
    FY: Integer;
    FHeight: Integer;
  public
    constructor Create;
    {@prop Height - Altura da linha em pixels. :/}
    property Height: Integer read FHeight write FHeight;
    {@prop Index - Índice da linha na lista de linhas. :/}
    property Index: Integer read FIndex;
    property Y: Integer read FY;
  end;
  {/@class}

  { TRLXLSCol }

  {@class TRLXLSCol - Representa uma coluna de uma planilha. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSCol = class
  private
    FIndex: Integer;
    FWidth: Integer;
    FX: Integer;
  public
    constructor Create;
    {@prop Width - Largura da coluna em pontos. :/}
    property Width: Integer read FWidth write FWidth;
    {@prop Index - Índice da coluna na lista de colunas. :/}
    property Index: Integer read FIndex;
    property X: Integer read FX;
  end;
  {/@class}

  { TRLXLSPageSetup }
  
  {@class TRLXLSPageSetup - Configuração da página de impressão no Excel. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSPageSetup = class(TPersistent)
  private
    FBlackAndWhite: Boolean;
    FCenterFooter: AnsiString;
    FCenterHeader: AnsiString;
    FCenterHorizontally: Boolean;
    FCenterVertically: Boolean;
    FDraft: Boolean;
    FFirstPageNumber: Integer;
    FFitToPagesTall: Boolean;
    FFitToPagesWide: Boolean;
    FLeftFooter: AnsiString;
    FLeftHeader: AnsiString;
    FOrder: TRLXLSOrderType;
    FOrientation: TRLXLSOrientationType;
    FPaperSize: TRLXLSPaperSizeType;
    FPrintGridLines: Boolean;
    FPrintHeaders: Boolean;
    FPrintNotes: Boolean;
    FRightFooter: AnsiString;
    FRightHeader: AnsiString;
    FLeftMargin: Double;
    FRightMargin: Double;
    FTopMargin: Double;
    FBottomMargin: Double;
    FFooterMargin: Double;
    FHeaderMargin: Double;
    FZoom: Integer;
    FCopies: Integer;
    property PaperSize: TRLXLSPaperSizeType read FPaperSize write FPaperSize;
    property Orientation: TRLXLSOrientationType read FOrientation write FOrientation;
    property Order: TRLXLSOrderType read FOrder write FOrder;
    property PrintHeaders: Boolean read FPrintHeaders write FPrintHeaders;
  protected
    procedure ReadBottomMargin(Reader: TReader);
    procedure ReadLeftMargin(Reader: TReader);
    procedure ReadRightMargin(Reader: TReader);
    procedure ReadTopMargin(Reader: TReader);
    procedure WriteBottomMargin(Writer: TWriter);
    procedure WriteLeftMargin(Writer: TWriter);
    procedure WriteRightMargin(Writer: TWriter);
    procedure WriteTopMargin(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create;
  published
    {@prop Copies - Quantidade inicial de cópias para imprimir. :/}
    property Copies: Integer read FCopies write FCopies default 1;
    {@prop Zoom - Percentual de zoom inicial. :/}
    property Zoom: Integer read FZoom write FZoom default 100;
    {@prop CenterHorizontally - Centralizar página horizontalmente. :/}
    property CenterHorizontally: Boolean read FCenterHorizontally write FCenterHorizontally default False;
    {@prop CenterVertically - Centralizar página verticalmente. :/}
    property CenterVertically: Boolean read FCenterVertically write FCenterVertically default False;
    {@prop BlackAndWhite - Imprimir em preto e branco. :/}
    property BlackAndWhite: Boolean read FBlackAndWhite write FBlackAndWhite default False;
    {@prop Draft - Imprimir em modo de rascunho. :/}
    property Draft: Boolean read FDraft write FDraft default False;
    {@prop PrintNotes - Imprimir notas de rodapé. :/}
    property PrintNotes: Boolean read FPrintNotes write FPrintNotes default False;
    {@prop PrintGridLines - Imprimir linhas de grade. :/}
    property PrintGridLines: Boolean read FPrintGridLines write FPrintGridLines default False;
    {@prop LeftMargin - Margem de impressão a esquerda em cm. :/}
    property LeftMargin: Double read FLeftMargin write FLeftMargin stored False;
    {@prop TopMargin - Margem de impressão superior em cm. :/}
    property TopMargin: Double read FTopMargin write FTopMargin stored False;
    {@prop RightMargin - Margem de impressão a direita em cm. :/}
    property RightMargin: Double read FRightMargin write FRightMargin stored False;
    {@prop BottomMargin - Margem de impressão inferior em cm. :/}
    property BottomMargin: Double read FBottomMargin write FBottomMargin stored False;
    {@prop FirstPageNumber - Número para a primeira página. :/}
    property FirstPageNumber: Integer read FFirstPageNumber write FFirstPageNumber default 1;
    {@prop FitToPagesTall - Encaixar a página de acordo com a altura. :/}
    property FitToPagesTall: Boolean read FFitToPagesTall write FFitToPagesTall default True;
    {@prop FitToPagesWide - Encaixar a página de acordo com a largura. :/}
    property FitToPagesWide: Boolean read FFitToPagesWide write FFitToPagesWide default True;
    {@prop LeftFooter - Texto para rodapé à esquerda. :/}
    property LeftFooter: AnsiString read FLeftFooter write FLeftFooter;
    {@prop LeftHeader - Texto para cabeçalho à esquerda. :/}
    property LeftHeader: AnsiString read FLeftHeader write FLeftHeader;
    {@prop CenterFooter - Texto para rodapé centralizado. :/}
    property CenterFooter: AnsiString read FCenterFooter write FCenterFooter;
    {@prop CenterHeader - Texto para cabeçalho centralizado. :/}
    property CenterHeader: AnsiString read FCenterHeader write FCenterHeader;
    {@prop RightFooter - Texto para rodapé à direita. :/}
    property RightFooter: AnsiString read FRightFooter write FRightFooter;
    {@prop RightHeader - Texto para cabeçalho à direita. :/}
    property RightHeader: AnsiString read FRightHeader write FRightHeader;
    {@prop HeaderMargin - Margem para o cabeçalho em cm. :/}
    property HeaderMargin: Double read FHeaderMargin write FHeaderMargin;
    {@prop FooterMargin - Margem para o rodapé em cm. :/}
    property FooterMargin: Double read FFooterMargin write FFooterMargin;
  end;
  {/@class}

  { TRLXLSWorksheet }

  {@class TRLXLSWorksheet - Representa uma aba de uma planilha Excel. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSWorksheet = class
  private
    FWorkbook: TRLXLSWorkbook;
    FTitle: AnsiString;
    FRanges: TObjectList;
    FCols: TObjectList;
    FRows: TObjectList;
    FCellArea : TRLXLSCellArea;
    function GetRangeCount: Integer;
    function GetRanges(I: Integer): TRLXLSRange;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetIndex: Integer;
    function GetCols(I: Integer): TRLXLSCol;
    function GetRows(I: Integer): TRLXLSRow;
    procedure SetTitle(const Value: AnsiString);
    procedure AddRange(Range: TRLXLSRange);
    function NewRow(ARowIndex: Integer): TRLXLSRow;
    function NewCol(AColIndex: Integer): TRLXLSCol;
    procedure SolveConflicts(var RangeNew: TRLXLSRange);
  public
    constructor Create(AWorkbook: TRLXLSWorkbook);
    destructor Destroy; override;
    {@method FindRow - Retorna a referência para a linha indicada pelo índice informado.
    Pode opcionalmente criar a linha se não a encontrar.
    @links TRLXLSRow. :/}
    function FindRow(ARowIndex: Integer; ACanCreate: Boolean): TRLXLSRow;
    {@method FindCol - Retorna a referência para a coluna indicada pelo índice informado.
    Pode opcionalmente criar a coluna se não a encontrar.
    @links TRLXLSCol. :/}
    function FindCol(AColIndex: Integer; ACanCreate: Boolean): TRLXLSCol;
    function NewRange(X0, Y0, X1, Y1: Integer): TRLXLSRange;
    {@prop Title - Título da aba. :/}
    property Title: AnsiString read FTitle write SetTitle;
    {@prop Workbook - Referência à planilha. @links TRLXLSWorkbook. :/}
    property Workbook: TRLXLSWorkbook read FWorkbook;
    {@prop Index - Índice da aba dentre as abas da planilha. :/}
    property Index: Integer read GetIndex;
    {@prop CellArea - Tamanho da aba medida em celulas. :/}
    property CellArea: TRLXLSCellArea read FCellArea;
    {@prop Ranges - Referência a i-ésima faixa de células. @links TRLXLSRange. :/}
    property Ranges[I: Integer]: TRLXLSRange read GetRanges;
    {@prop RangeCount - Retorna a quantidade de faixas de células. @links Ranges. :/}
    property RangeCount: Integer read GetRangeCount;
    {@prop Rows - Referência a i-ésima linha da aba da planilha. @links TRLXLSRow. :/}
    property Rows[ARowIndex: Integer]: TRLXLSRow read GetRows;
    {@prop RowCount - Quantidade de linhas da aba. :/}
    property RowCount: Integer read GetRowCount;
    {@prop Cols - Referência a i-ésima coluna da aba da planilha. @links TRLXLSCol. :/}
    property Cols[AColIndex: Integer]: TRLXLSCol read GetCols;
    {@prop ColCount - Quantidade de colunas da aba. :/}
    property ColCount: Integer read GetColCount;
  end;
  {/@class}

  { TRLXLSWorkbook }

  TRLXLSRangeRec = record
    iXF: Integer;
    iSST: Integer;
    iFont: Integer;
    iFormat: PtrInt;
  end;

  PXLSRangeRec = ^TRLXLSRangeRec;
  TRLXLSRangesRec = array[0..0] of TRLXLSRangeRec;
  PXLSRangesRec = ^TRLXLSRangesRec;

  TRLXLSSheetRec = record
    StreamBOFOffset: Integer;
    StreamBOFOffsetPosition: Integer;
  end;
  TRLXLSSheetsRecs = array[0..0] of TRLXLSSheetRec;
  PXLSSheetsRecs = ^TRLXLSSheetsRecs;

  {@class TRLXLSWorkbook - Representa uma planilha Excel. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSWorkbook = class
  private
    FUserName: AnsiString;
    FSheets: TObjectList;
    FPageSetup: TRLXLSPageSetup;
    FFindValueCells: Boolean;
    FBOFOffs: Integer;
    FUsedColors: TList;
    FRangesRecs: PXLSRangesRec;
    FColorPalette: packed array[0..XLSMaxColorsInPalette - 1] of TColor;
    FSheetsRecs: PXLSSheetsRecs;
    FPaletteModified: Boolean;
    procedure SetUserName(const Value: AnsiString);
    function GetSheetCount: Integer;
    function GetWorkSheet(I: Integer): TRLXLSWorksheet;
    function GetColorPaletteIndex(AColor: TColor): Integer;
    procedure WriteBookToStream(AStream: TStream);
    procedure WriteBIFF(AStream: TStream; ACode: Word; ABuff: Pointer; ASize: Integer);
    procedure WriteBIFFFont(AStream: TStream; AFont: TFont; AColorPaletteIndex: Word);
    procedure WriteBIFFFormat(AStream: TStream; const AFormatString: AnsiString; AFormatCode: Word);
    procedure BuildFontList(FontList: TObjectList);
    procedure BuildFormatList(FormatList: TStringList);
    procedure BuildXFRecord(ARange: TRLXLSRange; var AXF: TRLXLSBiff8XF; ARec: PXLSRangeRec);
    procedure BuildXFList(XFList: TList);
    procedure WriteBoundSheetToStream(AStream: TStream; var ASheetRec: TRLXLSSheetRec;
      ASheet: TRLXLSWorksheet);
    procedure WriteSheetDataToStream(AStream: TStream; var ASheetRec: TRLXLSSheetRec;
      ASheet: TRLXLSWorksheet);
    procedure WriteRangeToStream(AStream: TStream; ARange: TRLXLSRange; ACurrentRow: Integer; var AIndexInCellsOffsArray: Integer; var ACellsOffs: TRLXLSBiff8DBCELLCellsOffsArray);
    procedure WriteStylesToStream(AStream: TStream);
    procedure WriteFontsToStream(AStream: TStream);
    procedure WriteFormatsToStream(AStream: TStream);
    procedure WriteXFListToStream(AStream: TStream);
    procedure WritePaletteToStream(AStream: TStream);
    procedure WriteWindowData(AStream: TStream);
    procedure WriteBookHeader(AStream: TStream);
    procedure WriteHeaderExtras(AStream: TStream);
    procedure WriteSSTTable(AStream: TStream);
  protected
    function NewSheetTitle: AnsiString;
{$ifdef NATIVEEXCEL}
    procedure NativeExcelSaveToStream(const AStream: TStream);
{$endif}
    procedure FortesSaveToStream(AStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    {@method Clear - Limpa a planilha excluindo todas os textos e abas. :/}
    procedure Clear;
    {@method NewSheet - Adiciona uma nova aba e retorna referência a ela. @links TRLXLSWorksheet. :/}
    function NewSheet: TRLXLSWorksheet;

    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: AnsiString);

    {@prop UserName - Nome do usuário dono da planilha. :/}
    property UserName: AnsiString read FUserName write SetUserName;
    {@prop SheetCount - Quantidade de abas da planilha. :/}
    property SheetCount: Integer read GetSheetCount;
    {@prop Sheets - Retorna referência a i-ésima aba da planilha. :/}
    property Sheets[I: Integer]: TRLXLSWorksheet read GetWorkSheet;
    {@prop PageSetup - Configuração da impressão das páginas. @links TRLXLSPageSetup:/}
    property PageSetup: TRLXLSPageSetup read FPageSetup;
    property FindValueCells: Boolean read FFindValueCells write FFindValueCells;
  end;
  {/@class}

  { TRLXLSFilter }

  {@type TRLXLSFilterOptions - Opções para a geração do arquivo planilha.
   Pode ser um conjunto dos seguintes valores:
   foFindNumberCells - Tenta encontrar valores e datas no relatório e formata as
   células correspondentes;
   foOneSheetOnly - Cria apenas uma aba para conter todas as páginas do relatório
   ao invés de criar uma aba para cada página (padrão). :}
  TRLXLSFilterOption = (foFindValueCells, foOneSheetOnly);

  TRLXLSFilterOptions = set of TRLXLSFilterOption;
  {/@type}
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLXLSTab = class
  public
    Position: Integer;
    Count: Integer;
    Alignment: TRLMetaTextAlignment;
    ComplementPosition: Integer;
    TextOrigin: AnsiString;
  end;

  TRLXLSTabColumn = record
    StartPos: Integer;
    EndPos: Integer;
    Width: Integer;
  end;
  TRLXLSTabColumns = array of TRLXLSTabColumn;
	
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLXLSTabs = class(TObjectList)
  private
    function GetTabs(I: Integer): TRLXLSTab;
  public
    function InsertTab(APosition, AComplementPosition: Integer; AAlignment: TRLMetaTextAlignment; TextOrigin: AnsiString): TRLXLSTab;
    procedure RemoveTab(APosition: Integer);
    function GetColumns(AMinWidth: Integer): TRLXLSTabColumns;
    property Tabs[I: Integer]: TRLXLSTab read GetTabs; default;
  end;

  {@class TRLXLSFilter - Filtro para criação de planilhas formato Excel XLS a partir de um relatório.
   Este filtro gera arquivos binários compatíveis com o formato XLS legíveis pelo Microsoft Excel ou ExcelViewer.
   São exportados todos os textos presentes no relatório com suas fontes e posições mantidas.
   Para cada página do relatório será criada uma aba na planilha.
   Nota: Gráficos, linhas e cores ainda não são suportados.
   @links TRLHTMLFilter, TRLRichFilter, TRLPDFFilter.
   @ancestor TRLCustomSaveFilter.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLXLSFilter = class(TRLCustomSaveFilter)
  private
    FWorkbook: TRLXLSWorkbook;
    FOptions: TRLXLSFilterOptions;
    FHorzTabs: TRLXLSTabs;
    FVertTabs: TRLXLSTabs;
    FFirstPage: Boolean;
    FOffsetRow: Integer;
    function GetPageSetup: TRLXLSPageSetup;
    procedure SetPageSetup(const Value: TRLXLSPageSetup);
    procedure CriarTabsMudancaAlinhamento;
  protected
    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalNewPage; override;
    procedure InternalDrawPage(APage: TRLGraphicSurface); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {@prop WorkBook - Referência o objeto planilha interno do filtro.
     @links TRLXLSWorkbook. :/}
    property WorkBook: TRLXLSWorkbook read FWorkbook;
  published
    {@prop Options - Opções diversas de geração do arquivo planilha. @links TRLXLSFilterOptions. :/}
    property Options: TRLXLSFilterOptions read FOptions write FOptions default [];
    {@prop PageSetup - Configuração da impressão das páginas. @links TRLXLSPageSetup:/}
    property PageSetup: TRLXLSPageSetup read GetPageSetup write SetPageSetup;
    {@prop FileName = ancestor /}
    property FileName;
    {@prop DisplayName = ancestor /}
    property DisplayName;
    {@prop ShowProgress - ancestor /}
    property ShowProgress;
  end;
  {/@class}
  
{/@unit}

implementation

const
  MaxBiffRecordSize = 8228;

const
  DefaultFontName = 'Arial';
  DefaultCellHeight = 255;
  DefaultCellLength = 8;

const
  B8_EOF = $000A;
  B8_BOF = $0809;
  B8_COLINFO = $007D;
  B8_XF = $00E0;
  B8_LABEL = $0204;
  B8_BLANK = $0201;
  B8_DIMENSIONS = $0200;
  B8_ROW = $0208;
  B8_INTERFACHDR = $00E1;
  B8_INTERFACEND = $00E2;
  B8_MMS = $00C1;
  B8_CODEPAGE = $0042;
  B8_TABID = $013D;
  B8_FNGROUPCOUNT = $009C;
  B8_WINDOWPROTECT = $0019;
  B8_PROTECT = $0012;
  B8_PASSWORD = $0013;
  B8_WINDOW1 = $003D;
  B8_BACKUP = $0040;
  B8_HIDEOBJ = $008D;
  B8_1904 = $0022;
  B8_PRECISION = $000E;
  B8_BOOKBOOL = $00DA;
  B8_FONT = $0031; // MSDN=$0231
  B8_FORMAT = $041E;
  B8_COUNTRY = $008C;
  B8_INDEX = $020B;
  B8_CALCMODE = $000D;
  B8_CALCCOUNT = $000C;
  B8_REFMODE = $000F;
  B8_ITERATION = $0011;
  B8_SAVERECALC = $005F;
  B8_DELTA = $0010;
  B8_PRINTHEADERS = $002A;
  B8_PRINTGRIDLINES = $002B;
  B8_GRIDSET = $0082;
  B8_GUTS = $0080;
  B8_DEFAULTROWHEIGHT = $0225;
  B8_WSBOOL = $0081;
  B8_HEADER = $0014;
  B8_FOOTER = $0015;
  B8_HCENTER = $0083;
  B8_VCENTER = $0084;
  B8_DEFCOLWIDTH = $0055;
  B8_WRITEACCESS = $005C;
  B8_DOUBLESTREAMFILE = $0161;
  B8_PROT4REV = $01AF;
  B8_PROT4REVPASS = $01BC;
  B8_REFRESHALL = $01B7;
  B8_USESELFS = $0160;
  B8_BOUNDSHEET = $0085;
  B8_WINDOW2 = $023E;
  B8_SELECTION = $001D;
  B8_DBCELL = $00D7;
  B8_MULBLANK = $00BE;
  B8_MERGE = $00E5;
  B8_PALETTE = $0092;
  B8_CONTINUE = $003C;
  B8_SETUP = $00A1;
  B8_SST = $00FC;
  B8_EXTSST = $00FF;
  B8_LABELSST = $00FD;
  B8_NUMBER = $0203;

  B8_BOF_vers = $0600;

  B8_BOF_dt_WorkbookGlobals = $0005;
  B8_BOF_dt_VisualBasicModule = $0006;
  B8_BOF_dt_Worksheet = $0010;
  B8_BOF_dt_Chart = $0020;
  B8_BOF_dt_MacroSheet = $0040;
  B8_BOF_dt_WorkspaceFile = $0100;

  B8_BOF_rupBuild_Excel97 = $15EC; //$0DBB;
  B8_BOF_rupYear_Excel97 = $07CD; //$07CC;

  B8_XF_Opt1_fLocked = $0001;
  B8_XF_Opt1_fHidden = $0002;
  B8_XF_Opt1_fStyleXF = $0004;
  B8_XF_Opt1_f123Prefix = $0008;
  B8_XF_Opt1_ixfParent = $FFF0;

  B8_XF_Opt2_alcGeneral = $0000;
  B8_XF_Opt2_alcLeft = $0001;
  B8_XF_Opt2_alcCenter = $0002;
  B8_XF_Opt2_alcRight = $0003;
  B8_XF_Opt2_alcFill = $0004;
  B8_XF_Opt2_alcJustify = $0005;
  B8_XF_Opt2_alcCenterAcrossSelection = $0006;

  B8_XF_Opt2_fWrap = $0008;

  B8_XF_Opt2_alcVTop = $0000;
  B8_XF_Opt2_alcVCenter = $0010;
  B8_XF_Opt2_alcVBottom = $0020;
  B8_XF_Opt2_alcVJustify = $0030;

  B8_XF_Opt3_fMergeCell = $0020;
  B8_XF_Opt3_fAtrNum = $0400;
  B8_XF_Opt3_fAtrFnt = $0800;
  B8_XF_Opt3_fAtrAlc = $1000;
  B8_XF_Opt3_fAtrBdr = $2000;
  B8_XF_Opt3_fAtrPat = $4000;
  B8_XF_Opt3_fAtrProt = $8000;

  B8_XF_Border_None = $0000;
  B8_XF_Border_Thin = $0001;
  B8_XF_Border_Medium = $0002;
  B8_XF_Border_Dashed = $0003;
  B8_XF_Border_Dotted = $0004;
  B8_XF_Border_Thick = $0005;
  B8_XF_Border_Double = $0006;
  B8_XF_Border_Hair = $0007;
  B8_XF_Border_MediumDashed = $0008;
  B8_XF_Border_DashDot = $0009;
  B8_XF_Border_MediumDashDot = $000A;
  B8_XF_Border_DashDotDot = $000B;
  B8_XF_Border_MediumDashDotDot = $000C;
  B8_XF_Border_SlantedDashDot = $000D;

  B8_INTERFACHDR_cv_IBMPC = $01B5;
  B8_INTERFACHDR_cv_Macintosh = $8000;
  B8_INTERFACHDR_cv_ANSI = $04E4;

  B8_CODEPAGE_cv_IBMPC = $01B5;
  B8_CODEPAGE_cv_Macintosh = $8000;
  B8_CODEPAGE_cv_ANSI = $04E4;

  B8_WINDOW1_grbit_fHidden = $0001;
  B8_WINDOW1_grbit_fIconic = $0002;
  B8_WINDOW1_grbit_fDspHScroll = $0008;
  B8_WINDOW1_grbit_fDspVScroll = $0010;
  B8_WINDOW1_grbit_fBotAdornment = $0020;

  B8_FONT_grbit_fItalic = $0002;
  B8_FONT_grbit_fStrikeout = $0008;
  B8_FONT_grbit_fOutline = $0010;
  B8_FONT_grbit_fShadow = $0020;

  B8_DEFAULTROWHEIGHT_fUnsynced = $0001;
  B8_DEFAULTROWHEIGHT_fDyZero = $0002;
  B8_DEFAULTROWHEIGHT_fExAsc = $0004;
  B8_DEFAULTROWHEIGHT_fExDsc = $0008;

  B8_WSBOOL_fShowAutoBreaks = $0001;
  B8_WSBOOL_fDialog = $0010;
  B8_WSBOOL_fApplyStyles = $0020;
  B8_WSBOOL_fRwSumsBelow = $0040;
  B8_WSBOOL_fColSumsRight = $0080;
  B8_WSBOOL_fFitToPage = $0100;
  B8_WSBOOL_fDspGuts = $0200;
  B8_WSBOOL_fAee = $0400;
  B8_WSBOOL_fAfe = $8000;

  B8_WINDOW1_fHidden = $0001;
  B8_WINDOW1_fIconic = $0002;
  B8_WINDOW1_fDspHScroll = $0008;
  B8_WINDOW1_fDspVScroll = $0010;
  B8_WINDOW1_fBotAdornment = $0020;


  B8_WINDOW2_grbit_fDspFmla = $0001;
  B8_WINDOW2_grbit_fDspGrid = $0002;
  B8_WINDOW2_grbit_fDspRwCol = $0004;
  B8_WINDOW2_grbit_fFrozen = $0008;
  B8_WINDOW2_grbit_fDspZeros = $0010;
  B8_WINDOW2_grbit_fDefaultHdr = $0020;
  B8_WINDOW2_grbit_fArabic = $0040;
  B8_WINDOW2_grbit_fDspGuts = $0080;
  B8_WINDOW2_grbit_fFrozenNoSplit = $0100;
  B8_WINDOW2_grbit_fSelected = $0200;
  B8_WINDOW2_grbit_fPaged = $0400;
  B8_WINDOW2_grbit_fSLV = $0800;

  B8_ROW_grbit_fCollapsed = $0010;
  B8_ROW_grbit_fDyZero = $0020;
  B8_ROW_grbit_fUnsynced = $0040;
  B8_ROW_grbit_fGhostDirty = $0080;
  B8_ROW_grbit_mask_iOutLevel = $0007;

  B8_COLINFO_fHidden = $0001;
  B8_COLINFO_fCollapsed = $1000;

  B8_SETUP_fLeftToRight = $0001;
  B8_SETUP_fLandscape = $0002;
  B8_SETUP_fNoPls = $0004;
  B8_SETUP_fNoColor = $0008;
  B8_SETUP_fDraft = $0010;
  B8_SETUP_fNotes = $0020;
  B8_SETUP_fNoOrient = $0040;
  B8_SETUP_fUsePage = $0080;

  B8_LEFTMARGIN = $0026;
  B8_RIGHTMARGIN = $0027;
  B8_TOPMARGIN = $0028;
  B8_BOTTOMMARGIN = $0029;

{ UTILS }

procedure MulRectX(var R: TRect; FX, FY: Double);
var
  W, H: Integer;
begin
  W := Trunc((R.Right - R.Left) * FX);
  H := Trunc((R.Bottom - R.Top) * FY);
  R.Left := Trunc(R.Left * FX);
  R.Right := Trunc(R.Right * FX);
  if R.Right - R.Left < W then
    R.Right := R.Left + W;
  R.Top := Trunc(R.Top * FY);
  R.Bottom := Trunc(R.Bottom * FY);
  if R.Bottom - R.Top < H then
    R.Bottom := R.Top + H;
end;

function NewCellArea(X0, Y0, X1, Y1: Integer): TRLXLSCellArea;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.X1 := X1;
  Result.Y1 := Y1;
end;

function ChordsInBounds(X, Y: Integer; const Bounds: TRLXLSCellArea): Boolean;
begin
  Result := ((X >= Bounds.X0) and (X <= Bounds.X1)) and ((Y >= Bounds.Y0) and (Y <= Bounds.Y1));
end;

function CellAreaIntercepts(const Bounds1, Bounds2: TRLXLSCellArea): Boolean;
begin
  Result := (Bounds1.X1 >= Bounds2.X0) and (Bounds1.X0 <= Bounds2.X1) and
    (Bounds1.Y1 >= Bounds2.Y0) and (Bounds1.Y0 <= Bounds2.Y1);
end;

function WideStringSize(const WideStr: AnsiString): Integer;
begin
  Result := Length(WideStr) * SizeOf(WideChar);
end;

procedure StringToWideChar(const Str: AnsiString; WideDestPtr: PWideChar; WideDestRoom: Integer);
var
  Cast: WideString;
begin
  Cast := Str;
  if Cast <> '' then
    Move(Cast[1], WideDestPtr^, WideDestRoom)
  else
    WideDestPtr[0] := #0;
end;

function IsNum(const Str: AnsiString; var Digits: AnsiString; var Value: Integer): Boolean;
var
  I, StrLen: Integer;
begin
  Result := False;
  StrLen := Length(Str);
  if StrLen = 0 then
    Exit;
  for I := 1 to StrLen do
    if not (Str[I] in ['0'..'9']) then
      Exit;
  Digits := Str;
  Result := TryStrToInt(Str, Value);
end;

function TwipsX(X: Integer): Integer;
begin
 {$ifdef NATIVEEXCEL}
   Result := X;
 {$else}
   Result := Round((X / 96) * 1440 * 2.54);
 {$endif}
end;

function TwipsY(Y: Integer): Integer;
begin
  Result := Y;
end;

function SameFont(AFont1, AFont2: TFont): Boolean;
begin
  Result := (AFont1.Charset = AFont2.Charset) and
    (AFont1.Color = AFont2.Color) and
    (AFont1.Height = AFont2.Height) and
    (AFont1.Name = AFont2.Name) and
    (AFont1.Pitch = AFont2.Pitch) and
    (AFont1.Size = AFont2.Size) and
    (AFont1.Style = AFont2.Style);
end;

function FindSimilarFont(FontList: TObjectList; FontSample: TFont): TFont;
var
  F: Integer;
begin
  for F := 0 to FontList.Count - 1 do
  begin
    Result := FontList[F] as TFont;
    if SameFont(Result, FontSample) then
      Exit;
  end;
  Result := nil;
end;

{ TRLXLSRow }

constructor TRLXLSRow.Create;
begin
  inherited;
  FIndex := 0;
  FHeight := DefaultCellHeight;
  FY := 0;
end;

{ TRLXLSCol }

constructor TRLXLSCol.Create;
begin
  inherited;
  FIndex := 0;
  FWidth := DefaultCellLength * 256;
  FX := 0;
end;

{ TRLXLSBorder }

constructor TRLXLSBorder.Create;
begin
  inherited;
  FLineStyle := lsNone;
  FWeight := weHairline;
  FColor := clBlack;
end;

{ TRLXLSBorders }

constructor TRLXLSBorders.Create;
var
  I: TRLXLSBorderType;
begin
  inherited;
  for I := Low(TRLXLSBorderType) to High(TRLXLSBorderType) do
    FBorders[I] := TRLXLSBorder.Create;
end;

destructor TRLXLSBorders.Destroy;
var
  I: TRLXLSBorderType;
begin
  inherited;
  for I := Low(TRLXLSBorderType) to High(TRLXLSBorderType) do
    FBorders[I].Free;
end;

function TRLXLSBorders.GetItem;
begin
  Result := FBorders[I];
end;

{ TRLXLSRange }

constructor TRLXLSRange.Create;
begin
  inherited Create;
  FVerticalAlignment := vaBottom;
  FHorizontalAlignment := haGeneral;
  FWorksheet := nil;
  FBorders := TRLXLSBorders.Create;
  FFont := TFont.Create;
  FFont.Name := DefaultFontName;
  FFont.Size := 10;
  FFont.Color := clBlack;
  FValue := '';
  FDataType := ctString;
end;

destructor TRLXLSRange.Destroy;
begin
  inherited;
  FBorders.Free;
  FFont.Free;
end;

function TRLXLSRange.GetWorkbook;
begin
  if FWorksheet <> nil then
    Result := FWorksheet.Workbook
  else
    Result := nil;
end;

function TRLXLSRange.IsValue(const Str: AnsiString; var ValueText: AnsiString; var Value: Double): Boolean;
var
  ThousandChar: AnsiString;
  ErrorCode: Integer;
  EntreParenteses: Boolean;
  BackupValueText: string;
begin
  BackupValueText:=ValueText;
  Result := False;
  {$ifdef HAS_FORMATSETTINGS}
  ThousandChar := IfThen(FormatSettings.DecimalSeparator = '.', ',', '.');
  {$else}
  ThousandChar := IfThen(DecimalSeparator = '.', ',', '.');
  {$EndIf}
  // limpa o texto
  ValueText := Str;
  ValueText := StringReplace(ValueText, #13#10, ' ', [rfReplaceAll]);
  ValueText := StringReplace(ValueText, #10, ' ', [rfReplaceAll]);
  ValueText := StringReplace(ValueText, ThousandChar, '', [rfReplaceAll]); // retira separador de milhares
  {$ifdef HAS_FORMATSETTINGS}
  ValueText := StringReplace(ValueText, FormatSettings.DecimalSeparator, '.', [rfReplaceAll]); // coloca ponto como separador de decimais
  {$else}
  ValueText := StringReplace(ValueText, DecimalSeparator, '.', [rfReplaceAll]); // coloca ponto como separador de decimais
  {$EndIf}
  ValueText := Trim(ValueText);
  if SameText(ValueText, '0.00') or SameText(ValueText, '0') then
    //Não faz nada
  else if not ((ValueText+' ')[1] in ['1'..'9', '-', '(']) then //desconsiderando números iniciados por 0 para não traduzir erroneamente códigos ex: 0002
    Exit;
  EntreParenteses := AnsiStartsText('(', ValueText) and AnsiEndsText(')', ValueText);
  if EntreParenteses then
  begin
    // tenta pegar o valor da AnsiString
    ValueText := Copy(ValueText, 2, Length(ValueText)-2);
    Val(ValueText, Value, ErrorCode);
    Value := -Value;
  end
  else
  begin
    // tenta pegar o valor da AnsiString
    try
      Val(ValueText, Value, ErrorCode);
    except
      //Fred/Rolim/Marcio Martins - Tenta converter o número, se não puder, retorna false e retorna o ValueText original.
      Result:=False;
      ValueText:=BackupValueText;
      Exit;
    end;

    if Pos('.', ValueText) = 0 then
      if Value > MaxInt then
        ErrorCode := 1;
  end;
  if (ValueText <> '') and (ErrorCode = 0) then
  begin
    System.Str(Value, ValueText); // transforma o valor de volta em AnsiString com os decimais corretos
    {$ifdef HAS_FORMATSETTINGS}
    ValueText := Trim(StringReplace(ValueText, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]));
    {$else}
    ValueText := Trim(StringReplace(ValueText, '.', DecimalSeparator, [rfReplaceAll]));
    {$EndIf}
    Result := True;
  end;
end;

function TRLXLSRange.IsDate(const Str: AnsiString; var DateText: AnsiString; var DateValue: TDateTime): Boolean;
var
  Bar1, Bar2, Bar3: Integer;
  DayText, MonthText, YearText: AnsiString;
  DayValue, MonthValue, YearValue: Integer;
  NowYear, Epoch1, Epoch2, Decade1, Decade2: Integer;
begin
  Result := False;
  // limpa o texto
  DateText := Str;
  DateText := StringReplace(DateText, #13#10, ' ', [rfReplaceAll]);
  DateText := StringReplace(DateText, #10, ' ', [rfReplaceAll]);
  DateText := Trim(DateText);
  Bar1 := PosEx('/', DateText, 1);
  if Bar1 = 0 then
    Exit;
  Bar2 := PosEx('/', DateText, Bar1 + 1);
  if Bar2 = 0 then
    Exit;
  Bar3 := PosEx('/', DateText, Bar2 + 1);
  if Bar3 <> 0 then
    Exit
  else
    Bar3 := Length(DateText) + 1;
  if not IsNum(Copy(DateText, 1, Bar1 - 1), DayText, DayValue) or (Length(DayText) > 2) or (DayValue < 1) or (DayValue > 31) then
    Exit;
  if not IsNum(Copy(DateText, Bar1 + 1, Bar2 - Bar1 - 1), MonthText, MonthValue) or (Length(MonthText) > 2) or (MonthValue < 1) or (MonthValue > 12) then
    Exit;
  if not IsNum(Copy(DateText, Bar2 + 1, Bar3 - Bar2 - 1), YearText, YearValue) then
    Exit;
  if (Length(YearText) = 2) and (YearValue >= 00) and (YearValue < 100) then
  begin
    NowYear := YearOf(Today);
    Epoch1 := NowYear - 80;
    Epoch2 := NowYear + 20;
    Decade1 := Epoch1 mod 100;
    Decade2 := Epoch2 mod 100;
    if YearValue < Decade1 then
      YearValue := (Epoch2 - Decade2) + YearValue
    else
      YearValue := (Epoch1 - Decade1) + YearValue;
  end  
  else if (Length(YearText) = 4) and (YearValue > 1000) and (YearValue < 3000) then
  else
    Exit;
  if not TryEncodeDate(YearValue, MonthValue, DayValue, DateValue) then
    Exit;
  DateText := DayText + '/' + MonthText + '/' + YearText;
  Result := True;
end;

{function TRLXLSRange.IsTime(const Str: AnsiString; var TimeText: AnsiString; var TimeValue: TDateTime): Boolean;
var
  TwoDots1, TwoDots2, TwoDots3: Integer;
  HourText, MinuteText, SecondText: AnsiString;
  HourValue, MinuteValue, SecondValue: Integer;
begin
  Result := False;
  // limpa o texto
  TimeText := Str;
  TimeText := StringReplace(TimeText, #13#10, ' ', [rfReplaceAll]);
  TimeText := StringReplace(TimeText, #10, ' ', [rfReplaceAll]);
  TimeText := Trim(TimeText);
  TwoDots1 := PosEx(':', TimeText, 1);
  if TwoDots1 = 0 then
    Exit;
  TwoDots2 := PosEx(':', TimeText, TwoDots1 + 1);
  if TwoDots2 = 0 then
    Exit;
  TwoDots3 := PosEx(':', TimeText, TwoDots2 + 1);
  if TwoDots3 <> 0 then
    Exit
  else
    TwoDots3 := Length(TimeText) + 1;
  if not IsNum(Copy(TimeText, 1, TwoDots1 - 1), HourText, HourValue) or (Length(HourText) > 2) or (HourValue < 0) or (HourValue > 23) then
    Exit;
  if not IsNum(Copy(TimeText, TwoDots1 + 1, TwoDots2 - TwoDots1 - 1), MinuteText, MinuteValue) or (Length(MinuteText) > 2) or (MinuteValue < 0) or (MinuteValue > 59) then
    Exit;
  if not IsNum(Copy(TimeText, TwoDots2 + 1, TwoDots3 - TwoDots2 - 1), SecondText, SecondValue) or (Length(SecondText) > 2) or (SecondValue < 0) or (SecondValue > 59) then
    Exit;
  TimeValue := EncodeTime(HourValue, MinuteValue, SecondValue, 0);
  TimeText := HourText + ':' + MinuteText + ':' + SecondText;
  Result := True;
end;
}///

procedure TRLXLSRange.SetValue(const Value: AnsiString);
var
  AuxStr: AnsiString;
  DoubleValue: Double;
  TimeValue: TDateTime;
begin
  FValue := StringReplace(Value, #13#10, #10, [rfReplaceAll]);
  FDataType := ctString;
  if Self.Workbook.FindValueCells then
    if IsValue(Value, AuxStr, DoubleValue) then
    begin
      FValue := AuxStr;
      FDataType := ctNumber;
    end
    else if IsDate(Value, AuxStr, TimeValue) then
    begin
      FValue := AuxStr;
      FDataType := ctDate;
      FFormat := 'dd\/mm\/yyyy';
    end;
    {
    else if IsTime(Value, AuxStr, TimeValue) then
    begin
      FValue := AuxStr;
      FDataType := ctTime;
      FFormat := 'hh\:nn\:ss';
    end;}///
end;

const
  DefaultLeftMargin = 2;
  DefaultTopMargin = 2.5;
  DefaultRightMargin = 2;
  DefaultBottomMargin = 2.5;

{ TRLXLSPageSetup }

constructor TRLXLSPageSetup.Create;
begin
  inherited;
  FLeftMargin := DefaultLeftMargin;
  FTopMargin := DefaultTopMargin;
  FRightMargin := DefaultRightMargin;
  FBottomMargin := DefaultBottomMargin;
  FPaperSize := szPaperA4;
  FCopies := 1;
  FZoom := 100;
  FFitToPagesTall := True;
  FFitToPagesWide := True;
  FFirstPageNumber := 1;
  FCenterHorizontally := False;
  FCenterVertically := False;
  FBlackAndWhite := False;
  FDraft := False;
  FPrintNotes := False;
  FPrintGridLines := False;
  FLeftFooter := '';
  FLeftHeader := '';
  FCenterFooter := '';
  FCenterHeader := '';
  FRightFooter := '';
  FRightHeader := '';
  FHeaderMargin := 0;
  FFooterMargin := 0;
end;

procedure TRLXLSPageSetup.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('LeftMargin', ReadLeftMargin, WriteLeftMargin, FLeftMargin <> DefaultLeftMargin);
  Filer.DefineProperty('TopMargin', ReadTopMargin, WriteTopMargin, FTopMargin <> DefaultTopMargin);
  Filer.DefineProperty('RightMargin', ReadRightMargin, WriteRightMargin, FRightMargin <> DefaultRightMargin);
  Filer.DefineProperty('BottomMargin', ReadBottomMargin, WriteBottomMargin, FBottomMargin <> DefaultBottomMargin);
end;

procedure TRLXLSPageSetup.ReadLeftMargin(Reader: TReader);
begin
  FLeftMargin := Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteLeftMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FLeftMargin);
end;

procedure TRLXLSPageSetup.ReadTopMargin(Reader: TReader);
begin
  FTopMargin := Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteTopMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FTopMargin);
end;

procedure TRLXLSPageSetup.ReadRightMargin(Reader: TReader);
begin
  FRightMargin := Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteRightMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FRightMargin);
end;

procedure TRLXLSPageSetup.ReadBottomMargin(Reader: TReader);
begin
  FBottomMargin := Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteBottomMargin(Writer: TWriter);
begin
  Writer.WriteFloat(FBottomMargin);
end;

{ TRLXLSWorksheet }

constructor TRLXLSWorksheet.Create(AWorkbook: TRLXLSWorkbook);
begin
  inherited Create;
  FWorkbook := AWorkbook;
  FCellArea := NewCellArea(-1, -1, -1, -1);
  FTitle := '';
  FRanges := nil;
  FCols := nil;
  FRows := nil;
  FRanges := TObjectList.Create;
  FCols := TObjectList.Create;
  FRows := TObjectList.Create;
end;

destructor TRLXLSWorksheet.Destroy;
begin
  inherited;
  if Assigned(FRanges) then
    FRanges.Free;
  if Assigned(FCols) then
    FCols.Free;
  if Assigned(FRows) then
    FRows.Free;
end;

function TRLXLSWorksheet.GetIndex;
begin
  Result := FWorkBook.FSheets.IndexOf(Self);
end;

procedure TRLXLSWorksheet.SetTitle(const Value: AnsiString);
begin
  FTitle := Trim(Copy(Value, 1, 31));
end;

function TRLXLSWorksheet.GetCols(I: Integer): TRLXLSCol;
begin
  Result := FCols[I] as TRLXLSCol;
end;

function TRLXLSWorksheet.GetRows(I: Integer): TRLXLSRow;
begin
  Result := FRows[I] as TRLXLSRow;
end;

function TRLXLSWorksheet.GetColCount: Integer;
begin
  Result := FCols.Count;
end;

function TRLXLSWorksheet.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TRLXLSWorksheet.GetRangeCount: Integer;
begin
  Result := FRanges.Count;
end;

function TRLXLSWorksheet.GetRanges(I: Integer): TRLXLSRange;
begin
  Result := FRanges[I] as TRLXLSRange;
end;

function TRLXLSWorksheet.FindRow(ARowIndex: Integer; ACanCreate: Boolean): TRLXLSRow;
var
  Y, I: Integer;
begin
  Y := 0;
  I := 0;
  while (I < RowCount) and (Rows[I].Index <> ARowIndex) do
  begin
    Inc(Y, Rows[I].Height);
    Inc(I);
  end;
  if I < RowCount then
    Result := Rows[I]
  else if ACanCreate then
  begin
    Result := NewRow(ARowIndex);
    Result.FY := Y;
  end
  else
    Result := nil; 
end;

function TRLXLSWorksheet.NewRow(ARowIndex: Integer): TRLXLSRow;
begin
  Result := TRLXLSRow.Create;
  Result.FIndex := ARowIndex;
  FRows.Add(Result);
  // expande as dimensões da sheet
  if (FCellArea.Y0 = -1) or (ARowIndex < FCellArea.Y0) then
    FCellArea.Y0 := ARowIndex;
  if (FCellArea.Y1 = -1) or (ARowIndex > FCellArea.Y1) then
    FCellArea.Y1 := ARowIndex;
end;

function TRLXLSWorksheet.FindCol(AColIndex: Integer; ACanCreate: Boolean): TRLXLSCol;
var
  X, I: Integer;
begin
  X := 0;
  I := 0;
  while (I < ColCount) and (Cols[I].Index <> AColIndex) do
  begin
    Inc(X, Cols[I].Width);
    Inc(I);
  end;
  if I < ColCount then
    Result := Cols[I]
  else if ACanCreate then
  begin
    Result := NewCol(AColIndex);
    Result.FX := X;
  end
  else
    Result := nil;
end;

function TRLXLSWorksheet.NewCol(AColIndex: Integer): TRLXLSCol;
begin
  Result := TRLXLSCol.Create;
  Result.FIndex := AColIndex;
  FCols.Add(Result);
  // expande as dimensões da sheet
  if (FCellArea.X0 = -1) or (AColIndex < FCellArea.X0) then
    FCellArea.X0 := AColIndex;
  if (FCellArea.X1 = -1) or (AColIndex > FCellArea.X1) then
    FCellArea.X1 := AColIndex;
end;

procedure TRLXLSWorksheet.AddRange(Range: TRLXLSRange);
begin
  Range.FWorksheet := Self;
  FRanges.Add(Range);
  // expande as dimensões da sheet
  if (Self.FCellArea.X0 = -1) or (Range.CellArea.X0 < Self.FCellArea.X0) then
    Self.FCellArea.X0 := Range.CellArea.X0;
  if (Self.FCellArea.Y0 = -1) or (Range.CellArea.Y0 < Self.FCellArea.Y0) then
    Self.FCellArea.Y0 := Range.CellArea.Y0;
  if (Self.FCellArea.X1 = -1) or (Range.CellArea.X1 > Self.FCellArea.X1) then
    Self.FCellArea.X1 := Range.CellArea.X1;
  if (Self.FCellArea.Y1 = -1) or (Range.CellArea.Y1 > Self.FCellArea.Y1) then
    Self.FCellArea.Y1 := Range.CellArea.Y1;
end;

function TRLXLSWorksheet.NewRange(X0, Y0, X1,
  Y1: Integer): TRLXLSRange;
begin
  Result := TRLXLSRange.Create;
  Result.FCellArea := NewCellArea(X0, Y0, X1, Y1);
  Self.AddRange(Result);
end;

{ TRLXLSWorkbook }
  
constructor TRLXLSWorkbook.Create;
begin
  inherited Create;
  FUsedColors := TList.Create;
  FUserName := CS_ProductTitleStr;
  FSheets := nil;
  FPageSetup := nil;
  FSheets := TObjectList.Create;
  FPageSetup := TRLXLSPageSetup.Create;
  FFindValueCells := False;
end;

destructor TRLXLSWorkbook.Destroy;
begin
  inherited;
  if Assigned(FUsedColors) then
    FUsedColors.Free;
  if Assigned(FSheets) then
    FSheets.Free;
  if Assigned(FPageSetup) then
    FPageSetup.Free;
end;

function TRLXLSWorkbook.NewSheetTitle: AnsiString;
var
  titleno, I: Integer;
begin
  titleno := FSheets.Count;
  repeat
    Inc(titleno);
    Result := GetLocalizeStr(LocaleStrings.LS_PageStr + IntToStr(titleno));
    I := 0;
    while (I < SheetCount) and not AnsiSameText(Sheets[I].Title, Result) do
      Inc(I);
  until not (I < SheetCount);
end;

procedure TRLXLSWorkbook.SetUserName(const Value: AnsiString);
const
  MaxUserName = 66;
begin
  FUserName := Trim(Copy(Value, 1, MaxUserName));
end;

function TRLXLSWorkbook.GetSheetCount: Integer;
begin
  Result := FSheets.Count;
end;

function TRLXLSWorkbook.GetWorkSheet(I: Integer): TRLXLSWorksheet;
begin
  Result := TRLXLSWorksheet(FSheets[I]);
end;

function TRLXLSWorkbook.NewSheet;
begin
  Result := TRLXLSWorksheet.Create(Self);
  Result.Title := NewSheetTitle;
  FSheets.Add(Result);
end;

procedure TRLXLSWorkbook.Clear;
begin
  FSheets.Clear;
end;

{$ifdef NATIVEEXCEL}
procedure TRLXLSWorkbook.NativeExcelSaveToStream(const AStream: TStream);
var
  I, J: Integer;
  Book: IXLSWorkbook;
  Sheet: IXLSWorksheet;
  RLSheet: TRLXLSWorksheet;
  RLRange: TRLXLSRange;
  Range: IXLSRange;
  function ViraLetra(X: Integer): string;
  begin
    Result := Chr(65+(X mod 26));
    if X > 25 then
      Result := ViraLetra(X div 26 - 1) + Result;
  end;
  function ViraCell(X, Y: Integer): string;
  begin
    Result := ViraLetra(X) + IntToStr(Y+1);
  end;
begin
  Book := TXLSWorkbook.Create;
  for I := 0 to Self.SheetCount - 1 do
  begin
    RLSheet := Self.Sheets[I];
    Sheet := Book.Sheets.Add;
    Sheet.Name := RLSheet.Title;
    for J := 0 to RLSheet.ColCount - 1 do
      Sheet._ColumnInfo.Width[J] := RLSheet.Cols[J].Width / 7.1;

    for J := 0 to RLSheet.RowCount - 1 do
      Sheet._RowInfo.Height[J] := RLSheet.Rows[J].Height;

    for J := 0 to RLSheet.RangeCount - 1 do
    begin
      RLRange := RLSheet.Ranges[J];
      Range := Sheet.Range[ViraCell(RLRange.CellArea.X0, RLRange.CellArea.Y0), ViraCell(RLRange.CellArea.X0, RLRange.CellArea.Y0)];
      case RLRange.DataType of
        ctNumber: Range.Value := StrToFloat(RLRange.Value);
        ctDate:
        begin
          Range.Value := StrToDate(RLRange.Value);
          Range.NumberFormat := 'dd/mm/yyyy';
        end;
        ctTime:
        begin
          Range.Value := StrToTime(RLRange.Value);
          Range.NumberFormat := 'hh:mm:ss';
        end;
        else
          Range.Value := RLRange.Value;
      end;
      { TODO :
C:\Projetos\RLib_zBACKUPS\RLib_Excel_Não Usar\Debug\ExemplosXLS\FROTA INSPEÇÃO PARA REVISÃO DA FROTA.rpf
Ajustar a fonte no XLS. }
      Range.Font.Name := RLRange.Font.Name;
      Range.Font.Size := RLRange.Font.Size;
      Range.Font.Bold := fsBold in RLRange.Font.Style;
      Range.Font.Italic := fsItalic in RLRange.Font.Style;
      case RLRange.VerticalAlignment of
        vaTop: Range.VerticalAlignment := xlVAlignTop;
        vaCenter: Range.VerticalAlignment := xlVAlignCenter;
        vaBottom: Range.VerticalAlignment := xlVAlignBottom;
        vaJustify: Range.VerticalAlignment := xlVAlignJustify;
      end;
      case RLRange.HorizontalAlignment of
        haCenter: Range.HorizontalAlignment := xlHAlignCenter;
        haCenterAcrossSelection: Range.HorizontalAlignment := xlHAlignCenterAcrossSelection;
        //xlHAlignDistributed
        haFill: Range.HorizontalAlignment := xlHAlignFill;
        haGeneral: Range.HorizontalAlignment := xlHAlignGeneral;
        haJustify: Range.HorizontalAlignment := xlHAlignJustify;
        haLeft: Range.HorizontalAlignment := xlHAlignLeft;
        haRight: Range.HorizontalAlignment := xlHAlignRight;
      end;
      Range := Sheet.Range[ViraCell(RLRange.CellArea.X0, RLRange.CellArea.Y0), ViraCell(RLRange.CellArea.X1, RLRange.CellArea.Y1)];
      if ViraCell(RLRange.CellArea.X0, RLRange.CellArea.Y0) <> ViraCell(RLRange.CellArea.X1, RLRange.CellArea.Y1) then
        Range.Merge(False);
    end;
  end;
  Book.SaveAs(AStream);
end;
{$ENDIF}

procedure TRLXLSWorkbook.SaveToStream(AStream: TStream);
begin
  {$ifdef NATIVEEXCEL}
  NativeExcelSaveToStream(AStream);
  {$ELSE}
  FortesSaveToStream(AStream);
  {$ENDIF}
end;

{ TRLXLSTabs }

function TRLXLSTabs.GetTabs(I: Integer): TRLXLSTab;
begin
  Result := Items[I] as TRLXLSTab;
end;

function TRLXLSTabs.InsertTab(APosition, AComplementPosition: Integer; AAlignment: TRLMetaTextAlignment; TextOrigin: AnsiString): TRLXLSTab;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (Tabs[I].Position < APosition) do
    Inc(I);
  if I < Count then
    if Tabs[I].Position = APosition then
    begin
      Result := Tabs[I];
      Inc(Result.Count);
      Exit;
    end;
  Result := TRLXLSTab.Create;
  Insert(I, Result);
  Result.Position := APosition;
  Result.ComplementPosition:=AComplementPosition;
  Result.Alignment := AAlignment;
  Result.Count := 1;
  Result.TextOrigin:=TextOrigin;
end;

procedure TRLXLSTabs.RemoveTab(APosition: Integer);
var
  Tab: TRLXLSTab;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Tab := Tabs[I];
    if Tab.Position = APosition then
      Delete(I);
  end;
end;

function TRLXLSTabs.GetColumns(AMinWidth: Integer): TRLXLSTabColumns;
var
  I, ColCount, ColWidth, LastPosition: Integer;
begin
  // N tabulacoes dao na maximo N+1 colunas
  SetLength(Result, Self.Count + 1);
  ColCount := 0;
  LastPosition := -1;
  for I := 0 to Self.Count - 2 do
  begin
    if LastPosition = -1 then
      LastPosition := Tabs[I].Position;
    ColWidth := Tabs[I + 1].Position - LastPosition;
    if ColWidth >= AMinWidth then
    begin
      Result[ColCount].StartPos := LastPosition;
      Result[ColCount].EndPos := Tabs[I + 1].Position;
      Result[ColCount].Width := ColWidth;
      LastPosition := Result[ColCount].EndPos;
      Inc(ColCount);
    end;
  end;
  SetLength(Result, ColCount);
end;

(*
  if IsNewSheet then
    with FHorzTabs do
      for I := 1 to Count - 1 do
        Tabs[I - 1].Length := Tabs[I].Position - Tabs[I - 1].Position;
  with FVertTabs do
    for I := 1 to Count - 1 do
      Tabs[I - 1].Length := Tabs[I].Position - Tabs[I - 1].Position;
  // retira as colunas nulas
  if IsNewSheet then
    with FHorzTabs do
      for I := Count - 1 downto 0 do
        if Tabs[I].Length < MinHorzTwips then
          FHorzTabs.RemoveTabIndex(I);
  with FVertTabs do
    for I := Count - 1 downto 0 do
      if Tabs[I].Length < MinVertTwips then
        FVertTabs.RemoveTabIndex(I);

*)

{ TRLXLSFilter }

constructor TRLXLSFilter.Create(AOwner: TComponent);
begin
  inherited;
  FWorkbook := TRLXLSWorkbook.Create;
  FOptions := [];
  FHorzTabs := TRLXLSTabs.Create;
  FVertTabs := TRLXLSTabs.Create;
  DefaultExt := '.xls';
  DisplayName := GetLocalizeStr(LocaleStrings.LS_XLSFormatStr97_2013);
///  FilterStyle := FilterStyle + []; ///fsSetupDialog];
end;

destructor TRLXLSFilter.Destroy;
begin
  inherited;
  if Assigned(FWorkbook) then
    FWorkbook.Free;
  if Assigned(FHorzTabs) then
    FHorzTabs.Free;
  if Assigned(FVertTabs) then
    FVertTabs.Free;
end;

procedure TRLXLSWorkbook.WriteBIFF(AStream: TStream; ACode: Word; ABuff: Pointer; ASize: Integer);
var
  sz: Word;
begin
  repeat
    AStream.Write(ACode, 2);
    sz := Min(ASize, MaxBiffRecordSize - 4);
    AStream.Write(sz, 2);
    if sz > 0 then
    begin
      AStream.Write(ABuff^, sz);
      ABuff := Pointer(PtrInt(ABuff) + sz);
      ASize := ASize - sz;
      ACode := B8_CONTINUE;
    end;
  until ASize = 0;
end;

procedure TRLXLSWorkbook.WriteBIFFFont(AStream: TStream; AFont: TFont; AColorPaletteIndex: Word);
var
  room: Integer;
  font: PRLXLSBiff8FONT;
{$ifdef MSWINDOWS}
  lf: TLogFont;
{$endif}
begin
  room := WideStringSize(AnsiString(AFont.Name));
  font := AllocMem(SizeOf(TRLXLSBiff8FONT) + room);
  try
{$ifdef MSWINDOWS}
    GetObject(AFont.Handle, SizeOf(TLogFont), @lf);
{$endif}
    StringToWideChar(AnsiString(AFont.Name), PWideChar(PtrInt(font) + SizeOf(TRLXLSBiff8FONT)), room);
    font.dyHeight := AFont.Size * 20;
    if fsItalic in AFont.Style then
      font.grbit := font.grbit or B8_FONT_grbit_fItalic;
    if fsStrikeout in AFont.Style then
      font.grbit := font.grbit or B8_FONT_grbit_fStrikeout;
    font.icv := AColorPaletteIndex;
    if fsBold in AFont.Style then
      font.bls := $3E8 // ref MSDN
    else
      font.bls := $64; // ref MSDN
    if fsUnderline in AFont.Style then
      font.uls := 1; // ref MSDN
{$ifdef MSWINDOWS}
    font.bFamily := lf.lfPitchAndFamily;
    font.bCharSet := lf.lfCharSet;
{$else}
    case AFont.Pitch of
      fpDefault: font.bFamily := 0;
      fpFixed: font.bFamily := 1;
      fpVariable: font.bFamily := 2;
    end;
    case AFont.CharSet of
      {$IfDef CLX}
       fcsLatin1: font.bCharSet := 0;
       fcsKOI8R: font.bCharSet := 130;
       fcsSet_Ja: font.bCharSet := $80;
       fcsSet_Ko: font.bCharSet := 129;
      {$EndIf}
      ANSI_CHARSET: font.bCharSet := 0;
      DEFAULT_CHARSET: font.bCharSet := 1;
      SYMBOL_CHARSET: font.bCharSet := 2;
      MAC_CHARSET: font.bCharSet := 77;
      SHIFTJIS_CHARSET: font.bCharSet := $80;
      HANGEUL_CHARSET: font.bCharSet := 129;
      JOHAB_CHARSET: font.bCharSet := 130;
      GB2312_CHARSET: font.bCharSet := 134;
      CHINESEBIG5_CHARSET: font.bCharSet := 136;
      GREEK_CHARSET: font.bCharSet := 161;
      TURKISH_CHARSET: font.bCharSet := 162;
      HEBREW_CHARSET: font.bCharSet := 177;
      ARABIC_CHARSET: font.bCharSet := 178;
      BALTIC_CHARSET: font.bCharSet := 186;
      RUSSIAN_CHARSET: font.bCharSet := 204;
      THAI_CHARSET: font.bCharSet := 222;
      EASTEUROPE_CHARSET: font.bCharSet := 238;
      OEM_CHARSET: font.bCharSet := 255;
    else
      font.bCharSet := 0;
    end;
{$endif}
    font.cch := Length(AFont.Name);
    font.cchgrbit := $01;
    WriteBIFF(AStream, B8_FONT, font, SizeOf(TRLXLSBiff8FONT) + room);
  finally
    FreeMem(font);
  end;
end;

procedure TRLXLSWorkbook.WriteBIFFFormat(AStream: TStream; const AFormatString: AnsiString; AFormatCode: Word);
var
  format: PRLXLSBiff8FORMAT;
  room: Integer;
begin
  room := WideStringSize(AFormatString);
  format := AllocMem(SizeOf(TRLXLSBiff8FORMAT) + room);
  try
    StringToWideChar(AFormatString, PWideChar(PtrInt(format) + SizeOf(TRLXLSBiff8FORMAT)), room);
    format.ifmt := AFormatCode;
    format.cch := Length(AFormatString);
    format.cchgrbit := $01;
    WriteBIFF(AStream, B8_FORMAT, format, SizeOf(TRLXLSBiff8FORMAT) + room);
  finally
    FreeMem(format);
  end;
end;

procedure TRLXLSWorkbook.BuildFontList(FontList: TObjectList);
var
  OneRange: TRLXLSRange;
  OneSheet: TRLXLSWorksheet;
  FontFound: TFont;
  I, SheetIndex, RangeIndex, RecIndex, FontIndex: Integer;
begin
  // adiciono as fontes default
  for I := 0 to 3 do
  begin
    FontFound := TFont.Create;
    FontList.Add(FontFound);
    FontFound.Name := DefaultFontName;
    FontFound.Size := 10;
    FontFound.Color := clBlack;
  end;

  RecIndex := 0;
  for SheetIndex := 0 to Self.SheetCount - 1 do
  begin
    OneSheet := Self.Sheets[SheetIndex];
    for RangeIndex := 0 to OneSheet.RangeCount - 1 do
    begin
      OneRange := OneSheet.Ranges[RangeIndex];
      OneRange.ExportData := @FRangesRecs[RecIndex];

      FontFound := FindSimilarFont(FontList, OneRange.Font);
      if FontFound = nil then
      begin
        FontFound := TFont.Create;
        FontList.Add(FontFound);
        FontFound.Assign(OneRange.Font);
      end;
      FontIndex := FontList.IndexOf(FontFound);
      
      FRangesRecs[RecIndex].iFont := FontIndex + 1;
      Inc(RecIndex);
    end;
  end;
end;

procedure TRLXLSWorkbook.BuildFormatList(FormatList: TStringList);
var
  sheet: TRLXLSWorksheet;
  range: TRLXLSRange;
  I, J: Integer;
  K, N: Integer;
  M: Integer;
begin
  N := FormatList.Count;
  M := 0;
  for I := 0 to Self.SheetCount - 1 do
  begin
    sheet := Self.Sheets[I];
    for J := 0 to sheet.RangeCount - 1 do
    begin
      range := sheet.Ranges[J];
      if range.Format = '' then
        FRangesRecs[M].iFormat := 0
      else
      begin
        K := FormatList.IndexOf(range.Format);
        if K = -1 then
          K := FormatList.AddObject(range.Format, Pointer(FormatList.Count - N + $32));
        FRangesRecs[M].iFormat := PtrInt(FormatList.Objects[K]);
      end;
      Inc(M);
    end;
  end;
end;

procedure TRLXLSWorkbook.BuildXFRecord(ARange: TRLXLSRange; var AXF: TRLXLSBiff8XF; ARec: PXLSRangeRec);
const
  FillPatterns: array[TRLXLSFillPattern] of Integer = (0, -4105, 9, 16, -4121, 18, 
    17, -4124, -4125, -4126, 15, -4128, 13, 11, 14, 12, 10, 1, -4162, -4166);
  HorizontalAlignments: array[TRLXLSHorizontalAlignmentType] of Integer = 
    (B8_XF_Opt2_alcGeneral, 
     B8_XF_Opt2_alcLeft, 
     B8_XF_Opt2_alcCenter, 
     B8_XF_Opt2_alcRight, 
     B8_XF_Opt2_alcFill, 
     B8_XF_Opt2_alcJustify, 
     B8_XF_Opt2_alcCenterAcrossSelection);
  VerticalAlignments: array[TRLXLSVerticalAlignmentType] of Integer = 
    (B8_XF_Opt2_alcVTop, 
     B8_XF_Opt2_alcVCenter, 
     B8_XF_Opt2_alcVBottom, 
     B8_XF_Opt2_alcVJustify);
  WrapTexts: array[Boolean] of Integer = (0, B8_XF_Opt2_fWrap);
  BorderLineStyles: array[TRLXLSLineStyleType] of Word = 
    (B8_XF_Border_None, 
     B8_XF_Border_Thin, 
     B8_XF_Border_Medium, 
     B8_XF_Border_Dashed, 
     B8_XF_Border_Dotted, 
     B8_XF_Border_Thick, 
     B8_XF_Border_Double, 
     B8_XF_Border_Hair, 
     B8_XF_Border_MediumDashed, 
     B8_XF_Border_DashDot, 
     B8_XF_Border_MediumDashDot, 
     B8_XF_Border_DashDotDot, 
     B8_XF_Border_MediumDashDotDot, 
     B8_XF_Border_SlantedDashDot);
  function GetBorderColorIndex(B: TRLXLSBorderType): Integer;
  begin
    if ARange.Borders[B].LineStyle = lsNone then
      Result := 0
    else
      Result := GetColorPaletteIndex(ARange.Borders[B].Color) + 8; // ???+8
  end;
var
  DiagBorderLineStyle: TRLXLSLineStyleType;
  DiagBorderColorIndex: Integer;
begin
  FillChar(AXF, SizeOf(AXF), 0);
  AXF.ifnt := ARec.iFont;
  AXF.ifmt := PXLSRangeRec(ARange.ExportData).iFormat;
  AXF.Opt1 := $0001;
  AXF.Opt2 := HorizontalAlignments[ARange.HorizontalAlignment] or
    WrapTexts[ARange.WrapText] or
    VerticalAlignments[ARange.VerticalAlignment];
  AXF.trot := ARange.Rotation;
  AXF.Opt3 := B8_XF_Opt3_fAtrNum or
    B8_XF_Opt3_fAtrFnt or
    B8_XF_Opt3_fAtrAlc or
    B8_XF_Opt3_fAtrBdr or
    B8_XF_Opt3_fAtrPat;
  if (ARange.CellArea.X0 <> ARange.CellArea.X1) or (ARange.CellArea.Y0 <> ARange.CellArea.Y1) then
    AXF.Opt3 := AXF.Opt3 or B8_XF_Opt3_fMergeCell;
  AXF.Borders1 := (BorderLineStyles[ARange.Borders[bdEdgeLeft].LineStyle]) or
    (BorderLineStyles[ARange.Borders[bdEdgeRight].LineStyle] shl 4) or
    (BorderLineStyles[ARange.Borders[bdEdgeTop].LineStyle] shl 8) or
    (BorderLineStyles[ARange.Borders[bdEdgeBottom].LineStyle] shl 12);
  DiagBorderLineStyle := lsNone;
  DiagBorderColorIndex := 0;
  AXF.Borders2 := 0;
  if ARange.Borders[bdDiagonalDown].LineStyle <> lsNone then
  begin
    AXF.Borders2 := AXF.Borders2 or $4000;
    DiagBorderLineStyle := ARange.Borders[bdDiagonalDown].LineStyle;
    DiagBorderColorIndex := GetColorPaletteIndex(ARange.Borders[bdDiagonalDown].Color) + 8;
  end;
  if ARange.Borders[bdDiagonalUp].LineStyle <> lsNone then
  begin
    AXF.Borders2 := AXF.Borders2 or $8000;
    DiagBorderLineStyle := ARange.Borders[bdDiagonalUp].LineStyle;
    DiagBorderColorIndex := GetColorPaletteIndex(ARange.Borders[bdDiagonalUp].Color) + 8;
  end;
  AXF.Borders2 := AXF.Borders2 or (GetBorderColorIndex(bdEdgeLeft)) or (GetBorderColorIndex(bdEdgeRight) shl 7);
  AXF.Borders3 := (GetBorderColorIndex(bdEdgeTop)) or (GetBorderColorIndex(bdEdgeBottom) shl 7) or
    (DiagBorderColorIndex shl 14) or (BorderLineStyles[DiagBorderLineStyle] shl 21) or
    (FillPatterns[ARange.FillPattern] shl 26);
  AXF.Colors := GetColorPaletteIndex(ARange.ForegroundColor) or
    (GetColorPaletteIndex(ARange.BackgroundColor) shl 7);
end;

procedure TRLXLSWorkbook.BuildXFList(XFList: TList);
var
  OneSheet: TRLXLSWorksheet;
  OneRange: TRLXLSRange;
  xf: TRLXLSBiff8XF;
  I, J: Integer;
  K, N: Integer;
  P: Pointer;
begin
  N := 0;
  for I := 0 to Self.SheetCount - 1 do
  begin
    OneSheet := Self.Sheets[I];
    for J := 0 to OneSheet.RangeCount - 1 do
    begin
      OneRange := OneSheet.Ranges[J];
      BuildXFRecord(OneRange, xf, @FRangesRecs[N]);
      K := 0;
      while (K < XFList.Count) and not CompareMem(XFList[K], @xf, SizeOf(TRLXLSBiff8XF)) do
        Inc(K);
      if K < XFList.Count then
      else
      begin
        GetMem(P, SizeOf(TRLXLSBiff8XF));
        Move(xf, P^, SizeOf(TRLXLSBiff8XF));
        K := XFList.Add(P);
      end;
      FRangesRecs[N].iXF := K + 15;
      Inc(N);
    end;
  end;
end;

function TRLXLSWorkbook.GetColorPaletteIndex(AColor: TColor): Integer;
  function DefaultColorIndex(C: TColor): Integer;
  begin
    Result := 0;
    while (Result < XLSMaxDefaultColors) and (XLSDefaultColors[Result] <> C) do 
      Inc(Result);
    if Result >= XLSMaxDefaultColors then
      Result := -1;
  end; 
begin
  if (AColor and $80000000) <> 0 then
    AColor := ColorToRGB(AColor and $00FFFFFF);
  if FUsedColors.IndexOf(Pointer(AColor)) = -1 then
    FUsedColors.Add(Pointer(AColor));
  Result := 0;
  while (Result < XLSMaxColorsInPalette) and (FColorPalette[Result] <> AColor) do
    Inc(Result);
  if Result < XLSMaxColorsInPalette then
    Exit;
  Result := 0;
  while Result < XLSMaxColorsInPalette do
  begin
    if (DefaultColorIndex(FColorPalette[Result]) = -1) and (FUsedColors.IndexOf(Pointer(FColorPalette[Result])) = -1) then
    begin
      FColorPalette[Result] := AColor;
      FPaletteModified := True;
      Exit;
    end;
    Inc(Result);
  end;
  Result := 1;
end;

procedure TRLXLSWorkbook.WriteStylesToStream(AStream: TStream);
begin
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$01#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$01#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$02#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$02#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
  StreamWrite(AStream, #$e0#$00#$14#$00#$00#$00#$00#$00#$f5#$ff#$20#$00#$00#$f4#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20);
end;

procedure TRLXLSWorkbook.WriteFontsToStream(AStream: TStream);
var
  FontList: TObjectList;
  OneFont: TFont;
  I: Integer;
begin
  FontList := TObjectList.Create;
  try
    BuildFontList(FontList);

    // escrevo as fontes de fato no arquivo
    for I := 0 to FontList.Count - 1 do
    begin
      OneFont := FontList[I] as TFont;
      WriteBIFFFont(AStream, OneFont, GetColorPaletteIndex(OneFont.Color));
    end;
  finally
    FontList.Free;
  end;
end;

procedure TRLXLSWorkbook.WriteFormatsToStream(AStream: TStream);
var
  FormatList: TStringList;
  I: Integer;
begin
  FormatList := TStringList.Create;
  try
    FormatList.AddObject('#,##0"ð.";\-#,##0"ð."', Pointer($0005));
    FormatList.AddObject('#,##0"ð.";[Red]\-#,##0"ð."', Pointer($0006));
    FormatList.AddObject('#,##0.00"ð.";\-#,##0.00"ð."', Pointer($0007));
    FormatList.AddObject('#,##0.00"ð.";[Red]\-#,##0.00"ð."', Pointer($0008));
    FormatList.AddObject('_-* #,##0"ð."_-;\-* #,##0"ð."_-;_-* "-""ð."_-;_-@_-', Pointer($002A));
    FormatList.AddObject('_-* #,##0_ð_._-;\-* #,##0_ð_._-;_-* "-"_ð_._-;_-@_-', Pointer($0029));
    FormatList.AddObject('_-* #,##0.00"ð."_-;\-* #,##0.00"ð."_-;_-* "-"??"ð."_-;_-@_-', Pointer($002C));
    FormatList.AddObject('_-* #,##0.00_ð_._-;\-* #,##0.00_ð_._-;_-* "-"??_ð_._-;_-@_-', Pointer($002B));
    FormatList.AddObject('dd\/mm\/yyyy', Pointer($002D));
///      FormatList.AddObject('hh\:nn\:ss', Pointer($012E));
    BuildFormatList(FormatList);
    for I := 0 to FormatList.Count - 1 do
      WriteBIFFFormat(AStream, FormatList[I], Word(FormatList.Objects[I]));
  finally
    FormatList.Free;
  end;
end;

procedure TRLXLSWorkbook.WriteXFListToStream(AStream: TStream);
var
  XFList: TList;
  Aux: AnsiString;
  Buf: Pointer;
  I: Integer;
begin
  XFList := TList.Create;
  try
    Aux := #$00#$00#$00#$00#$01#$00#$20#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$c0#$20;
    GetMem(Buf, Length(Aux));
    Move(Aux[1], Buf^, Length(Aux));
    XFList.Add(Buf);

    BuildXFList(XFList);
    for I := 0 to XFList.Count - 1 do
      WriteBIFF(AStream, B8_XF, XFList[I], SizeOf(TRLXLSBiff8XF));
  finally
    for I := 0 to XFList.Count - 1 do
      FreeMem(XFList[I]);
    XFList.Free;
  end;
end;

procedure TRLXLSWorkbook.WritePaletteToStream(AStream: TStream);
var
  Palette: TRLXLSBiff8PALETTE;
  I: Integer;
begin
  Palette.ccv := XLSMaxColorsInPalette;
  for I := 0 to XLSMaxColorsInPalette - 1 do
    Palette.colors[I] := FColorPalette[I];
  WriteBIFF(AStream, B8_PALETTE, @Palette, SizeOf(Palette));
end;

procedure TRLXLSWorkbook.WriteBoundSheetToStream(AStream: TStream;
  var ASheetRec: TRLXLSSheetRec; ASheet: TRLXLSWorksheet);
var
  BoundSheet: PRLXLSBiff8BOUNDSHEET;
  TitleLen: Integer;
begin
  ASheetRec.StreamBOFOffsetPosition := AStream.Position + 4;
  TitleLen := WideStringSize(ASheet.Title);
  BoundSheet := AllocMem(SizeOf(TRLXLSBiff8BOUNDSHEET) + TitleLen);
  try
    BoundSheet.grbit := 0;
    BoundSheet.cch := Length(ASheet.Title);
    BoundSheet.cchgrbit := 1;
    if BoundSheet.cch > 0 then
      StringToWideChar(ASheet.Title, PWideChar(PtrInt(BoundSheet) + SizeOf(TRLXLSBiff8BOUNDSHEET)), TitleLen);
    WriteBIFF(AStream, B8_BOUNDSHEET, BoundSheet, SizeOf(TRLXLSBiff8BOUNDSHEET) + TitleLen);
  finally
    FreeMem(BoundSheet);
  end;
end;

procedure TRLXLSWorkbook.WriteWindowData(AStream: TStream);
var
  window1: TRLXLSBiff8WINDOW1;
begin
  FillChar(window1, SizeOf(window1), 0);
  window1.xWn := $0168;
  window1.yWn := $001E;
  window1.dxWn := $1D1E;
  window1.dyWn := $1860;
  window1.grbit := $0038;
  window1.itabCur := $0000;
  window1.itabFirst := $0000;
  window1.ctabSel := $0001;
  window1.wTabRatio := $0258;
  WriteBIFF(AStream, B8_WINDOW1, @window1, SizeOf(window1));
end;

procedure TRLXLSWorkbook.WriteBookHeader(AStream: TStream);
var
  bof: TRLXLSBiff8BOF;
  interfachdr: TRLXLSBiff8InterfaceHeader;
  mms: TRLXLSBiff8MMS;
  writeaccess: TRLXLSBiff8WRITEACCESS;
  codepage: TRLXLSBiff8CodePage;
  doublestreamfile: TRLXLSBiff8DOUBLESTREAMFILE;
begin
  FillChar(bof, SizeOf(bof), 0);
  bof.vers := B8_BOF_vers;
  bof.dt := B8_BOF_dt_WorkbookGlobals;
  bof.rupBuild := B8_BOF_rupBuild_Excel97;
  bof.rupYear := B8_BOF_rupYear_Excel97;
  bof.bfh := 0;
  bof.sfo := B8_BOF_vers;
  FillChar(interfachdr, SizeOf(interfachdr), 0);
  interfachdr.cv := B8_INTERFACHDR_cv_ANSI;
  FillChar(mms, SizeOf(mms), 0);
  FillChar(writeaccess, SizeOf(writeaccess), 0);
  StringToWideChar(Self.UserName, @writeaccess.stName, sizeof(writeaccess));
  codepage.cv := B8_CODEPAGE_cv_ANSI;
  doublestreamfile.FDSF := 0;

  WriteBIFF(AStream, B8_BOF, @bof, SizeOf(bof));
  WriteBIFF(AStream, B8_INTERFACHDR, @interfachdr, SizeOf(interfachdr));
  WriteBIFF(AStream, B8_MMS, @mms, SizeOf(mms));
  WriteBIFF(AStream, B8_INTERFACEND, nil, 0);
  WriteBIFF(AStream, B8_WRITEACCESS, @writeaccess, SizeOf(writeaccess));
  WriteBIFF(AStream, B8_CODEPAGE, @codepage, SizeOf(codepage));
  WriteBIFF(AStream, B8_DOUBLESTREAMFILE, @doublestreamfile, SizeOf(doublestreamfile));
  WriteBIFF(AStream, $01C0, nil, 0);
end;

procedure TRLXLSWorkbook.WriteHeaderExtras(AStream: TStream);
var
  fngroupcount: TRLXLSBiff8FNGroupCount;
  windowprotect: TRLXLSBiff8WindowProtect;
  protect: TRLXLSBiff8Protect;
  password: TRLXLSBiff8Password;
  prot4rev: TRLXLSBiff8PROT4REV;
  prot4revpass: TRLXLSBiff8PROT4REVPASS;
begin
  fngroupcount.cFnGroup := $000E;
  windowprotect.FLockWn := 0;
  protect.FLock := 0;
  password.wPassword := 0;
  prot4rev.FRevLock := 0;
  prot4revpass.wrevPass := 0;

  WriteBIFF(AStream, B8_FNGROUPCOUNT, @fngroupcount, SizeOf(fngroupcount));
  WriteBIFF(AStream, B8_WINDOWPROTECT, @windowprotect, SizeOf(windowprotect));
  WriteBIFF(AStream, B8_PROTECT, @protect, SizeOf(protect));
  WriteBIFF(AStream, B8_PASSWORD, @password, SizeOf(password));
  WriteBIFF(AStream, B8_PROT4REV, @prot4rev, SizeOf(prot4rev));
  WriteBIFF(AStream, B8_PROT4REVPASS, @prot4revpass, SizeOf(prot4revpass));
end;

procedure TRLXLSWorkbook.WriteSSTTable(AStream: TStream);
var
  extsstsize: Integer;
  sst, sstbuf: PAnsiChar;
  SSTCount: Integer;
  sstsizeoffset: Integer;
  sstblockoffset: Integer;
  lsstbuf: Integer;
  sstsize: Integer;
  extsst: PRLXLSBiff8EXTSST;
  SheetIndex, GlobalRangeIndex, RangeIndex: Integer;
  OneSheet: TRLXLSWorksheet;
  OneRange: TRLXLSRange;
  RangeText: AnsiString;
  RangeTextLen: Integer;
  ltitleoffset: Integer;
  sz: Word;
  Biff8SST: PRLXLSBiff8SST;
  Biff8ISSTINF: PRLXLSBiff8ISSTINF;
begin
  extsstsize := SizeOf(TRLXLSBiff8EXTSST);
  extsst := AllocMem(extsstsize);
  extsst.Dsst := 8;

  sstsize := SizeOf(TRLXLSBiff8SST) + 4;
  sst := AllocMem(sstsize);
  PWord(sst)^ := B8_SST;
  sstsizeoffset := 2;
  PWord(sst + sstsizeoffset)^ := SizeOf(TRLXLSBiff8SST);
  sstblockoffset := sstsize;
  lsstbuf := 0;
  sstbuf := nil;
  
  SSTCount := 0;
  GlobalRangeIndex := 0;
  try
    for SheetIndex := 0 to Self.SheetCount - 1 do
    begin
      OneSheet := Self.Sheets[SheetIndex];
      for RangeIndex := 0 to OneSheet.RangeCount - 1 do
      begin
        OneRange := OneSheet.Ranges[RangeIndex];
        if OneRange.DataType = ctString then
        begin
          RangeText := OneRange.Value;
          if RangeText <> '' then
          begin
            FRangesRecs[GlobalRangeIndex].iSST := SSTCount;
            Inc(SSTCount);
            RangeTextLen := WideStringSize(RangeText);
            if lsstbuf < RangeTextLen then
            begin
              lsstbuf := RangeTextLen;
              ReallocMem(sstbuf, lsstbuf);
            end;
            StringToWideChar(RangeText, PWideChar(sstbuf), RangeTextLen);

            if MaxBiffRecordSize - sstblockoffset <= 4 then
            begin
              ReallocMem(sst, sstsize + 4);
              PWord(sst + sstsize)^ := B8_CONTINUE;
              sstsize := sstsize + 2;
              sstsizeoffset := sstsize;
              PWord(sst + sstsize)^ := 0;
              sstsize := sstsize + 2;
              sstblockoffset := 4;
            end;

            if (SSTCount mod 8) = 1 then
            begin
              ReallocMem(extsst, extsstsize + SizeOf(TRLXLSBiff8ISSTINF));
              Biff8ISSTINF := PRLXLSBiff8ISSTINF(PAnsiChar(extsst) + extsstsize);
              Biff8ISSTINF.cb := sstblockoffset;
              Biff8ISSTINF.ib := AStream.Position + sstsize;
              Biff8ISSTINF.res1 := 0;
              Inc(extsstsize, SizeOf(TRLXLSBiff8ISSTINF));
            end;

            ReallocMem(sst, sstsize + 3);
            PWord(sst + sstsize)^ := Length(RangeText);
            sstsize := sstsize + 2;
            PByte(sst + sstsize)^ := 1;
            sstsize := sstsize + 1;
            PWord(sst + sstsizeoffset)^ := PWord(sst + sstsizeoffset)^ + 3;
            sstblockoffset := sstblockoffset + 3;

            ltitleoffset := 0;
            repeat
              sz := (Min(RangeTextLen - ltitleoffset, MaxBiffRecordSize - sstblockoffset)) and (not 1);
              ReallocMem(sst, sstsize + sz);
              Move(Pointer(PtrInt(sstbuf) + ltitleoffset)^, Pointer(PtrInt(sst) + sstsize)^, sz);
              sstsize := sstsize + sz;
              sstblockoffset := sstblockoffset + sz;
              ltitleoffset := ltitleoffset + sz;
              PWord(sst + sstsizeoffset)^ := PWord(sst + sstsizeoffset)^ + sz;
              if (RangeTextLen > ltitleoffset) and ((MaxBiffRecordSize - sstblockoffset) <= 4) then
              begin
                ReallocMem(sst, sstsize + 5);
                PWord(sst + sstsize)^ := B8_CONTINUE;
                sstsize := sstsize + 2;
                sstsizeoffset := sstsize;
                PWord(sst + sstsize)^ := 1;
                sstsize := sstsize + 2;
                PByte(sst + sstsize)^ := 1;
                sstsize := sstsize + 1;
                sstblockoffset := 5;
              end;
            until RangeTextLen <= ltitleoffset;
          end;
        end;
        Inc(GlobalRangeIndex);
      end;
    end;
    if SSTCount <> 0 then
    begin
      Biff8SST := PRLXLSBiff8SST(sst + 4);
      Biff8SST.cstTotal := SSTCount;
      Biff8SST.cstUnique := SSTCount;
      AStream.Write(sst^, sstsize);
      WriteBIFF(AStream, B8_EXTSST, extsst, extsstsize);
    end;
  finally
    FreeMem(sst);
    FreeMem(sstbuf);
    FreeMem(extsst);
  end;
end;

procedure TRLXLSWorkbook.WriteBookToStream(AStream: TStream);
var
  backup: TRLXLSBiff8BACKUP;
  hideobj: TRLXLSBiff8HIDEOBJ;
  s1904: TRLXLSBiff81904;
  precision: TRLXLSBiff8PRECISION;
  bookbool: TRLXLSBiff8BOOKBOOL;
  refreshall: TRLXLSBiff8REFRESHALL;
  useselfs: TRLXLSBiff8USESELFS;
  country: TRLXLSBiff8COUNTRY;
  I: Integer;
  buf: Pointer;
  TotalRangeCount: Integer;
begin
  // dimensiono o vetor de Faixas (uma faixa eh um retangulo de celulas mergeadas)
  TotalRangeCount := 0;
  for I := 0 to Self.SheetCount - 1 do
    TotalRangeCount := TotalRangeCount + Self.Sheets[I].RangeCount;
  GetMem(FRangesRecs, TotalRangeCount * SizeOf(TRLXLSRangeRec));

  // dimensiono o vetor de abas
  GetMem(FSheetsRecs, Self.SheetCount * SizeOf(TRLXLSSheetRec));
  try
    // carrego as cores padrao dentro da paleta deste workbook
    Move(XLSDefaultColorPalette[0], FColorPalette[0], XLSMaxColorsInPalette * 4);
    FPaletteModified := False;
    FUsedColors.Clear;

    FBOFOffs := AStream.Position;
    WriteBookHeader(AStream);

    GetMem(buf, Self.SheetCount * 2);
    try
      for I := 0 to Self.SheetCount - 1 do
        PWordArray(buf)^[I] := I;
      WriteBIFF(AStream, B8_TABID, buf, Self.SheetCount * 2);
    finally
      FreeMem(buf);
    end;

    WriteHeaderExtras(AStream);

    WriteWindowData(AStream);

    backup.FBackupFile := 0; 
    WriteBIFF(AStream, B8_BACKUP, @backup, SizeOf(backup));
  
    hideobj.FHideObj := 0; 
    WriteBIFF(AStream, B8_HIDEOBJ, @hideobj, SizeOf(hideobj));

    s1904.f1904 := 0; 
    WriteBIFF(AStream, B8_1904, @s1904, SizeOf(s1904));
  
    precision.FFullPrec := 1;
    WriteBIFF(AStream, B8_PRECISION, @precision, SizeOf(precision));

    refreshall.FRefreshAll := 0;
    WriteBIFF(AStream, B8_REFRESHALL, @refreshall, SizeOf(refreshall));
  
    bookbool.FNoSaveSupp := 0;
    WriteBIFF(AStream, B8_BOOKBOOL, @bookbool, SizeOf(bookbool));

    // grava a lista de fontes, primeiro as fontes default, depois as fontes extras usadas na planilha
    WriteFontsToStream(AStream);

    // grava os formatos de celula padrao no arquivo
    WriteFormatsToStream(AStream);

    // salva estilos padrao no arquivo
    WriteStylesToStream(AStream);

    // XF
    WriteXFListToStream(AStream);

    // grava a paleta de cores
    if FPaletteModified then
      WritePaletteToStream(AStream);

    StreamWrite(AStream, #$93#$02#$04#$00#$10#$80#$04#$FF);
    StreamWrite(AStream, #$93#$02#$04#$00#$11#$80#$07#$FF);
    StreamWrite(AStream, #$93#$02#$04#$00#$00#$80#$00#$FF);
    StreamWrite(AStream, #$93#$02#$04#$00#$12#$80#$05#$FF);
    StreamWrite(AStream, #$93#$02#$04#$00#$13#$80#$03#$FF);
    StreamWrite(AStream, #$93#$02#$04#$00#$14#$80#$06#$FF);

    useselfs.FUsesElfs := 0;
    WriteBIFF(AStream, B8_USESELFS, @useselfs, SizeOf(useselfs));
  
    // grava entradas de registro para as abas
    for I := 0 to Self.SheetCount - 1 do
      WriteBoundSheetToStream(AStream, FSheetsRecs[I], Self.Sheets[I]);

    country.iCountryDef := $07;
    country.iCountryWinIni := $07;
    WriteBIFF(AStream, B8_COUNTRY, @country, SizeOf(country));

    // SST TABLE
    WriteSSTTable(AStream);

    WriteBIFF(AStream, B8_EOF, nil, 0);
    for I := 0 to Self.SheetCount - 1 do
      WriteSheetDataToStream(AStream, FSheetsRecs[I], Self.Sheets[I]);
    for I := 0 to Self.SheetCount - 1 do
    begin
      AStream.Position := FSheetsRecs[I].StreamBOFOffsetPosition;
      AStream.Write(FSheetsRecs[I].StreamBOFOffset, 4);
    end;
  finally
    FUsedColors.Clear;
    FreeMem(FRangesRecs);
    FRangesRecs := nil;
    FreeMem(FSheetsRecs);
    FSheetsRecs := nil;
  end;
end;

const
  MaxHeaderFATEntries = 109;
  
type
  CHAR8 = packed array[0..7] of AnsiChar;
  CHAR16 = packed array[0..15] of AnsiChar;
  CHAR64 = packed array[0..32 * SizeOf(WideChar) - 1] of AnsiChar;
  SECT109 = packed array[0..MaxHeaderFATEntries - 1] of Cardinal;

type
  TStructuredStorageHeader = packed record
    _abSig: CHAR8; // [0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1] for current version
    _clid: CHAR16; // reserved must be zero (WriteClassStg/GetClassFile uses root directory class id)
    _uMinorVersion: Word; // minor version of the format: 33 is written by reference implementation
    _uDllVersion: Word; // major version of the dll/format: 3 for 512-byte sectors, 4 for 4 KB sectors
    _uByteOrder: Word; // 0xFFFE: indicates Intel byte-ordering
    _uSectorShift: Word; // size of sectors in power-of-two; typically 9 indicating 512-byte sectors
    _uMiniSectorShift: Word; // size of mini-sectors in power-of-two; typically 6 indicating 64-byte mini-sectors
    _usReserved: Word; // reserved, must be zero
    _ulReserved1: Cardinal; // reserved, must be zero
    _csectDir: Cardinal; // must be zero for 512-byte sectors, number of SECTs in directory chain for 4 KB sectors
    _csectFat: Cardinal; // number of SECTs in the FAT chain
    _sectDirStart: Cardinal; // first SECT in the directory chain
    _signature: Cardinal; // signature used for transactions; must be zero. The reference implementation does not support transactions
    _ulMiniSectorCutoff: Cardinal; // minimum size for a standard stream (not mini fat); typically 4096 bytes
    _sectMiniFatStart: Cardinal; // first SECT in the MiniFAT chain
    _csectMiniFat: Cardinal; // number of SECTs in the MiniFAT chain
    _sectDifStart: Cardinal; // first SECT in the DIFAT chain (> 7MB)
    _csectDif: Cardinal; // number of SECTs in the DIFAT chain
    _sectFat: SECT109; // the SECTs of first 109 FAT sectors
  end;

  TIME_T = packed record
    dwLowDateTime: Cardinal;
    dwHighDateTime: Cardinal;
  end;

  TIME_T2 = packed array[0..1] of TIME_T;

  TStructuredStorageDirectoryEntry = packed record
    _ab: CHAR64;
    _cb: Word;
    _mse: Byte;
    _bflags: Byte;
    _sidLeftSib: Cardinal;
    _sidRightSib: Cardinal;
    _sidChild: Cardinal;
    _clsId: CHAR16;
    _dwUserFlags: Cardinal;
    _time: TIME_T2;
    _sectStart: Cardinal;
    _ulSizeLow: Cardinal;
    _ulSizeHigh: Cardinal;
  end;

  TStructuredStorageFAT = packed array[0..128 - 1] of Cardinal;

procedure TRLXLSWorkbook.FortesSaveToStream(AStream: TStream);
const
  StorageSignature: AnsiString = #$D0#$CF#$11#$E0#$A1#$B1#$1A#$E1;
  RootEntry: AnsiString = 'R'#0'o'#0'o'#0't'#0' '#0'E'#0'n'#0't'#0'r'#0'y'#0#0#0;
  Workbook: AnsiString = 'W'#0'o'#0'r'#0'k'#0'b'#0'o'#0'o'#0'k'#0#0#0;
  MAXREGSECT: Cardinal = $FFFFFFFA;
  DIFSECT: Cardinal = $FFFFFFFC;
  FATSECT: Cardinal = $FFFFFFFD;
  ENDOFCHAIN: Cardinal = $FFFFFFFE;
  FREESECT: Cardinal = $FFFFFFFF;
  MAXREGSID: Cardinal = $FFFFFFFA; // maximum directory entry ID
  NOSTREAM: Cardinal = $FFFFFFFF; // unallocated directory entry
const
  // STGTY
  STGTY_INVALID = 0;
  STGTY_STORAGE = 1;
  STGTY_STREAM = 2;
  STGTY_LOCKBYTES = 3;
  STGTY_PROPERTY = 4;
  STGTY_ROOT = 5;
  // DECOLOR
  DE_RED = 0;
  DE_BLACK = 1;
const
  OneSectorSize = 512;
  OneSectorShift = 9;
  OneMiniSectorSize = 64;
  OneMiniSectorShift = 6;
  MiniSectorCutoff = 4096;
  FATEntriesPerSector = OneSectorSize div SizeOf(Cardinal);
var
  HeaderOffset: Cardinal;
  HeaderSectorRec: TStructuredStorageHeader;
  DataFirstSector: Cardinal;
  DataLastSector: Cardinal;
  DataSectorNo: Cardinal;
  DataTotalSize: Cardinal;
  DataSectorCount: Cardinal;
  FATOffset: Cardinal;
  FATFirstSector: Cardinal;
  FATLastSector: Cardinal;
  FATSectorNo: Cardinal;
  FATSectorRec: TStructuredStorageFAT;
  FATItemCount: Cardinal;
  FATSectorCount: Cardinal;
  DirectoryOffset: Cardinal;
  DirectoryFirstSector: Cardinal;
  DirectoryLastSector: Cardinal;

  DirectorySectorNo: Cardinal;
  DirectorySectorRec: TStructuredStorageDirectoryEntry;
  SectorRec: packed array[0..OneSectorSize - 1] of AnsiChar;
  BytesRead: Integer;
  NextSectorNo: Cardinal;
  I: Cardinal;
  TempFileName: AnsiString;
  TempFileStream: TFileStream;

  procedure WriteSectorRec;
  begin
    AStream.Write(SectorRec, SizeOf(SectorRec));
    Inc(NextSectorNo);
  end;

  function NewFATItemIndex: Cardinal;
  begin
    Inc(FATItemCount);
    // esta fat já encheu, grava e passa pra outra
    if FATItemCount > FATEntriesPerSector then
    begin
      AStream.Write(FATSectorRec, SizeOf(FATSectorRec));
      FATItemCount := 1;
    end;
    Result := FATItemCount - 1;
  end;

begin
  // reserva e marca o primeiro setor (offset 0) para o header
  HeaderOffset := AStream.Position;
  WriteSectorRec;

  // o setor de numero 0 é gravado na verdade no offset 512 do arquivo
  // o offset de um setor é calculado pela formula: (SectorNo+1) shl _uSectorShift
  // onde _uSectorShift normalmente é 9, que indica setores de 512 bytes
  NextSectorNo := 0;
  DataTotalSize := 0;
  
  // re-escreve o arquivo xls puro num aquivo temporario
  // depois, grava o temporario em setores de dados de 512 bytes no stream do storage
  TempFileName := GetTempFileName;
  TempFileStream := TFileStream.Create(TempFileName, fmCreate);
  try
    WriteBookToStream(TempFileStream);
    TempFileStream.Position := 0;
    DataFirstSector := NextSectorNo;
    DataLastSector := NextSectorNo;
    repeat
      FillChar(SectorRec, SizeOf(SectorRec), 0);
      BytesRead := TempFileStream.Read(SectorRec, SizeOf(SectorRec));
      if BytesRead = 0 then
        Break;
      // marca o setor final dos dados   
      DataLastSector := NextSectorNo;
      WriteSectorRec;
      Inc(DataTotalSize, BytesRead);
    until False;
  finally
    TempFileStream.Free;
    SysUtils.DeleteFile(TempFileName);
  end;
  
  // quantidade de setores de 512 bytes necessários para os dados
  DataSectorCount := DataLastSector - DataFirstSector + 1;
  
  // calcula quantas fats de 128 entradas são necessárias para referenciar todos os setores
  FATSectorCount := (DataSectorCount + 128 - 1) div 128;

  // reserva (FATSectorCount) setores para a fat a partir do offset atual
  FATOffset := AStream.Position;
  FATFirstSector := NextSectorNo;
  FATLastSector := NextSectorNo;
  for I := 0 to FATSectorCount - 1 do
  begin
    FATLastSector := NextSectorNo;
    WriteSectorRec;
  end;

  // reserva um único setor para diretorio
  DirectoryOffset := AStream.Position;
  DirectoryFirstSector := NextSectorNo;
  DirectoryLastSector := NextSectorNo;
  WriteSectorRec;

  // atualiza o header e grava  
  FillChar(HeaderSectorRec, SizeOf(HeaderSectorRec), 0);
  Move(StorageSignature[1], HeaderSectorRec._abSig, Length(StorageSignature));
  HeaderSectorRec._uMinorVersion := $003E;
  HeaderSectorRec._uDllVersion := 3;
  HeaderSectorRec._uByteOrder := $FFFE;
  HeaderSectorRec._uSectorShift := 9;
  HeaderSectorRec._uMiniSectorShift := 6;
  HeaderSectorRec._csectFat := FATSectorCount;
  HeaderSectorRec._sectDirStart := DirectoryFirstSector; 
  /// todo: se os dados for menores que 4kb tem que usar o minifat (manual pg.3)
  HeaderSectorRec._ulMiniSectorCutoff := $00001000;
  HeaderSectorRec._sectMiniFatStart := ENDOFCHAIN;
  HeaderSectorRec._sectDifStart := ENDOFCHAIN;
  // o header tem MaxHeaderFATEntries entradas para setores de fat
  // preenchemos aqui os offsets das fats utilizadas e marcamos com nulo o restante
  for I := 0 to MaxHeaderFATEntries - 1 do 
    if I < FATSectorCount then
      HeaderSectorRec._sectFat[I] := FATFirstSector + I
    else
      HeaderSectorRec._sectFat[I] := FREESECT;
  // voltamos o cursor da stream para o ponto de gravacao do header e gravamos 
  AStream.Position := HeaderOffset;
  AStream.Write(HeaderSectorRec, SizeOf(HeaderSectorRec));

  // voltamos o cursor da stream para o ponto de gravacao das fats
  // o itemindex indica a entrada da fat corrente que esta sendo preenchida
  // qdo itemindex passar de 128 entradas, grava a fat e reinicia
  AStream.Position := FATOffset;
  FATItemCount := 0;
  // grava primeiro as fats contendo ponteiros para dados
  // marca o final dos setores de dados com ENDOFCHAIN
  DataSectorNo := DataFirstSector;
  while DataSectorNo < DataLastSector do
  begin
    Inc(DataSectorNo);
    FATSectorRec[NewFATItemIndex] := DataSectorNo;
  end;
  FATSectorRec[NewFATItemIndex] := ENDOFCHAIN;
  // reserva os setores das próprias fats
  FATSectorNo := FATFirstSector;
  while FATSectorNo <= FATLastSector do
  begin
    FATSectorRec[NewFATItemIndex] := FATSECT;
    Inc(FATSectorNo);
  end;
  // grava as fats contendo ponteiros para diretorios
  // marca o final dos diretorios
  DirectorySectorNo := DirectoryFirstSector;
  while DirectorySectorNo < DirectoryLastSector do
  begin
    Inc(DirectorySectorNo);
    FATSectorRec[NewFATItemIndex] := DirectorySectorNo;
  end;
  FATSectorRec[NewFATItemIndex] := ENDOFCHAIN;
  // completa as 128 entradas da última fat com ponteiros nulos e grava
  if FATItemCount < FATEntriesPerSector then
  begin
    while FATItemCount < FATEntriesPerSector do
    begin
      FATSectorRec[FATItemCount] := FREESECT;
      Inc(FATItemCount);
    end;
    AStream.Write(FATSectorRec, SizeOf(FATSectorRec));
  end; 

  // grava diretorio
  AStream.Position := DirectoryOffset;
  // ROOT
  FillChar(DirectorySectorRec, SizeOf(DirectorySectorRec), 0);
  Move(RootEntry[1], DirectorySectorRec._ab, Length(RootEntry));
  DirectorySectorRec._cb := Length(RootEntry);
  DirectorySectorRec._mse := STGTY_ROOT;
  DirectorySectorRec._bflags := DE_BLACK;
  DirectorySectorRec._sidLeftSib := NOSTREAM;
  DirectorySectorRec._sidRightSib := NOSTREAM;
  DirectorySectorRec._sidChild := 1;
  DirectorySectorRec._clsId := ' '#8#2#0#0#0#0#0'À'#0#0#0#0#0#0'F';
  DirectorySectorRec._sectStart := ENDOFCHAIN;
  AStream.Write(DirectorySectorRec, SizeOf(DirectorySectorRec));
  // STREAM
  FillChar(DirectorySectorRec, SizeOf(DirectorySectorRec), 0);
  Move(Workbook[1], DirectorySectorRec._ab, Length(Workbook));
  DirectorySectorRec._cb := Length(Workbook);
  DirectorySectorRec._mse := STGTY_STREAM;
  DirectorySectorRec._bflags := DE_BLACK;
  DirectorySectorRec._sidLeftSib := NOSTREAM;
  DirectorySectorRec._sidRightSib := NOSTREAM;
  DirectorySectorRec._sidChild := NOSTREAM;
  DirectorySectorRec._sectStart := DataFirstSector;
  DirectorySectorRec._ulSizeLow := DataTotalSize;
  AStream.Write(DirectorySectorRec, SizeOf(DirectorySectorRec));
  // 2x NULL
  FillChar(DirectorySectorRec, SizeOf(DirectorySectorRec), 0);
  DirectorySectorRec._sidLeftSib := NOSTREAM;
  DirectorySectorRec._sidRightSib := NOSTREAM;
  DirectorySectorRec._sidChild := NOSTREAM;
  AStream.Write(DirectorySectorRec, SizeOf(DirectorySectorRec));
  AStream.Write(DirectorySectorRec, SizeOf(DirectorySectorRec));
end;

function RangeSortCallback(Item1, Item2: Pointer): Integer;
begin
  Result := TRLXLSRange(Item1).CellArea.X0 - TRLXLSRange(Item2).CellArea.X0;
end;

procedure TRLXLSWorkbook.WriteRangeToStream(AStream: TStream; ARange: TRLXLSRange;
  ACurrentRow: Integer; var AIndexInCellsOffsArray: Integer;
  var ACellsOffs: TRLXLSBiff8DBCELLCellsOffsArray);
var
  blank: TRLXLSBiff8BLANK;
  I, left: Integer;
  number: TRLXLSBiff8NUMBER;
  mulblank: PRLXLSBiff8MULBLANK;
  labelsst: TRLXLSBiff8LABELSST;
  procedure AddToCellsOffsArray;
  begin
    if AIndexInCellsOffsArray = 0 then
      ACellsOffs[AIndexInCellsOffsArray] := AStream.Position
    else
      ACellsOffs[AIndexInCellsOffsArray] := AStream.Position - ACellsOffs[AIndexInCellsOffsArray - 1];
    Inc(AIndexInCellsOffsArray);
  end;
begin
  left := ARange.CellArea.X0;
  if ACurrentRow = ARange.CellArea.Y0 then
  begin
    AddToCellsOffsArray;
    case ARange.DataType of
      ctNumber:
      begin
        number.rw := ACurrentRow;
        number.col := ARange.CellArea.X0;
        number.ixfe := PXLSRangeRec(ARange.ExportData).iXF;
        number.num := StrToFloat(ARange.Value);
        WriteBIFF(AStream, B8_NUMBER, @number, SizeOf(TRLXLSBiff8NUMBER));
      end;
      ctDate: 
      begin
        number.rw := ACurrentRow;
        number.col := ARange.CellArea.X0;
        number.ixfe := PXLSRangeRec(ARange.ExportData).iXF;
        number.num := StrToDate(ARange.Value);
        WriteBIFF(AStream, B8_NUMBER, @number, SizeOf(TRLXLSBiff8NUMBER));
      end;
      ctTime: 
      begin
        labelsst.rw := ACurrentRow;
        labelsst.col := ARange.CellArea.X0;
        labelsst.ixfe := PXLSRangeRec(ARange.ExportData).iXF;
        labelsst.isst := PXLSRangeRec(ARange.ExportData).iSST;
        WriteBIFF(AStream, B8_LABELSST, @labelsst, SizeOf(TRLXLSBiff8LABELSST));
      end;
      ctString: 
      begin
        labelsst.rw := ACurrentRow;
        labelsst.col := ARange.CellArea.X0;
        labelsst.ixfe := PXLSRangeRec(ARange.ExportData).iXF;
        labelsst.isst := PXLSRangeRec(ARange.ExportData).iSST;
        WriteBIFF(AStream, B8_LABELSST, @labelsst, SizeOf(TRLXLSBiff8LABELSST));
      end;
    end;
    Inc(left);
  end;
  if left < ARange.CellArea.X1 then
  begin
    AddToCellsOffsArray;
    mulblank := AllocMem(SizeOf(TRLXLSBiff8MULBLANK) + (ARange.CellArea.X1 - left + 1) * 2 + 2);
    try
      mulblank.rw := ACurrentRow;
      mulblank.colFirst := left;
      for I := 0 to ARange.CellArea.X1 - left do
        PWordArray(PAnsiChar(mulblank) + SizeOf(TRLXLSBiff8MULBLANK))^[I] := PXLSRangeRec(ARange.ExportData).iXF;
      PWord(PAnsiChar(mulblank) + SizeOf(TRLXLSBiff8MULBLANK) + (ARange.CellArea.X1 - left + 1) * 2)^ := ARange.CellArea.X1;
      WriteBIFF(AStream, B8_MULBLANK, mulblank, SizeOf(TRLXLSBiff8MULBLANK) + (ARange.CellArea.X1 - left + 1) * 2 + 2);
    finally
      FreeMem(mulblank);
    end;
  end
  else if left = ARange.CellArea.X1 then
  begin
    AddToCellsOffsArray;
    blank.rw := ACurrentRow;
    blank.col := left;
    blank.ixfe := PXLSRangeRec(ARange.ExportData).iXF;
    WriteBIFF(AStream, B8_BLANK, @blank, SizeOf(blank));
  end;
end;

procedure TRLXLSWorkbook.WriteSheetDataToStream(AStream: TStream; var ASheetRec: TRLXLSSheetRec;
  ASheet: TRLXLSWorksheet);
type
  TCardinalArray = array[0..0] of Cardinal;
  PCardinalArray = ^TCardinalArray;
var
  bof: TRLXLSBiff8BOF;
  calcmode: TRLXLSBiff8CALCMODE;
  calccount: TRLXLSBiff8CALCCOUNT;
  refmode: TRLXLSBiff8REFMODE;
  iteration: TRLXLSBiff8ITERATION;
  saverecalc: TRLXLSBiff8SAVERECALC;
  printheaders: TRLXLSBiff8PRINTHEADERS;
  printgridlines: TRLXLSBiff8PRINTGRIDLINES;
  gridset: TRLXLSBiff8GRIDSET;
  guts: TRLXLSBiff8GUTS;
  defaultrowheight: TRLXLSBiff8DEFAULTROWHEIGHT;
  wsbool: TRLXLSBiff8WSBOOL;
  hcenter: TRLXLSBiff8HCENTER;
  vcenter: TRLXLSBiff8VCENTER;
  defcolwidth: TRLXLSBiff8DEFCOLWIDTH;
  dimensions: TRLXLSBiff8Dimensions;
  window2: TRLXLSBiff8WINDOW2;
  selection: PRLXLSBiff8SELECTION;
  header: PRLXLSBiff8HEADER;
  footer: PRLXLSBiff8FOOTER;
  INDEXOffs: Integer;
  BlocksInSheet: Integer;
  IndexInDBCELLsOffs: Integer;
  dbcell: TRLXLSBiff8DBCELLfull;
  IndexInCellsOffsArray: Integer;
  ms: TMemoryStream;
  FirstRowOffs: Integer;
  SecondRowOffs: Integer;
  merge: PRLXLSBiff8MERGE;
  colinfo: TRLXLSBiff8ColumnInfo;
  leftmargin: TRLXLSBiff8LEFTMARGIN;
  rightmargin: TRLXLSBiff8RIGHTMARGIN;
  topmargin: TRLXLSBiff8TOPMARGIN;
  bottommargin: TRLXLSBiff8BOTTOMMARGIN;
  setup: TRLXLSBiff8SETUP;
  index: PRLXLSBiff8INDEX;
  room: Integer;
  L: TList;
  range: TRLXLSRange;
  rw: TRLXLSRow;
  row: TRLXLSBiff8Row;
  bc, I, J: Integer;
  aux: AnsiString;
begin
  ASheetRec.StreamBOFOffset := AStream.Position;

  FillChar(bof, SizeOf(bof), 0);
  bof.vers := B8_BOF_vers;
  bof.dt := B8_BOF_dt_Worksheet;
  bof.rupBuild := B8_BOF_rupBuild_Excel97;
  bof.rupYear := B8_BOF_rupYear_Excel97;
  bof.bfh := 0;
  bof.sfo := 0;
  WriteBIFF(AStream, B8_BOF, @bof, SizeOf(bof));
  if (ASheet.CellArea.Y1 <> -1) and (ASheet.CellArea.Y0 <> -1) then
  begin
    BlocksInSheet := (ASheet.CellArea.Y1 - ASheet.CellArea.Y0 + 1) div XLSMaxRowsInBlock;
    if (ASheet.CellArea.Y1 = ASheet.CellArea.Y0) or (((ASheet.CellArea.Y1 - ASheet.CellArea.Y0 + 1) mod XLSMaxRowsInBlock) <> 0) then
      Inc(BlocksInSheet);
  end
  else
    BlocksInSheet := 0;
  index := AllocMem(SizeOf(TRLXLSBiff8INDEX) + BlocksInSheet * 4);
  try
    if (ASheet.CellArea.Y1 <> -1) and (ASheet.CellArea.Y0 <> -1) then
    begin
      index.rwMic := ASheet.CellArea.Y0;
      index.rwMac := ASheet.CellArea.Y1 + 1;
    end;
    INDEXOffs := AStream.Position;
    IndexInDBCELLsOffs := 0;
    WriteBIFF(AStream, B8_INDEX, index, SizeOf(TRLXLSBiff8INDEX) + BlocksInSheet * 4);
    calcmode.FAutoRecalc := 1;
    WriteBIFF(AStream, B8_CALCMODE, @calcmode, SizeOf(calcmode));
    calccount.cIter := $0064;
    WriteBIFF(AStream, B8_CALCCOUNT, @calccount, SizeOf(calccount));
    refmode.FRefA1 := $0001;
    WriteBIFF(AStream, B8_REFMODE, @refmode, SizeOf(refmode));
    iteration.FIter := $0000;
    WriteBIFF(AStream, B8_ITERATION, @iteration, SizeOf(iteration));
    aux := #$10#$00#$08#$00#$fc#$a9#$f1#$d2#$4d#$62#$50#$3f;
    AStream.Write(aux[1], Length(aux));
    saverecalc.FSaveRecalc := $0001;
    WriteBIFF(AStream, B8_SAVERECALC, @saverecalc, SizeOf(saverecalc));
    if ASheet.Workbook.PageSetup.PrintHeaders then
      printheaders.FPrintRwCol := 1
    else
      printheaders.FPrintRwCol := 0;
    WriteBIFF(AStream, B8_PRINTHEADERS, @printheaders, SizeOf(printheaders));
    if ASheet.Workbook.PageSetup.PrintGridLines then
      printgridlines.FPrintGrid := 1
    else
      printgridlines.FPrintGrid := 0;
    WriteBIFF(AStream, B8_PRINTGRIDLINES, @printgridlines, SizeOf(printgridlines));
    gridset.FGridSet := $0001;
    WriteBIFF(AStream, B8_GRIDSET, @gridset, SizeOf(gridset));
    FillChar(guts, SizeOf(guts), 0);
    WriteBIFF(AStream, B8_GUTS, @guts, SizeOf(guts));
    defaultrowheight.grbit := $0000;
    defaultrowheight.miyRw := DefaultCellHeight;
    WriteBIFF(AStream, B8_DEFAULTROWHEIGHT, @defaultrowheight, SizeOf(defaultrowheight));
    wsbool.grbit := $04C1;
    WriteBIFF(AStream, B8_WSBOOL, @wsbool, SizeOf(wsbool));
    aux := '';
    if ASheet.Workbook.PageSetup.LeftHeader <> '' then
      aux := aux + '&L' + ASheet.Workbook.PageSetup.LeftHeader;
    if ASheet.Workbook.PageSetup.CenterHeader <> '' then
      aux := aux + '&C' + ASheet.Workbook.PageSetup.CenterHeader;
    if ASheet.Workbook.PageSetup.RightHeader <> '' then
      aux := aux + '&R' + ASheet.Workbook.PageSetup.RightHeader;
    if aux <> '' then
    begin
      room := WideStringSize(aux);
      GetMem(header, SizeOf(TRLXLSBiff8HEADER) + room);
      try
        header.cch := Length(aux);
        header.cchgrbit := 1;
        StringToWideChar(aux, PWideChar(PtrInt(header) + SizeOf(TRLXLSBiff8HEADER)), room);
        WriteBIFF(AStream, B8_HEADER, header, SizeOf(TRLXLSBiff8HEADER) + room);
      finally
        FreeMem(header);
      end;
    end;

    aux := '';
    if ASheet.Workbook.PageSetup.LeftFooter <> '' then
      aux := aux + '&L' + ASheet.Workbook.PageSetup.LeftFooter;
    if ASheet.Workbook.PageSetup.CenterFooter <> '' then
      aux := aux + '&C' + ASheet.Workbook.PageSetup.CenterFooter;
    if ASheet.Workbook.PageSetup.RightFooter <> '' then
      aux := aux + '&R' + ASheet.Workbook.PageSetup.RightFooter;
    if aux <> '' then
    begin
      room := WideStringSize(aux);
      GetMem(footer, SizeOf(TRLXLSBiff8FOOTER) + room);
      try
        footer.cch := Length(aux);
        footer.cchgrbit := 1;
        StringToWideChar(aux, PWideChar(PtrInt(footer) + SizeOf(TRLXLSBiff8HEADER)), room);
        WriteBIFF(AStream, B8_FOOTER, footer, SizeOf(TRLXLSBiff8FOOTER) + room);
      finally
        FreeMem(footer);
      end;
    end;

    if ASheet.Workbook.PageSetup.CenterHorizontally then
      hcenter.FHCenter := 1
    else
      hcenter.FHCenter := 0;
    WriteBIFF(AStream, B8_HCENTER, @hcenter, SizeOf(hcenter));

    if ASheet.Workbook.PageSetup.CenterVertically then
      vcenter.FVCenter := 1
    else
      vcenter.FVCenter := 0;
    WriteBIFF(AStream, B8_VCENTER, @vcenter, SizeOf(vcenter));

    leftmargin.num := ASheet.Workbook.PageSetup.LeftMargin / 2.54;
    WriteBIFF(AStream, B8_LEFTMARGIN, @leftmargin, SizeOf(TRLXLSBiff8LEFTMARGIN));
    rightmargin.num := ASheet.Workbook.PageSetup.RightMargin / 2.54;
    WriteBIFF(AStream, B8_RIGHTMARGIN, @rightmargin, SizeOf(TRLXLSBiff8RIGHTMARGIN));
    topmargin.num := ASheet.Workbook.PageSetup.TopMargin / 2.54;
    WriteBIFF(AStream, B8_TOPMARGIN, @topmargin, SizeOf(TRLXLSBiff8TOPMARGIN));
    bottommargin.num := ASheet.Workbook.PageSetup.BottomMargin / 2.54;
    WriteBIFF(AStream, B8_BOTTOMMARGIN, @bottommargin, SizeOf(TRLXLSBiff8BOTTOMMARGIN));
    FillChar(setup, SizeOf(TRLXLSBiff8SETUP), 0);
    setup.iPaperSize := Word(ASheet.Workbook.PageSetup.PaperSize);
    setup.iPageStart := ASheet.Workbook.PageSetup.FirstPageNumber;
    setup.iFitWidth := Byte(ASheet.Workbook.PageSetup.FitToPagesWide);
    setup.iFitHeight := Byte(ASheet.Workbook.PageSetup.FitToPagesTall);
    setup.numHdr := ASheet.Workbook.PageSetup.HeaderMargin / 2.54;
    setup.numFtr := ASheet.Workbook.PageSetup.FooterMargin / 2.54;
    setup.iCopies := ASheet.Workbook.PageSetup.Copies;
    setup.iScale := ASheet.Workbook.PageSetup.Zoom;
    if ASheet.Workbook.PageSetup.Order = odOverThenDown then
      setup.grbit := setup.grbit or B8_SETUP_fLeftToRight;
    if ASheet.Workbook.PageSetup.Orientation = orPortrait then
      setup.grbit := setup.grbit or B8_SETUP_fLandscape;
    if ASheet.Workbook.PageSetup.BlackAndWhite then
      setup.grbit := setup.grbit or B8_SETUP_fNoColor;
    if ASheet.Workbook.PageSetup.Draft then
      setup.grbit := setup.grbit or B8_SETUP_fDraft;
    if ASheet.Workbook.PageSetup.PrintNotes then
      setup.grbit := setup.grbit or B8_SETUP_fNotes;
    if ASheet.Workbook.PageSetup.FirstPageNumber <> 1 then
      setup.grbit := setup.grbit or B8_SETUP_fUsePage;
    WriteBIFF(AStream, B8_SETUP, @setup, SizeOf(TRLXLSBiff8SETUP));
    defcolwidth.cchdefColWidth := DefaultCellLength;
    WriteBIFF(AStream, B8_DEFCOLWIDTH, @defcolwidth, SizeOf(defcolwidth));
    for I := 0 to ASheet.ColCount - 1 do
      with ASheet.FindCol(I, True) do
      begin
        FillChar(colinfo, SizeOf(colinfo), 0);
        colinfo.colFirst := Index;
        colinfo.colLast := Index;
        colinfo.coldx := Width;
        WriteBIFF(AStream, B8_COLINFO, @colinfo, SizeOf(colinfo));
      end;
    FillChar(dimensions, SizeOf(dimensions), 0);
    if (ASheet.CellArea.X0 <> -1) and (ASheet.CellArea.X1 <> -1) and (ASheet.CellArea.Y0 <> -1) and (ASheet.CellArea.Y1 <> -1) then
    begin
      dimensions.rwMic := ASheet.CellArea.Y0;
      dimensions.rwMac := ASheet.CellArea.Y1 + 1;
      dimensions.colMic := ASheet.CellArea.X0;
      dimensions.colMac := ASheet.CellArea.X1 + 1;
    end;
    WriteBIFF(AStream, B8_DIMENSIONS, @dimensions, SizeOf(dimensions));
    if (ASheet.CellArea.Y0 <> -1) and (ASheet.CellArea.Y1 <> -1) then
    begin
      L := TList.Create;
      ms := TMemoryStream.Create;
      try
        bc := 0;
        FirstRowOffs := 0;
        SecondRowOffs := 0;
        for I := ASheet.CellArea.Y0 to ASheet.CellArea.Y1 do
        begin
          L.Clear;
          for J := 0 to ASheet.RangeCount - 1 do
          begin
            range := ASheet.Ranges[J];
            if (range.CellArea.Y0 <= I) and (I <= range.CellArea.Y1) then
              L.Add(range);
          end;
          L.Sort(RangeSortCallback);
          if bc = 0 then
            FirstRowOffs := AStream.Position;
          FillChar(row, SizeOf(row), 0); 
          row.rw := I;
          if L.Count > 0 then
          begin
            row.colMic := TRLXLSRange(L[0]).CellArea.X0;
            row.colMac := TRLXLSRange(L[L.Count - 1]).CellArea.X1 + 1;
          end
          else
          begin
            row.colMic := 0;
            row.colMac := 0;
          end;
          rw := ASheet.FindRow(I, False);
          if rw = nil then
          begin
            row.miyRw := DefaultCellHeight;
            row.grbit := 0;
          end
          else
          begin
            row.miyRw := rw.Height * 20;
            row.grbit := B8_ROW_grbit_fUnsynced;
          end;
          WriteBIFF(AStream, B8_ROW, @row, SizeOf(row));
          if bc = 0 then
            SecondRowOffs := AStream.Position;
          IndexInCellsOffsArray := 0;
          for J := 0 to L.Count - 1 do
            WriteRangeToStream(ms, TRLXLSRange(L[J]), I, IndexInCellsOffsArray, dbcell.CellsOffs);
          Inc(bc);
          if (bc = XLSMaxRowsInBlock) or (I = ASheet.CellArea.Y1) then
          begin
            dbcell.CellsOffs[0] := AStream.Position - SecondRowOffs;
            ms.SaveToStream(AStream);
            PCardinalArray(PAnsiChar(index) + SizeOf(TRLXLSBiff8INDEX))^[IndexInDBCELLsOffs] := AStream.Position - FBOFOffs;
            Inc(IndexInDBCELLsOffs);
            dbcell.dbRtrw := AStream.Position - FirstRowOffs;
            WriteBIFF(AStream, B8_DBCELL, @dbcell,
              SizeOf(TRLXLSBiff8DBCELL) + IndexInCellsOffsArray * 2);
            ms.Clear;
            bc := 0;
          end;
        end;
      finally
        L.Free;
        ms.Free;
      end;
      AStream.Position := INDEXOffs;
      WriteBIFF(AStream, B8_INDEX, index, SizeOf(TRLXLSBiff8INDEX) + BlocksInSheet * 4);
      AStream.Seek(0, soFromEnd);
    end;
  finally
    FreeMem(index);
  end;

  FillChar(window2, SizeOf(window2), 0);
  window2.grbit := B8_WINDOW2_grbit_fPaged or
                 B8_WINDOW2_grbit_fDspGuts or
                 B8_WINDOW2_grbit_fDspZeros or
                 B8_WINDOW2_grbit_fDefaultHdr or
                 B8_WINDOW2_grbit_fDspGrid or
                 B8_WINDOW2_grbit_fDspRwCol;
  if ASheet.Index = 0 then
    window2.grbit := window2.grbit + B8_WINDOW2_grbit_fSelected;
  window2.rwTop := 0;
  window2.colLeft := 0;
  window2.icvHdr := $00000040;
  window2.wScaleSLV := 0;
  window2.wScaleNormal := 0;
  WriteBIFF(AStream, B8_WINDOW2, @window2, SizeOf(window2));
  selection := AllocMem(SizeOf(TRLXLSBiff8SELECTION) + 6);
  try
    selection.pnn := 3;
    selection.cref := 1;
    WriteBIFF(AStream, B8_SELECTION, selection, SizeOf(TRLXLSBiff8SELECTION) + 6);
  finally
    FreeMem(selection);
  end;
  if ASheet.RangeCount > 0 then
  begin
    J := 0;
    for I := 0 to ASheet.RangeCount - 1 do
    begin
      range := ASheet.Ranges[I];
      if (range.CellArea.X0 <> range.CellArea.X1) or
         (range.CellArea.Y0 <> range.CellArea.Y1) then
        Inc(J);
    end;
    if J > 0 then
    begin
      merge := AllocMem(SizeOf(TRLXLSBiff8MERGE) + J * 8);
      try
        merge.cnt := J;
        J := 0;
        for I := 0 to ASheet.RangeCount - 1 do
        begin
          range := ASheet.Ranges[I];
          if (range.CellArea.X0 <> range.CellArea.X1) or
             (range.CellArea.Y0 <> range.CellArea.Y1) then
          begin
            with PRLXLSBiff8MERGErec(PAnsiChar(merge) + SizeOf(TRLXLSBiff8MERGE) + J * 8)^ do
            begin
              left := range.CellArea.X0;
              top := range.CellArea.Y0;
              right := range.CellArea.X1;
              bottom := range.CellArea.Y1;
            end;
            Inc(J);
          end;
        end;
        WriteBIFF(AStream, B8_MERGE, merge, SizeOf(TRLXLSBiff8MERGE) + J * 8);
      finally
        FreeMem(merge);
      end;
    end;
  end;
  WriteBIFF(AStream, B8_EOF, nil, 0);
end;

procedure TRLXLSWorkbook.SaveToFile(const AFileName: AnsiString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRLXLSFilter.InternalBeginDoc;
begin
  WorkBook.Clear;
  WorkBook.FindValueCells := foFindValueCells in Options;
  FFirstPage := True;
  FOffsetRow := 0;
end;

function BestBounds(const Rect: TRect; Alignment: TRLMetaTextAlignment; Layout: TRLMetaTextLayout;
  AutoSize: Boolean; const Cols, Rows: TRLXLSTabColumns): TRLXLSCellArea;
var
  FixedLeft, FixedRight: Boolean;
  I: Integer;
begin
  FixedLeft := (Alignment in [MetaTextAlignmentCenter, MetaTextAlignmentJustify]) or (Alignment <> MetaTextAlignmentRight) or not AutoSize;
  FixedRight := (Alignment in [MetaTextAlignmentCenter, MetaTextAlignmentJustify]) or (Alignment = MetaTextAlignmentRight) or not AutoSize;

  if FixedLeft then
  begin
    Result.X0 := -1;
    for I := 0 to Length(Cols) - 1 do
      if (Result.X0 = -1) or (Abs(Cols[I].StartPos - Rect.Left) < Abs(Cols[Result.X0].StartPos - Rect.Left)) then
        Result.X0 := I;
  end;
  if FixedRight then
  begin
    Result.X1 := -1;
    for I := 0 to Length(Cols) - 1 do
      if (Result.X1 = -1) or (Abs(Cols[I].EndPos - Rect.Right) < Abs(Cols[Result.X1].EndPos - Rect.Right)) then
        Result.X1 := I;
  end;
  if FixedRight and not FixedLeft then
    Result.X0 := Result.X1;
  if FixedLeft and not FixedRight then
    Result.X1 := Result.X0;

  Result.Y0 := -1;
  for I := 0 to Length(Rows) - 1 do
    if (Result.Y0 = -1) or (Abs(Rows[I].StartPos - Rect.Top) < Abs(Rows[Result.Y0].StartPos - Rect.Top)) then
      Result.Y0 := I;
  Result.Y1 := -1;
  for I := 0 to Length(Rows) - 1 do
    if Rows[I].EndPos >= Rows[Result.Y0].EndPos then
      if (Result.Y1 = -1) or (Abs(Rows[I].EndPos - Rect.Bottom) < Abs(Rows[Result.Y1].EndPos - Rect.Bottom)) then
        Result.Y1 := I;
end;

procedure TRLXLSWorksheet.SolveConflicts(var RangeNew: TRLXLSRange);
  procedure SetHalf(var A1, A2: TRLXLSCellArea);
  var
    XMin, XMax, XMid: Integer;
  begin
    XMin := Min(A1.X0, A2.X0);
    XMax := Max(A1.X1, A2.X1);
    XMid := (XMin + XMax) div 2;
    A1.X0 := XMin;
    A1.X1 := Max(XMid - 1, A1.X0);
    A2.X0 := A1.X1 + 1;
    A2.X1 := XMax;
    if A2.X0 > A2.X1 then
      raise Exception.Create('SetHalf: A2.X0 > A2.X1');
  end;
var
  RangeOld: TRLXLSRange;
  AreaOld, AreaNew: TRLXLSCellArea;
  AlignOld, AlignNew: TRLXLSHorizontalAlignmentType;
  I: Integer;
begin
  AreaNew := RangeNew.CellArea;
  AlignNew := RangeNew.HorizontalAlignment;
  for I := RangeCount - 1 downto 0 do
  begin
    RangeOld := Ranges[I];
    AreaOld := RangeOld.CellArea;
    AlignOld := RangeOld.HorizontalAlignment;

    if CellAreaIntercepts(AreaNew, AreaOld) and
       (RangeOld.Value <> '') and (RangeNew.Value <> '') then
      if (AreaNew.X0 = AreaNew.X1) and (AreaOld.X0 = AreaOld.X1) then
      begin
        // concateno os valores se a concorrencia for ferrenea
        RangeOld.Value := RangeOld.Value + ' ' + RangeNew.Value;
        if RangeNew.HorizontalAlignment = haRight then
          RangeOld.HorizontalAlignment := haRight;
        RangeNew := nil;
        Exit;   
      end
      else if AlignOld = haLeft then
        if AlignNew = haLeft then
          // se os dois estao a esq, corto o mais a esq
          if AreaOld.X0 < AreaNew.X0 then
            AreaOld.X1 := AreaNew.X0 - 1
          else if AreaNew.X0 < AreaOld.X0 then
            AreaNew.X1 := AreaOld.X0 - 1
          else
            SetHalf(AreaOld, AreaNew)
        else
          SetHalf(AreaOld, AreaNew)
      else if AlignOld = haRight then
        // se os dois estao a dir, corto o mais a dir
        if AlignNew = haRight then
          if AreaOld.X1 > AreaNew.X1 then
            AreaNew.X1 := AreaOld.X0 - 1
          else if AreaNew.X1 > AreaOld.X1 then
            AreaOld.X1 := AreaNew.X0 - 1
          else
            SetHalf(AreaOld, AreaNew)
        else
          SetHalf(AreaOld, AreaNew)
      else
        SetHalf(AreaOld, AreaNew);

      RangeNew.CellArea := AreaNew;
      RangeOld.CellArea := AreaOld;
  end;
end;

function CompareTextObjects(Item1, Item2: Pointer): Integer;
var
  Text1, Text2: TRLTextObject;
begin
  Text1 := Item1;
  Text2 := Item2;
  Result := CompareValue(Text1.BoundsRect.Top, Text2.BoundsRect.Top);
  if Result = 0 then
    Result := CompareValue(Text1.BoundsRect.Left, Text2.BoundsRect.Left);
end;

function IsAutoSize(C: TRLTextObject): Boolean;
begin
  Result := (C.TextFlags and MetaTextFlagAutoSize) = MetaTextFlagAutoSize; 
end;

procedure TRLXLSFilter.CriarTabsMudancaAlinhamento;
var
  TabAnterior,TabAtual: TRLXLSTab;
  i: Integer;
begin
  if FHorzTabs.Count>1 then
  begin
    i:= FHorzTabs.Count-1;
    TabAnterior:=nil;
    while i>=0 do
    begin
      TabAtual := FHorzTabs.Tabs[i];
      if TabAnterior<>nil then
        if (TabAnterior.Alignment=MetaTextAlignmentRight) and (TabAtual.Alignment=MetaTextAlignmentLeft) then
          FHorzTabs.InsertTab(TabAtual.ComplementPosition, 0, 0,'');
      TabAnterior := TabAtual;
      Dec(i);
    end;
  end;
end;

procedure TRLXLSFilter.InternalDrawPage(APage: TRLGraphicSurface);
const
  MulFactX = 1.2;
  MulFactY = 1;
  MinTwips = 10;
var
  PageSheet: TRLXLSWorksheet;
  NewestRange: TRLXLSRange;
  ObjectRect: TRect;
  ObjectArea: TRLXLSCellArea;
  TextObject: TRLTextObject;
  MinHorzTwips: Integer;
  MinVertTwips: Integer;
  IsNewSheet: Boolean;
  HorzAlignment: TRLXLSHorizontalAlignmentType;
  VertAlignment: TRLXLSVerticalAlignmentType;
  CellText: AnsiString;
  LastRight: Integer;
  LastBottom: Integer;
  Cols: TRLXLSTabColumns;
  Rows: TRLXLSTabColumns;
  SelectedTexts: TObjectList;
  TopCut: Integer;
  I: Integer;
begin
  // largura ou altura minima para uma celula
  // celulas menores que isso serao absorvidas por uma vizinha
  MinHorzTwips := TwipsX(MinTwips);
///  MinVertTwips := TwipsY(MinTwips);

  MinVertTwips := TwipsY(0);

  // calculo as larguras das colunas na primeira pagina ou a cada nova sheet
  IsNewSheet := FFirstPage or not (foOneSheetOnly in Options);
  FFirstPage := False;

  if IsNewSheet then
  begin
    PageSheet := WorkBook.NewSheet;
    FHorzTabs.Clear;
    FVertTabs.Clear;
  end
  else
    PageSheet := WorkBook.Sheets[0];

  SelectedTexts := TObjectList.Create(False);
  try

    TopCut := -1;
    for I := 0 to APage.ObjectCount - 1 do
      if APage.Objects[I] is TRLTextObject then
      begin
        TextObject := APage.Objects[I] as TRLTextObject;
{Fred/Tiago        // só me interessam os textos nao vazios, pois nao vou criar celulas vazias
        if TextObject.DisplayText = '' then
          ;///Continue;}

        if (TopCut = -1) or (TextObject.BoundsRect.Top < TopCut) then
          TopCut := TextObject.BoundsRect.Top;

        SelectedTexts.Add(TextObject);
      end;

    Dec(FOffsetRow, TopCut);

    SelectedTexts.Sort(CompareTextObjects);

    LastRight := 0;
    LastBottom := 0;
    for I := 0 to SelectedTexts.Count - 1 do
    begin
      TextObject := SelectedTexts[I] as TRLTextObject;
      if (TextObject.Text='01') or (TextObject.Text='DOM') then
        TextObject.Text:=TextObject.Text; ///

      // amplio o objeto para reduzir a possibilidade de intersecao
      ObjectRect := FromMetaRect(TextObject.BoundsRect);
      MulRectX(ObjectRect, MulFactX, MulFactY);

      // aqui eu cadastro as divisorias entre celulas, tanto na horizontal quanto na vertical
      if IsNewSheet then
        if TextObject.Alignment in [MetaTextAlignmentCenter, MetaTextAlignmentJustify] then
        begin
          FHorzTabs.InsertTab(TwipsX(ObjectRect.Left), TwipsX(ObjectRect.Right), TextObject.Alignment, TextObject.Text);
          FHorzTabs.InsertTab(TwipsX(ObjectRect.Right), TwipsX(ObjectRect.Left), TextObject.Alignment, TextObject.Text);
        end
        else if TextObject.Alignment = MetaTextAlignmentRight then
        begin
          if not IsAutoSize(TextObject) then
            FHorzTabs.InsertTab(TwipsX(ObjectRect.Left), TwipsX(ObjectRect.Right), TextObject.Alignment, TextObject.Text);
          FHorzTabs.InsertTab(TwipsX(ObjectRect.Right), TwipsX(ObjectRect.Left), TextObject.Alignment, TextObject.Text);
        end
        else
        begin
          FHorzTabs.InsertTab(TwipsX(ObjectRect.Left), TwipsX(ObjectRect.Right), TextObject.Alignment, TextObject.Text);
          if not IsAutoSize(TextObject) then
            FHorzTabs.InsertTab(TwipsX(ObjectRect.Right), TwipsX(ObjectRect.Left), TextObject.Alignment, TextObject.Text);
        end;
      FVertTabs.InsertTab(TwipsY(ObjectRect.Top + FOffsetRow), TwipsY(ObjectRect.Bottom + FOffsetRow), TextObject.Alignment, TextObject.Text);

      // o ultimo right vai gerar a ultima divisoria
      LastRight := Max(LastRight, TwipsX(ObjectRect.Right));
      LastBottom := Max(LastBottom, TwipsY(ObjectRect.Bottom));
    end;

    if IsNewSheet then
      CriarTabsMudancaAlinhamento;

    if IsNewSheet and (LastRight <> 0) then
      FHorzTabs.InsertTab(LastRight, 0, 0,'');
    if LastBottom <> 0 then
      FVertTabs.InsertTab(TwipsY(LastBottom + FOffsetRow), 0, 0,'');

    // calcula larguras das colunas baseado na distancia entre as divisorias,
    // tambem retira as colunas/linhas muito pequenas
    Cols := FHorzTabs.GetColumns(MinHorzTwips);
    Rows := FVertTabs.GetColumns(MinVertTwips);

    // seta largura das celulas da planilha baseado nas tabulacoes ja calculadas
    if IsNewSheet then
      for I := 0 to Length(Cols) - 1 do
        PageSheet.FindCol(I, True).Width := Cols[I].Width;
    for I := 0 to Length(Rows) - 1 do
      PageSheet.FindRow(I, True).Height := Rows[I].Width;

    // distribui textos e faz colspan

    for I := 0 to SelectedTexts.Count - 1 do
    begin
      TextObject := SelectedTexts[I] as TRLTextObject;
      CellText := Trim(TextObject.DisplayText);

      // se eu mandar AnsiString vazia para o excel, ele repete o valor de uma celula
      // melhor eu colocar um espaco em branco para garantir
      if CellText = '' then
        CellText := ' ';

      ObjectRect := FromMetaRect(TextObject.BoundsRect);
      MulRectX(ObjectRect, MulFactX, MulFactY);
      ObjectRect.Left := TwipsX(ObjectRect.Left);
      ObjectRect.Top := TwipsY(ObjectRect.Top + FOffsetRow);
      ObjectRect.Right := TwipsX(ObjectRect.Right);
      ObjectRect.Bottom := TwipsY(ObjectRect.Bottom + FOffsetRow);

      case TextObject.Alignment of
        MetaTextAlignmentLeft: HorzAlignment := haLeft;
        MetaTextAlignmentRight: HorzAlignment := haRight;
        MetaTextAlignmentCenter: HorzAlignment := haCenter;
        MetaTextAlignmentJustify: HorzAlignment := haJustify;
      else
        HorzAlignment := haLeft;
      end;

      case TextObject.Layout of
        MetaTextLayoutTop:
        begin
          VertAlignment := vaTop;
          ObjectRect.Bottom := ObjectRect.Top;
        end;
        MetaTextLayoutBottom: VertAlignment := vaBottom;
        MetaTextLayoutCenter: VertAlignment := vaCenter;
        MetaTextLayoutJustify: VertAlignment := vaJustify;
      else
        VertAlignment := vaTop;
      end;
 
      // procura faixa de células
      ObjectArea := BestBounds(ObjectRect, TextObject.Alignment, TextObject.Layout,
        IsAutoSize(TextObject), Cols, Rows);
      NewestRange := TRLXLSRange.Create;
      NewestRange.FWorksheet:=PageSheet;
      NewestRange.FCellArea := ObjectArea;
      NewestRange.HorizontalAlignment := HorzAlignment;
      NewestRange.VerticalAlignment := VertAlignment;
      NewestRange.Value := CellText;
      FromMetaFont(TextObject.Font, NewestRange.Font);
      PageSheet.SolveConflicts(NewestRange);

      if NewestRange <> nil then
      begin
        PageSheet.AddRange(NewestRange);
      end;
    end;

    Inc(FOffsetRow, LastBottom);

  finally
    SelectedTexts.Free;
  end;
end;

procedure TRLXLSFilter.InternalNewPage;
begin
end;

procedure TRLXLSFilter.InternalEndDoc;
begin
  Workbook.SaveToFile(FileName);
end;

function TRLXLSFilter.GetPageSetup: TRLXLSPageSetup;
begin
  Result := FWorkbook.PageSetup;
end;

procedure TRLXLSFilter.SetPageSetup(const Value: TRLXLSPageSetup);
begin
  PageSetup.Assign(Value);
end;

end.
