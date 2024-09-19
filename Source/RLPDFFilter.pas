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

{@unit RLPDFFilter - Implementação do filtro para criação de arquivos PDF. }
unit RLPDFFilter;

{$Q-}
{$R-}
interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  SysUtils, Classes, Math,
  {$IfDef FPC}
   LCLIntf, LCLType, LConvEncoding,
  {$Else}
   {$IfDef DELPHIXE8_UP} Vcl.Imaging.jpeg {$Else} Jpeg {$EndIf},
  {$EndIf}
  {$IfDef CLX}
   QTypes, QGraphics, RLMetaCLX,
  {$Else}
   Types, Graphics, RLMetaVCL,
  {$EndIf}
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFilters;

const
  PDF_FILEVERSION = '1.2';
  PDF_MAXPSTYPE1 = 14; // max. standard postscript font type 1
  PDF_POINTSPERINCH = 72; // point size reference : 12p = 72 = 7.2 per character
  PDF_MAXCOLUMNS = 8;
  PDF_MAXROWS = 4;
  PDF_MAXCOLORSTACK = 64;
  PDF_EOL = #13#10;

const
  PS1FONTNAMES: array[1..PDF_MAXPSTYPE1] of AnsiString = (
    'Courier',
    'Courier-Bold',
    'Courier-Oblique',
    'Courier-BoldOblique',
    'Helvetica',
    'Helvetica-Bold',
    'Helvetica-Oblique',
    'Helvetica-BoldOblique',
    'Symbol',
    'Times-Roman',
    'Times-Bold',
    'Times-Italic',
    'Times-BoldItalic',
    'ZapfDingbats');

const
  voRegular = 0;
  voHideToolBar = 1;
  voHideMenuBar = 2;
  voHideWindowUI = 4;
  voFitWindow = 8;
  voCenterWindow = 16;

type
  TRLPDFFilterPageModeType = (pmRegular, pmOutlines, pmThumbs, pmFullScreen);
  TRLPDFFilterPageLayoutType = (plRegular, plSinglePage, plOneColumn,
    plTwoColumnLeft, plTwoColumnRight);
  TRLPDFFilterFontEncodingType = (feNoEncoding, feMacRomanEncoding, feWinAnsiEncoding);
  TRLPDFFilterFontTypeType = (ftPsType1, ftTrueType);

  TRLPDFFilterPaperSizeType = packed record
    Width, Height: Word;
  end;

  TRLPDFFilterDashPatternType = (dpRegular, dpSolid, dp33, dp1222,
    dp2121, dp23535, dp13232);

  TRLPDFFilterColumnBorderType = (cbRegular, cbSplitter, cbBorder);

  TRLPDFFilterMarginType = packed record
    Top, Bottom, Left, Right, Width, Height: Word;
  end;

  TRLPDFFilterLocationType = packed record
    X, Y: Word;
  end;

  TRLPDFNameString = packed array[0..127] of AnsiChar;

  TRLPDFFilterOutlineType = packed record
    ObjId: Word;
    Page: Word;
    Title: TRLPDFNameString;
  end;

  TRLPDFFilterColumnType = packed record
    WorkArea: TRLPDFFilterMarginType;
    CharCount: Word;
    LineCount: Word;
    CurrentPoint: TRLPDFFilterLocationType;
    CurrentCharPoint: Word;
    CurrentLine: Word;
  end;

  TRLPDFFilterColumnsType = packed array[1..PDF_MAXCOLUMNS, 1..PDF_MAXROWS] of
    TRLPDFFilterColumnType;

  TRLPDFFilterFontType = packed record
    FontName: TRLPDFNameString;
    FontStyle: TRLMetaFontStyles;
    FontObjAt: Integer;
  end;

  TRLPDFFilterImageType = packed record
    ImageObjAt: Integer;
  end;

  TRLPDFFilterPageSetup = class;
  TRLPDFFilterDocumentInfo = class;
  TRLPDFFilterTextControl = class;

  TRLPDFFilterImageFormat = (ifBitmap, ifJPeg, ifOriginal);

  TRLPDFIntegerArray = packed array[0..0] of Integer;
  TRLPDFOutlineArray = packed array[0..0] of TRLPDFFilterOutlineType;
  TRLPDFFontArray = packed array[0..0] of TRLPDFFilterFontType;
  TRLPDFImageArray = packed array[0..0] of TRLPDFFilterImageType;

  { TRLPDFFilter }

  {@class TRLPDFFilter - Filtro para criação de documentos PDF a partir de um relatório.
   O arquivo gerado pode ser lido pelo aplicativo Adobe AcrobatReader e pelo plugin associado
   ao navegador de internet. Inclui imagens e fontes em um só volume e o resultado final é
   bastante fiel ao relatório original, sendo ideal para distribuição via internet.
   @links TRLHTMLFilter, TRLRichFilter, TRLXLSFilter.
   @ancestor TRLCustomSaveFilter.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLPDFFilter = class(TRLCustomSaveFilter)
  private
    // variables
    FOutputStream: TStream;
    FPrintCut: TPoint;
    FPrintSize: TPoint;
    FPagePrintSize: TPoint;
    FWritePos: Integer;
    FCurrentPageNo: Integer;
    FObjects: ^TRLPDFIntegerArray;
    FObjectCapacity: Integer;
    FObjectCount: Integer;
    FOutlines: ^TRLPDFOutlineArray;
    FOutlineCapacity: Integer;
    FOutlineCount: Integer;
    FReserveds: ^TRLPDFIntegerArray;
    FReservedCapacity: Integer;
    FReservedCount: Integer;
    FFonts: ^TRLPDFFontArray;
    FFontCapacity: Integer;
    FFontCount: Integer;
    FImages: ^TRLPDFImageArray;
    FImageCapacity: Integer;
    FImageCount: Integer;
    FPageMode: TRLPDFFilterPageModeType;
    FFullPageMode: TRLPDFFilterPageModeType;
    FPageLayout: TRLPDFFilterPageLayoutType;
    FViewerOptions: Word;
    FForceASCII: Boolean;
    FFontEncoding: TRLPDFFilterFontEncodingType;
    FFontName: string;
    FPages: ^TRLPDFIntegerArray;
    FPageCapacity: Integer;
    FPageCount: Integer;
    FPageOffset: Integer;
    FPageTitle: string;
    FFontColor: TColor;
    FOldFontColor: Integer;
    FXRefOfs: Integer;
    FContentsObjAt: Integer;
    FLengthObjAt: Integer;

    FDocumentInfo: TRLPDFFilterDocumentInfo;
    FPageSetup: TRLPDFFilterPageSetup;
    FTextControl: TRLPDFFilterTextControl;

    FPageTreeObjAt: Integer;
    FOutlinesObjAt: Integer;
    FResourcesObjAt: Integer;
    FCatalogObjAt: Integer;
    FInfoObjAt: Integer;

    FUsingExternalOutputBuffer: Boolean;

    FImageFormat: TRLPDFFilterImageFormat;

    procedure SetDocumentInfo(const Value: TRLPDFFilterDocumentInfo);
    procedure SetPageSetup(const Value: TRLPDFFilterPageSetup);
    procedure SetTextControl(const Value: TRLPDFFilterTextControl);

    procedure Write(const AStr: AnsiString = '');
    procedure Writeln(const AStr: AnsiString = '');

    procedure WriteBOF;
    procedure WriteInfo;
    procedure WriteCatalog;

    procedure WriteFonts;
    procedure WriteResources;
    procedure WriteOutlines;
    procedure WritePageTree;
    procedure WriteXRef;
    procedure WriteTrailer;
    procedure WriteEOF;

    function ReservObj: Integer;
    function NextObjId: Integer;
    function NewObjId: Integer;
    function IsReservedObj(AIndex: Integer): Boolean;
    function GetLastObjectId: Integer;
    function BeginObj(AIndex: Integer = 0): Integer;
    procedure EndObj;
    function BeginShortObj(AIndex: Integer = 0): Integer;
    procedure EndShortObj;
    procedure BeginDoc;
    procedure EndDoc;
    procedure BeginPage(APage: TRLGraphicSurface);
    procedure EndPage;
    function BeginStream: Integer;
    function EndStream: Integer;
    function GetFontId(const AFontName: AnsiString; AFontStyle: TRLMetaFontStyles): Integer;
    procedure FixupPageSetup;
    procedure Reset;

    procedure WriteText(ALeft, ATop: Double; const AText: string;
      AFontId, AFontSize: Integer);
    function WriteBitmap(ABitmap: TBitmap): Integer;
    procedure WriteBitmapData(ABitmap: TBitmap);
    function WriteJpeg(AJpeg: TJpegImage; InternalDraw: Boolean = False): Integer;
    procedure WriteJpegData(AJpeg: TJpegImage);

    function PDF_PointStr(X, Y: Double): string;
    function PDF_BoundsStr(ALeft, ATop, ARight, ABottom: Double): string;
    function RGBStr(ARed, AGreen, ABlue: Integer): string;
    function GetTextSize(const AText: string; AFont: TRLMetaFont): TPoint;
    procedure DrawText(AObj: TRLTextObject);
    function CreateImageObj(AObj: TRLImageObject): Integer;
    procedure DrawEllipse(AObj: TRLEllipseObject);
    procedure DrawFillRect(AObj: TRLFillRectObject);
    procedure DrawImage(AObj: TRLImageObject);
    procedure WriteLineWidth(Pen: TRLMetaPen);
    procedure DrawLine(AObj: TRLLineObject);
    procedure DrawPixel(AObj: TRLPixelObject);
    procedure DrawPolygon(AObj: TRLPolygonObject);
    procedure DrawPolyline(AObj: TRLPolylineObject);
    procedure DrawRectangle(AObj: TRLRectangleObject);
    procedure DrawResetClipRect(AObj: TRLResetClipRectObject);
    procedure DrawSetClipRect(AObj: TRLSetClipRectObject);
  protected
    // override methods
    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalNewPage; override;
    procedure InternalDrawPage(APage: TRLGraphicSurface); override;
  public
    // constructors & destructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function PDF_CalcTextLeadingPointSize(AFontSize: Word): Word;
    class function PDF_CurrentDateTimeStr: string;
    class function PDF_EncodeText(const AText: string): string;
    class function PDF_FloatToStr(AFloat: Double): string;
    class function PDF_GetDashPattern(ADashPattern: TRLPDFFilterDashPatternType): string;
    class function PDF_IndirectObjStr(AIndex: Integer): AnsiString;
    class function PDF_PixelsToPoints(APixels: Double): Double;
    class function PDF_Zeros(AValue, AWidth: Integer): AnsiString;

    property PageSetup: TRLPDFFilterPageSetup read FPageSetup write SetPageSetup;
    property TextControl: TRLPDFFilterTextControl read FTextControl write SetTextControl;

    //Ricardo - 20101019 - Sobrecarga criada para atender a DLL de geração do DANFE
    procedure FilterPages(APages: TRLGraphicStorage; AOutputBuffer: TStream); overload;
  published
    {@prop DocumentInfo - Informações do documento gerado.
     @links TRLPDFFilterDocumentInfo. :/}
    property DocumentInfo: TRLPDFFilterDocumentInfo
      read FDocumentInfo write SetDocumentInfo;
    {@prop FileName = ancestor /}
    property FileName;
    {@prop DisplayName = ancestor /}
    property DisplayName;
    {@prop ImageFormat - Formato da imagem. :/}
    property ImageFormat: TRLPDFFilterImageFormat
      read FImageFormat write FImageFormat default ifJPeg;
  end;

  {/@class}
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLPDFFilterPageSetup = class(TPersistent)
  private
    FPaperSize: TRLPDFFilterPaperSizeType;
    FLandScape: Boolean;
    FMediaSize: TRLPDFFilterPaperSizeType;
    FMargins: TRLPDFFilterMarginType;
    FPageBorder: Boolean;
    FColumnBorder: TRLPDFFilterColumnBorderType;
    FBorderDashPattern: TRLPDFFilterDashPatternType;
    FColumnMargin: TRLPDFFilterMarginType;
    FColumnGap: TRLPDFFilterLocationType;
    FColumnCount: Word;
    FRowCount: Word;
    FFontPointSize: Word;
    FColumnFontPointSize: Word;
    FLeadingPointSize: Word;
    FColumnLeadingPointSize: Word;
    FCharCount: Word;
    FWorkArea: TRLPDFFilterMarginType;
    FColumns: TRLPDFFilterColumnsType;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TRLPDFFilterPageSetup); reintroduce;
    procedure Clear;

    property PaperSize: TRLPDFFilterPaperSizeType read FPaperSize write FPaperSize;
    property MediaSize: TRLPDFFilterPaperSizeType read FMediaSize write FMediaSize;
    property Margins: TRLPDFFilterMarginType read FMargins write FMargins;
    property ColumnMargin: TRLPDFFilterMarginType read FColumnMargin write FColumnMargin;
    property ColumnGap: TRLPDFFilterLocationType read FColumnGap write FColumnGap;
    property WorkArea: TRLPDFFilterMarginType read FWorkArea write FWorkArea;
  published
    property LandScape: Boolean read FLandScape write FLandScape;
    property PageBorder: Boolean read FPageBorder write FPageBorder;
    property ColumnBorder: TRLPDFFilterColumnBorderType
      read FColumnBorder write FColumnBorder;
    property BorderDashPattern: TRLPDFFilterDashPatternType
      read FBorderDashPattern write FBorderDashPattern;
    property ColumnCount: Word read FColumnCount write FColumnCount;
    property RowCount: Word read FRowCount write FRowCount;
    property FontPointSize: Word read FFontPointSize write FFontPointSize;
    property ColumnFontPointSize: Word read FColumnFontPointSize
      write FColumnFontPointSize;
    property LeadingPointSize: Word read FLeadingPointSize write FLeadingPointSize;
    property ColumnLeadingPointSize: Word read FColumnLeadingPointSize
      write FColumnLeadingPointSize;
    property CharCount: Word read FCharCount write FCharCount;
  end;
	
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  {@class TRLPDFFilterDocumentInfo - Informações para a geração de documento PDF. }
  TRLPDFFilterDocumentInfo = class(TPersistent)
  private
    FTitle: string;
    FSubject: string;
    FAuthor: string;
    FKeyWords: string;
    FCreator: string;
    FProducer: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TRLPDFFilterDocumentInfo); reintroduce;
    procedure Clear;
  published
    {@prop Title - Título do documento. :/}
    property Title: string read FTitle write FTitle;

    {@prop Subject - Assunto do documento. :/}
    property Subject: string read FSubject write FSubject;

    {@prop Author - Nome do autor do documento. :/}
    property Author: string read FAuthor write FAuthor;

    {@prop KeyWords - Palavras chaves para busca. :/}
    property KeyWords: string read FKeyWords write FKeyWords;

    {@prop Creator - Programa utilitário gerador. :/}
    property Creator: string read FCreator write FCreator;

    {@prop Producer - Nome da produtora. :/}
    property Producer: string read FProducer write FProducer;
  end;

  {/@class}
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLPDFFilterTextControl = class(TPersistent)
  private
    FFormFeed: Boolean;
    FWordWrap: Boolean;
    FTabSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TRLPDFFilterTextControl); reintroduce;
    procedure Clear;
  published
    property FormFeed: Boolean read FFormFeed write FFormFeed;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property TabSize: Integer read FTabSize write FTabSize;
  end;

var
  DefaultDocumentInfo: TRLPDFFilterDocumentInfo = nil;
  DefaultTextControl: TRLPDFFilterTextControl = nil;

{/@unit}

implementation

uses StrUtils;

const
  NULLPAPERSIZE: TRLPDFFilterPaperSizeType = (Width: 0; Height: 0);
  NULLMARGIN: TRLPDFFilterMarginType = (Top: 0; Bottom: 0; Left: 0;
    Right: 0; Width: 0; Height: 0);
  NULLLOCATION: TRLPDFFilterLocationType = (X: 0; Y: 0);

// UTILS

function IntToOctal(AValue: Integer; AWidth: Integer = 0): string;
begin
  Result := '';
  repeat
    Result := IntToStr(AValue mod 8) + Result;
    AValue := AValue div 8;
  until AValue = 0;
  while Length(Result) < AWidth do
    Result := '0' + Result;
end;

function GrowNewMen(MemSize: integer): integer;
var
  Delta: Integer;
begin
  if MemSize > 64 then
    Delta := MemSize
  else
    if MemSize > 8 then
      Delta := 16
    else
      Delta := 4;
  Result := MemSize + Delta;
end;

function GrowCapacity(var Ptr; var Capacity: Integer;
  Count, ItemSize, PageIncrement: Integer): Boolean;
var
  NewMem: Integer;
begin
  if Count > Capacity then
  begin
    Capacity := (Count + PageIncrement) ;
    NewMem := GrowNewMen(Capacity * ItemSize);
    if Pointer(Ptr) = nil then
      GetMem(Pointer(Ptr), NewMem)
    else
      ReallocMem(Pointer(Ptr), NewMem);
    Result := True;
  end
  else
    Result := False;
end;

{ TRLPDFFilterPageSetup }

constructor TRLPDFFilterPageSetup.Create;
begin
  FPaperSize := NULLPAPERSIZE;
  FLandScape := False;
  FMediaSize := NULLPAPERSIZE;
  FMargins := NULLMARGIN;
  FPageBorder := False;
  FColumnBorder := cbRegular;
  FBorderDashPattern := dpRegular;
  FColumnMargin := NULLMARGIN;
  FColumnGap := NULLLOCATION;
  FColumnCount := 0;
  FRowCount := 0;
  FFontPointSize := 0;
  FColumnFontPointSize := 0;
  FLeadingPointSize := 0;
  FColumnLeadingPointSize := 0;
  FCharCount := 0;
  FWorkArea := NULLMARGIN;

  inherited Create;
end;

procedure TRLPDFFilterPageSetup.Clear;
begin
  FPaperSize := NULLPAPERSIZE;
  FLandScape := False;
  FMediaSize := NULLPAPERSIZE;
  FMargins := NULLMARGIN;
  FPageBorder := False;
  FColumnBorder := cbRegular;
  FBorderDashPattern := dpRegular;
  FColumnMargin := NULLMARGIN;
  FColumnGap := NULLLOCATION;
  FColumnCount := 0;
  FRowCount := 0;
  FFontPointSize := 0;
  FColumnFontPointSize := 0;
  FLeadingPointSize := 0;
  FColumnLeadingPointSize := 0;
  FCharCount := 0;
  FWorkArea := NULLMARGIN;
end;

procedure TRLPDFFilterPageSetup.Assign(Source: TRLPDFFilterPageSetup);
begin
  PaperSize := Source.PaperSize;
  LandScape := Source.LandScape;
  MediaSize := Source.MediaSize;
  Margins := Source.Margins;
  PageBorder := Source.PageBorder;
  ColumnBorder := Source.ColumnBorder;
  BorderDashPattern := Source.BorderDashPattern;
  ColumnMargin := Source.ColumnMargin;
  ColumnGap := Source.ColumnGap;
  ColumnCount := Source.ColumnCount;
  RowCount := Source.RowCount;
  FontPointSize := Source.FontPointSize;
  ColumnFontPointSize := Source.ColumnFontPointSize;
  LeadingPointSize := Source.LeadingPointSize;
  ColumnLeadingPointSize := Source.ColumnLeadingPointSize;
  CharCount := Source.CharCount;
  WorkArea := Source.WorkArea;
end;

destructor TRLPDFFilterPageSetup.Destroy;
begin
  inherited;
end;

{ TRLPDFFilterDocumentInfo }

constructor TRLPDFFilterDocumentInfo.Create;
begin
  FTitle := '';
  FSubject := '';
  FAuthor := '';
  FKeyWords := '';
  FCreator := '';
  FProducer := '';

  inherited Create;
end;

procedure TRLPDFFilterDocumentInfo.Clear;
begin
  FTitle := '';
  FSubject := '';
  FAuthor := '';
  FKeyWords := '';
  FCreator := '';
  FProducer := '';
end;

procedure TRLPDFFilterDocumentInfo.Assign(Source: TRLPDFFilterDocumentInfo);
begin
  Title := Source.Title;
  Subject := Source.Subject;
  Author := Source.Author;
  KeyWords := Source.KeyWords;
  Creator := Source.Creator;
  Producer := Source.Producer;
end;

destructor TRLPDFFilterDocumentInfo.Destroy;
begin
  inherited;
end;

{ TRLPDFFilterTextControl }

constructor TRLPDFFilterTextControl.Create;
begin
  FFormFeed := True;
  FWordWrap := False;
  FTabSize := 8;

  inherited Create;
end;

procedure TRLPDFFilterTextControl.Clear;
begin
  FFormFeed := False;
  FWordWrap := False;
  FTabSize := 8;
end;

procedure TRLPDFFilterTextControl.Assign(Source: TRLPDFFilterTextControl);
begin
  FormFeed := Source.FormFeed;
  WordWrap := Source.WordWrap;
  TabSize := Source.TabSize;
end;

destructor TRLPDFFilterTextControl.Destroy;
begin
  inherited;
end;

{ TRLPDFFilter }

constructor TRLPDFFilter.Create(AOwner: TComponent);
begin
  FDocumentInfo := nil;
  FPageSetup := nil;
  FTextControl := nil;
  FObjects := nil;
  FObjectCapacity := 0;
  FOutlines := nil;
  FOutlineCapacity := 0;
  FReserveds := nil;
  FReservedCapacity := 0;
  FFonts := nil;
  FFontCapacity := 0;
  FImages := nil;
  FImageCapacity := 0;
  FPages := nil;
  FPageCapacity := 0;

  FDocumentInfo := TRLPDFFilterDocumentInfo.Create;
  FPageSetup := TRLPDFFilterPageSetup.Create;
  FTextControl := TRLPDFFilterTextControl.Create;
  FImageFormat := ifJPeg;

  inherited Create(AOwner);

  DefaultExt := '.pdf';
  DisplayName := GetLocalizeStr(LocaleStrings.LS_PDFFormatStr);

  FixupPageSetup;
  Reset;
end;

destructor TRLPDFFilter.Destroy;
begin
  if Assigned(FDocumentInfo) then
    FreeAndNil(FDocumentInfo);
  if Assigned(FPageSetup) then
    FreeAndNil(FPageSetup);
  if Assigned(FTextControl) then
    FreeAndNil(FTextControl);
  if FObjects <> nil then
    FreeMem(FObjects);
  if FOutlines <> nil then
    FreeMem(FOutlines);
  if FReserveds <> nil then
    FreeMem(FReserveds);
  if FFonts <> nil then
    FreeMem(FFonts);
  if FImages <> nil then
    FreeMem(FImages);
  if FPages <> nil then
    FreeMem(FPages);

  inherited;
end;

procedure TRLPDFFilter.InternalBeginDoc;
begin
  FPrintCut.X := 0;
  FPrintCut.Y := 0;
  FPrintSize.X := Pages.OrientedWidth;
  FPrintSize.Y := Pages.OrientedHeight;
  FPagePrintSize := FPrintSize;

  if not FUsingExternalOutputBuffer then
    FOutputStream := TFileStream.Create(FileName, fmCreate);

  BeginDoc;
end;

procedure TRLPDFFilter.InternalEndDoc;
begin
  EndDoc;

  if not FUsingExternalOutputBuffer then  
    FOutputStream.Free;
end;

function TRLPDFFilter.GetTextSize(const AText: string; AFont: TRLMetaFont): TPoint;
var
  B: TBitmap;
begin
  B := NeedAuxBitmap;
  try
    with B.Canvas do
    begin
      Font.Name := AFont.Name;
      Font.Size := AFont.Size;
      Result.X := TextWidth(AText);
      Result.Y := TextHeight(AText);
    end;
  finally
    B.Free;
  end;
end;

function Bitmap32Of(Src: TGraphic): TBitmap;
begin
  if (Src is TBitmap) and (TBitmap(Src).PixelFormat = pf32bit) then
    Result := TBitmap(Src)
  else
  begin
    Result := TBitmap.Create;
    Result.Width := Src.Width;
    Result.Height := Src.Height;
    Result.PixelFormat := pf32bit;
    Result.Canvas.Draw(0, 0, Src);
  end;
end;

function JPeg8Of(Src: TGraphic): TJPEGImage;
var
  bmp: TBitmap;
begin
  if (Src is TJPEGImage) and (TJPEGImage(Src).PixelFormat = {$IfDef FPC}pf8bit{$Else}jf8Bit{$EndIf}) then
    Result := TJPEGImage(Src)
  else
  begin
    {$IfDef FPC}
    Result := TJPEGImage.Create;
    Result.Width := Src.Width;
    Result.Height := Src.Height;
    Result.PixelFormat := pf32bit;
    Result.Canvas.Draw(0, 0, Src);
    // FPC always will change "PixelFormat" to pf24bit, when you try to read it...
    // So I changed WriteJpeg() to receive a Flag as a parameter
    {$Else}
    bmp := Bitmap32Of(Src);
    try
      Result := TJPEGImage.Create;
      Result.PixelFormat := {$IfDef FPC}pf8bit{$Else}jf8Bit{$EndIf};
      Result.Assign(bmp);
    finally
      if bmp <> Src then
        bmp.Free;
    end;
    {$EndIf}
  end;
end;

procedure TRLPDFFilter.DrawText(AObj: TRLTextObject);
var
  L, T: Integer;
  W: TPoint;
  S: string;
  R: TRLMetaRect;
  MagicX, MagicY: Integer;
begin
  MagicX := GetTextSize('1', AObj.Font).X;
  MagicY := 0;

  Writeln('q');

  // font color
  with AObj.Font.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');

  // define retângulo de corte
  if (AObj.TextFlags and MetaTextFlagAutoSize) = 0 then
  begin
    R := AObj.BoundsRect;
    case AObj.Alignment of
      MetaTextAlignmentLeft: Inc(R.Right, MagicX); //isso faz vazar à direita, mas é aceitável. Por enquanto.
      MetaTextAlignmentRight: Dec(R.Left, MagicX);
      MetaTextAlignmentCenter,
      MetaTextAlignmentJustify:
      begin
        Dec(R.Left, MagicX div 2);
        Inc(R.Right, MagicX div 2);
      end;
    end;
    Writeln(PDF_BoundsStr(R.Left, R.Top, R.Right, R.Bottom) + ' re');
    Writeln('W n');
  end;

  // posicao esquerda
  S := AObj.DisplayText;
  W := GetTextSize(S, AObj.Font);
  case AObj.Alignment of
    MetaTextAlignmentRight:
    begin
      L := AObj.BoundsRect.Right - W.X;
      L := L - MagicX;
    end;
    MetaTextAlignmentCenter: L :=
        (AObj.BoundsRect.Left + AObj.BoundsRect.Right - W.X) div 2;
  else
    L := AObj.Origin.X;
  end;

  case AObj.Layout of
    MetaTextLayoutTop:
    begin
      T := FPagePrintSize.Y - (AObj.BoundsRect.Top + W.Y);
      T := T - MagicY;
    end;
    MetaTextLayoutCenter:
    begin
      T := (AObj.BoundsRect.Top + AObj.BoundsRect.Bottom - W.Y) div 2;
      T := FPagePrintSize.Y - (T + W.Y);
    end;
  else
    T := FPagePrintSize.Y - AObj.BoundsRect.Bottom;
  end;
  T := T + W.Y div 4; //Ajuste fino para as posições verticais das palavras impressas.(magic)
  // textout
  WriteText(
    PDF_PixelsToPoints(L),
    PDF_PixelsToPoints(T),
    AObj.DisplayText, GetFontId(AObj.Font.Name, AObj.Font.Style), AObj.Font.Size);
  Writeln('Q');
end;

function TRLPDFFilter.CreateImageObj(AObj: TRLImageObject): Integer;
var
  grp: TGraphic;
  bmp: TBitmap;
  jpg: TJPEGImage;
  imf: TRLPDFFilterImageFormat;
begin
  Result := 0;
  bmp := nil;
  jpg := nil;
  grp := FromMetaGraphic(AObj.Data);
  try
    if FImageFormat = ifOriginal then
      if grp is TJPEGImage then
        imf := ifJPeg
      else
        imf := ifBitmap
    else
      imf := FImageFormat;
    case imf of
      ifBitmap:
      begin
        bmp := Bitmap32Of(grp);
        Result := WriteBitmap(bmp);
      end;
      ifJPeg:
      begin
        jpg := JPeg8Of(grp);
        Result := WriteJpeg(jpg, True);
      end;
    else
      raise Exception.Create('Unknown imageformat');
    end;
  finally
    if (jpg <> nil) and (jpg <> grp) then
      jpg.Free;
    if (bmp <> nil) and (bmp <> grp) then
      bmp.Free;
    grp.Free;
  end;
end;

procedure TRLPDFFilter.DrawImage(AObj: TRLImageObject);
begin
  Writeln('q');
  // matriz de posicionamento (translation: 1 0 0 1 left top)
  Writeln('1 0 0 1 ' + PDF_PointStr(AObj.BoundsRect.Left,
    AObj.BoundsRect.Bottom) + ' cm');
  // matriz de rotação (rotation: cosT sinT -sinT cosT 0 0)
  // WriteLn('1 0 0 1 0 0 cm');
  // matriz de escalonameto (scale: width 0 0 height 0 0)
  Writeln(PDF_FloatToStr(PDF_PixelsToPoints(AObj.BoundsRect.Right -
    AObj.BoundsRect.Left)) + ' 0 0 ' + PDF_FloatToStr(
    PDF_PixelsToPoints(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top)) + ' 0 0 cm');
  // matriz de inclinação (skew: 1 tanX tanY 1 0 0)
  // WriteLn('1 0 0 1 0 0 cm');
  Writeln('/Image' + IntToStr(AObj.Tag) + ' Do');
  Writeln('Q');
end;

procedure TRLPDFFilter.WriteLineWidth(Pen: TRLMetaPen);
begin
  // line width
  if Pen.Style = MetaPenStyleClear then
    Writeln(PDF_FloatToStr(PDF_PixelsToPoints(0)) + ' w')
  else
    Writeln(PDF_FloatToStr(PDF_PixelsToPoints(Pen.Width)) + ' w');
end;

procedure TRLPDFFilter.DrawLine(AObj: TRLLineObject);
begin
  // line width
  WriteLineWidth(AObj.Pen);
  // shape of ending points (0 = none)
  Writeln('0 J');
  // dashing
  case AObj.Pen.Style of
    MetaPenStyleSolid: Writeln('[] 0 d');
    MetaPenStyleDash: Writeln('[3] 0 d');
    MetaPenStyleDot: Writeln('[1] 0 d');
    MetaPenStyleDashDot: Writeln('[2 1] 0 d');
    MetaPenStyleDashDotDot: Writeln('[3 5] 6 d');
    MetaPenStyleClear: ;
    MetaPenStyleInsideFrame: Writeln('[] 0 d');
  end;
  // color
  with AObj.Pen.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' RG');
  // moveto
  with AObj.FromPoint do
    Writeln(PDF_PointStr(X, Y) + ' m');
  // lineto
  with AObj.ToPoint do
    Writeln(PDF_PointStr(X, Y) + ' l');
  // makeit
  Writeln('S');
end;

procedure TRLPDFFilter.DrawRectangle(AObj: TRLRectangleObject);
begin
  // line width
  WriteLineWidth(AObj.Pen);
  // shape of ending points (0 = none)
  Writeln('0 J');
  // dashing
  case AObj.Pen.Style of
    MetaPenStyleSolid: Writeln('[] 0 d');
    MetaPenStyleDash: Writeln('[3] 0 d');
    MetaPenStyleDot: Writeln('[1] 0 d');
    MetaPenStyleDashDot: Writeln('[2 1] 0 d');
    MetaPenStyleDashDotDot: Writeln('[3 5] 6 d');
    MetaPenStyleClear: ;
    MetaPenStyleInsideFrame: Writeln('[] 0 d');
  end;
  // pen color
  with AObj.Pen.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' RG');
  // brush color
  with AObj.Brush.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');
  // rectangle
  with AObj.BoundsRect do
    Writeln(PDF_BoundsStr(Left, Top, Right, Bottom) + ' re');
  // makeit
  Writeln('f');
end;

procedure TRLPDFFilter.DrawSetClipRect(AObj: TRLSetClipRectObject);
begin
  Writeln('q');
  // rectangle
  with AObj.BoundsRect do
    Writeln(PDF_BoundsStr(Left, Top, Right, Bottom) + ' re');
  Writeln('W n');
end;

procedure TRLPDFFilter.DrawResetClipRect(AObj: TRLResetClipRectObject);
begin
  Writeln('Q');
end;

procedure TRLPDFFilter.DrawFillRect(AObj: TRLFillRectObject);
begin
  // line width
  Writeln(PDF_FloatToStr(PDF_PixelsToPoints(0)) + ' w');
  // brush color
  with AObj.Brush.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');
  // rectangle
  with AObj.BoundsRect do
    Writeln(PDF_BoundsStr(Left, Top, Right, Bottom) + ' re');
  // makeit
  Writeln('f');
end;

procedure TRLPDFFilter.DrawEllipse(AObj: TRLEllipseObject);
var
  cx, cy, rx, ry, D, S, C: Double;
  I: Integer;
begin
  // line width
  WriteLineWidth(AObj.Pen);
  // shape of ending points (0 = none)
  Writeln('0 J');
  // dashing
  case AObj.Pen.Style of
    MetaPenStyleSolid: Writeln('[] 0 d');
    MetaPenStyleDash: Writeln('[3] 0 d');
    MetaPenStyleDot: Writeln('[1] 0 d');
    MetaPenStyleDashDot: Writeln('[2 1] 0 d');
    MetaPenStyleDashDotDot: Writeln('[3 5] 6 d');
    MetaPenStyleClear: ;
    MetaPenStyleInsideFrame: Writeln('[] 0 d');
  end;
  // pen color
  with AObj.Pen.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' RG');
  // brush color
  with AObj.Brush.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');
  // the curves
  with AObj.BoundsRect do
  begin
    cx := (Left + Right) / 2;
    cy := (Top + Bottom) / 2;
    rx := (Right - Left) / 2;
    ry := (Bottom - Top) / 2;
  end;
  I := 0;
  while I < 36 do
  begin
    D := 2 * pi * I / 36;
    S := Sin(D);
    C := Cos(D);
    Write(PDF_PointStr(cx + C * rx, cy + S * ry));
    if I = 0 then
      Write(' m ')
    else
      Write(' l ');
    Inc(I, 1);
  end;
  Writeln('b');
end;

procedure TRLPDFFilter.DrawPolygon(AObj: TRLPolygonObject);
var
  I: Integer;
begin
  // line width
  WriteLineWidth(AObj.Pen);
  // shape of ending points (0 = none)
  Writeln('0 J');
  // dashing
  case AObj.Pen.Style of
    MetaPenStyleSolid: Writeln('[] 0 d');
    MetaPenStyleDash: Writeln('[3] 0 d');
    MetaPenStyleDot: Writeln('[1] 0 d');
    MetaPenStyleDashDot: Writeln('[2 1] 0 d');
    MetaPenStyleDashDotDot: Writeln('[3 5] 6 d');
    MetaPenStyleClear: ;
    MetaPenStyleInsideFrame: Writeln('[] 0 d');
  end;
  // pen color
  with AObj.Pen.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' RG');
  // brush color
  with AObj.Brush.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');

  for I := 0 to High(AObj.Points) do
  begin
    Write(PDF_PointStr(AObj.Points[I].X, AObj.Points[I].Y));
    if I = 0 then
      Writeln(' m ')
    else
      Writeln(' l ');
  end;
  // makeit
  Writeln('b');
end;

procedure TRLPDFFilter.DrawPolyline(AObj: TRLPolylineObject);
var
  I: Integer;
begin
  // line width
  WriteLineWidth(AObj.Pen);
  // shape of ending points (0 = none)
  Writeln('0 J');
  // dashing
  case AObj.Pen.Style of
    MetaPenStyleSolid: Writeln('[] 0 d');
    MetaPenStyleDash: Writeln('[3] 0 d');
    MetaPenStyleDot: Writeln('[1] 0 d');
    MetaPenStyleDashDot: Writeln('[2 1] 0 d');
    MetaPenStyleDashDotDot: Writeln('[3 5] 6 d');
    MetaPenStyleClear: ;
    MetaPenStyleInsideFrame: Writeln('[] 0 d');
  end;
  // pen color
  with AObj.Pen.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' RG');

  for I := 0 to High(AObj.Points) do
  begin
    Write(PDF_PointStr(AObj.Points[I].X, AObj.Points[I].Y));
    if I = 0 then
      Writeln(' m ')
    else
      Writeln(' l ');
  end;
  // makeit
  Writeln('S');
end;

procedure TRLPDFFilter.DrawPixel(AObj: TRLPixelObject);
begin
  // line width
  Writeln(PDF_FloatToStr(PDF_PixelsToPoints(0)) + ' w');
  // shape of ending points (0 = none)
  Writeln('0 J');
  // brush color
  with AObj.Color do
    Writeln(RGBStr(Red, Green, Blue) + ' rg');
  // rectangle
  with AObj.BoundsRect do
    Writeln(PDF_BoundsStr(Left, Top, Right, Bottom) + ' re');
  // makeit
  Writeln('f');
end;

procedure TRLPDFFilter.InternalDrawPage(APage: TRLGraphicSurface);
var
  obj: TRLGraphicObject;
  I: Integer;
begin
  // cria imagens
  for I := 0 to APage.ObjectCount - 1 do
  begin
    obj := APage.Objects[I];
    if obj is TRLImageObject then
      obj.Tag := CreateImageObj(TRLImageObject(obj));
  end;

  BeginPage(APage);
  for I := 0 to APage.ObjectCount - 1 do
  begin
    obj := APage.Objects[I];
    if obj is TRLPixelObject then
      DrawPixel(TRLPixelObject(obj))
    else if obj is TRLLineObject then
      DrawLine(TRLLineObject(obj))
    else if obj is TRLRectangleObject then
      DrawRectangle(TRLRectangleObject(obj))
    else if obj is TRLTextObject then
      DrawText(TRLTextObject(obj))
    else if obj is TRLFillRectObject then
      DrawFillRect(TRLFillRectObject(obj))
    else if obj is TRLEllipseObject then
      DrawEllipse(TRLEllipseObject(obj))
    else if obj is TRLPolygonObject then
      DrawPolygon(TRLPolygonObject(obj))
    else if obj is TRLPolylineObject then
      DrawPolyline(TRLPolylineObject(obj))
    else if obj is TRLImageObject then
      DrawImage(TRLImageObject(obj))
    else if obj is TRLSetClipRectObject then
      DrawSetClipRect(TRLSetClipRectObject(obj))
    else if obj is TRLResetClipRectObject then
      DrawResetClipRect(TRLResetClipRectObject(obj));
  end;
  EndPage;
end;

procedure TRLPDFFilter.InternalNewPage;
begin
end;

procedure TRLPDFFilter.SetDocumentInfo(const Value: TRLPDFFilterDocumentInfo);
begin
  if Value = nil then
    FDocumentInfo.Clear
  else
    FDocumentInfo.Assign(Value);
end;

procedure TRLPDFFilter.SetPageSetup(const Value: TRLPDFFilterPageSetup);
begin
  if Value = nil then
    FPageSetup.Clear
  else
    FPageSetup.Assign(Value);
end;

procedure TRLPDFFilter.SetTextControl(const Value: TRLPDFFilterTextControl);
begin
  if Value = nil then
    FTextControl.Clear
  else
    FTextControl.Assign(Value);
end;

procedure TRLPDFFilter.Write(const AStr: AnsiString = '');
var
  L: Integer;
begin
  L := Length(AStr);
  if L > 0 then
  begin
    FOutputStream.Write(AStr[1], L);
    Inc(FWritePos, L);
  end;
end;

procedure TRLPDFFilter.Writeln(const AStr: AnsiString = '');
begin
  Write(AStr);
  Write(PDF_EOL);
end;

function TRLPDFFilter.PDF_PointStr(X, Y: Double): string;
begin
  Result := PDF_FloatToStr(PDF_PixelsToPoints(X)) + ' ' +
    PDF_FloatToStr(PDF_PixelsToPoints(FPagePrintSize.Y - Y));
end;

function TRLPDFFilter.PDF_BoundsStr(ALeft, ATop, ARight, ABottom: Double): string;
begin
  Result := PDF_FloatToStr(PDF_PixelsToPoints(ALeft)) + ' ' +
    PDF_FloatToStr(PDF_PixelsToPoints(FPagePrintSize.Y - ABottom)) +
    ' ' + PDF_FloatToStr(PDF_PixelsToPoints(ARight - ALeft)) +
    ' ' + PDF_FloatToStr(PDF_PixelsToPoints(ABottom - ATop));
end;

function TRLPDFFilter.RGBStr(ARed, AGreen, ABlue: Integer): string;
begin
  Result := PDF_FloatToStr(ARed / 255) + ' ' + PDF_FloatToStr(AGreen / 255) +
    ' ' + PDF_FloatToStr(ABlue / 255);
end;

procedure TRLPDFFilter.BeginDoc;
begin
  FWritePos := 0;
  FCurrentPageNo := 0;
  FObjectCount := 0;
  FOutlineCount := 0;
  FReservedCount := 0;
  FPageTreeObjAt := 0;
  FOutlinesObjAt := 0;
  FCatalogObjAt := 0;
  FInfoObjAt := 0;
  FPageMode := pmRegular;
  FFullPageMode := pmRegular;
  FPageLayout := plRegular;
  FViewerOptions := voRegular;
  FForceASCII := False;
  FFontEncoding := feWinAnsiEncoding;
  FFontName := PS1FONTNAMES[5];
  FFontCount := 0;
  FImageCount := 0;
  FPageCount := 0;
  FResourcesObjAt := 0;
  FPageOffset := 0;
  FPageTitle := '';
  FXRefOfs := 0;
  FContentsObjAt := 0;
  FLengthObjAt := 0;

  WriteBOF;
  WriteInfo;
  WriteCatalog;

  FResourcesObjAt := ReservObj;
end;

procedure TRLPDFFilter.EndDoc;
begin
  WriteFonts;
  WriteResources;
  if FOutlinesObjAt > 0 then
    WriteOutlines;
  WritePageTree;
  WriteXRef;
  WriteTrailer;
  WriteEOF;
end;

function TRLPDFFilter.GetFontId(const AFontName: AnsiString;
  AFontStyle: TRLMetaFontStyles): Integer;
var
  I: Integer;
  N: AnsiString;
  S: TRLMetaFontStyles;
  F: ^TRLPDFFilterFontType;
begin
  N := AFontName;
  S := AFontStyle;
  I := 1;
  while (I <= FFontCount) and not (SameText(StrPas(FFonts[I - 1].FontName), N) and
      (FFonts[I - 1].FontStyle = S)) do
    Inc(I);
  if I <= FFontCount then
    Result := I
  else
  begin
    Inc(FFontCount);
    GrowCapacity(FFonts, FFontCapacity, FFontCount, SizeOf(TRLPDFFilterFontType), 64);
    F := @FFonts[FFontCount - 1];
    StrPCopy(F.FontName, Copy(N, 1, SizeOf(F.FontName) - 1));
    F.FontStyle := S;
    F.FontObjAt := ReservObj;
    Result := FFontCount;
  end;
end;

procedure TRLPDFFilter.WriteBOF;
begin
  Writeln('%PDF-' + PDF_FILEVERSION);
end;

procedure TRLPDFFilter.WriteInfo;
begin
  FInfoObjAt := BeginObj;

  Writeln('/CreationDate(' + PDF_CurrentDateTimeStr + ')');
  with FDocumentInfo do
  begin
    if Title <> '' then
      Writeln('/Title(' + GetAnsiStr(Title) + ')');
    if Subject <> '' then
      Writeln('/Subject(' + GetAnsiStr(Subject) + ')');
    if Author <> '' then
      Writeln('/Author(' + GetAnsiStr(Author) + ')');
    if Keywords <> '' then
      Writeln('/Keywords(' + GetAnsiStr(Keywords) + ')');
    if Creator <> '' then
      Writeln('/Creator(' + GetAnsiStr(Creator) + ')');
    if Producer <> '' then
      Writeln('/Producer(' + GetAnsiStr(Producer) + ')');
  end;

  EndObj;
end;

procedure TRLPDFFilter.WriteCatalog;
begin
  FCatalogObjAt := BeginObj;

  Writeln('/Type/Catalog');
  FPageTreeObjAt := ReservObj;
  Writeln('/Pages ' + PDF_IndirectObjStr(FPageTreeObjAt));
  if FPageMode = pmOutlines then
  begin
    FOutlinesObjAt := ReservObj;
    Writeln('/Outlines ' + PDF_IndirectObjStr(FOutlinesObjAt));
  end;
  // setpagemode
  if FPageMode <> pmRegular then
  begin
    Write('/PageMode');
    case FPageMode of
      pmOutlines: Writeln('/UseOutlines');
      pmThumbs: Writeln('/UseThumbs');
      pmFullScreen: Writeln('/FullScreen');
    end;
  end;
  // viewer
  if (FViewerOptions <> voRegular) or (FPageLayout <> plRegular) then
  begin
    Write('/ViewerPreferences [');
    if (FViewerOptions and voHideToolBar) <> 0 then
      Write('/HideToolBar');
    if (FViewerOptions and voHideMenuBar) <> 0 then
      Write('/HideMenuBar');
    if (FViewerOptions and voHideWindowUI) <> 0 then
      Write('/HideWindowUI');
    if (FViewerOptions and voFitWindow) <> 0 then
      Write('/FitWindow');
    if (FViewerOptions and voCenterWindow) <> 0 then
      Write('/CenterWindow');
    if FPageLayout <> plRegular then
    begin
      Write('/PageLayout');
      case FPageLayout of
        plSinglePage: Write('/SinglePage');
        plOneColumn: Write('/OneColumn');
        plTwoColumnLeft: Write('/TwoColumnLeft');
        plTwoColumnRight: Write('/TwoColumnRight');
      end;
    end;
    if (FPageMode = pmFullScreen) and (FFullPageMode <> pmRegular) then
    begin
      Write('/PageMode');
      case FFullPageMode of
        pmOutlines: Write('/UseOutlines');
        pmThumbs: Write('/UseThumbs');
        pmFullScreen: Write('/FullScreen');
      end;
    end;
    Writeln(']');
  end;

  EndObj;
end;

procedure TRLPDFFilter.WriteFonts;
var
  I, J, W: Integer;
  N: AnsiString;
  S: TRLMetaFontStyles;
  dsc: Integer;
  ttf: Boolean;
  rec: TRLMetaFontMetrics;
begin
  dsc := ReservObj;
  for I := 1 to FFontCount do
  begin
    with FFonts[I - 1] do
    begin
      N := StringReplace(FontName, ' ', '', [rfReplaceAll]);
      S := FontStyle;
      BeginObj(FontObjAt);
    end;
    J := 1;
    while (J <= PDF_MAXPSTYPE1) and not SameText(PS1FONTNAMES[J], N) do
      Inc(J);
    Write('/Type/Font/Subtype');
    ttf := (J > PDF_MAXPSTYPE1);
    if ttf then
    begin
      FontGetMetrics(FFonts[I - 1].FontName, FromMetaFontStyles(S), rec);
      if (S and MetaFontStyleBold) = MetaFontStyleBold then
        if (S and MetaFontStyleItalic) = MetaFontStyleItalic then
          N := N + ',BoldItalic'
        else
          N := N + ',Bold'
      else if (S and MetaFontStyleItalic) = MetaFontStyleItalic then
        N := N + ',Italic';
      if (S and MetaFontStyleUnderline) = MetaFontStyleUnderline then
        N := N + ',Underline';
      if (S and MetaFontStyleStrikeOut) = MetaFontStyleStrikeOut then
        N := N + ',StrikeOut';
      Writeln('/TrueType/Name/F' + IntToStr(I) + '/BaseFont/' + N);
      Writeln('/FirstChar ' + IntToStr(rec.FirstChar));
      Writeln('/LastChar ' + IntToStr(rec.LastChar));
      Writeln('/Widths [');
      W := 0;
      for J := rec.FirstChar to rec.LastChar do
      begin
        if W > 17 then
        begin
          Writeln;
          W := 0;
        end;
        Write(IntToStr(MulDiv(rec.Widths[J], 96, ScreenPPI)) + ' ');
        Inc(W);
      end;
      Writeln;
      Writeln(']');
      Writeln('/FontDescriptor ' + PDF_IndirectObjStr(dsc));
    end
    else
    begin
      Write('/Type1/Name/F' + IntToStr(I) + '/BaseFont/' + N);
      if FForceASCII then
        Write('/FirstChar 0/LastChar 255');
    end;
    case FFontEncoding of
      feNoEncoding: ;
      feMacRomanEncoding: Write('/Encoding/MacRomanEncoding');
      feWinAnsiEncoding: Write('/Encoding/WinAnsiEncoding');
    end;
    Writeln;
    EndObj;

    if ttf then
    begin
      BeginObj(dsc);
      Writeln('/Type/FontDescriptor');
      Writeln('/FontName/' + N);
      Writeln('/Flags ' + IntToStr(rec.FontDescriptor.Flags));
      with rec.FontDescriptor.FontBBox do
        Writeln('/FontBBox [ ' + IntToStr(Left) + ' ' + IntToStr(Bottom) +
          ' ' + IntToStr(Right) + ' ' + IntToStr(Top) + ' ]');
      Writeln('/MissingWidth 0');
      Writeln('/StemV 73');
      Writeln('/StemH 73');
      Writeln('/ItalicAngle ' + IntToStr(rec.FontDescriptor.ItalicAngle));
      Writeln('/CapHeight ' + IntToStr(rec.FontDescriptor.CapHeight));
      Writeln('/XHeight ' + IntToStr(rec.FontDescriptor.XHeight));
      Writeln('/Ascent ' + IntToStr(rec.FontDescriptor.Ascent));
      Writeln('/Descent ' + IntToStr(rec.FontDescriptor.Descent));
      Writeln('/Leading ' + IntToStr(rec.FontDescriptor.Leading));
      Writeln('/MaxWidth ' + IntToStr(rec.FontDescriptor.MaxWidth));
      Writeln('/AvgWidth ' + IntToStr(rec.FontDescriptor.AvgWidth));
      EndObj;
    end;
  end;
end;

procedure TRLPDFFilter.WriteResources;
var
  I: Integer;
begin
  BeginObj(FResourcesObjAt);
  Writeln('/ProcSet [ /PDF/Text/ImageB ]');
  Writeln('/Font <<');
  for I := 1 to FFontCount do
    Writeln('/F' + IntToStr(I) + ' ' + PDF_IndirectObjStr(FFonts[I - 1].FontObjAt));
  Writeln('>>');
  Writeln('/XObject <<');
  for I := 1 to FImageCount do
    Writeln('/Image' + IntToStr(I) + ' ' + PDF_IndirectObjStr(
      FImages[I - 1].ImageObjAt));
  Writeln('>>');
  EndObj;
end;

procedure TRLPDFFilter.WriteOutlines;
var
  I, this, prev: Integer;
begin
  BeginObj(FOutlinesObjAt);
  Writeln('/Count ' + IntToStr(FOutlineCount));
  if FOutlineCount > 0 then
  begin
    Writeln('/First ' + PDF_IndirectObjStr(FObjectCount + 1));
    Writeln('/Last ' + PDF_IndirectObjStr(FObjectCount + FOutlineCount));
  end;
  EndObj;

  prev := 0;
  for I := 1 to FOutlineCount do
    with FOutlines[I - 1] do
    begin
      this := BeginObj;
      if (ObjId = 0) and ((Page > 0) and (Page <= FPageCount)) then
        ObjId := FPages[Page - 1];
      Writeln('/Title (' + Title + ')');
      Writeln('/Dest [' + PDF_IndirectObjStr(ObjId) +
        {IntToStr(i-1)+} ' /XYZ null null null]');
      Writeln('/Parent ' + PDF_IndirectObjStr(FOutlinesObjAt));
      if prev <> 0 then
        Writeln('/Previous ' + PDF_IndirectObjStr(prev));
      prev := this;
      if I < FOutlineCount then
        Writeln('/Next ' + PDF_IndirectObjStr(NextObjId));
      EndObj;
    end;
end;

procedure TRLPDFFilter.WritePageTree;
var
  I: Integer;
begin
  BeginObj(FPageTreeObjAt);
  Writeln('/Type/Pages');
  Writeln('/Count ' + IntToStr(FPageCount));
  Writeln('/MediaBox [ 0 0 ' + PDF_FloatToStr(PDF_PixelsToPoints(FPagePrintSize.X)) +
    ' ' + PDF_FloatToStr(PDF_PixelsToPoints(FPagePrintSize.Y)) + ' ]');
  Write('/Kids [ ');
  for I := 1 to FPageCount do
    Write(PDF_IndirectObjStr(FPages[I - 1]) + ' ');
  Writeln(']');
  EndObj;
end;

procedure TRLPDFFilter.WriteXRef;
var
  I: Integer;
begin
  FXRefOfs := FWritePos;
  Writeln('xref');
  Writeln('0 ' + IntToStr(GetLastObjectId + 1)); // mais 1 por causa da linha abaixo:
  Writeln('0000000000 65535 f');
  for I := 1 to GetLastObjectId do
    Writeln(PDF_Zeros(FObjects[I - 1], 10) + ' 00000 n');
end;

procedure TRLPDFFilter.WriteTrailer;
begin
  Writeln('trailer');
  Writeln('<<');
  Writeln('/Size ' + IntToStr(GetLastObjectId + 1));
  Writeln('/Root ' + PDF_IndirectObjStr(FCatalogObjAt));
  Writeln('/Info ' + PDF_IndirectObjStr(FInfoObjAt));
  Writeln('>>');
end;

procedure TRLPDFFilter.WriteEOF;
begin
  Writeln('startxref');
  Writeln(IntToStr(FXRefOfs));
  Writeln('%%EOF');
end;

function TRLPDFFilter.ReservObj: Integer;
var
  I, M: Integer;
begin
  M := FObjectCount;
  for I := 1 to FReservedCount do
    M := Max(M, FReserveds[I - 1]);
  Result := M + 1;
  Inc(FReservedCount);
  GrowCapacity(FReserveds, FReservedCapacity, FReservedCount, SizeOf(Integer), 512);
  FReserveds[FReservedCount - 1] := Result;
end;

function TRLPDFFilter.IsReservedObj(AIndex: Integer): Boolean;
var
  I: Integer;
begin
  I := 1;
  while (I <= FReservedCount) and (FReserveds[I - 1] <> AIndex) do
    Inc(I);
  Result := I <= FReservedCount;
end;

function TRLPDFFilter.GetLastObjectId: Integer;
var
  I: Integer;
begin
  Result := FObjectCount;
  for I := 1 to FReservedCount do
    if FReserveds[I - 1] > Result then
      Result := FReserveds[I - 1];
end;

function TRLPDFFilter.NextObjId: Integer;
begin
  Result := FObjectCount;
  repeat
    Inc(Result);
  until not IsReservedObj(Result);
end;

function TRLPDFFilter.NewObjId: Integer;
begin
  Result := NextObjId;
  FObjectCount := Result;
  GrowCapacity(FObjects, FObjectCapacity, FObjectCount, SizeOf(Integer), 512);
end;

function TRLPDFFilter.BeginObj(AIndex: Integer = 0): Integer;
begin
  if AIndex = 0 then
    Result := NewObjId
  else
    Result := AIndex;
  FObjects[Result - 1] := FWritePos;
  Writeln(IntToStr(Result) + ' 0 obj <<');
end;

procedure TRLPDFFilter.EndObj;
begin
  Writeln('>> endobj');
end;

function TRLPDFFilter.BeginShortObj(AIndex: Integer = 0): Integer;
begin
  if AIndex = 0 then
    Result := NewObjId
  else
    Result := AIndex;
  FObjects[Result - 1] := FWritePos;
  Writeln(IntToStr(Result) + ' 0 obj');
end;

procedure TRLPDFFilter.EndShortObj;
begin
  Writeln('endobj');
end;

procedure TRLPDFFilter.BeginPage(APage: TRLGraphicSurface);
begin
  // 20130108:ronaldo,req:marcela
  FPagePrintSize.X := APage.Width;
  FPagePrintSize.Y := APage.Height;

  Inc(FPageCount);
  GrowCapacity(FPages, FPageCapacity, FPageCount, SizeOf(Integer), 128);
  FPages[FPageCount - 1] := BeginObj;
  Writeln('/Type/Page');
  Writeln('/Parent ' + PDF_IndirectObjStr(FPageTreeObjAt));
  Writeln('/Resources ' + PDF_IndirectObjStr(FResourcesObjAt));
  Writeln('/MediaBox [ 0 0 ' + PDF_FloatToStr(PDF_PixelsToPoints(APage.Width)) +
    ' ' + PDF_FloatToStr(PDF_PixelsToPoints(APage.Height)) + ' ]');
  FContentsObjAt := ReservObj;
  Writeln('/Contents ' + PDF_IndirectObjStr(FContentsObjAt));
  EndObj;

  FLengthObjAt := ReservObj;
  BeginObj(FContentsObjAt);
  Writeln('/Length ' + PDF_IndirectObjStr(FLengthObjAt));
  FPageOffset := BeginStream;
end;

procedure TRLPDFFilter.EndPage;
var
  stmsize: Integer;
begin
  stmsize := EndStream - FPageOffset;

  BeginShortObj(FLengthObjAt);
  Writeln(IntToStr(stmsize));
  EndShortObj;
end;

function TRLPDFFilter.BeginStream: Integer;
begin
  Writeln('>>');
  Writeln('stream');
  // marca o offset de início deste stream
  Result := FWritePos;
end;

function TRLPDFFilter.EndStream: Integer;
begin
  // calcula o tamanho da stream em bytes
  Result := FWritePos;
  Writeln('endstream');
  Writeln('endobj');
end;

procedure TRLPDFFilter.WriteText(ALeft, ATop: Double; const AText: string;
  AFontId, AFontSize: Integer);
var
  FontSize: Integer;
begin
  // begin text
  Writeln('BT');
  // font number and size
  FontSize := AFontSize;
  if AFontSize < 0 then //Resolver problema com font size negativo, causando texto invertido
    FontSize := (AFontSize * (-1)) - 2;
  Writeln('/F' + IntToStr(AFontId) + ' ' + IntToStr(FontSize) + ' Tf');
  // position
  Writeln(PDF_FloatToStr(ALeft) + ' ' + PDF_FloatToStr(ATop) + ' Td');
  // sub/superscript
  //  +-9 Ts
  // the text
  Writeln('(' + PDF_EncodeText(AText) + ') Tj');
  // end text
  Writeln('ET');
end;

procedure TRLPDFFilter.WriteBitmapData(ABitmap: TBitmap);
var
  X, Y: Integer;
  wid: Integer;
  rgb: TRLMetaColor;
  hex: string;
begin
  wid := 0;
  for Y := 0 to ABitmap.Height - 1 do
    for X := 0 to ABitmap.Width - 1 do
    begin
      rgb := ToMetaColor(CanvasGetPixels(ABitmap.Canvas, X, Y));
      hex := ByteToHex(rgb.Red) + ByteToHex(rgb.Green) + ByteToHex(rgb.Blue);
      Write(hex);
      Inc(wid, Length(hex));
      if wid >= 80 then
      begin
        Writeln;
        wid := 0;
      end;
    end;
end;

procedure TRLPDFFilter.WriteJpegData(AJpeg: TJpegImage);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    AJpeg.SaveToStream(S);
    FOutputStream.Write(S.Memory^, S.Size);
    Inc(FWritePos, S.Size);
  finally
    S.Free;
  end;
end;

// retorna o id do objeto criado
function TRLPDFFilter.WriteBitmap(ABitmap: TBitmap): Integer;
var
  begstm: Integer;
  endstm: Integer;
  lenat: Integer;
begin
  lenat := ReservObj;
  Inc(FImageCount);
  GrowCapacity(FImages, FImageCapacity, FImageCount, SizeOf(TRLPDFFilterImageType), 64);
  FImages[FImageCount - 1].ImageObjAt := BeginObj;
  Result := FImageCount;
  Writeln('/Type/XObject');
  Writeln('/Subtype/Image');
  Writeln('/Width ' + IntToStr(ABitmap.Width));
  Writeln('/Height ' + IntToStr(ABitmap.Height));
  Writeln('/ColorSpace/DeviceRGB');
  Writeln('/BitsPerComponent 8');
  Writeln('/Length ' + PDF_IndirectObjStr(lenat));
  Writeln('/Filter [/ASCIIHexDecode]');
  begstm := BeginStream;
  WriteBitmapData(ABitmap);
  Writeln('>');
  endstm := EndStream;
  BeginShortObj(lenat);
  Writeln(IntToStr(endstm - begstm));
  EndShortObj;
end;

function TRLPDFFilter.WriteJpeg(AJpeg: TJpegImage; InternalDraw: Boolean): Integer;
var
  begstm: Integer;
  endstm: Integer;
  lenat: Integer;
begin
  lenat := ReservObj;
  Inc(FImageCount);
  GrowCapacity(FImages, FImageCapacity, FImageCount, SizeOf(TRLPDFFilterImageType), 64);
  FImages[FImageCount - 1].ImageObjAt := BeginObj;
  Result := FImageCount;
  Writeln('/Type/XObject/Subtype/Image');
  Writeln('/Width ' + IntToStr(AJpeg.Width));
  Writeln('/Height ' + IntToStr(AJpeg.Height));
  if AJpeg.Grayscale then
    Writeln('/ColorSpace/DeviceGray')
  else
    Writeln('/ColorSpace/DeviceRGB');

  if InternalDraw or (AJpeg.PixelFormat = {$IfDef FPC}pf8bit{$Else}jf8Bit{$EndIf}) then
    Writeln('/BitsPerComponent 8')
  else
    Writeln('/BitsPerComponent 24');
  Writeln('/Length ' + PDF_IndirectObjStr(lenat));
  Writeln('/Filter/DCTDecode');
  begstm := BeginStream;
  WriteJpegData(AJpeg);
  Writeln('>');
  endstm := EndStream;
  BeginShortObj(lenat);
  Writeln(IntToStr(endstm - begstm));
  EndShortObj;
end;

procedure TRLPDFFilter.FilterPages(APages: TRLGraphicStorage;
  AOutputBuffer: TStream);
begin
  FUsingExternalOutputBuffer := True;
  FOutputStream := AOutputBuffer;
  try
    FilterPages(APages);
  finally
    FUsingExternalOutputBuffer := False;
    FOutputStream := nil;
  end;
end;

procedure TRLPDFFilter.FixupPageSetup;
var
  column: TRLPDFFilterColumnType;
  xp, yp, xw, yw, C, R: Word;
begin
  with FPageSetup do
  begin
    // <page> calculate "tl" (text leading) in point size from "tf" (text font)
    if FontPointSize <= 0 then
      FontPointSize := 10;
    LeadingPointSize := PDF_CalcTextLeadingPointSize(FontPointSize);
    // <column> calculate "tl" (text leading) in point size from "tf" (text font)
    if ColumnFontPointSize <= 0 then
      ColumnFontPointSize := 10;
    ColumnLeadingPointSize := PDF_CalcTextLeadingPointSize(ColumnFontPointSize);
    // calculate actual height and width for "landscape"
    if LandScape then
    begin
      FMediaSize.Height := PaperSize.Width;
      FMediaSize.Width := PaperSize.Height;
    end
    else
    begin
      FMediaSize.Height := PaperSize.Height;
      FMediaSize.Width := PaperSize.Width;
    end;
    // calculate "page workarea" - absolute address location in point size
    with FWorkArea do
    begin
      Top := Margins.Bottom;
      if Length(FPageTitle) >= 1 then
        Inc(Top, Round(LeadingPointSize * 1.5));
      Bottom := MediaSize.Height - Margins.Top;
      Left := Margins.Left;
      Right := MediaSize.Width - Margins.Right;
      Height := Bottom - Top + 1; // runtime calculation here
      Width := Right - Left + 1; // runtime calculation here
    end;
    // calculate "characters per line"
    CharCount := Round(WorkArea.Width * 11.5) div PDF_POINTSPERINCH;
    // calculate "column workarea" - absolute address location in point size
    if ColumnCount <= 0 then
      ColumnCount := 1;
    if ColumnCount > PDF_MAXCOLUMNS then
      ColumnCount := PDF_MAXCOLUMNS;
    if RowCount <= 0 then
      RowCount := 1;
    if RowCount > PDF_MAXROWS then
      RowCount := PDF_MAXROWS;
    if (ColumnCount = 1) and (RowCount = 1) then
    begin
      FColumns[1, 1].WorkArea := WorkArea;
      FColumns[1, 1].CharCount := CharCount;
    end
    else
    begin
      if ColumnCount > 1 then
        xw := (WorkArea.Width div ColumnCount) - (ColumnMargin.Left +
          ColumnMargin.Right + ColumnGap.X)
      else
        xw := WorkArea.Width;
      if RowCount > 1 then
        yw := (WorkArea.Height div RowCount) - (ColumnMargin.Top +
          ColumnMargin.Bottom + ColumnGap.Y)
      else
        yw := WorkArea.Height;
      xp := 0;
      yp := 0;
      C := 1;
      R := 1;
      while (C <= ColumnCount) and (R <= RowCount) do
      begin
        if C = 1 then
        begin
          xp := WorkArea.Left;
          if ColumnCount > 1 then
            Inc(xp, ColumnMargin.Left);
        end;
        if R = 1 then
        begin
          yp := WorkArea.Top;
          if RowCount > 1 then
            Inc(yp, ColumnMargin.Bottom);
        end;
        FColumns[C, R].WorkArea.Top := yp;
        FColumns[C, R].WorkArea.Bottom := yp + yw;
        FColumns[C, R].WorkArea.Left := xp;
        FColumns[C, R].WorkArea.Right := xp + xw;
        FColumns[C, R].WorkArea.Width :=
          FColumns[C, R].WorkArea.Right - FColumns[C, R].WorkArea.Left + 1;
        FColumns[C, R].WorkArea.Height :=
          FColumns[C, R].WorkArea.Bottom - FColumns[C, R].WorkArea.Top + 1;
        FColumns[C, R].CharCount :=
          Round(FColumns[C, R].WorkArea.Width * 11.5) div PDF_POINTSPERINCH;

        Inc(C);
        if C > ColumnCount then
        begin
          Inc(R);
          if not (R > RowCount) then
          begin
            C := 1;
            Inc(yp, yw + ColumnMargin.Bottom + ColumnGap.Y + ColumnMargin.Top);
          end;
        end
        else
          Inc(xp, xw + ColumnMargin.Right + ColumnGap.X + ColumnMargin.Left);
      end;
      // pdf always drawing from bottom-up
      yp := RowCount div 2;
      R := 0;
      while (R < yp) do
      begin
        Inc(R);
        C := 0;
        while C < ColumnCount do
        begin
          Inc(C);
          column := FColumns[C, R];
          FColumns[C, R] := FColumns[C, RowCount - R + 1];
          FColumns[C, RowCount - R + 1] := column;
        end;
      end;
    end;
  end;
end;

procedure TRLPDFFilter.Reset;
begin
  FPageCount := 0;
  FObjectCount := 0;
  FWritePos := 0;
  FFontColor := clBlack;
  FOldFontColor := 0;

  with DocumentInfo do
  begin
    Title := DefaultDocumentInfo.Title;
    Subject := DefaultDocumentInfo.Subject;
    Author := DefaultDocumentInfo.Author;
    Keywords := DefaultDocumentInfo.Keywords;
    Creator := DefaultDocumentInfo.Creator;
    Producer := DefaultDocumentInfo.Producer;
  end;

  with TextControl do
  begin
    FormFeed := DefaultTextControl.FormFeed;
    WordWrap := DefaultTextControl.WordWrap;
    TabSize := DefaultTextControl.TabSize;
  end;
end;

class function TRLPDFFilter.PDF_PixelsToPoints(APixels: Double): Double;
var
  inches: Double;
begin
  inches := APixels / ScreenPPI;
  Result := inches * PDF_POINTSPERINCH;
end;

class function TRLPDFFilter.PDF_Zeros(AValue, AWidth: Integer): AnsiString;
begin
  Result := IntToStr(AValue);
  while Length(Result) < AWidth do
    Result := '0' + Result;
end;

class function TRLPDFFilter.PDF_FloatToStr(AFloat: Double): string;
begin
  Str(AFloat: 0: 4, Result);
  while (Result <> '') and (Result[Length(Result)] = '0') do
    Delete(Result, Length(Result), 1);
  if (Result <> '') and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

class function TRLPDFFilter.PDF_CurrentDateTimeStr: string;
var
  Y, M, D, H, N, S, L: Word;
  dt: TDateTime;
begin
  dt := Now;
  DecodeDate(dt, Y, M, D);
  DecodeTime(dt, H, N, S, L);
  Result := 'D:' + PDF_Zeros(Y, 4) + PDF_Zeros(M, 2) + PDF_Zeros(D, 2) +
    PDF_Zeros(H, 2) + PDF_Zeros(N, 2) + PDF_Zeros(S, 2);
end;

// String de referência ao objeto aIndex
class function TRLPDFFilter.PDF_IndirectObjStr(AIndex: Integer): AnsiString;
begin
  Result := IntToStr(AIndex) + ' 0 R';
end;

class function TRLPDFFilter.PDF_GetDashPattern(ADashPattern:
  TRLPDFFilterDashPatternType): string;
begin
  case ADashPattern of
    dpSolid: Result := '[] 0 d';
    dp33: Result := '[3] 0 d';
    dp1222: Result := '[2] 0 d';
    dp2121: Result := '[2 1] 0 d';
    dp23535: Result := '[3 5] 6 d';
    dp13232: Result := '[2 3] 11 d';
  else
    // dpRegular
    Result := '';
  end;
end;

class function TRLPDFFilter.PDF_CalcTextLeadingPointSize(AFontSize: Word): Word;
begin
  if AFontSize <= 0 then
    AFontSize := 10;
  Result := Round((AFontSize * 14.5) / 12);
end;

class function TRLPDFFilter.PDF_EncodeText(const AText: string): string;
var
  I: Integer;
begin
  Result := GetAnsiStr(AText);

  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], ['(', ')', '\']) then
    begin
      case Result[I] of
        #08: Result[I] := 'b';
        #10: Result[I] := 'n';
        #12: Result[I] := 'f';
        #13: Result[I] := 'r';
      end;
      Insert('\', Result, I);
    end
    else if not CharInSet(Result[I], [#32..#126, #128..#255]) then
    begin
      Insert(IntToOctal(Ord(Result[I]), 3), Result, I + 1);
      Result[I] := '\';
    end;
end;

initialization

  DefaultTextControl := TRLPDFFilterTextControl.Create;
  with DefaultTextControl do
  begin
    FormFeed := True;
    WordWrap := False;
    TabSize := 8;
  end;

  DefaultDocumentInfo := TRLPDFFilterDocumentInfo.Create;
  with DefaultDocumentInfo do
  begin
    Title := '';
    Subject := '';
    Author := '';
    KeyWords := '';
    Creator := CS_ProductTitleStr + ' v' + CS_Version + ' \251 ' + CS_CopyrightStr;
    Producer := '';
  end;

finalization
  DefaultTextControl.Free;
  DefaultDocumentInfo.Free;

end.
