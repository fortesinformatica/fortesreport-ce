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

unit RLMetaCLX;

interface

uses
{$IFDEF LINUX}
  xlib, QConsts, 
{$ELSE}
  Windows, SysUtils, 
{$endif}
  Types, QGraphics, Qt, Classes, Math, QStdCtrls, 
  RLMetaFile, RLUtils, RLConsts;

type
  TPointArray = array of TPoint;
  
function ToMetaRect(const ASource: TRect): TRLMetaRect;
function ToMetaColor(ASource: TColor): TRLMetaColor;
function ToMetaPenMode(ASource: TPenMode): TRLMetaPenMode;
function ToMetaPenStyle(ASource: TPenStyle): TRLMetaPenStyle;
procedure ToMetaPen(ASource: TPen; ADest: TRLMetaPen);
function ToMetaBrushStyle(ASource: TBrushStyle): TRLMetaBrushStyle;
procedure ToMetaBrush(ASource: TBrush; ADest: TRLMetaBrush);
function ToMetaPoint(const ASource: TPoint): TRLMetaPoint;
function ToMetaPointArray(const ASource: array of TPoint): TRLMetaPointArray;
function ToMetaFontCharset(ASource: TFontCharset): TRLMetaFontCharset;
function ToMetaFontPitch(ASource: TFontPitch): TRLMetaFontPitch;
function ToMetaFontStyles(ASource: TFontStyles): TRLMetaFontStyles;
procedure ToMetaFont(ASource: TFont; ADest: TRLMetaFont);
function ToMetaGraphic(ASource: TGraphic): String;
function ToMetaTextAlignment(ASource: TAlignment): TRLMetaTextAlignment;
function ToMetaTextLayout(ASource: TTextLayout): TRLMetaTextLayout; 
function FromMetaRect(const ASource: TRLMetaRect): TRect;
function FromMetaPoint(const ASource: TRLMetaPoint): TPoint;
function FromMetaColor(const ASource: TRLMetaColor): TColor;
function FromMetaPenMode(ASource: TRLMetaPenMode): TPenMode;
function FromMetaPenStyle(ASource: TRLMetaPenStyle): TPenStyle;
procedure FromMetaPen(ASource: TRLMetaPen; ADest: TPen);
function FromMetaBrushStyle(ASource: TRLMetaBrushStyle): TBrushStyle;
procedure FromMetaBrush(ASource: TRLMetaBrush; ADest: TBrush);
function FromMetaFontStyles(ASource: TRLMetaFontStyles): TFontStyles;
function FromMetaFontCharset(ASource: TRLMetaFontCharset): TFontCharset;
function FromMetaFontPitch(ASource: TRLMetaFontPitch): TFontPitch;
procedure FromMetaFont(ASource: TRLMetaFont; ADest: TFont; AFactor: Double = 1);
function FromMetaGraphic(const ASource: String): TGraphic;
function FromMetaPointArray(const ASource: TRLMetaPointArray): TPointArray;
function FromMetaTextAlignment(ASource: TRLMetaTextAlignment): TAlignment;
function FromMetaTextLayout(ASource: TRLMetaTextLayout): TTextLayout;
//
procedure PenInflate(APen: TPen; AFactor: Double);
procedure CanvasStart(ACanvas: TCanvas);
procedure CanvasStop(ACanvas: TCanvas);
function CanvasGetClipRect(ACanvas: TCanvas): TRect;
procedure CanvasSetClipRect(ACanvas: TCanvas; const ARect: TRect);
procedure CanvasResetClipRect(ACanvas: TCanvas);
function CanvasGetRectData(ACanvas: TCanvas; const ARect: TRect): String;
procedure CanvasSetRectData(ACanvas: TCanvas; const ARect: TRect; const AData: String; AParity: Boolean);
procedure CanvasStretchDraw(ACanvas: TCanvas; const ARect: TRect; const AData: String; AParity: Boolean);
procedure CanvasTextRectEx(ACanvas: TCanvas; const ARect: TRect; AX, AY: Integer; const AText: String; AAlignment: TRLMetaTextAlignment; ALayout: TRLMetaTextLayout; ATextFlags: TRLMetaTextFlags);
function CanvasGetPixels(ACanvas: TCanvas; X, Y: Integer): TColor;
procedure CanvasLineToEx(ACanvas: TCanvas; X, Y: Integer);
procedure FontGetMetrics(const AFontName: String; AFontStyles: TFontStyles; var AFontRec: TRLMetaFontMetrics);

implementation

{ CONVERSION }

function ToMetaRect(const ASource: TRect): TRLMetaRect;
begin
  Result.Left := ASource.Left;
  Result.Top := ASource.Top;
  Result.Right := ASource.Right;
  Result.Bottom := ASource.Bottom;
end;

function ToMetaColor(ASource: TColor): TRLMetaColor;
var
  rgb: Cardinal;
begin
  rgb := ColorToRGB(ASource);
  Result.Red := Byte(rgb);
  Result.Green := Byte(rgb shr 8);
  Result.Blue := Byte(rgb shr 16);
end;

function ToMetaPenMode(ASource: TPenMode): TRLMetaPenMode;
begin
  case ASource of
    pmBlack: Result := MetaPenModeBlack;
    pmWhite: Result := MetaPenModeWhite;
    pmNop: Result := MetaPenModeNop;
    pmNot: Result := MetaPenModeNot;
    pmCopy: Result := MetaPenModeCopy;
    pmNotCopy: Result := MetaPenModeNotCopy;
    pmMergePenNot: Result := MetaPenModeMergePenNot;
    pmMaskPenNot: Result := MetaPenModeMaskPenNot;
    pmMergeNotPen: Result := MetaPenModeMergeNotPen;
    pmMaskNotPen: Result := MetaPenModeMaskNotPen;
    pmMerge: Result := MetaPenModeMerge;
    pmNotMerge: Result := MetaPenModeNotMerge;
    pmMask: Result := MetaPenModeMask;
    pmNotMask: Result := MetaPenModeNotMask;
    pmXor: Result := MetaPenModeXor;
    pmNotXor: Result := MetaPenModeNotXor;
  else
    Result := MetaPenModeCopy;
  end;
end;

function ToMetaPenStyle(ASource: TPenStyle): TRLMetaPenStyle;
begin
  case ASource of
    psSolid: Result := MetaPenStyleSolid;
    psDash: Result := MetaPenStyleDash;
    psDot: Result := MetaPenStyleDot;
    psDashDot: Result := MetaPenStyleDashDot;
    psDashDotDot: Result := MetaPenStyleDashDotDot;
    psClear: Result := MetaPenStyleClear;
    //psInsideFrame: result:=MetaPenStyleInsideFrame;
  else
    Result := MetaPenStyleSolid;
  end;
end;

procedure ToMetaPen(ASource: TPen; ADest: TRLMetaPen);
begin
  ADest.Color := ToMetaColor(ASource.Color);
  ADest.Mode := ToMetaPenMode(ASource.Mode);
  ADest.Style := ToMetaPenStyle(ASource.Style);
  ADest.Width := ASource.Width;
end;

function ToMetaBrushStyle(ASource: TBrushStyle): TRLMetaBrushStyle;
begin
  case ASource of
    bsSolid: Result := MetaBrushStyleSolid;
    bsClear: Result := MetaBrushStyleClear;
    bsHorizontal: Result := MetaBrushStyleHorizontal;
    bsVertical: Result := MetaBrushStyleVertical;
    bsFDiagonal: Result := MetaBrushStyleFDiagonal;
    bsBDiagonal: Result := MetaBrushStyleBDiagonal;
    bsCross: Result := MetaBrushStyleCross;
    bsDiagCross: Result := MetaBrushStyleDiagCross;
  else
    Result := MetaBrushStyleSolid;
  end;
end;

procedure ToMetaBrush(ASource: TBrush; ADest: TRLMetaBrush);
begin
  ADest.Color := ToMetaColor(ASource.Color);
  ADest.Style := ToMetaBrushStyle(ASource.Style);
end;

function ToMetaPoint(const ASource: TPoint): TRLMetaPoint;
begin
  Result.X := ASource.X;
  Result.Y := ASource.Y;
end;

function ToMetaPointArray(const ASource: array of TPoint): TRLMetaPointArray;
var
  I: Integer;
begin
  SetLength(Result, High(ASource) + 1);
  for I := 0 to High(ASource) do
    Result[I] := ToMetaPoint(ASource[I]);
end;

function ToMetaFontCharset(ASource: TFontCharset): TRLMetaFontCharset;
begin
  Result := TRLMetaFontCharset(ASource);
end;

function ToMetaFontPitch(ASource: TFontPitch): TRLMetaFontPitch;
begin
  case ASource of
    fpDefault: Result := MetaFontPitchDefault;
    fpVariable: Result := MetaFontPitchVariable;
    fpFixed: Result := MetaFontPitchFixed;
  else
    Result := MetaFontPitchDefault;
  end;
end;

function ToMetaFontStyles(ASource: TFontStyles): TRLMetaFontStyles;
begin
  Result := 0;
  if fsBold in ASource then
    Result := Result or MetaFontStyleBold;
  if fsItalic in ASource then
    Result := Result or MetaFontStyleItalic;
  if fsUnderline in ASource then
    Result := Result or MetaFontStyleUnderline;
  if fsStrikeOut in ASource then
    Result := Result or MetaFontStyleStrikeOut;
end;

procedure ToMetaFont(ASource: TFont; ADest: TRLMetaFont);
begin
  ADest.PixelsPerInch := ASource.PixelsPerInch;
  ADest.Charset := ToMetaFontCharset(ASource.Charset);
  ADest.Color := ToMetaColor(ASource.Color);
  ADest.Height := ASource.Height;
  ADest.Name := ASource.Name;
  ADest.Pitch := ToMetaFontPitch(ASource.Pitch);
  ADest.Size := ASource.Size;
  ADest.Style := ToMetaFontStyles(ASource.Style);
end;

function ToMetaGraphic(ASource: TGraphic): String;
var
  S: TStringStream;
  M: TBitmap;
  G: TGraphic;
begin
  M := nil;
  S := TStringStream.Create('');
  try
    G := ASource;
    // identifica os tipos nativos
    if G = nil then
      S.WriteString('NIL')
    else if G is TBitmap then
      S.WriteString('BMP')
    else if G is TIcon then
      S.WriteString('ICO')
    else
    begin
      // qualquer outro formato é transformado em bmp para ficar compatível com um carregador de qualquer plataforma
      M := TRLBitmap.Create;
      M.Width := ASource.Width;
      M.Height := ASource.Height;
      G := M;
      M.Canvas.Draw(0, 0, ASource);
      S.WriteString('BMP');
    end;
    if Assigned(G) then
      G.SaveToStream(S);
    Result := S.DataString;
  finally
    if Assigned(M) then
      M.free;
    S.free;
  end;
end;

function ToMetaTextAlignment(ASource: TAlignment): TRLMetaTextAlignment;
begin
  case ASource of
    taLeftJustify: Result := MetaTextAlignmentLeft;
    taRightJustify: Result := MetaTextAlignmentRight;
    taCenter: Result := MetaTextAlignmentCenter;
  else
    if ASource = succ(taCenter) then
      Result := MetaTextAlignmentJustify
    else
      Result := MetaTextAlignmentLeft;
  end;
end;

function ToMetaTextLayout(ASource: TTextLayout): TRLMetaTextLayout;
begin
  case ASource of
    tlTop: Result := MetaTextLayoutTop;
    tlBottom: Result := MetaTextLayoutBottom;
    tlCenter: Result := MetaTextLayoutCenter;
  else
    if ASource = succ(tlCenter) then
      Result := MetaTextLayoutJustify
    else
      Result := MetaTextLayoutTop;
  end;
end;

function FromMetaRect(const ASource: TRLMetaRect): TRect;
begin
  Result.Left := ASource.Left;
  Result.Top := ASource.Top;
  Result.Right := ASource.Right;
  Result.Bottom := ASource.Bottom;
end;

function FromMetaPoint(const ASource: TRLMetaPoint): TPoint;
begin
  Result.X := ASource.X;
  Result.Y := ASource.Y;
end;

function FromMetaColor(const ASource: TRLMetaColor): TColor;
begin
  Result := RGB(ASource.Red, ASource.Green, ASource.Blue);
end;

function FromMetaPenMode(ASource: TRLMetaPenMode): TPenMode;
begin
  case ASource of
    MetaPenModeBlack: Result := pmBlack;
    MetaPenModeWhite: Result := pmWhite;
    MetaPenModeNop: Result := pmNop;
    MetaPenModeNot: Result := pmNot;
    MetaPenModeCopy: Result := pmCopy;
    MetaPenModeNotCopy: Result := pmNotCopy;
    MetaPenModeMergePenNot: Result := pmMergePenNot;
    MetaPenModeMaskPenNot: Result := pmMaskPenNot;
    MetaPenModeMergeNotPen: Result := pmMergeNotPen;
    MetaPenModeMaskNotPen: Result := pmMaskNotPen;
    MetaPenModeMerge: Result := pmMerge;
    MetaPenModeNotMerge: Result := pmNotMerge;
    MetaPenModeMask: Result := pmMask;
    MetaPenModeNotMask: Result := pmNotMask;
    MetaPenModeXor: Result := pmXor;
    MetaPenModeNotXor: Result := pmNotXor;
  else
    Result := pmCopy; 
  end;
end;

function FromMetaPenStyle(ASource: TRLMetaPenStyle): TPenStyle;
begin
  case ASource of
    MetaPenStyleSolid: Result := psSolid;
    MetaPenStyleDash: Result := psDash;
    MetaPenStyleDot: Result := psDot;
    MetaPenStyleDashDot: Result := psDashDot;
    MetaPenStyleDashDotDot: Result := psDashDotDot;
    MetaPenStyleClear: Result := psClear;
    //MetaPenStyleInsideFrame: result:=psInsideFrame;
  else
    Result := psSolid;
  end;
end;

procedure FromMetaPen(ASource: TRLMetaPen; ADest: TPen);
begin
  ADest.Color := FromMetaColor(ASource.Color);
  ADest.Mode := FromMetaPenMode(ASource.Mode);
  ADest.Style := FromMetaPenStyle(ASource.Style);
  ADest.Width := ASource.Width;
end;

function FromMetaBrushStyle(ASource: TRLMetaBrushStyle): TBrushStyle;
begin
  case ASource of
    MetaBrushStyleSolid: Result := bsSolid;
    MetaBrushStyleClear: Result := bsClear;
    MetaBrushStyleHorizontal: Result := bsHorizontal;
    MetaBrushStyleVertical: Result := bsVertical;
    MetaBrushStyleFDiagonal: Result := bsFDiagonal;
    MetaBrushStyleBDiagonal: Result := bsBDiagonal;
    MetaBrushStyleCross: Result := bsCross;
    MetaBrushStyleDiagCross: Result := bsDiagCross;
  else
    Result := bsSolid;
  end;
end;

procedure FromMetaBrush(ASource: TRLMetaBrush; ADest: TBrush);
begin
  ADest.Color := FromMetaColor(ASource.Color);
  ADest.Style := FromMetaBrushStyle(ASource.Style);
end;

function FromMetaFontStyles(ASource: TRLMetaFontStyles): TFontStyles;
begin
  Result := [];
  if (MetaFontStyleBold and ASource) = MetaFontStyleBold then
    Include(Result, fsBold);
  if (MetaFontStyleItalic and ASource) = MetaFontStyleItalic then
    Include(Result, fsItalic);
  if (MetaFontStyleUnderline and ASource) = MetaFontStyleUnderline then
    Include(Result, fsUnderline);
  if (MetaFontStyleStrikeOut and ASource) = MetaFontStyleStrikeOut then
    Include(Result, fsStrikeOut);
end;

function FromMetaFontCharset(ASource: TRLMetaFontCharset): TFontCharset;
begin
  Result := TFontCharset(ASource);
end;

function FromMetaFontPitch(ASource: TRLMetaFontPitch): TFontPitch;
begin
  case ASource of
    MetaFontPitchDefault: Result := fpDefault;
    MetaFontPitchVariable: Result := fpVariable;
    MetaFontPitchFixed: Result := fpFixed;
  else
    Result := fpDefault;
  end;
end;

procedure FromMetaFont(ASource: TRLMetaFont; ADest: TFont; AFactor: Double = 1);
var
  A, B: Integer;
begin
  A := ASource.PixelsPerInch;
  if A = 0 then
    A := ScreenPPI;
  B := ADest.PixelsPerInch;
  if B = 0 then
    B := ScreenPPI;
  //  
  //aDest.PixelsPerInch:=aSource.PixelsPerInch;
  ADest.Charset := FromMetaFontCharset(ASource.Charset);
  ADest.Color := FromMetaColor(ASource.Color);
  //aDest.Height       :=aSource.Height;
  ADest.Name := ASource.Name;
  ADest.Pitch := FromMetaFontPitch(ASource.Pitch);
  ADest.Size := Round(ASource.Size * AFactor * A / B);
  ADest.Style := FromMetaFontStyles(ASource.Style);
end;

function FromMetaGraphic(const ASource: String): TGraphic;
var
  S: TStringStream;
  T: String;
begin
  if ASource = '' then
    Result := nil
  else
  begin
    S := TStringStream.Create(ASource);
    try
      S.Seek(0, soFromBeginning);
      T := S.ReadString(3);
      if T = 'NIL' then
        Result := nil
      else if T = 'BMP' then
        Result := TRLBitmap.Create
      else if T = 'ICO' then
        Result := TIcon.Create
      else
        Result := nil;
      if Assigned(Result) then
        Result.LoadFromStream(S);
    finally
      S.free;
    end;
  end; 
end;

function FromMetaPointArray(const ASource: TRLMetaPointArray): TPointArray;
var
  I: Integer;
begin
  SetLength(Result, High(ASource) + 1);
  for I := 0 to High(ASource) do
    Result[I] := FromMetaPoint(ASource[I]);
end;

function FromMetaTextAlignment(ASource: TRLMetaTextAlignment): TAlignment;
begin
  case ASource of
    MetaTextAlignmentLeft: Result := taLeftJustify;
    MetaTextAlignmentRight: Result := taRightJustify;
    MetaTextAlignmentCenter: Result := taCenter;
    MetaTextAlignmentJustify: Result := succ(taCenter);
  else
    Result := taLeftJustify;
  end;
end;

function FromMetaTextLayout(ASource: TRLMetaTextLayout): TTextLayout;
begin
  case ASource of
    MetaTextLayoutTop: Result := tlTop;
    MetaTextLayoutBottom: Result := tlBottom;
    MetaTextLayoutCenter: Result := tlCenter;
    MetaTextLayoutJustify: Result := succ(tlCenter);
  else
    Result := tlTop;
  end;
end;

{ MISC }

procedure PenInflate(APen: TPen; AFactor: Double);
begin
  if APen.Width <> 0 then
    APen.Width := Max(1, Round(APen.Width * AFactor));
end;

procedure CanvasStart(ACanvas: TCanvas);
begin
  ACanvas.Start;
end;

procedure CanvasStop(ACanvas: TCanvas);
begin
  ACanvas.Stop;
end;

function CanvasGetClipRect(ACanvas: TCanvas): TRect;
begin
  Result := ACanvas.ClipRect;
end;

procedure CanvasSetClipRect(ACanvas: TCanvas; const ARect: TRect);
var
  isnull: Boolean;
begin
  isnull := ((ARect.Right - ARect.Left) = 0) or ((ARect.Bottom - ARect.Top) = 0);
  if isnull then
    ACanvas.ResetClipRegion
  else
    ACanvas.SetClipRect(ARect);
end;

procedure CanvasResetClipRect(ACanvas: TCanvas);
begin
  ACanvas.ResetClipRegion;
end;

function CanvasGetRectData(ACanvas: TCanvas; const ARect: TRect): String;
var
  graphic: TBitmap;
begin
  graphic := TRLBitmap.Create;
  with graphic do
    try
      Width := ARect.Right - ARect.Left;
      Height := ARect.Bottom - ARect.Top;
      PixelFormat := pf32bit;
      Canvas.CopyRect(Rect(0, 0, Width, Height), ACanvas, ARect);
      Result := ToMetaGraphic(graphic);
    finally
      graphic.free;
    end;
end;

procedure CanvasSetRectData(ACanvas: TCanvas; const ARect: TRect; const AData: String; AParity: Boolean);
var
  graphic: TGraphic;
  auxrect: TRect;
  aux: Integer;
begin
  graphic := FromMetaGraphic(AData);
  if graphic <> nil then
    try
      auxrect := ARect;
      if AParity then
      begin
        aux := (auxrect.Right - auxrect.Left) div graphic.Width;
        auxrect.Right := auxrect.Left + aux * graphic.Width + 1;
      end;
      ACanvas.StretchDraw(auxrect, graphic);
    finally
      graphic.free;
    end;
end;

procedure CanvasStretchDraw(ACanvas: TCanvas; const ARect: TRect; const AData: String; AParity: Boolean);
begin
  CanvasSetRectData(ACanvas, ARect, AData, AParity);
end;

procedure CanvasTextRectEx(ACanvas: TCanvas; const ARect: TRect; AX, AY: Integer; const AText: String; AAlignment: TRLMetaTextAlignment; ALayout: TRLMetaTextLayout; ATextFlags: TRLMetaTextFlags);
var
  delta, left, top, txtw, txth, wid, I: Integer;
  buff: String;
begin
  buff := AText;
  delta := ACanvas.TextWidth(' ') div 2;
  txtw := ACanvas.TextWidth(buff + ' ');
  txth := ACanvas.TextHeight(buff + ' ');
  case AAlignment of
    MetaTextAlignmentCenter: left := (ARect.Left + ARect.Right - txtw) div 2;
    MetaTextAlignmentRight: left := ARect.Right - txtw + delta;
  else
    left := AX + delta;
  end;
  case ALayout of
    MetaTextLayoutCenter: top := (ARect.Top + ARect.Bottom - txth) div 2;
    MetaTextLayoutBottom: top := ARect.Bottom - txth;
  else
    top := AY;
  end;
  if AAlignment = MetaTextAlignmentJustify then
  begin
    wid := ARect.Right - left;
    I := Length(buff);
    while (ACanvas.TextWidth(buff + #32) <= wid) and IterateJustification(buff, I) do;
  end;
  if (ATextFlags and MetaTextFlagAutoSize) = MetaTextFlagAutoSize then
    ACanvas.TextOut(left, top, buff)
  else
    ACanvas.TextRect(ARect, left, top, buff);
end;

function CanvasGetPixels(ACanvas: TCanvas; X, Y: Integer): TColor;
begin
  with TRLBitmap.Create do
    try
      Width := 1;
      Height := 1;
      PixelFormat := pf32bit;
      Canvas.CopyRect(Rect(0, 0, 1, 1), ACanvas, Rect(X, Y, X + 1, Y + 1));
      with TRGBArray(ScanLine[Y]^)[X] do
        Result := RGB(rgbRed, rgbGreen, rgbBlue);
    finally
      free;
    end;
end;
procedure CanvasLineToEx(ACanvas: TCanvas; X, Y: Integer);
type
  TPattern = record
    Count: Byte;
    Lengths: array[0..5] of Byte;
  end;
const
  Patterns: array[TPenStyle] of TPattern = ((Count: 0;Lengths: (0, 0, 0, 0, 0, 0)), // psSolid
                                         (Count: 2;Lengths: (3, 1, 0, 0, 0, 0)), // psDash
                                         (Count: 2;Lengths: (1, 1, 0, 0, 0, 0)), // psDot
                                         (Count: 4;Lengths: (2, 1, 1, 1, 0, 0)), // psDashDot
                                         (Count: 6;Lengths: (3, 1, 1, 1, 1, 1)), // psDashDotDot
                                         (Count: 0;Lengths: (0, 0, 0, 0, 0, 0)) // psClear
{$ifndef CLX}
, (Count: 0;Lengths: (0, 0, 0, 0, 0, 0))
{$endif}
); // psInsideFrame
var
  x0, y0: Integer;
  xb, yb: Integer;
  I, P, dist: Integer;
  theta: Double;
  sn, cs: Double;
  patt: ^TPattern;
  forecl: TColor;
  backcl: TColor;
  width0: Integer;
  style0: TPenStyle;
  factor: Integer;
  cli: Integer;
begin
  if (Patterns[ACanvas.Pen.Style].Count = 0) or (ACanvas.Pen.Width <= 1) then
    ACanvas.LineTo(X, Y)
  else
  begin
    style0 := ACanvas.Pen.Style;
    width0 := ACanvas.Pen.Width;
    x0 := ACanvas.PenPos.X;
    y0 := ACanvas.PenPos.Y;
    theta := ArcTan((Y - y0) / (X - x0));
    sn := Sin(theta);
    cs := Cos(theta);
    dist := Round(Sqrt(Sqr(X - x0) + Sqr(Y - y0)));
    patt := @Patterns[ACanvas.Pen.Style];
    P := 0;
    I := 0;
    forecl := ACanvas.Pen.Color;
    backcl := ACanvas.Brush.Color;
    factor := 4 * ACanvas.Pen.Width;
    ACanvas.Pen.Style := psSolid;
    if ACanvas.Brush.Style <> bsClear then
    begin
      ACanvas.Pen.Color := backcl;
      ACanvas.LineTo(X, Y);
    end;
    ACanvas.Pen.Color := forecl;
    ACanvas.MoveTo(x0, y0);
    cli := 0;
    while I < dist do
    begin
      Inc(I, patt^.Lengths[P] * factor);
      if not (I < dist) then
        I := dist;
      xb := x0 + Round(I * cs);
      yb := y0 + Round(I * sn);
      if cli = 0 then
        ACanvas.LineTo(xb, yb)
      else
        ACanvas.MoveTo(xb, yb);
      cli := 1 - cli;
      P := Succ(P) mod patt^.Count;
    end;
    ACanvas.Pen.Style := style0;
    ACanvas.Pen.Width := width0;
  end;
end;

procedure FontGetMetrics(const AFontName: String; AFontStyles: TFontStyles; var AFontRec: TRLMetaFontMetrics);
var
  bmp: TRLBitmap;
  I: Integer;
begin
  bmp := TRLBitmap.Create;
  try
    bmp.Width := 1;
    bmp.Height := 1;
    bmp.Canvas.Font.Name := AFontName;
    bmp.Canvas.Font.Style := AFontStyles;
    bmp.Canvas.Font.Size := 750;
    //
    AFontRec.TrueType := True;
    AFontRec.BaseFont := AFontName;
    AFontRec.FirstChar := 32;
    AFontRec.LastChar := 255;
    for I := AFontRec.FirstChar to AFontRec.LastChar do
      AFontRec.Widths[I] := bmp.Canvas.TextWidth(Chr(I));
    //
    AFontRec.FontDescriptor.Name := AFontName;
    AFontRec.FontDescriptor.Styles := '';
    if fsBold in AFontStyles then
      AFontRec.FontDescriptor.Styles := AFontRec.FontDescriptor.Styles + 'Bold';
    if fsItalic in AFontStyles then
      AFontRec.FontDescriptor.Styles := AFontRec.FontDescriptor.Styles + 'Italic';
    if fsUnderline in AFontStyles then
      AFontRec.FontDescriptor.Styles := AFontRec.FontDescriptor.Styles + 'Underline';
    if fsStrikeOut in AFontStyles then
      AFontRec.FontDescriptor.Styles := AFontRec.FontDescriptor.Styles + 'StrikeOut';
    AFontRec.FontDescriptor.Flags := 32;
    AFontRec.FontDescriptor.FontBBox := Rect( - 498, 1023, 1120, - 307);
    AFontRec.FontDescriptor.MissingWidth := 0;
    AFontRec.FontDescriptor.StemV := 0;
    AFontRec.FontDescriptor.StemH := 0;
    AFontRec.FontDescriptor.ItalicAngle := 0;
    AFontRec.FontDescriptor.CapHeight := 0;
    AFontRec.FontDescriptor.XHeight := 0;
    AFontRec.FontDescriptor.Ascent := 0;
    AFontRec.FontDescriptor.Descent := 0;
    AFontRec.FontDescriptor.Leading := 0;
    AFontRec.FontDescriptor.MaxWidth := 0;
    AFontRec.FontDescriptor.AvgWidth := 0;
  finally
    bmp.Free;
  end;
end;

end.

