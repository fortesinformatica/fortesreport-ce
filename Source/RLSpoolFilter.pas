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

{@unit RLSpoolFilter - Implementação do filtro padrão para impressoras. }
unit RLSpoolFilter;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils,
  {$IfDef CLX}
   QTypes, QGraphics, RLMetaCLX,
  {$else}
   Types, Graphics, RLMetaVCL,
  {$EndIf}
  {$IfDef FPC}
   LCLIntf, LCLType,
  {$EndIf}
  RLMetaFile, RLFilters, RLTypes, RLPrinters, RLConsts, RLUtils;

type

  { TRLSpoolFilter }

  {@class TRLSpoolFilter - Filtro de impressão padrão.
   É o filtro natural que envia as páginas para o spooler do SO.
   @links TRLDraftFilter.
   @pub }
  TRLSpoolFilter = class(TRLCustomPrintFilter)
  private

    // variables

    FPrinterRect: TRect;
    FDocStarted: Boolean;
    FPaperWidth: Double;
    FPaperHeight: Double;
    FOrientation: TRLMetaOrientation;
    
    procedure SetDocBounds(APaperWidth, APaperHeight: Double; AOrientation: TRLMetaOrientation);

  protected

    // override methods

    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalNewPage; override;
    procedure InternalDrawPage(APage: TRLGraphicSurface); override;

  public

    // constructors & destructors
    
    constructor Create(AOwner: TComponent); override;
  end;
  {/@class}

{@func SpoolFilter - Referência para o filtro padrão de impressora.
 @links TRLSpoolFilter. :/}
function SpoolFilter: TRLSpoolFilter;

{/@unit}

implementation

var
  SpoolFilterInstance: TRLSpoolFilter = nil;

function SpoolFilter: TRLSpoolFilter;
begin
  if not Assigned(SpoolFilterInstance) then
    SpoolFilterInstance := TRLSpoolFilter.Create(nil);
  Result := SpoolFilterInstance;
end;

{Fred - 02/03/2010 - Correção da impressão de bitmaps}
procedure SpecialStretchDraw(ADest: TCanvas; ARect: TRect; AGraphic: TGraphic);
begin
  ADest.StretchDraw(ARect,AGraphic);
end;

(*
{$ifndef LINUX}
procedure SpecialStretchDraw(ADest: TCanvas; ARect: TRect; AGraphic: TGraphic);
var
  BitmapDC: HDC;
  isDCPaletteDevice: Boolean;
  CompatibleDC: HDC;
  CompatibleBitmap: HBitmap;
  OldMemBitmap: HBitmap;
  hDibHeader: THandle;
  pDibHeader: pointer;
  hBits: THandle;
  pBits: pointer;
  pPalette: PLOGPALETTE;
  PaletteHandle: HPalette;
  OldPaletteHandle: HPalette;
  I: Integer;
  foo: TBitmap;
begin
  foo := TRLBitmap.Create;
  try
    foo.Width := AGraphic.Width;
    foo.Height := AGraphic.Height;
    foo.PixelFormat := pf32bit;
    foo.Canvas.Draw(0, 0, AGraphic);
    //
    pPalette := nil;
    PaletteHandle := 0;
    OldPaletteHandle := 0;
    // get the bitmap dc
    BitmapDC := foo.Canvas.Handle;
    // create a compatible dc
    CompatibleDC := CreateCompatibleDC(BitmapDC);
    // create a bitmap
    CompatibleBitmap := CreateCompatibleBitmap(BitmapDC, foo.Width, foo.Height);
    // select the bitmap into the dc
    OldMemBitmap := SelectObject(CompatibleDC, CompatibleBitmap);
    // lets prepare to try a fixup for broken video drivers
    isDCPaletteDevice := False;
    if GetDeviceCaps(BitmapDC, RASTERCAPS) and RC_PALETTE = RC_PALETTE then
    begin
      GetMem(pPalette, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
      FillChar(pPalette^, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)), 0);
      pPalette^.palVersion := $300;
      pPalette^.palNumEntries := GetSystemPaletteEntries(BitmapDC, 0, 256, pPalette^.palPalEntry);
      if pPalette^.PalNumEntries <> 0 then
      begin
        PaletteHandle := CreatePalette(pPalette^);
        OldPaletteHandle := SelectPalette(CompatibleDC, PaletteHandle, False);
        isDCPaletteDevice := True;
      end
      else
      begin
        FreeMem(pPalette);
        pPalette := nil;
      end;
    end;
    // copy from the bitmap to the CompatibleDC/bitmap
    BitBlt(CompatibleDC, 0, 0, foo.Width, foo.Height, foo.Canvas.Handle, 0, 0, SRCCOPY);
    //
    if isDCPaletteDevice then
    begin
      SelectPalette(CompatibleDC, OldPaletteHandle, False);
      DeleteObject(PaletteHandle);
    end;
    // unselect the bitmap
    SelectObject(CompatibleDC, OldMemBitmap);
    // delete the memory dc
    DeleteDC(CompatibleDC);
    // allocate memory for a DIB structure
    hDibHeader := GlobalAlloc(GHND, SizeOf(TBITMAPINFO) + (SizeOf(TRGBQUAD) * 256));
    // get a pointer to the alloced memory
    pDibHeader := GlobalLock(hDibHeader);
    // fill in the dib structure with info on the way we want the DIB
    FillChar(pDibHeader^, SizeOf(TBITMAPINFO) + (SizeOf(TRGBQUAD) * 256), 0);
    PBITMAPINFOHEADER(pDibHeader)^.biSize := SizeOf(TBITMAPINFOHEADER);
    PBITMAPINFOHEADER(pDibHeader)^.biPlanes := 1;
    PBITMAPINFOHEADER(pDibHeader)^.biBitCount := 8;
    PBITMAPINFOHEADER(pDibHeader)^.biWidth := foo.width;
    PBITMAPINFOHEADER(pDibHeader)^.biHeight := foo.height;
    PBITMAPINFOHEADER(pDibHeader)^.biCompression := BI_RGB;
    // find out how much memory for the bits
    GetDIBits(BitmapDC, CompatibleBitmap, 0, foo.Height, nil, TBitmapInfo(pDibHeader^), DIB_RGB_COLORS);
    // alloc memory for the bits
    hBits := GlobalAlloc(GHND, PBitmapInfoHeader(pDibHeader)^.BiSizeImage);
    // get a pointer to the bits
    pBits := GlobalLock(hBits);
    // call fn again, but this time give us the bits!
    GetDIBits(BitmapDC, CompatibleBitmap, 0, foo.height, pBits, PBitmapInfo(pDibHeader)^, DIB_RGB_COLORS);
    // lets try a fixup for broken video drivers
    if isDCPaletteDevice then
      if pPalette <> nil then
      begin
        for I := 0 to pPalette^.PalNumEntries - 1 do
        begin
          PBitmapInfo(pDibHeader)^.bmiColors[I].rgbRed := pPalette^.palPalEntry[I].peRed;
          PBitmapInfo(pDibHeader)^.bmiColors[I].rgbGreen := pPalette^.palPalEntry[I].peGreen;
          PBitmapInfo(pDibHeader)^.bmiColors[I].rgbBlue := pPalette^.palPalEntry[I].peBlue;
        end;
        FreeMem(pPalette);
      end;
    // delete the bitmap
    DeleteObject(CompatibleBitmap);
    // just incase the printer drver is a palette device
    isDCPaletteDevice := False;
    if GetDeviceCaps(ADest.Handle, RASTERCAPS) and RC_PALETTE = RC_PALETTE then
    begin
      // create palette from DIB
      GetMem(pPalette, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
      FillChar(pPalette^, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)), 0);
      pPalette^.palVersion := $300;
      pPalette^.palNumEntries := 256;
      for I := 0 to pPalette^.PalNumEntries - 1 do
      begin
        pPalette^.palPalEntry[I].peRed := PBitmapInfo(pDibHeader)^.bmiColors[I].rgbRed;
        pPalette^.palPalEntry[I].peGreen := PBitmapInfo(pDibHeader)^.bmiColors[I].rgbGreen;
        pPalette^.palPalEntry[I].peBlue := PBitmapInfo(pDibHeader)^.bmiColors[I].rgbBlue;
      end;
      PaletteHandle := CreatePalette(pPalette^);
      FreeMem(pPalette);
      OldPaletteHandle := SelectPalette(ADest.Handle, PaletteHandle, False);
      isDCPaletteDevice := True;
    end;
    // send the bits to aDest
    StretchDiBits(ADest.Handle, 
                  ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, 
                  0, 0, foo.Width, foo.Height, 
                  pBits, PBitmapInfo(pDibHeader)^, DIB_RGB_COLORS, SRCCOPY);
    // just incase you printer drver is a palette device
    if isDCPaletteDevice then
    begin
      SelectPalette(ADest.Handle, OldPaletteHandle, False);
      DeleteObject(PaletteHandle);
    end;
    // clean up allocated memory
    GlobalUnlock(hBits);
    GlobalFree(hBits);
    GlobalUnlock(hDibHeader);
    GlobalFree(hDibHeader);
  finally
    foo.free;
  end;
end;
{$endif}
*)


{ TRLSpoolFilter }

constructor TRLSpoolFilter.Create(AOwner: TComponent);
begin
  inherited;
  ClassOptions := ClassOptions + [foEmulateCopies];
end;

procedure TRLSpoolFilter.SetDocBounds(APaperWidth, APaperHeight: Double; AOrientation: TRLMetaOrientation);
var
  changed: Boolean;
  metrics: TRLPrinterMetrics;
  aux: string;
begin
  changed := (APaperWidth <> FPaperWidth) or (APaperHeight <> FPaperHeight) or (AOrientation <> FOrientation);
  if not FDocStarted or changed then
  begin
    FPaperWidth := APaperWidth;
    FPaperHeight := APaperHeight;
    FOrientation := AOrientation;
    if FDocStarted then
      RLPrinter.EndDoc;
    //  
    RLPrinter.SetPaperSize(FPaperWidth, FPaperHeight, FOrientation = MetaOrientationLandscape, False, True);
    RLPrinter.LoadMetrics(metrics);
    //
    FPrinterRect.Left := 0;
    FPrinterRect.Top := 0;
    if FOrientation = MetaOrientationLandscape then
    begin
      FPrinterRect.Right := Round(FPaperHeight * metrics.PPIX / InchAsMM);
      FPrinterRect.Bottom := Round(FPaperWidth * metrics.PPIY / InchAsMM);
    end
    else
    begin
      FPrinterRect.Right := Round(FPaperWidth * metrics.PPIX / InchAsMM);
      FPrinterRect.Bottom := Round(FPaperHeight * metrics.PPIY / InchAsMM);
    end;
    OffsetRect(FPrinterRect, - metrics.MarginLeft, - metrics.MarginTop);
    //
    aux := Pages.JobTitle;
    if aux = '' then
      aux := Pages.Title;
    if aux = '' then
      aux := 'Sem título';
    RLPrinter.BeginDoc(aux);
    FDocStarted := True;
  end;
end;

procedure TRLSpoolFilter.InternalBeginDoc;
begin
  FDocStarted := False;
  SetDocBounds(Pages.PaperWidth, Pages.PaperHeight, Pages.Orientation);
end;

procedure TRLSpoolFilter.InternalEndDoc;
begin
  RLPrinter.EndDoc;
end;

procedure TRLSpoolFilter.InternalNewPage;
begin
  RLPrinter.NewPage;
end;

procedure TRLSpoolFilter.InternalDrawPage(APage: TRLGraphicSurface);
var
  xfactor: Double;
  yfactor: Double;
  cliprct: TRect;
  clipstack: TList;
  obj: TRLGraphicObject;
  I: Integer;
  procedure ProjectX(const S: Integer; var D: Integer);
  begin
    D := FPrinterRect.Left + Round(S * xfactor);
  end;
  procedure ProjectY(const S: Integer; var D: Integer);
  begin
    D := FPrinterRect.Top + Round(S * yfactor);
  end;
  procedure ProjectRect(const S: TRLMetaRect; var D: TRect);
  begin
    ProjectX(S.Left, D.Left);
    ProjectY(S.Top, D.Top);
    ProjectX(S.Right, D.Right);
    ProjectY(S.Bottom, D.Bottom);
    if not (D.Right > D.Left) then
      D.Right := D.Left + 1;
    if not (D.Bottom > D.Top) then
      D.Bottom := D.Top + 1;
  end;
  procedure DrawPixel(AObj: TRLPixelObject);
  var
    R: TRect;
  begin
    ProjectRect(AObj.BoundsRect, R);
    //
    RLPrinter.Canvas.Brush.Style := bsSolid;
    RLPrinter.Canvas.Brush.Color := FromMetaColor(AObj.Color);
    RLPrinter.Canvas.FillRect(R);
  end;
  procedure ProjectPoint(const S: TRLMetaPoint; var D: TPoint);
  begin
    ProjectX(S.X, D.X);
    ProjectY(S.Y, D.Y);
  end;
  procedure DrawLine(AObj: TRLLineObject);
  var
    p1, p2: TPoint;
  begin
    ProjectPoint(AObj.FromPoint, p1);
    ProjectPoint(AObj.ToPoint, p2);
    //
    FromMetaPen(AObj.Pen, RLPrinter.Canvas.Pen);
    PenInflate(RLPrinter.Canvas.Pen, xfactor);
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    RLPrinter.Canvas.MoveTo(p1.X, p1.Y);
    CanvasLineToEx(RLPrinter.Canvas, p2.X, p2.Y);
  end;
  procedure DrawRectangle(AObj: TRLRectangleObject);
  var
    R: TRect;
  begin
    ProjectRect(AObj.BoundsRect, R);
    //
    FromMetaPen(AObj.Pen, RLPrinter.Canvas.Pen);
    PenInflate(RLPrinter.Canvas.Pen, xfactor);
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    RLPrinter.Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;
  procedure DrawText(AObj: TRLTextObject);
  var
    R: TRect;
    O: TPoint;
    T: String;
  begin
    ProjectRect(AObj.BoundsRect, R);
    ProjectPoint(AObj.Origin, O);
    //
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    FromMetaFont(AObj.Font, RLPrinter.Canvas.Font, yfactor);
    T := AObj.DisplayText;
    CanvasTextRectEx(RLPrinter.Canvas, R, O.X, O.Y, T, AObj.Alignment, AObj.Layout, AObj.TextFlags);
  end;
  procedure DrawFillRect(AObj: TRLFillRectObject);
  var
    R: TRect;
  begin
    ProjectRect(AObj.BoundsRect, R);
    //
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    RLPrinter.Canvas.FillRect(R);
  end;
  procedure DrawEllipse(AObj: TRLEllipseObject);
  var
    R: TRect;
  begin
    ProjectRect(AObj.BoundsRect, R);
    //
    FromMetaPen(AObj.Pen, RLPrinter.Canvas.Pen);
    PenInflate(RLPrinter.Canvas.Pen, xfactor);
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    RLPrinter.Canvas.Ellipse(R);
  end;
  procedure ProjectPoints(const S: TRLMetaPointArray; var P: TPointArray);
  var
    I: Integer;
  begin
    SetLength(P, High(S) + 1);
    for I := 0 to High(P) do
      ProjectPoint(S[I], P[I]);
  end;
  procedure DrawPolygon(AObj: TRLPolygonObject);
  var
    P: TPointArray;
  begin
    ProjectPoints(AObj.Points, P);
    //  
    FromMetaPen(AObj.Pen, RLPrinter.Canvas.Pen);
    PenInflate(RLPrinter.Canvas.Pen, xfactor);
    FromMetaBrush(AObj.Brush, RLPrinter.Canvas.Brush);
    RLPrinter.Canvas.Polygon(P);
  end;
  procedure DrawPolyline(AObj: TRLPolylineObject);
  var
    P: TPointArray;
  begin
    ProjectPoints(AObj.Points, P);
    //
    FromMetaPen(AObj.Pen, RLPrinter.Canvas.Pen);
    PenInflate(RLPrinter.Canvas.Pen, xfactor);
    RLPrinter.Canvas.Brush.Style := bsClear;
    RLPrinter.Canvas.Polyline(P);
  end;
  procedure DrawImage(AObj: TRLImageObject);
  var
    R: TRect;
{$ifndef LINUX}
    B: TGraphic;
{$endif}
  begin
    ProjectRect(AObj.BoundsRect, R);
    //
{$ifndef LINUX}
    B := FromMetaGraphic(AObj.Data);
    try
      if AObj.Parity then
        R.Right := R.Left + ((R.Right - R.Left) div B.Width) * B.Width + 1;
      SpecialStretchDraw(RLPrinter.Canvas, R, B);
    finally
      B.Free;
    end;
{$else}
    CanvasStretchDraw(RLPrinter.Canvas, R, AObj.Data, AObj.Parity);
{$endif}
  end;
  procedure PushClipRect(const ARect: TRect);
  var
    P: PRect;
  begin
    New(P);
    P^ := ARect;
    clipstack.Insert(0, P);
  end;
  procedure PopClipRect(var ARect: TRect);
  var
    P: PRect;
  begin
    P := clipstack[0];
    ARect := P^;
    Dispose(P);
    clipstack.Delete(0);
  end;
  procedure DrawSetClipRect(AObj: TRLSetClipRectObject);
  begin
    PushClipRect(cliprct);
    ProjectRect(AObj.BoundsRect, cliprct);
    CanvasSetClipRect(RLPrinter.Canvas, cliprct);
  end;
  procedure DrawResetClipRect(AObj: TRLResetClipRectObject);
  begin
    PopClipRect(cliprct);
    CanvasSetClipRect(RLPrinter.Canvas, cliprct);
  end;
begin
  SetDocBounds(APage.PaperWidth, APage.PaperHeight, APage.Orientation);
  //
  if APage.Width = 0 then
    xfactor := 1
  else
    xfactor := (FPrinterRect.Right - FPrinterRect.Left) / APage.Width;
  if APage.Height = 0 then
    yfactor := 1
  else
    yfactor := (FPrinterRect.Bottom - FPrinterRect.Top) / APage.Height;
  //
  clipstack := TList.Create;
  try
    CanvasStart(RLPrinter.Canvas);
    try
      cliprct := FPrinterRect;
      CanvasSetClipRect(RLPrinter.Canvas, cliprct);
      try
        for I := 0 to APage.ObjectCount - 1 do
        begin
          obj := TRLGraphicObject(APage.Objects[I]);
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
      finally
        CanvasResetClipRect(RLPrinter.Canvas);
      end;
    finally
      CanvasStop(RLPrinter.Canvas);
    end;
  finally
    while clipstack.Count > 0 do
      PopClipRect(cliprct);
    clipstack.free;
  end;
end;


initialization

finalization
  if Assigned(SpoolFilterInstance) then
    SpoolFilterInstance.free;

end.

