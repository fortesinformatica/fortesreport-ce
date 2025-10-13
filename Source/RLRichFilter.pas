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

{@unit RLRichFilter - Implementação do filtro para geração de arquivos no formato RichText. }
unit RLRichFilter;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  SysUtils, Classes, Types,
  {$IfDef FPC}
   LCLIntf,
  {$EndIf}
  {$IfDef CLX}
   QGraphics, RLMetaCLX,
  {$Else}
   Graphics, RLMetaVCL,
  {$EndIf}
  RLMetaFile, RLFilters, RLTypes, RLConsts, RLUtils;

type

  { TRLRichFilter }

  {@class TRLRichFilter - Filtro para geração de arquivos no formato RichText (RTF).
   Este filtro permite que os relatórios sejam salvos como arquivos no formato RichText.
   Arquivos neste formato podem ser lidos por editores de texto como MS-Word ou StarOffice.
   @links TRLHTMLFilter, TRLPDFFilter, TRLXLSFilter.
   @ancestor TRLCustomSaveFilter.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLRichFilter = class(TRLCustomSaveFilter)
  private

    // variables

    FTempStream: TFileStream;
    FTempFileName: String;
    FPrintCut: TPoint;
    FPrintSize: TPoint;
    FFontNames: TStringList;
    FPageNo: Integer;

    // methods
    
    procedure Write(const AStr: AnsiString = '');
    procedure Writeln(const AStr: AnsiString = '');
    procedure WrapWrite(const AStr: AnsiString; var AWidth: Integer);
    //
    procedure GetObjList(APage: TRLGraphicSurface; AList: TList; AFonts: TStrings);

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

  published

    // properties

    {@prop FileName = TRLCustomSaveFilter.FileName :/}
    property FileName;
    {@prop DisplayName = TRLCustomFilter.DisplayName :/}
    property DisplayName;
    {@prop ShowProgress - ancestor /}
    property ShowProgress;
  end;
  {/@class}

{/@unit}

implementation

const
  TWIPININCHES = 1 / 1440;

function PixelsToTwips(APixels: Integer): Integer;
var
  inches: Double;
begin
  inches := APixels / ScreenPPI;
  Result := Round(inches / TWIPININCHES);
end;

function PixelsToExt(APixels: Integer): Integer;
begin
  Result := Round(APixels * 26.46875);
end;

function RTF_PenStyle(APenStyle: TRLMetaPenStyle): String;
begin
  case APenStyle of
    MetaPenStyleSolid: Result := '\dplinesolid';
    MetaPenStyleDash: Result := '\dplinedash';
    MetaPenStyleDot: Result := '\dplinedot';
    MetaPenStyleDashDot: Result := '\dplinedado';
    MetaPenStyleDashDotDot: Result := '\dplinedadodo';
    MetaPenStyleClear: Result := '\dplinehollow';
    MetaPenStyleInsideFrame: Result := '\dplinesolid';
  else
    Result := '\dplinesolid'; 
  end; 
end;

function RTF_BrushStyle(ABrushStyle: TRLMetaBrushStyle): String;
begin 
  case ABrushStyle of
    MetaBrushStyleSolid: Result := '\dpfillpat1';
    MetaBrushStyleClear: Result := '\dpfillpat0';
    MetaBrushStyleHorizontal: Result := '\dpfillpat14';
    MetaBrushStyleVertical: Result := '\dpfillpat15';
    MetaBrushStyleFDiagonal: Result := '\dpfillpat16';
    MetaBrushStyleBDiagonal: Result := '\dpfillpat17';
    MetaBrushStyleCross: Result := '\dpfillpat18';
    MetaBrushStyleDiagCross: Result := '\dpfillpat19';
  else
    Result := '\dpfillpat1';
  end;
end;

function RTF_BoundsRect(ALeft, ATop, AWidth, AHeight: Integer): String;
begin
  Result := '\dpx' + IntToStr(PixelsToTwips(ALeft)) + 
          '\dpy' + IntToStr(PixelsToTwips(ATop)) + 
          '\dpxsize' + IntToStr(PixelsToTwips(AWidth)) + 
          '\dpysize' + IntToStr(PixelsToTwips(AHeight));
end;

function RTF_Size(AWidth, AHeight: Integer): String;
begin
  Result := '\dpxsize' + IntToStr(PixelsToTwips(AWidth)) + 
          '\dpysize' + IntToStr(PixelsToTwips(AHeight));
end;

function RTF_PenColor(ARed, AGreen, ABlue: Integer): String;
const
  prefix = 'dplineco';
begin
  Result := '\' + prefix + 'r' + IntToStr(ARed) + '\' + prefix + 'g' + IntToStr(AGreen) + '\' + prefix + 'b' + IntToStr(ABlue);
end;

function RTF_BrushColor(ARed, AGreen, ABlue: Integer): String;
const
  prefix = 'dpfillbgc';
begin
  Result := '\' + prefix + 'r' + IntToStr(ARed) + '\' + prefix + 'g' + IntToStr(AGreen) + '\' + prefix + 'b' + IntToStr(ABlue);
end;

function RTF_PenWidth(APenWidth: Integer): String;
begin
  Result := '\dplinew' + IntToStr(PixelsToTwips(APenWidth));
end;

function RTF_FormatText(const AText: String): String;
var
  I: Integer;
begin
  Result := GetAnsiStr(AText);
  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], ['{', '}', '\']) then
      Insert('\', Result, I);
end;

// hexadecimal invertido
function RTF_IntToHex(AInt, AWid: Integer): AnsiString;
var
  I: Integer;
  S: AnsiString;
begin
  S := AnsiString(IntToHex(AInt, AWid));
  SetLength(Result, AWid);
  I := 1;
  while I <= Length(S) do
  begin
    Move(S[I], Result[AWid - I], 2);
    Inc(I, 2);
  end;
end;

{ TRLRichFilter }

constructor TRLRichFilter.Create(AOwner: TComponent);
begin
  FFontNames := nil;
  FTempStream := nil;
  FTempFileName := '';
  //
  inherited;
  //
  FFontNames := TStringList.Create;
  //
  DefaultExt := '.rtf';
  DisplayName := GetLocalizeStr(LocaleStrings.LS_RichFormatStr);
end;

destructor TRLRichFilter.Destroy;
begin
  if Assigned(FFontNames) then
    FFontNames.free;
  if Assigned(FTempStream) then
  begin
    FTempStream.free;
    SysUtils.DeleteFile(FTempFileName);
  end;
  //
  inherited;
end;

procedure TRLRichFilter.Write(const AStr: AnsiString);
begin
  StreamWrite(FTempStream, AStr);
end;

procedure TRLRichFilter.Writeln(const AStr: AnsiString = '');
begin
  StreamWriteLn(FTempStream, AStr);
end;

procedure TRLRichFilter.WrapWrite(const AStr: AnsiString; var AWidth: Integer);
const
  maxwidth = 128;
begin
  Write(AStr);
  Inc(AWidth, Length(AStr));
  if AWidth > maxwidth then
  begin
    Writeln;
    AWidth := 0;
  end; 
end;

procedure TRLRichFilter.InternalBeginDoc;
begin
  FPageNo := 1; //Número da página para o flyanchor
  FPrintCut.X := 0;
  FPrintCut.Y := 0;
  FPrintSize.X := Pages.OrientedWidth;
  FPrintSize.Y := Pages.OrientedHeight;
  //
  FFontNames.Clear;
  //
  if Assigned(FTempStream) then
  begin
    FTempStream.free;
    SysUtils.DeleteFile(FTempFileName);
  end;
  FTempFileName := GetTempFileName;
  RegisterTempFile(FTempFileName);
  FTempStream := TFileStream.Create(FTempFileName, fmCreate);
end;

procedure TRLRichFilter.InternalEndDoc;
var
  endstream: TFileStream;
  I: Integer;
begin
  // cria arquivo destino
  try
    endstream := TFileStream.Create(FileName, fmCreate);
    try
      // grava header
      StreamWriteLn(endstream, '{\rtf1\ansi\ansicpg1252\deff0\deflang1046');
      // grava fontes
      StreamWriteLn(endstream, '{\fonttbl{');
      for I := 0 to FFontNames.Count - 1 do
        StreamWriteLn(endstream, '\f' + IntToStr(I) + '\fnil\fcharset0 ' + FFontNames[I] + ';');
      StreamWriteLn(endstream, '}}');
      // grava tamanho da página
      StreamWriteLn(endstream, '\paperw' + IntToStr(PixelsToTwips(FPrintSize.X)));
      StreamWriteLn(endstream, '\paperh' + IntToStr(PixelsToTwips(FPrintSize.Y)));
      if Pages.Orientation = MetaOrientationLandscape then
        StreamWriteLn(endstream, '\landscape');
      //
      StreamWriteLn(endstream, '\viewkind1');
      StreamWriteLn(endstream, '\margl0\margt0\margr0\margb0');
      // transfere dados do temp
      FTempStream.Seek(0, soFromBeginning);
      endstream.CopyFrom(FTempStream, FTempStream.Size);
      // grava fim de arquivo
      StreamWriteLn(endstream, '\par}');
    finally
      endstream.free;
    end;
  except
    SysUtils.DeleteFile(FileName);
    raise;
  end;
  // apaga arquivo temporário
  FreeAndNil(FTempStream);
  SysUtils.DeleteFile(FTempFileName);
  UnregisterTempFile(FTempFileName);
end;

procedure TRLRichFilter.InternalNewPage;
begin
  Writeln('\par\page\pard\plain\par');
  Inc(FPageNo);
end;

procedure TRLRichFilter.GetObjList(APage: TRLGraphicSurface; AList: TList; AFonts: TStrings);
var
  obj: TRLGraphicObject;
  ref: TRLGraphicObject;
  I, J: Integer;
  fn: String;
begin
  AList.Clear;
  for I := 0 to APage.ObjectCount - 1 do
  begin
    obj := APage.Objects[I];
    // insere texto ordenadamente
    J := 0;
    while J < AList.Count do
    begin
      ref := TRLGraphicObject(AList[J]);
      if (obj.BoundsRect.Top < ref.BoundsRect.Top) or ((obj.BoundsRect.Top = ref.BoundsRect.Top) and (obj.BoundsRect.Left < ref.BoundsRect.Left)) then
        Break;
      Inc(J);
    end;
    if J < AList.Count then
      AList.Insert(J, obj)
    else
      AList.Add(obj);
    // adiciona fonte
    if obj is TRLTextObject then
    begin
      fn := TRLTextObject(obj).Font.Name;
      if AFonts.IndexOf(fn) = -1 then
        AFonts.Add(fn);
    end;
  end;
end;

procedure TRLRichFilter.InternalDrawPage(APage: TRLGraphicSurface);
var
  objlist: TList;
  obj: TRLGraphicObject;
  I: Integer;
  cliprct: TRect;
  clipstack: TList;
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
  procedure DrawGraphic(Graphic: TGraphic; const Rect: TRect);
  var
    bmp: TBitmap;
    bmx: TBitmap;
    str: TMemoryStream;
    wrp: Integer;
    ch: Byte;
    N: Integer;
    cut: TRect;
    aux: TRect;
  begin
    bmp := nil;
    try
      // obriga bitmap 8bits
      aux := Rect;
      OffsetRect(aux, - aux.Left, - aux.Top);
      bmx := TRLBitmap.Create;
      try
        bmx.Width := aux.Right - aux.Left;
        bmx.Height := aux.Bottom - aux.Top;
        bmx.PixelFormat := pf32bit;
        bmx.Canvas.StretchDraw(aux, Graphic);
        IntersectRect(cut, Rect, cliprct);
        OffsetRect(cut, - Rect.Left, - Rect.Top);
        bmp := ClipGraphic(bmx, cut, False);
        {$IfNDef FPC}
         bmp.PixelFormat := pf8bit;
        {$EndIf} 
      finally
        bmx.free;
      end;
      //
      with Rect do
        Writeln('\pard\plain' + 
                '\absw' + IntToStr(PixelsToTwips(cut.Right - cut.Left)) + 
                '\absh-' + IntToStr(PixelsToTwips(cut.Bottom - cut.Top)) + 
                '\pvpg\posy' + IntToStr(PixelsToTwips(Top - FPrintCut.Y)) + 
                '\phpg\posx' + IntToStr(PixelsToTwips(Left - FPrintCut.X)) + 
                '{\*\flyanchor2\flypage' + IntToStr(FPageNo) + '}' + 
                '{\pict\wmetafile8' + 
                '\picw' + IntToStr(PixelsToExt(Right - Left)) + 
                '\pich' + IntToStr(PixelsToExt(Bottom - Top)) + 
                '\picbmp\picbpp8');
      //
      wrp := 0;
      str := TMemoryStream.Create;
      try
        bmp.SaveToStream(str);
        str.Seek(0, soFromBeginning);
        // metafile data
        str.Position := str.Position + 2; // skip trash
        str.Read(N, 4);
        N := N div 2 + 7;
        WrapWrite('010009000003' + RTF_IntToHex(N + 36, 8) + '0000', wrp);
        WrapWrite(RTF_IntToHex(N, 8) + '0000050000000B0200000000050000000C02', wrp);
        WrapWrite(RTF_IntToHex(bmp.Height, 4), wrp);
        WrapWrite(RTF_IntToHex(bmp.Width, 4), wrp);
        WrapWrite('05000000090200000000050000000102FFFFFF000400000007010300', wrp);
        WrapWrite(RTF_IntToHex(N, 8), wrp);
        WrapWrite('430F2000CC000000', wrp);
        WrapWrite(RTF_IntToHex(bmp.Height, 4), wrp);
        WrapWrite(RTF_IntToHex(bmp.Width, 4) + '00000000', wrp);
        WrapWrite(RTF_IntToHex(bmp.Height, 4), wrp);
        WrapWrite(RTF_IntToHex(bmp.Width, 4) + '00000000', wrp);
        str.Position := str.Position + 8; // skip trash
        while str.Read(ch, 1) > 0 do
          WrapWrite(IntToHex(ch, 2), wrp);
        WrapWrite('030000000000', wrp);
        Writeln;
      finally
        str.free;
      end;
      //
      Writeln('}\par\pard');
    finally
      if (bmp <> nil) and (bmp <> Graphic) then
        bmp.free;
    end;
  end;
  function CreateBitmap(AObj: TRLGraphicObject): TBitmap;
  begin
    Result := TRLBitmap.Create;
    Result.PixelFormat := pf32bit;
    Result.Width := AObj.BoundsRect.Right - AObj.BoundsRect.Left;
    Result.Height := AObj.BoundsRect.Bottom - AObj.BoundsRect.Top;
    Result.Canvas.Pen.Style := psSolid;
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
  end;
  procedure CopyBrush(Brush: TRLMetaBrush; Bitmap: TBitmap);
  begin
    with Brush.Color do
      Bitmap.Canvas.Brush.Color := RGB(Red, Green, Blue);
    Bitmap.Canvas.Brush.Style := FromMetaBrushStyle(Brush.Style);
  end;
  procedure CopyPen(Pen: TRLMetaPen; Bitmap: TBitmap);
  begin
    with Pen.Color do
      Bitmap.Canvas.Pen.Color := RGB(Red, Green, Blue);
    Bitmap.Canvas.Pen.Style := FromMetaPenStyle(Pen.Style);
    Bitmap.Canvas.Pen.Width := Pen.Width;
  end;
  procedure DrawPixel(AObj: TRLPixelObject);
  begin 
    Write('{\*\do\dobxpage\dobypage\dprect');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write(RTF_PenStyle(MetaPenStyleClear));
    Write(RTF_BrushStyle(MetaBrushStyleSolid));
    with AObj.Color do
      Write(RTF_BrushColor(Red, Green, Blue));
    Writeln('}');
  end;
  procedure DrawLine(AObj: TRLLineObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dpline');
    Write(RTF_BoundsRect(AObj.FromPoint.X - FPrintCut.X, AObj.FromPoint.Y - FPrintCut.Y, AObj.ToPoint.X - AObj.FromPoint.X, AObj.ToPoint.Y - AObj.FromPoint.Y));
    Write(RTF_PenStyle(AObj.Pen.Style));
    with AObj.Pen.Color do
      Write(RTF_PenColor(Red, Green, Blue));
    Write(RTF_PenWidth(AObj.Pen.Width));
    Writeln('}');
  end;
  procedure DrawRectangle(AObj: TRLRectangleObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dprect');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write(RTF_PenStyle(AObj.Pen.Style));
    with AObj.Pen.Color do
      Write(RTF_PenColor(Red, Green, Blue));
    Write(RTF_PenWidth(AObj.Pen.Width));
    Write(RTF_BrushStyle(AObj.Brush.Style));
    with AObj.Brush.Color do
      Write(RTF_BrushColor(Red, Green, Blue));
    Writeln('}');
  end;
  procedure DrawFillRect(AObj: TRLFillRectObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dprect');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write(RTF_PenStyle(MetaPenStyleClear));
    Write(RTF_PenWidth(0));
    Write(RTF_BrushStyle(AObj.Brush.Style));
    with AObj.Brush.Color do
      Write(RTF_BrushColor(Red, Green, Blue));
    Writeln('}');
  end;
  procedure DrawEllipse(AObj: TRLEllipseObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dpellipse');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write(RTF_PenStyle(AObj.Pen.Style));
    with AObj.Pen.Color do
      Write(RTF_PenColor(Red, Green, Blue));
    Write(RTF_PenWidth(AObj.Pen.Width));
    Write(RTF_BrushStyle(AObj.Brush.Style));
    with AObj.Brush.Color do
      Write(RTF_BrushColor(Red, Green, Blue));
    Writeln('}');
  end;
  procedure DrawPolygon(AObj: TRLPolygonObject);
  var
    I: Integer;
  begin
    Write('{\*\do\dobxpage\dobypage\dppolygon');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write('\dppolycount' + IntToStr(High(AObj.Points) + 1));
    for I := 0 to High(AObj.Points) do
      Write('\dpptx' + IntToStr(PixelsToTwips(AObj.Points[I].X - FPrintCut.X - AObj.BoundsRect.Left)) + 
            '\dppty' + IntToStr(PixelsToTwips(AObj.Points[I].Y - FPrintCut.Y - AObj.BoundsRect.Top)));
    Write(RTF_PenStyle(AObj.Pen.Style));
    with AObj.Pen.Color do
      Write(RTF_PenColor(Red, Green, Blue));
    Write(RTF_PenWidth(AObj.Pen.Width));
    Write(RTF_BrushStyle(AObj.Brush.Style));
    with AObj.Brush.Color do
      Write(RTF_BrushColor(Red, Green, Blue));
    Writeln('}');
  end;
  procedure DrawPolyline(AObj: TRLPolylineObject);
  var
    I: Integer;
  begin
    Write('{\*\do\dobxpage\dobypage\dppolyline');
    with AObj.BoundsRect do
      Write(RTF_BoundsRect(Left - FPrintCut.X, Top - FPrintCut.Y, Right - Left, Bottom - Top));
    Write('\dppolycount' + IntToStr(High(AObj.Points) + 1));
    for I := 0 to High(AObj.Points) do
      Write('\dpptx' + IntToStr(PixelsToTwips(AObj.Points[I].X - FPrintCut.X - AObj.BoundsRect.Left)) + 
            '\dppty' + IntToStr(PixelsToTwips(AObj.Points[I].Y - FPrintCut.Y - AObj.BoundsRect.Top)));
    Write(RTF_PenStyle(AObj.Pen.Style));
    with AObj.Pen.Color do
      Write(RTF_PenColor(Red, Green, Blue));
    Write(RTF_PenWidth(AObj.Pen.Width));
    Writeln('}');
  end;
  procedure DrawImage(AObj: TRLImageObject);
  var
    grp: TGraphic;
  begin
    grp := FromMetaGraphic(AObj.Data);
    try
      DrawGraphic(grp, FromMetaRect(AObj.BoundsRect));
    finally
      grp.free;
    end;
  end;
  procedure DrawText(AObj: TRLTextObject);
  var
    textrct: TRect;
    newwid: Integer;
    magic: Integer;
    ww: String;
  begin
    IntersectRect(textrct, FromMetaRect(AObj.BoundsRect), cliprct);
    // para compensar a diferença de tamanho da fonte, aumenta-se a largura do rect em 5% (mágico)
    newwid := textrct.Right - textrct.Left;
    magic := Trunc(newwid * 1.05) - newwid + 1;
    case AObj.Alignment of
      MetaTextAlignmentLeft: Inc(textrct.Right, magic);
      MetaTextAlignmentRight: Dec(textrct.Left, magic);
      MetaTextAlignmentCenter: begin
                                  Dec(textrct.Left, magic div 2);
                                  Inc(textrct.Right, magic - (magic div 2));
                                end;
      MetaTextAlignmentJustify: Inc(textrct.Right, magic);
    end;
    // para evitar o wordwrap
    case AObj.Alignment of
      MetaTextAlignmentLeft: ww := '\rin-1000';
      MetaTextAlignmentRight: ww := '\lin-1000';
      MetaTextAlignmentCenter: ww := '\rin-1000\lin-1000';
      MetaTextAlignmentJustify: ww := '';
   else
      ww := '';
    end;
    with textrct do
      Write('\pard\plain' + 
            '\absw' + IntToStr(PixelsToTwips(Right - Left)) + 
            '\absh-' + IntToStr(PixelsToTwips(Bottom - Top)) + 
            ww + 
            '\pvpg\posy' + IntToStr(PixelsToTwips(Top - FPrintCut.Y)) + 
            '\phpg\posx' + IntToStr(PixelsToTwips(Left - FPrintCut.X)) + 
            '{\*\flyanchor2\flypage' + IntToStr(FPageNo) + '}');
    // justifica o texto
    case AObj.Alignment of
      MetaTextAlignmentLeft: Write('\ql');
      MetaTextAlignmentRight: Write('\qr');
      MetaTextAlignmentCenter: Write('\qc');
      MetaTextAlignmentJustify: Write('\qj');
    end;
    Write('{');
    // seleciona fonte
    Write('\f' + IntToStr(FFontNames.IndexOf(AObj.Font.Name)) + '\fs' + IntToStr(AObj.Font.Size * 2));
    // efeitos
    if (AObj.Font.Style and MetaFontStyleBold) = MetaFontStyleBold then
      Write('\b');
    if (AObj.Font.Style and MetaFontStyleItalic) = MetaFontStyleItalic then
      Write('\i');
    if (AObj.Font.Style and MetaFontStyleUnderline) = MetaFontStyleUnderline then
      Write('\ul');
    if (AObj.Font.Style and MetaFontStyleStrikeOut) = MetaFontStyleStrikeOut then
      Write('\strike');
    // o texto
    Write(' ' + RTF_FormatText(AObj.DisplayText));
    // retorna fonte
    if (AObj.Font.Style and MetaFontStyleStrikeOut) = MetaFontStyleStrikeOut then
      Write('\strike0');
    if (AObj.Font.Style and MetaFontStyleUnderline) = MetaFontStyleUnderline then
      Write('\ul0');
    if (AObj.Font.Style and MetaFontStyleItalic) = MetaFontStyleItalic then
      Write('\i0');
    if (AObj.Font.Style and MetaFontStyleBold) = MetaFontStyleBold then
      Write('\b0');
    // fim de parágrafo 
    if AObj.Alignment = MetaTextAlignmentJustify then
      Write('\line');
    Writeln('}\par\pard');
  end;
  procedure DrawSetClipRect(AObj: TRLSetClipRectObject);
  begin
    PushClipRect(cliprct);
    IntersectRect(cliprct, cliprct, FromMetaRect(AObj.BoundsRect));
  end;
  procedure DrawResetClipRect(AObj: TRLResetClipRectObject);
  begin
    PopClipRect(cliprct);
  end;
begin
  clipstack := TList.Create;
  try
    cliprct := Rect(0, 0, APage.Width, APage.Height);
    objlist := TList.Create;
    try
      // coleta textos e fontes
      GetObjList(APage, objlist, FFontNames);
      // grava objetos
      for I := 0 to objlist.Count - 1 do
      begin 
        obj := TRLGraphicObject(objlist[I]);
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
      objlist.free;
    end;
  finally
    while clipstack.Count > 0 do
      PopClipRect(cliprct);
    clipstack.free;
  end;
end;

end.

