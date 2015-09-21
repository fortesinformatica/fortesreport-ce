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

{@unit RLPreview - Implementa��o dos componentes de pr�-visualiza��o. }

unit RLPreview;

interface

uses
  Classes, SysUtils, Math, Contnrs,
{$ifndef LINUX}
  Windows,
{$else}
  Types,
{$endif}
{$ifdef VCL}
  Graphics, Controls, ExtCtrls, Forms, Menus, Clipbrd, Dialogs,
{$else}
  QGraphics, QControls, QExtCtrls, QForms, QMenus, QClipbrd, QDialogs,
{$endif}
  RLMetaFile, RLConsts, RLUtils;

type

  TRLPreviewBox = class;

  { TRLPreview }

  TRLPreviewLastFind = record
    PageIndex: Integer;
    ObjectIndex: Integer;
    Found: Boolean;
  end;

  {@class TRLPreview - Componente para pr�-visualiza��o das p�ginas geradas.
   Possui m�todos para controlar a navega��o.
   @pub }
  TRLPreview = class(TScrollBox)
  private

    // variables

    FBoxes: TObjectList;
    FVisibleBoxes: Integer;
    FOnChangeView: TNotifyEvent;
    FZoomFactor: Double;
    FPages: TRLGraphicStorage;
    FPageIndex: Integer;
    FLastSize: TPoint;
    FEditing: Boolean;
    FPopup: TPopupMenu;
    FLastFind: TRLPreviewLastFind;
    FCapturing: boolean;
    FMoving: boolean;
    FCapturedPoint: TPoint;

    // assign methods

    function GetPageNumber: Integer;
    procedure SetPageNumber(const AValue: Integer);
    procedure SetPageIndex(const AValue: Integer);
    procedure SetZoomFactor(const AValue: Double);
    procedure SetPages(const AValue: TRLGraphicStorage);
    procedure SetEditing(const Value: Boolean);
    procedure SetMultipleMode(const AValue: Boolean);
    function GetMultipleMode: Boolean;

    // event handlers

    procedure CopyPageBMP(Sender: TObject);
    procedure CopyPageWMF(Sender: TObject);

    // custom methods

    procedure RealignBoxes;
    procedure DoChangeView;
    procedure CancelEdit;

    procedure FollowMouse(aX, aY: integer);
  protected

    // override

    procedure Click; override;

  public
    procedure DisableCapture;
    procedure EnableCapture;
    procedure Capture(aX, aY: integer);
    procedure FinalizeCapture;
    // constructors & destructors

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // override

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // custom methods

    {@method FirstPage - Posiciona na primeira p�gina. :/}
    procedure FirstPage;

    {@method LastPage - Posiciona na �ltima p�gina. :/}
    procedure LastPage;

    {@method NextPage - Posiciona na pr�xima p�gina ou grupo de p�ginas. :/}
    procedure NextPage;

    {@method PriorPage - Posiciona na p�gina anterior ou grupo de p�ginas anterior. :/}
    procedure PriorPage;

    {@method PageTop - Posiciona no topo da p�gina corrente. :/}
    procedure PageTop;

    {@method PageBottom - Posiciona na parte inferior da p�gina corrente. :/}
    procedure PageBottom;

    {@method PageLeft - Posiciona na parte mais � esquerda da p�gina corrente. :/}
    procedure PageLeft;

    {@method PageRight - Posiciona na parte mais � direita da p�gina corrente. :/}
    procedure PageRight;

    {@method HalfPageUp - Rola a p�gina para cima. :/}
    procedure HalfPageUp;

    {@method HalfPageDown - Rola a p�gina para baixo. :/}
    procedure HalfPageDown;

    {@method HalfPageLeft - Rola a p�gina para a esquerda. :/}
    procedure HalfPageLeft;

    {@method HalfPageRight - Rola a p�gina para a direita. :/}
    procedure HalfPageRight;

    {@method ScrollLeft - Rola a p�gina um caractere para a esquerda. :/}
    procedure ScrollLeft;

    {@method ScrollRight - Rola a p�gina um caractere para a direita. :/}
    procedure ScrollRight;

    {@method ScrollUp - Rola a p�gina uma linha para cima. :/}
    procedure ScrollUp;

    {@method ScrollDown - Rola a p�gina uma linha para baixo. :/}
    procedure ScrollDown;

    {@method ZoomPage - Focaliza uma p�gina quando em modo m�ltiplas p�ginas. :/}
    procedure ZoomPage(Index: Integer);

    {@method GetZoomFactorFullPage - Retorna o fator de zoom ideal para que a p�gina caiba inteiramente na tela. :/}
    function GetZoomFactorFullPage: Double;

    {@method ZoomFullPage - Aplica zoom na p�gina corrente para que esta caiba inteira na tela. :/}
    procedure ZoomFullPage;

    {@method ZoomIn - Aumenta o zoom na p�gina corrente para focar um ponto espec�fico. :/}
    procedure ZoomIn(aPercentual:integer=-1);

    {@method ZoomOut - Diminui o zoom na p�gina corrente para visualizar um espa�o maior da p�gina. :/}
    procedure ZoomOut(aPercentual:integer=-1);

    {@method GetZoomFactorFullWidth - Retorna o fator de zoom ideal para que a largura da p�gina caiba na tela. :/}
    function GetZoomFactorFullWidth: Double;

    {@method ZoomFullWidth - Aplica zoom na p�gina corrente para que a sua largura caiba inteira na tela. :/}
    procedure ZoomFullWidth;

    {@method GetZoomFactorMultiplePages - Retorna o fator de zoom ideal para que v�rias p�ginas caibam na tela. :/}
    function GetZoomFactorMultiplePages: Double;

    {@method ZoomMultiplePages - Alterna entre os modos p�gina �nica e m�ltiplas p�ginas. :/}
    procedure ZoomMultiplePages;

    {@method FindText - Posiciona na pr�xima ocorr�ncia do texto. :/}
    function FindText(const AText: String; AWholeWords, AMatchCase, AFindBackward: Boolean): Boolean;

    // custom properties
    {@prop PageNumber - Determina ou indica o n�mero da p�gina atual.
     Nota: O n�mero da p�gina leva em considera��o a numera��o inicial. :/}
    property PageNumber: Integer read GetPageNumber write SetPageNumber;

    {@prop PageIndex - Determina ou indica o �ndice da p�gina atual.
     Este �ndice vai de 0 at� (Pages.Count-1). :/}
    property PageIndex: Integer read FPageIndex write SetPageIndex;

    {@prop ZoomFactor - Determina ou indica o fator de zoom atual. :/}
    property ZoomFactor: Double read FZoomFactor write SetZoomFactor;

    {@prop Editing - Determina ou indica se o preview est� em modo de edi��o.
     Quando em modo de edi��o, o objeto preview permite que se altere algumas caracter�sticas do
     relat�rio como fontes, cores e at� mesmo textos. Estas altera��es podem ser salvas novamente,
     exportadas ou impressas. :/}
    property Editing: Boolean read FEditing write SetEditing;

    {@prop Pages - Refer�ncia para a cole��o de p�ginas.
     Deve apontar para um objeto TRLGraphicStorage, que pode ser obtido atrav�s da prop
     Pages do componente TRLReport. Este objeto tamb�m pode ser instanciado e carregar
     um relat�rio a partir de um arquivo em disco ou stream. :/}
    property Pages: TRLGraphicStorage read FPages write SetPages;

    {@prop MultipleMode - Determina ou indica o estado de visualiza��o de m�ltiplas p�ginas. :/}
    property MultipleMode: Boolean read GetMultipleMode write SetMultipleMode;

  published

    // custom properties

    {@prop OnChangeView - Ao mudar as caracter�sticas de visualiza��o. :/}
    property OnChangeView: TNotifyEvent read FOnChangeView write FOnChangeView;
  end;
  {/@class}


  { TRLPreviewBox }

  {@class TRLPreviewBox - Caixa de visualiza��o de p�gina.
   @pub }
  TRLPreviewBox = class(TCustomControl)
  private

    // variables

    FPreview: TRLPreview;
    FSelected: TRLGraphicObject;

    // assign methods

    function GetPage: TRLGraphicSurface;

    // custom methods

    function DoZoom(X: Integer): Integer;
    function UndoZoom(X: Integer): Integer;

  protected

    // override methods

    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    {@method ObjectAt - Retorna refer�ncia para o objeto que intercepta as coordenadas. :/}
    function ObjectAt(X, Y: Integer): TRLGraphicObject;

  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;


    // override methods

    procedure Paint; override;

    // custom properties

    {@prop Page - Refer�ncia para o objeto p�gina. :/}
    property Page: TRLGraphicSurface read GetPage;
    destructor Destroy; override;
  end;
  {/@class}
  

{/@unit}

implementation

{$R RLReport.res}

const
  crHandOpen = 1;
  crHandClosed = 2;

// UTILS

procedure DrawGrab(ACanvas: TCanvas; X, Y: Integer);
const
  GRABRAD = 3;
begin
  with ACanvas do
    Rectangle(X - GRABRAD, Y - GRABRAD, X + GRABRAD, Y + GRABRAD);
end;

procedure DrawGrabs(ACanvas: TCanvas; ARect: TRect; AColor: TColor);
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    Brush.Color := AColor;
    //
    DrawGrab(ACanvas, ARect.Left, ARect.Top);
    DrawGrab(ACanvas, (ARect.Left + ARect.Right) div 2, ARect.Top);
    DrawGrab(ACanvas, ARect.Right, ARect.Top);
    DrawGrab(ACanvas, ARect.Right, (ARect.Top + ARect.Bottom) div 2);
    DrawGrab(ACanvas, ARect.Right, ARect.Bottom);
    DrawGrab(ACanvas, (ARect.Left + ARect.Right) div 2, ARect.Bottom);
    DrawGrab(ACanvas, ARect.Left, ARect.Bottom);
    DrawGrab(ACanvas, ARect.Left, (ARect.Top + ARect.Bottom) div 2);
  end;
end;

procedure DrawFound(ACanvas: TCanvas; ARect: TRect);
const
  FoundBorderSize = 3;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Pen.Width := FoundBorderSize;
    Pen.Mode := pmXor;
    Brush.Style := bsClear;
    Rectangle(ARect.Left - FoundBorderSize, ARect.Top - FoundBorderSize, ARect.Right + FoundBorderSize, ARect.Bottom + FoundBorderSize);
  end;
end;

{ TRLPreview }

constructor TRLPreview.Create(AOwner: TComponent);
var
  B: TRLPreviewBox;
  I: Integer;
  M: TMenuItem;
begin
  FOnChangeView := nil;
  FPages := nil;
  FZoomFactor := 100;
  FVisibleBoxes := 1;
  FPageIndex := -1;
  FLastSize := Point(0, 0);
  FEditing := False;
  FBoxes := nil;
  FPopup := nil;
  FLastFind.Found := False;
  FLastFind.PageIndex := -1;
  //
  FBoxes := TObjectList.Create;
  //
  FPopup := TPopupMenu.Create(Self);
  M := TMenuItem.Create(FPopup);
  M.Caption := 'Copiar como Bitmap';
  M.OnClick := CopyPageBMP;
  FPopup.Items.Add(M);
  M := TMenuItem.Create(FPopup);
  M.Caption := 'Copiar como MetaFile';
  M.OnClick := CopyPageWMF;
  FPopup.Items.Add(M);
  //
  inherited Create(AOwner);
  //
  for I := 0 to 2 do
  begin
    B := TRLPreviewBox.Create(Self);
    B.PopupMenu := FPopup;
    B.Visible := (I = 0);
    B.Parent := Self;
    FBoxes.Add(B);
  end;
  //
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
end;

destructor TRLPreview.Destroy;
begin
  FreeAndNIL(FBoxes);
  //
  inherited;
end;

procedure TRLPreview.CopyPageBMP(Sender: TObject);
var
  bitmap: TBitmap;
  page: TRLGraphicSurface;
begin
  if FPopup.PopupComponent is TRLPreviewBox then
  begin
    page := TRLPreviewBox(FPopup.PopupComponent).Page;
    if page <> nil then
    begin
      bitmap := NeedAuxBitmap;
      bitmap.Width := page.Width;
      bitmap.Height := page.Height;
      bitmap.PixelFormat := pf32bit;
      page.PaintTo(bitmap.Canvas, Rect(0, 0, bitmap.Width, bitmap.Height));
      ClipBoard.Assign(bitmap);
    end;
  end;
end;

procedure TRLPreview.CopyPageWMF(Sender: TObject);
var
  mf: TMetaFile;
  mfc: TMetaFileCanvas;
  page: TRLGraphicSurface;
begin
  if FPopup.PopupComponent is TRLPreviewBox then
  begin
    page := TRLPreviewBox(FPopup.PopupComponent).Page;
    if page <> nil then
    begin
      mf := TMetaFile.Create;
      try
//        mf.Width      :=page.Width;
//        mf.Height     :=page.Height;
        mfc := TMetafileCanvas.Create(mf, 0);
        page.PaintTo(mfc, Rect(0, 0, page.Width, page.Height));
        mfc.Free;
        ClipBoard.Assign(mf);
      finally
        mf.Free;
      end;
    end;
  end;
end;

procedure TRLPreview.FirstPage;
begin
  if Assigned(FPages) then
    PageIndex := 0;
end;

procedure TRLPreview.LastPage;
begin
  if Assigned(FPages) then
    PageIndex := Max(0, FPages.PageCount - FVisibleBoxes);
end;

procedure TRLPreview.NextPage;
begin
  if Assigned(FPages) then
    PageIndex := Min(FPages.PageCount - 1, PageIndex + FVisibleBoxes);
end;

procedure TRLPreview.PriorPage;
begin
  if Assigned(FPages) then
    PageIndex := Max(0, PageIndex - FVisibleBoxes);
end;

procedure TRLPreview.PageTop;
begin
  VertScrollBar.Position := 0;
end;

procedure TRLPreview.PageBottom;
begin
  VertScrollBar.Position := VertScrollBar.range;
end;

procedure TRLPreview.PageLeft;
begin
  HorzScrollBar.Position := 0;
end;

procedure TRLPreview.PageRight;
begin
  HorzScrollBar.Position := HorzScrollBar.range;
end;

procedure TRLPreview.HalfPageUp;
begin
  VertScrollBar.Position := VertScrollBar.Position - Height;
end;

procedure TRLPreview.HalfPageDown;
begin
  VertScrollBar.Position := VertScrollBar.Position + Height;
end;

procedure TRLPreview.HalfPageLeft;
begin
  HorzScrollBar.Position := HorzScrollBar.Position - Width;
end;

procedure TRLPreview.HalfPageRight;
begin
  HorzScrollBar.Position := HorzScrollBar.Position + Width;
end;

procedure TRLPreview.ScrollLeft;
begin
  HorzScrollBar.Position := HorzScrollBar.Position - 10;
end;

procedure TRLPreview.ScrollRight;
begin
  HorzScrollBar.Position := HorzScrollBar.Position + 10;
end;

procedure TRLPreview.ScrollUp;
begin
  VertScrollBar.Position := VertScrollBar.Position - 10;
end;

procedure TRLPreview.ScrollDown;
begin
  VertScrollBar.Position := VertScrollBar.Position + 10;
end;

function TRLPreview.GetMultipleMode: Boolean;
begin
  Result := (FVisibleBoxes > 1);
end;

procedure TRLPreview.SetMultipleMode(const AValue: Boolean);
var
  I: Integer;
begin
  if AValue then
    FVisibleBoxes := FBoxes.Count
  else
    FVisibleBoxes := 1;
  for I := 0 to FBoxes.Count - 1 do
    TRLPreviewBox(FBoxes.Items[I]).Visible := (I < FVisibleBoxes);
end;

procedure TRLPreview.ZoomPage(Index: Integer);
begin
  if PageIndex <> Index then
    PageIndex := Index;
  if MultipleMode then
    ZoomFullWidth
  else
    ZoomMultiplePages;
end;

function TRLPreview.GetZoomFactorFullPage: Double;
var
  zw, zh: Double;
begin
  if Assigned(FPages) then
  begin
    zw := Round(100 * (Self.Width - 7) / FPages.OrientedWidth);
    zh := Round(100 * (Self.Height - 7) / FPages.OrientedHeight);
    if zw < zh then
      Result := zw
    else
      Result := zh;
  end
  else
    Result := 100;
end;

procedure TRLPreview.ZoomFullPage;
begin
  MultipleMode := False;
  ZoomFactor := GetZoomFactorFullPage;
end;

procedure TRLPreview.ZoomIn(aPercentual:integer=-1);
begin
  if aPercentual = -1 then aPercentual := 10;
  ZoomFactor := ZoomFactor + aPercentual;
end;

procedure TRLPreview.ZoomOut(aPercentual:integer=-1);
begin
  if aPercentual = -1 then aPercentual := 10;
  ZoomFactor := ZoomFactor - aPercentual;
end;

function TRLPreview.GetZoomFactorFullWidth: Double;
begin
  if Assigned(FPages) then
    Result := Round(100 * (Self.Width - 7) / FPages.OrientedWidth)
  else
    Result := 100;
end;

procedure TRLPreview.ZoomFullWidth;
begin
  MultipleMode := False;
  ZoomFactor := GetZoomFactorFullWidth;
end;

function TRLPreview.GetZoomFactorMultiplePages: Double;
var
  zw, zh: Double;
begin
  if Assigned(FPages) then
  begin
    zw := Round(100 * ((Self.Width - 7) / FVisibleBoxes) / FPages.OrientedWidth);
    zh := Round(100 * (Self.Height - 7) / FPages.OrientedHeight);
    if zw < zh then
      Result := zw
    else
      Result := zh;
  end
  else
    Result := 100;
end;

procedure TRLPreview.ZoomMultiplePages;
begin
  MultipleMode := True;
  ZoomFactor := GetZoomFactorMultiplePages;
end;

function TRLPreview.FindText(const AText: String; AWholeWords, AMatchCase, AFindBackward: Boolean): Boolean;
var
  P: TRLGraphicSurface;
  O: TRLGraphicObject;
  I, J: Integer;
  S, T: String;
begin
  Result := False;
  //
  if not Assigned(FPages) then
    Exit;
  T := AText;
  if not AMatchCase then
    T := AnsiUpperCase(T);
  I := FPageIndex;
  while (I >= 0) and (I < FPages.PageCount) do
  begin
    P := FPages[I];
    if FLastFind.PageIndex = I then
    begin
      J := FLastFind.ObjectIndex;
      if AFindBackward then
        Dec(J)
      else
        Inc(J);
    end
    else
    begin
      FLastFind.PageIndex := -1;
      if AFindBackward then
        J := P.ObjectCount - 1
      else
        J := 0;
    end;
    while (J >= 0) and (J < P.ObjectCount) do
    begin
      O := P.Objects[J];
      if O is TRLTextObject then
      begin
        S := TRLTextObject(O).Text;
        if not AMatchCase then
          S := AnsiUpperCase(S);
        if Pos(T, S) > 0 then
        begin
          FLastFind.PageIndex := I;
          FLastFind.ObjectIndex := J;
          FLastFind.Found := True;
          CancelEdit;
          Self.PageIndex := I;
          Result := True;
          Exit;
        end;
      end;
      if AFindBackward then
        Dec(J)
      else
        Inc(J);
    end;
    if AFindBackward then
      Dec(I)
    else
      Inc(I);
  end;
end;

function TRLPreview.GetPageNumber: Integer;
begin
  if Assigned(FPages) then
    Result := PageIndex + 1
  else
    Result := 0; 
end;

procedure TRLPreview.DoChangeView;
begin
  if Assigned(FOnChangeView) then
    FOnChangeView(Self);
end;

procedure TRLPreview.CancelEdit;
var
  I: Integer;
begin
  for I := 0 to FVisibleBoxes - 1 do
    with TRLPreviewBox(FBoxes.Items[I]) do
    begin
      FSelected := nil;
      Invalidate;
    end;
end;

procedure TRLPreview.SetPageIndex(const AValue: Integer);
begin
  if Assigned(FPages) then
    if (AValue >= 0) and (AValue < FPages.PageCount) and (AValue <> FPageIndex) then
    begin
      FPageIndex := AValue;
      VertScrollBar.Position := 0;
      HorzScrollBar.Position := 0;
      RealignBoxes;
    end;
end;

procedure TRLPreview.SetPageNumber(const AValue: Integer);
begin
  if Assigned(FPages) then
    PageIndex := AValue - 1;
end;

procedure TRLPreview.SetPages(const AValue: TRLGraphicStorage);
begin
  if AValue = FPages then
    Exit;
  if Assigned(FPages) then
    FPages.Unlink(Self);
  FPages := AValue;
  if Assigned(FPages) then
    FPages.Link(Self);
  //  
  RealignBoxes;
  FirstPage;
end;

procedure TRLPreview.SetZoomFactor(const AValue: Double);
begin
  if (AValue >= 10) and (AValue <> FZoomFactor) then
  begin
    FZoomFactor := AValue;
    RealignBoxes;
  end;
end;

procedure TRLPreview.RealignBoxes;
var
  I, offsetleft, offsettop, docwidth, docheight, pagewidth, pageheight, totalwidth, totalheight: Integer;
  box: TRLPreviewBox;
begin
  if Assigned(FPages) then
  begin
    docwidth := Round(FPages.OrientedWidth * FZoomFactor / 100) + 2 + 5;
    docheight := Round(FPages.OrientedHeight * FZoomFactor / 100) + 2 + 5;
    totalwidth := 0;
    totalheight := 0;
    for I := 0 to FVisibleBoxes - 1 do
    begin
      box := TRLPreviewBox(FBoxes.Items[I]);
      if box.Page <> nil then
      begin
        pagewidth := Round(box.Page.Width * FZoomFactor / 100) + 2 + 5;
        pageheight := Round(box.Page.Height * FZoomFactor / 100) + 2 + 5;
      end
      else
      begin
        pagewidth := docwidth;
        pageheight := docheight;
      end;
      Inc(totalwidth, pagewidth);
      totalheight := Max(totalheight, pageheight);
    end;
    if totalwidth < Width then
    begin
      HorzScrollBar.Position := 0;
      offsetleft := (Width - totalwidth) div 2;
    end
    else
      offsetleft := -HorzScrollBar.Position;
    if totalheight < Height then
    begin
      VertScrollBar.Position := 0;
      offsettop := (Height - totalheight) div 2;
    end
    else
      offsettop := -VertScrollBar.Position;
    for I := 0 to FVisibleBoxes - 1 do
    begin
      box := TRLPreviewBox(FBoxes.Items[I]);
      if box.Page <> nil then
      begin
        pagewidth := Round(box.Page.Width * FZoomFactor / 100) + 2 + 5;
        pageheight := Round(box.Page.Height * FZoomFactor / 100) + 2 + 5;
      end
      else
      begin
        pagewidth := docwidth;
        pageheight := docheight;
      end;
      box.BoundsRect := Rect(offsetleft, offsettop + (totalheight - pageheight) div 2, offsetleft + pagewidth, offsettop + (totalheight - pageheight) div 2 + pageheight);
      Inc(offsetleft, pagewidth);
      box.Invalidate;
    end;
    DoChangeView;
  end;
end;

procedure TRLPreview.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  //
  if (Width <> FLastSize.X) or (Height <> FLastSize.Y) then
    RealignBoxes;
  FLastSize := Point(Width, Height); 
end;

procedure TRLPreview.Click;
begin
  inherited;
  //
  DoEnter;
end;

procedure TRLPreview.SetEditing(const Value: Boolean);
begin
  if Value = FEditing then
    Exit;
  FEditing := Value;
  if not FEditing then
    CancelEdit;
end;

procedure TRLPreview.EnableCapture;
begin
  if FCapturing then Exit;
  Screen.Cursor := crHandOpen;
  FCapturing := True;
end;

procedure TRLPreview.DisableCapture;
begin
  if FMoving then
    Screen.Cursor := crHandClosed
  else
    Screen.Cursor := crDefault;
  FCapturing := False;
end;

procedure TRLPreview.Capture(aX, aY: integer);
begin
  if not FMoving then
  begin
    FMoving := True;
    Screen.Cursor := crHandClosed;
    FCapturedPoint.X := aX;
    FCapturedPoint.Y := aY;
  end;
end;

procedure TRLPreview.FinalizeCapture;
begin
  if FMoving then
  begin
    FMoving := False;
    if FCapturing then
      Screen.Cursor := crHandOpen
    else
      Screen.Cursor := crDefault;
  end;
end;

procedure TRLPreview.FollowMouse(aX, aY: integer);
begin
  if FMoving then
  begin
    HorzScrollBar.Position := HorzScrollBar.Position - (aX - FCapturedPoint.X);// div 2;
    VertScrollBar.Position := VertScrollBar.Position - (aY - FCapturedPoint.Y)// div 2;
  end;
end;

{ TRLPreviewBox }

constructor TRLPreviewBox.Create(AOwner: TComponent);
begin
  FPreview := TRLPreview(AOwner);
  FSelected := nil;
  //
  inherited Create(nil);
  //
  Left := 0;
  Top := 0;
  Width := 1;
  Height := 1;
  ControlStyle := ControlStyle + [csOpaque];
end;

function TRLPreviewBox.GetPage: TRLGraphicSurface;
var
  I, J: Integer;
begin
  I := FPreview.FPageIndex;
  J := FPreview.FBoxes.IndexOf(Self);
  Result := FPreview.Pages[I + J];
end;

function TRLPreviewBox.DoZoom(X: Integer): Integer;
begin
  Result := Round(X * (FPreview.ZoomFactor / 100));
end;

function TRLPreviewBox.UndoZoom(X: Integer): Integer;
begin
  Result := Round(X / (FPreview.ZoomFactor / 100));
end;

procedure TRLPreviewBox.Paint;
var
  R, rr, rb: TRect;
  surface: TRLGraphicSurface;
  obj: TRLGraphicObject;
  L: TList;
  I: Integer;
  cl: TColor;
begin
  inherited;
  //
  surface := Page;
  with Canvas do
  begin
    if Assigned(surface) then
      Brush.Color := clWhite
    else
      Brush.Color := clGray;
    Brush.Style := bsSolid;
    Pen.Width := 1;
    Pen.Color := clBlack;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    //
    R := BoundsRect;
    OffsetRect(R, - R.Left, - R.Top);
    Dec(R.Right, 2 + 5);
    Dec(R.Bottom, 2 + 5);
    //
    rr := Rect(R.Right, R.Top + 5, R.Right + 5, R.Bottom + 5);
    rb := Rect(R.Left + 5, R.Bottom, R.Right, R.Bottom + 5);
    Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, - 1, - 1);
    if Assigned(surface) then
      surface.PaintTo(Self.Canvas, R)
    else
    begin
      MoveTo(R.Left, R.Top); LineTo(R.Right, R.Bottom);
      MoveTo(R.Right, R.Top); LineTo(R.Left, R.Bottom);
    end;
    Brush.Color := clGray;
    FillRect(rr);
    FillRect(rb);
  end;
  //
  if Assigned(surface) and Assigned(FSelected) then
  begin
    // pega todos de mesmo grupo
    L := TList.Create;
    try
      for I := 0 to surface.ObjectCount - 1 do
      begin
        obj := surface.Objects[I];
        if (obj.GroupId <> 0) and (obj.GroupId = FSelected.GroupId) then
          L.Add(obj);
      end; 
      if L.Count > 1 then
        cl := clSilver
      else
        cl := clBlack;
      for I := 0 to L.Count - 1 do
        with TRLGraphicObject(L[I]) do
          DrawGrabs(Self.Canvas, Rect(DoZoom(BoundsRect.Left), 
                                     DoZoom(BoundsRect.Top), 
                                     DoZoom(BoundsRect.Right), 
                                     DoZoom(BoundsRect.Bottom)), 
                                     cl);
    finally
      L.Free;
    end;
  end;
  //
  if Assigned(surface) and (surface.PageIndex = FPreview.FLastFind.PageIndex) and FPreview.FLastFind.Found then
  begin
    I := FPreview.FLastFind.ObjectIndex;
    if I < surface.ObjectCount then
      with surface.Objects[I] do
        DrawFound(Self.Canvas, Rect(DoZoom(BoundsRect.Left), 
                                   DoZoom(BoundsRect.Top), 
                                   DoZoom(BoundsRect.Right), 
                                   DoZoom(BoundsRect.Bottom)));
  end; 
end;

procedure TRLPreviewBox.DblClick;
begin
  inherited;
  //
  FPreview.ZoomPage(FPreview.PageIndex + FPreview.FBoxes.IndexOf(Self));
end;

procedure TRLPreviewBox.Click;
begin
  inherited;
  //
  FPreview.DoEnter;
end;

procedure TRLPreviewBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  obj: TRLGraphicObject;
begin
  inherited;
  //
  if FPreview.Editing then
  begin
    obj := ObjectAt(UndoZoom(X), UndoZoom(Y));
    if obj <> FSelected then
    begin
      FSelected := obj;
      Invalidate;
    end;
  end;
  if Parent is TRLPreview and (ssCtrl in Shift) then
    (Parent as TRLPreview).Capture(X, Y);
end;

procedure TRLPreviewBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Parent is TRLPreview then
    (Parent as TRLPreview).FinalizeCapture;
end;

procedure TRLPreviewBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Parent is TRLPreview then
    (Parent as TRLPreview).FollowMouse(X, Y);
end;

function TRLPreviewBox.ObjectAt(X, Y: Integer): TRLGraphicObject;
var
  I, leastarea, area: Integer;
  surface: TRLGraphicSurface;
  obj: TRLGraphicObject;
begin
  Result := nil;
  //
  surface := Page;
  leastarea := 0;
  if surface <> nil then
    for I := 0 to surface.ObjectCount - 1 do
    begin
      obj := surface.Objects[I];
      if (obj.BoundsRect.Left <= X) and (obj.BoundsRect.Right >= X) and (obj.BoundsRect.Top <= Y) and (obj.BoundsRect.Bottom >= Y) then
      begin
        area := (obj.BoundsRect.Right - obj.BoundsRect.Left) * (obj.BoundsRect.Bottom - obj.BoundsRect.Top);
        if (Result = nil) or (area < leastarea) then
        begin
          Result := obj;
          leastarea := area;
        end;
      end;
    end;
end;

destructor TRLPreviewBox.Destroy;
begin
  inherited;

end;

initialization
  Screen.Cursors[crHandOpen] := LoadCursor(hInstance, 'HANDOPEN');
  Screen.Cursors[crHandClosed] := LoadCursor(hInstance, 'HANDCLOSED');
end.

