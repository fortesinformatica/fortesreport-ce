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

{@unit RLHTMLFilter - Implementação do filtro para criação de páginas web. }
unit RLHTMLFilter;

interface

uses
  SysUtils, Classes, Contnrs, Types,
  {$IfDef CLX}
   QGraphics, RLMetaCLX,
  {$Else}
   Graphics, RLMetaVCL,
  {$EndIf}
  RLMetaFile, RLConsts, RLFilters, RLUtils, RLTypes;

type
  {@type TRLHTMLDocumentStyle - Estilo para geração de páginas html.
   Pode ser um dos seguintes valores:
   dsCSS2 - O filtro gera apenas uma página html para todo o relatório (além dos arquivos
   de imagens) que inclui instruções especiais da especificação CSS2 para salto de página;
   dsOldStyle - É gerada uma página html para cada página de relatório. :/}
  TRLHTMLDocumentStyle = (dsCSS2, dsOldStyle);

  {@type TRLHTMLSaveGraphicEvent - Evento para a gravação de imagem em disco em formato
  ou localização alternativas. Implementando este evento vc se responsabiliza por gravar
  a imagem em disco, podendo mudar o formato e o nome do arquivo. :/}
  TRLHTMLSaveGraphicEvent = procedure(Sender: TObject; AGraphic: TGraphic; var FileName: String; var Saved: Boolean) of object;

  { TRLHTMLFilter }

  {@class TRLHTMLFilter - Filtro para criação de páginas html a partir de um relatório.
   @links TRLRichFilter, TRLPDFFilter, TRLXLSFilter. 
   @ancestor TRLCustomSaveFilter.
   @pub }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLHTMLFilter = class(TRLCustomSaveFilter)
  private

    // variables

    FWorkingFileHandle: textfile;
    FGraphIndex: Integer;
    FWorkingFileName: String;
    FDocumentStyle: TRLHTMLDocumentStyle;
    FFileCount: Integer;
    FFileIndex: Integer;
    FFontList: TStringList;
    FPrintCut: TPoint;
    FPrintSize: TPoint;
    FOnSaveGraphic: TRLHTMLSaveGraphicEvent;

    // custom methods

    procedure FontListNeeded;
    function AddFont(AFont: TRLMetaFont): Integer;
    procedure WritePageHeader(var AFile: textfile);
    procedure WritePageFooter(var AFile: textfile);
    procedure WritePageTools(var AFile: textfile);
    function FileNameByIndex(AFileIndex: Integer): String;
    function DoSaveGraphic(AGraphic: TGraphic; var AFileName: String): Boolean;

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
    
    {@prop DocumentStyle - Estilo para a geração das páginas html.
     @links TRLHTMLDocumentStyle. :/}
    property DocumentStyle: TRLHTMLDocumentStyle read FDocumentStyle write FDocumentStyle;

    {@prop FileName = ancestor /}
    property FileName;
    {@prop DisplayName = ancestor /}
    property DisplayName;

    {@event OnSaveGraphic - Ao salvar imagem em disco. Implemente este evento se quiser
    interferir na gravação das imagens em disco. As imagens normalmente são gravadas em
    formato bitmap (.bmp). :/}
    property OnSaveGraphic: TRLHTMLSaveGraphicEvent read FOnSaveGraphic write FOnSaveGraphic;
  end;
  {/@class}
  

{@func ColorToHex - Devolve uma string com a cor em formato RGB-Hexadecimal. :/}
function ColorToHex(AColor: TColor): String;

{@func EncodeISO - Codifica uma string padrão ascii em formato ISO para html. :/}
function EncodeISO(const AStr: String): String;

{/@unit}

implementation

const
  BEGINPAGETAG = '<HTMLFilter.Page>';
  ENDPAGETAG = '</HTMLFilter.Page>';

// UTILS

function ColorToHex(AColor: TColor): String;
type
  TRGBSplit = record
              Red, Green, Blue, Pallete: Byte;
            end;
var
  rgb: record case Byte of
    0: (AsDWORD: DWORD);
    1: (AsSplit: TRGBSplit);
  end;
begin
  rgb.AsDWORD := ColorToRGB(AColor);
  Result := ByteToHex(rgb.AsSplit.Red) + ByteToHex(rgb.AsSplit.Green) + ByteToHex(rgb.AsSplit.Blue); 
end;

function EncodeISO(const AStr: String): String;
type
  TCharAndSymbol = record
    Code: char;
    Symbol: String;
  end;
const
  MAXENCODECHARS = 99;
  ENCODECHARS: array[1..MAXENCODECHARS] of TCharAndSymbol = (
    (Code: #038; Symbol: '&amp;'), // ampersand
    (Code: #062; Symbol: '&gt;'), // greater than
    (Code: #060; Symbol: '&lt;'), // less than
    (Code: #160; Symbol: '&nbsp;'), // no-break space
    (Code: #161; Symbol: '&iexcl;'), // inverted exclamation mark
    (Code: #162; Symbol: '&cent;'), // cent sign
    (Code: #163; Symbol: '&pound;'), // pound sterling sign
    (Code: #164; Symbol: '&curren;'), // general currency sign
    (Code: #165; Symbol: '&yen;'), // yen sign
    (Code: #166; Symbol: '&brvbar;'), // broken (vertical) bar
    (Code: #167; Symbol: '&sect;'), // section sign
    (Code: #168; Symbol: '&uml;'), // umlaut (dieresis)
    (Code: #169; Symbol: '&copy;'), // copyright sign
    (Code: #170; Symbol: '&ordf;'), // ordinal indicator, feminine
    (Code: #171; Symbol: '&laquo;'), // angle quotation mark, left
    (Code: #172; Symbol: '&not;'), // not sign
    (Code: #173; Symbol: '&shy;'), // soft hyphen
    (Code: #174; Symbol: '&reg;'), // registered sign
    (Code: #175; Symbol: '&macr;'), // macron
    (Code: #176; Symbol: '&deg;'), // degree sign
    (Code: #177; Symbol: '&plusmn;'), // plus-or-minus sign
    (Code: #178; Symbol: '&sup2;'), // superscript two
    (Code: #179; Symbol: '&sup3;'), // superscript three
    (Code: #180; Symbol: '&acute;'), // acute accent
    (Code: #181; Symbol: '&micro;'), // micro sign
    (Code: #182; Symbol: '&para;'), // pilcrow (paragraph sign)
    (Code: #183; Symbol: '&middot;'), // middle dot
    (Code: #184; Symbol: '&cedil;'), // cedilla
    (Code: #185; Symbol: '&sup1;'), // superscript one
    (Code: #186; Symbol: '&ordm;'), // ordinal indicator, masculine
    (Code: #187; Symbol: '&raquo;'), // angle quotation mark, right
    (Code: #188; Symbol: '&frac14;'), // fraction one-quarter
    (Code: #189; Symbol: '&frac12;'), // fraction one-half
    (Code: #190; Symbol: '&frac34;'), // fraction three-quarters
    (Code: #191; Symbol: '&iquest;'), // inverted question mark
    (Code: #192; Symbol: '&Agrave;'), // capital A, grave accent
    (Code: #193; Symbol: '&Aacute;'), // capital A, acute accent
    (Code: #194; Symbol: '&Acirc;'), // capital A, circumflex accent
    (Code: #195; Symbol: '&Atilde;'), // capital A, tilde
    (Code: #196; Symbol: '&Auml;'), // capital A, dieresis or umlaut mark
    (Code: #197; Symbol: '&Aring;'), // capital A, ring
    (Code: #198; Symbol: '&AElig;'), // capital AE diphthong (ligature)
    (Code: #199; Symbol: '&Ccedil;'), // capital C, cedilla
    (Code: #200; Symbol: '&Egrave;'), // capital E, grave accent
    (Code: #201; Symbol: '&Eacute;'), // capital E, acute accent
    (Code: #202; Symbol: '&Ecirc;'), // capital E, circumflex accent
    (Code: #203; Symbol: '&Euml;'), // capital E, dieresis or umlaut mark
    (Code: #204; Symbol: '&Igrave;'), // capital I, grave accent
    (Code: #205; Symbol: '&Iacute;'), // capital I, acute accent
    (Code: #206; Symbol: '&Icirc;'), // capital I, circumflex accent
    (Code: #207; Symbol: '&Iuml;'), // capital I, dieresis or umlaut mark
    (Code: #208; Symbol: '&ETH;'), // capital Eth, Icelandic
    (Code: #209; Symbol: '&Ntilde;'), // capital N, tilde
    (Code: #210; Symbol: '&Ograve;'), // capital O, grave accent
    (Code: #211; Symbol: '&Oacute;'), // capital O, acute accent
    (Code: #212; Symbol: '&Ocirc;'), // capital O, circumflex accent
    (Code: #213; Symbol: '&Otilde;'), // capital O, tilde
    (Code: #214; Symbol: '&Ouml;'), // capital O, dieresis or umlaut mark
    (Code: #215; Symbol: '&times;'), // multiply sign
    (Code: #216; Symbol: '&Oslash;'), // capital O, slash
    (Code: #217; Symbol: '&Ugrave;'), // capital U, grave accent
    (Code: #218; Symbol: '&Uacute;'), // capital U, acute accent
    (Code: #219; Symbol: '&Ucirc;'), // capital U, circumflex accent
    (Code: #220; Symbol: '&Uuml;'), // capital U, dieresis or umlaut mark
    (Code: #221; Symbol: '&Yacute;'), // capital Y, acute accent
    (Code: #222; Symbol: '&THORN;'), // capital THORN, Icelandic
    (Code: #223; Symbol: '&szlig;'), // small sharp s, German (sz ligature)
    (Code: #224; Symbol: '&agrave;'), // small a, grave accent
    (Code: #225; Symbol: '&aacute;'), // small a, acute accent
    (Code: #226; Symbol: '&acirc;'), // small a, circumflex accent
    (Code: #227; Symbol: '&atilde;'), // small a, tilde
    (Code: #228; Symbol: '&auml;'), // small a, dieresis or umlaut mark
    (Code: #229; Symbol: '&aring;'), // small a, ring
    (Code: #230; Symbol: '&aelig;'), // small ae diphthong (ligature)
    (Code: #231; Symbol: '&ccedil;'), // small c, cedilla
    (Code: #232; Symbol: '&egrave;'), // small e, grave accent
    (Code: #233; Symbol: '&eacute;'), // small e, acute accent
    (Code: #234; Symbol: '&ecirc;'), // small e, circumflex accent
    (Code: #235; Symbol: '&euml;'), // small e, dieresis or umlaut mark
    (Code: #236; Symbol: '&igrave;'), // small i, grave accent
    (Code: #237; Symbol: '&iacute;'), // small i, acute accent
    (Code: #238; Symbol: '&icirc;'), // small i, circumflex accent
    (Code: #239; Symbol: '&iuml;'), // small i, dieresis or umlaut mark
    (Code: #240; Symbol: '&eth;'), // small eth, Icelandic
    (Code: #241; Symbol: '&ntilde;'), // small n, tilde
    (Code: #242; Symbol: '&ograve;'), // small o, grave accent
    (Code: #243; Symbol: '&oacute;'), // small o, acute accent
    (Code: #244; Symbol: '&ocirc;'), // small o, circumflex accent
    (Code: #245; Symbol: '&otilde;'), // small o, tilde
    (Code: #246; Symbol: '&ouml;'), // small o, dieresis or umlaut mark
    (Code: #247; Symbol: '&divide;'), // divide sign
    (Code: #248; Symbol: '&oslash;'), // small o, slash
    (Code: #249; Symbol: '&ugrave;'), // small u, grave accent
    (Code: #250; Symbol: '&uacute;'), // small u, acute accent
    (Code: #251; Symbol: '&ucirc;'), // small u, circumflex accent
    (Code: #252; Symbol: '&uuml;'), // small u, dieresis or umlaut mark
    (Code: #253; Symbol: '&yacute;'), // small y, acute accent
    (Code: #254; Symbol: '&thorn;'), // small thorn, Icelandic
    (Code: #255; Symbol: '&yuml;')); // small y, dieresis or umlaut mark
var
  I, J: Integer;
begin
  Result := GetAnsiStr( AStr );
  for I := Length(Result) downto 1 do
  begin
    for J := 1 to MAXENCODECHARS do
      if Result[I] = ENCODECHARS[J].Code then
      begin
        Delete(Result, I, 1);
        Insert(ENCODECHARS[J].Symbol, Result, I);
        Break;
      end;
  end;
end;

function EqualRect(const ARect1, ARect2: TRect): Boolean;
begin
  Result := (ARect1.Left = ARect2.Left) and
          (ARect1.Top = ARect2.Top) and
          (ARect1.Right = ARect2.Right) and
          (ARect1.Bottom = ARect2.Bottom);
end;

{ TRLHTMLFilter }

// CONSTRUCTORS & DESTRUCTORS

constructor TRLHTMLFilter.Create(AOwner: TComponent);
begin
  FWorkingFileName := '';
  FDocumentStyle := dsCSS2;
  FFileCount := 0;
  FFileIndex := 0;
  FFontList := nil;
  FOnSaveGraphic := nil;
  //
  inherited;
  //
  DefaultExt := '.htm';
  DisplayName := GetLocalizeStr(LocaleStrings.LS_WebPageStr);
end;

destructor TRLHTMLFilter.Destroy;
begin
  if Assigned(FFontList) then
    FFontList.free;
  //
  inherited;
end;

// CUSTOM

procedure TRLHTMLFilter.FontListNeeded;
begin
  if not Assigned(FFontList) then
    FFontList := TStringList.Create;
end;

function TRLHTMLFilter.AddFont(AFont: TRLMetaFont): Integer;
var
  fontstr: String;
begin
  fontstr := AFont.Name + '|' + IntToStr(AFont.Size) + '|' + ColorToHex(FromMetaColor(AFont.Color));
  Result := FFontList.IndexOf(fontstr);
  if Result = -1 then
    Result := FFontList.Add(fontstr);
end;

// OVERRIDE

procedure TRLHTMLFilter.InternalBeginDoc;
begin
  FPrintCut.X := 0;
  FPrintCut.Y := 0;
  FPrintSize.X := Pages.OrientedWidth;
  FPrintSize.Y := Pages.OrientedHeight;
  //
  FFileCount := 0;
  FFileIndex := 0;
  FGraphIndex := 0;
  FWorkingFileName := GetTempFileName;
  RegisterTempFile(FWorkingFileName);
  //
  AssignFile(FWorkingFileHandle, FWorkingFileName);
  Rewrite(FWorkingFileHandle);
  // demarca inicio da página
  Writeln(FWorkingFileHandle);
  Writeln(FWorkingFileHandle, BEGINPAGETAG);
  //
  FontListNeeded;
end;

procedure TRLHTMLFilter.InternalEndDoc;
var
  S: String;
  F: textfile;
  N: String;
begin
  // demarca final da página
  Writeln(FWorkingFileHandle);
  Writeln(FWorkingFileHandle, ENDPAGETAG);
  Inc(FFileCount);
  CloseFile(FWorkingFileHandle);
  //
  N := '';
  Reset(FWorkingFileHandle);
  try
    while not Eof(FWorkingFileHandle) do
    begin
      ReadLn(FWorkingFileHandle, S);
      if S = BEGINPAGETAG then
        if (FDocumentStyle <> dsCSS2) or (FFileIndex = 0) then
        begin
          N := FileNameByIndex(FFileIndex);
          AssignFile(F, N);
          Rewrite(F);
          WritePageHeader(F);
        end
        else
      else if S = ENDPAGETAG then
      begin
        WritePageTools(F);
        if (FDocumentStyle <> dsCSS2) or (FFileIndex = FFileCount - 1) then
        begin
          WritePageFooter(F);
          CloseFile(F);
          N := '';
        end; 
        Inc(FFileIndex);
      end
      else if N <> '' then
        Writeln(F, S);
    end;
  finally
    CloseFile(FWorkingFileHandle);
    SysUtils.DeleteFile(FWorkingFileName);
    UnregisterTempFile(FWorkingFileName);
  end;
end;

procedure TRLHTMLFilter.InternalNewPage;
begin
  // demarca final da página
  Writeln(FWorkingFileHandle);
  Writeln(FWorkingFileHandle, ENDPAGETAG);
  Inc(FFileCount);
  // demarca inicio da página
  Writeln(FWorkingFileHandle);
  Writeln(FWorkingFileHandle, BEGINPAGETAG);
end;

procedure TRLHTMLFilter.InternalDrawPage(APage: TRLGraphicSurface);
var
  obj: TRLGraphicObject;
  bmpcalc: TBitmap;
  cliprct: TRect;
  I: Integer;
  function TextWidth(const AText: String; AFont: TRLMetaFont): Integer;
  begin
    with bmpcalc.Canvas do
    begin
      Font.Name := AFont.Name;
      Font.Pitch := FromMetaFontPitch(AFont.Pitch);
      Font.Size := AFont.Size;
      Font.Style := FromMetaFontStyles(AFont.Style);
      Result := TextWidth(AText);
    end;
  end;
  function ClipStr(const AInRect, AOuterRect: TRect): String; overload;
  var
    cut: TRect;
  begin
    IntersectRect(cut, AInRect, AOuterRect);
    if not EqualRect(cut, AInRect) then
      Result := 'clip:rect(' + IntToStr(cut.Top - AInRect.Top) + 'px ' + 
                           IntToStr(cut.Right - AInRect.Left) + 'px ' + 
                           IntToStr(cut.Bottom - AInRect.Top) + 'px ' + 
                           IntToStr(cut.Left - AInRect.Left) + 'px)' // top right bottom left
    else
      Result := '';
  end;
  function ClipStr(const AInPoint: TPoint; const AOuterRect: TRect): String; overload;
  var
    inrect: TRect;
  begin
    inrect.Left := AInPoint.X;
    inrect.Top := AInPoint.Y;
    inrect.Right := AOuterRect.Right;
    inrect.Bottom := AOuterRect.Bottom;
    Result := ClipStr(inrect, AOuterRect);
  end;
  procedure DrawPixel(AObj: TRLPixelObject);
  begin
    Write(FWorkingFileHandle, '<hr style="position:absolute;' + 
                             'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                             'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                             'width:1px;' + 
                             'height:1px;' + 
                             ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + ';' + 
                             'color:#' + ColorToHex(FromMetaColor(AObj.Color)) + ';">');
    Writeln(FWorkingFileHandle);
  end;
  procedure DrawLine(AObj: TRLLineObject);
  begin
    Write(FWorkingFileHandle, '<hr style="position:absolute;' + 
                             'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                             'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                             'width:' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + 'px;' + 
                             'height:' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + 'px;' + 
                             ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + ';' + 
                             'color:#' + ColorToHex(FromMetaColor(AObj.Pen.Color)) + ';">');
    Writeln(FWorkingFileHandle);
  end;
  procedure DrawRectangle(AObj: TRLRectangleObject);
  begin
    Write(FWorkingFileHandle, '<hr style="position:absolute;' + 
                             'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                             'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                             'width:' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + 'px;' + 
                             'height:' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + 'px;' + 
                             ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + ';' + 
                             'color:#' + ColorToHex(FromMetaColor(AObj.Brush.Color)) + ';">');
    Writeln(FWorkingFileHandle);
  end;
  procedure DrawText(AObj: TRLTextObject);
  begin
    // cor de fundo
    if AObj.Brush.Style <> MetaBrushStyleClear then
      Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                               'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                               'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                               'width:' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + 'px;' + 
                               'height:' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + 'px;' + 
                               ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + ';' + 
                               'background-color:#' + ColorToHex(FromMetaColor(AObj.Brush.Color)) + ';"></div>');
    // início de layer
    if (AObj.TextFlags and MetaTextFlagAutoSize) = MetaTextFlagAutoSize then
      Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                               'left:' + IntToStr(AObj.Origin.X - FPrintCut.X) + 'px;' + 
                               'top:' + IntToStr(AObj.Origin.Y - FPrintCut.Y) + 'px;' + 
                               ClipStr(FromMetaPoint(AObj.Origin), cliprct) + '">')
    else
      case AObj.Alignment of
        MetaTextAlignmentCenter: Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                                                          'left:' + IntToStr((AObj.BoundsRect.Right + AObj.BoundsRect.Left - TextWidth(AObj.DisplayText, AObj.Font)) div 2) + 'px;' + 
                                                          'top:' + IntToStr(AObj.Origin.Y - FPrintCut.Y) + 'px;' + 
                                                          ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + '">');
        MetaTextAlignmentRight: Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                                                          'left:' + IntToStr(AObj.BoundsRect.Right - TextWidth(AObj.DisplayText, AObj.Font)) + 'px;' + 
                                                          'top:' + IntToStr(AObj.Origin.Y - FPrintCut.Y) + 'px;' + 
                                                          ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + '">');
      else //TextAlignmentLeftJustify
        Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                                 'left:' + IntToStr(AObj.Origin.X - FPrintCut.X) + 'px;' + 
                                 'top:' + IntToStr(AObj.Origin.Y - FPrintCut.Y) + 'px;' + 
                                 ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + '">');
      end;
    // seleciona fonte
    Write(FWorkingFileHandle, '<font class="f' + IntToStr(AddFont(AObj.Font)) + '">');
    // efeitos
    if (AObj.Font.Style and MetaFontStyleBold) = MetaFontStyleBold then
      Write(FWorkingFileHandle, '<b>');
    if (AObj.Font.Style and MetaFontStyleItalic) = MetaFontStyleItalic then
      Write(FWorkingFileHandle, '<i>');
    if (AObj.Font.Style and MetaFontStyleUnderline) = MetaFontStyleUnderline then
      Write(FWorkingFileHandle, '<u>');
    if (AObj.Font.Style and MetaFontStyleStrikeOut) = MetaFontStyleStrikeOut then
      Write(FWorkingFileHandle, '<strike>');
    // o texto
    Write(FWorkingFileHandle, EncodeISO(AObj.DisplayText));
    // retorna fonte
    if (AObj.Font.Style and MetaFontStyleStrikeOut) = MetaFontStyleStrikeOut then
      Write(FWorkingFileHandle, '</strike>');
    if (AObj.Font.Style and MetaFontStyleUnderline) = MetaFontStyleUnderline then
      Write(FWorkingFileHandle, '</u>');
    if (AObj.Font.Style and MetaFontStyleItalic) = MetaFontStyleItalic then
      Write(FWorkingFileHandle, '</i>');
    if (AObj.Font.Style and MetaFontStyleBold) = MetaFontStyleBold then
      Write(FWorkingFileHandle, '</b>');
    Write(FWorkingFileHandle, '</font>');
    // fim de layer
    Write(FWorkingFileHandle, '</div>');
    Writeln(FWorkingFileHandle);
  end;
  procedure DrawFillRect(AObj: TRLFillRectObject);
  begin
    if AObj.Brush.Style = MetaBrushStyleClear then
      Exit;
    Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                             'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                             'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                             'width:' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + 'px;' + 
                             'height:' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + 'px;' + 
                             ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + ';' + 
                             'background-color:#' + ColorToHex(FromMetaColor(AObj.Brush.Color)) + '"></div>');
    Writeln(FWorkingFileHandle);
  end;
  procedure DrawEllipse(AObj: TRLEllipseObject);
  begin
    ///
  end;
  procedure DrawPolygon(AObj: TRLPolygonObject);
  begin
    ///
  end;
  procedure DrawPolyline(AObj: TRLPolylineObject);
  begin
    ///
  end;
  procedure DrawImage(AObj: TRLImageObject);
  var
    graphfn: String;
    graph: TGraphic;
  begin
    /// verificar repetição de arq de imagens aqui (crc?)
    graph := FromMetaGraphic(AObj.Data);
    if Assigned(graph) then
      try
        if DoSaveGraphic(graph, graphfn) then
        begin
          Write(FWorkingFileHandle, '<div style="position:absolute;' + 
                                   'left:' + IntToStr(AObj.BoundsRect.Left - FPrintCut.X) + 'px;' + 
                                   'top:' + IntToStr(AObj.BoundsRect.Top - FPrintCut.Y) + 'px;' + 
                                   'width:' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + 'px;' + 
                                   'height:' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + 'px;' + 
                                   ClipStr(FromMetaRect(AObj.BoundsRect), cliprct) + '">');
          Write(FWorkingFileHandle, '<img src="' + ExtractFileName(graphfn) + '" ' + 
                                   'width=' + IntToStr(AObj.BoundsRect.Right - AObj.BoundsRect.Left) + ' ' + 
                                   'height=' + IntToStr(AObj.BoundsRect.Bottom - AObj.BoundsRect.Top) + '>');
          Write(FWorkingFileHandle, '</div>');
          Writeln(FWorkingFileHandle);
        end; 
      finally
        graph.free;
      end;
  end;
  procedure DrawSetClipRect(AObj: TRLSetClipRectObject);
  begin
    cliprct := FromMetaRect(AObj.BoundsRect);
  end;
  procedure DrawResetClipRect(AObj: TRLResetClipRectObject);
  begin
    cliprct := Rect(0, 0, FPrintSize.X, FPrintSize.Y);
  end;
begin
  Writeln(FWorkingFileHandle, '<div style="position:relative;' + 
                             'width:' + IntToStr(FPrintSize.X) + 'px;' + 
                             'height:' + IntToStr(FPrintSize.Y) + 'px;' + 
                             'background-color:#FFFFFF;">');
  cliprct := Rect(0, 0, FPrintSize.X, FPrintSize.Y);
  bmpcalc := NeedAuxBitmap;
  // grava tags
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
  Writeln(FWorkingFileHandle, '</div>');
end;

procedure TRLHTMLFilter.WritePageHeader(var AFile: textfile);
var
  I, P: Integer;
  S, fn, fz, fc: String;
begin
  // cabeçalho do documento
  Writeln(AFile, '<html>');
  Writeln(AFile);
  Writeln(AFile, '<head>');
  Writeln(AFile, '<meta http-equiv="Content-Type" content="text/html" charset="iso-8859-1">');
  Writeln(AFile, '<meta name="GENERATOR" content="' + CS_ProductTitleStr + ' ' + CS_Version + '">');
  if Pages.Title <> '' then
    Writeln(AFile, '<title>' + Pages.Title + '</title>');
  Writeln(AFile, '<style>');
  //
  if FDocumentStyle = dsCSS2 then
  begin
    Writeln(AFile, '@media print');
    Writeln(AFile, '{');
    Writeln(AFile, '.nonprintable {display:none;}');
    Writeln(AFile, '}');
  end;
  // write fonts
  for I := 0 to FFontList.Count - 1 do
  begin
    S := FFontList[I];
    P := Pos('|', S); if P = 0 then P := Length(S) + 1; fn := Copy(S, 1, P - 1); Delete(S, 1, P);
    P := Pos('|', S); if P = 0 then P := Length(S) + 1; fz := Copy(S, 1, P - 1); Delete(S, 1, P);
    P := Pos('|', S); if P = 0 then P := Length(S) + 1; fc := Copy(S, 1, P - 1); Delete(S, 1, P);
    Writeln(AFile, 'font.f' + IntToStr(I) + ' {font-family:"' + fn + '"; font-size:' + fz + 'pt; color:#' + fc + '}');
  end;
  //
  Writeln(AFile, '</style>');
  Writeln(AFile, '</head>');
  Writeln(AFile);
  Writeln(AFile, '<body>');
  Writeln(AFile);
end;

procedure TRLHTMLFilter.WritePageFooter(var AFile: textfile);
begin
  // fim de documento
  Writeln(AFile, '</body>');
  Writeln(AFile);
  Writeln(AFile, '</html>');
end;

procedure TRLHTMLFilter.WritePageTools(var AFile: textfile);
var
  I, Q, F: Integer;
begin
  case FDocumentStyle of
    dsCSS2: if FFileIndex < FFileCount - 1 then
                  Writeln(AFile, '<div style="position:relative; page-break-after:always"><br></div>');
    dsOldStyle: begin
                  Writeln(AFile, '<div style="width:' + IntToStr(FPrintSize.X) + 'px; height:20px">');
                  Writeln(AFile, '<table width="100%" height="100%">');
                  Writeln(AFile, '<tr>');
                  Writeln(AFile, '<td align="center">');
                  if FFileIndex > 0 then
                  begin
                    Writeln(AFile, '<a href="' + FileNameByIndex(0) + '">' + EncodeISO('|<') + '</a>');
                    Writeln(AFile, '<a href="' + FileNameByIndex(FFileIndex - 1) + '">' + EncodeISO('<') + '</a>');
                  end;
                  //
                  Q := 20;
                  I := FFileIndex - Q;
                  if I < 0 then
                    I := 0;
                  if I + Q > FFileCount - 1 then
                    Q := FFileCount - 1 - I;
                  F := I + Q;
                  while I < F do
                  begin
                    if I = FFileIndex then
                      Writeln(AFile, IntToStr(I + 1))
                    else
                      Writeln(AFile, '<a href="' + FileNameByIndex(I) + '">' + IntToStr(I + 1) + '</a>');
                    Inc(I);
                  end;
                  //
                  if FFileIndex < FFileCount - 1 then
                  begin
                    Writeln(AFile, '<a href="' + FileNameByIndex(FFileIndex + 1) + '">' + EncodeISO('>') + '</a>');
                    Writeln(AFile, '<a href="' + FileNameByIndex(FFileCount - 1) + '">' + EncodeISO('>|') + '</a>');
                  end;
                  Writeln(AFile, '</td>');
                  Writeln(AFile, '</tr>');
                  Writeln(AFile, '</table>');
                  Writeln(AFile, '</div>');
                end;
  end;
end;

function TRLHTMLFilter.FileNameByIndex(AFileIndex: Integer): String;
var
  E: String;
begin
  if AFileIndex = 0 then
    Result := FileName
  else
  begin
    E := ExtractFileExt(FileName);
    Result := Copy(FileName, 1, Length(FileName) - Length(E)) + IntToStr(AFileIndex) + E;
  end;
end;

function TRLHTMLFilter.DoSaveGraphic(AGraphic: TGraphic; var AFileName: String): Boolean;
var
  graphext: String;
begin
  if AGraphic is TBitmap then
    graphext := 'bmp'
  else if AGraphic is TIcon then 
    graphext := 'ico'
  else
    graphext := '';
  Inc(FGraphIndex);
  AFileName := FileNameByIndex(FFileIndex);
  AFileName := Copy(AFileName, 1, Length(AFileName) - Length(ExtractFileExt(AFileName))) + '-img' + IntToStr(FGraphIndex) + '.' + graphext;
  if Assigned(FOnSaveGraphic) then
  begin
    Result := True;
    FOnSaveGraphic(Self, AGraphic, AFileName, Result);
  end
  else if graphext = '' then
    Result := False
  else 
  begin
    AGraphic.SaveToFile(AFileName);
    Result := True;
  end;
end;

end.

