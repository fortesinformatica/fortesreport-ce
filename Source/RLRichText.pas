{@unit RLRichText - Implementação dos componentes de impressão de texto no formato RichText.}
unit RLRichText;

interface

uses
  Classes, SysUtils, Contnrs, Math, 
{$ifndef LINUX}
  Windows, 
{$else}
  Types, 
{$endif}
{$ifdef CLX}
  QGraphics, RLMetaCLX, 
{$else}
  Graphics, RLMetaVCL, 
{$endif}
  RLReport, RLUtils, RLMetaFile;

type
  { TRLCustomRichText }

  {@class TRLCustomRichText - Classe base para caixa de texto formato RichText. }
  TRLCustomRichText = class(TRLCustomMemo)
  private
    // custom methods
    procedure CustomRichTextCalcSize(var ASize: TPoint);
    procedure CustomRichTextInternalPrint;
    procedure CustomRichTextPaint;
  protected
    // override & reintroduce
    procedure CalcSize(var ASize: TPoint); override;
    function InternalMakeCaption: String; override;
    procedure InternalPrint; override;
  public
    // constructors & destructors
    constructor Create(AOwner: TComponent); override;
    //
    procedure Paint; override;
  end;
  {/@class}
  

  { TRLCustomDBRichText }

  {@class TRLCustomDBRichText - Classe base para caixa de texto formato RichText ligado a campo de dataset. }
  TRLCustomDBRichText = class(TRLCustomDBMemo)
  protected
    // override & reintroduce
    procedure CalcSize(var ASize: TPoint); override;
    function InternalMakeCaption: String; override;
    procedure InternalPrint; override;
  public
    // constructors & destructors
    constructor Create(AOwner: TComponent); override;
    //
    procedure Paint; override;
  end;
  {/@class}
  

  { TRLRichText }

  {@class TRLRichText - Componente para texto multilinhas em formato RichText. 
   @pub }
  TRLRichText = class(TRLCustomRichText)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
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
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop Lines = ancestor /}
    property Lines;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop WordWrap = ancestor /}
    property WordWrap;

    // events

    {@prop AfterPrint = ancestor /}
    property AfterPrint;
    {@prop BeforePrint = ancestor /}
    property BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property OnMeasureHeight;
  end;
  {/@class}
  

  { TRLDBRichText }

  {@class TRLDBRichText - Componente para texto multilinhas em formato RichText ligado a campo de dataset.
   @pub }
  TRLDBRichText = class(TRLCustomDBRichText)
  published

    // properties

    {@prop Align = ancestor /}
    property Align;
    {@prop Alignment = ancestor /}
    property Alignment;
    {@prop Anchors = ancestor /}
    property Anchors;
    {@prop AutoSize = ancestor /}
    property AutoSize;
    {@prop Behavior = ancestor /}
    property Behavior;
    {@prop Borders = ancestor /}
    property Borders;
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
    {@prop IntegralHeight = ancestor /}
    property IntegralHeight;
    {@prop Layout = ancestor /}
    property Layout;
    {@prop ParentColor = ancestor /}
    property ParentColor;
    {@prop ParentFont = ancestor /}
    property ParentFont;
    {@prop RealBounds = ancestor /}
    property RealBounds;
    {@prop SecondHolder = ancestor /}
    property SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property Transparent;
    {@prop Visible = ancestor /}
    property Visible;
    {@prop WordWrap = ancestor /}
    property WordWrap;

    // events

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

type
  TRichFont = class
  public
    Name: String;
    Charset: TFontCharset;
  end;

  TRichRGB = class
  public
    Red: Byte;
    Green: Byte;
    Blue: Byte;
  end;
  
  TRichAction = (raMeasure, raDraw);

procedure RichTextParse(const ARichText: string; ACanvas: TObject; ARect: TRect; AWordWrap: Boolean; var ASize: TPoint; AAction: TRichAction);
const
  NULLSET = [#9, #13, #10, #26];
  SPACESET = NULLSET + [#32];
  CTRLSET = ['{', '}', '\', ';'];
  DIGITSET = ['0'..'9'];
  ALPHASET = ['a'..'z', 'A'..'Z', '_'];
  ALPHASETEX = ALPHASET + DIGITSET;
  TEXTSET = [#32..#126, #128..#255] - CTRLSET - NULLSET;
const
  MaxLevels = 255;
type
  TLevelSet = record
    Alignment: TRLMetaTextAlignment;
  end;
type
  TRichTokenKind = (tkEof, tkNull, tkIndent, tkUnindent, tkEscape, tkEndItem, tkLiteral);
var
  Token: String;
  TokenText: String;
  Escape: String;
  EscapeParam: Integer;
  TokenKind: TRichTokenKind;
  FontList: TObjectList;
  Font: TRichFont;
  RGBList: TObjectList;
  RGB: TRichRGB;
  TextPos: TPoint;
  LineHeight: Integer;
  TextWidth: Integer;
  TextCut: Integer;
  Level: Integer;
  LevelSets: array[0..MaxLevels - 1] of TLevelSet;
  PackedRGB: TRLMetaColor;
var
  Buffer: pchar;
  RefusedCh: array[1..255] of char;
  RefusedChCount: Byte;
  RefusedToken: String;
  RefusedTokenKind: TRichTokenKind;
function NextCh(var ACh: char): Boolean;
begin
  if RefusedChCount > 0 then
  begin
    ACh := RefusedCh[RefusedChCount];
    Dec(RefusedChCount);
  end
  else if Boolean(Buffer^) then
  begin
    ACh := Buffer^;
    Inc(Buffer);
  end
  else
    ACh := #0;
  Result := (ACh <> #0);
end;
function NextValidCh(var ACh: char): Boolean;
begin
  while NextCh(ACh) and CharInSet(ACh, NULLSET) do;
  Result := (ACh <> #0);
end;
procedure RefuseCh(const ACh: String);
var
  I: Integer;
begin
  if ACh <> '' then
    for I := Length(ACh) downto 1 do
      if ACh[I] <> #0 then
      begin
        Inc(RefusedChCount);
        RefusedCh[RefusedChCount] := ACh[I];
      end;
end;
function NextToken(var AToken: String; var ATokenKind: TRichTokenKind): Boolean;
var
  ch: char;
  h1, h2: char;
begin
  if RefusedToken <> '' then
  begin
    AToken := RefusedToken;
    ATokenKind := RefusedTokenKind;
    RefusedToken := '';
    RefusedTokenKind := tkEof;
  end
  else
  begin
    AToken := '';
    ATokenKind := tkEof;
    if NextValidCh(ch) then
      if ch = '{' then
      begin
        AToken := ch;
        ATokenKind := tkIndent;
      end
      else if ch = '}' then
      begin
        AToken := ch;
        ATokenKind := tkUnindent;
      end
      else if ch = ';' then
      begin
        AToken := ch;
        ATokenKind := tkEndItem;
      end
      else if ch = '\' then
        if NextCh(ch) then
          if ch = '''' then
            if NextCh(h1) and NextCh(h2) then
            begin
              AToken := AnsiChar(HexToByte(h1 + h2));
              ATokenKind := tkLiteral;
            end
            else
          else if CharInSet(ch, ALPHASET) then
          begin
            ATokenKind := tkEscape;
            repeat
              AToken := AToken + ch;
            until not NextCh(ch) or not CharInSet(ch, ALPHASETEX);
            if ch <> #32 then
              RefuseCh(ch);
          end
          else
            ATokenKind := tkNull
        else
      else if CharInSet(ch, TEXTSET) then
      begin
        ATokenKind := tkLiteral;
        repeat
          AToken := AToken + ch;
        until not NextCh(ch) or not CharInSet(ch, TEXTSET);
        RefuseCh(ch);
      end
      else
        ATokenKind := tkNull;
  end;
  Result := (ATokenKind <> tkEof);
end;
procedure RefuseToken(const AToken: String; ATokenKind: TRichTokenKind);
begin
  if not (ATokenKind in [tkEof, tkNull]) then
  begin
    RefusedToken := AToken;
    RefusedTokenKind := ATokenKind;
  end;
end;
procedure SplitEscape(const AToken: String; var AEscape: String; var AEscapeParam: Integer);
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(AToken)) and not CharInSet(AToken[I], DIGITSET) do
    Inc(I);
  AEscape := Copy(AToken, 1, I - 1);
  AEscapeParam := StrToIntDef(Copy(AToken, I, Length(AToken)), 0);
end;
procedure SetBrushStyle(AStyle: TBrushStyle);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Brush.Style := AStyle
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Brush.Style := AStyle;
end;
procedure SetFontName(const AName: String);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Font.Name := AName
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Font.Name := AName;
end;
procedure SetFontCharset(ACharset: TFontCharset);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Font.Charset := ACharset
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Font.Charset := ACharset;
end;
function GetFontName: String;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).Font.Name
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).Font.Name;
end;
function GetFontCharset: TFontCharset;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).Font.Charset
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).Font.Charset
  else
    FillChar(Result, SizeOf(Result), 0); 
end;
function GetFontColor: TColor;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).Font.Color
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).Font.Color
  else
    Result := 0;
end;
procedure SetFontColor(AColor: TColor);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Font.Color := AColor
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Font.Color := AColor;
end;
function GetFontStyle: TFontStyles;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).Font.Style
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).Font.Style;
end;
procedure SetFontStyle(AStyle: TFontStyles);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Font.Style := AStyle
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Font.Style := AStyle;
end;
procedure SetFontSize(ASize: Integer);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).Font.Size := ASize
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).Font.Size := ASize;
end;
function GetTextWidth(const AText: String): Integer;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).TextWidth(AText)
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).TextWidth(AText)
  else
    Result := 0;
end;
function GetTextHeight(const AText: String): Integer;
begin
  if ACanvas is TCanvas then
    Result := TCanvas(ACanvas).TextHeight(AText)
  else if ACanvas is TRLGraphicSurface then
    Result := TRLGraphicSurface(ACanvas).TextHeight(AText)
  else
    Result := 0;
end;
procedure TextOut(AX, AY: Integer; const AText: AnsiString);
begin
  if ACanvas is TCanvas then
    TCanvas(ACanvas).TextOut(AX, AY, AText)
  else if ACanvas is TRLGraphicSurface then
    TRLGraphicSurface(ACanvas).TextOut(AX, AY, AText);
end;
begin
  ASize.X := 0;
  ASize.Y := 0;
  Buffer := @ARichText[1];
  RefusedChCount := 0;
  RefusedToken := '';
  //
  TextPos := ARect.TopLeft;
  LineHeight := 0;
  Level := 0;
  LevelSets[Level].Alignment := MetaTextAlignmentLeft;
  //
  try
    FontList := TObjectList.Create;
    try
      RGBList := TObjectList.Create;
      try
        SetBrushStyle(FromMetaBrushStyle(MetaBrushStyleClear));
        while NextToken(Token, TokenKind) do
          case TokenKind of
            tkIndent: begin
                          Inc(Level);
                          LevelSets[Level] := LevelSets[Level - 1];
                        end;
            tkUnindent: begin
                          Dec(Level);
                          if Level <= 0 then
                            Break;
                        end;
            tkEscape: begin
                          SplitEscape(Token, Escape, EscapeParam);
                          if Escape = 'fonttbl' then
                          begin
                            FontList.Clear;
                            Font := nil;
                            while NextToken(Token, TokenKind) do
                              case TokenKind of
                                tkIndent: while NextToken(Token, TokenKind) do
                                              case TokenKind of
                                                tkUnindent: Break;
                                                tkEscape: begin
                                                             SplitEscape(Token, Escape, EscapeParam);
                                                             if Escape = 'f' then
                                                             begin
                                                               Font := TRichFont.Create;
                                                               Font.Name := GetFontName;
                                                               Font.Charset := GetFontCharset;
                                                               while EscapeParam > FontList.Count do
                                                                 FontList.Add(nil);
                                                               FontList.Add(Font);
                                                             end
                                                             else if Escape = 'fnil' then
                                                             else if Escape = 'fcharset' then
{$ifndef CLX}
                                                               Font.Charset := EscapeParam
{$endif}
                                                             else
                                                           end;
                                                tkEndItem: Font := nil;
                                                tkLiteral: Font.Name := Token;
                                              end;
                                tkUnindent: Break;
                              end;
                          end
                          else if Escape = 'colortbl' then
                          begin
                            RGBList.Clear;
                            RGB := TRichRGB.Create;
                            try
                              PackedRGB := ToMetaColor(GetFontColor);
                              RGB.Red := PackedRGB.Red;
                              RGB.Green := PackedRGB.Green;
                              RGB.Blue := PackedRGB.Blue;
                              while NextToken(Token, TokenKind) do
                                case TokenKind of
                                  tkUnindent: Break;
                                  tkEscape: begin
                                                SplitEscape(Token, Escape, EscapeParam);
                                                if Escape = 'red' then
                                                  RGB.Red := EscapeParam
                                                else if Escape = 'green' then
                                                  RGB.Green := EscapeParam
                                                else if Escape = 'blue' then
                                                  RGB.Blue := EscapeParam;
                                              end;
                                  tkEndItem: begin
                                                RGBList.Add(RGB);
                                                RGB := TRichRGB.Create;
                                                RGB.Red := PackedRGB.Red;
                                                RGB.Green := PackedRGB.Green;
                                                RGB.Blue := PackedRGB.Blue;
                                              end;
                                end;
                            finally
                              RGB.free;
                            end;
                          end
                          else if Escape = 'par' then
                          begin
                            Inc(TextPos.Y, LineHeight);
                            TextPos.X := ARect.Left;
                            LineHeight := 0;
                          end
                          else if Escape = 'pard' then
                          begin
                            Inc(TextPos.Y, LineHeight);
                            TextPos.X := ARect.Left;
                            LineHeight := 0;
                            LevelSets[Level].Alignment := MetaTextAlignmentLeft;
                          end
                          else if Escape = 'f' then
                            with TRichFont(FontList[EscapeParam]) do
                            begin
                              SetFontName(Name);
                              SetFontCharset(Charset);
                            end
                          else if Token = 'b' then
                            SetFontStyle(GetFontStyle + [fsBold])
                          else if Token = 'b0' then
                            SetFontStyle(GetFontStyle - [fsBold])
                          else if Token = 'i' then
                            SetFontStyle(GetFontStyle + [fsItalic])
                          else if Token = 'i0' then
                            SetFontStyle(GetFontStyle - [fsItalic])
                          else if Token = 'ul' then
                            SetFontStyle(GetFontStyle + [fsUnderline])
                          else if Token = 'ulnone' then
                            SetFontStyle(GetFontStyle - [fsUnderline])
                          else if Escape = 'fs' then
                            SetFontSize(EscapeParam div 2)
                          else if Escape = 'cf' then
                            with TRichRGB(RGBList[EscapeParam]) do
                              SetFontColor(FromMetaColor(MetaColor(Red, Green, Blue)))
                          else if Escape = 'ql' then
                            LevelSets[Level].Alignment := MetaTextAlignmentLeft
                          else if Escape = 'qr' then
                            LevelSets[Level].Alignment := MetaTextAlignmentRight
                          else if Escape = 'qj' then
                            LevelSets[Level].Alignment := MetaTextAlignmentJustify
                          else if Escape = 'qc' then
                            LevelSets[Level].Alignment := MetaTextAlignmentCenter;
                        end;
            tkEndItem: ;
            tkLiteral: begin
                          TokenText := Token;
                          while NextToken(Token, TokenKind) and (TokenKind = tkLiteral) do
                            TokenText := TokenText + Token;
                          RefuseToken(Token, TokenKind);
                          while TokenText <> '' do
                          begin
                            TextCut := Length(TokenText);
                            TextWidth := GetTextWidth(TokenText);
                            if AWordWrap then
                            begin
                              while (TextCut > 0) and (TextPos.X + TextWidth > ARect.Right) do
                              begin
                                while (TextCut > 0) and not CharInSet(TokenText[TextCut], SPACESET) do
                                  Dec(TextCut);
                                while (TextCut > 0) and CharInSet(TokenText[TextCut], SPACESET) do
                                  Dec(TextCut);
                                TextWidth := GetTextWidth(Copy(TokenText, 1, TextCut));
                              end;
                              if TextCut <= 0 then
                              begin
                                TextCut := Length(TokenText);
                                TextWidth := GetTextWidth(TokenText);
                              end;
                            end;
                            case LevelSets[Level].Alignment of
                              MetaTextAlignmentLeft: ;
                              MetaTextAlignmentRight: TextPos.X := ARect.Right - TextWidth;
                              MetaTextAlignmentJustify: ;
                              MetaTextAlignmentCenter: TextPos.X := (ARect.Left + ARect.Right - TextWidth) div 2;
                            end;
                            TextOut(TextPos.X, TextPos.Y, Copy(TokenText, 1, TextCut));
                            Inc(TextPos.X, TextWidth);
                            ASize.X := Max(ASize.X, TextPos.X);
                            LineHeight := Max(LineHeight, GetTextHeight(TokenText));
                            while (TextCut < Length(TokenText)) and CharInSet(TokenText[TextCut + 1], SPACESET) do
                              Inc(TextCut);
                            Delete(TokenText, 1, TextCut);
                            if (TokenText <> '') or (LevelSets[Level].Alignment <> MetaTextAlignmentLeft) then
                            begin
                              Inc(TextPos.Y, LineHeight);
                              TextPos.X := ARect.Left;
                              LineHeight := 0;
                            end;
                          end;
                        end;
          end;
      finally
        RGBList.free;
      end;
    finally
      FontList.free;
    end;
  except
  end;
  ASize.Y := TextPos.Y + LineHeight;
end;

function RichTextBounds(const ARichText: String; ARect: TRect; AWordWrap: Boolean): TPoint;
var
  bmp: TBitmap;
begin
  bmp := NeedAuxBitmap;
  RichTextParse(ARichText, bmp.Canvas, ARect, AWordWrap, Result, raMeasure);
end;

procedure RichTextDraw(const ARichText: String; ACanvas: TObject; ARect: TRect; AWordWrap: Boolean);
var
  foo: TPoint;
begin
  RichTextParse(ARichText, ACanvas, ARect, AWordWrap, foo, raDraw);
end;

{ TRLCustomRichText }

constructor TRLCustomRichText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TRLCustomRichText.CustomRichTextCalcSize(var ASize: TPoint);
var
  W: Integer;
  C: String;
begin
  ASize := Point(Width, Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  C := Caption;
  if (C = '') and not IsPreparing then
    C := Name;
  // dimensões do texto
  ASize.Y := RichTextBounds(C, Rect(0, 0, Width, Height), WordWrap).Y;
  // adicional das bordas
  W := Borders.Width;
  if W > 0 then
  begin
    Inc(W);
    if Borders.CanDrawTop then
      Inc(ASize.Y, W);
    if Borders.CanDrawBottom then
      Inc(ASize.Y, W);
  end;
end;

procedure TRLCustomRichText.CalcSize(var ASize: TPoint);
begin
  CustomRichTextCalcSize(ASize);
end;

function TRLCustomRichText.InternalMakeCaption: String;
begin
  Result := Lines.Text;
end;

procedure TRLCustomRichText.CustomRichTextInternalPrint;
var
  R: TRect;
begin
  CustomControlPrint;
  R := CalcPrintClientRect;
  RichTextDraw(Caption, RequestParentSurface, R, WordWrap);
end;

procedure TRLCustomRichText.InternalPrint;
begin
  CustomRichTextInternalPrint;
end;

procedure TRLCustomRichText.CustomRichTextPaint;
var
  R: TRect;
begin
  CustomControlPaint;
  R := GetClientRect;
  RichTextDraw(Caption, Canvas, R, WordWrap);
end;

procedure TRLCustomRichText.Paint;
begin
  CustomRichTextPaint;
end;

{ TRLCustomDBRichText }

constructor TRLCustomDBRichText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TRLCustomDBRichText.CalcSize(var ASize: TPoint);
begin
  TRLCustomRichText(Self).CustomRichTextCalcSize(ASize);
end;

function TRLCustomDBRichText.InternalMakeCaption: String;
begin
  Result := GetFieldText;
end;

procedure TRLCustomDBRichText.InternalPrint;
begin
  TRLCustomRichText(Self).CustomRichTextInternalPrint;
end;

procedure TRLCustomDBRichText.Paint;
begin
  TRLCustomRichText(Self).CustomRichTextPaint;
end;

end.

