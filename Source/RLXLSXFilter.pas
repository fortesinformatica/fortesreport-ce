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

{$IFDEF DELPHI15_UP}
 {$DEFINE HAS_FORMATSETTINGS}
{$ENDIF}
{$IfDef FPC}
 {$DEFINE HAS_FORMATSETTINGS}
{$EndIf}

unit RLXLSXFilter;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  SysUtils, StrUtils, Classes, Contnrs, Math, DateUtils,
  {$IfDef CLX}
   Types, QGraphics, RLMetaCLX,
  {$Else}
   Graphics, RLMetaVCL,
  {$EndIf}
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFilters, RlCompilerConsts,
  RLXLSXFileFormat;

type
  TRLXLSXCellArea = record
    X0, Y0, X1, Y1: Integer;
  end;

  TRLXLSXFilterOption = (xfoFindValueCells, xfoOneSheetPerPage);
  TRLXLSXFilterOptions = set of TRLXLSXFilterOption;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSXTab = class
  public
    Position: Integer;
    Count: Integer;
    Alignment: TRLMetaTextAlignment;
    ComplementPosition: Integer;
    TextOrigin: AnsiString;
  end;

  TRLXLSXTabColumn = record
    StartPos: Integer;
    EndPos: Integer;
    Width: Integer;
  end;
  TRLXLSXTabColumns = array of TRLXLSXTabColumn;
	
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRLXLSXTabs = class(TObjectList)
  private
    function GetTabs(I: Integer): TRLXLSXTab;
  public
    function InsertTab(APosition, AComplementPosition: Integer; AAlignment: TRLMetaTextAlignment; TextOrigin: AnsiString): TRLXLSXTab;
    procedure RemoveTab(APosition: Integer);
    function GetColumns(AMinWidth: Integer): TRLXLSXTabColumns;
    property Tabs[I: Integer]: TRLXLSXTab read GetTabs; default;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TRLXLSXFilter = class(TRLCustomSaveFilter)
  private
    FWorkbook: TRLXLSXWorkbook;
    FOptions: TRLXLSXFilterOptions;
    FHorzTabs: TRLXLSXTabs;
    FVertTabs: TRLXLSXTabs;
    FFirstPage: Boolean;
    FOffsetRow: Integer;
    procedure CriarTabsMudancaAlinhamento;
    function IsValue(const Str: AnsiString; var ValueText: AnsiString; var Value: Double): Boolean;
    function IsDate(const Str: AnsiString; var DateText: AnsiString; var DateValue: TDateTime): Boolean;
  protected
    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalNewPage; override;
    procedure InternalDrawPage(APage: TRLGraphicSurface); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Workbook: TRLXLSXWorkbook read FWorkbook;
  published
    property Options: TRLXLSXFilterOptions read FOptions write FOptions default [];
    property FileName;
    property DisplayName;
    property ShowProgress;
  end;

implementation

const
  XLSXDefaultFontName = 'Arial';
  XLSXDefaultCellHeight = 255;
  XLSXDefaultCellLength = 8;
  XLSXDefaultLeftMargin = 2;
  XLSXDefaultTopMargin = 2.5;
  XLSXDefaultRightMargin = 2;
  XLSXDefaultBottomMargin = 2.5;

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

function NewCellArea(X0, Y0, X1, Y1: Integer): TRLXLSXCellArea;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.X1 := X1;
  Result.Y1 := Y1;
end;

function ChordsInBounds(X, Y: Integer; const Bounds: TRLXLSXCellArea): Boolean;
begin
  Result := ((X >= Bounds.X0) and (X <= Bounds.X1)) and ((Y >= Bounds.Y0) and (Y <= Bounds.Y1));
end;

function CellAreaIntercepts(const Bounds1, Bounds2: TRLXLSXCellArea): Boolean;
begin
  Result := (Bounds1.X1 >= Bounds2.X0) and (Bounds1.X0 <= Bounds2.X1) and
    (Bounds1.Y1 >= Bounds2.Y0) and (Bounds1.Y0 <= Bounds2.Y1);
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
  Result := Round((X / 96) * 5 * 2.54); ///Round((X / 96) * 1440 * 2.54);
end;

function TwipsY(Y: Integer): Integer;
begin
  Result := Y;
end;

{ TRLXLSXTabs }

function TRLXLSXTabs.GetTabs(I: Integer): TRLXLSXTab;
begin
  Result := Items[I] as TRLXLSXTab;
end;

function TRLXLSXTabs.InsertTab(APosition, AComplementPosition: Integer; AAlignment: TRLMetaTextAlignment; TextOrigin: AnsiString): TRLXLSXTab;
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
  Result := TRLXLSXTab.Create;
  Insert(I, Result);
  Result.Position := APosition;
  Result.ComplementPosition:=AComplementPosition;
  Result.Alignment := AAlignment;
  Result.Count := 1;
  Result.TextOrigin:=TextOrigin;
end;

procedure TRLXLSXTabs.RemoveTab(APosition: Integer);
var
  Tab: TRLXLSXTab;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Tab := Tabs[I];
    if Tab.Position = APosition then
      Delete(I);
  end;
end;

function TRLXLSXTabs.GetColumns(AMinWidth: Integer): TRLXLSXTabColumns;
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

{ TRLXLSXFilter }

constructor TRLXLSXFilter.Create(AOwner: TComponent);
begin
  inherited;
  FWorkbook := TRLXLSXWorkbook.Create;
  FOptions := [];
  FHorzTabs := TRLXLSXTabs.Create;
  FVertTabs := TRLXLSXTabs.Create;
  DefaultExt := '.xlsx';
  DisplayName := LocaleStrings.LS_XLSFormatStr;
end;

destructor TRLXLSXFilter.Destroy;
begin
  inherited;
  FWorkbook.Free;
  FHorzTabs.Free;
  FVertTabs.Free;
end;

procedure TRLXLSXFilter.InternalBeginDoc;
begin
  Workbook.Clear;
  FFirstPage := True;
  FOffsetRow := 0;
end;

function BestBounds(const Rect: TRect; Alignment: TRLMetaTextAlignment; Layout: TRLMetaTextLayout;
  AutoSize: Boolean; const Cols, Rows: TRLXLSXTabColumns): TRLXLSXCellArea;
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

procedure TRLXLSXFilter.CriarTabsMudancaAlinhamento;
var
  TabAnterior,TabAtual: TRLXLSXTab;
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

function TRLXLSXFilter.IsValue(const Str: AnsiString; var ValueText: AnsiString; var Value: Double): Boolean;
var
  ThousandChar: AnsiString;
  ErrorCode: Integer;
  EntreParenteses: Boolean;
  BackupValueText: string;
begin
  BackupValueText:=ValueText;
  Result := False;
  {$IfDef HAS_FORMATSETTINGS}
  ThousandChar := IfThen(FormatSettings.DecimalSeparator = '.', ',', '.');
  {$Else}
  ThousandChar := IfThen(DecimalSeparator = '.', ',', '.');
  {$EndIf}
  // limpa o texto
  ValueText := Str;
  ValueText := StringReplace(ValueText, #13#10, ' ', [rfReplaceAll]);
  ValueText := StringReplace(ValueText, #10, ' ', [rfReplaceAll]);
  ValueText := StringReplace(ValueText, ThousandChar, '', [rfReplaceAll]); // retira separador de milhares
  {$IfDef HAS_FORMATSETTINGS}
  ValueText := StringReplace(ValueText, FormatSettings.DecimalSeparator, '.', [rfReplaceAll]); // coloca ponto como separador de decimais
  {$Else}
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
    {$IfDef HAS_FORMATSETTINGS}
    ValueText := Trim(StringReplace(ValueText, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]));
    {$Else}
    ValueText := Trim(StringReplace(ValueText, '.', DecimalSeparator, [rfReplaceAll]));
    {$EndIf}
    Result := True;
  end;
end;

function TRLXLSXFilter.IsDate(const Str: AnsiString; var DateText: AnsiString; var DateValue: TDateTime): Boolean;
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

procedure TRLXLSXFilter.InternalDrawPage(APage: TRLGraphicSurface);
const
  MulFactX = 1.2;
  MulFactY = 1;
  MinTwips = 10;
var
  PageSheet: TRLXLSXWorksheet;
  NewestRange: TRLXLSXCell;
  ObjectRect: TRect;
  ObjectArea: TRLXLSXCellArea;
  TextObject: TRLTextObject;
  MinHorzTwips: Integer;
  MinVertTwips: Integer;
  IsNewSheet: Boolean;
  HorzAlignment: TRLXLSXFormatHorzAlignment;
  VertAlignment: TRLXLSXFormatVertAlignment;
  CellText: AnsiString;
  LastRight: Integer;
  LastBottom: Integer;
  Cols: TRLXLSXTabColumns;
  Rows: TRLXLSXTabColumns;
  SelectedTexts: TObjectList;
  TopCut: Integer;
  I: Integer;
  AuxStr: AnsiString;
  DoubleValue: Double;
  TimeValue: TDateTime;
  CellType: (xctString, xctNumber, xctTime);
begin
  // largura ou altura minima para uma celula
  // celulas menores que isso serao absorvidas por uma vizinha
  MinHorzTwips := TwipsX(MinTwips);
  MinVertTwips := TwipsY(0);

  // calculo as larguras das colunas na primeira pagina ou a cada nova sheet
  IsNewSheet := FFirstPage or (xfoOneSheetPerPage in Options);
  FFirstPage := False;

  if IsNewSheet then
  begin
    PageSheet := Workbook.AddSheet;
    FHorzTabs.Clear;
    FVertTabs.Clear;
  end
  else
    PageSheet := Workbook.Sheets[0];

  SelectedTexts := TObjectList.Create(False);
  try

    TopCut := -1;
    for I := 0 to APage.ObjectCount - 1 do
      if APage.Objects[I] is TRLTextObject then
      begin
        TextObject := APage.Objects[I] as TRLTextObject;
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
      ;///PageSheet.FindRow(I, True).Height := Rows[I].Width;

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

      case TextObject.Layout of
        MetaTextLayoutBottom: VertAlignment := TRLXLSXFormatVertAlignmentBottom;
        MetaTextLayoutCenter: VertAlignment := TRLXLSXFormatVertAlignmentCenter;
        ///MetaTextLayoutJustify: VertAlignment := vaJustify;
      else
        VertAlignment := TRLXLSXFormatVertAlignmentTop;
        ObjectRect.Bottom := ObjectRect.Top;
      end;

      case TextObject.Alignment of
        MetaTextAlignmentRight: HorzAlignment := TRLXLSXFormatHorzAlignmentRight;
        MetaTextAlignmentCenter: HorzAlignment := TRLXLSXFormatHorzAlignmentCenter;
        ///MetaTextAlignmentJustify: HorzAlignment := TRLXLSXFormatAlignmentJustify;
      else
        HorzAlignment := TRLXLSXFormatHorzAlignmentLeft;
      end;
 
      // procura faixa de células
      ObjectArea := BestBounds(ObjectRect, TextObject.Alignment, TextObject.Layout, IsAutoSize(TextObject), Cols, Rows);

      CellType := xctString;
      if xfoFindValueCells in Options then
        if IsValue(CellText, AuxStr, DoubleValue) then
        begin
          CellText := AuxStr;
          CellType := xctNumber;
        end
        else if IsDate(CellText, AuxStr, TimeValue) then
        begin
          CellText := AuxStr;
          CellType := xctTime;
        end;

      case CellType of
        xctNumber:
        NewestRange := PageSheet.MergeFloat(ObjectArea.Y0, ObjectArea.X0, ObjectArea.Y1, ObjectArea.X1, DoubleValue);
      else
        NewestRange := PageSheet.MergeString(ObjectArea.Y0, ObjectArea.X0, ObjectArea.Y1, ObjectArea.X1, CellText);
      end;
      NewestRange.Format := Workbook.Format(0,
        Workbook.Font(
          TextObject.Font.Name,
          TextObject.Font.Size,
          (MetaFontStyleBold and TextObject.Font.Style) = MetaFontStyleBold,
          (MetaFontStyleItalic and TextObject.Font.Style) = MetaFontStyleItalic),
        HorzAlignment, VertAlignment,
        0, 0);
    end;

    Inc(FOffsetRow, LastBottom);

  finally
    SelectedTexts.Free;
  end;
end;

procedure TRLXLSXFilter.InternalNewPage;
begin
end;

procedure TRLXLSXFilter.InternalEndDoc;
begin
  Workbook.SaveToFile(FileName);
end;

end.

