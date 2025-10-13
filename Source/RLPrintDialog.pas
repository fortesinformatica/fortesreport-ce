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

unit RLPrintDialog;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows, Messages,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils, Math,
  {$IfDef FPC}
   LCLIntf, LCLType,
  {$EndIf}
  {$IfDef CLX}
   QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  {$Else}
   Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$EndIf}
  RLFilters, RLConsts, RLPrinters, RLTypes, RLUtils, RLComponentFactory;

type
  TRLPrintDialogOption = (rpoPrintToFile, rpoPageNums, rpoSelection, rpoWarning, rpoHelp, rpoDisablePrintToFile);
  TRLPrintDialogOptions = set of TRLPrintDialogOption;
  
  TRLPrintRange = (rprAllPages, rprSelection, rprPageNums);

  TRLPrintDialog = class(TForm)
  private
    GroupBoxPrinter: TGroupBox;
    GroupBoxPages: TGroupBox;
    GroupBoxCopies: TGroupBox;
    GroupBoxDuplex: TGroupBox;
    CheckBoxDuplex: TCheckBox;
    ComboBoxPrinterNames: TComboBox;
    LabelPrinterName: TLabel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    EditFromPage: TEdit;
    LabelToPage: TLabel;
    EditToPage: TEdit;
    LabelCopies: TLabel;
    EditCopies: TEdit;
    CheckBoxPrintToFile: TCheckBox;
    ComboBoxFilters: TComboBox;
    LabelFilterName: TLabel;
    LabelOptions: TLabel;
    ComboBoxOptions: TComboBox;
    ButtonPrinterSetup: TButton;
    EditPageSelection: TEdit;
    LabelPageSelectionHint: TLabel;
    LabelFromPage: TLabel;
    LabelOddPages: TLabel;
    ComboBoxOddPages: TComboBox;
    procedure EditFromPageChange(Sender: TObject);
    procedure ComboBoxFiltersChange(Sender: TObject);
    procedure ButtonPrinterSetupClick(Sender: TObject);
    procedure ComboBoxPrinterNamesChange(Sender: TObject);
    procedure EditPageSelectionChange(Sender: TObject);
  private
    { Private declarations }
    FMaxPage: Integer;
    FToPage: Integer;
    FMinPage: Integer;
    FFromPage: Integer;
    FOptions: TRLPrintDialogOptions;
    FPrintRange: TRLPrintRange;
    FPageRanges: string;
    FCopies: Integer;
    FPrintToFile: Boolean;
    FFileName: string;
    FOrientation: TRLPageOrientation;
    FDuplex: Boolean;
    //
    procedure SetMaxPage(const Value: Integer);
    //
    procedure LoadEditors;
    procedure SaveEditors;
    procedure LoadPrinterList;
    procedure LoadFilterList;
    procedure Init;
  protected
    { Protected declarations }
    procedure DoCreate; override;
  public
    { Public declarations }
    function Execute: Boolean;
    //
    property Options: TRLPrintDialogOptions read FOptions write FOptions;
    property MaxPage: Integer read FMaxPage write SetMaxPage;
    property MinPage: Integer read FMinPage write FMinPage;
    property FromPage: Integer read FFromPage write FFromPage;
    property ToPage: Integer read FToPage write FToPage;
    property PrintRange: TRLPrintRange read FPrintRange write FPrintRange;
    property PageRanges: string read FPageRanges write FPageRanges;
    property Copies: Integer read FCopies write FCopies;
    property Duplex: Boolean read FDuplex write FDuplex;
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile;
    property FileName: string read FFileName write FFileName;
    property Orientation: TRLPageOrientation read FOrientation write FOrientation;
  end;

implementation

uses Types;

// UTILS

function IntToEmptyStr(AInt: Integer): string;
begin
  if AInt = 0 then
    Result := ''
  else
    Result := IntToStr(AInt);
end;

function EmptyStrToInt(const AStr: string): Integer;
begin
  Result := StrToIntDef(AStr, 0);
end;

{ TRLPrintDialog }

// OVERRIDE

procedure TRLPrintDialog.DoCreate;
begin
  FMinPage := 1;
  FMaxPage := 9999;
  FFromPage := FMinPage;
  FToPage := FMaxPage;
  FOptions := [rpoPrintToFile, rpoPageNums, rpoSelection, rpoWarning, rpoDisablePrintToFile];
  FPrintRange := rprAllPages;
  FPageRanges := '';
  FCopies := RLPrinter.Copies;
  FDuplex := RLPrinter.SupportsDuplex and RLPrinter.Duplex; 
  FPrintToFile := False;
  FFileName := '';
  //
  inherited;
  //
  Init;
end;

// PUBLIC

function TRLPrintDialog.Execute: Boolean;
begin
  LoadPrinterList;
  LoadFilterList;
  LoadEditors;
  Result := (ShowModal = mrOk);
  if Result then
    SaveEditors;
end;

type
  TControlArray = array of TControl;

function Cols(AControls: array of TControl): TControlArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AControls));
  for I := 0 to Length(AControls) - 1 do
    Result[I] := AControls[I];
end;

type
  TTableLayout = class
  public
    ColSizes: array of Integer;
    LineHeight: Integer;
    Margins: TRect;
    Spacing: TPoint;
    procedure SetColWidths(AColSizes: array of Integer);
    procedure Cell(ARowIndex, AColIndex: Integer; ACtrl: TControl);
    procedure Range(ARowIndex0, AColIndex0, ARowIndex1, AColIndex1: Integer; ACtrl: TControl);
    procedure Row(ARowIndex: Integer; ACtrls: array of TControl);
    function CellRect(ARowIndex, AColIndex: Integer): TRect;
    function RangeRect(ARowIndex0, AColIndex0, ARowIndex1, AColIndex1: Integer): TRect;
  end;

function TTableLayout.CellRect(ARowIndex, AColIndex: Integer): TRect;
var
  X, Y, C: Integer;
begin
  X := Margins.Left;
  for C := 0 to AColIndex - 1 do
    Inc(X, ColSizes[C] + Spacing.X);
  Y := Margins.Top + ARowIndex * (LineHeight + Spacing.Y);
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + ColSizes[AColIndex];
  Result.Bottom := Y + LineHeight;
end;

function TTableLayout.RangeRect(ARowIndex0, AColIndex0, ARowIndex1, AColIndex1: Integer): TRect;
var
  R0, R1: TRect;
begin
  R0 := CellRect(ARowIndex0, AColIndex0);
  R1 := CellRect(ARowIndex1, AColIndex1);
  Result.Left := R0.Left;
  Result.Top := R0.Top;
  Result.Right := R1.Right;
  Result.Bottom := R1.Bottom;
end;

procedure TTableLayout.Row(ARowIndex: Integer; ACtrls: array of TControl);
var
  C: Integer;
begin
  for C := 0 to Length(ACtrls) - 1 do
    if ACtrls[C] <> nil then
      Cell(ARowIndex, C, ACtrls[C]);
end;

procedure TTableLayout.Cell(ARowIndex, AColIndex: Integer; ACtrl: TControl);
begin
  ACtrl.BoundsRect := CellRect(ARowIndex, AColIndex);
end;

procedure TTableLayout.Range(ARowIndex0, AColIndex0, ARowIndex1, AColIndex1: Integer; ACtrl: TControl);
begin
  ACtrl.BoundsRect := RangeRect(ARowIndex0, AColIndex0, ARowIndex1, AColIndex1);
end;

procedure TTableLayout.SetColWidths(AColSizes: array of Integer);
var
  I: Integer;
begin
  SetLength(ColSizes, Length(AColSizes));

  for I := 0 to Length(AColSizes) - 1 do
    ColSizes[I] := AColSizes[I];
end;

procedure TableLayout(ARows: array of TControlArray; const AMargins: TRect;
  ALineHeight: Integer; const ASpacing: TPoint; AParent: TControl);
const
  MaxCols = 100;
var
  ColWidths: array of Integer;
  R, C, X, Y: Integer;
  Ctrl: TControl;
  OuterSize: TPoint;
begin
  SetLength(ColWidths, MaxCols);
  for C := 0 to MaxCols - 1 do
    ColWidths[C] := 0;
  for R := 0 to Length(ARows) - 1 do
    for C := 0 to Length(ARows[R]) - 1 do
      ColWidths[C] := Max(ColWidths[C], ARows[R][C].Width);
  OuterSize := Point(0, 0);
  Y := AMargins.Top;
  for R := 0 to Length(ARows) - 1 do
  begin
    X := AMargins.Left;
    for C := 0 to Length(ARows[R]) - 1 do
    begin
      Ctrl := ARows[R][C];
      Ctrl.SetBounds(X, Y, ColWidths[C], ALineHeight);
      OuterSize.X := Max(OuterSize.X, Ctrl.BoundsRect.Right);
      OuterSize.Y := Max(OuterSize.Y, Ctrl.BoundsRect.Bottom);
      Inc(X, ColWidths[C] + ASpacing.X);
    end;
    Inc(Y, ALineHeight + ASpacing.Y);
  end;
  Inc(OuterSize.X, AMargins.Right);
  Inc(OuterSize.Y, AMargins.Bottom);
  AParent.Width := OuterSize.X;
  AParent.Height := OuterSize.Y;
end;

// PRIVATE

procedure TRLPrintDialog.Init;
const
  {$IfDef FPC}
  GroupMarginX = 8;
  GroupMarginY = 2;
  LineHeight = 21;
  LineSpacing = 6;
  ColSpacing = 4;
  PlataformSpacing = 5;
  {$Else}
  GroupMarginX = 8;
  GroupMarginY = 16;
  LineHeight = 21;
  LineSpacing = 4;
  ColSpacing = 4;
  PlataformSpacing = 0;
  {$EndIf}
var
  TableLayout: TTableLayout;
  BottomLine, MiddleCol: Integer;
begin
  AutoScroll := False;
  BorderWidth := 8;
  Caption := 'Imprimir';
  Position := poScreenCenter;
{$IfNDef FPC}
  Scaled := False;
{$Else}
  BorderSpacing.InnerBorder := LineSpacing;
{$EndIf}

{$ifdef CLX}
  BorderStyle := fbsDialog;
{$else}
  BorderStyle := bsDialog;
{$endif};

  TRLComponentFactory.CreateComponent(TGroupBox, Self, GroupBoxPrinter);
  with GroupBoxPrinter do
  begin
    Name := 'GroupBoxPrinter';
    Parent := Self;
    Caption := 'Impressora';
    TabOrder := 0;
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelPrinterName);
  with LabelPrinterName do
  begin
    Name := 'LabelPrinterName';
    Parent := GroupBoxPrinter;
    AutoSize := True;
    Caption := '&Nome:';
  end;
  
  TRLComponentFactory.CreateComponent(TLabel, Self, LabelFilterName);
  with LabelFilterName do
  begin
    Name := 'LabelFilterName';
    Parent := GroupBoxPrinter;
    AutoSize := True;
    Caption := 'Usar &filtro:';
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelOptions);
  with LabelOptions do
  begin
    Name := 'LabelOptions';
    Parent := GroupBoxPrinter;
    AutoSize := True;
    Caption := 'Opções do filtro:';
  end;

  TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxPrinterNames);
  with ComboBoxPrinterNames do
  begin
    Name := 'ComboBoxPrinterNames';
    Parent := GroupBoxPrinter;
    Style := csDropDownList;
    ItemHeight := 13;
    TabOrder := 0;
    OnChange := ComboBoxPrinterNamesChange;
  end;
    
  TRLComponentFactory.CreateComponent(TCheckBox, Self, CheckBoxPrintToFile);
  with CheckBoxPrintToFile do
  begin
    Name := 'CheckBoxPrintToFile';
    Parent := GroupBoxPrinter;
    TabStop := False;
    Caption := 'Imprimir em arquivo';
    TabOrder := 4;
  end;

  TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxFilters);
  with ComboBoxFilters do
  begin
    Name := 'ComboBoxFilters';
    Parent := GroupBoxPrinter;
    Style := csDropDownList;
    ItemHeight := 13;
    TabOrder := 1;
    OnChange := ComboBoxFiltersChange;
  end;

  TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxOptions);
  with ComboBoxOptions do
  begin
    Name := 'ComboBoxOptions';
    Parent := GroupBoxPrinter;
    Style := csDropDownList;
    ItemHeight := 13;
    TabOrder := 2;
  end;

  TRLComponentFactory.CreateComponent(TButton, Self, ButtonPrinterSetup);
  with ButtonPrinterSetup do
  begin
    Name := 'ButtonPrinterSetup';
    Parent := GroupBoxPrinter;
    Caption := 'Propriedades';
    TabOrder := 3;
    TabStop := False;
    OnClick := ButtonPrinterSetupClick;
    {$IfDef FPC}
    AutoSize := True;
    AnchorSideTop.Control := ComboBoxPrinterNames;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}

  end;

  TRLComponentFactory.CreateComponent(TGroupBox, Self, GroupBoxPages);
  with GroupBoxPages do
  begin
    Name := 'GroupBoxPages';
    Parent := Self;
    Caption := 'Intervalo de páginas';
    TabOrder := 1;
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelPageSelectionHint);
  with LabelPageSelectionHint do
  begin
    Name := 'LabelPageSelectionHint';
    Parent := GroupBoxPages;
    AutoSize := False;
    Caption := '';
    WordWrap := True;
  end;

  TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesAll);
  with RadioButtonPagesAll do
  begin
    Name := 'RadioButtonPagesAll';
    Parent := GroupBoxPages;
    Caption := '&Todas';
    Checked := True;
    TabOrder := 0;
    TabStop := True;
  end;

  TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesInterval);
  with RadioButtonPagesInterval do
  begin
    Name := 'RadioButtonPagesInterval';
    Parent := GroupBoxPages;
    Caption := 'Intervalo';
    TabOrder := 1;
  end;

  TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesSelect);
  with RadioButtonPagesSelect do
  begin
    Name := 'RadioButtonPagesSelect';
    Parent := GroupBoxPages;
    Caption := 'Seleção';
    TabOrder := 4;
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelFromPage);
  with LabelFromPage do
  begin
    Name := 'LabelFromPage';
    Parent := GroupBoxPages;
    Caption := '&de:';
    {$IfDef FPC}
    AnchorSideTop.Control := RadioButtonPagesInterval;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}
  end;

  TRLComponentFactory.CreateComponent(TEdit, Self, EditFromPage);
  with EditFromPage do
  begin
    Name := 'EditFromPage';
    Parent := GroupBoxPages;
    TabStop := False;
    TabOrder := 2;
    Text := '1';
    OnChange := EditFromPageChange;
    {$IfDef FPC}
    AnchorSideTop.Control := RadioButtonPagesInterval;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelToPage);
  with LabelToPage do
  begin
    Name := 'LabelToPage';
    Parent := GroupBoxPages;
    Caption := '&até:';
    {$IfDef FPC}
    AnchorSideTop.Control := RadioButtonPagesInterval;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}
  end;

  TRLComponentFactory.CreateComponent(TEdit, Self, EditToPage);
  with EditToPage do
  begin
    Name := 'EditToPage';
    Parent := GroupBoxPages;
    TabStop := False;
    TabOrder := 3;
    Text := IntToStr(MaxPageNo);
    OnChange := EditFromPageChange;
    {$IfDef FPC}
    AnchorSideTop.Control := RadioButtonPagesInterval;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}
  end;

  TRLComponentFactory.CreateComponent(TEdit, Self, EditPageSelection);
  with EditPageSelection do
  begin
    Name := 'EditPageSelection';
    Parent := GroupBoxPages;
    Text := '';
    TabOrder := 5;
    OnChange := EditPageSelectionChange;
    {$IfDef FPC}
    AnchorSideTop.Control := RadioButtonPagesSelect;
    AnchorSideTop.Side := asrCenter;
    {$EndIf}
  end;

  TRLComponentFactory.CreateComponent(TGroupBox, Self, GroupBoxCopies);
  with GroupBoxCopies do
  begin
    Name := 'GroupBoxCopies';
    Parent := Self;
    Caption := 'Cópias';
    TabOrder := 2;
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelCopies);
  with LabelCopies do
  begin
    Name := 'LabelCopies';
    Parent := GroupBoxCopies;
    Caption := 'Número de &cópias:';
  end;

  TRLComponentFactory.CreateComponent(TEdit, Self, EditCopies);
  with EditCopies do
  begin
    Name := 'EditCopies';
    Parent := GroupBoxCopies;
    TabOrder := 0;
    Text := '1';
  end;

  TRLComponentFactory.CreateComponent(TLabel, Self, LabelOddPages);
  with LabelOddPages do
  begin
    Name := 'LabelOddPages';
    Parent := GroupBoxCopies;
    Alignment := taRightJustify;
    Caption := 'Pares/Ímpares:';
    FocusControl := ComboBoxOddPages;
  end;

  TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxOddPages);
  with ComboBoxOddPages do
  begin
    Name := 'ComboBoxOddPages';
    Parent := GroupBoxCopies;
    Style := csDropDownList;
    ItemHeight := 13;
    TabOrder := 1;
    Items.Text := 'Pares'+sLineBreak+
                  'Ímpares'+sLineBreak+
                  'Todas';
  end;

  TRLComponentFactory.CreateComponent(TGroupBox, Self, GroupBoxDuplex);
  with GroupBoxDuplex do
  begin
    Name := 'GroupBoxDuplex';
    Parent := Self;
    Caption := 'Duplex';
  end;

  TRLComponentFactory.CreateComponent(TCheckBox, Self, CheckBoxDuplex);
  with CheckBoxDuplex do
  begin
    Name := 'CheckBoxDuplex';
    Parent := GroupBoxDuplex;
    TabStop := False;
    Caption := 'Automatic two-sided printing';
    TabOrder := 0;
  end;

  TRLComponentFactory.CreateComponent(TButton, Self, ButtonOk);
  with ButtonOk do
  begin
    Name := 'ButtonOk';
    Parent := Self;
    Caption := 'OK';
    Default := True;
    ModalResult := 1;
    TabOrder := 3;
  end;

  TRLComponentFactory.CreateComponent(TButton, Self, ButtonCancel);
  with ButtonCancel do
  begin
    Name := 'ButtonCancel';
    Parent := Self;
    Cancel := True;
    Caption := 'Cancelar';
    ModalResult := 2;
    TabOrder := 4;
  end;
  
  //
  ActiveControl := ComboBoxPrinterNames;
  LabelPrinterName.FocusControl := ComboBoxPrinterNames;
  LabelFilterName.FocusControl := ComboBoxFilters;
  LabelOptions.FocusControl := ComboBoxOptions;
  LabelToPage.FocusControl := EditToPage;
  LabelFromPage.FocusControl := EditFromPage;
  //
  LabelPageSelectionHint.Caption := GetLocalizeStr(LocaleStrings.LS_PageSelectionHint);
  Caption := GetLocalizeStr(LocaleStrings.LS_PrintStr);
  GroupBoxPrinter.Caption := GetLocalizeStr(' ' + LocaleStrings.LS_PrinterStr + ' ');
  LabelPrinterName.Caption := GetLocalizeStr(LocaleStrings.LS_NameStr + ':');
  LabelFilterName.Caption := GetLocalizeStr(LocaleStrings.LS_UseFilterStr + ':');
  CheckBoxPrintToFile.Caption := GetLocalizeStr(LocaleStrings.LS_PrintToFileStr);
  ButtonPrinterSetup.Caption := GetLocalizeStr(LocaleStrings.LS_Propriedades);
  GroupBoxPages.Caption := GetLocalizeStr(' ' + LocaleStrings.LS_PageRangeStr + ' ');
  LabelFromPage.Caption := GetLocalizeStr(LocaleStrings.LS_RangeFromStr + ':');
  LabelToPage.Caption := GetLocalizeStr(LocaleStrings.LS_RangeToStr + ':');
  RadioButtonPagesAll.Caption := GetLocalizeStr(LocaleStrings.LS_AllStr);
  RadioButtonPagesInterval.Caption := GetLocalizeStr(LocaleStrings.LS_PagesStr);
  RadioButtonPagesSelect.Caption := GetLocalizeStr(LocaleStrings.LS_SelectionStr);
  GroupBoxCopies.Caption := GetLocalizeStr(' ' + LocaleStrings.LS_CopiesStr + ' ');
  EditCopies.Text := IntToStr(FCopies);
  LabelCopies.Caption := GetLocalizeStr(LocaleStrings.LS_NumberOfCopiesStr + ':');
  ButtonOk.Caption := GetLocalizeStr(LocaleStrings.LS_OkStr);
  ButtonCancel.Caption := GetLocalizeStr(LocaleStrings.LS_CancelStr);
  LabelOddPages.Caption := GetLocalizeStr(LocaleStrings.LS_OddPages + '/' + LocaleStrings.LS_EvenPages + ':');
  ComboBoxOddPages.Items.Text :=
    GetLocalizeStr(LocaleStrings.LS_OddPagesOnly + sLineBreak +
    LocaleStrings.LS_EvenPagesOnly + sLineBreak +
    LocaleStrings.LS_AllOddAndEven);
  case RLPrinter.OddEven of
    odOddPagesOnly: ComboBoxOddPages.ItemIndex := 0;
    odEvenPagesOnly: ComboBoxOddPages.ItemIndex := 1;
  else
    ComboBoxOddPages.ItemIndex := 2;
  end;
  LabelOptions.Visible := False;
  ComboBoxOptions.Visible := False;
  CheckBoxDuplex.Caption := GetLocalizeStr(LocaleStrings.LS_Duplex);

  //
  Self.ClientWidth := 560 + (PlataformSpacing*4);
  Self.ClientHeight := 280 + (PlataformSpacing*4);
  MiddleCol := 240;
  GroupBoxPrinter.SetBounds(0, 0, Self.ClientWidth, 100);
  BottomLine := GroupBoxPrinter.BoundsRect.Bottom;
  GroupBoxPages.SetBounds(0, BottomLine, MiddleCol, 140 + (PlataformSpacing*4));
  Inc(MiddleCol, ColSpacing);
  GroupBoxCopies.SetBounds(MiddleCol, BottomLine, Self.ClientWidth - MiddleCol, 70 + (PlataformSpacing*2));
  BottomLine := GroupBoxCopies.BoundsRect.Bottom;
  GroupBoxDuplex.SetBounds(MiddleCol, BottomLine, GroupBoxCopies.Width, 70 + (PlataformSpacing*2));
  BottomLine := GroupBoxPages.BoundsRect.Bottom + LineSpacing;
  ButtonCancel.SetBounds(Self.ClientWidth - PlataformSpacing - 75, BottomLine, 75, 25);
  ButtonOk.SetBounds(ButtonCancel.BoundsRect.Left - ColSpacing - 75, BottomLine, 75, 25);
  //
  TableLayout := TTableLayout.Create;
  try
    TableLayout.LineHeight := LineHeight;
    TableLayout.Spacing := Point(ColSpacing, LineSpacing);
    TableLayout.Margins := Rect(GroupMarginX, GroupMarginY, GroupMarginX, GroupMarginY);

    TableLayout.SetColWidths([75, 310, 150]);
    TableLayout.Row(0, [LabelPrinterName, ComboBoxPrinterNames, ButtonPrinterSetup]);
    TableLayout.Row(1, [LabelFilterName, ComboBoxFilters, CheckBoxPrintToFile]);
    TableLayout.Row(2, [LabelOptions, ComboBoxOptions]);

    TableLayout.SetColWidths([85, 20, 40, 20, 40]);
    TableLayout.Cell(0, 0, RadioButtonPagesAll);
    TableLayout.Row(1, [RadioButtonPagesInterval, LabelFromPage, EditFromPage, LabelToPage, EditToPage]);
    TableLayout.Cell(2, 0, RadioButtonPagesSelect);
    TableLayout.Range(2, 1, 2, 4, EditPageSelection);
    TableLayout.Range(3, 0, 4, 4, LabelPageSelectionHint);

    TableLayout.SetColWidths([130, 165]);
    TableLayout.Row(0, [LabelCopies, EditCopies]);
    TableLayout.Row(1, [LabelOddPages, ComboBoxOddPages]);

    TableLayout.SetColWidths([200]);
    TableLayout.Row(0, [CheckBoxDuplex]);
  finally
    TableLayout.Free;
  end;
end;

procedure TRLPrintDialog.LoadPrinterList;
var
  I, J: Integer;
begin
  ComboBoxPrinterNames.Items.Clear;
  J := 0;
  RLPrinter.Refresh;
  for I := 0 to RLPrinter.Printers.Count - 1 do
  begin
    if RLPrinter.PrinterNames[I] = RLPrinter.PrinterName then
      J := I;
    ComboBoxPrinterNames.Items.Add(RLPrinter.PrinterDisplays[I]);
  end;
  ComboBoxPrinterNames.ItemIndex := J;
  ButtonPrinterSetup.Enabled := ComboBoxPrinterNames.ItemIndex <> -1;
  CheckBoxDuplex.Enabled := RLPrinter.SupportsDuplex;
end;

procedure TRLPrintDialog.LoadFilterList;
var
  I, J, P: Integer;
  N: string;
  F: TRLCustomPrintFilter;
begin
  ComboBoxFilters.Items.Clear;
  ComboBoxFilters.Items.AddObject(GetLocalizeStr(LocaleStrings.LS_DefaultStr), nil);
  //
  J := 0;
  for I := 0 to ActiveFilters.Count - 1 do
    if TObject(ActiveFilters[I]) is TRLCustomPrintFilter then
    begin
      F := TRLCustomPrintFilter(ActiveFilters[I]);
      N := F.GetDisplayLabel;
      if N <> '' then
      begin
        P := ComboBoxFilters.Items.AddObject(N, F);
        if Assigned(SelectedFilter) and (SelectedFilter = F) then
          J := P;
      end;
    end;
  //
  ComboBoxFilters.ItemIndex := J;
  if ComboBoxFilters.Items.Count <= 1 then
  begin
    ComboBoxFilters.Enabled := False;
    ComboBoxFilters.Color := Self.Color;
  end;
  ComboBoxFiltersChange(ComboBoxFilters);
end;

procedure TRLPrintDialog.LoadEditors;
const
  StateColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  case FPrintRange of
    rprAllPages: RadioButtonPagesAll.Checked := True;
    rprSelection: RadioButtonPagesSelect.Checked := True;
    rprPageNums: RadioButtonPagesInterval.Checked := True;
  end;
  EditFromPage.Text := IntToEmptyStr(FFromPage);
  EditToPage.Text := IntToEmptyStr(FToPage);
  EditPageSelection.Text := FPageRanges;
  EditCopies.Text := IntToEmptyStr(FCopies);
  CheckBoxDuplex.Checked := FDuplex;
  case RLPrinter.OddEven of
    odOddPagesOnly: ComboBoxOddPages.ItemIndex := 0;
    odEvenPagesOnly: ComboBoxOddPages.ItemIndex := 1;
  else
    ComboBoxOddPages.ItemIndex := 2;
  end;
  CheckBoxPrintToFile.Visible := (rpoPrintToFile in FOptions);
  CheckBoxPrintToFile.Enabled := not (rpoDisablePrintToFile in FOptions);
  CheckBoxPrintToFile.Checked := FPrintToFile;
  RadioButtonPagesInterval.Enabled := (rpoPageNums in FOptions);
  EditFromPage.Enabled := (rpoPageNums in FOptions);
  EditToPage.Enabled := (rpoPageNums in FOptions);
  EditPageSelection.Enabled := (rpoSelection in FOptions);
  EditFromPage.Color := StateColors[EditFromPage.Enabled];
  EditToPage.Color := StateColors[EditToPage.Enabled];
  EditPageSelection.Color := StateColors[EditPageSelection.Enabled];
  RadioButtonPagesSelect.Enabled := (rpoSelection in FOptions);
  if rpoHelp in FOptions then
    BorderIcons := BorderIcons + [biHelp]
  else
    BorderIcons := BorderIcons - [biHelp];
end;

procedure TRLPrintDialog.SaveEditors;
begin
  if RadioButtonPagesSelect.Checked then
    FPrintRange := rprSelection
  else if RadioButtonPagesInterval.Checked then
    FPrintRange := rprPageNums
  else
    FPrintRange := rprAllPages;
  case FPrintRange of
    rprAllPages:
    begin
      FFromPage := FMinPage;
      FToPage := FMaxPage;
    end;
    rprSelection:
    begin
      FFromPage := FMinPage;
      FToPage := FMaxPage;
      FPageRanges := EditPageSelection.Text;
    end;
    rprPageNums:
    begin
      FFromPage := EmptyStrToInt(EditFromPage.Text);
      FToPage := EmptyStrToInt(EditToPage.Text);
    end;
  end;
  FCopies := EmptyStrToInt(EditCopies.Text);
  FDuplex := CheckBoxDuplex.Checked;
  FPrintToFile := CheckBoxPrintToFile.Checked;
  //
  if ComboBoxPrinterNames.ItemIndex <> -1 then
    RLPrinter.PrinterIndex := ComboBoxPrinterNames.ItemIndex;
  if ComboBoxFilters.ItemIndex <> -1 then
  begin
    SelectedFilter := TRLCustomFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
    if (SelectedFilter <> nil) and (SelectedFilter is TRLCustomPrintFilter) then
      TRLCustomPrintFilter(SelectedFilter).OptionIndex := ComboBoxOptions.ItemIndex; 
  end; 
  RLPrinter.Copies := FCopies;
  if RLPrinter.SupportsDuplex then
    RLPrinter.Duplex := FDuplex;
  case ComboBoxOddPages.ItemIndex of
    0: RLPrinter.OddEven := odOddPagesOnly;
    1: RLPrinter.OddEven := odEvenPagesOnly;
  else
    RLPrinter.OddEven := odAllPages;
  end;
end;

// EVENTS

procedure TRLPrintDialog.EditFromPageChange(Sender: TObject);
begin
  if not RadioButtonPagesInterval.Checked then
    RadioButtonPagesInterval.Checked := True;
end;

procedure TRLPrintDialog.SetMaxPage(const Value: Integer);
begin
  if FToPage = FMaxPage then
    FToPage := Value;
  FMaxPage := Value;
end;

procedure TRLPrintDialog.ComboBoxFiltersChange(Sender: TObject);
var
  P: TRLCustomPrintFilter;
begin
  if ComboBoxFilters.ItemIndex = -1 then
    P := nil
  else
    P := TRLCustomPrintFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
  if (P <> nil) and (P.Options <> nil) then
  begin
    P.SetOrientation(Orientation);
    LabelOptions.Caption := P.OptionsLabel + ':';
    ComboBoxOptions.Items := P.Options;
    ComboBoxOptions.ItemIndex := P.OptionIndex;
    LabelOptions.Show;
    ComboBoxOptions.Show;
  end
  else
  begin
    LabelOptions.Hide;
    ComboBoxOptions.Hide;
  end;
end;

procedure TRLPrintDialog.ButtonPrinterSetupClick(Sender: TObject);
begin
  ButtonPrinterSetup.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    if not RLPrinter.ExecuteSetup(Self.Handle) then
      raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_PrintDialogError));
  finally
    Screen.Cursor := crDefault;
    ButtonPrinterSetup.Enabled := True;
  end;
end;

procedure TRLPrintDialog.ComboBoxPrinterNamesChange(Sender: TObject);
begin
  RLPrinter.PrinterIndex := ComboBoxPrinterNames.ItemIndex;
  ButtonPrinterSetup.Enabled := RLPrinter.PrinterIndex <> -1;
  CheckBoxDuplex.Enabled := RLPrinter.SupportsDuplex;
  CheckBoxDuplex.Checked := RLPrinter.SupportsDuplex and RLPrinter.Duplex;
end;

procedure TRLPrintDialog.EditPageSelectionChange(Sender: TObject);
begin
  RadioButtonPagesSelect.Checked := True;
end;

end.

