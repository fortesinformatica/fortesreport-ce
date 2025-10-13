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

unit RLSaveDialog;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows, Messages,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils,
  {$IfDef FPC}
   LCLIntf, LCLType,
  {$EndIf}
  {$IfDef CLX}
   QGraphics, QControls, QForms, QDialogs, QStdCtrls, QButtons,
  {$Else}
   Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  {$EndIf}
  RLFilters, RLConsts, RLTypes, RLUtils, RLComponentFactory;

type
  TRLSaveRange = (rprAllPages, rprSelection, rprPageNums);

  TRLSaveDialog = class(TForm)
    GroupBoxPages: TGroupBox;
    ButtonSave: TButton;
    ButtonCancel: TButton;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    LabelFromPage: TLabel;
    EditFromPage: TEdit;
    LabelToPage: TLabel;
    EditToPage: TEdit;
    LabelFileName: TLabel;
    EditFileName: TEdit;
    LabelUseFilter: TLabel;
    ComboBoxFilters: TComboBox;
    SpeedButtonLookup: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure EditFromPageChange(Sender: TObject);
    procedure SpeedButtonLookupClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    { Private declarations }
    FFileName: String;
    FMaxPage: Integer;
    FToPage: Integer;
    FMinPage: Integer;
    FFromPage: Integer;
    FSaveRange: TRLSaveRange;
    //
    procedure SetMaxPage(const Value: Integer);
    procedure LoadEditors;
    procedure SaveEditors;
    procedure LoadFilterList;
    procedure Init;
    procedure ComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    { Protected declarations }
    procedure DoCreate; override;
  public
    { Public declarations }
    function Execute: Boolean;
    procedure ApplyExt(var AFileName: String);
    //
    property FileName: String read FFileName write FFileName;
    property MaxPage: Integer read FMaxPage write SetMaxPage;
    property MinPage: Integer read FMinPage write FMinPage;
    property FromPage: Integer read FFromPage write FFromPage;
    property ToPage: Integer read FToPage write FToPage;
    property SaveRange: TRLSaveRange read FSaveRange write FSaveRange;
  end;

implementation

//{$R *.DFM}

// UTILS

function IntToEmptyStr(AInt: Integer): String;
begin
  if AInt = 0 then
    Result := ''
  else
    Result := IntToStr(AInt);
end;

function EmptyStrToInt(const AStr: String): Integer;
begin
  Result := StrToIntDef(AStr, 0);
end;

{ TRLSaveDialog }

// OVERRIDE

procedure TRLSaveDialog.DoCreate;
begin
  FFileName := '';
  FMinPage := 1;
  FMaxPage := 9999;
  FFromPage := FMinPage;
  FToPage := FMaxPage;
  FSaveRange := rprAllPages;
  //
  inherited;
  //
  Init;
end;

// PUBLIC

function TRLSaveDialog.Execute: Boolean;
begin
  LoadFilterList;
  LoadEditors;
  ActiveControl := ComboBoxFilters;///EditFileName;
  Result := (ShowModal = mrOk);
  if Result then
    SaveEditors;
end;

// PRIVATE

procedure TRLSaveDialog.Init;
Const
  GbTop = {$IfDef FPC} 0 {$Else} 20{$EndIf};
begin
  Left := 211;
  Top := 407;
  ActiveControl := EditFileName;
{$ifdef CLX}
  BorderStyle := fbsDialog;
{$else}
  BorderStyle := bsDialog;
{$endif};
  Caption := 'Salvar como';
  ClientHeight := 224;
  ClientWidth := 391;
  Color := clBtnFace;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Height := 11;
  Font.Name := 'MS Sans Serif';
  Font.Pitch := fpVariable;
  Font.Style := [];
  Position := poScreenCenter;
  //
  TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxFilters);
  TRLComponentFactory.CreateComponent(TLabel, Self, LabelUseFilter);
  with LabelUseFilter do
  begin
    Name := 'LabelUseFilter';
    Parent := Self;
    Left := 12;
    Top := 16;//44;
    Width := 86;
    Height := 13;
    Caption := 'Salvar no formato:';
  end;
  with ComboBoxFilters do
  begin
    Name := 'ComboBoxFilters';
    Parent := Self;
    Left := 108;
    Top := 12;//40;
    Width := 269;
    Height := 21;
    Style := csDropDownList;
    ItemHeight := 13;
    TabOrder := 0;//1;
    OnKeyDown:=ComboBoxKeyDown;
  end;
  ///
  TRLComponentFactory.CreateComponent(TLabel, Self, LabelFileName);
  with LabelFileName do
  begin
    Name := 'LabelFileName';
    Parent := Self;
    Left := 12;
    Top := 44;//16;
    Width := 84;
    Height := 13;
    Caption := 'Nome do arquivo:';
  end;
  TRLComponentFactory.CreateComponent(TEdit, Self, EditFileName);
  with EditFileName do
  begin
    Name := 'EditFileName';
    Parent := Self;
    Left := 108;
    Top := 40;//12;
    Width := 249;
    Height := 21;
    TabOrder := 1;//0;
  end;
  TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonLookup);
  with SpeedButtonLookup do
  begin
    Name := 'SpeedButtonLookup';
    Parent := Self;
    Left := 356;
    Top := 40;
    Width := 21;
    Height := 21;
    Caption := '...';
    OnClick := SpeedButtonLookupClick;
  end;
  TRLComponentFactory.CreateComponent(TGroupBox, Self, GroupBoxPages);
  with GroupBoxPages do
  begin
    Name := 'GroupBoxPages';
    Parent := Self;
    Left := 12;
    Top := 68;
    Width := 365;
    Height := 101;
    Caption := ' Páginas no intervalo';
    TabOrder := 2;
    TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesAll);
    with RadioButtonPagesAll do
    begin
      Name := 'RadioButtonPagesAll';
      Parent := GroupBoxPages;
      Left := 8;
      Top := GbTop;
      Width := 113;
      Height := 17;
      Caption := 'Salvar &tudo';
      Checked := True;
      TabOrder := 0;
      TabStop := True;
    end;
    TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesInterval);
    with RadioButtonPagesInterval do
    begin
      Name := 'RadioButtonPagesInterval';
      Parent := GroupBoxPages;
      Left := 8;
      Top := 24 + GbTop;
      Width := 61;
      Height := 17;
      Caption := 'Páginas';
      TabOrder := 1;
    end;
    TRLComponentFactory.CreateComponent(TRadioButton, Self, RadioButtonPagesSelect);
    with RadioButtonPagesSelect do
    begin
      Name := 'RadioButtonPagesSelect';
      Parent := GroupBoxPages;
      Left := 8;
      Top := 48 + GbTop;
      Width := 73;
      Height := 17;
      Caption := '&Seleção';
      TabOrder := 2;
    end;
    TRLComponentFactory.CreateComponent(TLabel, Self, LabelFromPage);
    with LabelFromPage do
    begin
      Name := 'LabelFromPage';
      Parent := GroupBoxPages;
      Left := 68;
      Top := 25 + GbTop;
      Width := 15;
      Height := 13;
      Caption := '&de:';
      FocusControl := EditFromPage;
      {$IfDef FPC}
      AnchorSideLeft.Control := RadioButtonPagesInterval;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := RadioButtonPagesInterval;
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Left := 10;
      {$EndIf}
    end;
    TRLComponentFactory.CreateComponent(TEdit, Self, EditFromPage);
    with EditFromPage do
    begin
      Name := 'EditFromPage';
      Parent := GroupBoxPages;
      Left := 88;
      Top := 24 + GbTop;
      Width := 41;
      Height := 21;
      TabStop := False;
      TabOrder := 3;
      Text := '1';
      OnChange := EditFromPageChange;
      {$IfDef FPC}
      AnchorSideLeft.Control := LabelFromPage;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := RadioButtonPagesInterval;
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Left := 5;
      {$EndIf}
    end;
    TRLComponentFactory.CreateComponent(TLabel, Self, LabelToPage);
    with LabelToPage do
    begin
      Name := 'LabelToPage';
      Parent := GroupBoxPages;
      Left := 136;
      Top := 25 + GbTop;
      Width := 18;
      Height := 13;
      Caption := '&até:';
      FocusControl := EditToPage;
      {$IfDef FPC}
      AnchorSideLeft.Control := EditFromPage;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := RadioButtonPagesInterval;
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Left := 5;
      {$EndIf}
    end;
    TRLComponentFactory.CreateComponent(TEdit, Self, EditToPage);
    with EditToPage do
    begin
      Name := 'EditToPage';
      Parent := GroupBoxPages;
      Left := 160;
      Top := 24 + GbTop;
      Width := 41;
      Height := 21;
      TabStop := False;
      TabOrder := 4;
      OnChange := EditFromPageChange;
      {$IfDef FPC}
      AnchorSideLeft.Control := LabelToPage;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := RadioButtonPagesInterval;
      AnchorSideTop.Side := asrCenter;
      BorderSpacing.Left := 5;
      {$EndIf}
    end;
  end;
  TRLComponentFactory.CreateComponent(TButton, Self, ButtonSave);
  with ButtonSave do
  begin
    Name := 'ButtonSave';
    Parent := Self;
    Left := 220;
    Top := 184;
    Width := 75;
    Height := 25;
    Caption := 'Salvar';
    Default := True;
    ModalResult := 0;
    TabOrder := 3;
    OnClick := ButtonSaveClick;
  end;
  TRLComponentFactory.CreateComponent(TButton, Self, ButtonCancel);
  with ButtonCancel do
  begin
    Name := 'ButtonCancel';
    Parent := Self;
    Left := 304;
    Top := 184;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := 'Cancelar';
    ModalResult := 2;
    TabOrder := 4;
  end;
  TRLComponentFactory.CreateComponent(TSaveDialog, Self, SaveDialog);
  with SaveDialog do
  begin
    Name := 'SaveDialog';
    Left := 340;
    Top := 80;
  end;
  //
  Caption := GetLocalizeStr(LocaleStrings.LS_SaveStr);
  LabelFileName.Caption := GetLocalizeStr(LocaleStrings.LS_FileNameStr);
  LabelUseFilter.Caption := GetLocalizeStr(LocaleStrings.LS_UseFilterStr);
  GroupBoxPages.Caption := GetLocalizeStr(' ' + LocaleStrings.LS_PageRangeStr + ' ');
  LabelFromPage.Caption := GetLocalizeStr(LocaleStrings.LS_RangeFromStr);
  LabelToPage.Caption := GetLocalizeStr(LocaleStrings.LS_RangeToStr);
  RadioButtonPagesAll.Caption := GetLocalizeStr(LocaleStrings.LS_AllStr);
  RadioButtonPagesInterval.Caption := GetLocalizeStr(LocaleStrings.LS_PagesStr);
  RadioButtonPagesSelect.Caption := GetLocalizeStr(LocaleStrings.LS_SelectionStr);
  ButtonSave.Caption := GetLocalizeStr(LocaleStrings.LS_SaveStr);
  ButtonCancel.Caption := GetLocalizeStr(LocaleStrings.LS_CancelStr);
end;

procedure TRLSaveDialog.LoadFilterList;
var
  I, J, P: Integer;
  F: TRLCustomSaveFilter;
begin
  ComboBoxFilters.Items.Clear;
  ComboBoxFilters.Items.AddObject(GetLocalizeStr(LocaleStrings.LS_DefaultStr), nil);
  J := 0;
  for I := 0 to ActiveFilters.Count - 1 do
    if TObject(ActiveFilters[I]) is TRLCustomSaveFilter then
    begin
      F := TRLCustomSaveFilter(ActiveFilters[I]);
      P := ComboBoxFilters.Items.AddObject(F.GetDisplayLabel, F);
      if Assigned(SelectedFilter) and (F = SelectedFilter) then
        J := P;
    end;
  ComboBoxFilters.ItemIndex := J;
end;

procedure TRLSaveDialog.LoadEditors;
const
  StateColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  case FSaveRange of
    rprAllPages: RadioButtonPagesAll.Checked := True;
    rprSelection: RadioButtonPagesSelect.Checked := True;
    rprPageNums: RadioButtonPagesInterval.Checked := True;
  end;
  EditFileName.Text := FFileName;
  EditFromPage.Text := IntToEmptyStr(FFromPage);
  EditToPage.Text := IntToEmptyStr(FToPage);
  RadioButtonPagesInterval.Enabled := True;
  EditFromPage.Enabled := True;
  EditToPage.Enabled := True;
  EditFromPage.Color := StateColors[EditFromPage.Enabled];
  EditToPage.Color := StateColors[EditToPage.Enabled];
  RadioButtonPagesSelect.Enabled := False;
end;

procedure TRLSaveDialog.SaveEditors;
begin
  FFileName := EditFileName.Text;
  if RadioButtonPagesAll.Checked then
    FSaveRange := rprAllPages
  else if RadioButtonPagesSelect.Checked then
    FSaveRange := rprSelection
  else if RadioButtonPagesInterval.Checked then
    FSaveRange := rprPageNums;
  case FSaveRange of
    rprAllPages: begin
                    FFromPage := FMinPage;
                    FToPage := FMaxPage;
                  end;
    rprSelection: begin
                    FFromPage := EmptyStrToInt(EditFromPage.Text);
                    FToPage := FFromPage;
                  end;
    rprPageNums: begin
                    FFromPage := EmptyStrToInt(EditFromPage.Text);
                    FToPage := EmptyStrToInt(EditToPage.Text);
                  end;
  end;
  //
  if ComboBoxFilters.ItemIndex <> -1 then
    SelectedFilter := TRLCustomFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
end;

// EVENTS

procedure TRLSaveDialog.EditFromPageChange(Sender: TObject);
begin
  if not RadioButtonPagesInterval.Checked then
    RadioButtonPagesInterval.Checked := True;
end;

procedure TRLSaveDialog.SetMaxPage(const Value: Integer);
begin
  if FToPage = FMaxPage then
    FToPage := Value;
  FMaxPage := Value;
end;

function FilterStr(const ADescription, AExt: String): String;
begin
  Result := ADescription + ' (*' + FormatFileExt(AExt) + ')|*' + FormatFileExt(AExt);
end;

procedure TRLSaveDialog.ApplyExt(var AFileName: String);
var
  S: TRLCustomSaveFilter;
begin
  if ComboBoxFilters.ItemIndex <> -1 then
    S := TRLCustomSaveFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex])
  else
    S := nil;
  if Assigned(S) then
    AFileName := ChangeFileExt(AFileName, S.DefaultExt)
  else
    AFileName := ChangeFileExt(AFileName, ReportFileExt);
end;

procedure TRLSaveDialog.SpeedButtonLookupClick(Sender: TObject);
var
  S: TRLCustomSaveFilter;
begin
  if ComboBoxFilters.ItemIndex <> -1 then
    S := TRLCustomSaveFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex])
  else
    S := nil;
  if Assigned(S) then
  begin
    SaveDialog.Filter := FilterStr(S.GetDisplayLabel, S.DefaultExt);
    SaveDialog.DefaultExt := S.DefaultExt;
  end
  else
  begin
    SaveDialog.Filter := FilterStr(CS_ProductTitleStr, ReportFileExt);
    SaveDialog.DefaultExt := ReportFileExt;
  end;
  SaveDialog.FileName := EditFileName.Text;
  if SaveDialog.Execute then
    EditFileName.Text := SaveDialog.FileName;
end;

procedure TRLSaveDialog.ComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  cbx: TComboBox;
begin
  cbx:=TComboBox(Sender);
  if Key=vk_next then
  begin
    Key:=0;
    if not cbx.DroppedDown then
      cbx.DroppedDown:=True;
  end;
end;

procedure TRLSaveDialog.ButtonSaveClick(Sender: TObject);
begin
  if FileExists(EditFileName.Text) then
    if MessageDlg(GetLocalizeStr(LocaleStrings.LS_FileExists), mtConfirmation, mbOKCancel, 0) = mrCancel then
      Exit;
  ModalResult := mrOK;
end;

end.

