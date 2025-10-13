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

unit RLFindDialog;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  SysUtils, Contnrs, Classes, TypInfo,
  {$IfDef FPC}
   LCLIntf, LCLType,
  {$EndIf}
  {$IfDef CLX}
   QTypes, QGraphics, QControls, QForms, QDialogs, QStdCtrls, QButtons, QExtCtrls, Qt,
  {$Else}
   Types, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  {$EndIf}
  RLConsts, RLUtils, RLComponentFactory;

type
  TRLFindOption = (foWholeWords, foMatchCase, foFindBackward);
  TRLFindOptions = set of TRLFindOption;

  TRLOnFindEvent = procedure(Sender: TObject; const AText: String; Options: TRLFindOptions; var Found: Boolean) of object;

  TfrmRLFindDialog = class(TForm)
    LabelTextToFind: TLabel;
    EditTextToFind: TEdit;
    BitBtnFindNext: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxWholeWords: TCheckBox;
    CheckBoxMatchCase: TCheckBox;
    RadioGroupDirection: TGroupBox;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnFindNextClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    FOnFind: TRLOnFindEvent;
    //
    function GetTextValue: String;
    procedure SetTextValue(const Value: String);
    function GetOptions: TRLFindOptions;
    procedure SetOptions(const Value: TRLFindOptions);
    //
    procedure Init;
    function RadioGroupDirection_GetItemIndex: Integer;
    procedure RadioGroupDirection_SetItemIndex(Value: Integer);
    function RadioGroupDirection_GetItems: TStrings;
  public
    { Public declarations }
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    //
    property Text: String read GetTextValue write SetTextValue;
    property Options: TRLFindOptions read GetOptions write SetOptions;
    //
    property OnFind: TRLOnFindEvent read FOnFind write FOnFind;
  end;

var
  frmRLFindDialog: TfrmRLFindDialog;

implementation

///{$R *.DFM}

{ TfrmRLFindDialog }

constructor TfrmRLFindDialog.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  FOnFind := nil;
  //
  inherited;
  //
  Init;
end;

procedure TfrmRLFindDialog.Init;
Const
  PlataformSpacing = {$IfDef FPC} 20 {$Else} 0{$EndIf};
begin
  BorderIcons := [biSystemMenu];
  {$ifdef CLX}
   BorderStyle := fbsDialog;
  {$else}
   BorderStyle := bsDialog;
  {$endif}
  Caption := 'Procurar';
  ClientHeight := 94 + PlataformSpacing;
  ClientWidth := 367 + PlataformSpacing;
  Color := clBtnFace;
  Position := poScreenCenter;
  OnDeactivate := FormDeactivate;
  TRLComponentFactory.CreateComponent(TLabel, Self, LabelTextToFind);
  with LabelTextToFind do
  begin
    Name := 'LabelTextToFind';
    Parent := Self;
    Left := 8;
    Top := 16;
    Width := 30;
    Height := 13;
    Caption := 'Te&xto:';
  end;
  TRLComponentFactory.CreateComponent(TEdit, Self, EditTextToFind);
  with EditTextToFind do
  begin
    Name := 'EditTextToFind';
    Parent := Self;
    Left := 48;
    Top := 12;
    Width := 229 + PlataformSpacing;
    Height := 21;
    TabOrder := 0;
  end;
  TRLComponentFactory.CreateComponent(TBitBtn, Self, BitBtnFindNext);
  with BitBtnFindNext do
  begin
    Name := 'BitBtnFindNext';
    Parent := Self;
    Left := 284 + PlataformSpacing;
    Top := 12;
    Width := 75;
    Height := 21;
    Caption := '&Próxima';
    Default := True;
    TabOrder := 1;
    OnClick := BitBtnFindNextClick;
  end;
  TRLComponentFactory.CreateComponent(TBitBtn, Self, BitBtnCancel);
  with BitBtnCancel do
  begin
    Name := 'BitBtnCancel';
    Parent := Self;
    Left := 284 + PlataformSpacing;
    Top := 36;
    Width := 75;
    Height := 21;
    Cancel := True;
    Caption := '&Cancelar';
    TabOrder := 2;
    OnClick := BitBtnCancelClick;
  end;
  TRLComponentFactory.CreateComponent(TCheckBox, Self, CheckBoxWholeWords);
  with CheckBoxWholeWords do
  begin
    Name := 'CheckBoxWholeWords';
    Parent := Self;
    Left := 8;
    Top := 44;
    Width := 133;
    Height := 17;
    Caption := 'Palavras &inteiras';
    TabOrder := 3;
  end;
  TRLComponentFactory.CreateComponent(TCheckBox, Self, CheckBoxMatchCase);
  with CheckBoxMatchCase do
  begin
    Name := 'CheckBoxMatchCase';
    Parent := Self;
    Left := 8;
    Top := 64;
    Width := 193;
    Height := 17;
    Caption := 'Diferenciar &maiúsculas e minúsculas';
    TabOrder := 4;
  end;
  TRLComponentFactory.CreateComponent(TRadioGroup, Self, RadioGroupDirection);
  with RadioGroupDirection do
  begin
    Name := 'RadioGroupDirection';
    Parent := Self;
    Left := 204 + PlataformSpacing;
    Top := 36;
    Width := 73;
    Height := 49 + PlataformSpacing;
    Caption := ' Direção ';
    TabOrder := 5;
  end;

  //
  Caption := GetLocalizeStr(LocaleStrings.LS_FindCaptionStr);
  LabelTextToFind.Caption := GetLocalizeStr(LocaleStrings.LS_TextToFindStr + ':');
  EditTextToFind.Text := '';
  BitBtnFindNext.Caption := GetLocalizeStr(LocaleStrings.LS_FindNextStr);
  BitBtnCancel.Caption := GetLocalizeStr(LocaleStrings.LS_CancelStr);
  CheckBoxWholeWords.Caption := GetLocalizeStr(LocaleStrings.LS_WholeWordsStr);
  CheckBoxMatchCase.Caption := GetLocalizeStr(LocaleStrings.LS_MatchCaseStr);

  RadioGroupDirection.Caption := GetLocalizeStr(' ' + LocaleStrings.LS_DirectionCaptionStr + ' ');

  RadioGroupDirection_GetItems.Clear;
  RadioGroupDirection_GetItems.Add(GetLocalizeStr(LocaleStrings.LS_DirectionUpStr));
  RadioGroupDirection_GetItems.Add(GetLocalizeStr(LocaleStrings.LS_DirectionDownStr));
  RadioGroupDirection_SetItemIndex(1);
end;

function TfrmRLFindDialog.RadioGroupDirection_GetItems: TStrings;
begin
  Result := TStrings(GetObjectProp(RadioGroupDirection, 'Items'));
end;

function TfrmRLFindDialog.RadioGroupDirection_GetItemIndex: Integer;
begin
  Result := GetOrdProp(RadioGroupDirection, 'ItemIndex');
end;

procedure TfrmRLFindDialog.RadioGroupDirection_SetItemIndex(Value: Integer);
begin
  SetOrdProp(RadioGroupDirection, 'ItemIndex', Value);
end;

function TfrmRLFindDialog.GetTextValue: String;
begin
  Result := EditTextToFind.Text;
end;

procedure TfrmRLFindDialog.SetTextValue(const Value: String);
begin
  EditTextToFind.Text := Value;
end;

function TfrmRLFindDialog.GetOptions: TRLFindOptions;
begin
  Result := [];
  if CheckBoxWholeWords.Checked then
    Include(Result, foWholeWords);
  if CheckBoxMatchCase.Checked then
    Include(Result, foMatchCase);
  if RadioGroupDirection_GetItemIndex = 0 then
    Include(Result, foFindBackward);
end;

procedure TfrmRLFindDialog.SetOptions(const Value: TRLFindOptions);
begin
  CheckBoxWholeWords.Checked := foWholeWords in Value;
  CheckBoxMatchCase.Checked := foMatchCase in Value;
  RadioGroupDirection_SetItemIndex(1 - Byte(foFindBackward in Value));
end;

procedure TfrmRLFindDialog.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRLFindDialog.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmRLFindDialog.BitBtnFindNextClick(Sender: TObject);
var
  found: Boolean;
begin
  found := False;
  if Assigned(FOnFind) then
  begin
    Screen.Cursor := crHourGlass;
    try
      FOnFind(Self, Text, Options, found);
    finally
      Screen.Cursor := crDefault;
    end;
  end; 
  if not found then
	raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NotFoundStr));
end;

destructor TfrmRLFindDialog.Destroy;
begin
  inherited;
end;

end.

