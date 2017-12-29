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

{@unit RLPreviewForm - Implementação do form padrão de pré-visualização. }
unit RLPreviewForm;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
   Messages, SysUtils, Math, Contnrs, Classes,
  {$IfDef FPC}
   LMessages, LCLIntf, LCLType, LazFileUtils,
  {$EndIf}
  {$IfDef CLX}
   QTypes, QControls, QButtons, QExtCtrls, QForms, QDialogs, QStdCtrls, QGraphics, Qt,
  {$Else}
   Types, Controls, Buttons, ExtCtrls, Forms, Dialogs, StdCtrls, Graphics,
  {$EndIf}
  RLConsts, RLMetaFile, RLPreview, RLFilters, RLUtils, RLPrintDialog,
  RLSaveDialog, RLPrinters, RLTypes, RLFindDialog, RLComponentFactory;

{$IfDef FPC}
const
  CM_MOUSEWHEEL = LM_MOUSEWHEEL;
{$EndIf}

type

  { TRLPreviewForm }

  TRLPreviewForm = class(TForm)
    TimerRepeat: TTimer;
    PanelContainer: TPanel;
    PanelTools: TPanel;
    SpeedButtonPrint: TSpeedButton;
    SpeedButtonFirst: TSpeedButton;
    SpeedButtonPrior: TSpeedButton;
    SpeedButtonNext: TSpeedButton;
    SpeedButtonLast: TSpeedButton;
    SpeedButtonZoomDown: TSpeedButton;
    SpeedButtonZoomUp: TSpeedButton;
    SpeedButtonClose: TSpeedButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    SpeedButtonSave: TSpeedButton;
    SpeedButtonViews: TSpeedButton;
    Bevel5: TBevel;
    PanelPages: TPanel;
    LabelPage: TLabel;
    LabelOf: TLabel;
    EditPageNo: TEdit;
    PanelZoom: TPanel;
    ComboBoxZoom: TComboBox;
    PanelPageCount: TPanel;
    PanelCopyright: TPanel;
    SpeedButtonCopyright: TSpeedButton;
    SpeedButtonEdit: TSpeedButton;
    Bevel6: TBevel;
    SpeedButtonSend: TSpeedButton;
    procedure ComboBoxZoomChange(Sender: TObject);
    procedure SpeedButtonPrintClick(Sender: TObject);
    procedure SpeedButtonFirstClick(Sender: TObject);
    procedure SpeedButtonLastClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
    procedure SpeedButtonCloseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rpvDefaultChangeView(Sender: TObject);
    procedure TimerRepeatTimer(Sender: TObject);
    procedure SpeedButtonPriorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonPriorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditPageNoChange(Sender: TObject);
    procedure SpeedButtonZoomUpClick(Sender: TObject);
    procedure SpeedButtonZoomDownClick(Sender: TObject);
    procedure SpeedButtonViewsClick(Sender: TObject);
    procedure SpeedButtonEditClick(Sender: TObject);
    procedure SpeedButtonSendClick(Sender: TObject);
  private
    { Private declarations }
    FPreviewIndex: Integer;
    FPreviewList: TObjectList;
    FEditingZoom: Boolean;
    FEditingPageNo: Boolean;
    FFindDialog: TfrmRLFindDialog;
    FSpeedButtonCustomAction: TSpeedButton;
    //
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure Init;
    procedure NewPreview;
    procedure ReleasePreview;
    procedure OrganizePreviews;
    procedure SetPreview(APreview: TRLPreview);
    function GetPreview: TRLPreview;
    procedure PreviewEnter(Sender: TObject);
    procedure CreateCustomAction;
    procedure CreateShortCuts;
    procedure UpdateComboBoxZoom;
    procedure UpdateEditPageNo;
    procedure ShowFindDialog;
    procedure OnFindHandler(Sender: TObject; const AText: string; Options: TRLFindOptions; var Found: Boolean);
    procedure SpeedButtonCustomActionClick(Sender: TObject);
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  protected
    { Protected declarations }
    procedure DoClose(var Action: TCloseAction); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    //
    property Preview: TRLPreview read GetPreview;
  end;

  TRLPreviewFormButtons = (pbPrint, pbSave, pbSend, pbCustom);
  TRLPreviewFormButtonsSet = set of TRLPreviewFormButtons;
  TRLPreviewEditOptions = (eoCanReposition, eoCanResizeItems, eoCanEditText, eoCanDeleteItems, eoCanPointOut);
  TRLPreviewEditOptionsSet = set of TRLPreviewEditOptions;

  {@class TRLPreviewSetup - Opções do pré-visualizador padrão.
   Todos os relatórios que não tiverem suas próprias configurações de previsualização
   seguirão as regras estabelecidas neste componente. }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	 
  TRLPreviewSetup = class(TComponent)
  private
    FBeforePrint: TNotifyEvent;
    FAfterPrint: TNotifyEvent;
    FBeforeSave: TNotifyEvent;
    FAfterSave: TNotifyEvent;
    FBeforeSend: TNotifyEvent;
    FOnSend: TNotifyEvent;
    FAfterSend: TNotifyEvent;
    FEnabledButtons: TRLPreviewFormButtonsSet;
    FEditOptions: TRLPreviewEditOptionsSet;
    FCustomActionText: string;
    FOnCustomAction: TNotifyEvent;
    //
    function GetBorderIcons: TBorderIcons;
    function GetCaption: string;
    function GetFormStyle: TFormStyle;
    function GetHelpContext: Integer;
    function GetHelpFile: string;
    function GetPosition: TPosition;
    function GetSentToPrinter: Boolean;
    function GetShowModal: Boolean;
    function GetWindowBounds: TRect;
    function GetWindowState: TWindowState;
    function GetZoomFactor: Double;
    procedure SetBorderIcons(const Value: TBorderIcons);
    procedure SetCaption(const Value: string);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetHelpContext(const Value: Integer);
    procedure SetHelpFile(const Value: string);
    procedure SetPosition(const Value: TPosition);
    procedure SetSentToPrinter(const Value: Boolean);
    procedure SetShowModal(const Value: Boolean);
    procedure SetWindowBounds(const Value: TRect);
    procedure SetWindowState(const Value: TWindowState);
    procedure SetZoomFactor(const Value: Double);
    function IsZoomFactor: Boolean;
    procedure SetEnabledButtons(const Value: TRLPreviewFormButtonsSet);
    procedure SetEditOptions(const Value: TRLPreviewEditOptionsSet);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {@prop SentToPrinter - Indica se o relatório foi impresso ao menos uma vez. :/}
    property SentToPrinter: Boolean read GetSentToPrinter write SetSentToPrinter;

    {@prop WindowBounds - Indica ou determina as dimensões padrões da janela quando ela não está maximizada. :/}
    property WindowBounds: TRect read GetWindowBounds write SetWindowBounds;
  published
    {@prop WindowState - Indica o estado inicial da janela de preview. :/}
    property WindowState: TWindowState read GetWindowState write SetWindowState default wsMaximized;

    {@prop FormStyle - Indica o estilo da janela de preview. :/}
    property FormStyle: TFormStyle read GetFormStyle write SetFormStyle default fsNormal;

    {@prop ShowModal - Indica se a janela de preview será modal. :/}
    property ShowModal: Boolean read GetShowModal write SetShowModal default False;

    {@prop Position - Indica a posição da janela de preview. :/}
    property Position: TPosition read GetPosition write SetPosition default poScreenCenter;

    {@prop BorderIcons - Seleciona os botões da janela de preview. :/}
    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons default [biSystemMenu, biMinimize, biMaximize];

    {@prop HelpFile - Nome do arquivo de help para a janela preview, se houver. :/}
    property HelpFile: string read GetHelpFile write SetHelpFile;

    {@prop HelpContext - Contexto de help para a janela preview, se houver. :/}
    property HelpContext: Integer read GetHelpContext write SetHelpContext default 0;

    {@prop Caption - Título da janela de preview. :/}
    property Caption: string read GetCaption write SetCaption;

    {@prop ZoomFactor - Fator de zoom inicial (percentual). :/}
    property ZoomFactor: Double read GetZoomFactor write SetZoomFactor stored IsZoomFactor;

    {@prop EnabledButtons - Botões habilitados.
                            Através desta prop pode-se mostrar ou esconder botões da barra de ferramentas.
     @links TRLPreviewFormButtonsSet. :/}
    property EnabledButtons: TRLPreviewFormButtonsSet read FEnabledButtons write SetEnabledButtons default [pbPrint, pbSave, pbSend];

    property CustomActionText: string read FCustomActionText write FCustomActionText;
    property OnCustomAction: TNotifyEvent read FOnCustomAction write FOnCustomAction;

    {@prop EditOptions - Opções de edição (ainda não disponível).
     Determina que operações poderão ser realizadas pelo usuário no conteúdo do relatório
     já preparado.
     @links TRLPreviewEditOptions,TRLPreviewEditOptionsSet. :/}
    property EditOptions: TRLPreviewEditOptionsSet read FEditOptions write SetEditOptions default [];

    {@event BeforePrint - Sempre antes de imprimir ou quando o usuário pressiona o botão "Imprimir". :/}
    property BeforePrint: TNotifyEvent read FBeforePrint write FBeforePrint;

    {@event AfterPrint - Sempre após a impressão ou quando o filtro de impressão termina de processar todas as páginas. :/}
    property AfterPrint: TNotifyEvent read FAfterPrint write FAfterPrint;

    {@event BeforeSave - Sempre antes de salvar/exportar ou quando o usuário pressiona o botão "Salvar". :/}
    property BeforeSave: TNotifyEvent read FBeforeSave write FBeforeSave;

    {@event AfterSave - Sempre após o salvamento/exportação ou quando o filtro de salvamento termina de processar todas as páginas. :/}
    property AfterSave: TNotifyEvent read FAfterSave write FAfterSave;

    {@event BeforeSend - Sempre antes de enviar o relatório via e-mail ou quando o usuário pressionar o botão "Enviar". :/}
    property BeforeSend: TNotifyEvent read FBeforeSend write FBeforeSend;

    {@event OnSend - Sempre ao enviar um relatório via e-mail.
     O programador deve implementar este evento e providenciar o envio do relatório.
     O FortesReport não prove esta rotina, apenas fornece a interface para isso. :/}
    property OnSend: TNotifyEvent read FOnSend write FOnSend;

    {@event AfterSend - Sempre após a conclusão do envio do relatório via e-mail. :/}
    property AfterSend: TNotifyEvent read FAfterSend write FAfterSend;
  end;
  {/@class}

const
  ZoomFactorFullWidth = -1;
  ZoomFactorFullPage = -2;
  ZoomFactorMultiplePages = -3;

type
  TOnCreatePreviewFormProc = procedure(PreviewForm:TRLPreviewForm);

var
  DefaultWindowState: TWindowState = wsMaximized;
  DefaultWindowBounds: TRect;
  DefaultFormStyle: TFormStyle = fsNormal;
  DefaultShowModal: Boolean = False;
  DefaultPosition: TPosition = poScreenCenter;
  DefaultBorderIcons: TBorderIcons = [biSystemMenu, biMinimize, biMaximize];
  DefaultHelpFile: string = '';
  DefaultHelpContext: Integer = 0;
  DefaultCaption: string = '';
  DefaultZoomFactor: Double = 100;
  SentToPrinter: Boolean = False;
  OnCreatePreviewForm: TOnCreatePreviewFormProc = nil;
  ShowPreviewOnWindowsTaskBar: Boolean = False;

{@proc PreviewPagesWithOptions - Exibe o form padrão de pré-visualização com opções. :}
procedure PreviewPagesWithOptions(APages: TRLGraphicStorage; AShowModal: Boolean;
  AFormStyle: TFormStyle; APosition: TPosition; AWindowState: TWindowState;
  ABorderIcons: TBorderIcons; const AHelpFile: string; AHelpContext: Integer; ACaption: TCaption);
{/@proc}

{@proc PreviewPages - Exibe o form padrão de pré-visualização com as opções default. :/}
procedure PreviewPages(APages: TRLGraphicStorage);
{@proc PreviewFromFile - Carrega o arquivo de relatório e exibe o form padrão de pré-visualização. :/}
procedure PreviewFromFile(const AFileName: string);
{@proc PreviewFromStream - Carrega a stream de relatório e exibe o form padrão de pré-visualização. :/}
procedure PreviewFromStream(AStream: TStream);
{@proc PreviewFromFileDialog - Exibe diálogo para a carga de arquivo de relatório e exibe
 o form padrão de pré-visualização. :/}
procedure PreviewFromFileDialog;

{/@unit}

implementation

{$IfNDef FPC}
uses VCLCom;
{$endif}

///{$R *.dfm}

var
  SetupInstance: TRLPreviewSetup = nil;

procedure PreviewPagesWithOptions(APages: TRLGraphicStorage; AShowModal: Boolean; AFormStyle: TFormStyle; APosition: TPosition; AWindowState: TWindowState; ABorderIcons: TBorderIcons; const AHelpFile: string; AHelpContext: Integer; ACaption: TCaption);
begin
  SentToPrinter := False;
  with TRLPreviewForm.Create(nil) do
  begin
    Preview.Pages := APages;
    if DefaultZoomFactor = ZoomFactorFullWidth then
      Preview.ZoomFullWidth
    else if DefaultZoomFactor = ZoomFactorFullPage then
      Preview.ZoomFullPage
    else if DefaultZoomFactor = ZoomFactorMultiplePages then
      Preview.ZoomMultiplePages
    else if DefaultZoomFactor > 0 then
      Preview.ZoomFactor := DefaultZoomFactor;
    UpdateComboBoxZoom;
    Position := APosition;
    WindowState := AWindowState;
    BorderIcons := ABorderIcons;
    HelpFile := AHelpFile;
    HelpContext := AHelpContext;
    if ACaption <> '' then
      Caption := ACaption;
    FormStyle := AFormStyle;
    if AShowModal then
      ShowModal
    else if Visible then
      BringToFront
    else
      Show;
  end;
end;

procedure PreviewPages(APages: TRLGraphicStorage);
begin
  PreviewPagesWithOptions(APages, DefaultShowModal, DefaultFormStyle, DefaultPosition, DefaultWindowState, DefaultBorderIcons, DefaultHelpFile, DefaultHelpContext, DefaultCaption);
end;

procedure PreviewFromFile(const AFileName: string);
var
  savecursor: TCursor;
  pages: TRLGraphicStorage;
begin
  if not FileExists(AFileName) then
    raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_FileNotFoundStr + ' "' + AFileName + '"'));
  //
  pages := TRLGraphicStorage.Create;
  try
    savecursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      pages.LoadFromFile(AFileName);
    finally
      Screen.Cursor := savecursor;
    end;
    PreviewPages(pages);
  finally
    pages.Unlink;
  end;
end;

procedure PreviewFromStream(AStream: TStream);
var
  savecursor: TCursor;
  pages: TRLGraphicStorage;
begin
  pages := TRLGraphicStorage.Create;
  try
    savecursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      pages.LoadFromStream(AStream);
    finally
      Screen.Cursor := savecursor;
    end;
    PreviewPages(pages);
  finally
    pages.Unlink;
  end;
end;

procedure PreviewFromFileDialog;
var
  OpenDialog: TOpenDialog;
begin
  TRLComponentFactory.CreateComponent(TOpenDialog, nil, OpenDialog);
  with OpenDialog do
    try
      DefaultExt := FormatFileExt(ReportFileExt);
      Filter := AddFileFilter('', CS_ProductTitleStr, ReportFileExt);
      FilterIndex := 1;
      Title := GetLocalizeStr(LocaleStrings.LS_LoadReportStr);
      if Execute then
        PreviewFromFile(FileName);
    finally
      Free;
    end;
end;

{$ifdef VCL}
const
  key_escape = vk_escape;
  key_home = vk_home;
  key_prior = vk_prior;
  key_next = vk_next;
  key_end = vk_end;
  key_up = vk_up;
  key_down = vk_down;
  key_left = vk_left;
  key_right = vk_right;
  key_f3 = vk_f3;
  key_plus = 187;
  key_num_plus = vk_add;
  key_subtract = 189;
  key_num_subtract = vk_subtract;
{$endif}

constructor TRLPreviewForm.Create(AOwner: TComponent);
begin
  if ShowPreviewOnWindowsTaskBar then
    ParentWindow := 0;
  FPreviewList := nil;
  FPreviewIndex := 0;
  FEditingZoom := False;
  FEditingPageNo := False;
  FFindDialog := nil;
  FSpeedButtonCustomAction := nil;
  //
  inherited CreateNew(AOwner);
  //
  FPreviewList := TObjectList.Create;
  //
  Init;
  if Assigned(SetupInstance) and (pbCustom in SetupInstance.EnabledButtons) then
    CreateCustomAction;
  CreateShortCuts;
  //
  NewPreview;
  OrganizePreviews;
end;

destructor TRLPreviewForm.Destroy;
begin
  FPreviewList.Free;
  if Assigned(FFindDialog) then
    FFindDialog.Free;
  //
  inherited;
end;

procedure TRLPreviewForm.CreateCustomAction;
var
  C: TControl;
  I: Integer;
  B: TBitmap;
begin
  TRLComponentFactory.CreateComponent(TSpeedButton, Self, FSpeedButtonCustomAction);
  with FSpeedButtonCustomAction do
  begin
    Name := 'SpeedButtonCustomAction';
    Parent := PanelTools;
    if Assigned(SetupInstance) then
      Caption := SetupInstance.CustomActionText;
    Left := SpeedButtonClose.BoundsRect.Right;
    Top := SpeedButtonClose.Top;
    Height := SpeedButtonClose.Height;
    // copia a mesma margem dos outros botòes
    B := NeedAuxBitmap;
    B.Canvas.Font := PanelTools.Font;
    Width := B.Canvas.TextWidth(Caption) + SpeedButtonClose.Width - B.Canvas.TextWidth(SpeedButtonClose.Caption);
    //
    Flat := True;
    ParentShowHint := False;
    ShowHint := True;
    Spacing := -1;
    OnClick := SpeedButtonCustomActionClick;
    if Assigned(SetupInstance) then
      Enabled := @SetupInstance.OnCustomAction <> nil;
  end;
  for I := 0 to PanelTools.ControlCount - 1 do
  begin
    C := PanelTools.Controls[I];
    if (C.Align = alNone) and (C.Left > FSpeedButtonCustomAction.Left) then
      C.Left := C.Left + FSpeedButtonCustomAction.Width;
  end;
end;

procedure TRLPreviewForm.CreateShortCuts;
var
  L: TStringList;
  B: TSpeedButton;
  ch: char;
  K, N: string;
  I, J: Integer;
  function IsValidCaption(const ACaption: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(ACaption) do
      if CharInSet(ACaption[I], ['A'..'Z', 'a'..'z', '0'..'9']) then
      begin
        Result := True;
        Break;
      end;
  end;
begin
  L := TStringList.Create;
  try
    //
    for I := 0 to ComponentCount - 1 do
      if Components[I] is TSpeedButton then
      begin
        B := TSpeedButton(Components[I]);
        if IsValidCaption(B.Caption) then
          L.AddObject(B.Caption, B);
      end;
    //
    K := '';
    for I := 0 to L.Count - 1 do
    begin
      N := L[I];
      for J := 1 to Length(N) do
      begin
        ch := UpCase(N[J]);
        if CharInSet(ch, ['A'..'Z', '0'..'9']) and (Pos(ch, K) = 0) then
        begin
          Insert('&', N, J);
          L[I] := N;
          K := K + ch;
          Break;
        end;
      end;
    end;
    //
    for I := 0 to L.Count - 1 do
      TSpeedButton(L.Objects[I]).Caption := L[I];
  finally
    L.Free;
  end;
end;

procedure TRLPreviewForm.OrganizePreviews;
var
  I, T, W: Integer;
  N: TControl;
  S: TSplitter;
begin
  if FPreviewList.Count > 0 then
  begin
    for I := FPreviewList.Count - 1 downto 0 do
    begin
      N := TControl(TRLPreview(FPreviewList[I]));
      S := TSplitter(N.Tag);
      S.Align := alNone;
      N.Align := alNone;
    end;
    W := ClientHeight div FPreviewList.Count;
    T := 0;
    for I := 0 to FPreviewList.Count - 1 do
    begin
      N := TControl(TRLPreview(FPreviewList[I]));
      S := TSplitter(N.Tag);
      N.Height := W;
      if I = FPreviewList.Count - 1 then
      begin
        N.Align := alClient;
        S.Hide;
      end
      else
      begin
        N.Align := alTop;
        N.Height := W;
        N.Top := T;
        Inc(T, N.Height);
        S.Align := alTop;
        S.Height := 3;
        S.Visible := True;
        S.Top := T;
        Inc(T, S.Height);
      end;
    end;
  end;
end;

procedure TRLPreviewForm.NewPreview;
var
  N: TRLPreview;
  S: TSplitter;
begin
  N := TRLPreview.Create(nil);
  with N do
  begin
    Width := 0;
    OnChangeView := rpvDefaultChangeView;
    OnEnter := PreviewEnter;
  end;
  TControl(N).Parent := PanelContainer;
  TRLComponentFactory.CreateComponent(TSplitter, Self, S);
  with S do
  begin
    AutoSnap := False;
    Width := 0;
    Parent := PanelContainer;
  end;
  N.Tag := PtrInt(S);

  FPreviewList.Add(N);
  if FPreviewList.Count > 1 then
    N.Pages := TRLPreview(FPreviewList[0]).Pages;
end;

procedure TRLPreviewForm.ReleasePreview;
begin
  if FPreviewList.Count > 1 then
  begin
    FPreviewList.Delete(FPreviewList.Count - 1);
    if FPreviewIndex > FPreviewList.Count - 1 then
      SetPreview(TRLPreview(FPreviewList[FPreviewList.Count - 1]));
  end;
end;

procedure TRLPreviewForm.Init;
begin
  Left := 195;
  Top := 181;
  Width := 815;
  Height := 375;
  VertScrollBar.Range := 29;
  AutoScroll := False;
  Caption := 'Pré-visualização';
  KeyPreview := True;
  WindowState := wsNormal;
  OnKeyDown := FormKeyDown;
  OnKeyUp := FormKeyUp;
  PixelsPerInch := 96;
  TRLComponentFactory.CreateComponent(TPanel, Self, PanelContainer);
  with PanelContainer do
  begin
    Name := 'PanelContainer';
    Parent := Self;
    Left := 0;
    Top := 24;
    Width := 807;
    Height := 311;
    Align := alClient;
    BevelOuter := bvLowered;
    Caption := ' ';
    TabOrder := 1;
  end;
  TRLComponentFactory.CreateComponent(TPanel, Self, PanelTools);
  with PanelTools do
  begin
    Name := 'PanelTools';
    Parent := Self;
    Left := 0;
    Top := 0;
    Width := 798;
    Height := 23;
    Align := alTop;
    BevelOuter := bvNone;
    BorderWidth := 1;
    Caption := ' ';
    TabOrder := 2;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonPrint);
    with SpeedButtonPrint do
    begin
      Name := 'SpeedButtonPrint';
      Parent := PanelTools;
      Left := 1;
      Top := 1;
      Width := 50;
      Height := 22;
      Caption := 'Imprimir';
      Flat := True;
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonPrintClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonFirst);
    with SpeedButtonFirst do
    begin
      Name := 'SpeedButtonFirst';
      Parent := PanelTools;
      Left := 209;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Glyph := HexToBitmap(
        'D6000000424DD60000000000000076000000280000000E0000000C0000000100' +
        '04000000000060000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '7700777770777077770077770077007777007770C070C0777700770CE00CE000' +
        '070070CEEECEEEEEC70070E7FFE7FFFFC700770EFC0EFCCCC7007770EC70EC77' +
        '770077770C770C77770077777C777C7777007777777777777700');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonFirstClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonPrior);
    with SpeedButtonPrior do
    begin
      Name := 'SpeedButtonPrior';
      Parent := PanelTools;
      Left := 231;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Glyph := HexToBitmap(
        'D6000000424DD60000000000000076000000280000000E0000000C0000000100' +
        '04000000000060000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '7700777770777777770077770077777777007770C07777777700770CE0000000' +
        '070070CEEEEEEEEEC70070E7FFFFFFFFC700770EFCCCCCCCC7007770EC777777' +
        '770077770C777777770077777C77777777007777777777777700');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnMouseDown := SpeedButtonPriorMouseDown;
      OnMouseUp := SpeedButtonPriorMouseUp;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonNext);
    with SpeedButtonNext do
    begin
      Name := 'SpeedButtonNext';
      Parent := PanelTools;
      Left := 392;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Glyph := HexToBitmap(
        'D6000000424DD60000000000000076000000280000000E0000000C0000000100' +
        '04000000000060000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '770077777777077777007777777700777700777777770C077700700000000EC0' +
        '77007CEEEEEEEEEC07007CFFFFFFFF7E07007CCCCCCCCFE0770077777777CE07' +
        '770077777777C077770077777777C77777007777777777777700');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnMouseDown := SpeedButtonPriorMouseDown;
      OnMouseUp := SpeedButtonPriorMouseUp;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonLast);
    with SpeedButtonLast do
    begin
      Name := 'SpeedButtonLast';
      Parent := PanelTools;
      Left := 414;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Glyph := HexToBitmap(
        'D6000000424DD60000000000000076000000280000000E0000000C0000000100' +
        '04000000000060000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '77007777077707777700777700770077770077770C070C07770070000EC00EC0' +
        '77007CEEEEECEEEC07007CFFFF7EFF7E07007CCCCFE0CFE077007777CE07CE07' +
        '77007777C077C07777007777C777C77777007777777777777700');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonLastClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonZoomDown);
    with SpeedButtonZoomDown do
    begin
      Name := 'SpeedButtonZoomDown';
      Parent := PanelTools;
      Left := 447;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Hint := 'Você também pode aumentar ou reduzir o zoom do relatório'+sLineBreak+
              'precionando "Ctrl" e usando a rolagem do mouse.';
      ShowHint := True;
      Glyph := HexToBitmap(
        '4E010000424D4E01000000000000760000002800000012000000120000000100' +
        '040000000000D8000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '77777700000070000000007777777700000070FFF7F7F07777777700000070F7' +
        '7777707777000700000070F77777F07776F60700000070F7777770776FE60700' +
        '000070F7778800067E607700000070F77866666786077700000070F78EEEEE66' +
        '00777700000070F8777777EE60777700000070F877F777EE6077770000007008' +
        '7700000E60777700000077787FFFFF7E60777700000077787FFFFF7E60777700' +
        '0000777787FFFF7E077777000000777778777770777777000000777777880007' +
        '777777000000777777777777777777000000');
      Spacing := -1;
      OnClick := SpeedButtonZoomDownClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonZoomUp);
    with SpeedButtonZoomUp do
    begin
      Name := 'SpeedButtonZoomUp';
      Parent := PanelTools;
      Left := 469;
      Top := 1;
      Width := 22;
      Height := 22;
      Caption := ' ';
      Flat := True;
      Hint := 'Você também pode aumentar ou reduzir o zoom do relatório'+sLineBreak+
              'precionando "Ctrl" e usando a rolagem do mouse.';
      ShowHint := True;
      Glyph := HexToBitmap(
        '4E010000424D4E01000000000000760000002800000012000000120000000100' +
        '040000000000D8000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
        '77777700000070000000007777777700000070FFF7F7F07777777700000070F7' +
        '7777707777007700000070F77777F07776F60700000070F7777770776FE60700' +
        '000070F7778800067E600700000070F77866666786077700000070F78EEEEE66' +
        '00777700000070F8777707EE60777700000070F877F7077E6077770000007008' +
        '7700000E60777700000077787FF7077E60777700000077787FFF0F7E60777700' +
        '0000777787FFFF7E077777000000777778777770777777000000777777880007' +
        '777777000000777777777777777777000000');
      Spacing := -1;
      OnClick := SpeedButtonZoomUpClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonClose);
    with SpeedButtonClose do
    begin
      Name := 'SpeedButtonClose';
      Parent := PanelTools;
      Left := 151;
      Top := 1;
      Width := 49;
      Height := 22;
      Caption := 'Fechar';
      Flat := True;
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonCloseClick;
    end;
    TRLComponentFactory.CreateComponent(TBevel, Self, Bevel1);
    with Bevel1 do
    begin
      Name := 'Bevel1';
      Parent := PanelTools;
      Left := 204;
      Top := 1;
      Width := 2;
      Height := 21;
    end;
    TRLComponentFactory.CreateComponent(TBevel, Self, Bevel3);
    with Bevel3 do
    begin
      Name := 'Bevel3';
      Parent := PanelTools;
      Left := 441;
      Top := 1;
      Width := 2;
      Height := 21;
    end;
    TRLComponentFactory.CreateComponent(TBevel, Self, Bevel4);
    with Bevel4 do
    begin
      Name := 'Bevel4';
      Parent := PanelTools;
      Left := 640;
      Top := 1;
      Width := 2;
      Height := 21;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonSave);
    with SpeedButtonSave do
    begin
      Name := 'SpeedButtonSave';
      Parent := PanelTools;
      Left := 51;
      Top := 1;
      Width := 50;
      Height := 22;
      Caption := 'Salvar';
      Flat := True;
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonSaveClick;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonViews);
    with SpeedButtonViews do
    begin
      Name := 'SpeedButtonViews';
      Parent := PanelTools;
      Left := 646;
      Top := 1;
      Width := 22;
      Height := 22;
      Hint := 'V'#225'rias P'#225'ginas';
      Flat := True;
      Glyph := HexToBitmap(
        'EE000000424DEE000000000000007600000028000000100000000F0000000100' +
        '04000000000078000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBBBBB00BBB' +
        'BBBBBBBBBB0000BBBBBBBBBBB0B00B0BBBBBBBBBBBB00BBBBBBBBBBBBBB00BBB' +
        'BBBB8888888008888888F777777007777778F770000000000778F77777700777' +
        '7778FFFFFFF00FFFFFF8BBBBBBB00BBBBBBBBBBBBBB00BBBBBBBBBBBB0B00B0B' +
        'BBBBBBBBBB0000BBBBBBBBBBBBB00BBBBBBB');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonViewsClick;
    end;
    TRLComponentFactory.CreateComponent(TBevel, Self, Bevel5);
    with Bevel5 do
    begin
      Name := 'Bevel5';
      Parent := PanelTools;
      Left := 672;
      Top := 1;
      Width := 2;
      Height := 21;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonEdit);
    with SpeedButtonEdit do
    begin
      Name := 'SpeedButtonEdit';
      Parent := PanelTools;
      Left := 677;
      Top := 1;
      Width := 22;
      Height := 22;
      Hint := 'Editar';
      AllowAllUp := True;
      GroupIndex := 1;
      Flat := True;
      Glyph := HexToBitmap(
        'EE000000424DEE000000000000007600000028000000100000000F0000000100' +
        '04000000000078000000C40E0000C40E00001000000000000000000000000000' +
        '80000080000000808000800000008000800080800000C0C0C000808080000000' +
        'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBBBBBBBBBB' +
        'BBBBBBBBBBBBB00BBBBBBBBBBBBBB00BBBBBBBBBB0BB00BBBBBBBBBBB00000BB' +
        'BBBBBBBBB00000BBBBBBBBBBB0000000BBBBBBBBB000000BBBBBBBBBB00000BB' +
        'BBBBBBBBB0000BBBBBBBBBBBB000BBBBBBBBBBBBB00BBBBBBBBBBBBBB0BBBBBB' +
        'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB');
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonEditClick;
    end;
    TRLComponentFactory.CreateComponent(TBevel, Self, Bevel6);
    with Bevel6 do
    begin
      Name := 'Bevel6';
      Parent := PanelTools;
      Left := 703;
      Top := 1;
      Width := 2;
      Height := 21;
    end;
    TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonSend);
    with SpeedButtonSend do
    begin
      Name := 'SpeedButtonSend';
      Parent := PanelTools;
      Left := 101;
      Top := 1;
      Width := 50;
      Height := 22;
      Caption := 'Enviar';
      Flat := True;
      ParentShowHint := False;
      ShowHint := True;
      Spacing := -1;
      OnClick := SpeedButtonSendClick;
    end;
    TRLComponentFactory.CreateComponent(TPanel, Self, PanelPages);
    with PanelPages do
    begin
      Name := 'PanelPages';
      Parent := PanelTools;
      Left := 256;
      Top := 1;
      Width := 134;
      Height := 21;
      BevelOuter := bvNone;
      Caption := ' ';
      TabOrder := 0;;
      TRLComponentFactory.CreateComponent(TLabel, Self, LabelPage);
      with LabelPage do
      begin
        Name := 'LabelPage';
        Parent := PanelPages;
        Left := 0;
        Top := 4;
        Width := 33;
        Height := 13;
        Caption := 'P'#225'gina';
      end;
      TRLComponentFactory.CreateComponent(TLabel, Self, LabelOf);
      with LabelOf do
      begin
        Name := 'LabelOf';
        Parent := PanelPages;
        Left := 80;
        Top := 4;
        Width := 12;
        Height := 13;
        Caption := 'de';
      end;
      TRLComponentFactory.CreateComponent(TEdit, Self, EditPageNo);
      with EditPageNo do
      begin
        Name := 'EditPageNo';
        Parent := PanelPages;
        Left := 40;
        Top := 0;
        Width := 37;
        Height := 21;
        TabOrder := 0;
        OnChange := EditPageNoChange;
      end;
      TRLComponentFactory.CreateComponent(TPanel, Self, PanelPageCount);
      with PanelPageCount do
      begin
        Name := 'PanelPageCount';
        Parent := PanelPages;
        Left := 96;
        Top := 0;
        Width := 37;
        Height := 21;
        Alignment := taLeftJustify;
        BevelOuter := bvLowered;
        Caption := '99999';
        TabOrder := 1;
      end
    end;
    TRLComponentFactory.CreateComponent(TPanel, Self, PanelZoom);
    with PanelZoom do
    begin
      Name := 'PanelZoom';
      Parent := PanelTools;
      Left := 492;
      Top := 1;
      Width := 145;
      Height := 21;
      BevelOuter := bvNone;
      Caption := ' ';
      TabOrder := 1;;
      TRLComponentFactory.CreateComponent(TComboBox, Self, ComboBoxZoom);
      with ComboBoxZoom do
      begin
        Name := 'ComboBoxZoom';
        Parent := PanelZoom;
        Left := 0;
        Top := 0;
        Width := 145;
        Height := 21;
        DropDownCount := 11;
        ItemHeight := 13;
        TabOrder := 0;
        OnChange := ComboBoxZoomChange;
        Items.Text :=
          '500%'+sLineBreak+
          '200%'+sLineBreak+
          '150%'+sLineBreak+
          '100%'+sLineBreak+
          '75%'+sLineBreak+
          '50%'+sLineBreak+
          '25%'+sLineBreak+
          '10%'+sLineBreak+
          'Largura da página'+sLineBreak+
          'Página inteira'+sLineBreak+
          'Várias páginas'+sLineBreak;
      end;
    end;
    TRLComponentFactory.CreateComponent(TPanel, Self, PanelCopyright);
    with PanelCopyright do
    begin
      Name := 'PanelCopyright';
      Parent := PanelTools;
      Left := 775;
      Top := 1;
      Width := 22;
      Height := 21;
      Align := alRight;
      BevelOuter := bvNone;
      Caption := ' ';
      TabOrder := 2;;
      TRLComponentFactory.CreateComponent(TSpeedButton, Self, SpeedButtonCopyright);
      with SpeedButtonCopyright do
      begin
        Name := 'SpeedButtonCopyright';
        Parent := PanelCopyright;
        Left := 0;
        Top := 0;
        Width := 22;
        Height := 22;
        Caption := ' ';
        Flat := True;
        Glyph := HexToBitmap(
          '66010000424D6601000000000000760000002800000014000000140000000100' +
          '040000000000F0000000C40E0000C40E00001000000000000000000000000000' +
          '80000080000000808000800000008000800080800000C0C0C000808080000000' +
          'FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777' +
          '7777777700007777777777797777777700007777777777797777777700007777' +
          '7777779777777777000077777777799777777777000077777777797777777777' +
          '0000777777979977777777770000777997779977777777770000777997799977' +
          '7777777700007999777999777777777700007999777999777777777700007799' +
          '9979997777777777000077799979997777777777000077777999997777777777' +
          '0000777777799997777799770000779977799999999977770000799997779999' +
          '7777777700007999977777999997799700007799777777799999997700007777' +
          '77777777777777770000');
        ParentShowHint := False;
        ShowHint := True;
        Spacing := -1;
      end;
    end;
  end;
  TimerRepeat := TTimer.Create(Self);
  with TimerRepeat do
  begin
    Name := 'TimerRepeat';
    Interval := 100;
    OnTimer := TimerRepeatTimer;
    Left := 520;
    Top := 220;
  end;
  //
  Caption := GetLocalizeStr(LocaleStrings.LS_PreviewStr);
  SpeedButtonFirst.Hint := GetLocalizeStr(LocaleStrings.LS_FirstPageStr);
  SpeedButtonFirst.ShowHint := True;
  SpeedButtonPrior.Hint := GetLocalizeStr(LocaleStrings.LS_PriorPageStr);
  SpeedButtonPrior.ShowHint := True;
  SpeedButtonNext.Hint := GetLocalizeStr(LocaleStrings.LS_NextPageStr);
  SpeedButtonNext.ShowHint := True;
  SpeedButtonLast.Hint := GetLocalizeStr(LocaleStrings.LS_LastPageStr);
  SpeedButtonLast.ShowHint := True;
  SpeedButtonViews.Hint := GetLocalizeStr(LocaleStrings.LS_DivideScreenStr);
  SpeedButtonViews.ShowHint := True;
  SpeedButtonPrint.Hint := GetLocalizeStr(LocaleStrings.LS_PrintStr);
  SpeedButtonPrint.ShowHint := True;
  SpeedButtonPrint.Caption := GetLocalizeStr(LocaleStrings.LS_PrintStr);
  SpeedButtonSave.Hint := GetLocalizeStr(LocaleStrings.LS_SaveToFileStr);
  SpeedButtonSave.ShowHint := True;
  SpeedButtonSave.Caption := GetLocalizeStr(LocaleStrings.LS_SaveStr);
  SpeedButtonEdit.Hint := GetLocalizeStr(LocaleStrings.LS_EditStr);
  SpeedButtonEdit.ShowHint := True;
  SpeedButtonSend.Caption := GetLocalizeStr(LocaleStrings.LS_SendStr);
  SpeedButtonSend.Hint := GetLocalizeStr(LocaleStrings.LS_SendToStr);
  SpeedButtonSend.ShowHint := True;
  LabelPage.Caption := GetLocalizeStr(LocaleStrings.LS_PageStr);
  LabelOf.Caption := GetLocalizeStr(LocaleStrings.LS_OfStr);
  PanelPageCount.Caption := '0';
  SpeedButtonClose.Hint := GetLocalizeStr(LocaleStrings.LS_CloseStr);
  SpeedButtonClose.ShowHint := True;
  SpeedButtonClose.Caption := GetLocalizeStr(LocaleStrings.LS_CloseStr);
  SpeedButtonCopyright.Hint := GetLocalizeStr(CS_ProductTitleStr + '  ' + CS_Version);
  SpeedButtonCopyright.ShowHint := True;
  SpeedButtonZoomDown.Hint := GetLocalizeStr(LocaleStrings.LS_ZoomHint);
  SpeedButtonZoomUp.Hint := GetLocalizeStr(LocaleStrings.LS_ZoomHint);
  ComboBoxZoom.Items[8] := GetLocalizeStr(LocaleStrings.LS_EntireWidthStr);
  ComboBoxZoom.Items[9] := GetLocalizeStr(LocaleStrings.LS_EntirePageStr);
  ComboBoxZoom.Items[10] := GetLocalizeStr(LocaleStrings.LS_MultiplePagesStr);
  //
  if Assigned(SetupInstance) then
  begin
    SpeedButtonPrint.Enabled := (pbPrint in SetupInstance.EnabledButtons);
    SpeedButtonSave.Enabled := (pbSave in SetupInstance.EnabledButtons);
    SpeedButtonSend.Enabled := (pbSend in SetupInstance.EnabledButtons) and Assigned(SetupInstance.OnSend);
    SpeedButtonEdit.Enabled := (SetupInstance.EditOptions <> []);
  end
  else
    SpeedButtonSend.Enabled := False;

  if Assigned(OnCreatePreviewForm) then
    OnCreatePreviewForm(Self);
end;

procedure TRLPreviewForm.DoClose(var Action: TCloseAction);
begin
  DefaultWindowState := WindowState;
  DefaultWindowBounds := BoundsRect;
  //
  Action := caFree;
end;

procedure TRLPreviewForm.ComboBoxZoomChange(Sender: TObject);
var
  Z: Double;
  I, E: Integer;
begin
  FEditingZoom := True;
  try
    I := ComboBoxZoom.Items.IndexOf(ComboBoxZoom.Text);
    if I <> -1 then
      Z := 0
    else
    begin
      Val(StringReplace(StringReplace(ComboBoxZoom.Text, '%', '', []), ',', '.', []), Z, E);
      if E <> 0 then
        Z := 0;
      I := ComboBoxZoom.ItemIndex;
    end;
    if Z >= 10 then
      Preview.ZoomFactor := Z
    else
      case I of
        0: Preview.ZoomFactor := 500;
        1: Preview.ZoomFactor := 200;
        2: Preview.ZoomFactor := 150;
        3: Preview.ZoomFactor := 100;
        4: Preview.ZoomFactor := 75;
        5: Preview.ZoomFactor := 50;
        6: Preview.ZoomFactor := 25;
        7: Preview.ZoomFactor := 10;
        8: Preview.ZoomFullWidth;
        9: Preview.ZoomFullPage;
        10: Preview.ZoomMultiplePages;
      end;
  finally
    FEditingZoom := False;
  end;
end;

procedure TRLPreviewForm.EditPageNoChange(Sender: TObject);
begin
  FEditingPageNo := True;
  try
    Preview.PageIndex := StrToIntDef(EditPageNo.Text, Preview.PageIndex) - 1;
  finally
    FEditingPageNo := False;
  end;
end;

procedure TRLPreviewForm.SpeedButtonPrintClick(Sender: TObject);
var
  priorfocus: TWinControl;
  firstpg: Integer;
  lastpg: Integer;
  selection: string;
  dialog: TRLPrintDialog;
  oddp: Integer;
begin
  if Assigned(SetupInstance) and Assigned(SetupInstance.BeforePrint) then
    SetupInstance.BeforePrint(Self);
  RLPrinter.OddEven := odAllPages;
  priorfocus := Screen.ActiveControl;
  try
    dialog := TRLPrintDialog.CreateNew(nil);
    try
      dialog.MaxPage := Preview.Pages.PageCount;
      if Preview.Pages.Orientation = MetaOrientationLandscape then
        dialog.Orientation := poLandscape
      else
        dialog.Orientation := poPortrait;
      if not dialog.Execute then
        Exit;
      firstpg := dialog.FromPage;
      lastpg := dialog.ToPage;
      selection := dialog.PageRanges;
    finally
      dialog.Free;
    end;
    case RLPrinter.OddEven of
      odOddPagesOnly: oddp := PrintOddPagesOnly;
      odEvenPagesOnly: oddp := PrintEvenPagesOnly;
    else
      oddp := PrintOddAndEvenPages;
    end;
    FilterPages(Preview.Pages, nil, firstpg, lastpg, selection, oddp);
    SentToPrinter := True;
  finally
    // controle de foco para CLX
    if not Assigned(priorfocus) or not ((priorfocus is TCustomEdit) or (priorfocus is TCustomComboBox)) then
      priorfocus := EditPageNo;
    priorfocus.SetFocus;
  end;
  if Assigned(SetupInstance) and Assigned(SetupInstance.AfterPrint) then
    SetupInstance.AfterPrint(Self);
end;

procedure TRLPreviewForm.SpeedButtonFirstClick(Sender: TObject);
begin
  Preview.FirstPage;
end;

procedure TRLPreviewForm.SpeedButtonLastClick(Sender: TObject);
begin
  Preview.LastPage;
end;

function FileNameFromText(const AText: string): string;
var
  I: Integer;
begin
  Result := Trim(AText);
  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], [#0..#31, #127, '?', '*', ':', '/', '\', '>', '<', '|', '"', '.']) then
      Delete(Result, I, 1);
end;

procedure TRLPreviewForm.SpeedButtonSaveClick(Sender: TObject);
var
  priorfocus: TWinControl;
  filt: TRLCustomSaveFilter;
  fname: string;
  fext: string;
  firstpg: Integer;
  lastpg: Integer;
begin
  if Assigned(SetupInstance) and Assigned(SetupInstance.BeforeSave) then
    SetupInstance.BeforeSave(Self);
  priorfocus := Screen.ActiveControl;
  try
    with TRLSaveDialog.CreateNew(nil) do
      try
        MaxPage := Preview.Pages.PageCount;
        if Self.Preview.Pages.Title <> '' then
        {$IFDEF FPC}
         FileName := ExpandFileNameUTF8(FileNameFromText(Self.Preview.Pages.Title))
        {$ELSE}
         FileName := ExpandFileName(FileNameFromText(Self.Preview.Pages.Title))
        {$ENDIF}
        else if (SelectedFilter <> nil) and (SelectedFilter is TRLCustomSaveFilter) then
          FileName := TRLCustomSaveFilter(SelectedFilter).FileName
        else
          FileName := '';
        if not Execute then
          Exit;
        firstpg := FromPage;
        lastpg := ToPage;
        fname := FileName;
        fext := ExtractFileExt(fname);
        if fext = '' then
          ApplyExt(fname);
        filt := SaveFilterByFileName(fname);
        if filt <> nil then
        begin
          filt.FileName := fname;
          filt.FilterPages(Preview.Pages, firstpg, lastpg, '', PrintOddAndEvenPages);
        end
        else
          Preview.Pages.SaveToFile(fname);
      finally
        Free;
      end;
  finally
    // controle de foco para CLX
    if not Assigned(priorfocus) or not ((priorfocus is TCustomEdit) or (priorfocus is TCustomComboBox)) then
      priorfocus := EditPageNo;
    priorfocus.SetFocus;
  end;
  if Assigned(SetupInstance) and Assigned(SetupInstance.AfterSave) then
    SetupInstance.AfterSave(Self);
end;

procedure TRLPreviewForm.SpeedButtonSendClick(Sender: TObject);
begin
  if Assigned(SetupInstance) and Assigned(SetupInstance.BeforeSend) then
    SetupInstance.BeforeSend(Self);
  if Assigned(SetupInstance) and Assigned(SetupInstance.OnSend) then
    SetupInstance.OnSend(Self);
  if Assigned(SetupInstance) and Assigned(SetupInstance.AfterSend) then
    SetupInstance.AfterSend(Self);
end;

procedure TRLPreviewForm.SpeedButtonCustomActionClick(Sender: TObject);
begin
  if @SetupInstance.OnCustomAction <> nil then
    SetupInstance.OnCustomAction(Sender);
end;

procedure TRLPreviewForm.SpeedButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRLPreviewForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    KEY_ESCAPE: SpeedButtonClose.Click;
    KEY_HOME: if ssCtrl in Shift then
                  Preview.FirstPage
                else
                  Preview.PageLeft;
    KEY_PRIOR: if ssCtrl in Shift then
                  Preview.PageTop
                else
                  Preview.PriorPage;
    KEY_NEXT: if ssCtrl in Shift then
                  Preview.PageBottom
                else
                  Preview.NextPage;
    KEY_END: if ssCtrl in Shift then
                  Preview.LastPage
                else
                  Preview.PageRight;
    KEY_UP: if ssCtrl in Shift then
                  Preview.HalfPageUp
                else
                  Preview.ScrollUp;
    KEY_DOWN: if ssCtrl in Shift then
                  Preview.HalfPageDown
                else
                  Preview.ScrollDown;
    KEY_LEFT: if ssCtrl in Shift then
                  Preview.HalfPageLeft
                else
                  Preview.ScrollLeft;
    KEY_RIGHT: if ssCtrl in Shift then
                  Preview.HalfPageRight
                else
                  Preview.ScrollRight;
    KEY_PLUS,
    KEY_NUM_PLUS: if ssCtrl in Shift then
                  Preview.ZoomIn;
    KEY_SUBTRACT,
    KEY_NUM_SUBTRACT: if ssCtrl in Shift then
                  Preview.ZoomOut;
    Ord('0'): if ssCtrl in Shift then
                  Preview.ZoomFactor := DefaultZoomFactor;
    Ord('F'),
    KEY_F3: if ssCtrl in Shift then
                  ShowFindDialog
                else
                  Exit;
    VK_CONTROL: Preview.EnableCapture;
  else
    Exit;
  end;
  //
  Key := 0;
end;

procedure TRLPreviewForm.ShowFindDialog;
begin
  if not Assigned(FFindDialog) then
  begin
    FFindDialog := TfrmRLFindDialog.CreateNew(nil);
    FFindDialog.OnFind := OnFindHandler;
  end;
  FFindDialog.ActiveControl := FFindDialog.EditTextToFind;
  FFindDialog.Show;
end;

procedure TRLPreviewForm.OnFindHandler(Sender: TObject; const AText: string; Options: TRLFindOptions; var Found: Boolean);
begin
  Found := Preview.FindText(AText, foWholeWords in Options, foMatchCase in Options, foFindBackward in Options);
end;

procedure TRLPreviewForm.UpdateComboBoxZoom;
var
  savedevent: TNotifyEvent;
begin
  if FEditingZoom then
    Exit;
  savedevent := ComboBoxZoom.OnChange;
  try
    ComboBoxZoom.ItemIndex := -1;
    ComboBoxZoom.Text := FloatToStr(Preview.ZoomFactor) + '%';
  finally
    ComboBoxZoom.OnChange := savedevent;
  end;
end;

procedure TRLPreviewForm.UpdateEditPageNo;
var
  savedevent: TNotifyEvent;
begin
  if FEditingPageNo then
    Exit;
  savedevent := EditPageNo.OnChange;
  try
    EditPageNo.Text := IntToStr(Preview.PageIndex + 1);
  finally
    EditPageNo.OnChange := savedevent;
  end;
end;

procedure TRLPreviewForm.rpvDefaultChangeView(Sender: TObject);
begin
  PanelPageCount.Caption := inttostr(Preview.Pages.PageCount);
  UpdateEditPageNo;
  UpdateComboBoxZoom;
end;

procedure TRLPreviewForm.TimerRepeatTimer(Sender: TObject);
begin
  if ShowPreviewOnWindowsTaskBar and not IsWindowEnabled(Handle) then
    EnableWindow(Handle,True);

  if TimerRepeat.Tag > 500 then
    if SpeedButtonPrior.Tag > 0 then
      if SpeedButtonNext.Tag > 0 then
        Exit
      else
        Preview.PriorPage
    else if SpeedButtonNext.Tag > 0 then
      Preview.NextPage
    else
      Exit;
  TimerRepeat.Tag := TimerRepeat.Tag + Integer(TimerRepeat.Interval);
end;

procedure TRLPreviewForm.SpeedButtonPriorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;
  TSpeedButton(Sender).Tag := 1;
  TimerRepeat.Tag := 0;
  if Sender = SpeedButtonPrior then
    Preview.PriorPage
  else if Sender = SpeedButtonNext then
    Preview.NextPage;
end;

procedure TRLPreviewForm.SpeedButtonPriorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;
  TSpeedButton(Sender).Tag := 0;
end;

procedure TRLPreviewForm.SpeedButtonZoomUpClick(Sender: TObject);
begin
  Preview.ZoomFactor := Preview.ZoomFactor + 10;
  UpdateComboBoxZoom;
end;

procedure TRLPreviewForm.SpeedButtonZoomDownClick(Sender: TObject);
begin
  Preview.ZoomFactor := Max(10, Preview.ZoomFactor - 10);
  UpdateComboBoxZoom;
end;

procedure TRLPreviewForm.SpeedButtonViewsClick(Sender: TObject);
begin
  if FPreviewList.Count > 1 then
    ReleasePreview
  else
    NewPreview;
  OrganizePreviews;
end;

procedure TRLPreviewForm.SpeedButtonEditClick(Sender: TObject);
begin
  Preview.Editing := not Preview.Editing;
end;

function TRLPreviewForm.GetPreview: TRLPreview;
begin
  Result := TRLPreview(FPreviewList[FPreviewIndex]);
end;

procedure TRLPreviewForm.PreviewEnter(Sender: TObject);
begin
  SetPreview(TRLPreview(Sender));
end;

procedure TRLPreviewForm.SetPreview(APreview: TRLPreview);
begin
  FPreviewIndex := FPreviewList.IndexOf(APreview);
  UpdateEditPageNo;
  UpdateComboBoxZoom;
end;

{ TRLPreviewSetup }

constructor TRLPreviewSetup.Create(AOwner: TComponent);
begin
  FBeforePrint := nil;
  FAfterPrint := nil;
  FBeforeSave := nil;
  FAfterSave := nil;
  FBeforeSend := nil;
  FOnSend := nil;
  FAfterSend := nil;
  FEnabledButtons := [pbPrint, pbSave, pbSend];
  FEditOptions := [];
  FCustomActionText := '';
  FOnCustomAction := nil;
  //
  inherited;
  //
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(SetupInstance) then
      raise Exception.Create(Format(GetLocalizeStr(LocaleStrings.LS_OnlyOneInstance), [ClassName]));
    SetupInstance := Self;
  end;
end;

destructor TRLPreviewSetup.Destroy;
begin
  if SetupInstance = Self then
    SetupInstance := nil;
  //
  inherited;
end;

function TRLPreviewSetup.GetBorderIcons: TBorderIcons;
begin
  Result := DefaultBorderIcons;
end;

function TRLPreviewSetup.GetCaption: string;
begin
  Result := DefaultCaption;
end;

function TRLPreviewSetup.GetFormStyle: TFormStyle;
begin
  Result := DefaultFormStyle;
end;

function TRLPreviewSetup.GetHelpContext: Integer;
begin
  Result := DefaultHelpContext;
end;

function TRLPreviewSetup.GetHelpFile: string;
begin
  Result := DefaultHelpFile;
end;

function TRLPreviewSetup.GetPosition: TPosition;
begin
  Result := DefaultPosition;
end;

function TRLPreviewSetup.GetSentToPrinter: Boolean;
begin
  Result := RLPreviewForm.SentToPrinter;
end;

function TRLPreviewSetup.GetShowModal: Boolean;
begin
  Result := DefaultShowModal;
end;

function TRLPreviewSetup.GetWindowBounds: TRect;
begin
  Result := DefaultWindowBounds;
end;

function TRLPreviewSetup.GetWindowState: TWindowState;
begin
  Result := DefaultWindowState;
end;

function TRLPreviewSetup.GetZoomFactor: Double;
begin
  Result := DefaultZoomFactor;
end;

function TRLPreviewSetup.IsZoomFactor: Boolean;
begin
  Result := (DefaultZoomFactor <> 100);
end;

procedure TRLPreviewSetup.SetBorderIcons(const Value: TBorderIcons);
begin
  DefaultBorderIcons := Value;
end;

procedure TRLPreviewSetup.SetCaption(const Value: string);
begin
  DefaultCaption := Value;
end;

procedure TRLPreviewSetup.SetEditOptions(const Value: TRLPreviewEditOptionsSet);
begin
  FEditOptions := Value;
end;

procedure TRLPreviewSetup.SetEnabledButtons(const Value: TRLPreviewFormButtonsSet);
begin
  FEnabledButtons := Value;
end;

procedure TRLPreviewSetup.SetFormStyle(const Value: TFormStyle);
begin
  DefaultFormStyle := Value;
end;

procedure TRLPreviewSetup.SetHelpContext(const Value: Integer);
begin
  DefaultHelpContext := Value;
end;

procedure TRLPreviewSetup.SetHelpFile(const Value: string);
begin
  DefaultHelpFile := Value;
end;

procedure TRLPreviewSetup.SetPosition(const Value: TPosition);
begin
  DefaultPosition := Value;
end;

procedure TRLPreviewSetup.SetSentToPrinter(const Value: Boolean);
begin
  RLPreviewForm.SentToPrinter := Value;
end;

procedure TRLPreviewSetup.SetShowModal(const Value: Boolean);
begin
  DefaultShowModal := Value;
end;

procedure TRLPreviewSetup.SetWindowBounds(const Value: TRect);
begin
  DefaultWindowBounds := Value;
end;

procedure TRLPreviewSetup.SetWindowState(const Value: TWindowState);
begin
  DefaultWindowState := Value;
end;

procedure TRLPreviewSetup.SetZoomFactor(const Value: Double);
begin
  DefaultZoomFactor := Value;
end;

procedure TRLPreviewForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if ShowPreviewOnWindowsTaskBar then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW or WS_EX_NOPARENTNOTIFY;
    {$IfDef MSWINDOWS}
     Params.WndParent := GetDesktopwindow;
    {$Else}
     Params.WndParent := Screen.ActiveForm.Handle;
    {$EndIf}
  end;
end;

procedure TRLPreviewForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if ShowPreviewOnWindowsTaskBar and not IsWindowEnabled(Handle) then
    EnableWindow(Handle,True);
end;

procedure TRLPreviewForm.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if GetKeyState(VK_CONTROL) < 0 then
  begin
    if Message.WheelDelta > 0 then
      Preview.ZoomIn
    else
      Preview.ZoomOut;
  end
  else if Message.WheelDelta > 0 then
      Preview.ScrollUp
    else
      Preview.ScrollDown;
end;

procedure TRLPreviewForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_CONTROL: Preview.DisableCapture;
  end;
end;

end.

