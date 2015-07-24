unit RLFeedBack;

{$ifdef FPC}
{$mode delphi}
{$endif}

{$I RLReport.inc}

interface

uses
  SysUtils, Classes,
{$ifdef VCL}
  Graphics, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Controls, ComCtrls,
{$else}
  QTypes, QGraphics, QForms, QDialogs, QStdCtrls, QExtCtrls, QButtons, QControls, QComCtrls,
{$endif}
  RLUtils, RLComponentFactory;

type
  TRLFeedBackCancelEvent = procedure(Sender: TObject; var CancelIt: Boolean) of object;

  TfrmRLFeedBack = class(TForm)
    BitBtnCancel: TBitBtn;
    ProgressBar: TProgressBar;
    TimerBlink: TTimer;
    LabelStepName: TLabel;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure TimerBlinkTimer(Sender: TObject);
  private
    { Private declarations }
    ProgressBarStage: TProgressBar;
    procedure Init;
  public
    { Public declarations }
    Canceled: Boolean;
    Finished: Boolean;
    OnCancel: TRLFeedBackCancelEvent;
    //
    constructor Create(const ATitle: String; ALevels: Integer = 1); reintroduce;
    destructor Destroy; override;
    //
    procedure SetMax(N: Integer);
    procedure NextLevel;
    procedure Tick;
    procedure Finish;
    procedure StepCaption(const ACaption: String);
  end;

var
  frmRLFeedBack: TfrmRLFeedBack;

implementation

///{$R *.DFM}

uses
  RLReport, RLConsts;

procedure TfrmRLFeedBack.Init;
begin
  Left := 289;
  Top := 252;
  Width := 393;
  Height := 129;
  HorzScrollBar.Range := 61;
  VertScrollBar.Range := 45;
  ActiveControl := BitBtnCancel;
  AutoScroll := False;
  Caption := 'Progresso';
  Color := clBtnFace;
  Font.Color := clWindowText;
  Font.Height := 11;
  Font.Name := 'MS Sans Serif';
  Font.Pitch := fpVariable;
  Font.Style := [];
  Position := poScreenCenter;
{$ifdef VCL}
  BorderStyle := bsDialog
{$else}
  BorderStyle := fbsDialog;
{$endif};
  PixelsPerInch := 96;
  TRLComponentFactory.CreateComponent(TLabel, Self, LabelStepName);
  with LabelStepName do
  begin
    Name := 'LabelStepName';
    Parent := Self;
    Left := 14;
    Top := 12;
    Width := 39;
    Height := 13;
    Caption := 'LabelStepName';
    Font.Color := clWindowText;
    Font.Height := 12;
    Font.Name := 'MS Sans Serif';
    Font.Pitch := fpVariable;
    Font.Style := [];
    ParentFont := False;
  end;
  TRLComponentFactory.CreateComponent(TProgressBar, Self, ProgressBar);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    Parent := Self;
    Left := 14;
    Top := 28;
    Width := 355;
    Height := 17;
    Min := 0;
    Max := 100;
    Step := 1;
  end;
  TRLComponentFactory.CreateComponent(TBitBtn, Self, BitBtnCancel);
  with BitBtnCancel do
  begin
    Name := 'BitBtnCancel';
    Parent := Self;
    Left := 151;
    Top := 65;
    Width := 85;
    Height := 25;
    Caption := '&Cancelar';
    TabOrder := 0;
    OnClick := BitBtnCancelClick;
    Kind := bkCancel;
  end;
  TimerBlink := TTimer.Create(Self);
  with TimerBlink do
  begin
    Name := 'TimerBlink';
    Enabled := False;
    Interval := 300;
    OnTimer := TimerBlinkTimer;
    Left := 4;
    Top := 64;
  end;
  //
  BitBtnCancel.Caption := GetLocalizeStr(LocaleStrings.LS_CancelStr);
  LabelStepName.Caption := GetLocalizeStr(LocaleStrings.LS_WaitStr);
end;

constructor TfrmRLFeedBack.Create(const ATitle: String; ALevels: Integer = 1);
var
  H, D: Integer;
begin
  inherited CreateNew(nil);
  Init;
  //
  D := Height - BitBtnCancel.Top;
  Caption := ATitle;
  H := 0;
  //
  ProgressBarStage := nil;
  if ALevels > 1 then
  begin
    TRLComponentFactory.CreateComponent(TProgressBar, Self, ProgressBarStage);
    with ProgressBarStage do
    begin
      Name := 'ProgressBarStage';
      Parent := ProgressBar.Parent;
      BoundsRect := ProgressBar.BoundsRect;
      Top := ProgressBar.Top + ProgressBar.Height + 2;
      Max := ALevels;
      Position := 0;
      Step := 1;
    end;
    Inc(H, ProgressBar.Height + 2);
  end;
  Height := Height + H;
  BitBtnCancel.Top := Height - D;
  OnCancel := nil;
  Canceled := False;
  Finished := False;
end;

procedure TfrmRLFeedBack.BitBtnCancelClick(Sender: TObject);
begin
  if BitBtnCancel.Kind = bkOk then
    Finished := True
  else
  begin
    Canceled := True;
    if Assigned(OnCancel) then
      OnCancel(Self, Canceled);
  end; 
end;

procedure TfrmRLFeedBack.SetMax(N: Integer);
begin
  ProgressBar.Max := N;
  ProgressBar.Position := 0;
end;

procedure TfrmRLFeedBack.StepCaption(const ACaption: String);
begin
  LabelStepName.Caption := ACaption;
  LabelStepName.Update;
end;

procedure TfrmRLFeedBack.Tick;
begin
  ProgressBar.StepIt;
  Application.ProcessMessages;
end;

procedure TfrmRLFeedBack.NextLevel;
begin
  ProgressBarStage.StepIt;
  ProgressBar.Position := 0;
end;

procedure TfrmRLFeedBack.Finish;
begin
  LabelStepName.Caption := GetLocalizeStr(LocaleStrings.LS_FinishedStr);
  BitBtnCancel.Kind := bkOk;
  BitBtnCancel.Caption := GetLocalizeStr(LocaleStrings.LS_CloseStr);
  BitBtnCancel.Default := True;
  TimerBlink.Enabled := True;
  while not Finished do 
    Application.ProcessMessages;
end;

procedure TfrmRLFeedBack.TimerBlinkTimer(Sender: TObject);
begin
  LabelStepName.Visible := not LabelStepName.Visible;
end;

destructor TfrmRLFeedBack.Destroy;
begin
  inherited;
end;

end.

