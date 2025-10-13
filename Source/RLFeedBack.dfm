object frmRLFeedBack: TfrmRLFeedBack
  Left = 291
  Top = 251
  Width = 393
  Height = 129
  HorzScrollBar.Range = 61
  VertScrollBar.Range = 45
  ActiveControl = BitBtnCancel
  AutoScroll = False
  Caption = 'Progresso'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStepName: TLabel
    Left = 14
    Top = 12
    Width = 76
    Height = 13
    Caption = 'LabelStepName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 12
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
  end
  object ProgressBar: TProgressBar
    Left = 14
    Top = 28
    Width = 355
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Min = 0
    Max = 100
    Step = 1
    TabOrder = 1
  end
  object BitBtnCancel: TBitBtn
    Left = 151
    Top = 65
    Width = 85
    Height = 25
    Anchors = [akBottom]
    Caption = '&Cancelar'
    TabOrder = 0
    OnClick = BitBtnCancelClick
    Kind = bkCancel
  end
  object TimerBlink: TTimer
    Enabled = False
    Interval = 300
    OnTimer = TimerBlinkTimer
    Left = 4
    Top = 64
  end
end
