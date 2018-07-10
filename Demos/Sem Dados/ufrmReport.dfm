object frmReport: TfrmReport
  Left = 0
  Top = 0
  Caption = 'frmReport'
  ClientHeight = 606
  ClientWidth = 919
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RLReport1: TRLReport
    Left = 24
    Top = 16
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    OnNeedData = RLReport1NeedData
    object RLBand1: TRLBand
      Left = 38
      Top = 38
      Width = 718
      Height = 91
      BandType = btTitle
      object RLLabel1: TRLLabel
        Left = 240
        Top = 40
        Width = 182
        Height = 16
        Caption = 'Relat'#243'rio Sem Fonte de Dados'
      end
    end
    object RLBand2: TRLBand
      Left = 38
      Top = 129
      Width = 718
      Height = 27
      object RLMemo1: TRLMemo
        Left = 3
        Top = 3
        Width = 185
        Height = 16
        Behavior = [beSiteExpander]
        BeforePrint = RLMemo1BeforePrint
      end
    end
  end
end
