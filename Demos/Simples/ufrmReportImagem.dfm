object frmReportImagem: TfrmReportImagem
  Left = 0
  Top = 0
  Caption = 'frmReportImagem'
  ClientHeight = 527
  ClientWidth = 1130
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
    Left = 56
    Top = 8
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
      Height = 59
      BandType = btTitle
      object RLLabel1: TRLLabel
        Left = 248
        Top = 24
        Width = 134
        Height = 16
        Caption = 'Relat'#243'rio com Imagem'
      end
    end
    object RLBand2: TRLBand
      Left = 38
      Top = 97
      Width = 718
      Height = 158
      AutoSize = True
      object RLImage1: TRLImage
        Left = 3
        Top = 45
        Width = 710
        Height = 113
        Center = True
      end
    end
  end
end
