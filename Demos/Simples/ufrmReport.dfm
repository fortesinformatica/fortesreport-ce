object frmReport: TfrmReport
  Left = 0
  Top = 0
  Caption = 'frmReport'
  ClientHeight = 520
  ClientWidth = 914
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
    Left = 16
    Top = 8
    Width = 794
    Height = 1123
    AllowedBands = [btTitle, btDetail]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Title = 'Aqui o Titulo do Relat'#243'rio na lista impressora'
    OnNeedData = RLReport1NeedData
    object RLBand1: TRLBand
      Left = 38
      Top = 38
      Width = 718
      Height = 67
      BandType = btTitle
      object RLLabel1: TRLLabel
        Left = 280
        Top = 32
        Width = 89
        Height = 16
        Caption = 'Carregar Texto'
      end
    end
    object RLBand2: TRLBand
      Left = 38
      Top = 105
      Width = 718
      Height = 32
      object rlmmTexto: TRLMemo
        Left = 0
        Top = 0
        Width = 718
        Height = 16
        Align = faClientTop
        Alignment = taJustify
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
      end
    end
    object RLBand3: TRLBand
      Left = 38
      Top = 137
      Width = 718
      Height = 40
      object RLSystemInfo1: TRLSystemInfo
        Left = 16
        Top = 6
        Width = 87
        Height = 16
        Info = itPageNumber
        Text = ''
      end
      object RLSystemInfo2: TRLSystemInfo
        Left = 680
        Top = 6
        Width = 36
        Height = 16
        Alignment = taRightJustify
        Info = itTitle
        Text = ''
      end
    end
  end
end
