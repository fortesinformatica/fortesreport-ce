object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  Caption = 'frmPrincipal'
  ClientHeight = 242
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnRelatorioTexto: TButton
    Left = 24
    Top = 16
    Width = 249
    Height = 25
    Caption = 'Relat'#243'rio - Carregar arquivo texto'
    TabOrder = 0
    OnClick = btnRelatorioTextoClick
  end
  object btnRelatorioImagem: TButton
    Left = 24
    Top = 56
    Width = 249
    Height = 25
    Caption = 'Relat'#243'rios - Carregar arquivo Imagem'
    TabOrder = 1
    OnClick = btnRelatorioImagemClick
  end
  object OpenDialog1: TOpenDialog
    Left = 360
    Top = 16
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 256
    Top = 128
  end
end
