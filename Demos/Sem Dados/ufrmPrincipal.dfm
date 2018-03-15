object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  Caption = 'frmPrincipal'
  ClientHeight = 386
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
  object Button1: TButton
    Left = 336
    Top = 24
    Width = 97
    Height = 25
    Caption = 'Adicionar Lista'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 32
    Top = 24
    Width = 265
    Height = 337
    ItemHeight = 13
    Items.Strings = (
      'Linha 1'
      'Linha 2'
      'Linha 3'
      'Linha 4'
      'Linha 5')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 358
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Imprimir'
    TabOrder = 2
    OnClick = Button2Click
  end
end
