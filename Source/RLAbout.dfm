object frmRLAbout: TFormRLAbout
  Left = 250
  Top = 223
  ActiveControl = bbtOk
  BorderStyle = bsDialog
  Caption = ' '
  ClientHeight = 155
  ClientWidth = 373
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object imgLogo: TImage
    Left = 12
    Top = 12
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      07544269746D617076020000424D760200000000000076000000280000002000
      000020000000010004000000000000020000120B0000120B0000100000001000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00FFFFFFFFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF99FFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF99FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF99FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFF99FFFFFFFFFFFFFFFFFFFFFFFFFFFFF999FFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF99FFFFFFFFFFFFFFFFFFFFFFFFFFFFF999FFFFFFFFFFFF
      FFFFFFFFFFFF99FFF999FFFFFFFFFFFFFFFFFFFFFF99FFFF999FFFFFFFFFFFFF
      FFFFFFFF999FFFFF999FFFFFFFFFFFFFFFFFFFF9999FFFF9999FFFFFFFFFFFFF
      FFFFFF9999FFFFF9999FFFFFFFFFFFFFFFFFF99999FFFFF9999FFFFFFFFFFFFF
      FFFFF99999FFFF99999FFFFFFFFFFFFFFFFFF99999FFFF99999FFFFFFFFFFFFF
      FFFFF999999FFF99999FFFFFFFFFFFFFFFFFFF999999FF99999FFFFFFFFFFFFF
      FFFFFFF999999F99999FFFFFFFFFFFFFFFFFFFFFF9999999999FFFFFFFFFFFFF
      FFFFFFFFFFF999999999FFFFFFFFFFFFFFFFFFFFFFFFF9999999FFFFFFFFFFFF
      FFFFFFFFFFFFFF9999999FFFFFFFFF999FFFFF9999FFFFF999999999999999FF
      FFFFF999999FFFFF99999999999FFFFFFFFFF999999FFFFFF999999FFFFFFFFF
      FFFFF999999FFFFFFF9999999FFFFFFFFFFFF999999FFFFFFFFF99999999FFFF
      999FFF9999FFFFFFFFFFFF99999999999FFFFFFFFFFFFFFFFFFFFFFFF9999FFF
      FFFF}
  end
  object lblTitle: TLabel
    Left = 52
    Top = 12
    Width = 101
    Height = 19
    Caption = 'FortesReport'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 19
    Font.Name = 'Arial'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblVersion: TLabel
    Left = 52
    Top = 32
    Width = 65
    Height = 13
    Caption = 'Versão 0.2.06'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 13
    Font.Name = 'helvetica'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
  end
  object lblHome: TLabel
    Left = 52
    Top = 72
    Width = 169
    Height = 14
    Caption = 'http://www.fortesinformatica.com.br'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Pitch = fpVariable
    Font.Style = [fsUnderline]
    ParentFont = False
  end
  object lblCopyright: TLabel
    Left = 52
    Top = 56
    Width = 211
    Height = 14
    Caption = 'Copyright (c) 1998-2002  Fortes Informática'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
  end
  object bbtOk: TBitBtn
    Left = 256
    Top = 112
    Width = 99
    Height = 26
    Caption = '&Ok'
    TabOrder = 0
    Kind = bkOK
  end
end
