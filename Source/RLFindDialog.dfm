object frmRLFindDialog: TfrmRLFindDialog
  Left = 504
  Top = 799
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Procurar'
  ClientHeight = 94
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTextToFind: TLabel
    Left = 8
    Top = 16
    Width = 30
    Height = 13
    Caption = 'Te&xto:'
  end
  object EditTextToFind: TEdit
    Left = 48
    Top = 12
    Width = 229
    Height = 21
    TabOrder = 0
  end
  object BitBtnFindNext: TBitBtn
    Left = 284
    Top = 12
    Width = 75
    Height = 21
    Caption = '&Próxima'
    Default = True
    TabOrder = 1
    OnClick = BitBtnFindNextClick
  end
  object BitBtnCancel: TBitBtn
    Left = 284
    Top = 36
    Width = 75
    Height = 21
    Cancel = True
    Caption = '&Cancelar'
    TabOrder = 2
    OnClick = BitBtnCancelClick
  end
  object CheckBoxWholeWords: TCheckBox
    Left = 8
    Top = 44
    Width = 133
    Height = 17
    Caption = 'Palavras &inteiras'
    TabOrder = 3
  end
  object CheckBoxMatchCase: TCheckBox
    Left = 8
    Top = 64
    Width = 193
    Height = 17
    Caption = 'Diferenciar &maiúsculas e minúsculas'
    TabOrder = 4
  end
  object RadioGroupDirection: TRadioGroup
    Left = 204
    Top = 36
    Width = 73
    Height = 49
    Caption = ' Direção '
    ItemIndex = 1
    Items.Strings = (
      'A&cima'
      'A&baixo')
    TabOrder = 5
  end
end
