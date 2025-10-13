object RLSaveDialog: TRLSaveDialog
  Left = 211
  Top = 407
  ActiveControl = EditFileName
  BorderStyle = bsDialog
  Caption = 'Salvar como'
  ClientHeight = 224
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelFileName: TLabel
    Left = 12
    Top = 16
    Width = 84
    Height = 13
    Caption = 'Nome do arquivo:'
  end
  object LabelUseFilter: TLabel
    Left = 12
    Top = 44
    Width = 86
    Height = 13
    Caption = 'Salvar no formato:'
  end
  object SpeedButtonLookup: TSpeedButton
    Left = 356
    Top = 12
    Width = 21
    Height = 21
    Caption = '...'
  end
  object GroupBoxPages: TGroupBox
    Left = 12
    Top = 68
    Width = 365
    Height = 101
    Caption = ' Páginas no intervalo  '
    TabOrder = 2
    object LabelFromPage: TLabel
      Left = 68
      Top = 45
      Width = 15
      Height = 13
      Caption = '&de:'
      FocusControl = EditFromPage
    end
    object LabelToPage: TLabel
      Left = 136
      Top = 45
      Width = 18
      Height = 13
      Caption = '&até:'
      FocusControl = EditToPage
    end
    object RadioButtonPagesAll: TRadioButton
      Left = 8
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Salvar &tudo'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonPagesInterval: TRadioButton
      Left = 8
      Top = 44
      Width = 61
      Height = 17
      Caption = 'Páginas'
      TabOrder = 1
    end
    object RadioButtonPagesSelect: TRadioButton
      Left = 8
      Top = 68
      Width = 73
      Height = 17
      Caption = '&Seleção'
      TabOrder = 2
    end
    object EditFromPage: TEdit
      Left = 88
      Top = 44
      Width = 41
      Height = 21
      TabStop = False
      TabOrder = 3
      Text = '1'
      OnChange = EditFromPageChange
    end
    object EditToPage: TEdit
      Left = 160
      Top = 44
      Width = 41
      Height = 21
      TabStop = False
      TabOrder = 4
      OnChange = EditFromPageChange
    end
  end
  object ButtonSave: TButton
    Left = 220
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Salvar'
    Default = True
    ModalResult = 0
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 304
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 4
  end
  object EditFileName: TEdit
    Left = 108
    Top = 12
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object ComboBoxFilters: TComboBox
    Left = 108
    Top = 40
    Width = 269
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBoxFiltersChange
  end
  object SaveDialog: TSaveDialog
    Left = 340
    Top = 80
  end
end
