object RLPrintDialog: TRLPrintDialog
  Left = 189
  Top = 520
  ActiveControl = ComboBoxPrinterNames
  AutoScroll = False
  AutoSize = True
  BorderWidth = 8
  Caption = 'Imprimir'
  ClientHeight = 291
  ClientWidth = 561
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
  object GroupBoxPrinter: TGroupBox
    Left = 0
    Top = 0
    Width = 561
    Height = 105
    Caption = 'Impressora'
    TabOrder = 0
    object LabelPrinterName: TLabel
      Left = 12
      Top = 24
      Width = 31
      Height = 13
      Caption = '&Nome:'
      FocusControl = ComboBoxPrinterNames
    end
    object LabelFilterName: TLabel
      Left = 12
      Top = 48
      Width = 47
      Height = 13
      Caption = 'Usar &filtro:'
      FocusControl = ComboBoxPrinterNames
    end
    object LabelOptions: TLabel
      Left = 12
      Top = 72
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Op'#231#245'es do filtro:'
      FocusControl = ComboBoxOptions
    end
    object ComboBoxPrinterNames: TComboBox
      Left = 68
      Top = 20
      Width = 365
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxPrinterNamesChange
    end
    object CheckBoxPrintToFile: TCheckBox
      Left = 440
      Top = 48
      Width = 113
      Height = 17
      TabStop = False
      Caption = 'Imprimir em arquivo'
      TabOrder = 4
    end
    object ComboBoxFilters: TComboBox
      Left = 68
      Top = 44
      Width = 365
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBoxFiltersChange
    end
    object ComboBoxOptions: TComboBox
      Left = 96
      Top = 68
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object ButtonPrinterSetup: TButton
      Left = 440
      Top = 20
      Width = 109
      Height = 21
      Caption = 'Propriedades'
      TabOrder = 3
      TabStop = False
      OnClick = ButtonPrinterSetupClick
    end
  end
  object GroupBoxPages: TGroupBox
    Left = 0
    Top = 108
    Width = 261
    Height = 145
    Caption = 'Intervalo de p'#225'ginas'
    TabOrder = 1
    object LabelToPage: TLabel
      Left = 172
      Top = 48
      Width = 18
      Height = 13
      Caption = '&at'#233':'
      FocusControl = EditToPage
    end
    object LabelPageSelectionHint: TLabel
      Left = 12
      Top = 96
      Width = 237
      Height = 41
      AutoSize = False
      Caption = 
        'Separe com ponto-e-v'#237'rgula os n'#250'meros e/ou intervalos de p'#225'ginas' +
        ' a serem impressos. Ex.: 1;3;5-12;4'
      WordWrap = True
    end
    object LabelFromPage: TLabel
      Left = 96
      Top = 49
      Width = 15
      Height = 13
      Caption = '&de:'
      FocusControl = EditFromPage
    end
    object RadioButtonPagesAll: TRadioButton
      Left = 12
      Top = 24
      Width = 113
      Height = 17
      Caption = '&Todas'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonPagesInterval: TRadioButton
      Left = 12
      Top = 48
      Width = 81
      Height = 17
      Caption = 'Intervalo'
      TabOrder = 1
    end
    object RadioButtonPagesSelect: TRadioButton
      Left = 12
      Top = 72
      Width = 65
      Height = 17
      Caption = 'Sele'#231#227'o'
      TabOrder = 4
    end
    object EditFromPage: TEdit
      Left = 116
      Top = 44
      Width = 49
      Height = 21
      TabStop = False
      TabOrder = 2
      Text = '1'
      OnChange = EditFromPageChange
    end
    object EditToPage: TEdit
      Left = 196
      Top = 44
      Width = 49
      Height = 21
      TabStop = False
      TabOrder = 3
      OnChange = EditFromPageChange
    end
    object EditPageSelection: TEdit
      Left = 96
      Top = 68
      Width = 149
      Height = 21
      TabOrder = 5
      OnChange = EditPageSelectionChange
    end
  end
  object GroupBoxCopies: TGroupBox
    Left = 268
    Top = 108
    Width = 293
    Height = 145
    Caption = 'C'#243'pias'
    TabOrder = 2
    object LabelCopies: TLabel
      Left = 12
      Top = 24
      Width = 89
      Height = 13
      Caption = 'N'#250'mero de &c'#243'pias:'
    end
    object LabelOddPages: TLabel
      Left = 28
      Top = 48
      Width = 73
      Height = 13
      Alignment = taRightJustify
      Caption = 'Pares/'#237'mpares:'
      FocusControl = ComboBoxOddPages
    end
    object EditCopies: TEdit
      Left = 108
      Top = 20
      Width = 49
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object ComboBoxOddPages: TComboBox
      Left = 108
      Top = 44
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Pares'
        #205'mpares'
        'Todas')
    end
  end
  object ButtonOk: TButton
    Left = 402
    Top = 266
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 486
    Top = 266
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 4
  end
end
