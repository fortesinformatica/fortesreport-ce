object RLPreviewForm: TRLPreviewForm
  Left = 214
  Top = 221
  Width = 806
  Height = 406
  VertScrollBar.Range = 29
  AutoScroll = False
  Caption = 'Pr'#233'-visualiza'#231#227'o'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  WindowState = wsMaximized
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PanelContainer: TPanel
    Left = 0
    Top = 23
    Width = 798
    Height = 349
    Align = alClient
    BevelOuter = bvLowered
    Caption = ' '
    TabOrder = 0
  end
  object PanelTools: TPanel
    Left = 0
    Top = 0
    Width = 798
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = ' '
    TabOrder = 1
    object SpeedButtonPrint: TSpeedButton
      Left = 1
      Top = 1
      Width = 50
      Height = 22
      Caption = 'Imprimir'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonPrintClick
    end
    object SpeedButtonFirst: TSpeedButton
      Left = 209
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        D6000000424DD60000000000000076000000280000000E0000000C0000000100
        04000000000060000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7700777770777077770077770077007777007770C070C0777700770CE00CE000
        070070CEEECEEEEEC70070E7FFE7FFFFC700770EFC0EFCCCC7007770EC70EC77
        770077770C770C77770077777C777C7777007777777777777700}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonFirstClick
    end
    object SpeedButtonPrior: TSpeedButton
      Left = 231
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        D6000000424DD60000000000000076000000280000000E0000000C0000000100
        04000000000060000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7700777770777777770077770077777777007770C07777777700770CE0000000
        070070CEEEEEEEEEC70070E7FFFFFFFFC700770EFCCCCCCCC7007770EC777777
        770077770C777777770077777C77777777007777777777777700}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnMouseDown = SpeedButtonPriorMouseDown
      OnMouseUp = SpeedButtonPriorMouseUp
    end
    object SpeedButtonNext: TSpeedButton
      Left = 392
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        D6000000424DD60000000000000076000000280000000E0000000C0000000100
        04000000000060000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        770077777777077777007777777700777700777777770C077700700000000EC0
        77007CEEEEEEEEEC07007CFFFFFFFF7E07007CCCCCCCCFE0770077777777CE07
        770077777777C077770077777777C77777007777777777777700}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnMouseDown = SpeedButtonPriorMouseDown
      OnMouseUp = SpeedButtonPriorMouseUp
    end
    object SpeedButtonLast: TSpeedButton
      Left = 414
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        D6000000424DD60000000000000076000000280000000E0000000C0000000100
        04000000000060000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77007777077707777700777700770077770077770C070C07770070000EC00EC0
        77007CEEEEECEEEC07007CFFFF7EFF7E07007CCCCFE0CFE077007777CE07CE07
        77007777C077C07777007777C777C77777007777777777777700}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonLastClick
    end
    object SpeedButtonZoomDown: TSpeedButton
      Left = 447
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D8000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777700000070000000007777777700000070FFF7F7F07777777700000070F7
        7777707777000700000070F77777F07776F60700000070F7777770776FE60700
        000070F7778800067E607700000070F77866666786077700000070F78EEEEE66
        00777700000070F8777777EE60777700000070F877F777EE6077770000007008
        7700000E60777700000077787FFFFF7E60777700000077787FFFFF7E60777700
        0000777787FFFF7E077777000000777778777770777777000000777777880007
        777777000000777777777777777777000000}
      Spacing = -1
      OnClick = SpeedButtonZoomDownClick
    end
    object SpeedButtonZoomUp: TSpeedButton
      Left = 469
      Top = 1
      Width = 22
      Height = 22
      Caption = ' '
      Flat = True
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D8000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777700000070000000007777777700000070FFF7F7F07777777700000070F7
        7777707777007700000070F77777F07776F60700000070F7777770776FE60700
        000070F7778800067E600700000070F77866666786077700000070F78EEEEE66
        00777700000070F8777707EE60777700000070F877F7077E6077770000007008
        7700000E60777700000077787FF7077E60777700000077787FFF0F7E60777700
        0000777787FFFF7E077777000000777778777770777777000000777777880007
        777777000000777777777777777777000000}
      Spacing = -1
      OnClick = SpeedButtonZoomUpClick
    end
    object SpeedButtonClose: TSpeedButton
      Left = 151
      Top = 1
      Width = 49
      Height = 22
      Caption = 'Fechar'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonCloseClick
    end
    object Bevel1: TBevel
      Left = 204
      Top = 1
      Width = 2
      Height = 21
    end
    object Bevel3: TBevel
      Left = 441
      Top = 1
      Width = 2
      Height = 21
    end
    object Bevel4: TBevel
      Left = 640
      Top = 1
      Width = 2
      Height = 21
    end
    object SpeedButtonSave: TSpeedButton
      Left = 51
      Top = 1
      Width = 50
      Height = 22
      Caption = 'Salvar'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonSaveClick
    end
    object SpeedButtonViews: TSpeedButton
      Left = 646
      Top = 1
      Width = 22
      Height = 22
      Hint = 'V'#225'rias P'#225'ginas'
      Flat = True
      Glyph.Data = {
        EE000000424DEE000000000000007600000028000000100000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBBBBB00BBB
        BBBBBBBBBB0000BBBBBBBBBBB0B00B0BBBBBBBBBBBB00BBBBBBBBBBBBBB00BBB
        BBBB8888888008888888F777777007777778F770000000000778F77777700777
        7778FFFFFFF00FFFFFF8BBBBBBB00BBBBBBBBBBBBBB00BBBBBBBBBBBB0B00B0B
        BBBBBBBBBB0000BBBBBBBBBBBBB00BBBBBBB}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonViewsClick
    end
    object Bevel5: TBevel
      Left = 672
      Top = 1
      Width = 2
      Height = 21
    end
    object SpeedButtonEdit: TSpeedButton
      Left = 677
      Top = 1
      Width = 22
      Height = 22
      Hint = 'Editar'
      AllowAllUp = True
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        EE000000424DEE000000000000007600000028000000100000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBBBBBBBBBB
        BBBBBBBBBBBBB00BBBBBBBBBBBBBB00BBBBBBBBBB0BB00BBBBBBBBBBB00000BB
        BBBBBBBBB00000BBBBBBBBBBB0000000BBBBBBBBB000000BBBBBBBBBB00000BB
        BBBBBBBBB0000BBBBBBBBBBBB000BBBBBBBBBBBBB00BBBBBBBBBBBBBB0BBBBBB
        BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB}
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonEditClick
    end
    object Bevel6: TBevel
      Left = 703
      Top = 1
      Width = 2
      Height = 21
    end
    object SpeedButtonSend: TSpeedButton
      Left = 101
      Top = 1
      Width = 50
      Height = 22
      Caption = 'Enviar'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      Spacing = -1
      OnClick = SpeedButtonSendClick
    end
    object PanelPages: TPanel
      Left = 256
      Top = 1
      Width = 134
      Height = 21
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 0
      object LabelPage: TLabel
        Left = 0
        Top = 4
        Width = 33
        Height = 13
        Caption = 'P'#225'gina'
      end
      object LabelOf: TLabel
        Left = 80
        Top = 4
        Width = 12
        Height = 13
        Caption = 'de'
      end
      object EditPageNo: TEdit
        Left = 40
        Top = 0
        Width = 37
        Height = 21
        TabOrder = 0
        OnChange = EditPageNoChange
      end
      object PanelPageCount: TPanel
        Left = 96
        Top = 0
        Width = 37
        Height = 21
        Alignment = taLeftJustify
        BevelOuter = bvLowered
        Caption = '99999'
        TabOrder = 1
      end
    end
    object PanelZoom: TPanel
      Left = 492
      Top = 1
      Width = 145
      Height = 21
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object ComboBoxZoom: TComboBox
        Left = 0
        Top = 0
        Width = 145
        Height = 21
        DropDownCount = 11
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBoxZoomChange
        Items.Strings = (
          '500%'
          '200%'
          '150%'
          '100%'
          '75%'
          '50%'
          '25%'
          '10%'
          'Largura da p'#225'gina'
          'P'#225'gina inteira'
          'V'#225'rias p'#225'ginas')
      end
    end
    object PanelCopyright: TPanel
      Left = 775
      Top = 1
      Width = 22
      Height = 21
      Align = alRight
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 2
      object SpeedButtonCopyright: TSpeedButton
        Left = 0
        Top = 0
        Width = 22
        Height = 22
        Caption = ' '
        Flat = True
        Glyph.Data = {
          66010000424D6601000000000000760000002800000014000000140000000100
          040000000000F0000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777700007777777777797777777700007777777777797777777700007777
          7777779777777777000077777777799777777777000077777777797777777777
          0000777777979977777777770000777997779977777777770000777997799977
          7777777700007999777999777777777700007999777999777777777700007799
          9979997777777777000077799979997777777777000077777999997777777777
          0000777777799997777799770000779977799999999977770000799997779999
          7777777700007999977777999997799700007799777777799999997700007777
          77777777777777770000}
        ParentShowHint = False
        ShowHint = True
        Spacing = -1
      end
    end
  end
  object TimerRepeat: TTimer
    Interval = 100
    OnTimer = TimerRepeatTimer
    Left = 520
    Top = 220
  end
end
