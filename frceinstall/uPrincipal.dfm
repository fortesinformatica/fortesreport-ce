object frmPrincipal: TfrmPrincipal
  Left = 359
  Top = 202
  ActiveControl = wizPgInicio
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Instalador FortesReport Community Edition'
  ClientHeight = 478
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object wizPrincipal: TJvWizard
    Left = 0
    Top = 0
    Width = 720
    Height = 478
    ActivePage = wizPgInicio
    ButtonBarHeight = 42
    ButtonStart.Caption = 'Para o in'#237'cio'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Width = 100
    ButtonLast.Caption = 'Para o fim'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Width = 100
    ButtonBack.Caption = '< &Voltar'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Width = 100
    ButtonNext.Caption = '&Pr'#243'ximo >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Width = 100
    ButtonFinish.Caption = '&Finalizar'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Width = 100
    ButtonCancel.Caption = 'Cancelar'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 100
    ButtonHelp.Caption = '&Ajuda'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Width = 100
    ShowRouteMap = True
    OnFinishButtonClick = wizPrincipalFinishButtonClick
    OnCancelButtonClick = wizPrincipalCancelButtonClick
    DesignSize = (
      720
      478)
    object wizPgInicio: TJvWizardWelcomePage
      Header.Visible = False
      Header.Height = 50
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Bem vindo a instala'#231#227'o do projeto ACBr'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Arial'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = False
      Header.Subtitle.Text = 
        'Este instalar o guiar'#225' no processo de instala'#231#227'o do projeto ACBr' +
        '.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Arial'
      Header.Subtitle.Font.Style = []
      VisibleButtons = [bkNext, bkCancel]
      Color = clWhite
      Caption = 'In'#237'cio'
      OnNextButtonClick = wizPgInicioNextButtonClick
      WaterMark.Visible = False
      WaterMark.Image.Alignment = iaCenter
      WaterMark.Image.Layout = ilTop
      WaterMark.Width = 80
      object Label6: TLabel
        Left = 25
        Top = 34
        Width = 418
        Height = 52
        Caption = 
          'Este assistente o guiar'#225' no processo de instala'#231#227'o do FortesRepo' +
          'rt Community Edition em seu computador.'#13#10#13#10#201' recomend'#225'vel fechar' +
          ' todos os outros aplicativos antes de continuar.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblUrlForum1: TLabel
        Left = 93
        Top = 230
        Width = 254
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://github.com/fortesinformatica/fortesreport-ce'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object lblUrlfrce1: TLabel
        Left = 93
        Top = 172
        Width = 156
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://www.fortesreport.com.br'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label19: TLabel
        Left = 25
        Top = 156
        Width = 352
        Height = 13
        Caption = 
          'Para maiores informa'#231#245'es sobre o FortesReport Community Edition ' +
          'visite:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label21: TLabel
        Left = 25
        Top = 214
        Width = 241
        Height = 13
        Caption = 'Para tirar d'#250'vidas, ajudar ou dar sugest'#245'es visite:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label11: TLabel
        Left = 93
        Top = 287
        Width = 137
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://tortoisesvn.tigris.org/'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label12: TLabel
        Left = 25
        Top = 271
        Width = 250
        Height = 13
        Caption = 'Para baixar o cliente de SVN TORTOISE visite o site:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 93
        Top = 184
        Width = 271
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://groups.yahoo.com/neo/groups/fortesreport/info'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label8: TLabel
        Left = 93
        Top = 335
        Width = 149
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://git-scm.com/downloads'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label20: TLabel
        Left = 25
        Top = 319
        Width = 145
        Height = 13
        Caption = 'Para baixar o GIT visite o site:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
    object wizPgConfiguracao: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Configurando a sua instala'#231#227'o'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Arial'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'Selecione as op'#231#245'es de instala'#231#227'o abaixo conforme as suas necess' +
        'idades'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Arial'
      Header.Subtitle.Font.Style = []
      Color = clWhite
      Caption = 'Configura'#231#245'es'
      OnNextButtonClick = wizPgConfiguracaoNextButtonClick
      object Label4: TLabel
        Left = 18
        Top = 157
        Width = 79
        Height = 13
        Caption = 'Vers'#227'o do delphi'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 196
        Top = 157
        Width = 52
        Height = 13
        Caption = 'Plataforma'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 18
        Top = 102
        Width = 321
        Height = 13
        Caption = 
          'Diret'#243'rio onde ser'#225' instalado (o diret'#243'rio ser'#225' criado se n'#227'o ex' +
          'istir)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnSelecDirInstall: TSpeedButton
        Left = 511
        Top = 116
        Width = 26
        Height = 24
        Hint = 'Clique para procurar pelo diret'#243'rio onde deseja instalar'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSelecDirInstallClick
      end
      object edtDelphiVersion: TComboBox
        Left = 18
        Top = 173
        Width = 172
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = edtDelphiVersionChange
      end
      object edtPlatform: TComboBox
        Left = 196
        Top = 173
        Width = 172
        Height = 21
        Style = csDropDownList
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 2
        Text = 'Win32'
        Items.Strings = (
          'Win32'
          'Win64')
      end
      object edtDirDestino: TEdit
        Left = 18
        Top = 118
        Width = 487
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'C:\fortesreport-ce'
      end
    end
    object wizPgObterFontes: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Obtendo os fontes atualizados'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Arial'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'O assistente far'#225' o download ou atualiza'#231#227'o dos fontes diretamen' +
        'te do reposit'#243'rio do ACBr neste momento.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Arial'
      Header.Subtitle.Font.Style = []
      Color = clWhite
      Caption = 'Reposit'#243'rio'
      OnEnterPage = wizPgObterFontesEnterPage
      object Label1: TLabel
        Left = 77
        Top = 177
        Width = 91
        Height = 13
        Caption = 'URL do  reposit'#243'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5788637
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblInfoObterFontes: TLabel
        Left = 77
        Top = 122
        Width = 103
        Height = 13
        Caption = 'Texto de informa'#231#227'o.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5788637
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnSVNCheckoutUpdate: TSpeedButton
        Left = 365
        Top = 220
        Width = 106
        Height = 25
        Caption = 'Checkout'
        OnClick = btnSVNCheckoutUpdateClick
      end
      object edtURL: TEdit
        Left = 77
        Top = 193
        Width = 394
        Height = 21
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'https://github.com/fortesinformatica/fortesreport-ce.git'
      end
      object ckbFecharTortoise: TCheckBox
        Left = 77
        Top = 260
        Width = 358
        Height = 17
        Caption = 
          'Fechar automaticamente o tortoise se n'#227'o ocorrem erros ou confli' +
          'tos?'
        TabOrder = 1
      end
    end
    object wizPgInstalacao: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Instala'#231#227'o'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Arial'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'Os pacotes escolhidos ser'#227'o instalados conforme as configura'#231#245'es' +
        ' escolhidas pelo usu'#225'rio'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Arial'
      Header.Subtitle.Font.Style = []
      Color = clWhite
      Caption = 'Instala'#231#227'o'
      OnEnterPage = wizPgInstalacaoEnterPage
      OnNextButtonClick = wizPgInstalacaoNextButtonClick
      object btnInstalarfrce: TSpeedButton
        Left = 431
        Top = 336
        Width = 106
        Height = 25
        Caption = 'Instalar'
        OnClick = btnInstalarfrceClick
      end
      object btnVisualizarLogCompilacao: TSpeedButton
        Left = 21
        Top = 336
        Width = 140
        Height = 25
        Caption = 'Visualizar log'
        Visible = False
        OnClick = btnVisualizarLogCompilacaoClick
      end
      object lstMsgInstalacao: TListBox
        Left = 20
        Top = 87
        Width = 516
        Height = 220
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object pgbInstalacao: TProgressBar
        Left = 21
        Top = 313
        Width = 516
        Height = 17
        TabOrder = 2
      end
      object pnlInfoCompilador: TPanel
        Left = 20
        Top = 15
        Width = 516
        Height = 66
        BevelOuter = bvLowered
        TabOrder = 0
        object lbInfo: TListBox
          Left = 1
          Top = 1
          Width = 514
          Height = 64
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          ParentColor = True
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object wizPgFinalizar: TJvWizardInteriorPage
      Header.Visible = False
      Header.ParentFont = False
      Header.Title.Color = clNone
      Header.Title.Text = 'Projeto ACBr'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Arial'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'O projeto ACBr foi instalado.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Arial'
      Header.Subtitle.Font.Style = []
      VisibleButtons = [bkFinish]
      Color = clWhite
      Caption = 'Fim'
      object Label3: TLabel
        Left = 39
        Top = 58
        Width = 401
        Height = 38
        Caption = 
          'A instala'#231#227'o do FortesReport Community Edition foi conclu'#237'da com' +
          ' '#234'xito.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5788637
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Font.Quality = fqClearType
        ParentFont = False
        WordWrap = True
      end
      object Label14: TLabel
        Left = 113
        Top = 202
        Width = 254
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://github.com/fortesinformatica/fortesreport-ce'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label15: TLabel
        Left = 113
        Top = 139
        Width = 124
        Height = 13
        Cursor = crHandPoint
        Caption = 'www.fortesreport.com.br'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
      object Label16: TLabel
        Left = 45
        Top = 123
        Width = 352
        Height = 13
        Caption = 
          'Para maiores informa'#231#245'es sobre o FortesReport Community Edition ' +
          'visite:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label18: TLabel
        Left = 45
        Top = 186
        Width = 241
        Height = 13
        Caption = 'Para tirar d'#250'vidas, ajudar ou dar sugest'#245'es visite:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label22: TLabel
        Left = 113
        Top = 221
        Width = 271
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://groups.yahoo.com/neo/groups/fortesreport/info'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = URLClick
      end
    end
    object wizMapa: TJvWizardRouteMapNodes
      Left = 0
      Top = 71
      Width = 166
      Height = 365
      ItemHeight = 30
      AllowClickableNodes = False
      Color = 5788637
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.Alignment = iaLeft
      Image.Layout = ilTop
      Indent = 15
      NodeColors.Selected = clBlack
      UsePageTitle = False
    end
    object pnlTopo: TPanel
      Left = 0
      Top = 0
      Width = 720
      Height = 71
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 4
      object Image1: TImage
        Left = 473
        Top = 9
        Width = 240
        Height = 50
        AutoSize = True
        Picture.Data = {
          0A544A504547496D616765E7100000FFD8FFE000104A46494600010101006000
          600000FFE100AE4578696600004D4D002A000000080001876900040000000100
          00001A000000000001928600070000007A0000002C00000000554E49434F4445
          0000430052004500410054004F0052003A002000670064002D006A0070006500
          67002000760031002E003000200028007500730069006E006700200049004A00
          470020004A00500045004700200076003600320029002C002000710075006100
          6C0069007400790020003D002000390030000AFFDB0043000302020302020303
          030304030304050805050404050A070706080C0A0C0C0B0A0B0B0D0E12100D0E
          110E0B0B1016101113141515150C0F171816141812141514FFDB004301030404
          05040509050509140D0B0D141414141414141414141414141414141414141414
          1414141414141414141414141414141414141414141414141414141414FFC000
          1108003200F003012200021101031101FFC4001F000001050101010101010000
          0000000000000102030405060708090A0BFFC400B51000020103030204030505
          04040000017D01020300041105122131410613516107227114328191A1082342
          B1C11552D1F02433627282090A161718191A25262728292A3435363738393A43
          4445464748494A535455565758595A636465666768696A737475767778797A83
          8485868788898A92939495969798999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8
          B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2
          F3F4F5F6F7F8F9FAFFC4001F0100030101010101010101010000000000000102
          030405060708090A0BFFC400B511000201020404030407050404000102770001
          02031104052131061241510761711322328108144291A1B1C109233352F01562
          72D10A162434E125F11718191A262728292A35363738393A434445464748494A
          535455565758595A636465666768696A737475767778797A8283848586878889
          8A92939495969798999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5
          C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FA
          FFDA000C03010002110311003F00FD2C92A16A9445F69BB8A2DC5771EA3B714C
          B8B796D65F2E55F98FDD61D1BE95CFD0F59357B10B1EB54A2D416F39B58A6BA4
          FF009EB1AE233F462403F866B91D6B50BEF1A6A89A5E990CC7438A6097B7AA87
          64F86F9915BBA8E87D79EDD7D2638C71801547403A01E95B4695D5E47354C572
          BE5819B0B5E432AC91DA4A1D4E47CC9FFC5574B63AB2DE4C206824826D9BC86D
          A47500F209F5ACA926666F2A3E0FEA7E959D25E4BA7EAD6D243D7CA9032EC241
          1B938ABE5515A1CBCD2AAD5CEDA8ACFB5D40DE4224427DD4C4723F5A9BCE97DF
          FEFD1FF1A44EC5AA2AAF9D2FBFFDFA3FE3479D2FBFFDFA3FE3408B54555F3A5F
          7FFBF47FC68F3A5F7FFBF47FC6802D51557CE97DFF00EFD1FF001A3CE97DFF00
          EFD1FF001A00B54555F3A5F7FF00BF47FC690CD37BFF00DF93FE34016E8ACF6B
          AB81D377FE0331FF00D9A9BF68B96EF20FF760DBFAB3628034A9AD2247F79957
          EA71599E66E277C99FFAE97007E8B524417F83683FF4CE124FE66802E7DA50FD
          D25FFDD04D1E6BF68F03FDA2054403B7699BEA42D2EC03AA203FED36681834AD
          DE545FF7466A3601FA99A5FA702A5DD8FE351FEEAD35BE6FE395BFDD18A06316
          12394B6553EAED4A5A45FBD34510FF00645218D5BFE584AFFEF37FF5E9446C3E
          EDAC6BFEF30FF0A450C2D137DEB877F65FFEB50AB0FF000DBC921F561FE352EE
          997AB4318FC4D31A63FC57B18FF740FF001A450F56947DCB654FAB014EC5CB77
          8D3F026ABF991375BC91BFDDFF00EB0A3FD1DBB5C49F839A02DE464D91FF0089
          A5B7D4FF002ABDE2CF9B4592203E69DD2007BAEF60A48F4201359B0C9E5DF5B3
          9E06FC1FC6B4BC48EAD6B6CBB864DD423AFF00B6295334C468D3478B6B5F0EB5
          CD3FE33E9BE379BC5D6DA3F82B4DB631BE9AEED1F1B0AF958FB9B3386CF5E381
          9E6B82F1CFC76D7BE22F8D97C19F0EE5C8959956E61708654519794B9C6D4033
          81C678C9E699F1723D5DBF688F0C58F8BA3953C097B78AB6B7473F6338425217
          6E88CD2000EEC6EDDC7159BF073C2FFF00083FED99F1034BB88FCB13E9925EE9
          A36ED1E4CB2C4E428F6CB2F1FDD35E14E52954F650BC6329D9EBAED7F927D0FD
          230B87A34F0BF5CC4B8D5A90A1CD0565CA92972EB6F8A516EF2BEC779A4FC26B
          1B20ADAC781B59D56E872FAA49AF472DC67BB0549536FD17F5AF50681236B08A
          3599D12D9957ED1B9E4C6531B8B1C93EB9C9AD9663D3BD665F2EFD420F9777EE
          9FF8377F12FBD7B51A10A317C8BF2FD0F82A98EAD8DAB1F6AF6F3935F736EDF2
          B1E1DFB6878DE7F869FB3CEBBAF5BB5D5BC90DDD9C7BACC98E4F9A655C6E0DD3
          9AF8EFE077C6B8FE2A5F69CB79F152DFC21AB4DAB43656DA26B3757924F79B9A
          3DAC862565C33314009EA0E78AFA8BFE0A21A3DEEAFF00B28F892D74FD3EEAFE
          EDAFAC0AC16768D24840B8524854C9E07B57CFDFF04F7FD94BC25E34F04DCF8B
          7C6FE14D521F1568DE2256D3A5B937168516248A58DBCB254301264E4839E949
          3B19D4A7CD2B22F7ED0DFB61F8D7E267ED257DF08FC15A5DC986D7576D0AD2DA
          D6F7ECDF6DB943B659256C70A183F5380A99EA6B03C69F1F3E277EC47F132C34
          DF15D85CEF9605BE16B6FA88BCB2BFB7DC5580DDB4860548CE030383D0F3CB7E
          D6FF0003FE247ECFDFB485E7C60F06E9D797BA45DEAC75DB3D52CED0DC2D8DD3
          92D2C33C6A0E14B17C1236B2B63208AE2AE344F8DFFF000516F8B9A5DC6A1A33
          C6914296126A50E9CF69A66976BB8B3B6E6CEE7CB336DDCCCC700600E2AF7395
          C795D99F4D7EDC7FB495B7873E20783EE2C356D4B4EB0D67C2F6DAB5BADBB491
          AB472BC854908DF7B18FCAB9D8FF00680D5BE0BFC63D1747D37E22DBF8BCCAF6
          6D245677CD7105C09CA836EC8598799838E0E4123A1E2B8BFF00829D7C31D5F4
          DF8B7E07D33C39E1DD6355D3349F075AD8452D9E9F2CCA0452CCAAACC8A46EDA
          07E75C9782FC05AF7EC55FB5CE9579A8781750F18F856C9E29D2E868925D9167
          3A2B0B88B6A109710927A739461C6E14F527A1EF5FB4C4DE3EF837E3AB99B5BD
          5AE6D2C35CBDBCB9D316D75391BF72B2E46541F9301D38FF000AEA208BC77E09
          FD97BC5DE33D77529858EB5169373A4DC47A8BCB28469B2D9E73192AEBF5E95C
          87FC15A341D67C5FA97C29BEF0E68DAAEB16A6C6FE4F3B4CD3E6902AB9B72A5B
          629DB91CE0E0D7A0FC71D1B5C7FF00825BF86ACAD34CD4A4D65746D0D5AD21B3
          91AE830921DC0A005F239CF1F5A64F44790FC33F1D9F195E58C77DF159BC3FA8
          4BA8456D069D79F6E9DEE37328521A305406276F27B73C575FFB547C79D62EFE
          3BEB1E14D4358B9D0341D2AEA2B458E10FB238D9159AE1A3520C84EE247B0005
          7C95F073C59E22F875259AEA7F02A6F196A71EA515D5BEAFABDA6A914B6D864D
          AA162DA85559777CC0F24E78AF60FDB57E3478AEF3E2EF8E74AF16FC15B7B9D3
          8BBD978775BBBD3AEACEFA0895022CE97301DB3AB3832047DC06EC703229742B
          A9F68FEC6F6F0DE5AF89AF2CBC7F6FE3BD355E186DA1B66988B7182C5A482521
          A272781D410A704D7D242DCFFCFB01FF006EE3FABD7E727FC12A7E09F8F341F1
          078A3C7FE23D3351D3340BED3534DB18F538195EF5FCD1234AB1BE08450980C4
          725CE3A1AFD1C5B73FF3EE3FF0147FF1556B6337B9326F8C606E4F60635FE59A
          915B3F79C7FC0A7FF015024641FF005647D2D80FEB561370FEF8FF00B6205310
          A367AC47FE045AA55DBD9907FBA948ACDFDE93FEFDD4818FF79FFEF9FF00EB50
          026EFF006DBF05A6B63FBD31FA0A7EEFF69FFEF9FF00EB534B7FD34907FC07FF
          00AD40C89954F58EE1BF13FE34DF2633FF002E7237FBC47F534F665FF9ED30FA
          2FFF005AA33247DEE271F81FF0A452B8E1181F76C57F12B4F532AFDDB545FF00
          8181FD2A1F321EF733FEBFE149E65BF7B99BFEFA3FE148AFEBA96775CF68A21F
          590FF8527FA5F6108FC58D57DF6DFF003F52FF00DFC6A3CCB3EF7527FDFD6FF1
          A07CBE5F81E29F12BC57AA69F71E45BAB431AF22402A8F84FC457DAC789B4113
          DD492C725DC65949E3383FD6BD3F55D1ED757B768AE625914FB57076BE0B7F0B
          F88F4EBB8049359C374B2158D0BB28C11D0727AD441D99D15E0E51BAE87A6DF4
          70DF472C13C51DC5BC836B433207461E841E0D796FC52F054B6BAF785BC7DA0D
          AB4BAC78619A2B8B38065AF34D90113C2A3BB203E620EE548EE2BD05B5B8BFE7
          DEFBFF0000A5FF00E2699FDB11F5FB3DF67FEBCE5FFE26B5A94E352367FD7632
          C2E2AA612A73C36D535D1A6ACD3F54EC5986EA1BBB78AE2DE559ADE65124722F
          21D4F2187D41A83EC326A3A8A08C296485CED65073F32FA9AACBA9431A844B4B
          C441D156CA50073E9B6B4BC3F235E6A8CEB6D72B1AC2C374D6CCA3258703701E
          9DAB492BC6CCE48C9C67CD12A496F359C9F344D13F6FDD81FAEEA649BE46CBAE
          F3D32D183FFB3576125A89576BC0197D1A243FD6B3AE3C371C99312B447D3CA5
          23F9D72B83E87A71C4A7F16873EAAD19CAA6D3D388C0FF00D9AA7B7BBB8B5C79
          67099CEC3182BF96EA9AE343BBB7FF00961E62FAA4487F4CD53688A920C441F4
          30AFF8D46A8E9F72A2EE74365E205650922FD9DBD906DFFD0AB4D24936E507CA
          791B5463FF0042AE2BCA3FF3CCFF00DFA5FF001A9ADE69ED1BF75B94775F2948
          FCB356A7DCE49E1BAC4F29F8E52EA9FF000B2912DA67D3ACFF00B32D165D63ED
          D340BA43C97332ADC98A26C480901497F947CBBBE5CD6843F16BC52DF135F4FF
          00F408AD975BFECAFECC95A0599E0C7FAF037F9DBC8FDE0217CBDBFF007D57AC
          43AA4336E5BBB304BAEC6610210CBE8467A568416D677371F6B8A085EE426CF3
          9604F302FF00773D71ED5C9F57973B94676BBBFF005FD7E87A5F5F846946955A
          29F2A695FCEDAF96DD3F3BB7E1BE17F8D1E24B8D1F46D4D35AD37C59A86A16B7
          72CFE1FB0B5092DA186191D5B2AE580DE891B6F1C9906DEC0937C54F122DB416
          9A5F8A347F113EA11594C6FAD6C10C7A6C935DC3094650F82196472AAE438319
          CE474F69F0EF84B4EF0A69F6F65A658ADBC5044B0AB796AD2145E819C9CB63DE
          ADA6936F6EB208AC628C48FE6384B68C6E7FEF1E793EF4D61EAF2A4E7AFCFF00
          CFFAE813CC30BED1CA1415AFD797BDD5F4E8BB6FF6AE703F0FED7555F1778E97
          50D4DF57F2AFADE3512DA46BB0FD921248C3719C8E071C67A935DDADB9EF6CB8
          F6B71FD1E95AC8AB315B5C16E588B78F9FAF34C16E57EF5BE07A9B407FF406CD
          76538724797D7F33C6AF57DB4F9ED6D12FB925D3D095542F58D57FED948BFE35
          3C7227AA7FDFE61FCEAB4722AFCA1914FA2CCF19FC9AAE2B4817ACD8FF0080B8
          FD2B439C914EEE818FFBB2E6A4E47693F306A1DEBDCC79FF006E32B4F503B221
          FF0075E801FBB1DE41F8534C83FE7AB0FAAFFF005A8391D5645FA1CD34CA17FE
          5B3A7FBCBFFD6A06279DE97483FDE028F3243F76E613FF0001FF00EBD279A5BE
          EDC42DFEF2FF00F5E8313B7FCB2B793FCFD291449FE93D9E16FF00809FF1A337
          7E909FC48A83ECE3BD8AFF00C0185279712F5B69D3FDD27FA1A455913EEB91FF
          002CE33FF6D0FF0085066B91FF002EEA7E927FF5AA02D0AF596E23FAEEA4F3A1
          FE1D4594FF00B457FA8A2E3E5F2FCCC16A866FE94515CC7A88EB34F25EC602DF
          312A324D58DA3D28A2BA96C78CF70DA3D28C0F4A28A6216928A28016AADFC31C
          96EC5D158E3F8803451532D8D29FC48E524E24207029BFC54515CE7B828A6331
          43B949561DC7068A285B9954F84EBB4C7692D54B31638EA4E6ADD145751E2BDC
          28A28A04232861860187A11595A82ADBB031288CFF00B0314514016AC5D9D3E6
          62DF53534D1AEDCED5CFD28A28194848CAC406207D6AEC2C59464E68A2801ED0
          A37DE456FA8AC9BD458CFC8A13FDD18A28A966B0196B3C9BB1E6363EA6B5A162
          DD493451422644D4D6456EAA0FD45145364C773FFFD9}
      end
      object Label9: TLabel
        Left = 14
        Top = 19
        Width = 444
        Height = 19
        Caption = 'Assistente de instala'#231#227'o FortesReport  Community Edition'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5788637
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label10: TLabel
        Left = 14
        Top = 41
        Width = 254
        Height = 13
        Cursor = crHandPoint
        Caption = 'https://github.com/fortesinformatica/fortesreport-ce'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5788637
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = URLClick
      end
    end
  end
end
