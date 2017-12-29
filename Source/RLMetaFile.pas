{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              Márcio Martins                                  }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informações você pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descrição...
******************************************************************************}

{$I RLReport.inc}

{@unit RLMetaFile - Implementação das classes e rotinas para manipulação de coleções gráficas. }
unit RLMetaFile;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils, Contnrs, Math,
  {$IfDef CLX}
   QTypes, QGraphics, QDialogs,
  {$Else}
   Types, Graphics, Dialogs,
  {$EndIf}
  RLUtils, RLConsts, RLTypes;

const
  MetaOrientationPortrait = 1;
  MetaOrientationLandscape = 2;

  MetaTextAlignmentLeft = 1;
  MetaTextAlignmentRight = 2;
  MetaTextAlignmentCenter = 3;
  MetaTextAlignmentJustify = 4;
                
  MetaTextLayoutTop = 1;
  MetaTextLayoutBottom = 2;
  MetaTextLayoutCenter = 3;
  MetaTextLayoutJustify = 4;

  MetaTextFlagAutoSize = 1;
  MetaTextFlagWordWrap = 2;
  MetaTextFlagIntegralHeight = 4;

  MetaBrushStyleSolid = 1;
  MetaBrushStyleClear = 2;
  MetaBrushStyleHorizontal = 3;
  MetaBrushStyleVertical = 4;
  MetaBrushStyleFDiagonal = 5;
  MetaBrushStyleBDiagonal = 6;
  MetaBrushStyleCross = 7;
  MetaBrushStyleDiagCross = 8;

  MetaFontPitchDefault = 1;
  MetaFontPitchVariable = 2;
  MetaFontPitchFixed = 3;

  MetaFontStyleBold = 1;
  MetaFontStyleItalic = 2;
  MetaFontStyleUnderline = 4;
  MetaFontStyleStrikeOut = 8;

  MetaPenModeBlack = 1;
  MetaPenModeWhite = 2;
  MetaPenModeNop = 3;
  MetaPenModeNot = 4;
  MetaPenModeCopy = 5;
  MetaPenModeNotCopy = 6;
  MetaPenModeMergePenNot = 7;
  MetaPenModeMaskPenNot = 8;
  MetaPenModeMergeNotPen = 9;
  MetaPenModeMaskNotPen = 10;
  MetaPenModeMerge = 11;
  MetaPenModeNotMerge = 12;
  MetaPenModeMask = 13;
  MetaPenModeNotMask = 14;
  MetaPenModeXor = 15;
  MetaPenModeNotXor = 16;

  MetaPenStyleSolid = 1;
  MetaPenStyleDash = 2;
  MetaPenStyleDot = 3;
  MetaPenStyleDashDot = 4;
  MetaPenStyleDashDotDot = 5;
  MetaPenStyleClear = 6;
  MetaPenStyleInsideFrame = 7;

const
  MAXPAGECACHE = 5;

type
  TRLGraphicObject = class;
  
  TRLMetaOrientation = Byte;

  TRLMetaTextFlags = Word;

  TRLMetaColor = packed record
    Red, Green, Blue: Byte;
  end;

  TRLMetaTextAlignment = Byte;
  TRLMetaTextLayout = Byte;

  TRLMetaPenMode = Byte;
  TRLMetaPenStyle = Byte;
  TRLMetaBrushStyle = Byte;
  TRLMetaFontCharset = Byte;
  TRLMetaFontStyles = Byte;
  TRLMetaFontPitch = Byte;

  TRLMetaRect = packed record
    Left, Top, Right, Bottom: Integer;
  end;

  TRLMetaPoint = packed record
    X, Y: Integer;
  end;

  TRLMetaPointArray = packed array of TRLMetaPoint;

  TRLMetaFontDescriptor = record
    Name: AnsiString;
    Styles: AnsiString;
    Flags: Integer;
    FontBBox: TRect;
    MissingWidth: Integer;
    StemV: Integer;
    StemH: Integer;
    ItalicAngle: Integer;
    CapHeight: Integer;
    XHeight: Integer;
    Ascent: Integer;
    Descent: Integer;
    Leading: Integer;
    MaxWidth: Integer;
    AvgWidth: Integer;
  end;

  TRLMetaFontMetrics = record
    TrueType: Boolean;
    BaseFont: AnsiString;
    FirstChar: Integer;
    LastChar: Integer;
    Widths: array[0..255] of Integer;
    FontDescriptor: TRLMetaFontDescriptor;
  end;

  TRLMetaPen = class
  private
    FUser: TRLGraphicObject;
    FColor: TRLMetaColor;
    FMode: TRLMetaPenMode;
    FStyle: TRLMetaPenStyle;
    FWidth: Integer;
  protected
    function GetColor: TRLMetaColor;
    procedure SetColor(const Value: TRLMetaColor);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetMode: TRLMetaPenMode;
    procedure SetMode(Value: TRLMetaPenMode);
    function GetStyle: TRLMetaPenStyle;
    procedure SetStyle(Value: TRLMetaPenStyle);
  public
    constructor Create(AUser: TRLGraphicObject);
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    //
    procedure Assign(AObject: TRLMetaPen);
    //
    procedure Inflate(AFactor: Double);
    //
    property Color: TRLMetaColor read GetColor write SetColor;
    property Mode: TRLMetaPenMode read GetMode write SetMode;
    property Style: TRLMetaPenStyle read GetStyle write SetStyle;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TRLMetaBrush = class
  private
    FUser: TRLGraphicObject;
    FColor: TRLMetaColor;
    FStyle: TRLMetaBrushStyle;
  protected
    function GetStyle: TRLMetaBrushStyle;
    procedure SetStyle(Value: TRLMetaBrushStyle);
    function GetColor: TRLMetaColor;
    procedure SetColor(const Value: TRLMetaColor);
  public
    constructor Create(AUser: TRLGraphicObject);
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    //
    procedure Assign(AObject: TRLMetaBrush);
    //
    property Color: TRLMetaColor read GetColor write SetColor;
    property Style: TRLMetaBrushStyle read GetStyle write SetStyle;
  end;

  TRLMetaFont = class
  private
    FUser: TRLGraphicObject;
    FPixelsPerInch: Integer;
    FCharset: TRLMetaFontCharset;
    FColor: TRLMetaColor;
    FHeight: Integer;
    FNameId: Integer;
    FPitch: TRLMetaFontPitch;
    FSize: Integer;
    FStyle: TRLMetaFontStyles;
  protected
    function GetName: string;
    procedure SetName(const Value: string);
    function GetCharset: TRLMetaFontCharset;
    procedure SetCharset(Value: TRLMetaFontCharset);
    function GetColor: TRLMetaColor;
    procedure SetColor(const Value: TRLMetaColor);
    function GetStyle: TRLMetaFontStyles;
    procedure SetStyle(Value: TRLMetaFontStyles);
    function GetSize: Integer;
    procedure SetSize(Value: Integer);
    function GetPixelsPerInch: Integer;
    procedure SetPixelsPerInch(Value: Integer);
    function GetPitch: TRLMetaFontPitch;
    procedure SetPitch(Value: TRLMetaFontPitch);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
  public
    constructor Create(AUser: TRLGraphicObject);
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    //
    procedure Assign(AObject: TRLMetaFont);
    //
    property PixelsPerInch: Integer read GetPixelsPerInch write SetPixelsPerInch;
    property Charset: TRLMetaFontCharset read GetCharset write SetCharset;
    property Color: TRLMetaColor read GetColor write SetColor;
    property Height: Integer read GetHeight write SetHeight;
    property Name: string read GetName write SetName;
    property Pitch: TRLMetaFontPitch read GetPitch write SetPitch;
    property Size: Integer read GetSize write SetSize;
    property Style: TRLMetaFontStyles read GetStyle write SetStyle;
  end;

  TRLGraphicStorage = class;
  TRLGraphicSurface = class;
  TRLGraphicObjectClass = class of TRLGraphicObject;

  TRLPageAllocation = class
  private
    FOffsets: array of Int64;
    FCount: Integer;
    function GetItems(I: Integer): Int64;
    procedure SetItems(I: Integer; Value: Int64);
  public
    constructor Create;
    function Add(Offset: Int64): Integer;
    procedure Clear;
    property Count: Integer read FCount;
    property Items[I: Integer]: Int64 read GetItems write SetItems; default;
  end;

  {@class TRLGraphicStorage - Coleção de páginas ou superfícies de desenho. }
  TRLGraphicStorage = class(TComponent)
  private
    // cache para páginas em memória
    FPageCache: TObjectList;
    // endereço das páginas em disco (stream)
    FPageAllocation: TRLPageAllocation;
    // arquivo temporário para armazenamento das páginas
    FTempStream: TStream;
    FTempFileName: String;
    // versão do arquivo carregado que indica também o formato da gravação
    FFileVersion: Integer;
    // metasímbolos
    FMacros: TStrings;
    // lista de referências à este objeto. quando não houver mais referências, o objeto é destruído
    FReferenceList: TList;
    // guarda referência à página no cache em memória
    procedure AddToCache(ASurface: TRLGraphicSurface);
    // retorna referência à página se ela estiver no cache em memória
    function GetFromCache(APageIndex: Integer): TRLGraphicSurface;
    // atualiza pendências do cache em disco
    procedure FlushCache;
    // instancia página e carrega do disco
    function LoadPageFromDisk(APageIndex: Integer): TRLGraphicSurface;
    // retorna referência à página quer esteja em disco ou cachê
    function GetPages(APageIndex: Integer): TRLGraphicSurface;
    // retorna a quantidade de páginas estocadas
    function GetPageCount: Integer;
    // força a criação do arquivo temporário
    procedure TempStreamNeeded;
    // armazena a página no espaço temporário em disco
    procedure StorePage(ASurface: TRLGraphicSurface);
    // recupera a página do espaço temporário em disco
    procedure RetrievePage(ASurface: TRLGraphicSurface);
    // policia o número da versão para gravação
    procedure SetFileVersion(AVersion: Integer);
    // getters e setters de símbolos especiais
    function GetFirstPageNumber: Integer;
    function GetHeight: Integer;
    function GetLastPageNumber: Integer;
    function GetOrientation: TRLMetaOrientation;
    function GetPaperHeight: Double;
    function GetPaperWidth: Double;
    function GetTitle: String;
    function GetWidth: Integer;
    function GetOrientedHeight: Integer;
    function GetOrientedPaperHeight: Double;
    function GetOrientedPaperWidth: Double;
    function GetOrientedWidth: Integer;
    procedure SetFirstPageNumber(const Value: Integer);
    procedure SetLastPageNumber(const Value: Integer);
    procedure SetOrientation(const Value: TRLMetaOrientation);
    procedure SetPaperHeight(const Value: Double);
    procedure SetPaperWidth(const Value: Double);
    procedure SetTitle(const Value: String);
    function GetJobTitle: String;
    procedure SetJobTitle(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent = nil); reintroduce;
    destructor Destroy; override;

    {@method Link - Cria uma referência para o componente.
     A instância é mantida até que não haja mais referências a ela. :/}
    procedure Link(AComponent: TComponent);

    {@method Unlink - Retira referência para o componente.
     Quando não houver mais referências, a instância é automaticamente liberada. :/}
    procedure Unlink(AComponent: TComponent = nil);

    {@method Add - Adiciona página à coleção. :/}
    procedure Add(ASurface: TRLGraphicSurface);

    {@method Update - Atualiza dados da página em disco. :/}
    procedure Update(ASurface: TRLGraphicSurface);

    {@method Clear - Libera todas as páginas da memória e do cachê. :/}
    procedure Clear;

    {@method SaveToFile - Salva páginas para uma arquivo em disco. :/}
    procedure SaveToFile(const AFileName: String);
    
    {@method LoadFromFile - Carrega páginas de um arquivo em disco. :/}
    procedure LoadFromFile(const AFileName: String);

    {@method SaveToStream - Salva páginas em uma stream. :/}
    procedure SaveToStream(AStream: TStream);

    {@method LoadFromStream - Carrega páginas de uma stream. :/}
    procedure LoadFromStream(AStream: TStream);

    {@prop Pages - Retorna página pelo índice. :/}
    property Pages[APageIndex: Integer]: TRLGraphicSurface read GetPages; default;

    {@prop PageCount - Retorna a quantidade páginas armazenadas. :/}
    property PageCount: Integer read GetPageCount;

    {@prop FileVersion - Indica versão do relatório carregado ou determina a versão do arquivo a ser gravado.
     Esta prop pode ser utilizada para converter arquivos de uma versão para outra, bastando para isso, carregar
     o arquivo, alterar a sua versão e salvá-lo novamente. :/}
    property FileVersion: Integer read FFileVersion write SetFileVersion;

    {@prop Macros - Lista de símbolos para tradução em tempo de visualização ou impressão. :/}
    property Macros: TStrings read FMacros;

    {@prop FirstPageNumber - Numeração para a primeira página.
     Este número é normalmente 1, mas o relatório pode ser parte de uma encadernação maior e por isso ter uma
     numeração intercalada. :/}
    property FirstPageNumber: Integer read GetFirstPageNumber write SetFirstPageNumber;

    {@prop LastPageNumber - Número da última página. :/}
    property LastPageNumber: Integer read GetLastPageNumber write SetLastPageNumber;

    {@prop Title - Título do relatório. :/}
    property Title: String read GetTitle write SetTitle;

    {@prop JobTitle - Título do relatório para o spool de impressão. :/}
    property JobTitle: String read GetJobTitle write SetJobTitle;

    {@prop Orientation - Orientação do papel. :/}
    property Orientation: TRLMetaOrientation read GetOrientation write SetOrientation;

    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property PaperWidth: Double read GetPaperWidth write SetPaperWidth;

    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property PaperHeight: Double read GetPaperHeight write SetPaperHeight;

    {@prop OrientedPaperWidth - Largura do papel orientado para leitura em milímetros. :/}
    property OrientedPaperWidth: Double read GetOrientedPaperWidth;

    {@prop OrientedPaperHeight - Altura do papel orientado para leitura em milímetros. :/}
    property OrientedPaperHeight: Double read GetOrientedPaperHeight;

    {@prop OrientedWidth - Largura da superfície orientada para leitura em pixels. :/}
    property OrientedWidth: Integer read GetOrientedWidth;

    {@prop OrientedHeight - Altura da superfície orientada para leitura em pixels. :/}
    property OrientedHeight: Integer read GetOrientedHeight;

    {@prop Width - Largura da superfície em pixels. :/}
    property Width: Integer read GetWidth;

    {@prop Height - Altura da superfície em pixels. :/}
    property Height: Integer read GetHeight;
  end;
  {/@class}

  {@class TRLGraphicSurface - Superfície de desenho.
   Assemelha-se ao TCanvas e, embora não haja qualquer relação hierárquica, contempla a maioria de seus métodos de
   desenho. }
  TRLGraphicSurface = class
  private
    // referência ao estoque. o estoque será avisado sempre que uma página for detruída para que seja excluída do cachê 
    FStorage: TRLGraphicStorage;
    // índice da página
    FPageIndex: Integer;
    // lista de objetos gráficos
    FObjects: TObjectList;
    // posição do cursor (caneta)
    FPenPos: TPoint;
    // largura, altura e orientação
    FWidth: Integer;
    FHeight: Integer;
    // prop de desenho atuais
    FBrush: TBrush;
    FFont: TFont;
    FPen: TPen;
    // margens para write e writeln
    FMargins: TRect;
    // indica se algo foi desenhado
    FOpened: Boolean;
    FModified: Boolean;
    // coleção de fontes
    FFonts: TStrings;
    // controle de clipping
    FClipStack: TList;
    FClipRect: TRect;
    // para livre uso
    FTag: Integer;
    // identificador de grupo e gerador
    FGeneratorId: PtrInt;
    // metasímbolos
    FMacros: TStrings;
    // retorna a quantidade de objetos incluídos
    function GetObjectCount: Integer;
    // referência ao objeto pelo índice
    function GetObjects(AIndex: Integer): TRLGraphicObject;
    // muda as props de desenho
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
    // desenho a nível de pontos
    function GetPixels(X, Y: Integer): TColor;
    procedure SetPixels(X, Y: Integer; const Value: TColor);
    //
    procedure SetStorage(AStorage: TRLGraphicStorage);
    // empilha o retângulo de corte 
    procedure PushClipRect(const ARect: TRect);
    // desempilha o retângulo de corte
    procedure PopClipRect(var ARect: TRect);
    // persistência de símbolos
    function GetOrientation: TRLMetaOrientation;
    procedure SetOrientation(const Value: TRLMetaOrientation);
    function GetPaperHeight: Double;
    function GetPaperWidth: Double;
    procedure SetPaperHeight(const Value: Double);
    procedure SetPaperWidth(const Value: Double);
    function GetOrientedPaperHeight: Double;
    function GetOrientedPaperWidth: Double;
    function GetOrientedHeight: Integer;
    function GetOrientedWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    {@method SaveToFile - Salva os dados da página em um arquivo. :/}
    procedure SaveToFile(const AFileName: String);

    {@method LoadFromFile - Restaura os dados da página de um arquivo. :/}
    procedure LoadFromFile(const AFileName: String);

    {@method SaveToStream - Salva os dados da página em uma stream. :/}
    procedure SaveToStream(AStream: TStream);

    {@method LoadFromStream - Carrega os dados da página de uma stream. :/}
    procedure LoadFromStream(AStream: TStream);

    {@method FindFreeRow - Retorna a altura neutra mais próxima da coordenada informada, aonde nenhum texto é cortado. :/}
    function FindFreeRow(ANearRow: Integer; var ARow: Integer): Boolean;

    {@method TextWidth - Retorna a largura do texto de acordo com a fonte atual. :/}
    function TextWidth(const AText: String): Integer;
    
    {@method TextHeight - Retorna a altura do texto de acordo com a fonte atual. :/}
    function TextHeight(const AText: String): Integer;

    {@method MoveTo - Posiciona o cursor de desenho e escrita. :/}
    procedure MoveTo(AX, AY: Integer);

    {@method LineTo - Traça uma linha reta ligando a posição atual do cursor às coordenadas passadas. :/}
    procedure LineTo(AX, AY: Integer);

    {@method Rectangle - Desenha um retângulo. :/}
    procedure Rectangle(ALeft, ATop, ARight, ABottom: Integer); overload;
    procedure Rectangle(const ARect: TRect); overload;
    {/@method}

    {@method Ellipse - Desenha uma ellipse. :/}
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer); overload;
    procedure Ellipse(const ARect: TRect); overload;
    {/@method}

    {@method Polygon - Desenha um polígono. :/}
    procedure Polygon(const APoints: array of TPoint);

    {@method Arc - Draw the arc. :/}
    procedure Arc(const ARectEllipse, ARectArc: TRect); overload;
    procedure Arc(const AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer); overload;

    {@method Polyline - Desenha uma série de linhas ligando os pontos passados. :/}
    procedure Polyline(const APoints: array of TPoint);

    {@method Write - Escreve um texto na posição atual do cursor. :/}
    procedure Write(const AText: String);

    {@method WriteLn - Escreve um texto na posição atual do cursor e salta para a linha seguinte. :/}
    procedure Writeln(const AText: String);

    {@method TextOut - Escreve um texto na posição informada. :/}
    procedure TextOut(ALeft, ATop: Integer; const AText: AnsiString);
    procedure TextOutEx(ALeft, ATop: Integer; const AText: AnsiString; ATextFlags: TRLMetaTextFlags);
    {/@method}

    {@method TextRect - Escreve um texto delimitado pelo retângulo informado. :/}
    procedure TextRect(const ARect: TRect; ALeft, ATop: Integer; const AText: AnsiString);
    procedure TextRectEx(const ARect: TRect; ALeft, ATop: Integer; const AText: AnsiString; AAlignment: TRLMetaTextAlignment; ALayout: TRLMetaTextLayout; ATextFlags: TRLMetaTextFlags);
    {/@method}

    {@method FillRect - Preenche um retângulo com os padrões definidos na prop Brush. :/}
    procedure FillRect(const ARect: TRect);
    
    {@method Draw - Desenha a imagem nas coordenadas indicadas mantendo seu tamanho e proporção. :}
    procedure Draw(AX, AY: Integer; AGraphic: TGraphic; AParity: Boolean = False); overload;
    procedure Draw(AX, AY: Integer; ASurface: TRLGraphicSurface); overload;
    {/@method}

    {@method StretchDraw - Desenha uma imagem alterando características de modo a preencher todo o retângulo. :}
    procedure StretchDraw(const ARect: TRect; AGraphic: TGraphic; AParity: Boolean = False); overload;
    procedure StretchDraw(const ARect: TRect; ASurface: TRLGraphicSurface); overload;
    {/@method}

    {@method ScaleDraw - Desenha uma imagem contida num retângulo respeitando suas proporções. :}
    procedure ScaleDraw(const ARect: TRect; AGraphic: TGraphic; ACenter: Boolean); overload;
    procedure ScaleDraw(const ARect: TRect; ASurface: TRLGraphicSurface; ACenter: Boolean); overload;
    {/@method}

    {@method ClipDraw - Desenha um corte de uma imagem de modo a interceptar o retângulo. :}
    procedure ClipDraw(const ARect: TRect; AGraphic: TGraphic; ACenter: Boolean); overload;
    procedure ClipDraw(const ARect: TRect; ASurface: TRLGraphicSurface; ACenter: Boolean); overload;
    {/@method}

    {@method CopyRect - Copia os objetos que interceptam o retângulo para uma outra superfície. :}
    procedure CopyRect(const ADest: TRect; ACanvas: TCanvas; const ASource: TRect); overload;
    procedure CopyRect(const ADest: TRect; ASurface: TRLGraphicSurface; const ASource: TRect); overload;
    {/@method}

    {@method SetClipRect - Determina um novo retângulo de corte para desenho e retorna a definição antiga. :/}
    procedure SetClipRect(const ARect: TRect);

    {@method ResetClipRect - Anula o retângulo de corte para desenho. :/}
    procedure ResetClipRect;

    {@method Open - Inicializa a superfície. :/}
    procedure Open;

    {@method Close - Finaliza a superfície e apaga tudo o que foi feito. :/}
    procedure Close;
    
    {@method Clear - Libera todos os objetos e fontes da página e reposiciona a caneta. :/}
    procedure Clear;

    {@method PaintTo - Desenha a superfície em um Canvas com fator de escala definido pelas relações entre o retângulo
     passado e as dimensões da superfície. :/}
    procedure PaintTo(ACanvas: TCanvas; ARect: TRect);

    {@method PageIndex - Retorna o índice da página na lista. :/}
    property PageIndex: Integer read FPageIndex;

    {@prop Opened - Indica se a superfície já foi aberta. :/}
    property Opened: Boolean read FOpened;

    {@prop Modified - Indica se a superfície foi modificada. :/}
    property Modified: Boolean read FModified write FModified;

    {@prop Objects - Vetor de objetos da superfície. :/}
    property Objects[AIndex: Integer]: TRLGraphicObject read GetObjects;

    {@prop ObjectCount - Quantidade de objetos na superfície. :/}
    property ObjectCount: Integer read GetObjectCount;

    {@prop Brush - Padrão utilizado para preenchimentos. :/}
    property Brush: TBrush read FBrush write SetBrush;

    {@prop Pen - Padrão utilizado para linhas. :/}
    property Pen: TPen read FPen write SetPen;

    {@prop Font - Fonte padrão para escrita. :/}
    property Font: TFont read FFont write SetFont;

    {@prop Pixels - Matriz de pontos. :/}
    property Pixels[X, Y: Integer]: TColor read GetPixels write SetPixels;

    {@prop Width - Largura da superfície em pixels. :/}
    property Width: Integer read FWidth write FWidth;

    {@prop Height - Altura da superfície em pixels. :/}
    property Height: Integer read FHeight write FHeight;

    {@prop Orientation - Orientação da superfície. :/}
    property Orientation: TRLMetaOrientation read GetOrientation write SetOrientation;

    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property PaperWidth: Double read GetPaperWidth write SetPaperWidth;

    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property PaperHeight: Double read GetPaperHeight write SetPaperHeight;

    {@prop OrientedPaperWidth - Largura do papel orientado para leitura em milímetros. :/}
    property OrientedPaperWidth: Double read GetOrientedPaperWidth;

    {@prop OrientedPaperHeight - Altura do papel orientado para leitura em milímetros. :/}
    property OrientedPaperHeight: Double read GetOrientedPaperHeight;

    {@prop OrientedWidth - Largura da superfície orientada para leitura em pixels. :/}
    property OrientedWidth: Integer read GetOrientedWidth;

    {@prop OrientedHeight - Altura da superfície orientada para leitura em pixels. :/}
    property OrientedHeight: Integer read GetOrientedHeight;

    {@prop PenPos - Posição atual do cursor. :/}
    property PenPos: TPoint read FPenPos write FPenPos;

    {@prop Margins - Margens de texto para uso com os métodos: Write e WriteLn. :/}
    property Margins: TRect read FMargins write FMargins;

    {@prop ClipRect - Retângulo de corte atual. :/}
    property ClipRect: TRect read FClipRect;

    {@prop Tag - Inteiro associado à superfície.
     Não tem significado para o sistema e pode ser livremente utilizado pelo usuário.
     Nota: Esta prop não é armazenada em disco. :/}
    property Tag: Integer read FTag write FTag;

    {@prop Fonts - Lista de fontes utilizadas. :/}
    property Fonts: TStrings read FFonts;

    {@prop GeneratorId - Identifica o objeto gerador para os próximos elementos gráficos. :/}
    property GeneratorId: PtrInt read FGeneratorId write FGeneratorId;

    {@prop Storage - Referência para o estoque ao qual pertence à superfície gráfica. :/}
    property Storage: TRLGraphicStorage read FStorage;

    {@prop Macros - Lista de símbolos para tradução em tempo de visualização ou impressão. :/}
    property Macros: TStrings read FMacros;
  end;
  {/@class}

  {@class TRLGraphicObject - Objeto primitivo de desenho. }
  TRLGraphicObject = class
  private
    FSurface: TRLGraphicSurface;
    //
    FBoundsRect: TRLMetaRect;
    FGroupId: Integer;
    FGeneratorId: Integer;
    FTag: Integer;
  public
    constructor Create(ASurface: TRLGraphicSurface); virtual;
    destructor Destroy; override;

    {@method SaveToStream - Salva os dados do objeto em uma stream. :/}
    procedure SaveToStream(AStream: TStream); dynamic;

    {@method LoadFromStream - Carrega os dados do objeto de uma stream. :/}
    procedure LoadFromStream(AStream: TStream); dynamic;

    {@method Clone - Instancia um novo objeto com características semelhantes. :/}
    function Clone(ASurface: TRLGraphicSurface): TRLGraphicObject;

    {@method PaintTo - Desenha o objeto em um canvas com os fatores de escala passados. :/}
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); dynamic; abstract;

    {@method Assign - Assume as características de um outro objeto. :/}
    procedure Assign(AObject: TRLGraphicObject); dynamic;

    {@method TransformRect - Transform ASourceRect rectangle for PaintTo procedure.
     HigherRightBottom - When True, Right and Bottom coordinates of the output rectagle are checked,
                         so they are at least one pixel larger than Left and Top. :/}
    function TransformRect(const ASourceRect: TRLMetaRect; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer; const HigherRightBottom: Boolean): TRect;

    {@method TransformBounds - Transform FBoundsRect rectangle for PaintTo procedure using TransformRect. :/}
    function TransformBounds(AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer; const HigherRightBottom: Boolean): TRect;

    {@method Offset - Desloca as coordenadas do objeto. :/}
    procedure Offset(AXDesloc, AYDesloc: Integer); dynamic;

    {@method Inflate - Redimensiona o controle de acordo com os fatores passados. :/}
    procedure Inflate(AXFactor, AYFactor: Double); dynamic;

    {@prop BoundsRect - Dimensões do objeto. :/}
    property BoundsRect: TRLMetaRect read FBoundsRect write FBoundsRect;

    {@prop GroupId - Índice de grupo. Os elementos gráficos gerados na mesma operação têm o mesmo GroupId. :/}
    property GroupId: Integer read FGroupId write FGroupId;

    {@prop GeneratorId - Identifica o objeto gerador do elemento gráfico. :/}
    property GeneratorId: Integer read FGeneratorId write FGeneratorId;

    {@prop Tag - Inteiro associado ao objeto.
     Não tem significado para o sistema e pode ser livremente utilizado pelo usuário.
     Nota: Esta prop não é armazenada em disco. :/}
    property Tag: Integer read FTag write FTag;

    {@prop Surface - Referência para a superfície gráfica à qual pertence o objeto. :/}
    property Surface: TRLGraphicSurface read FSurface;
  end;
  {/@class}

  { TRLPixelObject }

  TRLPixelObject = class(TRLGraphicObject)
  private
    FColor: TRLMetaColor;
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    //
    property Color: TRLMetaColor read FColor write FColor;
  end;

  { TRLLineObject }

  TRLLineObject = class(TRLGraphicObject)
  private
    FFromPoint: TRLMetaPoint;
    FToPoint: TRLMetaPoint;
    FPen: TRLMetaPen;
    FBrush: TRLMetaBrush;
    //
    procedure SetPen(Value: TRLMetaPen);
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Offset(AXDesloc, AYDesloc: Integer); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //
    property FromPoint: TRLMetaPoint read FFromPoint write FFromPoint;
    property ToPoint: TRLMetaPoint read FToPoint write FToPoint;
    property Pen: TRLMetaPen read FPen write SetPen;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
  end;

  { TRLRectangleObject }

  TRLRectangleObject = class(TRLGraphicObject)
  private
    FPen: TRLMetaPen;
    FBrush: TRLMetaBrush;
    //
    procedure SetPen(Value: TRLMetaPen);
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //
    property Pen: TRLMetaPen read FPen write SetPen;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
  end;

  { TRLTextObject }

  TRLTextObject = class(TRLGraphicObject)
  private
    FBrush: TRLMetaBrush;
    FFont: TRLMetaFont;
    FText: AnsiString;
    FOrigin: TRLMetaPoint;
    FAlignment: TRLMetaTextAlignment;
    FLayout: TRLMetaTextLayout;
    FTextFlags: TRLMetaTextFlags;
    //
    procedure TranslateMacros(var AText: AnsiString);
    //
    procedure SetBrush(Value: TRLMetaBrush);
    procedure SetFont(Value: TRLMetaFont);
    function GetDisplayText: AnsiString;
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Offset(AXDesloc, AYDesloc: Integer); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //                  
    property Alignment: TRLMetaTextAlignment read FAlignment write FAlignment;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
    property Font: TRLMetaFont read FFont write SetFont;
    property Layout: TRLMetaTextLayout read FLayout write FLayout;
    property Origin: TRLMetaPoint read FOrigin write FOrigin;
    property Text: AnsiString read FText write FText;
    property TextFlags: TRLMetaTextFlags read FTextFlags write FTextFlags;
    //
    property DisplayText: AnsiString read GetDisplayText;
  end;

  { TRLFillRectObject }

  TRLFillRectObject = class(TRLGraphicObject)
  private
    FBrush: TRLMetaBrush;
    //
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    //
    property Brush: TRLMetaBrush read FBrush write SetBrush;
  end;

  { TRLEllipseObject }

  TRLEllipseObject = class(TRLGraphicObject)
  private
    FPen: TRLMetaPen;
    FBrush: TRLMetaBrush;
    //
    procedure SetPen(Value: TRLMetaPen);
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //
    property Pen: TRLMetaPen read FPen write SetPen;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
  end;

  { TRLArcObject }

  TRLArcObject = class(TRLGraphicObject)
  private
    FArcRect: TRLMetaRect;
    FPen: TRLMetaPen;
    FBrush: TRLMetaBrush;
    //
    procedure SetPen(Value: TRLMetaPen);
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Inflate(AXFactor, AYFactor: double); override;
    //
    property Pen: TRLMetaPen read FPen write SetPen;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
    {@prop ArcRect - Dimensions of the Arc. :/}
    property ArcRect: TRLMetaRect read FArcRect write FArcRect;
   end;

  { TRLPolygonObject }

  TRLPolygonObject = class(TRLGraphicObject)
  private
    FPen: TRLMetaPen;
    FBrush: TRLMetaBrush;
    FPoints: TRLMetaPointArray;
    //
    procedure SetPen(Value: TRLMetaPen);
    procedure SetBrush(Value: TRLMetaBrush);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Offset(AXDesloc, AYDesloc: Integer); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //
    property Pen: TRLMetaPen read FPen write SetPen;
    property Brush: TRLMetaBrush read FBrush write SetBrush;
    property Points: TRLMetaPointArray read FPoints write FPoints;
  end;

  { TRLPolylineObject }

  TRLPolylineObject = class(TRLGraphicObject)
  private
    FPen: TRLMetaPen;
    FPoints: TRLMetaPointArray;
    //
    procedure SetPen(Value: TRLMetaPen);
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    procedure Offset(AXDesloc, AYDesloc: Integer); override;
    procedure Inflate(AXFactor, AYFactor: Double); override;
    //
    property Pen: TRLMetaPen read FPen write SetPen;
    property Points: TRLMetaPointArray read FPoints write FPoints;
  end;

  { TRLImageObject }

  TRLImageObject = class(TRLGraphicObject)
  private
    FData: AnsiString;
    FParity: Boolean;
    //
  public
    constructor Create(ASurface: TRLGraphicSurface); override;
    destructor Destroy; override;
    //
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    //
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
    procedure Assign(AObject: TRLGraphicObject); override;
    //
    property Data: AnsiString read FData write FData;
    property Parity: Boolean read FParity write FParity;
  end;

  { TRLSetClipRectObject }

  TRLSetClipRectObject = class(TRLGraphicObject)
  public
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
  end;

  { TRLResetClipRectObject }

  TRLResetClipRectObject = class(TRLGraphicObject)
  public
    procedure PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer); override;
  end;

function GetPointsBounds(const APoints: TRLMetaPointArray): TRect;
function FloatToPtStr(F: Double): AnsiString;
function PtStrToFloat(const S: String; def: Double = 0): Double;
function ClipGraphic(AGraphic: TGraphic; var ARect: TRect; const ACenter: Boolean): TBitmap;
function ClipSurface(ASurface: TRLGraphicSurface; var ARect: TRect; const ACenter: Boolean): TRLGraphicSurface;

function MetaPoint(X, Y: Integer): TRLMetaPoint;
function MetaRect(ALeft, ATop, ARight, ABottom: Integer): TRLMetaRect;
function MetaColor(ARed, AGreen, ABlue: Byte): TRLMetaColor;

{@function NewGroupId - Cria um identificador para um novo grupo de elementos gráficos.
 @links TRLGraphicObject.GroupId, TRLGraphicSurface.GeneratorId, TRLGraphicObject.GeneratorId. :/}
function NewGroupId: Integer;

{/@unit}

implementation

uses
  {$IfDef CLX} RLMetaCLX {$Else} RLMetaVCL {$EndIf};

{ UTILS }

var
  CurrentGroupId: Integer = 0;

function NewGroupId: Integer;
begin
  Inc(CurrentGroupId);
  Result := CurrentGroupId;
end;

// retorna dimensões de uma coleção de pontos
function GetPointsBounds(const APoints: TRLMetaPointArray): TRect;
var
  I: Integer;
  P: TRLMetaPoint;
begin
  for I := 0 to High(APoints) do
  begin
    P := APoints[I];
    if I = 0 then
    begin
      Result.Left := P.X;
      Result.Top := P.Y;
      Result.Right := P.X;
      Result.Bottom := P.Y;
    end
    else
    begin
      Result.Left := Min(P.X, Result.Left);
      Result.Top := Min(P.Y, Result.Top);
      Result.Right := Max(P.X, Result.Right);
      Result.Bottom := Max(P.Y, Result.Bottom);
    end;
  end;
  Dec(Result.Left);
  Dec(Result.Top);
  Inc(Result.Right);
  Inc(Result.Bottom);
end;

// de float para string com ponto como separador decimal
function FloatToPtStr(F: Double): AnsiString;
begin
  Str(F: 0: 4, Result);
end;

// de string com ponto como separador decimal para float 
function PtStrToFloat(const S: String; def: Double = 0): Double;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then
    Result := def;
end;

// retorna um bitmap a partir de um pedaço recortado do gráfico aGraphic que caiba em aRect
function ClipGraphic(AGraphic: TGraphic; var ARect: TRect; const ACenter: Boolean): TBitmap;
var
  graphicrect: TRect;
begin
  // cria um retângulo com o tamanho natural do gráfico na posição de corte 
  graphicrect := Rect(ARect.Left, ARect.Top, ARect.Left + AGraphic.Width, ARect.Top + AGraphic.Height);
  // centraliza os dois retângulos
  if ACenter then
    OffsetRect(graphicrect, ((ARect.Right - ARect.Left) - (graphicrect.Right - graphicrect.Left)) div 2, 
                           ((ARect.Bottom - ARect.Top) - (graphicrect.Bottom - graphicrect.Top)) div 2);
  // faz a interseção dos dois retângulos em aRect
  if IntersectRect(ARect, ARect, graphicrect) then
  begin
    // projeta um bitmap do tamanho de aRect e de qualidade compatível com aGraphic
    Result := TRLBitmap.Create;
    Result.Width := ARect.Right - ARect.Left;
    Result.Height := ARect.Bottom - ARect.Top;
    Result.PixelFormat := pf32bit;
    // transfere imagem para o novo bitmap
    Result.Canvas.Draw(graphicrect.Left - ARect.Left, graphicrect.Top - ARect.Top, AGraphic);
  end
  // se não houver interseção...
  else
    Result := nil;
end;

// retorna um bitmap a partir de um pedaço recortado do gráfico aGraphic que caiba em aRect
function ClipSurface(ASurface: TRLGraphicSurface; var ARect: TRect; const ACenter: Boolean): TRLGraphicSurface;
var
  graphicrect: TRect;
begin
  // cria um retângulo com o tamanho natural do gráfico na posição de corte
  graphicrect := Rect(ARect.Left, ARect.Top, ARect.Left + ASurface.Width, ARect.Top + ASurface.Height);
  // centraliza os dois retângulos
  if ACenter then
    OffsetRect(graphicrect, ((ARect.Right - ARect.Left) - (graphicrect.Right - graphicrect.Left)) div 2, 
                           ((ARect.Bottom - ARect.Top) - (graphicrect.Bottom - graphicrect.Top)) div 2);
  // faz a interseção dos dois retângulos em aRect
  if IntersectRect(ARect, ARect, graphicrect) then
  begin
    // projeta um bitmap do tamanho de aRect e de qualidade compatível com aGraphic
    Result := TRLGraphicSurface.Create;
    Result.Width := ARect.Right - ARect.Left;
    Result.Height := ARect.Bottom - ARect.Top;
    // transfere imagem para o novo bitmap
    Result.Draw(graphicrect.Left - ARect.Left, graphicrect.Top - ARect.Top, ASurface);
  end
  // se não houver interseção...
  else
    Result := nil;
end;

function MetaPoint(X, Y: Integer): TRLMetaPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MetaRect(ALeft, ATop, ARight, ABottom: Integer): TRLMetaRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function MetaColor(ARed, AGreen, ABlue: Byte): TRLMetaColor;
begin
  Result.Red := ARed;
  Result.Green := AGreen;
  Result.Blue := ABlue;
end;

{ Compatibility }

const
  // TGraphicKind
  gkPixel = 0;
  gkLine = 1;
  gkRectangle = 2;
  gkTextOut = 3;
  gkTextRect = 4;
  gkFillRect = 5;
  gkStretchDraw = 6;
  gkDraw = 7;
  gkEllipse = 8;
  gkPolygon = 9;
  gkPolyline = 10;
  gkCutBegin = 11;
  gkCutEnd = 12;
  gkArc = 13;

  // TImageKind
  ikBitmap = 0;
  ikJPeg = 1;
  ikIcon = 2;
  ikMetafile = 3;
type
  TGraphicKind = Byte;
  TImageKind = Byte;
  TTextAlignmentType = Byte;
  TPenRecord = record
    Color: TColor;
    Mode: TPenMode;
    Style: TPenStyle;
    Width: Integer;
  end;
  TBrushRecord = record
    Color: TColor;
    Style: TBrushStyle;
  end;
  TFontRecord = record
    Color: TColor;
    Height: Integer;
    Pitch: TFontPitch;
    PixelsPerInch: Integer;
    Size: Integer;
    Style: TFontStyles;
    Charset: TFontCharset;
    Angle: Double;
    NameId: Integer;
  end;
  TGraphicFileRecord = record
    X1, Y1, X2, Y2: Integer;
    X, Y: Integer;
    Tag: Integer;
    Kind: TGraphicKind;
    Color: TColor;
    HasPen: Boolean;
    Pen: TPenRecord;
    HasBrush: Boolean;
    Brush: TBrushRecord;
    HasFont: Boolean;
    Font: TFontRecord;
    Text: Integer;
    Alignment: TTextAlignmentType;
    AutoSize: Boolean;
  end;
  
  TPointArray = array of TPoint;

procedure UpgradePage(AStorage: TRLGraphicStorage; AInput, AOutput: TStream);
var
  surface: TRLGraphicSurface;
  texts: TStringList;
  count: Integer;
  len: Integer;
  I: Integer;
  S: String;
  rec: TGraphicFileRecord;
  pgraph: TGraphic;
  cutlist: array of TRect;
  cutrect: TRect;
  cutlen: Integer;
  cutsize: Integer;
function StrToGraphic(const AStr: String; AImageKind: TImageKind): TGraphic;
var
  S: TStringStream;
begin
  S := TStringStream.Create(AStr);
  try
    case AImageKind of
      ikBitmap: Result := TRLBitmap.Create;
      ikIcon: Result := TIcon.Create;
    else
      Result := nil;
    end;
    if Assigned(Result) then
    begin
      S.Position := 0;
      Result.LoadFromStream(S);
    end;
  finally
    S.free;
  end;
end;
function StrToPoints(const AStr: AnsiString): TPointArray;
var
  Q, I: Integer;
begin
  Q := Length(AStr) div SizeOf(TPoint);
  SetLength(Result, Q);
  for I := 0 to Q - 1 do
    Move(AStr[I * SizeOf(TPoint) + 1], Result[I], SizeOf(TPoint));
end;
begin
  cutlen := 0;
  cutsize := 0;
  //
  surface := TRLGraphicSurface.Create;
  try
    AInput.Read(surface.FWidth, SizeOf(surface.FWidth));
    AInput.Read(surface.FHeight, SizeOf(surface.FHeight));
    surface.Orientation := AStorage.Orientation;
    surface.PaperHeight := AStorage.PaperHeight;
    surface.PaperWidth := AStorage.PaperWidth;
    //
    cutrect := Rect(0, 0, surface.FWidth, surface.FHeight);
    // strings
    texts := TStringList.Create;
    try
      AInput.Read(count, SizeOf(count));
      for I := 0 to count - 1 do
      begin
        AInput.Read(len, SizeOf(len));
        SetLength(S, len);
        AInput.Read(S[1], len);
        texts.Add(S);
      end;
      // objects
      AInput.Read(count, SizeOf(count));
      for I := 1 to count do
      begin
        AInput.Read(rec, SizeOf(rec));

        case rec.Kind of
          gkPixel: with TRLPixelObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Color := ToMetaColor(rec.Color);
                         end;
          gkLine: with TRLLineObject.Create(surface) do
                         begin
                           FromPoint := ToMetaPoint(Point(rec.X1, rec.Y1));
                           ToPoint := ToMetaPoint(Point(rec.X2, rec.Y2));
                           BoundsRect := ToMetaRect(Rect(Min(FromPoint.X, ToPoint.X), 
                                                     Min(FromPoint.Y, ToPoint.Y), 
                                                     Max(FromPoint.X, ToPoint.X), 
                                                     Max(FromPoint.Y, ToPoint.Y)));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                         end;
          gkRectangle: with TRLRectangleObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                           Brush.Color := ToMetaColor(rec.Brush.Color);
                           Brush.Style := ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkTextOut, 
          gkTextRect: with TRLTextObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Text := AnsiString(texts[rec.Text]);
                           Origin := ToMetaPoint(Point(rec.X, rec.Y));
                           Alignment := rec.Alignment + 1;
                           Layout := MetaTextLayoutTop;
                           if rec.AutoSize or (rec.Kind = gkTextOut) then
                             TextFlags := TextFlags or MetaTextFlagAutoSize;
                           Brush.Color := ToMetaColor(rec.Brush.Color);
                           Brush.Style := ToMetaBrushStyle(rec.Brush.Style);
                           Font.PixelsPerInch := ScreenPPI; //rec.Font.PixelsPerInch;
                           Font.Charset := ToMetaFontCharset(rec.Font.Charset);
                           Font.Color := ToMetaColor(rec.Font.Color);
                           Font.Height := rec.Font.Height;
                           Font.Name := texts[rec.Font.NameId];
                           Font.Pitch := ToMetaFontPitch(rec.Font.Pitch);
                           Font.Size := -Round(Font.Height * 72 / Font.PixelsPerInch);
                           Font.Style := ToMetaFontStyles(rec.Font.Style);
                         end;
          gkFillRect: with TRLFillRectObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Brush.Color := ToMetaColor(rec.Brush.Color);
                           Brush.Style := ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkStretchDraw: with TRLImageObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Parity := False;
                           pgraph := StrToGraphic(texts[rec.Text], rec.Tag);
                           try
                             Data := AnsiString(ToMetaGraphic(pgraph));
                           finally
                             pgraph.free;
                           end;
                         end;
          gkDraw: with TRLImageObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Parity := False;
                           pgraph := StrToGraphic(texts[rec.Text], rec.Tag);
                           try
                             Data := AnsiString(ToMetaGraphic(pgraph));
                           finally
                             pgraph.free;
                           end;
                         end;
          gkEllipse: with TRLEllipseObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                           Brush.Color := ToMetaColor(rec.Brush.Color);
                           Brush.Style := ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkArc: with TRLArcObject.Create(surface) do
                         begin
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                           AInput.Read(rec, SizeOf(rec));
                           ArcRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                         end;
          gkPolygon: with TRLPolygonObject.Create(surface) do
                         begin
                           Points := ToMetaPointArray(StrToPoints(AnsiString(texts[rec.Text])));
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                           Brush.Color := ToMetaColor(rec.Brush.Color);
                           Brush.Style := ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkPolyline: with TRLPolylineObject.Create(surface) do
                         begin
                           Points := ToMetaPointArray(StrToPoints(AnsiString(texts[rec.Text])));
                           BoundsRect := ToMetaRect(Rect(rec.X1, rec.Y1, rec.X2, rec.Y2));
                           Pen.Color := ToMetaColor(rec.Pen.Color);
                           Pen.Mode := ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style := ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width := rec.Pen.Width;
                         end;
          gkCutBegin: with TRLSetClipRectObject.Create(surface) do
                         begin
                           Inc(cutlen);
                           if cutlen > cutsize then
                           begin
                             Inc(cutsize, 1024);
                             SetLength(cutlist, cutsize);
                           end;
                           cutlist[cutlen - 1] := cutrect;
                           cutrect := Rect(rec.X1, rec.Y1, rec.X2, rec.Y2);
                           BoundsRect := ToMetaRect(cutrect);
                         end;
          gkCutEnd: with TRLResetClipRectObject.Create(surface) do
                         begin
                           cutrect := cutlist[cutlen - 1];
                           Dec(cutlen);
                           BoundsRect := ToMetaRect(cutrect);
                         end;
        end;
      end;
    finally
      texts.free;
    end;
    //
    surface.SaveToStream(AOutput);
  finally
    surface.free;
  end;
end;

procedure DowngradePage(AInput, AOutput: TStream);
var
  surface: TRLGraphicSurface;
  pen: TRLMetaPen;
  brush: TRLMetaBrush;
  font: ^TRLMetaFont;
  rec: ^TGraphicFileRecord;
  textid: Integer;
  fontid: Integer;
  obj: TRLGraphicObject;
  texts: TStringList;
  objcs: TList;
  count: Integer;
  len: Integer;
  S: AnsiString;
  I: Integer;
begin
  surface := TRLGraphicSurface.Create;
  try
    surface.LoadFromStream(AInput);
    //
    texts := TStringList.Create;
    objcs := TList.Create;
    try
      for I := 0 to surface.ObjectCount - 1 do
      begin
        obj := surface.Objects[I];
        textid := -1;
        fontid := -1;

        new(rec);

        if obj is TRLTextObject then
        begin
          S := TRLTextObject(obj).Text;
          textid := texts.Add(S);
          S := AnsiString(TRLTextObject(obj).Font.GetName);
          fontid := texts.indexof(S);
          if fontid = -1 then
            fontid := texts.Add(S);
        end
        else if obj is TRLImageObject then
        begin
          S := TRLImageObject(obj).Data;
          Delete(S, 1, 3); // retira prefixo
          textid := texts.Add(S);
        end;

        if obj is TRLPixelObject then
          rec^.Kind := gkPixel
        else if obj is TRLLineObject then
          rec^.Kind := gkLine
        else if obj is TRLRectangleObject then
          rec^.Kind := gkRectangle
        else if obj is TRLTextObject then
          if (TRLTextObject(obj).TextFlags and MetaTextFlagAutoSize) = MetaTextFlagAutoSize then
            rec^.Kind := gkTextOut
          else
            rec^.Kind := gkTextRect
        else if obj is TRLFillRectObject then
          rec^.Kind := gkFillRect
        else if obj is TRLImageObject then
          rec^.Kind := gkStretchDraw
        else if obj is TRLEllipseObject then
          rec^.Kind := gkEllipse
        else if obj is TRLArcObject then
          rec^.Kind := gkArc
        else if obj is TRLPolygonObject then
          rec^.Kind := gkPolygon
        else if obj is TRLPolylineObject then
          rec^.Kind := gkPolyline
        else if obj is TRLSetClipRectObject then
          rec^.Kind := gkCutBegin
        else if obj is TRLResetClipRectObject then
          rec^.Kind := gkCutEnd;

        if obj is TRLLineObject then
        begin
          rec^.X1 := TRLLineObject(obj).FromPoint.X;
          rec^.Y1 := TRLLineObject(obj).FromPoint.Y;
          rec^.X2 := TRLLineObject(obj).ToPoint.X;
          rec^.Y2 := TRLLineObject(obj).ToPoint.Y;
        end
        else
        begin
          rec^.X1 := TRLLineObject(obj).BoundsRect.Left;
          rec^.Y1 := TRLLineObject(obj).BoundsRect.Top;
          rec^.X2 := TRLLineObject(obj).BoundsRect.Right;
          rec^.Y2 := TRLLineObject(obj).BoundsRect.Bottom;
        end;

        if obj is TRLTextObject then
        begin
          rec^.X := TRLTextObject(obj).Origin.X;
          rec^.Y := TRLTextObject(obj).Origin.Y;
        end
        else
        begin
          rec^.X := 0;
          rec^.Y := 0;
        end;

        if obj is TRLImageObject then
        begin
          S := TRLImageObject(obj).Data;
          if Copy(S, 1, 3) = 'BMP' then
            rec^.Tag := ord(ikBitmap)
          else if Copy(S, 1, 3) = 'ICO' then
            rec^.Tag := ord(ikIcon);
        end;

        rec^.Color := 0; // not used
        if obj is TRLTextObject then
        begin
          rec^.Alignment := TRLTextObject(obj).Alignment - 1;
          rec^.AutoSize := ((TRLTextObject(obj).TextFlags and MetaTextFlagAutoSize) = MetaTextFlagAutoSize);
        end;

        if obj is TRLLineObject then
          pen := TRLLineObject(obj).Pen
        else if obj is TRLRectangleObject then
          pen := TRLRectangleObject(obj).Pen
        else if obj is TRLEllipseObject then
          pen := TRLEllipseObject(obj).Pen
        else if obj is TRLArcObject then
          pen := TRLArcObject(obj).Pen
        else if obj is TRLPolygonObject then
          pen := TRLPolygonObject(obj).Pen
        else if obj is TRLPolylineObject then
          pen := TRLPolylineObject(obj).Pen
        else
          pen := nil;

        rec^.HasPen := (pen <> nil);
        if rec^.HasPen then
        begin
          rec^.Pen.Color := FromMetaColor(pen.Color);
          rec^.Pen.Mode := FromMetaPenMode(pen.Mode);
          rec^.Pen.Style := FromMetaPenStyle(pen.Style);
          rec^.Pen.Width := pen.Width;
        end;

        if obj is TRLRectangleObject then
          brush := TRLRectangleObject(obj).Brush
        else if obj is TRLTextObject then
          brush := TRLTextObject(obj).Brush
        else if obj is TRLFillRectObject then
          brush := TRLFillRectObject(obj).Brush
        else if obj is TRLEllipseObject then
          brush := TRLEllipseObject(obj).Brush
        else if obj is TRLArcObject then
          brush := TRLArcObject(obj).Brush
        else if obj is TRLPolygonObject then
          brush := TRLPolygonObject(obj).Brush
        else
          brush := nil;

        rec^.HasBrush := (brush <> nil);
        if rec^.HasBrush then
        begin
          rec^.Brush.Color := FromMetaColor(brush.Color);
          rec^.Brush.Style := FromMetaBrushStyle(brush.Style);
        end;

        if obj is TRLTextObject then
          font := @TRLTextObject(obj).Font
        else
          font := nil;
        rec^.HasFont := (font <> nil);
        if rec^.HasFont then
        begin
          rec^.Font.NameId := fontid;
          rec^.Font.Charset := FromMetaFontCharset(font^.Charset);
          rec^.Font.Pitch := FromMetaFontPitch(font^.Pitch);
          rec^.Font.Height := font^.Height;
          rec^.Font.Style := FromMetaFontStyles(font^.Style);
          rec^.Font.Color := FromMetaColor(font^.Color);
        end;

        rec^.Text := textid;

        objcs.Add(rec);

      if obj is TRLArcObject then
         begin
          new(rec);
          rec^.Kind := gkArc;
          with rec^,TRLArcObject(obj) do
            begin
              X1 := ArcRect.Left;
              Y1 := ArcRect.Top;
              X2 := ArcRect.Right;
              Y2 := ArcRect.Bottom;
            end;
          objcs.Add(rec);
         end;
      end;
      //
      AOutput.Write(surface.FWidth, SizeOf(surface.FWidth));
      AOutput.Write(surface.FHeight, SizeOf(surface.FHeight));
      count := texts.Count;
      AOutput.Write(count, SizeOf(count));
      for I := 0 to count - 1 do
      begin
        S := AnsiString(texts[I]);
        len := Length(S);
        AOutput.Write(len, SizeOf(len));
        AOutput.Write(S[1], len);
      end;
      count := objcs.Count;
      AOutput.Write(count, SizeOf(count));
      for I := 0 to count - 1 do
      begin
        rec := objcs[I];
        AOutput.Write(rec^, SizeOf(TGraphicFileRecord));
        dispose(rec);
      end;
    finally
      texts.free;
      objcs.free;
    end;
  finally
    surface.free;
  end;
end;

{ TRLPageAllocation }

constructor TRLPageAllocation.Create;
begin
  SetLength(FOffsets, 0);
  FCount := 0;
end;

function TRLPageAllocation.Add(Offset: Int64): Integer;
const
  Incr = 16 * 1024;
begin
  Result := FCount;
  Inc(FCount);
  if (FCount > Length(FOffsets)) then
    SetLength(FOffsets, FCount * 2 + Incr);
  FOffsets[FCount - 1] := Offset;
end;

procedure TRLPageAllocation.Clear;
begin
  SetLength(FOffsets, 0);
  FCount := 0;
end;

function TRLPageAllocation.GetItems(I: Integer): Int64;
begin
  Result := FOffsets[I];
end;

procedure TRLPageAllocation.SetItems(I: Integer; Value: Int64);
begin
  FOffsets[I] := Value;
end;

{ TRLGraphicStorage }

constructor TRLGraphicStorage.Create(AOwner: TComponent = nil);
begin
  FPageCache := nil;
  FPageAllocation := nil;
  FTempStream := nil;
  FTempFileName := '';
  FFileVersion := 3;
  FMacros := nil;
  FReferenceList := nil;
  //
  FPageCache := TObjectList.Create;
  FPageAllocation := TRLPageAllocation.Create;
  FMacros := TStringList.Create;
  FReferenceList := TList.Create;
  //
  inherited Create(nil);
  //
  if Assigned(AOwner) then
  begin
    Link(AOwner);
    AOwner.FreeNotification(Self);
  end;
end;

destructor TRLGraphicStorage.Destroy;
begin
  inherited;
  //
  if Assigned(FReferenceList) then
    FReferenceList.free;
  if Assigned(FPageCache) then
    FPageCache.free;
  if Assigned(FPageAllocation) then
    FPageAllocation.free;
  if Assigned(FMacros) then
    FMacros.free;
  if Assigned(FTempStream) then
  begin
    FTempStream.free;
    SysUtils.DeleteFile(FTempFileName);
    UnregisterTempFile(FTempFileName);
  end;
end;

procedure TRLGraphicStorage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  //
  if Operation = opRemove then
    Unlink(AComponent);
end;

procedure TRLGraphicStorage.Link(AComponent: TComponent);
begin
  if FReferenceList.IndexOf(AComponent) = -1 then
  begin
    FReferenceList.Add(AComponent);
    AComponent.FreeNotification(Self);
  end;
end;

procedure TRLGraphicStorage.Unlink(AComponent: TComponent = nil);
var
  I: Integer;
begin
  if Assigned(FReferenceList) then
  begin
    I := FReferenceList.IndexOf(AComponent);
    if I <> -1 then
      FReferenceList.Delete(I);
    if FReferenceList.Count = 0 then
      Free;
  end
  else
    Free;
end;

procedure TRLGraphicStorage.StorePage(ASurface: TRLGraphicSurface);
var
  datapos, beginpos, endpos: Int64;
  size: Integer;
begin
  TempStreamNeeded;
  FTempStream.Position := FTempStream.Size;
  // guarda a posição de gravação e reserva espaço para o tamanho
  beginpos := FTempStream.Position;
  size := 0;
  FTempStream.Write(size, SizeOf(size));
  datapos := FTempStream.Position;
  // atualiza a lista de páginas atribuindo o novo offset
  if ASurface.FPageIndex = -1 then
    ASurface.FPageIndex := FPageAllocation.Add(beginpos)
  else
    FPageAllocation[ASurface.FPageIndex] := beginpos;
  // salva a página em disco
  ASurface.SaveToStream(FTempStream);
  // atualiza o tamanho no início da gravação e retorna o cursor para o fim do arquivo
  endpos := FTempStream.Position;
  size := Integer(endpos - datapos);
  FTempStream.Position := beginpos;
  FTempStream.Write(size, SizeOf(size));
  FTempStream.Position := endpos;
end;

procedure TRLGraphicStorage.RetrievePage(ASurface: TRLGraphicSurface);
var
  size: Integer;
begin
  FTempStream.Position := FPageAllocation[ASurface.FPageIndex];
  FTempStream.Read(size, SizeOf(size));
  ASurface.LoadFromStream(FTempStream);
end;

procedure TRLGraphicStorage.Add(ASurface: TRLGraphicSurface);
begin
  ASurface.SetStorage(Self);
  StorePage(ASurface);
  AddToCache(ASurface);
end;

procedure TRLGraphicStorage.Update(ASurface: TRLGraphicSurface);
begin
  StorePage(ASurface);
  ASurface.Modified := False;
end;

procedure TRLGraphicStorage.Clear;
begin
  if Assigned(FTempStream) then
    FTempStream.Size := 0;
  FPageAllocation.Clear;
  FPageCache.Clear;
  FMacros.Clear;
end;

procedure TRLGraphicStorage.AddToCache(ASurface: TRLGraphicSurface);
var
  S: TRLGraphicSurface;
begin
  // limite de dez páginas em cachê
  if FPageCache.Count >= MAXPAGECACHE then
  begin
    S := TRLGraphicSurface(FPageCache[0]);
    if S.Modified then
      Update(S);
    FPageCache.Remove(S);
  end;
  FPageCache.Add(ASurface);
end;

function TRLGraphicStorage.GetFromCache(APageIndex: Integer): TRLGraphicSurface;
var
  I: Integer;
begin
  Result := nil;
  if (APageIndex >= 0) and Assigned(FPageCache) then
  begin
    I := 0;
    while (I < FPageCache.Count) and (TRLGraphicSurface(FPageCache[I]).PageIndex <> APageIndex) do
      Inc(I);
    if I < FPageCache.Count then
      Result := TRLGraphicSurface(FPageCache[I]);
  end;
end;

procedure TRLGraphicStorage.FlushCache;
var
  S: TRLGraphicSurface;
  I: Integer;
begin
  for I := 0 to FPageCache.Count - 1 do
  begin
    S := TRLGraphicSurface(FPageCache[I]);
    if S.Modified then
      Update(S);
  end;
end;

function TRLGraphicStorage.LoadPageFromDisk(APageIndex: Integer): TRLGraphicSurface;
begin
  if (APageIndex >= 0) and (APageIndex < FPageAllocation.Count) then
  begin
    Result := TRLGraphicSurface.Create;
    try
      Result.SetStorage(Self); 
      Result.FPageIndex := APageIndex;
      RetrievePage(Result);
    except
      Result.free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TRLGraphicStorage.GetPages(APageIndex: Integer): TRLGraphicSurface;
begin
  Result := GetFromCache(APageIndex);
  if Result = nil then
  begin
    Result := LoadPageFromDisk(APageIndex);
    if Result <> nil then
      AddToCache(Result);
  end;
end;

function TRLGraphicStorage.GetPageCount: Integer;
begin
  Result := FPageAllocation.Count;
end;

procedure TRLGraphicStorage.TempStreamNeeded;
begin
  if not Assigned(FTempStream) then
  begin
    FTempFileName := GetTempFileName;
    RegisterTempFile(FTempFileName);
    FTempStream := TFileStream.Create(FTempFileName, fmCreate);
  end;
end;

procedure TRLGraphicStorage.SaveToFile(const AFileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.free;
  end;
end;

procedure TRLGraphicStorage.LoadFromFile(const AFileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;

const
  MaxFileHeader = 20;
  FileHeaderVersion1: AnsiString = 'Fortes Metafile'#26;
  FileHeaderVersion2: AnsiString = 'RPF2'#26;
  FileHeaderVersion3: AnsiString = 'RLGraphicStorage3'#26;
  FileHeaderVersion4: AnsiString = 'RLGraphicStorage4'#26;

procedure TRLGraphicStorage.SaveToStream(AStream: TStream);
  function SaveHeaderToStream(AStream: TStream): Int64;
  var
    data: AnsiString;
  begin
    case FFileVersion of
      1: data := FileHeaderVersion1;
      2: data := FileHeaderVersion2;
      3: data := FileHeaderVersion3;
      4: data := FileHeaderVersion4;
    else
      raise Exception.Create(Format(GetLocalizeStr(LocaleStrings.LS_FileVersion), [FFileVersion]));
    end;
    Result := AStream.Position;
    AStream.Write(data[1], Length(data));
  end;
  procedure SaveMacrosToStream(AStream: TStream);
  var
    count, len, I, P: Integer;
    ln, name, value: AnsiString;
  begin
    // grava a quantidade de macros
    count := FMacros.Count;
    AStream.Write(count, SizeOf(count));
    // grava símbolos
    for I := 0 to count - 1 do
    begin
      ln := AnsiString(FMacros[I]);
      // downgrade
      P := Pos('=', ln);
      if P <> 0 then
      begin
        name := AnsiString(Trim(Copy(ln, 1, P - 1)));
        value := AnsiString(Trim(Copy(ln, P + 1, Length(ln))));
        if (FFileVersion < 3) and AnsiSameText(name, 'Orientation') then
          ln := name + '=' + AnsiString(IntToStr(StrToIntDef(value, 1) - 1));
      end;
      // grava length + nome
      len := Length(ln);
      AStream.Write(len, SizeOf(len));
      AStream.Write(ln[1], len);
    end;
  end;
  function SavePageToStream(AStream: TStream; APageIndex: Integer): Int64;
  var
    size: Integer;
  begin
    // lê o tamanho da página armazenada
    FTempStream.Position := FPageAllocation[APageIndex];
    FTempStream.Read(size, SizeOf(size));
    // guarda posição inicial de gravação da nova stream
    Result := AStream.Position;
    // versões >2 gravam o tamanho da página
    if FFileVersion > 2 then
      AStream.Write(size, SizeOf(size));
    // grava página no novo stream
    if FFileVersion > 2 then
      AStream.CopyFrom(FTempStream, size)
    else
      DowngradePage(FTempStream, AStream); 
  end;
  procedure SavePagesToStream(AStream: TStream);
  var
    pagetbl, page0, savedpos: Int64;
    count, I: Integer;
    offsets: array of Integer;
    offsets64: array of Int64;
  begin
    // grava a quantidade de páginas
    count := FPageAllocation.Count;
    AStream.Write(count, SizeOf(count));
    // guarda posição inicial de gravação da tabela de páginas
    pagetbl := AStream.Position;

    if FFileVersion >= 4 then
    begin
      // reserva espaço para os offsets
      SetLength(offsets64, count);
      for I := 0 to count - 1 do
        AStream.Write(offsets64[I], SizeOf(offsets64[I]));
      // grava páginas e memoriza os offsets
      for I := 0 to FPageAllocation.Count - 1 do
        offsets64[I] := SavePageToStream(AStream, I);
      // guarda posição atual, grava offsets e restaura posição
      savedpos := AStream.Position;
      AStream.Position := pagetbl;
      for I := 0 to count - 1 do
        AStream.Write(offsets64[I], SizeOf(offsets64[I]));
    end
    else
    begin
      // reserva espaço para os offsets
      SetLength(offsets, count);
      for I := 0 to count - 1 do
        AStream.Write(offsets[I], SizeOf(offsets[I]));
      // grava páginas e memoriza os offsets
      page0 := AStream.Position;
      for I := 0 to FPageAllocation.Count - 1 do
        offsets[I] := SavePageToStream(AStream, I);
      // guarda posição atual, grava offsets e restaura posição
      savedpos := AStream.Position;
      AStream.Position := pagetbl;
      for I := 0 to count - 1 do
      begin
        // nas versões <=2 o offsets da primeira página era 0
        if FFileVersion <= 2 then
          Dec(offsets[I], page0);
        AStream.Write(offsets[I], SizeOf(offsets[I]));
      end;
    end;
    AStream.Position := savedpos;
  end;
begin
  FlushCache;
  SaveHeaderToStream(AStream);
  if FFileVersion >= 2 then
    SaveMacrosToStream(AStream);
  SavePagesToStream(AStream);
end;

procedure TRLGraphicStorage.LoadFromStream(AStream: TStream);
  procedure LoadHeaderFromStream(AStream: TStream);
  var
    data: AnsiString;
    ch: AnsiChar;
    I: Integer;
  begin
    SetLength(data, MaxFileHeader);
    I := 0;
    while (I < MaxFileHeader) and (AStream.Read(ch, 1) = 1) do
    begin
      Inc(I);
      data[I] := ch;
      if ch = #26 then
        Break;
    end;
    SetLength(data, I);
    if data = FileHeaderVersion1 then
      FFileVersion := 1
    else if data = FileHeaderVersion2 then
      FFileVersion := 2
    else if data = FileHeaderVersion3 then
      FFileVersion := 3
    else if data = FileHeaderVersion4 then
      FFileVersion := 4
    else
      raise Exception.Create(Format(GetLocalizeStr(LocaleStrings.LS_FileCorruptedHeader), [data]));
  end;
  procedure LoadMacrosFromStream(AStream: TStream);
  var
    count, len, I, P: Integer;
    ln, name, value: AnsiString;
  begin
    AStream.Read(count, SizeOf(count));
    // grava símbolos e seus valores
    for I := 0 to count - 1 do
    begin
      // lê length + nome
      AStream.Read(len, SizeOf(len));
      SetLength(ln, len);
      AStream.Read(ln[1], len);
      // upgrade
      P := Pos('=', ln);
      if P <> 0 then
      begin
        name := AnsiString(Trim(Copy(ln, 1, P - 1)));
        value := AnsiString(Trim(Copy(ln, P + 1, Length(ln))));
        if (FFileVersion < 3) and AnsiSameText(name, 'Orientation') then
          ln := name + '=' + AnsiString(IntToStr(StrToIntDef(value, 0) + 1));
      end;
      //
      FMacros.Add(ln);
    end;
  end;
  procedure LoadPageTableFromStream(AStream: TStream);
  var
    offset64: Int64; 
    count, offset, page0, I: Integer;
  begin
    AStream.Read(count, SizeOf(count));

    if FFileVersion >= 4 then
    begin
      // lê offsets
      for I := 0 to count - 1 do
      begin
        AStream.Read(offset64, SizeOf(offset64));
        FPageAllocation.Add(offset64);
      end;
    end
    else
    begin
      // lê offsets
      for I := 0 to count - 1 do
      begin
        AStream.Read(offset, SizeOf(offset));
        FPageAllocation.Add(offset);
      end;
    end;

    // nas versões <=2 o offsets da primeira página era 0
    if FFileVersion <= 2 then
    begin
      page0 := AStream.Position;
      for I := 0 to count - 1 do
        FPageAllocation[I] := FPageAllocation[I] + page0;
    end;
  end;
  procedure LoadPageFromStream(AStream: TStream; APageIndex: Integer);
  var
    sizeat, beginat, endat: Int64;
    size: Integer;
  begin
    // lê o tamanho da página armazenada
    AStream.Position := FPageAllocation[APageIndex];
    // versões >2 indicam o tamanho da página
    if FFileVersion > 2 then
      AStream.Read(size, SizeOf(size))
    else
      size := 0;
    // atualiza tabela de páginas
    FPageAllocation[APageIndex] := FTempStream.Position;
    // grava o tamanho
    sizeat := FTempStream.Position;
    FTempStream.Write(size, SizeOf(size));
    // grava página no stream de trabalho
    if FFileVersion > 2 then
      FTempStream.CopyFrom(AStream, size)
    else
    begin
      beginat := FTempStream.Position;
      UpgradePage(Self, AStream, FTempStream);
      endat := FTempStream.Position;
      FTempStream.Position := sizeat;
      size := Integer(endat - beginat);
      FTempStream.Write(size, SizeOf(size));
      FTempStream.Position := endat;
    end;
  end;
  procedure LoadPagesFromStream(AStream: TStream);
  var
    I: Integer;
  begin
    for I := 0 to FPageAllocation.Count - 1 do
      LoadPageFromStream(AStream, I);
  end;
begin
  Clear;
  LoadHeaderFromStream(AStream);
  if FFileVersion >= 2 then
    LoadMacrosFromStream(AStream);
  LoadPageTableFromStream(AStream);
  TempStreamNeeded;
  LoadPagesFromStream(AStream);
end;

procedure TRLGraphicStorage.SetFileVersion(AVersion: Integer);
begin
  if (AVersion < 1) or (AVersion > 4) then
    raise Exception.Create(Format(GetLocalizeStr(LocaleStrings.LS_FileVersion), [AVersion]));
  FFileVersion := AVersion; 
end;

function TRLGraphicStorage.GetFirstPageNumber: Integer;
begin
  Result := StrToIntDef(FMacros.Values['FirstPageNumber'], 0);
end;

procedure TRLGraphicStorage.SetFirstPageNumber(const Value: Integer);
begin
  FMacros.Values['FirstPageNumber'] := IntToStr(Value);
end;

function TRLGraphicStorage.GetLastPageNumber: Integer;
begin
  Result := StrToIntDef(FMacros.Values['LastPageNumber'], 0);
end;

procedure TRLGraphicStorage.SetLastPageNumber(const Value: Integer);
begin
  FMacros.Values['LastPageNumber'] := IntToStr(Value);
end;

function TRLGraphicStorage.GetOrientation: TRLMetaOrientation;
begin
  Result := StrToIntDef(FMacros.Values['Orientation'], MetaOrientationPortrait);
end;

procedure TRLGraphicStorage.SetOrientation(const Value: TRLMetaOrientation);
begin
  FMacros.Values['Orientation'] := IntToStr(Value);
end;

function TRLGraphicStorage.GetPaperHeight: Double;
begin
  Result := PtStrToFloat(FMacros.Values['PaperHeight'], 0);
end;

procedure TRLGraphicStorage.SetPaperHeight(const Value: Double);
begin
  FMacros.Values['PaperHeight'] := FloatToPtStr(Value);
end;

function TRLGraphicStorage.GetPaperWidth: Double;
begin
  Result := PtStrToFloat(FMacros.Values['PaperWidth'], 0);
end;

procedure TRLGraphicStorage.SetPaperWidth(const Value: Double);
begin
  FMacros.Values['PaperWidth'] := FloatToPtStr(Value);
end;

function TRLGraphicStorage.GetTitle: String;
begin
  Result := FMacros.Values['Title'];
end;

procedure TRLGraphicStorage.SetTitle(const Value: String);
begin
  FMacros.Values['Title'] := Value;
end;

function TRLGraphicStorage.GetJobTitle: String;
begin
  Result := FMacros.Values['JobTitle'];
end;

procedure TRLGraphicStorage.SetJobTitle(const Value: String);
begin
  FMacros.Values['JobTitle'] := Value;
end;

function TRLGraphicStorage.GetHeight: Integer;
begin
  Result := Round(PaperHeight * MMAsPixels);
end;

function TRLGraphicStorage.GetWidth: Integer;
begin
  Result := Round(PaperWidth * MMAsPixels);
end;

function TRLGraphicStorage.GetOrientedPaperHeight: Double;
begin
  if Orientation = MetaOrientationPortrait then
    Result := PaperHeight
  else
    Result := PaperWidth;
end;

function TRLGraphicStorage.GetOrientedPaperWidth: Double;
begin
  if Orientation = MetaOrientationPortrait then
    Result := PaperWidth
  else
    Result := PaperHeight;
end;

function TRLGraphicStorage.GetOrientedHeight: Integer;
begin
  Result := Round(OrientedPaperHeight * MMAsPixels);
end;

function TRLGraphicStorage.GetOrientedWidth: Integer;
begin
  Result := Round(OrientedPaperWidth * MMAsPixels);
end;

{ TRLGraphicSurface }

constructor TRLGraphicSurface.Create;
begin
  FStorage := nil;
  FPageIndex := -1;
  FObjects := nil;
  FPenPos := Point(0, 0);
  FWidth := 0;
  FHeight := 0;
  FBrush := nil;
  FFont := nil;
  FPen := nil;
  FMargins := Rect(0, 0, 0, 0);
  FOpened := False;
  FModified := False;
  FFonts := nil;
  FClipStack := nil;
  FGeneratorId := 0;
  FMacros := nil;
  //
  FBrush := TBrush.Create;
  FBrush.Color := clWhite;
  FPen := TPen.Create;
  FPen.Color := clBlack;
  FFont := TFont.Create;
  FFont.Color := clBlack;
  //
  FObjects := TObjectList.Create;
  FFonts := TStringList.Create;
  FClipStack := TList.Create;
  FMacros := TStringList.Create;
  //
  inherited Create;
end;

destructor TRLGraphicSurface.Destroy;
begin
  inherited;
  //
  SetStorage(nil);
  if Assigned(FObjects) then
    FObjects.free;
  if Assigned(FBrush) then
    FBrush.free;
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FFont) then
    FFont.free;
  if Assigned(FFonts) then
    FFonts.free;
  if Assigned(FClipStack) then
    FClipStack.free;
  if Assigned(FMacros) then
    FMacros.free;
end;

procedure TRLGraphicSurface.SaveToFile(const AFileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.free;
  end;
end;

procedure TRLGraphicSurface.LoadFromFile(const AFileName: String);
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;

type
  TGraphicObjectKind = Byte; 

const
  ObjectKindPixel = 1;
  ObjectKindLine = 2;
  ObjectKindRectangle = 3;
  ObjectKindText = 4;
  ObjectKindFillRect = 5;
  ObjectKindEllipse = 6;
  ObjectKindPolygon = 7;
  ObjectKindPolyline = 8;
  ObjectKindImage = 9;
  ObjectKindSetClipRect = 10;
  ObjectKindResetClipRect = 11;
  ObjectKindArc = 12;

function GraphicObjectKind(AGraphicObject: TRLGraphicObject): TGraphicObjectKind;
begin
  if AGraphicObject is TRLPixelObject then
    Result := ObjectKindPixel
  else if AGraphicObject is TRLLineObject then
    Result := ObjectKindLine
  else if AGraphicObject is TRLRectangleObject then
    Result := ObjectKindRectangle
  else if AGraphicObject is TRLTextObject then
    Result := ObjectKindText
  else if AGraphicObject is TRLFillRectObject then
    Result := ObjectKindFillRect
  else if AGraphicObject is TRLEllipseObject then
    Result := ObjectKindEllipse
  else if AGraphicObject is TRLPolygonObject then
    Result := ObjectKindPolygon
  else if AGraphicObject is TRLPolylineObject then
    Result := ObjectKindPolyline
  else if AGraphicObject is TRLImageObject then
    Result := ObjectKindImage
  else if AGraphicObject is TRLSetClipRectObject then
    Result := ObjectKindSetClipRect
  else if AGraphicObject is TRLResetClipRectObject then
    Result := ObjectKindResetClipRect
  else if AGraphicObject is TRLArcObject then
    Result := ObjectKindArc
  else
    Result := 0; 
end;

const
  MaxSurfaceHeader = 20;
  SurfaceHeaderStr = 'RLGraphicSurface3'#26;
  
procedure TRLGraphicSurface.SaveToStream(AStream: TStream);
  procedure SaveHeaderToStream(AStream: TStream);
  var
    data: AnsiString;
  begin
    data := SurfaceHeaderStr;
    AStream.Write(data[1], Length(data));
  end;
  function SaveBoundsToStream(AStream: TStream): Int64;
  begin
    // guarda posição inicial de gravação
    Result := AStream.Position;
    AStream.Write(FWidth, SizeOf(FWidth));
    AStream.Write(FHeight, SizeOf(FHeight));
  end;
  procedure SaveMacrosToStream(AStream: TStream);
  var
    count, len, I: Integer;
    ln: AnsiString;
  begin
    // grava a quantidade de macros
    count := FMacros.Count;
    AStream.Write(count, SizeOf(count));
    // grava símbolos
    for I := 0 to count - 1 do
    begin
      ln := AnsiString(FMacros[I]);
      len := Length(ln);
      AStream.Write(len, SizeOf(len));
      AStream.Write(ln[1], len);
    end;
  end;
  function SaveFontsToStream(AStream: TStream): Int64;
  var
    count, len, I: Integer;
    name: AnsiString;
  begin
    // guarda posição inicial de gravação
    Result := AStream.Position;
    //
    count := FFonts.Count;
    AStream.Write(count, SizeOf(count));
    // grava nomes das fontes
    for I := 0 to count - 1 do
    begin
      name := AnsiString(FFonts[I]);
      len := Length(name);
      // grava length + nome
      AStream.Write(len, SizeOf(len));
      AStream.Write(name[1], len);
    end;
  end;
  function SaveObjectToStream(AStream: TStream; AObject: TRLGraphicObject): Int64;
  var
    kind: TGraphicObjectKind;
    size: Integer;
    sizeoffset: Int64;
    dataoffset: Int64;
    endpos: Int64;
  begin
    // guarda posição inicial de gravação
    Result := AStream.Position;
    // grava tipo
    kind := GraphicObjectKind(AObject);
    AStream.Write(kind, SizeOf(kind));
    // reserva tamanho
    sizeoffset := AStream.Position;
    size := 0;
    AStream.Write(size, SizeOf(size));
    // grava objeto
    dataoffset := AStream.Position;
    AObject.SaveToStream(AStream);
    // ajusta tamanho
    endpos := AStream.Position;
    size := Integer(endpos - dataoffset);
    AStream.Position := sizeoffset;
    AStream.Write(size, SizeOf(size));
    // restaura eof
    AStream.Position := endpos;
  end;
  procedure SaveObjectsToStream(AStream: TStream);
  var
    count, I: Integer;
  begin
    count := ObjectCount;
    AStream.Write(count, SizeOf(count));
    // grava dados dos objetos
    for I := 0 to count - 1 do
      SaveObjectToStream(AStream, Objects[I]);
  end;
begin
  SaveHeaderToStream(AStream);
  SaveBoundsToStream(AStream);
  SaveMacrosToStream(AStream);
  SaveFontsToStream(AStream);
  SaveObjectsToStream(AStream);
end;

function GraphicObjectClass(AGraphicObjectKind: TGraphicObjectKind): TRLGraphicObjectClass;
begin
  case AGraphicObjectKind of
    ObjectKindPixel: Result := TRLPixelObject;
    ObjectKindLine: Result := TRLLineObject;
    ObjectKindRectangle: Result := TRLRectangleObject;
    ObjectKindText: Result := TRLTextObject;
    ObjectKindFillRect: Result := TRLFillRectObject;
    ObjectKindEllipse: Result := TRLEllipseObject;
    ObjectKindPolygon: Result := TRLPolygonObject;
    ObjectKindPolyline: Result := TRLPolylineObject;
    ObjectKindImage: Result := TRLImageObject;
    ObjectKindSetClipRect: Result := TRLSetClipRectObject;
    ObjectKindResetClipRect: Result := TRLResetClipRectObject;
    ObjectKindArc: Result := TRLArcObject;
  else
    Result := nil;
  end;
end;

procedure TRLGraphicSurface.LoadFromStream(AStream: TStream);
  procedure LoadHeaderFromStream(AStream: TStream);
  var
    data: AnsiString;
    ch: char;
    I: Integer;
  begin
    SetLength(data, MaxSurfaceHeader);
    I := 0;
    while (I < MaxSurfaceHeader) and (AStream.Read(ch, 1) = 1) do
    begin
      Inc(I);
      data[I] := AnsiChar(ch);
      if AnsiChar(ch) = #26 then
        Break;
    end;
    SetLength(data, I);
    if data <> SurfaceHeaderStr then
      raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_FileCorrupted));
  end;
  procedure LoadBoundsFromStream(AStream: TStream);
  begin
    AStream.Read(FWidth, SizeOf(FWidth));
    AStream.Read(FHeight, SizeOf(FHeight));
  end;
  procedure LoadMacrosFromStream(AStream: TStream);
  var
    count, len, I: Integer;
    ln: AnsiString;
  begin
    AStream.Read(count, SizeOf(count));
    // grava símbolos e seus valores
    for I := 0 to count - 1 do
    begin
      // lê length + nome
      AStream.Read(len, SizeOf(len));
      SetLength(ln, len);
      AStream.Read(ln[1], len);
      //
      FMacros.Add(ln);
    end;
  end;
  procedure LoadFontsFromStream(AStream: TStream);
  var
    count, len, I: Integer;
    name: AnsiString;
  begin
    AStream.Read(count, SizeOf(count));
    // carrega nomes das fontes
    for I := 0 to count - 1 do
    begin
      AStream.Read(len, SizeOf(len));
      SetLength(name, len);
      AStream.Read(name[1], len);
      FFonts.Add(name);
    end;
  end;
  procedure LoadObjectsFromStream(AStream: TStream);
  var
    count: Integer;
    size: Integer;
    kind: TGraphicObjectKind;
    creator: TRLGraphicObjectClass;
    I: Integer;
  begin
    AStream.Read(count, SizeOf(count));
    for I := 0 to count - 1 do
    begin
      AStream.Read(kind, SizeOf(kind));
      AStream.Read(size, SizeOf(size));
      creator := GraphicObjectClass(kind);
      // se a classe não for conhecida, salta o segmento
      if creator <> nil then
        creator.Create(Self).LoadFromStream(AStream)
      else
        AStream.Position := AStream.Position + size; 
    end;
  end;
begin
  Clear;
  LoadHeaderFromStream(AStream);
  LoadBoundsFromStream(AStream);
  LoadMacrosFromStream(AStream);
  LoadFontsFromStream(AStream);
  LoadObjectsFromStream(AStream);
end;

function TRLGraphicSurface.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TRLGraphicSurface.GetObjects(AIndex: Integer): TRLGraphicObject;
begin
  Result := TRLGraphicObject(FObjects[AIndex]);
end;

function TRLGraphicSurface.FindFreeRow(ANearRow: Integer; var ARow: Integer): Boolean;
var
  I: Integer;
  G: TRLGraphicObject;
  B: Boolean;
begin
  ARow := ANearRow;
  repeat
    B := False;
    for I := 0 to ObjectCount - 1 do
    begin
      G := Objects[I];
      if (G is TRLTextObject) and ((TRLTextObject(G).TextFlags and MetaTextFlagIntegralHeight) = MetaTextFlagIntegralHeight) then
        if (ARow > G.BoundsRect.Top) and (ARow < G.BoundsRect.Bottom) then
        begin
          ARow := G.BoundsRect.Top;
          B := True;
        end;
    end;
  until not B or (ARow <= 0);
  Result := (ARow > 0);
end;

procedure TRLGraphicSurface.Open;
begin
  if not FOpened then
  begin
    FOpened := True;
    FPenPos := Point(FMargins.Left, FMargins.Top);
    FGeneratorId := 0;
    FClipRect := Rect(0, 0, FWidth, FHeight);
    FClipStack.Clear;
  end;
end;

procedure TRLGraphicSurface.Close;
begin
  if FOpened then
    FOpened := False;
end;

procedure TRLGraphicSurface.Clear;
begin
  FObjects.Clear;
  FFonts.Clear;
  FMacros.Clear;
  //
  FPenPos := Point(0, 0);
  FModified := True;
  FGeneratorId := 0;
end;

procedure TRLGraphicSurface.Ellipse(const ARect: TRect);
var
  obj: TRLEllipseObject;
begin
  Open;
  FModified := True;
  obj := TRLEllipseObject.Create(Self);
  obj.BoundsRect := ToMetaRect(ARect);
  ToMetaPen(Self.Pen, obj.Pen);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  Ellipse(Rect(AX1, AY1, AX2, AY2));
end;

procedure TRLGraphicSurface.FillRect(const ARect: TRect);
var
  obj: TRLFillRectObject;
begin
  Open;
  FModified := True;
  obj := TRLFillRectObject.Create(Self);
  obj.BoundsRect := ToMetaRect(ARect);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.MoveTo(AX, AY: Integer);
begin
  Open;
  FModified := True;
  FPenPos := Point(AX, AY);
end;

procedure TRLGraphicSurface.LineTo(AX, AY: Integer);
var
  obj: TRLLineObject;
begin
  Open;
  FModified := True;
  obj := TRLLineObject.Create(Self);
  obj.FromPoint := ToMetaPoint(FPenPos);
  FPenPos := Point(AX, AY);
  obj.ToPoint := ToMetaPoint(FPenPos);
  obj.BoundsRect := ToMetaRect(Rect(Min(obj.FromPoint.X, obj.ToPoint.X) - 1, 
                                  Min(obj.FromPoint.Y, obj.ToPoint.Y) - 1, 
                                  Max(obj.FromPoint.X, obj.ToPoint.X) + 1, 
                                  Max(obj.FromPoint.Y, obj.ToPoint.Y) + 1));
  ToMetaPen(Self.Pen, obj.Pen);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.Arc(const ARectEllipse, ARectArc: TRect);
var
  obj: TRLArcObject;
begin
  Open;
  FModified := True;
  obj := TRLArcObject.Create(Self);
  obj.ArcRect := ToMetaRect(ARectArc);
  obj.BoundsRect := ToMetaRect(ARectEllipse);
  ToMetaPen(Self.Pen, obj.Pen);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.Arc(const AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer);
begin
  Arc(Rect(AX1, AY1, AX2, AY2), Rect(SX, SY, EX, EY));
end;

procedure TRLGraphicSurface.Polygon(const APoints: array of TPoint);
var
  obj: TRLPolygonObject;
begin
  Open;
  FModified := True;
  obj := TRLPolygonObject.Create(Self);
  obj.Points := ToMetaPointArray(APoints);
  obj.BoundsRect := ToMetaRect(GetPointsBounds(obj.Points));
  ToMetaPen(Self.Pen, obj.Pen);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.Polyline(const APoints: array of TPoint);
var
  obj: TRLPolylineObject;
begin
  Open;
  FModified := True;
  obj := TRLPolylineObject.Create(Self);
  obj.Points := ToMetaPointArray(APoints);
  obj.BoundsRect := ToMetaRect(GetPointsBounds(obj.Points));
  ToMetaPen(Self.Pen, obj.Pen);
end;

procedure TRLGraphicSurface.Rectangle(ALeft, ATop, ARight, ABottom: Integer);
var
  obj: TRLRectangleObject;
begin
  Open;
  FModified := True;
  obj := TRLRectangleObject.Create(Self);
  obj.BoundsRect := ToMetaRect(Rect(ALeft, ATop, ARight, ABottom));
  ToMetaPen(Self.Pen, obj.Pen);
  ToMetaBrush(Self.Brush, obj.Brush);
end;

procedure TRLGraphicSurface.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom);
end;

procedure TRLGraphicSurface.SetClipRect(const ARect: TRect);
var
  obj: TRLSetClipRectObject;
begin
  Open;
  FModified := True;
  obj := TRLSetClipRectObject.Create(Self);
  obj.BoundsRect := ToMetaRect(ARect);
  PushClipRect(FClipRect);
  FClipRect := ARect;
end;

procedure TRLGraphicSurface.ResetClipRect;
var
  obj: TRLResetClipRectObject;
begin
  Open;
  FModified := True;
  obj := TRLResetClipRectObject.Create(Self);
  obj.BoundsRect := ToMetaRect(FClipRect);
  PopClipRect(FClipRect);
end;

procedure TRLGraphicSurface.PaintTo(ACanvas: TCanvas; ARect: TRect);
var
  xfactor, yfactor: Double;
  I: Integer;
begin
  if FWidth = 0 then
    xfactor := 1
  else
    xfactor := (ARect.Right - ARect.Left) / FWidth;
  if FHeight = 0 then
    yfactor := 1
  else
    yfactor := (ARect.Bottom - ARect.Top) / FHeight;
  //
  FClipStack.Clear;
  try
    FClipRect := ARect;
    CanvasStart(ACanvas);
    try
      CanvasSetClipRect(ACanvas, FClipRect);
      try
        for I := 0 to ObjectCount - 1 do
          Objects[I].PaintTo(ACanvas, xfactor, yfactor, ARect.Left, ARect.Top);
      finally
        CanvasResetClipRect(ACanvas);
      end;
    finally
      CanvasStop(ACanvas);
    end;
  finally
    while FClipStack.Count > 0 do
      PopClipRect(FClipRect);
  end;
end;

procedure TRLGraphicSurface.CopyRect(const ADest: TRect; ACanvas: TCanvas; const ASource: TRect);
var
  B: TBitmap;
begin
  B := NeedAuxBitmap;
  B.Width := ASource.Right - ASource.Left;
  B.Height := ASource.Bottom - ASource.Top;
  B.PixelFormat := pf32bit;
  B.Canvas.CopyRect(Rect(0, 0, B.Width, B.Height), ACanvas, ASource);
  StretchDraw(ADest, B);
end;

procedure TRLGraphicSurface.CopyRect(const ADest: TRect; ASurface: TRLGraphicSurface; const ASource: TRect);
var
  xfactor, yfactor: Double;
  xdesloc, ydesloc: Integer;
  obj, clone: TRLGraphicObject;
  P: TRLMetaRect;
  R: TRect;
  I: Integer;
begin
  Open;
  FModified := True;
  xfactor := (ADest.Right - ADest.Left) / (ASource.Right - ASource.Left);
  yfactor := (ADest.Bottom - ADest.Top) / (ASource.Bottom - ASource.Top);
  xdesloc := ADest.Left - Round(ASource.Left * xfactor);
  ydesloc := ADest.Top - Round(ASource.Top * yfactor);
  //
  SetClipRect(ADest);
  try
    for I := 0 to ASurface.ObjectCount - 1 do
    begin
      obj := ASurface.Objects[I];
      P := obj.FBoundsRect;
      R.Left := P.Left;
      R.Top := P.Top;
      R.Right := P.Right;
      R.Bottom := P.Bottom;
      if IntersectRect(R, ASource, R) then
      begin
        clone := obj.Clone(Self);
        clone.Inflate(xfactor, yfactor);
        clone.Offset(xdesloc, ydesloc);
      end;
    end;
  finally
    ResetClipRect;
  end;
end;

procedure TRLGraphicSurface.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TRLGraphicSurface.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TRLGraphicSurface.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

function TRLGraphicSurface.GetPixels(X, Y: Integer): TColor;
begin
  Result := Self.Brush.Color;
end;

procedure TRLGraphicSurface.SetPixels(X, Y: Integer; const Value: TColor);
var
  obj: TRLPixelObject;
begin
  Open;
  FModified := True;
  obj := TRLPixelObject.Create(Self);
  obj.BoundsRect := ToMetaRect(Rect(X, Y, X + 1, Y + 1));
  obj.Color := ToMetaColor(Value);
end;

procedure TRLGraphicSurface.SetStorage(AStorage: TRLGraphicStorage);
begin
  if FStorage <> AStorage then
  begin
    if Assigned(FStorage) then
      FStorage.FPageCache.Extract(Self);
    FStorage := AStorage;
  end; 
end;

function TRLGraphicSurface.TextWidth(const AText: String): Integer;
var
  B: TBitmap;
begin
  B := NeedAuxBitmap;
  B.Canvas.Font.Assign(FFont);
  Result := B.Canvas.TextWidth(AText);
end;

function TRLGraphicSurface.TextHeight(const AText: String): Integer;
var
  B: TBitmap;
begin
  B := NeedAuxBitmap;
  B.Canvas.Font.Assign(FFont);
  Result := B.Canvas.TextHeight(AText);
end;

procedure TRLGraphicSurface.TextOut(ALeft, ATop: Integer; const AText: AnsiString);
begin
  TextOutEx(ALeft, ATop, AText, MetaTextFlagAutoSize or MetaTextFlagIntegralHeight);
end;

procedure TRLGraphicSurface.TextOutEx(ALeft, ATop: Integer; const AText: AnsiString; ATextFlags: TRLMetaTextFlags);
var
  obj: TRLTextObject;
begin
  Open;
  FModified := True;
  obj := TRLTextObject.Create(Self);
  obj.BoundsRect := ToMetaRect(Rect(ALeft, ATop, ALeft + TextWidth(AText), ATop + TextHeight(AText)));
  obj.Text := AText;
  obj.Origin := ToMetaPoint(Point(ALeft, ATop));
  obj.Alignment := MetaTextAlignmentLeft;
  obj.Layout := MetaTextLayoutTop;
  obj.TextFlags := ATextFlags;
  ToMetaBrush(Self.Brush, obj.Brush);
  ToMetaFont(Self.Font, obj.Font);
end;

procedure TRLGraphicSurface.TextRect(const ARect: TRect; ALeft, ATop: Integer; const AText: AnsiString);
begin
  TextRectEx(ARect, ALeft, ATop, AText, MetaTextAlignmentLeft, MetaTextLayoutTop, MetaTextFlagIntegralHeight);
end;

procedure TRLGraphicSurface.TextRectEx(const ARect: TRect; ALeft, ATop: Integer; const AText: AnsiString; AAlignment: TRLMetaTextAlignment; ALayout: TRLMetaTextLayout; ATextFlags: TRLMetaTextFlags);
var
  obj: TRLTextObject;
  wid, left, I: Integer;
  text: AnsiString;
begin
  Open;
  FModified := True;
  obj := TRLTextObject.Create(Self);
  obj.BoundsRect := ToMetaRect(ARect);

  text := AText;
  if AAlignment = MetaTextAlignmentJustify then
  begin
    left := ALeft + Self.TextWidth(' ') div 2;

    wid := ARect.Right - left;
    I := Length(text);
    while (Self.TextWidth(text + #32) <= wid) and IterateJustification(text, I) do;
  end;
  obj.Text := text;

  obj.Origin := ToMetaPoint(Point(ALeft, ATop));
  obj.Alignment := AAlignment;
  obj.Layout := ALayout;
  obj.TextFlags := ATextFlags;
  ToMetaBrush(Self.Brush, obj.Brush);
  ToMetaFont(Self.Font, obj.Font);
end;

procedure TRLGraphicSurface.Write(const AText: String);
begin
  TextOut(FPenPos.X, FPenPos.Y, AnsiString(AText));
  Inc(FPenPos.X, TextWidth(AText));
end;

procedure TRLGraphicSurface.Writeln(const AText: String);
begin
  TextOut(FPenPos.X, FPenPos.Y, AnsiString(AText));
  FPenPos.X := FMargins.Left;
  Inc(FPenPos.Y, TextHeight(AText));
end;

procedure TRLGraphicSurface.Draw(AX, AY: Integer; AGraphic: TGraphic; AParity: Boolean = False);
var
  obj: TRLImageObject;
begin
  Open;
  FModified := True;
  obj := TRLImageObject.Create(Self);
  obj.BoundsRect := ToMetaRect(Rect(AX, AY, AX + AGraphic.Width, AY + AGraphic.Height));
  obj.Data := AnsiString(ToMetaGraphic(AGraphic));
  obj.Parity := AParity;
end;

procedure TRLGraphicSurface.Draw(AX, AY: Integer; ASurface: TRLGraphicSurface);
var
  I: Integer;
begin
  Open;
  FModified := True;
  for I := 0 to ASurface.ObjectCount - 1 do
    ASurface.Objects[I].Clone(Self).Offset(AX, AY);
  Self.Macros.AddStrings(ASurface.Macros);
end;

procedure TRLGraphicSurface.StretchDraw(const ARect: TRect; AGraphic: TGraphic; AParity: Boolean = False);
var
  obj: TRLImageObject;
begin
  Open;
  FModified := True;
  obj := TRLImageObject.Create(Self);
  obj.BoundsRect := ToMetaRect(ARect);
  obj.Data := AnsiString(ToMetaGraphic(AGraphic));
  obj.Parity := AParity;
end;

procedure TRLGraphicSurface.StretchDraw(const ARect: TRect; ASurface: TRLGraphicSurface);
begin
  CopyRect(ARect, ASurface, Rect(0, 0, ASurface.Width, ASurface.Height));
end;

procedure TRLGraphicSurface.ScaleDraw(const ARect: TRect; AGraphic: TGraphic; ACenter: Boolean);
var
  scaledrect: TRect;
begin
  scaledrect := ScaleRect(Rect(0, 0, AGraphic.Width, AGraphic.Height), ARect, ACenter);
  StretchDraw(scaledrect, AGraphic);
end;

procedure TRLGraphicSurface.ScaleDraw(const ARect: TRect; ASurface: TRLGraphicSurface; ACenter: Boolean);
var
  scaledrect: TRect;
begin
  scaledrect := ScaleRect(Rect(0, 0, ASurface.Width, ASurface.Height), ARect, ACenter);
  StretchDraw(scaledrect, ASurface);
end;

procedure TRLGraphicSurface.ClipDraw(const ARect: TRect; AGraphic: TGraphic; ACenter: Boolean);
var
  B: TBitmap;
  R: TRect;
begin
  R := ARect;
  B := ClipGraphic(AGraphic, R, ACenter);
  if Assigned(B) then
    try
      StretchDraw(R, B);
    finally
      B.free;
    end;
end;

procedure TRLGraphicSurface.ClipDraw(const ARect: TRect; ASurface: TRLGraphicSurface; ACenter: Boolean);
var
  B: TRLGraphicSurface;
  R: TRect;
begin
  R := ARect;
  B := ClipSurface(ASurface, R, ACenter);
  if Assigned(B) then
    try
      StretchDraw(R, B);
    finally
      B.free;
    end;
end;

procedure TRLGraphicSurface.PushClipRect(const ARect: TRect);
var
  P: PRect;
begin
  New(P);
  P^ := ARect;
  FClipStack.Insert(0, P);
end;

procedure TRLGraphicSurface.PopClipRect(var ARect: TRect);
var
  P: PRect;
begin
  P := FClipStack[0];
  ARect := P^;
  Dispose(P);
  FClipStack.Delete(0);
end;

function TRLGraphicSurface.GetOrientation: TRLMetaOrientation;
begin
  Result := StrToIntDef(FMacros.Values['Orientation'], MetaOrientationPortrait);
end;

procedure TRLGraphicSurface.SetOrientation(const Value: TRLMetaOrientation);
begin
  FMacros.Values['Orientation'] := IntToStr(Value);
end;

function TRLGraphicSurface.GetPaperHeight: Double;
begin
  Result := PtStrToFloat(FMacros.Values['PaperHeight'], 0);
end;

procedure TRLGraphicSurface.SetPaperHeight(const Value: Double);
begin
  FMacros.Values['PaperHeight'] := FloatToPtStr(Value);
end;

function TRLGraphicSurface.GetPaperWidth: Double;
begin
  Result := PtStrToFloat(FMacros.Values['PaperWidth'], 0);
end;

procedure TRLGraphicSurface.SetPaperWidth(const Value: Double);
begin
  FMacros.Values['PaperWidth'] := FloatToPtStr(Value);
end;

function TRLGraphicSurface.GetOrientedPaperHeight: Double;
begin
  if Orientation = MetaOrientationPortrait then
    Result := PaperHeight
  else
    Result := PaperWidth;
end;

function TRLGraphicSurface.GetOrientedPaperWidth: Double;
begin
  if Orientation = MetaOrientationPortrait then
    Result := PaperWidth
  else
    Result := PaperHeight;
end;

function TRLGraphicSurface.GetOrientedHeight: Integer;
begin
  if Orientation = MetaOrientationPortrait then
    Result := FHeight
  else
    Result := FWidth;
end;

function TRLGraphicSurface.GetOrientedWidth: Integer;
begin
  if Orientation = MetaOrientationPortrait then
    Result := FWidth
  else
    Result := FHeight;
end;

{ TRLMetaPen }

constructor TRLMetaPen.Create(AUser: TRLGraphicObject);
begin
  FUser := AUser;
  FColor := MetaColor(0, 0, 0);
  FMode := MetaPenModeCopy;
  FStyle := MetaPenStyleSolid;
  FWidth := 0;
  //
  inherited Create;
end;

destructor TRLMetaPen.Destroy;
begin
  inherited;
end;

procedure TRLMetaPen.SaveToStream(AStream: TStream);
begin
  AStream.Write(FColor, SizeOf(FColor));
  AStream.Write(FMode, SizeOf(FMode));
  AStream.Write(FStyle, SizeOf(FStyle));
  AStream.Write(FWidth, SizeOf(FWidth));
end;

procedure TRLMetaPen.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FColor, SizeOf(FColor));
  AStream.Read(FMode, SizeOf(FMode));
  AStream.Read(FStyle, SizeOf(FStyle));
  AStream.Read(FWidth, SizeOf(FWidth));
end;

procedure TRLMetaPen.Assign(AObject: TRLMetaPen);
begin
  Color := AObject.Color;
  Mode := AObject.Mode;
  Style := AObject.Style;
  Width := AObject.Width;
end;

procedure TRLMetaPen.Inflate(AFactor: Double);
begin
  if Width <> 0 then
    Width := Max(1, Round(Width * AFactor));
end;

function TRLMetaPen.GetColor: TRLMetaColor;
begin
  Result := FColor;
end;

function TRLMetaPen.GetMode: TRLMetaPenMode;
begin
  Result := FMode;
end;

function TRLMetaPen.GetStyle: TRLMetaPenStyle;
begin
  Result := FStyle;
end;

function TRLMetaPen.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TRLMetaPen.SetColor(const Value: TRLMetaColor);
begin
  FColor := Value;
end;

procedure TRLMetaPen.SetMode(Value: TRLMetaPenMode);
begin
  FMode := Value;
end;

procedure TRLMetaPen.SetStyle(Value: TRLMetaPenStyle);
begin
  FStyle := Value;
end;

procedure TRLMetaPen.SetWidth(Value: Integer);
begin
  FWidth := Value;
end;

{ TRLMetaBrush }

constructor TRLMetaBrush.Create(AUser: TRLGraphicObject);
begin
  FUser := AUser;
  FColor := MetaColor(0, 0, 0);
  FStyle := MetaBrushStyleSolid;
  //
  inherited Create;
end;

procedure TRLMetaBrush.SaveToStream(AStream: TStream);
begin
  AStream.Write(FColor, SizeOf(FColor));
  AStream.Write(FStyle, SizeOf(FStyle));
end;

procedure TRLMetaBrush.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FColor, SizeOf(FColor));
  AStream.Read(FStyle, SizeOf(FStyle));
end;

procedure TRLMetaBrush.Assign(AObject: TRLMetaBrush);
begin
  Color := AObject.Color;
  Style := AObject.Style;
end;

function TRLMetaBrush.GetColor: TRLMetaColor;
begin
  Result := FColor;
end;

function TRLMetaBrush.GetStyle: TRLMetaBrushStyle;
begin
  Result := FStyle;
end;

procedure TRLMetaBrush.SetColor(const Value: TRLMetaColor);
begin
  FColor := Value;
end;

procedure TRLMetaBrush.SetStyle(Value: TRLMetaBrushStyle);
begin
  FStyle := Value;
end;

destructor TRLMetaBrush.Destroy;
begin
  inherited;
end;

{ TRLMetaFont }

constructor TRLMetaFont.Create(AUser: TRLGraphicObject);
begin
  FUser := AUser;
  FPixelsPerInch := 72;
  FCharset := 0;
  FColor := MetaColor(0, 0, 0);
  FHeight := 0;
  FNameId := 0;
  FPitch := MetaFontPitchDefault;
  FSize := 0;
  FStyle := 0;
  //
  inherited Create;
end;

procedure TRLMetaFont.SaveToStream(AStream: TStream);
begin
  AStream.Write(FPixelsPerInch, SizeOf(FPixelsPerInch));
  AStream.Write(FCharset, SizeOf(FCharset));
  AStream.Write(FColor, SizeOf(FColor));
  AStream.Write(FHeight, SizeOf(FHeight));
  AStream.Write(FNameId, SizeOf(FNameId));
  AStream.Write(FPitch, SizeOf(FPitch));
  AStream.Write(FSize, SizeOf(FSize));
  AStream.Write(FStyle, SizeOf(FStyle));
end;

procedure TRLMetaFont.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FPixelsPerInch, SizeOf(FPixelsPerInch));
  AStream.Read(FCharset, SizeOf(FCharset));
  AStream.Read(FColor, SizeOf(FColor));
  AStream.Read(FHeight, SizeOf(FHeight));
  AStream.Read(FNameId, SizeOf(FNameId));
  AStream.Read(FPitch, SizeOf(FPitch));
  AStream.Read(FSize, SizeOf(FSize));
  AStream.Read(FStyle, SizeOf(FStyle));
end;

procedure TRLMetaFont.Assign(AObject: TRLMetaFont);
begin
  PixelsPerInch := AObject.PixelsPerInch;
  Charset := AObject.Charset;
  Color := AObject.Color;
  Height := AObject.Height;
  Name := AObject.Name;
  Pitch := AObject.Pitch;
  Size := AObject.Size;
  Style := AObject.Style;
end;

function TRLMetaFont.GetName: string;
begin
  Result := FUser.FSurface.FFonts[FNameId];
end;

procedure TRLMetaFont.SetName(const Value: string);
begin
  FNameId := FUser.FSurface.FFonts.IndexOf(Value);
  if FNameId = -1 then
    FNameId := FUser.FSurface.FFonts.Add(Value);
end;

function TRLMetaFont.GetCharset: TRLMetaFontCharset;
begin
  Result := FCharset;
end;

function TRLMetaFont.GetColor: TRLMetaColor;
begin
  Result := FColor;
end;

function TRLMetaFont.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TRLMetaFont.GetPitch: TRLMetaFontPitch;
begin
  Result := FPitch;
end;

function TRLMetaFont.GetPixelsPerInch: Integer;
begin
  Result := FPixelsPerInch;
end;

function TRLMetaFont.GetSize: Integer;
begin
  Result := FSize;
end;

function TRLMetaFont.GetStyle: TRLMetaFontStyles;
begin
  Result := FStyle;
end;

procedure TRLMetaFont.SetCharset(Value: TRLMetaFontCharset);
begin
  FCharset := Value;
end;

procedure TRLMetaFont.SetColor(const Value: TRLMetaColor);
begin
  FColor := Value;
end;

procedure TRLMetaFont.SetHeight(Value: Integer);
begin
  FHeight := Value;
end;

procedure TRLMetaFont.SetPitch(Value: TRLMetaFontPitch);
begin
  FPitch := Value;
end;

procedure TRLMetaFont.SetPixelsPerInch(Value: Integer);
begin
  FPixelsPerInch := Value;
end;

procedure TRLMetaFont.SetSize(Value: Integer);
begin
  FSize := Value;
end;

procedure TRLMetaFont.SetStyle(Value: TRLMetaFontStyles);
begin
  FStyle := Value;
end;

destructor TRLMetaFont.Destroy;
begin
  inherited;
end;

{ TRLGraphicObject }

constructor TRLGraphicObject.Create(ASurface: TRLGraphicSurface);
begin
  FSurface := ASurface;
  FBoundsRect := ToMetaRect(Rect(0, 0, 0, 0));
  FGroupId := CurrentGroupId;
  FGeneratorId := 0;
  FTag := 0;
  //
  inherited Create;
  //
  FSurface.FObjects.Add(Self);
end;

destructor TRLGraphicObject.Destroy;
begin
  FSurface.FObjects.Extract(Self);
  //
  inherited;
end;

procedure TRLGraphicObject.SaveToStream(AStream: TStream);
begin
  AStream.Write(FBoundsRect, SizeOf(FBoundsRect));
  AStream.Write(FGroupId, SizeOf(FGroupId));
  AStream.Write(FGeneratorId, SizeOf(FGeneratorId));
end;

procedure TRLGraphicObject.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FBoundsRect, SizeOf(FBoundsRect));
  AStream.Read(FGroupId, SizeOf(FGroupId));
  AStream.Read(FGeneratorId, SizeOf(FGeneratorId));
end;

function TRLGraphicObject.Clone(ASurface: TRLGraphicSurface): TRLGraphicObject;
begin
  Result := TRLGraphicObjectClass(Self.ClassType).Create(ASurface);
  Result.Assign(Self);
end;

procedure TRLGraphicObject.Assign(AObject: TRLGraphicObject); 
begin
  BoundsRect := AObject.BoundsRect;
  GroupId := AObject.GroupId;
  GeneratorId := AObject.GeneratorId;
end;

function TRLGraphicObject.TransformRect(const ASourceRect: TRLMetaRect; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer; const HigherRightBottom: Boolean): TRect;
begin
  Result.Left := AXDesloc + Round(ASourceRect.Left * AXFactor);
  Result.Top := AYDesloc + Round(ASourceRect.Top * AYFactor);
  Result.Right := AXDesloc + Round(ASourceRect.Right * AXFactor);
  Result.Bottom := AYDesloc + Round(ASourceRect.Bottom * AYFactor);
  //
  if HigherRightBottom then
  begin
    Result.Right := Max(Result.Right, Result.Left + 1);
    Result.Bottom := Max(Result.Bottom, Result.Top + 1);
  end;
end;

function TRLGraphicObject.TransformBounds(AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer; const HigherRightBottom: Boolean): TRect;
begin
  Result := TransformRect(BoundsRect, AXFactor, AYFactor, AXDesloc, AYDesloc, HigherRightBottom);
end;

procedure TRLGraphicObject.Offset(AXDesloc, AYDesloc: Integer);
begin
  Inc(FBoundsRect.Left, AXDesloc);
  Inc(FBoundsRect.Top, AYDesloc);
  Inc(FBoundsRect.Right, AXDesloc);
  Inc(FBoundsRect.Bottom, AYDesloc);
end;

procedure TRLGraphicObject.Inflate(AXFactor, AYFactor: Double);
begin
  FBoundsRect.Left := Round(FBoundsRect.Left * AXFactor);
  FBoundsRect.Top := Round(FBoundsRect.Top * AYFactor);
  FBoundsRect.Right := Round(FBoundsRect.Right * AXFactor);
  FBoundsRect.Bottom := Round(FBoundsRect.Bottom * AYFactor);
end;

{ TRLPixelObject }

constructor TRLPixelObject.Create(ASurface: TRLGraphicSurface);
begin
  FColor := ToMetaColor(clBlack);
  //
  inherited;
end;

procedure TRLPixelObject.SaveToStream(AStream: TStream);
begin
  inherited;
  //
  AStream.Write(FColor, SizeOf(FColor));
end;

procedure TRLPixelObject.LoadFromStream(AStream: TStream);
begin
  inherited;
  //
  AStream.Read(FColor, SizeOf(FColor));
end;

procedure TRLPixelObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, True);
  //
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FromMetaColor(FColor);
  ACanvas.FillRect(R);
end;

procedure TRLPixelObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject); 
  //
  Color := TRLPixelObject(AObject).Color;
end;

destructor TRLPixelObject.Destroy;
begin
  inherited;
end;

{ TRLLineObject }

constructor TRLLineObject.Create(ASurface: TRLGraphicSurface);
begin
  FFromPoint := MetaPoint(0, 0);
  FToPoint := MetaPoint(0, 0);
  FPen := nil;
  FBrush := nil;
  //
  FPen := TRLMetaPen.Create(Self);
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLLineObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLLineObject.SaveToStream(AStream: TStream);
begin
  inherited;
  //
  AStream.Write(FFromPoint, SizeOf(FFromPoint));
  AStream.Write(FToPoint, SizeOf(FToPoint));
  FPen.SaveToStream(AStream);
  FBrush.SaveToStream(AStream);
end;

procedure TRLLineObject.LoadFromStream(AStream: TStream);
begin
  inherited;
  //
  AStream.Read(FFromPoint, SizeOf(FFromPoint));
  AStream.Read(FToPoint, SizeOf(FToPoint));
  FPen.LoadFromStream(AStream);
  FBrush.LoadFromStream(AStream);
end;

procedure TRLLineObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  p1, p2: TPoint;
begin
  p1.X := AXDesloc + Round(FFromPoint.X * AXFactor);
  p1.Y := AYDesloc + Round(FFromPoint.Y * AYFactor);
  p2.X := AXDesloc + Round(FToPoint.X * AXFactor);
  p2.Y := AYDesloc + Round(FToPoint.Y * AYFactor);
  //
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  FromMetaBrush(FBrush, ACanvas.Brush);
  ACanvas.MoveTo(p1.X, p1.Y);
  CanvasLineToEx(ACanvas, p2.X, p2.Y);
end;

procedure TRLLineObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  FromPoint := TRLLineObject(AObject).FromPoint;
  ToPoint := TRLLineObject(AObject).ToPoint;
  Pen := TRLLineObject(AObject).Pen;
  Brush := TRLLineObject(AObject).Brush;
end;

procedure TRLLineObject.Offset(AXDesloc, AYDesloc: Integer); 
begin
  inherited Offset(AXDesloc, AYDesloc);
  //
  Inc(FFromPoint.X, AXDesloc);
  Inc(FFromPoint.Y, AYDesloc);
  Inc(FToPoint.X, AXDesloc);
  Inc(FToPoint.Y, AYDesloc);
end;

procedure TRLLineObject.Inflate(AXFactor, AYFactor: Double);
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  FFromPoint.X := Round(FFromPoint.X * AXFactor);
  FFromPoint.Y := Round(FFromPoint.Y * AYFactor);
  FToPoint.X := Round(FToPoint.X * AXFactor);
  FToPoint.Y := Round(FToPoint.Y * AYFactor);
  FPen.Inflate(AXFactor);
end;

procedure TRLLineObject.SetPen(Value: TRLMetaPen);
begin
  FPen.Assign(Value);
end;

procedure TRLLineObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLRectangleObject }

constructor TRLRectangleObject.Create(ASurface: TRLGraphicSurface);
begin
  FPen := nil;
  FBrush := nil;
  //
  FPen := TRLMetaPen.Create(Self);
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLRectangleObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLRectangleObject.SaveToStream(AStream: TStream);
begin
  inherited;
  //
  FPen.SaveToStream(AStream);
  FBrush.SaveToStream(AStream);
end;

procedure TRLRectangleObject.LoadFromStream(AStream: TStream);
begin
  inherited;
  //
  FPen.LoadFromStream(AStream);
  FBrush.LoadFromStream(AStream);
end;

procedure TRLRectangleObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, True);
  //
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  FromMetaBrush(FBrush, ACanvas.Brush);
  ACanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TRLRectangleObject.Assign(AObject: TRLGraphicObject);
begin
  inherited Assign(AObject);
  //
  Pen := TRLRectangleObject(AObject).Pen;
  Brush := TRLRectangleObject(AObject).Brush;
end;

procedure TRLRectangleObject.Inflate(AXFactor, AYFactor: Double);
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  FPen.Inflate(AXFactor);
end;

procedure TRLRectangleObject.SetPen(Value: TRLMetaPen);
begin
  FPen.Assign(Value);
end;

procedure TRLRectangleObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLTextObject }

constructor TRLTextObject.Create(ASurface: TRLGraphicSurface);
begin
  FAlignment := MetaTextAlignmentLeft;
  FBrush := nil;
  FFont := nil;
  FLayout := MetaTextLayoutTop;
  FOrigin := MetaPoint(0, 0);
  FText := '';
  FTextFlags := MetaTextFlagAutoSize or MetaTextFlagIntegralHeight;
  //
  FBrush := TRLMetaBrush.Create(Self);
  FFont := TRLMetaFont.Create(Self);
  //
  inherited;
end;

destructor TRLTextObject.Destroy;
begin
  inherited;
  //
  if Assigned(FBrush) then
    FBrush.free;
  if Assigned(FFont) then
    FFont.free;
end;

procedure TRLTextObject.SaveToStream(AStream: TStream);
var
  len: Integer;
begin
  inherited;
  //
  AStream.Write(FAlignment, SizeOf(FAlignment));
  AStream.Write(FLayout, SizeOf(FLayout));
  AStream.Write(FOrigin, SizeOf(FOrigin));
  AStream.Write(FTextFlags, SizeOf(FTextFlags));
  //
  len := Length(FText);
  AStream.Write(len, SizeOf(len));
  if len > 0 then
    AStream.Write(FText[1], len);
  //
  FBrush.SaveToStream(AStream);
  FFont.SaveToStream(AStream);
end;

procedure TRLTextObject.LoadFromStream(AStream: TStream);
var
  len: Integer;
begin
  inherited;
  //
  AStream.Read(FAlignment, SizeOf(FAlignment));
  AStream.Read(FLayout, SizeOf(FLayout));
  AStream.Read(FOrigin, SizeOf(FOrigin));
  AStream.Read(FTextFlags, SizeOf(FTextFlags));
  //
  AStream.Read(len, SizeOf(len));
  SetLength(FText, len);
  if len > 0 then
    AStream.Read(FText[1], len);
  //
  FBrush.LoadFromStream(AStream);
  FFont.LoadFromStream(AStream);
end;

// processa macros
procedure TRLTextObject.TranslateMacros(var AText: AnsiString);
var
  keyword, keyvalue: String;
  macros1, macros2: TStrings;
  I, M: Integer;
begin
  macros1 := FSurface.Macros;
  if Assigned(FSurface.FStorage) then
    macros2 := FSurface.FStorage.Macros
  else
    macros2 := nil;
  I := 1;
  while I <= Length(AText) do
    if AText[I] = '{' then
    begin
      M := I;
      while (I <= Length(AText)) and (AText[I] <> '}') do
        Inc(I);
      if I <= Length(AText) then
      begin
        keyword := Copy(AText, M + 1, I - (M + 1));
        if macros1.IndexOfName(keyword) <> -1 then
          keyvalue := macros1.Values[keyword]
        else if Assigned(macros2) and (macros2.IndexOfName(keyword) <> -1) then
          keyvalue := macros2.Values[keyword]
        else
          Continue; 
        Delete(AText, M, I - M + 1);
        Insert(AnsiString(keyvalue), AText, M);
        I := M + Length(keyvalue);
      end;
    end
    else
      Inc(I);
end;

procedure TRLTextObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
  O: TPoint;
  T: String;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  O.X := AXDesloc + Round(FOrigin.X * AXFactor);
  O.Y := AYDesloc + Round(FOrigin.Y * AYFactor);
  //
  FromMetaBrush(FBrush, ACanvas.Brush);
  FromMetaFont(FFont, ACanvas.Font, AYFactor);
  T := DisplayText;
  CanvasTextRectEx(ACanvas, R, O.X, O.Y, T, FAlignment, FLayout, FTextFlags);
end;

procedure TRLTextObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  Alignment := TRLTextObject(AObject).Alignment;
  TextFlags := TRLTextObject(AObject).TextFlags;
  Brush := TRLTextObject(AObject).Brush;
  Font := TRLTextObject(AObject).Font;
  Layout := TRLTextObject(AObject).Layout;
  Origin := TRLTextObject(AObject).Origin;
  Text := TRLTextObject(AObject).Text;
end;

procedure TRLTextObject.Offset(AXDesloc, AYDesloc: Integer); 
begin
  inherited Offset(AXDesloc, AYDesloc);
  //
  Inc(FOrigin.X, AXDesloc);
  Inc(FOrigin.Y, AYDesloc);
end;

procedure TRLTextObject.Inflate(AXFactor, AYFactor: Double);
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  FOrigin.X := Round(FOrigin.X * AXFactor);
  FOrigin.Y := Round(FOrigin.Y * AYFactor);
  FFont.Size := Round(FFont.Size * AYFactor);
end;

procedure TRLTextObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

procedure TRLTextObject.SetFont(Value: TRLMetaFont);
begin
  FFont.Assign(Value);
end;

function TRLTextObject.GetDisplayText: AnsiString;
begin
  Result := FText;
  TranslateMacros(Result);
end;

{ TRLFillRectObject }

constructor TRLFillRectObject.Create(ASurface: TRLGraphicSurface);
begin
  FBrush := nil;
  //
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLFillRectObject.Destroy;
begin
  inherited;
  //
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLFillRectObject.SaveToStream(AStream: TStream);
begin
  inherited;
  //
  FBrush.SaveToStream(AStream);
end;

procedure TRLFillRectObject.LoadFromStream(AStream: TStream);
begin
  inherited;
  //
  FBrush.LoadFromStream(AStream);
end;

procedure TRLFillRectObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  //
  FromMetaBrush(FBrush, ACanvas.Brush);
  ACanvas.FillRect(R);
end;

procedure TRLFillRectObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  Brush := TRLFillRectObject(AObject).Brush;
end;

procedure TRLFillRectObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLEllipseObject }

constructor TRLEllipseObject.Create(ASurface: TRLGraphicSurface);
begin
  FPen := nil;
  FBrush := nil;
  //
  FPen := TRLMetaPen.Create(Self);
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLEllipseObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLEllipseObject.SaveToStream(AStream: TStream);
begin
  inherited;
  //
  FPen.SaveToStream(AStream);
  FBrush.SaveToStream(AStream);
end;

procedure TRLEllipseObject.LoadFromStream(AStream: TStream);
begin
  inherited;
  //
  FPen.LoadFromStream(AStream);
  FBrush.LoadFromStream(AStream);
end;

procedure TRLEllipseObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  //
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  FromMetaBrush(FBrush, ACanvas.Brush);
  ACanvas.Ellipse(R);
end;

procedure TRLEllipseObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  Pen := TRLEllipseObject(AObject).Pen;
  Brush := TRLEllipseObject(AObject).Brush;
end;

procedure TRLEllipseObject.Inflate(AXFactor, AYFactor: Double);
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  FPen.Inflate(AXFactor);
end;

procedure TRLEllipseObject.SetPen(Value: TRLMetaPen);
begin
  FPen.Assign(Value);
end;

procedure TRLEllipseObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLArcObject }

constructor TRLArcObject.Create(aSurface:TRLGraphicSurface);
begin
  FPen := nil;
  FBrush := nil;
  //
  FPen := TRLMetaPen.Create(Self);
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLArcObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLArcObject.SaveToStream(aStream: TStream);
begin
  inherited;
  //
  FPen.SaveToStream(aStream);
  FBrush.SaveToStream(AStream);
end;

procedure TRLArcObject.LoadFromStream(aStream: TStream);
begin
  inherited;
  //
  FPen.LoadFromStream(aStream);
  FBrush.LoadFromStream(AStream);
end;

procedure TRLArcObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  RectEllipse, RectArc: TRect;
begin
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  //
  RectEllipse := TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, True);
  RectArc := TransformRect(ArcRect, AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  //
  ACanvas.Arc(RectEllipse.Left, RectEllipse.Top, RectEllipse.Right, RectEllipse.Bottom, RectArc.Left, RectArc.Top, RectArc.Right, RectArc.Bottom);
end;

procedure TRLArcObject.Assign(aObject: TRLGraphicObject);
begin
  inherited Assign(aObject);
  //
  Pen := TRLArcObject(aObject).Pen;
  Brush := TRLArcObject(AObject).Brush;
  ArcRect := TRLArcObject(AObject).ArcRect;
end;

procedure TRLArcObject.Inflate(aXFactor, aYFactor: double);
begin
  inherited Inflate(aXFactor, aYFactor);
  //
  FPen.Inflate(aXFactor);
end;

procedure TRLArcObject.SetPen(Value:TRLMetaPen);
begin
  FPen.Assign(Value);
end;

procedure TRLArcObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLPolygonObject }

constructor TRLPolygonObject.Create(ASurface: TRLGraphicSurface);
begin
  FPen := nil;
  FBrush := nil;
  SetLength(FPoints, 0);
  //
  FPen := TRLMetaPen.Create(Self);
  FBrush := TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLPolygonObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
  if Assigned(FBrush) then
    FBrush.free;
end;

procedure TRLPolygonObject.SaveToStream(AStream: TStream);
var
  I, count: Integer;
begin
  inherited;
  //
  FPen.SaveToStream(AStream);
  FBrush.SaveToStream(AStream);
  count := High(FPoints) + 1;
  AStream.Write(count, SizeOf(count));
  for I := 0 to count - 1 do
    AStream.Write(FPoints[I], SizeOf(FPoints[I]));
end;

procedure TRLPolygonObject.LoadFromStream(AStream: TStream);
var
  I, count: Integer;
begin
  inherited;
  //
  FPen.LoadFromStream(AStream);
  FBrush.LoadFromStream(AStream);
  AStream.Read(count, SizeOf(count));
  SetLength(FPoints, count);
  for I := 0 to count - 1 do
    AStream.Read(FPoints[I], SizeOf(FPoints[I]));
end;

procedure TRLPolygonObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  P: TPointArray;
  I: Integer;
begin
  SetLength(P, High(FPoints) + 1);
  for I := 0 to High(P) do
  begin
    P[I].X := AXDesloc + Round(FPoints[I].X * AXFactor);
    P[I].Y := AYDesloc + Round(FPoints[I].Y * AYFactor);
  end;
  //  
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  FromMetaBrush(FBrush, ACanvas.Brush);
  ACanvas.Polygon(P);
end;

procedure TRLPolygonObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  Pen := TRLPolygonObject(AObject).Pen;
  Brush := TRLPolygonObject(AObject).Brush;
  Points := TRLPolygonObject(AObject).Points;
end;

procedure TRLPolygonObject.Offset(AXDesloc, AYDesloc: Integer);
var
  I: Integer;
begin
  inherited Offset(AXDesloc, AYDesloc);
  //
  for I := 0 to High(FPoints) do
  begin
    Inc(FPoints[I].X, AXDesloc);
    Inc(FPoints[I].Y, AYDesloc);
  end;
end;

procedure TRLPolygonObject.Inflate(AXFactor, AYFactor: Double);
var
  I: Integer;
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  for I := 0 to High(FPoints) do
  begin
    FPoints[I].X := Round(FPoints[I].X * AXFactor);
    FPoints[I].Y := Round(FPoints[I].Y * AYFactor);
  end;
  FPen.Inflate(AXFactor);
end;

procedure TRLPolygonObject.SetPen(Value: TRLMetaPen);
begin
  FPen.Assign(Value);
end;

procedure TRLPolygonObject.SetBrush(Value: TRLMetaBrush);
begin
  FBrush.Assign(Value);
end;

{ TRLPolylineObject }

constructor TRLPolylineObject.Create(ASurface: TRLGraphicSurface);
begin
  FPen := nil;
  SetLength(FPoints, 0);
  //
  FPen := TRLMetaPen.Create(Self);
  //
  inherited;
end;

destructor TRLPolylineObject.Destroy;
begin
  inherited;
  //
  if Assigned(FPen) then
    FPen.free;
end;

procedure TRLPolylineObject.SaveToStream(AStream: TStream);
var
  I, count: Integer;
begin
  inherited;
  //
  FPen.SaveToStream(AStream);
  count := High(FPoints) + 1;
  AStream.Write(count, SizeOf(count));
  for I := 0 to count - 1 do
    AStream.Write(FPoints[I], SizeOf(FPoints[I]));
end;

procedure TRLPolylineObject.LoadFromStream(AStream: TStream);
var
  I, count: Integer;
begin
  inherited;
  //
  FPen.LoadFromStream(AStream);
  AStream.Read(count, SizeOf(count));
  SetLength(FPoints, count);
  for I := 0 to count - 1 do
    AStream.Read(FPoints[I], SizeOf(FPoints[I]));
end;

procedure TRLPolylineObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  P: TPointArray;
  I: Integer;
begin
  SetLength(P, High(FPoints) + 1);
  for I := 0 to High(P) do
  begin
    P[I].X := AXDesloc + Round(FPoints[I].X * AXFactor);
    P[I].Y := AYDesloc + Round(FPoints[I].Y * AYFactor);
  end;
  //
  FromMetaPen(FPen, ACanvas.Pen);
  PenInflate(ACanvas.Pen, AXFactor);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Polyline(P);
end;

procedure TRLPolylineObject.Assign(AObject: TRLGraphicObject); 
begin
  inherited Assign(AObject);
  //
  Pen := TRLPolylineObject(AObject).Pen;
  Points := TRLPolylineObject(AObject).Points;
end;

procedure TRLPolylineObject.Offset(AXDesloc, AYDesloc: Integer);
var
  I: Integer;
begin
  inherited Offset(AXDesloc, AYDesloc);
  //
  for I := 0 to High(FPoints) do
  begin
    Inc(FPoints[I].X, AXDesloc);
    Inc(FPoints[I].Y, AYDesloc);
  end;
end;

procedure TRLPolylineObject.Inflate(AXFactor, AYFactor: Double);
var
  I: Integer;
begin
  inherited Inflate(AXFactor, AYFactor);
  //
  for I := 0 to High(FPoints) do
  begin
    FPoints[I].X := Round(FPoints[I].X * AXFactor);
    FPoints[I].Y := Round(FPoints[I].Y * AYFactor);
  end;
  FPen.Inflate(AXFactor);
end;

procedure TRLPolylineObject.SetPen(Value: TRLMetaPen);
begin
  FPen.Assign(Value);
end;

{ TRLImageObject }

constructor TRLImageObject.Create(ASurface: TRLGraphicSurface);
begin
  FData := '';
  FParity := False;
  //
  inherited;
end;

procedure TRLImageObject.SaveToStream(AStream: TStream);
var
  len: Integer;
begin
  inherited;
  //
  len := Length(FData);
  AStream.Write(len, SizeOf(len));
  if len > 0 then
    AStream.Write(FData[1], len);
  //
  AStream.Write(FParity, SizeOf(FParity));
end;

procedure TRLImageObject.LoadFromStream(AStream: TStream);
var
  len: Integer;
begin
  inherited;
  //
  AStream.Read(len, SizeOf(len));
  SetLength(FData, len);
  if len > 0 then
    AStream.Read(FData[1], len);
  //
  AStream.Read(FParity, SizeOf(FParity));
end;

procedure TRLImageObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
var
  R: TRect;
begin
  R := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  //
  CanvasStretchDraw(ACanvas, R, Data, Parity);
end;

procedure TRLImageObject.Assign(AObject: TRLGraphicObject);
begin
  inherited Assign(AObject);
  //
  Data := TRLImageObject(AObject).Data;
  Parity := TRLImageObject(AObject).Parity;
end;

destructor TRLImageObject.Destroy;
begin
  inherited;
end;

{ TRLSetClipRectObject }

procedure TRLSetClipRectObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
begin
  FSurface.PushClipRect(FSurface.FClipRect);
  FSurface.FClipRect := Self.TransformBounds(AXFactor, AYFactor, AXDesloc, AYDesloc, False);
  CanvasSetClipRect(ACanvas, FSurface.FClipRect);
end;

{ TRLResetClipRectObject }

procedure TRLResetClipRectObject.PaintTo(ACanvas: TCanvas; AXFactor, AYFactor: Double; AXDesloc, AYDesloc: Integer);
begin
  FSurface.PopClipRect(FSurface.FClipRect);
  CanvasSetClipRect(ACanvas, FSurface.FClipRect);
end;

end.

