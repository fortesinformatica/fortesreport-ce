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

{@unit RLFilters - Implementação do filtro padrão de impressão e classes abstratas para filtros de gravação e impressão. }
unit RLFilters;

interface

uses
  {$IfDef MSWINDOWS}
   {$IfNDef FPC}
    Windows,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils,
  {$IfDef CLX}
   QTypes, QForms, QDialogs,
  {$Else}
   Types, Forms, Dialogs,
  {$EndIf}
  {$IfDef FPC}
   LCLIntf,
  {$EndIf}
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFeedBack, RLPrinters;

const
  PrintOddAndEvenPages = 0;
  PrintOddPagesOnly = 1;
  PrintEvenPagesOnly = 2;

type

  TRLCustomFilter = class;
  TRLCustomPrintFilter = class;
  TRLCustomSaveFilter = class;

  TRLFilterClassOption = (foEmulateCopies);
  TRLFilterClassOptions = set of TRLFilterClassOption;

  { TRLCustomFilter }

  {@class TRLCustomFilter - Classe abstrata ancestral de todos os filtro de gravação e impressão.
   @links TRLHTMLFilter, TRLPDFFilter, TRLRichFilter, TRLDraftFilter. }
  TRLCustomFilter = class(TComponent)
  private

    // variables

    FDisplayName: string;
    FPages: TRLGraphicStorage;
    FProgress: TfrmRLFeedBack;
    FShowProgress: Boolean;
    FCanceled: Boolean;
    FClassOptions: TRLFilterClassOptions;

    // assign methods

    procedure SetPages(const Value: TRLGraphicStorage);

    // custom methods

    function IsDisplayName: Boolean;
    procedure ProgressCanceled(Sender: TObject; var CancelIt: Boolean);
    procedure CreateProgress;
    procedure DestroyProgress;
    function PageInRange(APageNo: Integer;
      const APageSelection: string): Boolean;

  protected

    // virtual methods

    procedure InternalBeginDoc; virtual; abstract;
    procedure InternalEndDoc; virtual; abstract;
    procedure InternalNewPage; virtual; abstract;
    procedure InternalDrawPage(APage: TRLGraphicSurface); virtual; abstract;

    // override methods

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    //

    property ClassOptions: TRLFilterClassOptions read FClassOptions write FClassOptions;

  public

    // override methods

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    // virtual methods

    {@method GetDisplayLabel - Devolve nome do filtro para exibição em caixas de seleção. :/}
    function GetDisplayLabel: string; virtual;

    // static methods

    {@method GetDisplayName - Devolve nome do filtro para exibição em tempo de design. :/}
    function GetDisplayName: string;

    {@method BeginDoc - Inicializa o processo de filtragem. :/}
    procedure BeginDoc;

    {@method EndDoc - Finaliza o processo de filtragem. :/}
    procedure EndDoc;

    {@method NewPage - Adiciona uma nova página. :/}
    procedure NewPage;

    {@method DrawPage - Desenha o conteúdo da superfície informada na página corrente. :/}
    procedure DrawPage(APage: TRLGraphicSurface);

    {@method FilterPages - Processa as páginas através do filtro.
     A lista de páginas aPages pode ser obtida na prop Pages de um TRLReport após a preparação do relatório, ou de
     modo avulso criando uma instancia do TRLGraphicStorage e carregando um relatório do disco.
     Os parâmetros aFirstPage e aLastPage são opcionais e indicam o intervalo de páginas a processar.
     @links TRLGraphicStorage, TRLCustomReport.Pages. :}
    procedure FilterPages(APages: TRLGraphicStorage; AFirstPage, ALastPage: Integer; const APageSelection: string; AOddPages: Integer); overload;
    procedure FilterPages(APages: TRLGraphicStorage); overload;
    {/@method}

    // custom properties

    {@prop DisplayName - Retorna o nome para exibição em caixas de seleção. :/}
    property DisplayName: string read GetDisplayName write FDisplayName stored IsDisplayName;

    {@prop Pages - Referência à coleção de páginas a filtrar. :/}
    property Pages: TRLGraphicStorage read FPages write SetPages;

    {@prop ShowProgress - Mostra barra de progresso do salvamento ou impressão. :/}
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;

    {@prop Canceled - Indica se o processo foi interrompido pelo usuário. :/}
    property Canceled: Boolean read FCanceled write FCanceled;
  end;
  {/@class}
  

  { TRLCustomPrintFilter }

  {@class TRLCustomPrintFilter - Classe base para filtros de impressão.
   @links TRLDraftFilter. }
  TRLCustomPrintFilter = class(TRLCustomFilter)
  protected

    // virtual methods

    {@method GetOptionsLabel - Devolve texto para opções de filtragem.
     Reescreva este método nos descendentes para informar se há opções de filtragem e qual é o texto para o diálogo. :/}
    function GetOptionsLabel: string; virtual;

    {@method GetOptions - Devolve lista de opções de filtragem.
     Reescreva este método nos descendentes para informar que opções de filtragem devem aparecer no diálogo. :/}
    function GetOptions: TStrings; virtual;

    {@method GetOptionIndex - Devolve o índice da opção de filtragem atualmente selecionada.
     Reescreva este método nos descendentes para customizar a seleção de opções. :/}
    function GetOptionIndex: Integer; virtual;

    {@method SetOptionIndex - Altera o índice da opção de filtragem.
     Reescreva este método nos descendentes para customizar a seleção de opções. :/}
    procedure SetOptionIndex(const Value: Integer); virtual;

  public

    // custom methods

    {@method SetOrientation - Altera a orientação do papel. :/}
    procedure SetOrientation(AOrientation: TRLPageOrientation); virtual;

    // properties

    {@prop OptionsLabel - Texto para opções de filtragem. :/}
    property OptionsLabel: string read GetOptionsLabel;

    {@prop Options - Lista de opções de filtragem. :/}
    property Options: TStrings read GetOptions;

    {@prop OptionIndex - Opção default ou selecionada pelo diálogo de impressão. :/}
    property OptionIndex: Integer read GetOptionIndex write SetOptionIndex;
  end;
  {/@class}
  

  { TRLCustomSaveFilter }

  {@class TRLCustomSaveFilter - Classe base para filtros de salvamento.
   @links TRLHTMLFilter, TRLPDFFilter, TRLRichFilter. }
  TRLCustomSaveFilter = class(TRLCustomFilter)
  private

    // variables

    FFileName: string;
    FDefaultExt: string;
    
  public

    // constructors & destructors

    constructor Create(AOwner: TComponent); override;

    // properties

    {@prop FileName - Nome do arquivo destino para o filtro de salvamento. :/}
    property FileName: string read FFileName write FFileName;

    {@prop DefaultExt - Extensão padrão para o arquivo destino. :/}
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;
  {/@class}
  
{@var ActiveFilters - Lista de filtros ativos.
 Esta lista contém referências a todos os filtros de impressão e salvamento instanciados.
 @links TRLCustomFilter, SelectedFilter. :/}
var ActiveFilters: TList = nil;

{@var SelectedFilter - Filtro atualmente selecionado.
 Esta variável contém a referência para o filtro atualmente selecionado pelo usuário.
 @links TRLCustomFilter, ActiveFilters. :/}
var SelectedFilter: TRLCustomFilter = nil;

{@func FilterPages - Envia páginas para um filtro de impressão ou salvamento.
 A lista de páginas aPages pode ser obtida na prop Pages de um TRLReport após a preparação do relatório, ou de modo
 avulso criando uma instancia do TRLGraphicStorage e carregando um relatório do disco.
 @links TRLCustomFilter, TRLGraphicStorage, TRLCustomReport.Pages. :}
procedure FilterPages(APages: TRLGraphicStorage; AFilter: TRLCustomFilter; AFirstPage,
  ALastPage: Integer; const APageSelection: string; AOddPages: Integer);
{/@func}

{@func SaveFilterByFileName - Retorna uma referência para um filtro de salvamento instanciado baseado na extensão
 do nome de arquivo informado.
 @links TRLCustomSaveFilter. :/}
function SaveFilterByFileName(const AFileName: string): TRLCustomSaveFilter;

{/@unit}

implementation

uses
  RLSpoolFilter;

function SaveFilterByFileName(const AFileName: string): TRLCustomSaveFilter;
var
  F: TRLCustomSaveFilter;
  e1, e2: string;
  I: Integer;
begin
  Result := nil;
  e1 := FormatFileExt(ExtractFileExt(AFileName));
  for I := 0 to ActiveFilters.Count - 1 do
    if TObject(ActiveFilters[I]) is TRLCustomSaveFilter then
    begin
      F := TRLCustomSaveFilter(ActiveFilters[I]);
      e2 := FormatFileExt(F.DefaultExt);
      if AnsiSameText(e1, e2) then
      begin
        Result := F;
        Break;
      end;
    end;
end;

procedure FilterPages(APages: TRLGraphicStorage; AFilter: TRLCustomFilter; AFirstPage,
  ALastPage: Integer; const APageSelection: string; AOddPages: Integer);
begin
  if not Assigned(AFilter) then
    AFilter := SelectedFilter;
  if not Assigned(AFilter) then
    AFilter := SpoolFilter;
  AFilter.FilterPages(APages, AFirstPage, ALastPage, APageSelection, AOddPages);
end;

{ TRLCustomFilter }

constructor TRLCustomFilter.Create(AOwner: TComponent);
begin
  FDisplayName := '';
  FPages := nil;
  FProgress := nil;
  FShowProgress := True;
  FCanceled := False;
  FClassOptions := [];
  //
  inherited;
  //
  ActiveFilters.Add(Self);
end;

destructor TRLCustomFilter.Destroy;
begin
  if Assigned(ActiveFilters) then
    ActiveFilters.Extract(Self);
  if SelectedFilter = Self then
    SelectedFilter := nil;
  if Assigned(FPages) then
    FPages.Unlink(Self);
  if Assigned(FProgress) then
    FProgress.free;
  //
  inherited;
end;

procedure TRLCustomFilter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  //
  if Operation = opRemove then
    if AComponent = FPages then
      FPages := nil;
end;

procedure TRLCustomFilter.BeginDoc;
begin
  InternalBeginDoc;
end;

procedure TRLCustomFilter.EndDoc;
begin
  InternalEndDoc;
  // libera as páginas
  Pages := nil;
end;

procedure TRLCustomFilter.NewPage;
begin
  InternalNewPage;
end;

procedure TRLCustomFilter.DrawPage(APage: TRLGraphicSurface);
begin
  InternalDrawPage(APage);
end;

function TRLCustomFilter.PageInRange(APageNo: Integer; const APageSelection: string): Boolean;
var
  Ranges, Range: string;
  I, P, RangeStart, RangeEnd: Integer;
begin
  Result := True;
  Ranges := Trim(APageSelection);
  if Ranges = '' then
    Exit;
  while Ranges <> '' do
  begin
    I := Pos(';', Ranges);
    if I = 0 then
      I := Length(Ranges) + 1;
    Range := Copy(Ranges, 1, I - 1);
    Delete(Ranges, 1, I);
    P := Pos('-', Range);
    if P > 0 then
    begin
      RangeStart := StrToIntDef(Trim(Copy(Range, 1, P - 1)), -1);
      RangeEnd := StrToIntDef(Trim(Copy(Range, P + 1, MaxInt)), -1);
    end
    else
    begin
      RangeStart := StrToIntDef(Trim(Range), -1);
      RangeEnd := RangeStart;
    end;
    if (RangeStart <= APageNo) and (RangeEnd >= APageNo) then
      Exit;
  end;
  Result := False;
end;

procedure TRLCustomFilter.FilterPages(APages: TRLGraphicStorage);
begin
  FilterPages(APages, 1, MaxInt, '', 0);
end;

procedure TRLCustomFilter.FilterPages(APages: TRLGraphicStorage; AFirstPage, ALastPage: Integer; const APageSelection: string; AOddPages: Integer);
var
  I, J: Integer;
  copies: Integer;
  page: TRLGraphicSurface;
  jump: Boolean;
begin
  FCanceled := False;
  if (ALastPage = -1) or (ALastPage > APages.PageCount) then
    ALastPage := APages.PageCount;
  if FShowProgress then
    CreateProgress;
  try
    if foEmulateCopies in FClassOptions then
    begin
      copies := RLPrinter.Copies;
      RLPrinter.Copies := 1;  // To avoid double the copies
    end
    else
      copies := 1;
    if FShowProgress then
      FProgress.SetMax((ALastPage - AFirstPage + 1) * copies);
    Self.Pages := APages;
    try
      BeginDoc;
      try
        jump := False;
        for J := 1 to copies do
          for I := AFirstPage to ALastPage do
            if (APageSelection <> '') and not PageInRange(I, APageSelection) then
            else if (AOddPages = PrintOddPagesOnly) and not Odd(I) then
            else if (AOddPages = PrintEvenPagesOnly) and Odd(I) then
            else
            begin
              if jump then
                NewPage;
              jump := True;
              page := APages[I - 1];
              if not Assigned(page) then
                Break;
              DrawPage(page);
              if FShowProgress then
                FProgress.Tick;

              if FCanceled then
                Break;
            end;
      finally
        EndDoc;
      end;
    finally
      Self.Pages := nil;
    end;
  finally
    if FShowProgress then
      DestroyProgress;
  end;
end;

function TRLCustomFilter.GetDisplayName: string;
begin
  if FDisplayName = '' then
    Result := Name
  else
    Result := FDisplayName;
end;

function TRLCustomFilter.GetDisplayLabel: string;
begin
  Result := GetDisplayName;
end;

function TRLCustomFilter.IsDisplayName: Boolean;
begin
  Result := (GetDisplayName <> Name);
end;

procedure TRLCustomFilter.ProgressCanceled(Sender: TObject; var CancelIt: Boolean);
begin
  FCanceled := True;
end;

procedure TRLCustomFilter.CreateProgress;
begin
  FProgress := TfrmRLFeedBack.Create(GetLocalizeStr(LocaleStrings.LS_FilterInProgressStr));
  FProgress.Show;
  FProgress.SetFocus;
  FProgress.OnCancel := ProgressCanceled;
end;

procedure TRLCustomFilter.DestroyProgress;
begin
  FreeObj(FProgress);
end;

procedure TRLCustomFilter.SetPages(const Value: TRLGraphicStorage);
begin
  if Value = FPages then
    Exit;
  if Assigned(FPages) then
    FPages.Unlink(Self);
  FPages := Value;
  if Assigned(FPages) then
    FPages.Link(Self);
end;

procedure TRLCustomFilter.AfterConstruction;
begin
  inherited;
  //
end;

{ TRLCustomSaveFilter }

constructor TRLCustomSaveFilter.Create(AOwner: TComponent);
begin
  FFileName := '';
  FDefaultExt := '';
  //
  inherited;
end;

{ TRLCustomPrintFilter }

function TRLCustomPrintFilter.GetOptionsLabel: string;
begin
  Result := '';
end;

function TRLCustomPrintFilter.GetOptions: TStrings;
begin
  Result := nil;
end;

function TRLCustomPrintFilter.GetOptionIndex: Integer;
begin
  Result := -1;
end;

procedure TRLCustomPrintFilter.SetOptionIndex(const Value: Integer);
begin
end;

procedure TRLCustomPrintFilter.SetOrientation(AOrientation: TRLPageOrientation);
begin
end;

initialization
  // filter instance list
  ActiveFilters := TList.Create;

finalization
  FreeAndNil(ActiveFilters);


end.

