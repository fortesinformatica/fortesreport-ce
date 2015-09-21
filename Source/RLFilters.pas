{ Projeto: FortesReport Community Edition                                      }
{ � um poderoso gerador de relat�rios dispon�vel como um pacote de componentes }
{ para Delphi. Em FortesReport, os relat�rios s�o constitu�dos por bandas que  }
{ t�m fun��es espec�ficas no fluxo de impress�o. Voc� definir agrupamentos     }
{ subn�veis e totais simplesmente pela rela��o hier�rquica entre as bandas.    }
{ Al�m disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright � 1999-2015 Fortes Inform�tica     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              M�rcio Martins                                  }
{                              R�gys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informa��es voc� pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descri��o...
******************************************************************************}

{$I RLReport.inc}

{@unit RLFilters - Implementa��o do filtro padr�o de impress�o e classes abstratas para filtros de grava��o e impress�o. }

unit RLFilters;

interface

uses
  Classes, SysUtils, 
{$ifndef LINUX}
  Windows, 
{$else}
  Types, 
{$endif}
{$ifdef VCL}
  Forms, Dialogs, 
{$else}
  QForms, QDialogs, 
{$endif}
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

  {@class TRLCustomFilter - Classe abstrata ancestral de todos os filtro de grava��o e impress�o.
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

    {@method GetDisplayLabel - Devolve nome do filtro para exibi��o em caixas de sele��o. :/}
    function GetDisplayLabel: string; virtual;

    // static methods

    {@method GetDisplayName - Devolve nome do filtro para exibi��o em tempo de design. :/}
    function GetDisplayName: string;

    {@method BeginDoc - Inicializa o processo de filtragem. :/}
    procedure BeginDoc;

    {@method EndDoc - Finaliza o processo de filtragem. :/}
    procedure EndDoc;

    {@method NewPage - Adiciona uma nova p�gina. :/}
    procedure NewPage;

    {@method DrawPage - Desenha o conte�do da superf�cie informada na p�gina corrente. :/}
    procedure DrawPage(APage: TRLGraphicSurface);

    {@method FilterPages - Processa as p�ginas atrav�s do filtro.
     A lista de p�ginas aPages pode ser obtida na prop Pages de um TRLReport ap�s a prepara��o do relat�rio, ou de
     modo avulso criando uma instancia do TRLGraphicStorage e carregando um relat�rio do disco.
     Os par�metros aFirstPage e aLastPage s�o opcionais e indicam o intervalo de p�ginas a processar.
     @links TRLGraphicStorage, TRLCustomReport.Pages. :}
    procedure FilterPages(APages: TRLGraphicStorage; AFirstPage, ALastPage: Integer; const APageSelection: string; AOddPages: Integer); overload;
    procedure FilterPages(APages: TRLGraphicStorage); overload;
    {/@method}

    // custom properties

    {@prop DisplayName - Retorna o nome para exibi��o em caixas de sele��o. :/}
    property DisplayName: string read GetDisplayName write FDisplayName stored IsDisplayName;

    {@prop Pages - Refer�ncia � cole��o de p�ginas a filtrar. :/}
    property Pages: TRLGraphicStorage read FPages write SetPages;

    {@prop ShowProgress - Mostra barra de progresso do salvamento ou impress�o. :/}
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;

    {@prop Canceled - Indica se o processo foi interrompido pelo usu�rio. :/}
    property Canceled: Boolean read FCanceled write FCanceled;
  end;
  {/@class}
  

  { TRLCustomPrintFilter }

  {@class TRLCustomPrintFilter - Classe base para filtros de impress�o.
   @links TRLDraftFilter. }
  TRLCustomPrintFilter = class(TRLCustomFilter)
  protected

    // virtual methods

    {@method GetOptionsLabel - Devolve texto para op��es de filtragem.
     Reescreva este m�todo nos descendentes para informar se h� op��es de filtragem e qual � o texto para o di�logo. :/}
    function GetOptionsLabel: string; virtual;

    {@method GetOptions - Devolve lista de op��es de filtragem.
     Reescreva este m�todo nos descendentes para informar que op��es de filtragem devem aparecer no di�logo. :/}
    function GetOptions: TStrings; virtual;

    {@method GetOptionIndex - Devolve o �ndice da op��o de filtragem atualmente selecionada.
     Reescreva este m�todo nos descendentes para customizar a sele��o de op��es. :/}
    function GetOptionIndex: Integer; virtual;

    {@method SetOptionIndex - Altera o �ndice da op��o de filtragem.
     Reescreva este m�todo nos descendentes para customizar a sele��o de op��es. :/}
    procedure SetOptionIndex(const Value: Integer); virtual;

  public

    // custom methods

    {@method SetOrientation - Altera a orienta��o do papel. :/}
    procedure SetOrientation(AOrientation: TRLPageOrientation); virtual;

    // properties

    {@prop OptionsLabel - Texto para op��es de filtragem. :/}
    property OptionsLabel: string read GetOptionsLabel;

    {@prop Options - Lista de op��es de filtragem. :/}
    property Options: TStrings read GetOptions;

    {@prop OptionIndex - Op��o default ou selecionada pelo di�logo de impress�o. :/}
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

    {@prop DefaultExt - Extens�o padr�o para o arquivo destino. :/}
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;
  {/@class}
  
{@var ActiveFilters - Lista de filtros ativos.
 Esta lista cont�m refer�ncias a todos os filtros de impress�o e salvamento instanciados.
 @links TRLCustomFilter, SelectedFilter. :/}
var ActiveFilters: TList = nil;

{@var SelectedFilter - Filtro atualmente selecionado.
 Esta vari�vel cont�m a refer�ncia para o filtro atualmente selecionado pelo usu�rio.
 @links TRLCustomFilter, ActiveFilters. :/}
var SelectedFilter: TRLCustomFilter = nil;

{@func FilterPages - Envia p�ginas para um filtro de impress�o ou salvamento.
 A lista de p�ginas aPages pode ser obtida na prop Pages de um TRLReport ap�s a prepara��o do relat�rio, ou de modo
 avulso criando uma instancia do TRLGraphicStorage e carregando um relat�rio do disco.
 @links TRLCustomFilter, TRLGraphicStorage, TRLCustomReport.Pages. :}
procedure FilterPages(APages: TRLGraphicStorage; AFilter: TRLCustomFilter; AFirstPage,
  ALastPage: Integer; const APageSelection: string; AOddPages: Integer);
{/@func}

{@func SaveFilterByFileName - Retorna uma refer�ncia para um filtro de salvamento instanciado baseado na extens�o
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
  // libera as p�ginas
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
      copies := RLPrinter.Copies
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
  FProgress := TfrmRLFeedBack.Create(LocaleStrings.LS_FilterInProgressStr);
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
  ActiveFilters.free;

end.

