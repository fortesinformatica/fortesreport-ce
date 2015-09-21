{******************************************************************************}
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

unit RLDesign;

interface

uses
  Classes, TypInfo, Db, SysUtils, 
{$ifdef DELPHI5}
  DsgnIntF, 
{$else}
  DesignEditors, DesignIntf, 
{$endif}
{$ifdef VCL}
  Forms, 
{$endif}
{$ifdef CLX}
  QForms, 
{$endif}
  RLReport, RLConsts, RLUtils, RLTypes, 
  RLAbout;

type

{$ifdef DELPHI5}
  IDesignerClass = IFormDesigner;
{$else}
  IDesignerClass = IDesigner;
{$endif}

  { TRLReportDesigner }

  TRLReportDesigner = class(TComponentEditor)
  protected
    FReport: TRLReport;
    procedure ShowAboutBox; virtual;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesignerClass); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TRLListEditor }

  TRLListEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TStrings); virtual; abstract;
  end;

  TRLDataEditor = class(TRLListEditor)
  public
    procedure GetValueList(List: TStrings); override;
    function GetDataSource: TDataSource; virtual; abstract;
  end;

  TRLDataFieldEditor = class(TRLDataEditor)
  public
    function GetDataSource: TDataSource; override;
  end;

  TRLDataFieldsEditor = class(TRLDataEditor)
  public
    function GetDataSource: TDataSource; override;
  end;

  TRLPaperSizeEditor = class(TRLListEditor)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation

{ TRLReportDesigner }

constructor TRLReportDesigner.Create(AComponent: TComponent; ADesigner: IDesignerClass);
begin
  inherited;
  FReport := AComponent as TRLReport;
end;

function TRLReportDesigner.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LocaleStrings.LS_AboutTheStr + ' ' + CS_ProductTitleStr + '...';
    1: Result := '-';
    2: Result := LocaleStrings.LS_PreviewStr;
  end;
end;

function TRLReportDesigner.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TRLReportDesigner.ShowAboutBox;
var
  Form: TFormRLAbout;
begin
  Form := TFormRLAbout.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TRLReportDesigner.Edit;
begin
  FReport.Preview;
  (FReport.Owner as TForm).Invalidate;
end;

procedure TRLReportDesigner.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowAboutBox;
    1: ;
    2: Edit;
  end;
end;

function GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

{ TRLListEditor }

function TRLListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TRLListEditor.GetValues(Proc: TGetStrProc);
var
  ValuesGot: TStringList;
  I: Integer;
begin
  ValuesGot := TStringList.Create;
  try
    GetValueList(ValuesGot);
    for I := 0 to ValuesGot.Count - 1 do
      Proc(ValuesGot[I]);
  finally
    ValuesGot.Free;
  end;
end;

{ TRLDataEditor }

procedure TRLDataEditor.GetValueList(List: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

{ TRLDataFieldEditor }

function TRLDataFieldEditor.GetDataSource: TDataSource;
begin
  Result := GetPropertyValue(GetComponent(0), 'DataSource') as TDataSource;
end;

{ TRLDataFieldsEditor }

function TRLDataFieldsEditor.GetDataSource: TDataSource;
var
  Skipper: TRLCustomSkipper;
begin
  Skipper := TRLGroup(GetComponent(0)).FindParentSkipper;
  if Skipper <> nil then
    Result := Skipper.DataSource
  else
    Result := nil;
end;

{ TRLPaperSizeEditor }

procedure TRLPaperSizeEditor.GetValueList(List: TStrings);
var
  PaperSize: TRLPaperSize;
  PaperName: string;
begin
  for PaperSize := Low(TRLPaperSize) to High(TRLPaperSize) do
  begin
    PaperName := PaperInfo[PaperSize].Description;
    if PaperInfo[PaperSize].Emulated then
      PaperName := PaperName + '*';
    List.Add(PaperName);
  end; 
end;

end.

