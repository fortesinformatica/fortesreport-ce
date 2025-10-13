{******************************************************************************}
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

unit RLDesign;

interface

uses
  Classes, TypInfo, Db, SysUtils,
  {$IfDef FPC}
   PropEdits, ComponentEditors, LCLType, LResources,
  {$Else}
   {$IfDef DELPHI5}
    DsgnIntF,
   {$Else}
    DesignEditors, DesignIntf,
   {$EndIf}
  {$EndIf}
  {$IfDef CLX}
   QForms,
  {$Else}
   Forms,
  {$EndIf}
  RLReport, RLConsts, RLUtils, RLTypes, RLAbout;

type

{$IfDef FPC}
  IDesignerClass = TComponentEditorDesigner;
{$Else}
 {$IfDef DELPHI5}
  IDesignerClass = IFormDesigner;
 {$Else}
  IDesignerClass = IDesigner;
 {$EndIf}
{$EndIf}

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
    0: Result :=  GetLocalizeStr(LocaleStrings.LS_AboutTheStr + ' ' + CS_ProductTitleStr + '...');
    1: Result := '-';
    2: Result :=  GetLocalizeStr(LocaleStrings.LS_PreviewStr);
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

