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

