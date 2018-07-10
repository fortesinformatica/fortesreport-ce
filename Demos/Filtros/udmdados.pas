unit udmdados;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  Tdmdados = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDQuery1EmployeeId: TFDAutoIncField;
    FDQuery1LastName: TWideStringField;
    FDQuery1FirstName: TWideStringField;
    FDQuery1Title: TWideStringField;
    FDQuery1ReportsTo: TIntegerField;
    FDQuery1BirthDate: TDateTimeField;
    FDQuery1HireDate: TDateTimeField;
    FDQuery1Address: TWideStringField;
    FDQuery1City: TWideStringField;
    FDQuery1State: TWideStringField;
    FDQuery1Country: TWideStringField;
    FDQuery1PostalCode: TWideStringField;
    FDQuery1Phone: TWideStringField;
    FDQuery1Fax: TWideStringField;
    FDQuery1Email: TWideStringField;
    procedure FDConnection1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmdados: Tdmdados;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure Tdmdados.FDConnection1BeforeConnect(Sender: TObject);
begin
  FDConnection1.Params.Database := '..\..\..\Dados\chinook.db';
end;

end.
