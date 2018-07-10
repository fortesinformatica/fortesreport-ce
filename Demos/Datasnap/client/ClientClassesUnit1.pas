// 
// Created by the DataSnap proxy generator.
// 15/03/2018 03:22:33
// 

unit ClientClassesUnit1;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
    FRelatorioEmpregadosCommand: TDSRestCommand;
    FRelatorioEmpregadosCommand_Cache: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
    function RelatorioEmpregados(out size: Int64; const ARequestFilter: string = ''): TStream;
    function RelatorioEmpregados_Cache(out size: Int64; const ARequestFilter: string = ''): IDSRestCachedStream;
  end;

const
  TServerMethods1_EchoString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ReverseString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_RelatorioEmpregados: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'size'; Direction: 2; DBXType: 18; TypeName: 'Int64'),
    (Name: ''; Direction: 4; DBXType: 33; TypeName: 'TStream')
  );

  TServerMethods1_RelatorioEmpregados_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'size'; Direction: 2; DBXType: 18; TypeName: 'Int64'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

implementation

function TServerMethods1Client.EchoString(Value: string; const ARequestFilter: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FConnection.CreateCommand;
    FEchoStringCommand.RequestType := 'GET';
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare(TServerMethods1_EchoString);
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.Execute(ARequestFilter);
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string; const ARequestFilter: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FConnection.CreateCommand;
    FReverseStringCommand.RequestType := 'GET';
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare(TServerMethods1_ReverseString);
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.Execute(ARequestFilter);
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.RelatorioEmpregados(out size: Int64; const ARequestFilter: string): TStream;
begin
  if FRelatorioEmpregadosCommand = nil then
  begin
    FRelatorioEmpregadosCommand := FConnection.CreateCommand;
    FRelatorioEmpregadosCommand.RequestType := 'GET';
    FRelatorioEmpregadosCommand.Text := 'TServerMethods1.RelatorioEmpregados';
    FRelatorioEmpregadosCommand.Prepare(TServerMethods1_RelatorioEmpregados);
  end;
  FRelatorioEmpregadosCommand.Execute(ARequestFilter);
  size := FRelatorioEmpregadosCommand.Parameters[0].Value.GetInt64;
  Result := FRelatorioEmpregadosCommand.Parameters[1].Value.GetStream(FInstanceOwner);
end;

function TServerMethods1Client.RelatorioEmpregados_Cache(out size: Int64; const ARequestFilter: string): IDSRestCachedStream;
begin
  if FRelatorioEmpregadosCommand_Cache = nil then
  begin
    FRelatorioEmpregadosCommand_Cache := FConnection.CreateCommand;
    FRelatorioEmpregadosCommand_Cache.RequestType := 'GET';
    FRelatorioEmpregadosCommand_Cache.Text := 'TServerMethods1.RelatorioEmpregados';
    FRelatorioEmpregadosCommand_Cache.Prepare(TServerMethods1_RelatorioEmpregados_Cache);
  end;
  FRelatorioEmpregadosCommand_Cache.ExecuteCache(ARequestFilter);
  size := FRelatorioEmpregadosCommand_Cache.Parameters[0].Value.GetInt64;
  Result := TDSRestCachedStream.Create(FRelatorioEmpregadosCommand_Cache.Parameters[1].Value.GetString);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TServerMethods1Client.Destroy;
begin
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  FRelatorioEmpregadosCommand.DisposeOf;
  FRelatorioEmpregadosCommand_Cache.DisposeOf;
  inherited;
end;

end.
