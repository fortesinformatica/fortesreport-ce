//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXValue;

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  DSRestTypes, Classes, SysUtils, DBXFPCJSON;

type

  { TDBXValue }

  TDBXValue = class
  private
    FCurrentDBXType: TDBXDataTypes;
    FisSimpleValueType: boolean;
    function GetDBXType: TDBXDataTypes;
  protected
    FobjectValue: TObject;
    procedure SetDBXType(const Value: TDBXDataTypes);
    function GetAsInt8: Int8; virtual;
    procedure SetAsInt8(const Value: Int8); virtual;
    function GetAsInt16: Int16; virtual;
    procedure SetAsInt16(const Value: Int16); virtual;
    function GetAsInt32: Int32; virtual;
    procedure SetAsInt32(const Value: Int32); virtual;
    function GetAsInt64: int64; virtual;
    procedure SetAsInt64(const Value: int64); virtual;
    function GetAsBoolean: boolean; virtual;
    procedure SetAsBoolean(const Value: boolean); virtual;

    function GetAsUInt8: UInt8; virtual;
    procedure SetAsUInt8(const Value: UInt8); virtual;
    function GetAsUInt16: UInt16; virtual;
    procedure SetAsUInt16(const Value: UInt16); virtual;
    function GetAsUInt32: UInt32; virtual;
    procedure SetAsUInt32(const Value: UInt32); virtual;
    function GetAsUInt64: UInt64; virtual;
    procedure SetAsUInt64(const Value: UInt64); virtual;

    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
    function GetAsAnsiString: ansistring; virtual;
    procedure SetAsAnsiString(const Value: ansistring); virtual;
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;

    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
    function GetAsTimeStamp: TDateTime; virtual;
    procedure SetAsTimeStamp(const Value: TDateTime); virtual;
    function GetAsSingle: single; virtual;
    procedure SetAsSingle(const Value: single); virtual;

    function GetAsStream: TStream; virtual;
    function GetAsTDBXDate: longint; virtual;
    function GetAsTDBXTime: longint; virtual;
    procedure SetAsStream(const Value: TStream); virtual;
    procedure SetAsTDBXDate(const Value: integer); virtual;
    procedure SetAsTDBXTime(const Value: integer); virtual;
    function GetAsCurrency: currency; virtual;
    procedure SetAsCurrency(const Value: currency); virtual;
    function GetASDBXValue: TDBXValue; virtual;
    procedure SetAsDBXValue(const Value: TDBXValue); virtual;
    function GetAsBcd: double; virtual;
    procedure SetAsBcd(const Value: double); virtual;
    function GetAsDouble: double; virtual;
    procedure SetAsDouble(const Value: double); virtual;
    function GetAsJSONValue: TJSONValue; virtual;
    procedure SetAsJSONValue(const Value: TJSONValue); virtual;
    function GetAsBlob: TStream; virtual;
    procedure SetAsBlob(const Value: TStream); virtual;
    function GetAsTable: TObject; virtual;
    procedure SetAsTable(const Value: TObject); virtual;

    function checkCurrentDBXType(Value: TDBXDataTypes): boolean;
    function containsASimpleValueType: boolean;
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    function IsNull: boolean;
    procedure SetNull; virtual;
    function ToString: string;override;
    procedure SetTDBXNull(TypeName: String); virtual;
    procedure AppendTo(json: TJSONArray);

    property DBXType: TDBXDataTypes read GetDBXType write SetDBXType;

    property AsInt8: Int8 read GetAsInt8 write SetAsInt8;
    property AsInt16: Int16 read GetAsInt16 write SetAsInt16;
    property AsInt32: Int32 read GetAsInt32 write SetAsInt32;
    property AsInt64: int64 read GetAsInt64 write SetAsInt64;

    property AsUInt8: UInt8 read GetAsUInt8 write SetAsUInt8;
    property AsUInt16: UInt16 read GetAsUInt16 write SetAsUInt16;
    property AsUInt32: UInt32 read GetAsUInt32 write SetAsUInt32;
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;

    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: ansistring read GetAsAnsiString
      write SetAsAnsiString;
    property AsWideString: WideString read GetAsWideString
      write SetAsWideString;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsTimeStamp: TDateTime read GetAsTimeStamp write SetAsTimeStamp;

    property AsSingle: single read GetAsSingle write SetAsSingle;

    property AsCurrency: currency read GetAsCurrency write SetAsCurrency;

    property AsTDBXDate: integer read GetASTDBXDate write SetAsTDBXDate;
    property AsTDBXTime: integer read GetASTDBXTime write SetAsTDBXTime;
    property AsDBXValue: TDBXValue read GetASDBXValue write SetAsDBXValue;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsBcd: double read GetAsBcd write SetAsBcd;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsJsonValue: TJSONValue read GetAsJSONValue write SetAsJSONValue;
    property AsBlob: TStream read GetAsBlob write SetAsBlob;
    property AsTable: TObject read GetAsTable write SetAsTable;

  end;

  TDBXValueType = class
  private
    FChildPosition: integer;
    FHidden: boolean;
    FLiteral: boolean;
    FName: string;
    FCaption: string;
    FNullable: boolean;
    FOrdinal: integer;
    FParameterDirection: integer;
    FPrecision: longint;
    FScale: integer;
    FSize: longint;
    FSubType: integer;
    FValueParameter: boolean;
    function GetCaption: string;
    function GetChildPosition: integer;
    function GetHidden: boolean;
    function GetLiteral: boolean;
    function GetName: string;
    function GetNullable: boolean;
    function GetOrdinal: integer;
    function GetParameterDirection: integer;
    function GetPrecision: longint;
    function GetScale: integer;
    function GetSize: longint;
    function GetSubType: integer;
    function GetValueParameter: boolean;
    procedure SetChildPosition(const AValue: integer);
    procedure SetHidden(const AValue: boolean);
    procedure SetLiteral(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetCaption(const AValue: string);
    procedure SetNullable(const AValue: boolean);
    procedure SetOrdinal(const AValue: integer);
    procedure SetParameterDirection(const AValue: integer);
    procedure SetPrecision(const AValue: longint);
    procedure SetScale(const AValue: integer);
    procedure SetSize(const AValue: longint);
    procedure SetSubType(const AValue: integer);
    procedure SetValueParameter(const AValue: boolean);
  public
    property Name: string read GetName write SetName;
    property Caption: string read GetCaption write SetCaption;
    property Ordinal: integer read GetOrdinal write SetOrdinal;
    property SubType: integer read GetSubType write SetSubType;
    property Size: longint read GetSize write SetSize;
    property Precision: longint read GetPrecision write SetPrecision;
    property Scale: integer read GetScale write SetScale;
    property ChildPosition: integer read GetChildPosition
      write SetChildPosition;
    property Nullable: boolean read GetNullable write SetNullable;
    property ParameterDirection: integer read GetParameterDirection
      write SetParameterDirection;
    property Hidden: boolean read GetHidden write SetHidden;
    property ValueParameter: boolean read GetValueParameter
      write SetValueParameter;
    property Literal: boolean read GetLiteral write SetLiteral;
    procedure setDataType(dataType: TDBXDataTypes); virtual;
    function getDataType: integer; virtual;
  end;

  { TDBXWritableValue }

  TDBXWritableValue = class(TDBXValue)
  protected
    FBooleanValue: boolean;
    FINT32Value: Int32;
    FUINT32Value: longint;
    FINT64Value: int64;
    FUINT64Value: longint;
    FStringValue: string;
    FSingleValue: double;
    FDoubleValue: double;
    FDateTimeValue: TDateTime;
    FstreamValue: TStream;
    FobjectValue: TObject;
    FJsonValueValue: TJSONValue;
    FDBXValueValue: TDBXValue;
    FBcdValue: double;
    FTimeStampValue: TDateTime;
    procedure throwInvalidValue;
  public
    destructor Destroy; override;
    procedure SetTDBXNull(TypeName: String); override;
    function GetAsInt8: Int8; override;
    procedure SetAsInt8(const Value: Int8); override;
    function GetAsInt32: integer; override;
    procedure SetAsInt32(const Value: Int32); override;
    function GetAsInt16: Int16; override;
    procedure SetAsInt16(const Value: Int16); override;
    function GetAsInt64: int64; override;
    procedure SetAsInt64(const Value: int64); override;
    function GetAsBoolean: boolean; override;
    procedure SetAsBoolean(const Value: boolean); override;

    function GetAsUInt8: UInt8; override;
    procedure SetAsUInt8(const Value: UInt8); override;
    function GetAsUInt16: UInt16; override;
    procedure SetAsUInt16(const Value: UInt16); override;
    function GetAsUInt32: UInt32; override;
    procedure SetAsUInt32(const Value: UInt32); override;
    function GetAsUInt64: UInt64; override;
    procedure SetAsUInt64(const Value: UInt64); override;

    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsAnsiString: ansistring; override;
    procedure SetAsAnsiString(const Value: ansistring); override;
    function GetAsWideString: WideString; override;
    procedure SetAsWideString(const Value: WideString); override;

    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    function GetAsTimeStamp: TDateTime; override;
    procedure SetAsTimeStamp(const Value: TDateTime); override;
    function GetAsSingle: single; override;
    procedure SetAsSingle(const Value: single); override;

    function GetAsStream: TStream; override;
    function GetAsTDBXDate: longint; override;
    function GetAsTDBXTime: longint; override;
    procedure SetAsStream(const Value: TStream); override;
    procedure SetAsTDBXDate(const Value: integer); override;
    procedure SetAsTDBXTime(const Value: integer); override;
    function GetAsCurrency: currency; override;
    procedure SetAsCurrency(const Value: currency); override;
    function GetASDBXValue: TDBXValue; override;
    procedure SetAsDBXValue(const Value: TDBXValue); override;
    function GetAsBcd: double; override;
    procedure SetAsBcd(const Value: double); override;
    function GetAsDouble: double; override;
    procedure SetAsDouble(const Value: double); override;
    function GetAsJSONValue: TJSONValue; override;
    procedure SetAsJSONValue(const Value: TJSONValue); override;
    function GetAsBlob: TStream; override;
    procedure SetAsBlob(const Value: TStream); override;
    function GetAsTable: TObject; override;
    procedure SetAsTable(const Value: TObject); override;

    procedure Clear; override;

  end;

function TypeIsDBXValue(Value: string): boolean;

implementation

{ TDBXValue }

uses DBXDefaultFormatter, DBXJsonTools, DBXFPCCommon, FPCStrings;

function TypeIsDBXValue(Value: string): boolean;
begin
  Result := (pos('TDBX', Value) = 1) and
    (pos('Value', Value) = length(Value) - 4);
end;

procedure TDBXValue.AppendTo(json: TJSONArray);
var
  jvaluec, jvalue: TJSONValue;
begin
  try
    if containsASimpleValueType then
    begin
      if GetASDBXValue.IsNull then
        json.Add(TJSONNull.Create)
      else
        GetASDBXValue.AppendTo(json);

      Exit;
    end;

    case FCurrentDBXType of
      BooleanType:
        json.Add(AsBoolean);
      Int8Type:
        json.Add(AsInt8);
      Int16Type:
        json.Add(AsInt16);
      Int32Type:
        json.Add(AsInt32);
      Int64Type:
        json.Add(AsInt64);
      UInt8Type:
        json.Add(AsUInt8);
      UInt16Type:
        json.Add(AsUInt16);
      UInt32Type:
        json.Add(AsUInt32);
      UInt64Type:
        json.Add(AsUInt64);
      AnsiStringType, WideStringType:
        json.Add(AsString);
      DateTimeType:
        json.Add(TDBXDefaultFormatter.GetInstance.DateTimeToString(AsDateTime));
      TimeStampType:
        json.Add(TDBXDefaultFormatter.GetInstance.DateTimeToString
          (AsTimeStamp));
      DateType:
        json.Add(TDBXDefaultFormatter.GetInstance.DBXDateToString(AsTDBXDate));
      TimeType:
        json.Add(TDBXDefaultFormatter.GetInstance.DBXTimeToString(AsTDBXTime));
      JsonValueType:
        begin
          jvaluec:= nil;
          jvalue := AsJsonValue;
          if assigned(jvalue) then
            jvaluec := jvalue.clone;

          if assigned(json) and assigned(jvaluec) then
            json.Add(jvaluec);
        end;
      TableType:
        json.Add(TDBXJsonTools.SerializeTableType(AsTable));
      CurrencyType:
        json.Add(AsCurrency);
      DoubleType:
        json.Add(AsDouble);
      SingleType:
        json.Add(AsSingle);
      BinaryBlobType:
        json.Add(TDBXJsonTools.StreamToJSONArray(AsStream));
      BlobType:
        json.Add(TDBXJsonTools.StreamToJSONArray(AsBlob));
    else
      raise DBXException.Create('Cannot convert this type to string');
    end;

  except
    on e: DBXException do
      Exit;
  end;
end;

function TDBXValue.checkCurrentDBXType(Value: TDBXDataTypes): boolean;
begin
  result:= false;
  if FCurrentDBXType <> Value then
    raise DBXException.Create('Incorrect type in DBXValue');

end;

procedure TDBXValue.Clear;
begin
  SetDBXType(UnknownType);
end;

function TDBXValue.containsASimpleValueType: boolean;
begin
  Result := FisSimpleValueType;
end;

constructor TDBXValue.Create;
begin
  inherited Create;
  DBXType := UnknownType;
end;

function TDBXValue.GetAsAnsiString: ansistring;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsBcd: double;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsBoolean: boolean;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsCurrency: currency;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsDateTime: TDateTime;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetASDBXValue: TDBXValue;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsDouble: double;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsInt16: Int16;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsInt32: Int32;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsInt64: int64;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsInt8: Int8;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsSingle: single;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsStream: TStream;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsString: string;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetASTDBXDate: longint;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetASTDBXTime: longint;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsTimeStamp: TDateTime;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsUInt16: UInt16;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsUInt32: UInt32;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsUInt64: UInt64;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsUInt8: UInt8;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsWideString: WideString;
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetDBXType: TDBXDataTypes;
begin
  Result := FCurrentDBXType;
end;

function TDBXValue.GetAsTable: TObject;
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsTable(const Value: TObject);
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsBlob: TStream;
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsBlob(const Value: TStream);
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.GetAsJSONValue: TJSONValue;
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsJSONValue(const Value: TJSONValue);
begin
  raise DBXException.Create('Invalid Type Access');
end;

function TDBXValue.IsNull: boolean;
begin
  Result := FCurrentDBXType = UnknownType;
end;

procedure TDBXValue.SetNull;
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsAnsiString(const Value: ansistring);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsBcd(const Value: double);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsBoolean(const Value: boolean);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsCurrency(const Value: currency);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsDateTime(const Value: TDateTime);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsDBXValue(const Value: TDBXValue);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsDouble(const Value: double);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsInt16(const Value: Int16);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsInt32(const Value: Int32);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsInt64(const Value: int64);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsInt8(const Value: Int8);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsSingle(const Value: single);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsStream(const Value: TStream);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsTDBXDate(const Value: integer);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsTDBXTime(const Value: integer);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsString(const Value: string);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsTimeStamp(const Value: TDateTime);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsUInt16(const Value: UInt16);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsUInt32(const Value: UInt32);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsUInt64(const Value: UInt64);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsUInt8(const Value: UInt8);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetAsWideString(const Value: WideString);
begin
  raise DBXException.Create('Invalid Type Access');
end;

procedure TDBXValue.SetDBXType(const Value: TDBXDataTypes);
begin
  FCurrentDBXType := Value;
  FisSimpleValueType := False;
end;

function TDBXValue.ToString: string;
begin
  Result := '<CANNOT CONVERT TO STRING>';
  if containsASimpleValueType then
  begin
    if GetASDBXValue.IsNull then
    begin
      with TJSONNull.Create do
      begin
        Result := asJSONString;
        Free;
      end;
    end
    else
      Result := GetASDBXValue.ToString;
  end
  else
  begin
    case FCurrentDBXType of
      Int8Type:
        Result := TDBXDefaultFormatter.GetInstance.Int8ToString(GetAsInt8);
      Int16Type:
        Result := TDBXDefaultFormatter.GetInstance.Int16ToString(GetAsInt16);
      Int32Type:
        Result := TDBXDefaultFormatter.GetInstance.Int32ToString(GetAsInt32);
      Int64Type:
        Result := TDBXDefaultFormatter.GetInstance.Int64ToString(GetAsInt64);
      UInt8Type:
        Result := TDBXDefaultFormatter.GetInstance.UInt8ToString(GetAsUInt8);
      UInt16Type:
        Result := TDBXDefaultFormatter.GetInstance.UInt16ToString(GetAsUInt16);
      UInt32Type:
        Result := TDBXDefaultFormatter.GetInstance.UInt32ToString(GetAsUInt32);
      UInt64Type:
        Result := TDBXDefaultFormatter.GetInstance.UInt64ToString(GetAsUInt64);
      AnsiStringType:
        Result := TDBXDefaultFormatter.GetInstance.AnsiStringToString
          (GetAsAnsiString);
      WideStringType:
        Result := TDBXDefaultFormatter.GetInstance.WideStringToString
          (GetAsString);
      DateTimeType:
        Result := TDBXDefaultFormatter.GetInstance.DateTimeToString
          (GetAsDateTime);
      CurrencyType:
        Result := TDBXDefaultFormatter.GetInstance.CurrencyToString
          (GetAsCurrency);
      DoubleType:
        Result := TDBXDefaultFormatter.GetInstance.DoubleToString(GetAsDouble);
      SingleType:
        Result := TDBXDefaultFormatter.GetInstance.SingleToString(GetAsSingle);
      DateType:
        Result := TDBXDefaultFormatter.GetInstance.DBXDateToString
          (GetASTDBXDate);
      TimeType:
        Result := TDBXDefaultFormatter.GetInstance.DBXTimeToString
          (GetASTDBXTime);
      TimeStampType:
        Result := TDBXDefaultFormatter.GetInstance.DateTimeToString
          (GetAsTimeStamp);
      BooleanType:
        Result := TDBXDefaultFormatter.GetInstance.BooleanToString
          (GetAsBoolean);
      BcdType:
        Result := TDBXDefaultFormatter.GetInstance.DoubleToString(GetAsBcd);
    else
      DBXException.Create('Cannot convert this type to string');

    end;

  end;

end;

procedure TDBXValue.SetTDBXNull(TypeName: String);
begin
  raise DBXException.Create('Invalid Type Access');
end;

{ TDBXWritableValue }

procedure TDBXWritableValue.Clear;
begin
  FBooleanValue := False;
  FINT32Value := 0;
  FUINT32Value := 0;
  FINT64Value := 0;
  FUINT64Value := 0;
  FStringValue := '';
  FSingleValue := 0;
  FDoubleValue := 0;
  FDateTimeValue := 0;
  if assigned(FstreamValue) then
    FreeAndNil(FstreamValue);
  if assigned(FobjectValue) then
    FreeAndNil(FobjectValue);
  if assigned(FJsonValueValue) then
    FreeAndNil(FJsonValueValue);
  if assigned(FDBXValueValue) then
    FreeAndNil(FDBXValueValue);
  FBcdValue := 0;
  FTimeStampValue := 0;
end;

function TDBXWritableValue.GetAsAnsiString: ansistring;
begin
  try
{$WARNINGS OFF}
    Result := GetAsString;
{$WARNINGS ON}
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsBcd: double;
begin
  try
    checkCurrentDBXType(BcdType);
    Result := FBcdValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsBoolean: boolean;
begin
  try
    checkCurrentDBXType(BooleanType);
    Result := FBooleanValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsCurrency: currency;
begin
  try
    checkCurrentDBXType(CurrencyType);
    Result := FDoubleValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsDateTime: TDateTime;
begin
  try
    checkCurrentDBXType(DateTimeType);
    Result := FDateTimeValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetASDBXValue: TDBXValue;
begin
  if not FisSimpleValueType then
    raise DBXException.Create('Invalid DBX type');
  Result := FDBXValueValue;

end;

function TDBXWritableValue.GetAsDouble: double;
begin
  try
    checkCurrentDBXType(DoubleType);
    Result := FDoubleValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsInt16: Int16;
begin
  try
    checkCurrentDBXType(Int16Type);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsInt32: integer;
begin
  try
    checkCurrentDBXType(Int32Type);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsInt64: int64;
begin
  try
    checkCurrentDBXType(Int64Type);
    Result := FINT64Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsInt8: Int8;
begin
  try
    checkCurrentDBXType(Int8Type);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsSingle: single;
begin
  try
    checkCurrentDBXType(SingleType);
    Result := FSingleValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsStream: TStream;
begin
  try
    checkCurrentDBXType(BinaryBlobType);
    FstreamValue.Position := 0;
    Result := FstreamValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsString: string;
begin
  try
    checkCurrentDBXType(WideStringType);
    Result := FStringValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetASTDBXDate: longint;
begin
  try
    checkCurrentDBXType(DateType);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetASTDBXTime: longint;
begin
  try
    checkCurrentDBXType(TimeType);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsTimeStamp: TDateTime;
begin
  try
    checkCurrentDBXType(TimeStampType);
    Result := FTimeStampValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsUInt16: UInt16;
begin
  try
    checkCurrentDBXType(UInt16Type);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsUInt32: UInt32;
begin
  try
    checkCurrentDBXType(UInt32Type);
    Result := FUINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXWritableValue.GetAsUInt64: UInt64;
begin
  try
    checkCurrentDBXType(UInt64Type);
    Result := FUINT64Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsUInt8: UInt8;
begin
  try
    checkCurrentDBXType(UInt8Type);
    Result := FINT32Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

function TDBXWritableValue.GetAsWideString: WideString;
begin
  try
    checkCurrentDBXType(WideStringType);
    Result := FStringValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

procedure TDBXWritableValue.SetAsAnsiString(const Value: ansistring);
begin
  SetAsString(String(Value));
end;

procedure TDBXWritableValue.SetAsBcd(const Value: double);
begin
  try
    FBcdValue := Value;
    DBXType := BcdType;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsBoolean(const Value: boolean);
begin
  try
    FBooleanValue := Value;
    DBXType := BooleanType;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsCurrency(const Value: currency);
begin
  try
    FDoubleValue := Value;
    DBXType := CurrencyType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsDateTime(const Value: TDateTime);
begin
  try
    FDateTimeValue := Value;
    DBXType := DateTimeType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsDBXValue(const Value: TDBXValue);
begin
  try
    SetDBXType(Value.GetDBXType);
    FisSimpleValueType := True;
    FDBXValueValue := Value;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsDouble(const Value: double);
begin
  try
    FDoubleValue := Value;
    DBXType := DoubleType;
  except
    throwInvalidValue;
  end;

end;

function TDBXWritableValue.GetAsJSONValue: TJSONValue;
begin
  try
    checkCurrentDBXType(JsonValueType);
    Result := FJsonValueValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

procedure TDBXWritableValue.SetAsJSONValue(const Value: TJSONValue);
begin
  try
    Clear();
    FJsonValueValue := Value;
    DBXType := JsonValueType;
  except
    throwInvalidValue;
  end;
end;

function TDBXWritableValue.GetAsBlob: TStream;
begin
  try
    checkCurrentDBXType(BlobType);
    Result := TStream(FobjectValue);
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

procedure TDBXWritableValue.SetAsBlob(const Value: TStream);
begin
  try
    Clear();
    FobjectValue := Value;
    DBXType := BlobType;
  except
    throwInvalidValue;
  end;

end;

function TDBXWritableValue.GetAsTable: TObject;
begin
  try
    checkCurrentDBXType(TableType);
    Result := FobjectValue;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

procedure TDBXWritableValue.SetAsTable(const Value: TObject);
begin
  try
    Clear();
    DBXType := TableType;
    FobjectValue := Value;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsInt16(const Value: Int16);
begin
  try
    FINT32Value := Value;
    DBXType := Int16Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsInt32(const Value: Int32);
begin
  try
    FINT32Value := Value;
    DBXType := Int32Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsInt64(const Value: int64);
begin
  try
    FINT64Value := Value;
    DBXType := Int64Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsInt8(const Value: Int8);
begin
  try
    FINT32Value := Value;
    DBXType := Int8Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsSingle(const Value: single);
begin
  try
    FSingleValue := Value;
    DBXType := SingleType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsStream(const Value: TStream);
begin
  try
    Clear;
    Value.Position := 0;
    FstreamValue := TMemoryStream.Create;
    FstreamValue.Position := 0;
    FstreamValue.CopyFrom(Value, Value.Size);
    FstreamValue.Position := 0;
    DBXType := BinaryBlobType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsString(const Value: string);
begin
  try
    FStringValue := Value;
    DBXType := WideStringType;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsTDBXDate(const Value: integer);
begin
  try
    FINT32Value := Value;
    DBXType := DateType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsTDBXTime(const Value: integer);
begin
  try
    FINT32Value := Value;
    DBXType := TimeType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsTimeStamp(const Value: TDateTime);
begin
  try
    FTimeStampValue := Value;
    DBXType := TimeStampType;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsUInt16(const Value: UInt16);
begin
  try
    FINT32Value := Value;
    DBXType := UInt16Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsUInt32(const Value: UInt32);
begin
  try
    FUINT32Value := Value;
    DBXType := UInt32Type;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsUInt64(const Value: UInt64);
begin
  try
    FUINT64Value := Value;
    DBXType := UInt64Type;
  except
    throwInvalidValue;
  end;

end;

procedure TDBXWritableValue.SetAsUInt8(const Value: UInt8);
begin
  try
    FINT32Value := Value;
    DBXType := UInt8Type;
  except
    throwInvalidValue;
  end;
end;

procedure TDBXWritableValue.SetAsWideString(const Value: WideString);
begin
  SetAsString(Value);
end;

procedure TDBXWritableValue.throwInvalidValue;
begin
  raise DBXException.Create('Invalid value for param');
end;

destructor TDBXWritableValue.Destroy;
begin
  // FreeAndNil(FstreamValue);
  // FreeAndNil(FobjectValue);
  // FreeAndNil(FJsonValueValue);
  // FreeAndNil(FDBXValueValue);
  inherited Destroy;
end;

procedure TDBXWritableValue.SetTDBXNull(TypeName: String);
var
  v: TDBXValue;
begin
  v:= nil;
  try
    if TypeName = 'TDBXStringValue' then
    begin
      v := TDBXStringValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXAnsiCharsValue' then
    begin
      v := TDBXAnsiCharsValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXAnsiStringValue' then
    begin
      v := TDBXAnsiStringValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXWideStringValue' then
    begin
      v := TDBXWideStringValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXSingleValue' then
    begin
      v := TDBXSingleValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXDateValue' then
    begin
      v := TDBXDateValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXTimeValue' then
    begin
      v := TDBXTimeValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXBooleanValue' then
    begin
      v := TDBXBooleanValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXDoubleValue' then
    begin
      v := TDBXDoubleValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXInt64Value' then
    begin
      v := TDBXInt64Value.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXInt32Value' then
    begin
      v := TDBXInt32Value.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXInt16Value' then
    begin
      v := TDBXInt16Value.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXInt8Value' then
    begin
      v := TDBXInt8Value.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXStreamValue' then
    begin
      v := TDBXStreamValue.Create;
      v.SetNull;
    end
    else if TypeName = 'TDBXReaderValue' then
    begin
      v := TDBXReaderValue.Create;
      v.SetNull;
    end;
    if assigned(v) then
      AsDBXValue := v;

  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

{ TDBXValueType }

procedure TDBXValueType.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TDBXValueType.SetChildPosition(const AValue: integer);
begin
  if FChildPosition = AValue then
    Exit;
  FChildPosition := AValue;
end;

function TDBXValueType.GetCaption: string;
begin
  Result := FCaption;
end;

function TDBXValueType.GetChildPosition: integer;
begin
  Result := FChildPosition;
end;

function TDBXValueType.GetHidden: boolean;
begin
  Result := FHidden;
end;

function TDBXValueType.GetLiteral: boolean;
begin
  Result := FLiteral;
end;

function TDBXValueType.GetName: string;
begin
  Result := FName;
end;

function TDBXValueType.GetNullable: boolean;
begin
  Result := FNullable;
end;

function TDBXValueType.GetOrdinal: integer;
begin
  Result := FOrdinal;
end;

function TDBXValueType.GetParameterDirection: integer;
begin
  Result := FParameterDirection;
end;

function TDBXValueType.GetPrecision: longint;
begin
  Result := FPrecision;
end;

function TDBXValueType.GetScale: integer;
begin
  Result := FScale;
end;

function TDBXValueType.GetSize: longint;
begin
  Result := FSize;
end;

function TDBXValueType.GetSubType: integer;
begin
  Result := FSubType;
end;

function TDBXValueType.GetValueParameter: boolean;
begin
  Result := FValueParameter;
end;

procedure TDBXValueType.SetHidden(const AValue: boolean);
begin
  if FHidden = AValue then
    Exit;
  FHidden := AValue;
end;

procedure TDBXValueType.SetLiteral(const AValue: boolean);
begin
  if FLiteral = AValue then
    Exit;
  FLiteral := AValue;
end;

procedure TDBXValueType.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
end;

procedure TDBXValueType.SetNullable(const AValue: boolean);
begin
  if FNullable = AValue then
    Exit;
  FNullable := AValue;
end;

procedure TDBXValueType.SetOrdinal(const AValue: integer);
begin
  if FOrdinal = AValue then
    Exit;
  FOrdinal := AValue;
end;

procedure TDBXValueType.SetParameterDirection(const AValue: integer);
begin
  if FParameterDirection = AValue then
    Exit;
  FParameterDirection := AValue;
end;

procedure TDBXValueType.SetPrecision(const AValue: longint);
begin
  if FPrecision = AValue then
    Exit;
  FPrecision := AValue;
end;

procedure TDBXValueType.SetScale(const AValue: integer);
begin
  if FScale = AValue then
    Exit;
  FScale := AValue;
end;

procedure TDBXValueType.SetSize(const AValue: longint);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
end;

procedure TDBXValueType.SetSubType(const AValue: integer);
begin
  if FSubType = AValue then
    Exit;
  FSubType := AValue;
end;

procedure TDBXValueType.SetValueParameter(const AValue: boolean);
begin
  if FValueParameter = AValue then
    Exit;
  FValueParameter := AValue;
end;

procedure TDBXValueType.setDataType(dataType: TDBXDataTypes);
begin
  raise DBXException.Create('Must be overridden in the descendant classes');
end;

function TDBXValueType.getDataType: integer;
begin
  raise DBXException.Create('Must be overridden in the descendant classes');
end;

end.
