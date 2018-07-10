//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXFPCCommon;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses DBXValue, DSRestTypes, Classes, DBXFPCJSON, Contnrs;

type
  TDBXUInt8Value = class(TDBXValue)
  private
    FValueNull: boolean;
    FDBXUInt8Value: UInt8;
  protected
    function GetAsUInt8: UInt8; override;
    procedure SetAsUInt8(const Value: UInt8); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;

  end;

  TDBXUInt16Value = class(TDBXValue)
  private
    FValueNull: boolean;
    FDBXUInt16Value: UInt16;
  protected
    function GetAsUInt16: UInt16; override;
    procedure SetAsUInt16(const Value: UInt16); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;


  end;

  TDBXInt8Value = class(TDBXValue)
  private
    ValueNull: boolean;
    DBXInt8Value: Int8;
  protected
    function GetAsInt8: Int8; override;
    procedure SetAsInt8(const Value: Int8); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  { TDBXInt16Value }

  TDBXInt16Value = class(TDBXValue)
  private
    DBXInt16Value: Int16;
    ValueNull: boolean;
  protected
    function GetAsInt16: Int16; override;
    procedure SetAsInt16(const Value: Int16); override;
  public
    function isNull: boolean;
    procedure SetNull;override;
    constructor Create; override;
  end;

  TDBXInt32Value = class(TDBXValue)
  private
    DBXInt32Value: Int32;
    ValueNull: boolean;
  protected
    function GetAsInt32: Int32; override;
    procedure SetAsInt32(const Value: Int32); override;
  public
    function isNull: boolean;
    procedure SetNull(const Value: boolean);reintroduce;
    constructor Create; override;
  end;

  TDBXInt64Value = class(TDBXValue)
  private
    DBXInt64Value: int64;
    ValueNull: boolean;
  protected
    function GetAsInt64: int64; override;
    procedure SetAsInt64(const Value: int64); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXDoubleValue = class(TDBXValue)
  private
    DBXDoubleValue: double;
    ValueNull: boolean;
  protected
    function GetAsDouble: double; override;
    procedure SetAsDouble(const Value: double); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXSingleValue = class(TDBXValue)
  private
    DBXSingleValue: single;
    ValueNull: boolean;
  protected
    function GetAsSingle: single; override;
    procedure SetAsSingle(const Value: single); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXBcdValue = class(TDBXValue)
  private
    ValueNull: boolean;
    DBXBcdValue: double;
  protected
    function GetAsBcd: double; override;
    procedure SetAsBcd(const Value: double); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXBooleanValue = class(TDBXValue)
  private
    DBXBooleanValue: boolean;
    ValueNull: boolean;
  protected
    function GetAsBoolean: boolean; override;
    procedure SetAsBoolean(const Value: boolean); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXAnsiStringValue = class(TDBXValue)
  private
    DBXAnsiStringValue: AnsiString;
    ValueNull: boolean;
  protected

    function GetAsAnsiString: AnsiString; override;
    procedure SetAsAnsiString(const Value: AnsiString); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXAnsiCharsValue = class(TDBXValue)
  private
    DBXAnsiCharsValue: string;
    ValueNull: boolean;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXDateValue = class(TDBXValue)
  private
    DBXDateValue: longint;
    ValueNull: boolean;
  protected
    function GetAsTDBXDate: longint; override;
    procedure SetAsTDBXDate(const Value: longint); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXStreamValue = class(TDBXValue)
  private
    ValueNull: boolean;
    FStreamValue:TStream; 
  protected
    function GetAsStream: TStream; override;
    procedure SetAsStream(const Value: TStream); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXStringValue = class(TDBXValue)
  private
    ValueNull: boolean;
    DBXStringValue: string;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXWideStringValue = class(TDBXValue)
  private
    DBXWideStringValue: wideString;
    ValueNull: boolean;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;

    function GetAsWideString: wideString; override;
    procedure SetAsWideString(const Value: wideString); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXTimeValue = class(TDBXValue)
  private
    DBXTimeValue: longint;
    ValueNull: boolean;
  protected
    function GetAsTDBXTime: longint; override;
    procedure SetAsTDBXTime(const Value: longint); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  TDBXTimeStampValue = class(TDBXValue)
  private
    ValueNull: boolean;
    DBXTimeStampValue: TDateTime;
  protected
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    function GetAsTimeStamp: TDateTime; override;
    procedure SetAsTimeStamp(const Value: TDateTime); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
  end;

  { TDBXParameter }

  TDBXParameter = class(TDBXValueType)
  private
    Value: TDBXWritableValue;
  public
    function GetValue: TDBXWritableValue;
    procedure setDataType(dataType: TDBXDataTypes); override;
    function getDataType: integer; override;
    function ToJSON: TJSONArray;
    constructor Create;
    destructor Destroy; override;
  end;

  TDBXParameterList = class
  private
    FObjectList: TObjectList;
  protected
    function GetItems(Index: cardinal): TDBXParameter;
    procedure SetItems(Index: cardinal; const Value: TDBXParameter);
  public
    constructor Create; overload;
    constructor Create(FreeObjects: boolean); overload;
    procedure Clear;
    destructor Destroy; override;
    procedure Add(AObject: TDBXParameter); overload;
    function Add: TDBXParameter; overload;
    procedure Delete(Index: cardinal);
    function Count: cardinal;
    property Items[Index: cardinal]: TDBXParameter read GetItems
      write SetItems; default;
  end;


  // JSONSerializable = interface
  // ['{7057563B-46DB-4485-A8FE-EE0473B4F3DD}']
  // function asJSONObject: TJSONObject;
  // end;
  //
  // TableType = interface
  // ['{6C03419D-B362-4977-9225-392375A608B9}']
  // end;

  { TParams }

  TDSParams = class
  private
    FParams: TDBXParameterList;
  public
    function asJSONObject: TJSONObject;
    constructor Create;
    destructor Destroy; override;
    function AddParameter(parameter: TDBXParameter): TDSParams;
    function FindParamByName(Value: string): TDBXParameter;
    function GetParamByName(Value: string): TDBXParameter;
    function Size: integer;
    function getParameter(Index: integer): TDBXParameter;
    class procedure LoadParametersValues(params: TDSParams;
      Value: TJSONObject); overload;
    class function LoadParametersValues(params: TDSParams; Value: TJSONObject;
      Offset: integer): boolean; overload;
    class function CreateFrom(Value: TJSONObject): TDSParams;
    class function CreateParametersFromMetadata(paramsMetadata: TJSONArray)
      : TDSParams;
  end;

  { TDBXReader }

  TDBXReader = class
  private
    FInternalDataStore: TJSONObject;
    FColumns: TDSParams;
  protected
    currentPosition: longint;
    function GetColumns: TDSParams;
    procedure SetParameters(const Value: TDSParams);
  public
    property Columns: TDSParams read GetColumns write SetParameters;
    procedure Reset;
    function asJSONObject: TJSONObject;
    function GetValue(position: integer): TDBXWritableValue; overload;
    function GetValue(Name: string): TDBXWritableValue; overload;
    function Next: boolean;
    constructor Create(params: TDSParams; Value: TJSONObject);
    destructor Destroy; override;
    class function CreateFrom(Value: TJSONObject): TDBXReader;
  end;

  { TDBXReaderValue }

  TDBXReaderValue = class(TDBXValue)
  private
    ValueNull: boolean;

    function getAsDBXReader: TDBXReader;
    procedure setAsDBXReader(const Value: TDBXReader);
  protected
    function GetAsTable: TObject; override;
    procedure SetAsTable(const Value: TObject); override;
  public
    function isNull: boolean;
    procedure SetNull; override;
    constructor Create; override;
    property AsDBXReader:TDBXReader read  getAsDBXReader write setAsDBXReader;
  end;

implementation

uses
  SysUtils, DBXJsonTools,FPCStrings;

{ TDBXReaderValue }

function TDBXReaderValue.getAsDBXReader: TDBXReader;
begin
   result:=  GetAsTable as TDBXReader ;
end;

function TDBXReaderValue.GetAsTable: TObject;
begin
  Result := FobjectValue;
end;

procedure TDBXReaderValue.SetAsTable(const Value: TObject);
begin
  ValueNull := False;
  FobjectValue:=Value;
end;

function TDBXReaderValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXReaderValue.setAsDBXReader(const Value: TDBXReader);
begin
  SetAsTable(Value);
end;

procedure TDBXReaderValue.setNull;
begin
  ValueNull := True;
  Clear;
end;

constructor TDBXReaderValue.Create;
begin
  inherited;
  SetDBXType(TableType);
  ValueNull := False;
end;

{ TDBXParameter }

function TDBXParameter.GetValue: TDBXWritableValue;
begin
  Result := Value;
end;

procedure TDBXParameter.setDataType(dataType: TDBXDataTypes);
begin
  Value.DBXType := dataType;
end;

function TDBXParameter.getDataType: integer;
begin
  Result := Ord(GetValue.DBXType);
end;

function TDBXParameter.ToJSON: TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add(Name);
  Result.Add(getDataType);
  Result.Add(Ordinal);
  Result.Add(SubType);
  Result.Add(Scale);
  Result.Add(Size);
  Result.Add(Precision);
  Result.Add(ChildPosition);
  Result.Add(Nullable);
  Result.Add(Hidden);
  Result.Add(ParameterDirection);
  Result.Add(ValueParameter);
  Result.Add(Literal);

end;

constructor TDBXParameter.Create;
begin
  inherited;
  Value := TDBXWritableValue.Create;
  Value.Clear;
end;

destructor TDBXParameter.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

{ TDBXInt8Value }

constructor TDBXInt8Value.Create;
begin
  inherited;
  SetDBXType(Int8Type);
  ValueNull := False;
end;

function TDBXInt8Value.getAsInt8: Int8;
begin
  try
    Result := DBXInt8Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXInt8Value.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXInt8Value.SetAsInt8(const Value: Int8);
begin
  try
    DBXInt8Value := Value;
    ValueNull := False;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

procedure TDBXInt8Value.setNull;
begin
  SetAsInt8(0);
  ValueNull := True;
end;

{ TDBXInt16Value }

constructor TDBXInt16Value.Create;
begin
  SetDBXType(Int16Type);
end;

function TDBXInt16Value.getAsInt16: Int16;
begin
  Result := DBXInt16Value;
end;

function TDBXInt16Value.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXInt16Value.SetAsInt16(const Value: Int16);
begin
  ValueNull := False;
  DBXInt16Value := Value;
end;

procedure TDBXInt16Value.setNull;
begin
  SetAsInt16(0);
  ValueNull := True;
end;

{ TDBXInt32Value }

constructor TDBXInt32Value.Create;
begin
  inherited;
  SetDBXType(Int32Type);
  ValueNull := False;
end;

function TDBXInt32Value.getAsInt32: Int32;
begin
  Result := DBXInt32Value;
end;

function TDBXInt32Value.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXInt32Value.SetAsInt32(const Value: Int32);
begin
  ValueNull := False;
  DBXInt32Value := Value;
end;

procedure TDBXInt32Value.setNull;
begin
  SetAsInt32(0);
  ValueNull := True;
end;

{ TDBXInt64Value }

constructor TDBXInt64Value.Create;
begin
  inherited;
  SetDBXType(Int64Type);
  ValueNull := False;
end;

function TDBXInt64Value.getAsInt64: int64;
begin
  Result := DBXInt64Value;
end;

function TDBXInt64Value.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXInt64Value.SetAsInt64(const Value: int64);
begin
  ValueNull := False;
  DBXInt64Value := Value;
end;

procedure TDBXInt64Value.setNull;
begin
  SetAsInt64(0);
  ValueNull := True;
end;

{ TDBXDoubleValue }

constructor TDBXDoubleValue.Create;
begin
  inherited;
  SetDBXType(DoubleType);
  ValueNull := False;
end;

function TDBXDoubleValue.getAsDouble: double;
begin
  Result := DBXDoubleValue;
end;

function TDBXDoubleValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXDoubleValue.SetAsDouble(const Value: double);
begin
  ValueNull := False;
  DBXDoubleValue := Value;
end;

procedure TDBXDoubleValue.setNull;
begin
  SetAsDouble(0);
  ValueNull := True;
end;

{ TDBXSingleValue }

constructor TDBXSingleValue.Create;
begin
  inherited;
  SetDBXType(SingleType);
  ValueNull := False;
end;

function TDBXSingleValue.getAsSingle: single;
begin
  Result := DBXSingleValue;
end;

function TDBXSingleValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXSingleValue.SetAsSingle(const Value: single);
begin
  ValueNull := False;
  DBXSingleValue := Value;
end;

procedure TDBXSingleValue.setNull;
begin
  SetAsSingle(0);
  ValueNull := True;
end;

{ TDBXBcdValue }

constructor TDBXBcdValue.Create;
begin
  inherited;
  SetDBXType(BcdType);
  ValueNull := False;
end;

function TDBXBcdValue.getAsBcd: double;
begin
  Result := DBXBcdValue;
end;

function TDBXBcdValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXBcdValue.SetAsBcd(const Value: double);
begin
  ValueNull := False;
  DBXBcdValue := Value;
end;

procedure TDBXBcdValue.setNull;
begin
  ValueNull := True;
  SetAsBcd(0);
end;

{ TDBXBooleanValue }

constructor TDBXBooleanValue.Create;
begin
  inherited;
  SetDBXType(BooleanType);
  ValueNull := False;
end;

function TDBXBooleanValue.GetAsBoolean: boolean;
begin
  Result := DBXBooleanValue;
end;

function TDBXBooleanValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXBooleanValue.SetAsBoolean(const Value: boolean);
begin
  ValueNull := False;
  DBXBooleanValue := Value;
end;

procedure TDBXBooleanValue.setNull;
begin
  SetAsBoolean(False);
  ValueNull := True;
end;

{ TDBXAnsiStringValue }

constructor TDBXAnsiStringValue.Create;
begin
  inherited;
  SetDBXType(AnsiStringType);
  ValueNull := False;
end;

function TDBXAnsiStringValue.GetAsAnsiString: AnsiString;
begin
  Result := DBXAnsiStringValue;
end;

function TDBXAnsiStringValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXAnsiStringValue.SetAsAnsiString(const Value: AnsiString);
begin
  ValueNull := False;
  DBXAnsiStringValue := Value;
end;

procedure TDBXAnsiStringValue.setNull;
begin
  SetAsAnsiString('');
  ValueNull := True;
end;

{ TDBXAnsiCharsValue }

constructor TDBXAnsiCharsValue.Create;
begin
  inherited;
  SetDBXType(WideStringType);
  ValueNull := False;
end;

function TDBXAnsiCharsValue.GetAsString: string;
begin
  Result := DBXAnsiCharsValue;
end;

function TDBXAnsiCharsValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXAnsiCharsValue.SetAsString(const Value: string);
begin
  ValueNull := False;
  DBXAnsiCharsValue := Value;
end;

procedure TDBXAnsiCharsValue.setNull;
begin
  SetAsString('');
  ValueNull := True;
end;

{ TDBXDateValue }

constructor TDBXDateValue.Create;
begin
  inherited;
  SetDBXType(DateType);
  ValueNull := False;
end;

function TDBXDateValue.GetAsTDBXDate: longint;
begin
  Result := DBXDateValue;
end;

function TDBXDateValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXDateValue.SetAsTDBXDate(const Value: longint);
begin
  ValueNull := False;
  DBXDateValue := Value;
end;

procedure TDBXDateValue.setNull;
begin
  SetAsTDBXDate(0);
  ValueNull := True;
end;

{ TDBXStreamValue }

constructor TDBXStreamValue.Create;
begin
  inherited;
  SetDBXType(BinaryBlobType);
  ValueNull := False;
end;

function TDBXStreamValue.GetAsStream: TStream;
begin
  Result := FStreamValue;
end;

function TDBXStreamValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXStreamValue.SetAsStream(const Value: TStream);
begin
  ValueNull := False;
  FStreamValue := Value;
end;

procedure TDBXStreamValue.setNull;
begin
  ValueNull := True;
  Clear;
end;

{ TDBXStringValue }

constructor TDBXStringValue.Create;
begin
  inherited;
  SetDBXType(WideStringType);
  ValueNull := False;
end;

function TDBXStringValue.GetAsString: string;
begin
  Result := DBXStringValue;
end;

function TDBXStringValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXStringValue.SetAsString(const Value: string);
begin
  ValueNull := False;
  DBXStringValue := Value;
end;

procedure TDBXStringValue.setNull;
begin
  DBXStringValue := '';
  ValueNull := True;
end;

{ TDBXWideStringValue }

constructor TDBXWideStringValue.Create;
begin
  inherited;
  SetDBXType(WideStringType);
  ValueNull := False;
end;

function TDBXWideStringValue.GetAsString: string;
begin
  Result := GetAsWideString;
end;

function TDBXWideStringValue.GetAsWideString: wideString;
begin
  Result := DBXWideStringValue;
end;

function TDBXWideStringValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXWideStringValue.SetAsString(const Value: string);
begin
  SetAsWideString(Value);
end;

procedure TDBXWideStringValue.SetAsWideString(const Value: wideString);
begin
  ValueNull := False;
  DBXWideStringValue := Value;
end;

procedure TDBXWideStringValue.setNull;
begin
  DBXWideStringValue := '';
  ValueNull := True;
end;

{ TDBXTimeValue }

constructor TDBXTimeValue.Create;
begin
  inherited;
  SetDBXType(TimeType);
  ValueNull := False;
end;

function TDBXTimeValue.getAsTDBXTime: longint;
begin
  Result := DBXTimeValue;
end;

function TDBXTimeValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXTimeValue.setAsTDBXTime(const Value: longint);
begin
  ValueNull := False;
  DBXTimeValue := Value;
end;

procedure TDBXTimeValue.setNull;
begin
  DBXTimeValue := 0;
  ValueNull := True;
end;

{ TDBXTimeStampValue }

constructor TDBXTimeStampValue.Create;
begin
  inherited;
  SetDBXType(TimeStampType);
  ValueNull := False;
end;

function TDBXTimeStampValue.getAsDateTime: TDateTime;
begin
  Result:= GetAsTimeStamp;
end;

function TDBXTimeStampValue.getAsTimeStamp: TDateTime;
begin
  Result := DBXTimeStampValue;
end;

function TDBXTimeStampValue.isNull: boolean;
begin
  Result := ValueNull;
end;

procedure TDBXTimeStampValue.setAsDateTime(const Value: TDateTime);
begin
  SetAsTimeStamp(Value);

end;

procedure TDBXTimeStampValue.setAsTimeStamp(const Value: TDateTime);
begin
  ValueNull := False;
  DBXTimeStampValue := Value;
end;

procedure TDBXTimeStampValue.setNull;
begin
  ValueNull := True;
  DBXTimeStampValue := 0;
end;

{ TDBXReader }

function TDBXReader.asJSONObject: TJSONObject;
var
  lastPosition: longint;
begin
  try
    lastPosition := currentPosition;
    try
      Reset;
      Result := TDBXJsonTools.DBXReaderToJSONObject(Self);
    finally
      currentPosition := lastPosition;
    end;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

constructor TDBXReader.Create(params: TDSParams; Value: TJSONObject);
begin
  currentPosition := -1;
  FInternalDataStore := Value;
  SetParameters(params);
end;

destructor TDBXReader.Destroy;
begin
  FColumns.Free;
  FInternalDataStore.Free;
  inherited Destroy;
end;

class function TDBXReader.CreateFrom(Value: TJSONObject): TDBXReader;
begin
  try
    Result := TDBXReader.Create(TDSParams.CreateParametersFromMetadata
      (Value.GetJSONArray('table')), Value);
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXReader.GetColumns: TDSParams;
begin
  Result := FColumns;
end;

function TDBXReader.GetValue(Name: string): TDBXWritableValue;
begin
  Result := FColumns.GetParamByName(Name).GetValue;
end;

function TDBXReader.Next: boolean;
begin
  Inc(currentPosition);
  try

    Result := TDSParams.LoadParametersValues(Columns, FInternalDataStore,
      currentPosition);
  except
    Result := False;
  end;
end;

function TDBXReader.GetValue(position: integer): TDBXWritableValue;
begin
  Result := FColumns.getParameter(position).GetValue;
end;

procedure TDBXReader.SetParameters(const Value: TDSParams);
begin
  FColumns := Value;
end;

procedure TDBXReader.Reset;
begin
  currentPosition := -1;
end;

{ TParams }

function TDSParams.asJSONObject: TJSONObject;
begin
  try
    Result := TDBXJsonTools.DBXParametersToJSONObject(Self);
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

constructor TDSParams.Create;
begin
  inherited;
  FParams := TDBXParameterList.Create(True);
end;

destructor TDSParams.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TDSParams.AddParameter(parameter: TDBXParameter): TDSParams;
begin
  try
    if FindParamByName(parameter.Name) = nil then
      FParams.Add(parameter)
    else
      raise DBXException.Create('Parameter name must be unique');

    Result := Self;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDSParams.FindParamByName(Value: string): TDBXParameter;
var
  i: integer;
  p: TDBXParameter;
begin
  for i := 0 to FParams.Count - 1 do
  begin
    p := FParams.Items[i];
    if p.Name = Value then
    begin
      Exit(p);
    end;
  end;
  Result := nil;
end;

function TDSParams.GetParamByName(Value: string): TDBXParameter;
var
  p: TDBXParameter;
begin
  p := FindParamByName(Value);
  if p <> nil then
    Result := p
  else
    raise DBXException.Create('Parameter not found [ ' + Value + ' ]');
end;

function TDSParams.Size: integer;
begin
  Result := FParams.Count;
end;

function TDSParams.getParameter(Index: integer): TDBXParameter;
begin
  Result := FParams.GetItems(Index);
end;

class procedure TDSParams.LoadParametersValues(params: TDSParams;
  Value: TJSONObject);
begin
  try
    LoadParametersValues(params, Value, 0);
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

class function TDSParams.LoadParametersValues(params: TDSParams;
  Value: TJSONObject; Offset: integer): boolean;
var
  parValue: TJSONArray;
  par: TDBXParameter;
  val: TDBXValue;
  i: integer;
begin
  try
    if params.Size <= 0 then
      Exit(False);

    for i := 0 to params.Size - 1 do
    begin
      
      par := params.getParameter(i);
      try
        parValue := Value.GetJSONArray(par.Name);

      except
        raise DBXException.Create('Num ' + IntToStr(i) + ' par name = ' +
          par.Name);
      end;
      if parValue.Size < Offset + 1 then
        Exit(False);

      val := par.GetValue;

      TDBXJsonTools.jsonToDBX(parValue.Get(Offset), val, '');
    end;
    Result := True;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

class function TDSParams.CreateFrom(Value: TJSONObject): TDSParams;
var
  params: TDSParams;
begin
  params := CreateParametersFromMetadata(Value.GetJSONArray('table'));
  LoadParametersValues(params, Value);
  Result := params;
end;

class function TDSParams.CreateParametersFromMetadata(paramsMetadata
  : TJSONArray): TDSParams;
var
  o: TDSParams;
  paramMetadata: TJSONArray;
  { todo }
  // parameter: TDBXValueType;
  parameter: TDBXParameter;
  i: integer;
begin
  try
    o := TDSParams.Create;
    for i := 0 to paramsMetadata.Size - 1 do
    begin
      paramMetadata := paramsMetadata.GetJSONArray(i);
      parameter := TDBXParameter.Create;
      TDBXJsonTools.JSONToValueType(paramMetadata, TDBXValueType(parameter));
      o.AddParameter(parameter);
    end;
    Result := o;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

{ TDBXParameterList }

function TDBXParameterList.Add: TDBXParameter;
begin
  Result := TDBXParameter.Create;
  Add(Result);
end;

procedure TDBXParameterList.Add(AObject: TDBXParameter);
begin
  FObjectList.Add(AObject);
end;

procedure TDBXParameterList.Clear;
begin
  FObjectList.Clear;
end;

function TDBXParameterList.Count: cardinal;
begin
  Result := FObjectList.Count;
end;

constructor TDBXParameterList.Create(FreeObjects: boolean);
begin
  inherited Create;
  FObjectList := TObjectList.Create(FreeObjects);
end;

constructor TDBXParameterList.Create;
begin
  inherited;
  FObjectList := TObjectList.Create(True);
end;

procedure TDBXParameterList.Delete(Index: cardinal);
begin
  FObjectList.Delete(Index);
end;

destructor TDBXParameterList.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TDBXParameterList.GetItems(Index: cardinal): TDBXParameter;
begin
  Result := TDBXParameter(FObjectList[Index]);
end;

procedure TDBXParameterList.SetItems(Index: cardinal;
  const Value: TDBXParameter);
begin
  FObjectList[Index] := Value;
end;

{ TDBXUInt8Value }

constructor TDBXUInt8Value.Create;
begin
  inherited;
  SetDBXType(UInt8Type);
  FValueNull := False;

end;

function TDBXUInt8Value.getAsUInt8: UInt8;
begin
  try
    Result := FDBXUInt8Value;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;
end;

function TDBXUInt8Value.isNull: boolean;
begin
  result:= FValueNull;
end;

procedure TDBXUInt8Value.SetAsUInt8(const Value: UInt8);
begin
  try
    FDBXUInt8Value := Value;
    FValueNull := False;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;


end;

procedure TDBXUInt8Value.setNull;
begin
  inherited;
  SetAsUInt8(0);
  FValueNull:= true;
end;

{ TDBXUInt16Value }

constructor TDBXUInt16Value.Create;
begin
  inherited;
  SetDBXType(UInt16Type);
end;

function TDBXUInt16Value.getAsUInt16: UInt16;
begin
   result:= FDBXUInt16Value;
end;

function TDBXUInt16Value.isNull: boolean;
begin
  result:= FValueNull;
end;

procedure TDBXUInt16Value.SetAsUInt16(const Value: UInt16);
begin
  try
    FDBXUInt16Value := Value;
    FValueNull := False;
  except
    on e: Exception do
      raise DBXException.Create(e.Message);
  end;

end;

procedure TDBXUInt16Value.setNull;
begin
  SetAsUInt16(0);
  FValueNull:= true;
end;

end.
