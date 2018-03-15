//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXJsonTools;
{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  DBXFPCJSON,
  DBXFPCCommon,
  Classes,
{$IFDEF FPC}
  DB,
  BufDataset,
  FMTBcd,
{$ELSE}
  Data.DB,
{$ENDIF}
  DSRestTypes,
  DBXValue;

type

  { TDBXJsonTools }

  TDBXJsonTools = class
  public
    class procedure jsonToDBX(obj: TJSONValue; var value: TDBXValue;
      dbxTypeName: String);
    class procedure JSONToValueType(json: TJSONArray; var vt: TDBXValueType);
    class function DBXParametersToJSONObject(dbxParameters: TDSParams)
      : TJSONObject;
    class function DBXReaderToJSONObject(dbxReader: TDBXReader): TJSONObject;
    class function CreateTDataSetFromJSON(value: TJSONObject): TDataset;
    class function TDataSetToJSONObject(value: TDataset): TJSONObject;
    class function GetTFieldTypeByTDBXDataTypes(DBXDataTypes: TDBXDataTypes)
      : TFieldType;
    class function GetTDBXDataTypesByTFieldType(FieldType: TFieldType)
      : TDBXDataTypes;
    class function CreateTStreamFromJSONArray(value: TJSONArray): TStream;
    class function StreamToJSONArray(value: TStream): TJSONArray;
    class function JSONToTableType(value: TJSONValue;
      dbxTypeName: String): TObject;
    class function SerializeTableType(Objetc: TObject): TJSONObject;

  end;

implementation

uses
  SysUtils, DBXDefaultFormatter,FPCStrings;

{ TDBXJsonTools }

class procedure TDBXJsonTools.jsonToDBX(obj: TJSONValue; var value: TDBXValue;
  dbxTypeName: String);
begin
  if TypeIsDBXValue(dbxTypeName) and (obj is TJSONNull) then
  begin
    value.AsDBXValue.SetNull;
  end
  else
  begin
    if not((obj is TJSONNull) and (trim(dbxTypeName) = '')) then
    begin

      case value.DBXType of
        Int8Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsInt8 := TJSONNumber(obj).GetIntValue
            else
              value.AsInt8 := TJSONNumber(obj).GetIntValue;
          end;
        Int16Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsInt16 := TJSONNumber(obj).GetIntValue
            else
              value.AsInt16 := TJSONNumber(obj).GetIntValue;
          end;

        Int32Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.ASInt32 := TJSONNumber(obj).GetIntValue
            else
              value.ASInt32 := TJSONNumber(obj).GetIntValue;
          end;
        Int64Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.ASInt64 := TJSONNumber(obj).GetIntValue
            else
              value.ASInt64 := TJSONNumber(obj).GetIntValue;
          end;
        UInt8Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsUInt8 := TJSONNumber(obj).GetIntValue
            else
              value.AsUInt8 := TJSONNumber(obj).GetIntValue;
          end;
        UInt16Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsUInt16 := TJSONNumber(obj).GetIntValue
            else
              value.AsUInt16 := TJSONNumber(obj).GetIntValue;
          end;

        UInt32Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsUInt32 := TJSONNumber(obj).GetIntValue
            else
              value.AsUInt32 := TJSONNumber(obj).GetIntValue;
          end;
        UInt64Type:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.ASInt32 := TJSONNumber(obj).GetIntValue
            else
              value.ASInt32 := TJSONNumber(obj).GetIntValue;
          end;

        AnsiStringType:
          begin
          {$WARNINGS OFF}
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsAnsiString := TJSONString(obj).GetValue
            else
              value.AsAnsiString := TJSONString(obj).GetValue;
          {$WARNINGS ON}
          end;


        WideStringType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsString := TJSONString(obj).GetValue
            else
              value.AsString := TJSONString(obj).GetValue;
          end;
        DateType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsTDBXDate :=
                TDBXDefaultFormatter.GetInstance.StringToDBXDate
                (TJSONString(obj).GetValue)
            else
              value.AsTDBXDate := TDBXDefaultFormatter.GetInstance.
                StringToDBXDate(TJSONString(obj).GetValue);

          end;
        DoubleType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsDouble := TJSONNumber(obj).GetValue
            else
              value.AsDouble := TJSONNumber(obj).GetValue;

          end;
        BcdType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsBcd := TJSONNumber(obj).GetValue
            else
              value.AsBcd := TJSONNumber(obj).GetValue;

          end;
        BooleanType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsBoolean := obj is TJSONTrue
            else
              value.AsBoolean := obj is TJSONTrue;
          end;
        TimeType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsTDBXTime :=
                TDBXDefaultFormatter.GetInstance.StringToDBXTime
                (TJSONString(obj).GetValue)
            else
              value.AsTDBXTime := TDBXDefaultFormatter.GetInstance.
                StringToDBXTime(TJSONString(obj).GetValue);

          end;
        DateTimeType:
          value.AsDateTime := TDBXDefaultFormatter.GetInstance.StringToDateTime
            (TJSONString(obj).GetValue);
        TableType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsTable := TDBXJsonTools.JSONToTableType
                (obj.Clone, dbxTypeName)
            else
              value.AsTable := TDBXJsonTools.JSONToTableType(obj.Clone,
                dbxTypeName);
          end;

        TimeStampType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsTimeStamp :=
                TDBXDefaultFormatter.GetInstance.StringToDateTime
                (TJSONString(obj).GetValue)
            else
              value.AsTimeStamp := TDBXDefaultFormatter.GetInstance.
                StringToDateTime(TJSONString(obj).GetValue);
          end;
        CurrencyType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsCurrency := TJSONNumber(obj).GetCurrencyValue
            else
              value.AsCurrency := TJSONNumber(obj).GetCurrencyValue;

          end;
        SingleType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsSingle := TJSONNumber(obj).GetValue
            else
              value.AsSingle := TJSONNumber(obj).GetValue;

          end;
        JsonValueType:
          value.AsJsonValue := obj.Clone;
        BinaryBlobType:
          begin
            if TypeIsDBXValue(dbxTypeName) then
              value.AsDBXValue.AsStream :=
                TDBXJsonTools.CreateTStreamFromJSONArray(TJSONArray(obj))
            else
              value.AsStream := TDBXJsonTools.CreateTStreamFromJSONArray
                (TJSONArray(obj));
          end;
        BlobType:
          value.AsBlob := TDBXJsonTools.CreateTStreamFromJSONArray
            (TJSONArray(obj));
        CallbackType:
          ;
      else
        raise DBXException.create('Cannot convert Json to DBX');
      end;
    end
    else
      value.clear;
  end;

end;

class procedure TDBXJsonTools.JSONToValueType(json: TJSONArray;
  var vt: TDBXValueType);
begin
  vt.Name := json.getString(0);
  vt.setDataType(TDBXDataTypes(json.getInt(1)));
  vt.Ordinal := json.getInt(2);
  vt.SubType := json.getInt(3);
  vt.Scale := json.getInt(4);
  vt.Size := json.getInt(5);
  vt.Precision := json.getInt(6);
  vt.ChildPosition := json.getInt(7);
  vt.Nullable := json.getBoolean(8);
  vt.Hidden := json.getBoolean(9);
  vt.ParameterDirection := json.getInt(10);
  vt.ValueParameter := json.getBoolean(11);
  vt.Literal := json.getBoolean(12);
end;

class function TDBXJsonTools.DBXParametersToJSONObject(dbxParameters: TDSParams)
  : TJSONObject;
var
  json: TJSONObject;
  arrParameters, arrParam, arrParamsValue: TJSONArray;
  I: Integer;
begin
  json := TJSONObject.create;
  arrParameters := TJSONArray.create;
  json.AddPairs('table', arrParameters);

  for I := 0 to dbxParameters.Size - 1 do
  begin
    arrParam := TJSONArray.create;
    arrParam.Add(dbxParameters.getParameter(I).Name);
    arrParam.Add(dbxParameters.getParameter(I).getDataType);
    arrParam.Add(dbxParameters.getParameter(I).Ordinal);
    arrParam.Add(dbxParameters.getParameter(I).SubType);
    arrParam.Add(dbxParameters.getParameter(I).Scale);
    arrParam.Add(dbxParameters.getParameter(I).Size);
    arrParam.Add(dbxParameters.getParameter(I).Precision);
    arrParam.Add(dbxParameters.getParameter(I).ChildPosition);
    arrParam.Add(dbxParameters.getParameter(I).Nullable);
    arrParam.Add(dbxParameters.getParameter(I).Hidden);
    arrParam.Add(dbxParameters.getParameter(I).ParameterDirection);
    arrParam.Add(dbxParameters.getParameter(I).ValueParameter);
    arrParam.Add(dbxParameters.getParameter(I).Literal);
    arrParameters.Add(arrParam);
  end;

  for I := 0 to dbxParameters.Size() - 1 do
  begin
    arrParamsValue := TJSONArray.create;
    arrParamsValue.Add(dbxParameters.getParameter(I).GetValue.toString);
    json.AddPairs(dbxParameters.getParameter(I).Name, arrParamsValue);
  end;
  Result := json;

end;

class function TDBXJsonTools.DBXReaderToJSONObject(dbxReader: TDBXReader)
  : TJSONObject;
var
  json: TJSONObject;
  arrayParams: TJSONArray;
  columns: TDSParams;
  I, C: Integer;
begin
  try
    json := TJSONObject.create;
    columns := dbxReader.columns;
    arrayParams := TJSONArray.create;
    for I := 0 to columns.Size - 1 do
    begin
      arrayParams.Add(columns.getParameter(I).ToJSON);
      // Create the empty JSONArray for the data. Will be filled after
      json.AddPairs(columns.getParameter(I).Name, TJSONArray.create);
    end;

    while (dbxReader.Next) do
    begin
      for C := 0 to columns.Size - 1 do
      begin
        dbxReader.columns.getParameter(C).GetValue.AppendTo
          (json.GetJSONArray(columns.getParameter(C).Name));
      end;
    end;
    json.AddPairs('table', arrayParams);

    Result := json;
  except
    on e: exception do
      raise DBXException.create(e.Message);
  end;
end;

class function TDBXJsonTools.CreateTDataSetFromJSON(value: TJSONObject)
  : TDataset;
{$IFDEF FPC}
var
  json: TJSONArray;
  I: Integer;
  val: TDBXValue;
  FieldName, s: String;
  FieldSize: Integer;
  FieldType: TFieldType;
  rdr: TDBXReader;
{$ENDIF}
begin
{$IFDEF FPC}
  Result := TBufDataset.create(nil);
  rdr := TDBXReader.CreateFrom(value);
  try
    for I := 0 to rdr.columns.Size - 1 do
    begin
      FieldName := rdr.columns.getParameter(I).Name;
      FieldType := GetTFieldTypeByTDBXDataTypes
        (TDBXDataTypes(rdr.columns.getParameter(I).getDataType));
      if FieldType in [ftString, ftWideString] then
        FieldSize := rdr.columns.getParameter(I).Size
      else
        FieldSize := 0;
      Result.FieldDefs.Add(FieldName, FieldType, FieldSize);
    end;
    TBufDataset(Result).CreateDataset;
    Result.Active := True;
    I := 0;
    while rdr.Next do
    begin
      Result.Append;
      for I := 0 to rdr.columns.Size - 1 do
      begin
        case TDBXDataTypes(rdr.columns.getParameter(I).getDataType) of
          AnsiStringType, WideStringType:
            Result.Fields[I].AsString := rdr.GetValue(I).AsString;
          DateType:
            Result.Fields[I].AsDateTime := rdr.GetValue(I).AsTDBXDate;
          DateTimeType, TimeStampType:
            Result.Fields[I].AsDateTime := rdr.GetValue(I).AsDateTime;
          TimeType:
            Result.Fields[I].AsDateTime := rdr.GetValue(I).AsTDBXTime;
          BooleanType:
            Result.Fields[I].AsBoolean := rdr.GetValue(I).AsBoolean;
          Int16Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).AsInt16;
          Int32Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).ASInt32;
          UInt16Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).AsUInt16;
          UInt32Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).AsUInt32;
          Int8Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).AsInt8;
          UInt8Type:
            Result.Fields[I].AsInteger := rdr.GetValue(I).AsUInt8;
          DoubleType, BcdType:
            Result.Fields[I].AsFloat := rdr.GetValue(I).AsDouble;
          SingleType:
            Result.Fields[I].AsFloat := rdr.GetValue(I).AsSingle;
          Int64Type:
            Result.Fields[I].AsLargeInt := rdr.GetValue(I).ASInt64;
          UInt64Type:
            Result.Fields[I].AsLargeInt := rdr.GetValue(I).AsUInt64;
          CurrencyType:
            Result.Fields[I].AsCurrency := rdr.GetValue(I).AsCurrency;
          BinaryBlobType:
            TBlobField(Result.Fields[I])
              .LoadFromStream(rdr.GetValue(I).AsStream);
          BlobType:
            TBlobField(Result.Fields[I]).LoadFromStream(rdr.GetValue(I).AsBlob);
          // TimeStampOffsetType, TimeStampType:
          // Result.Fields[i].AsLargeInt := rdr.GetValue(i).AsTimeStamp;
        end;
      end;
      Result.Post;
    end;
    Result.First;
  finally
    rdr.Free;
  end;
{$ELSE}
  result:= nil;
{$ENDIF}
end;

class function TDBXJsonTools.TDataSetToJSONObject(value: TDataset): TJSONObject;
var
  json: TJSONObject;
  arr, arrayParams: TJSONArray;
  column: TDBXParameter;
  I, C: Integer;
  ms: TMemoryStream;
begin
  try
    json := TJSONObject.create;
    arrayParams := TJSONArray.create;
    for I := 0 to value.FieldCount - 1 do
    begin
      column := TDBXParameter.create;
      column.Name := value.FieldDefs[I].Name;
      column.setDataType(TDBXJsonTools.GetTDBXDataTypesByTFieldType
        (value.FieldDefs[I].DataType));
      column.Size := value.FieldDefs[I].Size;
      arrayParams.Add(column.ToJSON);
      // Create the empty JSONArray for the data. Will be filled after
      json.AddPairs(column.Name, TJSONArray.create);
      column.Free;
    end;
    value.First;
    while not value.EOF do
    begin
      for C := 0 to value.FieldCount - 1 do
      begin
        arr := json.GetJSONArray(value.FieldDefs[C].Name);
        case value.FieldDefs[C].DataType of
          ftString:
            arr.Add(value.Fields[C].AsString);
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            arr.Add(TDBXDefaultFormatter.GetInstance.DateTimeToString
              (value.Fields[C].AsDateTime));
          ftBoolean:
            arr.Add(value.Fields[C].AsBoolean);
          ftInteger, ftWord:
            arr.Add(value.Fields[C].AsInteger);
          ftFloat:
            arr.Add(value.Fields[C].AsFloat);
{$IFDEF FPC}
          ftBCD:
            arr.Add(BCDToDouble(value.Fields[C].AsBcd));
{$ENDIF}
          ftLargeint:
            arr.Add(value.Fields[C].AsLargeInt);
          ftCurrency:
            arr.Add(double(value.Fields[C].AsCurrency));
          ftWideString:
            arr.Add(value.Fields[C].AsWideString);
          ftBlob, ftMemo, ftGraphic, ftWideMemo:
            begin
              ms := TMemoryStream.create;
              try
                TBlobField(value.Fields[C]).SaveToStream(ms);
                ms.Position := 0;
                arr.Add(StreamToJSONArray(ms));
              finally
                ms.Free;
              end;
            end;
        else
          raise DBXException.create('Fied type not supported');
        end; // case
      end; // for
      value.Next;
    end; // while
    json.AddPairs('table', arrayParams);
    Result := json;
  except
    on e: exception do
      raise DBXException.create(e.Message);
  end;
end;

class function TDBXJsonTools.GetTFieldTypeByTDBXDataTypes
  (DBXDataTypes: TDBXDataTypes): TFieldType;
begin
  case DBXDataTypes of
    AnsiStringType:
      Result := ftString;
    DateType:
      Result := ftDate;
    BlobType:
      Result := ftBlob;
    BooleanType:
      Result := ftBoolean;
    Int16Type, Int32Type, UInt16Type, UInt32Type, Int8Type, UInt8Type:
      Result := ftInteger;
    DoubleType:
      Result := ftFloat;
    BcdType:
      Result := ftBCD;
    TimeType:
      Result := ftTime;
    DateTimeType:
      Result := ftDateTime;
    Int64Type, UInt64Type:
      Result := ftLargeint;
    TimeStampType:
      Result := ftTimeStamp;
    CurrencyType:
      Result := ftCurrency;
    WideStringType:
      Result := ftWideString;
    SingleType:
      Result := ftFloat;
    BinaryBlobType:
      Result := ftBlob;
    TimeStampOffsetType:
      Result := ftTimeStamp;
  else
    raise DBXException.create('invalid data type');
  end;
end;

class function TDBXJsonTools.GetTDBXDataTypesByTFieldType(FieldType: TFieldType)
  : TDBXDataTypes;
begin
  case FieldType of
    ftString:
      Result := AnsiStringType;
    ftDate:
      Result := DateType;
    ftBoolean:
      Result := BooleanType;
    ftInteger, ftWord:
      Result := Int32Type;
    ftFloat:
      Result := DoubleType;
    ftBCD:
      Result := BcdType;
    ftTime:
      Result := TimeType;
    ftDateTime:
      Result := DateTimeType;
    ftLargeint:
      Result := Int64Type;
    ftTimeStamp:
      Result := TimeStampType;
    ftCurrency:
      Result := CurrencyType;
    ftWideString:
      Result := WideStringType;
    ftBlob:
      Result := BinaryBlobType;
    ftMemo:
      Result := BlobType;
  else
    raise DBXException.create('invalid field type');
  end;
end;

class function TDBXJsonTools.CreateTStreamFromJSONArray
  (value: TJSONArray): TStream;
{$IFDEF FPC}
var
  I: Integer;
  b: byte;
begin
  try
    Result := TMemoryStream.create;
    for I := 0 to value.Size - 1 do
    begin
      b := byte(value.getInt(I));
      Result.WriteByte(b);
    end;
    Result.Position := 0;
  except
    on e: exception do
      raise DBXException.create(e.Message);
  end;
{$ELSE}
begin
  result:= nil;
{$ENDIF}
end;

class function TDBXJsonTools.StreamToJSONArray(value: TStream): TJSONArray;
{$IFDEF FPC}
var
  b: byte;
  old_pos: Integer;
{$ENDIF}
begin
Result := TJSONArray.create;
{$IFDEF FPC}
  if assigned( value) then
  begin
  old_pos := value.Position;
  
  value.Position := 0;
  while value.Position < value.Size do
  begin
    b := value.ReadByte;
    Result.Add(b);
  end;
  value.Position := old_pos;
end;
{$ENDIF}
end;


class function TDBXJsonTools.JSONToTableType(value: TJSONValue;
  dbxTypeName: String): TObject;
begin
  try
    if dbxTypeName = 'TDSParams' then
    begin
      exit(TDSParams.CreateFrom(TJSONObject(value)))
    end
    else if ((dbxTypeName = 'TDBXReader') or (dbxTypeName = 'TDBXReaderValue'))
    then
    begin
      exit(TDBXReader.CreateFrom(TJSONObject(value)));
    end
    else if dbxTypeName = 'TDataSet' then
    begin
      exit(TDBXJsonTools.CreateTDataSetFromJSON(TJSONObject(value)));
    end
    else
      raise DBXException.create(dbxTypeName + ' is not a table type');
  except
    on e: exception do
      raise DBXException.create(e.Message);
  end;
end;

class function TDBXJsonTools.SerializeTableType(Objetc: TObject): TJSONObject;
begin
  try
    if Objetc is TDSParams then
    begin
      Result := TDSParams(Objetc).asJSONObject;
    end
    else if Objetc is TDBXReader then
    begin
      Result := TDBXReader(Objetc).asJSONObject;
    end
    else if Objetc is TDataset then
    begin
      Result := TDBXJsonTools.TDataSetToJSONObject(TDataset(Objetc));
    end
    else
      raise DBXException.create('Invalid table type to serialize');

  except
    on e: exception do
      raise DBXException.create(e.Message);
  end;
end;

end.
