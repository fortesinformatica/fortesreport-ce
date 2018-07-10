//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXFPCJSON;
{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  Classes, Contnrs, FPCStrings, Math,
{$IFDEF FPC}
  fpjson, jsonparser, variants, DSRESTTypes,
{$ELSE}
  DBXJSON,
{$ENDIF}
  SysUtils;

const
  NullString = 'null';

type
  FPJSONValueType = (jvtJSONObject, jvtJSONArray, jvtJSONString, jvtJSONNumber,
    jvtJSONTrue, jvtJSONFalse, jvtJSONNull);
{$IFDEF FPC}
  TFPJSONObject = fpjson.TJSONObject;
  TFPJSONNumber = fpjson.TJSONNumber;
  TFPJSONNull = fpjson.TJSONNull;
  TFPJSONArray = fpjson.TJSONArray;
{$ELSE}
  TFPJSONObject = DBXJSON.TJSONObject;
  TFPJSONNumber = DBXJSON.TJSONNumber;
  TFPJSONNull = DBXJSON.TJSONNull;
  TFPJSONArray = DBXJSON.TJSONArray;
{$ENDIF}
  { TJSONValue }

  TJSONValue = class abstract
  protected
    (* *
      * A String that represents the JSON Null String that is "null"
    *)
  public
    (* *
      * Returns the specified internal JSONValue wrapped
      *
      * @return the internal object
    *)
    function GetInternalObject: TObject; virtual;
    (* *
      * Returns the specified {@link JSONValueType} of the internal JSONValue
      * wrapped
      *
      * @return
    *)
    function GetJsonValueType: FPJSONValueType; virtual;
    (* *
      * Returns the JSON String representation for this object
    *)
    function ToString: String; override;
    function GetNullString: String;
    function Clone: TJSONValue;
  end;

  { TJSONPair }

  TJSONPair = class
  private
    Fname: String;
    FValue: TJSONValue;
    procedure Setname(const AValue: String);
    procedure Setvalue(const AValue: TJSONValue);
  public
    property name: String read Fname write Setname;
    property value: TJSONValue read FValue write Setvalue;
    constructor Create(PairName: String; PairValue: TJSONValue);
      reintroduce; overload;
    constructor Create(PairName: String; PairValue: String);
      reintroduce; overload;
    destructor Destroy; override;
  end;

  { TJSONPairList }

  TJSONPairList = class
  private
    FObjectList: TObjectList;
    function GetItems(Index: Cardinal): TJSONPair;
    procedure SetItems(Index: Cardinal; const value: TJSONPair);
  public
    constructor Create; overload;
    constructor Create(FreeObjects: Boolean); overload;
    procedure Clear;
    destructor Destroy; override;
    procedure Add(AObject: TJSONPair); overload;
    function Add: TJSONPair; overload;
    procedure Delete(Index: Cardinal);
    function Count: Cardinal;
    property Items[Index: Cardinal]: TJSONPair read GetItems
      write SetItems; default;
  end;

  { TJSONValueList }

  TJSONValueList = class
  private
    FObjectList: TObjectList;
    function GetItems(Index: Cardinal): TJSONValue;
    procedure SetItems(Index: Cardinal; const value: TJSONValue);
  public
    constructor Create; overload;
    constructor Create(FreeObjects: Boolean); overload;
    procedure Clear;
    destructor Destroy; override;
    procedure Add(AObject: TJSONValue); overload;
    function Add: TJSONValue; overload;
    procedure Delete(Index: Cardinal);
    function Count: Cardinal;
    property Items[Index: Cardinal]: TJSONValue read GetItems
      write SetItems; default;
  end;

  { TJSONString }

  TJSONString = class(TJSONValue)
  private
    FIsNull: Boolean;
    FValue: string;
    procedure Setvalue(const AValue: string);
  protected
    property Value: string read FValue write Setvalue;
  public
    function GetInternalObject: String; reintroduce;
    function GetJsonValueType: FPJSONValueType; override;
    function ToString: String; override;
    function GetValue: string;
    constructor Create; reintroduce; overload;
    constructor Create(val: String); reintroduce; overload;
  end;

  TJSONArray = class;

  { TJSONObject }

  TJSONObject = class(TJSONValue)
  protected
    FElements: TJSONPairList;
{$IFDEF FPC}
    procedure BuildElements(o: TFPJSONObject);
    function AsJSONObject: TFPJSONObject;
{$ENDIF}
  public
    function Has(name: String): Boolean;
    function GetJsonValueType: FPJSONValueType; override;
{$IFDEF FPC}
    function GetInternalObject: TFPJSONObject;
{$ENDIF}
    function Get(name: String): TJSONPair;
    function GetString(name: String): String;
    function GetBoolean(name: String): Boolean;
    function GetDouble(name: String): Double;
    function GetInt(name: String): Integer;
    function GetJSONArray(name: String): TJSONArray;
    function GetJSONObject(name: String): TJSONObject; overload;
    function ToString: String; override;
    function AddPairs(pair: TJSONPair): TJSONObject; overload;
    function AddPairs(name: String; value: TJSONValue): TJSONObject; overload;
    function AddPairs(name: String; value: Integer): TJSONObject; overload;
    function AddPairs(name: String; value: String): TJSONObject; overload;
    function AddPairs(name: String; value: Int64): TJSONObject; overload;
    function AddPairs(name: String; value: Double): TJSONObject; overload;
    function AddPairs(name: String; value: Boolean): TJSONObject; overload;
    constructor Create; reintroduce; overload;
{$IFDEF FPC}
    constructor Create(json: TFPJSONObject); reintroduce; overload;
{$ENDIF}
    constructor Create(pair: TJSONPair); reintroduce; overload;
    destructor Destroy; override;
    class function Parse(value: String): TJSONObject;

  end;

  { TJSONNumber }

  TJSONNumber = class(TJSONValue)
  private
    FIsNull: Boolean;
    FValue: Double;
    procedure Setvalue(const AValue: Double);
  protected
{$IFDEF FPC}
    FJSONNumber: TFPJSONNumber;
{$ENDIF}
    property value: Double read FValue write Setvalue;
  public
    function GetInternalObject: Double; reintroduce;
    function GetJsonValueType: FPJSONValueType; override;
    function ToString: String; override;
    function GetValue: Double;
    function GetIntValue: Integer;
    function GetCurrencyValue: Currency;
    constructor Create; reintroduce; overload;
    constructor Create(val: Double); reintroduce; overload;
    constructor Create(val: Integer); reintroduce; overload;
    constructor Create(val: Int64); reintroduce; overload;
    constructor Create(val: String); reintroduce; overload;
  end;

  { TJSONTrue }

  TJSONTrue = class(TJSONValue)
  public
    function GetInternalObject: Boolean; reintroduce;
    function GetJsonValueType: FPJSONValueType; override;
    function ToString: String; override;
    function asJSONString: String;
  end;

  { TJSONFalse }

  TJSONFalse = class(TJSONValue)
  public
    function GetInternalObject: Boolean; reintroduce;
    function GetJsonValueType: FPJSONValueType; override;
    function ToString: String; override;
    function asJSONString: String;
  end;

  { TJSONNull }

  TJSONNull = class(TJSONValue)
  public
    function GetInternalObject: variant; reintroduce;
    function GetJsonValueType: FPJSONValueType; override;
    function ToString: String; override;
    function asJSONString: String;
  end;

  { TJSONArray }

  TJSONArray = class(TJSONValue)
  protected
    FElements: TJSONValueList;
{$IFDEF FPC}
    FJSONArray: TFPJSONArray;
    function BuildElements(arr: TFPJSONArray): TJSONValueList;
    function AsJSONArray: TFPJSONArray;
{$ENDIF}
  public
    function GetJsonValueType: FPJSONValueType; override;
{$IFDEF FPC}
    function GetInternalObject: TFPJSONArray; override;
{$ENDIF}
    function ToString: String; override;
    function size: Int64;
    function Add(value: Integer): TJSONArray; overload;
    function Add(value: Double): TJSONArray; overload;
    function Add(value: String): TJSONArray; overload;
    function Add(value: Boolean): TJSONArray; overload;
    function Add(value: TJSONValue): TJSONArray; overload;
    function Get(Index: Integer): TJSONValue;
    function GetString(Index: Integer): String;
    function GetDouble(Index: Integer): Double;
    function GetInt(Index: Integer): Integer;
    function GetBoolean(Index: Integer): Boolean;
    function GetJSONString(Index: Integer): TJSONString;
    function GetJSONObject(Index: Integer): TJSONObject;
    function GetJSONArray(Index: Integer): TJSONArray;
    function Remove(Index: Integer): TJSONArray;
    constructor Create; reintroduce; overload;
    constructor Create(JSONValues: TJSONValueList); reintroduce; overload;
    destructor Destroy; override;
{$IFDEF FPC}
    constructor Create(json: TFPJSONArray); reintroduce; overload;
{$ENDIF}
    class function Parse(JSONString: String): TJSONArray;
  end;

implementation

{ TJSONArray }

uses DBXDefaultFormatter;

{$IFDEF FPC}

function TJSONArray.BuildElements(arr: TFPJSONArray): TJSONValueList;
var
  res: TJSONValueList;
  obj: fpjson.TJSONData;
  i: Integer;
begin
  res := TJSONValueList.Create(true);
  for i := 0 to arr.Count - 1 do
  begin
    obj := arr.Items[i];
    case obj.JSONType of
      jtUnknown:
        begin

        end;
      jtNumber:
        begin
          res.Add(TJSONNumber.Create(obj.AsFloat));
        end;
      jtString:
        begin
          res.Add(TJSONString.Create(obj.AsString));
        end;
      jtBoolean:
        begin
          if obj.value then
            res.Add(TJSONTrue.Create)
          else
            res.Add(TJSONFalse.Create);
        end;
      jtNull:
        begin
          res.Add(TJSONNull.Create);
        end;
      jtArray:
        begin
          res.Add(TJSONArray.Create(obj as TFPJSONArray));
        end;
      jtObject:
        begin
          res.Add(TJSONObject.Create(obj as TFPJSONObject));
        end;
    end;
  end;
  Result := res;
end;
{$ENDIF}
{$IFDEF FPC}

function TJSONArray.AsJSONArray: TFPJSONArray;
var
  arr: TFPJSONArray;
  v: TJSONValue;
  i: Integer;
  Parser: TJSONParser;
begin
  arr := TFPJSONArray.Create;
  for i := 0 to FElements.Count - 1 do
  begin
    v := FElements[i];
    case v.GetJsonValueType of
      jvtJSONObject:
        begin
          Parser := TJSONParser.Create(TJSONObject(v).ToString);
          try
            arr.Add(Parser.Parse);
          finally
            Parser.Free;
          end;
        end;
      jvtJSONArray:
        begin
          Parser := TJSONParser.Create(TJSONArray(v).ToString);
          try
            arr.Add(fpjson.TJSONArray(Parser.Parse));
          finally
            Parser.Free;
          end;
        end;
      jvtJSONString:
        begin
          arr.Add(TJSONString(v).GetValue);
        end;
      jvtJSONNumber:
        begin
          if ceil(TJSONNumber(v).GetIntValue) = TJSONNumber(v).GetIntValue then
            arr.Add(TJSONNumber(v).GetIntValue)
          else
            arr.Add(TJSONNumber(v).GetValue);

          // arr.Add(TJSONNumber(v).GetValue);
        end;
      jvtJSONTrue:
        begin
          arr.Add(true);
        end;
      jvtJSONFalse:
        begin
          arr.Add(false);
        end;
      jvtJSONNull:
        begin
          arr.Add(TFPJSONNull.Create);
        end;
    end;
  end;
  Result := arr;
end;
{$ENDIF}

function TJSONArray.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONArray;
end;

{$IFDEF FPC}

function TJSONArray.GetInternalObject: TFPJSONArray;
begin
  Result := AsJSONArray;
end;
{$ENDIF}

function TJSONArray.ToString: String;
{$IFDEF FPC}
var
  arr: TFPJSONArray;
{$ENDIF}
begin
{$IFDEF FPC}
  arr := AsJSONArray;
  try
    Result := arr.AsJSON;
  finally
    arr.Free;
  end;
{$ELSE}
  Result := '';
{$ENDIF}
end;

function TJSONArray.size: Int64;
begin
  Result := FElements.Count;
end;

function TJSONArray.Add(value: Integer): TJSONArray;
begin
{$IFDEF FPC}
  FElements.Add(TJSONNumber.Create(value));
  Result := Self;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TJSONArray.Add(value: Double): TJSONArray;
begin
{$IFDEF FPC}
  FElements.Add(TJSONNumber.Create(value));
  Result := Self;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TJSONArray.Add(value: String): TJSONArray;
begin
{$IFDEF FPC}
  FElements.Add(TJSONString.Create(value));
  Result := Self;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TJSONArray.Add(value: Boolean): TJSONArray;
begin
{$IFDEF FPC}
  if (value) then
    FElements.Add(TJSONTrue.Create)
  else
    FElements.Add(TJSONFalse.Create);
  Result := Self;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TJSONArray.Add(value: TJSONValue): TJSONArray;
begin
{$IFDEF FPC}
  FElements.Add(value);
  Result := Self;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TJSONArray.Get(Index: Integer): TJSONValue;
begin
  Result := FElements.Items[index];
end;

function TJSONArray.GetString(Index: Integer): String;
var
  p: TJSONValue;
begin
  p := Get(index);
  if Assigned(p) then
    Result := TJSONString(p).GetValue
  else
    Result := '';
end;

function TJSONArray.GetDouble(Index: Integer): Double;
var
  p: TJSONValue;
begin
  p := Get(index);
  if Assigned(p) then
    Result := TJSONNumber(p).GetValue
  else
    Result := 0;
end;

function TJSONArray.GetInt(Index: Integer): Integer;
begin
  Result := trunc(GetDouble(index));
end;

function TJSONArray.GetJSONString(Index: Integer): TJSONString;
begin
  Result := TJSONString(Get(index));
end;

function TJSONArray.GetJSONObject(Index: Integer): TJSONObject;
begin
  Result := TJSONObject(Get(index));
end;

function TJSONArray.GetJSONArray(Index: Integer): TJSONArray;
begin
  Result := TJSONArray(Get(index));
end;

function TJSONArray.GetBoolean(Index: Integer): Boolean;
var
  p: TJSONValue;
begin
  result:= false;
  p := Get(index);
  if p is TJSONTrue then
    Result := true
  else if p is TJSONFalse then
    Result := false;
end;

function TJSONArray.Remove(Index: Integer): TJSONArray;
begin
  FElements.Delete(index);
  Result := Self;
end;

constructor TJSONArray.Create;
begin
  inherited;
  FElements := TJSONValueList.Create(true);
end;

constructor TJSONArray.Create(JSONValues: TJSONValueList);
begin
  inherited Create;
  FElements := JSONValues;
end;

destructor TJSONArray.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

{$IFDEF FPC}

constructor TJSONArray.Create(json: TFPJSONArray);
begin
  inherited Create;
  FElements := BuildElements(json);
end;
{$ENDIF}

class function TJSONArray.Parse(JSONString: String): TJSONArray;
{$IFDEF FPC}
var
  Parser: TJSONParser;
  arr: TFPJSONArray;
{$ENDIF}
begin
  Result := nil;
{$IFDEF FPC}
  Parser := TJSONParser.Create(JSONString);
  try
    arr := Parser.Parse as TFPJSONArray;
    try
    if Assigned(arr) then
      Result := TJSONArray.Create(arr);
    finally
      FreeAndNil(arr);
    end;
  finally
    Parser.Free;
  end;
{$ENDIF}
end;

{ TJSONNull }

function TJSONNull.GetInternalObject: variant;
begin
{$IFDEF FPC}
  Result := variants.Null;
{$ELSE}
  Result := vaNull;
{$ENDIF}
end;

function TJSONNull.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONNull;
end;

function TJSONNull.ToString: String;
begin
  Result := asJSONString;
end;

function TJSONNull.asJSONString: String;
begin
  Result := 'null';
end;

{ TJSONFalse }

function TJSONFalse.GetInternalObject: Boolean;
begin
  Result := false;
end;

function TJSONFalse.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONFalse;
end;

function TJSONFalse.ToString: String;
begin
  Result := asJSONString;
end;

function TJSONFalse.asJSONString: String;
begin
  Result := 'false';
end;

{ TJSONTrue }

function TJSONTrue.GetInternalObject: Boolean;
begin
  Result := true;
end;

function TJSONTrue.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONTrue;
end;

function TJSONTrue.ToString: String;
begin
  Result := asJSONString;
end;

function TJSONTrue.asJSONString: String;
begin
  Result := 'true';
end;

{ TJSONNumber }

procedure TJSONNumber.Setvalue(const AValue: Double);
begin
  FIsNull := false;
  if FValue <> AValue then
    FValue := AValue;
end;

function TJSONNumber.GetCurrencyValue: Currency;
begin
  Result := FloatToCurr(FValue);
end;

function TJSONNumber.GetInternalObject: Double;
begin
  Result := FValue;
end;

function TJSONNumber.GetIntValue: Integer;
begin
  Result := trunc(FValue);
end;

function TJSONNumber.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONNumber;
end;

function TJSONNumber.ToString: String;
begin
  if not FIsNull then
    Result := DBXDefaultFormatter.TDBXDefaultFormatter.GetInstance.
      FloatToString(FValue)
  else
    Result := NullString;
end;

function TJSONNumber.GetValue: Double;
begin
  Result := FValue;
end;

constructor TJSONNumber.Create;
begin
  inherited;
  FIsNull := true;
  FValue := 0;
end;

constructor TJSONNumber.Create(val: Double);
begin
  Create;
  Setvalue(val);
end;

constructor TJSONNumber.Create(val: Integer);
begin
  Create;
  Setvalue(val);
end;

constructor TJSONNumber.Create(val: Int64);
begin
  Create;
  Setvalue(val);
end;

constructor TJSONNumber.Create(val: String);
begin
  Create;
  Setvalue(StrToFloat(val));
end;

{ TJSONValue }

function TJSONValue.GetInternalObject: TObject;
begin
    raise Exception.Create('Abstract method');
end;

function TJSONValue.GetJsonValueType: FPJSONValueType;
begin
 raise Exception.Create('Abstract method');
end;

function TJSONValue.GetNullString: String;
begin
  Result := 'null';
end;

function TJSONValue.ToString: String;
begin
  raise Exception.Create('abstract method');
end;

function TJSONValue.Clone: TJSONValue;
{$IFDEF FPC}
var
  Parser: TJSONParser;
  obj: TJSONData;
begin
  Parser := TJSONParser.Create(ToString);
  try
    obj := Parser.Parse;
    try
      case obj.JSONType of
        jtUnknown:
          begin
            raise DBXException.Create('Invalid JSON type');
          end;
        jtNumber:
          begin
            Result := TJSONNumber.Create(obj.AsFloat);
          end;
        jtString:
          begin
            Result := TJSONString.Create(obj.AsString);
          end;
        jtBoolean:
          begin
            if obj.value then
              Result := TJSONTrue.Create
            else
              Result := TJSONFalse.Create;
          end;
        jtNull:
          begin
            Result := TJSONNull.Create;
          end;
        jtArray:
          begin
            Result := TJSONArray.Create(obj as TFPJSONArray);
          end;
        jtObject:
          begin
            Result := TJSONObject.Create(obj as TFPJSONObject);
          end;
      end;
    finally
      obj.Free;
    end;
  finally
    Parser.Free;
  end;
{$ELSE}

begin
  result:= nil;
{$ENDIF}
end;

{ TJSONString }

procedure TJSONString.Setvalue(const AValue: string);
begin
  FIsNull := false;
  if FValue <> AValue then
    FValue := AValue;
end;

function TJSONString.GetInternalObject: String;
begin
  Result := value;
end;

function TJSONString.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONString;
end;

function TJSONString.ToString: String;
begin
  if not FIsNull then
  begin
    {$IFDEF FPC}
    Result:= '"' + StringToJSONString(FValue) + '"';
    {$ENDIF}
  end
  else
    Result := NullString;
end;

function TJSONString.GetValue: string;
begin
  Result := FValue;
end;

constructor TJSONString.Create;
begin
  inherited;
  FIsNull := true;
  FValue := '';
end;

constructor TJSONString.Create(val: String);
begin
  Create;
  Setvalue(val);
end;

{ TJSONObject }

{$IFDEF FPC}

procedure TJSONObject.BuildElements(o: TFPJSONObject);
var
  i: Integer;
  name: string;
  obj: fpjson.TJSONData;
begin
  FElements.Clear;
  for i := 0 to o.Count - 1 do
  begin
    obj := o.Items[i];
    name := o.Names[i];
    case obj.JSONType of
      jtUnknown:
        begin
          raise DBXException.Create('Invalid JSON type');
        end;
      jtNumber:
        begin
          AddPairs(TJSONPair.Create(name, TJSONNumber.Create(obj.AsFloat)));
        end;
      jtString:
        begin
          AddPairs(TJSONPair.Create(name, TJSONString.Create(obj.AsString)));
        end;
      jtBoolean:
        begin
          if obj.value then
            AddPairs(TJSONPair.Create(name, TJSONTrue.Create))
          else
            AddPairs(TJSONPair.Create(name, TJSONFalse.Create));
        end;
      jtNull:
        begin
          AddPairs(TJSONPair.Create(name, TJSONNull.Create));
        end;
      jtArray:
        begin
          AddPairs(TJSONPair.Create(name,
            TJSONArray.Create(obj as TFPJSONArray)));
        end;
      jtObject:
        begin
          AddPairs(TJSONPair.Create(name,
            TJSONObject.Create(obj as TFPJSONObject)));
        end;
    end;
  end;
end;
{$ENDIF}
{$IFDEF FPC}

function TJSONObject.AsJSONObject: TFPJSONObject;
var
  j: TFPJSONObject;
  i: Integer;
  p: TJSONPair;
  Parser: TJSONParser;
begin
  j := TFPJSONObject.Create;
  try
    for i := 0 to FElements.Count - 1 do
    begin
      p := FElements[i];
      case p.value.GetJsonValueType of
        jvtJSONObject:
          begin
            Parser := TJSONParser.Create(p.value.ToString);
            try
              j.Add(p.name, Parser.Parse);
            finally
              Parser.Free;
            end;
          end;
        jvtJSONArray:
          begin
            Parser := TJSONParser.Create(p.value.ToString);
            try
              j.Add(p.name, fpjson.TJSONArray(Parser.Parse));
            finally
              Parser.Free;
            end;
          end;
        jvtJSONString:
          begin
            j.Add(p.name, TJSONString(p.value).GetValue);
          end;
        jvtJSONNumber:
          begin
            if ceil(TJSONNumber(p.value).GetValue) = TJSONNumber(p.value).GetValue
            then
              j.Add(p.name, TJSONNumber(p.value).GetIntValue)
            else
              j.Add(p.name, TJSONNumber(p.value).GetValue);
          end;
        jvtJSONTrue:
          begin
            j.Add(p.name, true);
          end;
        jvtJSONFalse:
          begin
            j.Add(p.name, false);
          end;
        jvtJSONNull:
          begin
            j.Add(p.name, TFPJSONNull.Create);
          end;
      end;
    end;
    Result := j;
  except
    on e: exception do
    begin
      j.Free;
      raise exception.Create('Error to asJSONObject ' + e.Message);
    end;
  end;
end;
{$ENDIF}

function TJSONObject.GetJSONArray(name: String): TJSONArray;
var
  p: TJSONPair;
begin
  result:= nil;
  p := Get(name);
  if Assigned(p) then
    Result := TJSONArray(p.value);
end;

function TJSONObject.GetJSONObject(name: String): TJSONObject;
var
  p: TJSONPair;
begin
  p := Get(name);
  if Assigned(p) then
    Result := TJSONObject(p.value)
  else
    Result := nil;
end;

function TJSONObject.GetJsonValueType: FPJSONValueType;
begin
  Result := jvtJSONObject;
end;

{$IFDEF FPC}

function TJSONObject.GetInternalObject: TFPJSONObject;
begin
  Result := AsJSONObject;
end;
{$ENDIF}

class function TJSONObject.Parse(value: String): TJSONObject;
{$IFDEF FPC}
var
  p: TJSONParser;
  o: TJSONData;
{$ENDIF}
begin
  Result := nil;
{$IFDEF FPC}
  p := TJSONParser.Create(value);
  try
    o := p.Parse;
    try
      if Assigned(o) then
      begin
        if o is TFPJSONObject then
          Result := TJSONObject.Create(o as TFPJSONObject);
      end;
    finally
      FreeAndNil(o);
    end;
  finally
    p.Free;
  end;
{$ENDIF}
end;

function TJSONObject.GetString(name: String): String;
var
  p: TJSONPair;
begin
  p := Get(name);
  if Assigned(p) then
    Result := TJSONString(p.value).GetValue
  else
    Result := '';
end;

function TJSONObject.Has(name: String): Boolean;
begin
  Result := Get(name) <> nil;
end;

function TJSONObject.Get(name: String): TJSONPair;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FElements.Count - 1 do
  begin
    if name = FElements[i].name then
    begin
      Result := FElements[i];
      break;
    end;
  end;
end;

function TJSONObject.GetBoolean(name: String): Boolean;
var
  p: TJSONPair;
begin
  result:= false;
  p := Get(name);
  if p.value is TJSONTrue then
    Result := true
  else if p.value is TJSONFalse then
    Result := false;
end;

function TJSONObject.GetDouble(name: String): Double;
var
  p: TJSONPair;
begin
  p := Get(name);
  if Assigned(p) then
    Result := TJSONNumber(p.value).GetValue
  else
    Result := 0;
end;

function TJSONObject.GetInt(name: String): Integer;
begin
  Result := trunc(GetDouble(name));
end;

function TJSONObject.ToString: String;
{$IFDEF FPC}
var
  o: TFPJSONObject;
{$ENDIF}
begin
{$IFDEF FPC}
  o := AsJSONObject;
  try
    Result := o.AsJSON;
  finally
    o.Free;
  end;
{$ELSE}
  Result := '';
{$ENDIF}
end;

function TJSONObject.AddPairs(pair: TJSONPair): TJSONObject;
begin
  result:= self;
  FElements.Add(pair);

end;

function TJSONObject.AddPairs(name: String; value: TJSONValue): TJSONObject;
begin
  Result := AddPairs(TJSONPair.Create(name, value));
end;

constructor TJSONObject.Create;
begin
  inherited;
  FElements := TJSONPairList.Create(true);
end;

{$IFDEF FPC}

constructor TJSONObject.Create(json: TFPJSONObject);
begin
  Create;
  // FElements := TJSONPairList.Create(true);
  BuildElements(json);
end;
{$ENDIF}

function TJSONObject.AddPairs(name: String; value: Integer): TJSONObject;
begin
  Result := AddPairs(TJSONPair.Create(name, TJSONNumber.Create(value)));
end;

function TJSONObject.AddPairs(name, value: String): TJSONObject;
begin
  Result := AddPairs(TJSONPair.Create(name, TJSONString.Create(value)));
end;

constructor TJSONObject.Create(pair: TJSONPair);
begin
  Create;
  AddPairs(pair);
end;

destructor TJSONObject.Destroy;
begin
{$IFDEF FPC}
  FElements.Free;
{$ENDIF}
  inherited Destroy;
end;

{ TJSONPair }

procedure TJSONPair.Setname(const AValue: String);
begin
  if Fname = AValue then
    exit;
  Fname := AValue;
end;

procedure TJSONPair.Setvalue(const AValue: TJSONValue);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
end;

constructor TJSONPair.Create(PairName: String; PairValue: TJSONValue);
begin
  Setname(PairName);
  Setvalue(PairValue);
end;

constructor TJSONPair.Create(PairName: String; PairValue: String);
begin
  Setname(PairName);
  Setvalue(TJSONString.Create(PairValue));
end;

destructor TJSONPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TJSONObject.AddPairs(name: String; value: Int64): TJSONObject;
begin
  Result := AddPairs(TJSONPair.Create(name, TJSONNumber.Create(value)));
end;

function TJSONObject.AddPairs(name: String; value: Double): TJSONObject;
begin
  Result := AddPairs(TJSONPair.Create(name, TJSONNumber.Create(value)));
end;

function TJSONObject.AddPairs(name: String; value: Boolean): TJSONObject;
begin
  if value then
    Result := AddPairs(TJSONPair.Create(name, TJSONTrue.Create))
  else
    Result := AddPairs(TJSONPair.Create(name, TJSONFalse.Create));
end;

{ TJSONPairList }

function TJSONPairList.Add: TJSONPair;
begin
  Result := TJSONPair.Create;
  Add(Result);
end;

procedure TJSONPairList.Add(AObject: TJSONPair);
begin
  FObjectList.Add(AObject);
end;

procedure TJSONPairList.Clear;
begin
  FObjectList.Clear;
end;

function TJSONPairList.Count: Cardinal;
begin
  Result := FObjectList.Count;
end;

constructor TJSONPairList.Create(FreeObjects: Boolean);
begin
  inherited Create;
  FObjectList := TObjectList.Create(FreeObjects);
end;

constructor TJSONPairList.Create;
begin
  inherited;
  FObjectList := TObjectList.Create(true);
end;

procedure TJSONPairList.Delete(Index: Cardinal);
begin
  FObjectList.Delete(Index);
end;

destructor TJSONPairList.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TJSONPairList.GetItems(Index: Cardinal): TJSONPair;
begin
  Result := TJSONPair(FObjectList[Index]);
end;

procedure TJSONPairList.SetItems(Index: Cardinal; const value: TJSONPair);
begin
  FObjectList[Index] := value;
end;

{ TJSONValueList }

function TJSONValueList.Add: TJSONValue;
begin
  Result := TJSONValue.Create;
  Add(Result);
end;

procedure TJSONValueList.Add(AObject: TJSONValue);
begin
  FObjectList.Add(AObject);
end;

procedure TJSONValueList.Clear;
begin
  FObjectList.Clear;
end;

function TJSONValueList.Count: Cardinal;
begin
  Result := FObjectList.Count;
end;

constructor TJSONValueList.Create(FreeObjects: Boolean);
begin
  inherited Create;
  FObjectList := TObjectList.Create(FreeObjects);
end;

constructor TJSONValueList.Create;
begin
  inherited;
  FObjectList := TObjectList.Create(true);
end;

procedure TJSONValueList.Delete(Index: Cardinal);
begin
  FObjectList.Delete(Index);
end;

destructor TJSONValueList.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TJSONValueList.GetItems(Index: Cardinal): TJSONValue;
begin
  Result := TJSONValue(FObjectList[Index]);
end;

procedure TJSONValueList.SetItems(Index: Cardinal; const value: TJSONValue);
begin
  FObjectList[Index] := value;
end;

end.
