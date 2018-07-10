//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSRESTParameterMetaData;

interface

uses
  SysUtils, Types, Classes, Variants, DSRestTypes;

type
  TDSRESTParameterMetaData = class;
  TDSRESTParameterMetaDataArray = array of TDSRESTParameterMetaData;

  TDSRESTParameterMetaData = class
  private
    FName: String;
    FDirection: TDSRESTParamDirection;
    FDBXType: TDBXDataTypes;
    FTypeName: string;
  public
    class procedure ReleaseArray(var aArr: TDSRESTParameterMetaDataArray);
    class function CreateParam(Name: string; Direction: TDSRESTParamDirection;
      DBXType: TDBXDataTypes; TypeName: String): TDSRESTParameterMetaData;
    constructor Create(Name: string; Direction: TDSRESTParamDirection;
      DBXType: TDBXDataTypes; TypeName: String); reintroduce; virtual;
    property Direction: TDSRESTParamDirection read FDirection write FDirection;
    property DBXType: TDBXDataTypes read FDBXType write FDBXType;
    property TypeName: String read FTypeName write FTypeName;

  end;

implementation

{ TDSRESTParameterMetaData }


constructor TDSRESTParameterMetaData.Create(Name: string;
  Direction: TDSRESTParamDirection; DBXType: TDBXDataTypes; TypeName: String);
begin
  inherited Create;
  FName := Name;
  FDirection := Direction;
  FDBXType := DBXType;
  FTypeName := TypeName;
end;

class function TDSRESTParameterMetaData.CreateParam(Name: string;
  Direction: TDSRESTParamDirection; DBXType: TDBXDataTypes;
  TypeName: String): TDSRESTParameterMetaData;
begin
  Result := TDSRESTParameterMetaData.Create(Name, Direction, DBXType, TypeName);
end;

class procedure TDSRESTParameterMetaData.ReleaseArray
  (var aArr: TDSRESTParameterMetaDataArray);
var
  i: Integer;
begin
  if Length(aArr) = 0 then
    exit;
  for i := 0 to Length(aArr) - 1 do
  begin
    aArr[i].Free;
    aArr[i] := nil;
  end;
  SetLength(aArr, 0);
  Finalize(aArr);
end;

end.

