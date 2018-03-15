//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXFPCCallback;

interface

uses
  DBXFPCJSON;

{
  Base class to implement callbacks. The class is used to implement responses
  for callbacks .
}
type
  TDBXFPCCallback = class abstract
  public
    {
      override this method to implement callbacks actions

      @param params
      @return
    }
  public
    function Execute(Value: TJSONValue; JSONType: Integer): TJSONValue;
      virtual; abstract;

  end;

implementation

end.
