//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSRestParameter;

interface
  uses
    DSRestTypes,DBXValue;
  type
   TDSRestParameter = class
   private
    FName:string;
    FDirection:TDSRESTParamDirection;
    FDBXType:TDBXDataTypes;
    FTypeName:String;
    FValue:TDBXValue;
   public
     constructor create; overload;
     constructor create(aName:string;aDirection:TDSRESTParamDirection;
     aTypeName:STring); overload;
     destructor Destroy; override;

     function isDBXValue:boolean;
     property  Name:string read FName write FName;
     property  Direction:TDSRESTParamDirection read FDirection write FDirection;
     property  DBXType:TDBXDataTypes read FDBXType write FDBXType;
     property  TypeName:String read FTypeName write FTypeName;
     property  Value:TDBXValue read FValue write FValue;


   end;

implementation
uses sysutils;

{ TDSRestParameter }
constructor TDSRestParameter.create;
begin
  inherited create;
  FValue:= TDBXWritableValue.Create;
end;

constructor TDSRestParameter.create(aName: string;
  aDirection: TDSRESTParamDirection; aTypeName: STring);
begin
  self.create;
  FName:= aName;
  FDirection:= aDirection;
  FTypeName:= aTypeName;
end;

destructor TDSRestParameter.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TDSRestParameter.isDBXValue:Boolean;
begin
 result := TypeIsDBXValue(TypeName);
end;
end.
