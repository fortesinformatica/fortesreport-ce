//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSRESTConnection;

{$IFDEF FPC}
{$mode DELPHI}
{$IFNDEF WINDOWS}
{$modeswitch objectivec2}
{$ENDIF}
{$ENDIF}
{$I dsrestdefines.inc}

interface

uses
  SysUtils, Types, Classes,
  DSRestParameter,
  DSRESTParameterMetaData,
  Contnrs,
  DSRestTypes,
  DBXFPCJSON

{$IFDEF FPC}
    , DBXConnection, iPhoneAll
{$ENDIF};

type
  TDSRESTConnection = class;
  TRequestTypes = (GET, POST, PUT, Delete);
  TDSRESTParameterList = class;

  TDSRESTCommand = class(TObject)
  private
    FConnection: TDSRESTConnection;
    FFullyQualifiedMethodName: string;
    FParameters: TDSRESTParameterList;

    FText: string;
    FRequestType: TRequestTypes;

  public
    constructor Create(aConn: TDSRESTConnection); reintroduce; virtual;
    destructor Destroy; override;
    procedure Prepare(metadatas: TDSRESTParameterMetaDataArray);
    procedure Execute;

    property Connection: TDSRESTConnection read FConnection write FConnection;
    property FullyQualifiedMethodName: string read FFullyQualifiedMethodName
      write FFullyQualifiedMethodName;
    property Parameters: TDSRESTParameterList read FParameters
      write FParameters;
    property Text: string read FText write FText;
    property RequestType: TRequestTypes read FRequestType write FRequestType;

  end;

  TDSRESTConnection = class(TComponent)
  private
    FPort: integer;
    FUrlPath: string;
    FHost: String;
    FProtocol: String;
    FContext: string;
    FSessionID: String;
    FSessionIDExpires: LongInt;
    FUserName: string;
    FPassword: string;
    FConnectionTimeout: integer;
{$IFDEF FPC_ON_IOS}
    FListner: TConnectionEventLister;
    FInternalDelegate: TDBXConnectionEventHook;
{$ENDIF}
  protected
{$IFDEF FPC_ON_IOS}
    function GetInnerDelegate: TDBXConnectionEventHook;
{$ENDIF}
    function isUrlParameter(parameter: TDSRestParameter): Boolean;
{$IFDEF FPC}
    function buildRequestURLNS(command: TDSRESTCommand): NSMutableString;
    function encodeURIComponentWithParamNS(parameter: TDSRestParameter)
      : NSString;
    function encodeURIComponentNS(Value: NSString): NSString;
    procedure SetUpSessionHeader(var request: NSMutableURLRequest);
    procedure setUpAuthorizationHeader(var request: NSMutableURLRequest);
    procedure setUpHeader(var request: NSMutableURLRequest);
    function CreateRequestNS(command: TDSRESTCommand): NSMutableURLRequest;
    function BuildRequestNS(arequestType: TRequestTypes; aUrl: NSString;
      Parameters: TJSONArray): NSMutableURLRequest;
    procedure setSessionIdentifier(response: NSHTTPURLResponse);
    function SafeParseResponse(response: NSHTTPURLResponse; err: NSError;
      jsonStr: String): TJSONObject;
    function NSDataToStream(Value: NSData): TStream;
{$ENDIF}
    function isThereOnlyOneStreamInOutput(Parameters
      : TDSRESTParameterList): Boolean;
    function isOnlyOneParameterInOutput(Parameters
      : TDSRESTParameterList): Boolean;

    procedure throwJsonExceptionIfNeed(json: TJSONObject; athrowEx: Boolean);
    procedure CloseSession;
  public
    constructor Create(AOwner: TComponent); override;
    function Clone(includeSession: Boolean): TDSRESTConnection;
    function CreateCommand: TDSRESTCommand;
    procedure Execute(cmd: TDSRESTCommand);

    property Port: integer read FPort write FPort;
    property UrlPath: string read FUrlPath write FUrlPath;
    property Host: String read FHost write FHost;
    property Protocol: String read FProtocol write FProtocol;
    property Context: string read FContext write FContext;
    property SessionID: String read FSessionID write FSessionID;
    property SessionIDExpires: LongInt read FSessionIDExpires
      write FSessionIDExpires;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property ConnectionTimeout: integer read FConnectionTimeout
      write FConnectionTimeout;
{$IFDEF FPC_ON_IOS}
    property Listener: TConnectionEventLister read FListner write FListner;
{$ENDIF}
  end;

  TDSRESTParameterList = class
  private
    FList: TObjectList;
    FOwnsObjects: Boolean;
    function getItems(index: integer): TDSRestParameter;
    procedure SetOwnsObjects(const Value: Boolean);
  public

    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function Add(Value: TDSRestParameter): TDSRestParameter;
    property OwnsObjects: Boolean read FOwnsObjects write SetOwnsObjects;
    property Items[index: integer]: TDSRestParameter read getItems;
  end;

implementation

uses
  DBXDefaultFormatter, DBXJsonTools, DBXValue, FPCStrings
{$IFDEF FPC_ON_IOS} , CFURL, CFString{$ENDIF};

{$IFDEF FPC_ON_IOS}

function EncodeUrlNS(Value: string): NSString;

begin
  Result := NSString(CFURLCreateStringByAddingPercentEscapes(nil,
    CFSTR(PChar(Value)), nil, CFSTR(PChar('!*();:@&=+$,/?%#[]''')),
    kCFStringEncodingUTF8));
  Result.autorelease;
end;

function EncodeUrlNS2(Value: NSString): NSString;

begin
  Result := NSString(CFURLCreateStringByAddingPercentEscapes(nil,
    CFSTR(Value.UTF8String), nil, CFSTR(PChar('!*();:@&=+$,/?%#[]''')),
    kCFStringEncodingUTF8));
  Result.autorelease;
end;

function TDSRESTConnection.encodeURIComponentNS(Value: NSString): NSString;
begin
  Result := EncodeUrlNS2(Value);
end;

function TDSRESTConnection.buildRequestURLNS(command: TDSRESTCommand)
  : NSMutableString;

var
  LPathPrefix, LMethodName, LUrl: NSMutableString;
  LPort: integer;
  LProtocol, LHost, LPortString, LContext: NSString;
  replaceRange, replaceRange2: NSRange;
begin

  LPathPrefix := NSMutableString.alloc.initWithCapacity(0);
  if (Trim(FUrlPath) <> '') then
    LPathPrefix.appendString(StringToNS(FUrlPath));

  LPort := FPort;

  LMethodName := NSMutableString.alloc.initWithString(StringToNS(command.Text));

  LProtocol := NSString.alloc.initWithString(StringToNS(FProtocol));

  if (LProtocol.isEqualToString(StringToNS(''))) then
  begin
    LProtocol.Release;
    LProtocol := StringToNS('http').retain;
  end;

  LHost := NSString.alloc.initWithString(StringToNS(FHost));
  if (LHost.isEqualToString(StringToNS(''))) then
  begin
    LHost.Release;
    LHost := StringToNS('localhost').retain;
  end;
  if (not LPathPrefix.isEqualToString(StringToNS(''))) then
    LPathPrefix.insertString_atIndex(StringToNS('/'), 0);
  LPortString := StringToNS('');
  if (LPort > 0) then
  begin
    LPortString.Release;
    LPortString := NSString.alloc.initWithString
      (StringToNS(':' + IntToStr(LPort)));
  end;
  if (command.RequestType in [GET, Delete]) then
  begin
    replaceRange := LMethodName.rangeOfString(StringToNS('.'));
    if (replaceRange.location <> NSNotFound) then
      LMethodName.replaceCharactersInRange_WithString(replaceRange,
        StringToNS('/'));

    replaceRange2 := LMethodName.rangeOfString(LMethodName);
    LMethodName.replaceOccurrencesOfString_withString_options_range
      (StringToNS('\\'), StringToNS('\%22'), NSCaseInsensitiveSearch,
      replaceRange2);

  end
  else
  begin
    replaceRange := LMethodName.rangeOfString(StringToNS('.'));
    if (replaceRange.location <> NSNotFound) then
      LMethodName.replaceCharactersInRange_WithString(replaceRange,
        StringToNS('/%22'));

    replaceRange2 := LMethodName.rangeOfString(LMethodName);
    LMethodName.replaceOccurrencesOfString_withString_options_range
      (StringToNS('\'), StringToNS('\%22'), NSCaseInsensitiveSearch,
      replaceRange2);
    LMethodName.appendString(StringToNS('%22'));
  end;

  LContext := NSString.alloc.initWithString(StringToNS(FContext));
  if (LContext.isEqualToString(StringToNS(''))) then
  begin
    LContext.Release;
    LContext := StringToNS('datasnap/').retain;
  end;
  LUrl := NSMutableString.alloc.initWithCapacity(0);
  LUrl.appendString(LProtocol);
  LUrl.appendString(StringToNS('://'));
  LUrl.appendString(encodeURIComponentNS(LHost));
  LUrl.appendString(LPortString);
  LUrl.appendString(LPathPrefix);
  LUrl.appendString(StringToNS('/'));
  LUrl.appendString(LContext);
  LUrl.appendString(StringToNS('rest/'));
  LUrl.appendString(LMethodName);

  LHost.Release;
  LProtocol.Release;
  LPortString.Release;
  LPathPrefix.Release;
  LContext.Release;
  LMethodName.Release;

  Result := LUrl.autorelease;

end;

{$ENDIF}
{ TDSRestParameterList }

function TDSRESTParameterList.Add(Value: TDSRestParameter): TDSRestParameter;
begin
  FList.Add(Value);
  Result := Value;
end;

function TDSRESTParameterList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TDSRESTParameterList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TDSRESTParameterList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TDSRESTParameterList.getItems(index: integer): TDSRestParameter;
begin
  Result := TDSRestParameter(FList.Items[index]);

end;

procedure TDSRESTParameterList.SetOwnsObjects(const Value: Boolean);
begin
  FOwnsObjects := Value;
  FList.OwnsObjects := Value;
end;

{ TDSRestCommand }

constructor TDSRESTCommand.Create(aConn: TDSRESTConnection);
begin
  inherited Create;
  FConnection := aConn;
  FParameters := TDSRESTParameterList.Create;
  FParameters.OwnsObjects := True;
end;

destructor TDSRESTCommand.Destroy;
begin
  FParameters.Free;
  inherited;

end;

procedure TDSRESTCommand.Execute;
begin
  FConnection.Execute(self);
end;

procedure TDSRESTCommand.Prepare(metadatas: TDSRESTParameterMetaDataArray);
var
  p: TDSRestParameter;
  param: TDSRESTParameterMetaData;
begin
  for param in metadatas do
  begin
    p := TDSRestParameter.Create;
    p.Direction := param.Direction;
    p.DBXType := param.DBXType;
    p.TypeName := param.TypeName;
    p.Value.DBXType := param.DBXType;
    Parameters.Add(p);
  end;

end;

{ TDSREstConnection }

procedure TDSRESTConnection.CloseSession;
begin
  FSessionID := '';
  FSessionIDExpires := -1;
end;

constructor TDSRESTConnection.Create(AOwner: TComponent);
begin
  inherited;
  FConnectionTimeout := 30;
end;

function TDSRESTConnection.CreateCommand: TDSRESTCommand;
begin
  Result := TDSRESTCommand.Create(self);
end;

procedure TDSRESTConnection.throwJsonExceptionIfNeed(json: TJSONObject;
  athrowEx: Boolean);
var
  msg: String;
begin
  if json.has('error') then
  begin
    msg := json.getString('error');
    raise DBXException.Create(msg);
  end
  else if json.has('SessionExpired') then
  begin
    CloseSession;
    msg := json.getString('SessionExpired');

    raise DBXException.Create(msg);
  end;
end;

{$IFDEF FPC}

function TDSRESTConnection.encodeURIComponentWithParamNS
  (parameter: TDSRestParameter): NSString;
begin
  Result := encodeURIComponentNS(StringToNS(parameter.Value.ToString));

end;

function TDSRESTConnection.BuildRequestNS(arequestType: TRequestTypes;
  aUrl: NSString; Parameters: TJSONArray): NSMutableURLRequest;
var
  url, _dataString: NSString;
  nsBody: NSString;
  cmdurl: NSURL;
  _nsdata: NSData;
  jbody: TJSONObject;
begin
  cmdurl := NSURL.URLWithString(aUrl);
  Result := NSMutableURLRequest.requestWithURL(cmdurl);
  case arequestType of
    GET:
      Result.setHTTPMethod(NSStr(string('GET')));
    Delete:
      Result.setHTTPMethod(NSStr(string('DELETE')));
    POST:
      begin
        Result.setHTTPMethod(NSStr(string('POST')));
        if not assigned(Parameters) then
          raise DBXException.Create
            ('Parameters cannot be null in a POST request');
        if Parameters.size > 0 then
        begin
          jbody := TJSONObject.Create;
          try
            jbody.addPairs('_parameters', Parameters.Clone);
            nsBody := StringToNS(jbody.ToString);
          finally
            jbody.Free;
          end;

          Result.setHTTPBody(nsBody.dataUsingEncoding(NSUTF8StringEncoding));
        end;
      end;
    PUT:
      begin
        Result.setHTTPMethod(NSStr(string('POST')));
        if Parameters.size > 0 then
        begin
          nsBody := NSStr(string('body'));
          Result.setHTTPBody(nsBody.dataUsingEncoding(NSUTF8StringEncoding));
        end;

      end;
  end;
  Result.setTimeoutInterval(FConnectionTimeout);

end;

procedure TDSRESTConnection.setSessionIdentifier(response: NSHTTPURLResponse);
var
  found: Boolean;
  hd: NSDictionary;
  pragma: NSString;
  ds, er: NSArray;
  e, s: NSString;
  i: integer;
begin
  found := False;
  hd := response.allHeaderFields;
  pragma := hd.objectForKey(NSStr('Pragma'));
  if assigned(pragma) then
  begin
    ds := pragma.componentsSeparatedByString(NSStr(','));
    for i := 0 to ds.Count - 1 do
    begin
      e := NSString(ds.objectAtIndex(i));
      er := e.componentsSeparatedByString(NSStr('='));
      if (er.Count > 0) then
      begin
        s := NSString(er.objectAtIndex(0));
        if (s.isEqualToString(NSStr('dssession'))) then
        begin
          SessionID := String(NSToString(NSString(er.objectAtIndex(1))));
          found := True;
        end
        else if (s.isEqualToString(NSStr('dssessionexpires'))) then
          FSessionIDExpires := LongInt(er.objectAtIndex(1));
      end;
    end;
  end;
  if (not found) then
    CloseSession;

end;

function TDSRESTConnection.SafeParseResponse(response: NSHTTPURLResponse;
  err: NSError; jsonStr: String): TJSONObject;
var
  msg: String;
  json: TJSONObject;
begin
  Result := nil;
  if (response.isKindOfClass(NSHTTPURLResponse.classClass)) then
  begin
    if (response.statusCode <> 200) then
    begin
      try
        Result := TJSONObject.parse(jsonStr);
      except
      end;
      if Result = nil then
      begin
        msg := NSToString(NSHTTPURLResponse.localizedStringForStatusCode
          (response.statusCode));
        msg := msg + NSToString(err.userInfo.description);
        raise DBXException.Create(format('%i:%s', [response.statusCode, msg]));
      end
      else
        throwJsonExceptionIfNeed(Result, True);
    end;
  end;
  if assigned(err) then
  begin
    msg := NSToString(err.localizedDescription);
    raise DBXException.Create(msg);
  end;

end;

function TDSRESTConnection.NSDataToStream(Value: NSData): TStream;
var
  s: NSString;
  sStream: String;
begin
  Result := nil;
  s := NSString.alloc.initWithData_encoding(Value, NSUTF8StringEncoding);
  try
    sStream := NSToString(s);
    if sStream <> '' then
    begin
      Result := TStringStream.Create(sStream);
    end;
  finally
    s.Release;
  end;

end;

procedure TDSRESTConnection.SetUpSessionHeader(var request
  : NSMutableURLRequest);
var
  s: String;
begin
  if Trim(FSessionID) <> '' then
  begin
    s := 'dssession=' + FSessionID;
    request.addValue_forHTTPHeaderField(StringToNS(s), NSStr(String('Pragma')));
  end;
end;

procedure TDSRESTConnection.setUpAuthorizationHeader
  (var request: NSMutableURLRequest);
var
  s: String;
begin
  if Trim(FUserName) = '' then
    request.addValue_forHTTPHeaderField(NSStr(string('Basic Og==')),
      NSStr(String('Authorization')))
  else
  begin
    s := TDBXDefaultFormatter.getInstance.Base64Encode
      (FUserName + ':' + FPassword);
    request.addValue_forHTTPHeaderField(StringToNS(s),
      NSStr(String('Authorization')));
  end;

end;

procedure TDSRESTConnection.setUpHeader(var request: NSMutableURLRequest);
begin
  request.addValue_forHTTPHeaderField
    (NSStr(string('Mon, 1 Oct 1990 05:00:00 GMT')),
    NSStr(String('If-Modified-Since')));
  request.addValue_forHTTPHeaderField(NSStr(string('Keep-Alive')),
    NSStr(String('Connection')));
  request.addValue_forHTTPHeaderField(NSStr(string('text/plain;charset=UTF-8')),
    NSStr(String('Content-Type')));
  request.addValue_forHTTPHeaderField(NSStr(string('application/JSON')),
    NSStr(String('Accept')));
  request.addValue_forHTTPHeaderField(NSStr(string('identity')),
    NSStr(String('Accept-Encoding')));
  request.addValue_forHTTPHeaderField
    (NSStr(string('Mozilla/3.0 (compatible; Indy Library)')),
    NSStr(String('User-Agent')));
  if Trim(FSessionID) = '' then
    setUpAuthorizationHeader(request)
  else
    SetUpSessionHeader(request);

end;

function TDSRESTConnection.CreateRequestNS(command: TDSRESTCommand)
  : NSMutableURLRequest;
var
  url: NSMutableString;
  parametersToSend: TObjectList;
  _parameters: TJSONArray;
  i: integer;
  p: TDSRestParameter;
  CanAddParamsToUrl: Boolean;
begin
  Result := nil;
  url := buildRequestURLNS(command);

  parametersToSend := TObjectList.Create(False);
  _parameters := TJSONArray.Create;
  try
    for i := 0 to command.Parameters.Count - 1 do
    begin
      p := TDSRestParameter(command.Parameters.Items[i]);
      if p.Direction in [Input, InputOutput] then
      begin
        parametersToSend.Add(p);
      end;
    end;
    if command.RequestType in [GET, Delete] then
    begin
      for i := 0 to parametersToSend.Count - 1 do
      begin
        p := TDSRestParameter(parametersToSend.Items[i]);
        url.appendString(StringToNS('/'));
        url.appendString(encodeURIComponentWithParamNS(p));
      end;
    end
    else
    begin
      CanAddParamsToUrl := True;
      for i := 0 to parametersToSend.Count - 1 do
      begin
        p := TDSRestParameter(parametersToSend.Items[i]);
        if CanAddParamsToUrl and isUrlParameter(p) then
        begin
          url.appendString(StringToNS('/'));
          url.appendString(encodeURIComponentWithParamNS(p));
        end
        else
        begin
          CanAddParamsToUrl := False;
          p.Value.appendTo(_parameters);
        end;

      end;

    end;
    Result := BuildRequestNS(command.RequestType, url, _parameters);
    setUpHeader(Result);
  finally
    _parameters.Free;
    parametersToSend.Free;
  end;
end;

{$ENDIF}

function CloneStream(Value: TStream): TStream;
begin
  Result := nil;
  if assigned(Value) then
  begin
    Result := TMemoryStream.Create;
    Value.Position := 0;
    Result.CopyFrom(Value, Value.size);
    Result.Position := 0;
  end;
end;

{$IFDEF FPC_ON_IOS}

function TDSRESTConnection.GetInnerDelegate: TDBXConnectionEventHook;
begin
  if not assigned(FInternalDelegate) then
  begin
{$IFDEF FPC}
    FInternalDelegate := TDBXConnectionEventHook.alloc.init.autorelease;
{$ELSE}
    FInternalDelegate := TDBXConnectionEventHook.Create;
{$ENDIF}
    FInternalDelegate.SetListener(self.Listener);
  end;
  Result := FInternalDelegate;

end;
{$ENDIF}
{$IFNDEF FPC}

procedure TDSRESTConnection.Execute(cmd: TDSRESTCommand);
begin
  raise Exception.Create
    ('This proxy cannot be used with DELPHI on Windows. You have to use it only on FreePascal for iOS');
end;

{$ELSE}

procedure TDSRESTConnection.Execute(cmd: TDSRESTCommand);
var
  request: NSMutableURLRequest;
  urlr: NSHTTPURLResponse = nil;
  urle: NSError;
  json_string1: NSString;
  res: NSData;
  response, cpyResponse: TStream;
  json: TJSONObject;
  results: TJSONArray;
  i, returnParIndex: integer;
  v: TDBXValue;
begin
  response := nil;
  json := nil;
  request := CreateRequestNS(cmd);
  // objective c object are in autorelease
  urlr := nil;
  urle := nil;
  try
    res := TDBXConnection.
      sendSynchronousRequest_returningResponse_error_usingDelegate(request,
      NSURLResponse(urlr), urle, GetInnerDelegate);

    if not assigned(res) then
    begin
      raise DBXException.Create(NSToString(urle.localizedDescription));
    end;
    json_string1 := NSString.alloc.initWithData_encoding(res,
      NSUTF8StringEncoding);
    response := NSDataToStream(res);

    try
      setSessionIdentifier(urlr);
      try
        // try to read the error from the json response
        json := SafeParseResponse(urlr, urle, NSToString(json_string1));
        // try to find an error in the http response
        if isThereOnlyOneStreamInOutput(cmd.Parameters) then
        begin
          // Here I know that HTTP response is not an error and there isn't a json in the response
          for i := 0 to cmd.Parameters.Count - 1 do
          begin
            if (cmd.Parameters.Items[i].Direction in [ReturnValue, InputOutput,
              Output]) then
            begin

              if cmd.Parameters.Items[i].isDBXValue then
                cmd.Parameters.Items[i].Value.AsDBXValue.AsStream :=
                  CloneStream(response)
              else
                cmd.Parameters.Items[i].Value.AsStream := CloneStream(response);
            end;
          end;
        end
        else
        begin
          if not assigned(json) then
          begin
            json := TJSONObject.parse(NSToString(json_string1));
          end;
          throwJsonExceptionIfNeed(json, True);

          // Here I know that HTTP response is not an error, JSON is valid and doesn't contain an error
          results := json.getJSONArray('result');
          returnParIndex := 0;
          for i := 0 to cmd.Parameters.Count - 1 do
          begin
            if (cmd.Parameters.Items[i].Direction in [ReturnValue, InputOutput,
              Output]) then
            begin
              v := cmd.Parameters.Items[i].Value;
              TDBXJsonTools.jsonToDBX(results.GET(returnParIndex), v,
                cmd.Parameters.Items[i].TypeName);
              Inc(returnParIndex);
            end;
          end;
          FreeAndNil(json);
        end;
      finally
        FreeAndNil(json);
      end;
    finally
      response.Free;
      json_string1.Release;
    end;
  finally
    if assigned(urle) then
      urle.Release;
    if assigned(urlr) then
      urlr.Release;
  end
end;
{$ENDIF}

function TDSRESTConnection.isOnlyOneParameterInOutput
  (Parameters: TDSRESTParameterList): Boolean;
var
  i, Count: integer;

begin
  Count := 0;
  for i := 0 to Parameters.Count - 1 do
  begin
    if Parameters.Items[i].Direction in [ReturnValue, InputOutput, Output] then
    begin
      Inc(Count);
    end;

  end;
  Result := Count = 1;
end;

function TDSRESTConnection.isThereOnlyOneStreamInOutput
  (Parameters: TDSRESTParameterList): Boolean;
var
  i: integer;
begin
  Result := False;
  if isOnlyOneParameterInOutput(Parameters) then
  begin
    for i := 0 to Parameters.Count - 1 do
    begin
      if (Parameters.Items[i].Direction in [ReturnValue, InputOutput, Output])
        and (Parameters.Items[i].DBXType = BinaryBlobType) then
      begin
        Exit(True);
      end;
    end;
  end;
end;

function TDSRESTConnection.isUrlParameter(parameter: TDSRestParameter): Boolean;
begin
  Result := not(parameter.DBXType in [JSONValueType, BinaryBlobType,
    TableType]);
end;

function TDSRESTConnection.Clone(includeSession: Boolean): TDSRESTConnection;
begin
  Result := TDSRESTConnection.Create(nil);
  Result.Host := self.Host;
  Result.Port := self.Port;
  Result.Protocol := self.Protocol;
  Result.UserName := self.UserName;
  Result.Password := self.Password;
  Result.UrlPath := self.UrlPath;
  Result.ConnectionTimeout := self.ConnectionTimeout;
  if (includeSession) then
  begin
    Result.SessionID := self.SessionID;
    Result.SessionIDExpires := self.SessionIDExpires;
  end;
end;

end.
