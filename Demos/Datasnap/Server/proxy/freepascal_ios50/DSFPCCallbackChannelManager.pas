//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSFPCCallbackChannelManager;

{$IFDEF FPC}
{$mode DELPHI}
{$modeswitch objectivec1}
{$codepage utf8}
{$ENDIF}

{$I dsrestdefines.inc}

interface

uses
  DSRESTConnection, DSAdmin, DBXFPCJSON,
  SysUtils, Classes, DBXFPCCallback, FPCStrings
{$IFNDEF FPC}
  , Winapi.Windows
{$ELSE}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  , cthreads
  {$ENDIF}{$ENDIF}
{$ENDIF};

/// Handle callback communications.
type
  TDSFPCCallbackChannelManager = class;
  TDSFPCExceptionEvent = procedure(Sender: TObject;
    Manager: TDSFPCCallbackChannelManager; E: Exception) of object;

  // / CALLBACKs WORKER THREAD
  // ///////////////////////////////////////////////////////

  { TWorkerThread }

  TWorkerThread = class(TThread)
  public
    constructor Create(CallbackId: String; Callback: TDBXFPCCallback;
      Connection: TDSRESTConnection; mngr: TDSFPCCallbackChannelManager);
    destructor Destroy; override;
    procedure RemoveCallback(CallbackId: String);
    procedure AddCallback(CallbackId: String; Callback: TDBXFPCCallback);
    procedure Terminate;
    function GetSessionCreated: Boolean;
    function GetConnection: TDSRESTConnection;
  protected
    procedure Execute; override;
  private
    FSessionCreated: boolean;
    FStopped: Boolean;
    FDSAdmin: TDSAdmin;
    FManager: TDSFPCCallbackChannelManager;
    Callbacks: TStringList;
    FirstCallback: String;
    procedure ExecuteCallback(Arg: TJSONObject);
    procedure InvokeEvent(json: TJSONObject);
    procedure BroadcastEvent(json: TJSONObject);
    function ChannelCallbackExecute: TJSONObject;
  end;

  TDSFPCCallbackChannelManager = class
  private
    ChannelName: String;
    MaxRetries: Integer;
    RetryDelay: Integer;
    ManagerID: String;
    wThread: TWorkerThread;
    Connection: TDSRESTConnection;
    FDSAdmin: TDSAdmin;
    CS: TRTLCriticalSection;
    FSecurityToken: String;
    FMaxRetries: Integer;
    FRetryDelay: Integer;
    FOnException: TDSFPCExceptionEvent;
    function RegisterClientCallback(CallbackId: String): boolean;
    procedure Initialize(AConnection: TDSRESTConnection;
      AChannelName, AManagerID: String);
    procedure DoOnException(mngr: TDSFPCCallbackChannelManager; E: Exception);
  public
    constructor Create(Connection: TDSRESTConnection;
      ChannelName, ManagerID: String); overload;
    constructor Create(Connection: TDSRESTConnection; ChannelName: String);
      overload;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    function RegisterCallback(CallbackId: String;
      Callback: TDBXFPCCallback): boolean;
    function CloseClientChannel: boolean;
    procedure Stop;
    function NotifyCallback(CallbackId: String; Msg: TJSONValue;
      out Response: TJSONValue): boolean;
    function BroadcastToChannel(Msg: TJSONValue): boolean;
    function UnRegisterCallback(CallbackId: String): boolean;
    function GetChannelName: string;
    function GetManagerID: String;
    function GetSecurityToken: String;
    procedure SetMaxRetries(MaxRetries: Integer);
    function GetMaxRetries: Integer;
    procedure SetRetryDelay(RetryDelay: Integer);
    function GetRetryDelay: Integer;
    class function GetNewManagerID(): String;
    property OnException: TDSFPCExceptionEvent read FOnException write FOnException;
  end;

implementation

uses
  SyncObjs, DSRestTypes {$IFDEF FPC_ON_IOS}, iPhoneAll {$ENDIF};

{ TDSFPCCallbackChannelManager }

function TWorkerThread.GetConnection: TDSRESTConnection;
begin
  Result := FDSAdmin.Connection;
end;

function TDSFPCCallbackChannelManager.BroadcastToChannel
  (Msg: TJSONValue): boolean;
begin
  try
    Result := FDSAdmin.BroadcastToChannel(GetChannelName(), Msg);
  except
    on E: Exception do
    begin
      DoOnException(Self, E);
      Result := False;
    end;
  end;
end;

function TDSFPCCallbackChannelManager.CloseClientChannel: boolean;
begin
  result:= false;
  Lock;
  try
    try
      Result := FDSAdmin.CloseClientChannel(GetChannelName, GetSecurityToken());
{$WARNINGS OFF}
      wThread.Suspend;
{$WARNINGS ON}

      WThread.Free;
      result:= true;
    except
    end;
  finally
    UnLock;
  end;
end;

constructor TDSFPCCallbackChannelManager.Create(Connection: TDSRESTConnection;
  ChannelName: String);
begin
  Create(Connection, ChannelName, GetNewManagerID);
end;

constructor TDSFPCCallbackChannelManager.Create(Connection: TDSRESTConnection;
  ChannelName, ManagerID: String);
begin
  inherited Create;
  Initialize(Connection, ChannelName, ManagerID);
end;

destructor TDSFPCCallbackChannelManager.Destroy;
begin
  try
    wThread.Terminate;
  except
  end;
{$IFNDEF FPC}
  DeleteCriticalSection(CS);
{$ELSE}
  DoneCriticalSection(CS);
{$ENDIF}
  inherited;
end;

procedure TDSFPCCallbackChannelManager.DoOnException
  (mngr: TDSFPCCallbackChannelManager; E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self, mngr, E);
end;

function TDSFPCCallbackChannelManager.GetChannelName: string;
begin
  Result := ChannelName;
end;

function TDSFPCCallbackChannelManager.GetManagerID: String;
begin
  Result := ManagerID;
end;

function TDSFPCCallbackChannelManager.GetMaxRetries: Integer;
begin
  Result := FMaxRetries;
end;

class function TDSFPCCallbackChannelManager.GetNewManagerID: String;
begin
  Randomize;
  Result := IntToStr(Random(100000)) + '.' + IntToStr(Random(100000));
end;

function TDSFPCCallbackChannelManager.GetRetryDelay: Integer;
begin
  Result := FRetryDelay;
end;

function TDSFPCCallbackChannelManager.GetSecurityToken: String;
begin
  Result := FSecurityToken;
end;

procedure TDSFPCCallbackChannelManager.Initialize(AConnection: TDSRESTConnection;
  AChannelName, AManagerID: String);
begin
{$IFNDEF FPC}
  InitializeCriticalSection(CS);
{$ELSE}
  InitCriticalSection(CS);
{$ENDIF}
  MaxRetries := 5;
  RetryDelay := 1000;
  ChannelName := AChannelName;
  ManagerID := AManagerID;
  Connection := AConnection;
  Connection.ConnectionTimeout := 30000;
{$IFDEF FPC}
  FDSAdmin := TDSAdmin.Create(Connection);
{$ENDIF}
  // is not a manager id, but the security token is calculated in the same way
  FSecurityToken := GetNewManagerID + '.' + GetNewManagerID;
end;

procedure TDSFPCCallbackChannelManager.Lock;
begin
  EnterCriticalSection(CS);
end;

function TDSFPCCallbackChannelManager.NotifyCallback(CallbackId: String;
  Msg: TJSONValue; out Response: TJSONValue): boolean;
begin
  result:= false;
  try
    Result := FDSAdmin.NotifyCallback(GetManagerID(), CallbackId, Msg, Response);
  except
    on E: Exception do
    begin
      DoOnException(Self, E);
    end;
  end;
end;

function TDSFPCCallbackChannelManager.RegisterCallback(CallbackId: String;
  Callback: TDBXFPCCallback): boolean;
begin
  Lock;
  try
    Result := True;
    if not Assigned(wThread) then
    begin
      wThread := TWorkerThread.Create(CallbackId, Callback,
        Connection.Clone(True), Self);
      wThread.FreeOnTerminate := False;
      wThread.Start;
    end
    else
    begin
      if wThread.GetSessionCreated then
      begin
        wThread.AddCallback(CallbackId, Callback);
        if not RegisterClientCallback(CallbackId) then
        begin
          wThread.RemoveCallback(CallbackId);
          Result := False;
        end;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDSFPCCallbackChannelManager.RegisterClientCallback
  (CallbackId: String): boolean;
begin
  Result := FDSAdmin.RegisterClientCallbackServer(GetManagerID(),
    CallbackId, '', GetSecurityToken());
end;

procedure TDSFPCCallbackChannelManager.SetMaxRetries(MaxRetries: Integer);
begin
  FMaxRetries := MaxRetries;
end;

procedure TDSFPCCallbackChannelManager.SetRetryDelay(RetryDelay: Integer);
begin
  FRetryDelay := RetryDelay;
end;

procedure TDSFPCCallbackChannelManager.Stop;
begin
  CloseClientChannel;
end;


procedure TDSFPCCallbackChannelManager.UnLock;
begin
  LeaveCriticalSection(CS);
end;

function TDSFPCCallbackChannelManager.UnRegisterCallback
  (CallbackId: String): boolean;
begin
  Lock;
  try
    Result := False;
    try
      Result := FDSAdmin.UnregisterClientCallback(ChannelName,
        CallbackId, GetSecurityToken);
      wThread.RemoveCallback(CallbackId);
    except
      on E: Exception do
        DoOnException(Self, E);
    end;
  finally
    UnLock;
  end;
end;

{ TWorkerThread }

procedure TWorkerThread.AddCallback(CallbackId: String; Callback: TDBXFPCCallback);
begin
  FManager.Lock;
  try
    Callbacks.AddObject(CallbackId, Callback);
  finally
    FManager.UnLock;
  end;
end;

procedure TWorkerThread.BroadcastEvent(json: TJSONObject);
var
  arr: TJSONArray;
  Value: TJSONValue;
  n: Integer;
  I: Integer;
  cb: TDBXFPCCallback;
begin
  try
    arr := json.GetJSONArray('broadcast');
    Value := arr.Get(0);
    n := arr.GetInt(1);
    for I := 0 to Callbacks.Count - 1 do
    begin
      cb := TDBXFPCCallback(Callbacks.Objects[I]);
      if Assigned(cb) then
        cb.Execute(Value, n)
      else
        raise DBXException.Create('Invalid callback name');
    end;
  except
    on E: Exception do
      if not FStopped then
        FManager.DoOnException(FManager, E);
  end;
end;

function TWorkerThread.ChannelCallbackExecute: TJSONObject;
var
  res, Value: TJSONValue;
  lastRequestAttempt: TDateTime;
  retries: Integer;
begin
  try
    res:= nil;
    retries := 0;
    lastRequestAttempt := Now;
    while not FStopped do
    begin
      try
        lastRequestAttempt := Now;
        Value := TJSONTrue.Create;
        try
          res := FDSAdmin.ConsumeClientChannel(FManager.GetChannelName,
            FManager.GetManagerID, '', '', FManager.GetSecurityToken, Value);
          break;
        finally
          Value.Free;
        end;
      except
        on E: Exception do
        begin

          if (now - lastRequestAttempt >= FDSAdmin.Connection.ConnectionTimeout +
            1000) then
            retries := 0;

          if retries = FManager.MaxRetries then
          begin
            FManager.DoOnException(FManager, E);
            res := nil;
            break;
          end;

          Inc(retries);
          Sleep(FManager.RetryDelay);
        end;
      end;
    end;
      Result := TJSONObject(res)
  except
    on E: Exception do
      raise DBXException.Create(E.Message);
  end;
end;

constructor TWorkerThread.Create(CallbackId: String; Callback: TDBXFPCCallback;
  Connection: TDSRESTConnection; mngr: TDSFPCCallbackChannelManager);
begin
  inherited Create(True);
  FSessionCreated := False;
  FDSAdmin := TDSAdmin.Create(Connection);
  Self.FManager := mngr;
  Callbacks := TStringList.Create;
  FirstCallback := CallbackId;
  AddCallback(CallbackId, Callback);
end;

destructor TWorkerThread.Destroy;
var
  NeedToCloseChannel: boolean;
begin
  FManager.Lock;
  try
    NeedToCloseChannel := Callbacks.Count > 0;
    while Callbacks.Count > 0 do
    begin
      try
        RemoveCallback(Callbacks[0]);
      except
      end;
      Callbacks.Objects[0].Free;
      Callbacks.Delete(0);
    end;
    if NeedToCloseChannel then
      try
        FDSAdmin.CloseClientChannel(FManager.ChannelName,
          FManager.GetSecurityToken())
      except
      end;
    Callbacks.Free;
  finally
    FManager.UnLock;
  end;
  inherited;
end;

procedure TWorkerThread.ExecuteCallback(Arg: TJSONObject);
begin
  try
    if Arg.Has('broadcast') then
      BroadcastEvent(Arg)
    else if Arg.Has('invoke') then
      InvokeEvent(Arg)
    else if Arg.Has('close') then
      FStopped := Arg.GetBoolean('close')
    else
      raise DBXException.Create('Invalid callback result type [' + Arg.ToString + ']');
  except
    on E: Exception do
      raise DBXException.Create(E.Message);
  end;
end;

procedure TWorkerThread.InvokeEvent(json: TJSONObject);
var
  arr: TJSONArray;
  CallbackId: String;
  v: TJSONValue;
  n: Integer;
  cb: TDBXFPCCallback;
begin
  try
    arr := json.GetJSONArray('invoke');
    CallbackId := arr.GetJSONString(0).GetValue;
    v := arr.Get(1);
    n := arr.GetInt(2);
    cb := TDBXFPCCallback(Callbacks.Objects[Callbacks.IndexOf(CallbackId)]);
    if Assigned(cb) then
      cb.Execute(v, n)
    else
      raise DBXException.Create('Invalid callback response');
  except
    on E: Exception do
      raise DBXException.Create(E.Message);

  end;
end;

procedure TWorkerThread.RemoveCallback(CallbackId: String);
begin
  FManager.Lock;
  try
    Callbacks.Objects[Callbacks.IndexOf(CallbackId)].Free;
    Callbacks.Delete(Callbacks.IndexOf(CallbackId));
  finally
    FManager.UnLock
  end;
end;

procedure TWorkerThread.Execute;
var
  t, res: TJSONValue;
  Response: boolean;
  jobj: TJSONObject;
  {$IFDEF FPC_ON_IOS}
  pool: NSAutoreleasePool;
  {$ENDIF}
begin
  {$IFDEF FPC_ON_IOS}
  pool := NSAutoreleasePool.alloc.init;
  try
  {$ENDIF}
    FStopped := False;
    try
      FManager.Lock;
      try
        t := TJSONTrue.Create;
        try
          res := FDSAdmin.ConsumeClientChannel(FManager.ChannelName,
            FManager.ManagerID, FirstCallback, '', FManager.GetSecurityToken(), t);
          try
            if TJSONObject(res).Has('error') then
              raise DBXException.Create('Cannot create callback (' +
                TJSONObject(res).GetString('error') + ')');
            Response := TJSONObject(res).GetJSONArray('invoke').GetJSONObject(1)
              .GetBoolean('created');
            if not Response then
              raise DBXException.Create('Cannot create callback');
            FSessionCreated := True;
          finally
            res.Free;
          end;
        finally
          t.Free;
        end;
      finally
        FManager.UnLock;
      end;

      //This is the callbacks main loop
      while not FStopped do
      begin
        jobj := ChannelCallbackExecute;
        try
          if Assigned(jobj) then
          begin
            FManager.Lock();
            try
              ExecuteCallback(jobj);
            finally
              FManager.UnLock();
            end;
          end;
        finally
          jobj.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        if not FStopped then
        begin
          FStopped := True;
        end;
      end;
    end;
  {$IFDEF FPC_ON_IOS}
  finally
    pool.Release;
  end;
  {$ENDIF}
end;

procedure TWorkerThread.Terminate;
begin
  FStopped := True;
end;

function TWorkerThread.GetSessionCreated: Boolean;
begin
  Result := FSessionCreated;
end;

end.

