//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXConnection;
{$IFDEF FPC}
{$mode DELPHI}
{$modeswitch objectivec2}
{$ENDIF}

interface

uses

  DSRESTTypes
{$IFDEF FPC}
    , FMX_Dialogs,
  iphoneAll
{$ENDIF};

type
{$IFDEF FPC}
  TDBXConnectionEventHook = objcclass(NSObject)
{$ELSE}
    TDBXConnectionEventHook = class
{$ENDIF}
    private FListener: TConnectionEventLister;
public
  procedure SetListener(aConnection: TConnectionEventLister);
{$IFDEF FPC}
  message 'SetListener:'; {$ENDIF}
  procedure DBXconnection_didReceiveAuthenticationChallenge
    (var connection: NSURLConnection;
    var challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC}
  message 'DBXconnection:didReceiveAuthenticationChallenge:';
{$ENDIF}
  function DBXconnectionShouldUseCredentialStorage
    (var connection: NSURLConnection): boolean;
{$IFDEF FPC}
  message 'DBXconnectionShouldUseCredentialStorage:'; {$ENDIF}
  function DBXconnection_willSendRequest_redirectResponse
    (var connection: NSURLConnection; var request: NSURLRequest;
    var response: NSURLResponse): NSURLRequest;
{$IFDEF FPC}
  message 'DBXconnection:willSendRequest:redirectResponse:'; {$ENDIF}
  function DBXconnection_needNewBodyStream(var connection: NSURLConnection;
    var request: NSURLRequest): NSInputStream;
{$IFDEF FPC}
  message 'DBXconnection:needNewBodyStream:'; {$ENDIF}
  function DBXconnection_canAuthenticateAgainstProtectionSpace
    (var connection: NSURLConnection;
    var protectionSpace: NSURLProtectionSpace): boolean;
{$IFDEF FPC}
  message 'DBXconnection:canAuthenticateAgainstProtectionSpace:'; {$ENDIF}
  procedure DBXconnection_didCancelAuthenticationChallenge
    (var connection: NSURLConnection;
    var challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC}
  message 'DBXconnection:didCancelAuthenticationChallenge:'; {$ENDIF}
  procedure DBXconnection_didReceiveResponse(var connection: NSURLConnection;
    var response: NSURLResponse);
{$IFDEF FPC}
  message 'DBXconnection:didReceiveResponse:'; {$ENDIF}
  procedure DBXconnection_didReceiveData(var connection: NSURLConnection;
    var data: NSData);
{$IFDEF FPC}
  message 'DBXconnection:didReceiveData:'; {$ENDIF}
  procedure DBXconnection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
    (var connection: NSURLConnection; bytesWritten: NSInteger;
    totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger);

{$IFDEF FPC}
  message 'DBXconnection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:';
{$ENDIF}
  procedure DBXconnectionDidFinishLoading(var connection: NSURLConnection);
{$IFDEF FPC}
  message 'DBXconnectionDidFinishLoading:'; {$ENDIF}
  procedure DBXconnection_didFailWithError(var connection: NSURLConnection;
    error: NSError);
{$IFDEF FPC}
  message 'DBXconnection:didFailWithError:'; {$ENDIF}
  function DBXconnection_willCacheResponse(var connection: NSURLConnection;
    var cachedResponse: NSCachedURLResponse): NSCachedURLResponse;
{$IFDEF FPC}
  message 'DBXconnection:willCacheResponse:'; {$ENDIF}
  end;
(* Connection metods have changed from 4.2 to 5.0:
   in 4.2 they are in a category and need to be overriden
   in 5.0 are protocols and don't need to be overriden
   
 *)
{$IFDEF FPC}
  TDBXConnection = objcclass(NSObject)
{$ELSE}
  TDBXConnection = class
{$ENDIF}
    private
{$IFDEF FPC}
    dbxconn: NSURLConnection;

  internalDelegate: TDBXConnectionEventHook;
  complete, cancelled: boolean;
  receivedData: NSMutableData;
  urlresponse: ^NSURLResponse;
  urlerror: ^NSError;
{$ENDIF}
protected
  procedure start;
{$IFDEF FPC}
  message 'start';
{$ENDIF}
public
  isSynchronous: boolean;

  class
  function sendSynchronousRequest_returningResponse_error_usingDelegate
    (request: NSURLRequest; var response: NSURLResponse; var error: NSError;
    aDelegate: TDBXConnectionEventHook): NSData;
{$IFDEF FPC} message 'sendSynchronousRequest:returningResponse:error:usingDelegate:'; {$ENDIF}
  function InitWithRequest_returningResponse_error_usingDelegate
    (request: NSURLRequest; var response: NSURLResponse; var error: NSError;
    aDelegate: TDBXConnectionEventHook): id;
{$IFDEF FPC} message 'initWithRequest:returningResponse:error:usingDelegate:';
{$ENDIF}
  function getReceivedData: NSMutableData;
{$IFDEF FPC}message 'getReceivedData'; {$ENDIF}
  procedure dealloc;
{$IFDEF FPC} override; {$ENDIF}
  function connection_willSendRequest_redirectResponse
    (connection: NSURLConnection; request: NSURLRequest;
    response: NSURLResponse): NSURLRequest;
{$IFDEF FPC} override;
  message 'connection:willSendRequest:redirectResponse:'; {$ENDIF}
  function connection_needNewBodyStream(connection: NSURLConnection;
    request: NSURLRequest): NSInputStream;
{$IFDEF FPC} override;
  message 'connection:needNewBodyStream:'; {$ENDIF}
  function connection_canAuthenticateAgainstProtectionSpace
    (connection: NSURLConnection;
    protectionSpace: NSURLProtectionSpace): boolean;
{$IFDEF FPC} override;
  message 'connection:canAuthenticateAgainstProtectionSpace:'; {$ENDIF}
  procedure connection_didReceiveAuthenticationChallenge
    (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC}  override;
  message 'connection:didReceiveAuthenticationChallenge:'; {$ENDIF}
  procedure connection_didCancelAuthenticationChallenge
    (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC} override;
  message 'connection:didCancelAuthenticationChallenge:'; {$ENDIF}
  function connectionShouldUseCredentialStorage
    (connection: NSURLConnection): boolean;
{$IFDEF FPC} override;
  message 'connectionShouldUseCredentialStorage:'; {$ENDIF}
  procedure connection_didReceiveResponse(connection: NSURLConnection;
    response: NSURLResponse);
{$IFDEF FPC} override;
  message 'connection:didReceiveResponse:'; {$ENDIF}
  procedure connection_didReceiveData(connection: NSURLConnection;
    data: NSData);
{$IFDEF FPC} override;
  message 'connection:didReceiveData:'; {$ENDIF}
  procedure connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
    (connection: NSURLConnection; bytesWritten: NSInteger;
    totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger);
{$IFDEF FPC} override;
  message 'connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:';
{$ENDIF}
  procedure connectionDidFinishLoading(connection: NSURLConnection);
{$IFDEF FPC} override;
  message 'connectionDidFinishLoading:'; {$ENDIF}
  procedure connection_didFailWithError(connection: NSURLConnection;
    error: NSError);
{$IFDEF FPC} override;
  message 'connection:didFailWithError:'; {$ENDIF}
  function connection_willCacheResponse(connection: NSURLConnection;
    cachedResponse: NSCachedURLResponse): NSCachedURLResponse;
{$IFDEF FPC} override;
  message 'connection:willCacheResponse:'; {$ENDIF}
  end;

implementation

class function TDBXConnection.
  sendSynchronousRequest_returningResponse_error_usingDelegate
  (request: NSURLRequest; var response: NSURLResponse; var error: NSError;
  aDelegate: TDBXConnectionEventHook): NSData;
{$IFDEF FPC}
var
  con: TDBXConnection;
begin
  con := TDBXConnection.alloc;
  con.isSynchronous := true;
  con.InitWithRequest_returningResponse_error_usingDelegate(request, response,
    error, aDelegate);
  try
    con.start;

    result := NSData.dataWithData(con.getReceivedData);
  finally
    con.release;
  end;
{$ELSE}
begin
  result := nil;
{$ENDIF}
end;

function TDBXConnection.getReceivedData: NSMutableData;
begin
{$IFDEF FPC}
  result := receivedData;
{$ELSE}
  result := nil;
{$ENDIF}
end;

procedure TDBXConnection.dealloc;
begin
{$IFDEF FPC}
  dbxconn.cancel;
  dbxconn.release;

  receivedData.release;

  inherited;
{$ENDIF}
end;

procedure TDBXConnection.start;
begin
{$IFDEF FPC}
  dbxconn.start;

  while (not complete and not cancelled) do
  begin
    NSRunLoop.currentRunLoop.runMode_beforeDate(NSDefaultRunLoopMode,
      NSDate.distantFuture);
  end;
{$ENDIF}
end;

function TDBXConnection.InitWithRequest_returningResponse_error_usingDelegate
  (request: NSURLRequest; var response: NSURLResponse; var error: NSError;
  aDelegate: TDBXConnectionEventHook): id;
begin
{$IFDEF FPC}
  result := self.init;
  if assigned(result) then
  begin
    internalDelegate := aDelegate;
    complete := False;
    cancelled := False;
    urlerror := @error;
    urlresponse := @response;
    dbxconn := NSURLConnection.connectionWithRequest_delegate(request, self);
    dbxconn.retain;
  end
{$ELSE}
  result := nil;
{$ENDIF}
end;

function TDBXConnection.connection_willSendRequest_redirectResponse
  (connection: NSURLConnection; request: NSURLRequest; response: NSURLResponse)
  : NSURLRequest;
{$IFDEF FPC}
var
  s: sel;
begin
  result := request;

  s := objcselector('DBXconnection:willSendRequest:redirectResponse:');
  if internalDelegate.respondsToSelector(s) then
  begin
    result := internalDelegate.DBXconnection_willSendRequest_redirectResponse
      (connection, request, response);
  end;

{$ELSE}


begin
  result := request;
{$ENDIF}
end;

function TDBXConnection.connection_needNewBodyStream
  (connection: NSURLConnection; request: NSURLRequest): NSInputStream;
{$IFDEF FPC}
var
  s: sel;
begin
  result := nil;
  s := objcselector('DBXconnection:needNewBodyStream:');
  if internalDelegate.respondsToSelector(s) then
  begin

    result := internalDelegate.DBXconnection_needNewBodyStream
      (connection, request);
  end;
{$ELSE}


begin
  result := nil;
{$ENDIF}
end;

function TDBXConnection.connection_canAuthenticateAgainstProtectionSpace
  (connection: NSURLConnection; protectionSpace: NSURLProtectionSpace): boolean;
{$IFDEF FPC}
var
  s: sel;
begin
  result := False;
  s := objcselector('DBXconnection:canAuthenticateAgainstProtectionSpace:');
  if internalDelegate.respondsToSelector(s) then
  begin

    result := internalDelegate.DBXconnection_canAuthenticateAgainstProtectionSpace
      (connection, protectionSpace);
  end;
{$ELSE}


begin
  result := False;
{$ENDIF}
end;

procedure TDBXConnection.connection_didReceiveAuthenticationChallenge
  (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC}
var
  s: sel;
begin
  s := objcselector('DBXconnection:didReceiveAuthenticationChallenge:');
  if internalDelegate.respondsToSelector(s) then
  begin

    internalDelegate.DBXconnection_didReceiveAuthenticationChallenge(connection,
      challenge);
  end;

{$ELSE}


begin
{$ENDIF}
end;

procedure TDBXConnection.connection_didCancelAuthenticationChallenge
  (connection: NSURLConnection; challenge: NSURLAuthenticationChallenge);
{$IFDEF FPC}
var
  s: sel;
begin
  s := objcselector('DBXconnection:didCancelAuthenticationChallenge:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.DBXconnection_didCancelAuthenticationChallenge(connection,
      challenge);
  end;
{$ELSE}


begin
{$ENDIF}
end;

function TDBXConnection.connectionShouldUseCredentialStorage
  (connection: NSURLConnection): boolean;
{$IFDEF FPC}
var
  s: sel;
begin
  result := False;
  s := objcselector('DBXconnectionShouldUseCredentialStorage');
  if internalDelegate.respondsToSelector(s) then
    result := internalDelegate.DBXconnectionShouldUseCredentialStorage
      (connection);
{$ELSE}

begin
  result := False;
{$ENDIF}
end;

procedure TDBXConnection.connection_didReceiveResponse
  (connection: NSURLConnection; response: NSURLResponse);
{$IFDEF FPC}
var
  s: sel;
begin
  if (isSynchronous) then
  begin

    // every response could mean a redirect
    receivedData.release;
    receivedData := nil;
    response.retain;
    urlresponse^ := response;
  end;
  s := objcselector('DBXconnection:didReceiveResponse:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.DBXconnection_didReceiveResponse(connection, response);
  end;
{$ELSE}


begin
{$ENDIF}
end;

procedure TDBXConnection.connection_didReceiveData(connection: NSURLConnection;
  data: NSData);
{$IFDEF FPC}
var
  s: sel;
begin
  if (isSynchronous) then
  begin

    if not assigned(receivedData) then
      // no store yet, make one
      receivedData := NSMutableData.alloc.initWithData(data)

    else
      // append to previous chunks
      receivedData.appendData(data);

  end;
  s := objcselector('DBXconnection:didReceiveData:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.DBXconnection_didReceiveData(connection, data);
  end;
{$ELSE}


begin
{$ENDIF}
end;

procedure TDBXConnection.
  connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
  (connection: NSURLConnection; bytesWritten: NSInteger;
  totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger);
{$IFDEF FPC}
var
  s: sel;
begin
  s := objcselector
    ('DBXconnection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.
      DBXconnection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
      (connection, bytesWritten, totalBytesWritten, totalBytesExpectedToWrite);
  end;
{$ELSE}


begin
{$ENDIF}
end;

procedure TDBXConnection.connectionDidFinishLoading
  (connection: NSURLConnection);
{$IFDEF FPC}
var
  s: sel;
begin
  if (isSynchronous) then
  begin
    complete := true;
  end;
  s := objcselector('DBXconnectionDidFinishLoading:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.DBXconnectionDidFinishLoading(connection);
  end;
{$ELSE}


begin
{$ENDIF}
end;

procedure TDBXConnection.connection_didFailWithError
  (connection: NSURLConnection; error: NSError);
{$IFDEF FPC}
var
  s: sel;
begin
  if (isSynchronous) then
  begin
    error.retain;
    urlerror^ := error;
    cancelled := true;
  end;
  s := objcselector('DBXconnection:didFailWithError:');
  if internalDelegate.respondsToSelector(s) then
  begin
    internalDelegate.DBXconnection_didFailWithError(connection, error);
  end;
{$ELSE}


begin
{$ENDIF}
end;

function TDBXConnection.connection_willCacheResponse
  (connection: NSURLConnection; cachedResponse: NSCachedURLResponse)
  : NSCachedURLResponse;
{$IFDEF FPC}
var
  s: sel;
begin
  result := nil;
  s := objcselector('DBXconnection:willCacheResponse:');
  if internalDelegate.respondsToSelector(s) then
  begin
    result := internalDelegate.DBXconnection_willCacheResponse(connection,
      cachedResponse);
  end;
{$ELSE}


begin
  result := nil;
{$ENDIF}
end;
{ TDBXConnectionEventHook }

procedure TDBXConnectionEventHook.DBXconnectionDidFinishLoading
  (var connection: NSURLConnection);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connectionDidFinishLoading(connection);
{$ENDIF}
end;

function TDBXConnectionEventHook.DBXconnectionShouldUseCredentialStorage
  (var connection: NSURLConnection): boolean;
begin
  result := False;
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connectionShouldUseCredentialStorage
      (connection, result);
{$ENDIF}
end;

function TDBXConnectionEventHook.
  DBXconnection_canAuthenticateAgainstProtectionSpace(var connection
  : NSURLConnection;
  var protectionSpace: NSURLProtectionSpace): boolean;
begin
  result := False;
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_canAuthenticateAgainstProtectionSpace
      (connection, protectionSpace, result);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.DBXconnection_didCancelAuthenticationChallenge
  (var connection: NSURLConnection;
  var challenge: NSURLAuthenticationChallenge);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_didCancelAuthenticationChallenge(connection,
      challenge);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.DBXconnection_didFailWithError
  (var connection: NSURLConnection; error: NSError);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_didFailWithError(connection, error);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.DBXconnection_didReceiveAuthenticationChallenge
  (var connection: NSURLConnection;
  var challenge: NSURLAuthenticationChallenge);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_didReceiveAuthenticationChallenge
      (connection, challenge);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.DBXconnection_didReceiveData
  (var connection: NSURLConnection; var data: NSData);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_didReceiveData(connection, data);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.DBXconnection_didReceiveResponse
  (var connection: NSURLConnection; var response: NSURLResponse);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_didReceiveResponse(connection, response);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.
  DBXconnection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
  (var connection: NSURLConnection; bytesWritten, totalBytesWritten,
  totalBytesExpectedToWrite: NSInteger);
begin
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.
      connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
      (connection, bytesWritten, totalBytesWritten, totalBytesExpectedToWrite);
{$ENDIF}
end;

function TDBXConnectionEventHook.DBXconnection_needNewBodyStream
  (var connection: NSURLConnection; var request: NSURLRequest): NSInputStream;
begin
  result := nil;
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_needNewBodyStream(connection,
      request, result);
{$ENDIF}
end;

function TDBXConnectionEventHook.DBXconnection_willCacheResponse
  (var connection: NSURLConnection; var cachedResponse: NSCachedURLResponse)
  : NSCachedURLResponse;
begin
  result := cachedResponse;
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_willCacheResponse(connection,
      cachedResponse, result);
{$ENDIF}
end;

function TDBXConnectionEventHook.DBXconnection_willSendRequest_redirectResponse
  (var connection: NSURLConnection; var request: NSURLRequest;
  var response: NSURLResponse)
  : NSURLRequest;
begin
  result := request;
{$IFDEF FPC}
  if assigned(FListener) then
    FListener.connection_willSendRequest_redirectResponse(connection,
      request, response, result);
{$ENDIF}
end;

procedure TDBXConnectionEventHook.SetListener(aConnection: TConnectionEventLister
  );
begin
  FListener := aConnection;
end;

end.
