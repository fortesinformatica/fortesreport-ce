//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSRESTTypes;

{$I dsrestdefines.inc}

interface

uses
  Contnrs, SysUtils{$IFDEF FPC_ON_IOS}, IphoneAll{$ENDIF};

type

  DBXException = class(Exception)

  end;

  Int8 = shortint;
  Int16 = smallint;
  Int32 = integer;
  UInt8 = Byte;
  UInt16 = word;
  UInt32 = cardinal;

  TDSRESTParamDirection = (Unknown = 0, Input = 1, Output = 2, InputOutput = 3,
    ReturnValue = 4);
  TDBXDataTypes = (
    // / <summary>Data types supported by DBX.</summary>

    // /<summary></summary>
    UnknownType = 0,
    // /<summary>8 bit Ansi String</summary>
    AnsiStringType = 1,
    // /<summary>32 bit Date</summary>
    DateType = 2,
    // /<summary>Blob with a subtype</summary>
    BlobType = 3,
    // /<summary>16 big Boolean</summary>
    BooleanType = 4,
    // /<summary>16 bit signed integer</summary>
    Int16Type = 5,
    // /<summary>32 bit signed integer</summary>
    Int32Type = 6,
    // /<summary>64 bit floating point</summary>
    DoubleType = 7,
    // /<summary>TBcd decimal from the FMTBcd unit</summary>
    BcdType = 8, // { BCD } */
    // /<summary>Fixed length byte array</summary>
    BytesType = 9,
    // /<summary>32 bit Time</summary>
    TimeType = 10,
    // /<summary>TDateTime
    // / Internally managed as a <c>TDBXDataTypes.TimeStampType</c>
    // /</summary>
    DateTimeType = 11, // * { Time-stamp (64 bit) } */
    // /<summary>Unsigned 16 bit integer</summary>
    UInt16Type = 12,
    // /<summary>Unsigned 32 bit integer</summary>
    UInt32Type = 13,
    // ///<summary></summary>
    // FLOATIEEE = 14; { 80-bit IEEE float }
    // /<summary>Variable length byte array with maximum length of 64
    // kilobytes</summary>
    VarBytesType = 15,
    // ///<summary></summary>
    // LOCKINFO = 16; { Look for LOCKINFO typedef }
    // /<summary>Oracle cursor type</summary>
    CursorType = 17,
    // /<summary>64 bit integer</summary>
    Int64Type = 18,
    // /<summary>unsigned 64 bit integer</summary>
    UInt64Type = 19,
    // /<summary>Abstract data type</summary>
    AdtType = 20,
    // /<summary>Array data type</summary>
    ArrayType = 21,
    // /<summary>Reference data type</summary>
    RefType = 22,
    // /<summary>Nested table data type</summary>
    TableType = 23,
    // /<summary>TSQLTimeStamp in the SqlTimSt unit</summary>
    TimeStampType = 24,
    // /<summary>Delphi Currency data type in System unit.
    // / Internally managed as a <c>TDBXDataTypes.BCDType</c>
    // /</summary>
    CurrencyType = 25,
    // /<summary>UCS2 unicode string</summary>
    WideStringType = 26,

    // /<summary>32 bit floating point</summary>
    SingleType = 27,

    // /<summary>8 bit signed integer</summary>
    Int8Type = 28,
    // /<summary>8 bit unsigned integer</summary>
    UInt8Type = 29,
    // /<summary>Object serialization</summary>
    ObjectType = 30,
    // /<summary>Character array</summary>
    CharArrayType = 31,
    // /<summary>Time Interval</summary>
    IntervalType = 32,
    // /<summary>BinaryBlobType equivalent to <c>BlobType</c> with a
    // /<c>BinarySubType</c> sub type/
    // /</summary>
    BinaryBlobType = 33,
    // /<summary>
    // / DBXConnection type for DataSnap server methods that receive or set the
    // server side
    // / connection.
    // /</summary>
    DBXConnectionType = 34,
    // /<summary>
    // / Variant out or return parameter. Not supported as a TDBXReader column.
    // /</summary>
    VariantType = 35, TimeStampOffsetType = 36,
    // /<summary>DBX type for a JSON value
    // /</summary>
    JsonValueType = 37,
    // /<summary>
    // /DBX type for a callback argument
    // /</summary>
    CallbackType = 38,
    // /<summary>Maximum number of base types excluding sub types that
    // / are supported by TDataSet type system.
    // /</summary>
    MaxBaseTypes = 39);
{$IFNDEF FPC}

  // Fake declaration to compile on Delphi
  NSURLConnection = class
  end;

  NSURLRequest = class
  end;

  NSURLResponse = class
  end;

  NSInputStream = class
  end;

  NSURLProtectionSpace = class
  end;

  NSURLAuthenticationChallenge = class
  end;

  NSCachedURLResponse = class
  end;

  NSError = class
  end;

  NSData = class
  end;

  NSInteger = class
  end;

  id = class
  end;

  NSMutableData = class
  end;

  sel = class
  end;
{$ENDIF}

{$IFNDEF FPC_ON_IOS}
  TConnectionEventLister = class
  end;
{$ELSE}

  TConnectionEventLister = class
  public
    procedure connection_didReceiveAuthenticationChallenge
      (var connection: NSURLConnection;
      var challenge: NSURLAuthenticationChallenge); virtual;
    procedure connectionShouldUseCredentialStorage(var connection
      : NSURLConnection;
      var result: boolean); virtual;
    procedure connection_willSendRequest_redirectResponse
      (var connection: NSURLConnection; var request: NSURLRequest;
      var response: NSURLResponse; var result: NSURLRequest); virtual;
    procedure connection_needNewBodyStream(var connection: NSURLConnection;
      var request: NSURLRequest; var result: NSInputStream); virtual;
    procedure connection_canAuthenticateAgainstProtectionSpace
      (var connection: NSURLConnection;
      var protectionSpace: NSURLProtectionSpace;
      var result: boolean); virtual;
    procedure connection_didCancelAuthenticationChallenge
      (var connection: NSURLConnection;
      var challenge: NSURLAuthenticationChallenge); virtual;
    procedure connection_didReceiveResponse(var connection: NSURLConnection;
      var response: NSURLResponse); virtual;
    procedure connection_didReceiveData(var connection: NSURLConnection;
      var data: NSData); virtual;
    procedure connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
      (var connection: NSURLConnection; bytesWritten: NSInteger;
      totalBytesWritten: NSInteger;
      totalBytesExpectedToWrite: NSInteger); virtual;

    procedure connectionDidFinishLoading(var connection: NSURLConnection);
    procedure connection_didFailWithError(var connection: NSURLConnection;
      error: NSError); virtual;
    procedure connection_willCacheResponse(var connection: NSURLConnection;
      var cachedResponse: NSCachedURLResponse;
      var result: NSCachedURLResponse); virtual;

  end;
{$ENDIF}

implementation

{$IFDEF FPC_ON_IOS}

{ TConnectionEventLister }

procedure TConnectionEventLister.connectionDidFinishLoading
  (var connection: NSURLConnection);
begin

end;

procedure TConnectionEventLister.connectionShouldUseCredentialStorage
  (var connection: NSURLConnection; var result: boolean);
begin

end;

procedure TConnectionEventLister.connection_canAuthenticateAgainstProtectionSpace
  (var connection: NSURLConnection; var protectionSpace: NSURLProtectionSpace;
  var result: boolean);
begin

end;

procedure TConnectionEventLister.connection_didCancelAuthenticationChallenge
  (var connection: NSURLConnection;
  var challenge: NSURLAuthenticationChallenge);
begin

end;

procedure TConnectionEventLister.connection_didFailWithError
  (var connection: NSURLConnection; error: NSError);
begin

end;

procedure TConnectionEventLister.connection_didReceiveAuthenticationChallenge
  (var connection: NSURLConnection;
  var challenge: NSURLAuthenticationChallenge);
begin

end;

procedure TConnectionEventLister.connection_didReceiveData
  (var connection: NSURLConnection; var data: NSData);
begin

end;

procedure TConnectionEventLister.connection_didReceiveResponse
  (var connection: NSURLConnection; var response: NSURLResponse);
begin

end;

procedure TConnectionEventLister.
  connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
  (var connection: NSURLConnection; bytesWritten, totalBytesWritten,
  totalBytesExpectedToWrite: NSInteger);
begin

end;

procedure TConnectionEventLister.connection_needNewBodyStream
  (var connection: NSURLConnection; var request: NSURLRequest;
  var result: NSInputStream);
begin

end;

procedure TConnectionEventLister.connection_willCacheResponse
  (var connection: NSURLConnection; var cachedResponse: NSCachedURLResponse;
  var result: NSCachedURLResponse);
begin

end;

procedure TConnectionEventLister.connection_willSendRequest_redirectResponse
  (var connection: NSURLConnection; var request: NSURLRequest;
  var response: NSURLResponse;
  var result: NSURLRequest);
begin

end;

{$ENDIF}

end.
