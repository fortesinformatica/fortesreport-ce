//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DBXDefaultFormatter;
{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  DSRestTypes, Classes
{$IFDEF FPC}
    , base64
{$ENDIF}
    ;

type

  { TDBXDefaultFormatter }

  TDBXDefaultFormatter = class
  strict private
    constructor Create;
  public
    class function GetInstance: TDBXDefaultFormatter;
    class procedure ReleaseInstance;
    function Int8ToString(const value: Int8): String;
    function Int16ToString(const value: Int16): String;
    function Int32ToString(const value: Int32): String;
    function Int64ToString(const value: Int64): String;
    function UInt8ToString(const value: UInt8): String;
    function UInt16ToString(const value: UInt16): String;
    function UInt32ToString(const value: UInt32): String;
    function UInt64ToString(const value: UInt64): String;

    function AnsiStringToString(const value: AnsiString): String;
    function WideStringToString(const value: WideString): String;

    function Base64Encode(value: String): String;
    function FloatToString(value: Double): String;
    function DateTimeToString(const value: TDateTime): String;
    function CurrencyToString(const value: Currency): String;
    function DoubleToString(const value: Double): String;
    function SingleToString(const value: Single): String;
    function DateToString(const value: TDate): String;
    function TimeToString(const value: TTime): String;
    function BooleanToString(const value: Boolean): String;
    function DBXTimeToString(const value: integer): string;
    function DBXDateToString(const value: integer): string;

    function StringToDateTime(const value: String): TDateTime;
    function StringToDate(const value: String): TDate;
    function StringToDBXTime(const value: string): integer;
    function StringToDBXDate(const value: string): integer;

    function StreamToString(value: TStream): String;
  end;

implementation

uses
  SysUtils, FPCStrings;

var
  FInstance: TDBXDefaultFormatter;
  FGENERALSETTING: TFormatSettings;

  { TDBXDefaultFormatter }

function TDBXDefaultFormatter.AnsiStringToString(const value
  : AnsiString): String;
begin
{$WARNINGS OFF}
  Result := value;
{$WARNINGS ON}
end;

function TDBXDefaultFormatter.Base64Encode(value: String): String;
begin
  Result := '';
{$IFDEF PFC}
  Result := EncodeStringBase64(value);
{$ENDIF}
end;

function TDBXDefaultFormatter.BooleanToString(const value: Boolean): String;
begin
  Result := BoolToStr(value, true);
end;

function TDBXDefaultFormatter.FloatToString(value: Double): String;
var
  fs: TFormatSettings;
begin
  fs.DecimalSeparator := '.';
  Result := SysUtils.FormatFloat('#############.0##', value, fs);
end;

constructor TDBXDefaultFormatter.Create;
begin
  inherited Create;
end;

function TDBXDefaultFormatter.CurrencyToString(const value: Currency): String;
begin
  Result := CurrToStr(value);
end;

function TDBXDefaultFormatter.DateTimeToString(const value: TDateTime): String;
begin
  Result := FormatDateTime(FGENERALSETTING.longDateFormat, value);
end;

function TDBXDefaultFormatter.DateToString(const value: TDate): String;
begin
  Result := FormatDateTime(FGENERALSETTING.ShortDateFormat, value);

end;

function TDBXDefaultFormatter.DBXDateToString(const value: integer): string;
var
  Date: TDateTime;
  Time: TTimeStamp;
begin
  Time.Time := 0;
  Time.Date := value;
  Date := TimeStampToDateTime(Time);
  Result := FormatDateTime(FGENERALSETTING.ShortDateFormat, Date);

end;

function TDBXDefaultFormatter.DBXTimeToString(const value: integer): string;
var
  Date: TDateTime;
  Time: TTimeStamp;
begin
  Time.Time := value;
  Time.Date := DateDelta;
  Date := TimeStampToDateTime(Time);
  Result := FormatDateTime(FGENERALSETTING.ShortTimeFormat, Date);

end;

function TDBXDefaultFormatter.DoubleToString(const value: Double): String;
begin
  Result := FloatToStr(value);
end;

class function TDBXDefaultFormatter.GetInstance: TDBXDefaultFormatter;
begin
  if not assigned(FInstance) then
  begin
    FInstance := TDBXDefaultFormatter.Create;
  end;
  Result := FInstance;
end;

function TDBXDefaultFormatter.Int16ToString(const value: Int16): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.Int32ToString(const value: Int32): String;
begin
  Result := IntToStr(value);
end;

function TDBXDefaultFormatter.Int64ToString(const value: Int64): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.Int8ToString(const value: Int8): String;
begin
  Result := IntToStr(value);

end;

class procedure TDBXDefaultFormatter.ReleaseInstance;
begin
  if assigned(FInstance) then
    FInstance.free;
end;

function TDBXDefaultFormatter.SingleToString(const value: Single): String;
begin

  Result := FloatToStr(value);
end;

function TDBXDefaultFormatter.StringToDateTime(const value: String): TDateTime;
var
  year, month, day, hour, minutes, seconds, msec: word;
begin
  hour := 0;
  minutes := 0;
  seconds := 0;
  msec := 0;
  if length(value) < 10 then
    raise DBXException.Create('Invalid date time');
  year := strtoint(Copy(value, 1, 4));
  month := strtoint(Copy(value, 6, 2));
  day := strtoint(Copy(value, 9, 2));
  if length(value) > 12 then
  // datetime returned without hour is ex.01-01-2000.0
  begin
    hour := strtoint(Copy(value, 12, 2));
    minutes := strtoint(Copy(value, 15, 2));
    seconds := strtoint(Copy(value, 18, 2));
    msec := strtoint(Copy(value, 21, 3));
  end;
  Result := EncodeDate(year, month, day) + EncodeTime(hour, minutes,
    seconds, msec);
end;

function TDBXDefaultFormatter.StreamToString(value: TStream): String;
var
  s: TStringStream;
begin
  Result := '';
  if assigned(value) then
  begin
    s := TStringStream.create('');
    try
      value.Position := 0;
      s.CopyFrom(value, value.Size);
      Result := s.DataString;
    finally
      s.Free;
    end;
  end;

end;

function TDBXDefaultFormatter.StringToDate(const value: String): TDate;
begin
  Result := StrToDateTime(value, FGENERALSETTING);
end;

function TDBXDefaultFormatter.StringToDBXDate(const value: string): integer;
var
  Date: TDateTime;
  Time: TTimeStamp;
begin

  Date := StrToDateTime(value, FGENERALSETTING);
  Time := DateTimeToTimeStamp(Date);
  Result := Time.Date;
end;

function TDBXDefaultFormatter.StringToDBXTime(const value: string): integer;
var
  Date: TDateTime;
  Time: TTimeStamp;
begin

  Date := StrToDateTime(value, FGENERALSETTING);
  Time := DateTimeToTimeStamp(Date);
  Result := Time.Time;

end;

function TDBXDefaultFormatter.TimeToString(const value: TTime): String;
begin
  Result := FormatDateTime(FGENERALSETTING.ShortTimeFormat, Time);
end;

function TDBXDefaultFormatter.UInt16ToString(const value: UInt16): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.UInt32ToString(const value: UInt32): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.UInt64ToString(const value: UInt64): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.UInt8ToString(const value: UInt8): String;
begin
  Result := IntToStr(value);

end;

function TDBXDefaultFormatter.WideStringToString(const value
  : WideString): String;
begin
  Result := value;
end;

initialization

FGENERALSETTING.ShortDateFormat := 'yyyy-mm-dd';;
FGENERALSETTING.TimeSeparator := ':';
FGENERALSETTING.DateSeparator := '-';
FGENERALSETTING.LongTimeFormat := 'hh:nn:ss.zzz';
FGENERALSETTING.longDateFormat := 'yyyy-mm-dd hh:nn:ss.zzz';
FGENERALSETTING.ShortTimeFormat := 'hh:nn:ss';

finalization

TDBXDefaultFormatter.ReleaseInstance;

end.
