//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit FPCStrings;

{$IFDEF FPC}
{$mode DELPHI}
//{$mode objfpc}
{$modeswitch objectivec2}
{$ENDIF}
interface

 {$IFDEF FPC}
 {$IFNDEF WINDOWS}
uses iPhoneAll;

function NStoString(Value: NSString): String;
function StringToNS(Value: String): NSString;
 {$ENDIF} {$ENDIF}
procedure Log(Value: string);
function NSFStr(Value: String): string;

implementation



uses SysUtils;


{$IFDEF FPC}{$IFNDEF WINDOWS}
function NStoString(Value: NSString): String;
begin
  Result := Value.UTF8String;
end;

function StringToNS(Value: String): NSString;
begin
  Result := NSString.stringWithUTF8String(PChar(Value));
end;

{$ENDIF}{$ENDIF}
procedure Log(Value: string);
begin
 {$IFDEF FPC}{$IFNDEF WINDOWS}
  nsLog(StringToNS(Value));
 {$ENDIF}{$ENDIF}
end;

function NSFStr(Value: String): string;
begin
  Result := Value;
  {$IFDEF FPC}
  Result := StringReplace(Result, '%', '%%', [rfIgnoreCase, rfReplaceAll]);
 {$ENDIF}
end;

end.

