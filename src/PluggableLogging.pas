{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit PluggableLogging;

interface

// This unit provides an abstract interface to a logging mechanism. Assign your
// custom logging facility Logger variable, and calls to LogEntry will invoke
// that callback. Should you not want logging, simply set Logger := nil (its
// default state).

type
  TSeverityLevel = (slDebug, slError, slWarning, slInfo);

  TLoggerProcedure = procedure(Description: String;
                               SourceDesc: String;
                               Severity: TSeverityLevel;
                               EventRef: Cardinal;
                               DebugInfo: String);

function IntToSeverityLevel(N: Integer): TSeverityLevel;
procedure LogEntry(Description: String;
                   SourceDesc: String;
                   Severity: TSeverityLevel;
                   EventRef: Cardinal;
                   DebugInfo: String);
function  NameToSeverityLevel(S: String): TSeverityLevel;

var
  Logger: TLoggerProcedure;

// Well-known event codes:
const
  LogEventRefDroppedMessage = 29;
  LogEventRefTimerEvent     = 20;
  LogEventException         = $FFFF;

implementation

uses
  SysUtils;

function IntToSeverityLevel(N: Integer): TSeverityLevel;
begin
  if (N <= 0) then
    Result := slDebug
  else if (N <= 2) then
    Result := slError
  else if (N <= 4) then
    Result := slError
  else
    Result := slInfo;
end;

procedure LogEntry(Description: String;
                   SourceDesc: String;
                   Severity: TSeverityLevel;
                   EventRef: Cardinal;
                   DebugInfo: String);
begin
  if Assigned(Logger) then
    Logger(Description, SourceDesc, Severity, EventRef, DebugInfo);
end;

function NameToSeverityLevel(S: String): TSeverityLevel;
var
  Name: String;
begin
  Name := Lowercase(S);

       if (Name = 'debug') then
    Result := slDebug
  else if (Name = 'error') then
    Result := slError
  else if (Name = 'warning') then
    Result := slWarning
  else
    Result := slInfo;
end;

end.
