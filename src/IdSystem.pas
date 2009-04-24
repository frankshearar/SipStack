{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
    * Guido Gybels
}
unit IdSystem;

interface

{ This unit contains all platform-specific (i.e., OS calls) that you might need
  to call, except for network-related functions (which are in IdNetworking.pas).
  Currently only the Windows (2000 and more recent) platform is implemented.
}

type
  TIdOsType = (otWindows95, otWindows98, otWindowsMe, otWindowsNT4, otWindows2k,
               otWindowsXP, otWindowsServer2003, otWindowsVista, otUnknown);

function  ConstructUUID: String;
function  ConstructUUIDURN: String;
function  GetCurrentProcessId: Cardinal;
function  GetFullUserName: WideString;
function  GetTickCount: Cardinal;
function  GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
function  GetUserName: WideString;
function  OsType: TIdOsType;
function  WindowsOsType(PlatformID, MajorVersion, MinorVersion: Cardinal): TIdOsType;

implementation

uses
  IdSimpleParser, SysUtils, Windows;

function ConstructUUID: String;
var
  NewGuid: TGUID;
begin
  CreateGUID(NewGuid);
  Result := Lowercase(WithoutFirstAndLastChars(GUIDToString(NewGuid)));
end;

function ConstructUUIDURN: String;
begin
  Result := 'urn:uuid:' + ConstructUUID;
end;

function GetCurrentProcessId: Cardinal;
begin
  Result := Windows.GetCurrentProcessId;
end;

function GetFullUserName: WideString;
begin
  // Really, we want to get something like "John Random" rather than "JohnR" or
  // "JRandom".
  Result := GetUserName;
end;

function GetTickCount: Cardinal;
var
  Count, Frequency: Int64;
  Temp:             Int64;
begin
  if QueryPerformanceFrequency(Frequency) and QueryPerformanceCounter(Count) then begin
    Temp := Trunc((Count * 1000)/ Frequency);

    if (Temp > High(Cardinal)) then
      Result := Temp - High(Cardinal)
    else
      Result := Temp
  end
  else
    Result:= Windows.GetTickCount
end;

function GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
begin
  // Remember: tick counts can roll over

  if (NewTickCount >= OldTickCount) then begin
    Result := NewTickCount - OldTickCount
  end
  else
    Result := High(Cardinal) - OldTickCount + NewTickCount;
end;

function GetUserName: WideString;
var
  Buf: PWideChar;
  Len: Cardinal;
begin
  Len := 1000;
  GetMem(Buf, Len);
  try
    if not GetUserNameW(Buf, Len) then
      raise Exception.Create(SysErrorMessage(GetLastError));

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

function OsType: TIdOsType;
begin
  {$IFDEF MSWINDOWS}
  Result := WindowsOsType(Win32Platform, Win32MajorVersion, Win32MinorVersion);
  {$ELSE}
  Result := otUnknown;
  {$ENDIF}
end;

function WindowsOsType(PlatformID, MajorVersion, MinorVersion: Cardinal): TIdOsType;
var
  OsID: Cardinal;
begin
  OsID := ((PlatformID and $000000ff) shl 16)
       or ((MajorVersion and $000000ff) shl 8)
       or (MinorVersion and $000000ff);

  case OsID of
    $00010400: Result := otWindows95;
    $0001040A: Result := otWindows98;
    $0001045A: Result := otWindowsMe;
    $00020400: Result := otWindowsNT4;
    $00020500: Result := otWindows2k;
    $00020501: Result := otWindowsXP;
    $00020502: Result := otWindowsServer2003;
    $00020600: Result := otWindowsVista;
  else
    Result := otUnknown;
  end;
end;

end.
