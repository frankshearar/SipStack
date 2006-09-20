{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSystem;

interface

uses
  Classes;

{ This unit contains all platform-specific (i.e., OS calls) that you might need
  to call. Currently only the Windows (2000 and more recent) platform is
  implemented.
}

function  ConstructUUID: String;
function  GetCurrentProcessId: Cardinal;
function  GetFullUserName: WideString;
function  GetHostName: String;
function  GetTickCount: Cardinal;
function  GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
function  GetUserName: WideString;
function  LocalAddress: String;
procedure LocalAddresses(IPs: TStrings);

implementation

uses
  IdSimpleParser, IdGlobal, IdStack, IdUDPServer, SysUtils, Windows, Winsock;

function ConstructUUID: String;
var
  NewGuid: TGUID;
begin
  CreateGUID(NewGuid);
  Result := Lowercase(WithoutFirstAndLastChars(GUIDToString(NewGuid)));
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

function GetHostName: String;
var
  Buf:       PAnsiChar;
  ErrorCode: Integer;
  Len:       Integer;
  RC:        Integer;
  Unused:    TWSAData;
begin
  // Return the full name of this machine.

  Len := 1000;
  GetMem(Buf, Len*Sizeof(AnsiChar));
  try
    RC := Winsock.gethostname(Buf, Len);

    if (RC = 0) then
      Result := Buf
    else begin
      ErrorCode := WSAGetLastError;
      raise Exception.Create('GetHostName: ' + IntToStr(ErrorCode) + '(' + SysErrorMessage(ErrorCode) + ')');
    end;
  finally
    FreeMem(Buf);
  end;
end;

function GetTickCount: Cardinal;
var
  Count, Frequency: Int64;
  Temp:             Int64;
begin
  if QueryPerformanceFrequency(Frequency) then
    if QueryPerformanceCounter(Count) then begin
       Temp := Trunc((Count * 1000)/ Frequency);

       if (Temp > High(Cardinal)) then
         Result := Temp - High(Cardinal)
       else
         Result := Temp
    end
    else
       Result:= Windows.GetTickCount
  else
    Result:= Windows.GetTickCount;
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

function LocalAddress: String;
var
  UnusedServer: TIdUDPServer;
begin
  if not Assigned(GStack) then begin
    UnusedServer := TIdUDPServer.Create(nil);
    try
      Result := GStack.LocalAddress;
    finally
      UnusedServer.Free;
    end;
  end
  else
    Result := GStack.LocalAddress;
end;

procedure LocalAddresses(IPs: TStrings);
var
  UnusedServer: TIdUDPServer;
begin
  IPs.Clear;

  if not Assigned(GStack) then begin
    UnusedServer := TIdUDPServer.Create(nil);
    try
      IPs.AddStrings(GStack.LocalAddresses);
    finally
      UnusedServer.Free;
    end;
  end
  else
    IPs.AddStrings(GStack.LocalAddresses);
end;

end.
