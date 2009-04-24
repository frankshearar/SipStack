{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSystem;

interface

uses
  IdSipMockDnsServer, IdSystem, TestFramework;

type
  // This suite currently only supports Windows (2000).
  TestFunctions = class(TTestCase)
  published
    procedure TestConstructUUIDURN;
    procedure TestGetCurrentProcessId;
    procedure TestWindowsOsType;
  end;

implementation

uses
  Classes, IdSocketHandle, IdSimpleParser, IdSipMessage, IdUdpServer, SysUtils,
  TypInfo, Windows, Winsock;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSystem unit tests');
  Result.AddTest(TestFunctions.Suite);
end;

function OsTypeName(OS: TIdOsType): String;
begin
  Result := GetEnumName(TypeInfo(TIdOsType), Integer(OS));
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestConstructUUIDURN;
var
  URN: String;
begin
  URN := ConstructUUIDURN;
  Check(TIdSipParser.IsUuidUrn(URN), 'ConstructUUIDURN returned "' + URN + '", an invalid UUID URN');
end;

procedure TestFunctions.TestGetCurrentProcessId;
begin
  CheckEquals(Windows.GetCurrentProcessId,
              IdSystem.GetCurrentProcessId,
              'GetCurrentProcessId');
end;

procedure TestFunctions.TestWindowsOsType;
begin
  CheckEquals(OsTypeName(otWindows95), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 4, 0)),  'Windows 95');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 4, 1)),  'Wintendo 4.1?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 4, 2)),  'Wintendo 4.2?');
  CheckEquals(OsTypeName(otWindows98), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 4, 10)),  'Wintendo 98');
  CheckEquals(OsTypeName(otWindowsMe), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 4, 90)),  'Wintendo Me');

  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 5, 0)),  'Wintendo 5.0?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 5, 1)),  'Wintendo 5.1?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 5, 2)),  'Wintendo 5.2?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 5, 10)),  'Wintendo 5.10?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 5, 90)),  'Wintendo 5.90?');

  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 6, 0)),  'Wintendo 6.0?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 6, 1)),  'Wintendo 6.1?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 6, 2)),  'Wintendo 6.2?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 6, 10)),  'Wintendo 6.10?');
  CheckEquals(OsTypeName(otUnknown),   OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_WINDOWS, 6, 90)),  'Wintendo 6.90?');

  CheckEquals(OsTypeName(otWindowsNT4), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 4, 0)),  'Windows NT 4.0');
  CheckEquals(OsTypeName(otUnknown),    OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 4, 1)),  'Windows NT 4.1?');
  CheckEquals(OsTypeName(otUnknown),    OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 4, 2)),  'Windows NT 4.2?');
  CheckEquals(OsTypeName(otUnknown),    OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 4, 10)), 'Windows NT 4.10?');
  CheckEquals(OsTypeName(otUnknown),    OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 4, 90)), 'Windows NT 4.90?');

  CheckEquals(OsTypeName(otWindows2k),         OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 5, 0)),  'Windows 2000');
  CheckEquals(OsTypeName(otWindowsXP),         OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 5, 1)),  'Windows XP');
  CheckEquals(OsTypeName(otWindowsServer2003), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 5, 2)),  'Windows Server 2003');
  CheckEquals(OsTypeName(otUnknown),           OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 5, 10)), 'Windows version 5.10?');
  CheckEquals(OsTypeName(otUnknown),           OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 5, 90)), 'Windows version 5.90?');

  CheckEquals(OsTypeName(otWindowsVista), OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 6, 0)),  'Windows Vista (or Server "Longhorn")');
  CheckEquals(OsTypeName(otUnknown),      OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 6, 1)),  'Windows 6.1?');
  CheckEquals(OsTypeName(otUnknown),      OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 6, 2)),  'Windows 6.2?');
  CheckEquals(OsTypeName(otUnknown),      OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 6, 10)), 'Windows 6.10?');
  CheckEquals(OsTypeName(otUnknown),      OsTypeName(WindowsOsType(VER_PLATFORM_WIN32_NT, 6, 90)), 'Windows 6.90?');
end;

initialization
  RegisterTest('System-specific functions', Suite);
end.
