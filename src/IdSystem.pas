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

uses
  Classes;

{ This unit contains all platform-specific (i.e., OS calls) that you might need
  to call. Currently only the Windows (2000 and more recent) platform is
  implemented.
}

type
  TIdOsType = (otWindows95, otWindows98, otWindowsMe, otWindowsNT4, otWindows2k,
               otWindowsXP, otWindowsServer2003, otWindowsVista, otUnknown);

function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
function  BestRouteIsDefaultRouteNT4(DestinationIP, LocalIP: String): Boolean;
function  ConstructUUID: String;
function  ConstructUUIDURN: String;
procedure DefineLocalAddress(AAddress: String); {Allows you to use a specified local IP address}
procedure DefineNetMask(AMask: String); {Let you set the netmask, used to identify if any IP address is local or not}
procedure DefineRoutableAddress(AAddress: String); {Allows you to set a public IP address}
function  GetBestLocalAddress(DestinationAddress: String): String;
function  GetBestLocalAddressNT4(DestinationAddress: String): String;
function  GetCurrentProcessId: Cardinal;
function  GetFullUserName: WideString;
function  GetHostName: String;
function  GetTickCount: Cardinal;
function  GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
function  GetUserName: WideString;
function  HtoNL(N: Cardinal): Cardinal;
function  LocalAddress: String;
procedure LocalAddresses(IPs: TStrings);
function  NtoHL(N: Cardinal): Cardinal;
function  OnSameNetwork(AAddress1, AAddress2: String): Boolean; overload;
function  OnSameNetwork(Address1, Address2, Netmask: String): Boolean; overload;
function  OsType: TIdOsType;
function  ResolveARecords(Name: String; ResolvedList: TStrings): Integer;
function  RoutableAddress: String;
function  WindowsOsType(PlatformID, MajorVersion, MinorVersion: Cardinal): TIdOsType;

implementation

uses
  IdGlobal, IdRoutingTable, IdSimpleParser, IdStack, IdUDPServer, SyncObjs,
  SysUtils, Windows, Winsock;

const
  ANY_SIZE = 100;

var
  WinsockData:    TWSAData;
  WinsockLock:    TCriticalSection;
  WinsockStarted: Boolean;

// See GetHostByName's explanation.
type
  PPCharArray = ^TPCharArray;
  TPCharArray = array[0..(MaxInt div 4)-2] of PChar;

  PBytePointerArray = ^TBytePointerArray;
  TBytePointerArray = array[0..(MaxInt div 4)-2] of PByteArray;

  PHostEnt = ^THostEnt;
  THostEnt = packed record
    h_name: PAnsiChar;
    h_aliases: PPCharArray;
    h_addrtype: SmallInt;
    h_length: SmallInt;
    h_addr_list: PBytePointerArray;
  end;

  _MIB_IPADDRROW = record
    dwAddr:      DWORD;
    dwIndex:     DWORD;
    dwMask:      DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    unused1:     Word;
    wType:       Word;
  end;
  MIB_IPADDRROW = _MIB_IPADDRROW;
  TMibIpAddrRow = MIB_IPADDRROW;
  PMibIpAddrRow = TMibIpAddrRow;

  _MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    table: array[0..ANY_SIZE - 1] of MIB_IPADDRROW;
  end;
  MIB_IPADDRTABLE = _MIB_IPADDRTABLE;
  TMibIpAddrTable = MIB_IPADDRTABLE;
  PMibIpAddrTable = ^TMibIpAddrTable;

  // Delphi's WinSock incorrectly translate S_addr's unsigned value in the
  // WinSock C API to a signed value (Longint).
  PInAddr = ^TInAddr;
  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: Cardinal);
  end;
  TInAddr = in_addr;

  TIpAddr = TInAddr;
  PIpAddr = ^TIpAddr;

  _MIB_IPFORWARDROW = record
    dwForwardDest:      DWORD;
    dwForwardMask:      DWORD;
    dwForwardPolicy:    DWORD;
    dwForwardNextHop:   DWORD;
    dwForwardIfIndex:   DWORD;
    dwForwardType:      DWORD;
    dwForwardProto:     DWORD;
    dwForwardAge:       DWORD;
    dwForwardNextHopAS: DWORD;
    dwForwardMetric1:   DWORD;
    dwForwardMetric2:   DWORD;
    dwForwardMetric3:   DWORD;
    dwForwardMetric4:   DWORD;
    dwForwardMetric5:   DWORD;
  end;
  MIB_IPFORWARDROW = _MIB_IPFORWARDROW;
  TMibIpForwardRow = MIB_IPFORWARDROW;
  PMibIpForwardRow = ^TMibIpForwardRow;

  _MIB_IPFORWARDTABLE = record
    dwNumEntries: DWORD;
    table: array[0..ANY_SIZE - 1] of MIB_IPFORWARDROW;
  end;

  MIB_IPFORWARDTABLE = _MIB_IPFORWARDTABLE;
  TMibIpForwardTable = MIB_IPFORWARDTABLE;
  PMibIpForwardTable = ^TMibIpForwardTable;

const
  IpHelper  = 'iphlpapi.dll';
  WinSocket = 'wsock32.dll';

function GetBestInterface(dwDestAddr: TIpAddr; var pdwBestIfIndex: DWORD): DWORD; stdcall; external IpHelper name 'GetBestInterface';
function GetBestRoute(dwDestAddr: DWORD; dwSourceAddr: DWORD; var pBestRoute: TMibIpForwardRow): DWORD; stdcall; external IpHelper name 'GetBestRoute';
function GetIpAddrTable(pIpAddrTable: PMibIpAddrTable; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall; external IpHelper name 'GetIpAddrTable';
function GetIpForwardTable(pIpForwardTable: PMibIpForwardTable; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall; external IpHelper name 'GetIpForwardTable';

// WinSock's gethostbyname & hostent have terrible declarations. We override
// these to provide a more natural interface to the information.
function GetHostByName(Name: PChar): PHostEnt; stdcall; external WinSocket name 'gethostbyname';

{See commentary for LocalAddress and RoutableAddress for explanations of these variables}
var
  idLocalAddress, idRoutableAddress, idNetMask: String;

procedure PossiblyInitialiseWinsock;
var
  ErrorCode: Integer;
  RC:        Integer;
begin
  WinsockLock.Acquire;
  try
    if not WinsockStarted then begin
      RC := WSAStartup($202, WinsockData);

      if (RC <> 0) then begin
        ErrorCode := WSAGetLastError;
        raise Exception.Create('PossiblyInitialiseWinsock: ' + IntToStr(ErrorCode) + ' (' + SysErrorMessage(ErrorCode) + ')');
     end;

      WinsockStarted := true;
    end;
  finally
    WinsockLock.Release;
  end;
end;

function GetIpAddress(InterfaceIndex: DWORD): String;
var
  I:    Integer;
  RC:   DWORD;
  Size: ULONG;
  Table: PMibIpAddrTable;
begin
  Result := '';
  Size   := 0;
  if not GetIpAddrTable(nil, Size, true) = ERROR_BUFFER_OVERFLOW then Exit;

  Table := AllocMem(Size);
  try
    RC := GetIpAddrTable(Table, Size, true);
    if (RC <> 0) then Exit;

    for I := 0 to Table.dwNumEntries - 1 do begin
      if (Table.table[I].dwIndex = InterfaceIndex) then begin
        Result := TIdIPAddressParser.IPv4AddressToStr(NtoHL(Table.table[I].dwAddr));
        Break;
      end;
    end;
  finally
    FreeMem(Table);
  end;
end;

procedure LoadRoutingTable(RT: TIdMockRoutingTable);
var
  I:     Integer;
  RC:    DWORD;
  Size:  ULONG;
  Table: PMibIpForwardTable;
begin
  Size   := 0;
  if not GetIpForwardTable(nil, Size, true) = ERROR_BUFFER_OVERFLOW then Exit;

  Table := AllocMem(Size);
  try
    RC := GetIpForwardTable(Table, Size, true);
    if (RC <> 0) then Exit;

    RT.RemoveAllOsRoutes;
    for I := 0 to Table.dwNumEntries - 1 do begin
      RT.AddOsRoute(TIdIPAddressParser.IPv4AddressToStr(NtoHL(Table.table[I].dwForwardDest)),
                    TIdIPAddressParser.IPv4AddressToStr(NtoHL(Table.table[I].dwForwardMask)),
                    TIdIPAddressParser.IPv4AddressToStr(NtoHL(Table.table[I].dwForwardNextHop)),
                    Cardinal(Table.table[I].dwForwardMetric1),
                    IntToStr(Table.table[I].dwForwardIfIndex),
                    GetIpAddress(Table.table[I].dwForwardIfIndex));
    end;
  finally
    FreeMem(Table);
  end;
end;

function BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
var
  DstAddr: DWORD;
  Route:   TMibIpForwardRow;
  SrcAddr: DWORD;
begin
  // I'm not sure about the value of Result when you try break this function by
  // mixing your address types (say, DestinationIP being IPv4 and LocalIP being
  // IPv6...).

  Assert(TIdIPAddressParser.IPVersion(DestinationIP) = TIdIPAddressParser.IPVersion(LocalIP),
         Format('"%s" and "%s" are not of the same IP version', [DestinationIP, LocalIP]));

  if TIdIPAddressParser.IsIPv4Address(DestinationIP) then begin
    DstAddr := HtoNL(TIdIPAddressParser.InetAddr(DestinationIP));
    SrcAddr := HtoNL(TIdIPAddressParser.InetAddr(LocalIP));

    GetBestRoute(DstAddr, SrcAddr, Route);
    Result := Route.dwForwardDest = 0;
  end
  else begin
    Result := false;
//    raise Exception.Create('Implement BestRouteIsDefaultRoute for IPv6');
  end;
end;

function BestRouteIsDefaultRouteNT4(DestinationIP, LocalIP: String): Boolean;
var
  RT: TIdMockRoutingTable;
begin
  // NT 4 doesn't support GetBestRoute from the IP Helper API.

  RT := TIdMockRoutingTable.Create;
  try
    LoadRoutingTable(RT);

    Result := RT.BestRouteIsDefaultRoute(DestinationIP, LocalIP);
  finally
    RT.Free;
  end;
end;

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

{See commentary for LocalAddress for an explanation of this function}
procedure DefineLocalAddress(AAddress: String);
begin
  idLocalAddress:=AAddress;
end;

procedure DefineNetMask(AMask: String); {Let you set the netmask, used to identify if any IP address is local or not}
begin
  idNetMask:=AMask;
end;

procedure DefineRoutableAddress(AAddress: String); {Allows you to set a public IP address}
begin
  idRoutableAddress:=AAddress;
end;

function GetBestLocalAddress(DestinationAddress: String): String;
var
  DstAddr:        TIpAddr;
  InterfaceIndex: DWORD;
  RC:             DWORD;
begin
  // Return the most appropriate local address to use when sending packets to
  // the machine at DestinationAddress.
  // Right now, this only supports IPv4 addresses.

  Result := '';

  if not TIdIPAddressParser.IsIPv4Address(DestinationAddress) then
    raise Exception.Create('We do not support IPv6 addresses yet, because only XP and onwards have an IPv6 stack');

  DstAddr.S_addr := HtoNL(TIdIPAddressParser.InetAddr(DestinationAddress));
  RC := GetBestInterface(DstAddr, InterfaceIndex);

  if (RC <> 0) then
    raise Exception.Create(Format('GetBestInterface for destination "%s" failed with code %d (%s)', [DestinationAddress, RC, SysErrorMessage(RC)]));

  Result := GetIpAddress(InterfaceIndex);
end;

function GetBestLocalAddressNT4(DestinationAddress: String): String;
var
  RT: TIdMockRoutingTable;
begin
  // NT 4 doesn't support GetBestInterface from the IP Helper API.

  RT := TIdMockRoutingTable.Create;
  try
    LoadRoutingTable(RT);
    Result := RT.GetBestLocalAddress(DestinationAddress);
  finally
    RT.Free;
  end;
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
begin
  // Return the full name of this machine.

  PossiblyInitialiseWinsock;

  Len := 1000;
  GetMem(Buf, Len*Sizeof(AnsiChar));
  try
    RC := Winsock.gethostname(Buf, Len);

    if (RC = 0) then
      Result := Buf
    else begin
      ErrorCode := WSAGetLastError;
      raise Exception.Create('GetHostName: ' + IntToStr(ErrorCode) + ' (' + SysErrorMessage(ErrorCode) + ')');
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

function  HtoNL(N: Cardinal): Cardinal;
begin
  // Yes, this looks crazy. But WinSock's htonl is declared as taking a u_long -
  // presumably an UNSIGNED value - and yes u_long is declared as Longint, a
  // SIGNED value. In the interests of not making the reader go blind with
  // twofold casting everywhere, this function hides the awfulness behind a
  // PROPERLY DECLARED htonl.

  Result := Cardinal(Winsock.htonl(Integer(N)));
end;

{Normally, the local machine address is automatically discovered when using the "LocalAddress" function.
 In certain scenarios, however, you might want to preselect which local address to use. This applies
 for instance to multihomed scenarios as well as cases where you wish to signal a public IP address
 in stead of the local address.
 If you want to override the automatic address discovery, call "DefineLocalAddress" with the address
 you wish to use. Subsequent calls to LocalAddress will then always return this address. To restore
 automatic discovery, call DefineLocalAddress with a zero length string or with "0.0.0.0" as the
 address.}
function LocalAddress: String;
var
  UnusedServer: TIdUDPServer;
begin
  if (Length(idLocalAddress)=0) or (idLocalAddress='0.0.0.0') then
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
  end else
    Result:=idLocalAddress;
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

function NtoHL(N: Cardinal): Cardinal;
begin
  // See the comment in HtoNL

  Result := Cardinal(Winsock.ntohl(Integer(N)));
end;

{This function allows you to identify if two different IP addresses are on the "same" network, by
 using idNetMask.}
function OnSameNetwork(AAddress1, AAddress2: String): Boolean;
begin
  Result := OnSameNetwork(AAddress1, AAddress2, idNetMask);
end;

function OnSameNetwork(Address1, Address2, Netmask: String): Boolean;
var
  Addr1:    DWord;
  Addr2:    DWord;
  Mask:     DWord;
  Ip6Addr1: TIdIPv6AddressRec;
  Ip6Addr2: TIdIPv6AddressRec;
  Ip6Mask:  TIdIPv6AddressRec;
  I:        Integer;
begin
  if TIdIPAddressParser.IsIPv4Address(Address1) then begin
    if TIdIPAddressParser.IsIPv4Address(Address2)
      and TIdIPAddressParser.IsIPv4Address(NetMask) then begin
      Mask   := TIdIPAddressParser.InetAddr(Netmask);
      Addr1 := TIdIPAddressParser.InetAddr(Address1);
      Addr2 := TIdIPAddressParser.InetAddr(Address2);
      Result := (Addr1 and Mask) = (Addr2 and Mask);
    end
    else
      Result := false;
  end
  else begin
    if TIdIPAddressParser.IsIPv6Address(Address1)
      and TIdIPAddressParser.IsIPv6Address(Address2)
      and TIdIPAddressParser.IsIPv6Address(NetMask) then begin
      TIdIPAddressParser.ParseIPv6Address(Address1, Ip6Addr1);
      TIdIPAddressParser.ParseIPv6Address(Address2, Ip6Addr2);
      TIdIPAddressParser.ParseIPv6Address(Netmask, Ip6Mask);
      Result := true;
      for I := Low(Ip6Mask) to High(Ip6Mask) do begin
        Result := Result
              and ((Ip6Addr1[I] and Ip6Mask[I]) = (Ip6Addr2[I] and Ip6Mask[I]));
      end;
    end
    else
      Result := false;
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

function RoutableAddress: String;
begin
  // If you have a machine sitting behind a Network Address Translator (NAT),
  // your LocalAddress will probably return an IP in one of the private address
  // ranges (e.g., in the 192.168.0.0/24 subnet). These addresses are by
  // definition not reachable by machines on the public Internet. This function
  // returns an address that other people can use to make calls to you.

  if (Length(idRoutableAddress)=0) or (idRoutableAddress='0.0.0.0') then begin
    // This is a stub implementation; a proper implementation might contact an
    // external server to discover the external IP, or use STUN, or something
    // similar.
    Result := LocalAddress;
  end else
    Result := idRoutableAddress;
end;

function ResolveARecords(Name: String; ResolvedList: TStrings): Integer;
var
  AHostEnt: PHostEnt;
  I, J:     Integer;
  AString:  String;
begin
  PossiblyInitialiseWinsock;
  ResolvedList.Clear;
  AHostEnt := GetHostByName(PChar(Name));
  if (AHostEnt <> nil) then begin
    if (AHostEnt.h_addrtype = AF_INET) then begin
      I := 0;
      while (AHostEnt^.h_addr_list[I] <> nil) do begin
        AString := '';
        for J := 0 to AHostEnt^.h_length - 1 do begin
          if (J > 0) then
             AString := AString + '.';
          AString := AString + IntToStr(AHostEnt^.h_addr_list[I]^[J]);
        end;
        ResolvedList.Add(AString);
        Inc(I);
      end;
      Result := 0;
    end
    else
      Result := WSANO_DATA;
  end
  else
  begin
    Result := WSAGetLastError;
  end;
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

initialization
  idLocalAddress:='0.0.0.0';
  idRoutableAddress:='0.0.0.0';
  idNetMask:='0.0.0.0';

  WinsockLock := TCriticalSection.Create;
finalization
  if WinsockStarted then
    WSACleanup;

  WinsockLock.Free;
end.
