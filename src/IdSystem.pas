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

function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
function  ConstructUUID: String;
function  ConstructUUIDURN: String;
function  GetBestLocalAddress(DestinationAddress: String): String;
function  GetCurrentProcessId: Cardinal;
function  GetFullUserName: WideString;
function  GetHostName: String;
function  GetTickCount: Cardinal;
function  GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
function  GetUserName: WideString;
function  HtoNL(N: Cardinal): Cardinal;
function  LocalAddress: String;
function  RoutableAddress: String;
procedure LocalAddresses(IPs: TStrings);
procedure DefineLocalAddress(AAddress: String); {Allows you to use a specified local IP address}
procedure DefineRoutableAddress(AAddress: String); {Allows you to set a public IP address}
procedure DefineNetMask(AMask: String); {Let you set the netmask, used to identify if any IP address is local or not}
function OnSameNetwork(AAddress1, AAddress2: String): Boolean; overload;
function OnSameNetwork(Address1, Address2, Netmask: String): Boolean; overload;
function  ResolveARecords(Name: String; ResolvedList: TStrings): Integer;

implementation

uses
  IdSimpleParser, IdGlobal, IdStack, IdUDPServer, SysUtils, Windows, Winsock;

const
  ANY_SIZE = 100;

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

const
  IpHelper  = 'iphlpapi.dll';
  WinSocket = 'wsock32.dll';

function GetBestInterface(dwDestAddr: TIpAddr; var pdwBestIfIndex: DWORD): DWORD; stdcall; external IpHelper name 'GetBestInterface';
function GetBestRoute(dwDestAddr: DWORD; dwSourceAddr: DWORD; var pBestRoute: TMibIpForwardRow): DWORD; stdcall; external IpHelper name 'GetBestRoute';
function GetIpAddrTable(pIpAddrTable: PMibIpAddrTable; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall; external IpHelper name 'GetIpAddrTable';

// WinSock's gethostbyname & hostent have terrible declarations. We override
// these to provide a more natural interface to the information.
function GetHostByName(Name: PChar): PHostEnt; stdcall; external WinSocket name 'gethostbyname';

{See commentary for LocalAddress and RoutableAddress for explanations of these variables}
var
  idLocalAddress, idRoutableAddress, idNetMask: String;

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
        Result := TIdIPAddressParser.IPv4AddressToStr(ntohl(Table.table[I].dwAddr));
        Break;
      end;
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
         '');

  if TIdIPAddressParser.IsIPv4Address(DestinationIP) then begin
    DstAddr := HtoNL(TIdIPAddressParser.InetAddr(DestinationIP));
    SrcAddr := HtoNL(TIdIPAddressParser.InetAddr(LocalIP));

    GetBestRoute(DstAddr, SrcAddr, Route);
    Result := Route.dwForwardDest = 0;
  end
  else begin
    {$IFDEF INET6}

    raise Exception.Create('Implement BestRouteIsDefaultRoute for IPv6');
    {$ELSE}
    Result := false;
    {$ENDIF}
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

  if (RC <> 0) then Exit;

  Result := GetIpAddress(InterfaceIndex);
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

{See commentary for LocalAddress for an explanation of this function}
procedure DefineLocalAddress(AAddress: String);
begin
  idLocalAddress:=AAddress;
end;

procedure DefineRoutableAddress(AAddress: String); {Allows you to set a public IP address}
begin
  idRoutableAddress:=AAddress;
end;

procedure DefineNetMask(AMask: String); {Let you set the netmask, used to identify if any IP address is local or not}
begin
  idNetMask:=AMask;
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

function ResolveARecords(Name: String; ResolvedList: TStrings): Integer;
var
  AHostEnt: PHostEnt;
  I, J:     Integer;
  AString:  String;
begin
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

initialization
  idLocalAddress:='0.0.0.0';
  idRoutableAddress:='0.0.0.0';
  idNetMask:='0.0.0.0';

end.
