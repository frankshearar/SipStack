{
  (c) 2009 Technology Directorate, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdNetworking;

interface

uses
  IdSipMockDnsServer, TestFramework;

type
  // This suite currently only supports Windows (2000).
  TestFunctions = class(TTestCase)
  private
    NameServer: TIdSipMockDnsServer;

    procedure RestartWinsock;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckPortFree(Address: String; Port: Cardinal; Msg: String);
  published
    procedure TestBestRouteIsDefaultRoute;
    procedure TestDefineLocalAddress;
    procedure TestDefineRoutableAddress;
    procedure TestGetBestLocalAddress;
    procedure TestGetHostName;
    procedure TestGetHostNameNoWinsock;
    procedure TestIsPortFreeUDP;
    procedure TestIsPortFreeTCP;
    procedure TestOnSameNetwork;
    procedure TestOnSameNetworkWithMixedAddresses;
    procedure TestOnSameNetworkWithNetmask;
    procedure TestOnSameNetworkWithNetmaskIPv6;
//    procedure TestResolveARecords;
  end;

implementation

uses
  Classes, IdNetworking, IdSocketHandle, IdSimpleParser, IdSipMessage,
  IdTcpServer, IdUdpServer, SysUtils, TypInfo, Windows, Winsock;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Networking unit tests');
  Result.AddTest(TestFunctions.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.NameServer := TIdSipMockDnsServer.Create;
  Self.NameServer.Stop;
end;

procedure TestFunctions.TearDown;
begin
  Self.NameServer.Free;

  inherited TearDown;
end;

//* TestFunctions Private methods **********************************************

procedure TestFunctions.CheckPortFree(Address: String; Port: Cardinal; Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUDPServer;
  FailMsg: String;
begin
  FailMsg := 'Port ' + Address + ':' + IntToStr(Port) + ' is not free';
  if (Msg <> '') then
    FailMsg := Msg + ': ' + FailMsg;

  Server := TIdUDPServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    Binding.IP   := Address;
    Binding.Port := Port;

    try
      Server.Active := true;
    except
      on EIdCouldNotBindSocket do begin
        Fail(FailMsg);
      end;
    end;
  finally
    Server.Free;
  end;
end;

procedure TestFunctions.RestartWinsock;
var
  Data:      TWSAData;
  ErrorCode: Integer;
  RC:        Integer;
begin
  RC := WSAStartup($202, Data);

  if (RC <> 0) then begin
    ErrorCode := WSAGetLastError;
    raise Exception.Create('RestartWinsock: ' + IntToStr(ErrorCode) + ' (' + SysErrorMessage(ErrorCode) + ')');
  end;
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestBestRouteIsDefaultRoute;
var
  Addresses:   TStrings;
  Destination: String;
  I:           Integer;
begin
  // This test isn't foolproof.

  Destination := Localhost(Id_IPv4);
  Check(not BestRouteIsDefaultRoute(Destination, Localhost(Id_IPv4)), Destination + ' from ' + Localhost(Id_IPv4));

  Destination := TIdIPAddressParser.IncIPAddress(Destination);
  Check(not BestRouteIsDefaultRoute(Destination, Localhost(Id_IPv4)), Destination + ' from ' + Localhost(Id_IPv4));

  Destination := Localhost(Id_IPv6);
  Check(not BestRouteIsDefaultRoute(Destination, Localhost(Id_IPv6)), Destination + ' from ' + Localhost(Id_IPv6));

  Destination := TIdIPAddressParser.IncIPAddress(Destination);
  Check(not BestRouteIsDefaultRoute(Destination, Localhost(Id_IPv6)), Destination + ' from ' + Localhost(Id_IPv6));

  Addresses := TStringList.Create;
  try
    LocalAddresses(Addresses);
    for I := 0 to Addresses.Count - 1 do begin
      Destination := Addresses[I];
      Check(not BestRouteIsDefaultRoute(Destination, Addresses[I]), Destination + ' from ' + Addresses[I]);

      Destination := TIdIPAddressParser.IncIPAddress(Addresses[I]);
      Check(not BestRouteIsDefaultRoute(Destination, Addresses[I]), Destination + ' from ' + Addresses[I]);

      if TIdIPAddressParser.IsIPv4Address(Addresses[I]) then begin
        Check(BestRouteIsDefaultRoute('1.2.3.4', Addresses[I]), '1.2.3.4 from ' + Addresses[I]);
        Check(BestRouteIsDefaultRoute('90.3.135.129', Addresses[I]), '90.3.135.129 from ' + Addresses[I]);
      end
      else
        Check(BestRouteIsDefaultRoute('2002:deca:fbad::1', Addresses[I]), '2002:deca:fbad::1 from ' + Addresses[I]);
    end;
  finally
    Addresses.Free;
  end;
end;

procedure TestFunctions.TestDefineLocalAddress;
const
  AddressOne = '1.2.3.4';
  AddressTwo = '2.4.6.8';
begin
  try
    DefineLocalAddress(AddressOne);
    CheckEquals(AddressOne, LocalAddress, 'LocalAddress not set to the defined local address');

    DefineLocalAddress(AddressTwo);
    CheckEquals(AddressTwo, LocalAddress, 'LocalAddress not re-set');
  finally
    DefineLocalAddress('');
  end;
end;

procedure TestFunctions.TestDefineRoutableAddress;
const
  AddressOne = '1.2.3.4';
  AddressTwo = '2.4.6.8';
begin
  DefineRoutableAddress(AddressOne);
  CheckEquals(AddressOne, RoutableAddress, 'RoutableAddress not set to the defined routable address');

  DefineRoutableAddress(AddressTwo);
  CheckEquals(AddressTwo, RoutableAddress, 'RoutableAddress not re-set');
end;

procedure TestFunctions.TestGetBestLocalAddress;
var
  Addresses: TStrings;
  Dest:      String;
  I:         Integer;
begin
  Addresses := TStringList.Create;
  try
    LocalAddresses(Addresses);

    for I := 0 to Addresses.Count - 1 do begin
      Dest := TIdIpAddressParser.IncIPAddress(Addresses[I]);
      CheckEquals(Addresses[I], GetBestLocalAddress(Dest), 'Wrong local address for ' + Dest);
    end;
  finally
    Addresses.Free;
  end;
end;

procedure TestFunctions.TestGetHostName;
var
  Buf:       PAnsiChar;
  ErrorCode: Integer;
  Len:       Integer;
  RC:        Integer;
begin
  Len := 1000;
  GetMem(Buf, Len*Sizeof(AnsiChar));
  try
    RC := Winsock.gethostname(Buf, Len);

    if (RC <> 0) then begin
      ErrorCode := WSAGetLastError;
      Fail('gethostname failed: '
         + IntToStr(ErrorCode) + '(' + SysErrorMessage(ErrorCode) + ')');
    end;

    CheckEquals(Buf, IdNetworking.GetHostName, 'GetHostName');
  finally
    FreeMem(Buf);
  end;
end;

procedure TestFunctions.TestGetHostNameNoWinsock;
var
  Buf:       PAnsiChar;
  ErrorCode: Integer;
  Len:       Integer;
  RC:        Integer;
  WinsockRunning: Integer;
begin
  WinsockRunning := WSACleanup;
  try
    Len := 1000;
    GetMem(Buf, Len*Sizeof(AnsiChar));
    try
      RC := Winsock.gethostname(Buf, Len);

      if (RC <> 0) then begin
        ErrorCode := WSAGetLastError;
        Fail('gethostname failed: '
           + IntToStr(ErrorCode) + '(' + SysErrorMessage(ErrorCode) + ')');
      end;

      CheckEquals(Buf, IdNetworking.GetHostName, 'GetHostName');
    finally
      FreeMem(Buf);
    end;
  finally
    if (WinsockRunning <> WSANOTINITIALISED) then
      Self.RestartWinsock;
  end;
end;

procedure TestFunctions.TestIsPortFreeUDP;
const
  IP   = '127.0.0.1';
  Port = 1234;
var
  S: TIdUdpServer;
begin
  S := TIdUdpServer.Create(nil);
  try
    Check(IsPortFree(UdpTransport, IP, Port), 'Port not free?');

    S.Bindings.Add;
    S.Bindings[0].IP := IP;
    S.Bindings[0].Port := Port;
    S.Active := true;

    Check(not IsPortFree(UdpTransport, IP, Port), 'Port free?');

    S.Active := false;
    Check(IsPortFree(UdpTransport, IP, Port), 'Port not free after server stopped');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestIsPortFreeTCP;
const
  IP   = '127.0.0.1';
  Port = 1234;
var
  S: TIdTcpServer;
begin
  S := TIdTcpServer.Create(nil);
  try
    Check(IsPortFree(TcpTransport, IP, Port), 'Port not free?');

    S.Bindings.Add;
    S.Bindings[0].IP := IP;
    S.Bindings[0].Port := Port;
    S.Active := true;

    Check(not IsPortFree(TcpTransport, IP, Port), 'Port free?');

    S.Active := false;
    Check(IsPortFree(TcpTransport, IP, Port), 'Port not free after server stopped');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestOnSameNetwork;
begin
  DefineNetMask('0.0.0.0');
  // A "subnet" of all possible IPs
  Check(OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/0 + 1.1.1.1/0');
  Check(OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/0 + 1.1.2.1/0');
  Check(OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/0 + 1.2.1.1/0');
  Check(OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/0 + 2.1.1.1/0');

  // A class subnet
  DefineNetMask('255.0.0.0');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/8 + 1.1.1.1/8');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2'), '1.1.1.1/8 + 1.1.1.2/8');
  Check(    OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/8 + 1.1.2.1/8');
  Check(    OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/8 + 1.2.1.1/8');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/8 + 2.1.1.1/8');

  // B class subnet
  DefineNetMask('255.255.0.0');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/16 + 1.1.1.1/16');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2'), '1.1.1.1/16 + 1.1.1.2/16');
  Check(    OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/16 + 1.1.2.1/16');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/16 + 1.2.1.1/16');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/16 + 2.1.1.1/16');

  // C class subnet
  DefineNetMask('255.255.255.0');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/24 + 1.1.1.1/24');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2'), '1.1.1.1/24 + 1.1.1.2/24');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/24 + 1.1.2.1/24');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/24 + 1.2.1.1/24');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/24 + 2.1.1.1/24');

  // A subnet of 8 IP addresses
  DefineNetMask('255.255.255.248');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/29 + 1.1.1.1/29');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2'), '1.1.1.1/29 + 1.1.1.2/29');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/29 + 1.1.2.1/29');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/29 + 1.2.1.1/29');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/29 + 2.1.1.1/29');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.9'), '1.1.1.1/29 + 1.1.1.9/29');
  Check(    OnSameNetwork('1.1.1.254', '1.1.1.255'), '1.1.1.254/29 + 1.1.1.255/29');

  // A "subnet" of 1 IP
  DefineNetMask('255.255.255.255');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1'), '1.1.1.1/32 + 1.1.1.1/32');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.2'), '1.1.1.1/32 + 1.1.1.2/32');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1'), '1.1.1.1/32 + 1.1.2.1/32');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1'), '1.1.1.1/32 + 1.2.1.1/32');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1'), '2.1.1.1/32 + 2.1.1.1/32');

  // Badly formed parameters
  DefineNetMask('0.0.0.0');
  Check(not OnSameNetwork('256.0.0.0', '1.1.1.1'), '''256.0.0.0'' as first parameter');
  Check(not OnSameNetwork('abcd', '1.1.1.1'), '''abcd'' as first parameter');

  DefineNetMask('0.0.0.0');
  Check(not OnSameNetwork('1.1.1.1', '256.0.0.0'), '''256.0.0.0'' as second parameter');

  DefineNetMask('256.0.0.0');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.1'), '''256.0.0.0'' as netmask');
end;

procedure TestFunctions.TestOnSameNetworkWithMixedAddresses;
const
  IPv4Address = '127.0.0.1';
  IPv4Mask    = '0.0.0.0';
  IPv6Address = '::1';
  IPv6Mask    = '::';
begin
  Check(not OnSameNetwork(IPv4Address, IPv4Address, IPv6Mask), '1st address IPv4, 2nd address IPv4, mask IPv6');
  Check(not OnSameNetwork(IPv4Address, IPv6Address, IPv4Mask), '1st address IPv4, 2nd address IPv6, mask IPv6');
  Check(not OnSameNetwork(IPv4Address, IPv6Address, IPv6Mask), '1st address IPv4, 2nd address IPv6, mask IPv6');
  Check(not OnSameNetwork(IPv6Address, IPv4Address, IPv4Mask), '1st address IPv6, 2nd address IPv4, mask IPv4');
  Check(not OnSameNetwork(IPv6Address, IPv4Address, IPv6Mask), '1st address IPv6, 2nd address IPv4, mask IPv6');
  Check(not OnSameNetwork(IPv6Address, IPv6Address, IPv4Mask), '1st address IPv6, 2nd address IPv6, mask IPv4');
end;

procedure TestFunctions.TestOnSameNetworkWithNetmask;
begin
  // A "subnet" of all possible IPs
  Check(OnSameNetwork('1.1.1.1', '1.1.1.1', '0.0.0.0'), '1.1.1.1/0 + 1.1.1.1/0');
  Check(OnSameNetwork('1.1.1.1', '1.1.2.1', '0.0.0.0'), '1.1.1.1/0 + 1.1.2.1/0');
  Check(OnSameNetwork('1.1.1.1', '1.2.1.1', '0.0.0.0'), '1.1.1.1/0 + 1.2.1.1/0');
  Check(OnSameNetwork('1.1.1.1', '2.1.1.1', '0.0.0.0'), '2.1.1.1/0 + 2.1.1.1/0');

  // A class subnet
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1', '255.0.0.0'), '1.1.1.1/8 + 1.1.1.1/8');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2', '255.0.0.0'), '1.1.1.1/8 + 1.1.1.2/8');
  Check(    OnSameNetwork('1.1.1.1', '1.1.2.1', '255.0.0.0'), '1.1.1.1/8 + 1.1.2.1/8');
  Check(    OnSameNetwork('1.1.1.1', '1.2.1.1', '255.0.0.0'), '1.1.1.1/8 + 1.2.1.1/8');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1', '255.0.0.0'), '2.1.1.1/8 + 2.1.1.1/8');

  // B class subnet
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1', '255.255.0.0'), '1.1.1.1/16 + 1.1.1.1/16');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2', '255.255.0.0'), '1.1.1.1/16 + 1.1.1.2/16');
  Check(    OnSameNetwork('1.1.1.1', '1.1.2.1', '255.255.0.0'), '1.1.1.1/16 + 1.1.2.1/16');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1', '255.255.0.0'), '1.1.1.1/16 + 1.2.1.1/16');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1', '255.255.0.0'), '2.1.1.1/16 + 2.1.1.1/16');

  // C class subnet
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1', '255.255.255.0'), '1.1.1.1/24 + 1.1.1.1/24');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2', '255.255.255.0'), '1.1.1.1/24 + 1.1.1.2/24');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1', '255.255.255.0'), '1.1.1.1/24 + 1.1.2.1/24');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1', '255.255.255.0'), '1.1.1.1/24 + 1.2.1.1/24');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1', '255.255.255.0'), '2.1.1.1/24 + 2.1.1.1/24');

  // A subnet of 8 IP addresses
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1', '255.255.255.248'), '1.1.1.1/29 + 1.1.1.1/29');
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.2', '255.255.255.248'), '1.1.1.1/29 + 1.1.1.2/29');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1', '255.255.255.248'), '1.1.1.1/29 + 1.1.2.1/29');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1', '255.255.255.248'), '1.1.1.1/29 + 1.2.1.1/29');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1', '255.255.255.248'), '2.1.1.1/29 + 2.1.1.1/29');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.9', '255.255.255.248'), '1.1.1.1/29 + 1.1.1.9/29');
  Check(    OnSameNetwork('1.1.1.254', '1.1.1.255', '255.255.255.248'), '1.1.1.254/29 + 1.1.1.255/29');

  // A "subnet" of 1 IP
  Check(    OnSameNetwork('1.1.1.1', '1.1.1.1', '255.255.255.255'), '1.1.1.1/32 + 1.1.1.1/32');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.2', '255.255.255.255'), '1.1.1.1/32 + 1.1.1.2/32');
  Check(not OnSameNetwork('1.1.1.1', '1.1.2.1', '255.255.255.255'), '1.1.1.1/32 + 1.1.2.1/32');
  Check(not OnSameNetwork('1.1.1.1', '1.2.1.1', '255.255.255.255'), '1.1.1.1/32 + 1.2.1.1/32');
  Check(not OnSameNetwork('1.1.1.1', '2.1.1.1', '255.255.255.255'), '2.1.1.1/32 + 2.1.1.1/32');

  // Badly formed parameters
  Check(not OnSameNetwork('256.0.0.0', '1.1.1.1', '0.0.0.0'), '''256.0.0.0'' as first parameter');
  Check(not OnSameNetwork('abcd', '1.1.1.1', '0.0.0.0'), '''abcd'' as first parameter');
  Check(not OnSameNetwork('1.1.1.1', '256.0.0.0', '0.0.0.0'), '''256.0.0.0'' as second parameter');
  Check(not OnSameNetwork('1.1.1.1', '1.1.1.1', '256.0.0.0'), '''256.0.0.0'' as netmask');
end;

procedure TestFunctions.TestOnSameNetworkWithNetmaskIPv6;
begin
  Check(    OnSameNetwork('::1',             '::1', '::'), '::1/0 + ::1/0');
  Check(    OnSameNetwork('::1',             '::2', '::'), '::1/0 + ::2/0');
  Check(    OnSameNetwork('::1',           '::1:1', '::'), '::1/0 + ::1:1/0');
  Check(    OnSameNetwork('::1',         '::1:1:1', '::'), '::1/0 + ::1:1:1/0');
  Check(    OnSameNetwork('::1',       '::1:1:1:1', '::'), '::1/0 + ::1:1:1:1/0');
  Check(    OnSameNetwork('::1',     '::1:1:1:1:1', '::'), '::1/0 + ::1:1:1:1:1/0');
  Check(    OnSameNetwork('::1',   '::1:1:1:1:1:1', '::'), '::1/0 + ::1:1:1:1:1:1/0');
  Check(    OnSameNetwork('::1', '::1:1:1:1:1:1:1', '::'), '::1/0 + ::1:1:1:1:1:1:1/0');
  Check(    OnSameNetwork('::1', '1:1:1:1:1:1:1:1', '::'), '::1/0 + 1:1:1:1:1:1:1:1/0');
  Check(    OnSameNetwork('::1',             '::1', 'ff::'), '::1/16 + ::1/16');
  Check(    OnSameNetwork('::1',             '::2', 'ff::'), '::1/16 + ::2/16');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff::'), '::1/16 + ::1:1/16');
  Check(    OnSameNetwork('::1',         '::1:1:1', 'ff::'), '::1/16 + ::1:1:1/16');
  Check(    OnSameNetwork('::1',       '::1:1:1:1', 'ff::'), '::1/16 + ::1:1:1:1/16');
  Check(    OnSameNetwork('::1',     '::1:1:1:1:1', 'ff::'), '::1/16 + ::1:1:1:1:1/16');
  Check(    OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff::'), '::1/16 + ::1:1:1:1:1:1/16');
  Check(    OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff::'), '::1/16 + ::1:1:1:1:1:1:1/16');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff::'), '::1/16 + 1:1:1:1:1:1:1:1/16');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff::'), '::1/32 + ::1/32');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff::'), '::1/32 + ::2/32');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff:ff::'), '::1/32 + ::1:1/32');
  Check(    OnSameNetwork('::1',         '::1:1:1', 'ff:ff::'), '::1/32 + ::1:1:1/32');
  Check(    OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff::'), '::1/32 + ::1:1:1:1/32');
  Check(    OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff::'), '::1/32 + ::1:1:1:1:1/32');
  Check(    OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff::'), '::1/32 + ::1:1:1:1:1:1/32');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff::'), '::1/32 + ::1:1:1:1:1:1:1/32');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff::'), '::1/32 + 1:1:1:1:1:1:1:1/32');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff::'), '::1/48 + ::1/48');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff:ff::'), '::1/48 + ::2/48');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff:ff:ff::'), '::1/48 + ::1:1/48');
  Check(    OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff::'), '::1/48 + ::1:1:1/48');
  Check(    OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff::'), '::1/48 + ::1:1:1:1/48');
  Check(    OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff::'), '::1/48 + ::1:1:1:1:1/48');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff::'), '::1/48 + ::1:1:1:1:1:1/48');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff::'), '::1/48 + ::1:1:1:1:1:1:1/48');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff::'), '::1/48 + 1:1:1:1:1:1:1:1/48');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff:ff::'), '::1/64 + ::1/64');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff:ff:ff::'), '::1/64 + ::2/64');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1/64');
  Check(    OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1:1/64');
  Check(    OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1:1:1/64');
  Check(not OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1:1:1:1/64');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1:1:1:1:1/64');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff:ff::'), '::1/64 + ::1:1:1:1:1:1:1/64');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff:ff::'), '::1/64 + 1:1:1:1:1:1:1:1/64');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1/80');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff:ff:ff:ff::'), '::1/80 + ::2/80');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1/80');
  Check(    OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1:1/80');
  Check(not OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1:1:1/80');
  Check(not OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1:1:1:1/80');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1:1:1:1:1/80');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + ::1:1:1:1:1:1:1/80');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff::'), '::1/80 + 1:1:1:1:1:1:1:1/80');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1/96');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::2/96');
  Check(    OnSameNetwork('::1',           '::1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1/96');
  Check(not OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1:1/96');
  Check(not OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1:1:1/96');
  Check(not OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1:1:1:1/96');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1:1:1:1:1/96');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + ::1:1:1:1:1:1:1/96');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff::'), '::1/96 + 1:1:1:1:1:1:1:1/96');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1/112');
  Check(    OnSameNetwork('::1',             '::2', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::2/112');
  Check(not OnSameNetwork('::1',           '::1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1/112');
  Check(not OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1:1/112');
  Check(not OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1:1:1/112');
  Check(not OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1:1:1:1/112');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1:1:1:1:1/112');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + ::1:1:1:1:1:1:1/112');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff::'), '::1/112 + 1:1:1:1:1:1:1:1/112');
  Check(    OnSameNetwork('::1',             '::1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1/128');
  Check(not OnSameNetwork('::1',             '::2', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::2/128');
  Check(not OnSameNetwork('::1',           '::1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1/128');
  Check(not OnSameNetwork('::1',         '::1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1:1/128');
  Check(not OnSameNetwork('::1',       '::1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1:1:1/128');
  Check(not OnSameNetwork('::1',     '::1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1:1:1:1/128');
  Check(not OnSameNetwork('::1',   '::1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1:1:1:1:1/128');
  Check(not OnSameNetwork('::1', '::1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + ::1:1:1:1:1:1:1/128');
  Check(not OnSameNetwork('::1', '1:1:1:1:1:1:1:1', 'ff:ff:ff:ff:ff:ff:ff:ff::'), '::1/128 + 1:1:1:1:1:1:1:1/128');
end;
{
procedure TestFunctions.TestResolveARecords;
var
  Addresses: TStringList;
begin
  // Small problem: this test assumes, incorrectly, that ResolveARecords
  // contacts Self.NameServer. It doesn't. It requires your local nameserver to
  // have two A records for paranoid.leo-ix.net. The author is the only one with
  // such a setup.

  CheckPortFree('127.0.0.1', 53, 'This test requires a free port: 127.0.0.1:53/udp');

  Self.NameServer.AddAnswer(Self.NameServer.ARecords);

  Addresses := TStringList.Create;
  try
    ResolveARecords('paranoid.leo-ix.net', Addresses);

    CheckEquals(2, Addresses.Count, 'Address count');

    Addresses.Sort;
    CheckEquals('127.0.0.1', Addresses[0], 'First address');
    CheckEquals('127.0.0.2', Addresses[1], 'Second address');
  finally
    Addresses.Free;
  end;
end;
}
initialization
  RegisterTest('System-specific network functions', Suite);
end.
