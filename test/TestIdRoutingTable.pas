{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRoutingTable;

interface

uses
  IdMockRoutingTable, IdRoutingTable, TestFramework;

type
  TestTIdRouteEntry = class(TTestCase)
  private
    Route: TIdRouteEntry;
  protected
    function MakeRoute: TIdRouteEntry; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEquals; virtual;
    procedure TestEqualsDestinationDiffers;
    procedure TestEqualsGatewayDiffers;
    procedure TestEqualsInterfaceIndexDiffers;
    procedure TestEqualsMaskDiffers;
    procedure TestEqualsMetricDiffers;
    procedure TestIsIPv4Address;
    procedure TestIsMappedRoute; virtual;
    procedure TestSetDestination;
    procedure TestSetDestinationIPv6;
    procedure TestSetDestInetAddr;
    procedure TestSetMask;
    procedure TestSetMaskInetAddr;
    procedure TestWillRoute;
    procedure TestWillRouteIpv6;
  end;

  // While we use a mock routing table in these tests, note that the algorithms
  // under test cannot be overridden by subclasses. Ergo, real routing tables
  // will work the same way.
  TestTIdIPv4RoutingTable = class(TTestCase)
  private
    RouteA: TIdRouteEntry;
    RouteB: TIdRouteEntry;
    RT:     TIdMockRoutingTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMappedRouteAndCount;
    procedure TestGatewayForNoRoutes;
    procedure TestGatewayForOneMappedRoute;
    procedure TestGatewayForTwoMappedRoutes;
    procedure TestHasRoute;
    procedure TestRemoveRoute;
    procedure TestRouteSortDestinationAndMaskDiffers;
    procedure TestRouteSortGatewayAndMetricDiffers;
    procedure TestRouteSortIPv4RoutesFirst;
    procedure TestRouteSortMetricDiffers;
    procedure TestRouteSortOneRouteAMappedRoute;
    procedure TestRouteSortOnlyDestinationDiffers;
    procedure TestRouteSortOnlyGatewayDiffers;
    procedure TestRouteSortOnlyMaskDiffers;
    procedure TestRouteSortOnlyMetricDiffers;
    procedure TestRouteSortSameRoute;
  end;

implementation

uses
  IdSimpleParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSystem unit tests');
  Result.AddTest(TestTIdRouteEntry.Suite);
  Result.AddTest(TestTIdIPv4RoutingTable.Suite);
end;

//******************************************************************************
//* TestTIdRouteEntry                                                          *
//******************************************************************************
//* TestTIdRouteEntry Public methods *******************************************

procedure TestTIdRouteEntry.SetUp;
begin
  inherited SetUp;

  Self.Route := Self.MakeRoute;
  Self.Route.Destination    := '10.0.0.0';
  Self.Route.Gateway        := '10.0.0.1';
  Self.Route.InterfaceIndex := 'rl0';
  Self.Route.Mask           := '255.0.0.0';
  Self.Route.Metric         := 1;
end;

procedure TestTIdRouteEntry.TearDown;
begin
  Self.Route.Free;

  inherited SetUp;
end;

//* TestTIdRouteEntry Protected methods ****************************************

function TestTIdRouteEntry.MakeRoute: TIdRouteEntry;
begin
  Result := TIdRouteEntry.Create;
end;

//* TestTIdRouteEntry Published methods ****************************************

procedure TestTIdRouteEntry.TestEquals;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Check(Self.Route.Equals(Other), 'Self.Route must equal Other');
    Check(Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestEqualsDestinationDiffers;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Other.Destination := TIdIPAddressParser.IncIPAddress(Self.Route.Destination);
    Check(not Self.Route.Equals(Other), 'Self.Route <> Other because their destinations differ');
    Check(not Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestEqualsGatewayDiffers;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Other.Gateway := TIdIPAddressParser.IncIPAddress(Self.Route.Gateway);
    Check(not Self.Route.Equals(Other), 'Self.Route <> Other because their gateways differ');
    Check(not Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestEqualsInterfaceIndexDiffers;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Other.InterfaceIndex := Self.Route.InterfaceIndex + '1';
    Check(Self.Route.Equals(Other), 'InterfaceIndexes are irrelevant for route comparison');
    Check(Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestEqualsMaskDiffers;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Other.Mask := TIdIPAddressParser.IncIPAddress(Self.Route.Mask);
    Check(not Self.Route.Equals(Other), 'Self.Route <> Other because their masks differ');
    Check(not Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestEqualsMetricDiffers;
var
  Other: TIdRouteEntry;
begin
  Other := Self.Route.Clone;
  try
    Other.Metric := Self.Route.Metric + 1;
    Check(Self.Route.Equals(Other), 'Metrics are irrelevant for route comparison');
    Check(Other.Equals(Self.Route), 'Equals must be reflexive');
  finally
    Other.Free;
  end;
end;

procedure TestTIdRouteEntry.TestIsIPv4Address;
const
  IPv4Destination      = '0.0.0.0';
  IPv6Destination      = '::';
  MalformedDestination = '';
begin
  Self.Route.Destination := IPv4Destination;
  Check(Self.Route.IsIPv4Route, 'IPv4 route');

  Self.Route.Destination := IPv6Destination;
  Check(not Self.Route.IsIPv4Route, 'IPv6 route');

  Self.Route.Destination := MalformedDestination;
  Check(not Self.Route.IsIPv4Route, 'Malformed route');
end;

procedure TestTIdRouteEntry.TestIsMappedRoute;
begin
  Check(not Self.Route.IsMappedRoute, 'Normal route looks like a mapped route');
end;

procedure TestTIdRouteEntry.TestSetDestination;
const
  Destination        = '10.0.0.0';
  DestInetAddr       = $0a000000;
  SecondDestination  = '10.0.1.0';
  SecondDestInetAddr = $0a000100;
begin
  Self.Route.Destination := Destination;
  CheckEquals(Destination, Self.Route.Destination, 'Destination not set');
  CheckEquals(IntToHex(DestInetAddr, 8),
              IntToHex(Self.Route.DestInetAddrIPv4, 8),
              'DestInetAddrIPv4 not set');

  Self.Route.Destination := SecondDestination;
  CheckEquals(SecondDestination, Self.Route.Destination, 'Destination not reset');
  CheckEquals(IntToHex(SecondDestInetAddr, 8),
              IntToHex(Self.Route.DestInetAddrIPv4, 8),
              'DestInetAddrIPv4 not reset');
end;

procedure TestTIdRouteEntry.TestSetDestinationIPv6;
const
  Destination = '::';
begin
  Self.Route.Destination := Destination;
  CheckEquals(Destination, Self.Route.Destination, 'Destination not set');
  CheckEquals(IntToHex(0, 8),
              IntToHex(Self.Route.DestInetAddrIPv4, 8),
              'DestInetAddrIPv4 set');
end;

procedure TestTIdRouteEntry.TestSetDestInetAddr;
const
  Destination        = '10.0.0.0';
  DestInetAddr       = $0a000000;
  SecondDestination  = '10.0.1.0';
  SecondDestInetAddr = $0a000100;
begin
  Self.Route.DestInetAddrIPv4 := DestInetAddr;
  CheckEquals(Destination, Self.Route.Destination, 'Destination not set');
  CheckEquals(IntToHex(DestInetAddr, 8),
              IntToHex(Self.Route.DestInetAddrIPv4, 8),
              'DestInetAddrIPv4 not set');

  Self.Route.DestInetAddrIPv4 := SecondDestInetAddr;
  CheckEquals(SecondDestination, Self.Route.Destination, 'Destination not reset');
  CheckEquals(IntToHex(SecondDestInetAddr, 8),
              IntToHex(Self.Route.DestInetAddrIPv4, 8),
              'DestInetAddrIPv4 not reset');
end;

procedure TestTIdRouteEntry.TestSetMask;
const
  Mask               = '255.0.0.0';
  MaskInetAddr       = $ff000000;
  SecondMask         = '255.255.0.0';
  SecondMaskInetAddr = $ffff0000;
begin
  Self.Route.Mask := Mask;
  CheckEquals(Mask, Self.Route.Mask, 'Mask not set');
  CheckEquals(IntToHex(MaskInetAddr, 8),
              IntToHex(Self.Route.MaskInetAddrIPv4, 8),
              'MaskInetAddrIPv4 not set');

  Self.Route.Mask := SecondMask;
  CheckEquals(SecondMask, Self.Route.Mask, 'Mask not reset');
  CheckEquals(IntToHex(SecondMaskInetAddr, 8),
              IntToHex(Self.Route.MaskInetAddrIPv4, 8),
              'MaskInetAddrIPv4 not reset');
end;

procedure TestTIdRouteEntry.TestSetMaskInetAddr;
const
  Mask               = '255.0.0.0';
  MaskInetAddr       = $ff000000;
  SecondMask         = '255.255.0.0';
  SecondMaskInetAddr = $ffff0000;
begin
  Self.Route.MaskInetAddrIPv4 := MaskInetAddr;
  CheckEquals(Mask, Self.Route.Mask, 'Mask not set');
  CheckEquals(IntToHex(MaskInetAddr, 8),
              IntToHex(Self.Route.MaskInetAddrIPv4, 8),
              'MaskInetAddrIPv4 not set');

  Self.Route.MaskInetAddrIPv4 := SecondMaskInetAddr;
  CheckEquals(SecondMask, Self.Route.Mask, 'Mask not reset');
  CheckEquals(IntToHex(SecondMaskInetAddr, 8),
              IntToHex(Self.Route.MaskInetAddrIPv4, 8),
              'MaskInetAddrIPv4 not reset');
end;

procedure TestTIdRouteEntry.TestWillRoute;
begin
  Self.Route.Destination := '127.0.0.0';
  Self.Route.Mask        := '255.0.0.0';

  Check(    Self.Route.WillRoute('127.0.0.1'), '127.0.0.0/8 + 127.0.0.1');
  Check(    Self.Route.WillRoute('127.0.0.2'), '127.0.0.0/8 + 127.0.0.2');
  Check(    Self.Route.WillRoute('127.0.1.1'), '127.0.0.0/8 + 127.0.1.1');
  Check(    Self.Route.WillRoute('127.1.0.1'), '127.0.0.0/8 + 127.1.0.1');
  Check(not Self.Route.WillRoute('128.0.0.1'), '127.0.0.0/8 + 128.0.0.1');
  Check(not Self.Route.WillRoute('2002:deca:fbad::1'), '127.0.0.0/8 + 2002:deca:fbad::1');

  Self.Route.Destination := '127.0.0.1';
  Self.Route.Mask        := '255.255.0.0';
  Check(    Self.Route.WillRoute('127.0.0.1'), '127.0.0.0/16 + 127.0.0.1');
  Check(    Self.Route.WillRoute('127.0.0.2'), '127.0.0.0/16 + 127.0.0.2');
  Check(   Self.Route.WillRoute('127.0.1.1'), '127.0.0.0/16 + 127.0.1.1');
  Check(not Self.Route.WillRoute('127.1.0.1'), '127.0.0.0/16 + 127.1.0.1');
  Check(not Self.Route.WillRoute('128.0.0.1'), '127.0.0.0/16 + 128.0.0.1');
  Check(not Self.Route.WillRoute('2002:deca:fbad::1'), '127.0.0.0/16 + 2002:deca:fbad::1');

  Self.Route.Destination := '127.0.0.1';
  Self.Route.Mask        := '255.255.255.0';
  Check(    Self.Route.WillRoute('127.0.0.1'), '127.0.0.0/24 + 127.0.0.1');
  Check(    Self.Route.WillRoute('127.0.0.2'), '127.0.0.0/24 + 127.0.0.2');
  Check(not Self.Route.WillRoute('127.0.1.1'), '127.0.0.0/24 + 127.0.1.1');
  Check(not Self.Route.WillRoute('127.1.0.1'), '127.0.0.0/24 + 127.1.0.1');
  Check(not Self.Route.WillRoute('128.0.0.1'), '127.0.0.0/24 + 128.0.0.1');
  Check(not Self.Route.WillRoute('2002:deca:fbad::1'), '127.0.0.0/24 + 2002:deca:fbad::1');

  Self.Route.Destination := '127.0.0.1';
  Self.Route.Mask        := '255.255.255.255';
  Check(    Self.Route.WillRoute('127.0.0.1'), '127.0.0.0/32 + 127.0.0.1');
  Check(not Self.Route.WillRoute('127.0.0.2'), '127.0.0.0/32 + 127.0.0.2');
  Check(not Self.Route.WillRoute('127.0.1.1'), '127.0.0.0/32 + 127.0.1.1');
  Check(not Self.Route.WillRoute('127.1.0.1'), '127.0.0.0/32 + 127.1.0.1');
  Check(not Self.Route.WillRoute('128.0.0.1'), '127.0.0.0/32 + 128.0.0.1');
  Check(not Self.Route.WillRoute('2002:deca:fbad::1'), '127.0.0.0/32 + 2002:deca:fbad::1');
end;

procedure TestTIdRouteEntry.TestWillRouteIpv6;
begin
  Self.Route.Destination := '::';
  Self.Route.Mask        := '::';
  Check(not Self.Route.WillRoute('127.0.0.1'), '::/0 + 127.0.0.1');
  Check(Self.Route.WillRoute('::1'), '::/0 + ::1');
  Check(Self.Route.WillRoute('::1:1'), '::/0 + ::1:1');
  Check(Self.Route.WillRoute('::1:1:1'), '::/0 + ::1:1:1');
  Check(Self.Route.WillRoute('::1:1:1:1'), '::/0 + ::1:1:1:1');
  Check(Self.Route.WillRoute('::1:1:1:1:1'), '::/0 + ::1:1:1:1:1');
  Check(Self.Route.WillRoute('::1:1:1:1:1:1'), '::/0 + ::1:1:1:1:1:1');
  Check(Self.Route.WillRoute('::1:1:1:1:1:1:1'), '::/0 + ::1:1:1:1:1:1:1');
  Check(Self.Route.WillRoute('1:1:1:1:1:1:1:1'), '::/0 + 1:1:1:1:1:1:1:1');
end;

//******************************************************************************
//* TestTIdIPv4RoutingTable                                                    *
//******************************************************************************
//* TestTIdIPv4RoutingTable Public methods *************************************

procedure TestTIdIPv4RoutingTable.SetUp;
begin
  inherited SetUp;

  Self.RouteA := TIdRouteEntry.Create;
  Self.RouteA.Destination    := '10.0.0.0';
  Self.RouteA.Gateway        := '10.0.0.1';
  Self.RouteA.InterfaceIndex := '1';
  Self.RouteA.Mask           := '255.0.0.0';
  Self.RouteA.Metric         := 1;

  Self.RouteB := TIdRouteEntry.Create;
  Self.RouteB.Destination    := Self.RouteA.Destination;
  Self.RouteB.Gateway        := Self.RouteA.Gateway;
  Self.RouteB.InterfaceIndex := Self.RouteA.InterfaceIndex;
  Self.RouteB.Mask           := Self.RouteA.Mask;
  Self.RouteB.Metric         := Self.RouteA.Metric;

  Self.RT := TIdMockRoutingTable.Create;
end;

procedure TestTIdIPv4RoutingTable.TearDown;
begin
  Self.RouteB.Free;
  Self.RouteA.Free;
  Self.RT.Free;

  inherited TearDown;
end;

//* TestTIdIPv4RoutingTable Published methods **********************************

procedure TestTIdIPv4RoutingTable.TestAddMappedRouteAndCount;
begin
  CheckEquals(0, Self.RT.RouteCount, 'Empty list');

  Self.RT.AddMappedRoute('1.2.3.4', '255.255.255.255', '4.3.2.1');
  CheckEquals(1, Self.RT.RouteCount, 'One mapped route');

  Self.RT.AddMappedRoute('1.2.3.4', '255.255.255.255', '4.3.2.1');
  CheckEquals(1, Self.RT.RouteCount, 'Duplicate mapped route added');

  Self.RT.AddMappedRoute('1.2.3.5', '255.255.255.255', '5.3.2.1');
  CheckEquals(2, Self.RT.RouteCount, 'Two mapped routes');
end;

procedure TestTIdIPv4RoutingTable.TestGatewayForNoRoutes;
begin
  CheckEquals('', Self.RT.GatewayFor('10.0.0.1'), 'LAN destination, no routes');
end;

procedure TestTIdIPv4RoutingTable.TestGatewayForOneMappedRoute;
const
  LanGateway = '10.0.0.1';
begin
  Self.RT.AddMappedRoute('0.0.0.0', '0.0.0.0', LanGateway);

  CheckEquals(LanGateway, Self.RT.GatewayFor('10.0.0.1'), 'LAN destination, one mapped route');
  CheckEquals(LanGateway, Self.RT.GatewayFor('1.2.3.4'), 'Internet destination, one mapped route');
end;

procedure TestTIdIPv4RoutingTable.TestGatewayForTwoMappedRoutes;
const
  InetGateway = '4.3.2.1';
  LanGateway = '10.0.0.1';
begin
  Self.RT.AddMappedRoute('0.0.0.0', '0.0.0.0', InetGateway);
  Self.RT.AddMappedRoute('10.0.0.0', '255.0.0.0', LanGateway);

  CheckEquals(LanGateway, Self.RT.GatewayFor('10.0.0.1'), 'LAN destination, mapped routes for LAN + inet');
  CheckEquals(InetGateway, Self.RT.GatewayFor('1.2.3.4'), 'Internet destination, mapped routes for LAN + inet');
end;

procedure TestTIdIPv4RoutingTable.TestHasRoute;
begin
  Check(not Self.RT.HasRoute(Self.RouteA), 'Empty table');

  Self.RT.AddRoute(Self.RouteA.Destination, Self.RouteA.Mask, Self.RouteA.Gateway, Self.RouteA.Metric, Self.RouteA.InterfaceIndex);
  Check(Self.RT.HasRoute(Self.RouteA), 'Route not added? not found?');
  Check(Self.RT.HasRoute(Self.RouteB), 'Duplicate of in-table route not found');

  Self.RouteB.Destination := TIdIPAddressParser.IncIPAddress(Self.RouteB.Destination);
  Check(not Self.RT.HasRoute(Self.RouteB), 'Route not in table still found');
end;

procedure TestTIdIPv4RoutingTable.TestRemoveRoute;
begin
  // Make sure that RouteA <> RouteB.
  Self.RouteB.Destination := TIdIPAddressParser.IncIPAddress(Self.RouteB.Destination);
  Check(not Self.RouteA.Equals(Self.RouteB), 'Sanity check');

  Self.RT.AddRoute(Self.RouteA);
  Self.RT.AddRoute(Self.RouteB);
  Self.RT.RemoveRoute(Self.RouteA.Destination, Self.RouteA.Mask, Self.RouteA.Gateway);

  CheckEquals(1, Self.RT.RouteCount, 'Route not removed');
  Check(not Self.RT.HasRoute(Self.RouteA), 'Wrong route removed');
  Check(    Self.RT.HasRoute(Self.RouteB), 'Wrong route left in the table');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortDestinationAndMaskDiffers;
begin
  // "Higher" addresses appear earlier in the sorted routing table.

  Self.RouteA.Destination := '10.0.0.0';
  Self.RouteA.Mask        := '255.0.0.0';
  Self.RouteB.Destination := '192.168.0.0';
  Self.RouteB.Mask        := '255.255.255.0';

  CheckEquals(1,  RouteSort(Self.RouteA, Self.RouteB), '10.0.0.0/8 > 192.168.0.0/24');
  CheckEquals(-1, RouteSort(Self.RouteB, Self.RouteA), '192.168.0.0/24 > 10.0.0.0/8');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortGatewayAndMetricDiffers;
var
  PrimaryGateway, SecondaryGateway: TIdRouteEntry;
begin
  PrimaryGateway   := Self.RouteA;
  SecondaryGateway := Self.RouteB;

  // SecondaryGateway.Gateway < PrimaryGateway.Gateway, but
  // PrimaryGateway.Metric < SecondaryGateway.Metric. Metric takes priority.
  PrimaryGateway.Gateway   := '10.0.0.2';
  PrimaryGateway.Metric    := 1;
  SecondaryGateway.Gateway := '10.0.0.1';
  SecondaryGateway.Metric  := 2;

  CheckEquals(-1, RouteSort(PrimaryGateway, SecondaryGateway), 'PrimaryGateway < SecondaryGateway');
  CheckEquals(1,  RouteSort(SecondaryGateway, PrimaryGateway), 'SecondaryGateway > PrimaryGateway');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortIPv4RoutesFirst;
var
  IPv4Route, IPv6Route: TIdRouteEntry;
begin
  IPv4Route := Self.RouteA;
  IPv6Route := Self.RouteB;

  IPv4Route.Destination := '0.0.0.0';
  IPv4Route.Mask        := '0.0.0.0';
  IPv6Route.Destination := '::';
  IPv6Route.Mask        := '::';

  CheckEquals(-1, RouteSort(IPv4Route, IPv6Route), 'IPv4Route < IPv6Route');
  CheckEquals(1,  RouteSort(IPv6Route, IPv4Route), 'IPv6Route > IPv4Route');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortMetricDiffers;
begin
  // Even though RouteB's a "wider" route, it costs less
  Self.RouteA.Destination := '10.0.0.0';
  Self.RouteA.Mask        := '255.255.255.0';
  Self.RouteA.Metric      := 2;
  Self.RouteB.Destination := '10.0.0.0';
  Self.RouteB.Mask        := '255.0.0.0';
  Self.RouteB.Metric      := 1;

  CheckEquals(-1, RouteSort(Self.RouteA, Self.RouteB), 'RouteB < RouteA');
  CheckEquals(1,  RouteSort(Self.RouteB, Self.RouteA), 'RouteA < RouteB');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortOneRouteAMappedRoute;
var
  MappedRoute: TIdRouteEntry;
  NormalRoute: TIdRouteEntry;
begin
  NormalRoute := Self.RouteA;
  MappedRoute := Self.RouteB;

  MappedRoute.Gateway       := '1.2.3.4';
  MappedRoute.IsMappedRoute := true;

  CheckEquals(-1, RouteSort(MappedRoute, NormalRoute), 'All else being equal, MappedRoute < NormalRoute');
  CheckEquals(1,  RouteSort(NormalRoute, MappedRoute), 'All else being equal, NormalRoute > MappedRoute');

  MappedRoute.Metric := 999;
  CheckEquals(-1, RouteSort(MappedRoute, NormalRoute), 'MappedRoute < NormalRoute even when much more costly');
  CheckEquals(1,  RouteSort(NormalRoute, MappedRoute), 'NormalRoute > MappedRoute even when much less costly');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortOnlyDestinationDiffers;
var
  LowerAddress, HigherAddress: TIdRouteEntry;
begin
  // "Higher" addresses appear earlier in the sorted routing table.

  LowerAddress  := Self.RouteA;
  HigherAddress := Self.RouteB;

  LowerAddress.Destination  := '10.0.0.0';
  HigherAddress.Destination := '172.0.0.0';

  CheckEquals(1,  RouteSort(LowerAddress, HigherAddress), '10.0.0.0/8 < 172.0.0.0/8');
  CheckEquals(-1, RouteSort(HigherAddress, LowerAddress), '172.0.0.0/8 > 10.0.0.0/8');

  LowerAddress.Destination  := '0.0.0.0';
  HigherAddress.Destination := '172.0.0.0';

  CheckEquals(1,  RouteSort(LowerAddress, HigherAddress), '0.0.0.0/8 < 172.0.0.0/8');
  CheckEquals(-1, RouteSort(HigherAddress, LowerAddress), '172.0.0.0/8 > 0.0.0.0/8');

  LowerAddress.Destination  := '::';
  HigherAddress.Destination := '2002:0101:0101::';

  CheckEquals(1,  RouteSort(LowerAddress, HigherAddress), '::/8 < 2002:0101:0101::/8');
  CheckEquals(-1, RouteSort(HigherAddress, LowerAddress), '2002:0101:0101::/8 > ::/8');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortOnlyGatewayDiffers;
begin
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortOnlyMaskDiffers;
var
  NarrowerRoute, WiderRoute: TIdRouteEntry;
begin
  NarrowerRoute := Self.RouteA;
  WiderRoute    := Self.RouteB;

  WiderRoute.Destination    := '10.0.0.0';
  WiderRoute.Mask           := '255.0.0.0';
  NarrowerRoute.Destination := '10.0.0.0';
  NarrowerRoute.Mask        := '255.255.255.0';

  CheckEquals(-1, RouteSort(NarrowerRoute, WiderRoute), '10.0.0.0/24 < 10.0.0.0/8');
  CheckEquals(1,  RouteSort(WiderRoute, NarrowerRoute), '10.0.0.0/8 > 10.0.0.0/24');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortOnlyMetricDiffers;
var
  CheapRoute, ExpensiveRoute: TIdRouteEntry;
begin
  CheapRoute     := Self.RouteA;
  ExpensiveRoute := Self.RouteB;

  ExpensiveRoute.Metric := CheapRoute.Metric + 1;

  CheckEquals(-1, RouteSort(CheapRoute, ExpensiveRoute), 'CheapRoute < ExpensiveRoute');
  CheckEquals(1,  RouteSort(ExpensiveRoute, CheapRoute), 'ExpensiveRoute > CheapRoute');
end;

procedure TestTIdIPv4RoutingTable.TestRouteSortSameRoute;
begin
  CheckEquals(0, RouteSort(Self.RouteA, Self.RouteB), 'RouteA < RouteB');
  CheckEquals(0, RouteSort(Self.RouteB, Self.RouteA), 'RouteB > RouteA');
end;

initialization
  RegisterTest('Routing table functions', Suite);
end.
