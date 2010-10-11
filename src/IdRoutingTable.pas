{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRoutingTable;

interface

uses
  Classes, Contnrs, IdConnectionBindings, IdNetworking, IdSimpleParser,
  IdSipLocation, IdSystem;

type
  // I represent an entry in this machine's routing table, or a "mapped
  // route" - a route that passes through a NAT gateway.
  //
  // LocalAddress is only meaningful if I represent a mapped route - this will
  // contain the IP address that remote parties will need to contact me.
  TIdRouteEntry = class(TObject)
  private
    fDestination:      String;
    fDestInetAddrIPv4: Cardinal;
    fGateway:          String;
    fInterfaceIndex:   String;
    fIsMappedRoute:    Boolean;
    fLocalAddress:     String;
    fMask:             String;
    fMaskInetAddrIPv4: Cardinal;
    fMetric:           Cardinal;
    fPort:             TPortNum;

    function  IsDefaultIPv6Route: Boolean;
    procedure SetDestination(Value: String);
    procedure SetDestInetAddrIPv4(Value: Cardinal);
    procedure SetMask(Value: String);
    procedure SetMaskInetAddrIPv4(Value: Cardinal);
  public
    constructor Create;

    procedure Assign(Other: TIdRouteEntry);
    function  Copy: TIdRouteEntry;
    function  Equals(OtherRoute: TIdRouteEntry): Boolean;
    function  CanonicalFormMask: String;
    function  IsDefaultRoute: Boolean;
    function  IsIPv4Route: Boolean;
    function  WillRoute(DestinationIP: String): Boolean;

    property Destination:      String   read fDestination write SetDestination;
    property DestInetAddrIPv4: Cardinal read fDestInetAddrIPv4 write SetDestInetAddrIPv4;
    property Gateway:          String   read fGateway write fGateway;
    property InterfaceIndex:   String   read fInterfaceIndex write fInterfaceIndex;
    property IsMappedRoute:    Boolean  read fIsMappedRoute write fIsMappedRoute;
    property LocalAddress:     String   read fLocalAddress write fLocalAddress;
    property Mask:             String   read fMask write SetMask;
    property MaskInetAddrIPv4: Cardinal read fMaskInetAddrIPv4 write SetMaskInetAddrIPv4;
    property Metric:           Cardinal read fMetric write fMetric;
    property Port:             TPortNum read fPort write fPort;
  end;

  TIdRouteEntryClass = class of TIdRouteEntry;

  TIdRoutingTable = class;
  TIdRoutingTableClass = class of TIdRoutingTable;

  // I represent this machine's routing table, plus any user-defined "mapped
  // routes" (routes which pass through a NAT gateway).
  //
  // When you ask me what local address to use, I consult my list of mapped
  // routes (in Self.Routes), and return the best-matching route. Should none
  // be found, I ask the OS for the best local address to use.
  TIdRoutingTable = class(TObject)
  private
    Routes: TObjectList;
  protected
    function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean; virtual;
    function  InternalHasRoute(RouteList: TObjectList; Route: TIdRouteEntry): Boolean;
    procedure InternalRemoveRoute(RouteList: TObjectList; Destination, Mask, Gateway: String);
    function  RouteAt(Index: Integer): TIdRouteEntry;
  public
    class function PlatformRoutingTable(OS: TIdOsType): TIdRoutingTableClass;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddMappedRoute(Destination, Mask, MappedAddress: String; MappedPort: TPortNum = 0);
    procedure BestLocalAddress(LocalBindings: TIdSipLocations;
                               Destination: TIdSipLocation;
                               LocalAddress: TIdSipLocation);
    function  Copy: TIdRoutingTable; virtual;
    function  GetBestLocalAddress(DestinationIP: String): String; overload; virtual;
    procedure GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum); overload; virtual;
    function  HasRoute(Route: TIdRouteEntry): Boolean;
    function  HasRouteThrough(Gateway: String): Boolean;
    function  LocalOrMappedAddressFor(DestinationIP: String): String; overload;
    procedure LocalOrMappedAddressFor(DestinationIP: String; LocalAddress: TIdSipLocation; DefaultPort: TPortNum = 0); overload;
    function  MappedAddressFor(DestinationIP: String): String; overload;
    procedure MappedAddressFor(DestinationIP: String; LocalAddress: TIdSipLocation); overload;
    procedure RemoveRoute(Destination, Mask, Gateway: String);
    function  RouteCount: Integer;
  end;

  TIdWindowsRoutingTable = class(TIdRoutingTable)
  protected
    function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean; override;
  public
    function  GetBestLocalAddress(DestinationIP: String): String; overload; override;
    procedure GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum); overload; override;
  end;

  // Windows NT 4 doesn't support the IP Helper API's GetBestInterface function.
  // The IdNetworking unit thus has a special version of GetBestInterface that doesn't
  // rely on this function.
  TIdWindowsNT4RoutingTable = class(TIdWindowsRoutingTable)
  protected
    function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean; override;
   public
    function  GetBestLocalAddress(DestinationIP: String): String; overload; override;
    procedure GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum); overload; override;
  end;

  TIdMockRoutingTable = class(TIdRoutingTable)
  private
    LocalAddresses: TStringList;
    OsRoutes:       TObjectList;

    function HasOsRoutes: Boolean;
    function IsLocalAddress(DestinationIP: String): Boolean;
    function LastOsRoute: TIdRouteEntry;
    function LocalLoopRoute(IPType: TIdIPVersion): TIdRouteEntry;
    function OsRouteAt(Index: Integer): TIdRouteEntry;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddDefaultOsRoute(Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String);
    procedure AddLocalAddress(IP: String);
    procedure AddOsRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String); overload; //override;
    procedure AddOsRoute(Route: TIdRouteEntry); overload; //override;
    function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean; override;
    function  Copy: TIdRoutingTable; override;
    procedure GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation; DefaultPort: TPortNum); overload; override;
    procedure GetBestRoute(DestinationIP, LocalIP: String; Route: TIdRouteEntry);
    function  HasOsRoute(Route: TIdRouteEntry): Boolean;
    function  OsRouteCount: Integer;
    procedure RemoveAllOsRoutes;
    procedure RemoveOsRoute(Destination, Mask, Gateway: String);
  end;

function RouteSort(Item1, Item2: Pointer): Integer;

implementation

uses
  IdSipTransport, SysUtils;

function HighestIPv4AddressFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  Assert(RouteA.IsIPv4Route and RouteB.IsIPv4Route,
         'The routes must be both IPv4 routes');

  // Routes with "higher" addresses appear earlier in the routing table.
  if (RouteA.Destination <> RouteB.Destination) then begin
    if (RouteA.DestInetAddrIPv4 < RouteB.DestInetAddrIPv4) then
      Result := 1
    else
      Result := -1;
  end
  else
    Result := 0;
end;

function HighestIPv4GatewayFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  Assert(RouteA.IsIPv4Route and RouteB.IsIPv4Route,
         'The routes must be both IPv4 routes');

  // Routes with "higher" addresses appear earlier in the routing table.
  if (RouteA.Gateway <> RouteB.Gateway) then begin
    if (TIdIPAddressParser.InetAddr(RouteA.Gateway) < TIdIPAddressParser.InetAddr(RouteB.Gateway)) then
      Result := 1
    else
      Result := -1;
  end
  else
    Result := 0;
end;

function HighestIPv6AddressFirst(RouteA, RouteB: TIdRouteEntry): Integer;
  function ReverseCompareWord(A, B: Word): Integer;
  begin
    // "Reverse" because usually compare functions return -1 if A < B;
    // here we return -1 if A > B.
    if (A < B) then
      Result := 1
    else if (A > B) then
      Result := -1
    else
      Result := 0;
  end;

var
  AddressA, AddressB: TIdIPv6AddressRec;
  I:                  Integer;
begin
  Assert(not RouteA.IsIPv4Route and not RouteB.IsIPv4Route,
         'The routes must be both IPv6 routes');

  TIdIPAddressParser.ParseIPv6Address(RouteA.Destination, AddressA);
  TIdIPAddressParser.ParseIPv6Address(RouteB.Destination, AddressB);

  // Routes with "higher" addresses appear earlier in the routing table.
  for I := Low(TIdIPv6AddressRec) to High(TIdIPv6AddressRec) do begin
    Result := ReverseCompareWord(AddressA[I], AddressB[I]);

    if (Result <> 0) then Break;
  end;
end;

function HighestIPv6GatewayFirst(RouteA, RouteB: TIdRouteEntry): Integer;
  function ReverseCompareWord(A, B: Word): Integer;
  begin
    // "Reverse" because usually compare functions return -1 if A < B;
    // here we return -1 if A > B.
    if (A < B) then
      Result := 1
    else if (A > B) then
      Result := -1
    else
      Result := 0;
  end;

var
  GatewayA, GatewayB: TIdIPv6AddressRec;
  I:                  Integer;
begin
  Assert(not RouteA.IsIPv4Route and not RouteB.IsIPv4Route,
         'The routes must be both IPv6 routes');

  TIdIPAddressParser.ParseIPv6Address(RouteA.Gateway, GatewayA);
  TIdIPAddressParser.ParseIPv6Address(RouteB.Gateway, GatewayB);

  // Routes with "higher" Gatewayes appear earlier in the routing table.
  for I := Low(TIdIPv6AddressRec) to High(TIdIPv6AddressRec) do begin
    Result := ReverseCompareWord(GatewayA[I], GatewayB[I]);

    if (Result <> 0) then Break;
  end;
end;

function HighestAddressFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  Assert(RouteA.IsIPv4Route = RouteB.IsIPv4Route,
         'The routes must be either both IPv4 or both IPv6 routes');

  if (RouteA.Destination <> RouteB.Destination) then begin
    if RouteA.IsIPv4Route then
      Result := HighestIPv4AddressFirst(RouteA, RouteB)
    else
      Result := HighestIPv6AddressFirst(RouteA, RouteB)
  end
  else
    Result := 0;
end;

function IPv4RoutesFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  if (RouteA.IsIPv4Route <> RouteB.IsIPv4Route) then begin
    if RouteA.IsIPv4Route then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function HighestGatewayFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
    Assert(RouteA.IsIPv4Route = RouteB.IsIPv4Route,
           'The routes must be either both IPv4 or both IPv6 routes');

  if (RouteA.Gateway <> RouteB.Gateway) then begin
    if RouteA.IsIPv4Route then
      Result := HighestIPv4GatewayFirst(RouteA, RouteB)
    else
      Result := HighestIPv6GatewayFirst(RouteA, RouteB)
  end
  else
    Result := 0;
end;

function LowestMetricFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  if (RouteA.Metric <> RouteB.Metric) then begin
    if (RouteA.Metric < RouteB.Metric) then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function MappedRoutesFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  if (RouteA.IsMappedRoute <> RouteB.IsMappedRoute) then begin
    if RouteA.IsMappedRoute then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function MostRestrictiveMaskFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
  if (RouteA.Mask <> RouteB.Mask) then begin
    if (RouteA.Mask > RouteB.Mask) then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function RouteSort(Item1, Item2: Pointer): Integer;
var
  RouteA: TIdRouteEntry;
  RouteB: TIdRouteEntry;
begin
  RouteA := TIdRouteEntry(Item1);
  RouteB := TIdRouteEntry(Item2);

  // Sort order:
  // Mapped routes, then OS routes.
  // IPv4 routes, then IPv6 routes. (*)
  // "Higher" addresses first, so the default route is last. (**)
  // Tightest netmasks first.
  // Lowest metrics first.
  // Finally, "higher" gateways first. (*)
  //
  // (*) This is an arbitrary choice, and ought to be configurable (TODO!).
  // (**) Semi-arbitrary. This choice of sorting means that the default route
  // is always last, but that's the only reason.

  Result := MappedRoutesFirst(RouteA, RouteB);

  if (Result = 0) then
    Result := IPv4RoutesFirst(RouteA, RouteB);

  if (Result = 0) then
    Assert(RouteA.IsIPv4Route = RouteB.IsIPv4Route,
           'The routes should be either both IPv4 or both IPv6 routes');

  if (Result = 0) then
    Result := HighestAddressFirst(RouteA, RouteB);

  if (Result = 0) then
    Result := MostRestrictiveMaskFirst(RouteA, RouteB);

  if (Result = 0) then
    Result := LowestMetricFirst(RouteA, RouteB);

  if (Result = 0) then
    Result := HighestGatewayFirst(RouteA, RouteB);
end;

//******************************************************************************
//* TIdRouteEntry                                                              *
//******************************************************************************
//* TIdRouteEntry Public methods ***********************************************

constructor TIdRouteEntry.Create;
begin
  inherited Create;

  Self.IsMappedRoute := false;
end;

procedure TIdRouteEntry.Assign(Other: TIdRouteEntry);
begin
  Self.Destination    := Other.Destination;
  Self.Gateway        := Other.Gateway;
  Self.InterfaceIndex := Other.InterfaceIndex;
  Self.LocalAddress   := Other.LocalAddress;
  Self.Mask           := Other.Mask;
  Self.Metric         := Other.Metric;
  Self.Port           := Other.Port;
end;

function TIdRouteEntry.Copy: TIdRouteEntry;
begin
  Result := TIdRouteEntryClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

function TIdRouteEntry.Equals(OtherRoute: TIdRouteEntry): Boolean;
begin
  Result := (Self.Destination = OtherRoute.Destination)
        and (Self.CanonicalFormMask = OtherRoute.CanonicalFormMask)
        and (Self.Gateway = OtherRoute.Gateway);
end;

function TIdRouteEntry.CanonicalFormMask: String;
var
  N, E: Integer;
  Version: TIdIPVersion;
begin
  Val(Self.Mask, N, E);

  Version := TIdIPAddressParser.IPVersion(Self.Destination);

  if (E = 0) then
    Result := TIdIPAddressParser.MaskToAddress(N, Version)
  else begin
    if (Version = Id_IPv6) then
      Result := TIdIPAddressParser.ExpandIPv6Address(Self.Mask)
    else
      Result := Self.Mask;
  end;
end;

function TIdRouteEntry.IsDefaultRoute: Boolean;
begin
  if Self.IsIPv4Route then
    Result := Self.Destination = '0.0.0.0'
  else
    Result := Self.IsDefaultIPv6Route;
end;

function TIdRouteEntry.IsIPv4Route: Boolean;
begin
  Result := TIdIPAddressParser.IsIPv4Address(Self.Destination);
end;

function TIdRouteEntry.WillRoute(DestinationIP: String): Boolean;
begin
  Result := false;

  if Self.IsIPv4Route then begin
    if TIdIPAddressParser.IsIPv4Address(DestinationIP) then
      Result := OnSameNetwork(Self.Destination, DestinationIP, Self.Mask);
  end
  else begin
    if TIdIPAddressParser.IsIPv6Address(DestinationIP) then
      Result := OnSameNetwork(Self.Destination, DestinationIP, Self.Mask);
  end;
end;

//* TIdRouteEntry Private methods **********************************************

function TIdRouteEntry.IsDefaultIPv6Route: Boolean;
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  try
    TIdIPAddressParser.ParseIPv6Address(Self.Destination, Address);

    Result := true;
    for I := Low(Address) to High(Address) do
      Result := Result and (Address[I] = 0);
  except
    on EConvertError do
      Result := false;
  end;
end;

procedure TIdRouteEntry.SetDestination(Value: String);
begin
  Self.fDestination := Value;

  if TIdIPAddressParser.IsIPv4Address(Value) then
    Self.fDestInetAddrIPv4 := TIdIPAddressParser.InetAddr(Value)
  else
    Self.fDestInetAddrIPv4 := 0;
end;

procedure TIdRouteEntry.SetDestInetAddrIPv4(Value: Cardinal);
begin
  Self.fDestInetAddrIPv4 := Value;
  Self.fDestination      := TIdIPAddressParser.IPv4AddressToStr(Value);
end;

procedure TIdRouteEntry.SetMask(Value: String);
begin
  Self.fMask := Value;

  if TIdIPAddressParser.IsIPv4Address(Value) then
    Self.fMaskInetAddrIPv4 := TIdIPAddressParser.InetAddr(Value)
  else
    Self.fMaskInetAddrIPv4 := 0;
end;

procedure TIdRouteEntry.SetMaskInetAddrIPv4(Value: Cardinal);
begin
  Self.fMaskInetAddrIPv4 := Value;
  Self.fMask             := TIdIPAddressParser.IPv4AddressToStr(Value);
end;

//******************************************************************************
//* TIdRoutingTable                                                            *
//******************************************************************************
//* TIdRoutingTable Public methods *********************************************

class function TIdRoutingTable.PlatformRoutingTable(OS: TIdOsType): TIdRoutingTableClass;
begin
  case OS of
    otWindowsNT4:        Result := TIdWindowsNT4RoutingTable;
    otWindows2k,
    otWindowsXP,
    otWindowsServer2003,
    otWindowsVista:      Result := TIdWindowsRoutingTable;
  else
    Result := TIdMockRoutingTable;
  end;
end;

constructor TIdRoutingTable.Create;
begin
  inherited Create;

  Self.Routes := TObjectList.Create(true);
end;

destructor TIdRoutingTable.Destroy;
begin
  Self.Routes.Free;

  inherited Destroy;
end;

procedure TIdRoutingTable.AddMappedRoute(Destination, Mask, MappedAddress: String; MappedPort: TPortNum = 0);
var
  NewRoute: TIdRouteEntry;
begin
  NewRoute := TIdRouteEntry.Create;
  try
    NewRoute.Destination    := Destination;
    NewRoute.Mask           := Mask;
    NewRoute.Gateway        := MappedAddress;
    NewRoute.LocalAddress   := MappedAddress;
    NewRoute.Metric         := 0;
    NewRoute.InterfaceIndex := '';
    NewRoute.IsMappedRoute  := true;
    NewRoute.Port           := MappedPort;

    if not Self.HasRoute(NewRoute) then begin
      Self.Routes.Add(NewRoute.Copy);
      Self.Routes.Sort(RouteSort);
    end;
  finally
    NewRoute.Free;
  end;
end;

procedure TIdRoutingTable.BestLocalAddress(LocalBindings: TIdSipLocations;
                                           Destination: TIdSipLocation;
                                           LocalAddress: TIdSipLocation);
var
  ActualAddress: TIdSipLocation;
  DefaultPort:   TPortNum;
begin
  // This function returns the best local address to use to contact Destination,
  // from a set of local bindings. That local address might be capable of
  // connecting directly to the remote party, or be the local address best
  // suited for contacting the NAT between you and the remote party.
  //
  // On a machine with multiple local addresses, and at least one gateway, you
  // need to be careful what IP address/hostname you put into certain headers in
  //  some protocols (like SIP), since putting in a LAN IP address in your
  // Contact when you're making a call to someone on the public Internet is
  // going to result in the remote party's messages not reaching you.
  //
  // Only messages involved in creating a dialog care: INVITE, SUBSCRIBE, REFER,
  // and their responses. In-dialog messages already have "correct" (routable,
  // in other words) URIs.

  DefaultPort := TIdSipTransportRegistry.DefaultPortFor(Destination.Transport);
  Self.LocalOrMappedAddressFor(Destination.IPAddress, LocalAddress, DefaultPort);
  LocalAddress.Transport := Destination.Transport;

  // If the best local address to use to contact Destination is the localhost
  // address, then it doesn't matter which local binding we use, as long as the
  // transport matches.
  if LocalAddress.IsLocalhost then begin
    ActualAddress := LocalBindings.FirstAddressMatch(LocalAddress);

    // If we can't find LocalAddress among the actual local bindings, just use
    // any local binding.
    if not Assigned(ActualAddress) then
      ActualAddress := LocalBindings.FirstTransportMatch(LocalAddress)
  end
  else
    ActualAddress := LocalBindings.FirstAddressMatch(LocalAddress);

  if not Assigned(ActualAddress) then begin
    // There is no local binding that can be used to communicate with the UA
    // at Dest, or we have no binding on the best local address.
    //
    // Thus, we check to see if the UA at Dest is running on a local address:
    ActualAddress := LocalBindings.FirstAddressMatch(Destination);
  end;

  // Either Dest contains a non-local IP address (in which ActualAddress
  // contains the most appropriate local IP address to use), or Dest contains
  // a local IP address (in which case ActualAddress contains that local IP
  // address).
  if Assigned(ActualAddress) then begin
    LocalAddress.Assign(ActualAddress)
  end;
end;

function TIdRoutingTable.Copy: TIdRoutingTable;
begin
  Result := TIdRoutingTableClass(Self.ClassType).Create;
end;

function TIdRoutingTable.GetBestLocalAddress(DestinationIP: String): String;
var
  LocalAddress: TIdSipLocation;
begin
  // Return the best local address needed to contact the machine at DestinationIP.

  LocalAddress := TIdSipLocation.Create;
  try
    Self.GetBestLocalAddress(DestinationIP, LocalAddress, 0);

    Result := LocalAddress.IPAddress;
  finally
    LocalAddress.Free;
  end;
end;

procedure TIdRoutingTable.GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum);
begin
  // Return the best local address needed to contact the machine at DestinationIP.
  // This version of the method allows one to specify ports. Since this class is
  // protocol agnostic (above the IP layer), you probably want to set DefaultPort
  // to whatever default port your application layer protocol uses. If you were
  // using SIP, for instance, you'd set DefaultPort to 5060.
  //
  // An example of using this would be if you have multiple SIP User Agents
  // behind one NATting firewall: the firewall could redirect packets on port
  // 5060 to your boss, 5062 to your neighbouring colleague and 5064 to your
  // machine. Using this method allows you to send out your Contact with a URI
  // like sip:you@your.natted.address:5064.
end;

function TIdRoutingTable.HasRoute(Route: TIdRouteEntry): Boolean;
begin
  Result := Self.InternalHasRoute(Self.Routes, Route);
end;

function TIdRoutingTable.HasRouteThrough(Gateway: String): Boolean;
var
  I: Integer;
begin
  // Given an address Gateway, do we have any mapped routes using that address?
  Result := false;

  for I := 0 to Self.RouteCount - 1 do begin
    if (Self.RouteAt(I).Gateway = Gateway) then begin
      Result := true;
      Break;
    end;
  end;
end;

function TIdRoutingTable.LocalOrMappedAddressFor(DestinationIP: String): String;
var
  LocalAddress: TIdSipLocation;
begin
  LocalAddress := TIdSipLocation.Create;
  try
    Self.LocalOrMappedAddressFor(DestinationIP, LocalAddress);

    Result := LocalAddress.IPAddress;
  finally
    LocalAddress.Free;
  end;
end;

procedure TIdRoutingTable.LocalOrMappedAddressFor(DestinationIP: String; LocalAddress: TIdSipLocation; DefaultPort: TPortNum = 0);
var
  LocalIP: String;
begin
  // Return not only the gateway needed for a (mapped) route, but also the port
  // needed to allow remote parties to contact you via that gateway.

  LocalIP := Self.GetBestLocalAddress(DestinationIP);
  Assert(LocalIP <> '', 'GetBestLocalAddress must return _something_');

  if Self.BestRouteIsDefaultRoute(DestinationIP, LocalIP) then begin
    Self.MappedAddressFor(DestinationIP, LocalAddress);

    if (LocalAddress.IPAddress = '') then begin
      // There's no mapped route, so using the OS's default route is the right
      // thing to do.
      LocalAddress.IPAddress := LocalIP;
      LocalAddress.Port      := DefaultPort;
    end;
  end
  else begin
    LocalAddress.IPAddress := LocalIP;
    LocalAddress.Port      := DefaultPort;
  end;
end;

function TIdRoutingTable.MappedAddressFor(DestinationIP: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.Routes.Count - 1 do begin
    if Self.RouteAt(I).WillRoute(DestinationIP) then begin
      Result := Self.RouteAt(I).LocalAddress;
      Break;
    end;
  end;
end;

procedure TIdRoutingTable.MappedAddressFor(DestinationIP: String; LocalAddress: TIdSipLocation);
var
  I: Integer;
begin
  LocalAddress.IPAddress := '';
  LocalAddress.Port      := 0;
  for I := 0 to Self.Routes.Count - 1 do begin
    if Self.RouteAt(I).WillRoute(DestinationIP) then begin
      LocalAddress.IPAddress := Self.RouteAt(I).LocalAddress;
      LocalAddress.Port      := Self.RouteAt(I).Port;
      Break;
    end;
  end;
end;

procedure TIdRoutingTable.RemoveRoute(Destination, Mask, Gateway: String);
begin
  Self.InternalRemoveRoute(Self.Routes, Destination, Mask, Gateway);
end;

function TIdRoutingTable.RouteCount: Integer;
begin
  Result := Self.Routes.Count;
end;

//* TIdRoutingTable Protected methods ******************************************

function TIdRoutingTable.BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
begin
  Result := false;
end;

function TIdRoutingTable.InternalHasRoute(RouteList: TObjectList; Route: TIdRouteEntry): Boolean;
var
  I: Integer;
begin
  Result := false;
  I      := 0;
  while (I < RouteList.Count) and not Result do begin
    Result := (RouteList[I] as TIdRouteEntry).Equals(Route);
    Inc(I);
  end;
end;

procedure TIdRoutingTable.InternalRemoveRoute(RouteList: TObjectList; Destination, Mask, Gateway: String);
var
  I:           Integer;
  SearchRoute: TIdRouteEntry;
begin
  SearchRoute := TIdRouteEntry.Create;
  try
    SearchRoute.Destination := Destination;
    SearchRoute.Gateway     := Gateway;
    SearchRoute.Mask        := Mask;

    I := 0;
    while (I < RouteList.Count) do begin
      if (RouteList[I] as TIdRouteEntry).Equals(SearchRoute) then begin
        RouteList.Delete(I);
      end
      else
        Inc(I);
    end;
  finally
    SearchRoute.Free;
  end;
end;

function TIdRoutingTable.RouteAt(Index: Integer): TIdRouteEntry;
begin
  Result := TIdRouteEntry(Self.Routes[Index]);
end;

//******************************************************************************
//* TIdWindowsRoutingTable                                                     *
//******************************************************************************
//* TIdWindowsRoutingTable Public methods **************************************

function TIdWindowsRoutingTable.GetBestLocalAddress(DestinationIP: String): String;
begin
  Result := IdNetworking.GetBestLocalAddress(DestinationIP);
end;

procedure TIdWindowsRoutingTable.GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum);
begin
  LocalLocation.IPAddress := IdNetworking.GetBestLocalAddress(DestinationIP);
  LocalLocation.Port      := DefaultPort;
end;

//* TIdWindowsRoutingTable Protected methods ***********************************

function TIdWindowsRoutingTable.BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
begin
  Result := IdNetworking.BestRouteIsDefaultRoute(DestinationIP, LocalIP);
end;

//******************************************************************************
//* TIdWindowsNT4RoutingTable                                                  *
//******************************************************************************
//* TIdWindowsNT4RoutingTable Public methods ***********************************

function TIdWindowsNT4RoutingTable.GetBestLocalAddress(DestinationIP: String): String;
begin
  Result := IdNetworking.GetBestLocalAddressNT4(DestinationIP);
end;

procedure TIdWindowsNT4RoutingTable.GetBestLocalAddress(DestinationIP: String; LocalLocation: TIdSipLocation; DefaultPort: TPortNum);
begin
  LocalLocation.IPAddress := IdNetworking.GetBestLocalAddressNT4(DestinationIP);
  LocalLocation.Port      := DefaultPort;
end;

//* TIdWindowsNT4RoutingTable Protected methods ********************************

function TIdWindowsNT4RoutingTable.BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
begin
  Result := IdNetworking.BestRouteIsDefaultRouteNT4(DestinationIP, LocalIP);
end;

//******************************************************************************
//* TIdMockRoutingTable                                                        *
//******************************************************************************
//* TIdMockRoutingTable Public methods *****************************************

constructor TIdMockRoutingTable.Create;
begin
  inherited Create;

  Self.LocalAddresses := TStringList.Create;
  Self.LocalAddresses.Duplicates := dupIgnore;

  Self.OsRoutes := TObjectList.Create(true);
end;

destructor TIdMockRoutingTable.Destroy;
begin
  Self.OsRoutes.Free;
  Self.LocalAddresses.Free;

  inherited Destroy;
end;

procedure TIdMockRoutingTable.AddDefaultOsRoute(Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String);
var
  Version: TIdIPVersion;
  Mask:    String;
  Network: String;
begin
  Version := TIdIPAddressParser.IPVersion(LocalAddress);

  if (Version = Id_IPv4) then
    Network := '0.0.0.0'
  else if (Version = Id_IPv6) then
    Network := '::';

  Mask := TIdIPAddressParser.NetworkFor(Network, 0);

  Self.AddOsRoute(Network, Mask, Gateway, Metric, InterfaceIndex, LocalAddress);
end;

procedure TIdMockRoutingTable.AddLocalAddress(IP: String);
begin
  Self.LocalAddresses.Add(IP);
end;

procedure TIdMockRoutingTable.AddOsRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String);
var
  NewRoute: TIdRouteEntry;
begin
  NewRoute := TIdRouteEntry.Create;
  NewRoute.Destination    := Destination;
  NewRoute.Gateway        := Gateway;
  NewRoute.InterfaceIndex := InterfaceIndex;
  NewRoute.LocalAddress   := LocalAddress;
  NewRoute.Mask           := Mask;
  NewRoute.Metric         := Metric;

  Self.OsRoutes.Add(NewRoute);
  Self.OsRoutes.Sort(RouteSort);
end;

procedure TIdMockRoutingTable.AddOsRoute(Route: TIdRouteEntry);
begin
  Self.AddOsRoute(Route.Destination, Route.Mask, Route.Gateway, Route.Metric, Route.InterfaceIndex, Route.LocalAddress);
end;

function TIdMockRoutingTable.BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean;
var
  ProposedRoute: TIdRouteEntry;
begin
  ProposedRoute := TIdRouteEntry.Create;
  try
    Self.GetBestRoute(DestinationIP, LocalIP, ProposedRoute);

    Result := ProposedRoute.IsDefaultRoute;
  finally
    ProposedRoute.Free;
  end;
end;

function TIdMockRoutingTable.Copy: TIdRoutingTable;
var
  I:  Integer;
  RT: TIdMockRoutingTable;
begin
  RT := inherited Copy as TIdMockRoutingTable;

  for I := 0 to Self.OsRoutes.Count - 1 do
    RT.AddOsRoute(TIdRouteEntry(Self.OsRoutes[I]));

  // We access the private variables of RT simply because it's expedient.
  // Naughty, naughty.
  RT.LocalAddresses.AddStrings(Self.LocalAddresses);

  Result := RT;
end;

procedure TIdMockRoutingTable.GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation; DefaultPort: TPortNum);
var
  Found: Boolean;
  I:     Integer;
begin
  if Self.IsLocalAddress(DestinationIP) then begin
    Gateway.IPAddress := Localhost(TIdIPAddressParser.IPVersion(DestinationIP));
    Gateway.Port      := DefaultPort;
    Exit;
  end;

  Found := false;
  I     := 0;
  while (I < Self.OsRoutes.Count) and not Found do begin
    Found := Self.OsRouteAt(I).WillRoute(DestinationIP);

    if Found then begin
      Gateway.IPAddress := Self.OsRouteAt(I).LocalAddress;
      Gateway.Port      := Self.OsRouteAt(I).Port;
    end;

    Inc(I);
  end;

  // If all else fails, default to the last route; if there's a default route,
  // this is where it will be.
  if not Found then begin
    if Self.HasOsRoutes then begin
      Gateway.IPAddress := Self.LastOsRoute.LocalAddress;
      Gateway.Port      := Self.LastOsRoute.Port;
    end
    else begin
      Gateway.IPAddress := LocalHost(TIdIPAddressParser.IPVersion(DestinationIP));
      Gateway.Port      := DefaultPort;
    end;
  end;
end;

procedure TIdMockRoutingTable.GetBestRoute(DestinationIP, LocalIP: String; Route: TIdRouteEntry);
var
  Found: Boolean;
  I:     Integer;
begin
  if Self.IsLocalAddress(DestinationIP) then begin
    Route.Assign(Self.LocalLoopRoute(TIdIPAddressParser.IPVersion(DestinationIP)));
    Exit;
  end;

  Found := false;
  I     := 0;
  while (I < Self.OsRoutes.Count) and not Found do begin
    Found := Self.OsRouteAt(I).WillRoute(DestinationIP)
         and (Self.OsRouteAt(I).LocalAddress = LocalIP);

    if Found then
      Route.Assign(Self.OsRouteAt(I));

    Inc(I);
  end;

  // If all else fails, default to the last route: if there's a default route,
  // this is where it will be.
  if not Found and Self.HasOsRoutes then
    Route.Assign(Self.LastOsRoute);
end;

function TIdMockRoutingTable.HasOsRoute(Route: TIdRouteEntry): Boolean;
begin
  Result := Self.InternalHasRoute(Self.OsRoutes, Route);
end;

function TIdMockRoutingTable.OsRouteCount: Integer;
begin
  Result := Self.OsRoutes.Count;
end;

procedure TIdMockRoutingTable.RemoveAllOsRoutes;
begin
  Self.OsRoutes.Clear;
end;

procedure TIdMockRoutingTable.RemoveOsRoute(Destination, Mask, Gateway: String);
begin
  Self.InternalRemoveRoute(Self.OsRoutes, Destination, Mask, Gateway);
end;

//* TIdMockRoutingTable Protected methods **************************************

//* TIdMockRoutingTable Private methods ****************************************

function TIdMockRoutingTable.HasOsRoutes: Boolean;
begin
  Result := Self.OsRoutes.Count > 0;
end;

function TIdMockRoutingTable.IsLocalAddress(DestinationIP: String): Boolean;
begin
  Result := Self.LocalAddresses.IndexOf(DestinationIP) <> -1;
end;

function TIdMockRoutingTable.LastOsRoute: TIdRouteEntry;
begin
  Result := Self.OsRouteAt(Self.OsRoutes.Count - 1);
end;

function TIdMockRoutingTable.LocalLoopRoute(IPType: TIdIPVersion): TIdRouteEntry;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.OsRouteCount - 1 do begin
    if Self.OsRouteAt(I).WillRoute(Localhost(IPType)) then begin
      Result := Self.OsRouteAt(I);
      Break;
    end;
  end;
end;

function TIdMockRoutingTable.OsRouteAt(Index: Integer): TIdRouteEntry;
begin
  Result := TIdRouteEntry(Self.OsRoutes[Index]);
end;

end.
