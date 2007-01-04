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
  Classes;

type
  // I represent an entry in this machine's routing table.
  TIdRouteEntry = class(TObject)
  private
    fDestination:      String;
    fDestInetAddrIPv4: Cardinal;
    fGateway:          String;
    fInterfaceIndex:   String;
    fIsMappedRoute:    Boolean;
    fMask:             String;
    fMaskInetAddrIPv4: Cardinal;
    fMetric:           Cardinal;
    fPort:             Cardinal;

    procedure SetDestination(Value: String);
    procedure SetDestInetAddrIPv4(Value: Cardinal);
    procedure SetMask(Value: String);
    procedure SetMaskInetAddrIPv4(Value: Cardinal);
  public
    constructor Create;

    function Clone: TIdRouteEntry;
    function Equals(OtherRoute: TIdRouteEntry): Boolean;
    function IsIPv4Route: Boolean;
    function WillRoute(DestinationIP: String): Boolean;

    property Destination:      String   read fDestination write SetDestination;
    property DestInetAddrIPv4: Cardinal read fDestInetAddrIPv4 write SetDestInetAddrIPv4;
    property Gateway:          String   read fGateway write fGateway;
    property InterfaceIndex:   String   read fInterfaceIndex write fInterfaceIndex;
    property IsMappedRoute:    Boolean  read fIsMappedRoute write fIsMappedRoute;
    property Mask:             String   read fMask write SetMask;
    property MaskInetAddrIPv4: Cardinal read fMaskInetAddrIPv4 write SetMaskInetAddrIPv4;
    property Metric:           Cardinal read fMetric write fMetric;
    property Port:             Cardinal read fPort write fPort;
  end;

  TIdRouteEntryClass = class of TIdRouteEntry;

  // I represent this machine's routing table, plus any user-defined "mapped
  // routes" - routes which pass through a NAT gateway. Mapped routes have
  // precedence over the machine's routing table.
  TIdIPv4RoutingTable = class(TObject)
  private
    Routes: TList;
  protected
    procedure AddRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String); overload; virtual;
    procedure AddRoute(Route: TIdRouteEntry); overload; virtual;
    procedure ConstructRoutingTable(RoutingTable: TList);
    procedure ReadRoutingTable(RoutingTable: TList); virtual;
    function  RouteAt(Index: Integer): TIdRouteEntry;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddMappedRoute(Destination, Mask, MappedAddress: String);
    function  GatewayFor(DestinationIP: String): String; overload;
//    procedure GatewayFor(DestinationIP: String; Gateway: TIdSipLocation); overload;
    function  HasRoute(Route: TIdRouteEntry): Boolean;
    procedure RemoveRoute(Destination, Mask, Gateway: String);
    function  RouteCount: Integer;
  end;

function RouteSort(Item1, Item2: Pointer): Integer;

implementation

uses
  IdSimpleParser, IdSystem;

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

function LowestGatewayFirst(RouteA, RouteB: TIdRouteEntry): Integer;
begin
    Assert(RouteA.IsIPv4Route = RouteB.IsIPv4Route,
           'The routes must be either both IPv4 or both IPv6 routes');

  if (RouteA.Gateway <> RouteB.Gateway) then begin
    if (RouteA.Gateway < RouteB.Gateway) then
      Result := -1
    else
      Result := 1;
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
  // (*) This is an arbitrary choice.
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
    Result := LowestGatewayFirst(RouteA, RouteB);
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

function TIdRouteEntry.Clone: TIdRouteEntry;
begin
  Result := TIdRouteEntryClass(Self.ClassType).Create;
  Result.Destination    := Self.Destination;
  Result.Gateway        := Self.Gateway;
  Result.InterfaceIndex := Self.InterfaceIndex;
  Result.Mask           := Self.Mask;
  Result.Metric         := Self.Metric;
end;

function TIdRouteEntry.Equals(OtherRoute: TIdRouteEntry): Boolean;
begin
  Result := (Self.Destination = OtherRoute.Destination)
        and (Self.Mask = OtherRoute.Mask)
        and (Self.Gateway = OtherRoute.Gateway);
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
//* TIdIPv4RoutingTable                                                        *
//******************************************************************************
//* TIdIPv4RoutingTable Public methods *****************************************

constructor TIdIPv4RoutingTable.Create;
begin
  inherited Create;

  Self.Routes := TList.Create;
end;

destructor TIdIPv4RoutingTable.Destroy;
begin
  Self.Routes.Free;

  inherited Destroy;
end;

procedure TIdIPv4RoutingTable.AddMappedRoute(Destination, Mask, MappedAddress: String);
var
  NewRoute: TIdRouteEntry;
begin
  NewRoute := TIdRouteEntry.Create;
  try
    NewRoute.Destination    := Destination;
    NewRoute.Mask           := Mask;
    NewRoute.Gateway        := MappedAddress;
    NewRoute.Metric         := 0;
    NewRoute.InterfaceIndex := '';
    NewRoute.IsMappedRoute  := true;


    if not Self.HasRoute(NewRoute) then begin
      Self.Routes.Add(Pointer(NewRoute.Clone));
      Self.Routes.Sort(RouteSort);
    end;
  finally
    NewRoute.Free;
  end;
end;

function TIdIPv4RoutingTable.GatewayFor(DestinationIP: String): String;
var
  I: Integer;
begin
  Self.ConstructRoutingTable(Self.Routes);

  Result := '';
  for I := 0 to Self.Routes.Count - 1 do begin
    if Self.RouteAt(I).WillRoute(DestinationIP) then begin
      Result := Self.RouteAt(I).Gateway;
      Break;
    end;
  end;
end;
{
procedure TIdIPv4RoutingTable.GatewayFor(DestinationIP: String; Gateway: TIdSipLocation);
begin
  // Return not only the gateway needed for a (mapped) route, but also the port
  // needed to allow remote parties to contact you via that gateway.
end;
}
function TIdIPv4RoutingTable.HasRoute(Route: TIdRouteEntry): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to Self.RouteCount - 1 do begin
    if Self.RouteAt(I).Equals(Route) then begin
      Result := true;
    end;
  end;
end;

procedure TIdIPv4RoutingTable.RemoveRoute(Destination, Mask, Gateway: String);
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
    while (I < Self.RouteCount) do begin
      if Self.RouteAt(I).Equals(SearchRoute) then begin
        Self.RouteAt(I).Free;
        Self.Routes.Delete(I);
      end
      else
        Inc(I);
    end;
  finally
    SearchRoute.Free;
  end;
end;

function TIdIPv4RoutingTable.RouteCount: Integer;
begin
  Result := Self.Routes.Count;
end;

//* TIdIPv4RoutingTable Protected methods **************************************

procedure TIdIPv4RoutingTable.AddRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String);
var
  NewRoute: TIdRouteEntry;
begin
  NewRoute := TIdRouteEntry.Create;
  NewRoute.Destination    := Destination;
  NewRoute.Mask           := Mask;
  NewRoute.Gateway        := Gateway;
  NewRoute.Metric         := Metric;
  NewRoute.InterfaceIndex := InterfaceIndex;

  Self.Routes.Add(Pointer(NewRoute));

  Self.Routes.Sort(RouteSort);
end;

procedure TIdIPv4RoutingTable.AddRoute(Route: TIdRouteEntry);
begin
  Self.AddRoute(Route.Destination, Route.Mask, Route.Gateway, Route.Metric, Route.InterfaceIndex);
end;

procedure TIdIPv4RoutingTable.ConstructRoutingTable(RoutingTable: TList);
begin
  Self.ReadRoutingTable(RoutingTable);
  RoutingTable.Sort(RouteSort);
end;

procedure TIdIPv4RoutingTable.ReadRoutingTable(RoutingTable: TList);
begin
  // Override this to read the OS's routing table or such.
end;

function TIdIPv4RoutingTable.RouteAt(Index: Integer): TIdRouteEntry;
begin
  Result := TIdRouteEntry(Self.Routes[Index]);
end;

end.
