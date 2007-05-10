{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdMockRoutingTable;

interface

uses
  Classes, Contnrs, IdRoutingTable, IdSimpleParser, IdSipLocation;

type
  TIdMockRoutingTable = class(TIdRoutingTable)
  private
    LocalAddresses: TStringList;
    OsRoutes:       TObjectList;

    function HasOsRoutes: Boolean;
    function IsLocalAddress(DestinationIP: String): Boolean;
    function LastOsRoute: TIdRouteEntry;
    function LocalLoopRoute(IPType: TIdIPVersion): TIdRouteEntry;
    function OsRouteAt(Index: Integer): TIdRouteEntry;
  protected
    function  BestRouteIsDefaultRoute(DestinationIP, LocalIP: String): Boolean; override;
    function  GetBestLocalAddress(DestinationIP: String): String; overload; override;
    procedure GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation; DefaultPort: Cardinal); overload; override;
    procedure GetBestRoute(DestinationIP, LocalIP: String; Route: TIdRouteEntry);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddLocalAddress(IP: String);
    procedure AddOsRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String); overload; //override;
    procedure AddOsRoute(Route: TIdRouteEntry); overload; //override;
    function  HasOsRoute(Route: TIdRouteEntry): Boolean;
    function  OsRouteCount: Integer;
    procedure RemoveOsRoute(Destination, Mask, Gateway: String);
  end;

implementation

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

function TIdMockRoutingTable.HasOsRoute(Route: TIdRouteEntry): Boolean;
begin
  Result := Self.InternalHasRoute(Self.OsRoutes, Route);
end;

function TIdMockRoutingTable.OsRouteCount: Integer;
begin
  Result := Self.OsRoutes.Count;
end;

procedure TIdMockRoutingTable.RemoveOsRoute(Destination, Mask, Gateway: String);
begin
  Self.InternalRemoveRoute(Self.OsRoutes, Destination, Mask, Gateway);
end;

//* TIdMockRoutingTable Protected methods **************************************

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

function TIdMockRoutingTable.GetBestLocalAddress(DestinationIP: String): String;
var
  LocalAddress: TIdSipLocation;
begin
  LocalAddress := TIdSipLocation.Create;
  try
    Self.GetBestLocalAddress(DestinationIP, LocalAddress, 0);

    Result := LocalAddress.IPAddress;
  finally
    LocalAddress.Free;
  end;
end;

procedure TIdMockRoutingTable.GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation; DefaultPort: Cardinal);
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
