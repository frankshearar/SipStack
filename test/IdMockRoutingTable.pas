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
  Contnrs, IdRoutingTable, IdSipLocator;

type
  TIdMockRoutingTable = class(TIdRoutingTable)
  private
    OsRoutes: TObjectList;

    function HasOsRoutes: Boolean;
    function OsRouteAt(Index: Integer): TIdRouteEntry;
  protected
    function  GetBestLocalAddress(DestinationIP: String): String; overload; override;
    procedure GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation); overload; override;
    procedure GetBestRoute(DestinationIP, LocalIP: String; Route: TIdRouteEntry); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddOsRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String; LocalAddress: String); overload; //override;
    procedure AddOsRoute(Route: TIdRouteEntry); overload; //override;
  end;

implementation

//******************************************************************************
//* TIdMockRoutingTable                                                        *
//******************************************************************************
//* TIdMockRoutingTable Public methods *****************************************

constructor TIdMockRoutingTable.Create;
begin
  inherited Create;

  Self.OsRoutes := TObjectList.Create(true);
end;

destructor TIdMockRoutingTable.Destroy;
begin
  Self.OsRoutes.Free;

  inherited Destroy;
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

//* TIdMockRoutingTable Protected methods **************************************

function TIdMockRoutingTable.GetBestLocalAddress(DestinationIP: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.OsRoutes.Count - 1 do begin
    if Self.OsRouteAt(I).WillRoute(DestinationIP) then begin
      Result := Self.OsRouteAt(I).LocalAddress;
      Break;
    end;
  end;

  // If all else fails, default to the last route: if there's a default route,
  // this is where it will be.
  if (Result = '') and Self.HasOsRoutes then
    Result := Self.OsRouteAt(Self.OsRoutes.Count - 1).LocalAddress;
end;

procedure TIdMockRoutingTable.GetBestLocalAddress(DestinationIP: String; Gateway: TIdSipLocation);
var
  Found: Boolean;
  I:     Integer;
begin
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
  if not Found and Self.HasOsRoutes then begin
      Gateway.IPAddress := Self.OsRouteAt(Self.OsRoutes.Count - 1).LocalAddress;
      Gateway.Port      := Self.OsRouteAt(Self.OsRoutes.Count - 1).Port;
  end;
end;

procedure TIdMockRoutingTable.GetBestRoute(DestinationIP, LocalIP: String; Route: TIdRouteEntry);
var
  Found: Boolean;
  I:     Integer;
begin
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
    Route.Assign(Self.OsRouteAt(Self.OsRoutes.Count - 1));
end;

//* TIdMockRoutingTable Private methods ****************************************

function TIdMockRoutingTable.HasOsRoutes: Boolean;
begin
  Result := Self.OsRoutes.Count > 0;
end;

function TIdMockRoutingTable.OsRouteAt(Index: Integer): TIdRouteEntry;
begin
  Result := TIdRouteEntry(Self.OsRoutes[Index]);
end;

end.
