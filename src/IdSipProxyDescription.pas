{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipProxyDescription;

interface

uses
  Contnrs, IdAddressSpace, IdSipMessage;

type
  TIdProxyDescription = class(TIdParameterForAddressSpace)
  private
    fRoutePath: TIdSipRoutePath;

    procedure SetRoutePath(Value: TIdSipRoutePath);
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  ProxyFor(Address: String): Boolean;

    property RoutePath: TIdSipRoutePath read fRoutePath write SetRoutePath;
  end;

  // I contain a list of "proxy descriptions" - given a target address, I will
  // tell you what Route path you need to use when contacting that address. I
  // supply a default Route path, in the event that a target address doesn't
  // fall within any configured address space.
  //
  // Note that I do not ensure that you have specified disjoint address spaces:
  // in the event that you have not (for instance, you have address spaces
  // "10.0.0.0/8" and "10.0.0.0/16"), I return the Route path for the first
  // address space found.
  TIdProxyDescriptions = class(TIdParameterForAddressSpaceList)
  private
    function  FindProxyFor(Address: String): TIdProxyDescription;
    function  FindProxyForAddressSpace(AddressSpace: String): TIdProxyDescription;
    function  GetDefaultRoutePath: TIdSipRoutePath;
    procedure SetDefaultRoutePath(Value: TIdSipRoutePath);
  protected
    function CreateDefaultParameter: TIdParameterForAddressSpace; override;
  public
    procedure AddDescription(AddressSpace: String; RoutePath: TIdSipRoutePath);
    procedure AddRouteFor(AddressSpace: String; Route: TIdSipRouteHeader); overload;
    procedure AddRouteFor(AddressSpace: String; Route: TIdSipUri); overload;
    procedure ClearAllParameters; override;
    procedure RemoveDescription(AddressSpace: String);
    function  RoutePathFor(Address: String): TIdSipRoutePath;
    function  RoutePathForAddressSpace(AddressSpace: String): TIdSipRoutePath;

    property DefaultRoutePath: TIdSipRoutePath read GetDefaultRoutePath write SetDefaultRoutePath;
  end;

implementation

//******************************************************************************
//* TIdProxyDescription                                                        *
//******************************************************************************
//* TIdProxyDescription Public methods *****************************************

constructor TIdProxyDescription.Create;
begin
  inherited Create;

  Self.fRoutePath := TIdSipRoutePath.Create;
end;

destructor TIdProxyDescription.Destroy;
begin
  Self.RoutePath.Free;

  inherited Destroy;
end;

function TIdProxyDescription.ProxyFor(Address: String): Boolean;
begin
  Result := Self.ParameterFor(Address);
end;

//* TIdProxyDescription Private methods *****************************************

procedure TIdProxyDescription.SetRoutePath(Value: TIdSipRoutePath);
begin
  Self.fRoutePath.Clear;
  Self.fRoutePath.Add(Value);
end;

//******************************************************************************
//* TIdProxyDescriptions                                                       *
//******************************************************************************
//* TIdProxyDescriptions Public methods ****************************************

procedure TIdProxyDescriptions.AddDescription(AddressSpace: String; RoutePath: TIdSipRoutePath);
var
  Desc: TIdProxyDescription;
begin
  if Self.HasParameterForAddressSpace(AddressSpace) then Exit;

  Desc := TIdProxyDescription.Create;
  Desc.AddressSpace := AddressSpace;
  Desc.RoutePath    := RoutePath;

  Self.AddParameter(Desc);
end;

procedure TIdProxyDescriptions.AddRouteFor(AddressSpace: String; Route: TIdSipRouteHeader);
begin
  Self.AddRouteFor(AddressSpace, Route.Address);
end;

procedure TIdProxyDescriptions.AddRouteFor(AddressSpace: String; Route: TIdSipUri);
var
  NewPath: TIdSipRoutePath;
  Proxy:   TIdProxyDescription;
begin
  // Either append Route to AddressSpace's Route path or, if AddressSpace is a
  // new address space, add AddressSpace and then append Route to its (empty)
  // Route path.

  Proxy := Self.FindProxyForAddressSpace(AddressSpace);

  if Assigned(Proxy) then
    Proxy.RoutePath.AddRoute(Route)
  else begin
    NewPath := TIdSipRoutePath.Create;
    try
      NewPath.AddRoute(Route);
      Self.AddDescription(AddressSpace, NewPath);
    finally
      NewPath.Free;
    end;
  end;
end;

procedure TIdProxyDescriptions.ClearAllParameters;
begin
  inherited ClearAllParameters;

  Self.DefaultRoutePath.Clear;
end;

procedure TIdProxyDescriptions.RemoveDescription(AddressSpace: String);
begin
  Self.RemoveParameter(AddressSpace);
end;

function TIdProxyDescriptions.RoutePathFor(Address: String): TIdSipRoutePath;
var
  Proxy: TIdProxyDescription;
begin
  // Given an Address (typically a domain, or an IPv4 or IPv6 subnet), return
  // the Route path used to contact a User Agent at that address.

  Proxy := Self.FindProxyFor(Address);

  if Assigned(Proxy) then
    Result := Proxy.RoutePath
  else
    Result := Self.DefaultRoutePath;
end;

function TIdProxyDescriptions.RoutePathForAddressSpace(AddressSpace: String): TIdSipRoutePath;
var
  Desc: TIdProxyDescription;
begin
  Desc := Self.FindProxyForAddressSpace(AddressSpace);

  if Assigned(Desc) then
    Result := Desc.RoutePath
  else
    Result := Self.DefaultRoutePath;
end;

//* TIdProxyDescriptions Protected methods *************************************

function TIdProxyDescriptions.CreateDefaultParameter: TIdParameterForAddressSpace;
begin
  Result := TIdProxyDescription.Create;
end;

//* TIdProxyDescriptions Private methods ***************************************

function TIdProxyDescriptions.FindProxyFor(Address: String): TIdProxyDescription;
begin
  Result := Self.FindParameterFor(Address) as TIdProxyDescription;
end;

function TIdProxyDescriptions.FindProxyForAddressSpace(AddressSpace: String): TIdProxyDescription;
begin
  Result := Self.FindParameterForAddressSpace(AddressSpace) as TIdProxyDescription;
end;

function TIdProxyDescriptions.GetDefaultRoutePath: TIdSipRoutePath;
begin
  Result := (Self.DefaultParameter as TIdProxyDescription).RoutePath;
end;

procedure TIdProxyDescriptions.SetDefaultRoutePath(Value: TIdSipRoutePath);
var
  Default: TIdProxyDescription;
begin
  Default := Self.DefaultParameter as TIdProxyDescription;

  Default.RoutePath.Clear;
  Default.RoutePath.Add(Value);
end;

end.
