unit IdSipProxyDescription;

interface

uses
  Contnrs, IdSipMessage;

type
  TIdAddressSpaceType = (asIPv4Subnet, asIPv6Subnet, asDomain, asUnknown);

  // I describe an address space - a subnet (either IPv4 or IPv6) or a domain.
  // For instance, "10.0.0.0/8" and "example.com" are both address spaces. You
  // can specify subnets either with the number of significant bits
  // ("10.0.0.0/8") or with a netmask ("2002:1::/ffff::"). Domains don't have
  // masks - their "space" consists of all domain names that end in the
  // specified suffix (so if the domain is "example.com", then "bob.example.com"
  // is in the address space, while "alice.somewhere.else.org" is not). The
  // special domain "" (the empty string) contains ALL domain names. It's
  // analogous to IPv4's "0.0.0.0/0".
  TIdAddressSpace = class(TObject)
  private
    fDescription: String;

    function SubnetTypeMatchesAddressType(SpaceType: TIdAddressSpaceType;
                                          Subnet: String): Boolean;
  protected
    function  InternalContains(Address: String): Boolean; virtual;
    procedure SetDescription(Value: String); virtual;
    function  SpaceType: TIdAddressSpaceType; virtual;
  public
    class function AddressToAddressSpaceType(Address: String): TIdAddressSpaceType;
    class function CreateAddressSpace(SpaceType: TIdAddressSpaceType): TIdAddressSpace; overload;
    class function CreateAddressSpace(Description: String): TIdAddressSpace; overload;

    function Contains(Address: String): Boolean;
    function IdentifySpaceType(Address: String): TIdAddressSpaceType;

    property Description: String read fDescription write SetDescription;
  end;

  TIdAddressSpaceClass = class of TIdAddressSpace;

  TIdIPAddressSpace = class(TIdAddressSpace)
  protected
    Mask:   String;
    Subnet: String;

    function  InternalContains(Address: String): Boolean; override;
    procedure SetDescription(Value: String); override;
  end;

  TIdIPv4SubnetAddressSpace = class(TIdIPAddressSpace)
  protected
    function SpaceType: TIdAddressSpaceType; override;
  end;

  TIdIPv6SubnetAddressSpace = class(TIdIPAddressSpace)
  protected
    function SpaceType: TIdAddressSpaceType; override;
  end;

  TIdDomainAddressSpace = class(TIdAddressSpace)
  protected
    function InternalContains(Address: String): Boolean; override;
    function SpaceType: TIdAddressSpaceType; override;
  end;

  TIdProxyDescription = class(TObject)
  private
    fRoutePath: TIdSipRoutePath;
    Space:      TIdAddressSpace;

    function  GetAddressSpace: String;
    procedure SetAddressSpace(Value: String);
    procedure SetRoutePath(Value: TIdSipRoutePath);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    function ProxyFor(Address: String): Boolean;

    property AddressSpace: String          read GetAddressSpace write SetAddressSpace;
    property RoutePath:    TIdSipRoutePath read fRoutePath write SetRoutePath;
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
  TIdProxyDescriptions = class(TObject)
  private
    Descs:             TObjectList;
    fDefaultRoutePath: TIdSipRoutePath;

    function  FindProxyFor(Address: String): TIdProxyDescription;
    function  FindProxyForAddressSpace(AddressSpace: String): TIdProxyDescription;
    function  HasProxyForAddressSpace(AddressSpace: String): Boolean;
    procedure SetDefaultRoutePath(Value: TIdSipRoutePath);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddDescription(AddressSpace: String; RoutePath: TIdSipRoutePath);
    procedure AddRouteFor(AddressSpace: String; Route: TIdSipRouteHeader); overload;
    procedure AddRouteFor(AddressSpace: String; Route: TIdSipUri); overload;
    procedure RemoveDescription(AddressSpace: String);
    function  RoutePathFor(Address: String): TIdSipRoutePath;

    property DefaultRoutePath: TIdSipRoutePath read fDefaultRoutePath write SetDefaultRoutePath;
  end;

function Suffix(S: String; Len: Cardinal): String;

implementation

uses
  IdSimpleParser, IdSystem, SysUtils;

function Suffix(S: String; Len: Cardinal): String;
var
  StringLength: Cardinal;
begin
  // WHY would someone make Length() return a signed integer? Can someone please
  // tell me what a string with negative length looks like?
  StringLength := Cardinal(Length(S));

  if (Len > StringLength) then
    Result := S
  else begin
    Result := Copy(S, StringLength - Len + 1, Len)
  end;
end;

//******************************************************************************
//* TIdAddressSpace                                                            *
//******************************************************************************
//* TIdAddressSpace Public methods *********************************************

class function TIdAddressSpace.AddressToAddressSpaceType(Address: String): TIdAddressSpaceType;
begin
  case TIdIPAddressParser.IPVersion(Address) of
    Id_IPv4: Result := asIPv4Subnet;
    Id_IPv6: Result := asIPv6Subnet;
  else
    if TIdSimpleParser.IsFQDN(Address) or (Address = '') then
      Result := asDomain
    else
      Result := asUnknown;
  end;
end;

class function TIdAddressSpace.CreateAddressSpace(SpaceType: TIdAddressSpaceType): TIdAddressSpace;
var
  ResultType: TIdAddressSpaceClass;
begin
  case SpaceType of
    asIPv4Subnet: ResultType := TIdIPv4SubnetAddressSpace;
    asIPv6Subnet: ResultType := TIdIPv6SubnetAddressSpace;
    asDomain:     ResultType := TIdDomainAddressSpace;
  else
    ResultType := TIdAddressSpace;
  end;

  Result := ResultType.Create;
end;

class function TIdAddressSpace.CreateAddressSpace(Description: String): TIdAddressSpace;
var
  A: TIdAddressSpace;
begin
  // TODO: Creating an object just to get to IdentifySpaceType is lame.
  
  A := TIdAddressSpace.Create;
  try
    Result := Self.CreateAddressSpace(A.IdentifySpaceType(Description));
    Result.Description := Description;
  finally
    A.Free;
  end;
end;

function TIdAddressSpace.Contains(Address: String): Boolean;
begin
  if (Self.SpaceType <> Self.AddressToAddressSpaceType(Address)) then begin
    Result := false;
    Exit;
  end;

  Result := Self.InternalContains(Address);
end;

function TIdAddressSpace.IdentifySpaceType(Address: String): TIdAddressSpaceType;
var
  Subnet: String;
begin
  if (Pos('/', Address) > 0) then begin
    // Process subnet
    Subnet := Address;
    Address := Fetch(Subnet, '/');

    Result := Self.AddressToAddressSpaceType(Address);

    if Self.SubnetTypeMatchesAddressType(Result, Subnet) then
      Result := Result
    else
      Result := asUnknown;
  end
  else begin
    // Process address or domain
    Result := Self.AddressToAddressSpaceType(Address);
  end;
end;

//* TIdAddressSpace Protected methods ******************************************

function TIdAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := false;
end;

procedure TIdAddressSpace.SetDescription(Value: String);
begin
  Self.fDescription := Value;
end;

function TIdAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asUnknown;
end;

//* TIdAddressSpace Private methods ********************************************

function TIdAddressSpace.SubnetTypeMatchesAddressType(SpaceType: TIdAddressSpaceType;
                                                      Subnet: String): Boolean;
var
  SubnetVer: TIdIPVersion;
begin
  // Given a subnet description (either something like "255.255.255.0" or
  // "ffff::" or a number), return true if that subnet description is compatible
  // with SpaceType.

  Result := TIdSimpleParser.IsNumber(Subnet);

  if not Result then begin
    SubnetVer := TIdIPAddressParser.IPVersion(Subnet);

    Result := ((SpaceType = asIPv4Subnet) and (SubnetVer = Id_IPv4))
           or ((SpaceType = asIPv6Subnet) and (SubnetVer = Id_IPv6))
  end;
end;

//******************************************************************************
//* TIdIPAddressSpace                                                          *
//******************************************************************************
//* TIdIPAddressSpace Public methods *******************************************

function TIdIPAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := OnSameNetwork(Self.Subnet, Address, Self.Mask);
end;

procedure TIdIPAddressSpace.SetDescription(Value: String);
var
  M: String;
begin
  inherited SetDescription(Value);

  M           := Value;
  Self.Subnet := Fetch(M, '/');

  if TIdSimpleParser.IsNumber(M) then
    Self.Mask := TIdIPAddressParser.MaskToAddress(StrToInt(M), TIdIPAddressParser.IPVersion(Self.Subnet))
  else
    Self.Mask := M;
end;

//******************************************************************************
//* TIdIPv4SubnetAddressSpace                                                  *
//******************************************************************************
//* TIdIPv4SubnetAddressSpace Protected methods ********************************

function TIdIPv4SubnetAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asIPv4Subnet;
end;

//******************************************************************************
//* TIdIPv6SubnetAddressSpace                                                  *
//******************************************************************************
//* TIdIPv6SubnetAddressSpace Protected methods ********************************

function TIdIPv6SubnetAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asIPv6Subnet;
end;

//******************************************************************************
//* TIdDomainAddressSpace                                                      *
//******************************************************************************
//* TIdDomainAddressSpace Protected methods ************************************

function TIdDomainAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := Suffix(Address, Length(Self.Description)) = Self.Description;
end;

function TIdDomainAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asDomain;
end;

//******************************************************************************
//* TIdProxyDescription                                                        *
//******************************************************************************
//* TIdProxyDescription Public methods *****************************************

constructor TIdProxyDescription.Create;
begin
  inherited Create;

  Self.fRoutePath := TIdSipRoutePath.Create;
  Self.Space      := TIdAddressSpace.Create;
end;

destructor TIdProxyDescription.Destroy;
begin
  Self.Space.Free;
  Self.RoutePath.Free;

  inherited Destroy;
end;

function TIdProxyDescription.ProxyFor(Address: String): Boolean;
begin
  // Is Address a member of the set of addresses in Self.Space?  

  Result := Self.Space.Contains(Address);
end;

//* TIdProxyDescription Private methods *****************************************

function TIdProxyDescription.GetAddressSpace: String;
begin
  Result := Self.Space.Description;
end;

procedure TIdProxyDescription.SetAddressSpace(Value: String);
begin
  if (Self.AddressSpace = Value) then Exit;

  Self.Space.Free;
  Self.Space := TIdAddressSpace.CreateAddressSpace(Value);
end;

procedure TIdProxyDescription.SetRoutePath(Value: TIdSipRoutePath);
begin
  Self.fRoutePath.Clear;
  Self.fRoutePath.Add(Value);
end;

//******************************************************************************
//* TIdProxyDescriptions                                                       *
//******************************************************************************
//* TIdProxyDescriptions Public methods ****************************************

constructor TIdProxyDescriptions.Create;
begin
  inherited Create;

  Self.Descs := TObjectList.Create(true);
  Self.fDefaultRoutePath := TIdSipRoutePath.Create;
end;

destructor TIdProxyDescriptions.Destroy;
begin
  Self.fDefaultRoutePath.Free;
  Self.Descs.Free;

  inherited Destroy;
end;

procedure TIdProxyDescriptions.AddDescription(AddressSpace: String; RoutePath: TIdSipRoutePath);
var
  Desc: TIdProxyDescription;
begin
  if Self.HasProxyForAddressSpace(AddressSpace) then Exit;

  Desc := TIdProxyDescription.Create;
  Self.Descs.Add(Desc);

  Desc.AddressSpace := AddressSpace;
  Desc.RoutePath    := RoutePath;
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

procedure TIdProxyDescriptions.RemoveDescription(AddressSpace: String);
var
  I: Integer;
begin
  // Remove the proxy description associated with AddressSpace.

  for I := 0 to Self.Descs.Count - 1 do begin
    if (TIdProxyDescription(Self.Descs[I]).AddressSpace = AddressSpace) then begin
      Self.Descs.Delete(I);
      Break;
    end;
  end;
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

//* TIdProxyDescriptions Private methods ***************************************

function TIdProxyDescriptions.FindProxyFor(Address: String): TIdProxyDescription;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Descs.Count - 1 do begin
    if TIdProxyDescription(Self.Descs[I]).ProxyFor(Address) then begin
      Result := TIdProxyDescription(Self.Descs[I]);
      Break;
    end;
  end;
end;

function TIdProxyDescriptions.FindProxyForAddressSpace(AddressSpace: String): TIdProxyDescription;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Descs.Count - 1 do begin
    if (TIdProxyDescription(Self.Descs[I]).AddressSpace = AddressSpace) then begin
      Result := TIdProxyDescription(Self.Descs[I]);
      Break;
    end;
  end;
end;

function TIdProxyDescriptions.HasProxyForAddressSpace(AddressSpace: String): Boolean;
begin
  Result := Assigned(Self.FindProxyForAddressSpace(AddressSpace));
end;

procedure TIdProxyDescriptions.SetDefaultRoutePath(Value: TIdSipRoutePath);
begin
  Self.fDefaultRoutePath.Clear;
  Self.fDefaultRoutePath.Add(Value);
end;

end.
