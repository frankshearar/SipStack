{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipProxyDescription;

interface

uses
  IdAddressSpace, IdSipMessage, IdSipProxyDescription, TestFramework;

type
  TestTIdProxyDescription = class(TTestCase)
  private
    Desc: TIdProxyDescription;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressSpaceAndProxyFor;
    procedure TestContainsBeforeSettingDescription;
    procedure TestNumBitsAndNetmaskIdentical;
  end;

  TestTIdProxyDescriptions = class(TTestCase)
  private
    DefaultRoute:          TIdSipRoutePath;
    EmptyRoute:            TIdSipRoutePath;
    LanAddressSpace:       String;
    LanRoute:              TIdSipRoutePath;
    LanTarget:             String;
    LocalhostAddressSpace: String;
    LocalhostRoute:        TIdSipRoutePath;
    LocalhostTarget:       String;
    Proxies:               TIdProxyDescriptions;

    procedure CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String); overload;
    procedure CheckExceptionAddedFirst;
    procedure CheckExceptionAddedLast;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDescriptionAndRoutePathFor;
    procedure TestAddRoute;
    procedure TestAddRouteNoProxyPresent;
    procedure TestAddRouteForCanonicalisesNetmask;
    procedure TestAddRouteCanonicalisesNetmask;
    procedure TestClearAllParameters;
    procedure TestContainedRoutesOrderIndependent;
    procedure TestRemoveDescription;
    procedure TestRoutePathForDefaultRoute;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdProxyDescription unit tests');
  Result.AddSuite(TestTIdProxyDescription.Suite);
  Result.AddSuite(TestTIdProxyDescriptions.Suite);
end;

//******************************************************************************
//* TestTIdProxyDescription                                                    *
//******************************************************************************
//* TestTIdProxyDescription Public methods *************************************

procedure TestTIdProxyDescription.SetUp;
begin
  inherited SetUp;

  Self.Desc := TIdProxyDescription.Create;
end;

procedure TestTIdProxyDescription.TearDown;
begin
  Self.Desc.Free;

  inherited TearDown;
end;

procedure TestTIdProxyDescription.TestAddressSpaceAndProxyFor;
const
  LanClient        = 'roke.local';
  Localhost        = '127.0.0.1';
  SixToFourAddress = '2002:5156:4278::1';
begin
  Self.Desc.AddressSpace := '127.0.0.0/8';
  Check(    Self.Desc.ProxyFor(Localhost),        'Proxying for the local loop subnet: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for the local loop subnet: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'Proxying for the local loop subnet: FQDN');

  Self.Desc.AddressSpace := '::/0';
  Check(not Self.Desc.ProxyFor(Localhost),        'Proxying for IPv6: localhost');
  Check(    Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for IPv6: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'Proxying for IPv6: FQDN');

  Self.Desc.AddressSpace := 'local';
  Check(not Self.Desc.ProxyFor(Localhost),        'Proxying for a domain: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'Proxying for a domain: IPv6 address');
  Check(    Self.Desc.ProxyFor(LanClient),        'Proxying for a domain: FQDN');
end;

procedure TestTIdProxyDescription.TestContainsBeforeSettingDescription;
const
  LanClient        = 'roke.local';
  Localhost        = '127.0.0.1';
  SixToFourAddress = '2002:5156:4278::1';
begin
  Check(not Self.Desc.ProxyFor(Localhost),        'No description set: localhost');
  Check(not Self.Desc.ProxyFor(SixToFourAddress), 'No description set: IPv6 address');
  Check(not Self.Desc.ProxyFor(LanClient),        'No description set: FQDN');
end;

procedure TestTIdProxyDescription.TestNumBitsAndNetmaskIdentical;
const
  LanAddress = '10.0.0.1';
  LanNetwork = '10.0.0.0';
begin
  Self.Desc.AddressSpace := LanNetwork + '/8';
  Check(Self.Desc.ProxyFor(LanAddress), 'LAN address, Description a number of significant bits');

  Self.Desc.AddressSpace := LanNetwork + '/255.0.0.0';
  Check(Self.Desc.ProxyFor(LanAddress), 'LAN address, Description a netmask');
end;

//******************************************************************************
//* TestTIdProxyDescriptions                                                   *
//******************************************************************************
//* TestTIdProxyDescriptions Public methods ************************************

procedure TestTIdProxyDescriptions.SetUp;
begin
  inherited SetUp;

  Self.Proxies := TIdProxyDescriptions.Create;

  Self.DefaultRoute := TIdSipRoutePath.Create;
  Self.DefaultRoute.Add(RouteHeader).Value := '<sip:default_gateway>';

  Self.EmptyRoute := TIdSipRoutePath.Create;

  Self.LanAddressSpace := 'local';
  Self.LanRoute := TIdSipRoutePath.Create;
  Self.LanRoute.Add(RouteHeader).Value := '<sip:roke.local;lr>';
  Self.LanRoute.Add(RouteHeader).Value := '<sip:vpn.local;lr>';
  Self.LanTarget := 'gont.local';

  Self.LocalhostAddressSpace := '127.0.0.0/8';
  Self.LocalhostRoute := TIdSipRoutePath.Create;
  Self.LocalhostRoute.Add(RouteHeader).Value := '<sip:127.0.0.1:35060;lr>';
  Self.LocalhostTarget := '127.0.0.2';
end;

procedure TestTIdProxyDescriptions.TearDown;
begin
  Self.LocalhostRoute.Free;
  Self.LanRoute.Free;
  Self.EmptyRoute.Free;
  Self.DefaultRoute.Free;
  Self.Proxies.Free;

  inherited TearDown;
end;

//* TestTIdProxyDescriptions Private methods ***********************************

procedure TestTIdProxyDescriptions.CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String);
begin
  CheckEquals(Expected.Count, Received.Count, Msg + ': route path length');

  Expected.First;
  Received.First;
  while (Expected.HasNext) do begin
    CheckEquals(Expected.CurrentRoute.FullValue, Received.CurrentRoute.FullValue, Msg + ': unexpected Route');

    Expected.Next;
    Received.Next;
  end;
end;

procedure TestTIdProxyDescriptions.CheckExceptionAddedFirst;
const
  MainAddressSpace      = '10.0.0.0/8';
  ExceptionalAddress    = '10.0.0.0/24';
  MainSpaceAddress      = '10.0.1.1';
  ExceptionSpaceAddress = '10.0.0.1';
var
  ExceptionRoute: TIdSipRoutePath;
  MainRoute:      TIdSipRoutePath;
begin
  // Route: <null> 10.0.0.0/8
  // Route: <sip:proxy;lr> 10.0.0.0/24
  ExceptionRoute := TIdSipRoutePath.Create;
  try
    MainRoute := TIdSipRoutePath.Create;
    try
      MainRoute.Add(RouteHeader).Value := '<sip:proxy;lr>';

      Self.Proxies.AddDescription(ExceptionalAddress, ExceptionRoute);
      Self.Proxies.AddDescription(MainAddressSpace, MainRoute);

      Check(not Self.Proxies.RoutePathFor(MainSpaceAddress).IsEmpty,   'Wrong route path for main address space; exception added first');
      Check(    Self.Proxies.RoutePathFor(ExceptionSpaceAddress).IsEmpty, 'Wrong route path for exceptional address; exception added first');
    finally
      MainRoute.Free;
    end;
  finally
    ExceptionRoute.Free;
  end;
end;

procedure TestTIdProxyDescriptions.CheckExceptionAddedLast;
const
  MainAddressSpace      = 'tessier-ashpool.co.luna';
  ExceptionalAddress    = 'proxy.' + MainAddressSpace;
  MainSpaceAddress      = 'foo.' + MainAddressSpace;
  ExceptionSpaceAddress = 'foo.' + ExceptionalAddress;
var
  ExceptionRoute: TIdSipRoutePath;
  MainRoute:      TIdSipRoutePath;
begin
  // Route: <sip:proxy;lr> 10.0.0.0/24
  // Route: <null> 10.0.0.0/8
  ExceptionRoute := TIdSipRoutePath.Create;
  try
    MainRoute := TIdSipRoutePath.Create;
    try
      MainRoute.Add(RouteHeader).Value := '<sip:proxy;lr>';

      Self.Proxies.AddDescription(MainAddressSpace, MainRoute);
      Self.Proxies.AddDescription(ExceptionalAddress, ExceptionRoute);

      Check(not Self.Proxies.RoutePathFor(MainSpaceAddress).IsEmpty,   'Wrong route path for main address space; exception added last');
      Check(    Self.Proxies.RoutePathFor(ExceptionSpaceAddress).IsEmpty, 'Wrong route path for exceptional address; exception added last');
    finally
      MainRoute.Free;
    end;
  finally
    ExceptionRoute.Free;
  end;
end;

//* TestTIdProxyDescriptions Published methods *********************************

procedure TestTIdProxyDescriptions.TestAddDescriptionAndRoutePathFor;
begin
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  CheckEquals(Self.LanRoute,       Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space');
end;

procedure TestTIdProxyDescriptions.TestAddRoute;
var
  EmptyPath: TIdSipRoutePath;
begin
  EmptyPath := TIdSipRoutePath.Create;
  try
    Self.Proxies.AddDescription(Self.LanAddressSpace, EmptyPath);
  finally
    EmptyPath.Free;
  end;

  Self.LanRoute.First;
  while Self.LanRoute.HasNext do begin
    Self.Proxies.AddRouteFor(Self.LanAddressSpace, Self.LanRoute.CurrentRoute);
    Self.LanRoute.Next;
  end;

  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN routes not all added');
end;

procedure TestTIdProxyDescriptions.TestAddRouteNoProxyPresent;
begin
  Self.LanRoute.First;
  while Self.LanRoute.HasNext do begin
    Self.Proxies.AddRouteFor(Self.LanAddressSpace, Self.LanRoute.CurrentRoute);
    Self.LanRoute.Next;
  end;

  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN routes not all added');
end;

procedure TestTIdProxyDescriptions.TestAddRouteForCanonicalisesNetmask;
const
  CIDRFormat = '10.0.0.0/8';
begin
  // The first AddRouteFor adds a new address space.
  // The second AddRouteFor should match against the first address space added.

  Self.Proxies.AddRouteFor(CIDRFormat, Self.LanRoute.Items[0] as TIdSipRouteHeader);
  Self.Proxies.AddRouteFor(CIDRFormat, Self.LanRoute.Items[1] as TIdSipRouteHeader);

  CheckEquals(1, Self.Proxies.Count, 'AddRouteFor doesn''t canonicalise a netmask, if present');
  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor('10.0.0.1'), 'Route path incorrect');
end;

procedure TestTIdProxyDescriptions.TestAddRouteCanonicalisesNetmask;
begin
  Self.Proxies.AddDescription('10.0.0.0/8', Self.LanRoute);
  Self.Proxies.AddDescription('10.0.0.0/255.0.0.0', Self.LocalhostRoute);

  CheckEquals(1, Self.Proxies.Count, '10.0.0.0/8 and 10.0.0.0/255.0.0.0 are the same address space, but the ProxyDescriptions doesn''t recognise this');
end;

procedure TestTIdProxyDescriptions.TestClearAllParameters;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  Self.Proxies.ClearAllParameters;

  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space');
  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space');
  CheckEquals(Self.EmptyRoute, Self.Proxies.RoutePathFor('example.com'),        'Default address space');
end;

procedure TestTIdProxyDescriptions.TestContainedRoutesOrderIndependent;
begin
  // Previously, an address space for a destination was found on a first come,
  // first served basis. That means that if one wanted to have a description
  // like "all addresses in this address space, EXCEPT that address" one had to
  // be very careful about the order of the address spaces.

  Self.CheckExceptionAddedFirst;
  Self.CheckExceptionAddedLast;
end;

procedure TestTIdProxyDescriptions.TestRemoveDescription;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);

  Self.Proxies.RemoveDescription('unknown');
  CheckEquals(Self.LanRoute,       Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space removed instead of "unknown"');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space removed instead of "unknown"');

  Self.Proxies.RemoveDescription(Self.LanAddressSpace);
  CheckEquals(Self.DefaultRoute,   Self.Proxies.RoutePathFor(Self.LanTarget),       'LAN address space not removed');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Localhost address space removed instead of "unknown"');
end;

procedure TestTIdProxyDescriptions.TestRoutePathForDefaultRoute;
begin
  Self.Proxies.DefaultRoutePath := Self.DefaultRoute;
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'Default route, LAN target');
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'Default route, Localhost target');

  Self.Proxies.AddDescription(Self.LanAddressSpace, Self.LanRoute);
  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN route added, LAN target');
  CheckEquals(Self.DefaultRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'LAN route added, Localhost target');

  Self.Proxies.AddDescription(Self.LocalhostAddressSpace, Self.LocalhostRoute);
  CheckEquals(Self.LanRoute, Self.Proxies.RoutePathFor(Self.LanTarget), 'LAN & localhost routes added, LAN target');
  CheckEquals(Self.LocalhostRoute, Self.Proxies.RoutePathFor(Self.LocalhostTarget), 'LAN & localhost routes added, Localhost target');
end;

initialization
  RegisterTest('Proxy description tests', Suite);
end.
