{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipLocation;

interface

uses
  IdSipLocation, TestFramework, TestFrameworkSip;

type
  TestTIdSipLocation = class(TTestCaseSip)
  private
    Address:   String;
    Loc:       TIdSipLocation;
    Port:      Cardinal;
    Transport: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAsString;
    procedure TestCopy;
    procedure TestCreate;
    procedure TestCreatePeerLocation;
    procedure TestEquals;
    procedure TestIsLocalhost;
  end;

  TestTIdSipLocations = class(TTestCase)
  private
    Locs: TIdSipLocations;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddLocation;
    procedure TestAddLocations;
    procedure TestAddLocationsFromNames;
    procedure TestAddLocationToFront;
    procedure TestAsString;
    procedure TestAsStringWithPrefix;
    procedure TestContains;
    procedure TestCount;
    procedure TestFirst;
    procedure TestFirstAddressMatch;
    procedure TestFirstAddressMatchTriesToMatchTransports;
    procedure TestFirstTransportMatch;
    procedure TestFirstTransportMatchEmptyList;
    procedure TestIsEmpty;
    procedure TestLast;
    procedure TestRemoveFirst;
    procedure TestRemove;
    procedure TestRemoveDuplicateLocationsOnlyRemovesOne;
    procedure TestRemoveEquivalentLocation;
    procedure TestRemoveNonExtantLocationDoesNothing;
  end;

implementation

uses
  IdConnectionBindings, IdSimpleParser, IdSipDns, IdSipMessage, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocation unit tests');
  Result.AddTest(TestTIdSipLocation.Suite);
  Result.AddTest(TestTIdSipLocations.Suite);
end;

//******************************************************************************
//* TestTIdSipLocation                                                         *
//******************************************************************************
//* TestTIdSipLocation Public methods ******************************************

procedure TestTIdSipLocation.SetUp;
begin
  inherited SetUp;

  Self.Address   := '127.0.0.1';
  Self.Port      := 9999;
  Self.Transport := TcpTransport;

  Self.Loc := TIdSipLocation.Create(Self.Transport, Self.Address, Self.Port);
end;

procedure TestTIdSipLocation.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipLocation Published methods ***************************************

procedure TestTIdSipLocation.TestAssign;
var
  Other: TIdSipLocation;
begin
  Other := TIdSipLocation.Create;
  try
    Other.Assign(Self.Loc);

    CheckEquals(Self.Loc.Transport, Other.Transport, 'Transport');
    CheckEquals(Self.Loc.IPAddress, Other.IPAddress, 'IPAddress');
    CheckEquals(Self.Loc.Port,      Other.Port,      'Port');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipLocation.TestAsString;
begin
  CheckEquals(Format(LocationTuple,
                     [Self.Loc.Transport, Self.Loc.IPAddress, Self.Loc.Port]),
              Self.Loc.AsString,
              'AsString');
end;

procedure TestTIdSipLocation.TestCopy;
var
  Copy: TIdSipLocation;
begin
  Copy := Self.Loc.Copy;
  try
    CheckEquals(Self.Loc.IPAddress, Copy.IPAddress, 'IPAddress');
    CheckEquals(Self.Loc.Port,      Copy.Port,      'Port');
    CheckEquals(Self.Loc.Transport, Copy.Transport, 'Transport');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSipLocation.TestCreate;
begin
  CheckEquals(Self.Address,   Self.Loc.IPAddress, 'IPAddress');
  CheckEquals(Self.Port,      Self.Loc.Port,      'Port');
  CheckEquals(Self.Transport, Self.Loc.Transport, 'Transport');
end;

procedure TestTIdSipLocation.TestCreatePeerLocation;
var
  Conn: TIdConnectionBindings;
  Loc:  TIdSipLocation;
begin
  Conn := TIdConnectionBindings.Create('127.0.0.1', 5060, '1.2.3.4', 1234, 'SCTP');
  try
    Loc := TIdSipLocation.CreatePeerLocation(Conn);
    try
      CheckEquals(Conn.PeerIP,    Loc.IPAddress, 'IPAddress');
      CheckEquals(Conn.PeerPort,  Loc.Port,      'Port');
      CheckEquals(Conn.Transport, Loc.Transport, 'Transport');
    finally
      Loc.Free;
    end;
  finally
    Conn.Free;
  end;
end;

procedure TestTIdSipLocation.TestEquals;
var
  Other: TIdSipLocation;
begin
  Other := Self.Loc.Copy;
  try
    Check(Other.Equals(Self.Loc), 'Other <> Self.Loc');
    Check(Self.Loc.Equals(Other), 'Self.Loc <> Other');

    Other.Transport := UdpTransport;
    Check(not Other.Equals(Self.Loc), 'Other = Self.Loc but transports differ');
    Check(not Self.Loc.Equals(Other), 'Self.Loc = Other but transports differ');

    Other.Transport := Self.Loc.Transport;
    Other.Port      := Self.Loc.Port + 1;
    Check(not Other.Equals(Self.Loc), 'Other = Self.Loc but ports differ');
    Check(not Self.Loc.Equals(Other), 'Self.Loc = Other but ports differ');

    Other.Port    := Self.Loc.Port;
    Other.IPAddress := TIdIPAddressParser.IncIPAddress(Self.Loc.IPAddress);
    Check(not Other.Equals(Self.Loc), 'Other = Self.Loc but addresses differ');
    Check(not Self.Loc.Equals(Other), 'Self.Loc = Other but addresses differ');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipLocation.TestIsLocalhost;
begin
  Self.Loc.IPAddress := '127.0.0.1';
  Check(Self.Loc.IsLocalhost, '127.0.0.1');

  Self.Loc.IPAddress := '10.0.0.6';
  Check(not Self.Loc.IsLocalhost, '10.0.0.6');

  Self.Loc.IPAddress := '2002:deca:fbad::1';
  Check(not Self.Loc.IsLocalhost, '2002:deca:fbad::1');

  Self.Loc.IPAddress := '::1';
  Check(Self.Loc.IsLocalhost, '::1');

  // I'm really not sure about this!
  Self.Loc.IPAddress := '127.0.0.2';
  Check(not Self.Loc.IsLocalhost, '127.0.0.2');
end;

//******************************************************************************
//* TestTIdSipLocations                                                        *
//******************************************************************************
//* TestTIdSipLocations Public methods *****************************************

procedure TestTIdSipLocations.SetUp;
begin
  inherited SetUp;

  Self.Locs := TIdSipLocations.Create;
end;

procedure TestTIdSipLocations.TearDown;
begin
  Self.Locs.Free;

  inherited TearDown;
end;

//* TestTIdSipLocations Published methods **************************************

procedure TestTIdSipLocations.TestAddLocation;
const
  Transport = TcpTransport;
  Address   = 'foo.com';
  Port      = DefaultSipPort;
var
  Result: TIdSipLocation;
begin
  Result := Self.Locs.AddLocation(Transport, Address, Port);

  CheckEquals(Address,   Self.Locs.First.IPAddress, 'IPAddress');
  CheckEquals(Port,      Self.Locs.First.Port,      'Port');
  CheckEquals(Transport, Self.Locs.First.Transport, 'Transport');
  Check(Result = Self.Locs[0], 'AddLocation(String, Cardinal, String) returned unexpected location');

  Result := Self.Locs.AddLocation(Self.Locs[0]);
  CheckEquals(2, Self.Locs.Count, 'Location not added');
  CheckEquals(Address,   Self.Locs[1].IPAddress, 'IPAddress of 2nd location');
  CheckEquals(Port,      Self.Locs[1].Port,      'Port of 2nd location');
  CheckEquals(Transport, Self.Locs[1].Transport, 'Transport of 2nd location');
  Check(Result = Self.Locs[1], 'AddLocation(TIdSipLocatio) returned unexpected location');

  Check(Self.Locs[0] <> Self.Locs[1],
        'Locations added the location, not a COPY of the location');
end;

procedure TestTIdSipLocations.TestAddLocations;
var
  I:              Integer;
  OtherLocations: TIdSipLocations;
begin
  OtherLocations := TIdSipLocations.Create;
  try
    OtherLocations.AddLocation('UDP',  '127.0.0.1', 5060);
    OtherLocations.AddLocation('TCP',  '127.0.0.1', 5060);
    OtherLocations.AddLocation('SCTP', '127.0.0.1', 5060);

    Self.Locs.AddLocations(OtherLocations);
    CheckEquals(OtherLocations.Count,
                Self.Locs.Count,
                'Not all locations added');

    for I := 0 to OtherLocations.Count - 1 do
      CheckEquals(OtherLocations[I].AsString,
                  Self.Locs[I].AsString,
                  IntToStr(I) + 'th location');            
  finally
    OtherLocations.Free;
  end;
end;

procedure TestTIdSipLocations.TestAddLocationsFromNames;
const
  Transport = TcpTransport;
  Port      = DefaultSipPort;
var
  I:     Integer;
  Names: TIdDomainNameRecords;
begin
  Names := TIdDomainNameRecords.Create;
  try
    Names.Add(DnsAAAARecord, 'foo.com', '::1');
    Names.Add(DnsARecord,    'bar.com', '127.0.0.1');

    for I := 0 to Min(Names.Count, Self.Locs.Count) - 1 do begin
      CheckEquals(Transport,
                  Self.Locs[I].Transport,
                  IntToStr(I) + 'th location transport');
      CheckEquals(Names[I].IPAddress,
                  Self.Locs[I].IPAddress,
                  IntToStr(I) + 'th location address');
      CheckEquals(Port,
                  Self.Locs[I].Port,
                  IntToStr(I) + 'th location port');
    end;

    Self.Locs.AddLocationsFromNames(Transport, Port, Names);

    CheckEquals(Names.Count,
                Self.Locs.Count,
                'Number of records');
  finally
    Names.Free;
  end;
end;

procedure TestTIdSipLocations.TestAddLocationToFront;
const
  Transport = TcpTransport;
  Address   = '127.0.0.1';
  Port      = DefaultSipPort;
var
  NewAddress: String;
  Result:     TIdSipLocation;
begin
  Result := Self.Locs.AddLocationToFront(Transport, Address, Port);

  CheckEquals(Address,   Self.Locs.First.IPAddress, 'IPAddress');
  CheckEquals(Port,      Self.Locs.First.Port,      'Port');
  CheckEquals(Transport, Self.Locs.First.Transport, 'Transport');
  Check(Result = Self.Locs[0], 'AddLocation(String, Cardinal, String) returned unexpected location');

  Result := Self.Locs.AddLocationToFront(Self.Locs[0]);
  CheckEquals(2, Self.Locs.Count, 'Location not added');
  CheckEquals(Address,   Self.Locs[0].IPAddress, 'IPAddress of 2nd location (in order of addition)');
  CheckEquals(Port,      Self.Locs[0].Port,      'Port of 2nd location (in order of addition)');
  CheckEquals(Transport, Self.Locs[0].Transport, 'Transport of 2nd location (in order of addition)');
  Check(Result = Self.Locs[0], 'AddLocationToFront(TIdSipLocation) returned unexpected location');

  Check(Self.Locs[0] <> Self.Locs[1],
        'Locations added the location, not a COPY of the location');

  NewAddress := TIdIPAddressParser.IncIPv4Address(Address);
  Self.Locs.AddLocationToFront(Transport, NewAddress, Port);
  CheckEquals(NewAddress, Self.Locs[0].IPAddress, 'Location not added to FRONT of list');
end;

procedure TestTIdSipLocations.TestAsString;
const
  FirstTransport  = 'UDP';
  FirstAddress    = '127.0.0.1';
  FirstPort       = 5060;
  SecondTransport = 'TLS';
  SecondAddress   = '10.0.0.1';
  SecondPort      = 5061;
var
  FirstLoc: String;
  SecondLoc: String;
begin
  FirstLoc  := Format(LocationTuple, [FirstTransport, FirstAddress, FirstPort]);
  SecondLoc := Format(LocationTuple, [SecondTransport, SecondAddress, SecondPort]);

  CheckEquals('', Self.Locs.AsString, 'Empty list');

  Self.Locs.AddLocation(FirstTransport, FirstAddress, FirstPort);
  CheckEquals(FirstLoc, Self.Locs.AsString, 'One element list');

  Self.Locs.AddLocation(SecondTransport, SecondAddress, SecondPort);
  CheckEquals(FirstLoc + CRLF + SecondLoc, Self.Locs.AsString, 'Two element list');
end;

procedure TestTIdSipLocations.TestAsStringWithPrefix;
const
  FirstTransport  = 'UDP';
  FirstAddress    = '127.0.0.1';
  FirstPort       = 5060;
  Prefix          = 'Bindings: ';
  SecondTransport = 'TLS';
  SecondAddress   = '10.0.0.1';
  SecondPort      = 5061;
var
  FirstLoc: String;
  SecondLoc: String;
begin
  FirstLoc  := Prefix + Format(LocationTuple, [FirstTransport, FirstAddress, FirstPort]);
  SecondLoc := Prefix + Format(LocationTuple, [SecondTransport, SecondAddress, SecondPort]);

  CheckEquals('', Self.Locs.AsStringWithPrefix(Prefix), 'Empty list');

  Self.Locs.AddLocation(FirstTransport, FirstAddress, FirstPort);
  CheckEquals(FirstLoc, Self.Locs.AsStringWithPrefix(Prefix), 'One element list');

  Self.Locs.AddLocation(SecondTransport, SecondAddress, SecondPort);
  CheckEquals(FirstLoc + CRLF + SecondLoc, Self.Locs.AsStringWithPrefix(Prefix), 'Two element list');
end;

procedure TestTIdSipLocations.TestContains;
var
  Search: TIdSipLocation;
begin
  Search := TIdSipLocation.Create('TCP', '::1', 5060);
  try
    Check(not Self.Locs.Contains(Search), 'Empty list');

    Self.Locs.AddLocation(Search.Transport, Search.IPAddress, Search.Port);
    Check(Self.Locs.Contains(Search), 'Search looks for equality, not identity');
  finally
    Search.Free;
  end;
end;

procedure TestTIdSipLocations.TestCount;
var
  I: Integer;
begin
  CheckEquals(0, Self.Locs.Count, 'Empty list');

  for I := 1 to 5 do begin
    Self.Locs.AddLocation(TcpTransport, 'foo.com', I);
    CheckEquals(I,
                Self.Locs.Count,
                'Added ' + IntToStr(I) + ' item(s)');
  end;
end;

procedure TestTIdSipLocations.TestFirst;
var
  FirstAddress: TIdSipLocation;
begin
  CheckNull(Self.Locs.First, 'First on an empty collection');

  FirstAddress  := Self.Locs.AddLocation(TcpTransport, '10.0.0.1',  5060);
  Check(FirstAddress = Self.Locs.First, 'First, 1-element collection');

  Self.Locs.AddLocation(TcpTransport, '10.0.0.1', 5060);
  Check(FirstAddress = Self.Locs.First, 'First, 2-element collection');
end;

procedure TestTIdSipLocations.TestFirstAddressMatch;
const
  IPv6LocalHost = '::1';
  LanIP         = '10.0.0.1';
  LocalAddress  = '10.0.0.6';
var
  Found:     TIdSipLocation;
  SearchLoc: TIdSipLocation;
begin
  SearchLoc := TIdSipLocation.Create('UDP', LocalAddress, 5060);
  try
    CheckNull(Self.Locs.FirstAddressMatch(SearchLoc), 'Empty collection');

    Self.Locs.AddLocation('TCP', LanIP, 5060);
    CheckNull(Self.Locs.FirstAddressMatch(SearchLoc), 'No match');

    Self.Locs.AddLocation('TCP', LocalAddress, 15060);
    Found := Self.Locs.FirstAddressMatch(SearchLoc);
    CheckNotNull(Found, 'Same address, different transport, different port');
    CheckEquals(SearchLoc.IPAddress, Found.IPAddress, 'Address of found location');

    SearchLoc.IPAddress := IPv6LocalHost;
    Self.Locs.AddLocation('TCP', SearchLoc.IPAddress, 15060);
    Found := Self.Locs.FirstAddressMatch(SearchLoc);
    CheckNotNull(Found, 'Same address, different transport, different port (IPv6)');
    CheckEquals(SearchLoc.IPAddress, Found.IPAddress, 'Address of found location (IPv6)');
  finally
    SearchLoc.Free;
  end;
end;

procedure TestTIdSipLocations.TestFirstAddressMatchTriesToMatchTransports;
const
  LocalAddress  = '10.0.0.6';
var
  Found:     TIdSipLocation;
  SearchLoc: TIdSipLocation;
begin
  SearchLoc := TIdSipLocation.Create('UDP', LocalAddress, 5060);
  try
    Self.Locs.AddLocation('TCP', LocalAddress, 5060);

    Found := Self.Locs.FirstAddressMatch(SearchLoc);
    CheckNotNull(Found, 'Same address, different transport');
    CheckEquals('TCP', Found.Transport, 'Transport of found location');

    Self.Locs.AddLocation('UDP', LocalAddress, 5060);
    Found := Self.Locs.FirstAddressMatch(SearchLoc);
    CheckNotNull(Found, 'Same address, same transport, same port');
    CheckEquals('UDP', Found.Transport, 'Transport-matching location not used in preference');
  finally
    SearchLoc.Free;
  end;
end;

procedure TestTIdSipLocations.TestFirstTransportMatch;
const
  LocalAddress  = '10.0.0.6';
var
  Found:     TIdSipLocation;
  SearchLoc: TIdSipLocation;
begin
  SearchLoc := TIdSipLocation.Create('UDP', LocalAddress, 5060);
  try
    Self.Locs.AddLocation('TCP', LocalAddress, 5060);

    Found := Self.Locs.FirstTransportMatch(SearchLoc);
    Check(Found = nil, 'Different transport');

    Self.Locs.AddLocation('UDP', LocalAddress, 5060);
    Found := Self.Locs.FirstTransportMatch(SearchLoc);
    Check(Found = Self.Locs[1], 'Same transport');
  finally
    SearchLoc.Free;
  end;
end;

procedure TestTIdSipLocations.TestFirstTransportMatchEmptyList;
var
  SearchLoc: TIdSipLocation;
begin
  SearchLoc := TIdSipLocation.Create('UDP', '10.0.0.6', 5060);
  try
    Check(nil = Self.Locs.FirstTransportMatch(SearchLoc), 'Empty list');
  finally
    SearchLoc.Free;
  end;
end;

procedure TestTIdSipLocations.TestIsEmpty;
var
  I: Integer;
begin
  Check(Self.Locs.IsEmpty, 'Empty list');

  for I := 1 to 5 do begin
    Self.Locs.AddLocation(TcpTransport, 'foo.com', I);
    Check(not Self.Locs.IsEmpty,
          'IsEmpty after ' + IntToStr(I) + ' item(s)');
  end;
end;

procedure TestTIdSipLocations.TestLast;
var
  FirstAddress:  TIdSipLocation;
  SecondAddress: TIdSipLocation;
begin
  CheckNull(Self.Locs.Last, 'Last on an empty collection');

  FirstAddress  := Self.Locs.AddLocation(TcpTransport, '10.0.0.1',  5060);
  Check(FirstAddress = Self.Locs.Last, 'Last, 1-element collection');

  SecondAddress := Self.Locs.AddLocation(TcpTransport, '10.0.0.1', 5060);
  Check(SecondAddress = Self.Locs.Last, 'Last, 2-element collection');
end;

procedure TestTIdSipLocations.TestRemoveFirst;
const
  FirstAddress  = '0.0.0.1';
  SecondAddress = '0.0.0.2';
begin
  Self.Locs.AddLocation(TcpTransport, FirstAddress,  5060);
  Self.Locs.AddLocation(TcpTransport, SecondAddress, 5060);

  Self.Locs.RemoveFirst;

  CheckEquals(1, Self.Locs.Count, 'No location removed');
  CheckEquals(SecondAddress, Self.Locs.First.IPAddress, 'Wrong location removed');
end;

procedure TestTIdSipLocations.TestRemove;
const
  FirstAddress  = '0.0.0.1';
  SecondAddress = '0.0.0.2';
begin
  Self.Locs.AddLocation(TcpTransport, FirstAddress,  5060);
  Self.Locs.AddLocation(TcpTransport, SecondAddress, 5060);

  Self.Locs.Remove(Self.Locs[1]);

  CheckEquals(1, Self.Locs.Count, 'No location removed');
  CheckEquals(FirstAddress, Self.Locs.First.IPAddress, 'Wrong location removed');

  Self.Locs.AddLocation(TcpTransport, SecondAddress, 5060);
  Self.Locs.Remove(Self.Locs.First);

  CheckEquals(1, Self.Locs.Count, 'No location removed (#2)');
  CheckEquals(SecondAddress, Self.Locs.First.IPAddress, 'Wrong location removed (#2)');
end;

procedure TestTIdSipLocations.TestRemoveDuplicateLocationsOnlyRemovesOne;
const
  Address = '2002:deca:fbad::1';
var
  Location:      TIdSipLocation;
  OriginalCount: Integer;
begin
  Self.Locs.AddLocation(TcpTransport, Address, 5060);
  Self.Locs.AddLocation(TcpTransport, Address, 5060);

  Location := Self.Locs.First.Copy;
  try
    OriginalCount := Self.Locs.Count;
    Self.Locs.Remove(Location);
    CheckEquals(Self.Locs.Count + 1, OriginalCount, 'Too many copies of location not removed');
  finally
    Location.Free;
  end;
end;

procedure TestTIdSipLocations.TestRemoveEquivalentLocation;
const
  Address = '2002:deca:fbad::1';
var
  Location:      TIdSipLocation;
  OriginalCount: Integer;
begin
  Self.Locs.AddLocation(TcpTransport, Address, 5060);

  Location := Self.Locs.First.Copy;
  try
    OriginalCount := Self.Locs.Count;
    Self.Locs.Remove(Location);
    Check(Self.Locs.Count < OriginalCount, 'Copy of location not removed');
  finally
    Location.Free;
  end;
end;

procedure TestTIdSipLocations.TestRemoveNonExtantLocationDoesNothing;
const
  FirstAddress  = '0.0.0.1';
  SecondAddress = '0.0.0.2';
var
  Location:      TIdSipLocation;
  OriginalCount: Integer;
begin
  Self.Locs.AddLocation(TcpTransport, FirstAddress,  5060);
  Self.Locs.AddLocation(TcpTransport, SecondAddress, 5060);
  OriginalCount := Self.Locs.Count;

  Location := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
  try
    // Show that trying to remove a location not in the list of locations does
    // nothing.

    Self.Locs.Remove(Location);

    CheckEquals(OriginalCount, Self.Locs.Count, 'A location was removed');
    CheckEquals(FirstAddress,  Self.Locs.First.IPAddress, '1st location removed');
    CheckEquals(SecondAddress, Self.Locs[1].IPAddress, '2nd location removed');
  finally
    Location.Free;
  end;
end;

initialization
  RegisterTest('SIP Location Data', Suite);
end.
