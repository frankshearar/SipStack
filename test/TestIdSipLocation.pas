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
    procedure TestAsString;
    procedure TestCopy;
    procedure TestCreate;
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
    procedure TestCount;
    procedure TestIsEmpty;
    procedure TestRemoveFirst;
    procedure TestRemove;
    procedure TestRemoveNonExtantLocationDoesNothing;
  end;

implementation

uses
  IdSipConsts, IdSipDns, IdSipMessage, Math, SysUtils;

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
  Port      = IdPORT_SIP;
begin
  Self.Locs.AddLocation(Transport, Address, Port);

  CheckEquals(Address,   Self.Locs.First.IPAddress, 'IPAddress');
  CheckEquals(Port,      Self.Locs.First.Port,      'Port');
  CheckEquals(Transport, Self.Locs.First.Transport, 'Transport');

  Self.Locs.AddLocation(Self.Locs[0]);
  CheckEquals(2, Self.Locs.Count, 'Location not added');
  CheckEquals(Address,   Self.Locs[1].IPAddress, 'IPAddress of 2nd location');
  CheckEquals(Port,      Self.Locs[1].Port,      'Port of 2nd location');
  CheckEquals(Transport, Self.Locs[1].Transport, 'Transport of 2nd location');

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
  Port      = IdPORT_SIP;
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