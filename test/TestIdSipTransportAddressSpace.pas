unit TestIdSipTransportAddressSpace;

interface

uses
  IdSipTransportAddressSpace, TestFramework;

type
  TestTIdSipTransportSpecifiers = class(TTestCase)
  private
    DefaultTransportType:   String;
    LanAddressSpace:        String;
    LanTarget:              String;
    LanTransportType:       String;
    LocalhostAddressSpace:  String;
    LocalhostTarget:        String;
    LocalhostTransportType: String;
    Trans:                  TIdSipTransportSpecifiers;

    function SubnetOf(AddressSpace: String): String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClearAllParameters;
    procedure TestRemoveTransportFor;
    procedure TestSetTransportFor;
    procedure TestSetTransportForNestedAddressSpaces;
  end;

implementation

uses
  IdSimpleParser;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransportAddressSpace unit tests');
  Result.AddSuite(TestTIdSipTransportSpecifiers.Suite);
end;

//******************************************************************************
//* TestTIdSipTransportSpecifiers                                              *
//******************************************************************************
//* TestTIdSipTransportSpecifiers Public methods *******************************

procedure TestTIdSipTransportSpecifiers.SetUp;
begin
  inherited SetUp;

  Self.DefaultTransportType   := 'tls';

  Self.LanAddressSpace  := '192.168.0.0/16';
  Self.LanTarget        := '192.168.0.1';
  Self.LanTransportType := 'tcp';

  Self.LocalhostAddressSpace  := '127.0.0.0/8';
  Self.LocalhostTarget        := '127.0.0.2';
  Self.LocalhostTransportType := 'udp';

  Self.Trans := TIdSipTransportSpecifiers.Create;
end;

procedure TestTIdSipTransportSpecifiers.TearDown;
begin
  Self.Trans.Free;

  inherited TearDown;
end;

//* TestTIdSipTransportSpecifiers Private methods ******************************

function TestTIdSipTransportSpecifiers.SubnetOf(AddressSpace: String): String;
var
  Mask:    String;
  Network: String;
  NumBits: Cardinal;
begin
  // Return an address space half the size of that specified in AddressSpace.
  // If AddressSpace is actually an IP address ("10.0.0.1/32"), just return
  // AddressSpace.

  Mask := AddressSpace;
  Network := Fetch(Mask, '/');

  NumBits := TIdIPAddressParser.AddressToMask(Mask, Id_IPv4);

  if (NumBits > 1) then
    Result := Network + TIdIPAddressParser.MaskToAddress(NumBits div 2, Id_IPv4)
  else
    Result := AddressSpace;
end;

//* TestTIdSipTransportSpecifiers Published methods ****************************

procedure TestTIdSipTransportSpecifiers.TestClearAllParameters;
begin
  Self.Trans.SetTransportFor(Self.LanAddressSpace,       Self.LanTransportType);
  Self.Trans.SetTransportFor(Self.LocalhostAddressSpace, Self.LocalhostTransportType);

  Self.Trans.ClearAllParameters;

  CheckEquals(Self.Trans.DefaultTransportType, Self.Trans.TransportTypeFor(Self.LanTarget), 'Parameters not removed');
end;

procedure TestTIdSipTransportSpecifiers.TestRemoveTransportFor;
var
  SubnetOfLan: String;
begin
  SubnetOfLan := Self.SubnetOf(Self.LanAddressSpace);

  Self.Trans.SetTransportFor(Self.LanAddressSpace, Self.LanTransportType);

  CheckEquals(Self.LanTransportType, Self.Trans.TransportTypeFor(Self.LanTarget),
              'Address space not added');

  Self.Trans.RemoveTransportFor(Self.LocalhostAddressSpace);
  CheckEquals(Self.LanTransportType, Self.Trans.TransportTypeFor(Self.LanTarget),
              'Wrong address space removed');

  Self.Trans.RemoveTransportFor(SubnetOfLan);
  CheckEquals(Self.LanTransportType, Self.Trans.TransportTypeFor(Self.LanTarget),
              'Address space removed');

  Self.Trans.RemoveTransportFor(Self.LanAddressSpace);
  CheckEquals('', Self.Trans.TransportTypeFor(Self.LanTarget),
              'Address space not removed');
end;

procedure TestTIdSipTransportSpecifiers.TestSetTransportFor;
begin
  Self.Trans.SetTransportFor(Self.LanAddressSpace,       Self.LanTransportType);
  Self.Trans.SetTransportFor(Self.LocalhostAddressSpace, Self.LocalhostTransportType);
  Self.Trans.DefaultTransportType := Self.DefaultTransportType;

  CheckEquals(Self.LanTransportType, Self.Trans.TransportTypeFor(Self.LanTarget), 'LAN');
  CheckEquals(Self.LocalhostTransportType, Self.Trans.TransportTypeFor(Self.LocalhostTarget), 'Localhost');

  CheckEquals(Self.Trans.DefaultTransportType, Self.Trans.TransportTypeFor('172.1.1.1'), 'Default');
end;

procedure TestTIdSipTransportSpecifiers.TestSetTransportForNestedAddressSpaces;
const
  ExceptedAddressSpace  = '127.1.1.0/24';
  ExceptedTarget        = '127.1.1.1';
  ExceptedTransportType = 'sctp';
begin
  Self.Trans.SetTransportFor(Self.LanAddressSpace,       Self.LanTransportType);
  Self.Trans.SetTransportFor(Self.LocalhostAddressSpace, Self.LocalhostTransportType);
  Self.Trans.SetTransportFor(ExceptedAddressSpace,       ExceptedTransportType);
  Self.Trans.DefaultTransportType := Self.DefaultTransportType;


  CheckEquals(Self.LanTransportType, Self.Trans.TransportTypeFor(Self.LanTarget), 'LAN');
  CheckEquals(Self.LocalhostTransportType, Self.Trans.TransportTypeFor(Self.LocalhostTarget), 'Localhost');
  CheckEquals(ExceptedTransportType, Self.Trans.TransportTypeFor(ExceptedTarget), 'Hole in localhost address space');

  CheckEquals(Self.Trans.DefaultTransportType, Self.Trans.TransportTypeFor('172.1.1.1'), 'Default');
end;

initialization
  RegisterTest('Transport parameter/Address space tests', Suite);
end.
