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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClearAllParameters;
    procedure TestSetTransportFor;
    procedure TestSetTransportForNestedAddressSpaces;
  end;

implementation

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

//* TestTIdSipTransportSpecifiers Published methods ****************************

procedure TestTIdSipTransportSpecifiers.TestClearAllParameters;
begin
  Self.Trans.SetTransportFor(Self.LanAddressSpace,       Self.LanTransportType);
  Self.Trans.SetTransportFor(Self.LocalhostAddressSpace, Self.LocalhostTransportType);

  Self.Trans.ClearAllParameters;

  CheckEquals(Self.Trans.DefaultTransportType, Self.Trans.TransportTypeFor(Self.LanTarget), 'Parameters not removed');
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
