unit TestIdSipTransport;

interface

uses
  IdSipTransport, TestFramework;

type
  TestTIdSipTransport = class(TTestCase)
  protected
    Trans: TIdSipAbstractTransport;
    function TransportType: TIdSipTransportClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestWillUseReliableTransport; virtual;
  end;

  TestTIdSipMockTransport = class(TestTIdSipTransport)
  protected
    function TransportType: TIdSipTransportClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWillUseReliableTransport; override;
  end;

implementation

uses
  IdSipMessage, IdSipParser;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransport unit tests');
  Result.AddTest(TestTIdSipTransport.Suite);
  Result.AddTest(TestTIdSipMockTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipTransport                                                        *
//******************************************************************************
//* TestTIdSipTransport Public methods *****************************************

procedure TestTIdSipTransport.SetUp;
begin
  inherited SetUp;

  Self.Trans := Self.TransportType.Create;
end;

procedure TestTIdSipTransport.TearDown;
begin
  Self.Trans.Free;

  inherited TearDown;
end;

procedure TestTIdSipTransport.TestWillUseReliableTransport;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';

    Check(Self.Trans.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.Trans.WillUseReliableTranport(R), 'TLS');

    R.Path.LastHop.Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(not Self.Trans.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.Trans.WillUseReliableTranport(R), 'SCTP');

  finally
    R.Free;
  end;
end;

//* TestTIdSipTransport Protected methods **************************************

function TestTIdSipTransport.TransportType: TIdSipTransportClass;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipMockTransport                                                    *
//******************************************************************************
//* TestTIdSipMockTransport Public methods *************************************

procedure TestTIdSipMockTransport.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipMockTransport.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipMockTransport Protected methods **********************************

function TestTIdSipMockTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipMockTransport;
end;

//* TestTIdSipMockTransport Published methods **********************************

procedure TestTIdSipMockTransport.TestWillUseReliableTransport;
begin
  inherited TestWillUseReliableTransport;
end;

initialization
  RegisterTest('IdSipTransport', Suite);
end.
