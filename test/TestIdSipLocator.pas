{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipLocator;

interface

uses
  IdSipLocator, IdSipMockLocator, TestFramework;

type
  TestTIdSipLocator = class(TTestCase)
  private
    Loc: TIdSipLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

  TestTIdSipMockLocator = class(TTestCase)
  private
    Loc: TIdSipMockLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddLocation;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocator unit tests');
  Result.AddTest(TestTIdSipLocator.Suite);
  Result.AddTest(TestTIdSipMockLocator.Suite);
end;

//******************************************************************************
//* TestTIdSipLocator                                                          *
//******************************************************************************
//* TestTIdSipLocator Public methods *******************************************

procedure TestTIdSipLocator.SetUp;
begin
  inherited SetUp;

  Self.Loc := TIdSipLocator.Create;
end;

procedure TestTIdSipLocator.TearDown;
begin
  Self.Loc.Free;

  inherited Destroy;
end;

//******************************************************************************
//* TestTIdSipMockLocator                                                      *
//******************************************************************************
//* TestTIdSipMockLocator Public methods ***************************************

procedure TestTIdSipMockLocator.SetUp;
begin
  inherited SetUp;

  Self.Loc := TIdSipMockLocator.Create;
end;

procedure TestTIdSipMockLocator.TearDown;
begin
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipMockLocator Published methods ************************************

procedure TestTIdSipMockLocator.TestAddLocation;
const
  AOR       = 'sip:foo@bar';
  Address   = '1.2.3.4';
  Port      = 15060;
  Transport = 'SCTP';
var
  Location: TIdSipLocation;
begin
  Self.Loc.AddLocation(AOR, Transport, Address, Port);

  Location := Self.Loc.ServerFor(AOR);

  CheckEquals(Address,   Location.Address,   'IPAddress');
  CheckEquals(Port,      Location.Port,      'Port');
  CheckEquals(Transport, Location.Transport, 'Transport');
end;

initialization
  RegisterTest('SIP Location Services', Suite);
end.
