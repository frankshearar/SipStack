{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdConnectionBindings;

interface

uses
  IdConnectionBindings, TestFramework;

type
  TestTIdConnectionBindings = class(TTestCase)
  private
    Binding: TIdConnectionBindings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignFromLocation;
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
  end;

implementation

uses
  Classes, IdSipLocation, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('TIdConnectionBindings tests');
  Result.AddTest(TestTIdConnectionBindings.Suite);
end;

//******************************************************************************
//* TestTIdConnectionBindings                                               *
//******************************************************************************
//* TestTIdConnectionBindings Public methods ********************************

procedure TestTIdConnectionBindings.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create;
  Self.Binding.LocalIP   := '127.0.0.1';
  Self.Binding.LocalPort := 5060;
  Self.Binding.PeerIP    := '::1';
  Self.Binding.PeerPort  := 4444;
  Self.Binding.Transport := 'TLS-SCTP';
end;

procedure TestTIdConnectionBindings.TearDown;
begin
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdConnectionBindings Public methods ********************************

procedure TestTIdConnectionBindings.TestAssign;
var
  Other: TIdConnectionBindings;
begin
  Other := TIdConnectionBindings.Create;
  try
    Other.Assign(Self.Binding);
    CheckEquals(Self.Binding.LocalIP,
                Other.LocalIP,
                'LocalIP');
    CheckEquals(Self.Binding.LocalPort,
                Other.LocalPort,
                'LocalPort');
    CheckEquals(Self.Binding.PeerIP,
                Other.PeerIP,
                'PeerIP');
    CheckEquals(Self.Binding.PeerPort,
                Other.PeerPort,
                'PeerPort');
    CheckEquals(Self.Binding.Transport,
                Other.Transport,
                'Transport');
  finally
    Other.Free;
  end;
end;

procedure TestTIdConnectionBindings.TestAssignFromLocation;
var
  Loc: TIdSipLocation;
begin
  Loc := TIdSipLocation.Create('TLS', '10.0.0.1', 10000);
  try
    Self.Binding.Assign(Loc);
    CheckEquals('',            Self.Binding.LocalIP,   'LocalIP');
    CheckEquals(0,             Self.Binding.LocalPort, 'LocalPort');
    CheckEquals(Loc.IPAddress, Self.Binding.PeerIP,    'PeerIP');
    CheckEquals(Loc.Port,      Self.Binding.PeerPort,  'PeerPort');
    CheckEquals(Loc.Transport, Self.Binding.Transport, 'Transport');
  finally
    Loc.Free;
  end;
end;

procedure TestTIdConnectionBindings.TestAsString;
var
  Expected: String;
begin
  Expected := Format(BindingTuple, [Self.Binding.LocalIP,
                                    Self.Binding.LocalPort,
                                    Self.Binding.PeerIP,
                                    Self.Binding.PeerPort,
                                    Self.Binding.Transport]);

  CheckEquals(Expected,
              Self.Binding.AsString,
              'AsString');
end;

procedure TestTIdConnectionBindings.TestCopy;
var
  Other: TIdConnectionBindings;
begin
  Other := Self.Binding.Copy;
  try
    Check(Other.Equals(Self.Binding),
          'Copy doesn''t contain a copy of the binding''s values');
  finally
    Other.Free;
  end;
end;

procedure TestTIdConnectionBindings.TestEquals;
var
  Other: TIdConnectionBindings;
begin
  Other := TIdConnectionBindings.Create;
  try
    Other.LocalIP   := Self.Binding.LocalIP + '1';
    Other.LocalPort := Self.Binding.LocalPort + 1;
    Other.PeerIP    := Self.Binding.PeerIP + '1';
    Other.PeerPort  := Self.Binding.PeerPort + 1;

    Check(not Other.Equals(Self.Binding),
          'No values equal');

    Other.LocalIP := Self.Binding.LocalIP;
    Check(not Other.Equals(Self.Binding),
          'LocalIP equal');

    Other.LocalPort := Self.Binding.LocalPort;
    Check(not Other.Equals(Self.Binding),
          'LocalPort, LocalPort equal');

    Other.PeerIP := Self.Binding.PeerIP;
    Check(not Other.Equals(Self.Binding),
          'LocalPort, LocalPort, PeerIP equal');

    Other.PeerPort := Self.Binding.PeerPort;
    Check(not Other.Equals(Self.Binding),
          'LocalPort, LocalPort, PeerIP, PeerPort equal, transports not equal');

    Other.Transport := Self.Binding.Transport;
    Check(Other.Equals(Self.Binding),
          'LocalPort, LocalPort, PeerIP, PeerPort, Transport equal');
  finally
    Other.Free;
  end;
end;

initialization
  RegisterTest('Connection description', Suite);
end.
