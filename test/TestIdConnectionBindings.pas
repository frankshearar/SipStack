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
    procedure TestCreateWithParameters;
    procedure TestEquals;
  end;

  TestTIdConnectionBindingsSet = class(TTestCase)
  private
    BindingSet:   TIdConnectionBindingsSet;
    NewBinding:   TIdConnectionBindings;
    OtherBinding: TIdConnectionBindings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsStringWithPrefix;
    procedure TestClear;
    procedure TestGetBindings;
    procedure TestHasBinding;
    procedure TestIsEmpty;
    procedure TestNoDuplicates;
    procedure TestRemoveBinding;
  end;

implementation

uses
  Classes, IdSipLocation, SysUtils;

const
  CRLF = #$D#$A;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('TIdConnectionBindings tests');
  Result.AddTest(TestTIdConnectionBindings.Suite);
  Result.AddTest(TestTIdConnectionBindingsSet.Suite);
end;

//******************************************************************************
//* TestTIdConnectionBindings                                                  *
//******************************************************************************
//* TestTIdConnectionBindings Public methods ***********************************

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

//* TestTIdConnectionBindings Published methods ********************************

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

procedure TestTIdConnectionBindings.TestCreateWithParameters;
var
  C: TIdConnectionBindings;
begin
  C := TIdConnectionBindings.Create(Self.Binding.LocalIP,
                                    Self.Binding.LocalPort,
                                    Self.Binding.PeerIP,
                                    Self.Binding.PeerPort,
                                    Self.Binding.Transport);
  try
    CheckEquals(Self.Binding.LocalIP,   C.LocalIP,   'LocalIP');
    CheckEquals(Self.Binding.LocalPort, C.LocalPort, 'LocalPort');
    CheckEquals(Self.Binding.PeerIP,    C.PeerIP,    'PeerIP');
    CheckEquals(Self.Binding.PeerPort,  C.PeerPort,  'PeerPort');
    CheckEquals(Self.Binding.Transport, C.Transport, 'Transport');
  finally
    C.Free;
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

//******************************************************************************
//* TestTIdConnectionBindingsSet                                               *
//******************************************************************************
//* TestTIdConnectionBindingsSet Public methods ********************************

procedure TestTIdConnectionBindingsSet.SetUp;
begin
  inherited SetUp;

  Self.BindingSet := TIdConnectionBindingsSet.Create;

  Self.NewBinding := TIdConnectionBindings.Create;
  Self.NewBinding.LocalIP   := '127.0.0.1';
  Self.NewBinding.LocalPort := 5060;
  Self.NewBinding.Transport := 'TCP';

  Self.OtherBinding := TIdConnectionBindings.Create;
  Self.OtherBinding.LocalIP   := '10.0.0.1';
  Self.OtherBinding.LocalPort := 5060;
  Self.OtherBinding.Transport := 'TCP';
end;

procedure TestTIdConnectionBindingsSet.TearDown;
begin
  Self.NewBinding.Free;
  Self.BindingSet.Free;

  inherited TearDown;
end;

//* TestTIdConnectionBindingsSet Published methods *****************************

procedure TestTIdConnectionBindingsSet.TestAddAndCount;
begin
  CheckEquals(0, Self.BindingSet.Count, 'Empty set');

  Self.BindingSet.Add(Self.NewBinding);
  CheckEquals(1, Self.BindingSet.Count, 'One element set');

  Self.BindingSet.Add(Self.OtherBinding);
  CheckEquals(2, Self.BindingSet.Count, 'Two element set');
end;

procedure TestTIdConnectionBindingsSet.TestAssign;
var
  Other: TIdConnectionBindingsSet;
begin
  Other := TIdConnectionBindingsSet.Create;
  try
    Self.BindingSet.Add(Self.NewBinding);

    Other.Assign(Self.BindingSet);
    CheckEquals(Other.AsString, Self.BindingSet.AsString, 'Set not assigned');

    Self.BindingSet.Remove(Self.NewBinding);
    Self.BindingSet.Add(Self.OtherBinding);
    Other.Assign(Self.BindingSet);
    CheckEquals(Other.AsString, Self.BindingSet.AsString, 'Set not cleared, then assigned');
  finally
    Other.Free;
  end;
end;

procedure TestTIdConnectionBindingsSet.TestAsString;
begin
  CheckEquals('', Self.BindingSet.AsString, 'Empty set');

  Self.BindingSet.Add(Self.NewBinding);
  CheckEquals(Self.NewBinding.AsString, Self.BindingSet.AsString, 'One element set');

  Self.BindingSet.Add(Self.OtherBinding);
  CheckEquals(Self.NewBinding.AsString + CRLF + Self.OtherBinding.AsString,
              Self.BindingSet.AsString, 'Two element set');
end;

procedure TestTIdConnectionBindingsSet.TestAsStringWithPrefix;
const
  Prefix = 'Binding: ';
begin
  CheckEquals('', Self.BindingSet.AsStringWithPrefix(Prefix), 'Empty set');

  Self.BindingSet.Add(Self.NewBinding);
  CheckEquals(Prefix + Self.NewBinding.AsString, Self.BindingSet.AsStringWithPrefix(Prefix), 'One element set');

  Self.BindingSet.Add(Self.OtherBinding);
  CheckEquals(Prefix + Self.NewBinding.AsString + CRLF + Prefix + Self.OtherBinding.AsString,
              Self.BindingSet.AsStringWithPrefix(Prefix), 'Two element set');
end;

procedure TestTIdConnectionBindingsSet.TestClear;
begin
  Self.BindingSet.Clear;
  CheckEquals(0, Self.BindingSet.Count, 'Clearing an empty set is a no-op');

  Self.BindingSet.Add(Self.NewBinding);
  Self.BindingSet.Add(Self.OtherBinding);

  Self.BindingSet.Clear;
  CheckEquals(0, Self.BindingSet.Count, 'Set not cleared');
end;

procedure TestTIdConnectionBindingsSet.TestGetBindings;
begin
  Self.BindingSet.Add(Self.NewBinding);
  Self.BindingSet.Add(Self.OtherBinding);

  Check(Self.BindingSet[0].Equals(Self.NewBinding),   'Binding at index 0');
  Check(Self.BindingSet[1].Equals(Self.OtherBinding), 'Binding at index 1');
end;

procedure TestTIdConnectionBindingsSet.TestHasBinding;
begin
  Check(not Self.BindingSet.HasBinding(Self.NewBinding), 'Empty set');
  Check(not Self.BindingSet.HasBinding(Self.OtherBinding), 'Empty set');

  Self.BindingSet.Add(Self.NewBinding);
  Check(    Self.BindingSet.HasBinding(Self.NewBinding),   'Binding not added');
  Check(not Self.BindingSet.HasBinding(Self.OtherBinding), 'Unknown binding added');

  Self.BindingSet.Add(Self.OtherBinding);
  Check(Self.BindingSet.HasBinding(Self.NewBinding),   'Binding removed');
  Check(Self.BindingSet.HasBinding(Self.OtherBinding), 'Unknown binding not added');
end;

procedure TestTIdConnectionBindingsSet.TestIsEmpty;
begin
  Check(Self.BindingSet.IsEmpty, 'Empty set');

  Self.BindingSet.Add(Self.NewBinding);
  Check(not Self.BindingSet.IsEmpty, 'Non-empty set');

  Self.BindingSet.Clear;
  Check(Self.BindingSet.IsEmpty, 'Cleared set');
end;

procedure TestTIdConnectionBindingsSet.TestNoDuplicates;
begin
  Self.BindingSet.Add(Self.NewBinding);
  Self.BindingSet.Add(Self.NewBinding);
  CheckEquals(1, Self.BindingSet.Count, 'Sets don''t allow duplicates');
end;

procedure TestTIdConnectionBindingsSet.TestRemoveBinding;
begin
  Self.BindingSet.Remove(Self.NewBinding);
  CheckEquals(0, Self.BindingSet.Count, 'Removing from an empty set is a no-op');

  Self.BindingSet.Add(Self.OtherBinding);
  Self.BindingSet.Remove(Self.NewBinding);
  CheckEquals(1, Self.BindingSet.Count, 'Removing a non-element from a set is a no-op');

  Self.BindingSet.Add(Self.NewBinding);

  Self.BindingSet.Remove(Self.NewBinding);
  CheckEquals(1, Self.BindingSet.Count, 'No binding removed');
  Check(not Self.BindingSet.HasBinding(Self.NewBinding),   'Correct binding not removed');
  Check(    Self.BindingSet.HasBinding(Self.OtherBinding), 'Incorrect binding removed');
end;

initialization
  RegisterTest('Connection description', Suite);
end.
