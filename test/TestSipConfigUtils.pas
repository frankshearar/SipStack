unit TestSipConfigUtils;

interface

uses
  SipConfigUtils, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestDirective;
    procedure TestListenOn;
    procedure TestListenOnWithPort;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('SipConfigUtils unit tests');
  Result.AddTest(TestFunctions.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestDirective;
begin
  CheckEquals('Foo: Bar',          Directive('Foo', 'Bar'),          'Simple directive');
  CheckEquals('Foo: Bar Baz:Quux', Directive('Foo', 'Bar Baz:Quux'), 'Value with spaces');
  CheckEquals('Foo::Bar: Baz',     Directive('Foo::Bar', 'Baz'),     'Name with colons');
  CheckEquals('Foo :  Bar',        Directive('Foo ', ' Bar'),        'Name & value with spaces');
end;

procedure TestFunctions.TestListenOn;
begin
  CheckEquals('Listen: TCP AUTO',        ListenOn('TCP', 'AUTO'),        'AUTO');
  CheckEquals('Listen: TCP example.com', ListenOn('TCP', 'example.com'), 'FQDN');
  CheckEquals('Listen: UDP 127.0.0.1',   ListenOn('UDP', '127.0.0.1'),   'IPv4 address');
  CheckEquals('Listen: SCTP [::1]',      ListenOn('SCTP', '::1'),        'IPv6 address');
  CheckEquals('Listen: SCTP [::1]',      ListenOn('SCTP', '[::1]'),      'IPv6 reference');
end;

procedure TestFunctions.TestListenOnWithPort;
begin
  CheckEquals('Listen: TCP AUTO:1234',        ListenOn('TCP', 'AUTO', 1234),        'AUTO');
  CheckEquals('Listen: TCP example.com:1234', ListenOn('TCP', 'example.com', 1234), 'FQDN');
  CheckEquals('Listen: UDP 127.0.0.1:1234',   ListenOn('UDP', '127.0.0.1', 1234),   'IPv4 address');
  CheckEquals('Listen: SCTP [::1]:1234',      ListenOn('SCTP', '::1', 1234),        'IPv6 address');
  CheckEquals('Listen: SCTP [::1]:1234',      ListenOn('SCTP', '[::1]', 1234),      'IPv6 reference');
end;

initialization
  RegisterTest('SIP configuration tests', Suite);
end.
