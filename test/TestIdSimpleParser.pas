unit TestIdSimpleParser;

interface

uses
  IdSimpleParser, TestFramework;

type
  TestTIdSimpleParser = class(TTestCase)
  private
    P: TIdSimpleParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsAlphaNumeric;
    procedure TestIsByte;
    procedure TestIsDigit;
    procedure TestIsFQDN;
    procedure TestIsHexNumber;
    procedure TestIsIpv4Address;
    procedure TestIsIpv6Address;
    procedure TestIsLetter;
    procedure TestIsNumber;
    procedure TestPeek;
    procedure TestPeekLine;
    procedure TestReadOctet;
    procedure TestReadOctets;
    procedure TestReadln;
    procedure TestReadlnDoubleCrLf;
    procedure TestReadlnWithNoCrLf;
  end;

implementation

uses
  Classes;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSimpleParser unit tests');
  Result.AddTest(TestTIdSimpleParser.Suite);
end;

//******************************************************************************
//* TestTIdSimpleParser                                                        *
//******************************************************************************
//* TestTIdSimpleParser Public methods *****************************************

procedure TestTIdSimpleParser.SetUp;
begin
  inherited SetUp;

  Self.P := TIdSimpleParser.Create;
end;

procedure TestTIdSimpleParser.TearDown;
begin
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSimpleParser Published methods **************************************

procedure TestTIdSimpleParser.TestIsAlphaNumeric;
var
  I: Integer;
  S: String;
begin
  Check(not P.IsAlphaNumeric(''),           '''''');
  Check(not P.IsAlphaNumeric(#0),           '#0');
  Check(not P.IsAlphaNumeric(#13),          '#13');
  Check(not P.IsAlphaNumeric(#$FF),         '#$FF');
  Check(    P.IsAlphaNumeric('a'),          'a');
  Check(not P.IsAlphaNumeric('a'#13#10'b'), 'a#13#10b');
  Check(    P.IsAlphaNumeric('a1b2c3d4'),   'a1b2c3d4');

  S := '';
  for I := 1 to 1000000 do
    S := S + 'a';
  Check(P.IsAlphaNumeric(S), '1 000 000 a''s');
end;

procedure TestTIdSimpleParser.TestIsByte;
begin
  Check(not TIdSimpleParser.IsByte(''),      '''''');
  Check(not TIdSimpleParser.IsByte('ct'),    'ct');
  Check(not TIdSimpleParser.IsByte('abc'#0), 'abc#0');
  Check(    TIdSimpleParser.IsByte('0'),     '0');
  Check(    TIdSimpleParser.IsByte('13'),    '13');
  Check(    TIdSimpleParser.IsByte('255'),   '255');
  Check(not TIdSimpleParser.IsByte('256'),   '256');
end;

procedure TestTIdSimpleParser.TestIsDigit;
var
  C: Char;
begin
  for C := '0' to '9' do
    Check(P.IsNumber(C), C);

  for C := Chr(0) to Chr(Ord('0') - 1) do
    Check(not P.IsNumber(C), C);

  for C := Chr(Ord('9') + 1) to Chr(255) do
    Check(not P.IsNumber(C), C);
end;

procedure TestTIdSimpleParser.TestIsFQDN;
begin
  Check(not TIdSimpleParser.IsFQDN(''),          '''''');
  Check(not TIdSimpleParser.IsFQDN('127.0.0.1'), 'No leading digits');
  Check(not TIdSimpleParser.IsFQDN('a.b.c-'),    'No trailing hyphens');
  Check(not TIdSimpleParser.IsFQDN('a.-b.c'),    'No leading hyphens');
  Check(not TIdSimpleParser.IsFQDN('a.1b.c'),    'No leading digits');
  Check(not TIdSimpleParser.IsFQDN('a..1b.c'),   'Empty label');
  Check(not TIdSimpleParser.IsFQDN('a.b.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz'),
        'Length(<label>) >= 64');

  Check(TIdSimpleParser.IsFQDN('abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk'),
        'Length(<label>) = 63');

  Check(    TIdSimpleParser.IsFQDN('a'),        'a');
  Check(    TIdSimpleParser.IsFQDN('a.b.c'),    'a.b.c');
  Check(    TIdSimpleParser.IsFQDN('a-a.b.c'),  'a-a.b.c');
  Check(    TIdSimpleParser.IsFQDN('a1.b2.c3'), 'a1.b2.c3');
end;

procedure TestTIdSimpleParser.TestIsHexNumber;
var
  C: Char;
begin
  for C := Low(Char) to High(Char) do
    if (C in ['0'..'9', 'a'..'f', 'A'..'F']) then
      Check(P.IsHexNumber(C), C)
    else
      Check(not P.IsHexNumber(C), C);

  Check(not P.IsHexNumber(''),             '''''');
  Check(not P.IsHexNumber('x'),            'x');
  Check(not P.IsHexNumber('cafex'),        'cafex');
  Check(    P.IsHexNumber('cafe'),         'cafe');
  Check(    P.IsHexNumber('CaFe'),         'CaFe');
  Check(    P.IsHexNumber('cafef00dbabe'), 'cafef00dbabe');
end;

procedure TestTIdSimpleParser.TestIsIpv4Address;
begin
  Check(not TIdSimpleParser.IsIPv4Address(''),                '''''');
  Check(not TIdSimpleParser.IsIPv4Address('1'),               '1');
  Check(not TIdSimpleParser.IsIPv4Address('abcd'),            'abcd');
  Check(not TIdSimpleParser.IsIPv4Address('224.'),            '224.');
  Check(not TIdSimpleParser.IsIPv4Address('-1.0.0.0'),        '-1.0.0.0');
  Check(not TIdSimpleParser.IsIPv4Address('224.255.255.256'), '224.255.255.256');
  Check(    TIdSimpleParser.IsIPv4Address('127.2.17.12'),     '127.2.17.12');
  Check(    TIdSimpleParser.IsIPv4Address('224.2.17.12'),     '224.2.17.12');
  Check(    TIdSimpleParser.IsIPv4Address('0.0.0.0'),         '0.0.0.0');
  Check(    TIdSimpleParser.IsIPv4Address('224.0.0.0'),       '224.0.0.0');
  Check(    TIdSimpleParser.IsIPv4Address('224.255.255.255'), '224.255.255.255');
  Check(    TIdSimpleParser.IsIPv4Address('255.255.255.255'), '255.255.255.255');
end;

procedure TestTIdSimpleParser.TestIsIpv6Address;
begin
  Check(not TIdSimpleParser.IsIPv6Address(''),                           '''''');
  Check(not TIdSimpleParser.IsIPv6Address('x'),                          'x');
  Check(not TIdSimpleParser.IsIPv6Address('ffef1::'),                    'ffef1::');
  Check(    TIdSimpleParser.IsIPv6Address('::'),                         '::');
  Check(    TIdSimpleParser.IsIPv6Address('::1'),                        '::1');
  Check(    TIdSimpleParser.IsIPv6Address('1::'),                        '1::');
  Check(    TIdSimpleParser.IsIPv6Address('2002:c058:6301::'),           '2002:c058:6301::');
  Check(    TIdSimpleParser.IsIPv6Address('00:01:02:f0:90:84'),          '00:01:02:f0:90:84');
  Check(    TIdSimpleParser.IsIPv6Address('fe80::201:2ff:fef0'),         'fe80::201:2ff:fef0');
  Check(    TIdSimpleParser.IsIPv6Address('1080:0:0:0:8:800:200C:417A'), '1080:0:0:0:8:800:200C:417A');
  Check(    TIdSimpleParser.IsIPv6Address('1080:0:0:0:8:800:1.2.3.4'),   '1080:0:0:0:8:800:1.2.3.4');
  Check(    TIdSimpleParser.IsIPv6Address('::13.1.68.3'),                '::13.1.68.3');
  Check(    TIdSimpleParser.IsIPv6Address('::FFFF:129.144.52.38'),       '::FFFF:129.144.52.38');
end;

procedure TestTIdSimpleParser.TestIsLetter;
var
  C: Char;
begin
  Check(not P.IsLetter('_'), '_');
  Check(not P.IsLetter('1'), '1');

  for C := Low(Char) to High(Char) do
    if (C in ['a'..'z', 'A'..'Z']) then
      Check(P.IsLetter(C), C)
    else
      Check(not P.IsLetter(C), C)
end;

procedure TestTIdSimpleParser.TestIsNumber;
begin
  Check(not P.IsNumber(''),                     '''''');
  Check(not P.IsNumber('a'),                    'a');
  Check(not P.IsNumber(#0),                     '#0');
  Check(    P.IsNumber('13'),                   '13');
  Check(    P.IsNumber('98765432109876543210'), '98765432109876543210');
end;

procedure TestTIdSimpleParser.TestPeek;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    Self.P.Source := Str;

    CheckEquals('I', Self.P.Peek, 'Peek 1st line');
    Self.P.ReadLn;
    CheckEquals('V', Self.P.Peek, 'Peek 2nd line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestPeekLine;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    Self.P.Source := Str;

    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0', Self.P.PeekLine, 'PeekLine 1st line');
    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0', Self.P.PeekLine, 'PeekLine 1st line, 2nd time');
    Self.P.ReadLn;
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds', Self.P.PeekLine, 'PeekLine 2nd line');
    Self.P.ReadLn;
    CheckEquals('', Self.P.PeekLine, 'PeekLine past the EOF');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestReadOctet;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    Self.P.Source := Str;

    CheckEquals('I', Self.P.ReadOctet, '1st ReadOctet');
    CheckEquals('N', Self.P.ReadOctet, '2nd ReadOctet');
    CheckEquals('V', Self.P.ReadOctet, '3rd ReadOctet');
    CheckEquals('I', Self.P.ReadOctet, '4th ReadOctet');
    CheckEquals('T', Self.P.ReadOctet, '5th ReadOctet');
    CheckEquals('E', Self.P.ReadOctet, '6th ReadOctet');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestReadOctets;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    Self.P.Source := Str;

    CheckEquals('',       Self.P.ReadOctets(0), '0th ReadOctets(0)');
    CheckEquals('I',      Self.P.ReadOctets(1), '1st ReadOctets(1)');
    CheckEquals('NVIT',   Self.P.ReadOctets(4), '2nd ReadOctets(4)');
    CheckEquals('E sip:', Self.P.ReadOctets(6), '3rd ReadOctets(6)');
    CheckEquals('wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
              + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10,
                Self.P.ReadOctets(1000), 'ReadOctets(1000)');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestReadln;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    Self.P.Source := Str;

    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0',     Self.P.ReadLn, '1st ReadLn');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds', Self.P.ReadLn, '2nd ReadLn');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestReadlnDoubleCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('one'#13#10#13#10'three');
  try
    Self.P.Source := Str;
    CheckEquals('one',   Self.P.ReadLn, '1st line');
    CheckEquals('',      Self.P.ReadLn, '2nd line');
    CheckEquals('three', Self.P.ReadLn, '3rd line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestReadlnWithNoCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('new');
  try
    Self.P.Source := Str;
    CheckEquals('new', Self.P.ReadLn, 'ReadLn');
  finally
    Str.Free;
  end;
end;

initialization

  RegisterTest('IdSimpleParser', Suite);
end.
