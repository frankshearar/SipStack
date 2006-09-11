{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSimpleParser;

interface

uses
  IdSimpleParser, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestEncodeNonLineUnprintableChars;
    procedure TestHexDigitToInt;
    procedure TestHexToInt;
    procedure TestWithoutFirstAndLastChars;
  end;

  TestTIdIPAddressParser = class(TTestCase)
  private
    procedure CheckIncIPv6Address(Expected: String;
                                  Address: String;
                                  Increment: Cardinal = 1);
    procedure Clear(var Address: TIdIPv6AddressRec);
  published
    procedure TestExpandIPv6Address;
    procedure TestIncIPAddress;
    procedure TestIncIPv4Address;
    procedure TestIncIPv6Address;
    procedure TestIPv6AddressToStr;
    procedure TestIsIpv4Address;
    procedure TestIsIpv6Address;
    procedure TestIsIpv6Reference;
    procedure TestIsNumericAddress;
    procedure TestParseIpv6Address;
  end;

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
    procedure TestIsLetter;
    procedure TestIsNumber;
    procedure TestPeek;
    procedure TestPeekLine;
    procedure TestParseFailuresRaiseParserErrors;
    procedure TestReadOctet;
    procedure TestReadOctets;
    procedure TestReadln;
    procedure TestReadlnDoubleCrLf;
    procedure TestReadlnWithNoCrLf;
    procedure TestSkipBlankLinesDoubleCrLf;
    procedure TestSkipBlankLinesLeadingCrLf;
    procedure TestSkipBlankLinesWithNoCrLf;
  end;

implementation

uses
  Classes, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSimpleParser unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdIPAddressParser.Suite);
  Result.AddTest(TestTIdSimpleParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.TestEncodeNonLineUnprintableChars;
begin
  CheckEquals('',
              EncodeNonLineUnprintableChars(''),
              'Empty string');
  CheckEquals('abc',
              EncodeNonLineUnprintableChars('abc'),
              '''abc''');
  CheckEquals('#00',
              EncodeNonLineUnprintableChars(#0),
              '#0');
  CheckEquals('#0',
              EncodeNonLineUnprintableChars('#0'),
              '''#0''');
  CheckEquals('a'#13#10'b',
              EncodeNonLineUnprintableChars('a'#13#10'b'),
              '''a''#13#10''b''');

  CheckEquals('a#00'#13#10'b',
              EncodeNonLineUnprintableChars('a'#00#13#10'b'),
              '''a''#00#13#10''b''');

  // If you send a DEL character as part of a string, a Window control will
  // display it as an unprintable character - one of those ugly black blocks.
  // We check the special case of a DEL here:
  CheckEquals('abc#7F#7F#7F',
              EncodeNonLineUnprintableChars('abc'#$7f#$7f#$7f),
              '''abc''#$7f#$7f#$7f');
end;

procedure TestFunctions.TestHexDigitToInt;
var
  C: Char;
begin
  CheckEquals(0,  HexDigitToInt('0'), '0');
  CheckEquals(1,  HexDigitToInt('1'), '1');
  CheckEquals(2,  HexDigitToInt('2'), '2');
  CheckEquals(3,  HexDigitToInt('3'), '3');
  CheckEquals(4,  HexDigitToInt('4'), '4');
  CheckEquals(5,  HexDigitToInt('5'), '5');
  CheckEquals(6,  HexDigitToInt('6'), '6');
  CheckEquals(7,  HexDigitToInt('7'), '7');
  CheckEquals(8,  HexDigitToInt('8'), '8');
  CheckEquals(9,  HexDigitToInt('9'), '9');
  CheckEquals(10, HexDigitToInt('a'), 'a');
  CheckEquals(10, HexDigitToInt('A'), 'A');
  CheckEquals(11, HexDigitToInt('b'), 'b');
  CheckEquals(11, HexDigitToInt('B'), 'B');
  CheckEquals(12, HexDigitToInt('c'), 'c');
  CheckEquals(12, HexDigitToInt('C'), 'C');
  CheckEquals(13, HexDigitToInt('d'), 'd');
  CheckEquals(13, HexDigitToInt('D'), 'D');
  CheckEquals(14, HexDigitToInt('e'), 'e');
  CheckEquals(14, HexDigitToInt('E'), 'E');
  CheckEquals(15, HexDigitToInt('f'), 'f');
  CheckEquals(15, HexDigitToInt('F'), 'F');

  for C := Low(Char) to High(Char) do
    if not TIdSimpleParser.IsHexNumber(C) then begin
      try
        HexDigitToInt(C);
        Fail('Failed to bail out on non-hex digit with Ord ' + IntToStr(Ord(C)));
      except
        on EConvertError do
      end;
    end;
end;

procedure TestFunctions.TestHexToInt;
begin
  CheckEquals($00,       HexToInt('0'),        '0');
  CheckEquals($0f,       HexToInt('f'),        'f');
  CheckEquals($0f,       HexToInt('F'),        'F');
  CheckEquals($ff,       HexToInt('ff'),       'ff');
  CheckEquals($fad,      HexToInt('fad'),      'fad');
  CheckEquals($1234,     HexToInt('1234'),     '1234');
  CheckEquals(Integer($decafbad),
              Integer(HexToInt('dEcAfBaD')),
              'dEcAfBaD');

  try
    HexToInt('');
    Fail('Failed to bail out on ''''');
  except
    on EConvertError do;
  end;

  try
    HexToInt('food');
    Fail('Failed to bail out on ''food''');
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestWithoutFirstAndLastChars;
begin
  CheckEquals('bc', WithoutFirstAndLastChars('abcd'), 'abcd');
  CheckEquals('',   WithoutFirstAndLastChars('ab'), 'ab');
  CheckEquals('',   WithoutFirstAndLastChars('a'), 'a');
  CheckEquals('',   WithoutFirstAndLastChars(''), '''''');
end;

//******************************************************************************
//* TestTIdIPAddressParser                                                     *
//******************************************************************************
//* TestTIdIPAddressParser Public methods **************************************

procedure TestTIdIPAddressParser.TestExpandIPv6Address;
begin
  CheckEquals('2001:200:0:8002:203:47FF:FEA5:3085',
              TIdIPAddressParser.ExpandIPv6Address('2001:200:0:8002:203:47FF:FEA5:3085'),
              '2001:200:0:8002:203:47FF:FEA5:3085');
  CheckEquals('0:0:0:0:0:0:0:0',
              TIdIPAddressParser.ExpandIPv6Address('::'),
              '::');
  CheckEquals('1:0:0:0:0:0:0:2',
              TIdIPAddressParser.ExpandIPv6Address('1::2'),
              '1::2');
  CheckEquals('1:0:0:0:0:0:0:2',
              TIdIPAddressParser.ExpandIPv6Address('1:000::2'),
              '1:000::2');
  try
     TIdIPAddressParser.ExpandIPv6Address('not-an-address');
     Fail('Failed to bail out on ''not-an-address''');
  except
    on EConvertError do;
  end;
end;

procedure TestTIdIPAddressParser.TestIncIPAddress;
begin
  CheckEquals('0.0.0.1',
              TIdIPAddressParser.IncIPAddress('0.0.0.0'),
              '0.0.0.0');
  CheckEquals('0:0:0:0:0:0:0:1',
              TIdIPAddressParser.IncIPAddress('0:0:0:0:0:0:0:0'),
              '0:0:0:0:0:0:0:0');
end;

procedure TestTIdIPAddressParser.TestIncIPv4Address;
begin
  CheckEquals('0.0.0.1', TIdIPAddressParser.IncIPv4Address('0.0.0.0'),         '0.0.0.0');
  CheckEquals('0.0.1.0', TIdIPAddressParser.IncIPv4Address('0.0.0.255'),       '0.0.0.255');
  CheckEquals('0.1.0.0', TIdIPAddressParser.IncIPv4Address('0.0.255.255'),     '0.0.255.255');
  CheckEquals('1.0.0.0', TIdIPAddressParser.IncIPv4Address('0.255.255.255'),   '0.255.255.255');
  CheckEquals('0.0.0.0', TIdIPAddressParser.IncIPv4Address('255.255.255.255'), '255.255.255.255');

  CheckEquals('0.0.0.25',
              TIdIPAddressParser.IncIPv4Address('0.0.0.0', 25),
              '0.0.0.0, 25');

  CheckEquals('0.0.0.25',
              TIdIPAddressParser.IncIPv4Address('255.255.255.255', 26),
              '255.255.255.255, 26');
  CheckEquals('0.0.0.4',
              TIdIPAddressParser.IncIPv4Address('255.255.255.230', 30),
              '255.255.255.230, 26');

  try
    TIdIPAddressParser.IncIPv4Address('');
    Fail('Failed to bail out on ''''');
  except
    on EConvertError do;
  end;

  try
    TIdIPAddressParser.IncIPv4Address('1.2.3.a');
    Fail('Failed to bail out on ''1.2.3.a''');
  except
    on EConvertError do;
  end;
end;

procedure TestTIdIPAddressParser.TestIncIPv6Address;
var
  Addy: TIdIPv6AddressRec;
begin
  CheckIncIPv6Address('0:0:0:0:0:0:0:1',
                      '0:0:0:0:0:0:0:0');
  CheckIncIPv6Address('0:0:0:0:0:0:1:0',
                      '0:0:0:0:0:0:0:FFFF');
  CheckIncIPv6Address('0:0:0:0:0:1:0:0',
                      '0:0:0:0:0:0:FFFF:FFFF');
  CheckIncIPv6Address('0:0:0:0:1:0:0:0',
                      '0:0:0:0:0:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:0:0:1:0:0:0:0',
                      '0:0:0:0:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:0:1:0:0:0:0:0',
                      '0:0:0:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:1:0:0:0:0:0:0',
                      '0:0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('1:0:0:0:0:0:0:0',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:0:0:0:0:0:0:0',
                      'FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');

  CheckIncIPv6Address('0:0:0:0:DEAD:BEEF:FFFF:FFFF',
                      '0:0:0:0:DEAD:BEEF:0:0',
                      High(Cardinal));
  CheckIncIPv6Address('1:0:0:0:0:0:0:0',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');

  TIdIPAddressParser.ParseIPv6Address('0:0:0:0:dead:beee:0:0', Addy);
  TIdIPAddressParser.IncIPv6Address(Addy, High(Cardinal));
  TIdIPAddressParser.IncIPv6Address(Addy, 1);
  CheckEquals('0:0:0:0:DEAD:BEEF:0:0',
              TIdIPAddressParser.IPv6AddressToStr(Addy),
              '0:0:0:0:DEAD:BEEF:0:0, High(Cardinal) + 1');

  CheckIncIPv6Address('1:0:0:0:0:0:FFFF:FFFE',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF',
                      High(Cardinal));
end;

procedure TestTIdIPAddressParser.TestIPv6AddressToStr;
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  FillChar(Address, Sizeof(Address), 0);
  CheckEquals('0:0:0:0:0:0:0:0', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := I + 1;
  CheckEquals('1:2:3:4:5:6:7:8', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := 8 + I;
  CheckEquals('8:9:A:B:C:D:E:F', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := (I + 1) shl 4;
  CheckEquals('10:20:30:40:50:60:70:80', TIdIPAddressParser.IPv6AddressToStr(Address));
end;

procedure TestTIdIPAddressParser.TestIsIpv4Address;
begin
  Check(not TIdIPAddressParser.IsIPv4Address(''),                '''''');
  Check(not TIdIPAddressParser.IsIPv4Address('1'),               '1');
  Check(not TIdIPAddressParser.IsIPv4Address('abcd'),            'abcd');
  Check(not TIdIPAddressParser.IsIPv4Address('224.'),            '224.');
  Check(not TIdIPAddressParser.IsIPv4Address('-1.0.0.0'),        '-1.0.0.0');
  Check(not TIdIPAddressParser.IsIPv4Address('224.255.255.256'), '224.255.255.256');
  Check(    TIdIPAddressParser.IsIPv4Address('127.2.17.12'),     '127.2.17.12');
  Check(    TIdIPAddressParser.IsIPv4Address('224.2.17.12'),     '224.2.17.12');
  Check(    TIdIPAddressParser.IsIPv4Address('0.0.0.0'),         '0.0.0.0');
  Check(    TIdIPAddressParser.IsIPv4Address('224.0.0.0'),       '224.0.0.0');
  Check(    TIdIPAddressParser.IsIPv4Address('224.255.255.255'), '224.255.255.255');
  Check(    TIdIPAddressParser.IsIPv4Address('255.255.255.255'), '255.255.255.255');
end;

procedure TestTIdIPAddressParser.TestIsIpv6Address;
begin
  Check(not TIdIPAddressParser.IsIPv6Address(''),                           '''''');
  Check(not TIdIPAddressParser.IsIPv6Address('x'),                          'x');
  Check(not TIdIPAddressParser.IsIPv6Address('ffef1::'),                    'ffef1::');
  Check(    TIdIPAddressParser.IsIPv6Address('::'),                         '::');
  Check(    TIdIPAddressParser.IsIPv6Address('::1'),                        '::1');
  Check(    TIdIPAddressParser.IsIPv6Address('1::'),                        '1::');
  Check(    TIdIPAddressParser.IsIPv6Address('2002:C058:6301::'),           '2002:C058:6301::');
  Check(    TIdIPAddressParser.IsIPv6Address('00:01:02:f0:90:84'),          '00:01:02:f0:90:84');
  Check(    TIdIPAddressParser.IsIPv6Address('FE80::201:2FF:FEF0'),         'FE80::201:2FF:FEF0');
  Check(    TIdIPAddressParser.IsIPv6Address('1080:0:0:0:8:800:200C:417A'), '1080:0:0:0:8:800:200C:417A');

  // IPv4 stuff
  Check(not TIdIPAddressParser.IsIPv6Address('127.0.0.1'),                '127.0.0.1');
  Check(not TIdIPAddressParser.IsIPv6Address('256.0.0.0'),                '256.0.0.0');
  Check(    TIdIPAddressParser.IsIPv6Address('1080:0:0:0:8:800:1.2.3.4'), '1080:0:0:0:8:800:1.2.3.4');
  Check(    TIdIPAddressParser.IsIPv6Address('::13.1.68.3'),              '::13.1.68.3');
  Check(    TIdIPAddressParser.IsIPv6Address('::FFFF:129.144.52.38'),     '::FFFF:129.144.52.38');
  Check(    TIdIPAddressParser.IsIPv6Address('1::1:129.144.52.38'),       '1::1:129.144.52.38');
end;

procedure TestTIdIPAddressParser.TestIsIpv6Reference;
begin
  Check(not TIdIPAddressParser.IsIPv6Reference(''),                           '''''');
  Check(not TIdIPAddressParser.IsIPv6Reference('x'),                          'x');
  Check(    TIdIPAddressParser.IsIPv6Reference('[::]'),                       '[::]');
  Check(    TIdIPAddressParser.IsIPv6Reference('[1::1:129.144.52.38]'),       '[1::1:129.144.52.38]');
end;

procedure TestTIdIPAddressParser.TestIsNumericAddress;
begin
  Check(not TIdIPAddressParser.IsNumericAddress(''),                     '''''');
  Check(not TIdIPAddressParser.IsNumericAddress('x'),                    'x');
  Check(    TIdIPAddressParser.IsNumericAddress('127.0.0.1'),            '127.0.0.1');
  Check(    TIdIPAddressParser.IsNumericAddress('[1::1:129.144.52.38]'), '[1::1:129.144.52.38]');
  Check(    TIdIPAddressParser.IsNumericAddress('1::1:129.144.52.38'),   '1::1:129.144.52.38');
end;

procedure TestTIdIPAddressParser.TestParseIpv6Address;
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  TIdIPAddressParser.ParseIPv6Address('0:0:0:0:0:0:0:0', Address);
  for I := Low(Address) to High(Address) do
    CheckEquals(0, Address[I], '0:0:0:0:0:0:0:0, Index ' + IntToStr(I));

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('1:2:3:4:5:6:7:8', Address);
  for I := Low(Address) to High(Address) do
    CheckEquals(IntToHex(I + 1, 2),
                IntToHex(Address[I], 2),
                '1:2:3:4:5:6:7:8, Index ' + IntToStr(I));

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('10:20:30:40:50:60:70:80', Address);
  for I := Low(Address) to High(Address) do
    CheckEquals(IntToHex((I + 1) shl 4, 2),
                IntToHex(Address[I], 2),
                '10:20:30:40:50:60:70:80, Index ' + IntToStr(I));

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('::', Address);
  for I := Low(Address) to High(Address) do
    CheckEquals(0, Address[I], '::, Index ' + IntToStr(I));

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('::1', Address);
  for I := Low(Address) to High(Address) - 1 do
    CheckEquals(0, Address[I], '::1, Index ' + IntToStr(I));
  CheckEquals(1, Address[7], '::1, Index 7');

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('1::2', Address);
  CheckEquals(IntToHex($0001, 4),
              IntToHex(Address[Low(Address)], 4),
              '1::2, Index 0');
  for I := Low(Address) + 1 to High(Address) - 1 do
    CheckEquals(0, Address[I], '1::2, Index ' + IntToStr(I));
  CheckEquals(IntToHex($0002, 4),
              IntToHex(Address[High(Address)], 4),
              '1::2, Index 7');

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('2002:5156:4019:1::1', Address);
  CheckEquals(IntToHex($2002, 4),
              IntToHex(Address[0], 4),
              '2002:5156:4019:1::1, Index 0');
  CheckEquals(IntToHex($5156, 4),
              IntToHex(Address[1], 4),
              '2002:5156:4019:1::1, Index 1');
  CheckEquals(IntToHex($4019, 4),
              IntToHex(Address[2], 4),
              '2002:5156:4019:1::1, Index 2');
  CheckEquals(IntToHex($0001, 4),
              IntToHex(Address[3], 4),
              '2002:5156:4019:1::1, Index 3');
  CheckEquals(IntToHex($0000, 4),
              IntToHex(Address[4], 4),
              '2002:5156:4019:1::1, Index 4');
  CheckEquals(IntToHex($0000, 4),
              IntToHex(Address[5], 4),
              '2002:5156:4019:1::1, Index 5');
  CheckEquals(IntToHex($0000, 4),
              IntToHex(Address[6], 4),
              '2002:5156:4019:1::1, Index 6');
  CheckEquals(IntToHex($0001, 4),
              IntToHex(Address[7], 4),
              '2002:5156:4019:1::1, Index 7');

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('2002:5156::4019:2:1', Address);
  CheckEquals(IntToHex($2002, 4),
              IntToHex(Address[0], 4),
              '2002:5156::4019:2:1, Index 0');
  CheckEquals(IntToHex($5156, 4),
              IntToHex(Address[1], 4),
              '2002:5156::4019:2:1, Index 1');
  for I := 2 to 4 do
    CheckEquals(IntToHex($0, 4),
                IntToHex(Address[I], 4),
                '2002:5156::4019:2:1, Index ' + IntToStr(I));
  CheckEquals(IntToHex($2, 4),
              IntToHex(Address[6], 4),
              '2002:5156::4019:2:1, Index 6');
  CheckEquals(IntToHex($1, 4),
              IntToHex(Address[7], 4),
              '2002:5156::4019:2:1, Index 7');


  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1', Address);
  CheckEquals(IntToHex($2002, 4),
              IntToHex(Address[0], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 0');
  CheckEquals(IntToHex($dead, 4),
              IntToHex(Address[1], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 1');
  CheckEquals(IntToHex($beef, 4),
              IntToHex(Address[2], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 2');
  CheckEquals(IntToHex($feed, 4),
              IntToHex(Address[3], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 3');
  CheckEquals(IntToHex($babe, 4),
              IntToHex(Address[4], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 4');
  CheckEquals(IntToHex($f00d, 4),
              IntToHex(Address[5], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 5');
  CheckEquals(IntToHex($c0a8, 4),
              IntToHex(Address[6], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 6');
  CheckEquals(IntToHex($0001, 4),
              IntToHex(Address[7], 4),
              '2002:DEAD:BEEF:FEED:BABE:F00D:192.168.0.1, Index 7');

  Self.Clear(Address);
  TIdIPAddressParser.ParseIPv6Address('2002:dead:beef:feed:babe:f00d:192.168.0.1', Address);
  CheckEquals(IntToHex($2002, 4),
              IntToHex(Address[0], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 0');
  CheckEquals(IntToHex($dead, 4),
              IntToHex(Address[1], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 1');
  CheckEquals(IntToHex($beef, 4),
              IntToHex(Address[2], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 2');
  CheckEquals(IntToHex($feed, 4),
              IntToHex(Address[3], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 3');
  CheckEquals(IntToHex($babe, 4),
              IntToHex(Address[4], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 4');
  CheckEquals(IntToHex($f00d, 4),
              IntToHex(Address[5], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 5');
  CheckEquals(IntToHex($c0a8, 4),
              IntToHex(Address[6], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 6');
  CheckEquals(IntToHex($0001, 4),
              IntToHex(Address[7], 4),
              '2002:dead:beef:feed:babe:f00d:192.168.0.1, Index 7');

  try
    TIdIPAddressParser.ParseIPv6Address('', Address);
    Fail('Failed to raise exception on ''''');
  except
    on EConvertError do;
  end;

  try
    TIdIPAddressParser.ParseIPv6Address('1::2::3', Address);

    Fail('Failed to raise exception on ''1::2::3');
  except
    on EConvertError do;
  end;
end;

//* TestTIdIPAddressParser Private methods *************************************

procedure TestTIdIPAddressParser.CheckIncIPv6Address(Expected: String;
                                                     Address: String;
                                                     Increment: Cardinal = 1);
var
  Addy: TIdIPv6AddressRec;
begin
  TIdIPAddressParser.ParseIPv6Address(Address, Addy);
  TIdIPAddressParser.IncIPv6Address(Addy, Increment);

  CheckEquals(Expected, TIdIPAddressParser.IPv6AddressToStr(Addy), Address);
end;

procedure TestTIdIPAddressParser.Clear(var Address: TIdIPv6AddressRec);
var
  I: Integer;
begin
  for I := Low(Address) to High(Address) do
    Address[I] := 0;
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
  Check(not Self.P.IsAlphaNumeric(''),           '''''');
  Check(not Self.P.IsAlphaNumeric(#0),           '#0');
  Check(not Self.P.IsAlphaNumeric(#13),          '#13');
  Check(not Self.P.IsAlphaNumeric(#$FF),         '#$FF');
  Check(    Self.P.IsAlphaNumeric('a'),          'a');
  Check(not Self.P.IsAlphaNumeric('a'#13#10'b'), 'a#13#10b');
  Check(    Self.P.IsAlphaNumeric('a1b2c3d4'),   'a1b2c3d4');

  S := '';
  for I := 1 to 1000000 do
    S := S + 'a';
  Check(Self.P.IsAlphaNumeric(S), '1 000 000 a''s');
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
    Check(Self.P.IsNumber(C), C);

  for C := Chr(0) to Chr(Ord('0') - 1) do
    Check(not Self.P.IsNumber(C), C);

  for C := Chr(Ord('9') + 1) to Chr(255) do
    Check(not Self.P.IsNumber(C), C);
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
      Check(Self.P.IsHexNumber(C), C)
    else
      Check(not Self.P.IsHexNumber(C), C);

  Check(not Self.P.IsHexNumber(''),             '''''');
  Check(not Self.P.IsHexNumber('x'),            'x');
  Check(not Self.P.IsHexNumber('cafex'),        'cafex');
  Check(    Self.P.IsHexNumber('cafe'),         'cafe');
  Check(    Self.P.IsHexNumber('CaFe'),         'CaFe');
  Check(    Self.P.IsHexNumber('caFEF00dbabe'), 'caFEF00dbabe');
end;

procedure TestTIdSimpleParser.TestIsLetter;
var
  C: Char;
begin
  Check(not Self.P.IsLetter('_'), '_');
  Check(not Self.P.IsLetter('1'), '1');

  for C := Low(Char) to High(Char) do
    if (C in ['a'..'z', 'A'..'Z']) then
      Check(Self.P.IsLetter(C), C)
    else
      Check(not Self.P.IsLetter(C), C)
end;

procedure TestTIdSimpleParser.TestIsNumber;
begin
  Check(not Self.P.IsNumber(''),                     '''''');
  Check(not Self.P.IsNumber('a'),                    'a');
  Check(not Self.P.IsNumber(#0),                     '#0');
  Check(    Self.P.IsNumber('13'),                   '13');
  Check(    Self.P.IsNumber('98765432109876543210'), '98765432109876543210');
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

procedure TestTIdSimpleParser.TestParseFailuresRaiseParserErrors;
const
  MalformedLineEnd = 'foo'#$D;
var
  Line: String;
  Str:  TStringStream;
begin
  Str := TStringStream.Create(MalformedLineEnd);
  try
    Self.P.Source := Str;

    try
      Line := Self.P.ReadLn;
      Fail('Failed to raise exception reading a malformed line end. Read "' + Line + '"');
    except
      on EParserError do;
    end;
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

procedure TestTIdSimpleParser.TestSkipBlankLinesDoubleCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('one'#13#10#13#10'three');
  try
    Self.P.Source := Str;

    Self.P.SkipBlankLines;

    CheckEquals('one',   Self.P.ReadLn, '1st line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestSkipBlankLinesLeadingCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(#13#10#13#10'one'#13#10'three');
  try
    Self.P.Source := Str;

    Self.P.SkipBlankLines;

    CheckEquals('one',   Self.P.ReadLn, '1st line');
    CheckEquals('three', Self.P.ReadLn, '2nd line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSimpleParser.TestSkipBlankLinesWithNoCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('new');
  try
    Self.P.Source := Str;

    Self.P.SkipBlankLines;

    CheckEquals('new', Self.P.ReadLn, 'ReadLn');
  finally
    Str.Free;
  end;
end;

initialization
  RegisterTest('IdSimpleParser', Suite);
end.
