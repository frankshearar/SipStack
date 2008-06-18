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
    procedure TestEncodeQuotedStr;
    procedure TestEndsWith;
    procedure TestFetch;
    procedure TestFetchDefaultParameters;
    procedure TestFetchDelimiterNotPresent;
    procedure TestFetchDelimiterTheEmptyString;
    procedure TestFetchDontDelete;
    procedure TestFetchEmptyString;
    procedure TestFetchMulticharDelimiter;
    procedure TestFetchMultipleDelimiters;
    procedure TestFetchNullCharDelimiter;
    procedure TestFirstOfCharSet;
    procedure TestFirstOfString;
    procedure TestHexDigitToInt;
    procedure TestHexToInt;
    procedure TestLastPos;
    procedure TestStartsWith;
    procedure TestStripLeadingZeroes;
    procedure TestWithoutFirstAndLastChars;
  end;

  TestTIdIPAddressParser = class(TTestCase)
  private
    procedure CheckIncIPv6Address(Expected: String;
                                  Address: String;
                                  Increment: Cardinal = 1);
    procedure Clear(var Address: TIdIPv6AddressRec);
  published
    procedure TestAddressToMask;
    procedure TestExpandIPv6Address;
    procedure TestIncIPAddress;
    procedure TestIncIPv4Address;
    procedure TestIncIPv6Address;
    procedure TestInetAddr;
    procedure TestIPv4AddressToStr;
    procedure TestIPv6AddressEquivalence;
    procedure TestIPv6AddressToStr;
    procedure TestIPVersion;
    procedure TestIsIPAddress;
    procedure TestIsIpv4Address;
    procedure TestIsIpv6Address;
    procedure TestIsIpv6Reference;
    procedure TestIsNumericAddress;
    procedure TestMaskToAddress;
    procedure TestNetworkFor;
    procedure TestNetworkForNumBits;
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

procedure TestFunctions.TestEncodeQuotedStr;
begin
  CheckEquals('I am a ''normal'' string',
              EncodeQuotedStr('I am a ''normal'' string'),
              '''I am a ''''normal'''' string''');
  CheckEquals('',
              EncodeQuotedStr(''),
              '''''');
  CheckEquals('\\',
              EncodeQuotedStr('\'),
              '\');
  CheckEquals('\"',
              EncodeQuotedStr('"'),
              '"');
  CheckEquals('\\\"',
              EncodeQuotedStr('\"'),
              '\"');
  CheckEquals('\"I am a ''normal'' string\"',
              EncodeQuotedStr('"I am a ''normal'' string"'),
              '''"I am a ''normal'' string"''');
end;

procedure TestFunctions.TestEndsWith;
begin
  Check(EndsWith('', ''), 'The empty string ends with the empty string');
  Check(EndsWith('foo', ''), 'All strings end with the empty string');
  Check(EndsWith('foo', 'foo'), 'All strings end with themselves');
  Check(not EndsWith('', 'foo'), 'The empty string does not end with a non-empty string');

  Check(not EndsWith('foo', 'barbar'), 'No string ends with a string longer than itself');

  Check(EndsWith('foo', 'o'), '"foo" ends with "o"');
  Check(EndsWith('foo', 'oo'), '"foo" ends with "oo"');

  Check(not EndsWith('foo', 'bar'), '"foo" does not end with "bar"');
  Check(not EndsWith('foo', 'foobar'), '"foo" does not end with "foobar"');
  Check(not EndsWith('foo', 'fo'), '"foo" does not end with "fo"');
  Check(not EndsWith('foo', 'foobar'), '"foo" does not end with "foobar"');
  Check(not EndsWith('foo', 'O'), '"foo" does not end with "O"');
end;

procedure TestFunctions.TestFetch;
const
  FirstWord  = 'abc';
  SecondWord = 'def';
  ThirdWord  = 'ghi';
var
  Source: String;
  Token:  String;
begin
  Source := FirstWord + ' ' + SecondWord + ' ' + ThirdWord;

  Token := Fetch(Source, ' ', true);
  CheckEquals(FirstWord, Token, 'First token');

  Token := Fetch(Source, ' ', true);
  CheckEquals(SecondWord, Token, 'Second token');

  Token := Fetch(Source, ' ', true);
  CheckEquals(ThirdWord, Token, 'Third token');

  CheckEquals('', Source, 'Still data left in Source');
end;

procedure TestFunctions.TestFetchDefaultParameters;
const
  FirstWord  = 'abc';
  SecondWord = 'def';
var
  Source: String;
  Token:  String;
begin
  Source := FirstWord + ' ' + SecondWord;
  Token := Fetch(Source);

  CheckEquals(FirstWord, Token, 'Delimiter didn''t default to " "');
  CheckEquals(SecondWord, Source, 'Delete didn''t default to true');
end;

procedure TestFunctions.TestFetchDelimiterNotPresent;
const
  NoDelimiterPresent = 'abc def';
var
  Source: String;
  Token:  String;
begin
  Source := NoDelimiterPresent;
  Token := Fetch(Source, '|');
  CheckEquals(NoDelimiterPresent, Token, 'Fetch didn''t fetch all it should have');
  CheckEquals('', Source, 'Data still left in Source');
end;

procedure TestFunctions.TestFetchDelimiterTheEmptyString;
const
  OriginalSource = 'abc def';
var
  Source: String;
  Token:  String;
begin
  Source := OriginalSource;
  Token := Fetch(Source, '');
  CheckEquals(OriginalSource, Token, 'Fetch didn''t fetch all it should have');
  CheckEquals('', Source, 'Data still left in Source');
end;

procedure TestFunctions.TestFetchDontDelete;
const
  FirstToken     = 'abc';
  SecondToken    = 'def';
  OriginalSource = FirstToken + ' ' + SecondToken;
var
  Source: String;
  Token:  String;
begin
  Source := OriginalSource;
  Token := Fetch(Source, ' ', false);
  CheckEquals(FirstToken, Token, 'Fetch didn''t return the first token');
  CheckEquals(OriginalSource, Source, 'Fetch deleted the first token');
end;

procedure TestFunctions.TestFetchEmptyString;
var
  Source: String;
begin
  Source := '';
  CheckEquals('', Fetch(Source, ' '), 'Fetch('''', '' '')');
  CheckEquals('', Source, 'Sanity check; space delimiter');

  Source := '';
  CheckEquals('', Fetch(Source, ''), 'Fetch('''', '''')');
  CheckEquals('', Source, 'Sanity check; empty string delimiter');
end;

procedure TestFunctions.TestFetchMulticharDelimiter;
const
  LineOne       = 'abc';
  LineSeparator = #$D#$A;
  LineTwo       = 'def';
var
  Source: String;
  Token:  String;
begin
  Source := LineOne + LineSeparator + LineTwo;
  Token := Fetch(Source, LineSeparator);

  CheckEquals(LineOne, Token, 'Token not fetched correctly');
  CheckEquals(LineTwo, Source, 'Token and delimiter not removed');
end;

procedure TestFunctions.TestFetchMultipleDelimiters;
const
  LineOne = 'abc';
  LineTwo = 'def';
var
  OriginalSource: String;
  Source:         String;
  Token:          String;
begin
  OriginalSource := LineOne + #$D#$A + LineTwo;

  Source := OriginalSource;
  Token   := Fetch(Source, [#$D, #$A], true);
  CheckEquals(LineOne, Token, 'Token not fetched correctly; Delete');
  CheckEquals(#$A + LineTwo, Source, 'Token and delimiter not removed; Delete');

  Source := OriginalSource;
  Token  := Fetch(Source, [#$A, #$D], true);
  CheckEquals(LineOne, Token, 'Token not fetched correctly; arbitrarily ordered delimiters');
  CheckEquals(#$A + LineTwo, Source, 'Token or delimiter removed; arbitrarily ordered delimiters');

  Source := OriginalSource;
  Token  := Fetch(Source, [#$D, #$A], false);
  CheckEquals(LineOne, Token, 'Token not fetched correctly; not Delete');
  CheckEquals(OriginalSource, Source, 'Token or delimiter removed; not Delete');

  Source := OriginalSource;
  Token  := Fetch(Source, []);
  CheckEquals(OriginalSource, Token, 'Fetch with no delimiters');
  CheckEquals('', Source, 'Fetch didn''t eat the whole string; no delimiters specified');
end;

procedure TestFunctions.TestFetchNullCharDelimiter;
const
  OriginalSource = 'abcdef';
var
  Source: String;
begin
  Source := OriginalSource;
  CheckEquals(OriginalSource, Fetch(Source, #0), 'Fetch with (no) NUL delimiter');
  CheckEquals('', Source, 'Fetch didn''t eat the whole string; no delimiter present');

  Source := #0 + OriginalSource;
  CheckEquals('', Fetch(Source, #0), 'Fetch with leading NUL');
  CheckEquals(OriginalSource, Source, 'Fetch ate more than it should have; leading delimiter');

  Source := 'abc'#0'def';
  CheckEquals('abc', Fetch(Source, #0), 'Fetch with NUL delimiter');
  CheckEquals('def', Source, 'Fetch didn''t alter source properly; NUL delimiter in the middle of the string');
end;

procedure TestFunctions.TestFirstOfCharSet;
begin
  CheckEquals(0, FirstOf([], ''), 'No characters, empty string');
  CheckEquals(0, FirstOf(['a'], ''), '"a", empty string');
  CheckEquals(0, FirstOf(['z'], 'abc'), '"z", in "abc"');
  CheckEquals(0, FirstOf(['x', 'y', 'z'], 'abc'), '"x" or "y" or "z", in "abc"');

  CheckEquals(1, FirstOf(['a'], 'a'), '"a", in "a"');
  CheckEquals(1, FirstOf(['a'], 'abc'), '"a", in "abc"');
  CheckEquals(3, FirstOf(['c'], 'abc'), '"c", in "abc"');
  CheckEquals(1, FirstOf(['a', 'b', 'c'], 'abc'), '"a" or "b" or "c", in "abc"');
end;

procedure TestFunctions.TestFirstOfString;
begin
  CheckEquals(0, FirstOf('', ''), 'No characters, empty string');
  CheckEquals(0, FirstOf('a', ''), '"a", empty string');
  CheckEquals(0, FirstOf('z', 'abc'), '"z", in "abc"');
  CheckEquals(0, FirstOf('xyz', 'abc'), '"x" or "y" or "z", in "abc"');


  CheckEquals(1, FirstOf('a', 'a'), '"a", in "a"');
  CheckEquals(1, FirstOf('a', 'abc'), '"a", in "abc"');
  CheckEquals(3, FirstOf('c', 'abc'), '"c", in "abc"');
  CheckEquals(1, FirstOf('abc', 'abc'), '"a" or "b" or "c", in "abc"');
  CheckEquals(1, FirstOf('cab', 'abc'), '"c" or "a" or "b", in "abc"');
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

procedure TestFunctions.TestLastPos;
var
  Needle, Haystack: String;
begin
  Needle := 'abc';

  CheckEquals(0, LastPos(Needle, Haystack), 'Empty string');

  Haystack := 'xxx';
  CheckEquals(0, LastPos(Needle, Haystack), 'No Needle in the Haystack');

  Haystack := 'abc';
  CheckEquals(1, LastPos(Needle, Haystack), 'Needle = Haystack');
  CheckEquals(1,
              LastPos(Needle, Haystack, Length(Haystack) + 1),
              'Needle = Haystack, starting beyond end of Haystack');
  CheckEquals(1,
              LastPos(Needle, Haystack, -Length(Haystack)),
              'Needle = Haystack, starting before beginning of Haystack');

  Haystack := 'abcdef';
  CheckEquals(1, LastPos(Needle, Haystack), 'Needle at beginning of Haystack');

  Haystack := 'defabc';
  CheckEquals(4, LastPos(Needle, Haystack), 'Needle at end of Haystack');

  Haystack := 'defabcghi';
  CheckEquals(4, LastPos(Needle, Haystack), 'Needle inside Haystack');

  Haystack := 'abcabc';
  CheckEquals(4, LastPos(Needle, Haystack), 'Haystack = 2xNeedle');

  Haystack := 'abcabc';
  CheckEquals(4, LastPos(Needle, Haystack, 4), 'Haystack = 2xNeedle, starting in the middle');

  Haystack := 'abcabcabc';
  CheckEquals(1, LastPos(Needle, Haystack, 3), 'Haystack = 3xNeedle, starting at 3');
  CheckEquals(4, LastPos(Needle, Haystack, 4), 'Haystack = 3xNeedle, starting at 4');
  CheckEquals(4, LastPos(Needle, Haystack, 5), 'Haystack = 3xNeedle, starting at 5');
  CheckEquals(7, LastPos(Needle, Haystack, 7), 'Haystack = 3xNeedle, starting at 7');
  CheckEquals(7, LastPos(Needle, Haystack),    'Haystack = 3xNeedle');
end;

procedure TestFunctions.TestStartsWith;
begin
  Check(StartsWith('', ''), 'The empty string starts with the empty string');
  Check(StartsWith('foo', ''), 'All strings start with the empty string');
  Check(not StartsWith('', 'foo'), 'The empty string doesn''t start with any non-empty string');

  Check(not StartsWith('foo', 'foofoo'), 'No string starts with a string longer than itself');

  Check(StartsWith('foo', 'f'), '"foo" starts with "f"');
  Check(StartsWith('foo', 'fo'), '"foo" starts with "fo"');
  Check(StartsWith('foo', 'foo'), '"foo" starts with "foo"');
  Check(not StartsWith('foo', 'bar'), '"foo" does not start with "bar"');
  Check(not StartsWith('foo', 'o'), '"foo" does not start with "o"');
  Check(not StartsWith('foo', 'F'), '"foo" does not start with "F"');
end;

procedure TestFunctions.TestStripLeadingZeroes;
begin
  CheckEquals('',       StripLeadingZeroes(''),          'The empty string');
  CheckEquals('abcxyz', StripLeadingZeroes('abcxyz'),    'abcxyz');
  CheckEquals('123abc', StripLeadingZeroes('123abc'),    '123abc');
  CheckEquals('1',      StripLeadingZeroes('0001'),      '0001');
  CheckEquals('1000',   StripLeadingZeroes('1000'),      '1000');
  CheckEquals('1010',   StripLeadingZeroes('0001010'),   '0001010');
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

procedure TestTIdIPAddressParser.TestAddressToMask;
begin
  CheckEquals(0,  TIdIPAddressParser.AddressToMask('0.0.0.0', Id_IPv4),         '0.0.0.0');
  CheckEquals(8,  TIdIPAddressParser.AddressToMask('255.0.0.0', Id_IPv4),       '255.0.0.0');
  CheckEquals(7,  TIdIPAddressParser.AddressToMask('254.0.0.0', Id_IPv4),       '254.0.0.0');
  CheckEquals(32, TIdIPAddressParser.AddressToMask('255.255.255.255', Id_IPv4), '255.255.255.255');

  CheckEquals(0,   TIdIPAddressParser.AddressToMask('::', Id_IPv6),          '::');
  CheckEquals(8,   TIdIPAddressParser.AddressToMask('ff00::', Id_IPv6),      'ff00::');
  CheckEquals(32,  TIdIPAddressParser.AddressToMask('ffff:ffff::', Id_IPv6), 'ffff:ffff::');
  CheckEquals(128, TIdIPAddressParser.AddressToMask('FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF', Id_IPv6),
              'FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');

  CheckEquals(0,  TIdIPAddressParser.AddressToMask('0', Id_IPv4),  '0');
  CheckEquals(8,  TIdIPAddressParser.AddressToMask('8', Id_IPv4),  '8');
  CheckEquals(7,  TIdIPAddressParser.AddressToMask('7', Id_IPv4),  '7');
  CheckEquals(32, TIdIPAddressParser.AddressToMask('32', Id_IPv4), '32');

  // Don't do these things: they're here to document fully the behaviour of the
  // function.
  CheckEquals(0, TIdIPAddressParser.AddressToMask('7FFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF', Id_IPv6), '7FFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckEquals(0, TIdIPAddressParser.AddressToMask('127.255.255.255', Id_IPv4), '127.255.255.255');

  try
    TIdIPAddressParser.AddressToMask('', Id_IPv4);
    Fail('Failed to bail out on bad address '''' for an IPv4 address');
  except
    on EBadParameter do;
  end;

  try
    TIdIPAddressParser.AddressToMask(':?', Id_IPv6);
    Fail('Failed to bail out on bad address '''' for an IPv6 address');
  except
    on EBadParameter do;
  end;
end;

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
  CheckEquals('::1',
              TIdIPAddressParser.IncIPAddress('0:0:0:0:0:0:0:0'),
              '0:0:0:0:0:0:0:0');
  CheckEquals('::1',
              TIdIPAddressParser.IncIPAddress('::'),
              '::');
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
  CheckIncIPv6Address('::1',
                      '0:0:0:0:0:0:0:0');
  CheckIncIPv6Address('::1',
                      '::');
  CheckIncIPv6Address('::1:0',
                      '0:0:0:0:0:0:0:FFFF');
  CheckIncIPv6Address('::1:0:0',
                      '0:0:0:0:0:0:FFFF:FFFF');
  CheckIncIPv6Address('::1:0:0:0',
                      '0:0:0:0:0:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:0:0:1::',
                      '0:0:0:0:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:0:1::',
                      '0:0:0:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('0:1::',
                      '0:0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('1::',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');
  CheckIncIPv6Address('::',
                      'FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');

  CheckIncIPv6Address('::DEAD:BEEF:FFFF:FFFF',
                      '0:0:0:0:DEAD:BEEF:0:0',
                      High(Cardinal));
  CheckIncIPv6Address('1::',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF');

  TIdIPAddressParser.ParseIPv6Address('::DEAD:BEEE:0:0', Addy);
  TIdIPAddressParser.IncIPv6Address(Addy, High(Cardinal));
  TIdIPAddressParser.IncIPv6Address(Addy, 1);
  CheckEquals('::DEAD:BEEF:0:0',
              TIdIPAddressParser.IPv6AddressToStr(Addy),
              '::DEAD:BEEF:0:0, High(Cardinal) + 1');

  CheckIncIPv6Address('1::FFFF:FFFE',
                      '0:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF',
                      High(Cardinal));
end;

procedure TestTIdIPAddressParser.TestInetAddr;
const
  MalformedIPv4Address = '256.0.0.0';
var
  Result: Cardinal;
begin
  CheckEquals(IntToHex($00000000, 8), IntToHex(TIdIPAddressParser.InetAddr('0.0.0.0'), 8),         '0.0.0.0');
  CheckEquals(IntToHex($00ffffff, 8), IntToHex(TIdIPAddressParser.InetAddr('0.255.255.255'), 8),   '0.255.255.255');
  CheckEquals(IntToHex($ff00ffff, 8), IntToHex(TIdIPAddressParser.InetAddr('255.0.255.255'), 8),   '255.0.255.255');
  CheckEquals(IntToHex($ffff00ff, 8), IntToHex(TIdIPAddressParser.InetAddr('255.255.0.255'), 8),   '255.255.0.255');
  CheckEquals(IntToHex($ffffff00, 8), IntToHex(TIdIPAddressParser.InetAddr('255.255.255.0'), 8),   '255.255.255.0');
  CheckEquals(IntToHex($ffffffff, 8), IntToHex(TIdIPAddressParser.InetAddr('255.255.255.255'), 8), '255.255.255.255');
  CheckEquals(IntToHex($01020304, 8), IntToHex(TIdIPAddressParser.InetAddr('1.2.3.4'), 8),         '1.2.3.4');

  try
    Result := TIdIPAddressParser.InetAddr(MalformedIPv4Address);
    Fail('Failed to bail on ' + MalformedIPv4Address + '; Result = $' + IntToHex(Result, 8));
  except
    on EConvertError do;
  end;
end;

procedure TestTIdIPAddressParser.TestIPv4AddressToStr;
const
  TestCases: array[1..3] of String
             = ('0.0.0.0',
                '1.2.3.4',
                '255.255.255.255');
var
  I: Integer;
begin
  for I := Low(TestCases) to High(TestCases) do
    CheckEquals(TestCases[I],
                TIdIPAddressParser.IPv4AddressToStr(TIdIPAddressParser.InetAddr(TestCases[I])),
                TestCases[I]);
end;

procedure TestTIdIPAddressParser.TestIPv6AddressEquivalence;
const
  ShortForm = '2001:DB8::1428:57AB';
  Addresses : array [0..5] of String = (
              '2001:0DB8:0000:0000:0000:0000:1428:57AB',
              '2001:0DB8:0000:0000:0000::1428:57AB',
              '2001:0DB8:0:0:0:0:1428:57AB',
              '2001:0DB8:0:0::1428:57AB',
              '2001:0DB8::1428:57AB',
              ShortForm);
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  for I := Low(Addresses) to High(Addresses) do begin
    TIdIPAddressParser.ParseIPv6Address(Addresses[I], Address);
    CheckEquals(ShortForm, TIdIPAddressParser.IPv6AddressToStr(Address), Addresses[I]);
  end;
end;

procedure TestTIdIPAddressParser.TestIPv6AddressToStr;
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  FillChar(Address, Sizeof(Address), 0);
  CheckEquals('::', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := I + 1;
  CheckEquals('1:2:3:4:5:6:7:8', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := 8 + I;
  CheckEquals('8:9:A:B:C:D:E:F', TIdIPAddressParser.IPv6AddressToStr(Address));

  for I := Low(Address) to High(Address) do
    Address[I] := (I + 1) shl 4;
  CheckEquals('10:20:30:40:50:60:70:80', TIdIPAddressParser.IPv6AddressToStr(Address));

  FillChar(Address, Sizeof(Address), 0);
  Address[0] := $2002;
  Address[1] := $deca;
  Address[2] := $fbad;
  Address[7] := 1;
  CheckEquals('2002:DECA:FBAD::1', TIdIPAddressParser.IPv6AddressToStr(Address));

  FillChar(Address, Sizeof(Address), 0);
  Address[6] := $0102;
  Address[7] := $0304;
  CheckEquals('::102:304', TIdIPAddressParser.IPv6AddressToStr(Address), 'No encapsulated IPv4 addresses');
  CheckEquals('::1.2.3.4', TIdIPAddressParser.IPv6AddressToStr(Address, true), 'Show encapsulated IPv4 addresses');
end;

procedure TestTIdIPAddressParser.TestIPVersion;
begin
  Check(Id_IPv4      = TIdIPAddressParser.IPVersion('127.0.0.1'), '127.0.0.1');
  Check(Id_IPv6      = TIdIPAddressParser.IPVersion('::1'),       '::1');
  Check(Id_IPUnknown = TIdIPAddressParser.IPVersion('foo'),       'foo');
end;

procedure TestTIdIPAddressParser.TestIsIPAddress;
begin
  Check(not TIdIPAddressParser.IsIPAddress(''),                '''''');
  Check(not TIdIPAddressParser.IsIPAddress('1'),               '1');
  Check(not TIdIPAddressParser.IsIPAddress('abcd'),            'abcd');
  Check(not TIdIPAddressParser.IsIPAddress('224.'),            '224.');
  Check(not TIdIPAddressParser.IsIPAddress('-1.0.0.0'),        '-1.0.0.0');
  Check(not TIdIPAddressParser.IsIPAddress('224.255.255.256'), '224.255.255.256');
  Check(not TIdIPAddressParser.IsIPAddress('ffef1::'),                    'ffef1::');

  Check(    TIdIPAddressParser.IsIPAddress('1080:0:0:0:8:800:200C:417A'), '1080:0:0:0:8:800:200C:417A');
  Check(    TIdIPAddressParser.IsIPAddress('127.2.17.12'),                '127.2.17.12');
  Check(    TIdIPAddressParser.IsIPAddress('1080:0:0:0:8:800:1.2.3.4'),   '1080:0:0:0:8:800:1.2.3.4');
  Check(    TIdIPAddressParser.IsIPAddress('::13.1.68.3'),                '::13.1.68.3');
  Check(    TIdIPAddressParser.IsIPAddress('::FFFF:129.144.52.38'),       '::FFFF:129.144.52.38');
  Check(    TIdIPAddressParser.IsIPAddress('1::1:129.144.52.38'),         '1::1:129.144.52.38');
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
  Check(not TIdIPAddressParser.IsIPv6Address('1'),                          '1');
  Check(not TIdIPAddressParser.IsIPv6Address('ffff'),                       'ffff');
  Check(not TIdIPAddressParser.IsIPv6Address('10000'),                      '10000');
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

procedure TestTIdIPAddressParser.TestMaskToAddress;
begin
  CheckEquals('0.0.0.0',         TIdIPAddressParser.MaskToAddress(0, Id_IPv4), '0 significant digits, IPv4');
  CheckEquals('128.0.0.0',       TIdIPAddressParser.MaskToAddress(1, Id_IPv4), '1 significant digit, IPv4');
  CheckEquals('192.0.0.0',       TIdIPAddressParser.MaskToAddress(2, Id_IPv4), '2 significant digits, IPv4');
  CheckEquals('224.0.0.0',       TIdIPAddressParser.MaskToAddress(3, Id_IPv4), '3 significant digits, IPv4');
  CheckEquals('240.0.0.0',       TIdIPAddressParser.MaskToAddress(4, Id_IPv4), '4 significant digits, IPv4');
  CheckEquals('248.0.0.0',       TIdIPAddressParser.MaskToAddress(5, Id_IPv4), '5 significant digits, IPv4');
  CheckEquals('252.0.0.0',       TIdIPAddressParser.MaskToAddress(6, Id_IPv4), '6 significant digits, IPv4');
  CheckEquals('254.0.0.0',       TIdIPAddressParser.MaskToAddress(7, Id_IPv4), '7 significant digits, IPv4');
  CheckEquals('255.0.0.0',       TIdIPAddressParser.MaskToAddress(8, Id_IPv4), '8 significant digits, IPv4');
  CheckEquals('255.128.0.0',     TIdIPAddressParser.MaskToAddress(9, Id_IPv4), '9 significant digits, IPv4');
  CheckEquals('255.192.0.0',     TIdIPAddressParser.MaskToAddress(10, Id_IPv4), '10 significant digits, IPv4');
  CheckEquals('255.224.0.0',     TIdIPAddressParser.MaskToAddress(11, Id_IPv4), '11 significant digits, IPv4');
  CheckEquals('255.240.0.0',     TIdIPAddressParser.MaskToAddress(12, Id_IPv4), '12 significant digits, IPv4');
  CheckEquals('255.248.0.0',     TIdIPAddressParser.MaskToAddress(13, Id_IPv4), '13 significant digits, IPv4');
  CheckEquals('255.252.0.0',     TIdIPAddressParser.MaskToAddress(14, Id_IPv4), '14 significant digits, IPv4');
  CheckEquals('255.254.0.0',     TIdIPAddressParser.MaskToAddress(15, Id_IPv4), '15 significant digits, IPv4');
  CheckEquals('255.255.0.0',     TIdIPAddressParser.MaskToAddress(16, Id_IPv4), '16 significant digits, IPv4');
  CheckEquals('255.255.128.0',   TIdIPAddressParser.MaskToAddress(17, Id_IPv4), '17 significant digits, IPv4');
  CheckEquals('255.255.192.0',   TIdIPAddressParser.MaskToAddress(18, Id_IPv4), '18 significant digits, IPv4');
  CheckEquals('255.255.224.0',   TIdIPAddressParser.MaskToAddress(19, Id_IPv4), '19 significant digits, IPv4');
  CheckEquals('255.255.240.0',   TIdIPAddressParser.MaskToAddress(20, Id_IPv4), '20 significant digits, IPv4');
  CheckEquals('255.255.248.0',   TIdIPAddressParser.MaskToAddress(21, Id_IPv4), '21 significant digits, IPv4');
  CheckEquals('255.255.252.0',   TIdIPAddressParser.MaskToAddress(22, Id_IPv4), '22 significant digits, IPv4');
  CheckEquals('255.255.254.0',   TIdIPAddressParser.MaskToAddress(23, Id_IPv4), '23 significant digits, IPv4');
  CheckEquals('255.255.255.0',   TIdIPAddressParser.MaskToAddress(24, Id_IPv4), '24 significant digits, IPv4');
  CheckEquals('255.255.255.128', TIdIPAddressParser.MaskToAddress(25, Id_IPv4), '25 significant digits, IPv4');
  CheckEquals('255.255.255.192', TIdIPAddressParser.MaskToAddress(26, Id_IPv4), '26 significant digits, IPv4');
  CheckEquals('255.255.255.224', TIdIPAddressParser.MaskToAddress(27, Id_IPv4), '27 significant digits, IPv4');
  CheckEquals('255.255.255.240', TIdIPAddressParser.MaskToAddress(28, Id_IPv4), '28 significant digits, IPv4');
  CheckEquals('255.255.255.248', TIdIPAddressParser.MaskToAddress(29, Id_IPv4), '29 significant digits, IPv4');
  CheckEquals('255.255.255.252', TIdIPAddressParser.MaskToAddress(30, Id_IPv4), '30 significant digits, IPv4');
  CheckEquals('255.255.255.254', TIdIPAddressParser.MaskToAddress(31, Id_IPv4), '31 significant digits, IPv4');
  CheckEquals('255.255.255.255', TIdIPAddressParser.MaskToAddress(32, Id_IPv4), '32 significant digits, IPv4');

  CheckEquals('::',     TIdIPAddressParser.MaskToAddress(0, Id_IPv6), '0 significant digits, IPv6');
  CheckEquals('8000::', TIdIPAddressParser.MaskToAddress(1, Id_IPv6), '1 significant digit, IPv6');
  CheckEquals('C000::', TIdIPAddressParser.MaskToAddress(2, Id_IPv6), '2 significant digits, IPv6');
  CheckEquals('E000::', TIdIPAddressParser.MaskToAddress(3, Id_IPv6), '3 significant digits, IPv6');
  CheckEquals('F000::', TIdIPAddressParser.MaskToAddress(4, Id_IPv6), '4 significant digits, IPv6');
  CheckEquals('F800::', TIdIPAddressParser.MaskToAddress(5, Id_IPv6), '5 significant digits, IPv6');
  CheckEquals('FC00::', TIdIPAddressParser.MaskToAddress(6, Id_IPv6), '6 significant digits, IPv6');
  CheckEquals('FE00::', TIdIPAddressParser.MaskToAddress(7, Id_IPv6), '7 significant digits, IPv6');
  CheckEquals('FF00::', TIdIPAddressParser.MaskToAddress(8, Id_IPv6), '8 significant digits, IPv6');

  // Skip a whole bunch
  CheckEquals('FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFE', TIdIPAddressParser.MaskToAddress(127, Id_IPv6), '127 significant digits, IPv6');
  CheckEquals('FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF', TIdIPAddressParser.MaskToAddress(128, Id_IPv6), '128 significant digits, IPv6');

  // Bad parameters
  try
    TIdIPAddressParser.MaskToAddress(33, Id_IPv4);
    Fail('Failed to bail out on bad mask 33 for an IPv4 mask');
  except
    on EBadParameter do;
  end;

  try
    TIdIPAddressParser.MaskToAddress(129, Id_IPv4);
    Fail('Failed to bail out on bad mask 129 for an IPv6 mask');
  except
    on EBadParameter do;
  end;

  try
    TIdIPAddressParser.MaskToAddress(0, Id_IPUnknown);
    Fail('Failed to bail out on an unknown IP version');
  except
    on EBadParameter do;
  end;
end;

procedure TestTIdIPAddressParser.TestNetworkFor;
var
  I:    Integer;
  Mask: String;
begin
  CheckEquals('192.168.1.1', TIdIPAddressParser.NetworkFor('192.168.1.1', '255.255.255.255'), '192.168.1.1/255.255.255.255');
  CheckEquals('192.168.1.0', TIdIPAddressParser.NetworkFor('192.168.1.1', '255.255.255.0'),   '192.168.1.1/255.255.255.0');
  CheckEquals('192.168.0.0', TIdIPAddressParser.NetworkFor('192.168.1.1', '255.255.0.0'),     '192.168.1.1/255.255.0.0');
  CheckEquals('192.0.0.0',   TIdIPAddressParser.NetworkFor('192.168.1.1', '255.0.0.0'),       '192.168.1.1/255.255.0.0');
  CheckEquals('0.0.0.0',     TIdIPAddressParser.NetworkFor('192.168.1.1', '0.0.0.0'),         '192.168.1.1/255.255.0.0');

  for I := 0 to BitsInCardinal do begin
    Mask := TIdIPAddressParser.MaskToAddress(I, Id_IPv4);
    CheckEquals(Mask,
                TIdIPAddressParser.NetworkFor('255.255.255.255', Mask),
                '255.255.255.255/' + Mask);
  end;

  for I := 0 to BitsInCardinal do begin
    Mask := TIdIPAddressParser.MaskToAddress(I, Id_IPv4);
    CheckEquals(Mask,
                TIdIPAddressParser.NetworkFor('255.255.255.255', Mask),
                '255.255.255.255/' + Mask);
  end;

  for I := 0 to BitsInIPv6Address do begin
    Mask := TIdIPAddressParser.MaskToAddress(I, Id_IPv6);
    CheckEquals(Mask,
                TIdIPAddressParser.NetworkFor('ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff', Mask),
                'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff/' + Mask);
  end;
end;

procedure TestTIdIPAddressParser.TestNetworkForNumBits;
var
  I: Integer;
begin
  CheckEquals('192.168.1.1', TIdIPAddressParser.NetworkFor('192.168.1.1', 32), '192.168.1.1/32');
  CheckEquals('192.168.1.0', TIdIPAddressParser.NetworkFor('192.168.1.1', 24), '192.168.1.1/24');
  CheckEquals('192.168.0.0', TIdIPAddressParser.NetworkFor('192.168.1.1', 16), '192.168.1.1/16');
  CheckEquals('192.0.0.0',   TIdIPAddressParser.NetworkFor('192.168.1.1', 8),  '192.168.1.1/8');
  CheckEquals('0.0.0.0',     TIdIPAddressParser.NetworkFor('192.168.1.1', 0),  '192.168.1.1/0');

  for I := 0 to BitsInCardinal do begin
    CheckEquals(TIdIPAddressParser.MaskToAddress(I, Id_IPv4),
                 TIdIPAddressParser.NetworkFor('255.255.255.255', I),
                 '255.255.255.255/' + IntToStr(I));
  end;

  try
    TIdIPAddressParser.NetworkFor('192.168.1.1', 33);
    Fail('Failed to bail out on bad mask 33 for an IPv4 mask');
  except
    on EBadParameter do;
  end;

  CheckEquals('2002:DECA:FBAD:1::1', TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 128),  '2002:deca:fbad:1::1/128');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 120),  '2002:deca:fbad:1::1/120');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 112),  '2002:deca:fbad:1::1/112');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 104),  '2002:deca:fbad:1::1/104');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 96),   '2002:deca:fbad:1::1/96');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 88),   '2002:deca:fbad:1::1/88');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 80),   '2002:deca:fbad:1::1/80');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 72),   '2002:deca:fbad:1::1/72');
  CheckEquals('2002:DECA:FBAD:1::',  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 64),   '2002:deca:fbad:1::1/64');
  CheckEquals('2002:DECA:FBAD::',    TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 56),   '2002:deca:fbad:1::1/56');
  CheckEquals('2002:DECA:FBAD::',    TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 48),   '2002:deca:fbad:1::1/48');
  CheckEquals('2002:DECA:FB00::',    TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 40),   '2002:deca:fbad:1::1/40');
  CheckEquals('2002:DECA::',         TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 32),   '2002:deca:fbad:1::1/32');
  CheckEquals('2002:DE00::',         TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 24),   '2002:deca:fbad:1::1/24');
  CheckEquals('2002::',              TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 16),   '2002:deca:fbad:1::1/16');
  CheckEquals('2000::',              TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 8),    '2002:deca:fbad:1::1/8');
  CheckEquals('::',                  TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 0),    '2002:deca:fbad:1::1/0');

  for I := 0 to BitsInIPv6Address do begin
    CheckEquals(TIdIPAddressParser.MaskToAddress(I, Id_IPv6),
                TIdIPAddressParser.NetworkFor('ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff', I),
                'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff/' + IntToStr(I));
  end;

  try
    TIdIPAddressParser.NetworkFor('2002:deca:fbad:1::1', 129);
    Fail('Failed to bail out on bad mask 129 for an IPv4 mask');
  except
    on EBadParameter do;
  end;
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

  try
    TIdIPAddressParser.ParseIPv6Address('1::256.0.0.0', Address);

    Fail('Failed to raise exception on ''1::256.0.0.0');
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
