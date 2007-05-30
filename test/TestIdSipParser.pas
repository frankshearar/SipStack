{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipParser;

interface

uses
  Classes, IdSipMessage, TestFramework, TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestDecodeQuotedStr;
    procedure TestIsEqual;
    procedure TestShortMonthToInt;
  end;

  TestTIdSipParser = class(TTestCaseSip)
  private
    P:          TIdSipParser;
    Request:    TIdSipRequest;
    Response:   TIdSipResponse;

    procedure CheckBasicMessage(Msg: TIdSipMessage;
                                CheckBody: Boolean = true);
    procedure CheckBasicRequest(Msg: TIdSipMessage;
                                CheckBody: Boolean = true);
    procedure CheckBasicResponse(Msg: TIdSipMessage;
                                 CheckBody: Boolean = true);
    procedure CheckTortureTest(const RequestStr, ExpectedExceptionMsg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExtractAngleQuotedUri;
    procedure TestExtractAngleQuotedUriMalformed;
    procedure TestExtractQuotedString;
    procedure TestExtractQuotedStringMalformed;
    procedure TestExtractToken;
//    procedure TestHasValidSyntax;
    procedure TestIsIPv6Reference;
    procedure TestIsMethod;
    procedure TestIsQuotedString;
    procedure TestIsQValue;
    procedure TestIsRequest;
    procedure TestIsSipVersion;
    procedure TestIsToken;
    procedure TestIsTransport;
    procedure TestIsUuidUrn;
    procedure TestIsWord;
    procedure TestParseAndMakeMessageEmptyStream;
    procedure TestParseAndMakeMessageFromString;
    procedure TestParseAndMakeMessageMalformedRequest;
    procedure TestParseAndMakeMessageRequest;
    procedure TestParseAndMakeMessageResponse;
    procedure TestParseAndMakeRequestFromString;
    procedure TestParseAndMakeResponseFromString;
    procedure TestParseExtensiveRequest;
    procedure TestParseRequestMessageBodyLongerThanContentLength;
    procedure TestParseRequestMultipleVias;
    procedure TestParseRequestRequestUriHasSpaces;
    procedure TestParseRequestRequestUriInAngleBrackets;
//    procedure TestTortureTest1; // commented out because right now we don't accept non-SIP/SIPS URIs
    procedure TestTortureTest19;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest24;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
  end;

implementation

uses
  DateUtils, IdSimpleParser, SysUtils, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Parsing)');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestDecodeQuotedStr;
var
  Q: String;
begin
  Check(DecodeQuotedStr('', Q),     'parsing: ''''');
  CheckEquals('',       Q,          'result: ''''');
  Check(DecodeQuotedStr('abcd', Q), 'parsing: abcd');
  CheckEquals('abcd',   Q,          'result: abcd');
  Check(DecodeQuotedStr('\"', Q),   'parsing: ');
  CheckEquals('"',      Q,          'result: \"');
  Check(DecodeQuotedStr('\\', Q),   'parsing: ');
  CheckEquals('\',      Q,          'result: \\');

  Check(DecodeQuotedStr('\ ', Q),       'parsing: \ SP');
  CheckEquals(' ',      Q,              'result: \ SP');
  Check(DecodeQuotedStr('\a\b\c\d', Q), 'parsing: \a\b\c\d');
  CheckEquals('abcd',   Q,              'result: \a\b\c\d');
  Check(DecodeQuotedStr('\'#0, Q),      'parsing: \#0');
  CheckEquals(#0,       Q,              'result: \#0');
  Check(DecodeQuotedStr('hello\\', Q),  'parsing: hello\\');
  CheckEquals('hello\', Q,              'result: hello\\');

  Check(not DecodeQuotedStr('\', Q),      '\');
  Check(not DecodeQuotedStr('hello\', Q), 'hello\');
  Check(not DecodeQuotedStr('"', Q),      '"');
  Check(not DecodeQuotedStr('"""', Q),    '"""');
end;

procedure TestFunctions.TestIsEqual;
begin
  Check(    IsEqual('', ''),         ''''' & ''''');
  Check(not IsEqual(' ', ''),        ''' '' & ''''');
  Check(    IsEqual('abcd', 'AbCd'), '''abcd'', ''AbCd''');
  Check(not IsEqual('absd', 'Abcd'), '''absd'', ''Abcd''');
end;

procedure TestFunctions.TestShortMonthToInt;
begin
  CheckEquals(1, ShortMonthToInt('JAN'), 'JAN');
  CheckEquals(1, ShortMonthToInt('jan'), 'jan');

  CheckEquals(1,  ShortMonthToInt('Jan'), 'Jan');
  CheckEquals(2,  ShortMonthToInt('Feb'), 'Feb');
  CheckEquals(3,  ShortMonthToInt('Mar'), 'Mar');
  CheckEquals(4,  ShortMonthToInt('Apr'), 'Apr');
  CheckEquals(5,  ShortMonthToInt('May'), 'May');
  CheckEquals(6,  ShortMonthToInt('Jun'), 'Jun');
  CheckEquals(7,  ShortMonthToInt('Jul'), 'Jul');
  CheckEquals(8,  ShortMonthToInt('Aug'), 'Aug');
  CheckEquals(9,  ShortMonthToInt('Sep'), 'Sep');
  CheckEquals(10, ShortMonthToInt('Oct'), 'Oct');
  CheckEquals(11, ShortMonthToInt('Nov'), 'Nov');
  CheckEquals(12, ShortMonthToInt('Dec'), 'Dec');

  try
    ShortMonthToInt('xxx');
    Fail('Failed to bail out on malformed short month name');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''xxx'' to type Integer', E.Message, 'Unexpected error');
  end;
end;

//******************************************************************************
//* TestTIdSipParser                                                           *
//******************************************************************************
//* TestTIdSipParser Public methods ********************************************

procedure TestTIdSipParser.SetUp;
begin
  inherited SetUp;

  Self.P := TIdSipParser.Create;

  Self.Request := TIdSipRequest.Create;
  Self.Response := TIdSipResponse.Create;
end;

procedure TestTIdSipParser.TearDown;
begin
  Self.Request.Free;
  Self.Response.Free;
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSipParser Published methods *****************************************

procedure TestTIdSipParser.TestExtractAngleQuotedUri;
  procedure CheckExtractAngleQuotedUri(Source: String;
                                       const ExpectedAngleQuotedUri: String;
                                       const ExpectedRemainder: String);
  var
    Remainder:    String;
    AngleQuotedUri: String;
  begin
    Remainder := Source;
    AngleQuotedUri := TIdSipParser.ExtractAngleQuotedUri(Remainder);
    CheckEquals(ExpectedAngleQuotedUri,
                AngleQuotedUri,
                '''' + Source + ''': URI');
    CheckEquals(ExpectedRemainder,
                Remainder,
                '''' + Source + ''': remainder');
  end;
begin
  CheckExtractAngleQuotedUri('', '', '');
  CheckExtractAngleQuotedUri('<http://foo/>', 'http://foo/', '');
  CheckExtractAngleQuotedUri('<sip:foo>;+sip.instance="<urn:foo:bar>"',
                             'sip:foo',
                             ';+sip.instance="<urn:foo:bar>"');
end;

procedure TestTIdSipParser.TestExtractAngleQuotedUriMalformed;
  procedure CheckExtractAngleQuotedUriMalformed(Source: String);
  var
    Remainder: String;
  begin
    Remainder := Source;
    try
      TIdSipParser.ExtractAngleQuotedUri(Remainder);
      Fail('Failed to bail out parsing ''' + Source + '''');
    except
      on EParserError do;
    end;
  end;
begin
  CheckExtractAngleQuotedUriMalformed('<sip:foo');
end;

procedure TestTIdSipParser.TestExtractQuotedString;
  procedure CheckExtractQuotedString(Source: String;
                                     const ExpectedQuotedString: String;
                                     const ExpectedRemainder: String);
  var
    Remainder:    String;
    QuotedString: String;
  begin
    Remainder := Source;
    QuotedString := TIdSipParser.ExtractQuotedString(Remainder);
    CheckEquals(ExpectedQuotedString,
                QuotedString,
                '''' + Source + ''': quoted-string');
    CheckEquals(ExpectedRemainder,
                Remainder,
                '''' + Source + ''': remainder');
  end;
begin
  CheckExtractQuotedString('', '', '');

  // Eat whitespace
  CheckExtractQuotedString('  ', '', '');

  // Return the quoted-string
  CheckExtractQuotedString('"abc"', 'abc', '');

  // Eat whitespace between quoted-string and remainder
  CheckExtractQuotedString('"abc" def', 'abc', 'def');

  // Decode the quoted-string returned; leave remainder untouched
  CheckExtractQuotedString('"\"ac\\dc\"" "\""', '"ac\dc"', '"\""');
end;

procedure TestTIdSipParser.TestExtractQuotedStringMalformed;
  procedure CheckExtractQuotedStringMalformed(Source: String);
  var
    Remainder: String;
  begin
    Remainder := Source;
    try
      TIdSipParser.ExtractQuotedString(Remainder);
      Fail('Failed to bail out parsing ''' + Source + '''');
    except
      on EParserError do;
    end;
  end;
begin
  // Unmatched quotes
  CheckExtractQuotedStringMalformed('"');

  // Unmatched quotes - \" is an escaped quote
  CheckExtractQuotedStringMalformed('"\"');

  // Unmatched quotes - torture
  CheckExtractQuotedStringMalformed('"\\\"');
end;

procedure TestTIdSipParser.TestExtractToken;
  procedure CheckExtractToken(Source: String;
                              const ExpectedToken: String;
                              const ExpectedRemainder: String);
  var
    Remainder: String;
    Token:     String;
  begin
    Remainder := Source;
    Token := TIdSipParser.ExtractToken(Remainder);
    CheckEquals(ExpectedToken,     Token,     '''' + Source + ''': token');
    CheckEquals(ExpectedRemainder, Remainder, '''' + Source + ''': remainder');
  end;
begin
  CheckExtractToken('', '', '');
  CheckExtractToken('  ', '', '');

  CheckExtractToken('abc', 'abc', '');
  CheckExtractToken('<sip:foo>', '', '<sip:foo>');
  CheckExtractToken('"quoted string"', '', '"quoted string"');
  CheckExtractToken('abc/def', 'abc', '/def');

  CheckExtractToken(' abc', 'abc', '');
  CheckExtractToken('Bar <sip:foo>', 'Bar', '<sip:foo>');
end;
{
procedure TestTIdSipParser.TestHasValidSyntax;
var
  R: TIdSipRequest;
begin
  R := Self.P.ParseAndMakeRequest(LocalLoopRequest);
  try
    Check(Self.P.HasValidSyntax, 'Syntactically correct message');
  finally
    R.Free;
  end;

  R := Self.P.ParseAndMakeRequest(StringReplace(LocalLoopRequest,
                                                'INVITE',
                                                'INV''TE',
                                                [rfReplaceAll, rfIgnoreCase]));
  try
    Check(not Self.P.HasValidSyntax, 'Malformed method');
  finally
    R.Free;
  end;
end;
}
procedure TestTIdSipParser.TestIsIPv6Reference;
begin
  Check(not TIdSipParser.IsIPv6Reference(''),
        'Empty string');
  Check(not TIdSipParser.IsIPv6Reference('ff01:0:0:0:0:0:0:101'),
        'ff01:0:0:0:0:0:0:101');
  Check(not TIdSipParser.IsIPv6Reference('[]'),
        '[]');
  Check(TIdSipParser.IsIPv6Reference('[ff01:0:0:0:0:0:0:101]'),
        '[ff01:0:0:0:0:0:0:101]');
end;

procedure TestTIdSipParser.TestIsMethod;
begin
  Check(not TIdSipParser.IsMethod(''),
        'Empty string');
  Check(not TIdSipParser.IsMethod('Cra.-zy''+prea"cher%20man~`!'),
        'Cra.-zy''+prea"cher%20man~`!'); // no "'s
  Check(not TIdSipParser.IsMethod('LastChar"'),
        'LastChar"'); // no "'s
  Check(TIdSipParser.IsMethod('INVITE'),
        'INVITE');
  Check(TIdSipParser.IsMethod('X-INVITE'),
        'X-INVITE');
  Check(TIdSipParser.IsMethod('1'),
        '1');
  Check(TIdSipParser.IsMethod('a'),
        'a');
  Check(TIdSipParser.IsMethod('---'),
        '---');
  Check(TIdSipParser.IsMethod('X_CITE'),
        'X_CITE');
  Check(TIdSipParser.IsMethod('Cra.-zy''+preacher%20man~`!'),
        'Cra.-zy''+preacher%20man~`!');
end;

procedure TestTIdSipParser.TestIsQuotedString;
begin
  Check(not TIdSipParser.IsQuotedString(''),     '''''');
  Check(not TIdSipParser.IsQuotedString('a'),    'a');
  Check(not TIdSipParser.IsQuotedString('\'),    '\');
  Check(not TIdSipParser.IsQuotedString('\"'),   '\"');
  Check(not TIdSipParser.IsQuotedString('"\"'),  '"\"');
  Check(not TIdSipParser.IsQuotedString('"""'),  '"""');
  Check(    TIdSipParser.IsQuotedString('"a"'),  '"a"');
  Check(    TIdSipParser.IsQuotedString('"\""'), '"\""');
  Check(    TIdSipParser.IsQuotedString('"\\"'), '"\\"');
end;

procedure TestTIdSipParser.TestIsQValue;
begin
  Check(not TIdSipParser.IsQValue(''),       '''''');
  Check(not TIdSipParser.IsQValue('a'),      'a');
  Check(not TIdSipParser.IsQValue('0.a'),    '0.a');
  Check(not TIdSipParser.IsQValue('1.1'),    '1.1');
  Check(not TIdSipParser.IsQValue('0.1234'), '0.1234');
  Check(not TIdSipParser.IsQValue('.1'),     '.1');
  Check(not TIdSipParser.IsQValue('0.'),     '0.');
  Check(    TIdSipParser.IsQValue('0.0'),    '0.0');
  Check(    TIdSipParser.IsQValue('0.00'),   '0.00');
  Check(    TIdSipParser.IsQValue('0.000'),  '0.000');
  Check(    TIdSipParser.IsQValue('0.123'),  '0.123');
  Check(    TIdSipParser.IsQValue('0.666'),  '0.666');
  Check(    TIdSipParser.IsQValue('1.0'),    '1.0');
  Check(    TIdSipParser.IsQValue('1.00'),   '1.00');
  Check(    TIdSipParser.IsQValue('1.000'),  '1.000');
end;

procedure TestTIdSipParser.TestIsRequest;
begin
  Check(    TIdSipParser.IsRequest('INVITE sip:foo SIP/2.0'),    'Normal Request-Line');
  Check(    TIdSipParser.IsRequest('INVITE sip:foo SIP/2.0'),    'Unknown SIP-Version');
  Check(    TIdSipParser.IsRequest('UNKNOWN sip:foo SIP/2.0'),   'Unknown method');
  Check(    TIdSipParser.IsRequest('INVITE mailto:foo SIP/2.0'), 'mailto Request-URI');
  Check(not TIdSipParser.IsRequest('SIP/2.0 200 OK'),            'Status-Line');
  Check(not TIdSipParser.IsRequest(''),                          'Empty string');
end;

procedure TestTIdSipParser.TestIsSipVersion;
begin
  Check(not TIdSipParser.IsSipVersion(''),         '''''');
  Check(    TIdSipParser.IsSipVersion('SIP/2.0'),  'SIP/2.0');
  Check(    TIdSipParser.IsSipVersion('sip/2.0'),  'sip/2.0');
  Check(    TIdSipParser.IsSipVersion(SIPVersion), 'SIPVersion constant');
  Check(not TIdSipParser.IsSipVersion('SIP/X.Y'),  'SIP/X.Y');
end;

procedure TestTIdSipParser.TestIsToken;
begin
  Check(not TIdSipParser.IsToken(''),         '''''');
  Check(not TIdSipParser.IsToken('SIP/2.0'),  'SIP/2.0');
  Check(    TIdSipParser.IsToken('one'),      'one');
  Check(    TIdSipParser.IsToken('1two'),     '1two');
  Check(    TIdSipParser.IsToken('1-two'),    '1-two');
  Check(    TIdSipParser.IsToken('.'),        '.');
  Check(    TIdSipParser.IsToken('!'),        '!');
  Check(    TIdSipParser.IsToken('%'),        '%');
  Check(    TIdSipParser.IsToken('*'),        '*');
  Check(    TIdSipParser.IsToken('_'),        '_');
  Check(    TIdSipParser.IsToken('+'),        '+');
  Check(    TIdSipParser.IsToken('`'),        '`');
  Check(    TIdSipParser.IsToken(''''),        '''');
  Check(    TIdSipParser.IsToken('~'),        '~');
  Check(    TIdSipParser.IsToken('''baka'''), '''baka''');
end;

procedure TestTIdSipParser.TestIsTransport;
begin
  Check(not TIdSipParser.IsTransport(''),     '''''');
  Check(not TIdSipParser.IsTransport('tcp;'), 'tcp;');
  Check(    TIdSipParser.IsTransport('a'),     'a');  
  Check(    TIdSipParser.IsTransport('tcp'),  'tcp');
  Check(    TIdSipParser.IsTransport('TCP'),  'TCP');
  Check(    TIdSipParser.IsTransport('udp'),  'udp');
  Check(    TIdSipParser.IsTransport('UDP'),  'UDP');
  Check(    TIdSipParser.IsTransport('sctp'), 'sctp');
  Check(    TIdSipParser.IsTransport('SCTP'), 'SCTP');
  Check(    TIdSipParser.IsTransport('tls'),  'tls');
  Check(    TIdSipParser.IsTransport('TLS'),  'TLS');
end;

procedure TestTIdSipParser.TestIsUuidUrn;
begin
  Check(not TIdSipParser.IsUuidUrn(''),
        'The empty string');
  Check(not TIdSipParser.IsUuidUrn('00000000-0000-0000-0000-000000000000'),
        'Just a UUID');
  Check(not TIdSipParser.IsUuidUrn('uuid:00000000-0000-0000-0000-000000000000'),
        '"uuid" declaration and a UUID');
  Check(not TIdSipParser.IsUuidUrn('un:uuid:00000000-0000-0000-0000-000000000000'),
        '"un" instead of "urn"');
  Check(not TIdSipParser.IsUuidUrn('urn:uud:00000000-0000-0000-0000-000000000000'),
        '"uud" instead of "uuid"');
  Check(TIdSipParser.IsUuidUrn('urn:uuid:00000000-0000-0000-0000-000000000000'),
        'A proper URN (the zero UUID) -');
  Check(not TIdSipParser.IsUuidUrn('urn:uuid:00000000-0000-0000-0000-00000000000x'),
        'A proper declaration with an invalid UUID (last token has invalid character)');
  Check(not TIdSipParser.IsUuidUrn('urn:uuid:00000000-0000-0000-000-000000000000'),
        'A proper declaration with an invalid UUID (4th UUID token too short');
  Check(not TIdSipParser.IsUuidUrn('urn:uuid:00000000-0000-000g-0000-000000000000'),
        'A proper declaration with an invalid UUID (3rd UUID token has invalid character)');
  Check(not TIdSipParser.IsUuidUrn('urn:uuid:00000000-000-0000-0000-000000000000'),
        'A proper declaration with an invalid UUID (2nd UUID token too short)');
  Check(not TIdSipParser.IsUuidUrn('urn:uuid:0000000--0000-0000-0000-000000000000'),
        'A proper declaration with an invalid UUID (1st UUID token has invalid character or too short)');
end;

procedure TestTIdSipParser.TestIsWord;
var
  C: Char;
begin
  Check(not TIdSipParser.IsWord(''), '''''');

  for C := 'a' to 'z' do
    Check(TIdSipParser.IsWord(C), C);
  for C := '0' to '9' do
    Check(TIdSipParser.IsWord(C), C);
  for C := 'A' to 'Z' do
    Check(TIdSipParser.IsWord(C), C);

  Check(TIdSipParser.IsWord('-.!%*_+`''~()<>:\"/[]?{}'),
        '-.!%*_+`''~()<>:\"/[]?{}');
end;

procedure TestTIdSipParser.TestParseAndMakeMessageEmptyStream;
var
  Msg: TIdSipMessage;
begin
  Msg := TIdSipMessage.ReadMessageFrom('');
  try
    Check(Msg.IsMalformed,
          'Failed to bail out of empty string');
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageFromString;
var
  R: TIdSipMessage;
begin
  R := TIdSipMessage.ReadMessageFrom(BasicRequest);
  try
    CheckBasicRequest(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageMalformedRequest;
var
  Msg: TIdSipMessage;
begin
  Msg := TIdSipMessage.ReadMessageFrom('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/;2.0'#13#10
                                     + #13#10);
  try
    Check(Msg.IsMalformed,
          'Failed to bail out on parsing a malformed message');
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageRequest;
var
  Msg: TIdSipMessage;
begin
  Msg := TIdSipMessage.ReadMessageFrom(BasicRequest);
  try
    Self.CheckBasicRequest(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageResponse;
var
  Msg: TIdSipMessage;
begin
  Msg := TIdSipMessage.ReadMessageFrom(BasicResponse);
  try
    Self.CheckBasicResponse(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeRequestFromString;
var
  R: TIdSipRequest;
begin
  R := TIdSipMessage.ReadRequestFrom(BasicRequest);
  try
    CheckBasicRequest(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeResponseFromString;
var
  R: TIdSipResponse;
begin
  R := TIdSipMessage.ReadResponseFrom(BasicResponse);
  try
    CheckBasicResponse(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseExtensiveRequest;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipMessage.ReadRequestFrom(ExtensiveRequest);
  try
    CheckEquals('Accept: text/t140, text/plain;q=0.7;foo=bar, text/xml',
                Req.FirstHeader(AcceptHeader).AsString,
                'Accept');
    CheckEquals('Call-ID: a84b4c76e66710@gw1.leo-ix.org',
                Req.FirstHeader(CallIdHeaderFull).AsString,
                'Call-ID');
    CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.luna',
                Req.FirstContact.AsString,
                'Contact');
    CheckEquals('Content-Length: 29',
                Req.FirstHeader(ContentLengthHeaderFull).AsString,
                'Content-Length');
    CheckEquals('Content-Type: text/plain',
                Req.FirstHeader(ContentTypeHeaderFull).AsString,
                'Content-Type');
    CheckEquals('CSeq: 314159 INVITE',
                Req.FirstHeader(CSeqHeader).AsString,
                'CSeq');
    CheckEquals('Date: Thu, 1 Jan 1970 00:00:00 +0000',
                Req.FirstHeader(DateHeader).AsString,
                'Date');
    CheckEquals('Expires: 1000',
                Req.FirstHeader(ExpiresHeader).AsString,
                'Expires');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Req.FirstHeader(FromHeaderFull).AsString,
                'From');
    CheckEquals('Max-Forwards: 70',
                Req.FirstHeader(MaxForwardsHeader).AsString,
                'Max-Forwards');
    CheckEquals('Record-Route: localhost <sip:127.0.0.1;lr>',
                Req.FirstHeader(RecordRouteHeader).AsString,
                'Record-Route');
    CheckEquals('Route: localhost <sip:127.0.0.1;lr>',
                Req.FirstHeader(RouteHeader).AsString,
                'Route');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775',
                Req.FirstHeader(ToHeaderFull).AsString,
                'To');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds',
                Req.FirstHeader(ViaHeaderFull).AsString,
                'Via');
    CheckEquals('Warning: 301 draugr "Not really interested"',
                Req.FirstHeader(WarningHeader).AsString,
                'Warning');
    CheckEquals('X-Not-A-Header: I am not defined in RFC 3261',
                Req.FirstHeader('X-Not-A-Header').AsString,
                'X-Not-A-Header');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMessageBodyLongerThanContentLength;
var
  Leftovers: array[0..99] of Char;
  Req:       TIdSipMessage;
  Str:       TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest,
                              'Content-Length: 29',
                              'Content-Length: 4',
                              []));
  try
    Req := TIdSipMessage.ReadMessageFrom(Str);
    try
      CheckEquals(4,  Req.ContentLength, 'ContentLength');
      CheckEquals('', Req.Body,          'Body');

      CheckEquals(Length('I am a message. Hear me roar!'),
                  Str.Read(LeftOvers, Length(Leftovers)),
                  'Read unexpected number of bytes from the stream');
    finally
      Req.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMultipleVias;
var
  Req:        TIdSipRequest;
  Via0, Via1: TIdSipViaHeader;
begin
  Req := TIdSipMessage.ReadRequestFrom('INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org:5061;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.luna'#13#10
                            + 'Via: SIP/3.0/TLS gw5.cust1.leo-ix.org;branch=z9hG4bK776aheh'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    CheckEquals(2, Req.Path.Length, 'Path.Length');

    Via0 := Req.Path.Items[0] as TIdSipViaHeader;
    CheckEquals('Via',              Via0.Name,             'LastHop.Name');
    CheckEquals('SIP/2.0',          Via0.SipVersion,       'LastHop.SipVersion');
    CheckEquals(TcpTransport,       Via0.Transport,        'LastHop.Transport');
    CheckEquals('gw1.leo-ix.org',   Via0.SentBy,           'LastHop.SentBy');
    CheckEquals(5061,               Via0.Port,             'LastHop.Port');
    CheckEquals('z9hG4bK776asdhds', Via0.Params['branch'], 'LastHop.Params[''branch'']');
    CheckEquals('SIP/2.0/TCP gw1.leo-ix.org:5061',
                Via0.Value,
                'LastHop.Value');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org:5061;branch=z9hG4bK776asdhds',
                Via0.AsString,
                'LastHop.AsString');

    Via1 := Req.Path.Items[1] as TIdSipViaHeader;
    CheckEquals('Via',                   Via1.Name,             'Via1.Name');
    CheckEquals('SIP/3.0',               Via1.SipVersion,       'Via1.SipVersion');
    CheckEquals(TlsTransport,            Via1.Transport,        'Via1.Transport');
    CheckEquals('gw5.cust1.leo-ix.org',  Via1.SentBy,           'Via1.SentBy');
    CheckEquals(DefaultSipsPort,         Via1.Port,             'Via1.Port');
    CheckEquals('z9hG4bK776aheh',        Via1.Params['branch'], 'Via1.Params[''branch'']');
    CheckEquals('SIP/3.0/TLS gw5.cust1.leo-ix.org',
                Via1.Value,
                'Via1.Value');
    CheckEquals('Via: SIP/3.0/TLS gw5.cust1.leo-ix.org;branch=z9hG4bK776aheh',
                Via1.AsString,
                'Via1.AsString');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriHasSpaces;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipMessage.ReadRequestFrom('INVITE sip:wintermute@tessier ashpool.co.lu SIP/2.0'#13#10);
  try
    Check(Req.IsMalformed,
          'Malformed start line (Request-URI has spaces) parsed without error');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriInAngleBrackets;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipMessage.ReadRequestFrom('INVITE <sip:wintermute@tessier-ashpool.co.luna> SIP/2.0'#13#10);
  try
    Check(Req.IsMalformed,
          'Malformed start line (Request-URI enclosed in angle brackets) parsed without error');
  finally
    Req.Free;
  end;
end;
{
procedure TestTIdSipParser.TestTortureTest1;
var
  Res: TIdSipResponse;
begin
  Res := TIdSipMessage.ReadResponseFrom(TortureTest1);
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Self.Request);

    CheckEquals('INVITE',                              Self.Request.Method,         'Method');
    CheckEquals('SIP/2.0',                             Self.Request.SipVersion,     'SipVersion');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.Request.RequestUri.URI, 'RequestUri');
    CheckEquals(6,                                     Self.Request.MaxForwards,    'MaxForwards');
    CheckEquals('0ha0isndaksdj@10.1.1.1',              Self.Request.CallID,         'CallID');

    CheckEquals('',
                Self.Request.ToHeader.DisplayName,
                'ToHeader.DisplayName');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com',
                Self.Request.ToHeader.Address.URI,
                'ToHeader.Address.URI');
    CheckEquals(';tag=1918181833n',
                Self.Request.ToHeader.ParamsAsString,
                'ToHeader.ParamsAsString');

    CheckEquals('J Rosenberg \"',
                Self.Request.From.DisplayName,
                'From.DisplayName');
    CheckEquals('sip:jdrosen@lucent.com',
                Self.Request.From.Address.URI,
                'From.Address.URI');
    CheckEquals(';tag=98asjd8',
                Self.Request.From.ParamsAsString,
                'From.ParamsAsString');

    CheckEquals(3, Request.Path.Length, 'Path.Length');

    CheckEquals('To: sip:vivekg@chair.dnrc.bell-labs.com;tag=1918181833n',
                Self.Request.HeaderAt(0).AsString,
                'To header');
    CheckEquals('From: "J Rosenberg \\\"" <sip:jdrosen@lucent.com>;tag=98asjd8',
                Self.Request.HeaderAt(1).AsString,
                'From header');
    CheckEquals('Max-Forwards: 6',
                Self.Request.HeaderAt(2).AsString,
                'Max-Forwards header');
    CheckEquals('Call-ID: 0ha0isndaksdj@10.1.1.1',
                Self.Request.HeaderAt(3).AsString,
                'Call-ID header');
    CheckEquals('CSeq: 8 INVITE',
                Self.Request.HeaderAt(4).AsString,
                'CSeq header');
    CheckEquals('Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw',
                Self.Request.HeaderAt(5).AsString,
                'Via header #1');
    CheckEquals('Subject: ',
                Self.Request.HeaderAt(6).AsString,
                'Subject header');
    CheckEquals('NewFangledHeader: newfangled value more newfangled value',
                Self.Request.HeaderAt(7).AsString,
                'NewFangledHeader');
    CheckEquals('Content-Type: application/sdp',
                Self.Request.HeaderAt(8).AsString,
                'Content-Type');
    CheckEquals('Via: SIP/2.0/TCP 1192.168.156.222;branch=9ikj8',
                Self.Request.HeaderAt(9).AsString,
                'Via header #2');
    CheckEquals('Via: SIP/2.0/UDP 192.168.255.111;hidden',
                Self.Request.HeaderAt(10).AsString,
                'Via header #3');
    CheckEquals('Contact: "Quoted string \"\"" <sip:jdrosen@bell-labs.com>;newparam=newvalue;secondparam=secondvalue;q=0.33',
                Self.Request.HeaderAt(11).AsString,
                'Contact header #1');
    CheckEquals('Contact: tel:4443322',
                Self.Request.HeaderAt(12).AsString,
                'Contact header #2');
  finally
    Res.Free;
  end;
end;
}
procedure TestTIdSipParser.TestTortureTest19;
begin
  Self.CheckTortureTest(TortureTest19,
                        Format(MalformedToken, [ToHeaderFull,
                                                'To: "Mr. J. User <sip:j.user@company.com>']));
end;

procedure TestTIdSipParser.TestTortureTest22;
begin
  Self.CheckTortureTest(TortureTest22, RequestUriNoSpaces);
end;

procedure TestTIdSipParser.TestTortureTest23;
begin
  Self.CheckTortureTest(TortureTest23, RequestUriNoSpaces);
end;

procedure TestTIdSipParser.TestTortureTest24;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipMessage.ReadRequestFrom(StringReplace(TortureTest24, '%s', 'company.com', [rfReplaceAll]));
  try
    CheckEquals('sip:sip%3Auser%40example.com@company.com;other-param=summit',
                Req.RequestUri.URI,
                'Request-URI');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest35;
begin
  Self.CheckTortureTest(StringReplace(TortureTest24, '%s', 'company.com', [rfReplaceAll]),
                        Format(MalformedToken, [ExpiresHeader,
                                                'Expires: 0 0l@company.com']));
end;

procedure TestTIdSipParser.TestTortureTest40;
begin
  Self.CheckTortureTest(TortureTest40,
                        Format(MalformedToken, [FromHeaderFull,
                                                'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43']));
end;

//* TestTIdSipParser Private methods *******************************************

procedure TestTIdSipParser.CheckBasicMessage(Msg: TIdSipMessage;
                                             CheckBody: Boolean = true);
begin
  CheckEquals('SIP/2.0',                              Msg.SIPVersion,              'SipVersion');
  CheckEquals(29,                                     Msg.ContentLength,           'ContentLength');
  CheckEquals('text/plain',                           Msg.ContentType,             'ContentType');
  CheckEquals('a84b4c76e66710@gw1.leo-ix.org',        Msg.CallID,                  'CallID');
  CheckEquals('Wintermute',                           Msg.ToHeader.DisplayName,    'ToHeader.DisplayName');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Msg.ToHeader.Address.URI,    'ToHeader.Address.GetFullURI');
  CheckEquals(';tag=1928301775',                      Msg.ToHeader.ParamsAsString, 'Msg.ToHeader.ParamsAsString');
  CheckEquals('Case',                                 Msg.From.DisplayName,        'From.DisplayName');
  CheckEquals('sip:case@fried.neurons.org',           Msg.From.Address.URI,        'From.Address.GetFullURI');
  CheckEquals(';tag=1928301774',                      Msg.From.ParamsAsString,     'Msg.From.ParamsAsString');
  CheckEquals(314159,                                 Msg.CSeq.SequenceNo,         'Msg.CSeq.SequenceNo');
  CheckEquals('INVITE',                               Msg.CSeq.Method,             'Msg.CSeq.Method');

  CheckEquals(1,                  Msg.Path.Length,              'Path.Length');
  CheckEquals('SIP/2.0',          Msg.LastHop.SipVersion,       'LastHop.SipVersion');
  CheckEquals(TcpTransport,       Msg.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo-ix.org',   Msg.LastHop.SentBy,           'LastHop.SentBy');
  CheckEquals(DefaultSipPort,     Msg.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775',
              Msg.FirstHeader(ToHeaderFull).AsString,
              'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
              Msg.FirstHeader(FromHeaderFull).AsString,
              'From');
  CheckEquals('CSeq: 314159 INVITE',
              Msg.FirstHeader(CSeqHeader).AsString,
              'CSeq');
  CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.luna',
              Msg.FirstContact.AsString,
              'Contact');
  CheckEquals('Content-Type: text/plain',
              Msg.FirstHeader(ContentTypeHeaderFull).AsString,
              'Content-Type');

  if CheckBody then
    CheckEquals(BasicBody, Msg.Body, 'message-body');
end;

procedure TestTIdSipParser.CheckBasicRequest(Msg: TIdSipMessage;
                                             CheckBody: Boolean = true);
begin
  CheckEquals(TIdSipRequest.Classname, Msg.ClassName, 'Class type');

  CheckEquals('INVITE',
              (Msg as TIdSipRequest).Method,
              'Method');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              (Msg as TIdSipRequest).RequestUri.URI,
              'Request-URI');
  CheckEquals(70, (Msg as TIdSipRequest).MaxForwards, 'MaxForwards');
  CheckEquals(9,  Msg.HeaderCount, 'Header count');

  Self.CheckBasicMessage(Msg, CheckBody);
end;

procedure TestTIdSipParser.CheckBasicResponse(Msg: TIdSipMessage;
                                              CheckBody: Boolean = true);
begin
  CheckEquals(TIdSipResponse.Classname, Msg.ClassName, 'Class type');

  CheckEquals(486,         TIdSipResponse(Msg).StatusCode, 'StatusCode');
  CheckEquals('Busy Here', TIdSipResponse(Msg).StatusText, 'StatusText');
  CheckEquals(8,           Msg.HeaderCount,                'Header count');

  Self.CheckBasicMessage(Msg, CheckBody);
end;

procedure TestTIdSipParser.CheckTortureTest(const RequestStr, ExpectedExceptionMsg: String);
var
  Req: TIdSipRequest;
begin
  Req := TIdSipMessage.ReadRequestFrom(RequestStr);
  try
    Check(Req.IsMalformed,
          'Failed to bail out of a bad request');
  finally
    Req.Free;
  end;
end;

initialization
  RegisterTest('SIP Request Parsing', Suite);
end.
