unit TestIdSipParser;

interface

uses
  Classes, IdSipMessage, TestFramework, TestFrameworkEx;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestDecodeQuotedStr;
    procedure TestIsEqual;
    procedure TestShortMonthToInt;
  end;

  TestTIdSipParser = class(TTestCase)
  private
    P:        TIdSipParser;
    Request:  TIdSipRequest;
    Response: TIdSipResponse;

    procedure CheckBasicMessage(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
    procedure CheckBasicRequest(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
    procedure CheckBasicResponse(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
    procedure CheckTortureTest(const RequestStr, ExpectedExceptionMsg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaseInsensitivityOfContentLengthHeader;
    procedure TestGetHeaderName;
    procedure TestGetHeaderNumberValue;
    procedure TestGetHeaderValue;
    procedure TestIsIPv6Reference;
    procedure TestIsMethod;
    procedure TestIsQuotedString;
    procedure TestIsQValue;
    procedure TestIsSipVersion;
    procedure TestIsScheme;
    procedure TestIsToken;
    procedure TestIsTransport;
    procedure TestIsWord;
    procedure TestParseAndMakeMessageEmptyStream;
    procedure TestParseAndMakeMessageFromString;
    procedure TestParseAndMakeMessageMalformedRequest;
    procedure TestParseAndMakeMessageRequest;
    procedure TestParseAndMakeMessageResponse;
    procedure TestParseAndMakeRequest;
    procedure TestParseAndMakeRequestFromAResponseString;
    procedure TestParseAndMakeRequestFromString;
    procedure TestParseAndMakeResponse;
    procedure TestParseAndMakeResponseFromString;
    procedure TestParseExtensiveRequest;
    procedure TestParseReallyLongViaHeader;
    procedure TestParseRequest;
    procedure TestParseRequestBadCSeq;
    procedure TestParseRequestEmptyString;
    procedure TestParseRequestFoldedHeader;
    procedure TestParseRequestMalformedMaxForwards;
    procedure TestParseRequestMalformedMethod;
    procedure TestParseRequestMalformedRequestLine;
    procedure TestParseRequestMessageBodyLongerThanContentLength;
    procedure TestParseRequestMissingCallID;
    procedure TestParseRequestMissingCSeq;
    procedure TestParseRequestMissingFrom;
    procedure TestParseRequestMissingMaxForwards;
    procedure TestParseRequestMissingTo;
    procedure TestParseRequestMissingVia;
    procedure TestParseRequestMultipleVias;
    procedure TestParseRequestRequestUriHasSpaces;
    procedure TestParseRequestRequestUriInAngleBrackets;
    procedure TestParseRequestWithLeadingCrLfs;
    procedure TestParseRequestWithBodyAndNoContentType;
    procedure TestParseRequestWithMultipleRoutes;
    procedure TestParseResponse;
    procedure TestParseResponseEmptyString;
    procedure TestParseResponseFoldedHeader;
    procedure TestParseResponseInvalidStatusCode;
{    procedure TestParseResponseMissingCallID;
    procedure TestParseResponseMissingCSeq;
    procedure TestParseResponseMissingFrom;
    procedure TestParseResponseMissingMaxForwards;
    procedure TestParseResponseMissingTo;
    procedure TestParseResponseMissingVia;}
    procedure TestParseResponseWithLeadingCrLfs;
    procedure TestParseShortFormContentLength;
    procedure TestTortureTest1;
    procedure TestTortureTest8;
    procedure TestTortureTest11;
    procedure TestTortureTest13;
    procedure TestTortureTest15;
    procedure TestTortureTest19;
    procedure TestTortureTest21;
    procedure TestTortureTest22;
    procedure TestTortureTest23;
    procedure TestTortureTest24;
    procedure TestTortureTest35;
    procedure TestTortureTest40;
  end;

implementation

uses
  DateUtils, IdSipConsts, IdSimpleParser, IdSipHeaders, SysUtils, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (parsing)');
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
  P := TIdSipParser.Create;

  Request := TIdSipRequest.Create;
  Response := TIdSipResponse.Create;
end;

procedure TestTIdSipParser.TearDown;
begin
  Request.Free;
  Response.Free;
  P.Free;
end;

//* TestTIdSipParser Published methods *****************************************

procedure TestTIdSipParser.TestCaseInsensitivityOfContentLengthHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest,
                                            'Content-Length',
                                            'Content-LENGTH',
                                            [rfReplaceAll]));
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);

    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestGetHeaderName;
begin
  CheckEquals('haha', Self.P.GetHeaderName('haha'),        'haha');
  CheckEquals('haha', Self.P.GetHeaderName('haha: kief'),  'haha: kief');
  CheckEquals('haha', Self.P.GetHeaderName('haha:kief'),   'haha:kief');
  CheckEquals('haha', Self.P.GetHeaderName('haha :kief'),  'haha :kief');
  CheckEquals('haha', Self.P.GetHeaderName('haha : kief'), 'haha : kief');
  CheckEquals('haha', Self.P.GetHeaderName(' haha'),       ' haha');
  CheckEquals('',     Self.P.GetHeaderName(''),            '''''');
  CheckEquals('',     Self.P.GetHeaderName(#0),            '#0');
end;

procedure TestTIdSipParser.TestGetHeaderNumberValue;
begin
  CheckEquals(12, Self.P.GetHeaderNumberValue(Request, 'one :12'), 'one :12');
  CheckEquals(13, Self.P.GetHeaderNumberValue(Request, 'one:13'), 'one:13');
  CheckEquals(14, Self.P.GetHeaderNumberValue(Request, 'one : 14'), 'one : 14');

  try
    Self.P.GetHeaderNumberValue(Request, '');
    Fail('Failed to bail getting numeric value of '''' (request)');
  except
    on EBadRequest do;
  end;

  try
    Self.P.GetHeaderNumberValue(Response, '');
    Fail('Failed to bail getting numeric value of '''' (response)');
  except
    on EBadResponse do;
  end;

  try
    Self.P.GetHeaderNumberValue(Response, 'haha: one');
    Fail('Failed to bail getting numeric value of ''haha: one''');
  except
    on EBadResponse do;
  end;
end;

procedure TestTIdSipParser.TestGetHeaderValue;
begin
  CheckEquals('',     Self.P.GetHeaderValue('haha'),        'haha');
  CheckEquals('kief', Self.P.GetHeaderValue('haha: kief'),  'haha: kief');
  CheckEquals('kief', Self.P.GetHeaderValue('haha:kief'),   'haha:kief');
  CheckEquals('kief', Self.P.GetHeaderValue('haha :kief'),  'haha :kief');
  CheckEquals('kief', Self.P.GetHeaderValue('haha : kief'), 'haha : kief');
  CheckEquals('kief', Self.P.GetHeaderValue(' : kief'),  ' : kief');
  CheckEquals('kief', Self.P.GetHeaderValue(': kief'),  ': kief');
  CheckEquals('',     Self.P.GetHeaderValue(' haha'),       ' haha');
  CheckEquals('',     Self.P.GetHeaderValue(''),            '''''');
  CheckEquals('',     Self.P.GetHeaderValue(#0),            '#0');
end;

procedure TestTIdSipParser.TestIsIPv6Reference;
begin
  Check(not TIdSipParser.IsIPv6Reference(''),                       '''''');
  Check(not TIdSipParser.IsIPv6Reference('ff01:0:0:0:0:0:0:101'),   'ff01:0:0:0:0:0:0:101');
  Check(not TIdSipParser.IsIPv6Reference('[]'),                     '[]');
  Check(    TIdSipParser.IsIPv6Reference('[ff01:0:0:0:0:0:0:101]'), '[ff01:0:0:0:0:0:0:101]');
end;

procedure TestTIdSipParser.TestIsMethod;
begin
  Check(not TIdSipParser.IsMethod(''),                             '''''');
  Check(not TIdSipParser.IsMethod('Cra.-zy''+prea"cher%20man~`!'), 'Cra.-zy''+prea"cher%20man~`!'); // no "'s
  Check(not TIdSipParser.IsMethod('LastChar"'),                    'LastChar"'); // no "'s
  Check(    TIdSipParser.IsMethod('INVITE'),                       'INVITE');
  Check(    TIdSipParser.IsMethod('X-INVITE'),                     'X-INVITE');
  Check(    TIdSipParser.IsMethod('1'),                            '1');
  Check(    TIdSipParser.IsMethod('a'),                            'a');
  Check(    TIdSipParser.IsMethod('---'),                          '---');
  Check(    TIdSipParser.IsMethod('X_CITE'),                       'X_CITE');
  Check(    TIdSipParser.IsMethod('Cra.-zy''+preacher%20man~`!'),  'Cra.-zy''+preacher%20man~`!');
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
  Check(    TIdSipParser.IsQValue('0.123'),    '0.123');
  Check(    TIdSipParser.IsQValue('0.666'),    '0.666');
  Check(    TIdSipParser.IsQValue('1.0'),    '1.0');
  Check(    TIdSipParser.IsQValue('1.00'),   '1.00');
  Check(    TIdSipParser.IsQValue('1.000'),  '1.000');
end;

procedure TestTIdSipParser.TestIsSipVersion;
begin
  Check(not TIdSipParser.IsSipVersion(''),         '''''');
  Check(    TIdSipParser.IsSipVersion('SIP/2.0'),  'SIP/2.0');
  Check(    TIdSipParser.IsSipVersion('sip/2.0'),  'sip/2.0');
  Check(    TIdSipParser.IsSipVersion(SIPVersion), 'SIPVersion constant');
  Check(not TIdSipParser.IsSipVersion('SIP/X.Y'),  'SIP/X.Y');
end;

procedure TestTIdSipParser.TestIsScheme;
begin
  Check(not TIdSipParser.IsScheme(''),          '''''');
  Check(not TIdSipParser.IsScheme('%'),         '%');
  Check(not TIdSipParser.IsScheme('1sip'),      '1sip');
  Check(    TIdSipParser.IsScheme('sip-2.0+3'), 'sip-2.0+3');
end;

procedure TestTIdSipParser.TestIsToken;
begin
  Check(not TIdSipParser.IsToken(''),         '''''');
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
  Check(not TIdSipParser.IsTransport('a'),     'a');
  Check(not TIdSipParser.IsTransport('tcp;'), 'tcp;');
  Check(    TIdSipParser.IsTransport('tcp'),  'tcp');
  Check(    TIdSipParser.IsTransport('TCP'),  'TCP');
  Check(    TIdSipParser.IsTransport('udp'),  'udp');
  Check(    TIdSipParser.IsTransport('UDP'),  'UDP');
  Check(    TIdSipParser.IsTransport('sctp'), 'sctp');
  Check(    TIdSipParser.IsTransport('SCTP'), 'SCTP');
  Check(    TIdSipParser.IsTransport('tls'),  'tls');
  Check(    TIdSipParser.IsTransport('TLS'),  'TLS');
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

  Check(TIdSipParser.IsWord('-.!%*_+`''~()<>:\"/[]?{}'), '-.!%*_+`''~()<>:\"/[]?{}');
end;

procedure TestTIdSipParser.TestParseAndMakeMessageEmptyStream;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    Self.P.Source := Str;

    try
      Self.P.ParseAndMakeMessage.Free;
    except
      on E: EParser do
        CheckEquals(EmptyInputStream, E.Message, 'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageFromString;
var
  R: TIdSipRequest;
begin
  R := Self.P.ParseAndMakeMessage(BasicRequest) as TIdSipRequest;
  try
    CheckBasicRequest(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageMalformedRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/;2.0'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;

    try
      Self.P.ParseAndMakeMessage;
      Fail('Failed to bail out on parsing a malformed message');
    except
      on E: EBadRequest do;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageRequest;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicRequest);
  try
    Self.P.Source := Str;

    Msg := Self.P.ParseAndMakeMessage;
    try
      Self.CheckBasicRequest(Msg);
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeMessageResponse;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicResponse);
  try
    Self.P.Source := Str;

    Msg := Self.P.ParseAndMakeMessage;
    try
      Self.CheckBasicResponse(Msg);
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeRequest;
var
  Req: TIdSipRequest;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicRequest);
  try
    Self.P.Source := Str;

    Req := Self.P.ParseAndMakeRequest;
    try
      Self.CheckBasicRequest(Req);
    finally
      Req.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeRequestFromAResponseString;
var
  Req: TIdSipRequest;
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/;2.0 200 OK'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;

    try
      Req := Self.P.ParseAndMakeRequest;
      try
        Self.CheckBasicRequest(Req);
      finally
        Req.Free;
      end;

      Fail('Failed to bail out creating a request from a malformed response');
    except
      on EBadRequest do;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeRequestFromString;
var
  R: TIdSipRequest;
begin
  R := Self.P.ParseAndMakeRequest(BasicRequest);
  try
    CheckBasicRequest(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeResponse;
var
  Res: TIdSipResponse;
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicResponse);
  try
    Self.P.Source := Str;

    Res := Self.P.ParseAndMakeResponse;
    try
      Self.CheckBasicResponse(Res);
    finally
      Res.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseAndMakeResponseFromString;
var
  R: TIdSipResponse;
begin
  R := Self.P.ParseAndMakeResponse(BasicResponse);
  try
    CheckBasicResponse(R, false);
    CheckEquals(BasicBody, R.Body, 'Body should be set from a string');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipParser.TestParseExtensiveRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(ExtensiveRequest);
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);

    CheckEquals('Accept: text/t140, text/plain;q=0.7;foo=bar, text/xml',
                Self.Request.FirstHeader(AcceptHeader).AsString,
                'Accept');
    CheckEquals('Call-ID: a84b4c76e66710@gw1.leo-ix.org',
                Self.Request.FirstHeader(CallIdHeaderFull).AsString,
                'Call-ID');
    CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',
                Self.Request.FirstContact.AsString,
                'Contact');
    CheckEquals('Content-Length: 29',
                Self.Request.FirstHeader(ContentLengthHeaderFull).AsString,
                'Content-Length');
    CheckEquals('Content-Type: text/plain',
                Self.Request.FirstHeader(ContentTypeHeaderFull).AsString,
                'Content-Type');
    CheckEquals('CSeq: 314159 INVITE',
                Self.Request.FirstHeader(CSeqHeader).AsString,
                'CSeq');
    CheckEquals('Date: Thu, 1 Jan 1970 00:00:00 GMT',
                Self.Request.FirstHeader(DateHeader).AsString,
                'Date');
    CheckEquals('Error-Info: <http://www.error.com/info/bloop.wav>',
                Self.Request.FirstHeader(ErrorInfoHeader).AsString,
                'Error-Info');
    CheckEquals('Expires: 1000',
                Self.Request.FirstHeader(ExpiresHeader).AsString,
                'Expires');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Self.Request.FirstHeader(FromHeaderFull).AsString,
                'From');
    CheckEquals('Max-Forwards: 70',
                Self.Request.FirstHeader(MaxForwardsHeader).AsString,
                'Max-Forwards');
    CheckEquals('Record-Route: localhost <sip:127.0.0.1>;lr',
                Self.Request.FirstHeader(RecordRouteHeader).AsString,
                'Record-Route');
    CheckEquals('Route: localhost <sip:127.0.0.1>;lr',
                Self.Request.FirstHeader(RouteHeader).AsString,
                'Route');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>;tag=1928301775',
                Self.Request.FirstHeader(ToHeaderFull).AsString,
                'To');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds',
                Self.Request.FirstHeader(ViaHeaderFull).AsString,
                'Via');
    CheckEquals('Warning: 301 draugr "Not really interested"',
                Self.Request.FirstHeader(WarningHeader).AsString,
                'Warning');
    CheckEquals('X-Not-A-Header: I am not defined in RFC 3261',
                Self.Request.FirstHeader('X-Not-A-Header').AsString,
                'X-Not-A-Header');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseReallyLongViaHeader;
const
  IterCount = 49;
var
  Msg: TIdSipMessage;
  Str: TStringStream;
  S:   String;
  I, J: Integer;
begin
  S := 'Via: ';
  for I := 0 to IterCount do
    for J := 0 to IterCount do
      S := S + 'SIP/2.0/UDP 127.0.' + IntToStr(I) + '.' + IntToStr(J) + ',';
  Delete(S, Length(S), 1);

  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
               + 'Content-Type: text/plain'#13#10
               + 'Content-Length: 29'#13#10
               + S + #13#10
               + #13#10
               + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;

    Msg := Self.P.ParseAndMakeMessage;
    try
      Self.CheckEquals((IterCount+1)*(IterCount+1) + 1, Msg.Path.Length, 'Length');
    finally
      Msg.Free;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicRequest);
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);
    Self.CheckBasicRequest(Request);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestBadCSeq;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest,
                                            'CSeq: 314159 INVITE',
                                            'CSeq: 314159 REGISTER',
                                            []));
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(CSeqMethodMismatch,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out on parsing an empty string');
    except
      on EBadRequest do;
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestFoldedHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'From: Case'#13#10
                            + ' <sip:case@fried.neurons.org>'#13#10
                            + #9';tag=1928301774'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'CSeq: 8'#13#10
                            + '  INVITE'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Request.FirstHeader(FromHeaderFull).AsString,
                'From header');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedMaxForwards;
var
  Str: TStringStream;
begin
  // Section 20.22 states that 0 <= Max-Forwards <= 255
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Max-Forwards: 666'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out on a Bad Request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, [MaxForwardsHeader,
                                            'Max-Forwards: 666']),
                    E.Message,
                    'Exception type');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedMethod;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('Bad"method sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + #13#10);
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out on a Bad Request');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Method', 'Bad"method']), E.Message, 'Exception type');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMalformedRequestLine;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE  sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (too many spaces between Method and Request-URI) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces, E.Message, 'Too many spaces');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITEsip:wintermute@tessier-ashpool.co.luSIP/2.0'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (no spaces between Method and Request-URI) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'INVITEsip:wintermute@tessier-ashpool.co.luSIP/2.0']),
                    E.Message,
                    'Missing spaces');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('sip:wintermute@tessier-ashpool.co.lu SIP/2.0');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (no Method) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'sip:wintermute@tessier-ashpool.co.lu SIP/2.0']),
                    E.Message,
                    'Missing Method');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITE'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (no Request-URI, no SIP-Version) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(MalformedToken, ['Request-Line', 'INVITE']),
                    E.Message,
                    'Missing Request & SIP Version');
    end;
  finally
    Str.Free;
  end;

  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/;2.0'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (malformed SIP-Version) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(Format(InvalidSipVersion, ['SIP/;2.0']), E.Message, 'Malformed SIP-Version');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMessageBodyLongerThanContentLength;
var
  Leftovers: array[0..99] of Char;
  Str:       TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest,
                                            'Content-Length: 29',
                                            'Content-Length: 4',
                                            []));
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);
    CheckEquals(4,  Request.ContentLength, 'ContentLength');
    CheckEquals('', Request.Body,          'Body');

    CheckEquals(Length('I am a message. Hear me roar!'),
                Str.Read(LeftOvers, Length(Leftovers)),
                'Read unexpected number of bytes from the stream');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingCallID;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingCallID,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingCSeq;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingCSeq,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingFrom;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingFrom,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingMaxForwards;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingMaxForwards,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingTo;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingTo,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMissingVia;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Self.Request);
      Fail('Failed to bail out');
    except
      on E: EBadRequest do
        CheckEquals(MissingVia,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMultipleVias;
var
  Str:        TStringStream;
  Via0, Via1: TIdSipViaHeader;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org:5061;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Via: SIP/3.0/TLS gw5.cust1.leo_ix.org;branch=z9hG4bK776aheh'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);

    CheckEquals(2, Request.Path.Length, 'Path.Length');

    Via0 := Request.Path.Items[0] as TIdSipViaHeader;
    CheckEquals('Via',              Via0.Name,             'LastHop.Name');
    CheckEquals('SIP/2.0',          Via0.SipVersion,       'LastHop.SipVersion');
    Check      (sttTCP =            Via0.Transport,        'LastHop.Transport');
    CheckEquals('gw1.leo-ix.org',   Via0.SentBy,           'LastHop.SentBy');
    CheckEquals(5061,               Via0.Port,             'LastHop.Port');
    CheckEquals('z9hG4bK776asdhds', Via0.Params['branch'], 'LastHop.Params[''branch'']');
    CheckEquals('SIP/2.0/TCP gw1.leo-ix.org:5061',
                Via0.Value,
                'LastHop.Value');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org:5061;branch=z9hG4bK776asdhds',
                Via0.AsString,
                'LastHop.AsString');

    Via1 := Request.Path.Items[1] as TIdSipViaHeader;
    CheckEquals('Via',                   Via1.Name,             'Via1.Name');
    CheckEquals('SIP/3.0',               Via1.SipVersion,       'Via1.SipVersion');
    Check      (sttTLS =                 Via1.Transport,        'Via1.Transport');
    CheckEquals('gw5.cust1.leo_ix.org',  Via1.SentBy,           'Via1.SentBy');
    CheckEquals(IdPORT_SIPS,             Via1.Port,             'Via1.Port');
    CheckEquals('z9hG4bK776aheh',        Via1.Params['branch'], 'Via1.Params[''branch'']');
    CheckEquals('SIP/3.0/TLS gw5.cust1.leo_ix.org',
                Via1.Value,
                'Via1.Value');
    CheckEquals('Via: SIP/3.0/TLS gw5.cust1.leo_ix.org;branch=z9hG4bK776aheh',
                Via1.AsString,
                'Via1.AsString');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriHasSpaces;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier ashpool.co.lu SIP/2.0'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (Request-URI has spaces) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoSpaces,
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestRequestUriInAngleBrackets;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE <sip:wintermute@tessier-ashpool.co.lu> SIP/2.0'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseRequest(Request);
      Fail('Malformed start line (Request-URI enclosed in angle brackets) parsed without error');
    except
      on E: EBadRequest do
        CheckEquals(RequestUriNoAngleBrackets,
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestWithLeadingCrLfs;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(#13#10#13#10 + BasicRequest);
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);

    Self.CheckBasicRequest(Request);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestWithBodyAndNoContentType;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(EmptyRequest
                            + 'Content-Length: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out');
    except
      on E: EParser do
        CheckEquals(MissingContentType,
                    E.Message,
                    'Unexpected exception');
    end;

  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestWithMultipleRoutes;
const
  Route = 'Route: <sip:127.0.0.1>'#13#10
        + 'Route: wsfrank <sip:192.168.0.1>;low, <sip:192.168.0.1>'#13#10
        + BasicContentLengthHeader;
var
  N:   Cardinal;
  Str: TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest, BasicContentLengthHeader, Route, []));
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);

    N := Self.Request.HeaderCount - 1;
    CheckEquals('<sip:127.0.0.1>',           Self.Request.HeaderAt(N - 3).Value, '1st Route');
    CheckEquals('wsfrank <sip:192.168.0.1>', Self.Request.HeaderAt(N - 2).Value, '2nd Route');
    CheckEquals('<sip:192.168.0.1>',         Self.Request.HeaderAt(N - 1).Value, '3rd Route');

    CheckEquals(';low',
                Self.Request.HeaderAt(N - 2).ParamsAsString,
                '2nd Route''s params');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponse;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(BasicResponse);
  try
    Self.P.Source := Str;

    Self.P.ParseResponse(Response);
    Self.CheckBasicResponse(Response);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    Self.P.Source := Str;

//    try
      Self.P.ParseResponse(Response);

      CheckEquals('', Response.SipVersion, 'Sip-Version');
      CheckEquals(0,  Response.StatusCode, 'Status-Code');
      CheckEquals('', Response.StatusText, 'Status-Text');
//      Fail('Failed to bail out on parsing an empty string');
//    except
//      on EBadResponse do;
//    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseFoldedHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/1.0 200 OK'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + 'Via: SIP/2.0/TCP gw1.leo-ix.org'#13#10
                          + 'CSeq: 271828 INVITE'#13#10
                          + 'Call-ID: cafebabe@sip.neurons.org'#13#10
                          + #13#10);
  try
    Self.P.Source := Str;
    Self.P.ParseResponse(Response);

    CheckEquals('SIP/1.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusTest');

    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Response.FirstHeader(FromHeaderFull).AsString,
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Response.FirstHeader(ToHeaderFull).AsString,
                'To header');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseInvalidStatusCode;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/1.0 Aheh OK'#13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Response);
      Fail('Failed to reject a non-numeric Status-Code');
    except
      on E: EBadResponse do
        CheckEquals(Format(InvalidStatusCode, ['Aheh']),
                    E.Message,
                    '<>');
    end;
  finally
    Str.Free;
  end;
end;
{
procedure TestTIdSipParser.TestParseResponseMissingCallID;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Self.Response);
      Fail('Failed to bail out');
    except
      on E: EBadResponse do
        CheckEquals(MissingCallID,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseMissingCSeq;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Self.Response);
      Fail('Failed to bail out');
    except
      on E: EBadResponse do
        CheckEquals(MissingCSeq,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseMissingFrom;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Self.Response);
      Fail('Failed to bail out');
    except
      on E: EBadResponse do
        CheckEquals(MissingFrom,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseMissingMaxForwards;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;

    Self.P.ParseResponse(Self.Response);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseMissingTo;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Self.Response);
      Fail('Failed to bail out');
    except
      on E: EBadResponse do
        CheckEquals(MissingTo,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseResponseMissingVia;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('SIP/2.0 200 OK'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: sip:wintermute@tessier-ashpool.co.lu'#13#10
                            + 'Content-Type: text/plain'#13#10
                            + 'Content-Length: 29'#13#10
                            + #13#10);
  try
    Self.P.Source := Str;
    try
      Self.P.ParseResponse(Self.Response);
      Fail('Failed to bail out');
    except
      on E: EBadResponse do
        CheckEquals(MissingVia,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;
}
procedure TestTIdSipParser.TestParseResponseWithLeadingCrLfs;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(#13#10#13#10 + BasicResponse);
  try
    Self.P.Source := Str;
    Self.P.ParseResponse(Response);

    Self.CheckBasicResponse(Response);
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseShortFormContentLength;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(StringReplace(BasicRequest,
                                            'Content-Length',
                                            'l',
                                            [rfReplaceAll]));
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);
    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest1;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest1);
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);
    
    CheckEquals('INVITE',                              Request.Method,                 'Method');
    CheckEquals('SIP/2.0',                             Request.SipVersion,             'SipVersion');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Request.RequestUri.GetFullURI,  'RequestUri');
    CheckEquals(6,                                     Request.MaxForwards,            'MaxForwards');
    CheckEquals('0ha0isndaksdj@10.1.1.1',              Request.CallID,                 'CallID');

    CheckEquals('',
                Request.ToHeader.DisplayName,
                'ToHeader.DisplayName');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com',
                Request.ToHeader.Address.GetFullURI,
                'ToHeader.Address.GetFullURI');
    CheckEquals(';tag=1918181833n',
                Request.ToHeader.ParamsAsString,
                'ToHeader.ParamsAsString');

    CheckEquals('J Rosenberg \"',
                Request.From.DisplayName,
                'From.DisplayName');
    CheckEquals('sip:jdrosen@lucent.com',
                Request.From.Address.GetFullURI,
                'From.Address.GetFullURI');
    CheckEquals(';tag=98asjd8',
                Request.From.ParamsAsString,
                'From.ParamsAsString');

    CheckEquals(3, Request.Path.Length, 'Path.Length');

    CheckEquals('To: sip:vivekg@chair.dnrc.bell-labs.com;tag=1918181833n',
                Request.HeaderAt(0).AsString,
                'To header');
    CheckEquals('From: "J Rosenberg \\\"" <sip:jdrosen@lucent.com>;tag=98asjd8',
                Request.HeaderAt(1).AsString,
                'From header');
    CheckEquals('Max-Forwards: 6',
                Request.HeaderAt(2).AsString,
                'Max-Forwards header');
    CheckEquals('Call-ID: 0ha0isndaksdj@10.1.1.1',
                Request.HeaderAt(3).AsString,
                'Call-ID header');
    CheckEquals('CSeq: 8 INVITE',
                Request.HeaderAt(4).AsString,
                'CSeq header');
    CheckEquals('Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw',
                Request.HeaderAt(5).AsString,
                'Via header #1');
    CheckEquals('Subject: ',
                Request.HeaderAt(6).AsString,
                'Subject header');
    CheckEquals('NewFangledHeader: newfangled value more newfangled value',
                Request.HeaderAt(7).AsString,
                'NewFangledHeader');
    CheckEquals('Content-Type: application/sdp',
                Request.HeaderAt(8).AsString,
                'Content-Type');
    CheckEquals('Via: SIP/2.0/TCP 1192.168.156.222;branch=9ikj8',
                Request.HeaderAt(9).AsString,
                'Via header #2');
    CheckEquals('Via: SIP/2.0/UDP 192.168.255.111;hidden',
                Request.HeaderAt(10).AsString,
                'Via header #3');
    CheckEquals('Contact: "Quoted string \"\"" <sip:jdrosen@bell-labs.com>;newparam=newvalue;secondparam=secondvalue;q=0.33',
                Request.HeaderAt(11).AsString,
                'Contact header #1');
    CheckEquals('Contact: tel:4443322',
                Request.HeaderAt(12).AsString,
                'Contact header #2');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest8;
begin
  Self.CheckTortureTest(TortureTest8, CSeqMethodMismatch);
end;

procedure TestTIdSipParser.TestTortureTest11;
begin
  Self.CheckTortureTest(TortureTest11, MissingCallID);
end;

procedure TestTIdSipParser.TestTortureTest13;
begin
  Self.CheckTortureTest(TortureTest13,
                        Format(MalformedToken, [ExpiresHeader,
                                                'Expires: Thu, 44 Dec 19999 16:00:00 EDT']));
end;

procedure TestTIdSipParser.TestTortureTest15;
begin
  Self.CheckTortureTest(TortureTest15,
                        Format(MalformedToken, [ViaHeaderFull,
                                                'Via: SIP/2.0/UDP 135.180.130.133;;,;']));
end;

procedure TestTIdSipParser.TestTortureTest19;
begin
  Self.CheckTortureTest(TortureTest19,
                        Format(MalformedToken, [ToHeaderFull,
                                                'To: "Mr. J. User <sip:j.user@company.com>']));
end;

procedure TestTIdSipParser.TestTortureTest21;
begin
  Self.CheckTortureTest(TortureTest21, RequestUriNoAngleBrackets);
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
  Str: TStringStream;
begin
  Str := TStringStream.Create(TortureTest24);
  try
    Self.P.Source := Str;
    Self.P.ParseRequest(Request);

    CheckEquals('sip:sip%3Auser%40example.com@company.com;other-param=summit',
                Request.RequestUri.GetFullURI,
                'Request-URI');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestTortureTest35;
begin
  Self.CheckTortureTest(TortureTest35,
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

procedure TestTIdSipParser.CheckBasicMessage(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
begin
  CheckEquals('SIP/2.0',                              Msg.SIPVersion,                   'SipVersion');
  CheckEquals(29,                                     Msg.ContentLength,                'ContentLength');
  CheckEquals('text/plain',                           Msg.ContentType,                  'ContentType');
  CheckEquals(70,                                     Msg.MaxForwards,                  'MaxForwards');
  CheckEquals('a84b4c76e66710@gw1.leo-ix.org',        Msg.CallID,                       'CallID');
  CheckEquals('Wintermute',                           Msg.ToHeader.DisplayName,         'ToHeader.DisplayName');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Msg.ToHeader.Address.GetFullURI,  'ToHeader.Address.GetFullURI');
  CheckEquals(';tag=1928301775',                      Msg.ToHeader.ParamsAsString,      'Msg.ToHeader.ParamsAsString');
  CheckEquals('Case',                                 Msg.From.DisplayName,             'From.DisplayName');
  CheckEquals('sip:case@fried.neurons.org',           Msg.From.Address.GetFullURI,      'From.Address.GetFullURI');
  CheckEquals(';tag=1928301774',                      Msg.From.ParamsAsString,          'Msg.From.ParamsAsString');
  CheckEquals(314159,                                 Msg.CSeq.SequenceNo,              'Msg.CSeq.SequenceNo');
  CheckEquals('INVITE',                               Msg.CSeq.Method,                  'Msg.CSeq.Method');

  CheckEquals(1,                  Msg.Path.Length,              'Path.Length');
  CheckEquals('SIP/2.0',          Msg.LastHop.SipVersion,       'LastHop.SipVersion');
  Check      (sttTCP =            Msg.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo-ix.org',   Msg.LastHop.SentBy,           'LastHop.SentBy');
  CheckEquals(IdPORT_SIP,         Msg.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>;tag=1928301775',
              Msg.FirstHeader(ToHeaderFull).AsString,
              'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
              Msg.FirstHeader(FromHeaderFull).AsString,
              'From');
  CheckEquals('CSeq: 314159 INVITE',
              Msg.FirstHeader(CSeqHeader).AsString,
              'CSeq');
  CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',
              Msg.FirstContact.AsString,
              'Contact');
  CheckEquals('Content-Type: text/plain',
              Msg.FirstHeader(ContentTypeHeaderFull).AsString,
              'Content-Type');
  CheckEquals(9, Msg.HeaderCount, 'Header count');

  if CheckBody then
    CheckEquals('', Msg.Body, 'message-body');
end;

procedure TestTIdSipParser.CheckBasicRequest(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
begin
  CheckEquals(TIdSipRequest.Classname, Msg.ClassName, 'Class type');

  CheckEquals('INVITE',
              (Msg as TIdSipRequest).Method,
              'Method');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
              (Msg as TIdSipRequest).RequestUri.GetFullURI,
              'Request-URI');

  Self.CheckBasicMessage(Msg, CheckBody);
end;

procedure TestTIdSipParser.CheckBasicResponse(const Msg: TIdSipMessage; const CheckBody: Boolean = true);
begin
  CheckEquals(TIdSipResponse.Classname, Msg.ClassName, 'Class type');

  CheckEquals(486,         TIdSipResponse(Msg).StatusCode, 'StatusCode');
  CheckEquals('Busy Here', TIdSipResponse(Msg).StatusText, 'StatusText');

  Self.CheckBasicMessage(Msg, CheckBody);
end;

procedure TestTIdSipParser.CheckTortureTest(const RequestStr, ExpectedExceptionMsg: String);
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(RequestStr);
  try
    Self.P.Source := Str;

    try
      Self.P.ParseRequest(Request);
      Fail('Failed to bail out of a bad request');
    except
      on E: EBadRequest do
        CheckEquals(ExpectedExceptionMsg,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    Str.Free;
  end;
end;

initialization
  RegisterTest('SIP Request Parsing', Suite);
end.
