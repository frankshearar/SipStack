unit TestIdSipParser;

interface

uses
  Classes, IdSipMessage, IdSipParser, TestFramework, TestFrameworkEx;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestIsEqual;
    procedure TestQValueToStr;
    procedure TestShortMonthToInt;
    procedure TestStrToQValue;
    procedure TestStrToTransport;
    procedure TestTransportToStr;
  end;

  TestTIdSipParser = class(TTestCase)
  private
    P:        TIdSipParser;
    Request:  TIdSipRequest;
    Response: TIdSipResponse;

    procedure CheckBasicMessage(const Msg: TIdSipMessage);
    procedure CheckBasicRequest(const Msg: TIdSipMessage);
    procedure CheckBasicResponse(const Msg: TIdSipMessage);
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
    procedure TestIsQValue;
    procedure TestIsSipVersion;
    procedure TestIsToken;
    procedure TestIsTransport;
    procedure TestIsWord;
    procedure TestParseAndMakeMessageEmptyString;
    procedure TestParseAndMakeMessageMalformedRequest;
    procedure TestParseAndMakeMessageRequest;
    procedure TestParseAndMakeMessageResponse;
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
  DateUtils, IdSimpleParser, SysUtils, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipParser tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestIsEqual;
begin
  Check(    IsEqual('', ''),         ''''' & ''''');
  Check(not IsEqual(' ', ''),        ''' '' & ''''');
  Check(    IsEqual('abcd', 'AbCd'), '''abcd'', ''AbCd''');
  Check(not IsEqual('absd', 'Abcd'), '''absd'', ''Abcd''');
end;

procedure TestFunctions.TestQValueToStr;
begin
  CheckEquals('0',     QValueToStr(0),    'QValueToStr(0)');
  CheckEquals('0.001', QValueToStr(1),    'QValueToStr(1)');
  CheckEquals('0.01',  QValueToStr(10),   'QValueToStr(10)');
  CheckEquals('0.1',   QValueToStr(100),  'QValueToStr(100)');
  CheckEquals('0.666', QValueToStr(666),  'QValueToStr(666)');
  CheckEquals('1',     QValueToStr(1000), 'QValueToStr(1000)');
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

procedure TestFunctions.TestStrToQValue;
begin
  CheckEquals(0,    StrToQValue('0'),     'StrToQValue(''0'')');
  CheckEquals(0,    StrToQValue('0.0'),   'StrToQValue(''0.0'')');
  CheckEquals(0,    StrToQValue('0.00'),  'StrToQValue(''0.00'')');
  CheckEquals(0,    StrToQValue('0.000'), 'StrToQValue(''0.000'')');
  CheckEquals(666,  StrToQValue('0.666'), 'StrToQValue(''0.666'')');
  CheckEquals(1000, StrToQValue('1'),     'StrToQValue(''1'')');
  CheckEquals(1000, StrToQValue('1.0'),   'StrToQValue(''1.0'')');
  CheckEquals(1000, StrToQValue('1.00'),  'StrToQValue(''1.00'')');
  CheckEquals(1000, StrToQValue('1.000'), 'StrToQValue(''1.000'')');

  try
    StrToQValue('.');
    Fail('Failed to bail out on malformed q (.');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''.'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.');
    Fail('Failed to bail out on malformed q (0.');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0.'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0. 0');
    Fail('Failed to bail out on malformed q (0. 0)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0. 0'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.');
    Fail('Failed to bail out on malformed q (1.');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''1.'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0000');
    Fail('Failed to bail out on too many digits (0.0000)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0.0000'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0123');
    Fail('Failed to bail out on too many digits (0.0123)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0.0123'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.1234');
    Fail('Failed to bail out on too many digits (0.1234)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0.1234'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.a');
    Fail('Failed to bail out on letters');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''0.a'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.1');
    Fail('Failed to bail out on number too big (1.1)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''1.1'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('3');
    Fail('Failed to bail out on number too big (3)');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert ''3'' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('');
    Fail('Failed to bail out on empty string');
  except
    on E: EConvertError do
      CheckEquals('Failed to convert '''' to type TIdSipQValue',
                  E.Message,
                  'Unexpected exception');
  end;
end;

procedure TestFunctions.TestStrToTransport;
begin
  Check(sttSCTP = StrToTransport('SCTP'), 'SCTP');
  Check(sttTCP  = StrToTransport('TCP'),  'TCP');
  Check(sttTLS  = StrToTransport('TLS'),  'TLS');
  Check(sttUDP  = StrToTransport('UDP'),  'UDP');

  try
    StrToTransport('not a transport');
    Fail('Failed to bail out on an unknown transport type');
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestTransportToStr;
var
  T: TIdSipTransportType;
begin
  for T := Low(TIdSipTransportType) to High(TIdSipTransportType) do
    Check(T = StrToTransport(TransportToStr(T)), 'Ord(T) = ' + IntToStr(Ord(T)));
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

procedure TestTIdSipParser.TestParseAndMakeMessageEmptyString;
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

procedure TestTIdSipParser.TestParseExtensiveRequest;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create(ExtensiveRequest);
  try
    Self.P.Source := Str;

    Self.P.ParseRequest(Request);
//    CheckEquals('
    CheckEquals('a84b4c76e66710@gw1.leo-ix.org',
                Self.Request.Headers[CallIdHeaderFull].Value,
                'Call-ID');
    CheckEquals(TIdSipContactHeader.ClassName,
                Self.Request.Headers[ContactHeaderFull].ClassName,
                'Contact class');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                Self.Request.Headers[ContactHeaderFull].Value,
                'Contact');
    CheckEquals('',
                (Self.Request.Headers[ContactHeaderFull] as TIdSipAddressHeader).DisplayName,
                'Contact DisplayName');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                (Self.Request.Headers[ContactHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'Contact Address');
    CheckEquals(TIdSipNumericHeader.ClassName,
                Self.Request.Headers[ContentLengthHeaderFull].ClassName,
                'Content-Length class');
    CheckEquals(29,
                (Self.Request.Headers[ContentLengthHeaderFull] as TIdSipNumericHeader).NumericValue,
                'Content-Length');
    CheckEquals('text/plain',
                Self.Request.Headers[ContentTypeHeaderFull].Value,
                'Content-Type');
    CheckEquals(TIdSipCSeqHeader.ClassName,
                Self.Request.Headers[CSeqHeader].ClassName,
                'CSeq class');
    CheckEquals(314159,
                (Self.Request.Headers[CSeqHeader] as TIdSipCSeqHeader).SequenceNo,
                'CSeq SequenceNo');
    CheckEquals('INVITE',
                (Self.Request.Headers[CSeqHeader] as TIdSipCSeqHeader).Method,
                'CSeq Method');
    CheckEquals(TIdSipDateHeader.ClassName,
                Self.Request.Headers[DateHeader].ClassName,
                'Date class');
    CheckEquals('Thu, 1 Jan 1970 00:00:00 +0000',
                (Self.Request.Headers[DateHeader] as TIdSipDateHeader).Time.GetAsRFC822,
                'Date');
    CheckEquals(TIdSipNumericHeader.ClassName,
                Self.Request.Headers[ExpiresHeader].ClassName,
                'Expires class');
    CheckEquals(1000,
                (Self.Request.Headers[ExpiresHeader] as TIdSipNumericHeader).NumericValue,
                'Expires');
    CheckEquals(TIdSipFromToHeader.ClassName,
                Self.Request.Headers[FromHeaderFull].ClassName,
                'From class');
    CheckEquals('Case',
                (Self.Request.Headers[FromHeaderFull] as TIdSipAddressHeader).DisplayName,
                'From DisplayName');
    CheckEquals('sip:case@fried.neurons.org',
                (Self.Request.Headers[FromHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'From Address');
    CheckEquals(';tag=1928301774',
                Self.Request.Headers[FromHeaderFull].ParamsAsString,
                'From parameters');
    CheckEquals(TIdSipMaxForwardsHeader.ClassName,
                Self.Request.Headers[MaxForwardsHeader].ClassName,
                'Max-Forwards class');
    CheckEquals(70,
                (Self.Request.Headers[MaxForwardsHeader] as TIdSipMaxForwardsHeader).NumericValue,
                'Max-Forwards');
    CheckEquals(TIdSipRouteHeader.ClassName,
                Self.Request.Headers[RouteHeader].ClassName,
                'Route class');
    CheckEquals('localhost <sip:127.0.0.1>',
                Self.Request.Headers[RouteHeader].Value,
                'Route value');
    CheckEquals(';lr',
                Self.Request.Headers[RouteHeader].ParamsAsString,
                'Route params');
    CheckEquals(TIdSipFromToHeader.ClassName,
                Self.Request.Headers[ToHeaderFull].ClassName,
                'To class');
    CheckEquals('Wintermute',
                (Self.Request.Headers[ToHeaderFull] as TIdSipAddressHeader).DisplayName,
                ' DisplayName');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu',
                (Self.Request.Headers[ToHeaderFull] as TIdSipAddressHeader).Address.GetFullURI,
                'To Address');
    CheckEquals(TIdSipViaHeader.ClassName,
                Self.Request.Headers[ViaHeaderFull].ClassName,
                'Via class');
    CheckEquals('gw1.leo-ix.org',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Host,
                'Via Host');
    CheckEquals(5060,
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Port,
                'Via Port');
    CheckEquals('SIP/2.0',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).SipVersion,
                'Via SipVersion');
    Check       (sttTCP =
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).Transport,
                'Via Transport');
    CheckEquals(';branch=z9hG4bK776asdhds',
                (Self.Request.Headers[ViaHeaderFull] as TIdSipViaHeader).ParamsAsString,
                'Via Parameters');
    CheckEquals('I am not defined in RFC 3261',
                Self.Request.Headers['X-Not-A-Header'].Value,
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
                Request.Headers['from'].AsString,
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
  Str: TStringStream;
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

    CheckEquals(2,                       Request.Path.Length,                   'Path.Length');
    Check      (Request.Path.FirstHop <> Request.Path.LastHop,                  'Sanity check on Path');
    CheckEquals('Via',                   Request.Path.LastHop.Name,             'LastHop.Name');
    CheckEquals('SIP/2.0',               Request.Path.LastHop.SipVersion,       'LastHop.SipVersion');
    Check      (sttTCP =                 Request.Path.LastHop.Transport,        'LastHop.Transport');
    CheckEquals('gw1.leo-ix.org',        Request.Path.LastHop.Host,             'LastHop.Host');
    CheckEquals(5061,                    Request.Path.LastHop.Port,             'LastHop.Port');
    CheckEquals('z9hG4bK776asdhds',      Request.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');
    CheckEquals('SIP/2.0/TCP gw1.leo-ix.org:5061',
                Request.Path.LastHop.Value,
                'LastHop.Value');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo-ix.org:5061;branch=z9hG4bK776asdhds',
                Request.Path.LastHop.AsString,
                'LastHop.AsString');

    CheckEquals('Via',                   Request.Path.FirstHop.Name,             'FirstHop.Name');
    CheckEquals('SIP/3.0',               Request.Path.FirstHop.SipVersion,       'FirstHop.SipVersion');
    Check      (sttTLS =                 Request.Path.FirstHop.Transport,        'FirstHop.Transport');
    CheckEquals('gw5.cust1.leo_ix.org',  Request.Path.FirstHop.Host,             'FirstHop.Host');
    CheckEquals(IdPORT_SIP_TLS,          Request.Path.FirstHop.Port,             'FirstHop.Port');
    CheckEquals('z9hG4bK776aheh',        Request.Path.FirstHop.Params['branch'], 'FirstHop.Params[''branch'']');
    CheckEquals('SIP/3.0/TLS gw5.cust1.leo_ix.org',
                Request.Path.FirstHop.Value,
                'FirstHop.Value');
    CheckEquals('Via: SIP/3.0/TLS gw5.cust1.leo_ix.org;branch=z9hG4bK776aheh',
                Request.Path.FirstHop.AsString,
                'FirstHop.AsString');
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

    N := Self.Request.Headers.Count - 1;
    CheckEquals('<sip:127.0.0.1>',           Self.Request.Headers.Items[N - 3].Value, '1st Route');
    CheckEquals('wsfrank <sip:192.168.0.1>', Self.Request.Headers.Items[N - 2].Value, '2nd Route');
    CheckEquals('<sip:192.168.0.1>',         Self.Request.Headers.Items[N - 1].Value, '3rd Route');

    CheckEquals(';low',
                Self.Request.Headers.Items[N - 2].ParamsAsString,
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
    Self.P.ParseResponse(Response);

    CheckEquals(0,  Response.StatusCode, 'StatusCode');
    CheckEquals('', Response.StatusText, 'StatusText');
    CheckEquals('', Response.SipVersion, 'SipVersion');
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
                          + #13#10);
  try
    Self.P.Source := Str;
    Self.P.ParseResponse(Response);

    CheckEquals('SIP/1.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusTest');

//    CheckEquals(2, Response.Headers.Count, 'Header count');
    CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',
                Response.Headers['from'].AsString,
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Response.Headers['to'].AsString,
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
    
    CheckEquals('INVITE',                              Request.Method,      'Method');
    CheckEquals('SIP/2.0',                             Request.SipVersion,  'SipVersion');
    CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Request.Request,     'Request');
    CheckEquals(6,                                     Request.MaxForwards, 'MaxForwards');
    CheckEquals('0ha0isndaksdj@10.1.1.1',              Request.CallID,      'CallID');

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
                Request.Headers.Items[0].AsString,
                'To header');
    CheckEquals('From: "J Rosenberg \\\"" <sip:jdrosen@lucent.com>;tag=98asjd8',
                Request.Headers.Items[1].AsString,
                'From header');
    CheckEquals('Max-Forwards: 6',
                Request.Headers.Items[2].AsString,
                'Max-Forwards header');
    CheckEquals('Call-ID: 0ha0isndaksdj@10.1.1.1',
                Request.Headers.Items[3].AsString,
                'Call-ID header');
    CheckEquals('CSeq: 8 INVITE',
                Request.Headers.Items[4].AsString,
                'CSeq header');
    CheckEquals('Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw',
                Request.Headers.Items[5].AsString,
                'Via header #1');
    CheckEquals('Subject: ',
                Request.Headers.Items[6].AsString,
                'Subject header');
    CheckEquals('NewFangledHeader: newfangled value more newfangled value',
                Request.Headers.Items[7].AsString,
                'NewFangledHeader');
    CheckEquals('Content-Type: application/sdp',
                Request.Headers.Items[8].AsString,
                'Content-Type');
    CheckEquals('Via: SIP/2.0/TCP 1192.168.156.222;branch=9ikj8',
                Request.Headers.Items[9].AsString,
                'Via header #2');
    CheckEquals('Via: SIP/2.0/UDP 192.168.255.111;hidden',
                Request.Headers.Items[10].AsString,
                'Via header #3');
    CheckEquals('Contact: "Quoted string \"\"" <sip:jdrosen@bell-labs.com>;newparam=newvalue;secondparam=secondvalue;q=0.33',
                Request.Headers.Items[11].AsString,
                'Contact header #1');
    CheckEquals('Contact: tel:4443322',
                Request.Headers.Items[12].AsString,
                'Contact header #2');
//    CheckEquals(13, Request.Headers.Count, 'Header count');
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
                Request.Request,
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

procedure TestTIdSipParser.CheckBasicMessage(const Msg: TIdSipMessage);
begin
  CheckEquals('SIP/2.0',                              Msg.SIPVersion,                   'SipVersion');
  CheckEquals(29,                                     Msg.ContentLength,                'ContentLength');
  CheckEquals('text/plain',                           Msg.ContentType,                  'ContentType');
  CheckEquals(70,                                     Msg.MaxForwards,                  'MaxForwards');
  CheckEquals('a84b4c76e66710@gw1.leo-ix.org',        Msg.CallID,                       'CallID');
  CheckEquals('Wintermute',                           Msg.ToHeader.DisplayName,         'ToHeader.DisplayName');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Msg.ToHeader.Address.GetFullURI,  'ToHeader.Address.GetFullURI');
  CheckEquals('',                                     Msg.ToHeader.ParamsAsString,      'Msg.ToHeader.ParamsAsString');
  CheckEquals('Case',                                 Msg.From.DisplayName,             'From.DisplayName');
  CheckEquals('sip:case@fried.neurons.org',           Msg.From.Address.GetFullURI,      'From.Address.GetFullURI');
  CheckEquals(';tag=1928301774',                      Msg.From.ParamsAsString,          'Msg.From.ParamsAsString');
  CheckEquals(314159,                                 Msg.CSeq.SequenceNo,              'Msg.CSeq.SequenceNo');
  CheckEquals('INVITE',                               Msg.CSeq.Method,                  'Msg.CSeq.Method');

  CheckEquals(TIdSipContactHeader.ClassName,
              Msg.Headers[ContactHeaderFull].ClassName,
              'Contact header type');

  CheckEquals(1,                  Msg.Path.Length,                   'Path.Length');
  Check      (Msg.Path.FirstHop = Msg.Path.LastHop,                  'Sanity check on Path');
  CheckEquals('SIP/2.0',          Msg.Path.LastHop.SipVersion,       'LastHop.SipVersion');
  Check      (sttTCP =            Msg.Path.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo-ix.org',   Msg.Path.LastHop.Host,             'LastHop.Host');
  CheckEquals(IdPORT_SIP,         Msg.Path.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',  Msg.Headers['to'].AsString,           'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774', Msg.Headers['from'].AsString,         'From');
  CheckEquals('CSeq: 314159 INVITE',                                    Msg.Headers['cseq'].AsString,         'CSeq');
  CheckEquals('Contact: sip:wintermute@tessier-ashpool.co.lu',          Msg.Headers['contact'].AsString,      'Contact');
  CheckEquals('Content-Type: text/plain',                               Msg.Headers['content-type'].AsString, 'Content-Type');
  CheckEquals(9, Msg.Headers.Count, 'Header count');

  CheckEquals('', Msg.Body, 'message-body');
end;

procedure TestTIdSipParser.CheckBasicRequest(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipRequest.Classname, Msg.ClassName, 'Class type');

  CheckEquals('INVITE',                               TIdSipRequest(Msg).Method,  'Method');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', TIdSipRequest(Msg).Request, 'Request');

  Self.CheckBasicMessage(Msg);
end;

procedure TestTIdSipParser.CheckBasicResponse(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipResponse.Classname, Msg.ClassName, 'Class type');

  CheckEquals(486,         TIdSipResponse(Msg).StatusCode, 'StatusCode');
  CheckEquals('Busy Here', TIdSipResponse(Msg).StatusText, 'StatusText');

  Self.CheckBasicMessage(Msg);
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
