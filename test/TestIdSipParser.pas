unit TestIdSipParser;

interface

uses
  Classes, IdSipParser, TestFramework, TestFrameworkEx;

type
  TestFunctions = class(TTestCase)
  private
  public
  published
    procedure TestStrToTransport;
    procedure TestTransportToStr;
  end;

  TestTIdSipViaHeader = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsEqualTo;
  end;

  TestTIdSipPath = class(TTestCase)
  private
    Path: TIdSipPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndLastHop;
  end;

  TestTIdSipRequest = class(TExtendedTestCase)
  private
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestReadBody;
  end;

  TestTIdSipResponse = class(TExtendedTestCase)
  private
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestReadBody;
  end;


  TestTIdSipParser = class(TTestCase)
  private
    P:             TIdSipParser;
    Request:       TIdSipRequest;
    Response:      TIdSipResponse;

    procedure CheckBasicRequest(const Msg: TIdSipMessage);
    procedure CheckBasicResponse(const Msg: TIdSipMessage);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaseInsensitivityOfContentLengthHeader;
    procedure TestInviteParserTortureTestMessage;
    procedure TestIsContentLength;
    procedure TestIsEqual;
    procedure TestIsMaxForwards;
    procedure TestIsMethod;
    procedure TestIsNumber;
    procedure TestIsSipVersion;
    procedure TestIsVia;
    procedure TestParseAndMakeMessageEmptyString;
    procedure TestParseAndMakeMessageMalformedRequest;
    procedure TestParseAndMakeMessageRequest;
    procedure TestParseAndMakeMessageResponse;
    procedure TestParseRequest;
    procedure TestParseRequestEmptyString;
    procedure TestParseRequestFoldedHeader;
    procedure TestParseRequestMalformedMaxForwards;
    procedure TestParseRequestMalformedMethod;
    procedure TestParseRequestMalformedRequestLine;
    procedure TestParseRequestMessageBodyLongerThanContentLength;
    procedure TestParseRequestMultipleVias;
    procedure TestParseRequestRequestUriHasSpaces;
    procedure TestParseRequestRequestUriInAngleBrackets;
    procedure TestParseRequestWithLeadingCrLfs;
    procedure TestParseResponse;
    procedure TestParseResponseEmptyString;
    procedure TestParseResponseFoldedHeader;
    procedure TestParseResponseInvalidStatusCode;
    procedure TestParseResponseWithLeadingCrLfs;
    procedure TestParseShortFormContentLength;
    procedure TestPeek;
    procedure TestPeekLine;
    procedure TestReadOctet;
    procedure TestReadOctets;
    procedure TestReadln;
    procedure TestReadlnDoubleCrLf;
    procedure TestReadlnWithNoCrLf;
  end;

const
  BasicRequest = 'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
               + 'Content-Length: 29'#13#10
               + #13#10
               + 'I am a message. Hear me roar!';
  BasicResponse = 'SIP/2.0 486 Busy Here'#13#10
                + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                + 'Max-Forwards: 70'#13#10
                + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                + 'CSeq: 314159 INVITE'#13#10
                + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                + 'Content-Length: 29'#13#10
                + #13#10
                + 'I am a message. Hear me roar!';

implementation

uses
  SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('SipParser tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipViaHeader.Suite);
  Result.AddTest(TestTIdSipPath.Suite);
  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
  Result.AddTest(TestTIdSipParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

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
//* TestTIdSipViaHeader                                                        *
//******************************************************************************
//* TestTIdSipViaHeader Public methods *****************************************

procedure TestTIdSipViaHeader.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipViaHeader.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipViaHeader Published methods **************************************

procedure TestTIdSipViaHeader.TestIsEqualTo;
var
  Hop1, Hop2: TIdSipViaHeader;
begin
  Hop1 := TIdSipViaHeader.Create;
  try
    Hop2 := TIdSipViaHeader.Create;
    try
      Hop1.Host       := '127.0.0.1';
      Hop1.Port       := 5060;
      Hop1.SipVersion := 'SIP/2.0';
      Hop1.Transport  := sttSCTP;

      Hop2.Host       := '127.0.0.1';
      Hop2.Port       := 5060;
      Hop2.SipVersion := 'SIP/2.0';
      Hop2.Transport  := sttSCTP;

      Check(Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2)');
      Check(Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1)');

      Hop1.Host := '127.0.0.2';
      Check(not Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); Host');
      Check(not Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); Host');
      Hop1.Host := '127.0.0.1';
      Check(Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); Host reset');
      Check(Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); Host reset');

      Hop1.Port := 111;
      Check(not Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); Port');
      Check(not Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); Port');
      Hop1.Port := 5060;
      Check(Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); Port reset');
      Check(Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); Port reset');

      Hop1.SipVersion := 'xxx';
      Check(not Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); SipVersion');
      Check(not Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); SipVersion');
      Hop1.SipVersion := 'SIP/2.0';
      Check(Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); SipVersion reset');
      Check(Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); SipVersion reset');

      Hop1.Transport := sttTCP;
      Check(not Hop1.IsEqualTo(Hop2), 'Hop1.IsEqualTo(Hop2); Transport');
      Check(not Hop2.IsEqualTo(Hop1), 'Hop2.IsEqualTo(Hop1); Transport');
    finally
      Hop2.Free;
    end;
  finally
    Hop1.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipPath                                                             *
//******************************************************************************
//* TestTIdSipPath Public methods **********************************************

procedure TestTIdSipPath.SetUp;
begin
  inherited SetUp;

  Self.Path := TIdSipPath.Create;
end;

procedure TestTIdSipPath.TearDown;
begin
  Self.Path.Free;

  inherited TearDown;
end;

//* TestTIdSipPath Published methods *******************************************

procedure TestTIdSipPath.TestAddAndLastHop;
var
  Hop: TIdSipViaHeader;
begin
  CheckEquals(0, Self.Path.Length, 'Has hops, but is newly created');

  Hop := TIdSipViaHeader.Create;
  try
    Hop.Host       := '127.0.0.1';
    Hop.Port       := 5060;
    Hop.SipVersion := 'SIP/2.0';
    Hop.Transport  := sttSCTP;

    Self.Path.Add(Hop);
    CheckEquals(1, Self.Path.Length, 'Has no hops after Add');
    CheckEquals('127.0.0.1', Self.Path.LastHop.Host, 'Host');
    CheckEquals(5060, Self.Path.LastHop.Port, 'Port');
    CheckEquals('SIP/2.0', Self.Path.LastHop.SipVersion, 'SipVersion');
    Check(sttSCTP = Self.Path.LastHop.Transport, 'Transport');
  finally
    Hop.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipRequest                                                          *
//******************************************************************************
//* TestTIdSipRequest Public methods *******************************************

procedure TestTIdSipRequest.SetUp;
begin
  inherited SetUp;

  Request := TIdSipRequest.Create;  
end;

procedure TestTIdSipRequest.TearDown;
begin
  Request.Free;

  inherited TearDown;
end;

//* TestTIdSipRequest Published methods ****************************************

procedure TestTIdSipRequest.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Request.Method        := 'INVITE';
  Request.Request       := 'sip:wintermute@tessier-ashpool.co.lu';
  Request.SIPVersion    := SIPVersion;
  Request.ContentLength := 29;
  Request.Body          := 'I am a message. Hear me roar!';
  Request.OtherHeaders.Add('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds');
  Request.OtherHeaders.Add('Max-Forwards: 70');
  Request.OtherHeaders.Add('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>');
  Request.OtherHeaders.Add('From: Case <sip:case@fried.neurons.org>;tag=1928301774');
  Request.OtherHeaders.Add('Call-ID: a84b4c76e66710@gw1.leo_ix.org');
  Request.OtherHeaders.Add('CSeq: 314159 INVITE');
  Request.OtherHeaders.Add('Contact: <sip:wintermute@tessier-ashpool.co.lu>');

  Expected := TStringList.Create;
  try
    Expected.Text := BasicRequest;
{
    Expected.Add('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0');
    Expected.Add('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds');
    Expected.Add('Max-Forwards: 70');
    Expected.Add('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>');
    Expected.Add('From: Case <sip:case@fried.neurons.org>;tag=1928301774');
    Expected.Add('Call-ID: a84b4c76e66710@gw1.leo_ix.org');
    Expected.Add('CSeq: 314159 INVITE');
    Expected.Add('Contact: <sip:wintermute@tessier-ashpool.co.lu>');
    Expected.Add('Content-Length: 29');
    Expected.Add('');
    Expected.Add('I am a message. Hear me roar!');
}
    Received := TStringList.Create;
    try
      Received.Text := Request.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseRequest(Request);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipRequest.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Request.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Request.ReadBody(Str);
    CheckEquals('Negotium', Request.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipResponse                                                         *
//******************************************************************************
//* TestTIdSipResponse Public methods ******************************************

procedure TestTIdSipResponse.SetUp;
begin
  inherited SetUp;

  Response := TIdSipResponse.Create;
end;

procedure TestTIdSipResponse.TearDown;
begin
  Response.Free;

  inherited TearDown;
end;

//* TestTIdSipResponse Published methods ***************************************

procedure TestTIdSipResponse.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Response.StatusCode    := 486;
  Response.StatusText    := 'Busy Here';
  Response.SIPVersion    := SIPVersion;
  Response.ContentLength := 29;
  Response.Body          := 'I am a message. Hear me roar!';
  Response.OtherHeaders.Add('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds');
  Response.OtherHeaders.Add('Max-Forwards: 70');
  Response.OtherHeaders.Add('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>');
  Response.OtherHeaders.Add('From: Case <sip:case@fried.neurons.org>;tag=1928301774');
  Response.OtherHeaders.Add('Call-ID: a84b4c76e66710@gw1.leo_ix.org');
  Response.OtherHeaders.Add('CSeq: 314159 INVITE');
  Response.OtherHeaders.Add('Contact: <sip:wintermute@tessier-ashpool.co.lu>');

  Expected := TStringList.Create;
  try
    Expected.Text := BasicResponse;
{
    Expected.Add('SIP/2.0 486 Busy Here');
    Expected.Add('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds');
    Expected.Add('Max-Forwards: 70');
    Expected.Add('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>');
    Expected.Add('From: Case <sip:case@fried.neurons.org>;tag=1928301774');
    Expected.Add('Call-ID: a84b4c76e66710@gw1.leo_ix.org');
    Expected.Add('CSeq: 314159 INVITE');
    Expected.Add('Contact: <sip:wintermute@tessier-ashpool.co.lu>');
    Expected.Add('Content-Length: 29');
    Expected.Add('');
    Expected.Add('I am a message. Hear me roar!');
}
    Received := TStringList.Create;
    try
      Received.Text := Response.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseResponse(Response);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipResponse.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Response.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Response.ReadBody(Str);
    CheckEquals('Negotium', Response.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
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
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Content-LENGTH: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;

    P.ParseRequest(Request);

    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestInviteParserTortureTestMessage;
var
  Str: TStringStream;
begin
  // note: this is not the COMPLETE message!
  Str := TStringStream.Create('INVITE sip:vivekg@chair.dnrc.bell-labs.com SIP/2.0'#13#10
                            + 'TO :'#13#10
                            + ' sip:vivekg@chair.dnrc.bell-labs.com ;   tag    = 1918181833n'#13#10
                            + 'From   : "J Rosenberg \\\"" <sip:jdrosen@lucent.com>'#13#10
                            + '  ;'#13#10
                            + '  tag = 98asjd8'#13#10
                            + 'Max-Forwards: 6'#13#10
                            + 'Call-ID: 0ha0isndaksdj@10.1.1.1'#13#10);
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals(6, Request.MaxForwards, 'Max-Forwards');
    CheckEquals('TO : sip:vivekg@chair.dnrc.bell-labs.com ;   tag    = 1918181833n',
                Request.OtherHeaders[0],
                'To header');
    CheckEquals('From   : "J Rosenberg \\\"" <sip:jdrosen@lucent.com>  ;  tag = 98asjd8',
                Request.OtherHeaders[1],
                'From header');
    CheckEquals('Call-ID: 0ha0isndaksdj@10.1.1.1', Request.OtherHeaders[2], 'Call-ID header');
    CheckEquals(3, Request.OtherHeaders.Count, 'Header count');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestIsContentLength;
begin
  Check(    P.IsContentLength('Content-Length'),         'Content-Length');
  Check(    P.IsContentLength(ContentLengthHeaderFull),  'ContentLengthFull constant');
  Check(    P.IsContentLength('l'),                      'l');
  Check(    P.IsContentLength(ContentLengthHeaderShort), 'ContentLengthShort constant');
  Check(not P.IsContentLength(''),                       '''''');
  Check(not P.IsContentLength('Via'),                    'Via');
  Check(    P.IsContentLength('Content-Length:'),        'Content-Length:');
  Check(    P.IsContentLength('content-LeNgTh'),         'content-LeNgTh');
end;

procedure TestTIdSipParser.TestIsEqual;
begin
  Check(    P.IsEqual('', ''),         ''''' & ''''');
  Check(not P.IsEqual(' ', ''),        ''' '' & ''''');
  Check(    P.IsEqual('abcd', 'AbCd'), '''abcd'', ''AbCd''');
  Check(not P.IsEqual('absd', 'Abcd'), '''absd'', ''Abcd''');
end;

procedure TestTIdSipParser.TestIsMaxForwards;
begin
  Check(not P.IsMaxForwards(''),                '''''');
  Check(    P.IsMaxForwards('max-FORWARDS'),    'max-FORWARDS');
  Check(    P.IsMaxForwards(MaxForwardsHeader), 'MaxForwardsHeader constant');
end;

procedure TestTIdSipParser.TestIsMethod;
begin
  Check(    P.IsMethod('INVITE'),                       'INVITE');
  Check(    P.IsMethod('X-INVITE'),                     'X-INVITE');
  Check(not P.IsMethod(''),                             '''');
  Check(    P.IsMethod('1'),                            '1');
  Check(    P.IsMethod('a'),                            'a');
  Check(    P.IsMethod('X_CITE'),                       'X_CITE');
  Check(not P.IsMethod('Cra.-zy''+prea"cher%20man~`!'), 'Cra.-zy''+prea"cher%20man~`!');
  Check(    P.IsMethod('Cra.-zy''+preacher%20man~`!'),  'Cra.-zy''+preacher%20man~`!');
end;

procedure TestTIdSipParser.TestIsNumber;
begin
  Check(not P.IsNumber(''),                     '''''');
  Check(not P.IsNumber('a'),                    'a');
  Check(not P.IsNumber(#0),                     '#0');
  Check(    P.IsNumber('13'),                   '13');
  Check(    P.IsNumber('98765432109876543210'), '98765432109876543210');
end;

procedure TestTIdSipParser.TestIsSipVersion;
begin
  Check(not P.IsSipVersion(''),         '''''');
  Check(    P.IsSipVersion('SIP/2.0'),  'SIP/2.0');
  Check(    P.IsSipVersion('sip/2.0'),  'sip/2.0');
  Check(    P.IsSipVersion(SIPVersion), 'SIPVersion constant');
  Check(not P.IsSipVersion('SIP/X.Y'),  'SIP/X.Y');
end;

procedure TestTIdSipParser.TestIsVia;
begin
  Check(    P.IsVia('Via'),                  'Via');
  Check(    P.IsVia(ViaHeaderFull),          'ViaFull constant');
  Check(    P.IsVia('v'),                    'v');
  Check(    P.IsVia(ViaHeaderShort),         'ViaShort constant');
  Check(not P.IsVia(''),                     '''''');
  Check(not P.IsVia('Content-Length'),       'Content-Length');
  Check(    P.IsVia('Via:'),                 'Via:');
  Check(    P.IsVia('ViA'),                  'ViA');
end;

procedure TestTIdSipParser.TestParseAndMakeMessageEmptyString;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    P.Source := Str;

    try
      P.ParseAndMakeMessage.Free;
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
    P.Source := Str;

    try
      P.ParseAndMakeMessage;
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
    P.Source := Str;

    Msg := P.ParseAndMakeMessage;
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
    P.Source := Str;

    Msg := P.ParseAndMakeMessage;
    try
      Self.CheckBasicResponse(Msg);
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
    P.Source := Str;

    P.ParseRequest(Request);
    Self.CheckBasicRequest(Request);
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
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals('', Request.Method,     'Method');
    CheckEquals('', Request.Request,    'Request');
    CheckEquals('', Request.SipVersion, 'SipVersion');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestFoldedHeader;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                          + 'From: Case'#13#10
                          + ' <sip:case@fried.neurons.org>'#13#10
                          + #9';tag=1928301774'#13#10
                          + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                          + #13#10);
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals(2, Request.OtherHeaders.Count, 'Header count');
    CheckEquals('From: Case <sip:case@fried.neurons.org> ;tag=1928301774',
                Request.OtherHeaders[0],
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Request.OtherHeaders[1],
                'To header');
    CheckEquals(0, Request.ContentLength, 'ContentLength');
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
    P.Source := Str;

    try
      P.ParseRequest(Request);
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
    P.Source := Str;

    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
  Str: TStringStream;
  Leftovers: array[0..99] of Char;
  S: String;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;

    P.ParseRequest(Request);
    CheckEquals(4,  Request.ContentLength, 'ContentLength');
    CheckEquals('', Request.Body,          'Body');

    Str.Read(LeftOvers, 100);
    S := LeftOvers;
    CheckEquals('I am a message. Hear me roar!', S, 'Remaining (extra) bits in the message');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseRequestMultipleVias;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10
                            + 'Max-Forwards: 70'#13#10
                            + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                            + 'Call-ID: a84b4c76e66710@gw1.leo_ix.org'#13#10
                            + 'CSeq: 314159 INVITE'#13#10
                            + 'Contact: <sip:wintermute@tessier-ashpool.co.lu>'#13#10
                            + 'Via: SIP/3.0/TLS gw5.cust1.leo_ix.org:5061;branch=z9hG4bK776aheh'#13#10
                            + 'Content-Length: 4'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals(2,                       Request.Path.Length,                   'Path.Length');
    Check      (Request.Path.FirstHop <> Request.Path.LastHop,                  'Sanity check on Path');
    CheckEquals('SIP/2.0',               Request.Path.LastHop.SipVersion,       'LastHop.SipVersion');
    Check      (sttTCP =                 Request.Path.LastHop.Transport,        'LastHop.Transport');
    CheckEquals('gw1.leo_ix.org',        Request.Path.LastHop.Host,             'LastHop.Host');
    CheckEquals(IdPORT_SIP,              Request.Path.LastHop.Port,             'LastHop.Port');
    CheckEquals('z9hG4bK776asdhds',      Request.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

    CheckEquals('SIP/3.0',               Request.Path.FirstHop.SipVersion,       'FirstHop.SipVersion');
    Check      (sttTLS =                 Request.Path.FirstHop.Transport,        'FirstHop.Transport');
    CheckEquals('gw5.cust1.leo_ix.org',  Request.Path.FirstHop.Host,             'FirstHop.Host');
    CheckEquals(IdPORT_SIP_TLS,          Request.Path.FirstHop.Port,             'FirstHop.Port');
    CheckEquals('z9hG4bK776aheh',        Request.Path.FirstHop.Params['branch'], 'FirstHop.Params[''branch'']');
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
    P.Source := Str;
    try
      P.ParseRequest(Request);
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
  Str := TStringStream.Create(#13#10#13#10'INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10);
  try
    P.Source := Str;
    P.ParseRequest(Request);

    CheckEquals('INVITE',                               Request.Method,     'Method');
    CheckEquals('sip:wintermute@tessier-ashpool.co.lu', Request.Request,    'Request');
    CheckEquals('SIP/2.0',                              Request.SipVersion, 'SipVersion');
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
    P.Source := Str;

    P.ParseResponse(Response);
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
    P.Source := Str;
    P.ParseResponse(Response);

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
    P.Source := Str;
    P.ParseResponse(Response);

    CheckEquals('SIP/1.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusTest');

    CheckEquals(2, Response.OtherHeaders.Count, 'Header count');
    CheckEquals('From: Case <sip:case@fried.neurons.org> ;tag=1928301774',
                Response.OtherHeaders[0],
                'From header');
    CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',
                Response.OtherHeaders[1],
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
    P.Source := Str;
    try
      P.ParseResponse(Response);
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
  Str := TStringStream.Create(#13#10#13#10'SIP/2.0 200 OK'#13#10);
  try
    P.Source := Str;
    P.ParseResponse(Response);

    CheckEquals('SIP/2.0', Response.SipVersion, 'SipVersion');
    CheckEquals(200,       Response.StatusCode, 'StatusCode');
    CheckEquals('OK',      Response.StatusText, 'StatusText');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestParseShortFormContentLength;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'l: 29'#13#10
                            + #13#10
                            + 'I am a message. Hear me roar!');
  try
    P.Source := Str;
    P.ParseRequest(Request);
    CheckEquals(29, Request.ContentLength, 'ContentLength');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestPeek;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    P.Source := Str;

    CheckEquals('I', P.Peek, 'Peek 1st line');
    P.ReadLn;
    CheckEquals('V', P.Peek, 'Peek 2nd line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestPeekLine;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    P.Source := Str;

    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0', P.PeekLine, 'PeekLine 1st line');
    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0', P.PeekLine, 'PeekLine 1st line, 2nd time');
    P.ReadLn;
    CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds', P.PeekLine, 'PeekLine 2nd line');
    P.ReadLn;
    CheckEquals('', P.PeekLine, 'PeekLine past the EOF');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestReadOctet;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    P.Source := Str;

    CheckEquals('I', P.ReadOctet, '1st ReadOctet');
    CheckEquals('N', P.ReadOctet, '2nd ReadOctet');
    CheckEquals('V', P.ReadOctet, '3rd ReadOctet');
    CheckEquals('I', P.ReadOctet, '4th ReadOctet');
    CheckEquals('T', P.ReadOctet, '5th ReadOctet');
    CheckEquals('E', P.ReadOctet, '6th ReadOctet');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestReadOctets;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    P.Source := Str;

    CheckEquals('',       P.ReadOctets(0), '0th ReadOctets(0)');
    CheckEquals('I',      P.ReadOctets(1), '1st ReadOctets(1)');
    CheckEquals('NVIT',   P.ReadOctets(4), '2nd ReadOctets(4)');
    CheckEquals('E sip:', P.ReadOctets(6), '3rd ReadOctets(6)');
    CheckEquals('wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
              + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10,
                P.ReadOctets(1000), 'ReadOctets(1000)');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestReadln;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0'#13#10
                            + 'Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds'#13#10);
  try
    P.Source := Str;

    CheckEquals('INVITE sip:wintermute@tessier-ashpool.co.lu SIP/2.0',     P.ReadLn, '1st ReadLn');
    CheckEquals('Via: SIP/2.0/TCP gw1.leo_ix.org;branch=z9hG4bK776asdhds', P.ReadLn, '2nd ReadLn');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestReadlnDoubleCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('one'#13#10#13#10'three');
  try
    P.Source := Str;
    CheckEquals('one',   P.ReadLn, '1st line');
    CheckEquals('',      P.ReadLn, '2nd line');
    CheckEquals('three', P.ReadLn, '3rd line');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipParser.TestReadlnWithNoCrLf;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('new');
  try
    P.Source := Str;
    CheckEquals('new', P.ReadLn, 'ReadLn');
  finally
    Str.Free;
  end;
end;

//* TestTIdSipParser Private methods *******************************************

procedure TestTIdSipParser.CheckBasicRequest(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipRequest.Classname, Msg.ClassName, 'Class type');

  CheckEquals('INVITE',                               TIdSipRequest(Msg).Method,  'Method');
  CheckEquals('sip:wintermute@tessier-ashpool.co.lu', TIdSipRequest(Msg).Request, 'Request');
  CheckEquals('SIP/2.0',                              Msg.SIPVersion,             'SipVersion');
  CheckEquals(29,                                     Msg.ContentLength,          'ContentLength');
  CheckEquals(70,                                     Msg.MaxForwards,            'MaxForwards');

  CheckEquals(1,                  Msg.Path.Length,                   'Path.Length');
  Check      (Msg.Path.FirstHop = Msg.Path.LastHop,                  'Sanity check on Path');
  CheckEquals('SIP/2.0',          Msg.Path.LastHop.SipVersion,       'LastHop.SipVersion');
  Check      (sttTCP =            Msg.Path.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo_ix.org',   Msg.Path.LastHop.Host,             'LastHop.Host');
  CheckEquals(IdPORT_SIP,         Msg.Path.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',   Msg.OtherHeaders[0], 'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',  Msg.OtherHeaders[1], 'From');
  CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',                  Msg.OtherHeaders[2], 'Call-ID');
  CheckEquals('CSeq: 314159 INVITE',                                     Msg.OtherHeaders[3], 'CSeq');
  CheckEquals('Contact: <sip:wintermute@tessier-ashpool.co.lu>',         Msg.OtherHeaders[4], 'Contact');
  CheckEquals(5, Msg.OtherHeaders.Count, 'Header count');

  CheckEquals('', Msg.Body, 'message-body');
end;

procedure TestTIdSipParser.CheckBasicResponse(const Msg: TIdSipMessage);
begin
  CheckEquals(TIdSipResponse.Classname, Msg.ClassName, 'Class type');

  CheckEquals('SIP/2.0',   Msg.SIPVersion,                 'SipVersion');
  CheckEquals(486,         TIdSipResponse(Msg).StatusCode, 'StatusCode');
  CheckEquals('Busy Here', TIdSipResponse(Msg).StatusText, 'StatusText');
  CheckEquals(29,          Msg.ContentLength,              'ContentLength');
  CheckEquals(70,          Msg.MaxForwards,                'Max-Forwards');

  CheckEquals(1,                  Msg.Path.Length,                   'Path.Length');
  Check      (Msg.Path.FirstHop = Msg.Path.LastHop,                  'Sanity check on Path');
  CheckEquals('SIP/2.0',          Msg.Path.LastHop.SipVersion,       'LastHop.SipVersion');
  Check      (sttTCP =            Msg.Path.LastHop.Transport,        'LastHop.Transport');
  CheckEquals('gw1.leo_ix.org',   Msg.Path.LastHop.Host,             'LastHop.Host');
  CheckEquals(IdPORT_SIP,         Msg.Path.LastHop.Port,             'LastHop.Port');
  CheckEquals('z9hG4bK776asdhds', Msg.Path.LastHop.Params['branch'], 'LastHop.Params[''branch'']');

  CheckEquals('To: Wintermute <sip:wintermute@tessier-ashpool.co.lu>',   Msg.OtherHeaders[0], 'To');
  CheckEquals('From: Case <sip:case@fried.neurons.org>;tag=1928301774',  Msg.OtherHeaders[1], 'From');
  CheckEquals('Call-ID: a84b4c76e66710@gw1.leo_ix.org',                  Msg.OtherHeaders[2], 'Call-ID');
  CheckEquals('CSeq: 314159 INVITE',                                     Msg.OtherHeaders[3], 'CSeq');
  CheckEquals('Contact: <sip:wintermute@tessier-ashpool.co.lu>',         Msg.OtherHeaders[4], 'Contact');
  CheckEquals(5, Msg.OtherHeaders.Count, 'Header count');

  CheckEquals('', Msg.Body, 'message-body');
end;

initialization
  RegisterTest('SIP Request Parsing', Suite);
end.
