unit TestIdSipMessage;

interface

uses
  IdSipHeaders, IdSipDialogID, IdSipMessage, SysUtils, TestFramework,
  TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestDecodeQuotedStr;
    procedure TestFirstChar;
    procedure TestIsEqual;
    procedure TestLastChar;
    procedure TestShortMonthToInt;
    procedure TestWithoutFirstAndLastChars;
  end;

  TIdSipTrivialMessage = class(TIdSipMessage)
  protected
    function FirstLine: String; override;
  public
    function  IsEqualTo(Msg: TIdSipMessage): Boolean; override;
    function  IsRequest: Boolean; override;
    function  MalformedException: EBadMessageClass; override;
  end;

  TestTIdSipMessage = class(TTestCaseSip)
  private
    Msg: TIdSipMessage;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddHeader;
    procedure TestAddHeaderName;
    procedure TestAddHeaders;
    procedure TestAssignCopiesBody;
    procedure TestClearHeaders;
    procedure TestContactCount;
    procedure TestFirstContact;
    procedure TestFirstExpires;
    procedure TestFirstHeader;
    procedure TestFirstMinExpires;
    procedure TestHasExpiry;
    procedure TestHeaderCount;
    procedure TestLastHop;
    procedure TestQuickestExpiry;
    procedure TestQuickestExpiryNoExpires;
    procedure TestReadBody;
    procedure TestReadBodyWithZeroContentLength;
    procedure TestRemoveHeader;
    procedure TestRemoveHeaders;
    procedure TestSetCallID;
    procedure TestSetContentLength;
    procedure TestSetContentType;
    procedure TestSetCSeq;
    procedure TestSetFrom;
    procedure TestSetSipVersion;
    procedure TestSetTo;
  end;

  TestTIdSipRequest = class(TTestCaseSip)
  private
    ReceivedRequest: TIdSipRequest;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressOfRecord;
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure TestAsStringNoMaxForwardsSet;
    procedure TestNewRequestHasContentLength;
    procedure TestHasSipsUri;
    procedure TestIsAck;
    procedure TestIsBye;
    procedure TestIsCancel;
    procedure TestIsEqualToComplexMessages;
    procedure TestIsEqualToDifferentHeaders;
    procedure TestIsEqualToDifferentMethod;
    procedure TestIsEqualToDifferentRequestUri;
    procedure TestIsEqualToDifferentSipVersion;
    procedure TestIsEqualToFromAssign;
    procedure TestIsEqualToResponse;
    procedure TestIsEqualToTrivial;
    procedure TestIsInvite;
    procedure TestIsRegister;
    procedure TestIsRequest;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteServer;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
    procedure TestRequiresResponse;
    procedure TestSetMaxForwards;
    procedure TestSetPath;
  end;

  TestTIdSipResponse = class(TTestCaseSip)
  private
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure TestIsEqualToComplexMessages;
    procedure TestIsEqualToDifferentHeaders;
    procedure TestIsEqualToDifferentSipVersion;
    procedure TestIsEqualToDifferentStatusCode;
    procedure TestIsEqualToDifferentStatusText;
    procedure TestIsEqualToRequest;
    procedure TestIsEqualToTrivial;
    procedure TestIsFinal;
    procedure TestIsOK;
    procedure TestIsProvisional;
    procedure TestIsRequest;
    procedure TestIsTrying;
  end;

implementation

uses
  Classes, IdSipConsts, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Messages)');

  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipMessage.Suite);
  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestDecodeQuotedStr;
var
  Result: String;
begin
  Check(DecodeQuotedStr('', Result), 'Empty string result');
  CheckEquals('', Result,            'Empty string decoded');

  Check(DecodeQuotedStr('\"', Result), '\" result');
  CheckEquals('"', Result,             '\" decoded');

  Check(DecodeQuotedStr('\\', Result), '\\ result');
  CheckEquals('\', Result,             '\\ decoded');

  Check(DecodeQuotedStr('\a', Result), '\a result');
  CheckEquals('a', Result,             '\a decoded');

  Check(DecodeQuotedStr('foo', Result), 'foo result');
  CheckEquals('foo', Result,            'foo decoded');

  Check(DecodeQuotedStr('\"foo\\\"', Result), '\"foo\\\" result');
  CheckEquals('"foo\"', Result,               '\"foo\\\" decoded');

  Check(not DecodeQuotedStr('\', Result), '\ result');
end;

procedure TestFunctions.TestFirstChar;
begin
  CheckEquals('',  FirstChar(''),   'Empty string');
  CheckEquals('a', FirstChar('ab'), 'ab');
end;

procedure TestFunctions.TestIsEqual;
begin
  Check(    IsEqual('', ''),    'Empty strings');
  Check(not IsEqual('', 'a'),   'Empty string & ''a''');
  Check(    IsEqual('a', 'a'),  '''a'' & ''a''');
  Check(    IsEqual('A', 'a'),  '''A'' & ''a''');
  Check(    IsEqual('a', 'A'),  '''a'' & ''A''');
  Check(    IsEqual('A', 'A'),  '''A'' & ''A''');
  Check(not IsEqual(' a', 'a'), ''' a'' & ''a''');
end;

procedure TestFunctions.TestLastChar;
begin
  CheckEquals('',  LastChar(''),   'Empty string');
  CheckEquals('b', LastChar('ab'), 'ab');
end;

procedure TestFunctions.TestShortMonthToInt;
var
  I: Integer;
begin
  for I := Low(ShortMonthNames) to High(ShortMonthNames) do begin
    CheckEquals(I,
                ShortMonthToInt(ShortMonthNames[I]),
                ShortMonthNames[I]);
    CheckEquals(I,
                ShortMonthToInt(UpperCase(ShortMonthNames[I])),
                UpperCase(ShortMonthNames[I]));
  end;

  try
    ShortMonthToInt('foo');
    Fail('Failed to raise exception on ''foo''');
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestWithoutFirstAndLastChars;
begin
  CheckEquals('',    WithoutFirstAndLastChars(''),      'Empty string');
  CheckEquals('',    WithoutFirstAndLastChars('a'),     'a');
  CheckEquals('',    WithoutFirstAndLastChars('ab'),    'ab');
  CheckEquals('b',   WithoutFirstAndLastChars('abc'),   'abc');
  CheckEquals('abc', WithoutFirstAndLastChars('"abc"'), '"abc"');
  CheckEquals('abc', WithoutFirstAndLastChars('[abc]'), '[abc]');
end;

//******************************************************************************
//* TIdSipTrivialMessage                                                       *
//******************************************************************************
//* TIdSipTrivialMessage Public methods ****************************************

function TIdSipTrivialMessage.IsEqualTo(Msg: TIdSipMessage): Boolean;
begin
  Result := false;
end;

function TIdSipTrivialMessage.IsRequest: Boolean;
begin
  Result := false;
end;

function TIdSipTrivialMessage.MalformedException: EBadMessageClass;
begin
  Result := nil;
end;

//* TIdSipTrivialMessage Protected methods *************************************

function TIdSipTrivialMessage.FirstLine: String;
begin
  Result := '';
end;

//******************************************************************************
//* TestTIdSipMessage                                                          *
//******************************************************************************
//* TestTIdSipMessage Public methods *******************************************

procedure TestTIdSipMessage.SetUp;
begin
  inherited SetUp;

  Self.Msg := TIdSipTrivialMessage.Create;
end;

procedure TestTIdSipMessage.TearDown;
begin
  Self.Msg.Free;

  inherited TearDown;
end;

//* TestTIdSipMessage Published methods ****************************************

procedure TestTIdSipMessage.TestAddHeader;
var
  H: TIdSipHeader;
begin
  Self.Msg.ClearHeaders;

  H := TIdSipHeader.Create;
  try
    H.Name := UserAgentHeader;
    H.Value := 'Dog''s breakfast v0.1';

    Self.Msg.AddHeader(H);

    Check(Self.Msg.HasHeader(UserAgentHeader), 'No header added');

    CheckEquals(H.Name,
                Self.Msg.Headers.Items[0].Name,
                'Name not copied');

    CheckEquals(H.Value,
                Self.Msg.Headers.Items[0].Value,
                'Value not copied');
  finally
    H.Free;
  end;

  CheckEquals(UserAgentHeader,
              Self.Msg.Headers.Items[0].Name,
              'And we check that the header was copied & we''re not merely '
            + 'storing a reference');
end;

procedure TestTIdSipMessage.TestAddHeaderName;
begin
  Self.Msg.ClearHeaders;

  CheckNotNull(Self.Msg.AddHeader(UserAgentHeader), 'Nil returned');

  Check(Self.Msg.HasHeader(UserAgentHeader), 'No header added');
end;

procedure TestTIdSipMessage.TestAddHeaders;
var
  Headers: TIdSipHeaders;
begin
  Self.Msg.ClearHeaders;

  Headers := TIdSipHeaders.Create;
  try
    Headers.Add(UserAgentHeader).Value := '0';
    Headers.Add(UserAgentHeader).Value := '1';
    Headers.Add(UserAgentHeader).Value := '2';
    Headers.Add(UserAgentHeader).Value := '3';

    Self.Msg.AddHeaders(Headers);
    Self.Msg.Headers.IsEqualTo(Headers);
  finally
    Headers.Free;
  end;
end;

procedure TestTIdSipMessage.TestAssignCopiesBody;
var
  AnotherMsg: TIdSipMessage;
begin
  AnotherMsg := TIdSipTrivialMessage.Create;
  try
    Self.Msg.Body := 'I am a body';

    AnotherMsg.Assign(Self.Msg);
    CheckEquals(Self.Msg.Body,
                AnotherMsg.Body,
                'Body not assigned properly');
  finally
    AnotherMsg.Free;
  end;
end;

procedure TestTIdSipMessage.TestClearHeaders;
begin
  Self.Msg.AddHeader(UserAgentHeader);
  Self.Msg.AddHeader(UserAgentHeader);
  Self.Msg.AddHeader(UserAgentHeader);
  Self.Msg.AddHeader(UserAgentHeader);

  Self.Msg.ClearHeaders;

  CheckEquals(0, Self.Msg.HeaderCount, 'Headers not cleared');
end;

procedure TestTIdSipMessage.TestContactCount;
begin
  Self.Msg.ClearHeaders;
  CheckEquals(0, Self.Msg.ContactCount, 'No headers');

  Self.Msg.AddHeader(ContactHeaderFull);
  CheckEquals(1, Self.Msg.ContactCount, 'Contact');

  Self.Msg.AddHeader(ViaHeaderFull);
  CheckEquals(1, Self.Msg.ContactCount, 'Contact + Via');

  Self.Msg.AddHeader(ContactHeaderFull);
  CheckEquals(2, Self.Msg.ContactCount, '2 Contacts + Via');
end;

procedure TestTIdSipMessage.TestFirstContact;
var
  C: TIdSipHeader;
begin
  Self.Msg.ClearHeaders;

  CheckNotNull(Self.Msg.FirstContact, 'Contact not present');
  CheckEquals(1, Self.Msg.HeaderCount, 'Contact not auto-added');

  C := Self.Msg.FirstHeader(ContactHeaderFull);
  Self.Msg.AddHeader(ContactHeaderFull);

  Check(C = Self.Msg.FirstContact, 'Wrong Contact');
end;

procedure TestTIdSipMessage.TestFirstExpires;
var
  E: TIdSipHeader;
begin
  Self.Msg.ClearHeaders;

  CheckNotNull(Self.Msg.FirstExpires, 'Expires not present');
  CheckEquals(1, Self.Msg.HeaderCount, 'Expires not auto-added');

  E := Self.Msg.FirstHeader(ExpiresHeader);
  Self.Msg.AddHeader(ExpiresHeader);

  Check(E = Self.Msg.FirstExpires, 'Wrong Expires');
end;

procedure TestTIdSipMessage.TestFirstHeader;
var
  H: TIdSipHeader;
begin
  Self.Msg.ClearHeaders;
  H := Self.Msg.AddHeader(UserAgentHeader);
  Check(H = Self.Msg.FirstHeader(UserAgentHeader),
        'Wrong result returned for first User-Agent');

  H := Self.Msg.AddHeader(RouteHeader);
  Check(H = Self.Msg.FirstHeader(RouteHeader),
        'Wrong result returned for first Route');

  H := Self.Msg.AddHeader(RouteHeader);
  Check(H <> Self.Msg.FirstHeader(RouteHeader),
        'Wrong result returned for first Route of two');
end;

procedure TestTIdSipMessage.TestFirstMinExpires;
var
  E: TIdSipHeader;
begin
  Self.Msg.ClearHeaders;

  CheckNotNull(Self.Msg.FirstMinExpires, 'Min-Expires not present');
  CheckEquals(1, Self.Msg.HeaderCount, 'Min-Expires not auto-added');

  E := Self.Msg.FirstHeader(MinExpiresHeader);
  Self.Msg.AddHeader(MinExpiresHeader);

  Check(E = Self.Msg.FirstMinExpires, 'Wrong Min-Expires');
end;

procedure TestTIdSipMessage.TestHasExpiry;
begin
  Self.Msg.ClearHeaders;
  Check(not Self.Msg.HasExpiry, 'No headers');

  Self.Msg.AddHeader(ExpiresHeader);
  Check(Self.Msg.HasExpiry, 'Expires header');

  Self.Msg.ClearHeaders;
  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org';
  Check(not Self.Msg.HasExpiry,
        'Contact with no Expires parameter or Expires header');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org;expires=10';
  Check(Self.Msg.HasExpiry,
        'No Expires header and Contact with Expires parameter');

  Self.Msg.AddHeader(ExpiresHeader);
  Check(Self.Msg.HasExpiry,
        'Expires header and Contact with Expires parameter');
end;

procedure TestTIdSipMessage.TestHeaderCount;
begin
  Self.Msg.ClearHeaders;
  Self.Msg.AddHeader(UserAgentHeader);

  CheckEquals(1, Self.Msg.HeaderCount, 'HeaderCount not correct');
end;

procedure TestTIdSipMessage.TestLastHop;
begin
  Self.Msg.ClearHeaders;
  Check(Self.Msg.LastHop = Self.Msg.FirstHeader(ViaHeaderFull), 'Unexpected return for empty path');

  Self.Msg.AddHeader(ViaHeaderFull);
  Check(Self.Msg.LastHop = Self.Msg.Path.LastHop, 'Unexpected return');
end;

procedure TestTIdSipMessage.TestQuickestExpiry;
begin
  Self.Msg.ClearHeaders;
  CheckEquals(0, Self.Msg.QuickestExpiry, 'No headers');

  Self.Msg.AddHeader(ExpiresHeader).Value := '10';
  CheckEquals(10, Self.Msg.QuickestExpiry, 'An Expiry header');

  Self.Msg.AddHeader(ExpiresHeader).Value := '9';
  CheckEquals(9, Self.Msg.QuickestExpiry, 'Two Expiry headers');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org';
  CheckEquals(9, Self.Msg.QuickestExpiry, 'Two Expiry headers + Contact');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org;expires=10';
  CheckEquals(9, Self.Msg.QuickestExpiry, 'Two Expiry headers + two Contacts');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:case@fried.neurons.org;expires=8';
  CheckEquals(8, Self.Msg.QuickestExpiry, 'Two Expiry headers + three Contacts');
end;

procedure TestTIdSipMessage.TestQuickestExpiryNoExpires;
begin
  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org;expires=10';
  CheckEquals(10, Self.Msg.QuickestExpiry, 'One Contact');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:case@fried.neurons.org;expires=8';
  CheckEquals(8, Self.Msg.QuickestExpiry, 'Two Contacts');

  Self.Msg.AddHeader(ContactHeaderFull).Value := 'sip:case@fried.neurons.org;expires=22';
  CheckEquals(8, Self.Msg.QuickestExpiry, 'Three Contacts');
end;

procedure TestTIdSipMessage.TestReadBody;
var
  Len:       Integer;
  Msg:       String;
  Remainder: String;
  S:         String;
  Str:       TStringStream;
begin
  Self.Msg.ContentLength := 8;

  Msg := 'Negotium perambuians in tenebris';
  Str := TStringStream.Create(Msg);
  try
    Self.Msg.ReadBody(Str);
    CheckEquals(System.Copy(Msg, 1, 8), Self.Msg.Body, 'Body');

    Remainder := Msg;
    Delete(Remainder, 1, 8);

    Len := Length(Remainder);
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(Remainder, S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipMessage.TestReadBodyWithZeroContentLength;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
  Msg: String;
begin
  Self.Msg.ContentLength := 0;
  Msg := 'Negotium perambuians in tenebris';

  Str := TStringStream.Create(Msg);
  try
    Self.Msg.ReadBody(Str);
    CheckEquals('', Self.Msg.Body, 'Body');

    Len := Length(Msg);
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(Msg, S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipMessage.TestRemoveHeader;
begin
  Self.Msg.ClearHeaders;

  Self.Msg.AddHeader(ContentTypeHeaderFull);
  Check(Self.Msg.HasHeader(ContentTypeHeaderFull),
        'Content-Type wasn''t added');

  Self.Msg.RemoveHeader(Self.Msg.FirstHeader(ContentTypeHeaderFull));
  Check(not Self.Msg.HasHeader(ContentTypeHeaderFull),
        'Content-Type wasn''t removeed');
end;

procedure TestTIdSipMessage.TestRemoveHeaders;
begin
  Self.Msg.ClearHeaders;

  Self.Msg.AddHeader(ContentTypeHeaderFull);
  Self.Msg.AddHeader(ContentTypeHeaderFull);
  Self.Msg.AddHeader(ContentTypeHeaderFull);

  Self.Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);

  Check(not Self.Msg.HasHeader(ContentTypeHeaderFull),
        'Content-Type wasn''t removeed');
end;

procedure TestTIdSipMessage.TestSetCallID;
begin
  Self.Msg.CallID := '999';

  Self.Msg.CallID := '42';
  CheckEquals('42', Self.Msg.CallID, 'Call-ID not set');
end;

procedure TestTIdSipMessage.TestSetContentLength;
begin
  Self.Msg.ContentLength := 999;

  Self.Msg.ContentLength := 42;
  CheckEquals(42, Self.Msg.ContentLength, 'Content-Length not set');
end;

procedure TestTIdSipMessage.TestSetContentType;
begin
  Self.Msg.ContentType := 'text/plain';

  Self.Msg.ContentType := 'text/t140';
  CheckEquals('text/t140', Self.Msg.ContentType, 'Content-Type not set');
end;

procedure TestTIdSipMessage.TestSetCSeq;
var
  C: TIdSipCSeqHeader;
begin
  C := TIdSipCSeqHeader.Create;
  try
    C.Value := '314159 INVITE';

    Self.Msg.CSeq := C;

    Check(Self.Msg.CSeq.IsEqualTo(C), 'CSeq not set');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipMessage.TestSetFrom;
var
  From: TIdSipFromHeader;
begin
  Self.Msg.From.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'Case <sip:case@fried.neurons.org>';

    Self.Msg.From := From;

    CheckEquals(From.Value, Self.Msg.From.Value, 'From value not set');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipMessage.TestSetSipVersion;
begin
  Self.Msg.SIPVersion := 'SIP/2.0';

  Self.Msg.SIPVersion := 'SIP/7.7';
  CheckEquals('SIP/7.7', Self.Msg.SipVersion, 'SipVersion not set');
end;

procedure TestTIdSipMessage.TestSetTo;
var
  ToHeader: TIdSipToHeader;
begin
  Self.Msg.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Value := 'Case <sip:case@fried.neurons.org>';

    Self.Msg.ToHeader := ToHeader;

    CheckEquals(ToHeader.Value, Self.Msg.ToHeader.Value, 'To value not set');
  finally
    ToHeader.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipRequest                                                          *
//******************************************************************************
//* TestTIdSipRequest Public methods *******************************************

procedure TestTIdSipRequest.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  P := TIdSipParser.Create;
  try
    Self.Request         := P.ParseAndMakeRequest(BasicRequest);
    Self.ReceivedRequest := P.ParseAndMakeRequest(BasicRequest);
    Self.Response        := P.ParseAndMakeResponse(BasicResponse);
  finally
    P.Free;
  end;
end;

procedure TestTIdSipRequest.TearDown;
begin
  Self.Response.Free;
  Self.ReceivedRequest.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipRequest Published methods ****************************************

procedure TestTIdSipRequest.TestAddressOfRecord;
begin
  CheckEquals(Self.Request.ToHeader.AsAddressOfRecord,
              Self.Request.AddressOfRecord,
              'AddressOfRecord');

  Self.Request.RequestUri.Uri := 'sip:proxy.tessier-ashpool.co.luna';
  CheckEquals(Self.Request.ToHeader.AsAddressOfRecord,
              Self.Request.AddressOfRecord,
              'AddressOfRecord');
end;

procedure TestTIdSipRequest.TestAssign;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.SIPVersion := 'SIP/1.5';
    R.Method := 'NewMethod';
    R.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    R.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.ContentLength := 5;
    R.Body := 'hello';

    Self.Request.Assign(R);
    CheckEquals(R.SIPVersion,    Self.Request.SipVersion,    'SIP-Version');
    CheckEquals(R.Method,        Self.Request.Method,        'Method');
    CheckEquals(R.RequestUri,    Self.Request.RequestUri,    'Request-URI');

    Check(R.Headers.IsEqualTo(Self.Request.Headers),
          'Headers not assigned properly');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipRequest.TestAssignBad;
var
  P: TPersistent;
begin
  P := TPersistent.Create;
  try
    try
      Self.Request.Assign(P);
      Fail('Failed to bail out assigning a TPersistent to a TIdSipRequest');
    except
      on EConvertError do;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipRequest.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Expected := TStringList.Create;
  try
    Expected.Text := BasicRequest;

    Received := TStringList.Create;
    try
      Received.Text := Self.Request.AsString;

      CheckEquals(Expected, Received, 'AsString');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseRequest(Self.Request);
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

procedure TestTIdSipRequest.TestAsStringNoMaxForwardsSet;
begin
  Check(Pos(MaxForwardsHeader, Self.Request.AsString) > 0, 'No Max-Forwards header');
end;

procedure TestTIdSipRequest.TestNewRequestHasContentLength;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    Check(Pos(ContentLengthHeaderFull, R.AsString) > 0,
          'Content-Length missing from new request');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipRequest.TestHasSipsUri;
begin
  Self.Request.RequestUri.URI := 'tel://999';
  Check(not Self.Request.HasSipsUri, 'tel URI');

  Self.Request.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.Request.HasSipsUri, 'sip URI');

  Self.Request.RequestUri.URI := 'sips:wintermute@tessier-ashpool.co.luna';
  Check(Self.Request.HasSipsUri, 'sips URI');
end;

procedure TestTIdSipRequest.TestIsAck;
begin
  Self.Request.Method := MethodAck;
  Check(Self.Request.IsAck, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsAck, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsAck, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(not Self.Request.IsAck, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsAck, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsAck, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsAck, 'XXX');
end;

procedure TestTIdSipRequest.TestIsBye;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.IsBye, MethodAck);

  Self.Request.Method := MethodBye;
  Check(Self.Request.IsBye, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsBye, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(not Self.Request.IsBye, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsBye, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsBye, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsBye, 'XXX');
end;

procedure TestTIdSipRequest.TestIsCancel;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.IsCancel, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsCancel, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(Self.Request.IsCancel, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(not Self.Request.IsCancel, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsCancel, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsCancel, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsCancel, 'XXX');
end;

procedure TestTIdSipRequest.TestIsEqualToComplexMessages;
begin
  Check(Self.Request.IsEqualTo(Self.ReceivedRequest), 'Request = ReceivedRequest');
  Check(Self.ReceivedRequest.IsEqualTo(Self.Request), 'ReceivedRequest = Request');
end;

procedure TestTIdSipRequest.TestIsEqualToDifferentHeaders;
var
  R1, R2: TIdSipRequest;
begin
  R1 := TIdSipRequest.Create;
  try
    R2 := TIdSipRequest.Create;
    try
      R1.AddHeader(ViaHeaderFull);

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToDifferentMethod;
var
  R1, R2: TIdSipRequest;
begin
  R1 := TIdSipRequest.Create;
  try
    R2 := TIdSipRequest.Create;
    try
      R1.Method := MethodInvite;
      R2.Method := MethodOptions;

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToDifferentRequestUri;
var
  R1, R2: TIdSipRequest;
begin
  R1 := TIdSipRequest.Create;
  try
    R2 := TIdSipRequest.Create;
    try
      R1.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';
      R1.RequestUri.URI := 'sip:case@fried.neurons.org';

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToDifferentSipVersion;
var
  R1, R2: TIdSipRequest;
begin
  R1 := TIdSipRequest.Create;
  try
    R2 := TIdSipRequest.Create;
    try
      R1.SIPVersion := 'SIP/2.0';
      R2.SIPVersion := 'SIP/2.1';

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToFromAssign;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipRequest.Create;
  try
    Req.Assign(Self.Request);

    Check(Req.IsEqualTo(Self.Request), 'Assigned = Original');
    Check(Self.Request.IsEqualTo(Req), 'Original = Assigned');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToResponse;
var
  Req: TIdSipRequest;
  Res: TIdSipResponse;
begin
  Req := TIdSipRequest.Create;
  try
    Res := TIdSipResponse.Create;
    try
      Check(not Req.IsEqualTo(Res), 'Req <> Res');
    finally
      Res.Free;
    end;
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsEqualToTrivial;
var
  R1, R2: TIdSipRequest;
begin
  R1 := TIdSipRequest.Create;
  try
    R2 := TIdSipRequest.Create;
    try
      Check(R1.IsEqualTo(R2), 'R1 = R2');
      Check(R2.IsEqualTo(R1), 'R2 = R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipRequest.TestIsInvite;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.IsInvite, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsInvite, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsInvite, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(Self.Request.IsInvite, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsInvite, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsInvite, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsInvite, 'XXX');
end;

procedure TestTIdSipRequest.TestIsRegister;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.IsRegister, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsRegister, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsRegister, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(not Self.Request.IsRegister, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsRegister, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(Self.Request.IsRegister, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsRegister, 'XXX');
end;

procedure TestTIdSipRequest.TestIsRequest;
begin
  Check(Self.Request.IsRequest, 'IsRequest');
end;

procedure TestTIdSipRequest.TestMatchInviteClient;
begin
  Check(Self.Request.Match(Self.Response),
        'Identical headers');

  Self.Response.AddHeader(ContentLanguageHeader).Value := 'es';
  Check(Self.Request.Match(Self.Response),
        'Identical headers + irrelevant headers');

  (Self.Response.FirstHeader(FromHeaderFull) as TIdSipFromToHeader).Tag := '1';
  Check(Self.Request.Match(Self.Response),
        'Different From tag');
  Self.Response.FirstHeader(FromHeaderFull).Assign(Self.Request.FirstHeader(FromHeaderFull));

  (Self.Response.FirstHeader(ToHeaderFull) as TIdSipFromToHeader).Tag := '1';
  Check(Self.Request.Match(Self.Response),
        'Different To tag');
end;

procedure TestTIdSipRequest.TestMatchInviteClientAckWithInvite;
begin
  Self.Response.CSeq.Method := MethodAck;
  Check(Self.Request.Match(Self.Response),
        'ACK match against INVITE');
end;

procedure TestTIdSipRequest.TestMatchInviteClientDifferentCSeqMethod;
begin
  Self.Response.CSeq.Method := MethodCancel;

  Check(not Self.Request.Match(Self.Response),
        'Different CSeq method');
end;

procedure TestTIdSipRequest.TestMatchInviteClientDifferentViaBranch;
begin
  Self.Response.LastHop.Branch := BranchMagicCookie + 'foo';

  Check(not Self.Request.Match(Self.Response),
        'Different Via branch');
end;

procedure TestTIdSipRequest.TestMatchInviteServer;
begin
  Check(Self.Request.Match(Self.Request),
        'Identical INVITE request');

  Self.ReceivedRequest.LastHop.SentBy := 'cougar';
  Check(not Self.Request.Match(Self.ReceivedRequest),
        'Different sent-by');
  Self.ReceivedRequest.LastHop.SentBy := Self.Request.LastHop.SentBy;

  Self.ReceivedRequest.LastHop.Branch := 'z9hG4bK6';
  Check(not Self.Request.Match(Self.ReceivedRequest),
        'Different branch');

  Self.ReceivedRequest.LastHop.Branch := Self.Request.LastHop.Branch;
  Self.ReceivedRequest.Method := MethodAck;
  Check(Self.Request.Match(Self.ReceivedRequest), 'ACK');

  Self.ReceivedRequest.LastHop.SentBy := 'cougar';
  Check(not Self.Request.Match(Self.ReceivedRequest),
        'ACK but different sent-by');
  Self.ReceivedRequest.LastHop.SentBy := Self.Request.LastHop.SentBy;

  Self.ReceivedRequest.LastHop.Branch := 'z9hG4bK6';
  Check(not Self.Request.Match(Self.ReceivedRequest),
        'ACK but different branch');
end;

procedure TestTIdSipRequest.TestMatchNonInviteClient;
begin
  Self.Response.CSeq.Method := MethodCancel;
  Self.Request.Method           := MethodCancel;

  Check(Self.Request.Match(Self.Response),
        'Identical headers');

  Self.Response.AddHeader(ContentLanguageHeader).Value := 'es';
  Check(Self.Request.Match(Self.Response),
        'Identical headers + irrelevant headers');

  (Self.Response.FirstHeader(FromHeaderFull) as TIdSipFromToHeader).Tag := '1';
  Check(Self.Request.Match(Self.Response),
        'Different From tag');
  Self.Response.FirstHeader(FromHeaderFull).Assign(Self.Request.FirstHeader(FromHeaderFull));

  (Self.Response.FirstHeader(ToHeaderFull) as TIdSipFromToHeader).Tag := '1';
  Check(Self.Request.Match(Self.Response),
        'Different To tag');

  Self.Response.CSeq.Method := MethodRegister;
  Check(not Self.Request.Match(Self.Response),
        'Different method');
end;

procedure TestTIdSipRequest.TestMatchNonInviteServer;
begin
  Self.ReceivedRequest.Method := MethodCancel;
  Self.Request.Method     := MethodCancel;

  Check(Self.Request.Match(Self.ReceivedRequest),
        'Identical CANCEL request');

  Self.ReceivedRequest.Method := MethodRegister;
  Check(not Self.Request.Match(Self.ReceivedRequest),
        'Different method');
end;

procedure TestTIdSipRequest.TestRequiresResponse;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.RequiresResponse, 'ACKs don''t need responses');
  Self.Request.Method := MethodBye;
  Check(Self.Request.RequiresResponse, 'BYEs need responses');
  Self.Request.Method := MethodCancel;
  Check(Self.Request.RequiresResponse, 'CANCELs need responses');
  Self.Request.Method := MethodInvite;
  Check(Self.Request.RequiresResponse, 'INVITEs need responses');
  Self.Request.Method := MethodOptions;
  Check(Self.Request.RequiresResponse, 'OPTIONS need responses');
  Self.Request.Method := MethodRegister;
  Check(Self.Request.RequiresResponse, 'REGISTERs need responses');

  Self.Request.Method := 'NewFangledMethod';
  Check(Self.Request.RequiresResponse,
        'Unknown methods, by default (our assumption) require responses');
end;

procedure TestTIdSipRequest.TestSetMaxForwards;
var
  OrigMaxForwards: Byte;
begin
  OrigMaxForwards := Self.Request.MaxForwards;

  Self.Request.MaxForwards := Self.Request.MaxForwards + 1;

  CheckEquals(OrigMaxForwards + 1,
              Self.Request.MaxForwards,
              'Max-Forwards not set');
end;

procedure TestTIdSipRequest.TestSetPath;
var
  H: TIdSipHeaders;
  P: TIdSipViaPath;
begin
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';

  H := TIdSipHeaders.Create;
  try
    H.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
    H.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw3.leo-ix.org;branch=z9hG4bK776asdhds';
    P := TIdSipViaPath.Create(H);
    try
      Self.Request.Path := P;

      Check(Self.Request.Path.IsEqualTo(P), 'Path not correctly set');
    finally
      P.Free;
    end;
  finally
    H.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipResponse                                                         *
//******************************************************************************
//* TestTIdSipResponse Public methods ******************************************

procedure TestTIdSipResponse.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;
end;

procedure TestTIdSipResponse.TearDown;
begin
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipResponse Published methods ***************************************

procedure TestTIdSipResponse.TestAssign;
var
  R: TIdSipResponse;
begin
  R := TIdSipResponse.Create;
  try
    R.SIPVersion := 'SIP/1.5';
    R.StatusCode := 101;
    R.StatusText := 'Hehaeha I''ll get back to you';
    R.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.ContentLength := 5;
    R.Body := 'hello';

    Self.Response.Assign(R);
    CheckEquals(R.SIPVersion,    Self.Response.SipVersion,    'SIP-Version');
    CheckEquals(R.StatusCode,    Self.Response.StatusCode,    'Status-Code');
    CheckEquals(R.StatusText,    Self.Response.StatusText,    'Status-Text');

    Check(R.Headers.IsEqualTo(Self.Response.Headers),
          'Headers not assigned properly');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipResponse.TestAssignBad;
var
  P: TPersistent;
begin
  P := TPersistent.Create;
  try
    try
      Self.Response.Assign(P);
      Fail('Failed to bail out assigning a TObject to a TIdSipResponse');
    except
      on EConvertError do;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipResponse.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Self.Response.StatusCode                             := 486;
  Self.Response.StatusText                             := 'Busy Here';
  Self.Response.SIPVersion                             := SIPVersion;
  Self.Response.AddHeader(ViaHeaderFull).Value         := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Response.AddHeader(ToHeaderFull).Value          := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775';
  Self.Response.AddHeader(FromHeaderFull).Value        := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Self.Response.CallID                                 := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.Response.AddHeader(CSeqHeader).Value            := '314159 INVITE';
  Self.Response.AddHeader(ContactHeaderFull).Value     := '<sip:wintermute@tessier-ashpool.co.luna>';
  Self.Response.AddHeader(ContentTypeHeaderFull).Value := 'text/plain';
  Self.Response.ContentLength                          := 29;
  Self.Response.Body                                   := 'I am a message. Hear me roar!';

  Expected := TStringList.Create;
  try
    Expected.Text := BasicResponse;

    Received := TStringList.Create;
    try
      Received.Text := Self.Response.AsString;

      CheckEquals(Expected, Received, 'AsString');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseResponse(Self.Response);
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

procedure TestTIdSipResponse.TestIsEqualToComplexMessages;
var
  P:      TIdSipParser;
  R1, R2: TIdSipResponse;
begin
  P := TIdSipParser.Create;
  try
    R1 := P.ParseAndMakeResponse(LocalLoopResponse);
    try
      R2 := P.ParseAndMakeResponse(LocalLoopResponse);
      try
        Check(R1.IsEqualTo(R2), 'R1 = R2');
        Check(R2.IsEqualTo(R1), 'R2 = R1');
      finally
        R2.Free;
      end;
    finally
      R1.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToDifferentHeaders;
var
  R1, R2: TIdSipResponse;
begin
  R1 := TIdSipResponse.Create;
  try
    R2 := TIdSipResponse.Create;
    try
      R1.AddHeader(ViaHeaderFull);

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToDifferentSipVersion;
var
  R1, R2: TIdSipResponse;
begin
  R1 := TIdSipResponse.Create;
  try
    R2 := TIdSipResponse.Create;
    try
      R1.SIPVersion := 'SIP/2.0';
      R2.SIPVersion := 'SIP/2.1';

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToDifferentStatusCode;
var
  R1, R2: TIdSipResponse;
begin
  R1 := TIdSipResponse.Create;
  try
    R2 := TIdSipResponse.Create;
    try
      R1.StatusCode := SIPOK;
      R2.StatusCode := SIPTrying;

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToDifferentStatusText;
var
  R1, R2: TIdSipResponse;
begin
  R1 := TIdSipResponse.Create;
  try
    R2 := TIdSipResponse.Create;
    try
      R1.StatusText := RSSIPOK;
      R2.StatusText := RSSIPTrying;

      Check(not R1.IsEqualTo(R2), 'R1 <> R2');
      Check(not R2.IsEqualTo(R1), 'R2 <> R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToRequest;
var
  Req: TIdSipRequest;
  Res: TIdSipResponse;
begin
  Req := TIdSipRequest.Create;
  try
    Res := TIdSipResponse.Create;
    try
      Check(not Res.IsEqualTo(Req), 'Res <> Req');
    finally
      Res.Free;
    end;
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsEqualToTrivial;
var
  R1, R2: TIdSipResponse;
begin
  R1 := TIdSipResponse.Create;
  try
    R2 := TIdSipResponse.Create;
    try
      Check(R1.IsEqualTo(R2), 'R1 = R2');
      Check(R2.IsEqualTo(R1), 'R2 = R1');
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsFinal;
begin
  Self.Response.StatusCode := SIPTrying;
  Check(not Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPOK;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPMultipleChoices;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBadRequest;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPInternalServerError;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBusyEverywhere;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));
end;

procedure TestTIdSipResponse.TestIsOK;
var
  I: Integer;
begin
  for I := 100 to 199 do begin
    Self.Response.StatusCode := I;
    Check(not Self.Response.IsOK,
          IntToStr(I) + ' ' + Self.Response.StatusText);
  end;

  for I := 200 to 299 do begin
    Self.Response.StatusCode := I;
    Check(Self.Response.IsOK,
          IntToStr(I) + ' ' + Self.Response.StatusText);
  end;

  for I := 301 to 699 do begin
    Self.Response.StatusCode := I;
    Check(not Self.Response.IsOK,
          IntToStr(I) + ' ' + Self.Response.StatusText);
  end;
end;

procedure TestTIdSipResponse.TestIsProvisional;
begin
  Self.Response.StatusCode := SIPTrying;
  Check(Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPOK;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPMultipleChoices;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBadRequest;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPInternalServerError;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBusyEverywhere;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));
end;

procedure TestTIdSipResponse.TestIsRequest;
begin
  Check(not Self.Response.IsRequest, 'IsRequest');
end;

procedure TestTIdSipResponse.TestIsTrying;
var
  I: Integer;
begin
  for I := 101 to 699 do begin
    Self.Response.StatusCode := I;
    Check(not Self.Response.IsTrying,
          'StatusCode ' + IntToStr(I) + ' ' + Self.Response.StatusText);
  end;

  Self.Response.StatusCode := SIPTrying;
  Check(Self.Response.IsTrying, Self.Response.StatusText);
end;

initialization
  RegisterTest('SIP Messages', Suite);
end.
