unit TestIdSipMessage;

interface

uses
  IdSipHeaders, IdSipMessage, IdURI, TestFramework, TestFrameworkEx;

type
  TestTIdSipMessage = class(TExtendedTestCase)
  private
    procedure CheckEquals(Expected, Received: TIdURI; Message: String); overload;
  protected
    Message: TIdSipMessage;
  public
    procedure TearDown; override;
  published
    procedure TestAddHeader;
    procedure TestAddHeaderName;
    procedure TestAddHeaders;
    procedure TestAsStringNoMaxForwardsSet;
    procedure TestClearHeaders;
    procedure TestFirstContact;
    procedure TestFirstHeader;
    procedure TestHeaderAt;
    procedure TestHeaderCount;
    procedure TestLastHop;
    procedure TestReadBody;
    procedure TestRemoveHeader;
    procedure TestSetCallID;
    procedure TestSetContentLength;
    procedure TestSetContentType;
    procedure TestSetCSeq;
    procedure TestSetFrom;
    procedure TestSetMaxForwards;
    procedure TestSetSipVersion;
    procedure TestSetTo;
  end;

  TestTIdSipRequest = class(TestTIdSipMessage)
  private
    ReceivedRequest: TIdSipRequest;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure TestCreateDialogID;
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
    procedure TestIsRequest;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteServer;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
    procedure TestSetPath;
  end;

  TestTIdSipResponse = class(TestTIdSipMessage)
  private
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
  published
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure TestCreateDialogID;
    procedure TestIsEqualToComplexMessages;
    procedure TestIsEqualToDifferentHeaders;
    procedure TestIsEqualToDifferentSipVersion;
    procedure TestIsEqualToDifferentStatusCode;
    procedure TestIsEqualToDifferentStatusText;
    procedure TestIsEqualToRequest;
    procedure TestIsEqualToTrivial;
    procedure TestIsFinal;
    procedure TestIsProvisional;
    procedure TestIsRequest;
  end;

implementation

uses
  Classes, IdSipConsts, IdSipDialogID, SysUtils, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Messages)');

  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
end;

//******************************************************************************
//* TestTIdSipMessage                                                          *
//******************************************************************************
//* TestTIdSipMessage Public methods *******************************************

procedure TestTIdSipMessage.TearDown;
begin
  Self.Message.Free;

  inherited TearDown;
end;

//* TestTIdSipMessage Private methods ******************************************

procedure TestTIdSipMessage.CheckEquals(Expected, Received: TIdURI; Message: String);
begin
  CheckEquals(Expected.GetFullURI, Received.GetFullURI, Message);
end;

//* TestTIdSipMessage Published methods ****************************************

procedure TestTIdSipMessage.TestAddHeader;
var
  H: TIdSipHeader;
begin
  Self.Message.ClearHeaders;

  H := TIdSipHeader.Create;
  try
    H.Name := UserAgentHeader;
    H.Value := 'Dog''s breakfast v0.1';

    Self.Message.AddHeader(H);

    Check(Self.Message.HasHeader(UserAgentHeader), 'No header added');

    CheckEquals(H.Name,
                Self.Message.Headers.Items[0].Name,
                'Name not copied');

    CheckEquals(H.Value,
                Self.Message.Headers.Items[0].Value,
                'Value not copied');
  finally
    H.Free;
  end;

  CheckEquals(UserAgentHeader,
              Self.Message.Headers.Items[0].Name,
              'And we check that the header was copied & we''re not merely '
            + 'storing a reference');
end;

procedure TestTIdSipMessage.TestAddHeaderName;
begin
  Self.Message.ClearHeaders;

  CheckNotNull(Self.Message.AddHeader(UserAgentHeader), 'Nil returned');

  Check(Self.Message.HasHeader(UserAgentHeader), 'No header added');
end;

procedure TestTIdSipMessage.TestAddHeaders;
var
  Headers: TIdSipHeaders;
begin
  Self.Message.ClearHeaders;

  Headers := TIdSipHeaders.Create;
  try
    Headers.Add(UserAgentHeader).Value := '0';
    Headers.Add(UserAgentHeader).Value := '1';
    Headers.Add(UserAgentHeader).Value := '2';
    Headers.Add(UserAgentHeader).Value := '3';

    Self.Message.AddHeaders(Headers);
    Self.Message.Headers.IsEqualTo(Headers);
  finally
    Headers.Free;
  end;
end;

procedure TestTIdSipMessage.TestAsStringNoMaxForwardsSet;
begin
  Check(Pos(MaxForwardsHeader, Self.Message.AsString) > 0, 'No Max-Forwards header');
end;

procedure TestTIdSipMessage.TestClearHeaders;
begin
  Self.Message.AddHeader(UserAgentHeader);
  Self.Message.AddHeader(UserAgentHeader);
  Self.Message.AddHeader(UserAgentHeader);
  Self.Message.AddHeader(UserAgentHeader);

  Self.Message.ClearHeaders;

  CheckEquals(0, Self.Message.HeaderCount, 'Headers not cleared');
end;

procedure TestTIdSipMessage.TestFirstContact;
var
  C: TIdSipHeader;
begin
  Self.Message.ClearHeaders;

  CheckNotNull(Self.Message.FirstContact, 'Contact not present');
  CheckEquals(1, Self.Message.HeaderCount, 'Contact not auto-added');

  C := Self.Message.FirstHeader(ContactHeaderFull);
  Self.Message.AddHeader(ContactHeaderFull);

  Check(C = Self.Message.FirstContact, 'Wrong Contact');
end;

procedure TestTIdSipMessage.TestFirstHeader;
var
  H: TIdSipHeader;
begin
  Self.Message.ClearHeaders;
  H := Self.Message.AddHeader(UserAgentHeader);
  Check(H = Self.Message.FirstHeader(UserAgentHeader),
        'Wrong result returned for first User-Agent');

  H := Self.Message.AddHeader(RouteHeader);
  Check(H = Self.Message.FirstHeader(RouteHeader),
        'Wrong result returned for first Route');

  H := Self.Message.AddHeader(RouteHeader);
  Check(H <> Self.Message.FirstHeader(RouteHeader),
        'Wrong result returned for first Route of two');
end;

procedure TestTIdSipMessage.TestHeaderAt;
var
  S: TIdSipHeader;
  I: TIdSipHeader;
  P: TIdSipHeader;
begin
  Self.Message.ClearHeaders;

  S := Self.Message.AddHeader(ServerHeader);
  Self.Message.AddHeader(RouteHeader);
  I := Self.Message.AddHeader(InReplyToHeader);
  P := Self.Message.AddHeader(PriorityHeader);

  Check(S = Self.Message.HeaderAt(0), 'Wrong header at index 0');
  Check(I = Self.Message.HeaderAt(2), 'Wrong header at index 2');
  Check(P = Self.Message.HeaderAt(3), 'Wrong header at index 3');
end;

procedure TestTIdSipMessage.TestHeaderCount;
begin
  Self.Message.ClearHeaders;
  Self.Message.AddHeader(UserAgentHeader);

  CheckEquals(1, Self.Message.HeaderCount, 'HeaderCount not correct');
end;

procedure TestTIdSipMessage.TestLastHop;
begin
  Self.Message.ClearHeaders;
  Check(Self.Message.LastHop = Self.Message.FirstHeader(ViaHeaderFull), 'Unexpected return for empty path');

  Self.Message.AddHeader(ViaHeaderFull);
  Check(Self.Message.LastHop = Self.Message.Path.LastHop, 'Unexpected return');
end;

procedure TestTIdSipMessage.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Self.Message.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Self.Message.ReadBody(Str);
    CheckEquals('Negotium', Self.Message.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipMessage.TestRemoveHeader;
begin
  Self.Message.ClearHeaders;

  Self.Message.AddHeader(ContentTypeHeaderFull);
  Check(Self.Message.HasHeader(ContentTypeHeaderFull),
        'Content-Type wasn''t Add()ed');

  Self.Message.RemoveHeader(Self.Message.FirstHeader(ContentTypeHeaderFull));
  Check(not Self.Message.HasHeader(ContentTypeHeaderFull),
        'Content-Type wasn''t Remove()ed');
end;

procedure TestTIdSipMessage.TestSetCallID;
begin
  Self.Message.CallID := '999';

  Self.Message.CallID := '42';
  CheckEquals('42', Self.Message.CallID, 'Call-ID not set');
end;

procedure TestTIdSipMessage.TestSetContentLength;
begin
  Self.Message.ContentLength := 999;

  Self.Message.ContentLength := 42;
  CheckEquals(42, Self.Message.ContentLength, 'Content-Length not set');
end;

procedure TestTIdSipMessage.TestSetContentType;
begin
  Self.Message.ContentType := 'text/plain';

  Self.Message.ContentType := 'text/t140';
  CheckEquals('text/t140', Self.Message.ContentType, 'Content-Type not set');
end;

procedure TestTIdSipMessage.TestSetCSeq;
var
  C: TIdSipCSeqHeader;
begin
  C := TIdSipCSeqHeader.Create;
  try
    C.Value := '314159 INVITE';

    Self.Message.CSeq := C;

    Check(Self.Message.CSeq.IsEqualTo(C), 'CSeq not set');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipMessage.TestSetFrom;
var
  From: TIdSipFromHeader;
begin
  Self.Message.From.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'Case <sip:case@fried.neurons.org>';

    Self.Message.From := From;

    CheckEquals(From.Value, Self.Message.From.Value, 'From value not set');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipMessage.TestSetMaxForwards;
var
  OrigMaxForwards: Byte;
begin
  OrigMaxForwards := Self.Message.MaxForwards;

  Self.Message.MaxForwards := Self.Message.MaxForwards + 1;

  CheckEquals(OrigMaxForwards + 1, Self.Message.MaxForwards, 'Max-Forwards not set');
end;

procedure TestTIdSipMessage.TestSetSipVersion;
begin
  Self.Message.SIPVersion := 'SIP/2.0';

  Self.Message.SIPVersion := 'SIP/7.7';
  CheckEquals('SIP/7.7', Self.Message.SipVersion, 'SipVersion not set');
end;

procedure TestTIdSipMessage.TestSetTo;
var
  ToHeader: TIdSipToHeader;
begin
  Self.Message.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';

  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Value := 'Case <sip:case@fried.neurons.org>';

    Self.Message.ToHeader := ToHeader;

    CheckEquals(ToHeader.Value, Self.Message.ToHeader.Value, 'To value not set');
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
    Self.Message         := P.ParseAndMakeRequest(BasicRequest);
    Self.ReceivedRequest := P.ParseAndMakeRequest(BasicRequest);
    Self.Response        := P.ParseAndMakeResponse(BasicResponse);
  finally
    P.Free;
  end;

  Self.Request := Self.Message as TIdSipRequest;
end;

procedure TestTIdSipRequest.TearDown;
begin
  Self.Response.Free;
  Self.ReceivedRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipRequest Published methods ****************************************

procedure TestTIdSipRequest.TestAssign;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.SIPVersion := 'SIP/1.5';
    R.Method := 'NewMethod';
    R.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.lu';
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

procedure TestTIdSipRequest.TestCreateDialogID;
var
  ID: TIdSipDialogID;
begin
  ID := Self.Message.CreateDialogID;
  try
    CheckEquals(Self.Message.CallID,       ID.CallID,    'Call-ID');
    CheckEquals(Self.Message.From.Tag,     ID.LocalTag,  'Local tag');
    CheckEquals(Self.Message.ToHeader.Tag, ID.RemoteTag, 'Remote tag');
  finally
    ID.Free;
  end;
end;

procedure TestTIdSipRequest.TestHasSipsUri;
begin
  Self.Request.RequestUri.URI := 'tel://999';
  Check(not Self.Request.HasSipsUri, 'tel URI');

  Self.Request.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.lu';
  Check(not Self.Request.HasSipsUri, 'sip URI');

  Self.Request.RequestUri.URI := 'sips:wintermute@tessier-ashpool.co.lu';
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
      R1.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.lu';
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

  Self.Message := TIdSipResponse.Create;
  Self.Response := Self.Message as TIdSipResponse;
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
  Self.Response.MaxForwards                            := 70;
  Self.Response.AddHeader(ToHeaderFull).Value          := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>;tag=1928301775';
  Self.Response.AddHeader(FromHeaderFull).Value        := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Self.Response.CallID                                 := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.Response.AddHeader(CSeqHeader).Value            := '314159 INVITE';
  Self.Response.AddHeader(ContactHeaderFull).Value     := '<sip:wintermute@tessier-ashpool.co.lu>';
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

procedure TestTIdSipResponse.TestCreateDialogID;
var
  ID: TIdSipDialogID;
begin
  ID := Self.Message.CreateDialogID;
  try
    CheckEquals(Self.Message.CallID,       ID.CallID,    'Call-ID');
    CheckEquals(Self.Message.From.Tag,     ID.RemoteTag, 'Remote tag');
    CheckEquals(Self.Message.ToHeader.Tag, ID.LocalTag,  'Local tag');
  finally
    ID.Free;
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

initialization
  RegisterTest('SIP Messages', Suite);
end.
