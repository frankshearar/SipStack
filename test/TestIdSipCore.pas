unit TestIdSipCore;

interface

uses
  IdSipCore, IdSipDialog, IdSipDialogID, IdSipHeaders, IdSipMessage,
  IdSipMockTransactionDispatcher, IdSipTransaction, IdURI, TestFramework,
  TestFrameworkSip;

type
  TestTIdSipAbstractCore = class(TTestCase)
  private
    Core: TIdSipAbstractCore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNextCallID;
  end;

  TestTIdSipUserAgentCore = class(TTestCaseSip, IIdSipSessionListener)
  private
    Core:                      TIdSipUserAgentCore;
    Dlg:                       TIdSipDialog;
    Destination:               TIdSipToHeader;
    Dispatch:                  TIdSipMockTransactionDispatcher;
    ID:                        TIdSipDialogID;
    LocalSequenceNo:           Cardinal;
    LocalUri:                  TIdURI;
    OnEndedSessionFired:       Boolean;
    OnEstablishedSessionFired: Boolean;
    OnNewSessionFired:         Boolean;
    RemoteSequenceNo:          Cardinal;
    RemoteTarget:              TIdURI;
    RemoteUri:                 TIdURI;
    Request:                   TIdSipRequest;
    RouteSet:                  TIdSipHeaders;
    Session:                   TIdSipSession;

    procedure CheckCreateRequest(const Dest: TIdSipToHeader; const Request: TIdSipRequest);
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnNewSession(const Session: TIdSipSession);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAllowedLanguage;
    procedure TestAddAllowedLanguageLanguageAlreadyPresent;
    procedure TestAddAllowedMethod;
    procedure TestAddAllowedMethodMethodAlreadyPresent;
    procedure TestAddAllowedScheme;
    procedure TestAddAllowedSchemeSchemeAlreadyPresent;
    procedure TestAddSessionListener;
    procedure TestCreateBye;
    procedure TestCreateCancel;
    procedure TestCreateInvite;
    procedure TestCreateRequest;
    procedure TestCreateRequestInDialog;
    procedure TestCreateRequestInDialogRouteSetEmpty;
    procedure TestCreateRequestInDialogRouteSetWithLrParam;
    procedure TestCreateRequestInDialogRouteSetWithoutLrParam;
    procedure TestCreateRequestSipsRequestUri;
    procedure TestCreateRequestUserAgent;
    procedure TestCreateResponse;
    procedure TestCreateResponseRecordRoute;
    procedure TestCreateResponseSipsRecordRoute;
    procedure TestCreateResponseSipsRequestUri;
    procedure TestCreateResponseTryingWithTimestamps;
    procedure TestCreateResponseUserAgent;
    procedure TestDialogLocalSequenceNoMonotonicallyIncreases;
    procedure TestHasUnknownContentEncoding;
    procedure TestHasUnknownContentType;
    procedure TestIsMethodAllowed;
    procedure TestIsSchemeAllowed;
    procedure TestLoopDetection;
    procedure TestNextTag;
    procedure TestNotificationOfNewSession;
    procedure TestReceiveByeForUnmatchedDialog;
    procedure TestReceiveByeForDialog;
    procedure TestReceiveByeWithoutTags;
    procedure TestRemoveSessionListener;
    procedure TestRejectNoContact;
    procedure TestRejectUnknownContentEncoding;
    procedure TestRejectUnknownContentLanguage;
    procedure TestRejectUnknownContentType;
    procedure TestRejectUnknownExtension;
    procedure TestRejectUnknownScheme;
    procedure TestRejectUnsupportedMethod;
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
  end;

  TestTIdSipSession = class(TTestCaseSip, IIdSipSessionListener)
  private
    Core:     TIdSipUserAgentCore;
    Dest:     TIdSipToHeader;
    Dispatch: TIdSipMockTransactionDispatcher;
    Invite:   TIdSipRequest;
    Session:  TIdSipSession;

    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnNewSession(const Session: TIdSipSession);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptCall;
    procedure TestCall;
    procedure TestCallSecure;
    procedure TestCallSipsUriOverTcp;
    procedure TestCallSipUriOverTls;
    procedure TestTerminate;
  end;

implementation

uses
  Classes, IdException, IdGlobal, IdHttp, IdSipConsts, SysUtils,
  TestMessages, IdSipTransport, IdSipMockTransport;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipUserAgentCore.Suite);
  Result.AddTest(TestTIdSipSession.Suite);
end;

//******************************************************************************
//* TestTIdSipAbstractCore                                                     *
//******************************************************************************
//* TestTIdSipAbstractCore Public methods **************************************

// Self.Core is an (almost) abstract base class. We want to test the (static)
// methods that are not abstract, and we want to do this without the compiler
// moaning about something we know to be safe.
{$WARNINGS OFF}
procedure TestTIdSipAbstractCore.SetUp;
begin
  inherited SetUp;

  Self.Core := TIdSipAbstractCore.Create;
end;
{$WARNINGS ON}

procedure TestTIdSipAbstractCore.TearDown;
begin
  Self.Core.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractCore Published methods ***********************************

procedure TestTIdSipAbstractCore.TestNextCallID;
var
  CallID: String;
begin
  CallID := Self.Core.NextCallID;

  Fetch(CallID, '@');

  CheckEquals(Self.Core.HostName, CallID, 'HostName not used');
end;

//******************************************************************************
//* TestTIdSipUserAgentCore                                                    *
//******************************************************************************
//* TestTIdSipUserAgentCore Public methods *************************************

procedure TestTIdSipUserAgentCore.SetUp;
var
  C: TIdSipContactHeader;
  F: TIdSipFromHeader;
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo := 13;
  Self.LocalUri        := TIdUri.Create('sip:case@fried.neurons.org');
  Self.LocalSequenceNo := 42;
  Self.RemoteTarget    := TIdUri.Create('sip:sip-proxy1.tessier-ashpool.co.lu');
  Self.RemoteUri       := TIdUri.Create('sip:wintermute@tessier-ashpool.co.lu');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Dlg := TIdSipDialog.Create(Self.ID,
                                  Self.LocalSequenceNo,
                                  Self.RemoteSequenceNo,
                                  Self.LocalUri,
                                  Self.RemoteUri,
                                  Self.RemoteTarget,
                                  false,
                                  Self.RouteSet);

  Self.Destination := TIdSipToHeader.Create;
  Self.Destination.Value := 'sip:franks@localhost';

  Self.Dispatch := TIdSipMockTransactionDispatcher.Create;
  Self.Dispatch.Transport.LocalEchoMessages := false;
  Self.Dispatch.Transport.TransportType := sttTCP;

  Self.Core := TIdSipUserAgentCore.Create;
  Self.Core.Dispatcher := Self.Dispatch;

  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:wintermute@tessier-ashpool.co.lu';
    Self.Core.Contact := C;
  finally
    C.Free;
  end;

  F := TIdSipFromHeader.Create;
  try
    F.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
    Self.Core.From := F;
  finally
    F.Free;
  end;

  P := TIdSipParser.Create;
  try
    Self.Request := P.ParseAndMakeRequest(BasicRequest);
    Self.Request.RemoveAllHeadersNamed(ContentTypeHeaderFull);
    Self.Request.Body := '';
    Self.Request.ToHeader.Value := Self.Request.ToHeader.DisplayName
                                 + ' <' + Self.Request.ToHeader.Address.GetFullURI + '>';
  finally
    P.Free;
  end;
  Self.Request.ContentType := SdpMimeType;

  Self.OnNewSessionFired         := false;
  Self.OnEstablishedSessionFired := false;
end;

procedure TestTIdSipUserAgentCore.TearDown;
begin
  Self.Request.Free;
  Self.Core.Free;
  Self.Dispatch.Free;
  Self.Destination.Free;
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.RemoteUri.Free;
  Self.RemoteTarget.Free;
  Self.LocalUri.Free;
  Self.ID.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentCore Private methods ************************************

procedure TestTIdSipUserAgentCore.CheckCreateRequest(const Dest: TIdSipToHeader; const Request: TIdSipRequest);
var
  Contact: TIdSipContactHeader;
begin
  CheckEquals(Dest.Address,
              Request.RequestUri,
              'Request-URI not properly set');

  Check(Request.HasHeader(CallIDHeaderFull), 'No Call-ID header added');
  CheckNotEquals('',
                 (Request.FirstHeader(CallIDHeaderFull) as TIdSipCallIdHeader).Value,
                 'Call-ID must not be empty');

  Check(Request.HasHeader(ContactHeaderFull), 'No Contact header added');
  Contact := Request.FirstContact;
  Check(Contact.IsEqualTo(Self.Core.Contact), 'Contact header incorrectly set');

  CheckEquals(Request.From.DisplayName,
              Self.Core.From.DisplayName,
              'From.DisplayName');
  CheckEquals(Request.From.Address,
              Self.Core.From.Address,
              'From.Address');
    Check(Request.From.HasTag,
          'Requests MUST have a From tag; cf. RFC 3261 section 8.1.1.3');

  CheckEquals(Request.RequestUri,
              Request.ToHeader.Address,
              'To header incorrectly set');

  CheckEquals(1,
              Request.Path.Length,
              'New requests MUST have a Via header; cf. RFC 3261 section 8.1.1.7');
  Check(Request.LastHop.HasBranch,
        'New requests MUST have a branch; cf. RFC 3261 section 8.1.1.7');

  // optional headers
  Check(not Request.HasHeader(UserAgentHeader),
        'User-Agent header present when Core''s User-Agent name is blank');
end;

procedure TestTIdSipUserAgentCore.OnEndedSession(const Session: TIdSipSession);
begin
  Self.OnEndedSessionFired := true;
end;

procedure TestTIdSipUserAgentCore.OnEstablishedSession(const Session: TIdSipSession);
begin
  Self.OnEstablishedSessionFired := true;
end;

procedure TestTIdSipUserAgentCore.OnNewSession(const Session: TIdSipSession);
begin
  Self.OnNewSessionFired := true;

  Self.Session := Session;
end;

//* TestTIdSipUserAgentCore Published methods **********************************

procedure TestTIdSipUserAgentCore.TestAddAllowedLanguage;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('af');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(2, Languages.Count, 'Number of allowed Languages');

    CheckEquals('en', Languages[0], 'en first');
    CheckEquals('af', Languages[1], 'af second');
  finally
    Languages.Free;
  end;

  try
    Self.Core.AddAllowedLanguage(' ');
    Fail('Failed to forbid adding a malformed language ID');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddAllowedLanguageLanguageAlreadyPresent;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('en');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(1, Languages.Count, 'en was re-added');
  finally
    Languages.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddAllowedMethod;
var
  Methods: TStrings;
begin
  Methods := TStringList.Create;
  try
    Self.Core.AddAllowedMethod(MethodOptions);

    Methods.CommaText := Self.Core.AllowedMethods;

    CheckEquals(4, Methods.Count, 'Number of allowed methods');

    CheckEquals(MethodBye,     Methods[0], 'BYE first');
    CheckEquals(MethodCancel,  Methods[1], 'CANCEL second');
    CheckEquals(MethodInvite,  Methods[2], 'INVITE third');
    CheckEquals(MethodOptions, Methods[3], 'OPTIONS fourth');
  finally
    Methods.Free;
  end;

  try
    Self.Core.AddAllowedMethod(' ');
    Fail('Failed to forbid adding a non-token Method');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddAllowedMethodMethodAlreadyPresent;
var
  Methods: TStrings;
  MethodCount: Cardinal;
begin
  Methods := TStringList.Create;
  try
    Methods.CommaText := Self.Core.AllowedMethods;
    MethodCount := Methods.Count;

    Self.Core.AddAllowedMethod(MethodInvite);
    Methods.CommaText := Self.Core.AllowedMethods;

    CheckEquals(MethodCount, Methods.Count, MethodInvite + ' was re-added');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddAllowedScheme;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipsScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(2, Schemes.Count, 'Number of allowed Schemes');

    CheckEquals(SipScheme,  Schemes[0], 'SIP first');
    CheckEquals(SipsScheme, Schemes[1], 'SIPS second');
  finally
    Schemes.Free;
  end;

  try
    Self.Core.AddAllowedScheme(' ');
    Fail('Failed to forbid adding a malformed URI scheme');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddAllowedSchemeSchemeAlreadyPresent;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(1, Schemes.Count, 'SipScheme was re-added');
  finally
    Schemes.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
  Tran:   TIdSipTransaction;
begin
  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Core.AddSessionListener(L1);
      Self.Core.AddSessionListener(L2);

      Tran := Self.Dispatch.AddServerTransaction(Self.Request, Self.Dispatch.Transport);
      Tran.ReceiveRequest(Self.Request, Self.Dispatch.Transport);

      Check(L1.NewSession and L2.NewSession, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Core.CreateBye(Self.Dlg);
  try
    CheckEquals(MethodBye, Bye.Method, 'Unexpected method');
    CheckEquals(Bye.Method,
                Bye.CSeq.Method,
                'CSeq method doesn''t match request method');
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.Core.CreateCancel(Self.Dlg);
  try
    CheckEquals(MethodCancel, Cancel.Method, 'Unexpected method');
    CheckEquals(Cancel.Method,
                Cancel.CSeq.Method,
                'CSeq method doesn''t match request method');
  finally
    Cancel.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateInvite;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateInvite(Dest);
    try
      Self.CheckCreateRequest(Dest, Request);
      CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

      Check(not Request.ToHeader.HasTag,
            'This request is outside of a dialog, hence MUST NOT have a '
          + 'To tag. See RFC:3261, section 8.1.1.2');

      Check(Request.HasHeader(CSeqHeader), 'No CSeq header');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequest;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateRequest(Dest);
    try
      Self.CheckCreateRequest(Dest, Request);
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestInDialog;
var
  R: TIdSipRequest;
begin
  R := Self.Core.CreateRequest(Dlg);
  try
    CheckEquals(Self.Dlg.RemoteURI,    R.ToHeader.Address, 'To URI');
    CheckEquals(Self.Dlg.ID.RemoteTag, R.ToHeader.Tag,     'To tag');
    CheckEquals(Self.Dlg.LocalURI,     R.From.Address,     'From URI');
    CheckEquals(Self.Dlg.ID.LocalTag,  R.From.Tag,         'From tag');
    CheckEquals(Self.Dlg.ID.CallID,    R.CallID,           'Call-ID');

    // we should somehow check that CSeq.SequenceNo has been (randomly) generated. How?
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestInDialogRouteSetEmpty;
var
  P:        TIdSipParser;
  R:        TIdSipRequest;
  Response: TIdSipResponse;
  Routes:   TIdSipHeadersFilter;
begin
  P := TIdSipParser.Create;
  try
    Response := P.ParseAndMakeResponse(LocalLoopResponse);
    try
      Response.StatusCode := SIPTrying;
      Self.Dispatch.Transport.FireOnResponse(Response);
    finally
      Response.Free;
    end;
  finally
    P.Free;
  end;

  Self.Dlg.RouteSet.Clear;

  R := Self.Core.CreateRequest(Self.Dlg);
  try
    CheckEquals(Self.Dlg.RemoteTarget,
                R.RequestUri,
                'Request-URI');

    Routes := TIdSipHeadersFilter.Create(R.Headers, RouteHeader);
    try
      Check(Routes.IsEmpty, 'Route headers are present');
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestInDialogRouteSetWithLrParam;
var
  R:      TIdSipRequest;
  Routes: TIdSipHeadersFilter;
begin
  Self.Dlg.RouteSet.Clear;
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server10.biloxi.com;lr>';
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server9.biloxi.com>';
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server8.biloxi.com;lr>';

  R := Self.Core.CreateRequest(Dlg);
  try
    CheckEquals(Self.Dlg.RemoteTarget,
                R.RequestUri,
                'Request-URI');

    Routes := TIdSipHeadersFilter.Create(R.Headers, RouteHeader);
    try
      Check(Routes.IsEqualTo(Self.Dlg.RouteSet),
            'Route headers not set to the Dialog route set');
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestInDialogRouteSetWithoutLrParam;
var
  I:        Integer;
  P:        TIdSipParser;
  R:        TIdSipRequest;
  Response: TIdSipResponse;
  Routes:   TIdSipHeadersFilter;
begin
  P := TIdSipParser.Create;
  try
    Response := P.ParseAndMakeResponse(LocalLoopResponse);
    try
      Response.StatusCode := SIPTrying;
      Self.Dispatch.Transport.FireOnResponse(Response);
    finally
      Response.Free;
    end;
  finally
    P.Free;
  end;

  R := Self.Core.CreateRequest(Dlg);
  try
    CheckEquals((Self.Dlg.RouteSet.Items[0] as TIdSipRouteHeader).Address,
                R.RequestUri,
                'Request-URI');

    Routes := TIdSipHeadersFilter.Create(R.Headers, RouteHeader);
    try
      // These are the manipulations the dialog's meant to perform on its route
      // set. Just so you know we're not fiddling our test data.
      Self.Dlg.RouteSet.Delete(0);
      Self.Dlg.RouteSet.Add(RouteHeader).Value := '<' + Self.Dlg.RemoteURI.GetFullURI + '>';

      for I := 0 to Routes.Count - 1 do
        CheckEquals(Self.Dlg.RouteSet.Items[I].Value,
                    Routes.Items[I].Value,
                    'Route ' + IntToStr(I + 1) + ' value');
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sips:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateRequest(Dest);
    try
      Contact := Request.FirstContact;
      CheckEquals(SipsScheme,
                  Contact.Address.Protocol,
                  'Contact doesn''t have a SIPS URI');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestUserAgent;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';

  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateRequest(Dest);
    try
      CheckEquals(Self.Core.UserAgentName,
                  Request.FirstHeader(UserAgentHeader).Value,
                  'User-Agent header not set');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponse;
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    CheckEquals(SIPOK, Response.StatusCode,              'StatusCode mismatch');
    Check(Response.CSeq.IsEqualTo(Request.CSeq),         'Cseq header mismatch');
    Check(Response.From.IsEqualTo(Request.From),         'From header mismatch');
    Check(Response.ToHeader.IsEqualTo(Request.ToHeader), 'To header mismatch');
    Check(Response.Path.IsEqualTo(Request.Path),         'Via headers mismatch');

    Check(Response.HasHeader(ContactHeaderFull), 'Missing Contact header');

    Check(not Response.HasHeader(UserAgentHeader),
          'User-Agent header present when Core''s User-Agent name is blank');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseRecordRoute;
var
  RequestRecordRoutes:  TIdSipHeadersFilter;
  Response:             TIdSipResponse;
  ResponseRecordRoutes: TIdSipHeadersFilter;
begin
  Self.Request.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.Request.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:6001>';
  Self.Request.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:6002>';

  RequestRecordRoutes := TIdSipHeadersFilter.Create(Self.Request.Headers, RecordRouteHeader);
  try
    Response := Self.Core.CreateResponse(Self.Request, SIPOK);
    try
      ResponseRecordRoutes := TIdSipHeadersFilter.Create(Response.Headers, RecordRouteHeader);
      try
        Check(ResponseRecordRoutes.IsEqualTo(RequestRecordRoutes),
              'Record-Route header sets mismatch');
      finally
        ResponseRecordRoutes.Free;
      end;
    finally
      Response.Free;
    end;
  finally
    RequestRecordRoutes.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseSipsRecordRoute;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
  Self.Request.AddHeader(RecordRouteHeader).Value := '<sips:127.0.0.1:6000>';

  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    Contact := Response.FirstContact;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseSipsRequestUri;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
  Self.Request.RequestUri.URI := 'sips:wintermute@tessier-ashpool.co.lu';

  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    Contact := Response.FirstContact;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseTryingWithTimestamps;
var
  Response: TIdSipResponse;
begin
  Self.Request.AddHeader(TimestampHeader).Value := '1';

  Response := Self.Core.CreateResponse(Self.Request, SIPTrying);
  try
    Check(Response.HasHeader(TimestampHeader),
          'Timestamp header(s) not copied');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseUserAgent;
var
  Response: TIdSipResponse;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';
  Self.Request.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.lu';

  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    CheckEquals(Self.Core.UserAgentName,
                Response.FirstHeader(UserAgentHeader).Value,
                'User-Agent header not set');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestDialogLocalSequenceNoMonotonicallyIncreases;
var
  BaseSeqNo: Cardinal;
  R:         TIdSipRequest;
begin
  R := Self.Core.CreateRequest(Self.Dlg);
  try
     BaseSeqNo := R.CSeq.SequenceNo;
  finally
    R.Free;
  end;

  R := Self.Core.CreateRequest(Self.Dlg);
  try
    CheckEquals(BaseSeqNo + 1,
                R.CSeq.SequenceNo,
                'Not monotonically increasing by one');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestHasUnknownContentEncoding;
begin
  Self.Request.Headers.Remove(Self.Request.FirstHeader(ContentEncodingHeaderFull));

  Check(not Self.Core.HasUnknownContentEncoding(Self.Request),
        'Vacuously true');

  Self.Request.AddHeader(ContentEncodingHeaderFull);
  Check(Self.Core.HasUnknownContentEncoding(Self.Request),
        'No encodings are supported');
end;

procedure TestTIdSipUserAgentCore.TestHasUnknownContentType;
begin
  Self.Request.RemoveHeader(Self.Request.FirstHeader(ContentTypeHeaderFull));

  Check(not Self.Core.HasUnknownContentType(Self.Request),
        'Vacuously true');

  Self.Request.AddHeader(ContentTypeHeaderFull).Value := SdpMimeType;
  Check(not Self.Core.HasUnknownContentType(Self.Request),
        SdpMimeType + ' MUST supported');

  Self.Request.RemoveHeader(Self.Request.FirstHeader(ContentTypeHeaderFull));
  Self.Request.AddHeader(ContentTypeHeaderFull);
  Check(Self.Core.HasUnknownContentType(Self.Request),
        'Nothing else is supported');
end;

procedure TestTIdSipUserAgentCore.TestIsMethodAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not allowed');

  Self.Core.AddAllowedMethod(MethodRegister);
  Check(Self.Core.IsMethodAllowed(MethodRegister),
        MethodRegister + ' not recognised as an allowed method');

  Check(not Self.Core.IsMethodAllowed(' '),
        ''' '' not recognised as an allowed method');
end;

procedure TestTIdSipUserAgentCore.TestIsSchemeAllowed;
begin
  Check(not Self.Core.IsMethodAllowed(SipScheme),
        SipScheme + ' not allowed');

  Self.Core.AddAllowedScheme(SipScheme);
  Check(Self.Core.IsSchemeAllowed(SipScheme),
        SipScheme + ' not recognised as an allowed scheme');

  Check(not Self.Core.IsSchemeAllowed(' '),
        ''' '' not recognised as an allowed scheme');
end;

procedure TestTIdSipUserAgentCore.TestLoopDetection;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
  Tran:          TIdSipTransaction;
begin
  // cf. RFC 3261, section 8.2.2.2
  Tran := Self.Dispatch.AddServerTransaction(Self.Request, Self.Dispatch.Transport);

  // wipe out the tag & give a different branch
  Self.Request.ToHeader.Value := Self.Request.ToHeader.Address.GetFullURI;
  Self.Request.LastHop.Branch := Self.Request.LastHop.Branch + '1';

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Core.ReceiveRequest(Self.Request, Tran, Self.Dispatch.Transport);
  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPLoopDetected, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestNextTag;
var
  I:    Integer;
  Tags: TStringList;
begin
  // This is a woefully inadequate test. cf. RFC 3261, section 19.3

  Tags := TStringList.Create;
  try
    for I := 1 to 100 do
      Tags.Add(Self.Core.NextTag);

    // Find duplicates
    Tags.Sort;
    CheckNotEquals('', Tags[0], 'No null tags may be generated');

    for I := 1 to Tags.Count - 1 do begin
      CheckNotEquals('', Tags[I], 'No null tags may be generated (Tag #'
                                + IntToStr(I) + ')');

      CheckNotEquals(Tags[I-1], Tags[I], 'Duplicate tag generated');
    end;
  finally
  end;
end;

procedure TestTIdSipUserAgentCore.TestNotificationOfNewSession;
var
  Tran: TIdSipTransaction;
begin
  Self.Core.AddSessionListener(Self);

  Tran := Self.Dispatch.AddServerTransaction(Self.Request, Self.Dispatch.Transport);
  Tran.ReceiveRequest(Self.Request, Self.Dispatch.Transport);

  Check(Self.OnNewSessionFired, 'UI not notified of new session');
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeForUnmatchedDialog;
var
  Bye:           TIdSipRequest;
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Bye := Self.Core.CreateRequest(Self.Destination);
  try
    Bye.Method          := MethodBye;
    Bye.CSeq.SequenceNo := $deadbeef;
    Bye.CSeq.Method     := Bye.Method;

    ResponseCount := Self.Dispatch.Transport.SentResponseCount;

    Self.Dispatch.Transport.FireOnRequest(Bye);

    CheckEquals(ResponseCount + 1,
                Self.Dispatch.Transport.SentResponseCount,
                'no response sent');
    Response := Self.Dispatch.Transport.LastResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')

  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeForDialog;
var
  Bye:           TIdSipRequest;
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
  Temp:          String;
begin
  Self.Core.AddSessionListener(Self);

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Assigned(Self.Session), 'OnNewSession didn''t fire');
  Self.Session.AcceptCall;

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;
  Bye := Self.Core.CreateBye(Self.Session.Dialog);
  try
    // Because this BYE actually comes from the network, its from/to headers will
    // be the REVERSE of a locally generated BYE
    Temp := Bye.ToHeader.Value + Bye.ToHeader.ParamsAsString;
    Bye.ToHeader.Value := Bye.From.Value + Bye.From.ParamsAsString;
    Bye.From.Value := Temp;

    Self.Dispatch.Transport.FireOnRequest(Bye);
  finally
    Bye.Free;
  end;

  CheckEquals(ResponseCount + 1,
              Self.Dispatch.Transport.SentResponseCount,
              'SOMETHING should have sent a response');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckNotEquals(SIPCallLegOrTransactionDoesNotExist,
                 Response.StatusCode,
                 'UA tells us no matching dialog was found');
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeWithoutTags;
var
  Bye:           TIdSipRequest;
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Bye := Self.Core.CreateRequest(Self.Destination);
  try
    Bye.Method          := MethodBye;
    Bye.From.Value      := Bye.From.Address.URI;     // strip the tag
    Bye.ToHeader.Value  := Bye.ToHeader.Address.URI; // strip the tag
    Bye.CSeq.SequenceNo := $deadbeef;
    Bye.CSeq.Method     := Bye.Method;

    ResponseCount := Self.Dispatch.Transport.SentResponseCount;

    Self.Dispatch.Transport.FireOnRequest(Bye);

    CheckEquals(ResponseCount + 1,
                Self.Dispatch.Transport.SentResponseCount,
                'no response sent');
    Response := Self.Dispatch.Transport.LastResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRemoveSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
  Tran:   TIdSipTransaction;
begin
  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Core.AddSessionListener(L1);
      Self.Core.AddSessionListener(L2);
      Self.Core.RemoveSessionListener(L2);

      Tran := Self.Dispatch.AddServerTransaction(Self.Request, Self.Dispatch.Transport);
      Tran.ReceiveRequest(Self.Request, Self.Dispatch.Transport);

      Check(L1.NewSession and not L2.NewSession,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRejectNoContact;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Request.RemoveHeader(Self.Request.FirstContact);

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPBadRequest,        Response.StatusCode, 'Status-Code');
  CheckEquals(MissingContactHeader, Response.StatusText, 'Status-Text');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentEncoding;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Request.FirstHeader(ContentTypeHeaderFull).Value := SdpMimeType;

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Request.AddHeader(ContentEncodingHeaderFull).Value := 'gzip';

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptEncodingHeader), 'No Accept-Encoding header');
  CheckEquals('',
              Response.FirstHeader(AcceptEncodingHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentLanguage;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Core.AddAllowedLanguage('fr');

  Self.Request.AddHeader(ContentLanguageHeader).Value := 'en_GB';

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptLanguageHeader), 'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).Value,
              'Accept-Language value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentType;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Request.ContentType := 'text/xml';

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptHeader), 'No Accept header');
  CheckEquals(SdpMimeType,
              Response.FirstHeader(AcceptHeader).Value,
              'Accept value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownExtension;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Request.AddHeader(RequireHeader).Value := '100rel';

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPBadExtension, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(UnsupportedHeader), 'No Unsupported header');
  CheckEquals(Self.Request.FirstHeader(RequireHeader).Value,
              Response.FirstHeader(UnsupportedHeader).Value,
              'Unexpected Unsupported header value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownScheme;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Request.RequestUri.URI := 'tel://1';
  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPUnsupportedURIScheme, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnsupportedMethod;
var
  Allow:         TIdSipCommaSeparatedHeader;
  I:             Integer;
  Methods:       TStrings;
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Request.Method := MethodRegister;
  Self.Request.CSeq.Method := Self.Request.Method;

  ResponseCount := Self.Dispatch.Transport.SentResponseCount;

  Self.Dispatch.Transport.FireOnRequest(Self.Request);

  Check(Self.Dispatch.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatch.Transport.LastResponse;
  Check(Response.HasHeader(AllowHeader),
        'Allow header is mandatory. cf. RFC 3261 section 8.2.1');

  Allow := Response.FirstHeader(AllowHeader) as TIdSipCommaSeparatedHeader;
  Methods := TStringList.Create;
  try
    Methods.CommaText := Self.Core.AllowedMethods;

    for I := 0 to Methods.Count - 1 do
      CheckEquals(Methods[I], Allow.Values[I], IntToStr(I + 1) + 'th value');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContact;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:case@fried.neurons.org';
    Self.Core.Contact := C;

    Check(Self.Core.Contact.IsEqualTo(C),
                'Contact not set');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContactMailTo;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := 'mailto:wintermute@tessier-ashpool.co.lu';
      Self.Core.Contact := C;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EAssertionFailed do;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContactWildCard;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := '*';
      Fail('Wildcard Contact headers make no sense in a response that sets up '
         + 'a dialog');
    except
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetFrom;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    F.Value := 'sip:case@fried.neurons.org';
    Self.Core.From := F;

    Check(Self.Core.From.IsEqualTo(F),
                'From not set');
  finally
    F.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetFromMailTo;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    try
      F.Value := 'mailto:wintermute@tessier-ashpool.co.lu';
      Self.Core.From := F;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EAssertionFailed do;
    end;
  finally
    F.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipSession                                                          *
//******************************************************************************
//* TestTIdSipSession Public methods *******************************************

procedure TestTIdSipSession.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.Dest            := TIdSipToHeader.Create;
  Self.Dispatch        := TIdSipMockTransactionDispatcher.Create;
  Self.Core            := TIdSipUserAgentCore.Create;
  Self.Core.Dispatcher := Self.Dispatch;
  Self.Core.AddSessionListener(Self);

  Self.Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';

  P := TIdSipParser.Create;
  try
    Self.Invite := P.ParseAndMakeRequest(LocalLoopRequest);
    Self.Invite.Headers.RemoveAll(ContentTypeHeaderFull);
  finally
    P.Free;
  end;
end;

procedure TestTIdSipSession.TearDown;
begin
  Self.Invite.Free;
  Self.Core.Free;
  Self.Dispatch.Free;
  Self.Dest.Free;

  inherited TearDown;
end;

//* TestTIdSipSession Private methods ******************************************

procedure TestTIdSipSession.OnEndedSession(const Session: TIdSipSession);
begin
end;

procedure TestTIdSipSession.OnEstablishedSession(const Session: TIdSipSession);
begin
end;

procedure TestTIdSipSession.OnNewSession(const Session: TIdSipSession);
begin
  Self.Session := Session;
end;

//* TestTIdSipSession Published methods ****************************************

procedure TestTIdSipSession.TestAcceptCall;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatch.Transport.SentResponseCount;
  Self.Dispatch.Transport.TransportType := sttTCP;

  Self.Dispatch.Transport.FireOnRequest(Self.Invite);
  Check(Assigned(Self.Session), 'OnNewSession didn''t fire');

  Self.Session.AcceptCall;

  Check(ResponseCount < Self.Dispatch.Transport.SentResponseCount,
        'no responses sent');
  CheckNotNull(Session.Dialog, 'Dialog object wasn''t created');

  Response := Self.Dispatch.Transport.LastResponse;
  CheckEquals(SIPOK, Response.StatusCode,      'Status-Code');
  Check(Response.From.HasTag,                  'No From tag');
  Check(Response.ToHeader.HasTag,              'No To tag');
  Check(Response.HasHeader(ContactHeaderFull), 'No Contact header');
end;

procedure TestTIdSipSession.TestCall;
var
  RequestCount: Integer;
  Response:     TIdSipResponse;
  SessCount:    Integer;
  Session:      TIdSipSession;
  TranCount:    Integer;
begin
  RequestCount := Self.Dispatch.Transport.SentRequestCount;
  SessCount    := Self.Core.SessionCount;
  TranCount    := Self.Dispatch.TransactionCount;

  Session := Self.Core.Call(Self.Dest);

  CheckEquals(RequestCount + 1,
              Self.Dispatch.Transport.SentRequestCount,
              'no INVITE sent');

  CheckEquals(TranCount + 1,
              Self.Dispatch.TransactionCount,
              'no client INVITE transaction created');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'no new session created');

  Response := Self.Core.CreateResponse(Self.Dispatch.Transport.LastRequest, 100);
  try
    Self.Dispatch.Transport.FireOnResponse(Response);

    Check(Session.Dialog.IsEarly,
          'Dialog in incorrect state');
    Check(not Session.Dialog.IsSecure,
          'Dialog secure when TLS not used');

    Response.StatusCode := SIPOK;
    Dispatch.Transport.FireOnResponse(Response);

    Check(not Session.Dialog.IsEarly, 'Dialog in incorrect state');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipSession.TestCallSecure;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatch.Transport.TransportType := sttTLS;

  Self.Dest.Address.Protocol := SipsScheme;
  Session := Self.Core.Call(Self.Dest);

  Response := Self.Core.CreateResponse(Self.Dispatch.Transport.LastRequest, 100);
  try
    Self.Dispatch.Transport.FireOnResponse(Response);

    Response.StatusCode := SIPOK;
    Check(Session.Dialog.IsSecure, 'Dialog not secure when TLS used');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipSession.TestCallSipsUriOverTcp;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatch.Transport.TransportType := sttTCP;

  Self.Dest.Address.Protocol := SipsScheme;
  Session := Self.Core.Call(Self.Dest);

  Response := Self.Core.CreateResponse(Self.Dispatch.Transport.LastRequest, 100);
  try
    Self.Dispatch.Transport.FireOnResponse(Response);

    Response.StatusCode := SIPOK;
    Check(not Session.Dialog.IsSecure, 'Dialog secure when TCP used');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipSession.TestCallSipUriOverTls;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatch.Transport.TransportType := sttTCP;

  Session := Self.Core.Call(Self.Dest);

  Response := Self.Core.CreateResponse(Self.Dispatch.Transport.LastRequest, 100);
  try
    Response.FirstContact.Address.Protocol := SipsScheme;
    Response.StatusCode := SIPOK;
    Self.Dispatch.Transport.FireOnResponse(Response);

    Check(not Session.Dialog.IsSecure, 'Dialog secure when TLS used with a SIP URI');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipSession.TestTerminate;
var
  Request:      TIdSipRequest;
  RequestCount: Cardinal;
begin
  RequestCount := Self.Dispatch.Transport.SentRequestCount;

  Self.Dispatch.Transport.FireOnRequest(Self.Invite);
  Check(Assigned(Self.Session), 'OnNewSession didn''t fire');

  Self.Session.AcceptCall;
  Self.Session.Terminate;

  CheckEquals(RequestCount + 1,
              Self.Dispatch.Transport.SentRequestCount,
              'no BYE sent');

  Request := Self.Dispatch.Transport.LastRequest;
  Check(Request.IsBye, 'Unexpected last request');

  Check(Self.Session.IsTerminated, 'Session not marked as terminated');
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
