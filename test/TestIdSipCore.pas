unit TestIdSipCore;

interface

uses
  Classes, IdRTP, IdSdp, IdSimpleParser, IdSipCore, IdSipDialog,
  IdSipDialogID, IdSipMessage, IdSipMockCore, IdSipMockTransactionDispatcher,
  IdSipRegistrar, IdSipTransaction, IdSipTransport, IdSocketHandle,
  TestFramework, TestFrameworkEx, TestFrameworkSip;

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

  TTestCaseTU = class(TTestCaseSip)
  private
    procedure RemoveBody(Msg: TIdSipMessage);
  protected
    Core:        TIdSipUserAgentCore;
    Destination: TIdSipToHeader;
    Dispatcher:  TIdSipMockTransactionDispatcher;
    Invite:      TIdSipRequest;

    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    procedure SimulateRemoteBye(LocalDialog: TIdSipDialog);
    procedure SimulateRemoteInvite;

    procedure SimulateRemoteAccept(Invite: TIdSipRequest);
    procedure SimulateRemoteResponse(StatusCode: Cardinal);
    procedure SimulateRemoteRinging(Invite: TIdSipRequest);
    procedure SimulateRemoteTryingWithNoToTag(Invite: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipAbstractUserAgent = class(TTestCase)
  private
    Dispatcher: TIdSipMockTransactionDispatcher;
    Request:    TIdSipRequest;
    UA:         TIdSipAbstractUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAllowedContentType;
    procedure TestAddAllowedContentTypeMalformed;
    procedure TestAddAllowedLanguage;
    procedure TestAddAllowedLanguageLanguageAlreadyPresent;
    procedure TestAddAllowedMethod;
    procedure TestAddAllowedMethodMethodAlreadyPresent;
    procedure TestAddAllowedScheme;
    procedure TestAddAllowedSchemeSchemeAlreadyPresent;
  end;

  TestTIdSipUserAgentCore = class(TTestCaseTU,
                                  IIdSipObserver,
                                  IIdSipSessionListener,
                                  IIdSipUserAgentListener)
  private
    CheckOnInboundCall: TIdSipSessionEvent;
    Dlg:                TIdSipDialog;
    ID:                 TIdSipDialogID;
    LocalSequenceNo:    Cardinal;
    LocalUri:           TIdSipURI;
    OnInboundCallFired: Boolean;
    RemoteSequenceNo:   Cardinal;
    RemoteTarget:       TIdSipURI;
    RemoteUri:          TIdSipURI;
    RouteSet:           TIdSipHeaders;
    Session:            TIdSipSession;
    SessionEstablished: Boolean;

    procedure CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                         Header: TIdSipHeader;
                                         const Msg: String);
    procedure CheckCreateRequest(Dest: TIdSipToHeader;
                                 Request: TIdSipRequest);
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure SimulateRemoteAck(Response: TIdSipResponse);
    procedure SimulateRemoteBye(Dialog: TIdSipDialog);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddObserver;
    procedure TestAddUserAgentListener;
    procedure TestCallUsingProxy;
    procedure TestContentTypeDefault;
    procedure TestCreateBye;
    procedure TestCreateInvite;
    procedure TestCreateInviteWithBody;
    procedure TestCreateRegister;
    procedure TestCreateRegisterReusesCallIDForSameRegistrar;
    procedure TestCreateReInvite;
    procedure TestCreateRequest;
    procedure TestCreateRequestSipsRequestUri;
    procedure TestCreateRequestUserAgent;
    procedure TestCreateRequestWithTransport;
    procedure TestCreateResponseToTagMissing;
    procedure TestCreateResponseUserAgent;
    procedure TestCreateResponseUserAgentBlank;
    procedure TestDialogLocalSequenceNoMonotonicallyIncreases;
    procedure TestDispatchToCorrectSession;
    procedure TestDispatchAckToSession;
    procedure TestFork;
    procedure TestForkWithProvisionalResponse;
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
    procedure TestReceiveResponseWithMultipleVias;
    procedure TestRemoveObserver;
    procedure TestRemoveRegistration;
    procedure TestRemoveSession;
    procedure TestRemoveUserAgentListener;
    procedure TestRejectNoContact;
    procedure TestRejectUnknownContentEncoding;
    procedure TestRejectUnknownContentLanguage;
    procedure TestRejectUnknownContentType;
    procedure TestRejectUnknownExtension;
    procedure TestRejectUnknownScheme;
    procedure TestRejectUnsupportedMethod;
    procedure TestRejectUnsupportedSipVersion;
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
    procedure TestHangUpAllCalls;
  end;

  TestTIdSipAction = class(TTestCaseTU)
  protected
    ActionFailed: Boolean;

    function  CreateAction: TIdSipAction; virtual; abstract;
    procedure SimulateRemoteBadExtensionResponse;
    procedure SimulateRemoteMovedPermanently(const SipUrl: String);
  public
    procedure SetUp; override;
  published
{
    procedure TestReceiveResponseBadExtension; // Currently our stack can't sent Requires; ergo we can't test in the usual fashion
    procedure TestReceiveResponseBadExtensionWithoutRequires;
}
  end;

  TestTIdSipInboundSession = class(TestTIdSipAction,
                            IIdRTPDataListener,
                            IIdSipSessionListener,
                            IIdSipTransportSendingListener,
                            IIdSipUserAgentListener)
  private
    EstablishedSession:     TIdSipSession;
    MultiStreamSdp:         TIdSdpPayload;
    OnEndedSessionFired:    Boolean;
    OnModifiedSessionFired: Boolean;
    SentRequestTerminated:  Boolean;
    Session:                TIdSipInboundSession;
    SimpleSdp:              TIdSdpPayload;

    procedure CheckServerActiveOn(Port: Cardinal);
    function  CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateMultiStreamSdp: TIdSdpPayload;
    function  CreateSimpleSdp: TIdSdpPayload;
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Transport: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Transport: TIdSipTransport);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test2xxRetransmission;
    procedure TestAcceptCall;
    procedure TestAcceptCallWithUnfulfillableOffer;
    procedure TestAcceptCallRespectsContentType;
    procedure TestAcceptCallStartsListeningMedia;
    procedure TestAddSessionListener;
    procedure TestReceive2xxSendsAck;
    procedure TestReceiveBye;
//    procedure TestReceiveByeWithPendingRequests;
    procedure TestReceiveOutOfOrderRequest;
    procedure TestReceiveReInvite;
    procedure TestRemoveSessionListener;
    procedure TestTerminate;
  end;

  TestTIdSipOutboundSession = class(TestTIdSipAction,
                                    IIdSipSessionListener)
  private
    OnEndedSessionFired:    Boolean;
    OnModifiedSessionFired: Boolean;
    Session:                TIdSipOutboundSession;

    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestCall;
    procedure TestCallTwice;
    procedure TestCallRemoteRefusal;
    procedure TestCallNetworkFailure;
    procedure TestCallSecure;
    procedure TestCallSipsUriOverTcp;
    procedure TestCallSipUriOverTls;
    procedure TestFork;
    procedure TestDialogNotEstablishedOnTryingResponse;
    procedure TestReceiveFinalResponseSendsAck;
    procedure TestTerminateUnestablishedSession;
  end;

  TestBugHunt = class(TThreadingTestCase,
                      IIdSipUserAgentListener)
  private
    Destination: TIdSipToHeader;
    Dispatcher:  TIdSipMockTransactionDispatcher;
    Session:     TIdSipInboundSession;
    ToTag:       String;
    UA:          TIdSipUserAgentCore;
  private
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure SimulateRemoteInvite;
    procedure SimulateRemoteOK;
    procedure SimulateRemoteRinging;
    procedure SimulateRemoteTrying;
    procedure SimulateUnexpectedAck;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOutboundCallAndByeToXlite;
    procedure TestSimultaneousInAndOutboundCall;
    procedure TestXlitesAckBug;
  end;

  TestTIdSipSessionTimer = class(TTestCase)
  private
    Session: TIdSipMockSession;
    Timer:   TIdSipSessionTimer;
    UA:      TIdSipUserAgentCore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFireResendsSessionsLastResponse;
    procedure TestTimerIntervalIncreases;
  end;

  TestTIdSipRegistration = class(TestTIdSipAction,
                                 IIdSipRegistrationListener)
  private
    Challenged:  Boolean;
    Contacts:    TIdSipContacts;
    MinExpires:  Cardinal;
    Reg:         TIdSipRegistration;
    Registrar:   TIdSipRegistrar;
    Request:     TIdSipRequest;
    Succeeded:   Boolean;

    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure SimulateRemoteIntervalTooBrief;
    procedure SimulateRemoteOK;
    procedure SimulateRemoteResponse(StatusCode: Cardinal);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestRegister;
    procedure TestFindCurrentBindings;
    procedure TestReceiveFail;
    procedure TestReceiveIntervalTooBrief;
    procedure TestReceiveIntervalTooBriefForOneContact;
    procedure TestReceiveMovedPermanently;
    procedure TestReceiveOK;
    procedure TestReceiveUnauthorized;
    procedure TestSequenceNumberIncrements;
    procedure TestUnregister;
  end;

const
  DefaultTimeout = 5000;

implementation

uses
  IdException, IdGlobal, IdInterfacedObject, IdSipConsts, IdUdpServer, SyncObjs,
  SysUtils, TestMessages, IdSipMockTransport;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipAbstractUserAgent.Suite);
  Result.AddTest(TestTIdSipUserAgentCore.Suite);
  Result.AddTest(TestTIdSipInboundSession.Suite);
  Result.AddTest(TestTIdSipOutboundSession.Suite);
  Result.AddTest(TestBugHunt.Suite);
  Result.AddTest(TestTIdSipSessionTimer.Suite);
  Result.AddTest(TestTIdSipRegistration.Suite);
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
//* TTestCaseTU                                                                *
//******************************************************************************
//* TTestCaseTU Public methods *************************************************

procedure TTestCaseTU.SetUp;
begin
  inherited SetUp;

  Self.Destination := TIdSipToHeader.Create;
  Self.Destination.Value := 'sip:franks@localhost';

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.Dispatcher.Transport.LocalEchoMessages := false;
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Self.Core := TIdSipUserAgentCore.Create;
  Self.Core.Dispatcher := Self.Dispatcher;

  Self.Core.Contact.Value := 'sip:wintermute@localhost';
  Self.Core.From.Value    := 'sip:wintermute@localhost';

  Self.Invite := TIdSipTestResources.CreateBasicRequest;
  Self.RemoveBody(Self.Invite);
end;

procedure TTestCaseTU.TearDown;
begin
  Self.Invite.Free;
  Self.Core.Free;
  Self.Dispatcher.Free;
  Self.Destination.Free;

  inherited TearDown;
end;

//* TTestCaseTU Protected methods **********************************************

function TTestCaseTU.CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
var
  Temp: String;
begin
  Result := Self.Core.CreateBye(LocalDialog);
  try
    // Because this BYE actually comes from the network, its from/to headers will
    // be the REVERSE of a locally generated BYE
    Temp                  := Result.ToHeader.FullValue;
    Result.ToHeader.Value := Result.From.FullValue;
    Result.From.Value     := Temp;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TTestCaseTU.SimulateRemoteAccept(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  // This message appears to originate from the network. Invite originates from
  // us so presumably has no To tag. Having come from the network, the response
  // WILL have a To tag.
  Response := Self.Core.CreateResponse(Invite, SIPOK);
  try
    Response.ToHeader.Tag := Self.Core.NextTag;
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteResponse(StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                       StatusCode);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteBye(LocalDialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(LocalDialog);
  try
    Self.Dispatcher.Transport.FireOnRequest(Bye);
  finally
    Bye.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteInvite;
begin
  Self.Dispatcher.Transport.FireOnRequest(Self.Invite);
end;

procedure TTestCaseTU.SimulateRemoteRinging(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPRinging);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TTestCaseTU.SimulateRemoteTryingWithNoToTag(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Invite, SIPTrying);
  try
    // strip the To header tag
    Response.ToHeader.Value := Response.ToHeader.Value;

    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TTestCaseTU Private methods ************************************************

procedure TTestCaseTU.RemoveBody(Msg: TIdSipMessage);
begin
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.Body := '';
  Msg.ToHeader.Value := Msg.ToHeader.DisplayName
                               + ' <' + Msg.ToHeader.Address.URI + '>';
  Msg.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Msg.ContentLength := 0;
end;

procedure TestTIdSipAbstractUserAgent.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.UA := TIdSipAbstractUserAgent.Create;
  Self.UA.Dispatcher := Self.Dispatcher;

  Self.Request := TIdSipTestResources.CreateBasicRequest;
  Self.Request.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Self.Request.Body := '';
  Self.Request.ToHeader.Value := Self.Request.ToHeader.DisplayName
                               + ' <' + Self.Request.ToHeader.Address.URI + '>';
  Self.Request.RemoveAllHeadersNamed(ContentTypeHeaderFull);
  Self.Request.ContentLength := 0;
end;

procedure TestTIdSipAbstractUserAgent.TearDown;
begin
  Self.Request.Free;
  Self.UA.Free;
  Self.Dispatcher.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractUserAgent Published methods ******************************

procedure TestTIdSipAbstractUserAgent.TestAddAllowedContentType;
var
  ContentTypes: TStrings;
begin
  ContentTypes := TStringList.Create;
  try
    Self.UA.AddAllowedContentType(SdpMimeType);
    Self.UA.AddAllowedContentType(PlainTextMimeType);

    ContentTypes.CommaText := Self.UA.AllowedContentTypes;

    CheckEquals(2, ContentTypes.Count, 'Number of allowed ContentTypes');

    CheckEquals(SdpMimeType,       ContentTypes[0], SdpMimeType);
    CheckEquals(PlainTextMimeType, ContentTypes[1], PlainTextMimeType);
  finally
    ContentTypes.Free;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedContentTypeMalformed;
var
  ContentTypes: String;
begin
  ContentTypes := Self.UA.AllowedContentTypes;
  Self.UA.AddAllowedContentType(' ');
  CheckEquals(ContentTypes,
              Self.UA.AllowedContentTypes,
              'Malformed Content-Type was allowed');
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedLanguage;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.UA.AddAllowedLanguage('en');
    Self.UA.AddAllowedLanguage('af');

    Languages.CommaText := Self.UA.AllowedLanguages;

    CheckEquals(2, Languages.Count, 'Number of allowed Languages');

    CheckEquals('en', Languages[0], 'en first');
    CheckEquals('af', Languages[1], 'af second');
  finally
    Languages.Free;
  end;

  try
    Self.UA.AddAllowedLanguage(' ');
    Fail('Failed to forbid adding a malformed language ID');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedLanguageLanguageAlreadyPresent;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.UA.AddAllowedLanguage('en');
    Self.UA.AddAllowedLanguage('en');

    Languages.CommaText := Self.UA.AllowedLanguages;

    CheckEquals(1, Languages.Count, 'en was re-added');
  finally
    Languages.Free;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedMethod;
var
  Methods: TStrings;
begin
  Methods := TStringList.Create;
  try
    Self.UA.AddAllowedMethod(MethodBye);
    Self.UA.AddAllowedMethod(MethodCancel);
    Self.UA.AddAllowedMethod(MethodInvite);
    Self.UA.AddAllowedMethod(MethodOptions);

    Methods.CommaText := Self.UA.AllowedMethods;

    CheckEquals(4, Methods.Count, 'Number of allowed methods');

    CheckEquals(MethodBye,     Methods[0], 'BYE first');
    CheckEquals(MethodCancel,  Methods[1], 'CANCEL second');
    CheckEquals(MethodInvite,  Methods[2], 'INVITE third');
    CheckEquals(MethodOptions, Methods[3], 'OPTIONS fourth');
  finally
    Methods.Free;
  end;

  try
    Self.UA.AddAllowedMethod(' ');
    Fail('Failed to forbid adding a non-token Method');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedMethodMethodAlreadyPresent;
var
  Methods: TStrings;
  MethodCount: Cardinal;
begin
  Methods := TStringList.Create;
  try
    Self.UA.AddAllowedMethod(MethodInvite);
    Methods.CommaText := Self.UA.AllowedMethods;
    MethodCount := Methods.Count;

    Self.UA.AddAllowedMethod(MethodInvite);
    Methods.CommaText := Self.UA.AllowedMethods;

    CheckEquals(MethodCount, Methods.Count, MethodInvite + ' was re-added');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedScheme;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.UA.AddAllowedScheme(SipScheme);
    Self.UA.AddAllowedScheme(SipsScheme);

    Schemes.CommaText := Self.UA.AllowedSchemes;

    CheckEquals(2, Schemes.Count, 'Number of allowed Schemes');

    CheckEquals(SipScheme,  Schemes[0], 'SIP first');
    CheckEquals(SipsScheme, Schemes[1], 'SIPS second');
  finally
    Schemes.Free;
  end;

  try
    Self.UA.AddAllowedScheme(' ');
    Fail('Failed to forbid adding a malformed URI scheme');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipAbstractUserAgent.TestAddAllowedSchemeSchemeAlreadyPresent;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.UA.AddAllowedScheme(SipScheme);

    Schemes.CommaText := Self.UA.AllowedSchemes;

    CheckEquals(1, Schemes.Count, 'SipScheme was re-added');
  finally
    Schemes.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentCore                                                    *
//******************************************************************************
//* TestTIdSipUserAgentCore Public methods *************************************

procedure TestTIdSipUserAgentCore.SetUp;
var
  C: TIdSipContactHeader;
  F: TIdSipFromHeader;
begin
  inherited SetUp;

  Self.Core.AddUserAgentListener(Self);

  Self.CheckOnInboundCall := nil;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo := 13;
  Self.LocalUri        := TIdSipURI.Create('sip:case@fried.neurons.org');
  Self.LocalSequenceNo := 42;
  Self.RemoteTarget    := TIdSipURI.Create('sip:sip-proxy1.tessier-ashpool.co.luna');
  Self.RemoteUri       := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.luna');

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

  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:wintermute@tessier-ashpool.co.luna';
    Self.Core.Contact := C;
  finally
    C.Free;
  end;

  F := TIdSipFromHeader.Create;
  try
    F.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
    Self.Core.From := F;
  finally
    F.Free;
  end;

  Self.OnInboundCallFired := false;
  Self.SessionEstablished := false;
end;

procedure TestTIdSipUserAgentCore.TearDown;
begin
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.RemoteUri.Free;
  Self.RemoteTarget.Free;
  Self.LocalUri.Free;
  Self.ID.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentCore Private methods ************************************

procedure TestTIdSipUserAgentCore.CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                                             Header: TIdSipHeader;
                                                             const Msg: String);
var
  Hdr:    TIdSipCommaSeparatedHeader;
  I:      Integer;
  Values: TStringList;
begin
  CheckEquals(TIdSipCommaSeparatedHeader.ClassName,
              Header.ClassName,
              Msg + ': Unexpected header type in CheckCommaSeparatedHeaders');

  Hdr := Header as TIdSipCommaSeparatedHeader;
  Values := TStringList.Create;
  try
    Values.CommaText := ExpectedValues;

    for I := 0 to Values.Count - 1 do
      CheckEquals(Values[I],
                  Hdr.Values[I],
                  Msg + ': ' + IntToStr(I + 1) + 'th value');
  finally
    Values.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.CheckCreateRequest(Dest: TIdSipToHeader;
                                                     Request: TIdSipRequest);
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
  Check(Contact.Equals(Self.Core.Contact), 'Contact header incorrectly set');

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
  Check(sttTCP = Request.LastHop.Transport,
        'TCP should be the default transport');
end;

procedure TestTIdSipUserAgentCore.OnChanged(Observed: TObject);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                             Receiver: TIdSipTransport);
begin
end;                                                             

procedure TestTIdSipUserAgentCore.OnEndedSession(Session: TIdSipSession;
                                                 const Reason: String);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgentCore.OnEstablishedSession(Session: TIdSipSession);
begin
  Self.SessionEstablished := true;
end;

procedure TestTIdSipUserAgentCore.OnInboundCall(Session: TIdSipInboundSession);
begin
  if Assigned(Self.CheckOnInboundCall) then
    Self.CheckOnInboundCall(Session);

  Self.OnInboundCallFired := true;

  Self.Session := Session;
end;

procedure TestTIdSipUserAgentCore.OnModifiedSession(Session: TIdSipSession;
                                                    Invite: TIdSipRequest);
begin
end;

procedure TestTIdSipUserAgentCore.SimulateRemoteAck(Response: TIdSipResponse);
var
  Ack:  TIdSipRequest;
  Temp: String;
begin
  // Precondition: Self.Session has been established
  Check(Assigned(Self.Session),
        'SimulateRemoteAck called but Self.Session is nil');
  Check(Assigned(Self.Session.Dialog),
        'SimulateRemoteAck called but Self.Session''s dialog isn''t '
      + 'established');

  Ack := Self.Core.CreateRequest(Self.Session.Dialog);
  try
    Ack.Method          := MethodAck;
    Ack.CSeq.SequenceNo := Self.Session.Invite.CSeq.SequenceNo;
    Ack.CSeq.Method     := Ack.Method;
    // We have to swop the tags because CreateAck returns a LOCALLY created ACK,
    // so the remote tag is actually the far end's local tag.
    Temp             := Ack.From.Tag;
    Ack.From.Tag     := Ack.ToHeader.Tag;
    Ack.ToHeader.Tag := Temp;

    Self.Dispatcher.Transport.FireOnRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.SimulateRemoteBye(Dialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(Dialog);
  try
    Self.Dispatcher.Transport.FireOnRequest(Bye);
  finally
    Bye.Free;
  end;
end;

//* TestTIdSipUserAgentCore Published methods **********************************

procedure TestTIdSipUserAgentCore.TestAddObserver;
var
  L1, L2: TIdSipTestObserver;
begin
  L1 := TIdSipTestObserver.Create;
  try
    L2 := TIdSipTestObserver.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);

      Self.SimulateRemoteInvite;

      Check(L1.Changed and L2.Changed, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestAddUserAgentListener;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);

      Self.SimulateRemoteInvite;

      Check(L1.InboundCall and L2.InboundCall,
            'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCallUsingProxy;
const
  ProxyUri = 'sip:proxy.tessier-ashpool.co.luna';
var
  Invite: TIdSipRequest;
begin
  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy := true;

  Self.Core.Call(Self.Destination, '', '');

  Invite := Self.Dispatcher.Transport.LastRequest;
  Check(Invite.HasHeader(RouteHeader),
        'No Route header added');

  Invite.Route.First;
  CheckEquals(ProxyUri,
              Invite.Route.CurrentRoute.Address.Uri,
              'Route points to wrong proxy');
end;

procedure TestTIdSipUserAgentCore.TestContentTypeDefault;
begin
  CheckEquals(SdpMimeType,
              Self.Core.AllowedContentTypes,
              'AllowedContentTypes');
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

procedure TestTIdSipUserAgentCore.TestCreateInvite;
var
  Dest:    TIdSipToHeader;
  Request: TIdSipRequest;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateInvite(Dest, '', '');
    try
      Self.CheckCreateRequest(Dest, Request);
      CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

      Check(not Request.ToHeader.HasTag,
            'This request is outside of a dialog, hence MUST NOT have a '
          + 'To tag. See RFC:3261, section 8.1.1.2');

      Check(Request.HasHeader(CSeqHeader), 'No CSeq header');
      Check(not Request.HasHeader(ContentDispositionHeader),
            'Needless Content-Disposition header');

      Check(Request.HasHeader(AllowHeader), 'No Allow header');
      CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
                                 Request.FirstHeader(AllowHeader),
                                 'Allow header');

      Check(Request.HasHeader(SupportedHeaderFull), 'No Supported header');
      CheckEquals(Self.Core.AllowedExtensions,
                  Request.FirstHeader(SupportedHeaderFull).Value,
                  'Supported header value');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateInviteWithBody;
var
  Invite: TIdSipRequest;
  Body:   String;
begin
  Body := 'foo fighters';

  Invite := Self.Core.CreateInvite(Self.Destination, Body, 'text/plain');
  try
    CheckEquals(Length(Body), Invite.ContentLength, 'Content-Length');
    CheckEquals(Body,         Invite.Body,          'Body');

    Check(Invite.HasHeader(ContentDispositionHeader),
          'Missing Content-Disposition');
    CheckEquals(DispositionSession,
                Invite.ContentDisposition.Value,
                'Content-Disposition value');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRegister;
var
  Register: TIdSipRequest;
begin
  Register := Self.Core.CreateRegister(Self.Destination);
  try
    CheckEquals(MethodRegister, Register.Method,      'Incorrect method');
    CheckEquals(MethodRegister, Register.CSeq.Method, 'Incorrect CSeq method');
    CheckEquals('', Register.RequestUri.Username, 'Request-URI Username');
    CheckEquals('', Register.RequestUri.Password, 'Request-URI Password');

    CheckEquals(Self.Core.Contact.Value,
                Register.FirstHeader(ContactHeaderFull).Value,
                'Contact');
    CheckEquals(Self.Core.Contact.Value,
                Register.ToHeader.Value,
                'To');
    CheckEquals(Register.ToHeader.Value,
                Register.From.Value,
                'From');
  finally
    Register.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRegisterReusesCallIDForSameRegistrar;
var
  FirstCallID:  String;
  Reg:          TIdSipRequest;
  SecondCallID: String;
begin
  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    FirstCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    SecondCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  CheckEquals(FirstCallID,
              SecondCallID,
              'Call-ID SHOULD be the same for same registrar');

  Self.Destination.Address.Uri := 'sip:enki.org';
  Reg := Self.Core.CreateRegister(Self.Destination);
  try
    CheckNotEquals(FirstCallID,
                   Reg.CallID,
                   'Call-ID SHOULD be different for new registrar');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateReInvite;
var
  Invite: TIdSipRequest;
begin
  Invite := Self.Core.CreateReInvite(Self.Dlg, 'foo', 'bar');
  try
    CheckEquals(MethodInvite, Invite.Method, 'Method');
    CheckEquals('foo',        Invite.Body, 'Body');
    CheckEquals('bar',        Invite.ContentType, 'Content-Type');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequest;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
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

procedure TestTIdSipUserAgentCore.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sips:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(Dest);
    try
      Contact := Request.FirstContact;
      CheckEquals(SipsScheme,
                  Contact.Address.Scheme,
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
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
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

procedure TestTIdSipUserAgentCore.TestCreateRequestWithTransport;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
    Request := Self.Core.CreateRequest(Dest);
    try
      Check(Request.LastHop.Transport = sttUDP,
            'UDP transport not specified');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseToTagMissing;
var
  Response: TIdSipResponse;
begin
  // This culls the parameters
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Value;

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    Check(Response.ToHeader.HasTag,
          'To is missing a tag');

    CheckEquals(Response.ToHeader.Address,
                Self.Invite.ToHeader.Address,
                'To header address mismatch');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseUserAgent;
var
  Response: TIdSipResponse;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';
  Self.Invite.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    CheckEquals(Self.Core.UserAgentName,
                Response.FirstHeader(ServerHeader).Value,
                'User-Agent header not set');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseUserAgentBlank;
var
  Response: TIdSipResponse;
begin
  Self.Core.UserAgentName := '';
  Self.Invite.RequestUri.URI := 'sip:wintermute@tessier-ashpool.co.luna';

  Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    Check(not Response.HasHeader(UserAgentHeader),
          'User-Agent header not removed because it''s blank');
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

procedure TestTIdSipUserAgentCore.TestDispatchToCorrectSession;
var
  SessionOne: TIdSipSession;
  SessionTwo: TIdSipInboundSession;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  SessionOne := Self.Session;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';
  Self.SimulateRemoteInvite;
  Check(Self.Session <> SessionOne, 'OnInboundCall didn''t fire');
  SessionTwo := Self.Session as TIdSipInboundSession;
  CheckEquals(2,
              Self.Core.SessionCount,
              'Number of sessions after two INVITEs');

  SessionTwo.AcceptCall('', '');

  SessionTwo.AddSessionListener(Self);
  Self.ThreadEvent.ResetEvent;
  Self.ExceptionMessage := 'SessionTwo wasn''t terminated';
  Self.SimulateRemoteBye(SessionTwo.Dialog);

  Check(not SessionOne.IsTerminated, 'SessionOne was terminated');
  CheckEquals(1,
              Self.Core.SessionCount,
              'Number of sessions after one BYE');
end;

procedure TestTIdSipUserAgentCore.TestDispatchAckToSession;
var
  SessionOne: TIdSipInboundSession;
  SessionTwo: TIdSipInboundSession;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  SessionOne := Self.Session as TIdSipInboundSession;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';
  Self.SimulateRemoteInvite;
  Check(Self.Session <> SessionOne, 'OnInboundCall didn''t fire');
  SessionTwo := Self.Session as TIdSipInboundSession;
  CheckEquals(2,
              Self.Core.SessionCount,
              'Number of sessions after two INVITEs');
  SessionOne.AcceptCall('', '');
  SessionTwo.AcceptCall('', '');
  Self.SimulateRemoteAck(Self.Dispatcher.Transport.LastResponse);
  Check(not SessionOne.ReceivedAck, 'SessionOne got the ACK');
  Check(    SessionTwo.ReceivedAck, 'SessionTwo didn''t get the ACK');
end;

procedure TestTIdSipUserAgentCore.TestFork;
var
  OrigAckCount: Cardinal;
begin
  OrigAckCount := Self.Dispatcher.Transport.ACKCount;
  Self.Core.Call(Self.Destination, '', '');
  Self.SimulateRemoteAccept(Self.Dispatcher.Transport.LastRequest);
  Self.SimulateRemoteAccept(Self.Dispatcher.Transport.LastRequest);

  CheckEquals(2,
              Self.Core.SessionCount,
              'A fork must create multiple dialogs, hence multiple sessions');
  CheckEquals(2 + OrigAckCount,
              Self.Dispatcher.Transport.AckCount,
              'UAS MUST ACK every 2xx response to an INVITE - cf RFC 3261, '
            + 'section 13.2.2.4');
end;

procedure TestTIdSipUserAgentCore.TestForkWithProvisionalResponse;
var
  OrigAckCount: Cardinal;
begin
  OrigAckCount := Self.Dispatcher.Transport.ACKCount;
  Self.Core.Call(Self.Destination, '', '');
  Self.SimulateRemoteRinging(Self.Dispatcher.Transport.LastRequest);
  Self.SimulateRemoteRinging(Self.Dispatcher.Transport.LastRequest);

  CheckEquals(1,
              Self.Core.SessionCount,
              'A provisional response doesn''t make a fork');
  CheckEquals(OrigAckCount,
              Self.Dispatcher.Transport.AckCount,
              'UAS mustn''t ACK provisional responses');
end;

procedure TestTIdSipUserAgentCore.TestHasUnknownContentEncoding;
begin
  Self.Invite.Headers.Remove(Self.Invite.FirstHeader(ContentEncodingHeaderFull));

  Check(not Self.Core.HasUnknownContentEncoding(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentEncodingHeaderFull);
  Check(Self.Core.HasUnknownContentEncoding(Self.Invite),
        'No encodings are supported');
end;

procedure TestTIdSipUserAgentCore.TestHasUnknownContentType;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(ContentTypeHeaderFull));

  Check(not Self.Core.HasUnknownContentType(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentTypeHeaderFull).Value := SdpMimeType;
  Check(not Self.Core.HasUnknownContentType(Self.Invite),
        SdpMimeType + ' MUST supported');

  Self.Invite.RemoveHeader(Self.Invite.FirstHeader(ContentTypeHeaderFull));
  Self.Invite.AddHeader(ContentTypeHeaderFull);
  Check(Self.Core.HasUnknownContentType(Self.Invite),
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
begin
  // cf. RFC 3261, section 8.2.2.2
  Self.Dispatcher.AddServerTransaction(Self.Invite, Self.Dispatcher.Transport);

  // wipe out the tag & give a different branch
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Address.URI;
  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.SimulateRemoteInvite;
  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
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
begin
  Self.SimulateRemoteInvite;

  Check(Self.OnInboundCallFired, 'UI not notified of new session');
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

    ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

    Self.Dispatcher.Transport.FireOnRequest(Bye);

    CheckEquals(ResponseCount + 1,
                Self.Dispatcher.Transport.SentResponseCount,
                'no response sent');
    Response := Self.Dispatcher.Transport.LastResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')

  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveByeForDialog;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.SimulateRemoteInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.SimulateRemoteBye(Self.Session.Dialog);

  CheckEquals(ResponseCount + 1,
              Self.Dispatcher.Transport.SentResponseCount,
              'SOMETHING should have sent a response');

  Response := Self.Dispatcher.Transport.LastResponse;
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

    ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

    Self.Dispatcher.Transport.FireOnRequest(Bye);

    CheckEquals(ResponseCount + 1,
                Self.Dispatcher.Transport.SentResponseCount,
                'no response sent');
    Response := Self.Dispatcher.Transport.LastResponse;
    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Response.StatusCode,
                'Response Status-Code')
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestReceiveResponseWithMultipleVias;
var
  Response: TIdSipResponse;
begin
  Self.Core.Call(Self.Destination, '', '');

  Response := TIdSipResponse.InResponseTo(Self.Invite,
                                          SIPOK,
                                          Self.Core.Contact);
  try
    Response.AddHeader(Response.Path.LastHop);
    Self.Dispatcher.Transport.FireOnResponse(Response);
    Check(not Self.SessionEstablished,
          'Multiple-Via Response not dropped');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRemoveObserver;
var
  L1, L2: TIdSipTestObserver;
begin
  L1 := TIdSipTestObserver.Create;
  try
    L2 := TIdSipTestObserver.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);
      Self.Core.RemoveObserver(L2);

      Self.SimulateRemoteInvite;

      Check(L1.Changed and not L2.Changed,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestRemoveRegistration;
var
  Registration:      TIdSipRegistration;
  RegistrationCount: Cardinal;
begin
  // Yes, this seems crazy. The fact that a SipRegistration
  // gets created concerns us, not where we're sending the
  // registration.
  Registration := Self.Core.RegisterWith(Self.Core.Contact.Address);

  RegistrationCount := Self.Core.RegistrationCount;
  Self.Core.RemoveRegistration(Registration);
  CheckEquals(RegistrationCount - 1,
              Self.Core.RegistrationCount,
              'Registration wasn''t removed');
end;

procedure TestTIdSipUserAgentCore.TestRemoveSession;
var
  SessionCount: Cardinal;
begin
  Self.SimulateRemoteInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  SessionCount := Self.Core.SessionCount;
  Self.Core.RemoveSession(Self.Session);
  CheckEquals(SessionCount - 1,
              Self.Core.SessionCount,
              'Session wasn''t removed');
end;

procedure TestTIdSipUserAgentCore.TestRemoveUserAgentListener;
var
  L1, L2: TIdSipTestUserAgentListener;
begin
  L1 := TIdSipTestUserAgentListener.Create;
  try
    L2 := TIdSipTestUserAgentListener.Create;
    try
      Self.Core.AddUserAgentListener(L1);
      Self.Core.AddUserAgentListener(L2);
      Self.Core.RemoveUserAgentListener(L2);

      Self.SimulateRemoteInvite;

      Check(L1.InboundCall and not L2.InboundCall,
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
  Self.Invite.RemoveHeader(Self.Invite.FirstContact);

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPBadRequest,        Response.StatusCode, 'Status-Code');
  CheckEquals(MissingContactHeader, Response.StatusText, 'Status-Text');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownContentEncoding;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Invite.FirstHeader(ContentTypeHeaderFull).Value := SdpMimeType;

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.Invite.AddHeader(ContentEncodingHeaderFull).Value := 'gzip';

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
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

  Self.Invite.AddHeader(ContentLanguageHeader).Value := 'en_GB';

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
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
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.Invite.ContentType := 'text/xml';

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
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
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.Invite.AddHeader(RequireHeader).Value := '100rel';

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPBadExtension, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(UnsupportedHeader), 'No Unsupported header');
  CheckEquals(Self.Invite.FirstHeader(RequireHeader).Value,
              Response.FirstHeader(UnsupportedHeader).Value,
              'Unexpected Unsupported header value');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnknownScheme;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.Invite.RequestUri.URI := 'tel://1';
  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPUnsupportedURIScheme, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnsupportedMethod;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  Self.Invite.Method := MethodRegister;
  Self.Invite.CSeq.Method := Self.Invite.Method;

  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;

  Self.SimulateRemoteInvite;

  Check(Self.Dispatcher.Transport.SentResponseCount > ResponseCount,
        'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  Check(Response.HasHeader(AllowHeader),
        'Allow header is mandatory. cf. RFC 3261 section 8.2.1');

  CheckCommaSeparatedHeaders(Self.Core.AllowedMethods,
                             Response.FirstHeader(AllowHeader),
                             'Allow header');
end;

procedure TestTIdSipUserAgentCore.TestRejectUnsupportedSipVersion;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.Invite.SIPVersion := 'SIP/1.0';

  Self.SimulateRemoteInvite;

  CheckEquals(ResponseCount + 1,
              Self.Dispatcher.Transport.SentResponseCount,
              'No response sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPSIPVersionNotSupported,
              Response.StatusCode,
              'Status-Code');
end;

procedure TestTIdSipUserAgentCore.TestSetContact;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:case@fried.neurons.org';
    Self.Core.Contact := C;

    Check(Self.Core.Contact.Equals(C),
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
      C.Value := 'mailto:wintermute@tessier-ashpool.co.luna';
      Self.Core.Contact := C;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EBadHeader do;
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

    Check(Self.Core.From.Equals(F),
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
      F.Value := 'mailto:wintermute@tessier-ashpool.co.luna';
      Self.Core.From := F;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EBadHeader do;
    end;
  finally
    F.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestHangUpAllCalls;
var
  FirstSession:  TIdSipInboundSession;
  SecondSession: TIdSipInboundSession;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  FirstSession := Self.Session as TIdSipInboundSession;
  FirstSession.AcceptCall('', '');
  Self.Session := nil;

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  SecondSession := Self.Session as TIdSipInboundSession;
  SecondSession.AcceptCall('', '');

  CheckEquals(2,
              Self.Core.SessionCount,
              'Session count');
  Self.Core.HangUpAllCalls;
  CheckEquals(0,
              Self.Core.SessionCount,
              'Session count after HangUpAllCalls');
end;

//******************************************************************************
//* TestTIdSipAction                                                           *
//******************************************************************************
//* TestTIdSipAction Public methods ********************************************

procedure TestTIdSipAction.SetUp;
begin
  inherited SetUp;

  Self.ActionFailed := false;
end;

//* TestTIdSipAction Protected methods *****************************************

procedure TestTIdSipAction.SimulateRemoteBadExtensionResponse;
begin
  Self.SimulateRemoteResponse(SIPBadExtension);
end;

procedure TestTIdSipAction.SimulateRemoteMovedPermanently(const SipUrl: String);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.Dispatcher.Transport.LastRequest,
                                          SIPMovedPermanently);
  try
    Response.AddHeader(ContactHeaderFull).Value := SipUrl;
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipAction Published methods *****************************************
{
procedure TestTIdSipAction.TestReceiveResponseBadExtension;
var
  Action:          TIdSipAction;
  ActionClassname: String;
  RequestCount:    Cardinal;
begin
  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;
  Self.SimulateRemoteBadExtensionResponse;

  RequestCount := Self.Dispatcher.Transport.SentRequestCount;

  Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
        ActionClassname + ' request wasn''t reissued');
  Check(not Self.Dispatcher.Transport.LastRequest.HasHeader(RequireHeader),
        'Require header still in 2nd attempt');
end;

procedure TestTIdSipAction.TestReceiveResponseBadExtensionWithoutRequires;
var
  Action:          TIdSipAction;
  ActionClassname: String;
begin

  // If we send a request that has no Requires header, but get a 420 Bad
  // Extension back (which can only come from a bad SIP implementation on the
  // remote end), then we must report a failure.

  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;

  Self.SimulateRemoteBadExtensionResponse;
  Check(Self.ActionFailed, ActionClassName + ' failure not reported');
end;
}
//******************************************************************************
//* TestTIdSipInboundSession                                                   *
//******************************************************************************
//* TestTIdSipInboundSession Public methods ************************************

procedure TestTIdSipInboundSession.SetUp;
begin
  inherited SetUp;

  Self.Core.AddUserAgentListener(Self);

  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
  Self.SentRequestTerminated  := false;

  Self.MultiStreamSdp := Self.CreateMultiStreamSdp;
  Self.SimpleSdp      := Self.CreateSimpleSdp;

  Self.Invite.ContentType := SdpMimeType;
  Self.Invite.Body        := Self.SimpleSdp.AsString;

  Self.EstablishedSession := Self.Core.Call(Self.Destination, '', '');
  Self.SimulateRemoteAccept(Self.EstablishedSession.Invite);
end;

procedure TestTIdSipInboundSession.TearDown;
begin
  Self.Core.HangUpAllCalls;
  Self.SimpleSdp.Free;
  Self.MultiStreamSdp.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundSession Protected methods *********************************

function TestTIdSipInboundSession.CreateAction: TIdSipAction;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall not called');

  Result := Self.Session;
end;

//* TestTIdSipInboundSession Private methods ***********************************

procedure TestTIdSipInboundSession.CheckServerActiveOn(Port: Cardinal);
var
  Server: TIdUDPServer;
begin
  Server := TIdUDPServer.Create(nil);
  try
    Server.DefaultPort := Port;

    try
      Server.Active := true;
      Fail('No server started on ' + IntToStr(Port));
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Server.Free;
  end;
end;

function TestTIdSipInboundSession.CreateRemoteReInvite(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.Core.CreateRequest(LocalDialog);
  try
    Result.Assign(Self.Invite);
    Result.ToHeader.Tag := LocalDialog.ID.LocalTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TestTIdSipInboundSession.CreateMultiStreamSdp: TIdSdpPayload;
var
  Connection: TIdSdpConnection;
  MD:         TIdSdpMediaDescription;
begin
  Result := TIdSdpPayload.Create;
  Result.Version                := 0;

  Result.Origin.Username        := 'wintermute';
  Result.Origin.SessionID       := '2890844526';
  Result.Origin.SessionVersion  := '2890842807';
  Result.Origin.NetType         := Id_SDP_IN;
  Result.Origin.AddressType     := Id_IPv4;
  Result.Origin.Address         := '127.0.0.1';

  Result.SessionName            := 'Minimum Session Info';

  Connection := Result.AddConnection;
  Connection.NetType     := Id_SDP_IN;
  Connection.AddressType := Id_IPv4;
  Connection.Address     := '127.0.0.1';

  MD := Result.AddMediaDescription;
  MD.MediaType := mtAudio;
  MD.Port      := 10000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('0');

  MD := Result.AddMediaDescription;
  MD.MediaType := mtText;
  MD.Port      := 11000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('98');
  MD.AddAttribute(RTPMapAttribute, '98 t140/1000');
end;

function TestTIdSipInboundSession.CreateSimpleSdp: TIdSdpPayload;
var
  Connection: TIdSdpConnection;
  MD:         TIdSdpMediaDescription;
begin
  Result := TIdSdpPayload.Create;
  Result.Version               := 0;

  Result.Origin.Username       := 'wintermute';
  Result.Origin.SessionID      := '2890844526';
  Result.Origin.SessionVersion := '2890842807';
  Result.Origin.NetType        := Id_SDP_IN;
  Result.Origin.AddressType    := Id_IPv4;
  Result.Origin.Address        := '127.0.0.1';

  Result.SessionName           := 'Minimum Session Info';

  MD := Result.AddMediaDescription;
  MD.MediaType := mtText;
  MD.Port      := 11000;
  MD.Transport := AudioVisualProfile;
  MD.AddFormat('98');
  MD.AddAttribute(RTPMapAttribute, '98 t140/1000');

  MD.Connections.Add(TIdSdpConnection.Create);
  Connection := MD.Connections[0];
  Connection.NetType     := Id_SDP_IN;
  Connection.AddressType := Id_IPv4;
  Connection.Address     := '127.0.0.1';
end;

procedure TestTIdSipInboundSession.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                       Receiver: TIdSipTransport);
begin
end;

procedure TestTIdSipInboundSession.OnEndedSession(Session: TIdSipSession;
                                           const Reason: String);
begin
  Self.Session := Session as TIdSipInboundSession;
  Self.OnEndedSessionFired := true;
  Self.ActionFailed := true;
end;

procedure TestTIdSipInboundSession.OnEstablishedSession(Session: TIdSipSession);
begin
end;

procedure TestTIdSipInboundSession.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.Session := Session;
  Self.Session.AddSessionListener(Self);
end;

procedure TestTIdSipInboundSession.OnModifiedSession(Session: TIdSipSession;
                                              Invite: TIdSipRequest);
begin
  Self.OnModifiedSessionFired := true;
end;

procedure TestTIdSipInboundSession.OnNewData(Data: TIdRTPPayload;
                                      Binding: TIdSocketHandle);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipInboundSession.OnSendRequest(Request: TIdSipRequest;
                                          Transport: TIdSipTransport);
begin
end;

procedure TestTIdSipInboundSession.OnSendResponse(Response: TIdSipResponse;
                                           Transport: TIdSipTransport);
begin
  if (Response.StatusCode = SIPRequestTerminated) then
    Self.SentRequestTerminated := true;
end;

//* TestTIdSipInboundSession Published methods ****************************************

procedure TestTIdSipInboundSession.Test2xxRetransmission;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  (Self.Session as TIdSipInboundSession).AcceptCall('', '');
  Self.Dispatcher.Transport.ResetSentResponseCount;
  Self.Session.ResendLastResponse;
  CheckEquals(1,
              Self.Dispatcher.Transport.SentResponseCount,
              'Response not resent');
  CheckEquals(SIPOK,
              Self.Dispatcher.Transport.LastResponse.StatusCode,
              'Unexpected response code');
end;

procedure TestTIdSipInboundSession.TestAcceptCall;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  Check(ResponseCount < Self.Dispatcher.Transport.SentResponseCount,
        'no responses sent');
  CheckNotNull(Session.Dialog, 'Dialog object wasn''t created');

  Response := Self.Dispatcher.Transport.LastResponse;
  CheckEquals(SIPOK, Response.StatusCode,      'Status-Code');
  Check(Response.From.HasTag,                  'No From tag');
  Check(Response.ToHeader.HasTag,              'No To tag');
  Check(Response.HasHeader(ContactHeaderFull), 'No Contact header');
//  CheckEquals('', Response.Body,               'Body should be empty');
end;

procedure TestTIdSipInboundSession.TestAcceptCallWithUnfulfillableOffer;
var
  ActualOffer: TIdSdpPayload;
  Offer:       String;
  Udp:         TIdUDPServer;
begin
  // Given an SDP payload describing the desired local port,
  // the session will try open that port but, if that port's
  // already in use, will open the lowest free port higher
  // than the desired local port.

  Udp := TIdUDPServer.Create(nil);
  try
    Udp.DefaultPort := Self.SimpleSdp.MediaDescriptionAt(0).Port;
    Udp.Active := true;

    Self.SimulateRemoteInvite;
    Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
    Offer := (Self.Session as TIdSipInboundSession).AcceptCall(Self.SimpleSdp.AsString,
                                                               SdpMimeType);
    Check(Offer <> Self.SimpleSdp.AsString,
          'Actual offer identical to suggested offer');

    Check(Offer <> '', 'AcceptCall returned the empty string');
    ActualOffer := TIdSdpPayload.CreateFrom(Offer);
    try
      Check(ActualOffer.MediaDescriptionCount > 0,
            'No media descriptions');

      Self.CheckServerActiveOn(ActualOffer.MediaDescriptionAt(0).Port);
    finally
      ActualOffer.Free;
    end;
  finally
    Udp.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestAcceptCallRespectsContentType;
var
  Response:      TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.Dispatcher.Transport.SentResponseCount;
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  (Self.Session as TIdSipInboundSession).AcceptCall(Self.SimpleSdp.AsString,
                                                    SdpMimeType);

  Check(ResponseCount < Self.Dispatcher.Transport.SentResponseCount,
        'no responses sent');

  Response := Self.Dispatcher.Transport.LastResponse;
  Check(Response.HasHeader(ContentTypeHeaderFull),
        'No Content-Type header');
  CheckEquals(SdpMimeType,
              Response.FirstHeader(ContentTypeHeaderFull).Value,
              'Content-Type');
end;

procedure TestTIdSipInboundSession.TestAcceptCallStartsListeningMedia;
var
  Udp: TIdUDPServer;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  (Self.Session as TIdSipInboundSession).AcceptCall(Self.SimpleSdp.AsString,
                                                    SdpMimeType);

  Udp := TIdUDPServer.Create(nil);
  try
    Udp.DefaultPort := Self.SimpleSdp.MediaDescriptionAt(0).Port;
    try
      Udp.Active := true;
      Fail('No listening ports');
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    Udp.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestAddSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Session.AddSessionListener(L1);
      Self.Session.AddSessionListener(L2);

      Self.SimulateRemoteBye(Self.Session.Dialog);

      Check(L1.EndedSession and L2.EndedSession, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestReceive2xxSendsAck;
var
  Ack:    TIdSipRequest;
  Invite: TIdSipRequest;
begin
  // Remember, we established a dialog for EstablishedSession in the SetUp
  CheckEquals(1,
              Self.Dispatcher.Transport.ACKCount,
              'Retransmission');
  Self.Dispatcher.Transport.FireOnResponse(Self.Dispatcher.Transport.LastResponse);
  CheckEquals(2,
              Self.Dispatcher.Transport.ACKCount,
              'Retransmission');

  Ack := Self.Dispatcher.Transport.LastRequest;
  CheckEquals(MethodAck, Ack.Method, 'Unexpected method');
  Invite := Self.EstablishedSession.Invite;
  CheckEquals(Invite.CSeq.SequenceNo,
              Ack.CSeq.SequenceNo,
              'CSeq numerical portion');
  CheckEquals(MethodAck,
              Ack.CSeq.Method,
              'ACK must have an INVITE method in the CSeq');
end;

procedure TestTIdSipInboundSession.TestReceiveBye;
begin
  Self.SimulateRemoteInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');
  Self.Session.AddSessionListener(Self);

  Self.SimulateRemoteBye(Self.Session.Dialog);

  Check(Self.OnEndedSessionFired, 'OnEndedSession didn''t fire');
end;
{
procedure TestTIdSipInboundSession.TestReceiveByeWithPendingRequests;
var
  ReInvite: TIdSipRequest;
  Session:  TIdSipSession;
begin
  Fail('Can''t be done until Session can modify a session');
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall wasn''t fired');
  Self.Session.AcceptCall;

  // This must be a CLIENT transaction!
  ReInvite := Self.CreateRemoteReInvite(Self.Session.Dialog);
  try
    Self.Dispatcher.Transport.FireOnRequest(ReInvite);
    Self.SimulateRemoteBye(Self.Session.Dialog);

    Check(Self.SentRequestTerminated,
          'Pending request wasn''t responded to with a 481 Request Terminated');
  finally
    ReInvite.Free;
  end;
end;
}

procedure TestTIdSipInboundSession.TestReceiveOutOfOrderRequest;
var
  Invite:   TIdSipRequest;
  Response: TIdSipResponse;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall wasn''t fired');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  Invite := Self.CreateRemoteReInvite(Self.Session.Dialog);
  try
    Invite.CSeq.SequenceNo := Invite.CSeq.SequenceNo - 1;
    Self.Dispatcher.Transport.ResetSentResponseCount;
    Self.Dispatcher.Transport.FireOnRequest(Invite);
    CheckEquals(1,
                Self.Dispatcher.Transport.SentResponseCount,
                'No response sent');
    Response := Self.Dispatcher.Transport.LastResponse;
    CheckEquals(SIPInternalServerError,
                Response.StatusCode,
                'Unexpected response');
    CheckEquals(RSSIPRequestOutOfOrder,
                Response.StatusText,
                'Unexpected response, status text');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestReceiveReInvite;
var
  ReInvite: TIdSipRequest;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall wasn''t fired');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  ReInvite := Self.CreateRemoteReInvite(Self.Session.Dialog);
  try
    Self.Dispatcher.Transport.FireOnRequest(ReInvite);

    Check(Self.OnModifiedSessionFired, 'OnModifiedSession didn''t fire');
  finally
    ReInvite.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestRemoveSessionListener;
var
  L1, L2: TIdSipTestSessionListener;
begin
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  (Self.Session as TIdSipInboundSession).AcceptCall('', '');

  L1 := TIdSipTestSessionListener.Create;
  try
    L2 := TIdSipTestSessionListener.Create;
    try
      Self.Session.AddSessionListener(L1);
      Self.Session.AddSessionListener(L2);
      Self.Session.RemoveSessionListener(L2);

      Self.SimulateRemoteBye(Self.Session.Dialog);

      Check(L1.EndedSession and not L2.EndedSession,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipInboundSession.TestTerminate;
var
  Request:      TIdSipRequest;
  RequestCount: Cardinal;
begin
  RequestCount := Self.Dispatcher.Transport.SentRequestCount;

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  (Self.Session as TIdSipInboundSession).AcceptCall('', '');
  Self.Session.Terminate;

  CheckEquals(RequestCount + 1,
              Self.Dispatcher.Transport.SentRequestCount,
              'no BYE sent');

  Request := Self.Dispatcher.Transport.LastRequest;
  Check(Request.IsBye, 'Unexpected last request');

  Check(Self.Session.IsTerminated, 'Session not marked as terminated');
end;

//******************************************************************************
//* TestTIdSipOutboundSession                                                  *
//******************************************************************************
//* TestTIdSipOutboundSession Public methods ***********************************

procedure TestTIdSipOutboundSession.SetUp;
begin
  inherited SetUp;

  Self.Session := Self.Core.Call(Self.Destination, '', '');
  Self.Session.AddSessionListener(Self);

  Self.OnEndedSessionFired    := false;
  Self.OnModifiedSessionFired := false;
end;

//* TestTIdSipOutboundSession Protectedivate methods ***************************

function TestTIdSipOutboundSession.CreateAction: TIdSipAction;
begin
  Result := Self.Core.Call(Self.Destination, '', '');
  (Result as TIdSipSession).AddSessionListener(Self);
end;

//* TestTIdSipOutboundSession Private methods **********************************

procedure TestTIdSipOutboundSession.OnEndedSession(Session: TIdSipSession;
                                                   const Reason: String);
begin
  Self.OnEndedSessionFired := true;
end;

procedure TestTIdSipOutboundSession.OnEstablishedSession(Session: TIdSipSession);
begin
end;

procedure TestTIdSipOutboundSession.OnModifiedSession(Session: TIdSipSession;
                                                      Invite: TIdSipRequest);
begin
  Self.OnModifiedSessionFired := true;
end;

//* TestTIdSipOutboundSession Published methods ********************************

procedure TestTIdSipOutboundSession.TestCall;
var
  Invite:       TIdSipRequest;
  RequestCount: Cardinal;
  Response:     TIdSipResponse;
  SessCount:    Integer;
  Session:      TIdSipSession;
  TranCount:    Integer;
begin
  RequestCount := Self.Dispatcher.Transport.SentRequestCount;
  SessCount    := Self.Core.SessionCount;
  TranCount    := Self.Dispatcher.TransactionCount;

  Session := Self.Core.Call(Self.Destination, '', '');

  CheckEquals(RequestCount + 1,
              Self.Dispatcher.Transport.SentRequestCount,
              'no INVITE sent');
  Invite := Self.Dispatcher.Transport.LastRequest;

  CheckEquals(TranCount + 1,
              Self.Dispatcher.TransactionCount,
              'no client INVITE transaction created');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'no new session created');

  Response := Self.Core.CreateResponse(Invite, SIPRinging);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);

    Check(Session.Dialog.IsEarly,
          'Dialog in incorrect state: should be Early');
    Check(not Session.Dialog.IsSecure,
          'Dialog secure when TLS not used');

    CheckEquals(Response.CallID,
                Session.Dialog.ID.CallID,
                'Dialog''s Call-ID');
    CheckEquals(Response.From.Tag,
                Session.Dialog.ID.LocalTag,
                'Dialog''s Local Tag');
    CheckEquals(Response.ToHeader.Tag,
                Session.Dialog.ID.RemoteTag,
                'Dialog''s Remote Tag');
  finally
    Response.Free;
  end;

  Response := Self.Core.CreateResponse(Invite, SIPOK);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);

    Check(not Session.Dialog.IsEarly, 'Dialog in incorrect state: shouldn''t be early');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallTwice;
var
  SessCount: Integer;
  Session:   TIdSipOutboundSession;
  TranCount: Integer;
begin
  SessCount := Self.Core.SessionCount;
  TranCount := Self.Dispatcher.TransactionCount;

  Session := Self.Core.Call(Self.Destination, '', '');
  Session.Call(Self.Destination, '', '');

  CheckEquals(SessCount + 1,
              Self.Core.SessionCount,
              'The TU layer created a second session');
  CheckEquals(TranCount + 1,
              Self.Dispatcher.TransactionCount,
              'Session created a second transaction');
end;

procedure TestTIdSipOutboundSession.TestCallRemoteRefusal;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Session := Self.Core.Call(Self.Destination, '', '');

  Response := Self.Core.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                       SIPForbidden);
  try
    Session.AddSessionListener(Self);
    Self.Dispatcher.Transport.FireOnResponse(Response);

    Check(Self.OnEndedSessionFired, 'OnEndedSession wasn''t triggered');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallNetworkFailure;
var
  Response: TIdSipResponse;
  Session:  TIdSipOutboundSession;
begin
  Session := TIdSipOutboundSession.Create(Self.Core);
  try
    Session.AddSessionListener(Self);
    Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;
    Session.Call(Self.Destination, '', '');

    Response := Self.Core.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                         SIPForbidden);
    try
      Self.Dispatcher.Transport.FireOnResponse(Response);

      Check(Self.OnEndedSessionFired, 'OnEndedSession wasn''t triggered');
    finally
      Response.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallSecure;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.Transport.TransportType := sttTLS;

  Self.Destination.Address.Scheme := SipsScheme;
  Session := Self.Core.Call(Self.Destination, '', '');

  Response := Self.Core.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                       SIPRinging);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);

    Response.StatusCode := SIPOK;
    Check(Session.Dialog.IsSecure, 'Dialog not secure when TLS used');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestCallSipsUriOverTcp;
var
  RequestCount: Cardinal;
  SentInvite:   TIdSipRequest;
  Session:      TIdSipSession;
begin
  RequestCount := Self.Dispatcher.Transport.SentRequestCount;
  Self.Dispatcher.Transport.TransportType := sttTCP;
  Self.Destination.Address.Scheme := SipsScheme;

  Session := Self.Core.Call(Self.Destination, '', '');

  Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
        'INVITE wasn''t sent');
  SentInvite := Self.Dispatcher.Transport.LastRequest;

  Self.SimulateRemoteRinging(SentInvite);

  Check(not Session.Dialog.IsSecure, 'Dialog secure when TCP used');
end;

procedure TestTIdSipOutboundSession.TestCallSipUriOverTls;
var
  Response: TIdSipResponse;
  Session:  TIdSipSession;
begin
  Self.Dispatcher.Transport.TransportType := sttTCP;

  Session := Self.Core.Call(Self.Destination, '', '');

  Response := Self.Core.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                       SipRinging);
  try
    Response.FirstContact.Address.Scheme := SipsScheme;
    Response.StatusCode := SIPOK;
    Self.Dispatcher.Transport.FireOnResponse(Response);

    Check(not Session.Dialog.IsSecure, 'Dialog secure when TLS used with a SIP URI');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestFork;
var
  Fork: TIdSipOutboundSession;
  Ok:   TIdSipResponse;
begin
  Self.SimulateRemoteAccept(Self.Session.Invite);

  Ok := Self.Core.CreateResponse(Self.Invite, SIPOK);
  try
    CheckNotEquals(Ok.ToHeader.Tag,
                   Self.Session.Dialog.ID.RemoteTag,
                   'Sanity check on the To tag');

    Fork := Self.Session.Fork(Ok);
    try
      CheckEquals(Self.Session.Invite.CallID,
                  Fork.Invite.CallID,
                  'Call-ID');
      CheckEquals(Self.Session.Invite.From.Tag,
                  Fork.Invite.From.Tag,
                  'From tag');
      CheckEquals(Self.Session.Invite.LastHop.Branch,
                  Fork.Invite.LastHop.Branch,
                  'Topmost Via branch');            
    finally
      Fork.Free;
    end;
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipOutboundSession.TestDialogNotEstablishedOnTryingResponse;
var
  RequestCount: Cardinal;
  SentInvite:   TIdSipRequest;
  Session:      TIdSipSession;
begin
  RequestCount := Self.Dispatcher.Transport.SentRequestCount;

  Session := Self.Core.Call(Self.Destination, '', '');
  Check(not Session.DialogEstablished, 'Brand new session');

  Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
        'The INVITE wasn''t sent');
  SentInvite := Self.Dispatcher.Transport.LastRequest;

  Self.SimulateRemoteTryingWithNoToTag(SentInvite);
  Check(not Session.DialogEstablished,
        'Dialog established after receiving a 100 Trying');

  Self.SimulateRemoteRinging(SentInvite);
  Check(Session.DialogEstablished,
        'Dialog not established after receiving a 180 Ringing');
end;

procedure TestTIdSipOutboundSession.TestReceiveFinalResponseSendsAck;
var
  I:                Integer;
  OriginalAckCount: Cardinal;
begin
  // Of course this works. That's because the transaction sends the ACK for a
  // non-2xx final response.
  for I := 3 to 6 do begin
    OriginalAckCount := Self.Dispatcher.Transport.ACKCount;

    Self.Core.Call(Self.Destination, '', '');

    Self.SimulateRemoteResponse(I*100);
    Check(OriginalAckCount < Self.Dispatcher.Transport.ACKCount,
          'Session didn''t send an ACK to a final response, '
        + Self.Dispatcher.Transport.LastResponse.Description);
  end;
end;

procedure TestTIdSipOutboundSession.TestTerminateUnestablishedSession;
var
  Request:      TIdSipRequest;
  RequestCount: Cardinal;
  Session:      TIdSipSession;
begin
  Session := Self.Core.Call(Self.Destination, '', '');
  RequestCount := Self.Dispatcher.Transport.SentRequestCount;

  Session.Terminate;

  CheckEquals(RequestCount + 1,
              Self.Dispatcher.Transport.SentRequestCount,
              'no CANCEL sent');

  Request := Self.Dispatcher.Transport.LastRequest;
  Check(Request.IsCancel, 'Unexpected last request');

  Check(Session.IsTerminated, 'Session not marked as terminated');
end;

//******************************************************************************
//* TestBugHunt                                                                *
//******************************************************************************
//* TestBugHunt Public methods *************************************************

procedure TestBugHunt.SetUp;
begin
  inherited SetUp;

  Self.Destination := TIdSipToHeader.Create;
  Self.Destination.Address.Uri := 'sip:hiro@enki.org;transport=udp';

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatcher;
  Self.UA.Contact.Address.Uri := 'sip:vitaly@chernobyl.org';

  Self.ToTag := 'faketag';
end;

procedure TestBugHunt.TearDown;
begin
  Self.UA.Free;
  Self.Dispatcher.Free;
  Self.Destination.Free;

  inherited TearDown;
end;

//* TestBugHunt Privage methods ************************************************

procedure TestBugHunt.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                 Receiver: TIdSipTransport);
begin
end;

procedure TestBugHunt.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.Session := Session;
end;

procedure TestBugHunt.SimulateRemoteInvite;
var
  OurTo:   TIdSipToHeader;
  Request: TIdSipRequest;
begin
  OurTo := Self.UA.Contact.AsToHeader;
  try
    Request := Self.UA.CreateInvite(OurTo, '', '');
    try
      Self.Dispatcher.Transport.FireOnRequest(Request);
    finally
      Request.Free;
    end;
  finally
    OurTo.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteOK;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.Dispatcher.Transport.LastRequest, SIPOK);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteRinging;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.Dispatcher.Transport.LastRequest, SIPRinging);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestBugHunt.SimulateRemoteTrying;
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Self.Dispatcher.Transport.LastRequest, SIPTrying);
  try
    Response.ToHeader.Tag := Self.ToTag;
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestBugHunt.SimulateUnexpectedAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Dispatcher.Transport.LastRequest.AckFor(Self.Dispatcher.Transport.LastResponse);
  try
    Ack.LastHop.Branch := Ack.LastHop.Branch + '1';
    Self.Dispatcher.Transport.FireOnRequest(Ack);
  finally
    Ack.Free;
  end;
end;

//* TestBugHunt Published methods **********************************************

procedure TestBugHunt.TestOutboundCallAndByeToXlite;
var
  Session: TIdSipSession;
begin
  Session := Self.UA.Call(Self.Destination, '', '');

  Self.SimulateRemoteTrying;
  Check(not Session.DialogEstablished,
        Self.Dispatcher.Transport.LastResponse.Description
      + 's don''t make dialogs');

  Self.SimulateRemoteRinging;
  Check(Session.DialogEstablished,
        Self.Dispatcher.Transport.LastResponse.Description
      + 's with To tags make dialogs');
  Check(Session.Dialog.IsEarly,
        Self.Dispatcher.Transport.LastResponse.Description
      + 's make early dialogs');

  Self.SimulateRemoteOK;
  Check(not Session.Dialog.IsEarly,
        Self.Dispatcher.Transport.LastResponse.Description
      + 's make non-early dialogs');

  Self.SimulateRemoteOK;
  Self.SimulateRemoteOK;
  Self.SimulateRemoteOK;

  Self.UA.HangUpAllCalls;
  Check(Self.Dispatcher.Transport.LastRequest.IsBye,
        'Must send a BYE to terminate an established session');
end;

procedure TestBugHunt.TestSimultaneousInAndOutboundCall;
begin
  Self.UA.AddUserAgentListener(Self);
  Self.UA.Call(Self.Destination, '', '');
  Self.SimulateRemoteTrying;
  Self.SimulateRemoteRinging;

  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');

  Self.Session.AcceptCall('', '');
  CheckEquals(2, Self.UA.SessionCount, 'Session count');
end;

procedure TestBugHunt.TestXlitesAckBug;
begin
  Self.UA.AddUserAgentListener(Self);
  Self.SimulateRemoteInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.Session.AcceptCall('', '');

  Self.SimulateUnexpectedAck;
  CheckEquals(0,
              Self.Dispatcher.TransactionCount,
              'A transaction got made in response to an ACK');
  CheckEquals(1,
              Self.UA.SessionCount,
              'ACK wasn''t simply dropped by the TU');
end;

//******************************************************************************
//* TestTIdSipSessionTimer                                                     *
//******************************************************************************
//* TestTIdSipSessionTimer Public methods **************************************

procedure TestTIdSipSessionTimer.SetUp;
begin
  inherited SetUp;

  Self.UA      := TIdSipUserAgentCore.Create;
  Self.Session := TIdSipMockSession.Create(UA);
  Self.Timer   := TIdSipSessionTimer.Create(Self.Session, DefaultT1, DefaultT2);
end;

procedure TestTIdSipSessionTimer.TearDown;
begin
  Self.Timer.Free;
  Self.Session.Free;
  Self.UA.Free;

  inherited TearDown;
end;

//* TestTIdSipSessionTimer Published methods ***********************************

procedure TestTIdSipSessionTimer.TestFireResendsSessionsLastResponse;
begin
  Check(not Self.Session.ResponseResent, 'Sanity check');
  Self.Timer.Fire;
  Check(Self.Session.ResponseResent, 'Fire didn''t affect the Session');
end;

procedure TestTIdSipSessionTimer.TestTimerIntervalIncreases;
begin
  CheckEquals(DefaultT1,   Self.Timer.Interval, 'Initial interval');
  Self.Timer.Fire;
  CheckEquals(2*DefaultT1, Self.Timer.Interval, 'Fire once');
  Self.Timer.Fire;
  CheckEquals(4*DefaultT1, Self.Timer.Interval, 'Fire twice');
  Self.Timer.Fire;
  CheckEquals(DefaultT2,   Self.Timer.Interval, 'Fire thrice');
  Self.Timer.Fire;
  CheckEquals(DefaultT2,   Self.Timer.Interval, 'Fire four times');
end;

//******************************************************************************
//*  TestTIdSipRegistration                                                    *
//******************************************************************************
//*  TestTIdSipRegistration Public methods *************************************

procedure TestTIdSipRegistration.SetUp;
const
  TwoHours = 7200;
begin
  inherited SetUp;

  Self.Registrar := TIdSipRegistrar.Create;
  Self.Registrar.From.Address.Uri := 'sip:talking-head.tessier-ashpool.co.luna';

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP talking-head.tessier-ashpool.co.luna;branch='
                                               + BranchMagicCookie + 'f00L';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CSeq.SequenceNo := 1024;
  Self.Request.CallID := '1@selftest.foo';

  Self.Contacts := TIdSipContacts.Create(Self.Request.Headers);

  Self.Reg := TIdSipRegistration.Create(Self.Core);
  Self.Reg.AddListener(Self);

  Self.Challenged := false;
  Self.Succeeded  := false;
  Self.MinExpires := TwoHours;
end;

procedure TestTIdSipRegistration.TearDown;
begin
  Self.Reg.Free;
  Self.Contacts.Free;
  Self.Request.Free;
  Self.Registrar.Free;

  inherited TearDown;
end;

//*  TestTIdSipRegistration Protected methods **********************************

function TestTIdSipRegistration.CreateAction: TIdSipAction;
begin
  Result := Self.Core.RegisterWith(Self.Registrar.From.Address);
  (Result as TIdSipRegistration).AddListener(Self);
end;

//*  TestTIdSipRegistration Private methods ************************************

procedure TestTIdSipRegistration.OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                                           Response: TIdSipResponse);
begin
  Self.Challenged := true;
end;

procedure TestTIdSipRegistration.OnFailure(RegisterAgent: TIdSipRegistration;
                                           CurrentBindings: TIdSipContacts;
                                           const Reason: String);
begin
  Self.ActionFailed := true;
end;

procedure TestTIdSipRegistration.OnSuccess(RegisterAgent: TIdSipRegistration;
                                           CurrentBindings: TIdSipContacts);
begin
  Self.Succeeded := true;
end;

procedure TestTIdSipRegistration.SimulateRemoteIntervalTooBrief;
var
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                            SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.MinExpires);

    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipRegistration.SimulateRemoteOK;
begin
  Self.SimulateRemoteResponse(SIPOK);
end;

procedure TestTIdSipRegistration.SimulateRemoteResponse(StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.Dispatcher.Transport.LastRequest,
                                            StatusCode);
  try
    Self.Dispatcher.Transport.FireOnResponse(Response);
  finally
    Response.Free;
  end;
end;

//*  TestTIdSipRegistration Published methods **********************************

procedure TestTIdSipRegistration.TestAddListener;
var
  L1, L2: TIdSipTestRegistrationListener;
begin
  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Self.Reg.AddListener(L1);
      Self.Reg.AddListener(L2);

      Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
      Self.SimulateRemoteOK;

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipRegistration.TestRegister;
var
  Request: TIdSipRequest;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Check(Self.Dispatcher.Transport.SentRequestCount > 0,
        'No request sent');

  Request := Self.Dispatcher.Transport.LastRequest;
  CheckEquals(Self.Registrar.From.Address.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  Check(Request.Contacts.Equals(Self.Contacts),
        'Bindings');
end;

procedure TestTIdSipRegistration.TestFindCurrentBindings;
var
  Request: TIdSipRequest;
begin
  Self.Reg.FindCurrentBindings(Self.Registrar.From.Address);
  Check(Self.Dispatcher.Transport.SentRequestCount > 0,
        'No request sent');

  Request := Self.Dispatcher.Transport.LastRequest;
  Check(Request.Contacts.IsEmpty,
        'Contact headers present');
end;

procedure TestTIdSipRegistration.TestReceiveFail;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Self.SimulateRemoteResponse(SIPInternalServerError);
  Check(Self.ActionFailed, 'Registration succeeded');
end;

procedure TestTIdSipRegistration.TestReceiveIntervalTooBrief;
const
  OneHour = 3600;
var
  RequestSendCount: Cardinal;
begin
  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);

  RequestSendCount := Self.Dispatcher.Transport.SentRequestCount;
  Self.SimulateRemoteIntervalTooBrief;

  Check(Self.Dispatcher.Transport.SentRequestCount > RequestSendCount,
        'No re-request issued');
  Check(Self.Dispatcher.Transport.LastRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.Dispatcher.Transport.LastRequest.QuickestExpiry,
              'Re-request minimum expires');

  Self.SimulateRemoteOK;
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipRegistration.TestReceiveIntervalTooBriefForOneContact;
const
  OneHour = 3600;
var
  RequestSendCount:     Cardinal;
  RequestContacts:      TIdSipContacts;
  SecondContactExpires: Cardinal;
begin
  // We try to be tricky: One contact has a (too-brief) expires of one hour.
  // The other has an expires of three hours. The registrar accepts a minimum
  // expires of two hours. We expect the registrar to reject the request with
  // a 423 Interval Too Brief, and for the SipRegistration to re-issue the
  // request leaving the acceptable contact alone and only modifying the
  // too-short contact.

  SecondContactExpires := OneHour*3;

  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna;expires=' + IntToStr(SecondContactExpires);
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);

  RequestSendCount := Self.Dispatcher.Transport.SentRequestCount;
  Self.SimulateRemoteIntervalTooBrief;

  Check(Self.Dispatcher.Transport.SentRequestCount > RequestSendCount,
        'No re-request issued');
  Check(Self.Dispatcher.Transport.LastRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.Dispatcher.Transport.LastRequest.QuickestExpiry,
              'Re-request minimum expires');
  RequestContacts := TIdSipContacts.Create(Self.Dispatcher.Transport.LastRequest.Headers);
  try
    RequestContacts.First;
    Check(RequestContacts.HasNext, 'No Contacts');
    Check(RequestContacts.CurrentContact.WillExpire, 'First contact missing expires');
    CheckEquals(Self.MinExpires,
                RequestContacts.CurrentContact.Expires,
                'First (too brief) contact');
    RequestContacts.Next;
    Check(RequestContacts.HasNext, 'Too few Contacts');
    Check(RequestContacts.CurrentContact.WillExpire, 'Second contact missing expires');
    CheckEquals(SecondContactExpires,
                RequestContacts.CurrentContact.Expires,
                'Second, acceptable, contact');
  finally
    RequestContacts.Free;
  end;

  Self.SimulateRemoteOK;
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipRegistration.TestReceiveMovedPermanently;
var
  Action:          TIdSipAction;
  ActionClassname: String;
  RequestCount:    Cardinal;
begin
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;

  RequestCount := Self.Dispatcher.Transport.SentRequestCount;
  Self.SimulateRemoteMovedPermanently('sip:case@fried.neurons.org');
  Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
        ActionClassname + ' no request re-issued');
end;

procedure TestTIdSipRegistration.TestReceiveOK;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Self.SimulateRemoteOK;
  Check(Self.Succeeded, 'Registration failed');
end;

procedure TestTIdSipRegistration.TestReceiveUnauthorized;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Self.SimulateRemoteResponse(SIPUnauthorized);
  Check(Self.Challenged,
        'Authentication challenge');
end;

procedure TestTIdSipRegistration.TestSequenceNumberIncrements;
var
  SeqNo: Cardinal;
begin
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  SeqNo := Self.Dispatcher.Transport.LastRequest.CSeq.SequenceNo;
  Self.Reg.RegisterWith(Self.Registrar.From.Address, Self.Contacts);
  Check(SeqNo + 1 = Self.Dispatcher.Transport.LastRequest.CSeq.SequenceNo,
        'CSeq sequence number didn''t increment');
end;

procedure TestTIdSipRegistration.TestUnregister;
var
  Request: TIdSipRequest;
begin
  Self.Reg.Unregister(Self.Registrar.From.Address);
  Check(Self.Dispatcher.Transport.SentRequestCount > 0,
        'No request sent');

  Request := Self.Dispatcher.Transport.LastRequest;
  CheckEquals(Self.Registrar.From.Address.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  CheckEquals(1, Request.Contacts.Count,
             'Contact count');
  Check(Request.FirstContact.IsWildCard,
        'First Contact');
  CheckEquals(0, Request.QuickestExpiry,
             'Request expiry');
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
