{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipUserAgent;

interface

uses
  Classes, IdConnectionBindings, IdObservable, IdRoutingTable, IdSipCore,
  IdSipDialog, IdSipDialogID, IdSipDns, IdSipInviteModule, IdSipLocation,
  IdSipMessage, IdSipRegistration, IdSipTransport, IdSipUserAgent,
  IdSocketHandle, IdUdpServer, IdTimerQueue, SyncObjs, TestFrameworkEx,
  TestFramework, TestFrameworkSip, TestFrameworkSipTU;

type
  TestTIdSipUserAgent = class(TTestCaseTU,
                              IIdObserver,
                              IIdSipActionListener,
                              IIdSipInviteModuleListener,
                              IIdSipMessageModuleListener,
                              IIdSipTransportSendingListener,
                              IIdSipSessionListener,
                              IIdSipTransactionUserListener)
  private
    Dlg:                 TIdSipDialog;
    FailReason:          String;
    ID:                  TIdSipDialogID;
    InboundCallMimeType: String;
    InboundCallOffer:    String;
    LocalSequenceNo:     Cardinal;
    LocalUri:            TIdSipURI;
    OnChangedEvent:      TEvent;
    OnEndedSessionFired: Boolean;
    OnInboundCallFired:  Boolean;
    Password:            String;
    RemoteTarget:        TIdSipURI;
    RemoteUri:           TIdSipURI;
    RouteSet:            TIdSipHeaders;
    SendEvent:           TEvent;
    Session:             TIdSipInboundSession;
    SessionEstablished:  Boolean;
    TryAgain:            Boolean;
    UserAgentParam:      TIdSipAbstractCore;

    procedure CheckCreateRequest(Dest: TIdSipToHeader;
                                 Request: TIdSipRequest);
    procedure CheckGruuSet(ExpectedGruu: String; Msg: String);
    procedure CheckIsUnregister(Request: TIdSipRequest);
    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); overload;
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnProgressedSession(Session: TIdSipSession;
                                  Progress: TIdSipResponse);
    procedure OnReferral(Session: TIdSipSession;
                         Refer: TIdSipRequest;
                         Binding: TIdConnectionBindings);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure OnTerminated(Action: TIdSipAction);
    procedure ReceiveBye(Dialog: TIdSipDialog);
  protected
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Binding: TIdConnectionBindings); override;
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Binding: TIdConnectionBindings); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcksDontMakeTransactions;
    procedure TestAcceptCallSchedulesResendOk;
    procedure TestActionsNotifyUAObservers;
    procedure TestAddListener;
    procedure TestAddRoute;
//    procedure TestByeWithAuthentication;
    procedure TestCallUsingRoutePath;
    procedure TestCancelNotifiesTU;
    procedure TestConcurrentCalls;
    procedure TestContentTypeDefault;
    procedure TestCreateRequest;
    procedure TestCreateRequestInDialogWithRoutes;
    procedure TestCreateRequestSipsRequestUri;
    procedure TestCreateRequestUserAgent;
    procedure TestCreateRequestWithTransport;
    procedure TestCreateResponseToTagMissing;
    procedure TestCreateResponseUserAgent;
    procedure TestCreateResponseUserAgentBlank;
    procedure TestDeclinedCallNotifiesListeners;
    procedure TestDestroyUnregisters;
    procedure TestDialogLocalSequenceNoMonotonicallyIncreases;
    procedure TestDispatchToCorrectSession;
    procedure TestDontReAuthenticate;
    procedure TestInboundCall;
    procedure TestInviteRaceCondition;
    procedure TestMergedRequest;
    procedure TestNewUAHasSensibleFrom;
    procedure TestNotificationOfNewSession;
    procedure TestNotificationOfNewSessionRobust;
    procedure TestOutboundCallAndByeToXlite;
    procedure TestOutboundInviteSessionProgressResends;
    procedure TestOutboundInviteDoesNotTerminateWhenNoResponse;
    procedure TestReceiveByeForDialog;
    procedure TestReceiveByeDestroysTerminatedSession;
    procedure TestReceiveResponseWithMultipleVias;
    procedure TestRegisterWith;
    procedure TestRegisterWithGruu;
    procedure TestRegisterWithReceiveMultipleGruus;
    procedure TestRegisterWithRegistrarNameDoesntResolve;
    procedure TestRejectMalformedAuthorizedRequest;
    procedure TestRejectMethodNotAllowed;
    procedure TestRejectNoContact;
    procedure TestRejectUnauthorizedRequest;
    procedure TestRemoveUserAgentListener;
    procedure TestRFC2543InviteCallFlow;
    procedure TestScheduleEventActionClosure;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
    procedure TestSimultaneousInAndOutboundCall;
    procedure TestTerminateAllCalls;
//    procedure TestUnknownAcceptValue;
    procedure TestUnmatchedAckGetsDropped;
    procedure TestUnregisterFromWhenNotRegistered;
    procedure TestUnregisterFromWhenRegistered;
    procedure TestViaMatchesTransportParameter;
  end;

  TStackConfigurationTestCase = class(TThreadingTestCase)
  protected
    Address:       String;
    Conf:          TIdSipStackConfigurator;
    Configuration: TStrings;
    Port:          Cardinal;
    Timer:         TIdDebugTimerQueue;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TStackConfigurationTestCaseWithMockTransport = class(TStackConfigurationTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipStackConfigurator = class(TStackConfigurationTestCase)
  private
    NewRegistrar:               TIdUdpServer;
    NewRegistrarReceivedPacket: Boolean;
    NewRegistrarEvent:          TEvent;
    Port:                       Cardinal;
    ReceivedRequest:            TIdSipRequest;
    ReceivedPacket:             Boolean;
    Server:                     TIdUdpServer;

    function  ARecords: String;
    procedure CheckAutoAddress(Address: TIdSipAddressHeader);
    procedure CheckAutoFrom(UserAgent: TIdSipUserAgent);
    procedure CheckEventPackageRegistered(UA: TIdSipUserAgent;
                                          PackageName: String);
    procedure CheckLocalAddress(UA: TIdSipUserAgent; ExpectedLocalAddress, DestinationIP: String; Msg: String);
    procedure CheckTCPServerNotOnPort(const Host: String;
                                      Port: Cardinal;
                                      const Msg: String);
    procedure CheckUserAgentUsesGruu(Configuration: TStrings; Value: String; UsesGruu: Boolean);
    function  CreateUserAgentWithUsesGruuDirective(Configuration: TStrings; Value: String): TIdSipUserAgent;
    procedure CheckConfigurationInstanceIdAndRegister(ExtraDirectives: TStrings;
                                                      InstanceID: String);
    procedure NoteReceiptOfPacket(Sender: TObject;
                                  AData: TStream;
                                  ABinding: TIdSocketHandle);
    procedure NoteReceiptOfPacketOldRegistrar(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
    procedure ProvideAnswer(Sender: TObject;
                            AData: TStream;
                            ABinding: TIdSocketHandle);
    procedure SetBasicConfiguration(Configuration: TStrings);

  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckPortFree(Address: String;
                            Port: Cardinal;
                            Msg: String);
  published
    procedure TestCreateUserAgentHandlesMultipleSpaces;
    procedure TestCreateUserAgentHandlesTabs;
    procedure TestCreateUserAgentOnBusyPort;
    procedure TestCreateUserAgentRegisterDirectiveBeforeTransport;
    procedure TestCreateUserAgentReturnsSomething;
    procedure TestCreateUserAgentTransportHasMalformedPort;
    procedure TestCreateUserAgentWithAuthentication;
    procedure TestCreateUserAgentWithAutoFrom;
    procedure TestCreateUserAgentWithAutoTransport;
    procedure TestCreateUserAgentWithConserveConnections;
    procedure TestCreateUserAgentWithFrom;
    procedure TestCreateUserAgentWithHostName;
    procedure TestCreateUserAgentWithInstanceID;
    procedure TestCreateUserAgentWithInstanceIDThenRegister;
    procedure TestCreateUserAgentWithListeners;
    procedure TestCreateUserAgentWithLocator;
    procedure TestCreateUserAgentWithMalformedFrom;
    procedure TestCreateUserAgentWithMalformedLocator;
    procedure TestCreateUserAgentWithMalformedRoute;
    procedure TestCreateUserAgentWithMappedRoutes;
    procedure TestCreateUserAgentWithMockAuthenticator;
    procedure TestCreateUserAgentWithMockLocator;
    procedure TestCreateUserAgentWithMockLocatorConfigured;
    procedure TestCreateUserAgentWithMultipleEventPackageSupport;
    procedure TestCreateUserAgentWithMultipleTransports;
    procedure TestCreateUserAgentWithNoAuthentication;
    procedure TestCreateUserAgentWithNoFrom;
    procedure TestCreateUserAgentWithOneTransport;
    procedure TestCreateUserAgentWithReferSupport;
    procedure TestCreateUserAgentWithRegisterAndInstanceID;
    procedure TestCreateUserAgentWithRegistrar;
    procedure TestCreateUserAgentWithResolveNamesLocallyFirst;
    procedure TestCreateUserAgentWithoutResolveNamesLocallyFirst;
    procedure TestCreateUserAgentWithSuppressLocalResponses;
    procedure TestCreateUserAgentWithUnknownAuthentication;
    procedure TestCreateUserAgentWithUseGruu;
    procedure TestCreateUserAgentWithUseTransport;
    procedure TestCreateUserAgentWithUseInboundConnections;
    procedure TestCreateUserAgentWithUserAgentName;
    procedure TestUpdateConfigurationWithFrom;
    procedure TestUpdateConfigurationWithLocator;
    procedure TestUpdateConfigurationWithNewRegistrar;
    procedure TestUpdateConfigurationWithRegistrar;
    procedure TestUpdateConfigurationWithSupportEvent;
    procedure TestUpdateConfigurationWithBlankSupportEvent;
    procedure TestUpdateConfigurationWithSuppressLocalResponses;
    procedure TestUpdateConfigurationWithTransport;
    procedure TestUpdateConfigurationWithUseInboundConnections;
    procedure TestUpdateConfigurationWithUseTransport;
  end;

  TestConfigureProxies = class(TStackConfigurationTestCaseWithMockTransport)
  private
    EmptyRoutePath: TIdSipRoutePath;

    procedure CheckEmptyRoutePath(RoutePath: TIdSipRoutePath; Msg: String);
    procedure CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String); overload;
    procedure CheckRoutePath(Expected: TIdSipRoutePath;
                             UA: TIdSipUserAgent;
                             Target,
                             Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultRoutePath;
    procedure TestNoProxyAsDefault;
    procedure TestNoProxyToAddressSpace;
    procedure TestNoRoutePaths;
    procedure TestNullRoutesFirstInPathIgnored;
    procedure TestNullRoutesInPathIgnored;
    procedure TestReconfigureCanClearRoutePath;
    procedure TestReconfigureWithNullRoute;
    procedure TestTwoAddressSpaces;
  end;

  TestConfigureRegistrar = class(TStackConfigurationTestCaseWithMockTransport)
  private
    procedure CheckDatabaseType(ExpectedDatabase: TIdSipAbstractBindingDatabaseClass;
                                Configuration: TStrings);
    procedure CheckMockDatabase(Configuration: TStrings);
    procedure CheckUseGruu(ExpectedUseGruu: Boolean);
  published
    procedure TestActAsRegistrarDirectiveCanBeSwitchedOff;
    procedure TestActAsRegistrarDirectiveCreatesRegistrarModule;
    procedure TestActAsRegistrarDirectiveFalse;
    procedure TestActAsRegistrarDirectiveFalseThenTrue;
    procedure TestActAsRegistrarDirectiveParameters;
    procedure TestActAsRegistrarDirectiveTrueThenFalse;
    procedure TestDatabaseDirectiveImpliesActAsRegistrarDirective;
    procedure TestDatabaseDirectiveMock;
    procedure TestDatabaseDirectiveMockWithMatchOnlyUsernameOption;
    procedure TestDatabaseDirectiveUnknown;
    procedure TestMisorderedDatabaseDirectiveMock;
    procedure TestMisorderedUseGruuDirective;
    procedure TestNoRegisterDatabaseDirectiveUsesMockDatabase;
    procedure TestUseGruuDirective;
  end;

  TestConfigureMockRoutingTable = class(TStackConfigurationTestCaseWithMockTransport)
  published
    procedure TestAddLocalAddress;
    procedure TestMockRoute;
    procedure TestMockRoutingTable;
  end;

  TestConfigureMockLocator = class(TStackConfigurationTestCaseWithMockTransport)
  private
    NameRecords: TIdDomainNameRecords;
    Naptr:       TIdNaptrRecords;
    SRVs:        TIdSrvRecords;
    Target:      TIdSipUri;

    procedure CheckARecord(ExpectedIP,
                           HostName: String;
                           UserAgent: TIdSipUserAgent;
                           Records: TIdDomainNameRecords);
    procedure CheckMalformedRecord(Line, ErrorMsg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestALineBeforeLocatorLine;
    procedure TestAddARecord;
    procedure TestAddARecordMalformedDomainName;
    procedure TestAddARecordMalformedIPAddress;
    procedure TestAddAAAARecord;
    procedure TestAddNAPTRRecord;
    procedure TestAddNAPTRRecordMalformedKey;
    procedure TestAddNAPTRRecordMalformedOrder;
    procedure TestAddNAPTRRecordMalformedPreference;
    procedure TestAddNAPTRRecordMissingFlags;
    procedure TestAddSRVRecord;
    procedure TestAddSRVRecordMalformedPort;
    procedure TestAddSRVRecordMalformedPriority;
    procedure TestAddSRVRecordMalformedWeight;
  end;

  TestTIdSipReconfigureStackWait = class(TTestCase)
  private
    Configuration: TStrings;
    NewRoute:      String;
    OldRoute:      String;
    Stack:         TIdSipUserAgent;
    Timer:         TIdDebugTimerQueue;
    Wait:          TIdSipReconfigureStackWait;
  private
    procedure CheckTriggerDoesNothing(Msg: String);
    function  CreateUserAgent(Context: TIdTimerQueue): TIdSipUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerWithInappropriateObjectReference;
    procedure TestTriggerWithUnregisteredObjectID;
  end;

  TestTIdSipPendingRegistration = class(TTestCaseTU)
  private
    Pending: TIdSipPendingRegistration;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteSchedulesWait;
    procedure TestRegisterContainsUserAgentsFrom;
  end;

  TestTIdSipPendingTcpTransportConfiguration = class(TTestCaseTU)
  private
    Pending: TIdSipPendingTcpTransportConfiguration;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteSchedulesWait;
  end;

implementation

uses
  IdException, IdRegisteredObject, IdSdp, IdSimpleParser, IdSipAuthentication,
  IdSipIndyLocator, IdSipMockBindingDatabase, IdSipMockLocator,
  IdSipMockTransactionDispatcher, IdSipMockTransport, IdSipSubscribeModule,
  IdSipTCPTransport, IdSipUDPTransport, IdSystem, IdTcpClient, IdUnicode,
  SysUtils, TestFrameworkSipTransport;

const
  // SFTF: Sip Foundry Test Framework. cf. http://www.sipfoundry.org/sftf/
  SFTFInvite = 'INVITE sip:abc@80.168.137.82 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-first-request;rport=5060;received=81.86.64.25'#13#10
             + 'Via: SIP/2.0/UDP proxy1.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-proxy1-request1-fake'#13#10
             + 'Via: SIP/2.0/UDP ua.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-ua-request-fake'#13#10
             + 'From: sip:sc@81.86.64.25;tag=SCt-0-1105373135.56-81.86.64.25~case905'#13#10
             + 'Call-ID: 137057836-41e2a7cf@81.86.64.25'#13#10
             + 'Content-Length: 150'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'To: sip:abc@80.168.137.82'#13#10
             + 'Contact: sip:sc@81.86.64.25'#13#10
             + 'CSeq: 1 INVITE'#13#10
             + 'Supported:'#13#10
             + 'Content-Type: application/sdp'#13#10
             + #13#10
             + 'v=0'#13#10
             + 'o=sc 1105373135 1105373135 IN IP4 81.86.64.25'#13#10
             + 's=Dummy on hold SDP'#13#10
             + 'c=IN IP4 0.0.0.0'#13#10
             + 'm=audio 65534 RTP/AVP 0'#13#10
             + 'a=rtpmap:0 PCMU/8000'#13#10
             + 'a=recvonly'#13#10;
  SFTFMergedInvite = 'INVITE sip:abc@80.168.137.82 SIP/2.0'#13#10
                   + 'Via: SIP/2.0/UDP 81.86.64.25;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-second-request;rport=5060;received=81.86.64.25'#13#10
                   + 'Via: SIP/2.0/UDP proxy2.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-proxy2-request1-fake'#13#10
                   + 'Via: SIP/2.0/UDP ua.example.com;branch=z9hG4bK-SCb-0-1105373135.55-81.86.64.25-ua-request-fake'#13#10
                   + 'From: sip:sc@81.86.64.25;tag=SCt-0-1105373135.56-81.86.64.25~case905'#13#10
                   + 'Call-ID: 137057836-41e2a7cf@81.86.64.25'#13#10
                   + 'Content-Length: 150'#13#10
                   + 'Max-Forwards: 70'#13#10
                   + 'To: sip:abc@80.168.137.82'#13#10
                   + 'Contact: sip:sc@81.86.64.25'#13#10
                   + 'CSeq: 1 INVITE'#13#10
                   + 'Supported:'#13#10
                   + 'Content-Type: application/sdp'#13#10
                   + #13#10
                   + 'v=0'#13#10
                   + 'o=sc 1105373135 1105373135 IN IP4 81.86.64.25'#13#10
                   + 's=Dummy on hold SDP'#13#10
                   + 'c=IN IP4 0.0.0.0'#13#10
                   + 'm=audio 65534 RTP/AVP 0'#13#10
                   + 'a=rtpmap:0 PCMU/8000'#13#10
                   + 'a=recvonly'#13#10;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipUserAgent unit tests');
  Result.AddTest(TestTIdSipUserAgent.Suite);
  Result.AddTest(TestTIdSipStackConfigurator.Suite);
  Result.AddTest(TestConfigureProxies.Suite);
  Result.AddTest(TestConfigureRegistrar.Suite);
  Result.AddTest(TestConfigureMockRoutingTable.Suite);
  Result.AddTest(TestConfigureMockLocator.Suite);
  Result.AddTest(TestTIdSipReconfigureStackWait.Suite);
  Result.AddTest(TestTIdSipPendingRegistration.Suite);
  Result.AddTest(TestTIdSipPendingTcpTransportConfiguration.Suite);
end;

//******************************************************************************
//* TestTIdSipUserAgent                                                        *
//******************************************************************************
//* TestTIdSipUserAgent Public methods *****************************************

procedure TestTIdSipUserAgent.SetUp;
var
  Dest: TIdSipContactHeader;
  F:    TIdSipFromHeader;
  Ok:   TIdSipResponse;
begin
  inherited SetUp;

  Self.Dispatcher.AddTransportSendingListener(Self);

  Self.OnChangedEvent := TSimpleEvent.Create;

  Self.Core.AddListener(Self);
  Self.Core.InviteModule.AddListener(Self);

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

  Self.Invite.LastHop.SentBy := Self.Dispatcher.Transport.FirstIPBound;

  Dest := TIdSipContactHeader.Create;
  try
    Dest.Value := Self.Destination.FullValue;
    Ok := TIdSipResponse.InResponseTo(Self.Invite, SIPOK, Dest);
    try
      Self.Dlg := TIdSipDialog.CreateOutboundDialog(Self.Invite, Ok, false);
    finally
      Ok.Free;
    end;
  finally
    Dest.Free;
  end;

  F := TIdSipFromHeader.Create;
  try
    F.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
    Self.Core.From := F;
  finally
    F.Free;
  end;

  Self.SendEvent := TSimpleEvent.Create;

  Self.OnEndedSessionFired := false;
  Self.OnInboundCallFired  := false;
  Self.Password            := 'mycotoxin';
  Self.TryAgain            := true;
  Self.SessionEstablished  := false;

  Self.Locator.AddA(Self.Core.From.Address.Host, '127.0.0.1');
end;

procedure TestTIdSipUserAgent.TearDown;
begin
  Self.SendEvent.Free;
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.RemoteUri.Free;
  Self.RemoteTarget.Free;
  Self.LocalUri.Free;
  Self.ID.Free;
  Self.OnChangedEvent.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgent Protected methods **************************************

procedure TestTIdSipUserAgent.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport;
                                            Binding: TIdConnectionBindings);
begin
  inherited OnSendRequest(Request, Sender, Binding);
end;

procedure TestTIdSipUserAgent.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport;
                                             Binding: TIdConnectionBindings);
begin
  inherited OnSendResponse(Response, Sender, Binding);

  if (Response.StatusCode = SIPSessionProgress) then
    Self.SendEvent.SetEvent;
end;

//* TestTIdSipUserAgent Private methods ****************************************

procedure TestTIdSipUserAgent.CheckCreateRequest(Dest: TIdSipToHeader;
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
  CheckEquals(Self.Core.From.DisplayName,
              Contact.DisplayName,
              'Contact header incorrectly set');

  CheckEquals(Self.Core.From.DisplayName,
              Request.From.DisplayName,
              'From.DisplayName');
  CheckEquals(Self.Core.From.Address,
              Request.From.Address,
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
  CheckEquals(UdpTransport,
              Request.LastHop.Transport,
              'UDP should be the default transport');
end;

procedure TestTIdSipUserAgent.CheckGruuSet(ExpectedGruu: String; Msg: String);
begin
  Self.MarkSentRequestCount;
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
  CheckRequestSent('CheckGruuSet: no request sent (' + Msg + ')');

  CheckEquals(ExpectedGruu, Self.LastSentRequest.FirstContact.Address.AsString, Msg);
end;

procedure TestTIdSipUserAgent.CheckIsUnregister(Request: TIdSipRequest);
begin
  Check(Request.HasHeader(ContactHeaderFull), 'Request looks like a registration query');
  CheckEquals(0, Request.FirstContact.Expires, 'Request not a deregistration');
end;

procedure TestTIdSipUserAgent.OnAddAction(UserAgent: TIdSipAbstractCore;
                                          Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipUserAgent.OnAuthenticationChallenge(Action: TIdSipAction;
                                                        Response: TIdSipResponse);
begin
  raise Exception.Create('implement TestTIdSipUserAgent.OnAuthenticationChallenge');
end;

procedure TestTIdSipUserAgent.OnChanged(Observed: TObject);
begin
  Self.OnChangedEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                        Message: TIdSipMessage;
                                                        Binding: TIdConnectionBindings);
begin
  // Do nothing.
end;

procedure TestTIdSipUserAgent.OnEndedSession(Session: TIdSipSession;
                                             ErrorCode: Cardinal;
                                             const Reason: String);
begin
  Self.OnEndedSessionFired := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnEstablishedSession(Session: TIdSipSession;
                                                   const RemoteSessionDescription: String;
                                                   const MimeType: String);
begin
  Self.InboundCallMimeType := MimeType;
  Self.InboundCallOffer    := RemoteSessionDescription;
  Self.SessionEstablished  := true;
end;

procedure TestTIdSipUserAgent.OnInboundCall(UserAgent: TIdSipInviteModule;
                                            Session: TIdSipInboundSession);
begin
  Self.InboundCallMimeType := Session.RemoteMimeType;
  Self.InboundCallOffer    := Session.RemoteSessionDescription;
  Self.UserAgentParam      := UserAgent.UserAgent;
  Self.OnInboundCallFired := true;

  Session.AddSessionListener(Self);
  Self.Session := Session;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipUserAgent.OnModifiedSession(Session: TIdSipSession;
                                                Answer: TIdSipResponse);
begin
end;

procedure TestTIdSipUserAgent.OnModifySession(Session: TIdSipSession;
                                              const RemoteSessionDescription: String;
                                              const MimeType: String);
begin
end;

procedure TestTIdSipUserAgent.OnNetworkFailure(Action: TIdSipAction;
                                               ErrorCode: Cardinal;
                                               const Reason: String);
begin
  Self.FailReason := Reason;
end;

procedure TestTIdSipUserAgent.OnProgressedSession(Session: TIdSipSession;
                                                  Progress: TIdSipResponse);
begin
end;

procedure TestTIdSipUserAgent.OnReferral(Session: TIdSipSession;
                                         Refer: TIdSipRequest;
                                         Binding: TIdConnectionBindings);
begin
end;

procedure TestTIdSipUserAgent.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                             Action: TIdSipAction);
begin
  // Do nothing.
end;

procedure TestTIdSipUserAgent.OnTerminated(Action: TIdSipAction);
begin
end;

procedure TestTIdSipUserAgent.ReceiveBye(Dialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(Dialog);
  try
    Self.ReceiveRequest(Bye);
  finally
    Bye.Free;
  end;
end;

//* TestTIdSipUserAgent Published methods **************************************

procedure TestTIdSipUserAgent.TestAcksDontMakeTransactions;
var
  Ack:       TIdSipRequest;
  RemoteDlg: TIdSipDialog;
  TranCount: Cardinal;
begin
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.Session.AcceptCall('', '');

  TranCount := Self.Dispatcher.TransactionCount;

  RemoteDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                 Self.LastSentResponse,
                                                 false);
  try
    Ack := RemoteDlg.CreateAck;
    try
      Self.ReceiveRequest(Ack);

      CheckEquals(TranCount,
                Self.Dispatcher.TransactionCount,
                  'A transaction got made in response to an ACK');
      CheckEquals(1,
                  Self.Core.SessionCount,
                  'ACK wasn''t simply dropped by the TU');
    finally
      Ack.Free;
    end;
  finally
    RemoteDlg.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAcceptCallSchedulesResendOk;
begin
  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');
  Self.MarkSentResponseCount;
  
  Self.Session.AcceptCall('', '');
  Self.DebugTimer.TriggerEarliestEvent;
  CheckResponseSent('No OK sent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response sent');

  Self.MarkSentResponseCount;
  Self.DebugTimer.TriggerEarliestEvent;
  CheckResponseSent('No OK resent');
  CheckEquals(SIPOK, Self.LastSentResponse.StatusCode, 'Unexpected response resent');
end;

procedure TestTIdSipUserAgent.TestActionsNotifyUAObservers;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Core.AddObserver(L1);

    Self.ReceiveInvite;

    Check(L1.Changed, 'L1 not notified');
  finally
    Self.Core.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddListener;
var
  L1, L2: TIdSipTestTransactionUserListener;
begin
  L1 := TIdSipTestTransactionUserListener.Create;
  try
    L2 := TIdSipTestTransactionUserListener.Create;
    try
      Self.Core.AddListener(L1);
      Self.Core.AddListener(L2);

      Self.ReceiveOk(Self.Invite);

      Check(L1.DroppedUnmatchedMessage and L2.DroppedUnmatchedMessage,
            'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestAddRoute;
const
  LocalAddressSpace = '127.0.0.0/8';
var
  Entity: TIdSipUri;
begin
  Entity := TIdSipUri.Create('sip:gw.tessier-ashpool.co.luna');
  try
    Self.Core.AddRoute(LocalAddressSpace, Entity);

    // This makes sure that the INVITE goes to the localhost, and avoids any
    // influence from the Locator.
    Self.Destination.Address.Host := '127.0.0.1';
    Self.Destination.Address.Port := 5060;

    Self.MarkSentRequestCount;
    Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
    CheckRequestSent('No INVITE sent');
    Check(Self.LastSentRequest.HasRoute, 'No Route header added');
    CheckEquals(Entity.AsString, Self.LastSentRequest.FirstRoute.Address.AsString, 'Unexpected Route');
  finally
    Entity.Free;
  end;
end;
{
// Should we even bother with this test?
procedure TestTIdSipUserAgent.TestByeWithAuthentication;
var
  Session: TIdSipOutboundSession;
begin
  //  ---      INVITE      --->
  // <---      200 OK      ---
  //  ---        ACK       --->
  // ==========================
  //       Media streams
  // ==========================
  //  ---        BYE       --->
  // <--- 401 Unauthorized ---
  //  ---        BYE       --->
  // <---      200 OK      --->

  Session := Self.Core.InviteModule.Call(Self.Destination, '', '');
  Session.AddSessionListener(Self);
  Session.Send;

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent('No ACK sent: ' + Self.FailReason);

  Session.Terminate;

  // This is a bit tricky - the Transaction layer reissues the request, not the
  // Transaction-User layer. All the TU layer does is provide an authentication
  // token.
  Self.MarkSentRequestCount;
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  Self.CheckRequestSent('No re-issue of a BYE');
end;
}
procedure TestTIdSipUserAgent.TestCallUsingRoutePath;
const
  RouteOne = 'sip:proxy.tessier-ashpool.co.luna;lr';
  RouteTwo = 'sip:gw1.leo-ix.net;lr';
var
  Invite: TIdSipRequest;
  RouteUriOne: TIdSipUri;
  RouteUriTwo: TIdSipUri;
begin
  RouteUriOne := TIdSipUri.Create(RouteOne);
  try
    RouteUriTwo := TIdSipUri.Create(RouteTwo);
    try
      Self.Core.DefaultRoutePath.AddRoute(RouteUriOne);
      Self.Core.DefaultRoutePath.AddRoute(RouteUriTwo);

      Self.MarkSentRequestCount;
      Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
      CheckRequestSent('No request sent');
      CheckEquals(MethodInvite,
                  Self.LastSentRequest.Method,
                  'Unexpected request sent');

      Invite := Self.LastSentRequest;
      Check(Invite.HasHeader(RouteHeader),
            'No Route header added');

      Invite.Route.First;
      CheckEquals(RouteOne,
                  Invite.Route.CurrentRoute.Address.Uri,
                  'Route points to wrong next-hop');

      Invite.Route.Next;
      CheckEquals(RouteTwo,
                  Invite.Route.CurrentRoute.Address.Uri,
                  'Route points to wrong next-next-hop');
    finally
      RouteUriTwo.Free;
    end;
  finally
    RouteUriOne.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCancelNotifiesTU;
var
  SessCount: Integer;
begin
  Self.ReceiveInvite;
  SessCount := Self.Core.SessionCount;
  Self.ReceiveCancel;

  Check(Self.OnEndedSessionFired,
        'UA not notified of remote CANCEL');
  Check(Self.Core.SessionCount < SessCount,
        'UA didn''t remove cancelled session');
end;

procedure TestTIdSipUserAgent.TestConcurrentCalls;
var
  AckOne:    TIdSipRequest;
  AckTwo:    TIdSipRequest;
  ByeOne:    TIdSipRequest;
  ByeTwo:    TIdSipRequest;
  DialogOne: TIdSipDialog;
  DialogTwo: TIdSipDialog;
  InviteOne: TIdSipRequest;
  InviteTwo: TIdSipRequest;
begin
  // <---    INVITE #1   ---
  //  ---     100 #1     --->
  //  ---     180 #1     --->
  //  ---     200 #1     --->
  // <---     ACK #1     ---
  //  ---   200 #1 (ACK) --->
  // <---    INVITE #2   ---
  //  ---     100 #2     --->
  //  ---     180 #2     --->
  //  ---     200 #2     --->
  // <---     ACK #2     ---
  //  ---   200 #2 (ACK) --->
  // <---     BYE #1     ---
  //  ---   200 #1 (BYE) --->
  // <---     BYE #2     ---
  //  ---   200 #2 (BYE) --->

  InviteOne := TIdSipTestResources.CreateBasicRequest;
  try
    InviteTwo := TIdSipTestResources.CreateBasicRequest;
    try
      InviteOne.CallID         := '1.' + InviteOne.CallID;
      InviteOne.From.Tag       := '1';
      InviteOne.LastHop.Branch := InviteOne.LastHop.Branch + '1';
      InviteTwo.CallID         := '2.' + InviteTwo.CallID;
      InviteTwo.From.Tag       := '2';
      InviteTwo.LastHop.Branch := InviteTwo.LastHop.Branch + '2';

      Self.ReceiveRequest(InviteOne);
      Check(Self.OnInboundCallFired, 'OnInboundCall didn''t fire for 1st INVITE');
      Self.Session.AcceptCall('', '');

      // DialogOne represents the remote agent's dialog for the 1st INVITE.
      DialogOne := TIdSipDialog.CreateInboundDialog(InviteOne,
                                                    Self.LastSentResponse,
                                                    InviteOne.RequestUri.IsSecure);
      try
        AckOne := DialogOne.CreateAck;
        try
          Self.ReceiveRequest(AckOne);
        finally
          AckOne.Free;
        end;

        Self.OnInboundCallFired := false;
        Self.ReceiveRequest(InviteTwo);
        Check(Self.OnInboundCallFired, 'OnInboundCall didn''t fire for 2nd INVITE');
        Self.Session.AcceptCall('', '');

        // DialogTwo represents the remote agent's dialog for the 2nd INVITE.
        DialogTwo := TIdSipDialog.CreateInboundDialog(InviteTwo,
                                                      Self.LastSentResponse,
                                                      InviteTwo.RequestUri.IsSecure);
        try
          AckTwo := DialogTwo.CreateAck;
          try
            Self.ReceiveRequest(AckTwo);
          finally
            AckTwo.Free;
          end;

          Self.MarkSentResponseCount;
          ByeOne := DialogOne.CreateRequest;
          try
            Self.ReceiveBye(DialogOne);
          finally
            ByeOne.Free;
          end;

          CheckResponseSent('No response sent for the 1st INVITE''s BYE');
          CheckEquals(SIPOK,
                      Self.LastSentResponse.StatusCode,
                      'Unexpected response for the 1st INVITE''s BYE');

          Self.MarkSentResponseCount;
          ByeTwo := DialogTwo.CreateRequest;
          try
            Self.ReceiveBye(DialogTwo);
          finally
            ByeTwo.Free;
          end;

          CheckResponseSent('No response sent for the 2nd INVITE''s BYE');
          CheckEquals(SIPOK,
                      Self.LastSentResponse.StatusCode,
                      'Unexpected response for the 2nd INVITE''s BYE');
        finally
          DialogTwo.Free;
        end;
      finally
        DialogOne.Free;
      end;
    finally
      InviteTwo.Free;
    end;
  finally
    InviteOne.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestContentTypeDefault;
begin
  CheckEquals(SdpMimeType,
              Self.Core.AllowedContentTypes,
              'AllowedContentTypes');
end;

procedure TestTIdSipUserAgent.TestCreateRequest;
const
  MaxForwards   = 42;
  UnknownMethod = 'Foo';
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(UnknownMethod, Self.Core.From, Dest, MaxForwards);
    try
      CheckEquals(MaxForwards,   Request.MaxForwards, 'Max-Forwards');
      CheckEquals(UnknownMethod, Request.Method,      'Request-Method');
      Self.CheckCreateRequest(Dest, Request);
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequestInDialogWithRoutes;
const
  RouteOne = 'sip:proxy.tessier-ashpool.co.luna;lr';
  RouteTwo = 'sip:gw1.leo-ix.net;lr';
var
  Inv:         TIdSipRequest;
  RouteUriOne: TIdSipUri;
  RouteUriTwo: TIdSipUri;
begin
  RouteUriOne := TIdSipUri.Create(RouteOne);
  try
    RouteUriTwo := TIdSipUri.Create(RouteTwo);
    try
      Self.Core.DefaultRoutePath.AddRoute(RouteUriOne);
      Self.Core.DefaultRoutePath.AddRoute(RouteUriTwo);

      Check(Self.Dlg.RouteSet.IsEmpty, 'Dialog doesn''t have an empty route set');
      Inv := Self.Core.CreateRequest(MethodInvite, Self.Dlg);
      try
        Check(not Inv.HasRoute, 'In-dialog request created with Route header, but dialog has no route set');
      finally
        Inv.Free;
      end;
    finally
      RouteUriTwo.Free;
    end;
  finally
    RouteUriOne.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sips:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Dest, TIdSipRequest.DefaultMaxForwards);
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

procedure TestTIdSipUserAgent.TestCreateRequestUserAgent;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Self.Core.UserAgentName := 'SATAN/1.0';

  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Request := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Dest, TIdSipRequest.DefaultMaxForwards);
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

procedure TestTIdSipUserAgent.TestCreateRequestWithTransport;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=udp';
    Request := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Dest, TIdSipRequest.DefaultMaxForwards);
    try
      CheckEquals(UdpTransport,
                  Request.LastHop.Transport,
                  'UDP transport not specified');
    finally
      Request.Free;
    end;

    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=tcp';
    Request := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Dest, TIdSipRequest.DefaultMaxForwards);
    try
      CheckEquals(TcpTransport,
                  Request.LastHop.Transport,
                  'TCP transport not specified');
    finally
      Request.Free;
    end;

    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;transport=foo';
    Request := Self.Core.CreateRequest(MethodInvite, Self.Core.From, Dest, TIdSipRequest.DefaultMaxForwards);
    try
      CheckEquals('FOO',
                  Request.LastHop.Transport,
                  'foo transport not specified');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestCreateResponseToTagMissing;
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

procedure TestTIdSipUserAgent.TestCreateResponseUserAgent;
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

procedure TestTIdSipUserAgent.TestCreateResponseUserAgentBlank;
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

procedure TestTIdSipUserAgent.TestDeclinedCallNotifiesListeners;
var
  O: TIdObserverListener;
begin
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  O := TIdObserverListener.Create;
  try
    Self.Core.AddObserver(O);

    Self.ReceiveResponse(SIPDecline);

    Check(O.Changed, 'Clearing up a terminated action should notify observers');
  finally
    Self.Core.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDestroyUnregisters;
var
  Listener: TIdSipTestTransportSendingListener;
  UA:       TIdSipUserAgent;
begin
  Listener := TIdSipTestTransportSendingListener.Create;
  try
    UA := Self.CreateUserAgent('sip:case@localhost');
    try
      UA.RegisterWith(Self.Destination.Address, UA.From);
    finally
      (UA.Dispatcher as TIdSipMockTransactionDispatcher).Transport.AddTransportSendingListener(Listener);
      UA.Free;
      Check(Listener.SentRequest,
            'No REGISTER sent, so UA didn''t unregister when Freeing');
      CheckEquals(MethodRegister, Listener.RequestParam.Method, 'Unexpected request sent');
    end;
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDialogLocalSequenceNoMonotonicallyIncreases;
var
  BaseSeqNo: Cardinal;
  R:         TIdSipRequest;
begin
  R := Self.Core.CreateRequest(MethodInvite, Self.Dlg);
  try
     BaseSeqNo := R.CSeq.SequenceNo;
  finally
    R.Free;
  end;

  R := Self.Core.CreateRequest(MethodInvite, Self.Dlg);
  try
    CheckEquals(BaseSeqNo + 1,
                R.CSeq.SequenceNo,
                'Not monotonically increasing by one');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestDispatchToCorrectSession;
var
  SessionOne: TIdSipInboundSession;
  SessionTwo: TIdSipInboundSession;
begin
  // 1. Receive two inbound sessions.
  // 2. Receive a BYE for one of them.
  // 3. Check that the correct session died, and the other didn't.

  Self.ReceiveInvite;
  Check(Assigned(Self.Session),
        'OnInboundCall didn''t fire');
  SessionOne := Self.Session;

  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.Invite.ToHeader.Tag   := Self.Invite.ToHeader.Tag + '1';
  Self.ReceiveInvite;
  Check(Self.Session <> SessionOne,
        'OnInboundCall didn''t fire a second time');
  SessionTwo := Self.Session;
  CheckEquals(2,
              Self.Core.SessionCount,
              'Number of sessions after two INVITEs');


  SessionTwo.AcceptCall('', '');
  Check(SessionTwo.DialogEstablished, 'SessionTwo''s dialog wasn''t established');

  SessionTwo.AddSessionListener(Self);
  Self.ThreadEvent.ResetEvent;
  Self.ExceptionMessage := 'SessionTwo wasn''t terminated';
  Self.ReceiveBye(SessionTwo.Dialog);

  Check(not SessionOne.IsTerminated, 'SessionOne was terminated');
  CheckEquals(1,
              Self.Core.SessionCount,
              'Number of sessions after one BYE');
end;

procedure TestTIdSipUserAgent.TestDontReAuthenticate;
begin
  Self.TryAgain := false;

  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  Self.MarkSentRequestCount;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, QopAuthInt);

  CheckNoRequestSent('Reattempted authentication');
end;

procedure TestTIdSipUserAgent.TestInboundCall;
begin
  Self.Invite.Body          := TIdSipTestResources.BasicSDP('foo.com');
  Self.Invite.ContentLength := Length(Self.Invite.Body);
  Self.Invite.ContentType   := SdpMimeType;

  Self.ReceiveInvite;

  Check(Assigned(Self.UserAgentParam),
        'OnInboundCall didn''t fire');

  CheckEquals(Self.Invite.Body,
              Self.InboundCallOffer,
              'Offer');
  CheckEquals(Self.Invite.ContentType,
              Self.InboundCallMimeType,
              'Offer MIME type');
  Check(Self.Core = Self.UserAgentParam,
        'UserAgent param of Session''s InboundCall notification wrong');
end;

procedure TestTIdSipUserAgent.TestInviteRaceCondition;
begin
  CheckEquals(0,
              Self.Core.CountOf(MethodInvite),
              'Sanity check - new test should have no ongoing INVITE actions');

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;
  CheckEquals(1,
              Self.Core.CountOf(MethodInvite),
              'First INVITE didn''t make a new INVITE action');

  CheckResponseSent('No response sent');

  Self.ReceiveInvite;
  CheckEquals(1,
              Self.Core.CountOf(MethodInvite),
              'INVITE resend made a new INVITE action');
end;

procedure TestTIdSipUserAgent.TestMergedRequest;
var
  FirstInvite:  TIdSipRequest;
  SecondInvite: TIdSipRequest;
begin
  FirstInvite := TIdSipRequest.ReadRequestFrom(SFTFInvite);
  try
    SecondInvite := TIdSipRequest.ReadRequestFrom(SFTFMergedInvite);
    try
      Self.ReceiveRequest(FirstInvite);
      Self.MarkSentResponseCount;
      Self.ReceiveRequest(SecondInvite);

      CheckResponseSent('No response sent');

      Check(SecondInvite.Match(Self.LastSentResponse),
            'Response not for 2nd INVITE');
      CheckEquals(SIPLoopDetected,
                  Self.LastSentResponse.StatusCode,
                  'Unexpected response');
    finally
      SecondInvite.Free;
    end;
  finally
    FirstInvite.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestNewUAHasSensibleFrom;
var
  UA: TIdSipUserAgent;
begin
  UA := TIdSipUserAgent.Create;
  try
    Check(UA.HostName <> '',
          'Sanity check: the HostName property must be non-empty');

    CheckEquals(UA.HostName,
                UA.From.Address.Host,
                'From host should default to the UA''s HostName');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestNotificationOfNewSession;
begin
  Self.ReceiveInvite;

  Check(Self.OnInboundCallFired, 'UI not notified of new session');
end;

procedure TestTIdSipUserAgent.TestNotificationOfNewSessionRobust;
var
  L1, L2: TIdSipTestTransactionUserListener;
begin
  L1 := TIdSipTestTransactionUserListener.Create;
  try
    L2 := TIdSipTestTransactionUserListener.Create;
    try
      L1.FailWith := EParserError;

      Self.Core.AddListener(L1);
      Self.Core.AddListener(L2);

      Self.ReceiveOk(Self.Invite);

      Check(L2.DroppedUnmatchedMessage, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestOutboundCallAndByeToXlite;
var
  Session: TIdSipSession;
begin
  Session := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Session.AddSessionListener(Self);
  Session.Send;

  Self.ReceiveTrying(Self.LastSentRequest);
  Check(not Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's don''t make dialogs');

  Self.ReceiveRinging(Self.LastSentRequest);
  Check(Session.DialogEstablished,
        Self.LastSentResponse.Description
      + 's with To tags make dialogs');
  Check(Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make early dialogs');

  Self.MarkSentAckCount;
  Self.ReceiveOk(Self.LastSentRequest);
  CheckAckSent('No ACK sent: ' + Self.FailReason);
  Check(not Session.IsEarly,
        Self.LastSentResponse.Description
      + 's make non-early dialogs');

  Self.ReceiveOk(Self.LastSentRequest);
  Self.ReceiveOk(Self.LastSentRequest);
  Self.ReceiveOk(Self.LastSentRequest);

  Self.Core.TerminateAllCalls;
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Must send a BYE to terminate an established session');
end;

procedure TestTIdSipUserAgent.TestOutboundInviteSessionProgressResends;
begin
  Self.MarkSentResponseCount;

  // Receive an INVITE. Ring. Wait.
  Self.Core.ProgressResendInterval := 50;

  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');

  Self.DebugTimer.TriggerEarliestEvent;

  CheckResponseSent('No response sent');
  CheckEquals(SIPSessionProgress,
              Self.LastSentResponse.StatusCode,
              'Wrong response');
end;

procedure TestTIdSipUserAgent.TestOutboundInviteDoesNotTerminateWhenNoResponse;
begin
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
  CheckEquals(1, Self.Core.CountOf(MethodInvite), 'Calling makes an INVITE');

  Self.DebugTimer.TriggerEarliestEvent;
  CheckEquals(1,
              Self.Core.CountOf(MethodInvite),
              'If we never get a response then we DO NOT give up');
end;

procedure TestTIdSipUserAgent.TestReceiveByeForDialog;
var
  Response: TIdSipResponse;
begin
  Self.ReceiveInvite;

  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
  Self.Session.AcceptCall('', '');
  Self.ReceiveAck;

  Self.MarkSentResponseCount;
  Self.ReceiveBye(Self.Session.Dialog);

  CheckResponseSent('SOMETHING should have sent a response');

  Response := Self.LastSentResponse;
  CheckNotEquals(SIPCallLegOrTransactionDoesNotExist,
                 Response.StatusCode,
                 'UA tells us no matching dialog was found');
end;

procedure TestTIdSipUserAgent.TestReceiveByeDestroysTerminatedSession;
var
  O: TIdObserverListener;
begin
  O := TIdObserverListener.Create;
  try
    Self.ReceiveInvite;
    Check(Assigned(Self.Session), 'OnInboundCall didn''t fire');
    Self.Session.AcceptCall('', '');

    Self.Core.AddObserver(O);

    Self.ReceiveBye(Self.Session.Dialog);

    CheckEquals(0, Self.Core.SessionCount, 'Number of sessions after BYE');
    Check(O.Changed, 'Observer not notified after session ended');
  finally
    Self.Core.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestReceiveResponseWithMultipleVias;
var
  FakeContact: TIdSipContactHeader;
  Response:    TIdSipResponse;
begin
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');

  FakeContact := TIdSipContactHeader.Create;
  try
    FakeContact.Value := Self.Core.From.FullValue;

    Response := TIdSipResponse.InResponseTo(Self.Invite,
                                            SIPOK,
                                            FakeContact);
    try
      Response.AddHeader(Response.Path.LastHop);
      Self.ReceiveResponse(Response);
      Check(not Self.SessionEstablished,
            'Multiple-Via Response not dropped');
    finally
      Response.Free;
    end;
  finally
    FakeContact.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRegisterWith;
var
  RequestsFrom: TIdSipFromHeader;
begin
  Self.Core.UseGruu := false;

  Self.MarkSentRequestCount;
  Self.Core.RegisterWith(Self.RemoteTarget, Self.Core.From).Send;
  CheckRequestSent('No REGISTER sent');
  CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');

  Check(not Self.LastSentRequest.FirstContact.IsGruu,
        'If the Core doesn''t use GRUUs the Contact mustn''t be a GRUU');

  Check(not Self.LastSentRequest.FirstContact.HasParameter(SipInstanceParam),
        '"' + SipInstanceParam + '" parameters apply only to GRUUs');

  RequestsFrom := TIdSipFromHeader.Create;
  try
    RequestsFrom.Assign(Self.LastSentRequest.From);
    RequestsFrom.RemoveParameter(TagParam);

    CheckEquals(Self.Core.From.AsString, RequestsFrom.AsString, 'From');
  finally
    RequestsFrom.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRegisterWithGruu;
var
  Gruu:       TIdSipContactHeader;
  OkWithGruu: TIdSipResponse;
begin
  Self.Core.UseGruu := true;
  Self.Core.InstanceID := ConstructUUIDURN;

  Self.MarkSentRequestCount;
  Self.Core.RegisterWith(Self.RemoteTarget, Self.Core.From).Send;
  CheckRequestSent('No REGISTER sent');
  Check(Self.LastSentRequest.FirstContact.HasParameter(SipInstanceParam), 'REGISTER has no sip.instance parameter in its (first) Contact');
  CheckEquals(Self.Core.InstanceID, Self.LastSentRequest.FirstContact.SipInstance, 'sip.instance parameter not sent');

  OkWithGruu := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPOK);
  try
    OkWithGruu.Supported.Values.Add(ExtensionGruu);
    Gruu := OkWithGruu.AddHeader(ContactHeaderFull) as TIdSipContactHeader;

    // This, so that the address of record of Gruu and the Contact in the last
    // sent request match.
    Gruu.Value := Self.LastSentRequest.FirstContact.FullValue;
    Gruu.Gruu := 'sip:arbitrary.gruu@example.com;opaque=foo';

    Self.ReceiveResponse(OkWithGruu);

    CheckGruuSet(Gruu.Gruu, 'Core''s GRUU not set');
  finally
    OkWithGruu.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRegisterWithReceiveMultipleGruus;
const
  OurUrn   = 'urn:uuid:00000000-0000-0000-0000-000000000000';
  TheirUrn = 'urn:uuid:11111111-1111-1111-1111-111111111111';
var
  GruuOne:    TIdSipContactHeader;
  GruuTwo:    TidSipContactHeader;
  OkWithGruu: TIdSipResponse;
begin
  // If more than one UA registers for the same Address Of Record, then THIS
  // UA only wants to know ITS GRUU when it registers.

  Self.Core.InstanceID := OurUrn;
  Self.Core.UseGruu    := true;

  Self.MarkSentRequestCount;
  Self.Core.RegisterWith(Self.RemoteTarget, Self.Core.From).Send;
  CheckRequestSent('No REGISTER sent');

  OkWithGruu := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPOK);
  try
    OkWithGruu.Supported.Values.Add(ExtensionGruu);
    // The other UA
    GruuOne := OkWithGruu.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
    // This, so that the address of record of Gruu and the Contact in the last
    // sent request match.
    GruuOne.Value       := Self.LastSentRequest.FirstContact.FullValue;
    GruuOne.Gruu        := 'sip:arbitrary.contact@example.com;opaque=bar';
    GruuOne.SipInstance := TheirUrn;

    // Our UA
    GruuTwo := OkWithGruu.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
    GruuTwo.Value       := Self.LastSentRequest.FirstContact.FullValue;
    GruuTwo.Gruu        := 'sip:arbitrary.contact@example.com;opaque=foo';
    GruuTwo.SipInstance := OurUrn;

    Self.ReceiveResponse(OkWithGruu);

    CheckGruuSet(GruuTwo.Gruu,
                 'Core''s GRUU not set');
  finally
    OkWithGruu.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRegisterWithRegistrarNameDoesntResolve;
begin
  Self.Dispatcher.MockLocator.ReturnOnlySpecifiedRecords := true;
  Self.Destination.Value := 'sip:rlyeh.org';

  Self.MarkSentRequestCount;
  Self.Core.RegisterWith(Self.Destination.Address, Self.Core.From).Send;
  CheckNoRequestSent('Request sent even though "rlyeh.org" is not resolvable');
end;

procedure TestTIdSipUserAgent.TestRejectMalformedAuthorizedRequest;
var
  Auth:     TIdSipMockAuthenticator;
  Response: TIdSipResponse;
begin
  Auth := TIdSipMockAuthenticator.Create;
  try
    Self.Core.Authenticator := Auth;
    Auth.FailWith := EAuthenticate;

    Self.MarkSentResponseCount;

    Self.Invite.AddHeader(AuthorizationHeader);
    Self.ReceiveInvite;
    CheckResponseSent('No response sent');

    Response := Self.LastSentResponse;
    CheckEquals(SIPBadRequest,
                Response.StatusCode,
                'Status code');
  finally
    Auth.Free;
    Self.Core.Authenticator := Self.Authenticator;
  end;
end;

procedure TestTIdSipUserAgent.TestRejectMethodNotAllowed;
//var
//  Response: TIdSipResponse;
begin
  // This blank test serves as a reminder of missing functionality: we want to
  // support permissions on our URIs, so that we can express the fact that we
  // allow someone to subscribe to URI-A's state, but not to URI-B's.
  Fail('This blank test serves as a reminder of missing functionality.');
{
  Self.MarkSentResponseCount;

  Self.ReceiveSubscribe('Foo');

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPMethodNotAllowed,
              Response.StatusCode,
              'Unexpected response');
  Check(Response.HasHeader(AllowHeader),
        'No Allow header');
  CheckEquals(Self.Core.KnownMethods,
              Response.FirstHeader(AllowHeader).Value,
              'Currently we only support one URI - as a User Agent typically '
            + 'does. Obviously that''ll eventually change');
}
end;

procedure TestTIdSipUserAgent.TestRejectNoContact;
var
  Response: TIdSipResponse;
begin
  Self.Invite.RemoveHeader(Self.Invite.FirstContact);

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadRequest,        Response.StatusCode, 'Status-Code');
  CheckEquals(MissingContactHeader, Response.StatusText, 'Status-Text');
end;

procedure TestTIdSipUserAgent.TestRejectUnauthorizedRequest;
var
  Auth:     TIdSipMockAuthenticator;
  Response: TIdSipResponse;
begin
  Auth := Self.Core.Authenticator as TIdSipMockAuthenticator;
  Auth.AuthenticateAllRequests := false;

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnauthorized,
              Response.StatusCode,
              'Status code');
  Check(Response.HasWWWAuthenticate,
        'No WWW-Authenticate header');
end;

procedure TestTIdSipUserAgent.TestRemoveUserAgentListener;
var
  L1, L2: TIdSipTestTransactionUserListener;
begin
  L1 := TIdSipTestTransactionUserListener.Create;
  try
    L2 := TIdSipTestTransactionUserListener.Create;
    try
      Self.Core.AddListener(L1);
      Self.Core.AddListener(L2);
      Self.Core.RemoveListener(L2);

      Self.ReceiveOk(Self.Invite);

      Check(L1.DroppedUnmatchedMessage and not L2.DroppedUnmatchedMessage,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestRFC2543InviteCallFlow;
const
  RawSippInvite = 'INVITE sip:service@80.168.137.82:5060 SIP/2.0'#13#10
                + 'Via: SIP/2.0/UDP 81.86.64.25:5060'#13#10
                + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
                + 'To: sut <sip:service@80.168.137.82:5060>'#13#10
                + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
                + 'Max-Forwards: 70'#13#10
                + 'Subject: Performance Test'#13#10
                + 'Content-Length: 0'#13#10#13#10;
  RawSippAck = 'ACK sip:service@80.168.137.82:5060 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25'#13#10
             + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
             + 'To: sut <sip:service@80.168.137.82:5060>;tag=%s'#13#10
             + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
             + 'CSeq: 1 ACK'#13#10
             + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'Subject: Performance Test'#13#10
             + 'Content-Length: 0'#13#10#13#10;
  RawSippBye = 'BYE sip:service@80.168.137.82:5060 SIP/2.0'#13#10
             + 'Via: SIP/2.0/UDP 81.86.64.25'#13#10
             + 'From: sipp <sip:sipp@81.86.64.25:5060>;tag=1'#13#10
             + 'To: sut <sip:service@80.168.137.82:5060>;tag=%s'#13#10
             + 'Call-ID: 1.87901.81.86.64.25@sipp.call.id'#13#10
             + 'CSeq: 2 BYE'#13#10
             + 'Contact: sip:sipp@81.86.64.25:5060'#13#10
             + 'Max-Forwards: 70'#13#10
             + 'Subject: Performance Test'#13#10
             + 'Content-Length: 0'#13#10#13#10;
var
  SippAck:    TIdSipRequest;
  SippBye:    TIdSipRequest;
  SippInvite: TIdSipRequest;
begin
  // SIPp is a SIP testing tool: http://sipp.sourceforge.net/

  SippInvite := TIdSipRequest.ReadRequestFrom(RawSippInvite);
  try
    Self.MarkSentResponseCount;
    Self.ReceiveRequest(SippInvite);
    Check(Assigned(Self.Session),
          'OnInboundCall didn''t fire');
    Self.Session.AcceptCall('', '');

    SippAck := TIdSipRequest.ReadRequestFrom(Format(RawSippAck,
                                                    [Self.Session.Dialog.ID.LocalTag]));
    try
      Self.ReceiveRequest(SippAck);
    finally
      SippAck.Free;
    end;

    Self.MarkSentResponseCount;

    SippBye := TIdSipRequest.ReadRequestFrom(Format(RawSippBye,
                                                    [Self.Session.Dialog.ID.LocalTag]));
    try
      Self.ReceiveRequest(SippBye);
    finally
      SippBye.Free;
    end;

    CheckResponseSent('No response sent for the BYE');

    CheckEquals(SIPOK,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
  finally
    SippInvite.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestScheduleEventActionClosure;
var
  EventCount: Integer;
begin
  EventCount := Self.DebugTimer.EventCount;
  Self.Core.ScheduleEvent(TIdSipInboundInviteExpire, 50, Self.Invite.Copy, '');
  Check(EventCount < DebugTimer.EventCount,
        'Event not scheduled');
end;

procedure TestTIdSipUserAgent.TestSetFrom;
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

procedure TestTIdSipUserAgent.TestSetFromMailTo;
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

procedure TestTIdSipUserAgent.TestSimultaneousInAndOutboundCall;
begin
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
  Self.ReceiveTrying(Self.LastSentRequest);
  Self.ReceiveRinging(Self.LastSentRequest);

  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'TU not informed of inbound call');

  Self.Session.AcceptCall('', '');
  CheckEquals(2, Self.Core.SessionCount, 'Session count');
end;

procedure TestTIdSipUserAgent.TestTerminateAllCalls;
var
  InboundSession: TIdSipInboundSession;
  Sess:           TIdSipSession;
begin
  // We have:
  // * an established inbound call (#1);
  // * an unestablished inbound call (#2);
  // * an unestablished outbound call (#3);
  // * an established outbound call (#4).
  // When we terminate everything, we expect only the unestablished outbound
  // call to remain, because it can only terminate according to RFC 3261 section 9.1

  // Set up the established inbound call (#1)
  Self.ReceiveInvite;
  Check(Assigned(Self.Session), 'OnInboundCall didn''t fire, first INVITE');
  InboundSession := Self.Session;
  InboundSession.AddSessionListener(Self);
  InboundSession.AcceptCall('', '');
  Self.ReceiveAck;

  // Set up the unestablished inbound call (#2)
  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';
  Self.Invite.From.Tag       := Self.Invite.From.Tag + '1';
  Self.ReceiveInvite;

  // Set up the unestablished outbound call (#3)
  Sess := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Sess.AddSessionListener(Self);
  Sess.Send;
  Self.ReceiveTrying(Self.LastSentRequest);

  // Set up the established outbound call (#4)
  Sess := Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '');
  Sess.AddSessionListener(Self);
  Sess.Send;
  Self.ReceiveOk(Self.LastSentRequest);

  CheckEquals(4,
              Self.Core.SessionCount,
              'Session count');

  Self.Core.TerminateAllCalls;

  // This looks completely wrong, I know. However, we've sent a CANCEL to
  // terminate the not-yet-accepted INVITE we sent out. That session (#3) won't
  // end until we receive a 487 Request Terminated for that INVITE or we receive
  // 200 OK (in which case we send a BYE and immediately tear down the session),
  // or we time out (because the remote end was an RFC 2543 UAS).
  // cf. RFC 3261, section 9.1
  CheckEquals(1,
              Self.Core.SessionCount,
              'Session count after TerminateAllCalls');
end;
{
// I think this test exercises code that resulted from a misreading of RFC 3261,
// section 8.2.3
procedure TestTIdSipUserAgent.TestUnknownAcceptValue;
begin
  Self.Invite.AddHeader(AcceptHeader).Value := 'text/unsupportedtextvalue';

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;

  Self.CheckResponseSent('No response sent to INVITE');
  CheckEquals(SIPNotAcceptableClient,
              Self.LastSentResponse.StatusCode,
              'Inappropriate response');
  Check(Self.LastSentResponse.HasHeader(AcceptHeader),
        'Response missing Accept header');
  CheckEquals(Self.Core.AllowedContentTypes,
              Self.LastSentResponse.FirstHeader(AcceptHeader).Value,
              'Incorrect Accept header');
end;
}
procedure TestTIdSipUserAgent.TestUnmatchedAckGetsDropped;
var
  Ack:      TIdSipRequest;
  Listener: TIdSipTestTransactionUserListener;
begin
  Listener := TIdSipTestTransactionUserListener.Create;
  try
    Self.Core.AddListener(Listener);

    Self.MarkSentResponseCount;
    Ack := TIdSipRequest.Create;
    try
      Ack.Assign(Self.Invite);
      Ack.Method      := MethodAck;
      Ack.CSeq.Method := Ack.Method;

      Self.ReceiveRequest(Ack);
    finally
      Ack.Free;
    end;

    Check(Listener.DroppedUnmatchedMessage,
          'Unmatched ACK not dropped');
    Check(Listener.AbstractUserAgentParam = Self.Core,
          'UserAgent param of Session''s DroppedUnmatchedMessage notification wrong');
    CheckNoResponseSent('Sent a response to an unmatched ACK');
  finally
    Self.Core.RemoveListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestUnregisterFromWhenNotRegistered;
begin
  Self.MarkSentRequestCount;
  Self.Core.UnregisterFrom(Self.Destination.Address).Send;
  CheckRequestSent('No REGISTER sent');
  CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');
  CheckIsUnregister(Self.LastSentRequest);
end;

procedure TestTIdSipUserAgent.TestUnregisterFromWhenRegistered;
var
  RegisteredContact: TIdSipContactHeader;
begin
  RegisteredContact := TIdSipContactHeader.Create;
  try
    Self.MarkSentRequestCount;
    Self.Core.RegisterWith(Self.Destination.Address, Self.Core.From).Send;
    CheckRequestSent('No REGISTER sent');
    CheckEquals(MethodRegister, Self.LastSentRequest.Method, 'Unexpected request sent');

    RegisteredContact.Assign(Self.LastSentRequest.FirstContact);
    Self.ReceiveOk(Self.LastSentRequest);

    Self.MarkSentRequestCount;
    Self.Core.UnregisterFrom(Self.Destination.Address).Send;
    CheckRequestSent('No (un)REGISTER sent');
    CheckIsUnregister(Self.LastSentRequest);
    CheckEquals(RegisteredContact.Address.AsString,
                Self.LastSentRequest.FirstContact.Address.AsString,
                'Deregistration of unexpected Contact');
  finally
    RegisteredContact.Free;
  end;
end;

procedure TestTIdSipUserAgent.TestViaMatchesTransportParameter;
begin
  // Iterate over the registered transports? Or does
  // TIdSipTransport.TransportFor return the null transport instead?

  Self.Dispatcher.TransportType := UdpTransport;
  Self.Destination.Address.Transport := Self.Dispatcher.Transport.GetTransportType;
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  CheckEquals(Self.Dispatcher.Transport.GetTransportType,
              Self.LastSentRequest.LastHop.Transport,
              'Transport parameter = '
            + Self.Destination.Address.Transport);

  Self.Dispatcher.TransportType := TlsTransport;
  Self.Destination.Address.Transport := Self.Dispatcher.Transport.GetTransportType;
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  CheckEquals(Self.Dispatcher.Transport.GetTransportType,
              Self.LastSentRequest.LastHop.Transport,
              'Transport parameter = '
            + Self.Destination.Address.Transport);
end;

//******************************************************************************
//* TStackConfigurationTestCase                                                *
//******************************************************************************
//* TStackConfigurationTestCase Public methods *********************************

procedure TStackConfigurationTestCase.SetUp;
begin
  inherited SetUp;

  Self.Address       := '127.0.0.1';
  Self.Conf          := TIdSipStackConfigurator.Create;
  Self.Configuration := TStringList.Create;
  Self.Port          := 15060;
  Self.Timer         := TIdDebugTimerQueue.Create(true);
end;

procedure TStackConfigurationTestCase.TearDown;
begin
  Self.Configuration.Free;
  Self.Conf.Free;
  Self.Timer.Terminate;

  inherited TearDown;
end;

//******************************************************************************
//* TStackConfigurationTestCaseWithMockTransport                               *
//******************************************************************************
//* TStackConfigurationTestCaseWithMockTransport Public methods ****************

procedure TStackConfigurationTestCaseWithMockTransport.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(TcpTransport, TIdSipMockTCPTransport);
  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUDPTransport);
end;

procedure TStackConfigurationTestCaseWithMockTransport.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipStackConfigurator                                                *
//******************************************************************************
//* TestTIdSipStackConfigurator Public methods *********************************

procedure TestTIdSipStackConfigurator.SetUp;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(TcpTransport, TIdSipTCPTransport);
  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipUDPTransport);

  Self.NewRegistrarEvent := TSimpleEvent.Create;

  Self.Port := 5060;

  Self.NewRegistrar := TIdUDPServer.Create(nil);
  Self.NewRegistrar.DefaultPort   := Self.Port + 11000;
  Self.NewRegistrar.OnUDPRead     := Self.NoteReceiptOfPacketOldRegistrar;
  Self.NewRegistrar.ThreadedEvent := true;
  Self.NewRegistrar.Active        := true;

  Self.Server := TIdUDPServer.Create(nil);
  Self.Server.DefaultPort   := Self.Port + 10000;
  Self.Server.OnUDPRead     := Self.NoteReceiptOfPacket;
  Self.Server.ThreadedEvent := true;
  Self.Server.Active        := true;

  TIdSipEventPackageRegistry.RegisterEvent(TIdSipTargetDialogPackage);
  TIdSipEventPackageRegistry.RegisterEvent(TIdSipReferPackage);

  Self.ReceivedRequest := TIdSipRequest.Create;

  Self.NewRegistrarReceivedPacket := false;
  Self.ReceivedPacket             := false;
end;

procedure TestTIdSipStackConfigurator.TearDown;
begin
  TIdSipEventPackageRegistry.UnregisterEvent(TIdSipReferPackage);
  TIdSipEventPackageRegistry.UnregisterEvent(TIdSipTargetDialogPackage);

  Self.ReceivedRequest.Free;
  Self.Server.Free;
  Self.NewRegistrar.Free;
  Self.NewRegistrarEvent.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);

  inherited TearDown;
end;

procedure TestTIdSipStackConfigurator.CheckPortFree(Address: String;
                                                    Port: Cardinal;
                                                    Msg: String);
var
  Binding: TIdSocketHandle;
  Server:  TIdUDPServer;
  FailMsg: String;
begin
  FailMsg := 'Port ' + Address + ':' + IntToStr(Port) + ' is not free';
  if (Msg <> '') then
    FailMsg := Msg + ': ' + FailMsg;

  Server := TIdUDPServer.Create(nil);
  try
    Binding := Server.Bindings.Add;
    Binding.IP   := Address;
    Binding.Port := Port;

    try
      Server.Active := true;
    except
      on EIdCouldNotBindSocket do begin
        Fail(FailMsg);
      end;
    end;
  finally
    Server.Free;
  end;
end;

//* TestTIdSipStackConfigurator Private methods ********************************

function TestTIdSipStackConfigurator.ARecords: String;
begin

  // Dig would translate this data as
  // ;; QUERY SECTION:
  // ;;      paranoid.leo-ix.net, type = A, class = IN
  //
  // ;; ANSWER SECTION:
  // paranoid.leo-ix.net.    1H IN A         127.0.0.2
  // paranoid.leo-ix.net.    1H IN A         127.0.0.1
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             1H IN NS        ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // ns1.leo-ix.net.         1H IN A         127.0.0.1

  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$02#$00#$01#$00#$01#$08#$70#$61#$72
  + #$61#$6E#$6F#$69#$64#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74
  + #$00#$00#$01#$00#$01#$C0#$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00
  + #$04#$7F#$00#$00#$01#$C0#$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00
  + #$04#$7F#$00#$00#$02#$C0#$15#$00#$02#$00#$01#$00#$00#$0E#$10#$00
  + #$06#$03#$6E#$73#$31#$C0#$15#$C0#$51#$00#$01#$00#$01#$00#$00#$0E
  + #$10#$00#$04#$7F#$00#$00#$01;
end;

procedure TestTIdSipStackConfigurator.CheckAutoAddress(Address: TIdSipAddressHeader);
begin
  CheckEquals(UTF16LEToUTF8(GetFullUserName),
              Address.DisplayName,
              Address.Name + ': display-name');
  CheckEquals(UTF16LEToUTF8(GetUserName),
              Address.Address.Username,
              Address.Name + ': user-info');
  CheckEquals(LocalAddress,
              Address.Address.Host,
              Address.Name + ': host-info');
end;

procedure TestTIdSipStackConfigurator.CheckAutoFrom(UserAgent: TIdSipUserAgent);
begin
  Self.CheckAutoAddress(UserAgent.From);
end;

procedure TestTIdSipStackConfigurator.CheckEventPackageRegistered(UA: TIdSipUserAgent;
                                                                  PackageName: String);
var
  Module: TIdSipSubscribeModule;
begin
  Module := UA.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

  Check(Assigned(Module),
        'No Subscribe module attached to the UA');

  Check(Pos(PackageName, Module.AllowedEvents) > 0,
        '"' + PackageName + '" package not supported by the SubscribeModule');
end;

procedure TestTIdSipStackConfigurator.CheckLocalAddress(UA: TIdSipUserAgent; ExpectedLocalAddress, DestinationIP: String; Msg: String);
var
  Call:        TIdSipOutboundSession;
  Destination: TIdSipToHeader;
begin
  Destination := TIdSipToHeader.Create;
  try
    Destination.Address.Scheme := SipScheme;
    Destination.Address.Host   := DestinationIP;
    Call := UA.InviteModule.Call(UA.From, Destination, '', '');
    Call.Send;

    CheckEquals(ExpectedLocalAddress, Call.InitialRequest.FirstContact.Address.Host, Msg);
  finally
    Destination.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.CheckTCPServerNotOnPort(const Host: String;
                                                              Port: Cardinal;
                                                              const Msg: String);
var
  Client: TIdTcpClient;
begin
  try
    Client := TIdTcpClient.Create(nil);
    try
      Client.Host := Host;
      Client.Port := Port;
      Client.Connect;
      try
        Fail(Msg + ': Server running on ' + Host + ':' + IntToStr(Port));
      finally
        Client.Disconnect;
      end;
    finally
      Client.Free;
    end;
  except
    on EIdSocketError do;
  end;
end;

procedure TestTIdSipStackConfigurator.CheckUserAgentUsesGruu(Configuration: TStrings; Value: String; UsesGruu: Boolean);
var
  UA: TIdSipUserAgent;
begin
  UA := Self.CreateUserAgentWithUsesGruuDirective(Configuration, Value);
  try
    Check(UsesGruu = UA.UseGruu, 'UseGruu not set correctly');
  finally
    UA.Free;
  end;
end;

function TestTIdSipStackConfigurator.CreateUserAgentWithUsesGruuDirective(Configuration: TStrings; Value: String): TIdSipUserAgent;
var
  Conf: TStrings;
begin
  Conf := TStringList.Create;
  try
    Conf.AddStrings(Configuration);
    Conf.Add('UseGruu: ' + Value);

    Result := Self.Conf.CreateUserAgent(Conf, Self.Timer);
  finally
    Conf.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.CheckConfigurationInstanceIdAndRegister(ExtraDirectives: TStrings;
                                                                              InstanceID: String);
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('Registrar: sip:gw1.leo-ix.net');
  Self.Configuration.Add('UseGruu: true');
  Self.Configuration.AddStrings(ExtraDirectives);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(InstanceID, UA.InstanceID, 'Instance-ID');

    UA.StartAllTransports;
    Self.Timer.TriggerAllEventsOfType(TIdSipReregisterWait);

    Self.WaitForSignaled('Waiting for REGISTER');
    Check(Self.ReceivedPacket, 'No REGISTER sent to registrar');
    Check(Self.ReceivedRequest.FirstContact.HasParameter(SipInstanceParam),
          'REGISTER has no "sip.instance" parameter');
    CheckEquals(InstanceID, Self.ReceivedRequest.FirstContact.SipInstance, '"sip.instance" parameter');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.NoteReceiptOfPacket(Sender: TObject;
                                                          AData: TStream;
                                                          ABinding: TIdSocketHandle);
var
  Msg: TIdSipMessage;
begin
  Msg := TIdSipMessage.ReadMessageFrom(AData);
  try
    if Msg.IsRequest then
      Self.ReceivedRequest.Assign(Msg);
  finally
    Msg.Free;
  end;

  Self.ReceivedPacket := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipStackConfigurator.NoteReceiptOfPacketOldRegistrar(Sender: TObject;
                                                                      AData: TStream;
                                                                      ABinding: TIdSocketHandle);
begin
  Self.NewRegistrarReceivedPacket := true;
  Self.NewRegistrarEvent.SetEvent;
end;

procedure TestTIdSipStackConfigurator.ProvideAnswer(Sender: TObject;
                                                    AData: TStream;
                                                    ABinding: TIdSocketHandle);
var
  Answer:  String;
  ReplyID: String;
  S:       TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(AData, 0);

    ReplyID := Copy(S.DataString, 1, 2);
  finally
    S.Free;
  end;

  Answer := ReplyID + Self.ARecords;

  Self.Server.Send(ABinding.PeerIP,
                   ABinding.PeerPort,
                   Answer);

  Self.NoteReceiptOfPacket(Sender, AData, ABinding);
end;

procedure TestTIdSipStackConfigurator.SetBasicConfiguration(Configuration: TStrings);
begin
  Configuration.Clear;
  Configuration.Add('Contact: sip:unit121@anon.org');
  Configuration.Add('From: sip:case@fried-neurons.org');
  Configuration.Add('HostName: unit121.anon.org');
  Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Configuration.Add('Listen: TCP ' + Self.Address + ':' + IntToStr(Self.Port));
end;

//* TestTIdSipStackConfigurator Published methods ******************************

procedure TestTIdSipStackConfigurator.TestCreateUserAgentHandlesMultipleSpaces;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen  :     TCP 127.0.0.1:5060');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipTransportRegistry.TransportTypeFor(TcpTransport),
                UA.Dispatcher.Transports[0].ClassType,
                'Transport type');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentHandlesTabs;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen'#9':'#9'TCP 127.0.0.1:5060');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipTransportRegistry.TransportTypeFor(TcpTransport),
                UA.Dispatcher.Transports[0].ClassType,
                'Transport type');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentOnBusyPort;
var
  Server: TIdUDPServer;
  UA:     TIdSipUserAgent;
begin
  CheckNotEquals('127.0.0.1',
                 LocalAddress,
                 'You MUST have two IPs on this machine to complete this test!');

  CheckPortFree(LocalAddress,
                DefaultSipPort,
                'Close down all SIP UAs before running this test.');

  Server := TIdUDPServer.Create(nil);
  try
    with Server.Bindings.Add do begin
      Address := LocalAddress;
      Port    := DefaultSipPort
    end;
    Server.Active := true;

    Self.Configuration.Add(Format('Listen: UDP %s:%d', ['127.0.0.1', Server.Bindings[0].Port]));

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
    finally
      UA.Free;
    end;
  finally
    Server.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentRegisterDirectiveBeforeTransport;
var
  UA: TIdSipUserAgent;
begin
  // Any network actions (like registering) can only happen once we've
  // configured the Transport layer. Same goes for configuring the NameServer.
  Self.Configuration.Add('Register: sip:127.0.0.1:' + IntToStr(Self.Server.DefaultPort));
  Self.Configuration.Add('Listen: UDP 127.0.0.1:5060');
  Self.Configuration.Add('NameServer: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.StartAllTransports;
    Self.Timer.TriggerAllEventsOfType(TIdSipReregisterWait);
    Self.WaitForSignaled('Waiting for REGISTER');
    Check(Self.ReceivedPacket, 'No REGISTER received');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentReturnsSomething;
var
  UA: TIdSipUserAgent;
begin
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(Assigned(UA), 'CreateUserAgent didn''t return anything');
    Check(Assigned(UA.Dispatcher), 'Stack doesn''t have a Transaction layer');

    Check(Assigned(UA.Authenticator),
          'Transaction-User layer has no Authenticator');
    Check(Assigned(UA.Locator),
          'Transaction-User layer has no Locator');
    Check(Assigned(UA.RoutingTable),
          'Transaction-User layer has no RoutingTable');
    Check(Assigned(UA.Timer),
          'Transaction-User layer has no timer');
    Check(UA.Timer = UA.Dispatcher.Timer,
          'Transaction and Transaction-User layers have different timers');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentTransportHasMalformedPort;
const
  MalformedPort = 'aa';
begin
  Self.Configuration.Add('Listen: TCP ' + Self.Address + ':' + MalformedPort);

  try
    Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    Fail('Failed to bail out from a malformed port configuration');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithAuthentication;
const
  FooName = 'foo';
var
  UA: TIdSipUserAgent;
begin
  RegisterAuthenticationPolicy(FooName, TIdSipBasicAuthenticator);
  try
    Self.Configuration.Add('Authentication: ' + FooName);

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      CheckEquals(TIdSipBasicAuthenticator, UA.Authenticator.ClassType, 'Authentication directive ignored');
    finally
      UA.Free;
    end;
  finally
    UnregisterAuthenticationPolicy(FooName);
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithAutoFrom;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('From: AUTO');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.CheckAutoFrom(UA);
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithAutoTransport;
var
  Bindings: TIdSipLocations;
  UA:       TIdSipUserAgent;
begin
  CheckPortFree(LocalAddress,
                DefaultSipPort,
                'Close down all SIP UAs before running this test.');

  Self.Configuration.Add('Listen: UDP AUTO:5060');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.Dispatcher.Transports[0].Start;
    try
      Bindings := TIdSipLocations.Create;
      try
        UA.Dispatcher.LocalBindings(Bindings);

        CheckEquals(LocalAddress,
                    Bindings[0].IPAddress,
                    'Local NIC (or loopback) address not used');
      finally
        Bindings.Free;
      end;
    finally
      UA.Dispatcher.Transports[0].Stop;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithConserveConnections;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(ConserveConnectionsDirective + ': true');
  Self.Configuration.Add(ListenDirective + ': TCP 127.0.0.1:5060');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.Dispatcher.Transports[0].ConserveConnections, 'Transport not configured');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithFrom;
const
  DisplayName = 'Count Zero';
  FromUri     = 'sip:countzero@jammer.org';
  From        = '"' + DisplayName + '" <' + FromUri + '>';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('From: ' + From);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(DisplayName, UA.From.DisplayName,      'From display-name');
    CheckEquals(FromUri,     UA.From.Address.AsString, 'From URI');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithHostName;
const
  HostName = 'talking-head1.tessier-ashpool.co.luna';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('HostName: ' + HostName);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(HostName, UA.HostName, 'HostName');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithInstanceID;
const
  InstanceID = 'urn:uuid:12345678-1234-1234-1234-123456789012';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('InstanceID: ' + InstanceID);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(InstanceID, UA.InstanceID, 'Instance-ID');
    Check(UA.ModuleFor(MethodRefer).IsNull,
          'Setting the InstanceID doesn''t automatically/implicitly mean '
        + 'support for GRUU');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithInstanceIDThenRegister;
const
  InstanceID = 'urn:uuid:12345678-1234-1234-1234-123456789012';
var
  InstanceIDThenRegister: TStrings;
begin
  InstanceIDThenRegister := TStringList.Create;
  try
    InstanceIDThenRegister.Add('InstanceID: ' + InstanceID);
    InstanceIDThenRegister.Add('Register: sip:127.0.0.1:' + IntToStr(Self.Server.DefaultPort));

    Self.CheckConfigurationInstanceIdAndRegister(InstanceIDThenRegister, InstanceID);
  finally
    InstanceIDThenRegister.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithListeners;
var
  L1, L2: TIdSipTestTransactionUserListener;
  UA:     TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));

  L1 := TIdSipTestTransactionUserListener.Create;
  try
    L2 := TIdSipTestTransactionUserListener.Create;
    try
      UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer, [L1, L2]);
      UA.InviteModule.Call(UA.From, UA.From, '', '');

      Check(L1.ActionAdded, 'First listener not notified');
      Check(L2.ActionAdded, 'Second listener not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithLocator;
var
  UA: TIdSipUserAgent;
begin
  // This looks confusing. It isn't. We give the name server & port of Server,
  // and an unused port as the registrar. That's just because we don't care
  // about the REGISTER message - we just want to make sure the UA sends a DNS
  // query to the name server specified in the configuration.
  //
  // We also tell the stack to resolve names using only our nameserver so we
  // don't have to change the test machine's default nameserver.

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: 127.0.0.1:' + IntToStr(Self.Server.DefaultPort));
  Self.Configuration.Add('Register: sip:localhost:' + IntToStr(Self.Server.DefaultPort + 1));
  Self.Configuration.Add('ResolveNamesLocallyFirst: false');
  Self.Server.OnUDPRead := Self.ProvideAnswer;

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.StartAllTransports;
    Self.Timer.TriggerAllEventsUpToFirst(TIdSipReregisterWait);
    Check(Assigned(UA.Locator),
          'Transaction-User has no Locator');
    Self.WaitForSignaled('Waiting for DNS query');
    Check(Self.ReceivedPacket, 'No DNS query sent to name server');

    Check(Assigned(UA.Dispatcher.Locator),
          'No Locator assigned to the Transaction layer');
    Check(UA.Locator = UA.Dispatcher.Locator,
          'Transaction and Transaction-User layers have different Locators');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMalformedFrom;
const
  MalformedFromLine = '"Count Zero <sip:countzero@jammer.org>';
begin
  Self.Configuration.Add('From: ' + MalformedFromLine);

  try
    Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    Fail('Failed to bail out with malformed From');
  except
    on E: EParserError do
      Check(Pos(MalformedFromLine, E.Message) > 0,
            'Insufficient error message');
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMalformedLocator;
const
  MalformedNameServerLine = 'NameServer: 127.0.0.1:aa';
begin
  Self.Configuration.Add(MalformedNameServerLine);

  try
    Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    Fail('Failed to bail out with malformed locator port');
  except
    on E: EParserError do
      Check(Pos(MalformedNameServerLine, E.Message) > 0,
            'Insufficient error message');
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMalformedRoute;
const
  MalformedRouteLine = 'Route: sip://localhost'; // SIP URIs don't use "//"
begin
  Self.Configuration.Add(MalformedRouteLine);

  try
    Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    Fail('Failed to bail out with malformed Route header');
  except
    on E: EParserError do
      Check(Pos(MalformedRouteLine, E.Message) > 0,
            'Insufficient error message');
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMappedRoutes;
const
  InternetDestination = '5.6.7.8';
  InternetGateway     = '1.2.3.4';
  InternetMappedRoute = 'MappedRoute: 0.0.0.0/0.0.0.0 ' + InternetGateway + ' 15060';
  VpnDestination      = '192.168.0.2';
  VpnGateway          = '192.168.0.1';
  VpnMappedRoute      = 'MappedRoute: 192.168.0.0/24 ' + VpnGateway;
var
  LanDestination: String;
  LanIP:          String;
  UA:             TIdSipUserAgent;
begin
  // There's lots under test here: you can specify a mapped route with two kinds
  // of (address/mask)s. You can optionally specify a port.

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add(InternetMappedRoute);
  Self.Configuration.Add(VpnMappedRoute);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.Dispatcher.Transports[0].Start;
    LanIP := LocalAddress;
    LanDestination := TIdIPAddressParser.IncIPAddress(LanIP);

    CheckEquals(GetBestLocalAddress(LanDestination),
                UA.RoutingTable.LocalOrMappedAddressFor(LanDestination),
                'UA routing table not consulting OS');
    CheckLocalAddress(UA, InternetGateway, InternetDestination, 'Internet mapped route not used');
    CheckLocalAddress(UA, VpnGateway, VpnDestination, 'Vpn mapped route not used');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMockAuthenticator;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Authentication: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockAuthenticator.ClassName,
                UA.Authenticator.ClassName,
                'Authenticator type');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMockLocator;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('NameServer: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator.ClassName,
                UA.Locator.ClassName,
                'Locator type');
    Check(Assigned(UA.Dispatcher.Locator),
          'Transaction Dispatcher has no Locator');
    Check(UA.Locator = UA.Dispatcher.Locator,
          'Transaction User and Transaction layers don''t use the same Locator');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMockLocatorConfigured;
var
  Mock: TIdSipMockLocator;
  UA:   TIdSipUserAgent;
begin
  Self.Configuration.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator.ClassName,
                UA.Locator.ClassName,
                'Locator type');
    Mock := UA.Locator as TIdSipMockLocator;
    Check(Mock.ReturnOnlySpecifiedRecords, 'Mock locator not configured');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMultipleEventPackageSupport;
var
  Module: TIdSipSubscribeModule;
  UA:     TIdSipUserAgent;
begin
  TIdSipEventPackageRegistry.RegisterEvent(TIdSipReferPackage);
  TIdSipEventPackageRegistry.RegisterEvent(TIdSipTargetDialogPackage);
  try
    Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
    Self.Configuration.Add('SupportEvent: refer, target-dialog');

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      Module := UA.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

      Check(Assigned(Module),
            'No Subscribe module attached to the UA');

      CheckEventPackageRegistered(UA, TIdSipReferPackage.EventPackage);
      CheckEventPackageRegistered(UA, TIdSipTargetDialogPackage.EventPackage);
    finally
      UA.Free;
    end;
  finally
    TIdSipEventPackageRegistry.UnregisterEvent(TIdSipReferPackage);
    TIdSipEventPackageRegistry.UnregisterEvent(TIdSipTargetDialogPackage);
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithMultipleTransports;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port + 1));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(1,
                UA.Dispatcher.TransportCount,
                'Dispatcher didn''t use one transport with multiple bindings');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithNoAuthentication;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipNullAuthenticator, UA.Authenticator.ClassType, 'Default authenticator');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithNoFrom;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.CheckAutoFrom(UA);
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithOneTransport;
var
  Bindings: TIdSipLocations;
  UA:       TIdSipUserAgent;
begin
  Self.Port := 15060;
  Self.Configuration.Add('Listen: TCP ' + Self.Address + ':' + IntToStr(Self.Port));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Bindings := TIdSipLocations.Create;
    try
      UA.Dispatcher.LocalBindings(Bindings);
      CheckEquals(1, UA.Dispatcher.TransportCount, 'Number of transports');
    CheckEquals(TIdSipTransportRegistry.TransportTypeFor(TcpTransport),
                  UA.Dispatcher.Transports[0].ClassType,
                  'Transport type');
      CheckEquals(Port,
                  Bindings[0].Port,
                  'Transport port');
      CheckEquals(Self.Address,
                  Bindings[0].IPAddress,
                  'Transport address');
      CheckEquals(Self.Address,
                  UA.Dispatcher.Transports[0].HostName,
                  'Transport hostname');
      Check(Assigned(UA.Dispatcher.Transports[0].Timer),
            'Transport has no timer');
      Check(UA.Dispatcher.Timer = UA.Dispatcher.Transports[0].Timer,
            'Transport and Transaction layers have different timers');

      UA.Dispatcher.Transports[0].Start;
      try
        CheckTcpServerNotOnPort(Bindings[0].IPAddress,
                                DefaultSipPort,
                                'With only one listener (on a non-standard port) '
                              + 'there should be no server on the standard port');
      finally
        UA.Dispatcher.Transports[0].Stop;
      end;
    finally
     Bindings.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithReferSupport;
var
  Module: TIdSipSubscribeModule;
  UA:     TIdSipUserAgent;
begin
  TIdSipEventPackageRegistry.RegisterEvent(TIdSipReferPackage);
  try
    Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
    Self.Configuration.Add('SupportEvent: ' + PackageRefer);

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      Module := UA.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

      Check(Assigned(Module),
            'No Subscribe module attached to the UA');

      CheckEventPackageRegistered(UA, TIdSipReferPackage.EventPackage);
    finally
      UA.Free;
    end;
  finally
    TIdSipEventPackageRegistry.UnregisterEvent(TIdSipReferPackage);
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithRegisterAndInstanceID;
const
  InstanceID = 'urn:uuid:12345678-1234-1234-1234-123456789012';
var
  RegisterThenInstanceID: TStrings;
begin
  RegisterThenInstanceID := TStringList.Create;
  try
    RegisterThenInstanceID.Add('Register: sip:127.0.0.1:' + IntToStr(Self.Server.DefaultPort));
    RegisterThenInstanceID.Add('InstanceID: ' + InstanceID);

    Self.CheckConfigurationInstanceIdAndRegister(RegisterThenInstanceID, InstanceID);
  finally
    RegisterThenInstanceID.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithRegistrar;
const
  TwoSeconds = 2000; // milliseconds
var
  UA: TIdSipUserAgent;
begin
  Self.DefaultTimeout := TwoSeconds;

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('Register: sip:127.0.0.1:' + IntToStr(Self.Server.DefaultPort));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.StartAllTransports;
    Self.Timer.TriggerAllEventsOfType(TIdSipReregisterWait);
    Self.WaitForSignaled('Waiting for REGISTER');
    Check(Self.ReceivedPacket, 'No REGISTER sent to registrar');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithResolveNamesLocallyFirst;
var
  UA: TIdSipUserAgent;
begin
  // We check that, even though we specified our own name server, we make use of
  // the OS's name resolution services (i.e., gethostbyname or getaddrinfo and
  // the like) first. This test really only shows that the stack doesn't send a
  // query to the specified namserver: it won't fail if no DNS queries happen at
  // all (as in the case of using the /etc/hosts file), or because of a failed
  // attempt to contain the OS's specified name server.

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: 127.0.0.1:' + IntToStr(Self.Server.DefaultPort));
  Self.Configuration.Add('Register: sip:localhost:' + IntToStr(Self.Server.DefaultPort + 1));
  Self.Configuration.Add('ResolveNamesLocallyFirst: true');
  Self.Server.OnUDPRead := Self.ProvideAnswer;

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(Assigned(UA.Locator),
          'Transaction-User has no Locator');
    Self.WaitForTimeout('DNS query arrived');
    Check(not Self.ReceivedPacket, 'DNS queries not sent to OS''s name server');

    Check(Assigned(UA.Dispatcher.Locator),
          'No Locator assigned to the Transaction layer');
    Check(UA.Locator = UA.Dispatcher.Locator,
          'Transaction and Transaction-User layers have different Locators');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithoutResolveNamesLocallyFirst;
var
  UA: TIdSipUserAgent;
begin
  // This test demonstrates that the default behaviour of the stack is to use a
  // local name resolution strategy first.
  //
  // Also see the comment in Self.TestCreateUserAgentWithResolveNamesLocallyFirst.

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: 127.0.0.1:' + IntToStr(Self.Server.DefaultPort));
  Self.Configuration.Add('Register: sip:localhost:' + IntToStr(Self.Server.DefaultPort + 1));
  Self.Configuration.Add('ResolveNamesLocallyFirst: true');
  Self.Server.OnUDPRead := Self.ProvideAnswer;

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(Assigned(UA.Locator),
          'Transaction-User has no Locator');
    Self.WaitForTimeout('DNS query arrived');
    Check(not Self.ReceivedPacket, 'DNS queries not sent to OS''s name server');

    Check(Assigned(UA.Dispatcher.Locator),
          'No Locator assigned to the Transaction layer');
    Check(UA.Locator = UA.Dispatcher.Locator,
          'Transaction and Transaction-User layers have different Locators');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithSuppressLocalResponses;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(SuppressLocalResponsesDirective + ': true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.InviteModule.SuppressLocalResponses, SuppressLocalResponsesDirective + ' directive ignored');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithUnknownAuthentication;
const
  FooName = 'foo';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Authentication: ' + FooName);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipNullAuthenticator, UA.Authenticator.ClassType, 'Authentication directive with unknown policy');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithUseGruu;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));

  CheckUserAgentUsesGruu(Self.Configuration, 'true', true);
  CheckUserAgentUsesGruu(Self.Configuration, 'TRUE', true);
  CheckUserAgentUsesGruu(Self.Configuration, 'yes',  true);
  CheckUserAgentUsesGruu(Self.Configuration, 'YES',  true);
  CheckUserAgentUsesGruu(Self.Configuration, '1',    true);
  CheckUserAgentUsesGruu(Self.Configuration, 'on',   true);
  CheckUserAgentUsesGruu(Self.Configuration, 'ON',   true);

  CheckUserAgentUsesGruu(Self.Configuration, 'false', false);
  CheckUserAgentUsesGruu(Self.Configuration, 'FALSE', false);
  CheckUserAgentUsesGruu(Self.Configuration, 'no',    false);
  CheckUserAgentUsesGruu(Self.Configuration, 'NO',    false);
  CheckUserAgentUsesGruu(Self.Configuration, '0',     false);
  CheckUserAgentUsesGruu(Self.Configuration, 'off',   false);
  CheckUserAgentUsesGruu(Self.Configuration, 'OFF',   false);
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithUseTransport;
const
  DefaultTransport = TlsTransport;
  IPv6Target       = '::1';
  InternetTarget   = '1.2.3.4';
  LanAddressSpace  = '10.0.0.0/8';
  LanTarget        = '10.0.0.1';
  LanTransport     = SctpTransport;
  ZeroAddressSpace = '0.0.0.0/0';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add(Format('%s: %s %s', [UseTransportDirective, LanAddressSpace, LanTransport]));
  Self.Configuration.Add(Format('%s: %s %s', [UseTransportDirective, ZeroAddressSpace, DefaultTransport]));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(LanTransport,     UA.Dispatcher.PreferredTransportTypeFor(LanTarget),      'LAN preferences not configured');
    CheckEquals(DefaultTransport, UA.Dispatcher.PreferredTransportTypeFor(InternetTarget), 'Internet preferences not configured');
    CheckEquals('',               UA.Dispatcher.PreferredTransportTypeFor(IPv6Target),     'Other preferences not left alone');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithUseInboundConnections;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add(UseInboundConnectionsDirective + ': true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UseInboundConnections, 'UseInboundConnections directive ignored');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestCreateUserAgentWithUserAgentName;
const
  UserAgentName = 'FooBar/1.1';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('UserAgentName: ' + UserAgentName);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(UserAgentName, UA.UserAgentName, 'UserAgentName not set');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithFrom;
var
  NewConfig: TStrings;
  NewFrom:   String;
  OldFrom:   String;
  UA:        TIdSipUserAgent;
begin
  NewFrom := 'sip:case@jammers.org';

  Self.SetBasicConfiguration(Self.Configuration);
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    OldFrom := UA.From.FullValue;
    CheckNotEquals(NewFrom,
                   OldFrom,
                   'NewFrom contains the same From header as the original '
                 + 'configuration');

    NewConfig := TStringList.Create;
    try
      NewConfig.Add('From: ' + NewFrom);

      Self.Conf.UpdateConfiguration(UA, NewConfig);
      CheckEquals(NewFrom, UA.From.FullValue, 'UA''s From property not updated');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithLocator;
var
  NewConfig: TStrings;
  UA:        TIdSipUserAgent;
begin
  Self.SetBasicConfiguration(Self.Configuration);
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipIndyLocator.ClassName,
                UA.Locator.ClassName,
                'Sanity check: default locator type');

    NewConfig := TStringList.Create;
    try
      NewConfig.Add(NameServerDirective + ': ' + MockKeyword);

      Self.Conf.UpdateConfiguration(UA, NewConfig);
      CheckEquals(TIdSipMockLocator.ClassName,
                  UA.Locator.ClassName,
                  'Locator property not updated');
      Check(UA.Locator = UA.Dispatcher.Locator,
            'Transaction layer''s locator not updated');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithNewRegistrar;
var
  NewConfig: TStrings;
  Registrar: String;
  UA:        TIdSipUserAgent;
begin
  // Add a registrar setting to a UA that didn't have one before.

  Registrar := 'sip:127.0.0.1:' + IntToStr(Self.Server.DefaultPort);

  Self.SetBasicConfiguration(Self.Configuration);
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.StartAllTransports;
    NewConfig := TStringList.Create;
    try
      NewConfig.Add(RegisterDirective + ': ' + Registrar);

      Self.Conf.UpdateConfiguration(UA, NewConfig);

      Self.Timer.TriggerAllEventsOfType(TIdSipReregisterWait);

      Self.WaitForSignaled('Waiting for REGISTER');
      Check(Self.ReceivedPacket, 'No REGISTER sent to registrar');
      CheckEquals(Registrar, UA.RegisterModule.Registrar.AsString, 'Registrar not set');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithRegistrar;
var
  NewConfig:    TStrings;
  OldRegistrar: String;
  NewRegistrar: String;
  UA:           TIdSipUserAgent;
begin
  // Update an existing Registrar property.

  OldRegistrar := 'sip:127.0.0.2:' + IntToStr(Self.Server.DefaultPort);
  Self.SetBasicConfiguration(Self.Configuration);
  Self.Configuration.Add(RegisterDirective + ': ' + OldRegistrar);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    UA.StartAllTransports;
    Self.Timer.TriggerAllEventsUpToFirst(TIdSipReRegisterWait);
    // This has the side effect of resetting ThreadEvent.
    Self.WaitForSignaled('Waiting for REGISTER to old registrar');

    NewConfig := TStringList.Create;
    try
      NewRegistrar := 'sip:127.0.0.1:' + IntToStr(Self.NewRegistrar.DefaultPort);
      NewConfig.Add(RegisterDirective + ': ' + NewRegistrar);

      Self.Conf.UpdateConfiguration(UA, NewConfig);
      UA.StartAllTransports;
      Self.Timer.TriggerAllEventsUpToFirst(TIdSipReRegisterWait);
      Self.Timer.TriggerAllEventsUpToFirst(TIdSipActionSendWait);

      Self.WaitForSignaled('Waiting for unREGISTER to old registrar');
      Check(Self.ReceivedPacket, 'No unREGISTER sent to old registrar');

      CheckEquals(NewRegistrar, UA.RegisterModule.Registrar.AsString, 'Registrar not set');

      Self.WaitForSignaled(Self.NewRegistrarEvent, 'Waiting for REGISTER to new registrar');
      Check(Self.NewRegistrarReceivedPacket, 'No REGISTER sent to new registrar');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithSupportEvent;
var
  NewConfig: TStrings;
  NewEvent:  String;
  OldEvent:  String;
  SubMod:    TIdSipSubscribeModule;
  UA:        TIdSipUserAgent;
begin
  // This test demonstrates that you can add or remove support for event
  // packages through updating the UserAgent's configuration.

  OldEvent := TIdSipTargetDialogPackage.EventPackage;
  NewEvent := TIdSipReferPackage.EventPackage;

  Self.Configuration.Add('SupportEvent: ' + OldEvent);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipSubscribeModule), 'UA doesn''t use SubscribeModule');

    SubMod := UA.ModuleFor(TIdSipSubscribeModule) as TIdSipSubscribeModule;

    Check(Assigned(SubMod.Package(OldEvent)), 'Newly instantiated stack doesn''t support ' + OldEvent);

    NewConfig := TStringList.Create;
    try
      NewConfig.Add('SupportEvent: ' + NewEvent);

      Self.Conf.UpdateConfiguration(UA, NewConfig);

      Check(UA.UsesModule(TIdSipSubscribeModule), 'SubscribeModule removed');

      Check(not Assigned(SubMod.Package(OldEvent)), 'Support for ' + OldEvent + ' not removed');
      Check(not Assigned(SubMod.Package(OldEvent)), 'Support for ' + NewEvent + ' not added');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithBlankSupportEvent;
var
  NewConfig: TStrings;
  UA:        TIdSipUserAgent;
begin
  // This test demonstrates that if you remove support for all events, you DO
  // NOT remove the entire SubscribeModule. (To do so could cause
  // currently-running actions (say, a TIdSipOutboundReferral) to suddenly blow
  // up.

  Self.Configuration.Add('SupportEvent: ' + TIdSipTargetDialogPackage.EventPackage);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipSubscribeModule), 'UA doesn''t use SubscribeModule');

    NewConfig := TStringList.Create;
    try
      NewConfig.Add('SupportEvent: ');

      Self.Conf.UpdateConfiguration(UA, NewConfig);

      Check(UA.UsesModule(TIdSipSubscribeModule), 'SubscribeModule removed');
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithSuppressLocalResponses;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(SuppressLocalResponsesDirective + ': true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.Configuration.Delete(Self.Configuration.Count - 1);
    Self.Configuration.Add(SuppressLocalResponsesDirective + ': false');

    Self.Conf.UpdateConfiguration(UA, Self.Configuration);

    Check(not UA.InviteModule.SuppressLocalResponses, SuppressLocalResponsesDirective + ' directive ignored');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithTransport;
var
  Bindings:  TIdSipLocations;
  NewConfig: TStrings;
  NewPort:   Cardinal;
  UA:        TIdSipUserAgent;
begin
  NewPort := Self.Port + 1;

  Self.SetBasicConfiguration(Self.Configuration);
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    NewConfig := TStringList.Create;
    try
      NewConfig.Add('Listen: TCP ' + Self.Address + ':' + IntToStr(NewPort));

      Self.Conf.UpdateConfiguration(UA, NewConfig);

      Bindings := TIdSipLocations.Create;
      try
        UA.Dispatcher.LocalBindings(Bindings);

        CheckEquals(1,
                    UA.Dispatcher.TransportCount,
                    'Too many transports: old Listen directives still in force');
    CheckEquals(TIdSipTransportRegistry.TransportTypeFor(TcpTransport),
                    UA.Dispatcher.Transports[0].ClassType,
                    'New transport type');
        CheckEquals(NewPort,
                    Bindings[0].Port,
                    'New transport port');
        CheckEquals(Self.Address,
                    Bindings[0].IPAddress,
                    'New transport address');
        CheckEquals(Self.Address,
                    UA.Dispatcher.Transports[0].HostName,
                    'New transport hostname');
        Check(Assigned(UA.Dispatcher.Transports[0].Timer),
              'New transport has no timer');
        Check(UA.Dispatcher.Timer = UA.Dispatcher.Transports[0].Timer,
              'New transport and Transaction layers have different timers');
        CheckTCPServerNotOnPort(Self.Address, Self.Port, 'Old TCP transport still running');
      finally
        Bindings.Free;
      end;
    finally
      NewConfig.Free;
    end;
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithUseInboundConnections;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(UseInboundConnectionsDirective + ': true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.Configuration.Delete(Self.Configuration.Count - 1);
    Self.Configuration.Add(UseInboundConnectionsDirective + ': false');

    Self.Conf.UpdateConfiguration(UA, Self.Configuration);

    Check(not UA.UseInboundConnections, UseInboundConnectionsDirective + ' directive ignored');
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipStackConfigurator.TestUpdateConfigurationWithUseTransport;
const
  Transport = TcpTransport;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(Format('%s: %s %s', [UseTransportDirective, '127.0.0.0/8', Transport]));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.Configuration.Delete(Self.Configuration.Count - 1);
    Self.Configuration.Add(Format('%s: %s %s', [UseTransportDirective, '::/0', Transport]));

    Self.Conf.UpdateConfiguration(UA, Self.Configuration);

    CheckEquals('',        UA.Dispatcher.PreferredTransportTypeFor('127.0.0.1'), 'Old preferences not cleared');
    CheckEquals(Transport, UA.Dispatcher.PreferredTransportTypeFor('::1'),       'New preferences not set');
  finally
    UA.Free;
  end;
end;

//******************************************************************************
//* TestConfigureProxies                                                       *
//******************************************************************************

procedure TestConfigureProxies.SetUp;
begin
  inherited SetUp;

  Self.Configuration.Add('Listen: UDP AUTO:5060');
  Self.Configuration.Add('NameServer: MOCK');

  Self.EmptyRoutePath := TIdSipRoutePath.Create;
end;

procedure TestConfigureProxies.TearDown;
begin
  Self.EmptyRoutePath.Free;

  inherited TearDown;
end;

//* TestConfigureProxies Private methods ***************************************

procedure TestConfigureProxies.CheckEmptyRoutePath(RoutePath: TIdSipRoutePath; Msg: String);
begin
  Check(RoutePath.IsEmpty, Msg);
end;

procedure TestConfigureProxies.CheckEquals(Expected, Received: TIdSipRoutePath; Msg: String);
begin
  CheckEquals(Expected.Count, Received.Count, Msg + ': route path length');

  Expected.First;
  Received.First;
  while (Expected.HasNext) do begin
    CheckEquals(Expected.CurrentRoute.FullValue, Received.CurrentRoute.FullValue, Msg + ': unexpected Route');

    Expected.Next;
    Received.Next;
  end;
end;

procedure TestConfigureProxies.CheckRoutePath(Expected: TIdSipRoutePath;
                                              UA: TIdSipUserAgent;
                                              Target,
                                              Msg: String);
var
  Dest:      TIdSipToHeader;
  Transport: TIdSipMockTransport;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Value := 'sip:' + Target;

    UA.InviteModule.Call(UA.From, Dest, '', '').Send;
    Check(UA.Dispatcher.Transports.Count > 0, 'Insufficient transports?');

    Transport := UA.Dispatcher.Transports[0] as TIdSipMockTransport;
    Check(Transport.SentRequestCount > 0, 'No request sent');
    CheckEquals(Expected, Transport.LastRequest.Route, Msg);
  finally
    Dest.Free;
  end;
end;

//* TestConfigureProxies Published methods *************************************

procedure TestConfigureProxies.TestDefaultRoutePath;
const
  FirstProxy  = 'sip:first';
  SecondProxy = 'sip:second;lr';
var
  Expected: TIdSipRoutePath;
  UA:       TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <' + FirstProxy + '>');
  Self.Configuration.Add('Route: <' + SecondProxy + '>');

  Expected := TIdSipRoutePath.Create;
  try
    Expected.Add(RouteHeader).Value := '<' + FirstProxy + '>';
    Expected.Add(RouteHeader).Value := '<' + SecondProxy + '>';
    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      CheckEquals(Expected, UA.DefaultRoutePath, 'Default Route path');
      CheckRoutePath(Expected, UA, 'roke.local', 'a FQDN');
      CheckRoutePath(Expected, UA, '127.0.0.1', 'an IPv4 address');
    finally
      UA.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestConfigureProxies.TestNoProxyAsDefault;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <null>');
  Self.Configuration.Add('Route: <null>');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEmptyRoutePath(UA.DefaultRoutePath, 'Only null Route directives means no Route path');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureProxies.TestNoProxyToAddressSpace;
const
  Proxy = 'sip:proxy';
var
  Default: TIdSipRoutePath;
  UA:      TIdSipUserAgent;
begin
  // Check that one can configure the UA to NOT use a proxy to an address space.

  Self.Configuration.Add('Route: <null> 10.0.0.0/8');
  Self.Configuration.Add('Route: <' + Proxy + '>');

  Default := TIdSipRoutePath.Create;
  try
    Default.Add(RouteHeader).Value := '<' + Proxy + '>';

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      CheckRoutePath(Self.EmptyRoutePath, UA, '10.0.0.1', 'Directly reachable address space');
      CheckRoutePath(Default, UA, 'tessier-ashpool.co.luna', 'Default Route path');
    finally
      UA.Free;
    end;
  finally
    Default.Free;
  end;
end;

procedure TestConfigureProxies.TestNoRoutePaths;
var
  UA: TIdSipUserAgent;
begin
  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEmptyRoutePath(UA.DefaultRoutePath, 'No Route directives means no Route path');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureProxies.TestNullRoutesFirstInPathIgnored;
const
  AddressSpace = '10.0.0.0/8';
  FirstProxy   = 'sip:proxy.local';
  SecondProxy  = 'sip:10.0.0.1';
  Target       = 'roke.local';
var
  Expected: TIdSipRoutePath;
  UA:       TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <null> ' + AddressSpace);
  Self.Configuration.Add('Route: <' + FirstProxy + '> ' + AddressSpace);
  Self.Configuration.Add('Route: <' + SecondProxy + '> ' + AddressSpace);
  Self.Configuration.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
  Self.Configuration.Add('MockDns: A ' + Target + ' 10.0.0.2');

  Expected := TIdSipRoutePath.Create;
  try
    Expected.Add(RouteHeader).Value := '<' + FirstProxy + '>';
    Expected.Add(RouteHeader).Value := '<' + SecondProxy + '>';

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      CheckRoutePath(Expected, UA, Target, 'Null Route directives not ignored?');
    finally
      UA.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestConfigureProxies.TestNullRoutesInPathIgnored;
const
  AddressSpace = '10.0.0.0/8';
  FirstProxy   = 'sip:proxy.local';
  SecondProxy  = 'sip:10.0.0.1';
  Target       = 'roke.local';
var
  Expected: TIdSipRoutePath;
  UA:       TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <' + FirstProxy + '> ' + AddressSpace);
  Self.Configuration.Add('Route: <null> ' + AddressSpace);
  Self.Configuration.Add('Route: <' + SecondProxy + '> ' + AddressSpace);
  Self.Configuration.Add('NameServer: MOCK;ReturnOnlySpecifiedRecords');
  Self.Configuration.Add('MockDns: A ' + Target + ' 10.0.0.2');

  Expected := TIdSipRoutePath.Create;
  try
    Expected.Add(RouteHeader).Value := '<' + FirstProxy + '>';
    Expected.Add(RouteHeader).Value := '<' + SecondProxy + '>';

    UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    try
      CheckRoutePath(Expected, UA, Target, 'Null Route directives not ignored?');
    finally
      UA.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestConfigureProxies.TestReconfigureCanClearRoutePath;
const
  FirstAddressSpace  = '10.0.0.0/8';
  FirstProxy         = 'sip:proxy.local';
  LocalTarget        = 'roke.local';
  OtherNetTarget     = '192.168.1.1';
  SecondAddressSpace = '192.168.1.0/24';
  SecondProxy        = 'sip:10.0.0.1';
var
  Expected:  TIdSipRoutePath;
  ExpectedReconfigured: TIdSipRoutePath;
  UA:             TIdSipUserAgent;
begin
  Expected := TIdSipRoutePath.Create;
  try
    Expected.Add(RouteHeader).Value := '<' + FirstProxy + '>';

    ExpectedReconfigured := TIdSipRoutePath.Create;
    try
      ExpectedReconfigured.Add(RouteHeader).Value := '<' + SecondProxy + '>';

      Self.Configuration.Add('Route: <' + FirstProxy + '> ' + FirstAddressSpace);
      Self.Configuration.Add('NameServer: MOCK');
      Self.Configuration.Add('MockDns: A ' + LocalTarget + ' 10.0.0.2');

      UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
      try
        CheckRoutePath(Expected,             UA, LocalTarget,    'First address space');
        CheckRoutePath(Self.EmptyRoutePath,  UA, OtherNetTarget, 'Second address space');
        CheckRoutePath(Self.EmptyRoutePath,  UA, 'example.com',  'Default');

        Self.Configuration[Self.Configuration.Count - 1] := 'Route: <' + SecondProxy + '> ' + SecondAddressSpace;

        Self.Conf.UpdateConfiguration(UA, Self.Configuration);
        CheckRoutePath(Self.EmptyRoutePath,  UA, LocalTarget,    'Post-reconfiguration: First address space');
        CheckRoutePath(ExpectedReconfigured, UA, OtherNetTarget, 'Post-reconfiguration: Second address space');
        CheckRoutePath(Self.EmptyRoutePath,  UA, 'example.com',  'Post-reconfiguration: Default');
      finally
        UA.Free;
      end;
    finally
      ExpectedReconfigured.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestConfigureProxies.TestReconfigureWithNullRoute;
const
  FirstAddressSpace  = '10.0.0.0/8';
  FirstProxy         = 'sip:proxy.local';
  FirstTarget        = '10.0.0.2';
  SecondAddressSpace = '192.168.0.0/24';
  SecondProxy        = 'sip:10.0.0.1';
  SecondTarget       = '192.168.0.1';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <' + FirstProxy + '> ' + FirstAddressSpace);
  Self.Configuration.Add('Route: <' + SecondProxy + '> ' + SecondAddressSpace);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Self.Configuration.Clear;
    Self.Configuration.Add('Route: <null>');

    Self.Conf.UpdateConfiguration(UA, Self.Configuration);

    CheckRoutePath(Self.EmptyRoutePath, UA, FirstTarget,   'First address space not removed');
    CheckRoutePath(Self.EmptyRoutePath, UA, SecondTarget,  'Second address space not removed');
    CheckRoutePath(Self.EmptyRoutePath, UA, 'example.com', 'Default');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureProxies.TestTwoAddressSpaces;
const
  FirstAddressSpace  = '10.0.0.0/8';
  FirstProxy         = 'sip:proxy.local';
  FirstTarget        = '10.0.0.2';
  SecondAddressSpace = '192.168.0.0/24';
  SecondProxy        = 'sip:10.0.0.1';
  SecondTarget       = '192.168.0.1';
var
  ExpectedLocal:  TIdSipRoutePath;
  ExpectedSubnet: TIdSipRoutePath;
  UA:             TIdSipUserAgent;
begin
  Self.Configuration.Add('Route: <' + FirstProxy + '> ' + FirstAddressSpace);
  Self.Configuration.Add('Route: <' + SecondProxy + '> ' + SecondAddressSpace);

  ExpectedLocal := TIdSipRoutePath.Create;
  try
    ExpectedLocal.Add(RouteHeader).Value := '<' + FirstProxy + '>';

    ExpectedSubnet := TIdSipRoutePath.Create;
    try
      ExpectedSubnet.Add(RouteHeader).Value := '<' + SecondProxy + '>';

      UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
      try
        CheckRoutePath(ExpectedLocal,       UA, FirstTarget,   'First address space');
        CheckRoutePath(ExpectedSubnet,      UA, SecondTarget,  'Second address space');
        CheckRoutePath(Self.EmptyRoutePath, UA, 'example.com', 'Default');
      finally
        UA.Free;
      end;
    finally
      ExpectedSubnet.Free;
    end;
  finally
    ExpectedLocal.Free;
  end;
end;

//******************************************************************************
//* TestConfigureRegistrar                                                     *
//******************************************************************************
//* TestConfigureRegistrar Private methods *************************************

procedure TestConfigureRegistrar.CheckDatabaseType(ExpectedDatabase: TIdSipAbstractBindingDatabaseClass;
                                                   Configuration: TStrings);
var
  RegMod: TIdSipRegisterModule;
  UA:     TIdSipUserAgent;
begin
  UA := Self.Conf.CreateUserAgent(Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'Registrar module not instantiated');

    RegMod := UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;
    Check(Assigned(RegMod.BindingDB), 'Database not instantiated: directive ignored');

    CheckEquals(ExpectedDatabase.ClassName,
                (UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule).BindingDB.ClassName,
                'Database type');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.CheckMockDatabase(Configuration: TStrings);
begin
  CheckDatabaseType(TIdSipMockBindingDatabase, Configuration);
end;

procedure TestConfigureRegistrar.CheckUseGruu(ExpectedUseGruu: Boolean);
var
  RegMod: TIdSipRegisterModule;
  UA:     TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');
  Self.Configuration.Add('RegistrarUseGruu: ' + Self.BoolToStr(ExpectedUseGruu));

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'Registrar module not instantiated');

    RegMod := UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;
    CheckEquals(ExpectedUseGruu, RegMod.UseGruu, 'UseGruu directive not obeyed');
  finally
    UA.Free;
  end;
end;

//* TestConfigureRegistrar Published methods ***********************************

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveCanBeSwitchedOff;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'User agent can''t act as a registrar');

    Self.Configuration.Clear;
    Self.Configuration.Add(ActAsRegistrarDirective + ': false');

    Self.Conf.UpdateConfiguration(UA, Self.Configuration);

    Check(not UA.UsesModule(TIdSipRegisterModule), 'Register module not removed');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveCreatesRegistrarModule;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'User agent can''t act as a registrar');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveFalse;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: false');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(not UA.UsesModule(TIdSipRegisterModule), 'User agent acts as a registrar');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveFalseThenTrue;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: false');
  Self.Configuration.Add('ActAsRegistrar: true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'User agent can''t act as a registrar');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveParameters;
var
  Module: TIdSipRegisterModule;
  UA:     TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true;mintime=42');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'User agent doesn''t act as a registrar');

    Module := UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;
    CheckEquals(42, Module.MinimumExpiryTime, 'Directive parameters not passed to module');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestActAsRegistrarDirectiveTrueThenFalse;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');
  Self.Configuration.Add('ActAsRegistrar: false');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(not UA.UsesModule(TIdSipRegisterModule), 'User agent acts as a registrar');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestDatabaseDirectiveMock;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');
  Self.Configuration.Add('RegistrarDatabase: MOCK');

  Self.CheckMockDatabase(Self.Configuration);
end;

procedure TestConfigureRegistrar.TestDatabaseDirectiveMockWithMatchOnlyUsernameOption;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');
  Self.Configuration.Add('RegistrarDatabase: MOCK;MatchOnlyUsername');

  Self.CheckDatabaseType(TIdSipNameMatchingMockBindingDatabase, Self.Configuration);
end;

procedure TestConfigureRegistrar.TestDatabaseDirectiveUnknown;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');
  Self.Configuration.Add('RegistrarDatabase: MOCKMOCK');

  Self.CheckMockDatabase(Self.Configuration);
end;

procedure TestConfigureRegistrar.TestMisorderedDatabaseDirectiveMock;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('RegistrarDatabase: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');

  Self.CheckMockDatabase(Self.Configuration);
end;

procedure TestConfigureRegistrar.TestMisorderedUseGruuDirective;
var
  UA: TIdSipUserAgent;
begin
  // Check that User Agent creation doesn't blow up just because
  // RegistrarUseGruuDirective occurs before ActAsRegistrarDirective.

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('RegistrarUseGruu: true');
  Self.Configuration.Add('ActAsRegistrar: true');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'Registrar module not instantiated');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestNoRegisterDatabaseDirectiveUsesMockDatabase;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('ActAsRegistrar: true');

  Self.CheckMockDatabase(Self.Configuration);
end;

procedure TestConfigureRegistrar.TestDatabaseDirectiveImpliesActAsRegistrarDirective;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('RegistrarDatabase: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    Check(UA.UsesModule(TIdSipRegisterModule), 'Registrar module not instantiated');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureRegistrar.TestUseGruuDirective;
begin
  CheckUseGruu(true);
  CheckUseGruu(false);
end;

//******************************************************************************
//* TestConfigureMockRoutingTable                                              *
//******************************************************************************
//* TestConfigureMockRoutingTable Public methods *******************************

procedure TestConfigureMockRoutingTable.TestAddLocalAddress;
const
  LocalAddress = '192.168.1.42';
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add(RoutingTableDirective     + ': ' + MockKeyword);
  Self.Configuration.Add(MockLocalAddressDirective + ': ' + LocalAddress);
  Self.Configuration.Add(MockRouteDirective + ': 127.0.0.0/8 127.0.0.1 1 1 127.0.0.1');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdMockRoutingTable, UA.RoutingTable.ClassType, 'RoutingTable type');

    CheckEquals(Localhost(TIdIPAddressParser.IPVersion(LocalAddress)),
                UA.RoutingTable.LocalOrMappedAddressFor(LocalAddress),
                'Local address not stored in the routing table');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockRoutingTable.TestMockRoute;
var
  DefaultRoute:    String;
  InternetAddress: String;
  LanRoute:        String;
  LocalAddress:    String;
  RT:              TIdMockRoutingTable;
  UA:              TIdSipUserAgent;
begin
//  TIdIPAddressParser.NetworkFor(Self.Address, 24)
  LocalAddress    := '10.0.0.6';
  InternetAddress := '1.2.3.4';
  LanRoute        := '10.0.0.0/24 10.0.0.1 1 1 ' + LocalAddress;
  DefaultRoute    := '0.0.0.0/0 10.0.0.1 1 1 ' + LocalAddress;
  Self.Configuration.Add('Listen: UDP ' + LocalAddress + ':' + IntToStr(Port));
  Self.Configuration.Add('RoutingTable: MOCK');
  Self.Configuration.Add('MockRoute: ' + LanRoute);
  Self.Configuration.Add('MockRoute: ' + DefaultRoute);

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    RT := UA.RoutingTable as TIdMockRoutingTable;

    CheckEquals(2, RT.OsRouteCount, 'Not all routes added');

    CheckEquals(LocalAddress,
                RT.LocalOrMappedAddressFor(TIdIPAddressParser.IncIPAddress(Self.Address)),
                'Local address not set for LAN route');
    CheckEquals(LocalAddress,
                RT.LocalOrMappedAddressFor(InternetAddress),
                'Local address not set for default route');

    RT.RemoveOsRoute('10.0.0.0', '255.255.255.0', '10.0.0.1');
    CheckEquals(1, RT.OsRouteCount, 'LAN route not added');
    RT.RemoveOsRoute('0.0.0.0', '0.0.0.0', '10.0.0.1');
    CheckEquals(0, RT.RouteCount, 'Default route not added');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockRoutingTable.TestMockRoutingTable;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('RoutingTable: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdMockRoutingTable.ClassName,
                UA.RoutingTable.ClassName,
                'RoutingTable type');
    Check(Assigned(UA.Dispatcher.RoutingTable),
          'Transaction Dispatcher has no RoutingTable');
    Check(UA.RoutingTable = UA.Dispatcher.RoutingTable,
          'Transaction User and Transaction layers don''t use the same RoutingTable');
  finally
    UA.Free;
  end;
end;

//******************************************************************************
//* TestConfigureMockLocator                                                   *
//******************************************************************************
//* TestConfigureMockLocator Public methods ************************************

procedure TestConfigureMockLocator.SetUp;
begin
  inherited SetUp;

  Self.NameRecords := TIdDomainNameRecords.Create;
  Self.Naptr       := TIdNaptrRecords.Create;
  Self.SRVs        := TIdSrvRecords.Create;
  Self.Target      := TIdSipUri.Create('');
end;

procedure TestConfigureMockLocator.TearDown;
begin
  Self.Target.Free;
  Self.SRVs.Free;
  Self.Naptr.Free;
  Self.NameRecords.Free;

  inherited TearDown;
end;

//* TestConfigureMockLocator Private methods ***********************************

procedure TestConfigureMockLocator.CheckARecord(ExpectedIP,
                                                HostName: String;
                                                UserAgent: TIdSipUserAgent;
                                                Records: TIdDomainNameRecords);
begin
  UserAgent.Locator.ResolveNameRecords(HostName, Records);
  CheckEquals(1, Records.Count, 'Unexpected number of results for "' + HostName + '"');
  CheckEquals(ExpectedIP, Records[0].IPAddress, 'Wrong IP address for "' + HostName + '"');
end;

procedure TestConfigureMockLocator.CheckMalformedRecord(Line, ErrorMsg: String);
begin
  Self.Configuration.Clear;
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add(Line);

  try
    Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
    Fail('Failed to bail out with: ' + ErrorMsg);
  except
    on E: EParserError do
      Check(Pos(Line, E.Message) > 0,
            'Insufficient error message');
  end;
end;

//* TestConfigureMockLocator Published methods *********************************

procedure TestConfigureMockLocator.TestALineBeforeLocatorLine;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('MockDns: A localhost 127.0.0.1');
  Self.Configuration.Add('NameServer: MOCK');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator, UA.Locator.ClassType, 'Unexpected locator type');

    CheckARecord('127.0.0.1', 'localhost', UA, Self.NameRecords);
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockLocator.TestAddARecord;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('MockDns: A localhost 127.0.0.1');
  Self.Configuration.Add('MockDns: A tessier-ashpool.co.luna 1.2.3.4');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator, UA.Locator.ClassType, 'Unexpected locator type');

    CheckARecord('127.0.0.1', 'localhost', UA, Self.NameRecords);

    Self.NameRecords.Clear;
    CheckARecord('1.2.3.4', 'tessier-ashpool.co.luna', UA, Self.NameRecords);
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockLocator.TestAddARecordMalformedDomainName;
begin
  CheckMalformedRecord('MockDns: A 1foo 127.0.0.1', 'Malformed domain name');
end;

procedure TestConfigureMockLocator.TestAddARecordMalformedIPAddress;
begin
  CheckMalformedRecord('MockDns: A foo.com ::1', 'Malformed IPv4 address');
end;

procedure TestConfigureMockLocator.TestAddAAAARecord;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('MockDns: AAAA tessier-ashpool.co.luna ::1');
  Self.Configuration.Add('MockDns: AAAA tessier-ashpool.co.luna ::2');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator, UA.Locator.ClassType, 'Unexpected locator type');

    UA.Locator.ResolveNameRecords('tessier-ashpool.co.luna', Self.NameRecords);
    CheckEquals(2,     Self.NameRecords.Count,        'Unexpected number of results for "tessier-ashpool.co.luna"');
    CheckEquals('::1', Self.NameRecords[0].IPAddress, 'Wrong first IP address for "tessier-ashpool.co.luna"');
    CheckEquals('::2', Self.NameRecords[1].IPAddress, 'Wrong second IP address for "tessier-ashpool.co.luna"');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockLocator.TestAddNAPTRRecord;
var
  UA: TIdSipUserAgent;
begin
  Self.Target.Uri := 'sip:foo';

  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('MockDns: NAPTR foo 1 2 "" "SIP+D2T" "" bar');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator, UA.Locator.ClassType, 'Unexpected locator type');

    UA.Locator.ResolveNAPTR(Self.Target, Self.Naptr);
    CheckEquals(1, Self.Naptr.Count, 'Unexpected record count');
    CheckEquals('foo',     Self.Naptr[0].Key,         'Key');
    CheckEquals(1,         Self.Naptr[0].Order,       'Order');
    CheckEquals(2,         Self.Naptr[0].Preference,  'Preference');
    CheckEquals('',        Self.Naptr[0].Flags,       'Flags');
    CheckEquals('SIP+D2T', Self.Naptr[0].Service,     'Service');
    CheckEquals('bar',     Self.Naptr[0].Value,       'Value');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockLocator.TestAddNAPTRRecordMalformedKey;
begin
  // First token must be a domain name.
  CheckMalformedRecord('MockDns: NAPTR 1',
                       'Malformed NAPTR key');
end;

procedure TestConfigureMockLocator.TestAddNAPTRRecordMalformedOrder;
begin
  // Second token must be a (positive) integer.
  CheckMalformedRecord('MockDns: NAPTR foo a',
                       'Malformed NAPTR order');
end;

procedure TestConfigureMockLocator.TestAddNAPTRRecordMalformedPreference;
begin
  // Third token must be a (positive) integer.
  CheckMalformedRecord('MockDns: NAPTR foo 0 a',
                       'Malformed NAPTR preference');
end;

procedure TestConfigureMockLocator.TestAddNAPTRRecordMissingFlags;
begin
  CheckMalformedRecord('MockDns: NAPTR foo 0 0', 'Missing flags (and everything else)');
  CheckMalformedRecord('MockDns: NAPTR foo 0 0 "SIP+D2T" "" bar', 'Missing flags');
end;

procedure TestConfigureMockLocator.TestAddSRVRecord;
var
  UA: TIdSipUserAgent;
begin
  Self.Configuration.Add('Listen: UDP ' + Self.Address + ':' + IntToStr(Self.Port));
  Self.Configuration.Add('NameServer: MOCK');
  Self.Configuration.Add('MockDns: SRV _sip._tcp tessier-ashpool.co.luna 1 2 5060 proxy.tessier-ashpool.co.luna');

  UA := Self.Conf.CreateUserAgent(Self.Configuration, Self.Timer);
  try
    CheckEquals(TIdSipMockLocator, UA.Locator.ClassType, 'Unexpected locator type');

    UA.Locator.ResolveSRV('_sip._tcp.tessier-ashpool.co.luna', Self.SRVs);
    CheckEquals(1,                               Self.SRVs.Count,       'Unexpected number of SRV records');
    CheckEquals(1,                               Self.SRVs[0].Priority, 'Priority');
    CheckEquals(2,                               Self.SRVs[0].Weight,   'Weight');
    CheckEquals(5060,                            Self.SRVs[0].Port,     'Port');
    CheckEquals('proxy.tessier-ashpool.co.luna', Self.SRVs[0].Target,   'Target');
  finally
    UA.Free;
  end;
end;

procedure TestConfigureMockLocator.TestAddSRVRecordMalformedPort;
begin
  CheckMalformedRecord('MockDns: SRV _sip._tcp tessier-ashpool.co.luna 1 2 506a proxy.tessier-ashpool.co.luna',
                       'Malformed port');
end;

procedure TestConfigureMockLocator.TestAddSRVRecordMalformedPriority;
begin
  CheckMalformedRecord('MockDns: SRV _sip._tcp tessier-ashpool.co.luna a 2 5060 proxy.tessier-ashpool.co.luna',
                       'Malformed priority');
end;

procedure TestConfigureMockLocator.TestAddSRVRecordMalformedWeight;
begin
  CheckMalformedRecord('MockDns: SRV _sip._tcp tessier-ashpool.co.luna 1 a 5060 proxy.tessier-ashpool.co.luna',
                       'Malformed weight');
end;

//******************************************************************************
//* TestTIdSipReconfigureStackWait                                             *
//******************************************************************************
//* TestTIdSipReconfigureStackWait Public methods ******************************

procedure TestTIdSipReconfigureStackWait.SetUp;
begin
  inherited SetUp;

  Self.NewRoute := 'sip:gw1.leo-ix.net';
  Self.OldRoute := 'sip:proxy.tessier-ashpool.co.luna';

  Self.Configuration := TStringList.Create;
  Self.Configuration.Add(RouteHeaderDirective + ': <' + Self.NewRoute + '>');

  Self.Timer := TIdDebugTimerQueue.Create(true);
  Self.Stack := Self.CreateUserAgent(Self.Timer);

  Self.Wait := TIdSipReconfigureStackWait.Create;
  Self.Wait.Configuration := Self.Configuration;
  Self.Wait.UserAgentID   := Self.Stack.ID;
end;

procedure TestTIdSipReconfigureStackWait.TearDown;
begin
  Self.Wait.Free;
  Self.Stack.Free;
  Self.Timer.Terminate;
  Self.Configuration.Free;

  inherited TearDown;
end;

//* TestTIdSipReconfigureStackWait Published methods ***************************

procedure TestTIdSipReconfigureStackWait.CheckTriggerDoesNothing(Msg: String);
begin
  Self.Stack.DefaultRoutePath.First;
  CheckEquals(Self.OldRoute,
              Self.Stack.DefaultRoutePath.CurrentRoute.Address.Uri,
              Msg);
end;

function TestTIdSipReconfigureStackWait.CreateUserAgent(Context: TIdTimerQueue): TIdSipUserAgent;
var
  Conf: TIdSipStackConfigurator;
  Configuration: TStrings;
begin
  Conf := TIdSipStackConfigurator.Create;
  try
    Configuration := TStringList.Create;
    try
      Configuration.Add(RouteHeaderDirective + ': <' + Self.OldRoute + '>');

      Result := Conf.CreateUserAgent(Configuration, Self.Timer);
    finally
      Configuration.Free;
    end;
  finally
    Conf.Free;
  end;
end;

//* TestTIdSipReconfigureStackWait Published methods ***************************

procedure TestTIdSipReconfigureStackWait.TestTrigger;
begin
  Self.Wait.Trigger;

  Self.Stack.DefaultRoutePath.First;
  CheckEquals(Self.NewRoute,
              Self.Stack.DefaultRoutePath.CurrentRoute.Address.Uri,
              'Route not changed, ergo Wait didn''t trigger');
end;

procedure TestTIdSipReconfigureStackWait.TestTriggerWithInappropriateObjectReference;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.UserAgentID := R.ID;
    Self.Wait.Trigger;
    CheckTriggerDoesNothing('Wait triggered when given the ID of a non-UserAgent object');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipReconfigureStackWait.TestTriggerWithUnregisteredObjectID;
begin
  Self.Wait.UserAgentID := 'fake ID';
  Self.Wait.Trigger;
  CheckTriggerDoesNothing('Wait triggered when given a non-existent ID');
end;

//******************************************************************************
//* TestTIdSipPendingRegistration                                              *
//******************************************************************************
//* TestTIdSipPendingRegistration Public methods *******************************

procedure TestTIdSipPendingRegistration.SetUp;
begin
  inherited SetUp;

  Self.DebugTimer.TriggerImmediateEvents := false;

  Self.Core.RegisterModule.Registrar.Uri := 'sip:gw1.leo-ix.net';

  Self.Pending := TIdSipPendingRegistration.Create(Self.Core, Self.Core.RegisterModule.Registrar);
end;

procedure TestTIdSipPendingRegistration.TearDown;
begin
  Self.Pending.Free;

  inherited TearDown;
end;

//* TestTIdSipPendingRegistration Published methods ****************************

procedure TestTIdSipPendingRegistration.TestExecuteSchedulesWait;
begin
  Self.MarkSentRequestCount;
  Self.Pending.Execute;
  Self.DebugTimer.TriggerAllEventsUpToFirst(TIdSipReregisterWait);
  Self.CheckRequestSent('No REGISTER sent');
  CheckEquals(MethodRegister,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipPendingRegistration.TestRegisterContainsUserAgentsFrom;
begin
  Self.MarkSentRequestCount;
  Self.Pending.Execute;
  Self.DebugTimer.TriggerAllEventsUpToFirst(TIdSipReregisterWait);
  Self.CheckRequestSent('No REGISTER sent');

  CheckEquals(Self.Core.From.Address.AsString, Self.LastSentRequest.From.Address.AsString,
              'From header isn''t the UserAgent''s From');
end;

//******************************************************************************
//* TestTIdSipPendingTcpTransportConfiguration                                 *
//******************************************************************************
//* TestTIdSipPendingTcpTransportConfiguration Public methods ******************

procedure TestTIdSipPendingTcpTransportConfiguration.SetUp;
begin
  inherited SetUp;

  Self.Pending := TIdSipPendingTcpTransportConfiguration.Create(Self.Core);
end;

procedure TestTIdSipPendingTcpTransportConfiguration.TearDown;
begin
  Self.Pending.Free;

  inherited TearDown;
end;

//* TestTIdSipPendingTcpTransportConfiguration Published methods ***************

procedure TestTIdSipPendingTcpTransportConfiguration.TestExecuteSchedulesWait;
var
  I: Integer;
begin
  Self.Pending.ConserveConnections := true;

  Self.Pending.Execute;

  for I := 0 to Self.Core.Dispatcher.TransportCount - 1 do
    Check(Self.Core.Dispatcher.Transports[I].ConserveConnections,
          'Transport not configured; pending action didn''t execute');

  Self.Pending.ConserveConnections := false;

  Self.Pending.Execute;

  for I := 0 to Self.Core.Dispatcher.TransportCount - 1 do
    Check(not Self.Core.Dispatcher.Transports[I].ConserveConnections,
          'Transport not reconfigured; pending action didn''t execute');
end;

initialization
  RegisterTest('User Agent tests', Suite);
end.
