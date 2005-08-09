{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipCore;

interface

uses
  IdObservable, IdSipCore, IdSipDialog, IdSipMessage,
  IdSipMockTransactionDispatcher, IdSipTransport, IdSipUserAgent,
  TestFramework, TestFrameworkSip, TestFrameworkSipTU;

type
  TestTIdSipAbstractCore = class(TTestCaseTU)
  private
    ScheduledEventFired: Boolean;

    procedure ScheduledEvent(Sender: TObject);
  public
    procedure SetUp; override;
  published
    procedure TestScheduleEvent;
  end;

  TIdSipNullAction = class(TIdSipAction)
  protected
    function CreateNewAttempt: TIdSipRequest; override;
  public
    class function Method: String; override;
  end;

  TestTIdSipActions = class(TTestCaseTU)
  private
    ActionProcUsed:      String;
    Actions:             TIdSipActions;
    DidntFindActionName: String;
    FoundActionName:     String;
    Options:             TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestActionCount;
    procedure TestAddActionNotifiesObservers;
    procedure TestAddObserver;
    procedure TestCleanOutTerminatedActions;
    procedure TestFindActionAndPerformBlock;
    procedure TestFindActionAndPerformBlockNoActions;
    procedure TestFindActionAndPerformBlockNoMatch;
    procedure TestFindActionAndPerformOrBlock;
    procedure TestFindActionAndPerformOrBlockNoMatch;
    procedure TestInviteCount;
    procedure TestRemoveObserver;
    procedure TestTerminateAllActions;
  end;

  // These tests exercise the SIP discovery algorithms as defined in RFC 3263.
  TestLocation = class(TTestCaseTU,
                       IIdSipActionListener,
                       IIdSipInviteListener)
  private
    InviteOffer:    String;
    InviteMimeType: String;
    NetworkFailure: Boolean;
    TransportParam: String;

    function  CreateAction: TIdSipOutboundInitialInvite;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  public
    procedure SetUp; override;
  published
    procedure TestAllLocationsFail;
    procedure TestLooseRoutingProxy;
    procedure TestStrictRoutingProxy;
    procedure TestUseCorrectTransport;
    procedure TestUseTransportParam;
    procedure TestUseUdpByDefault;
    procedure TestVeryLargeMessagesUseAReliableTransport;
  end;

  TestTIdSipInboundOptions = class(TestTIdSipAction)
  private
    Options: TIdSipInboundOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestOptions;
    procedure TestOptionsWhenDoNotDisturb;
  end;

  TestTIdSipOutboundOptions = class(TestTIdSipAction,
                                    IIdSipOptionsListener)
  private
    ReceivedResponse: Boolean;

    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestIsOptions; override;
    procedure TestReceiveResponse;
    procedure TestRemoveListener;
  end;

  TestTIdSipOptionsResponseMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipOptionsResponseMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUserAgentAuthenticationChallengeMethod = class(TTestCase)
  private
    Challenge: TIdSipResponse;
    L1:        TIdSipTestTransactionUserListener;
    L2:        TIdSipTestTransactionUserListener;
    Method:    TIdSipUserAgentAuthenticationChallengeMethod;
    UserAgent: TIdSipAbstractUserAgent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFirstListenerDoesntSetPassword;
    procedure TestFirstListenerSetsPassword;
    procedure TestFirstListenerDoesntSetUsername;
    procedure TestFirstListenerSetsUsername;
    procedure TestNoListenerSetsPassword;
    procedure TestRun;
    procedure TestTryAgain;
  end;

  TestTIdSipUserAgentDroppedUnmatchedMessageMethod = class(TTestCase)
  private
    Method:   TIdSipUserAgentDroppedUnmatchedMessageMethod;
    Receiver: TIdSipTransport;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUserAgentInboundCallMethod = class(TActionMethodTestCase)
  private
    Method:  TIdSipUserAgentInboundCallMethod;
    Request: TIdSipRequest;
    Session: TIdSipInboundSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  IdException, IdSdp, IdSipDns, IdSipLocator, IdSipMockTransport,
  IdSipRegistration, SysUtils;

const
  DefaultTimeout = 5000;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipActions.Suite);
  Result.AddTest(TestLocation.Suite);
  Result.AddTest(TestTIdSipInboundOptions.Suite);
  Result.AddTest(TestTIdSipOutboundOptions.Suite);
  Result.AddTest(TestTIdSipOptionsResponseMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentAuthenticationChallengeMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentDroppedUnmatchedMessageMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentInboundCallMethod.Suite);
end;

//******************************************************************************
//* TestTIdSipAbstractCore                                                     *
//******************************************************************************
//* TestTIdSipAbstractCore Public methods **************************************

procedure TestTIdSipAbstractCore.SetUp;
begin
  inherited SetUp;

  Self.ScheduledEventFired := false;
end;

//* TestTIdSipAbstractCore Private methods *************************************

procedure TestTIdSipAbstractCore.ScheduledEvent(Sender: TObject);
begin
  Self.ScheduledEventFired := true;
  Self.ThreadEvent.SetEvent;
end;


//* TestTIdSipAbstractCore Published methods ***********************************

procedure TestTIdSipAbstractCore.TestScheduleEvent;
var
  EventCount: Integer;
begin
  EventCount := Self.DebugTimer.EventCount;
  Self.Core.ScheduleEvent(Self.ScheduledEvent, 50, Self.Invite.Copy);
  Check(EventCount < DebugTimer.EventCount,
        'Event not scheduled');
end;

//******************************************************************************
//* TIdSipNullAction                                                           *
//******************************************************************************
//* TIdSipNullAction Public methods ********************************************

class function TIdSipNullAction.Method: String;
begin
  Result := '';
end;

//* TIdSipNullAction Protected methods *****************************************

function TIdSipNullAction.CreateNewAttempt: TIdSipRequest;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipActions                                                          *
//******************************************************************************
//* TestTIdSipActions Public methods *******************************************

procedure TestTIdSipActions.SetUp;
begin
  inherited SetUp;

  Self.Actions := TIdSipActions.Create;
  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.Invite);
  Self.Options.Method := MethodOptions;

  Self.ActionProcUsed      := '';
  Self.DidntFindActionName := 'DidntFindAction';
  Self.FoundActionName     := 'FoundActionName';
end;

procedure TestTIdSipActions.TearDown;
begin
  Self.Options.Free;
  Self.Actions.Free;

  inherited TearDown;
end;

//* TestTIdSipActions Published methods ****************************************

procedure TestTIdSipActions.TestActionCount;
var
  I: Integer;
begin
  for I := 1 to 5 do begin
    Self.Actions.Add(TIdSipNullAction.Create(Self.Core));
    CheckEquals(I, Self.Actions.Count, 'Action not added');
  end;
end;

procedure TestTIdSipActions.TestAddActionNotifiesObservers;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(L1);

    Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

    Check(L1.Changed, 'L1 not notified');
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);

      Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

      Check(L1.Changed, 'L1 not notified, thus not added');
      Check(L2.Changed, 'L2 not notified, thus not added');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestCleanOutTerminatedActions;
var
  A:           TIdSipAction;
  ActionCount: Integer;
  O:           TIdObserverListener;
begin
  A := TIdSipNullAction.Create(Self.Core);
  Self.Actions.Add(A);

  ActionCount := Self.Actions.Count;
  A.Terminate;

  O := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(O);

    Self.Actions.CleanOutTerminatedActions;

    Check(Self.Actions.Count < ActionCount,
          'Terminated action not destroyed');
    Check(O.Changed, 'Observers not notified of change');
  finally
    Self.Actions.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
begin
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  A := Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(A.InitialRequest, Finder);

    Check(Finder.Action = A, 'Wrong action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoActions;
var
  Finder: TIdSipActionFinder;
begin
  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(Self.Options, Finder);

    Check(not Assigned(Finder.Action), 'An action found in an empty list');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoMatch;
var
  Finder: TIdSipActionFinder;
begin
  Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(Self.Options, Finder);

    Check(not Assigned(Finder.Action), 'An action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
  Switch: TIdSipActionSwitch;
begin
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  A := Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(A.InitialRequest,
                                          Finder,
                                          Switch);

      Check(Assigned(Finder.Action), 'Didn''t find action');
      Check(not Switch.Executed, 'Alternative block executed');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlockNoMatch;
var
  Finder: TIdSipActionFinder;
  Switch: TIdSipActionSwitch;
begin
  Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(Self.Options,
                                          Finder,
                                          Switch);

      Check(not Assigned(Finder.Action), 'Found action');
      Check(Switch.Executed, 'Alternative block didn''t execute');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestInviteCount;
begin
  CheckEquals(0, Self.Actions.InviteCount, 'No messages received');

  Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE');

  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundInvite.Create(Self.Core));
  CheckEquals(2, Self.Actions.InviteCount, 'Two INVITEs, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundSession.Create(Self.Core));
  CheckEquals(2,
              Self.Actions.InviteCount,
              'Two INVITEs, one OPTIONS, and a Session');
end;

procedure TestTIdSipActions.TestRemoveObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);
      Self.Actions.RemoveObserver(L1);

      Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

      Check(not L1.Changed, 'L1 notified, thus not removed');
      Check(L2.Changed, 'L2 not notified, thus not added');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestTerminateAllActions;
begin
  // We don't add INVITEs here because INVITEs need additional events to
  // properly terminate: an INVITE needs to wait for a final response, etc.
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  Self.Actions.Add(TIdSipOutboundRegistrationQuery.Create(Self.Core));
  Self.Actions.Add(TIdSipOutboundRegister.Create(Self.Core));

  Self.Actions.TerminateAllActions;
  Self.Actions.CleanOutTerminatedActions;
  CheckEquals(0,
              Self.Actions.Count,
              'Actions container didn''t terminate all actions');
end;

//******************************************************************************
//* TestLocation                                                               *
//******************************************************************************
//* TestLocation Public methods ************************************************

procedure TestLocation.SetUp;
begin
  inherited SetUp;

  Self.InviteMimeType := '';
  Self.InviteOffer    := '';
  Self.NetworkFailure := false;
  Self.TransportParam := SctpTransport;
end;

//* TestLocation Private methods ***********************************************

function TestLocation.CreateAction: TIdSipOutboundInitialInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Result.Destination := Self.Destination;
  Result.MimeType    := Self.InviteMimeType;
  Result.Offer       := Self.InviteOffer;
  Result.AddListener(Self);
  Result.Send;
end;

procedure TestLocation.OnAuthenticationChallenge(Action: TIdSipAction;
                                                 Response: TIdSipResponse);
begin
end;

procedure TestLocation.OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                                      Response: TIdSipResponse);
begin
end;                                      

procedure TestLocation.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                 Response: TIdSipResponse;
                                 const Reason: String);
begin
end;

procedure TestLocation.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                           NewDialog: TIdSipDialog);
begin
end;

procedure TestLocation.OnNetworkFailure(Action: TIdSipAction;
                                        ErrorCode: Cardinal;
                                        const Reason: String);
begin
  Self.NetworkFailure := true;
end;

procedure TestLocation.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                  Redirect: TIdSipResponse);
begin
end;

procedure TestLocation.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                 Response: TIdSipResponse);
begin
end;

//* TestLocation Published methods *********************************************

procedure TestLocation.TestAllLocationsFail;
var
  Locations: TIdSipLocations;
begin
  // SRV records point to Self.Destination.Address.Host;
  // Self.Destination.Address.Host resolves to two A records.

  Self.Locator.AddSRV(Self.Destination.Address.Host,
                      SrvTcpPrefix,
                      0,
                      0,
                      5060,
                      Self.Destination.Address.Host);
  Self.Locator.AddA   (Self.Destination.Address.Host, '127.0.0.1');
  Self.Locator.AddAAAA(Self.Destination.Address.Host, '::1');

  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.MarkSentRequestCount;
  Self.CreateAction;

  Locations := TIdSipLocations.Create;
  try
    Self.Locator.FindServersFor(Self.Destination.Address, Locations);

    // Locations.Count >= 0, so the typecast is safe.
    CheckEquals(Self.RequestCount + Cardinal(Locations.Count),
                Self.SentRequestCount,
                'Number of requests sent');
  finally
    Locations.Free;
  end;

  Check(Self.NetworkFailure,
        'No notification of failure after all locations attempted');
end;

procedure TestLocation.TestLooseRoutingProxy;
const
  ProxyAAAARecord = '::1';
  ProxyHost       = 'gw1.leo-ix.net';
  ProxyTransport  = SctpTransport;
  ProxyUri        = 'sip:' + ProxyHost + ';lr';
var
  RequestUriTransport: String;
begin
  RequestUriTransport := Self.Invite.LastHop.Transport;

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  Self.Locator.AddSRV(ProxyHost, SrvSctpPrefix, 0, 0, 5060, ProxyHost);
  Self.Locator.AddAAAA(ProxyHost, ProxyAAAARecord);

  Self.Locator.AddSRV(Self.Destination.Address.Host, SrvTcpPrefix, 0, 0,
                      5060, Self.Destination.Address.Host);

  Self.Locator.AddA(Self.Destination.Address.Host, '127.0.0.1');

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(ProxyTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestStrictRoutingProxy;
const
  ProxyUri = 'sip:127.0.0.1;transport=' + TransportParamSCTP;
var
  RequestUriTransport: String;
begin
  RequestUriTransport := Self.Invite.LastHop.Transport;

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  Self.Destination.Address.Transport := TransportParamTCP;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(RequestUriTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestUseCorrectTransport;
const
  CorrectTransport = SctpTransport;
var
  Action: TIdSipAction;
  Domain: String;
begin
  Domain := Self.Destination.Address.Host;

  // NAPTR record points to SCTP SRV record whose target resolves to the A
  // record.
  Self.Locator.AddNAPTR(Domain, 0, 0, NaptrDefaultFlags, NaptrSctpService, SrvSctpPrefix + Domain);
  Self.Locator.AddSRV(Domain, SrvSctpPrefix, 0, 0, 5060, Domain);
  Self.Locator.AddSRV(Domain, SrvTcpPrefix,  1, 0, 5060, Domain);

  Self.MarkSentRequestCount;
  Action := Self.CreateAction;

  CheckRequestSent('No request sent');
  CheckEquals(CorrectTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Incorrect transport');
  Check(Self.LastSentRequest.Equals(Action.InitialRequest),
        'Action''s InitialRequest not updated to the latest attempt');
end;

procedure TestLocation.TestUseTransportParam;
begin
  Self.Destination.Address.Transport := Self.TransportParam;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(SctpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use transport param');
end;

procedure TestLocation.TestUseUdpByDefault;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(UdpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use UDP by default');
end;

procedure TestLocation.TestVeryLargeMessagesUseAReliableTransport;
begin
  Self.InviteOffer    := TIdSipTestResources.VeryLargeSDP('localhost');
  Self.InviteMimeType := SdpMimeType;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(TcpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use a reliable transport despite the large size '
            + 'of the message');
end;

//******************************************************************************
//* TestTIdSipInboundOptions                                                   *
//******************************************************************************
//* TestTIdSipInboundOptions Public methods ************************************

procedure TestTIdSipInboundOptions.SetUp;
begin
  inherited SetUp;

  Self.Invite.Method := MethodOptions;
  Self.Options := TIdSipInboundOptions.CreateInbound(Self.Core,
                                                     Self.Invite,
                                                     false);
end;

procedure TestTIdSipInboundOptions.TearDown;
begin
  Self.Options.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundOptions Published methods *********************************

procedure TestTIdSipInboundOptions.TestIsInbound;
begin
  Check(Self.Options.IsInbound,
        Self.Options.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundOptions.TestIsInvite;
begin
  Check(not Self.Options.IsInvite,
          Self.Options.ClassName + ' marked as a Invite');
end;

procedure TestTIdSipInboundOptions.TestIsOptions;
begin
  Check(Self.Options.IsOptions,
        Self.Options.ClassName + ' not marked as an Options');
end;

procedure TestTIdSipInboundOptions.TestIsRegistration;
begin
  Check(not Self.Options.IsRegistration,
        Self.Options.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipInboundOptions.TestIsSession;
begin
  Check(not Self.Options.IsSession,
        Self.Options.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundOptions.TestOptions;
var
  Response: TIdSipResponse;
begin
  Check(Self.SentResponseCount > 0,
        'No response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(AllowHeader),
        'No Allow header');
  CheckEquals(Self.Core.KnownMethods,
              Response.FirstHeader(AllowHeader).FullValue,
              'Allow header');

  Check(Response.HasHeader(AcceptHeader),
        'No Accept header');
  CheckEquals(Self.Core.AllowedContentTypes,
              Response.FirstHeader(AcceptHeader).FullValue,
              'Accept header');

  Check(Response.HasHeader(AcceptEncodingHeader),
        'No Accept-Encoding header');
  CheckEquals(Self.Core.AllowedEncodings,
              Response.FirstHeader(AcceptEncodingHeader).FullValue,
              'Accept-Encoding header');

  Check(Response.HasHeader(AcceptLanguageHeader),
        'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).FullValue,
              'Accept-Language header');

  Check(Response.HasHeader(SupportedHeaderFull),
        'No Supported header');
  CheckEquals(Self.Core.AllowedExtensions,
              Response.FirstHeader(SupportedHeaderFull).FullValue,
              'Supported header value');

  Check(Response.HasHeader(ContactHeaderFull),
        'No Contact header');
  Check(Self.Core.Contact.Equals(Response.FirstContact),
        'Contact header value');

  Check(Response.HasHeader(WarningHeader),
        'No Warning header');
  CheckEquals(Self.Core.Hostname,
              Response.FirstWarning.Agent,
              'Warning warn-agent');
end;

procedure TestTIdSipInboundOptions.TestOptionsWhenDoNotDisturb;
var
  NewOptions: TIdSipInboundOptions;
  Response:   TIdSipResponse;
begin
  Self.Core.DoNotDisturb := true;

  Self.MarkSentResponseCount;
  NewOptions := TIdSipInboundOptions.CreateInbound(Self.Core,
                                                   Self.Options.InitialRequest,
                                                   false);
  try
    CheckResponseSent('No response sent');

    Response := Self.LastSentResponse;
    CheckEquals(SIPTemporarilyUnavailable,
                Response.StatusCode,
                'Do Not Disturb');
  finally
    NewOptions.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundOptions                                                  *
//******************************************************************************
//* TestTIdSipOutboundOptions Public methods ***********************************

procedure TestTIdSipOutboundOptions.SetUp;
begin
  inherited SetUp;

  Self.ReceivedResponse := false;
end;

//* TestTIdSipOutboundOptions Protected methods ********************************

function TestTIdSipOutboundOptions.CreateAction: TIdSipAction;
var
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Destination);
  Options.AddListener(Self);
  Options.Send;
  Result := Options;
end;

//* TestTIdSipOutboundOptions Private methods **********************************

procedure TestTIdSipOutboundOptions.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                               Response: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

//* TestTIdSipOutboundOptions Published methods ********************************

procedure TestTIdSipOutboundOptions.TestAddListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response, 'L1 not informed of response');
      Check(L2.Response, 'L2 not informed of response');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundOptions.TestIsOptions;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(Action.IsOptions,
        Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipOutboundOptions.TestReceiveResponse;
var
  OptionsCount: Integer;
  StatusCode:   Cardinal;
begin
  for StatusCode := SIPOKResponseClass to SIPGlobalFailureResponseClass do begin
    Self.ReceivedResponse := false;
    Self.CreateAction;

    OptionsCount := Self.Core.OptionsCount;

    Self.ReceiveResponse(StatusCode * 100);

    Check(Self.ReceivedResponse,
          'Listeners not notified of response ' + IntToStr(StatusCode * 100));
    Check(Self.Core.OptionsCount < OptionsCount,
          'OPTIONS action not terminated for ' + IntToStr(StatusCode) + ' response');       
  end;
end;

procedure TestTIdSipOutboundOptions.TestRemoveListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);
      Options.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response,
            'First listener not notified');
      Check(not L2.Response,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOptionsResponseMethod                                            *
//******************************************************************************
//* TestTIdSipOptionsResponseMethod Public methods *****************************

procedure TestTIdSipOptionsResponseMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipOptionsResponseMethod.Create;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Options  := Self.UA.QueryOptions(Nowhere);
    Self.Method.Response := Self.Response;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipOptionsResponseMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOptionsResponseMethod Published methods **************************

procedure TestTIdSipOptionsResponseMethod.TestRun;
var
  Listener: TIdSipTestOptionsListener;
begin
  Listener := TIdSipTestOptionsListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Response, 'Listener not notified');
    Check(Self.Method.Options = Listener.OptionsAgentParam,
          'OptionsAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentAuthenticationChallengeMethod                           *
//******************************************************************************
//* TestTIdSipUserAgentAuthenticationChallengeMethod Public methods ************

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.SetUp;
begin
  inherited SetUp;

  Self.Challenge := TIdSipResponse.Create;
  Self.UserAgent := TIdSipUserAgent.Create;
  Self.Method := TIdSipUserAgentAuthenticationChallengeMethod.Create;

  Self.Method.UserAgent := Self.UserAgent;

  Self.Method.Challenge := Self.Challenge;

  Self.L1 := TIdSipTestUserAgentListener.Create;
  Self.L2 := TIdSipTestUserAgentListener.Create;
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TearDown;
begin
  Self.L2.Free;
  Self.L1.Free;
  Self.Method.Free;
  Self.UserAgent.Free;
  Self.Challenge.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentAuthenticationChallengeMethod Published methods **********

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerDoesntSetPassword;
begin
  Self.L2.Password := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Password,
              Self.Method.FirstPassword,
              '2nd listener didn''t set password');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerSetsPassword;
begin
  Self.L1.Password := 'foo';
  Self.L2.Password := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'Returned password not 1st listener''s');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerDoesntSetUsername;
begin
  Self.L2.Username := 'foo';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L2.Username,
              Self.Method.FirstUsername,
              '2nd listener didn''t set Username');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestFirstListenerSetsUsername;
begin
  Self.L1.Username := 'foo';
  Self.L2.Username := 'bar';

  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'Returned Username not 1st listener''s');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestRun;
begin
  Self.L1.Password := 'foo';
  Self.L1.Username := 'foo';
  Self.L2.Password := 'bar';
  Self.L2.Username := 'bar';

  Self.Method.Run(Self.L1);
  Check(Self.L1.AuthenticationChallenge,
        'L1 not notified');
  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'L1 gives us the first password');
  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'L1 gives us the first username');

  Self.Method.Run(Self.L2);
  Check(Self.L2.AuthenticationChallenge,
        'L2 not notified');

  CheckEquals(Self.L1.Password,
              Self.Method.FirstPassword,
              'We ignore L2''s password');

  CheckEquals(Self.L1.Username,
              Self.Method.FirstUsername,
              'We ignore L2''s username');

  Check(Self.Method.UserAgent = Self.L1.AbstractUserAgentParam,
        'UserAgent param');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestTryAgain;
begin
  Self.L1.TryAgain := true;

  Self.Method.Run(Self.L1);

  Check(Self.Method.TryAgain, 'TryAgain not set');
end;

procedure TestTIdSipUserAgentAuthenticationChallengeMethod.TestNoListenerSetsPassword;
begin
  Self.Method.Run(Self.L1);
  Self.Method.Run(Self.L2);

  CheckEquals('',
              Self.Method.FirstPassword,
              'Something other than the listeners set the password');

  CheckEquals('',
              Self.Method.FirstUsername,
              'Something other than the listeners set the username');
end;

//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod                           *
//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ************

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipMockUdpTransport.Create;
  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Message := Self.Response.Copy;
end;

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;
  Self.Receiver.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Published methods *********

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TestRun;
var
  L: TIdSipTestTransactionUserListener;
begin
  L := TIdSipTestTransactionUserListener.Create;
  try
    Self.Method.Run(L);

    Check(L.DroppedUnmatchedMessage, 'Listener not notified');
    Check(Self.Method.Receiver = L.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Message = L.MessageParam,
          'Message param');
    Check(Self.Method.UserAgent = L.AbstractUserAgentParam,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentInboundCallMethod                                       *
//******************************************************************************
//* TestTIdSipUserAgentInboundCallMethod Public methods ************************

procedure TestTIdSipUserAgentInboundCallMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipTestResources.CreateBasicRequest;

  Self.Dispatcher.MockLocator.AddA(Self.Request.LastHop.SentBy, '127.0.0.1');

  Self.Session := TIdSipInboundSession.CreateInbound(Self.UA,
                                                     Self.Request,
                                                     false);
  Self.Method := TIdSipUserAgentInboundCallMethod.Create;
  Self.Method.Session := Self.Session;
end;

procedure TestTIdSipUserAgentInboundCallMethod.TearDown;
begin
  Self.Method.Free;
  Self.Session.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentInboundCallMethod Published methods *********************

procedure TestTIdSipUserAgentInboundCallMethod.TestRun;
var
  L: TIdSipTestUserAgentListener;
begin
  L := TIdSipTestUserAgentListener.Create;
  try
    Self.Method.Run(L);

    Check(L.InboundCall, 'Listener not notified');
    Check(Self.Method.Session = L.SessionParam,
          'Session param');
    Check(Self.Method.UserAgent = L.UserAgentParam,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
