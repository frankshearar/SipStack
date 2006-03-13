{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTransaction;

interface

uses
  IdSipAuthentication, IdSipCore, IdSipDialog, IdSipDns, IdSipLocator,
  IdSipMessage, IdSipMockCore, IdSipMockLocator, IdSipMockTransactionDispatcher,
  IdSipMockTransport, IdSipTransaction, IdSipTransport, IdTimerQueue, SysUtils,
  TestFramework, TestFrameworkSip;

type
  TMessageCountingTestCase = class(TTestCaseSip)
  protected
    AckCount:      Cardinal;
    MockTransport: TIdSipMockTransport;
    RequestCount:  Cardinal;
    ResponseCount: Cardinal;
  public
    procedure CheckAckSent(const Msg: String);
    procedure CheckNoACKSent(const Msg: String);
    procedure CheckNoRequestSent(const Msg: String);
    procedure CheckNoResponseSent(const Msg: String);
    procedure CheckRequestSent(const Msg: String;
                               Transport: TIdSipMockTransport = nil);
    procedure CheckResponseSent(const Msg: String;
                                Transport: TIdSipMockTransport = nil);

    function  LastSentACK: TIdSipRequest;
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    procedure MarkSentACKCount;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    function  SentAckCount: Cardinal;
    function  SentRequestCount: Cardinal;
    function  SentResponseCount: Cardinal;
  end;

  TestTIdSipTransactionDispatcher = class(TMessageCountingTestCase,
                                          IIdSipTransactionListener,
                                          IIdSipTransactionDispatcherListener)
  private
    AckCount:               Cardinal;
    Core:                   TIdSipMockCore;
    D:                      TIdSipTransactionDispatcher;
    Destination:            TIdSipLocation;
    Invite:                 TIdSipRequest;
    Locator:                TIdSipMockLocator;
    MockTcpTransport:       TIdSipMockTransport;
    MockUdpTransport:       TIdSipMockTransport;
    OnReceiveResponseFired: Boolean;
    OnTerminatedFired:      Boolean;
    Options:                TIdSipRequest;
    ReceivedRequest:        TIdSipRequest;
    ReceivedResponse:       TIdSipResponse;
    RejectedRequest:        TIdSipRequest;
    Response200:            TIdSipResponse;
    Timer:                  TIdDebugTimerQueue;
    TranRequest:            TIdSipRequest;
    Username:               String;

    function  CreateAck(Response: TIdSipResponse): TIdSipRequest;
    function  CreateMultipleChoices(Request: TIdSipRequest): TIdSipResponse;
    procedure MarkSentRequestCount;
    procedure MoveTranToCompleted(Tran: TIdSipClientTransaction); overload;
    procedure MoveTranToCompleted(Tran: TIdSipServerTransaction); overload;
    procedure MoveTranToConfirmed(Tran: TIdSipServerInviteTransaction);
    procedure OnFail(Transaction: TIdSipTransaction;
                     FailedMessage: TIdSipMessage;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport); overload;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); overload;
    procedure OnTerminated(Transaction: TIdSipTransaction);
    procedure OnTransportException(FailedMessage: TIdSipMessage;
                                   Error: Exception;
                                   const Reason: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckAckSent(const Msg: String);
    procedure CheckRequestSent(const Msg: String);
  published
    procedure TestAckDoesntCreateATransaction;
    procedure TestAckForInviteWontCreateTransaction;
    procedure TestAckHandedUpToTU;
    procedure TestAddClientTransaction;
    procedure TestAddServerTransaction;
    procedure TestAddTransportBinding;
    procedure TestAddTransportBindingAddsTimerToTransport;
    procedure TestClearAddAndCountTransports;
    procedure TestCreateNewTransaction;
    procedure TestDispatchToCorrectTransaction;
    procedure TestDispatcherDoesntGetTransactionRequests;
    procedure TestDispatcherDoesntGetTransactionResponses;
    procedure TestFailedMessageSendNotifiesListeners;
    procedure TestHandUnmatchedRequestToCore;
    procedure TestHandUnmatchedResponseToCore;
    procedure TestInviteDoesntSendTrying;
    procedure TestLoopDetected;
    procedure TestLoopDetectedRFC2543RequestWithNoBranch;
    procedure TestOnClientInviteTransactionTimerA;
    procedure TestOnClientInviteTransactionTimerB;
    procedure TestOnClientInviteTransactionTimerD;
    procedure TestOnClientNonInviteTransactionTimerE;
    procedure TestOnClientNonInviteTransactionTimerF;
    procedure TestOnClientNonInviteTransactionTimerK;
    procedure TestOnServerInviteTransactionTimerG;
    procedure TestOnServerInviteTransactionTimerH;
    procedure TestOnServerInviteTransactionTimerI;
    procedure TestOnServerNonInviteTransactionTimerJ;
    procedure TestSendAckWontCreateTransaction;
    procedure TestSendRequest;
    procedure TestSendRequestOverUdp;
    procedure TestSendResponse;
    procedure TestSendMessageButNoAppropriateTransport;
    procedure TestSendMessageWithAppropriateTransport;
    procedure TestServerInviteTransactionGetsAck;
    procedure TestTransactionsCleanedUp;
    procedure TestWillUseReliableTransport;
  end;

  // Test the location-using code in SendResponse
  TestLocation = class(TMessageCountingTestCase)
  private
    D:            TIdSipTransactionDispatcher;
    L:            TIdSipMockLocator;
    Timer:        TIdDebugTimerQueue;
    Request:      TIdSipRequest;
    Response:     TIdSipResponse;
    TcpTransport: TIdSipMockTransport;
    UdpTransport: TIdSipMockTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompleteNetworkFailure;
    procedure TestNetworkFailureTriesAlternateDestinations;
    procedure TestNormalOperation;
    procedure TestTransactionsWontRelookupDnsForRetransmittedResponses;
    procedure TestTransactionsWontRelookupDns;
  end;

  TestTIdSipTransaction = class(TTestCase)
  private
    Dispatcher:      TIdSipMockTransactionDispatcher;
    ReceivedRequest: TIdSipRequest;
    Request:         TIdSipRequest;
    Response:        TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransactionListener;
    procedure TestAllListenersNotified;
    procedure TestGetClientTransactionType;
    procedure TestGetServerTransactionType;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteServer;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
    procedure TestMatchRFC2543Ack;
    procedure TestMatchRFC2543Invite;
    procedure TestMatchRFC2543InviteDifferentCallID;
    procedure TestMatchRFC2543InviteDifferentCSeq;
    procedure TestMatchRFC2543InviteDifferentFromTag;
    procedure TestMatchRFC2543InviteDifferentRequestUri;
    procedure TestMatchRFC2543InviteDifferentToTag;
    procedure TestMatchRFC2543InviteDifferentViaBranch;
    procedure TestMatchRFC2543InviteDifferentViaSentBy;
    procedure TestRemoveTransactionListener;
  end;

  TIdSipTransactionEvent = procedure(Sender: TIdSipTransaction) of object;

  TTestIdSipRequestEvent = procedure(Sender: TObject;
                                     R: TIdSipRequest) of object;
  TTestIdSipResponseEvent = procedure(Sender: TObject;
                                      R: TIdSipResponse) of object;

  // Transactions behave slightly differently if a reliable transport is used -
  // certain messages are not resent. To this end, we test unreliable transports
  // by default, only checking that those certain messages are not resent when
  // using reliable transports in tests like TestReliableTransportFoo
  TTestTransaction = class(TMessageCountingTestCase,
                           IIdSipTransactionListener)
  protected
    CheckReceiveRequest:   TTestIdSipRequestEvent;
    CheckReceiveResponse:  TTestIdSipResponseEvent;
    CheckTerminated:       TIdSipTransactionEvent;
    Core:                  TIdSipAbstractCore;
    Destination:           TIdSipLocation;
    FailMsg:               String;
    MockDispatcher:        TIdSipMockTransactionDispatcher;
    MockLocator:           TIdSipMockLocator;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    Tran:                  TIdSipTransaction;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionProceeding: Boolean;
    TransactionTerminated: Boolean;

    procedure Completed(Sender: TObject;
                        R: TIdSipResponse);
    function  DebugTimer: TIdDebugTimerQueue;
    procedure OnFail(Transaction: TIdSipTransaction;
                     FailedMessage: TIdSipMessage;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport); virtual;
    procedure OnTerminated(Transaction: TIdSipTransaction);
    procedure Proceeding(Sender: TObject;
                         R: TIdSipResponse);
    procedure TransactionFail(Sender: TObject;
                              const Reason: String);
    procedure Terminated(Sender: TIdSipTransaction);
    function  TransactionType: TIdSipTransactionClass; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsNull;
  end;

  TestTIdSipServerInviteTransaction = class(TTestTransaction)
  private
    ServerTran:           TIdSipServerInviteTransaction;
    TransactionConfirmed: Boolean;

    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure MoveToTerminatedState;
    procedure OnInitialRequestSentToTU(Sender: TObject;
                                       R: TIdSipRequest);
    procedure ReceiveInvite;
    procedure Terminate(Tran: TIdSipTransaction);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAuthenticationChallengeTreatedStatelessly;
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestIsClient;
    procedure TestIsInvite;
    procedure TestIsServer;
    procedure TestReceive2xxFromTUInProceedingState;
    procedure TestReceiveAckInCompletedState;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveInviteInCompletedState;
    procedure TestReceiveInviteInConfirmedState;
    procedure TestReceiveInviteInProceedingState;
    procedure TestReceiveInviteInTerminatedState;
    procedure TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
    procedure TestReliableTransportNoFinalResponseRetransmissions;
    procedure TestReReceiveInitialRequestInCompletedState;
    procedure TestResponseRetransmissionInCompletedState;
    procedure TestTimerGEventScheduled;
    procedure TestTimerGIntervalIncreases;
    procedure TestTimerGOnlyFiresInCompletedState;
    procedure TestTimerGStops;
    procedure TestTimerHEventScheduled;
    procedure TestTimerHFired;
    procedure TestTimerIEventScheduled;
    procedure TestTimerIFired;
    procedure TestTransportErrorInCompletedState;
    procedure TestTransportErrorInProceedingState;
    procedure TestTransactionUserResponsesSentToTransport;
  end;

  TestTIdSipServerNonInviteTransaction = class(TTestTransaction)
  private
    TransactionTrying: Boolean;

    procedure MoveToCompletedState(Tran: TIdSipTransaction);
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
    procedure Trying(Sender: TObject;
                     R: TIdSipRequest);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestIsClient;
    procedure TestIsInvite;
    procedure TestIsServer;
    procedure TestReceiveFinalResponseFromTUInCompletedState;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveFinalResponseFromTUInTerminatedState;
    procedure TestReceiveFinalResponseFromTUInTryingState;
    procedure TestReceiveProvisionalResponseFromTUInProceedingState;
    procedure TestReceiveProvisionalResponseFromTUInTryingState;
    procedure TestReReceiveInitialRequestInCompletedState;
    procedure TestReReceiveInitialRequestInProceedingState;
    procedure TestResponseFromTUInCompletedState;
    procedure TestTimerJEventScheduled;
    procedure TestTimerJFired;
    procedure TestTransportErrorInCompletedState;
    procedure TestTransportErrorInProceedingState;
    procedure TestTuResponsesSentToTransport;
  end;

  TestTIdSipClientInviteTransaction = class(TTestTransaction)
  private
    ClientTran:  TIdSipClientInviteTransaction;

    procedure CheckACK(Ack: TIdSipRequest;
                       Response: TIdSipResponse);
    procedure MoveToCompletedState(Tran: TIdSipTransaction);
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
  protected
    procedure Terminate(Tran: TIdSipTransaction);
    function  TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestACK;
    procedure TestFireTimerAInCallingState;
    procedure TestFireTimerAInCompletedState;
    procedure TestFireTimerAInProceedingState;
    procedure TestFireTimerBInCallingState;
    procedure TestFireTimerBInCompletedState;
    procedure TestFireTimerBInProceedingState;
    procedure TestFireTimerDInCallingState;
    procedure TestFireTimerDInProceedingState;
    procedure TestInitialState;
    procedure TestInviteWithHostUnreachable;
    procedure TestIsClient;
    procedure TestIsInvite;
    procedure TestIsServer;
    procedure TestMultipleInviteSending;
    procedure TestNoInviteResendingInProceedingState;
    procedure TestNonInviteMethodInInitialRequest;
    procedure TestReceive1xxInCallingState;
    procedure TestReceive1xxInCompletedState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxInTerminatedState;
    procedure TestReceive2xxInCallingState;
    procedure TestReceive2xxInCompletedState;
    procedure TestReceive2xxInProceedingState;
    procedure TestReceive3xxInCallingState;
    procedure TestReceive3xxInCompletedState;
    procedure TestReceive3xxInProceedingState;
    procedure TestReceiveMultipleResponsesInCompletedState;
    procedure TestReliableTransportNoInviteRetransmissions;
    procedure TestSendRequestSchedulesTimerA;
    procedure TestSendRequestSchedulesTimerB;
    procedure TestSendRequestUntilTimeout;
    procedure TestTimerAIncreases;
    procedure TestTimerDFired;
    procedure TestTimerDScheduled;
    procedure TestTimeout;
    procedure TestTransportErrorInCallingState;
    procedure TestTransportErrorInCompletedState;
  end;

  TestTIdSipClientNonInviteTransaction = class(TTestTransaction)
  private
    ClientTran:       TIdSipClientNonInviteTransaction;
    ResponseReceived: Boolean;

    procedure AcknowledgeResponseReceipt(Sender: TObject;
                                         R: TIdSipResponse);
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
    procedure MoveToCompletedState(Tran: TIdSipTransaction);
    procedure MoveToTerminatedState(Tran: TIdSipClientNonInviteTransaction);
  protected
    procedure Terminate(Tran: TIdSipTransaction);
    function  TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestFireTimerEInCompletedState;
    procedure TestFireTimerEInProceedingState;
    procedure TestFireTimerEInTryingState;
    procedure TestFireTimerFInCompletedState;
    procedure TestFireTimerFInProceedingState;
    procedure TestFireTimerFInTryingState;
    procedure TestFireTimerKInProceedingState;
    procedure TestFireTimerKInTryingState;
    procedure TestInitialRequestSent;
    procedure TestInitialState;
    procedure TestIsClient;
    procedure TestIsInvite;
    procedure TestIsServer;
    procedure TestMultipleRequestSendingInProceedingState;
    procedure TestMultipleRequestSendingInTryingState;
    procedure TestReceive1xxInCompletedState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxInTerminatedState;
    procedure TestReceive1xxInTryingState;
    procedure TestReceiveFinalResponseInProceedingState;
    procedure TestReceiveFinalResponseInTryingState;
    procedure TestTimerEIntervalInProceedingRemainsConstant;
    procedure TestTimerEIntervalInTryingIncreases;
    procedure TestTimerEScheduled;
    procedure TestTimerEScheduledOnlyForUnreliableTransports;
    procedure TestTimerFScheduled;
    procedure TestTimerKFired;
    procedure TestTimerKScheduled;
    procedure TestTransportErrorInProceedingState;
    procedure TestTransportErrorInTryingState;
  end;

  TestTIdSipResponseLocationsList = class(TTestCase)
  private
    List:      TIdSipResponseLocationsList;
    Locations: TIdSipLocations;
    Response:  TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndLocationsFor;
    procedure TestContains;
    procedure TestLocationsForReturnsMutableList;
  end;

  TTransactionDispatcherListenerMethodTestCase = class(TTestCase)
  protected
    Receiver: TIdSipTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTransactionDispatcherListenerReceiveRequestMethod = class(TTransactionDispatcherListenerMethodTestCase)
  private
    Method:  TIdSipTransactionDispatcherListenerReceiveRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransactionDispatcherListenerReceiveResponseMethod = class(TTransactionDispatcherListenerMethodTestCase)
  private
    Method:   TIdSipTransactionDispatcherListenerReceiveResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TTransactionListenerMethodTestCase = class(TTestCase)
  protected
    Dispatcher:  TIdSipMockTransactionDispatcher;
    Request:     TIdSipRequest;
    Transaction: TIdSipTransaction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipTransactionListenerFailMethod = class(TTransactionListenerMethodTestCase)
  private
    Method: TIdSipTransactionListenerFailMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransactionListenerReceiveRequestMethod = class(TTransactionListenerMethodTestCase)
  private
    Method: TIdSipTransactionListenerReceiveRequestMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransactionListenerReceiveResponseMethod = class(TTransactionListenerMethodTestCase)
  private
    Method:   TIdSipTransactionListenerReceiveResponseMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipTransactionListenerTerminatedMethod = class(TTransactionListenerMethodTestCase)
  private
    Method: TIdSipTransactionListenerTerminatedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  Classes, IdException, IdRandom, IdSdp, IdSipConsts, Math, TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipTransactionDispatcher.Suite);
  Result.AddTest(TestLocation.Suite);
  Result.AddTest(TestTIdSipTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipResponseLocationsList.Suite);
  Result.AddTest(TestTIdSipTransactionDispatcherListenerReceiveRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransactionDispatcherListenerReceiveResponseMethod.Suite);
  Result.AddTest(TestTIdSipTransactionListenerFailMethod.Suite);
  Result.AddTest(TestTIdSipTransactionListenerReceiveRequestMethod.Suite);
  Result.AddTest(TestTIdSipTransactionListenerReceiveResponseMethod.Suite);
  Result.AddTest(TestTIdSipTransactionListenerTerminatedMethod.Suite);
end;

function Transaction(S: TIdSipTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipTransactionState), Integer(S));
end;

//******************************************************************************
//* TMessageCountingTestCase                                                   *
//******************************************************************************
//* TMessageCountingTestCase Public methods ************************************

procedure TMessageCountingTestCase.CheckAckSent(const Msg: String);
begin
  Check(Self.ACKCount < Self.SentACKCount, Msg);
end;

procedure TMessageCountingTestCase.CheckNoACKSent(const Msg: String);
begin
  CheckEquals(Self.ACKCount, Self.SentACKCount, Msg);
end;

procedure TMessageCountingTestCase.CheckNoRequestSent(const Msg: String);
begin
  CheckEquals(Self.RequestCount, Self.SentRequestCount, Msg);
end;

procedure TMessageCountingTestCase.CheckNoResponseSent(const Msg: String);
begin
  CheckEquals(Self.ResponseCount, Self.SentResponseCount, Msg);
end;

procedure TMessageCountingTestCase.CheckRequestSent(const Msg: String;
                                                    Transport: TIdSipMockTransport = nil);
begin
  if Assigned(Transport) then
    Check(Self.RequestCount < Transport.SentRequestCount, Msg)
  else
    Check(Self.RequestCount < Self.SentRequestCount, Msg);
end;

procedure TMessageCountingTestCase.CheckResponseSent(const Msg: String;
                                                     Transport: TIdSipMockTransport = nil);
begin
  if Assigned(Transport) then
    Check(Self.ResponseCount < Transport.SentResponseCount, Msg)
  else
    Check(Self.ResponseCount < Self.SentResponseCount, Msg);
end;

function TMessageCountingTestCase.LastSentACK: TIdSipRequest;
begin
  Result := Self.MockTransport.LastACK;
end;

function TMessageCountingTestCase.LastSentRequest: TIdSipRequest;
begin
  Result := Self.MockTransport.LastRequest;
end;

function TMessageCountingTestCase.LastSentResponse: TIdSipResponse;
begin
  Result := Self.MockTransport.LastResponse;
end;

procedure TMessageCountingTestCase.MarkSentACKCount;
begin
  Self.ACKCount := Self.SentACKCount;
end;

procedure TMessageCountingTestCase.MarkSentRequestCount;
begin
  Self.RequestCount := Self.SentRequestCount;
end;

procedure TMessageCountingTestCase.MarkSentResponseCount;
begin
  Self.ResponseCount := Self.SentResponseCount;
end;

function TMessageCountingTestCase.SentAckCount: Cardinal;
begin
  Result := Self.MockTransport.ACKCount;
end;

function TMessageCountingTestCase.SentRequestCount: Cardinal;
begin
  Result := Self.MockTransport.SentRequestCount;
end;

function TMessageCountingTestCase.SentResponseCount: Cardinal;
begin
  Result := Self.MockTransport.SentResponseCount;
end;

//******************************************************************************
//* TestTIdSipTransactionDispatcher                                            *
//******************************************************************************
//* TestTIdSipTransactionDispatcher Public methods *****************************

procedure TestTIdSipTransactionDispatcher.SetUp;
begin
  inherited SetUp;

  Self.Core := TIdSipMockCore.Create;

  Self.Locator := TIdSipMockLocator.Create;
  Self.Timer   := TIdDebugTimerQueue.Create(false);

  Self.D := TIdSipTransactionDispatcher.Create(Self.Timer, Self.Locator);
  Self.D.AddTransactionDispatcherListener(Self);

  Self.Core.Dispatcher := Self.D;

  // Remember, Self's subclass has registered mock transports for these symbols.
  Self.D.AddTransportBinding(TcpTransport, '127.0.0.1', IdPORT_SIP);
  Self.D.AddTransportBinding(UdpTransport, '127.0.0.1', IdPORT_SIP);

  Self.MockTcpTransport := Self.D.Transports[0] as TIdSipMockTransport;
  Self.MockUdpTransport := Self.D.Transports[1] as TIdSipMockTransport;
  Self.MockTransport    := Self.MockTcpTransport;

  // This must differ from Self.D's bindings, or we will make hairpin calls
  // when we send INVITEs. That in itself isn't a problem, but for most tests
  // that's not what we want!
  Self.Destination := TIdSipLocation.Create(TcpTransport, '127.0.0.2', IdPORT_SIP);

  Self.ReceivedRequest  := TIdSipTestResources.CreateLocalLoopRequest;
  Self.TranRequest      := TIdSipTestResources.CreateLocalLoopRequest;
  Self.ReceivedResponse := TIdSipTestResources.CreateLocalLoopResponse;

  Self.ReceivedResponse.StatusCode := SIPTrying;

  Self.Invite := TIdSipRequest.Create;
  Self.Invite.Assign(Self.ReceivedRequest);
  Self.Invite.Body := '';
  Self.Invite.ContentLength := 0;
  Self.Invite.ContentType := SdpMimeType;

  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.ReceivedRequest);
  Self.Options.Method := MethodOptions;
  Self.Options.CSeq.Method := Self.Options.Method;

  Self.RejectedRequest := TIdSipRequest.Create;

  Self.Response200 := TIdSipResponse.Create;
  Self.Response200.Assign(Self.ReceivedResponse);
  Self.Response200.StatusCode := SIPOK;
  Self.Response200.AddHeader(Self.ReceivedRequest.FirstContact);

  Self.OnReceiveResponseFired := false;
  Self.OnTerminatedFired      := false;
  Self.Username               := 'case';
end;

procedure TestTIdSipTransactionDispatcher.TearDown;
begin
  Self.Timer.Terminate;

  Self.Response200.Free;
  Self.RejectedRequest.Free;
  Self.Options.Free;
  Self.Invite.Free;
  Self.ReceivedResponse.Free;
  Self.TranRequest.Free;
  Self.ReceivedRequest.Free;

  Self.Destination.Free;
  Self.D.Free;
  Self.Locator.Free;
  Self.Core.Free;

  inherited TearDown;
end;

procedure TestTIdSipTransactionDispatcher.CheckAckSent(const Msg: String);
begin
  Check(Self.AckCount < Self.SentACKCount,
        Msg);
end;

procedure TestTIdSipTransactionDispatcher.CheckRequestSent(const Msg: String);
begin
  Check(Self.RequestCount < Self.SentRequestCount, Msg);
end;

//* TestTIdSipTransactionDispatcher Private methods ****************************

function TestTIdSipTransactionDispatcher.CreateAck(Response: TIdSipResponse): TIdSipRequest;
begin
  Result := Self.Invite.AckFor(Response);
end;

function TestTIdSipTransactionDispatcher.CreateMultipleChoices(Request: TIdSipRequest): TIdSipResponse;
var
  UA: TIdSipAbstractCore;
begin
  UA := TIdSipAbstractCore.Create;
  try
    Result := UA.CreateResponse(Request, SIPMultipleChoices);
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.MarkSentRequestCount;
begin
  Self.RequestCount := Self.SentRequestCount;
end;

procedure TestTIdSipTransactionDispatcher.MoveTranToCompleted(Tran: TIdSipClientTransaction);
var
  Ok: TIdSipResponse;
begin
  Ok := TIdSipResponse.InResponseTo(Tran.InitialRequest, SIPBadRequest);
  try
    Self.MockTransport.FireOnResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.MoveTranToCompleted(Tran: TIdSipServerTransaction);
var
  Ok: TIdSipResponse;
begin
  Ok := TIdSipResponse.InResponseTo(Tran.InitialRequest, SIPBadRequest);
  try
    Self.D.SendResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.MoveTranToConfirmed(Tran: TIdSipServerInviteTransaction);
var
  Ack: TIdSipRequest;
  Ok:  TIdSipResponse;
begin
  Ok := TIdSipResponse.InResponseTo(Tran.InitialRequest, SIPBadRequest);
  try
    Self.D.SendResponse(Ok);

    Ack := Tran.InitialRequest.AckFor(Ok);
    try
      Self.MockTransport.FireOnRequest(Ack);
    finally
      Ack.Free;
    end;
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.OnFail(Transaction: TIdSipTransaction;
                                                 FailedMessage: TIdSipMessage;
                                                 const Reason: String);
begin
  // Do nothing
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                           Transaction: TIdSipTransaction;
                                                           Receiver: TIdSipTransport);
begin
  // Do nothing
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                           Receiver: TIdSipTransport);
begin
  // Do nothing
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                            Transaction: TIdSipTransaction;
                                                            Receiver: TIdSipTransport);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnReceiveResponseFired := true;
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                            Receiver: TIdSipTransport);
begin
  // Do nothing
end;

procedure TestTIdSipTransactionDispatcher.OnTerminated(Transaction: TIdSipTransaction);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnTerminatedFired := true;
end;

procedure TestTIdSipTransactionDispatcher.OnTransportException(FailedMessage: TIdSipMessage;
                                                               Error: Exception;
                                                               const Reason: String);
begin
  // Do nothing.
end;

//* TestTIdSipTransactionDispatcher Published methods **************************

procedure TestTIdSipTransactionDispatcher.TestAckDoesntCreateATransaction;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.Invite.AckFor(Self.ReceivedResponse);
  try
    Ack.Method := MethodAck;

    Self.MockTransport.FireOnRequest(Ack);

    CheckEquals(0, Self.D.TransactionCount, 'ACK created a transaction');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestAckForInviteWontCreateTransaction;
var
  Ack: TIdSipRequest;
begin
  Self.D.SendRequest(Self.Invite, Self.Destination);
  CheckEquals(1, Self.D.TransactionCount, 'INVITE');

  Self.MockTransport.FireOnResponse(Self.Response200);
  CheckEquals(0, Self.D.TransactionCount, '200 terminates a client INVITE tran');

  Ack := Self.Invite.AckFor(Self.ReceivedResponse);
  try
    Self.D.SendRequest(Ack, Self.Destination);
  finally
    Ack.Free;
  end;

  CheckEquals(0,
              Self.D.TransactionCount,
              'ACK created a transaction');
end;

procedure TestTIdSipTransactionDispatcher.TestAckHandedUpToTU;
var
  Ack:          TIdSipRequest;
  RemoteDialog: TIdSipDialog;
  Listener:     TIdSipTestTransactionDispatcherListener;
  Tran:         TIdSipTransaction;
begin
  RemoteDialog := TIdSipDialog.CreateOutboundDialog(Self.Invite,
                                                    Self.Response200,
                                                    false);
  try
    Ack := RemoteDialog.CreateAck;
    try
      Tran := Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);

      Listener := TIdSipTestTransactionDispatcherListener.Create;
      try
        Self.D.AddTransactionDispatcherListener(Listener);

        Tran.SendResponse(Self.Response200);

        Self.MockTransport.FireOnRequest(Ack);
        // cf RFC 3261 section 13.3.1.4 - the Transaction User layer is
        // responsible for handling ACKs to a 2xx response!
        Check(Listener.ReceivedRequest,
              'ACK not handed up to TU');
      finally
        Listener.Free;
      end;
    finally
      Ack.Free;
    end;
  finally
    RemoteDialog.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestAddClientTransaction;
var
  Tran:      TIdSipTransaction;
  TranCount: Cardinal;
begin
  TranCount := Self.D.TransactionCount;
  Tran := Self.D.AddClientTransaction(Self.Invite);
  Check(Tran.IsClient,
        'Wrong kind of transaction added');
  CheckEquals(TranCount + 1,
              Self.D.TransactionCount,
              'Transaction wasn''t added');
end;

procedure TestTIdSipTransactionDispatcher.TestAddServerTransaction;
var
  Tran:      TIdSipTransaction;
  TranCount: Cardinal;
begin
  TranCount := Self.D.TransactionCount;
  Tran := Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);
  Check(not Tran.IsClient,
        'Wrong kind of transaction added');
  CheckEquals(TranCount + 1,
              Self.D.TransactionCount,
              'Transaction wasn''t added');
end;

procedure TestTIdSipTransactionDispatcher.TestAddTransportBinding;
var
  OriginalCount: Cardinal;
begin
  // Let's start with a clean slate, as far as transports are concerned.
  Self.D.Transports.Clear;
  OriginalCount := Self.D.TransportCount;

  Self.D.AddTransportBinding(TcpTransport, '127.0.0.1', 1);
  CheckEquals(OriginalCount + 1,
              Self.D.TransportCount,
              'New transport not added');

  Self.D.AddTransportBinding(TcpTransport, '127.0.0.1', 2);
  CheckEquals(OriginalCount + 1,
              Self.D.TransportCount,
              'Binding not added to existing TCP transport');

  Self.D.AddTransportBinding(TcpTransport, '127.0.0.2', 3);
  CheckEquals(OriginalCount + 1,
              Self.D.TransportCount,
              'Binding on different address not added to existing TCP transport');

  Self.D.AddTransportBinding(UdpTransport, '127.0.0.1', 1);
  CheckEquals(OriginalCount + 2,
              Self.D.TransportCount,
              'Binding on different transport not added to a new transport');
end;

procedure TestTIdSipTransactionDispatcher.TestAddTransportBindingAddsTimerToTransport;
begin
  // Let's start with a clean slate, as far as transports are concerned.
  Self.D.Transports.Clear;

  Self.D.AddTransportBinding(TcpTransport, '127.0.0.1', 1);
  Check(Self.D.Timer = Self.D.Transports[0].Timer,
        'Newly-added transport doesn''t use the dispatcher''s timer');
end;

procedure TestTIdSipTransactionDispatcher.TestClearAddAndCountTransports;
begin
  CheckNotEquals(0, Self.D.TransportCount, 'Precondition: SetUp didn''t add transports');

  Self.D.ClearTransports;
  CheckEquals(0, Self.D.TransportCount, 'After Clear');

  Self.D.AddTransportBinding(UdpTransport,
                             '127.0.0.1',
                             IdPORT_SIP);
  CheckEquals(1, Self.D.TransportCount, 'After one AddTransport');

  Self.D.AddTransportBinding(TcpTransport,
                             '127.0.0.1',
                             IdPORT_SIP);
  CheckEquals(2, Self.D.TransportCount, 'After two AddTransports');
end;

procedure TestTIdSipTransactionDispatcher.TestCreateNewTransaction;
var
  OriginalCount: Integer;
begin
  OriginalCount := Self.D.TransactionCount;

  Self.D.AddClientTransaction(Self.Invite);

  CheckEquals(OriginalCount + 1,
              Self.D.TransactionCount,
              'No new transaction was created');
end;

procedure TestTIdSipTransactionDispatcher.TestDispatchToCorrectTransaction;
var
  InviteTran:      TIdSipTransaction;
  InviteListener:  TIdSipTestTransactionListener;
  OptionsTran:     TIdSipTransaction;
  OptionsListener: TIdSipTestTransactionListener;
  OriginalCount:   Integer;
begin
  OriginalCount := Self.D.TransactionCount;

  InviteTran  := Self.D.AddClientTransaction(Self.Invite);
  OptionsTran := Self.D.AddClientTransaction(Self.Options);

  CheckEquals(OriginalCount + 2,
              Self.D.TransactionCount,
              'Sanity check on transaction count');

  InviteListener := TIdSipTestTransactionListener.Create;
  try
    OptionsListener := TIdSipTestTransactionListener.Create;
    try
      InviteTran.AddTransactionListener(InviteListener);
      try
        OptionsTran.AddTransactionListener(OptionsListener);
        try
          Self.MockTransport.FireOnResponse(Self.ReceivedResponse);

          Check(InviteListener.ReceivedResponse and not OptionsListener.ReceivedResponse,
                'Wrong transaction got the response');
        finally
          OptionsTran.RemoveTransactionListener(OptionsListener);
        end;
      finally
        InviteTran.RemoveTransactionListener(InviteListener);
      end;
    finally
      OptionsListener.Free;
    end;
  finally
    InviteListener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestDispatcherDoesntGetTransactionRequests;
var
  Listener: TIdSipTestTransactionDispatcherListener;
begin
  Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);

  Listener := TIdSipTestTransactionDispatcherListener.Create;
  try
    Self.D.AddTransactionDispatcherListener(Listener);
    Self.MockTransport.FireOnRequest(Self.Invite);

    Check(not Listener.ReceivedUnhandledRequest,
          'Dispatcher said it got an unhandled request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestDispatcherDoesntGetTransactionResponses;
var
  Listener: TIdSipTestTransactionDispatcherListener;
begin
  Self.D.AddClientTransaction(Self.Invite);

  Listener := TIdSipTestTransactionDispatcherListener.Create;
  try
    Self.D.AddTransactionDispatcherListener(Listener);
    Self.MockTransport.FireOnResponse(Self.ReceivedResponse);

    Check(not Listener.ReceivedUnhandledResponse,
          'Dispatcher said it got an unhandled response');
  finally
    Self.D.RemoveTransactionDispatcherListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestFailedMessageSendNotifiesListeners;
var
  Listener: TIdSipTestTransactionDispatcherListener;
  Tran:     TIdSipTransaction;
begin
  Listener := TIdSipTestTransactionDispatcherListener.Create;
  try
    Self.D.AddTransactionDispatcherListener(Listener);

    Tran := Self.D.AddClientTransaction(Self.Invite);
    Tran.SendRequest(Self.Destination);
    // Self.Destination uses TCP.
    Self.MockTcpTransport.FireOnException(Self.LastSentRequest,
                                          EIdConnectException,
                                          '10061',
                                          'Connection refused');

    Check(Listener.RaisedException,
          'Listener not informed of raised exception');
  finally
    Self.D.RemoveTransactionDispatcherListener(Listener);
    Listener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestHandUnmatchedRequestToCore;
begin
  Self.MockTransport.FireOnRequest(Self.ReceivedRequest);
  Check(Self.Core.ReceiveRequestCalled,
        'Unmatched request not handed to Core');
end;

procedure TestTIdSipTransactionDispatcher.TestHandUnmatchedResponseToCore;
begin
  Self.MockTransport.FireOnResponse(Self.ReceivedResponse);
  Check(Self.Core.ReceiveResponseCalled,
        'Unmatched Response not handed to Core');
end;

procedure TestTIdSipTransactionDispatcher.TestInviteDoesntSendTrying;
begin
  // The Transaction-User layer (and specifically classes in the InviteModule)
  // bears responsibility for sending a 100 Trying response to an INVITE.

  Self.MarkSentResponseCount;
  Self.MockTransport.FireOnRequest(Self.ReceivedRequest);
  CheckNoResponseSent('Response sent by the transaction');
end;

procedure TestTIdSipTransactionDispatcher.TestLoopDetected;
begin
  // cf. RFC 3261, section 8.2.2.2
  Check(not Self.D.LoopDetected(Self.Invite), 'No transactions hence no loop');

  Self.Invite.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  Self.D.AddServerTransaction(Self.TranRequest, Self.MockTransport);
  Check(not Self.D.LoopDetected(Self.Invite),
        'Loop should not be detected - requests match (same branch)');

  Self.Invite.LastHop.Branch := Self.TranRequest.LastHop.Branch + '1';
  Check(Self.D.LoopDetected(Self.Invite),
        'Loop should be detected - same From tag, Call-ID, CSeq but no match '
      + '(differing branch)');
end;

procedure TestTIdSipTransactionDispatcher.TestLoopDetectedRFC2543RequestWithNoBranch;
begin
  // This is illegal according to RFC 3261, but not according to RFC 2543.
  Self.Invite.LastHop.RemoveParameter(BranchParam);
  Self.TranRequest.LastHop.Assign(Self.Invite.LastHop);

  Check(not Self.D.LoopDetected(Self.Invite), 'No transactions hence no loop');

  Self.Invite.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  Self.D.AddServerTransaction(Self.TranRequest, Self.MockTransport);
  Check(not Self.D.LoopDetected(Self.Invite),
        'Loop should not be detected - requests match (same branch)');

  Self.Invite.LastHop.Branch := Self.TranRequest.LastHop.Branch + '1';
  Check(Self.D.LoopDetected(Self.Invite),
        'Loop should be detected - same From tag, Call-ID, CSeq but no match '
      + '(differing branch)');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientInviteTransactionTimerA;
var
  Tran: TIdSipClientInviteTransaction;
  Wait: TIdNotifyEventWait;
begin
  // If Timer A fires then the transaction resends its initial request.
  // cf RFC 3261, section 17.1.1

  // Timer A only has meaning when using an unreliable transport
  Self.Invite.LastHop.Transport := UdpTransport;

  Tran := Self.D.AddClientTransaction(Self.Invite) as TIdSipClientInviteTransaction;
  Tran.SendRequest(Self.Destination);

  Self.MarkSentRequestCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientInviteTransactionTimerA;

    Self.D.OnClientInviteTransactionTimerA(Wait);
  finally
    Wait.Free;
  end;

  CheckRequestSent('Timer A didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientInviteTransactionTimerB;
var
  Tran:      TIdSipClientInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer B fires then the transaction terminates.
  // cf RFC 3261, section 17.1.1

  Tran := Self.D.AddClientTransaction(Self.Invite) as TIdSipClientInviteTransaction;
  Tran.SendRequest(Self.Destination);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientInviteTransactionTimerB;

    Self.D.OnClientInviteTransactionTimerB(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer B didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientInviteTransactionTimerD;
var
  Tran:      TIdSipClientInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer D fires then the transaction terminates.
  // cf RFC 3261, section 17.1.1

  Tran := Self.D.AddClientTransaction(Self.Invite) as TIdSipClientInviteTransaction;
  Tran.SendRequest(Self.Destination);

  TranCount := Self.D.TransactionCount;

  Self.MoveTranToCompleted(Tran);

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientInviteTransactionTimerD;

    Self.D.OnClientInviteTransactionTimerD(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer B didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientNonInviteTransactionTimerE;
var
  Tran: TIdSipClientNonInviteTransaction;
  Wait: TIdNotifyEventWait;
begin
  // If Timer E fires then the transaction resends its initial request.
  // cf RFC 3261, section 17.1.2

  // Timer E only has meaning when using an unreliable transport
  Self.Options.LastHop.Transport := UdpTransport;

  Tran := Self.D.AddClientTransaction(Self.Options) as TIdSipClientNonInviteTransaction;
  Tran.SendRequest(Self.Destination);

  Self.MarkSentRequestCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientNonInviteTransactionTimerE;

    Self.D.OnClientNonInviteTransactionTimerE(Wait);
  finally
    Wait.Free;
  end;

  CheckRequestSent('Timer E didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientNonInviteTransactionTimerF;
var
  Tran:      TIdSipClientNonInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer F fires then the transaction terminates.
  // cf RFC 3261, section 17.1.2

  Tran := Self.D.AddClientTransaction(Self.Options) as TIdSipClientNonInviteTransaction;
  Tran.SendRequest(Self.Destination);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientNonInviteTransactionTimerF;

    Self.D.OnClientNonInviteTransactionTimerF(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer E didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnClientNonInviteTransactionTimerK;
var
  Tran:      TIdSipClientNonInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer K fires then the transaction terminates.
  // cf RFC 3261, section 17.1.2

  Tran := Self.D.AddClientTransaction(Self.Options) as TIdSipClientNonInviteTransaction;
  Tran.SendRequest(Self.Destination);

  Self.MoveTranToCompleted(Tran);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnClientNonInviteTransactionTimerK;

    Self.D.OnClientNonInviteTransactionTimerK(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer K didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnServerInviteTransactionTimerG;
var
  Tran: TIdSipServerInviteTransaction;
  Wait: TIdNotifyEventWait;
begin
  // If Timer G fires then the transaction resends the last response.
  // cf RFC 3261, section 17.2.1

  Self.Locator.AddA(Self.Invite.LastHop.SentBy, '127.0.0.1');

  // Timer G only has meaning when using an unreliable transport.
  Self.Invite.LastHop.Transport := UdpTransport;
  Self.MockTransport            := Self.MockUdpTransport;

  Tran := Self.D.AddServerTransaction(Self.Invite,
                                      Self.MockTransport) as TIdSipServerInviteTransaction;

  Self.MoveTranToCompleted(Tran);

  Self.MarkSentResponseCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnServerInviteTransactionTimerG;

    Self.D.OnServerInviteTransactionTimerG(Wait);
  finally
    Wait.Free;
  end;

  CheckResponseSent('Timer G didn''t fire', Self.MockUdpTransport);
end;

procedure TestTIdSipTransactionDispatcher.TestOnServerInviteTransactionTimerH;
var
  Tran:      TIdSipServerInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer H fires then the transaction terminates.
  // cf RFC 3261, section 17.2.1

  Tran := Self.D.AddServerTransaction(Self.Invite,
                                      Self.MockTransport) as TIdSipServerInviteTransaction;

  Self.MoveTranToCompleted(Tran);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnServerInviteTransactionTimerH;

    Self.D.OnServerInviteTransactionTimerH(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer H didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnServerInviteTransactionTimerI;
var
  Tran:      TIdSipServerInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer H fires then the transaction terminates.
  // cf RFC 3261, section 17.2.1

  Tran := Self.D.AddServerTransaction(Self.Invite,
                                      Self.MockTransport) as TIdSipServerInviteTransaction;

  Self.MoveTranToConfirmed(Tran);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnServerInviteTransactionTimerI;

    Self.D.OnServerInviteTransactionTimerI(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer I didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestOnServerNonInviteTransactionTimerJ;
var
  Tran:      TIdSipServerInviteTransaction;
  TranCount: Integer;
  Wait:      TIdNotifyEventWait;
begin
  // If Timer J fires then the transaction terminates.
  // cf RFC 3261, section 17.2.2

  Tran := Self.D.AddServerTransaction(Self.Invite,
                                      Self.MockTransport) as TIdSipServerInviteTransaction;

  Self.MoveTranToCompleted(Tran);

  TranCount := Self.D.TransactionCount;

  Wait := TIdNotifyEventWait.Create;
  try
    Wait.Data  := Tran.InitialRequest.Copy;
    Wait.Event := Self.D.OnServerNonInviteTransactionTimerJ;

    Self.D.OnServerInviteTransactionTimerI(Wait);
  finally
    Wait.Free;
  end;

  Check(Self.D.TransactionCount < TranCount,
        'Timer J didn''t fire');
end;

procedure TestTIdSipTransactionDispatcher.TestSendAckWontCreateTransaction;
var
  Ack:       TIdSipRequest;
  TranCount: Cardinal;
begin
  TranCount := Self.D.TransactionCount;

  Ack := Self.Invite.AckFor(Self.ReceivedResponse);
  try
    Self.D.SendRequest(Ack, Self.Destination);
  finally
    Ack.Free;
  end;

  CheckEquals(TranCount,
              Self.D.TransactionCount,
              'Dispatcher made a new transaction for an outbound ACK');
end;

procedure TestTIdSipTransactionDispatcher.TestSendRequest;
begin
  Self.MarkSentRequestCount;

  Self.D.SendToTransport(Self.TranRequest, Self.Destination);

  CheckRequestSent('No Request sent');
end;

procedure TestTIdSipTransactionDispatcher.TestSendRequestOverUdp;
var
  UdpDest: TIdSipLocation;
begin
  UdpDest := TIdSipLocation.Create(UdpTransport, '127.0.0.1', IdPORT_SIP);
  try
    Self.MockTransport := Self.MockUdpTransport;

    Self.MarkSentRequestCount;

    Self.TranRequest.LastHop.Transport := Self.MockTransport.GetTransportType;
    Self.D.SendToTransport(Self.TranRequest, UdpDest);

    CheckRequestSent('No Request sent');
  finally
    UdpDest.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendResponse;
var
  Destinations: TIdSipLocations;
begin
  Self.MockTransport := Self.MockUdpTransport;
  Self.Response200.LastHop.Transport := Self.MockTransport.GetTransportType;

  Destinations := TIdSipLocations.Create;
  try
    Destinations.AddLocation(Self.MockTransport.GetTransportType,
                             '127.0.0.1',
                             Self.MockUdpTransport.DefaultPort);

    Self.MarkSentResponseCount;

    Self.D.SendToTransport(Self.Response200, Destinations);

    CheckResponseSent('No response sent');
  finally
    Destinations.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendMessageButNoAppropriateTransport;
var
  Destinations: TIdSipLocations;
begin
  Self.Response200.LastHop.Transport := 'SOME-UNKNOWN-TRANSPORT';

  Destinations := TIdSipLocations.Create;
  try
    Destinations.AddLocation('SOME-UNKNOWN-TRANSPORT',
                             '127.0.0.1',
                             Self.MockUdpTransport.DefaultPort);

    try
      Self.D.SendToTransport(Self.Response200, Destinations);
      Fail('Failed to bail out');
    except
      on EUnknownTransport do;
    end;
  finally
    Destinations.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendMessageWithAppropriateTransport;
var
  Destinations:     TIdSipLocations;
  TcpResponseCount: Cardinal;
  UdpResponseCount: Cardinal;
begin
  TcpResponseCount := Self.MockTcpTransport.SentResponseCount;
  UdpResponseCount := Self.MockUdpTransport.SentResponseCount;

  Self.Response200.LastHop.Transport := Self.MockUdpTransport.GetTransportType;

  Destinations := TIdSipLocations.Create;
  try
    Destinations.AddLocation(Self.MockUdpTransport.GetTransportType,
                             '127.0.0.1',
                             Self.MockUdpTransport.DefaultPort);

    Self.D.SendToTransport(Self.Response200, Destinations);
  finally
    Destinations.Free;
  end;

  Check(UdpResponseCount < Self.MockUdpTransport.SentResponseCount,
        'No response sent down UDP');
  CheckEquals(TcpResponseCount, Self.MockTcpTransport.SentResponseCount,
              'TCP response was sent');
end;

procedure TestTIdSipTransactionDispatcher.TestServerInviteTransactionGetsAck;
var
  Ack:        TIdSipRequest;
  Listener:   TIdSipTestTransactionListener;
  Response:   TIdSipResponse;
  ServerTran: TIdSipTransaction;
begin
  // We want to check that a server invite transaction gets its ACK.
  Listener := TIdSipTestTransactionListener.Create;
  try
    ServerTran := Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);
    ServerTran.AddTransactionListener(Listener);

    Response := Self.CreateMultipleChoices(Self.Invite);
    try
      ServerTran.SendResponse(Response);

      Ack := Self.CreateAck(Response);
      try
        Self.MockTransport.FireOnRequest(Ack);
        Check(Listener.ReceivedRequest,
              'Server INVITE transaction didn''t get its ACK');
      finally
        Ack.Free;
      end;
    finally
     Response.Free;
    end;
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestTransactionsCleanedUp;
var
  TranCount: Integer;
begin
  Self.D.AddServerTransaction(Self.TranRequest, Self.MockTransport);

  TranCount := Self.D.TransactionCount;

  // A 200 OK response terminates an INVITE transaction
  Self.D.SendResponse(Self.Response200);

  Check(Self.D.TransactionCount < TranCount,
        'Terminated transaction wasn''t cleaned up');
end;

procedure TestTIdSipTransactionDispatcher.TestWillUseReliableTransport;
const
  ViaValue = 'SIP/2.0/%s gw1.leo-ix.org;branch=z9hG4bK776asdhds';
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.AddHeader(ViaHeaderFull).Value := Format(ViaValue, ['TCP']);

    Check(Self.D.WillUseReliableTranport(R), 'TCP');

    R.LastHop.Value := Format(ViaValue, ['TLS']);
    Check(Self.D.WillUseReliableTranport(R), 'TLS');

    R.LastHop.Value := Format(ViaValue, ['UDP']);
    Check(not Self.D.WillUseReliableTranport(R), 'UDP');

    R.LastHop.Value := Format(ViaValue, ['SCTP']);
    Check(Self.D.WillUseReliableTranport(R), 'SCTP');
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestLocation                                                               *
//******************************************************************************
//* TestLocation Public methods ************************************************

procedure TestLocation.SetUp;
begin
  inherited SetUp;

  Self.L     := TIdSipMockLocator.Create;
  Self.Timer := TIdDebugTimerQueue.Create(false);

  // Remember, Self's subclass has registered mock transports for these symbols.
  Self.D := TIdSipTransactionDispatcher.Create(Self.Timer, Self.L);
  Self.D.AddTransportBinding(IdSipMessage.TcpTransport, '127.0.0.1', 5060);
  Self.D.AddTransportBinding(IdSipMessage.UdpTransport, '127.0.0.1', 5060);

  Self.TcpTransport := Self.D.Transports[0] as TIdSipMockTransport;
  Self.UdpTransport := Self.D.Transports[1] as TIdSipMockTransport;
  Self.MockTransport := Self.UdpTransport;

  Self.Request  := TIdSipTestResources.CreateBasicRequest;
  Self.Response := TIdSipResponse.InResponseTo(Self.Request, SIPNotFound);

  Self.L.AddSRV(Self.Response.LastHop.SentBy, SrvTcpPrefix,  0, 0, IdPORT_SIP, 'localhost');
  Self.L.AddA('localhost', '127.0.0.1');
end;

procedure TestLocation.TearDown;
begin
  Self.Response.Free;
  Self.Request.Free;

  Self.D.Free;
  Self.Timer.Terminate;
  Self.L.Free;

  inherited TearDown;
end;

//* TestLocation Published methods *********************************************

procedure TestLocation.TestCompleteNetworkFailure;
begin
  // What is this test actually supposed to do? It looks like it does nothing
  // at all. (2006/03/09)

  Self.MarkSentResponseCount;
  Self.MockTransport.FailWith := EIdConnectTimeout;
  CheckNoResponseSent('Response sent');
end;

procedure TestLocation.TestNetworkFailureTriesAlternateDestinations;
var
  DnsLookupCount: Cardinal;
  I:              Integer;
  Tran:           TIdSipTransaction;
begin
  // The transaction should try send the response to all the IPs returned by
  // the A/AAAA lookup of the SRV of the sent-by (phew!).

  Self.L.AddA('localhost', '127.0.0.2');

  Tran := Self.D.AddServerTransaction(Self.Request, Self.TcpTransport);

  DnsLookupCount := Self.L.LookupCount;
  Self.MarkSentResponseCount;

  for I := 0 to Self.L.NameRecords.Count - 1 do begin
    Tran.SendResponse(Self.Response);
    Tran.DoOnTransportError(Self.Response, 'Connection refused');
  end;

  CheckEquals(Self.RequestCount + Cardinal(Self.L.NameRecords.Count),
              Self.TcpTransport.SentResponseCount,
              'Number of locations tried');
  CheckEquals(DnsLookupCount + 1,
              Self.L.LookupCount,
              'DNS lookups');
end;

procedure TestLocation.TestNormalOperation;
var
  DnsLookupCount: Cardinal;
  Tran:           TIdSipTransaction;
begin
  Tran := Self.D.AddServerTransaction(Self.Request, Self.MockTransport);

  DnsLookupCount := Self.L.LookupCount;
  Self.MarkSentResponseCount;

  Tran.SendResponse(Self.Response);

  Check(Self.ResponseCount < Self.TcpTransport.SentResponseCount,
        'No response sent');
  CheckEquals(DnsLookupCount + 1, Self.L.LookupCount, 'DNS lookups');
end;

procedure TestLocation.TestTransactionsWontRelookupDnsForRetransmittedResponses;
var
  DnsLookupCount: Cardinal;
  Tran:           TIdSipTransaction;
begin
  // When a transaction retransmits a response, don't issue a fresh DNS query,
  // in the interests of reducing network congestion.
  Tran := Self.D.AddServerTransaction(Self.Request, Self.MockTransport);

  DnsLookupCount := Self.L.LookupCount;

  Self.Response.StatusCode := SIPTrying;
  Tran.SendResponse(Self.Response);
  Tran.SendResponse(Self.Response);

  CheckEquals(DnsLookupCount + 1,
              Self.L.LookupCount,
              'Transaction used fresh DNS queries for retransmitted response');
end;

procedure TestLocation.TestTransactionsWontRelookupDns;
var
  DnsLookupCount: Cardinal;
  Tran:           TIdSipTransaction;
begin
  // When a transaction sends a new response, it must make a fresh DNS query.
  Tran := Self.D.AddServerTransaction(Self.Request, Self.MockTransport);

  DnsLookupCount := Self.L.LookupCount;

  Self.Response.StatusCode := SIPTrying;
  Tran.SendResponse(Self.Response);

  Self.Response.StatusCode := SIPNotFound;
  Tran.SendResponse(Self.Response);

  CheckEquals(DnsLookupCount + 1,
              Self.L.LookupCount,
              'Transaction used fresh DNS queries for each response');
end;

//******************************************************************************
//* TestTIdSipTransaction                                                      *
//******************************************************************************
//* TestTIdSipTransaction Public methods ***************************************

procedure TestTIdSipTransaction.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher      := TIdSipMockTransactionDispatcher.Create;
  Self.Request         := TIdSipTestResources.CreateBasicRequest;
  Self.ReceivedRequest := TIdSipTestResources.CreateBasicRequest;
  Self.Response        := TIdSipTestResources.CreateBasicResponse;

  Self.Dispatcher.MockLocator.AddA(Self.Response.LastHop.SentBy, '127.0.0.1');
end;

procedure TestTIdSipTransaction.TearDown;
begin
  Self.Response.Free;
  Self.ReceivedRequest.Free;
  Self.Request.Free;
  Self.Dispatcher.Free;

  inherited TearDown;
end;

//* TestTIdSipTransaction Published methods ************************************

procedure TestTIdSipTransaction.TestAddTransactionListener;
var
  Listener: TIdSipTestTransactionListener;
  Tran:     TIdSipTransaction;
  Response: TIdSipResponse;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Tran := TIdSipClientInviteTransaction.Create(Self.Dispatcher, Self.Request);
    try
      Response := TIdSipResponse.Create;
      try
        Tran.AddTransactionListener(Listener);

        Response.StatusCode := SIPTrying;
        Tran.ReceiveResponse(Response, Self.Dispatcher.Transport);

        Check(Listener.ReceivedResponse, 'Listener wasn''t added');
      finally
        Response.Free;
      end;
    finally
      Tran.Free;
    end;
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransaction.TestAllListenersNotified;
var
  L1, L2:   TIdSipTestTransactionListener;
  Tran:     TIdSipTransaction;
  Response: TIdSipResponse;
begin
  L1 := TIdSipTestTransactionListener.Create;
  try
    L2 := TIdSipTestTransactionListener.Create;
    try
      Tran := TIdSipClientInviteTransaction.Create(Self.Dispatcher, Self.Request);
      try
        Response := TIdSipResponse.Create;
        try
          Tran.AddTransactionListener(L1);
          Tran.AddTransactionListener(L2);

          Response.StatusCode := SIPTrying;
          Tran.ReceiveResponse(Response, Self.Dispatcher.Transport);

          Check(L1.ReceivedResponse and L2.ReceivedResponse, 'Listener wasn''t added');
        finally
          Response.Free;
        end;
      finally
        Tran.Free;
      end;
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipTransaction.TestGetClientTransactionType;
var
  R: TIdSipRequest;
  T: TIdSipTransaction;
begin
  R := TIdSipTestResources.CreateBasicRequest;
  try
    R.Method := MethodInvite;

    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientInviteTransaction.ClassName,
                  T.ClassName,
                  'Client INVITE');
    finally
      T.Free;
    end;

    R.Method := MethodAck;
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client ACK');
    finally
      T.Free;
    end;

    R.Method := MethodBye;
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client BYE');
    finally
      T.Free;
    end;

    R.Method := MethodCancel;
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client CANCEL');
    finally
      T.Free;
    end;

    R.Method := MethodOptions;
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client OPTIONS');
    finally
      T.Free;
    end;

    R.Method := MethodRegister;
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client REGISTER');
    finally
      T.Free;
    end;

    R.Method := 'NewFangledMethod';
    T := TIdSipTransaction.CreateClientTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Client NewFangledMethod');
    finally
      T.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipTransaction.TestGetServerTransactionType;
var
  R: TIdSipRequest;
  T: TIdSipTransaction;
begin
  R := TIdSipRequest.Create;
  try
    R.Method := MethodInvite;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerInviteTransaction.ClassName,
                  T.ClassName,
                  'Server INVITE');
    finally
      T.Free;
    end;

    R.Method := MethodAck;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server ACK');
    finally
      T.Free;
    end;

    R.Method := MethodBye;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server BYE');
    finally
      T.Free;
    end;

    R.Method := MethodCancel;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server CANCEL');
    finally
      T.Free;
    end;

    R.Method := MethodOptions;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server OPTIONS');
    finally
      T.Free;
    end;

    R.Method := MethodRegister;
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server REGISTER');
    finally
      T.Free;
    end;

    R.Method := 'NewFangledMethod';
    T := TIdSipTransaction.CreateServerTransactionType(Self.Dispatcher, R);
    try
      CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                  T.ClassName,
                  'Server NewFangledMethod');
    finally
      T.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipTransaction.TestMatchInviteClient;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Check(Tran.Match(Self.Response),
        'Identical headers');

  Self.Response.AddHeader(ContentLanguageHeader).Value := 'es';
  Check(Tran.Match(Self.Response),
        'Identical headers + irrelevant headers');

  Self.Response.ToHeader.Tag := '1';
  Check(Tran.Match(Self.Response),
        'Different From tag');
  Self.Response.From.Assign(Self.Request.From);

  Self.Response.ToHeader.Tag := '1';
  Check(Tran.Match(Self.Response),
        'Different To tag');
end;

procedure TestTIdSipTransaction.TestMatchInviteClientDifferentCSeqMethod;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.Response.CSeq.Method := MethodCancel;

  Check(not Tran.Match(Self.Response),
        'Different CSeq method');
end;

procedure TestTIdSipTransaction.TestMatchInviteClientDifferentViaBranch;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.Response.LastHop.Branch := BranchMagicCookie + 'foo';

  Check(not Tran.Match(Self.Response),
        'Different Via branch');
end;

procedure TestTIdSipTransaction.TestMatchInviteClientAckWithInvite;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.Response.CSeq.Method := MethodAck;
  Check(Tran.Match(Self.Response),
        'ACK match against INVITE');
end;

procedure TestTIdSipTransaction.TestMatchInviteServer;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                             Self.Dispatcher.Transport);

  Check(Tran.Match(Self.Request),
        'Identical INVITE request');

  Self.ReceivedRequest.LastHop.SentBy := 'cougar';
  Check(not Tran.Match(Self.ReceivedRequest),
        'Different sent-by');
  Self.ReceivedRequest.LastHop.SentBy := Self.Request.LastHop.SentBy;

  Self.ReceivedRequest.LastHop.Branch := 'z9hG4bK6';
  Check(not Tran.Match(Self.ReceivedRequest),
        'Different branch');

  Self.ReceivedRequest.LastHop.Branch := Self.Request.LastHop.Branch;
  Self.ReceivedRequest.Method := MethodAck;
  Check(Tran.Match(Self.ReceivedRequest), 'ACK');

  Self.ReceivedRequest.LastHop.SentBy := 'cougar';
  Check(not Tran.Match(Self.ReceivedRequest),
        'ACK but different sent-by');
  Self.ReceivedRequest.LastHop.SentBy := Self.Request.LastHop.SentBy;

  Self.ReceivedRequest.LastHop.Branch := 'z9hG4bK6';
  Check(not Tran.Match(Self.ReceivedRequest),
        'ACK but different branch');
end;

procedure TestTIdSipTransaction.TestMatchNonInviteClient;
var
  Tran: TIdSipTransaction;
begin
  Self.Response.CSeq.Method := MethodRegister;
  Self.Request.Method       := MethodRegister;

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Check(Self.Request.Match(Self.Response),
        'Identical headers');

  Self.Response.ContentLanguage := 'es';
  Check(Tran.Match(Self.Response),
        'Identical headers + irrelevant headers');

  Self.Response.From.Tag := '1';
  Check(Tran.Match(Self.Response),
        'Different From tag');
  Self.Response.From := Self.Request.From;

  Self.Response.ToHeader.Tag := '1';
  Check(Tran.Match(Self.Response),
        'Different To tag');

  Self.Response.CSeq.Method := MethodOptions;
  Check(not Tran.Match(Self.Response),
        'Different method');
end;

procedure TestTIdSipTransaction.TestMatchNonInviteServer;
var
  Tran: TIdSipTransaction;
begin
  Self.ReceivedRequest.Method := MethodRegister;
  Self.Request.Method         := MethodRegister;

  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                             Self.Dispatcher.Transport);

  Check(Tran.Match(Self.ReceivedRequest),
        'Identical REGISTER request');

  Self.ReceivedRequest.Method := MethodOptions;
  Check(not Tran.Match(Self.ReceivedRequest),
        'Different method');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543Ack;
var
  Ack:  TIdSipRequest;
  Tran: TIdSipTransaction;
  R:    TIdSipResponse;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                               Self.Dispatcher.Transport);

  // RFC2543 matching depends on the last response the server sent.
  // And remember, a 200 OK in response to an INVITE will TERMINATE THE
  // TRANSACTION!
  R := TIdSipResponse.InResponseTo(Self.Request, SIPBusyHere);
  try
    Tran.SendResponse(R);

    Ack := Self.Request.AckFor(R);
    try
      Check(Tran.Match(Ack), 'ACK');
    finally
      Ack.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipTransaction.TestMatchRFC2543Invite;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                             Self.Dispatcher.Transport);

  Check(Tran.Match(Self.Request), 'Identical INVITE');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentCallID;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.CallID := '1' + Self.Request.CallID;
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing Call-ID');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentCSeq;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.CSeq.Increment;
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing CSeq');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentFromTag;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.From.Tag := Self.Request.From.Tag + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing From tag');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentRequestUri;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.RequestUri.Host := Self.Request.RequestUri.Host + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing Request-URI');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentToTag;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-RFC 3261 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.ToHeader.Tag := Self.Request.ToHeader.Tag + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing To tag');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentViaBranch;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-RFC 3261 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.LastHop.Branch := Self.Request.LastHop.Branch + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing top Via branch');
end;

procedure TestTIdSipTransaction.TestMatchRFC2543InviteDifferentViaSentBy;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.LastHop.SentBy := Self.Request.LastHop.SentBy + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing top Via');
end;

procedure TestTIdSipTransaction.TestRemoveTransactionListener;
var
  Listener: TIdSipTestTransactionListener;
  Tran:     TIdSipTransaction;
  Response: TIdSipResponse;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Tran := TIdSipClientInviteTransaction.Create(Self.Dispatcher, Self.Request);
    try
      Response := TIdSipResponse.Create;
      try
        Tran.AddTransactionListener(Listener);
        Tran.RemoveTransactionListener(Listener);

        Response.StatusCode := SIPTrying;
        Tran.ReceiveResponse(Response, Self.Dispatcher.Transport);

        Check(not Listener.ReceivedResponse, 'Listener wasn''t removed');
      finally
        Response.Free;
      end;
    finally
      Tran.Free;
    end;
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TTestTransaction                                                           *
//******************************************************************************
//* TTestTransaction Public methods ********************************************

procedure TTestTransaction.SetUp;
begin
  inherited SetUp;

  Self.CheckReceiveRequest  := nil;
  Self.CheckReceiveResponse := nil;

  Self.Core := TIdSipMockCore.Create;

  Self.Request := TIdSipTestResources.CreateBasicRequest;
  Self.Request.Path.Clear;
  Self.Request.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Request.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Request.ToHeader.Tag := '';

  Self.Response := TIdSipResponse.InResponseTo(Self.Request, SIPOK);

  Self.MockDispatcher := TIdSipMockTransactionDispatcher.Create;
  Self.MockLocator := Self.MockDispatcher.MockLocator;

  Self.MockTransport := Self.MockDispatcher.Transport;
  Self.MockTransport.HostName := 'gw1.leo-ix.org';

  Self.Tran := Self.TransactionType.Create(Self.MockDispatcher, Self.Request);
  Self.Tran.AddTransactionListener(Self);

  // This must differ from the dispatcher's bindings, or we will make hairpin
  // calls when we send INVITEs. That in itself isn't a problem, but for most
  // tests that's not what we want!
  Self.Destination := TIdSipLocation.Create(TcpTransport, '127.0.0.2', IdPORT_SIP);

  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionProceeding := false;
  Self.TransactionTerminated := false;

  Self.MockLocator.AddA(Self.MockTransport.HostName, '127.0.0.1');

  Self.FailMsg  := '';
end;

procedure TTestTransaction.TearDown;
begin
  Self.Destination.Free;
  Self.Tran.Free;
  Self.MockDispatcher.Free;
  Self.Response.Free;
  Self.Request.Free;
  Self.Core.Free;

  inherited TearDown;
end;

//* TTestTransaction Protected methods *****************************************

procedure TTestTransaction.Completed(Sender: TObject;
                                     R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
  Self.ThreadEvent.SetEvent;
end;

function TTestTransaction.DebugTimer: TIdDebugTimerQueue;
begin
  Result := Self.MockDispatcher.DebugTimer;
end;

procedure TTestTransaction.OnFail(Transaction: TIdSipTransaction;
                                  FailedMessage: TIdSipMessage;
                                  const Reason: String);
begin
  Self.FailMsg           := Reason;
  Self.TransactionFailed := true;
end;

procedure TTestTransaction.OnReceiveRequest(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport);
begin
  if Assigned(Self.CheckReceiveRequest) then
    Self.CheckReceiveRequest(Self, Request);
end;

procedure TTestTransaction.OnReceiveResponse(Response: TIdSipResponse;
                                             Transaction: TIdSipTransaction;
                                             Receiver: TIdSipTransport);
begin
  if Assigned(Self.CheckReceiveResponse) then
    Self.CheckReceiveResponse(Self, Response);
end;

procedure TTestTransaction.OnTerminated(Transaction: TIdSipTransaction);
begin
  if Assigned(Self.CheckTerminated) then
    Self.CheckTerminated(Transaction);
end;

procedure TTestTransaction.Proceeding(Sender: TObject;
                                      R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TTestTransaction.TransactionFail(Sender: TObject;
                                           const Reason: String);
begin
  Self.TransactionFailed := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TTestTransaction.Terminated(Sender: TIdSipTransaction);
begin
  Self.TransactionTerminated := true;
  Self.ThreadEvent.SetEvent;
end;

//* TTestTransaction Published methods *****************************************

procedure TTestTransaction.TestIsNull;
begin
  Check(not Self.Tran.IsNull, 'IsNull');
end;

//******************************************************************************
//* TestTIdSipServerInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipServerInviteTransaction Public methods ***************************

procedure TestTIdSipServerInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockTransport);

  Self.ServerTran := Self.Tran as TIdSipServerInviteTransaction;

  Self.TransactionConfirmed := false;
end;

//* TestTIdSipServerInviteTransaction Protected methods ************************

function TestTIdSipServerInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipServerInviteTransaction;
end;

//* TestTIdSipServerInviteTransaction Private methods **************************

procedure TestTIdSipServerInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipServerInviteTransaction.MoveToConfirmedState;
begin
  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'MoveToCompletedState precondition');

  Self.Request.Method := MethodAck;
  Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipServerInviteTransaction.MoveToTerminatedState;
begin
  Self.ServerTran.FireTimerI;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.ServerTran.State),
              'MoveToTerminatedState postcondition');
end;

procedure TestTIdSipServerInviteTransaction.OnInitialRequestSentToTU(Sender: TObject;
                                                                     R: TIdSipRequest);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipServerInviteTransaction.ReceiveInvite;
var
  R: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockTransport);

  CheckResponseSent('No response was sent');

  R := Self.MockTransport.LastResponse;
  CheckEquals(Self.Request.CallID,
              R.CallID,
              'Call-ID');
  Check(Self.Request.CSeq.Equals(R.CSeq),
              'CSeq');
  Check(R.Path.Equals(Self.Request.Path),
              'Via path differs');
  CheckEquals(Self.Request.ToHeader.Address,
              R.ToHeader.Address,
              'To address');
  Check(not R.ToHeader.HasTag,
              'To tag');
end;

procedure TestTIdSipServerInviteTransaction.Terminate(Tran: TIdSipTransaction);
begin
  Tran.SendResponse(Self.Response);
  Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');
end;

//* TestTIdSipServerInviteTransaction Published methods ************************

procedure TestTIdSipServerInviteTransaction.TestAuthenticationChallengeTreatedStatelessly;
var
  AuthChallenge: TIdSipResponse;
begin
  // cf RFC 3261, section 26.3.2.4

  AuthChallenge := TIdSipResponse.InResponseTo(Self.Request, SIPUnauthorized);
  try
    Self.ServerTran.SendResponse(AuthChallenge);

    Self.MarkSentResponseCount;
    Self.ServerTran.FireTimerG;
    CheckNoResponseSent('Authentication response resent (Timer G fired)');
  finally
    AuthChallenge.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestInitialRequestSentToTU;
var
  Tran: TIdSipTransaction;
begin
  Self.CheckReceiveRequest := Self.OnInitialRequestSentToTU;

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request, Self.MockTransport);
    Check(Self.TransactionProceeding, 'Initial request not sent to TU');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestInitialState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    CheckEquals(Transaction(itsProceeding),
                Transaction(Tran.State),
                'Incorrect initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestIsClient;
begin
  Check(not Self.Tran.IsClient, 'IsClient not false');
end;

procedure TestTIdSipServerInviteTransaction.TestIsInvite;
begin
  Check(Self.Tran.IsInvite, 'IsInvite not true');
end;

procedure TestTIdSipServerInviteTransaction.TestIsServer;
begin
  Check(Self.Tran.IsServer, 'IsServer');
end;

procedure TestTIdSipServerInviteTransaction.TestReceive2xxFromTUInProceedingState;
begin
  Self.MarkSentResponseCount;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              '200 from TU');

  CheckResponseSent('No response sent to transport layer');
  CheckEquals(SIPOK,
              Self.MockTransport.LastResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveAckInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Request.Method := MethodAck;
  Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              '200 from TU');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.CheckReceiveResponse := Self.Completed;
  Self.ReceiveInvite;

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'Received an INVITE');

  Check(not Self.TransactionCompleted, 'Event was needlessly fired');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInConfirmedState;
begin
  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Self.MarkSentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Tran.State),
              'Received an INVITE');

  CheckNoResponseSent('After receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInProceedingState;
var
  Trying: TIdSipResponse;
begin
  // The Transaction-User layer sends a 100 Trying
  Trying := TIdSipResponse.InResponseTo(Self.Request, SIPTrying);
  try
    Tran.SendResponse(Trying);
  finally
    Trying.Free;
  end;

  Self.CheckReceiveResponse := Self.Proceeding;
  Self.ReceiveInvite;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'Received an INVITE');

  Check(not Self.TransactionProceeding, 'Event was needlessly fired');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInTerminatedState;
begin
  Self.Response.StatusCode := SIPRinging;

  Self.Tran.SendResponse(Self.Response);
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Self.MarkSentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received an INVITE');

  CheckNoResponseSent('Response count after receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  StatusCode: Cardinal;
  Tran:       TIdSipTransaction;
begin
  for StatusCode := 3 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Self.TransactionCompleted := false;
      Self.MarkSentResponseCount;
      Tran.AddTransactionListener(Self);

      Tran.ReceiveRequest(Self.Request,
                          Self.MockTransport);

      Self.Response.StatusCode := StatusCode*100;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'Received a ' + IntToStr(StatusCode) + ' from TU');
      CheckResponseSent('Response not sent to transport layer');
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
begin
  Self.MarkSentResponseCount;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Non-trying provisional');

  CheckResponseSent('No response was sent to the transport layer');

  CheckEquals(SIPRinging,
              Self.MockTransport.LastResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipServerInviteTransaction.TestReliableTransportNoFinalResponseRetransmissions;
var
  Tran: TIdSipServerInviteTransaction;
begin
  // Self.Tran is set up in SetUp, but its initial request is UDP. We thus have
  // to recreate a transport using TLS, but we don't want Self.Tran to send any
  // messages. Hence we terminate it.
  Self.Terminate(Self.Tran);

  Self.MockDispatcher.TransportType := TlsTransport;
  Self.Request.LastHop.Transport := TlsTransport;

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipServerInviteTransaction;
  try
    Tran.ReceiveRequest(Self.Request,
                        Self.MockTransport);

    Self.Response.StatusCode := SIPMultipleChoices;
    Tran.SendResponse(Self.Response);
    Self.MockTransport.ResetSentResponseCount;

    Tran.FireTimerG;
    CheckEquals(0,
                Self.MockTransport.SentResponseCount,
                'Reliable transports should not resend final response');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReReceiveInitialRequestInCompletedState;
var
  FirstResponse:  TIdSipResponse;
  SecondResponse: TIdSipResponse;
begin
  FirstResponse := TIdSipResponse.Create;
  try
    Self.MoveToCompletedState;
    Self.MockTransport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockTransport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

    CheckEquals(1,
                Self.MockTransport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockTransport.LastResponse;
    Check(FirstResponse.Equals(SecondResponse),
          'Different response sent to initial request retransmission');
  finally
    FirstResponse.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestResponseRetransmissionInCompletedState;
begin
  Self.MoveToCompletedState;
  Self.MockTransport.ResetSentResponseCount;

  // cf. RFC 3261, section 17.2.1
  Self.ServerTran.FireTimerG;
  CheckEquals(1,
              Self.MockTransport.SentResponseCount,
              'Insufficient or too many responses sent');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGEventScheduled;
var
  Event:       TNotifyEvent;
  EventCount:  Integer;
  TimerGEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerG;

  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToCompletedState;

  // "+1" because entering Completed starts TWO timers - G and H.
  Check(EventCount < Self.DebugTimer.EventCount + 1, 'No events scheduled');

  // "-2" because, arbitrarily, the implementation adds TimerG and then
  // TimerH.
  TimerGEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(Assigned(TimerGEvent), 'Event not scheduled?');
  Check(TimerGEvent.MatchEvent(@Event),
        'Wrong notify event');
  CheckEquals(Self.MockDispatcher.T1Interval,
              TimerGEvent.DebugWaitTime,
              'Bad wait time');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGIntervalIncreases;
var
  Event:            TNotifyEvent;
  EventCount:       Integer;
  ExpectedInterval: Cardinal;
  FireCount:        Integer;
  I:                Integer;
  TimerGEvent:      TIdWait;
begin
  // TimerG starts at Dispatcher.T1Interval. It then exponentially increases
  // up to Dispatcher.T2Interval, where it remains constant.

  Event := Self.MockDispatcher.OnServerInviteTransactionTimerG;
  EventCount := Self.DebugTimer.EventCount;

  // First TimerG
  Self.MoveToCompletedState;

  // "+1" because entering Completed starts TWO timers - G and H.
  Check(EventCount < Self.DebugTimer.EventCount + 1, 'No event scheduled');

  // "-2" because, arbitrarily, the implementation adds TimerG and then
  // TimerH.
  //
  TimerGEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(Assigned(TimerGEvent), 'Event not scheduled?');
  Check(TimerGEvent.MatchEvent(@Event),
        'Wrong notify event');
  CheckEquals(Self.MockDispatcher.T1Interval,
              TimerGEvent.DebugWaitTime,
              'Bad wait time');

  FireCount := 2;
  ExpectedInterval := 2 * Self.MockDispatcher.T1Interval;
  while (TimerGEvent.DebugWaitTime < Self.MockDispatcher.T2Interval) do begin
    EventCount := Self.DebugTimer.EventCount;

    Self.ServerTran.FireTimerG;

    TimerGEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
    Check(EventCount < Self.DebugTimer.EventCount,
          'No event scheduled (' + IntToStr(FireCount) + ')');
    CheckEquals(ExpectedInterval,
                TimerGEvent.DebugWaitTime,
                'Bad wait time (' + IntToStr(FireCount) + ')');

    ExpectedInterval := 2 * ExpectedInterval;
    Inc(FireCount);
  end;

  ExpectedInterval := Self.MockDispatcher.T2Interval;
  for I := 1 to 5 do begin
    EventCount := Self.DebugTimer.EventCount;

    Self.ServerTran.FireTimerG;

    TimerGEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
    Check(EventCount < Self.DebugTimer.EventCount,
          'No event scheduled (' + IntToStr(FireCount) + ')');
    CheckEquals(ExpectedInterval,
                TimerGEvent.DebugWaitTime,
                'Bad wait time (' + IntToStr(FireCount) + ')');

    Inc(FireCount);
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGOnlyFiresInCompletedState;
var
  EventCount: Integer;
begin
  EventCount := Self.DebugTimer.EventCount;

  Self.ServerTran.FireTimerG;

  CheckEquals(EventCount,
              Self.DebugTimer.EventCount,
              'Timer G fired in Proceeding');

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  EventCount := Self.DebugTimer.EventCount;
  Self.ServerTran.FireTimerG;
  CheckEquals(EventCount,
              Self.DebugTimer.EventCount,
              'Timer G fired in Confirmed');

  Self.MoveToTerminatedState;
  Self.ServerTran.FireTimerG;
  CheckEquals(EventCount,
              Self.DebugTimer.EventCount,
              'Timer G fired in Confirmed');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGStops;
begin
  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Self.MockTransport.ResetSentResponseCount;
  Self.ServerTran.FireTimerG;
  CheckEquals(0,
              Self.MockTransport.SentResponseCount,
              'Timer G wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerHEventScheduled;
var
  Event:       TNotifyEvent;
  EventCount:  Integer;
  TimerHEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerH;
  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToCompletedState;

  // "+1" because entering Completed starts TWO timers - G and H.
  Check(EventCount < Self.DebugTimer.EventCount + 1, 'No events scheduled');

  // "-1" because, arbitrarily, the implementation adds TimerH and then
  // TimerH.
  TimerHEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(TimerHEvent.MatchEvent(@Event),
        'Wrong notify event');
  CheckEquals(Self.ServerTran.TimerHInterval,
              TimerHEvent.DebugWaitTime,
              'Bad wait time');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerHFired;
var
  Tran: TIdSipServerInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipServerInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request,
                        Self.MockTransport);

    Response.StatusCode := SIPMultipleChoices;
    Tran.SendResponse(Self.Response);

    Tran.FireTimerH;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Timeout');
    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerIEventScheduled;
var
  Event:       TNotifyEvent;
  EventCount:  Integer;
  LatestEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerI;
  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Check(EventCount < Self.DebugTimer.EventCount, 'No event scheduled');

  LatestEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(LatestEvent.MatchEvent(@Event),
        'Wrong notify event');
  CheckEquals(Self.ServerTran.TimerIInterval,
              LatestEvent.DebugWaitTime,
              'Bad wait time');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerIFired;
var
  Trying: TIdSipResponse;
begin
  // The Transaction-User layer sends a 100 Trying
  Trying := TIdSipResponse.InResponseTo(Self.Request, SIPTrying);
  try
    Tran.SendResponse(Trying);
  finally
    Trying.Free;
  end;

  Self.CheckTerminated := Self.Terminated;

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Self.ServerTran.FireTimerI;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
  Check(not Self.TransactionTerminated, 'OnTerminated fired');
end;

procedure TestTIdSipServerInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Response.StatusCode := SIPRinging;

  Self.Tran.SendResponse(Self.Response);
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.Response.StatusCode := SIPRinging;

  Self.Tran.SendResponse(Self.Response);
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerInviteTransaction.TestTransactionUserResponsesSentToTransport;
begin
  Self.MarkSentResponseCount;

  Self.Tran.SendResponse(Self.Response);

  CheckResponseSent('Response from TU not sent to transport');
end;

//******************************************************************************
//* TestTIdSipServerNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipServerNonInviteTransaction Public methods ************************

procedure TestTIdSipServerNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.Request.Method := MethodOptions;
  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockTransport);

  Self.TransactionTrying := false;
end;

//* TestTIdSipServerNonInviteTransaction Protected methods *********************

function TestTIdSipServerNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipServerNonInviteTransaction;
end;

//* TestTIdSipServerNonInviteTransaction Private methods ***********************

procedure TestTIdSipServerNonInviteTransaction.MoveToCompletedState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'MoveToCompletedState postcondition');

end;

procedure TestTIdSipServerNonInviteTransaction.MoveToProceedingState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipServerNonInviteTransaction.Trying(Sender: TObject;
                                                      R: TIdSipRequest);
begin
  Self.TransactionTrying := true;
  Self.ThreadEvent.SetEvent;
end;

//* TestTIdSipServerNonInviteTransaction Published methods *********************

procedure TestTIdSipServerNonInviteTransaction.TestInitialRequestSentToTU;
var
  Tran: TIdSipTransaction;
begin
  Self.CheckReceiveRequest := Self.Trying;

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request,
                        Self.MockTransport);

    Check(Self.TransactionTrying, 'TU not informed of initial request');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestInitialState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    CheckEquals(Transaction(itsTrying),
                Transaction(Tran.State),
                'Incorrect initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestIsClient;
begin
  Check(not Self.Tran.IsClient, 'IsClient not false');
end;

procedure TestTIdSipServerNonInviteTransaction.TestIsInvite;
begin
  Check(not Self.Tran.IsInvite, 'IsInvite not false');
end;

procedure TestTIdSipServerNonInviteTransaction.TestIsServer;
begin
  Check(Self.Tran.IsServer, 'IsServer');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.MarkSentResponseCount;

  Self.Response.StatusCode := SIPMultipleChoices;

  Self.Tran.SendResponse(Self.Response);
  Self.MockTransport.FireOnException(Self.LastSentResponse,
                                     EIdConnectException,
                                     '10061',
                                     'Connection refused');

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State - response from the TU not simply ignored');

  CheckNoResponseSent('SentResponseCount - response from the TU not simply ignored');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  I:    Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.ReceiveRequest(Self.Request,
                          Self.MockTransport);

      Self.MoveToProceedingState(Tran);
      Self.Response.StatusCode := I*100;
      Self.MockTransport.ResetSentResponseCount;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

      CheckEquals(1,
                  Self.MockTransport.SentResponseCount,
                  'Transport wasn''t given a '
                + IntToStr(Self.Response.StatusCode)
                + ' Response to send');
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInTerminatedState;
var
  Tran: TIdSipServerNonInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipServerNonInviteTransaction;
  try
    Tran.ReceiveRequest(Self.Request,
                        Self.MockTransport);
    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);

    Tran.FireTimerJ;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Transaction not yet timed out');

    Self.MarkSentResponseCount;

    Self.Response.StatusCode := SIPMultipleChoices;

    Tran.SendResponse(Self.Response);
    Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'State - response not simply ignored');

    CheckNoResponseSent('SentResponseCount - response not simply ignored');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInTryingState;
var
  I:    Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.ReceiveRequest(Self.Request,
                          Self.MockTransport);

      Self.MockTransport.ResetSentResponseCount;
      Self.Response.StatusCode := I*100;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

      CheckEquals(1,
                  Self.MockTransport.SentResponseCount,
                  'Transport wasn''t given a '
                + IntToStr(Self.Response.StatusCode)
                + ' Response to send');
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveProvisionalResponseFromTUInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.MockTransport.ResetSentResponseCount;
  // A different Status-Code to the response in MoveToProceedingState so that
  // the response isn't merely a retransmission.
  Self.Response.StatusCode := SIPSessionProgress;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

  CheckEquals(1,
              Self.MockTransport.SentResponseCount,
              'Transport wasn''t given a '
            + IntToStr(Self.Response.StatusCode)
            + ' Response to send');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveProvisionalResponseFromTUInTryingState;
begin
  Self.MockTransport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

  CheckEquals(1,
              Self.MockTransport.SentResponseCount,
              'Transport wasn''t given a '
            + IntToStr(Self.Response.StatusCode)
            + ' Response to send');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReReceiveInitialRequestInCompletedState;
var
  FirstResponse:  TIdSipResponse;
  SecondResponse: TIdSipResponse;
begin
  FirstResponse := TIdSipResponse.Create;
  try
    Self.MoveToProceedingState(Self.Tran);
    Self.MoveToCompletedState(Self.Tran);
    Self.MockTransport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockTransport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

    CheckEquals(1,
                Self.MockTransport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockTransport.LastResponse;
    Check(FirstResponse.Equals(SecondResponse),
          'Different response sent to initial request retransmission');
  finally
    FirstResponse.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReReceiveInitialRequestInProceedingState;
var
  FirstResponse:  TIdSipResponse;
  SecondResponse: TIdSipResponse;
begin
  FirstResponse := TIdSipResponse.Create;
  try
    Self.MoveToProceedingState(Self.Tran);
    Self.MockTransport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockTransport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);

    CheckEquals(1,
                Self.MockTransport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockTransport.LastResponse;
    Check(FirstResponse.Equals(SecondResponse),
          'Different response sent to initial request retransmission');
  finally
    FirstResponse.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestResponseFromTUInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.MockTransport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(0,
              Self.MockTransport.SentResponseCount,
              'Response from TU wasn''t dropped on the floor');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTimerJEventScheduled;
var
  Event:       TNotifyEvent;
  EventCount:  Integer;
  LatestEvent: TIdWait;
  Tran:        TIdSipServerNonInviteTransaction;
begin
  Tran := Self.Tran as TIdSipServerNonInviteTransaction;

  Event := Self.MockDispatcher.OnServerNonInviteTransactionTimerJ;
  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToProceedingState(Tran);
  Self.MoveToCompletedState(Tran);

  Check(EventCount < Self.DebugTimer.EventCount, 'No event scheduled');

  LatestEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(LatestEvent.MatchEvent(@Event),
        'Wrong notify event');
  CheckEquals(Tran.TimerJInterval,
              LatestEvent.DebugWaitTime,
              'Bad wait time');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTimerJFired;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Self.CheckTerminated := Self.Terminated;

    Tran.ReceiveRequest(Self.Request,
                        Self.MockTransport);

    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);

    (Tran as TIdSipServerNonInviteTransaction).FireTimerJ;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Terminated');

    CheckEquals('', Self.FailMsg, 'Unexpected fail');
    Check(not Self.TransactionTerminated, 'OnTerminated fired');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.Tran.ReceiveRequest(Self.Request, Self.MockTransport);
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.Response.StatusCode := SIPTrying;

  Self.Tran.SendResponse(Self.Response);
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTuResponsesSentToTransport;
begin
  Self.MarkSentResponseCount;

  Self.Tran.SendResponse(Self.Response);

  CheckResponseSent('Response from TU not sent to transport');
end;

//******************************************************************************
//* TestTIdSipClientInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipClientInviteTransaction Public methods ***************************

procedure TestTIdSipClientInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.Tran.SendRequest(Self.Destination);

  Self.ClientTran := Self.Tran as TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Protected methods ************************

procedure TestTIdSipClientInviteTransaction.Terminate(Tran: TIdSipTransaction);
begin
  Self.ClientTran.FireTimerA;
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');
end;

function TestTIdSipClientInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Private methods **************************

procedure TestTIdSipClientInviteTransaction.CheckACK(Ack: TIdSipRequest;
                                                     Response: TIdSipResponse);
begin
  CheckEquals(MethodAck,               Ack.Method,     'Method');
  CheckEquals(Self.Request.SipVersion, Ack.SipVersion, 'SIP-Version');
  CheckEquals(Self.Request.RequestUri, Ack.RequestUri, 'Request-URI');
  CheckEquals(Self.Request.CallID,     Ack.CallID,     'Call-ID');
  Check(Self.Request.From.Equals(Ack.From),
        'From');
  Check(Response.ToHeader.Equals(Ack.ToHeader),
        'To');

  CheckEquals(1, Ack.Path.Length, 'Number of Via headers');
  Check(Self.Request.LastHop.Equals(Ack.LastHop),
        'Topmost Via');

  Check(Ack.HasHeader(MaxForwardsHeader),
        'Max-Forwards header is mandatory');

  CheckEquals(Self.Request.CSeq.SequenceNo,
              Ack.CSeq.SequenceNo,
              'CSeq sequence no');
  CheckEquals(MethodAck,
              Ack.CSeq.Method,
              'CSeq method');

  CheckEquals(0,
              Ack.ContentLength,
              'Content-Length');
  CheckEquals('',
              Ack.Body,
              'RFC 3261 recommends having an empty ACK body');

  Check(Self.Tran.InitialRequest.Route.Equals(Ack.Route),
        'Route path differs');
end;

procedure TestTIdSipClientInviteTransaction.MoveToCompletedState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipClientInviteTransaction.MoveToProceedingState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsCalling),
              Transaction(Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState postcondition');
end;

//* TestTIdSipClientInviteTransaction Published methods ************************

procedure TestTIdSipClientInviteTransaction.TestACK;
begin
  Self.Tran.InitialRequest.AddHeader(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>';
  Self.Tran.InitialRequest.AddHeader(RouteHeader).Value := 'localhost <sip:127.0.0.1>';
  Self.MarkSentACKCount;

  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Sent ack');

  CheckAckSent('No ACK sent');
  Self.CheckACK(Self.LastSentACK,
                Self.Response);
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInCallingState;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerA;
  EventCount := Self.DebugTimer.EventCount;

  Self.ClientTran.FireTimerA;

  Check(EventCount < Self.DebugTimer.EventCount,
        'No event added');
  LastEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
  Check(LastEvent.MatchEvent(@Event),
        'Wrong event scheduled');
  // 2* cos SendRequest calls FireTimerA the first time
  CheckEquals(2*Self.MockDispatcher.T1Interval,
              LastEvent.DebugWaitTime,
              'Wrong time for the second Timer A event');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInCompletedState;
begin
  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);

  Self.MarkSentRequestCount;

  Self.ClientTran.FireTimerA;

  CheckNoRequestSent('Timer A fired in Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);

  Self.MarkSentRequestCount;

  Self.ClientTran.FireTimerA;

  CheckNoRequestSent('Timer A fired in Proceeding state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerBInCallingState;
begin
  Self.MarkSentACKCount;
  Self.ClientTran.FireTimerB;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Timer B didn''t fire in Calling state');
  CheckNoACKSent('RFC 3261 section 17.1.1.2: MUST NOT generate an ACK if Timer '
               + 'B fires in the Calling state.')
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerBInCompletedState;
begin
  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);
  Self.ClientTran.FireTimerB;

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.ClientTran.State),
              'Timer B fired in Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerBInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);
  Self.ClientTran.FireTimerB;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.ClientTran.State),
              'Timer B fired in Proceeding state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerDInCallingState;
begin
  Self.ClientTran.SendRequest(Self.Destination);
  Self.ClientTran.FireTimerD;

  CheckNotEquals(Transaction(itsTerminated),
                 Transaction(Self.ClientTran.State),
                 'Timer D fired in Calling state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerDInProceedingState;
begin
  Self.ClientTran.SendRequest(Self.Destination);
  Self.MoveToProceedingState(Self.ClientTran);
  Self.ClientTran.FireTimerD;

  CheckNotEquals(Transaction(itsTerminated),
                 Transaction(Self.ClientTran.State),
                 'Timer D fired in Proceeding state');
end;

procedure TestTIdSipClientInviteTransaction.TestInitialState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    CheckEquals(Transaction(itsCalling),
                Transaction(Tran.State),
                'Incorrect initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestInviteWithHostUnreachable;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Self.CheckTerminated := Self.Terminated;

    Tran.SendRequest(Self.Destination);

    Tran.DoOnTransportError(Tran.InitialRequest, 'Host unreachable');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Connection timed out');

    Check(Self.TransactionFailed,
          'Listener not told about failure');
    Check(Self.TransactionTerminated,
          'Listener not told about termination');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestIsClient;
begin
  Check(Self.Tran.IsClient, 'IsClient not true');
end;

procedure TestTIdSipClientInviteTransaction.TestIsInvite;
begin
  Check(Self.Tran.IsInvite, 'IsInvite not true');
end;

procedure TestTIdSipClientInviteTransaction.TestIsServer;
begin
  Check(not Self.Tran.IsServer, 'IsServer');
end;

procedure TestTIdSipClientInviteTransaction.TestMultipleInviteSending;
begin
  Self.ClientTran.FireTimerA;
  CheckEquals(2,
              Self.MockTransport.SentRequestCount,
              'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientInviteTransaction.TestNoInviteResendingInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MockTransport.ResetSentRequestCount;
  Self.ClientTran.FireTimerA;
  CheckEquals(0,
              Self.MockTransport.SentRequestCount,
              'Timer A wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransaction.TestNonInviteMethodInInitialRequest;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Self.Request.Method := MethodAck;

    try
      Tran.SendRequest(Self.Destination);
      Fail('Failed to bail out on non-INVITE method');
    except
    end;
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInCallingState;
begin
  Self.CheckReceiveResponse := Self.Proceeding;

  Self.MoveToProceedingState(Self.Tran);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.CheckReceiveResponse := Self.Proceeding;
  Self.Response.StatusCode := SIPRinging;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInTerminatedState;
begin
  // This test is a sanity check. We should never ever manage to get
  // a response in the Terminated state.

  Self.MoveToProceedingState(Self.Tran);

  Self.Response.StatusCode := SIPMultipleChoices;

  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
  Self.Tran.DoOnTransportError(Self.Tran.InitialRequest, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Transport layer failed');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Terminated state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCallingState;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Tran.AddTransactionListener(Listener);
    Self.MarkSentACKCount;

    Self.Response.StatusCode := SIPOK;

    Self.MarkSentRequestCount;
    Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
    CheckNoRequestSent('Transactions MUST NOT send an ACK to a 2xx - the TU does that');

    CheckNoACKSent('ACK sending arrogated by transaction');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Self.Tran.State),
                'State on receiving a 200');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.Response.StatusCode := SIPOK;

  Self.MarkSentRequestCount;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
  CheckNoRequestSent('Transactions MUST NOT send an ACK to a 2xx - the TU does that');

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 2xx in the Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInProceedingState;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.MoveToProceedingState(Self.Tran);
    Self.Tran.AddTransactionListener(Listener);
    Self.MarkSentACKCount;

    Self.Response.StatusCode := SIPOK;

    Self.MarkSentRequestCount;
    Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
    CheckNoRequestSent('Transactions MUST NOT send an ACK to a 2xx - the TU does that');

    CheckNoACKSent('ACK sending arrogated by transaction');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Self.Tran.State),
                'State on receiving a 200');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCallingState;
begin
  Self.CheckReceiveResponse := Self.Completed;
  Self.MarkSentACKCount;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckAckSent('Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;
  Self.MoveToCompletedState(Self.Tran);

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(2, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;
  Self.MarkSentACKCount;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckACKSent('Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceiveMultipleResponsesInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;

  Self.MoveToCompletedState(Self.Tran);
  Self.TransactionCompleted := false;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
  Check(not Self.TransactionCompleted, '2nd response was passed up to TU');
end;

procedure TestTIdSipClientInviteTransaction.TestReliableTransportNoInviteRetransmissions;
var
  Tran: TIdSipClientInviteTransaction;
begin
  // Hack: we terminate Self.Tran so it doesn't keep sending INVITEs
  Self.Terminate(Self.Tran);

  Self.MockDispatcher.TransportType := TcpTransport;
  Self.Request.LastHop.Transport    := Self.MockDispatcher.TransportType;

  Self.MockTransport.ResetSentRequestCount;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest(Self.Destination);

    Self.MockTransport.ResetSentRequestCount;
    Tran.FireTimerA;
    CheckEquals(0,
                Self.MockTransport.SentRequestCount,
                'Reliable transports should not resend INVITE');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestSendRequestSchedulesTimerA;
var
  Event: TNotifyEvent;
begin
  Check(Self.DebugTimer.EventCount > 1,
        'Not enough timers scheduled');

  Event := Self.MockDispatcher.OnClientInviteTransactionTimerA;

  Check(Self.DebugTimer.FirstEventScheduledFor(@Event).MatchEvent(@Event),
        'Wrong timer scheduled');
  CheckEquals(Self.MockDispatcher.T1Interval,
              Self.DebugTimer.FirstEventScheduledFor(@Event).DebugWaitTime,
              'Wrong time for the timer');
end;

procedure TestTIdSipClientInviteTransaction.TestSendRequestSchedulesTimerB;
var
  Event:       TNotifyEvent;
  EventCount:  Integer;
  TimerBEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerB;
  EventCount := Self.DebugTimer.EventCount;

  // "+1" because the transaction schedules TWO events
  Check(EventCount < Self.DebugTimer.EventCount + 1,
        'No event added');

  // "-2" because the implementation schedules timer B then timer D.
  Check(Self.DebugTimer.EventCount >= 1,
        'Not enough events scheduled - no timer B!');
  TimerBEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(TimerBEvent.MatchEvent(@Event),
        'Wrong event scheduled');
  CheckEquals(Self.ClientTran.TimerBInterval,
              TimerBEvent.DebugWaitTime,
              'Wrong time');
end;

procedure TestTIdSipClientInviteTransaction.TestSendRequestUntilTimeout;
var
  I:         Integer;
  Tran:      TIdSipTransaction;
  TranCount: Integer;
begin
  // I know, I know - this is really ugly. The test created a
  // ClientInviteTransaction, whose SendRequest scheduled two events -
  // FireTimerA and FireTimerB. We trigger these first.
  Self.DebugTimer.TriggerEarliestEvent;
  Self.DebugTimer.TriggerEarliestEvent;

  // We have to involve the dispatcher as it owns the scheduled events.
  Tran := Self.MockDispatcher.AddClientTransaction(Self.Request);
  Tran.SendRequest(Self.Destination);

  TranCount := Self.MockDispatcher.TransactionCount;

  // Until timeout occurs, there should be 7 requests sent (every time Timer A
  // fires, at times t=0, 0.5, 1.5, 3.5, 7.5, 15.5, 31.5).
  for I := 1 to 6 do
    Self.DebugTimer.TriggerEarliestEvent;

  CheckEquals(Transaction(itsCalling),
              Transaction(Tran.State),
              'After 6 resends');

  // Then Timer B should be scheduled, which this next line should do.
  Self.DebugTimer.TriggerEarliestEvent;

  Check(Self.MockDispatcher.TransactionCount < TranCount,
        'Transaction didn''t terminate; Timer B didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestTimerAIncreases;
var
  ExpectedInterval: Cardinal;
  Event:            TNotifyEvent;
  EventCount:       Integer;
  FireCount:        Integer;
  LastEvent:        TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerA;

  ExpectedInterval := 2*Self.MockDispatcher.T1Interval;
  for FireCount := 2 to 8 do begin
    EventCount := Self.DebugTimer.EventCount;

    Self.ClientTran.FireTimerA;

    Check(EventCount < Self.DebugTimer.EventCount,
          'No event added (' + IntToStr(FireCount) + ')');
    LastEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event added (' + IntToStr(FireCount) + ')');
    CheckEquals(ExpectedInterval,
                LastEvent.DebugWaitTime,
                'Wrong time added (' + IntToStr(FireCount) + ')');
    ExpectedInterval := 2 * ExpectedInterval;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDFired;
begin
  Self.CheckTerminated := Self.Terminated;

  Self.ClientTran.SendRequest(Self.Destination);

  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);
  Self.ClientTran.FireTimerD;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.ClientTran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
  Check(not Self.TransactionTerminated, 'OnTerminated fired');
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDScheduled;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerD;
  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToProceedingState(Tran);
  Self.MoveToCompletedState(Tran);

  Check(EventCount < Self.DebugTimer.EventCount,
        'No event added');
  LastEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(LastEvent.MatchEvent(@Event),
        'Wrong event scheduled');
  CheckEquals(Self.ClientTran.TimerDInterval,
              LastEvent.DebugWaitTime,
              'Wrong time');
end;

procedure TestTIdSipClientInviteTransaction.TestTimeout;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request)  as TIdSipClientInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Tran.SendRequest(Self.Destination);

    Tran.FireTimerB;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Timeout');

    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTransportErrorInCallingState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher, Self.Request);
  try
    Tran.AddTransactionListener(Self);

    Tran.SendRequest(Self.Destination);
    Tran.DoOnTransportError(Self.LastSentRequest, 'Connection refused');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Connection timed out');
    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);

  // This makes the transaction try send an ACK, which fails.
  Self.Response.StatusCode := SIPMultipleChoices;

  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);
  Self.Tran.DoOnTransportError(Self.LastSentRequest, 'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Connection timed out');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

//******************************************************************************
//* TestTIdSipClientNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipClientNonInviteTransaction Public methods ************************

procedure TestTIdSipClientNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.ClientTran := Self.Tran as TIdSipClientNonInviteTransaction;

  Self.Request.Method := MethodOptions;
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.ClientTran.SendRequest(Self.Destination);

  Self.ResponseReceived := false;
end;

//* TestTIdSipClientNonInviteTransaction Protected methods *********************

procedure TestTIdSipClientNonInviteTransaction.Terminate(Tran: TIdSipTransaction);
begin
  Self.ClientTran.FireTimerE;
  Self.Tran.DoOnTransportError(Self.Tran.LastResponse, 'Connection refused');
end;

function TestTIdSipClientNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientNonInviteTransaction;
end;

//* TestTIdSipClientNonInviteTransaction Private methods ***********************

procedure TestTIdSipClientNonInviteTransaction.AcknowledgeResponseReceipt(Sender: TObject;
                                                                          R: TIdSipResponse);
begin
  Self.ResponseReceived := true;
end;

procedure TestTIdSipClientNonInviteTransaction.MoveToProceedingState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipClientNonInviteTransaction.MoveToCompletedState(Tran: TIdSipTransaction);
begin
  Check(Self.Tran.State in [itsTrying, itsProceeding],
        'Unexpected state '
      + Transaction(Tran.State)
      + ' in MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPOK;
  Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipClientNonInviteTransaction.MoveToTerminatedState(Tran: TIdSipClientNonInviteTransaction);
begin
  Check(Self.Tran.State in [itsTrying, itsProceeding, itsCompleted],
        'Unexpected state '
      + Transaction(Tran.State)
      + ' in MoveToTerminatedState precondition');

  Tran.FireTimerE;
  Tran.DoOnTransportError(Self.LastSentRequest, 'Connection refused');
  
  CheckEquals(Transaction(itsTerminated),
              Transaction(Tran.State),
              'MoveToTerminatedState postcondition');
end;

//* TestTIdSipClientNonInviteTransaction Published methods *********************

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerEInCompletedState;
begin
  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);

  Self.MarkSentRequestCount;

  Self.ClientTran.FireTimerE;

  CheckNoRequestSent('Timer E fired in Completed state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerEInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);

  Self.MarkSentRequestCount;

  Self.ClientTran.FireTimerE;

  CheckRequestSent('Timer E didn''t fire in Proceeding state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerEInTryingState;
begin
  Self.MarkSentRequestCount;

  Self.ClientTran.FireTimerE;

  CheckRequestSent('Timer E didn''t fire in Trying state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerFInCompletedState;
begin
  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);

  Self.ClientTran.FireTimerF;

  Check(not Self.ClientTran.IsTerminated,
        'Timer F fired in Completed state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerFInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);

  Self.ClientTran.FireTimerF;

  Check(Self.ClientTran.IsTerminated,
        'Timer F didn''t fire in Proceeding state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerFInTryingState;
begin
  Self.ClientTran.FireTimerF;

  Check(Self.ClientTran.IsTerminated,
        'Timer F didn''t fire in Trying state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerKInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);

  Self.ClientTran.FireTimerK;

  Check(not Self.ClientTran.IsTerminated,
        'Timer K fired in Trying state terminated transaction');
end;

procedure TestTIdSipClientNonInviteTransaction.TestFireTimerKInTryingState;
begin
  Self.ClientTran.FireTimerK;
  Check(not Self.ClientTran.IsTerminated,
        'Timer K fired in Trying state terminated transaction');
end;

procedure TestTIdSipClientNonInviteTransaction.TestInitialRequestSent;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Self.MockTransport.ResetSentRequestCount;
    Tran.SendRequest(Self.Destination);
    CheckEquals(1,
                Self.MockTransport.SentRequestCount,
                'Too many or too few requests sent');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestInitialState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    CheckEquals(Transaction(itsTrying),
                Transaction(Tran.State),
                'Incorrect initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestIsClient;
begin
  Check(Self.Tran.IsClient, 'IsClient not true');
end;

procedure TestTIdSipClientNonInviteTransaction.TestIsInvite;
begin
  Check(not Self.Tran.IsInvite, 'IsInvite not false');
end;

procedure TestTIdSipClientNonInviteTransaction.TestIsServer;
begin
  Check(not Self.Tran.IsServer, 'IsServer');
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInProceedingState;
var
 Tran: TIdSipClientNonInviteTransaction;
begin
  Self.Terminate(Self.Tran);

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientNonInviteTransaction;
  try
    Tran.SendRequest(Self.Destination);
    Self.MoveToProceedingState(Tran);
    Self.MockTransport.ResetSentRequestCount;
    Tran.FireTimerE;
    CheckEquals(1,
                Self.MockTransport.SentRequestCount,
                'Insufficient or too many requests sent');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInTryingState;
var
 Tran: TIdSipClientNonInviteTransaction;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientNonInviteTransaction;
  try
    Tran.SendRequest(Self.Destination);
    Self.MockTransport.ResetSentRequestCount;
    Tran.FireTimerE;
    CheckEquals(1,
                Self.MockTransport.SentRequestCount,
                'Insufficient or too many requests sent');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.CheckReceiveResponse := Self.Completed;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  Check(not Self.TransactionCompleted,
        'Response not dropped');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Proceeding;

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockTransport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInTerminatedState;
begin
  // This test is slightly devious. First, responses are completely processed
  // one at a time, in the context of a TIdTimerQueue. Second, Self.Tran has
  // terminated, and section 17.1.2 says that this transaction must immediately
  // be destroyed. That means that the transaction shouldn't even exist so
  // could never receive a 1xx response in the terminated state. However,
  // we just check to make sure of that. The transaction should not receive
  // the response.

  Self.MoveToTerminatedState(Self.ClientTran);

  Self.CheckReceiveResponse := Self.AcknowledgeResponseReceipt;
  Self.Response.StatusCode := SIPSessionProgress;
  Self.ClientTran.ReceiveResponse(Self.Response, Self.MockTransport);

  Check(not Self.ResponseReceived,
        'Response not dropped');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInTryingState;
begin
  Self.CheckReceiveResponse := Self.Proceeding;
  Self.MoveToProceedingState(Self.Tran);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceiveFinalResponseInProceedingState;
var
  I:    Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.AddTransactionListener(Self);
      Self.TransactionCompleted := false;
      Tran.SendRequest(Self.Destination);
      Self.MoveToProceedingState(Tran);

      Self.CheckReceiveResponse := Self.Completed;
      Self.Response.StatusCode := 100*I;
      Tran.ReceiveResponse(Self.Response, Self.MockTransport);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'Received a ' + IntToStr(Self.Response.StatusCode)
                + ' in Trying state');

      Check(Self.TransactionCompleted,
            'OnReceiveResponse didn''t fire: '
          + IntToStr(Self.Response.StatusCode));
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceiveFinalResponseInTryingState;
var
  I:    Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.AddTransactionListener(Self);
      Self.TransactionCompleted := false;
      Tran.SendRequest(Self.Destination);

      Self.CheckReceiveResponse := Self.Completed;
      Self.Response.StatusCode := 100*I;
      Tran.ReceiveResponse(Self.Response, Self.MockTransport);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

      Check(Self.TransactionCompleted,
            'OnReceiveResponse didn''t fire: '
          + IntToStr(Self.Response.StatusCode));
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerEIntervalInProceedingRemainsConstant;
var
  ExpectedInterval: Cardinal;
  Event:            TNotifyEvent;
  EventCount:       Integer;
  FireCount:        Integer;
  LastEvent:        TIdWait;
begin
  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerE;
  Self.MoveToProceedingState(Self.ClientTran);

  ExpectedInterval := Self.MockDispatcher.T2Interval;
  for FireCount := 2 to 8 do begin
    EventCount := Self.DebugTimer.EventCount;

    Self.ClientTran.FireTimerE;

    Check(EventCount < Self.DebugTimer.EventCount,
          'No event added (' + IntToStr(FireCount) + ')');
    LastEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event added (' + IntToStr(FireCount) + ')');
    CheckEquals(ExpectedInterval,
                LastEvent.DebugWaitTime,
                'Wrong time added (' + IntToStr(FireCount) + ')');
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerEIntervalInTryingIncreases;
var
  ExpectedInterval: Cardinal;
  Event:            TNotifyEvent;
  EventCount:       Integer;
  FireCount:        Integer;
  LastEvent:        TIdWait;
begin
  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerE;
  ExpectedInterval := 2*Self.MockDispatcher.T1Interval;

  for FireCount := 2 to 8 do begin
    EventCount := Self.DebugTimer.EventCount;

    Self.ClientTran.FireTimerE;

    Check(EventCount < Self.DebugTimer.EventCount,
          'No event added (' + IntToStr(FireCount) + ')');
    LastEvent := Self.DebugTimer.LastEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event added (' + IntToStr(FireCount) + ')');
    CheckEquals(ExpectedInterval,
                LastEvent.DebugWaitTime,
                'Wrong time added (' + IntToStr(FireCount) + ')');

    ExpectedInterval := 2 * ExpectedInterval;
    if (ExpectedInterval > Self.MockDispatcher.T2Interval) then
      ExpectedInterval := Self.MockDispatcher.T2Interval;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerEScheduled;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
  Tran:       TIdSipClientNonInviteTransaction;
begin
  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerE;
  EventCount := Self.DebugTimer.EventCount;

  // Timer E only fires for unreliable transports (cf RFC 3261, section
  // 17.1.2.2)
  Self.Request.LastHop.Transport := UdpTransport;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientNonInviteTransaction;
  try
    Tran.SendRequest(Self.Destination);


    // "+1" and "-1" because the transaction will always add Timer F. Last.
    Check(EventCount + 1 < Self.DebugTimer.EventCount,
          'No event added');
    LastEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event scheduled');
    CheckEquals(Tran.TimerEInterval,
                LastEvent.DebugWaitTime,
                'Wrong time');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerEScheduledOnlyForUnreliableTransports;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
  Tran:       TIdSipClientNonInviteTransaction;
begin
  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerF;

  // This disables the Timer E stuff (cf RFC 3261, section 17.1.2.2)
  Self.Request.LastHop.Transport := TcpTransport;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientNonInviteTransaction;
  try
    EventCount := Self.DebugTimer.EventCount;
    Tran.SendRequest(Self.Destination);

    CheckEquals(EventCount + 1,
                Self.DebugTimer.EventCount,
                'Timer E scheduled');
    LastEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event scheduled');
    CheckEquals(Tran.TimerFInterval,
                LastEvent.DebugWaitTime,
                'Wrong time');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerFScheduled;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
  Tran:       TIdSipClientNonInviteTransaction;
begin

  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerF;
  EventCount := Self.DebugTimer.EventCount;

  // This disables the Timer E stuff (cf RFC 3261, section 17.1.2.2)
  Self.Request.LastHop.Transport := TcpTransport;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientNonInviteTransaction;
  try
    Tran.SendRequest(Self.Destination);

    Check(EventCount < Self.DebugTimer.EventCount,
          'No event added');
    LastEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
    Check(LastEvent.MatchEvent(@Event),
          'Wrong event scheduled');
    CheckEquals(Tran.TimerFInterval,
                LastEvent.DebugWaitTime,
                'Wrong time');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerKFired;
begin
  Self.CheckTerminated := Self.Terminated;

  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);

  Self.ClientTran.FireTimerK;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.ClientTran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
  Check(not Self.TransactionTerminated, 'OnTerminated fired');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerKScheduled;
var
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
begin
  Event := Self.MockDispatcher.OnClientNonInviteTransactionTimerK;
  EventCount := Self.DebugTimer.EventCount;

  Self.MoveToProceedingState(Self.ClientTran);
  Self.MoveToCompletedState(Self.ClientTran);

  Check(EventCount < Self.DebugTimer.EventCount,
        'No event added');
  LastEvent := Self.DebugTimer.FirstEventScheduledFor(@Event);
  Check(LastEvent.MatchEvent(@Event),
        'Wrong event scheduled');
  CheckEquals(Self.ClientTran.TimerKInterval,
              LastEvent.DebugWaitTime,
              'Wrong time');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.MoveToProceedingState(Self.ClientTran);

  // When Timer E fires, the transaction resends the request.
  Self.ClientTran.FireTimerE;
  Self.ClientTran.DoOnTransportError(Self.LastSentRequest,
                                     'Connection refused');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.ClientTran.State),
              'Connection timed out');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTransportErrorInTryingState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);

    Tran.SendRequest(Self.Destination);
    Tran.DoOnTransportError(Self.LastSentRequest,
                            'Connection refused');

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Connection timed out');
    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipResponseLocationsList                                             *
//******************************************************************************
//* TestTIdSipResponseLocationsList Public methods ******************************

procedure TestTIdSipResponseLocationsList.SetUp;
begin
  inherited SetUp;

  Self.List      := TIdSipResponseLocationsList.Create;
  Self.Locations := TIdSipLocations.Create;
  Self.Response  := TIdSipTestResources.CreateBasicResponse;

  Self.Locations.AddLocation('UDP', '127.0.0.1', 5060);
  Self.Locations.AddLocation('TCP', '127.0.0.1', 5060);
end;

procedure TestTIdSipResponseLocationsList.TearDown;
begin
  Self.List.Free;
  Self.Locations.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipResponseLocationsList Published methods ***************************

procedure TestTIdSipResponseLocationsList.TestAddAndLocationsFor;
var
  I:      Integer;
  RepLoc: TIdSipLocations;
begin
  Self.List.Add(Self.Response, Self.Locations);

  RepLoc := Self.List.LocationsFor(Response);

  Check(Assigned(RepLoc),
        'LocationsFor returned nil');
  Check(RepLoc <> Self.Locations,
        'Add didn''t add a COPY of the locations, but stored a reference '
      + 'instead');
  CheckEquals(Self.Locations.Count,
              RepLoc.Count,
              'Response/Locations not added');

  for I := 0 to Self.Locations.Count - 1 do
    CheckEquals(Self.Locations[I].AsString,
                RepLoc[I].AsString,
                IntToStr(I) + 'th location differs');
end;

procedure TestTIdSipResponseLocationsList.TestContains;
begin
  Check(not Self.List.Contains(Self.Response),
        'Empty list');

  Self.List.Add(Self.Response, Self.Locations);
  Check(Self.List.Contains(Self.Response),
        'But we added the Response');
end;

procedure TestTIdSipResponseLocationsList.TestLocationsForReturnsMutableList;
var
  OldCount: Integer;
  RepLoc:   TIdSipLocations;
begin
  Self.List.Add(Self.Response, Self.Locations);

  RepLoc := Self.List.LocationsFor(Response);

  OldCount := RepLoc.Count;
  RepLoc.AddLocation(RepLoc.First);
  CheckEquals(OldCount + 1,
              RepLoc.Count,
              'LocationsFor returned an immutable list');
end;

//******************************************************************************
//* TTransactionDispatcherListenerMethodTestCase                               *
//******************************************************************************
//* TTransactionDispatcherListenerMethodTestCase Public methods ****************

procedure TTransactionDispatcherListenerMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipMockUdpTransport.Create;
end;

procedure TTransactionDispatcherListenerMethodTestCase.TearDown;
begin
  Self.Receiver.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTransactionDispatcherListenerReceiveRequestMethod                *
//******************************************************************************
//* TestTIdSipTransactionDispatcherListenerReceiveRequestMethod Public methods *

procedure TestTIdSipTransactionDispatcherListenerReceiveRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipTransactionDispatcherListenerReceiveRequestMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Request  := Self.Request;
end;

procedure TestTIdSipTransactionDispatcherListenerReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionDispatcherListenerReceiveRequestMethod Published methods

procedure TestTIdSipTransactionDispatcherListenerReceiveRequestMethod.TestRun;
var
  Listener: TIdSipTestTransactionDispatcherListener;
begin
  Listener := TIdSipTestTransactionDispatcherListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRequest,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          Self.ClassName + ': Receiver param');
    Check(Self.Method.Request = Listener.RequestParam,
          Self.ClassName + ': Request param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransactionDispatcherListenerReceiveResponseMethod               *
//******************************************************************************
//* TestTIdSipTransactionDispatcherListenerReceiveResponseMethod Public methods

procedure TestTIdSipTransactionDispatcherListenerReceiveResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipTransactionDispatcherListenerReceiveResponseMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Response  := Self.Response;
end;

procedure TestTIdSipTransactionDispatcherListenerReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionDispatcherListenerReceiveResponseMethod Published methods

procedure TestTIdSipTransactionDispatcherListenerReceiveResponseMethod.TestRun;
var
  Listener: TIdSipTestTransactionDispatcherListener;
begin
  Listener := TIdSipTestTransactionDispatcherListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedResponse,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          Self.ClassName + ': Receiver param');
    Check(Self.Method.Response = Listener.ResponseParam,
          Self.ClassName + ': Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TTransactionListenerMethodTestCase                                         *
//******************************************************************************
//* TTransactionListenerMethodTestCase Public methods **************************

procedure TTransactionListenerMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.Request := TIdSipTestResources.CreateLocalLoopRequest;
  Self.Request.Method := MethodOptions;

  Self.Transaction := TIdSipClientNonInviteTransaction.Create(Self.Dispatcher,
                                                              Self.Request);
end;

procedure TTransactionListenerMethodTestCase.TearDown;
begin
  Self.Transaction.Free;
  Self.Request.Free;
  Self.Dispatcher.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipTransactionListenerFailMethod                                    *
//******************************************************************************
//* TestTIdSipTransactionListenerFailMethod Public methods *********************

procedure TestTIdSipTransactionListenerFailMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipTransactionListenerFailMethod.Create;
  Self.Method.Reason      := 'Foo';
  Self.Method.Transaction := Self.Transaction;
end;

procedure TestTIdSipTransactionListenerFailMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionListenerFailMethod Published methods ******************

procedure TestTIdSipTransactionListenerFailMethod.TestRun;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Failed,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Transaction = Listener.TransactionParam,
          Self.ClassName + ': Transaction param');
    CheckEquals(Self.Method.Reason,
                Listener.ReasonParam,
                Self.ClassName + ': Reason param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransactionListenerReceiveRequestMethod                          *
//******************************************************************************
//* TestTIdSipTransactionListenerReceiveRequestMethod Public methods ***********

procedure TestTIdSipTransactionListenerReceiveRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipTransactionListenerReceiveRequestMethod.Create;
  Self.Method.Receiver    := Self.Dispatcher.Transport;
  Self.Method.Request     := Self.Request;
  Self.Method.Transaction := Self.Transaction;
end;

procedure TestTIdSipTransactionListenerReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionListenerReceiveRequestMethod Published methods ********

procedure TestTIdSipTransactionListenerReceiveRequestMethod.TestRun;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedRequest,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          Self.ClassName + ': Receiver param');
    Check(Self.Method.Transaction = Listener.TransactionParam,
          Self.ClassName + ': Transaction param');
    Check(Self.Method.Request = Listener.RequestParam,
          Self.ClassName + ': Request param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransactionListenerReceiveResponseMethod                         *
//******************************************************************************
//* TestTIdSipTransactionListenerReceiveResponseMethod Public methods **********

procedure TestTIdSipTransactionListenerReceiveResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipTestResources.CreateLocalLoopResponse;

  Self.Method := TIdSipTransactionListenerReceiveResponseMethod.Create;
  Self.Method.Receiver    := Self.Dispatcher.Transport;
  Self.Method.Response     := Self.Response;
  Self.Method.Transaction := Self.Transaction;
end;

procedure TestTIdSipTransactionListenerReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionListenerReceiveResponseMethod Published methods *******

procedure TestTIdSipTransactionListenerReceiveResponseMethod.TestRun;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.ReceivedResponse,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Receiver = Listener.ReceiverParam,
          Self.ClassName + ': Receiver param');
    Check(Self.Method.Transaction = Listener.TransactionParam,
          Self.ClassName + ': Transaction param');
    Check(Self.Method.Response = Listener.ResponseParam,
          Self.ClassName + ': Response param');
  finally
    Listener.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipTransactionListenerTerminatedMethod                              *
//******************************************************************************
//* TestTIdSipTransactionListenerTerminatedMethod Public methods ***************

procedure TestTIdSipTransactionListenerTerminatedMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipTransactionListenerTerminatedMethod.Create;
  Self.Method.Transaction := Self.Transaction;
end;

procedure TestTIdSipTransactionListenerTerminatedMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionListenerTerminatedMethod Published methods ******************

procedure TestTIdSipTransactionListenerTerminatedMethod.TestRun;
var
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Terminated,
          Self.ClassName + ': Listener not notified');
    Check(Self.Method.Transaction = Listener.TransactionParam,
          Self.ClassName + ': Transaction param');
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
