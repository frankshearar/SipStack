unit TestIdSipTransaction;

interface

uses
  IdSipCore, IdSipDialog, IdSipMessage, IdSipMockCore,
  IdSipMockTransactionDispatcher, IdSipMockTransport, IdSipTcpClient,
  IdSipTransaction, IdSipTransport, TestFramework, TestFrameworkSip;

type
  TestTIdSipTransactionDispatcher = class(TTestCase,
                                          IIdSipSessionListener,
                                          IIdSipTransactionListener)
  private
    Core:                   TIdSipMockCore;
    D:                      TIdSipTransactionDispatcher;
    Invite:                 TIdSipRequest;
    MockTransport:          TIdSipMockTransport;
    OnReceiveResponseFired: Boolean;
    OnTerminatedFired:      Boolean;
    Options:                TIdSipRequest;
    ReceivedRequest:        TIdSipRequest;
    ReceivedResponse:       TIdSipResponse;
    Response200:            TIdSipResponse;
    TranRequest:            TIdSipRequest;

    function  CreateAck(Response: TIdSipResponse): TIdSipRequest;
    function  CreateMultipleChoices(Request: TIdSipRequest): TIdSipResponse;
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Transport: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAckDoesntCreateATransaction;
    procedure TestAddAndCountTransport;
    procedure TestClearTransports;
    procedure TestCreateNewTransaction;
    procedure TestAckHandedUpToTU;
    procedure TestAddClientTransaction;
    procedure TestAddServerTransaction;
    procedure TestDispatchToCorrectTransaction;
    procedure TestDispatcherDoesntGetTransactionRequests;
    procedure TestDispatcherDoesntGetTransactionResponses;
    procedure TestHandUnmatchedRequestToCore;
    procedure TestHandUnmatchedResponseToCore;
    procedure TestLoopDetected;
    procedure TestSendRequest;
    procedure TestSendRequestOverTcp;
    procedure TestSendRequestOverTls;
    procedure TestSendResponse;
    procedure TestSendMessageButNoAppropriateTransport;
    procedure TestSendMessageWithAppropriateTransport;
    procedure TestSendVeryBigMessageWithTcpFailure;
    procedure TestSendVeryBigRequest;
    procedure TestServerInviteTransactionGetsAck;
    procedure TestTransactionsCleanedUp;
    procedure TestUnmatchableCancel;
    procedure TestWillUseReliableTransport;
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
    procedure TestMatchCancel;
    procedure TestMatchCancelDifferentViaBranch;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteServer;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
    procedure TestMatchSip1Ack;
    procedure TestMatchSip1Invite;
    procedure TestMatchSip1InviteDifferentCallID;
    procedure TestMatchSip1InviteDifferentCSeq;
    procedure TestMatchSip1InviteDifferentFromTag;
    procedure TestMatchSip1InviteDifferentRequestUri;
    procedure TestMatchSip1InviteDifferentToTag;
    procedure TestMatchSip1InviteDifferentViaBranch;
    procedure TestMatchSip1InviteDifferentViaSentBy;
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
  TTestTransaction = class(TTestCaseSip, IIdSipTransactionListener)
  protected
    CheckReceiveRequest:   TTestIdSipRequestEvent;
    CheckReceiveResponse:  TTestIdSipResponseEvent;
    CheckTerminated:       TIdSipTransactionEvent;
    Core:                  TIdSipAbstractCore;
    FailMsg:               String;
    MockDispatcher:        TIdSipMockTransactionDispatcher;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    Tran:                  TIdSipTransaction;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionProceeding: Boolean;
    TransactionTerminated: Boolean;

    procedure Completed(Sender: TObject;
                        R: TIdSipResponse);
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Transport: TIdSipTransport);
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
    procedure TestReceiveCancel; virtual;
  end;

  TestTIdSipServerInviteTransaction = class(TTestTransaction)
  private
    TransactionConfirmed: Boolean;

    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure OnInitialRequestSentToTU(Sender: TObject;
                                       R: TIdSipRequest);
    procedure ReceiveInvite;
    procedure Terminate(Tran: TIdSipTransaction);
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
    procedure TestReceive2xxFromTUInProceedingState;
    procedure TestReceiveAckInCompletedState;
    procedure TestReceiveCancel; override;
    procedure TestReceiveCancelAfterFinalResponse;
    procedure TestReceiveCancelTwice;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveInviteInCompletedState;
    procedure TestReceiveInviteInConfirmedState;
    procedure TestReceiveInviteInProceedingState;
    procedure TestReceiveInviteInTerminatedState;
    procedure TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
    procedure TestReliableTransportNoFinalResponseRetransmissions;
    procedure TestReReceiveInitialRequestInCompletedState;
    procedure TestResponseRetransmissionInCompletedState;
    procedure TestSending100;
    procedure TestTimerGStops;
    procedure TestTimerHFired;
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
    procedure TestTimerJFired;
    procedure TestTransportErrorInCompletedState;
    procedure TestTransportErrorInProceedingState;
    procedure TestTuResponsesSentToTransport;
  end;

  TestTIdSipClientInviteTransaction = class(TTestTransaction)
  private
    ClientInviteTran: TIdSipClientInviteTransaction;

    procedure CheckACK(Sender: TObject;
                       R: TIdSipResponse);
    procedure MoveToCompletedState(Tran: TIdSipTransaction);
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
  protected
    procedure Terminate(Tran: TIdSipTransaction);
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestACK;
    procedure TestCancel;
    procedure TestCancelAfterFinalResponse;
    procedure TestCancelBeforeProvisionalResponse;
    procedure TestInitialState;
    procedure TestInviteWithHostUnreachable;
    procedure TestIsClient;
    procedure TestIsInvite;
    procedure TestIsServer;
    procedure TestMultipleInviteSending;
    procedure TestNoInviteResendingInProceedingState;
    procedure TestNonInviteMethodInInitialRequest;
    procedure TestPrematureDestruction;
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
    procedure TestTimerDFired;
    procedure TestTimeout;
    procedure TestTransportErrorInCompletedState;
  end;

  TestTIdSipClientNonInviteTransaction = class(TTestTransaction)
  private
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
    procedure MoveToCompletedState(Tran: TIdSipTransaction);
  protected
    procedure Terminate(Tran: TIdSipTransaction);
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
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
    procedure TestTimerKFired;
    procedure TestTransportErrorInTryingState;
  end;

  TTestTimer = class(TTestCase)
  protected
    Dispatcher: TIdSipMockTransactionDispatcher;
    Request:    TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipServerInviteTransactionTimer = class(TTestTimer)
  private
    Timer:       TIdSipServerInviteTransactionTimer;
    Transaction: TIdSipServerInviteTransaction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompletedStartsTimerG;
    procedure TestCompletedStartsTimerH;
    procedure TestConfirmedStopsTimerG;
    procedure TestConfirmedStopsTimerH;
    procedure TestConfirmedStartsTimerI;
    procedure TestTerminateStopsAllTimers;
    procedure TestTimerHInterval;
  end;

  TestTIdSipClientInviteTransactionTimer = class(TTestTimer)
  private
    Timer:       TIdSipClientInviteTransactionTimer;
    Transaction: TIdSipClientInviteTransaction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCallingLeavesTimerARunning;
    procedure TestCallingLeavesTimerBRunning;
    procedure TestCancel;
    procedure TestCancelBeforeProvisionalResponse;
    procedure TestCompletedStartsTimerD;
    procedure TestCompletedStopsTimerA;
    procedure TestCompletedStopsTimerB;
    procedure TestInitialTimerAInterval;
    procedure TestInitialTimerAIntervalDoubles;
    procedure TestProceedingStopsTimerA;
    procedure TestProceedingStopsTimerB;
    procedure TestTerminateStopsAllTimers;
    procedure TestTimerAInitiallyStarted;
    procedure TestTimerBInitiallyStarted;
  end;

  TestTIdSipClientNonInviteTransactionTimer = class(TTestTimer)
  private
    Timer:       TIdSipClientNonInviteTransactionTimer;
    Transaction: TIdSipClientNonInviteTransaction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompletedStartsTimerK;
    procedure TestCompletedStopsTimerE;
    procedure TestCompletedStopsTimerF;
    procedure TestInitialTimerEInterval;
    procedure TestProceedingLeavesTimerFRunning;
    procedure TestProceedingTimerEInterval;
    procedure TestTerminatedStopsAllTimers;
    procedure TestTimerEInitiallyStarted;
    procedure TestTimerFInitiallyStarted;
    procedure TestTimerKInterval;
    procedure TestTryingLeavesTimerERunning;
    procedure TestTryingLeavesTimerFRunning;
    procedure TestTryingTimerEInterval;
  end;

implementation

uses
  IdException, IdSdp, IdSipConsts, Math, SysUtils, TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipTransactionDispatcher.Suite);
  Result.AddTest(TestTIdSipTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransactionTimer.Suite);
  Result.AddTest(TestTIdSipClientInviteTransactionTimer.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransactionTimer.Suite);
end;

function Transaction(S: TIdSipTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipTransactionState), Integer(S));
end;

//******************************************************************************
//* TestTIdSipTransactionDispatcher                                            *
//******************************************************************************
//* TestTIdSipTransactionDispatcher Public methods *****************************

procedure TestTIdSipTransactionDispatcher.SetUp;
begin
  inherited SetUp;

  Self.Core := TIdSipMockCore.Create;

  Self.D := TIdSipTransactionDispatcher.Create;

  Self.Core.Dispatcher := Self.D;

  Self.MockTransport := TIdSipMockTransport.Create(IdPORT_SIP);

  Self.MockTransport.TransportType := sttTCP;
  Self.D.AddTransport(Self.MockTransport);

  Self.ReceivedRequest  := TIdSipTestResources.CreateLocalLoopRequest;
  Self.TranRequest      := TIdSipTestResources.CreateLocalLoopRequest;
  Self.ReceivedResponse := TIdSipTestResources.CreateLocalLoopResponse;

  Self.ReceivedResponse.StatusCode := SIPTrying;
  Self.ReceivedResponse.AddHeaders(Self.ReceivedRequest.Headers);

  Self.Invite := TIdSipRequest.Create;
  Self.Invite.Assign(Self.ReceivedRequest);
  Self.Invite.Body := '';
  Self.Invite.ContentType := SdpMimeType;

  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.ReceivedRequest);
  Self.Options.Method := MethodOptions;

  Self.Response200 := TIdSipResponse.Create;
  Self.Response200.Assign(Self.ReceivedResponse);
  Self.Response200.StatusCode := SIPOK;

  Self.OnReceiveResponseFired := false;
  Self.OnTerminatedFired      := false;
end;

procedure TestTIdSipTransactionDispatcher.TearDown;
begin
  Self.Response200.Free;
  Self.Options.Free;
  Self.Invite.Free;
  Self.ReceivedResponse.Free;
  Self.TranRequest.Free;
  Self.ReceivedRequest.Free;

  Self.D.Free;
  Self.MockTransport.Free;
  Self.Core.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionDispatcher Private methods ****************************

function TestTIdSipTransactionDispatcher.CreateAck(Response: TIdSipResponse): TIdSipRequest;
begin
  Result := Self.Invite.AckFor(Response);
end;

function TestTIdSipTransactionDispatcher.CreateMultipleChoices(Request: TIdSipRequest): TIdSipResponse;
var
  UA: TIdSipUserAgentCore;
begin
  UA := TIdSipUserAgentCore.Create;
  try
    Result := UA.CreateResponse(Request, SIPMultipleChoices);
  finally
    UA.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.OnEndedSession(Session: TIdSipSession;
                                                         const Reason: String);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnEstablishedSession(Session: TIdSipSession);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnFail(Transaction: TIdSipTransaction;
                                                 const Reason: String);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnModifiedSession(Session: TIdSipSession;
                                                            Invite: TIdSipRequest);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                           Transaction: TIdSipTransaction;
                                                           Transport: TIdSipTransport);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                            Transaction: TIdSipTransaction;
                                                            Transport: TIdSipTransport);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnReceiveResponseFired := true;
end;

procedure TestTIdSipTransactionDispatcher.OnTerminated(Transaction: TIdSipTransaction);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnTerminatedFired := true;
end;

//* TestTIdSipTransactionDispatcher Published methods **************************

procedure TestTIdSipTransactionDispatcher.TestAckDoesntCreateATransaction;
var
  Ack: TIdSipRequest;
begin
  Ack := TIdSipRequest.Create;
  try
    Ack.Method := MethodAck;

    Self.MockTransport.FireOnRequest(Ack);

    CheckEquals(0, Self.D.TransactionCount, 'ACK created a transaction');
  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestAddAndCountTransport;
var
  OriginalCount: Cardinal;
  T1, T2:        TIdSipMockTransport;
begin
  OriginalCount := Self.D.TransportCount;

  T1 := TIdSipMockTransport.Create(IdPORT_SIP);
  try
    Self.D.AddTransport(T1);
    CheckEquals(OriginalCount + 1, Self.D.TransportCount, 'After one AddTransport');

    T2 := TIdSipMockTransport.Create(IdPORT_SIP);
    try
      Self.D.AddTransport(T1);
      CheckEquals(OriginalCount + 2, Self.D.TransportCount, 'After two AddTransports');
    finally
      T2.Free;
    end;
  finally
    T1.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestClearTransports;
var
  T1, T2: TIdSipMockTransport;
begin
  T1 := TIdSipMockTransport.Create(IdPORT_SIP);
  try
    Self.D.AddTransport(T1);

    T2 := TIdSipMockTransport.Create(IdPORT_SIP);
    try
      Self.D.AddTransport(T1);

      Self.D.ClearTransports;
      CheckEquals(0, Self.D.TransportCount, 'After Clear');
    finally
      T2.Free;
    end;
  finally
    T1.Free;
  end;
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

procedure TestTIdSipTransactionDispatcher.TestAckHandedUpToTU;
var
  Ack:      TIdSipRequest;
  Listener: TIdSipTestUnhandledMessageListener;
  Tran:     TIdSipTransaction;
begin
  Ack := Self.CreateAck(Self.Response200);
  try
    Tran := Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);

    Listener := TIdSipTestUnhandledMessageListener.Create;
    try
      Self.D.AddUnhandledMessageListener(Listener);

      Tran.SendResponse(Self.Response200);

      Self.MockTransport.FireOnRequest(Ack);
      // cf RFC 3261 section 13.3.1.4 - the Transaction User layer is
      // responsible for handling ACKs to a 2xx response!
      Check(Listener.ReceivedUnhandledRequest,
            'ACK not handed up to TU');
    finally
      Listener.Free;
    end;
  finally
    Ack.Free;
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
  Listener: TIdSipTestUnhandledMessageListener;
begin
  Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);

  Listener := TIdSipTestUnhandledMessageListener.Create;
  try
    Self.D.AddUnhandledMessageListener(Listener);
    Self.MockTransport.FireOnRequest(Self.Invite);

    Check(not Listener.ReceivedUnhandledRequest,
          'Dispatcher said it got an unhandled request');
  finally
    Listener.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestDispatcherDoesntGetTransactionResponses;
var
  Listener: TIdSipTestUnhandledMessageListener;
begin
  Self.D.AddClientTransaction(Self.Invite);

  Listener := TIdSipTestUnhandledMessageListener.Create;
  try
    Self.D.AddUnhandledMessageListener(Listener);
    Self.MockTransport.FireOnResponse(Self.ReceivedResponse);

    Check(not Listener.ReceivedUnhandledResponse,
          'Dispatcher said it got an unhandled response');
  finally
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

procedure TestTIdSipTransactionDispatcher.TestSendRequest;
var
  RequestCount: Cardinal;
begin
  RequestCount := Self.MockTransport.SentRequestCount;

  Self.D.Send(Self.TranRequest);

  CheckEquals(RequestCount + 1,
              Self.MockTransport.SentRequestCount,
              'No Request sent');
end;

procedure TestTIdSipTransactionDispatcher.TestSendRequestOverTcp;
var
  RequestCount: Cardinal;
begin
  Self.MockTransport.TransportType := sttTCP;

  RequestCount := Self.MockTransport.SentRequestCount;

  Self.TranRequest.LastHop.Transport := Self.MockTransport.TransportType;
  Self.D.Send(Self.TranRequest);

  CheckEquals(RequestCount + 1,
              Self.MockTransport.SentRequestCount,
              'No Request sent');
end;

procedure TestTIdSipTransactionDispatcher.TestSendRequestOverTls;
var
  RequestCount: Cardinal;
begin
  Self.MockTransport.TransportType := sttTLS;

  RequestCount := Self.MockTransport.SentRequestCount;

  Self.TranRequest.LastHop.Transport := Self.MockTransport.TransportType;
  Self.D.Send(Self.TranRequest);

  CheckEquals(RequestCount + 1,
              Self.MockTransport.SentRequestCount,
              'No Request sent');
end;

procedure TestTIdSipTransactionDispatcher.TestSendResponse;
var
  ResponseCount: Cardinal;
begin
  Self.MockTransport.TransportType := Self.Response200.LastHop.Transport;

  ResponseCount := Self.MockTransport.SentResponseCount;

  Self.D.Send(Self.Response200);

  CheckEquals(ResponseCount + 1,
              Self.MockTransport.SentResponseCount,
              'No response sent');
end;

procedure TestTIdSipTransactionDispatcher.TestSendMessageButNoAppropriateTransport;
begin
  Self.Response200.LastHop.Transport := sttTCP;
  Self.MockTransport.TransportType   := sttSCTP;

  try
    Self.D.Send(Self.Response200);
    Fail('Failed to bail out');
  except
    on EUnknownTransport do;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendMessageWithAppropriateTransport;
var
  TcpResponseCount: Cardinal;
  UdpResponseCount: Cardinal;
  UdpTran:          TIdSipMockTransport;
begin
  UdpTran := TIdSipMockTransport.Create(IdPORT_SIP);
  try
    UdpTran.TransportType := sttUDP;

    Self.D.AddTransport(UdpTran);

    TcpResponseCount := Self.MockTransport.SentResponseCount;
    UdpResponseCount := UdpTran.SentResponseCount;

    Self.Response200.LastHop.Transport := UdpTran.TransportType;
    Self.D.Send(Self.Response200);

    Check(UdpResponseCount < UdpTran.SentResponseCount,
          'No response sent down UDP');
    CheckEquals(TcpResponseCount, Self.MockTransport.SentResponseCount,
                'TCP response was sent');
  finally
    UdpTran.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendVeryBigMessageWithTcpFailure;
var
  TcpResponseCount: Cardinal;
  UdpResponseCount: Cardinal;
  UdpTran:          TIdSipMockTransport;
begin
  Self.MockTransport.TransportType := sttTCP;
  Self.MockTransport.FailWith      := EIdConnectTimeout;

  UdpTran := TIdSipMockTransport.Create(IdPORT_SIP);
  try
    UdpTran.TransportType := sttUDP;

    Self.D.AddTransport(UdpTran);

    TcpResponseCount := Self.MockTransport.SentResponseCount;
    UdpResponseCount := UdpTran.SentResponseCount;

    while (Length(Self.Response200.AsString) < MaximumUDPMessageSize) do
      Self.Response200.AddHeader(SubjectHeaderFull).Value := 'In R''lyeh dead Cthulhu lies dreaming';

    Self.Response200.LastHop.Transport := UdpTran.TransportType;
    Self.D.Send(Self.Response200);

    Check(UdpResponseCount < UdpTran.SentResponseCount,
          'No response sent down UDP');
    CheckEquals(TcpResponseCount, Self.MockTransport.SentResponseCount,
                'TCP response was sent');
  finally
    UdpTran.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestSendVeryBigRequest;
var
  TcpRequestCount: Cardinal;
  UdpRequestCount: Cardinal;
  UdpTran:         TIdSipMockTransport;
begin
  Self.MockTransport.TransportType := sttTCP;

  UdpTran := TIdSipMockTransport.Create(IdPORT_SIP);
  try
    UdpTran.TransportType := sttUDP;

    Self.D.AddTransport(UdpTran);

    TcpRequestCount := Self.MockTransport.SentRequestCount;
    UdpRequestCount := UdpTran.SentRequestCount;

    Self.TranRequest.LastHop.Transport := sttUDP;
    while (Length(Self.TranRequest.AsString) < MaximumUDPMessageSize) do
      Self.TranRequest.AddHeader(SubjectHeaderFull).Value := 'That is not dead which can eternal lie, '
                                                           + 'and with strange aeons even death may die.';

    Self.D.Send(Self.TranRequest);

    CheckEquals(UdpRequestCount, UdpTran.SentRequestCount,
                'UDP response was sent');
    Check(TcpRequestCount < Self.MockTransport.SentRequestCount,
          'No response sent down TCP');
  finally
    UdpTran.Free;
  end;
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
  Tran:      TIdSipTransaction;
  TranCount: Integer;
begin
  Tran := Self.D.AddServerTransaction(Self.TranRequest, Self.MockTransport);
  Tran.ReceiveRequest(Self.TranRequest, Self.MockTransport);
  TranCount := Self.D.TransactionCount;

  Tran.SendResponse(Self.Response200);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Tran.State),
              'Transaction wasn''t terminated');
  Check(Self.D.TransactionCount < TranCount,
        'Terminated transaction wasn''t cleaned up');
end;

procedure TestTIdSipTransactionDispatcher.TestUnmatchableCancel;
var
  Cancel:        TIdSipRequest;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockTransport.SentResponseCount;

  Cancel := Self.TranRequest.CreateCancel;
  try
    Self.MockTransport.FireOnRequest(Cancel);

    Check(ResponseCount < Self.MockTransport.SentResponseCount,
          'No response sent');

    CheckEquals(SIPCallLegOrTransactionDoesNotExist,
                Self.MockTransport.LastResponse.StatusCode,
                'Wrong response sent');
  finally
    Cancel.Free;
  end;
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
        Tran.ReceiveResponse(Response, nil);

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
          Tran.ReceiveResponse(Response, nil);

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
  R := TIdSipRequest.Create;
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

procedure TestTIdSipTransaction.TestMatchCancel;
var
  Cancel: TIdSipRequest;
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Cancel := Self.Request.CreateCancel;
  try
    Check(Tran.Match(Cancel), 'INVITE doesn''t match its CANCEL');
  finally
    Cancel.Free;
  end;
end;

procedure TestTIdSipTransaction.TestMatchCancelDifferentViaBranch;
var
  Cancel: TIdSipRequest;
  Tran: TIdSipTransaction;
begin
  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Cancel := Self.Request.CreateCancel;
  try
    Cancel.LastHop.Branch := Cancel.LastHop.Branch + '1';
    Check(not Tran.Match(Cancel), 'CANCEL matches a non-associated INVITE');
  finally
    Cancel.Free;
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

procedure TestTIdSipTransaction.TestMatchSip1Ack;
var
  Ack:  TIdSipRequest;
  Tran: TIdSipTransaction;
  R:    TIdSipResponse;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                             Self.Dispatcher.Transport);

  // SIP/1.0 matching depends on the last response the server sent.
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

procedure TestTIdSipTransaction.TestMatchSip1Invite;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddServerTransaction(Self.Request,
                                             Self.Dispatcher.Transport);

  Check(Tran.Match(Self.Request), 'Identical INVITE');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentCallID;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.CallID := '1' + Self.Request.CallID;
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing Call-ID');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentCSeq;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.CSeq.Increment;
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing CSeq');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentFromTag;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.From.Tag := Self.Request.From.Tag + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing From tag');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentRequestUri;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.RequestUri.Host := Self.Request.RequestUri.Host + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing Request-URI');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentToTag;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.ToHeader.Tag := Self.Request.ToHeader.Tag + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing To tag');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentViaBranch;
var
  Tran: TIdSipTransaction;
begin
  Self.Request.LastHop.Branch := '1'; // Some arbitrary non-SIP/2.0 branch

  Tran := Self.Dispatcher.AddClientTransaction(Self.Request);

  Self.ReceivedRequest.LastHop.Branch := Self.Request.LastHop.Branch + '1';
  Check(not Tran.Match(Self.ReceivedRequest), 'Differing top Via branch');
end;

procedure TestTIdSipTransaction.TestMatchSip1InviteDifferentViaSentBy;
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
        Tran.ReceiveResponse(Response, nil);

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
  Self.MockDispatcher.Transport.TransportType := sttUDP;
  Self.MockDispatcher.Transport.HostName      := 'gw1.leo-ix.org';

  Self.Tran := Self.TransactionType.Create(Self.MockDispatcher, Self.Request);
  Self.Tran.AddTransactionListener(Self);
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionProceeding := false;
  Self.TransactionTerminated := false;

  Self.FailMsg := '';
end;

procedure TTestTransaction.TearDown;
begin
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

procedure TTestTransaction.OnFail(Transaction: TIdSipTransaction;
                                  const Reason: String);
begin
  Self.FailMsg           := Reason;
  Self.TransactionFailed := true;
end;

procedure TTestTransaction.OnReceiveRequest(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction;
                                            Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckReceiveRequest) then
    Self.CheckReceiveRequest(Self, Request);
end;

procedure TTestTransaction.OnReceiveResponse(Response: TIdSipResponse;
                                             Transaction: TIdSipTransaction;
                                             Transport: TIdSipTransport);
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

procedure TTestTransaction.TestReceiveCancel;
var
  Cancel:        TIdSipRequest;
  ResponseCount: Cardinal;
begin
  // We ignore all CANCELs to anything other than a server INVITE transaction.

  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Cancel := Self.Tran.InitialRequest.CreateCancel;
  try
    Self.Tran.ReceiveCancel(Cancel, Self.MockDispatcher.Transport);

    CheckEquals(ResponseCount + 1,
                Self.MockDispatcher.Transport.SentResponseCount,
                Self.Tran.ClassName
              + ': UAS didn''t sent a response to a CANCEL');
    CheckEquals(SIPOK,
                Self.MockDispatcher.Transport.LastResponse.StatusCode,
                Self.Tran.ClassName + ': Wrong response sent');
  finally
    Cancel.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipServerInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipServerInviteTransaction Public methods ***************************

procedure TestTIdSipServerInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockDispatcher.Transport);

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
  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipServerInviteTransaction.OnInitialRequestSentToTU(Sender: TObject;
                                                                     R: TIdSipRequest);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipServerInviteTransaction.ReceiveInvite;
var
  R:             TIdSipResponse;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockDispatcher.Transport);

  Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
        'No response was sent');

  R := Self.MockDispatcher.Transport.LastResponse;
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
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Tran.SendResponse(Self.Response);
  Self.MockDispatcher.Transport.FailWith := nil;
end;

//* TestTIdSipServerInviteTransaction Published methods ************************

procedure TestTIdSipServerInviteTransaction.TestInitialRequestSentToTU;
var
  Tran: TIdSipTransaction;
begin
  Self.CheckReceiveRequest := Self.OnInitialRequestSentToTU;

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);
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
var
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              '200 from TU');

  Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
        'No response sent to transport layer');
  CheckEquals(SIPOK,
              Self.MockDispatcher.Transport.LastResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveAckInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Request.Method := MethodAck;
  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              '200 from TU');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveCancel;
var
  Cancel:        TIdSipRequest;
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Cancel := Self.Tran.InitialRequest.CreateCancel;
  try
    Self.Tran.ReceiveCancel(Cancel, Self.MockDispatcher.Transport);

    CheckEquals(ResponseCount +2,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Either the CANCEL transaction or the INVITE transaction '
              + 'didn''t send a response');
    CheckEquals(SIPRequestTerminated,
                Self.MockDispatcher.Transport.LastResponse.StatusCode,
                'Transaction sent wrong response');

    CheckEquals(SIPOK,
                Self.MockDispatcher.Transport.SecondLastResponse.StatusCode,
                'CANCEL transaction sent wrong response');

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'CANCEL should complete the transaction');
  finally
    Cancel.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveCancelAfterFinalResponse;
var
  Cancel:        TIdSipRequest;
  ResponseCount: Cardinal;
begin
  Self.MoveToCompletedState;
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Cancel := Self.Tran.InitialRequest.CreateCancel;
  try
    Self.Tran.ReceiveCancel(Cancel, Self.MockDispatcher.Transport);

    CheckEquals(ResponseCount + 1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'A transaction can''t be CANCELled once its sent a '
              + 'final response');
    CheckEquals(MethodCancel,
                Self.MockDispatcher.Transport.LastResponse.CSeq.Method,
                'The transaction shouldn''t send another response');
    CheckEquals(SIPOK,
                Self.MockDispatcher.Transport.LastResponse.StatusCode,
                'The UAS MUST send an OK in response to the CANCEL');

  finally
    Cancel.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveCancelTwice;
var
  Cancel:        TIdSipRequest;
  ResponseCount: Cardinal;
begin
  Self.MoveToCompletedState;
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Cancel := Self.Tran.InitialRequest.CreateCancel;
  try
    Self.Tran.ReceiveCancel(Cancel, Self.MockDispatcher.Transport);
    Self.Tran.ReceiveCancel(Cancel, Self.MockDispatcher.Transport);

    CheckEquals(ResponseCount + 2,
                Self.MockDispatcher.Transport.SentResponseCount,
                'A transaction can''t be CANCELled once its sent a '
              + 'final response');
    CheckEquals(SIPOK,
                Self.MockDispatcher.Transport.LastResponse.StatusCode,
                'The UAS MUST send an OK in response to the CANCEL (1st)');
    CheckEquals(SIPOK,
                Self.MockDispatcher.Transport.SecondLastResponse.StatusCode,
                'The UAS MUST send an OK in response to the CANCEL (2nd)');

  finally
    Cancel.Free;
  end;
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
var
  SentResponseCount: Cardinal;
begin
  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Tran.State),
              'Received an INVITE');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'After receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInProceedingState;
begin
  Self.CheckReceiveResponse := Self.Proceeding;
  Self.ReceiveInvite;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'Received an INVITE');

  Check(not Self.TransactionProceeding, 'Event was needlessly fired');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInTerminatedState;
var
  SentResponseCount: Cardinal;
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received an INVITE');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response count after receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  ResponseCount: Cardinal;
  StatusCode:    Cardinal;
  Tran:          TIdSipTransaction;
begin
  for StatusCode := 3 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Self.TransactionCompleted := false;
      ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;
      Tran.AddTransactionListener(Self);

      Tran.ReceiveRequest(Self.Request,
                          Self.MockDispatcher.Transport);

      Self.Response.StatusCode := StatusCode*100;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'Received a ' + IntToStr(StatusCode) + ' from TU');
      Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
            'Response not sent to transport layer');            
    finally
      Tran.Free;
    end;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
var
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Non-trying provisional');

  Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
        'No response was sent to the transport layer');

  CheckEquals(SIPRinging,
              Self.MockDispatcher.Transport.LastResponse.StatusCode,
              'Unexpected response sent');
end;

procedure TestTIdSipServerInviteTransaction.TestReliableTransportNoFinalResponseRetransmissions;
var
  Tran: TIdSipTransaction;
begin
  // Self.Tran is set up in SetUp, but its initial request is UDP. We thus have
  // to recreate a transport using TLS, but we don't want Self.Tran to send any
  // messages. Hence we terminate it.
  Self.Terminate(Self.Tran);

  Self.MockDispatcher.Transport.TransportType := sttTLS;
  Self.Request.LastHop.Transport := sttTLS;

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);

    Self.Response.StatusCode := SIPMultipleChoices;
    Tran.SendResponse(Self.Response);
    Self.MockDispatcher.Transport.ResetSentResponseCount;

    (Tran as TIdSipServerInviteTransaction).FireTimerG;
    CheckEquals(0,
                Self.MockDispatcher.Transport.SentResponseCount,
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
    Self.MockDispatcher.Transport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockDispatcher.Transport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

    CheckEquals(1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockDispatcher.Transport.LastResponse;
    Check(FirstResponse.Equals(SecondResponse),
          'Different response sent to initial request retransmission');
  finally
    FirstResponse.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestResponseRetransmissionInCompletedState;
begin
  Self.MoveToCompletedState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  // cf. RFC 3261, section 17.2.1
  (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;
  CheckEquals(1,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Insufficient or too many responses sent');
end;

procedure TestTIdSipServerInviteTransaction.TestSending100;
var
  R:             TIdSipResponse;
  ResponseCount: Cardinal;
  Tran:          TIdSipTransaction;
begin
  Self.Request.AddHeader(TimestampHeader).Value := '100';

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

    Tran.ReceiveRequest(Self.Request,
                             Self.MockDispatcher.Transport);

    Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
          'No response sent');

    R := Self.MockDispatcher.Transport.LastResponse;

    CheckEquals(SipVersion,  R.SipVersion, 'SIP-Version');
    CheckEquals(SIPTrying,   R.StatusCode, 'Status-Code');
    CheckEquals(RSSIPTrying, R.StatusText, 'Status-Text');

    CheckEquals(R.CallID,
                Self.Request.CallID,
                'Call-ID headers must match');
    Check(R.CSeq.Equals(Self.Request.CSeq),
                'CSeq headers must match');
    Check(R.From.Equals(Self.Request.From),
                'From headers must match');
    Check(R.ToHeader.Equals(Self.Request.ToHeader),
                'To headers must match');
    Check(R.Path.Equals(Self.Request.Path),
                'Via headers must match and have identical order');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGStops;
begin
  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Self.MockDispatcher.Transport.ResetSentResponseCount;
  (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Timer G wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerHFired;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);

    Response.StatusCode := SIPMultipleChoices;
    Tran.SendResponse(Self.Response);

    (Tran as TIdSipServerInviteTransaction).FireTimerH;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Timeout');
    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerIFired;
begin
  Self.CheckTerminated := Self.Terminated;

  Self.Tran.ReceiveRequest(Self.Request,
                           Self.MockDispatcher.Transport);

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  (Tran as TIdSipServerInviteTransaction).FireTimerI;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
  Check(not Self.TransactionTerminated, 'OnTerminated fired');
end;

procedure TestTIdSipServerInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerInviteTransaction.TestTransactionUserResponsesSentToTransport;
var
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.SendResponse(Self.Response);

  Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
        'Response from TU not sent to transport');
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
                           Self.MockDispatcher.Transport);

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
                        Self.MockDispatcher.Transport);

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
var
  SentResponseCount: Cardinal;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State - response not simply ignored');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'SentResponseCount - response not simply ignored');
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
                          Self.MockDispatcher.Transport);

      Self.MoveToProceedingState(Tran);
      Self.Response.StatusCode := I*100;
      Self.MockDispatcher.Transport.ResetSentResponseCount;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

      CheckEquals(1,
                  Self.MockDispatcher.Transport.SentResponseCount,
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
  SentResponseCount: Cardinal;
  Tran:              TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);
    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);

    (Tran as TIdSipServerNonInviteTransaction).FireTimerJ;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Transaction not yet timed out');

    SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

    Self.Response.StatusCode := SIPMultipleChoices;
    Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
    Tran.SendResponse(Self.Response);

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'State - response not simply ignored');

    CheckEquals(SentResponseCount,
                Self.MockDispatcher.Transport.SentResponseCount,
                'SentResponseCount - response not simply ignored');
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
                          Self.MockDispatcher.Transport);

      Self.MockDispatcher.Transport.ResetSentResponseCount;
      Self.Response.StatusCode := I*100;
      Tran.SendResponse(Self.Response);

      CheckEquals(Transaction(itsCompleted),
                  Transaction(Tran.State),
                  'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

      CheckEquals(1,
                  Self.MockDispatcher.Transport.SentResponseCount,
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

  Self.MockDispatcher.Transport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

  CheckEquals(1,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Transport wasn''t given a '
            + IntToStr(Self.Response.StatusCode)
            + ' Response to send');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveProvisionalResponseFromTUInTryingState;
begin
  Self.MockDispatcher.Transport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

  CheckEquals(1,
              Self.MockDispatcher.Transport.SentResponseCount,
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
    Self.MockDispatcher.Transport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockDispatcher.Transport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

    CheckEquals(1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockDispatcher.Transport.LastResponse;
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
    Self.MockDispatcher.Transport.ResetSentResponseCount;

    FirstResponse.Assign(Self.MockDispatcher.Transport.LastResponse);

    Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

    CheckEquals(1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Response not sent to re-received initial request');

    SecondResponse := Self.MockDispatcher.Transport.LastResponse;
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

  Self.MockDispatcher.Transport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response from TU wasn''t dropped on the floor');
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
                        Self.MockDispatcher.Transport);

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

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'Listener not told about failure');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTuResponsesSentToTransport;
var
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.SendResponse(Self.Response);

  Check(ResponseCount < Self.MockDispatcher.Transport.SentResponseCount,
        'Response from TU not sent to transport');
end;

//******************************************************************************
//* TestTIdSipClientInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipClientInviteTransaction Public methods ***************************

procedure TestTIdSipClientInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.Tran.SendRequest;

  Self.ClientInviteTran := Self.Tran as TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Protected methods ************************

procedure TestTIdSipClientInviteTransaction.Terminate(Tran: TIdSipTransaction);
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.ClientInviteTran.FireTimerA;
  Self.MockDispatcher.Transport.FailWith := nil;
end;

function TestTIdSipClientInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Private methods **************************

procedure TestTIdSipClientInviteTransaction.CheckACK(Sender: TObject;
                                                     R: TIdSipResponse);
var
  Ack:    TIdSipRequest;
  Routes: TIdSipHeadersFilter;
begin
  Ack := Self.MockDispatcher.Transport.LastACK;

  CheckEquals(MethodAck,               Ack.Method,     'Method');
  CheckEquals(Self.Request.SipVersion, Ack.SipVersion, 'SIP-Version');
  CheckEquals(Self.Request.RequestUri, Ack.RequestUri, 'Request-URI');
  CheckEquals(Self.Request.CallID,     Ack.CallID,     'Call-ID');
  Check(Self.Request.From.Equals(Ack.From),
        'From');
  Check(R.ToHeader.Equals(Ack.ToHeader),
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
              'Body of ACK is recommended to be empty');

  Routes := TIdSipHeadersFilter.Create(Ack.Headers, RouteHeader);
  try
    CheckEquals(2,
                Routes.Count,
                'Number of Route headers');
    CheckEquals('wsfrank <sip:192.168.1.43>',
                Routes.Items[0].Value,
                '1st Route');
    CheckEquals('localhost <sip:127.0.0.1>',
                Routes.Items[1].Value,
                '2nd Route');
  finally
    Routes.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.MoveToCompletedState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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
  Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'MoveToCompletedState postcondition');
end;

//* TestTIdSipClientInviteTransaction Published methods ************************

procedure TestTIdSipClientInviteTransaction.TestACK;
begin
  Self.Response.AddHeader(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>';
  Self.Response.AddHeader(RouteHeader).Value := 'localhost <sip:127.0.0.1>';

  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.CheckACK;
  Self.MoveToCompletedState(Self.Tran);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Sent ack');
end;

procedure TestTIdSipClientInviteTransaction.TestCancel;
var
  Cancel:       TIdSipRequest;
  RequestCount: Cardinal;
  TranCount:    Integer;
begin
  Self.MoveToProceedingState(Self.Tran);

  RequestCount := Self.MockDispatcher.Transport.SentRequestCount;
  TranCount    := Self.MockDispatcher.TransactionCount;

  Self.ClientInviteTran.Cancel;

  Check(RequestCount < Self.MockDispatcher.Transport.SentRequestCount,
        'no request sent');
  Check(Self.MockDispatcher.Transport.LastRequest.IsCancel,
        'Request wasn''t a CANCEL');

  Cancel := Self.Tran.InitialRequest.CreateCancel;
  try
    Check(Cancel.Equals(Self.MockDispatcher.Transport.LastRequest),
          'Unexpected request sent');
  finally
    Cancel.Free;
  end;

  Check(TranCount < Self.MockDispatcher.TransactionCount,
        'No new transaction created');
end;

procedure TestTIdSipClientInviteTransaction.TestCancelAfterFinalResponse;
var
  RequestCount: Cardinal;
begin
  Self.MoveToProceedingState(Self.ClientInviteTran);
  Self.MoveToCompletedState(Self.ClientInviteTran);
  RequestCount := Self.MockDispatcher.Transport.SentRequestCount;
  Self.ClientInviteTran.Cancel;
  CheckEquals(RequestCount,
              Self.MockDispatcher.Transport.SentRequestCount,
              'No CANCEL should be sent after receipt of a final response');
end;

procedure TestTIdSipClientInviteTransaction.TestCancelBeforeProvisionalResponse;
var
  RequestCount: Cardinal;
begin
  RequestCount := Self.MockDispatcher.Transport.SentRequestCount;
  Self.ClientInviteTran.Cancel;
  CheckEquals(RequestCount,
              Self.MockDispatcher.Transport.SentRequestCount,
              'No CANCEL must be sent prior to receipt of a provisional response');
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
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Self.CheckTerminated := Self.Terminated;

    Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
    Tran.SendRequest;

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
  Self.ClientInviteTran.FireTimerA;
  CheckEquals(2,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientInviteTransaction.TestNoInviteResendingInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Self.ClientInviteTran.FireTimerA;
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentRequestCount,
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
      Tran.SendRequest;
      Fail('Failed to bail out on non-INVITE method');
    except
    end;
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestPrematureDestruction;
var
  Tran:      TIdSipTransaction;
  TranCount: Cardinal;
begin
  // When the INVITE is sent, if there's a network error we go directly to the
  // Terminated state. Terminated transactions are immediately killed by the
  // dispatcher. This means that sending requests should be the last thing done
  // by a method.
  Tran := Self.MockDispatcher.AddClientTransaction(Self.Request);
  TranCount := Self.MockDispatcher.TransactionCount;

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Tran.SendRequest;

  CheckEquals(TranCount - 1,
              Self.MockDispatcher.TransactionCount,
              'Transaction wasn''t terminated and removed');
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
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.CheckReceiveResponse := Self.Proceeding;
  Self.Response.StatusCode := SIPRinging;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Transport layer failed');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Terminated state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCallingState;
var
  ACKCount: Cardinal;
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.Tran.AddTransactionListener(Listener);
    ACKCount := Self.MockDispatcher.Transport.ACKCount;

    Self.Response.StatusCode := SIPOK;
    Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

    CheckEquals(ACKCount, Self.MockDispatcher.Transport.ACKCount,
          'ACK sending arrogated by transaction');

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
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 2xx in the Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInProceedingState;
var
  ACKCount: Cardinal;
  Listener: TIdSipTestTransactionListener;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Self.MoveToProceedingState(Self.Tran);
    Self.Tran.AddTransactionListener(Listener);
    ACKCount := Self.MockDispatcher.Transport.ACKCount;

    Self.Response.StatusCode := SIPOK;
    Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

    CheckEquals(ACKCount, Self.MockDispatcher.Transport.ACKCount,
          'ACK sending arrogated by transaction');

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

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;
  Self.MoveToCompletedState(Self.Tran);

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(2, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceiveMultipleResponsesInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Completed;

  Self.MoveToCompletedState(Self.Tran);
  Self.TransactionCompleted := false;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);
  Check(not Self.TransactionCompleted, '2nd response was passed up to TU');
end;

procedure TestTIdSipClientInviteTransaction.TestReliableTransportNoInviteRetransmissions;
var
  Tran: TIdSipClientInviteTransaction;
begin
  // Hack: we terminate Self.Tran so it doesn't keep sending INVITEs
  Self.Terminate(Self.Tran);

  Self.MockDispatcher.Transport.TransportType := sttTCP;
  Self.Request.LastHop.Transport := sttTCP;

  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest;

    Self.MockDispatcher.Transport.ResetSentRequestCount;
    Tran.FireTimerA;
    CheckEquals(0,
                Self.MockDispatcher.Transport.SentRequestCount,
                'Reliable transports should not resend INVITE');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDFired;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Self.CheckTerminated := Self.Terminated;

    Tran.SendRequest;

    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);
    Tran.FireTimerD;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Terminated');

    CheckEquals('', Self.FailMsg, 'Unexpected fail');
    Check(not Self.TransactionTerminated, 'OnTerminated fired');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimeout;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request)  as TIdSipClientInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Tran.SendRequest;

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

procedure TestTIdSipClientInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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

  Self.Request.Method := MethodOptions;
  Self.Tran.SendRequest;
end;

//* TestTIdSipClientNonInviteTransaction Protected methods *********************

procedure TestTIdSipClientNonInviteTransaction.Terminate(Tran: TIdSipTransaction);
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  (Tran as TIdSipClientNonInviteTransaction).FireTimerE;
  Self.MockDispatcher.Transport.FailWith := nil;
end;

function TestTIdSipClientNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientNonInviteTransaction;
end;

//* TestTIdSipClientNonInviteTransaction Private methods ***********************

procedure TestTIdSipClientNonInviteTransaction.MoveToProceedingState(Tran: TIdSipTransaction);
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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
  Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'MoveToProceedingState postcondition');
end;

//* TestTIdSipClientNonInviteTransaction Published methods *********************

procedure TestTIdSipClientNonInviteTransaction.TestInitialRequestSent;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Self.MockDispatcher.Transport.ResetSentRequestCount;
    Tran.SendRequest;
    CheckEquals(1,
                Self.MockDispatcher.Transport.SentRequestCount,
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
 Tran: TIdSipTransaction;
begin
  Self.Terminate(Self.Tran);

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.SendRequest;
    Self.MoveToProceedingState(Tran);
    Self.MockDispatcher.Transport.ResetSentRequestCount;
    (Tran as TIdSipClientNonInviteTransaction).FireTimerE;
    CheckEquals(1,
                Self.MockDispatcher.Transport.SentRequestCount,
                'Insufficient or too many requests sent');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInTryingState;
var
 Tran: TIdSipTransaction;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.SendRequest;
    Self.MockDispatcher.Transport.ResetSentRequestCount;
    (Tran as TIdSipClientNonInviteTransaction).FireTimerE;
    CheckEquals(1,
                Self.MockDispatcher.Transport.SentRequestCount,
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
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  Check(not Self.TransactionCompleted,
        'Response not dropped');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.CheckReceiveResponse := Self.Proceeding;

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInTerminatedState;
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  Self.CheckReceiveResponse := Self.Completed;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  Check(not Self.TransactionCompleted,
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
  I: Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.AddTransactionListener(Self);
      Self.TransactionCompleted := false;
      Tran.SendRequest;
      Self.MoveToProceedingState(Tran);

      Self.CheckReceiveResponse := Self.Completed;
      Self.Response.StatusCode := 100*I;
      Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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
  I: Integer;
  Tran: TIdSipTransaction;
begin
  for I := 2 to 6 do begin
    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request);
    try
      Tran.AddTransactionListener(Self);
      Self.TransactionCompleted := false;
      Tran.SendRequest;

      Self.CheckReceiveResponse := Self.Completed;
      Self.Response.StatusCode := 100*I;
      Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

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

procedure TestTIdSipClientNonInviteTransaction.TestTimerKFired;
begin
  Self.CheckTerminated := Self.Terminated;

  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  (Self.Tran as TIdSipClientNonInviteTransaction).FireTimerK;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
  Check(not Self.TransactionTerminated, 'OnTerminated fired');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTransportErrorInTryingState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

    Tran.SendRequest;

    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

//******************************************************************************
//* TTestTimer                                                                 *
//******************************************************************************
//* TTestTimer Public methods **************************************************

procedure TTestTimer.SetUp;
begin
  inherited SetUp;

  Self.Dispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.Request := TIdSipTestResources.CreateLocalLoopRequest;
end;

procedure TTestTimer.TearDown;
begin
  Self.Request.Free;
  Self.Dispatcher.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipServerInviteTransactionTimer                                     *
//******************************************************************************
//* TestTIdSipServerInviteTransactionTimer Public methods **********************

procedure TestTIdSipServerInviteTransactionTimer.SetUp;
begin
  inherited SetUp;

  Self.Request.Method := MethodInvite;
  Self.Transaction := TIdSipServerInviteTransaction.Create(Self.Dispatcher,
                                                           Self.Request);

  Self.Timer := TIdSipServerInviteTransactionTimer.Create(Self.Transaction);
end;

procedure TestTIdSipServerInviteTransactionTimer.TearDown;
begin
  Self.Timer.Free;
  Self.Transaction.Free;

  inherited TearDown;
end;

//* TestTIdSipServerInviteTransactionTimer Published methods *******************

procedure TestTIdSipServerInviteTransactionTimer.TestCompletedStartsTimerG;
begin
  Self.Timer.ChangeState(itsCompleted);
  Check(Self.Timer.TimerGIsRunning, 'Timer G wasn''t started');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestCompletedStartsTimerH;
begin
  Self.Timer.ChangeState(itsCompleted);
  Check(Self.Timer.TimerHIsRunning, 'Timer H wasn''t started');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestTimerHInterval;
begin
  CheckEquals(64*DefaultT1,
              Self.Timer.TimerHInterval,
              'Timer H interval');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestConfirmedStopsTimerG;
begin
  Self.Timer.StartTimerG;
  Self.Timer.ChangeState(itsConfirmed);
  Check(not Self.Timer.TimerGIsRunning, 'Timer G wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestConfirmedStopsTimerH;
begin
  Self.Timer.StartTimerH;
  Self.Timer.ChangeState(itsConfirmed);
  Check(not Self.Timer.TimerHIsRunning, 'Timer H wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestConfirmedStartsTimerI;
begin
  Self.Timer.ChangeState(itsConfirmed);
  Check(Self.Timer.TimerIIsRunning, 'Timer I wasn''t started');
end;

procedure TestTIdSipServerInviteTransactionTimer.TestTerminateStopsAllTimers;
begin
  Self.Timer.StartTimerG;
  Self.Timer.StartTimerH;
  Self.Timer.StartTimerI;
  Self.Timer.ChangeState(itsTerminated);
  Check(not Self.Timer.TimerGIsRunning, 'Timer G wasn''t stopped');
  Check(not Self.Timer.TimerHIsRunning, 'Timer H wasn''t stopped');
  Check(not Self.Timer.TimerIIsRunning, 'Timer I wasn''t stopped');
end;

//******************************************************************************
//* TestTIdSipClientInviteTransactionTimer                                     *
//******************************************************************************
//* TestTIdSipClientInviteTransactionTimer Public methods **********************

procedure TestTIdSipClientInviteTransactionTimer.SetUp;
begin
  inherited SetUp;

  Self.Request.Method := MethodInvite;
  Self.Transaction := TIdSipClientInviteTransaction.Create(Self.Dispatcher,
                                                           Self.Request);

  Self.Timer := TIdSipClientInviteTransactionTimer.Create(Self.Transaction);
end;

procedure TestTIdSipClientInviteTransactionTimer.TearDown;
begin
  Self.Timer.Free;
  Self.Transaction.Free;

  inherited TearDown;
end;

//* TestTIdSipClientInviteTransactionTimer Published methods *******************

procedure TestTIdSipClientInviteTransactionTimer.TestCallingLeavesTimerARunning;
begin
  Self.Timer.StartTimerA;
  Self.Timer.ChangeState(itsCalling);
  Self.Timer.ChangeState(itsCalling);
  Check(Self.Timer.TimerAIsRunning, 'Timer A was stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCallingLeavesTimerBRunning;
begin
  Self.Timer.StartTimerB;
  Self.Timer.ChangeState(itsCalling);
  Self.Timer.ChangeState(itsCalling);
  Check(Self.Timer.TimerBIsRunning, 'Timer B was stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCancel;
var
  Response:     TIdSipResponse;
  RequestCount: Cardinal;
begin
  Response := TIdSipResponse.InResponseTo(Self.Transaction.InitialRequest,
                                          SIPTrying);
  try
    Self.Transaction.SendRequest;
    Self.Transaction.ReceiveResponse(Response,
                                     Self.Dispatcher.Transport);

    RequestCount := Self.Dispatcher.Transport.SentRequestCount;
    Self.Transaction.Cancel;
    Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
          'No (CANCEL) request sent');
    Check(Self.Dispatcher.Transport.LastRequest.IsCancel,
          'No CANCEL request sent - '
        + Self.Dispatcher.Transport.LastRequest.Method
        + ' instead');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCancelBeforeProvisionalResponse;
var
  Response:     TIdSipResponse;
  RequestCount: Cardinal;
begin
  Response := TIdSipResponse.InResponseTo(Self.Transaction.InitialRequest,
                                          SIPTrying);
  try
    Self.Transaction.SendRequest;

    RequestCount := Self.Dispatcher.Transport.SentRequestCount;
    Self.Transaction.Cancel;
    CheckEquals(RequestCount,
                Self.Dispatcher.Transport.SentRequestCount,
                'CANCEL sent before we''ve received a response');

    Self.Transaction.ReceiveResponse(Response,
                                     Self.Dispatcher.Transport);

    Check(RequestCount < Self.Dispatcher.Transport.SentRequestCount,
          'No (CANCEL) request sent');
    Check(Self.Dispatcher.Transport.LastRequest.IsCancel,
          'No CANCEL request sent - '
        + Self.Dispatcher.Transport.LastRequest.Method
        + ' instead');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCompletedStartsTimerD;
begin
  Self.Timer.ChangeState(itsCompleted);
  Check(Self.Timer.TimerDIsRunning, 'Timer D wasn''t started');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCompletedStopsTimerA;
begin
  Self.Timer.StartTimerA;
  Self.Timer.ChangeState(itsCompleted);
  Check(not Self.Timer.TimerAIsRunning, 'Timer A wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestCompletedStopsTimerB;
begin
  Self.Timer.StartTimerB;
  Self.Timer.ChangeState(itsCompleted);
  Check(not Self.Timer.TimerBIsRunning, 'Timer B wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestInitialTimerAInterval;
begin
  CheckEquals(DefaultT1,
              Self.Timer.TimerAInterval,
              'TimerA''s initial interval');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestInitialTimerAIntervalDoubles;
var
  I: Integer;
begin
  for I := 1 to 6 do begin
    Self.Timer.FireTimerA;
    CheckEquals(Power(2, I)*DefaultT1,
                Self.Timer.TimerAInterval,
                'TimerA''s interval after ' + IntToStr(I) + ' fires');
  end;
end;

procedure TestTIdSipClientInviteTransactionTimer.TestProceedingStopsTimerA;
begin
  Self.Timer.StartTimerA;
  Self.Timer.ChangeState(itsProceeding);
  Check(not Self.Timer.TimerAIsRunning, 'Timer A wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestProceedingStopsTimerB;
begin
  Self.Timer.StartTimerB;
  Self.Timer.ChangeState(itsProceeding);
  Check(not Self.Timer.TimerBIsRunning, 'Timer B wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestTerminateStopsAllTimers;
begin
  Self.Timer.Start;
  Self.Timer.StartTimerD;
  Self.Timer.ChangeState(itsTerminated);
  Check(not Self.Timer.TimerAIsRunning, 'Timer A wasn''t stopped');
  Check(not Self.Timer.TimerBIsRunning, 'Timer B wasn''t stopped');
  Check(not Self.Timer.TimerDIsRunning, 'Timer D wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestTimerAInitiallyStarted;
begin
  Self.Timer.Start;
  Check(Self.Timer.TimerAIsRunning, 'Timer A not running initially');
end;

procedure TestTIdSipClientInviteTransactionTimer.TestTimerBInitiallyStarted;
begin
  Self.Timer.Start;
  Check(Self.Timer.TimerBIsRunning, 'Timer B not running initially');
end;

//******************************************************************************
//* TestTIdSipClientNonInviteTransactionTimer                                  *
//******************************************************************************
//* TestTIdSipClientNonInviteTransactionTimer Public methods *******************

procedure TestTIdSipClientNonInviteTransactionTimer.SetUp;
begin
  inherited SetUp;

  Self.Request.Method := MethodOptions;

  Self.Transaction := TIdSipClientNonInviteTransaction.Create(Self.Dispatcher,
                                                              Self.Request);

  Self.Timer := TIdSipClientNonInviteTransactionTimer.Create(Self.Transaction);
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TearDown;
begin
  Self.Timer.Free;
  Self.Transaction.Free;

  inherited TearDown;
end;

//* TestTIdSipClientNonInviteTransactionTimer Published methods ****************

procedure TestTIdSipClientNonInviteTransactionTimer.TestCompletedStartsTimerK;
begin
  Self.Timer.StartTimerK;
  Self.Timer.ChangeState(itsCompleted);
  Check(Self.Timer.TimerKIsRunning, 'Timer K wasn''t started');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestCompletedStopsTimerE;
begin
  Self.Timer.StartTimerE;
  Self.Timer.ChangeState(itsCompleted);
  Check(not Self.Timer.TimerEIsRunning, 'Timer E wasn''t stopped');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestCompletedStopsTimerF;
begin
  Self.Timer.StartTimerF;
  Self.Timer.ChangeState(itsCompleted);
  Check(not Self.Timer.TimerFIsRunning, 'Timer F wasn''t stopped');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestInitialTimerEInterval;
begin
  CheckEquals(DefaultT1,
              Self.Timer.TimerEInterval,
              'TimerE''s initial interval');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestProceedingLeavesTimerFRunning;
begin
  Self.Timer.StartTimerF;
  Self.Timer.ChangeState(itsProceeding);
  Self.Timer.ChangeState(itsProceeding);
  Check(Self.Timer.TimerFIsRunning, 'Timer F was stopped');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestProceedingTimerEInterval;
begin
  Self.Timer.ChangeState(itsProceeding);
  Self.Timer.FireTimerE;
  CheckEquals(DefaultT2, Self.Timer.TimerEInterval, 'After 1 fire');
  Self.Timer.FireTimerE;
  CheckEquals(DefaultT2, Self.Timer.TimerEInterval, 'After 2 fires');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTerminatedStopsAllTimers;
begin
  Self.Timer.StartTimerE;
  Self.Timer.StartTimerF;
  Self.Timer.StartTimerK;
  Self.Timer.ChangeState(itsTerminated);
  Check(not Self.Timer.TimerEIsRunning, 'Timer E wasn''t stopped');
  Check(not Self.Timer.TimerFIsRunning, 'Timer F wasn''t stopped');
  Check(not Self.Timer.TimerKIsRunning, 'Timer K wasn''t stopped');  
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTimerEInitiallyStarted;
begin
  Self.Timer.Start;
  Check(Self.Timer.TimerEIsRunning, 'Timer E wasn''t started');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTimerFInitiallyStarted;
begin
  Self.Timer.Start;
  Check(Self.Timer.TimerFIsRunning, 'Timer F wasn''t started');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTimerKInterval;
begin
  CheckEquals(DefaultT4,
              Self.Timer.TimerKInterval,
              'TimerK''s interval');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTryingLeavesTimerERunning;
begin
  Self.Timer.StartTimerE;
  Self.Timer.ChangeState(itsTrying);
  Check(Self.Timer.TimerEIsRunning, 'Timer E was stopped');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTryingLeavesTimerFRunning;
begin
  Self.Timer.StartTimerF;
  Self.Timer.ChangeState(itsTrying);
  Check(Self.Timer.TimerFIsRunning, 'Timer F was stopped');
end;

procedure TestTIdSipClientNonInviteTransactionTimer.TestTryingTimerEInterval;
begin
  Self.Timer.ChangeState(itsTrying);
  Self.Timer.FireTimerE;
  CheckEquals(1000, Self.Timer.TimerEInterval, 'After 1 fire');
  Self.Timer.FireTimerE;
  CheckEquals(2000, Self.Timer.TimerEInterval, 'After 2 fires');
  Self.Timer.FireTimerE;
  CheckEquals(DefaultT2, Self.Timer.TimerEInterval, 'After 3 fires');
  Self.Timer.FireTimerE;
  CheckEquals(DefaultT2, Self.Timer.TimerEInterval, 'After 4 fires');
end;

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
