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
  IdSipCore, IdSipDialog, IdSipMessage, IdSipMockCore,
  IdSipMockTransactionDispatcher, IdSipMockTransport, IdSipTransaction,
  IdSipTransport, TestFramework, TestFrameworkSip;

type
  TestTIdSipTransactionDispatcher = class(TTestCase,
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
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAckDoesntCreateATransaction;
    procedure TestAckForInviteWontCreateTransaction;
    procedure TestAckHandedUpToTU;
    procedure TestAddAndCountTransport;
    procedure TestAddClientTransaction;
    procedure TestAddServerTransaction;
    procedure TestClearTransports;
    procedure TestCreateNewTransaction;
    procedure TestDispatchToCorrectTransaction;
    procedure TestDispatcherDoesntGetTransactionRequests;
    procedure TestDispatcherDoesntGetTransactionResponses;
    procedure TestHandUnmatchedRequestToCore;
    procedure TestHandUnmatchedResponseToCore;
    procedure TestInviteYieldsTrying;
    procedure TestLoopDetected;
    procedure TestSendAckWontCreateTransaction;
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
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
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
    procedure TestSending100;
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
    ClientInviteTran: TIdSipClientInviteTransaction;

    procedure CheckACK(Ack: TIdSipRequest;
                       Response: TIdSipResponse);
    procedure MoveToCompletedState(Tran: TIdSipTransaction);
    procedure MoveToProceedingState(Tran: TIdSipTransaction);
  protected
    procedure Terminate(Tran: TIdSipTransaction);
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestACK;
    procedure TestFireTimerAInCallingState;
    procedure TestFireTimerAInCompletedState;
    procedure TestFireTimerAInProceedingState;
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
    procedure TestTimerAFired;
    procedure TestTimerAIncreases;
    procedure TestTimerBFired;
    procedure TestTimerBScheduled;
    procedure TestTimerDFired;
    procedure TestTimerDScheduled;
    procedure TestTimeout;
    procedure TestTransportErrorInCallingState;
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
    procedure TestTransportErrorInProceedingState;
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

  TUnhandledMessageListenerMethodTestCase = class(TTestCase)
  protected
    Receiver: TIdSipTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipUnhandledMessageListenerReceiveRequestMethod = class(TUnhandledMessageListenerMethodTestCase)
  private
    Method:  TIdSipUnhandledMessageListenerReceiveRequestMethod;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUnhandledMessageListenerReceiveResponseMethod = class(TUnhandledMessageListenerMethodTestCase)
  private
    Method:   TIdSipUnhandledMessageListenerReceiveResponseMethod;
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
  Classes, IdException, IdSdp, IdSipConsts, IdTimerQueue, Math, SysUtils,
  TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipTransactionDispatcher.Suite);
  Result.AddTest(TestTIdSipTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransactionTimer.Suite);
  Result.AddTest(TestTIdSipUnhandledMessageListenerReceiveRequestMethod.Suite);
  Result.AddTest(TestTIdSipUnhandledMessageListenerReceiveResponseMethod.Suite);
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
//* TestTIdSipTransactionDispatcher                                            *
//******************************************************************************
//* TestTIdSipTransactionDispatcher Public methods *****************************

procedure TestTIdSipTransactionDispatcher.SetUp;
begin
  inherited SetUp;

  Self.Core := TIdSipMockCore.Create;

  Self.D := TIdSipTransactionDispatcher.Create;

  Self.Core.Dispatcher := Self.D;

  Self.MockTransport := TIdSipMockTransport.Create;
  Self.MockTransport.TransportType := sttTCP;

  Self.D.AddTransport(Self.MockTransport);

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

  Self.Response200 := TIdSipResponse.Create;
  Self.Response200.Assign(Self.ReceivedResponse);
  Self.Response200.StatusCode := SIPOK;
  Self.Response200.AddHeader(Self.ReceivedRequest.FirstContact);

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

procedure TestTIdSipTransactionDispatcher.OnFail(Transaction: TIdSipTransaction;
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

procedure TestTIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                            Transaction: TIdSipTransaction;
                                                            Receiver: TIdSipTransport);
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
  Self.D.SendRequest(Self.Invite);
  CheckEquals(1, Self.D.TransactionCount, 'INVITE');

  Self.MockTransport.FireOnResponse(Self.Response200);
  CheckEquals(0, Self.D.TransactionCount, '200 terminates an INVITE tran');

  Ack := Self.Invite.AckFor(Self.ReceivedResponse);
  try
    Self.D.SendRequest(Ack);
  finally
    Ack.Free;
  end;

  CheckEquals(0,
              Self.D.TransactionCount,
              'ACK');
end;

procedure TestTIdSipTransactionDispatcher.TestAckHandedUpToTU;
var
  Ack:          TIdSipRequest;
  RemoteDialog: TIdSipDialog;
  Listener:     TIdSipTestUnhandledMessageListener;
  Tran:         TIdSipTransaction;
begin
  RemoteDialog := TIdSipDialog.CreateOutboundDialog(Self.Invite,
                                                    Self.Response200,
                                                    false);
  try
    Ack := RemoteDialog.CreateAck;
    try
      Tran := Self.D.AddServerTransaction(Self.Invite, Self.MockTransport);

      Listener := TIdSipTestUnhandledMessageListener.Create;
      try
        Self.D.AddUnhandledMessageListener(Listener);

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

procedure TestTIdSipTransactionDispatcher.TestAddAndCountTransport;
var
  OriginalCount: Cardinal;
  T1, T2:        TIdSipMockTransport;
begin
  OriginalCount := Self.D.TransportCount;

  T1 := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T1);
    CheckEquals(OriginalCount + 1, Self.D.TransportCount, 'After one AddTransport');

    T2 := TIdSipMockTransport.Create;
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

procedure TestTIdSipTransactionDispatcher.TestClearTransports;
var
  T1, T2: TIdSipMockTransport;
begin
  T1 := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T1);

    T2 := TIdSipMockTransport.Create;
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

procedure TestTIdSipTransactionDispatcher.TestInviteYieldsTrying;
var
  ResponseCount: Cardinal;
begin
  ResponseCount := Self.MockTransport.SentResponseCount;
  Self.MockTransport.FireOnRequest(Self.ReceivedRequest);
  Check(ResponseCount < Self.MockTransport.SentResponseCount,
        'No response sent');

  CheckEquals(SIPTrying,
              Self.MockTransport.LastResponse.StatusCode,
              'First response');
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

procedure TestTIdSipTransactionDispatcher.TestSendAckWontCreateTransaction;
var
  Ack:       TIdSipRequest;
  TranCount: Cardinal;
begin
  TranCount := Self.D.TransactionCount;

  Ack := Self.Invite.AckFor(Self.ReceivedResponse);
  try
    Self.D.SendRequest(Ack);
  finally
    Ack.Free;
  end;

  CheckEquals(TranCount,
              Self.D.TransactionCount,
              'Dispatcher made a new transaction for an outbound ACK');
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
  UdpTran := TIdSipMockTransport.Create;
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

  UdpTran := TIdSipMockTransport.Create;
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

  UdpTran := TIdSipMockTransport.Create;
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

procedure TestTIdSipServerInviteTransaction.MoveToTerminatedState;
begin
  (Self.Tran as TIdSipServerInviteTransaction).FireTimerI;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'MoveToTerminatedState postcondition');
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

procedure TestTIdSipServerInviteTransaction.TestTimerGEventScheduled;
var
  DebugTimer:  TIdDebugTimerQueue;
  Event:       TNotifyEvent;
  EventCount:  Integer;
  TimerGEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerG;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;
    EventCount := DebugTimer.EventCount;

    Self.MoveToCompletedState;

    DebugTimer.LockTimer;
    try
      // "+1" because entering Completed starts TWO timers - G and H.
      Check(EventCount < DebugTimer.EventCount + 1, 'No events scheduled');

      // "-2" because, arbitrarily, the implementation adds TimerG and then
      // TimerH.
      TimerGEvent := DebugTimer.EventAt(DebugTimer.EventCount - 2);
      Check(TimerGEvent.MatchEvent(@Event),
            'Wrong notify event');
      CheckEquals(Self.MockDispatcher.T1Interval,
                  TimerGEvent.DebugWaitTime,
                  'Bad wait time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGIntervalIncreases;
var
  DebugTimer:       TIdDebugTimerQueue;
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
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;
    EventCount := DebugTimer.EventCount;

    // First TimerG
    Self.MoveToCompletedState;

    DebugTimer.LockTimer;
    try
      // "+1" because entering Completed starts TWO timers - G and H.
      Check(EventCount < DebugTimer.EventCount + 1, 'No event scheduled');

      // "-2" because, arbitrarily, the implementation adds TimerG and then
      // TimerH.
      //
      TimerGEvent := DebugTimer.EventAt(DebugTimer.EventCount - 2);
      Check(TimerGEvent.MatchEvent(@Event),
            'Wrong notify event');
      CheckEquals(Self.MockDispatcher.T1Interval,
                  TimerGEvent.DebugWaitTime,
                  'Bad wait time');
    finally
      DebugTimer.UnlockTimer;
    end;

    FireCount := 2;
    ExpectedInterval := 2 * Self.MockDispatcher.T1Interval;
    while (TimerGEvent.DebugWaitTime < Self.MockDispatcher.T2Interval) do begin
      EventCount := DebugTimer.EventCount;

      (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;

      DebugTimer.LockTimer;
      try
        TimerGEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
        Check(EventCount < DebugTimer.EventCount,
              'No event scheduled (' + IntToStr(FireCount) + ')');
        CheckEquals(ExpectedInterval,
                    TimerGEvent.DebugWaitTime,
                    'Bad wait time (' + IntToStr(FireCount) + ')');
      finally
        DebugTimer.UnlockTimer;
      end;

      ExpectedInterval := 2 * ExpectedInterval;
      Inc(FireCount);
    end;

    ExpectedInterval := Self.MockDispatcher.T2Interval;
    for I := 1 to 5 do begin
      EventCount := DebugTimer.EventCount;

      (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;

      DebugTimer.LockTimer;
      try
        TimerGEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
        Check(EventCount < DebugTimer.EventCount,
              'No event scheduled (' + IntToStr(FireCount) + ')');
        CheckEquals(ExpectedInterval,
                    TimerGEvent.DebugWaitTime,
                    'Bad wait time (' + IntToStr(FireCount) + ')');
      finally
        DebugTimer.UnlockTimer;
      end;

      Inc(FireCount);
    end;

  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGOnlyFiresInCompletedState;
var
  DebugTimer: TIdDebugTimerQueue;
  EventCount: Integer;
begin
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;

    EventCount := DebugTimer.EventCount;

    (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;

    DebugTimer.LockTimer;
    try
      CheckEquals(EventCount,
                  DebugTimer.EventCount,
                  'Timer G fired in Proceeding');
    finally
      DebugTimer.UnlockTimer;
    end;

    Self.MoveToCompletedState;
    Self.MoveToConfirmedState;

    DebugTimer.LockTimer;
    try
      EventCount := DebugTimer.EventCount;
      (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;
      CheckEquals(EventCount,
                  DebugTimer.EventCount,
                  'Timer G fired in Confirmed');

      Self.MoveToTerminatedState;
      (Self.Tran as TIdSipServerInviteTransaction).FireTimerG;
      CheckEquals(EventCount,
                  DebugTimer.EventCount,
                  'Timer G fired in Confirmed');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
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

procedure TestTIdSipServerInviteTransaction.TestTimerHEventScheduled;
var
  DebugTimer:  TIdDebugTimerQueue;
  Event:       TNotifyEvent;
  EventCount:  Integer;
  TimerHEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerH;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;
    EventCount := DebugTimer.EventCount;

    Self.MoveToCompletedState;

    DebugTimer.LockTimer;
    try
      // "+1" because entering Completed starts TWO timers - G and H.
      Check(EventCount < DebugTimer.EventCount + 1, 'No events scheduled');

      // "-1" because, arbitrarily, the implementation adds TimerH and then
      // TimerH.
      TimerHEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
      Check(TimerHEvent.MatchEvent(@Event),
            'Wrong notify event');
      CheckEquals(Self.MockDispatcher.TimerHInterval,
                  TimerHEvent.DebugWaitTime,
                  'Bad wait time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
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

procedure TestTIdSipServerInviteTransaction.TestTimerIEventScheduled;
var
  DebugTimer:  TIdDebugTimerQueue;
  Event:       TNotifyEvent;
  EventCount:  Integer;
  LatestEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerInviteTransactionTimerI;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;
    EventCount := DebugTimer.EventCount;

    Self.MoveToCompletedState;
    Self.MoveToConfirmedState;

    DebugTimer.LockTimer;
    try
      Check(EventCount < DebugTimer.EventCount, 'No event scheduled');

      LatestEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
      Check(LatestEvent.MatchEvent(@Event),
            'Wrong notify event');
      CheckEquals(Self.MockDispatcher.TimerIInterval,
                  LatestEvent.DebugWaitTime,
                  'Bad wait time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
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

procedure TestTIdSipServerNonInviteTransaction.TestTimerJEventScheduled;
var
  DebugTimer:  TIdDebugTimerQueue;
  Event:       TNotifyEvent;
  EventCount:  Integer;
  LatestEvent: TIdWait;
begin
  Event := Self.MockDispatcher.OnServerNonInviteTransactionTimerJ;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;
    EventCount := DebugTimer.EventCount;

    Self.MoveToProceedingState(Self.Tran);
    Self.MoveToCompletedState(Self.Tran);

    DebugTimer.LockTimer;
    try
      Check(EventCount < DebugTimer.EventCount, 'No event scheduled');

      LatestEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
      Check(LatestEvent.MatchEvent(@Event),
            'Wrong notify event');
      CheckEquals(Self.MockDispatcher.TimerJInterval,
                  LatestEvent.DebugWaitTime,
                  'Bad wait time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
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
var
  AckCount: Cardinal;
begin
  Self.Tran.InitialRequest.AddHeader(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>';
  Self.Tran.InitialRequest.AddHeader(RouteHeader).Value := 'localhost <sip:127.0.0.1>';
  AckCount := Self.MockDispatcher.Transport.ACKCount;

  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Sent ack');

  Check(AckCount < Self.MockDispatcher.Transport.ACKCount,
        'No ACK sent');
  Self.CheckACK(Self.MockDispatcher.Transport.LastACK,
                Self.Response);
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInCallingState;
var
  DebugTimer: TIdDebugTimerQueue;
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
  Tran:       TIdSipClientInviteTransaction;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerA;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;

    EventCount := DebugTimer.EventCount;

    Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                        Self.Request) as TIdSipClientInviteTransaction;
    try
      Tran.SendRequest;

      DebugTimer.LockTimer;
      try
        Check(EventCount < DebugTimer.EventCount,
              'No event added');
        LastEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
        Check(LastEvent.MatchEvent(@Event),
              'Wrong event scheduled');
        CheckEquals(Self.MockDispatcher.T1Interval,
                    LastEvent.DebugWaitTime,
                    'Wrong time');
      finally
        DebugTimer.UnlockTimer;
      end;
    finally
      Tran.Free;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInCompletedState;
var
  SentRequestCount: Cardinal;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  SentRequestCount := Self.MockDispatcher.Transport.SentRequestCount;

  (Self.Tran as TIdSipClientInviteTransaction).FireTimerA;

  CheckEquals(SentRequestCount,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Timer A fired in Completed state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerAInProceedingState;
var
  SentRequestCount: Cardinal;
begin
  Self.MoveToProceedingState(Self.Tran);

  SentRequestCount := Self.MockDispatcher.Transport.SentRequestCount;

  (Self.Tran as TIdSipClientInviteTransaction).FireTimerA;

  CheckEquals(SentRequestCount,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Timer A fired in Proceeding state');
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerBInCompletedState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest;
    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);
    Tran.FireTimerB;

    CheckNotEquals(Transaction(itsTerminated),
                   Transaction(Tran.State),
                   'Timer B fired in Calling state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerBInProceedingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest;
    Self.MoveToProceedingState(Tran);
    Tran.FireTimerB;

    CheckNotEquals(Transaction(itsTerminated),
                   Transaction(Tran.State),
                   'Timer B fired in Proceeding state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerDInCallingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest;
    Tran.FireTimerD;

    CheckNotEquals(Transaction(itsTerminated),
                   Transaction(Tran.State),
                   'Timer D fired in Calling state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestFireTimerDInProceedingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.SendRequest;
    Self.MoveToProceedingState(Tran);
    Tran.FireTimerD;

    CheckNotEquals(Transaction(itsTerminated),
                   Transaction(Tran.State),
                   'Timer D fired in Proceeding state');
  finally
    Tran.Free;
  end;
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

procedure TestTIdSipClientInviteTransaction.TestTimerAFired;
var
  SentRequestCount: Cardinal;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  SentRequestCount := Self.MockDispatcher.Transport.SentRequestCount;

  (Self.Tran as TIdSipClientInviteTransaction).FireTimerA;

  CheckEquals(SentRequestCount,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Timer A fired in Proceeding state');
end;              

procedure TestTIdSipClientInviteTransaction.TestTimerAIncreases;
var
  DebugTimer:       TIdDebugTimerQueue;
  ExpectedInterval: Cardinal;
  Event:            TNotifyEvent;
  EventCount:       Integer;
  FireCount:        Integer;
  LastEvent:        TIdWait;
  Tran:             TIdSipClientInviteTransaction;
begin
  Tran := Self.Tran as TIdSipClientInviteTransaction;
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerA;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;


    ExpectedInterval := 2*Self.MockDispatcher.T1Interval;
    for FireCount := 2 to 8 do begin
      EventCount := DebugTimer.EventCount;

      Tran.FireTimerA;

      DebugTimer.LockTimer;
      try
        Check(EventCount < DebugTimer.EventCount,
              'No event added (' + IntToStr(FireCount) + ')');
        LastEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
        Check(LastEvent.MatchEvent(@Event),
              'Wrong event added (' + IntToStr(FireCount) + ')');
        CheckEquals(ExpectedInterval,
                    LastEvent.DebugWaitTime,
                    'Wrong time added (' + IntToStr(FireCount) + ')');
      finally
        DebugTimer.UnlockTimer;
      end;
      ExpectedInterval := 2 * ExpectedInterval;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimerBFired;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request) as TIdSipClientInviteTransaction;
  try
    Tran.AddTransactionListener(Self);
    Self.CheckTerminated := Self.Terminated;

    Tran.SendRequest;

    Tran.FireTimerB;

    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Terminated');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimerBScheduled;
var
  DebugTimer: TIdDebugTimerQueue;
  Event:      TNotifyEvent;
  EventCount: Integer;
  TimerBEvent:  TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerB;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;

    EventCount := DebugTimer.EventCount;

    Self.MoveToProceedingState(Self.Tran);
    Self.MoveToCompletedState(Self.Tran);

    DebugTimer.LockTimer;
    try
      // "+1" because the transaction schedules TWO events
      Check(EventCount < DebugTimer.EventCount + 1,
            'No event added');

      // "-2" because the implementation schedules timer B then timer D.
      TimerBEvent := DebugTimer.EventAt(DebugTimer.EventCount - 2);
      Check(TimerBEvent.MatchEvent(@Event),
            'Wrong event scheduled');
      CheckEquals(Self.MockDispatcher.TimerBInterval,
                  TimerBEvent.DebugWaitTime,
                  'Wrong time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
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

procedure TestTIdSipClientInviteTransaction.TestTimerDScheduled;
var
  DebugTimer: TIdDebugTimerQueue;
  Event:      TNotifyEvent;
  EventCount: Integer;
  LastEvent:  TIdWait;
begin
  Event := Self.MockDispatcher.OnClientInviteTransactionTimerD;
  DebugTimer := TIdDebugTimerQueue.Create;
  try
    Self.MockDispatcher.Timer := DebugTimer;

    EventCount := DebugTimer.EventCount;

    Self.MoveToProceedingState(Self.Tran);
    Self.MoveToCompletedState(Self.Tran);

    DebugTimer.LockTimer;
    try
      Check(EventCount < DebugTimer.EventCount,
            'No event added');
      LastEvent := DebugTimer.EventAt(DebugTimer.EventCount - 1);
      Check(LastEvent.MatchEvent(@Event),
            'Wrong event scheduled');
      CheckEquals(Self.MockDispatcher.TimerDInterval,
                  LastEvent.DebugWaitTime,
                  'Wrong time');
    finally
      DebugTimer.UnlockTimer;
    end;
  finally
    Self.MockDispatcher.Timer := nil;
    DebugTimer.Terminate;
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

procedure TestTIdSipClientInviteTransaction.TestTransportErrorInCallingState;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher, Self.Request);
  try
    Tran.AddTransactionListener(Self);
    Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
    Tran.SendRequest;

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

procedure TestTIdSipClientNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  // When Timer E fires, the transaction resends the request. 
  (Self.Tran as TIdSipClientNonInviteTransaction).FireTimerE;

  CheckEquals(Transaction(itsTerminated),
              Transaction(Tran.State),
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
    Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

    Tran.SendRequest;

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

//******************************************************************************
//* TUnhandledMessageListenerMethodTestCase                                    *
//******************************************************************************
//* TUnhandledMessageListenerMethodTestCase Public methods *********************

procedure TUnhandledMessageListenerMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipNullTransport.Create;
end;

procedure TUnhandledMessageListenerMethodTestCase.TearDown;
begin
  Self.Receiver.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipUnhandledMessageListenerReceiveRequestMethod                     *
//******************************************************************************
//* TestTIdSipUnhandledMessageListenerReceiveRequestMethod Public methods ******

procedure TestTIdSipUnhandledMessageListenerReceiveRequestMethod.SetUp;
begin
  inherited SetUp;

  Self.Request := TIdSipRequest.Create;

  Self.Method := TIdSipUnhandledMessageListenerReceiveRequestMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Request  := Self.Request;
end;

procedure TestTIdSipUnhandledMessageListenerReceiveRequestMethod.TearDown;
begin
  Self.Method.Free;
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipUnhandledMessageListenerReceiveRequestMethod Published methods ***

procedure TestTIdSipUnhandledMessageListenerReceiveRequestMethod.TestRun;
var
  Listener: TIdSipTestUnhandledMessageListener;
begin
  Listener := TIdSipTestUnhandledMessageListener.Create;
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
//* TestTIdSipUnhandledMessageListenerReceiveResponseMethod                    *
//******************************************************************************
//* TestTIdSipUnhandledMessageListenerReceiveResponseMethod Public methods *****

procedure TestTIdSipUnhandledMessageListenerReceiveResponseMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipUnhandledMessageListenerReceiveResponseMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Response  := Self.Response;
end;

procedure TestTIdSipUnhandledMessageListenerReceiveResponseMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipUnhandledMessageListenerReceiveResponseMethod Published methods **

procedure TestTIdSipUnhandledMessageListenerReceiveResponseMethod.TestRun;
var
  Listener: TIdSipTestUnhandledMessageListener;
begin
  Listener := TIdSipTestUnhandledMessageListener.Create;
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
