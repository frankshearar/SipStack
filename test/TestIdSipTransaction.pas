unit TestIdSipTransaction;

interface

uses
  IdSipCore, IdSipDialog, IdSipMessage, IdSipMockCore,
  IdSipMockTransactionDispatcher, IdSipMockTransport, IdSipTransaction,
  IdSipTransport, TestFramework, TestFrameworkSip;

type
  TestTIdSipTransactionDispatcher = class(TTestCase, IIdSipTransactionListener)
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

    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Transport: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCountTransport;
    procedure TestClearTransports;
    procedure TestCreateNewTransaction;
    procedure TestAddClientTransaction;
    procedure TestAddServerTransaction;
    procedure TestDispatchToCorrectTransaction;
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
    procedure TestTransactionsCleanedUp;
    procedure TestWillUseReliableTransport;
//    procedure TestTortureTest41;
  end;

  TestTIdSipTransaction = class(TTestCase)
  private
    Dispatch:       TIdSipTransactionDispatcher;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddTransactionListener;
    procedure TestAllListenersNotified;
    procedure TestGetClientTransactionType;
    procedure TestGetServerTransactionType;
    procedure TestRemoveTransactionListener;
  end;

  TIdSipTransactionEvent = procedure(Sender: TIdSipTransaction) of object;

  // Transactions behave slightly differently if a reliable transport is used -
  // certain messages are not resent. To this end, we test unreliable transports
  // by default, only checking that those certain messages are not resent when
  // using reliable transports in tests like TestReliableTransportFoo
  TTestTransaction = class(TTestCaseSip, IIdSipTransactionListener)
  protected
    CheckReceiveRequest:   TIdSipRequestEvent;
    CheckReceiveResponse:  TIdSipResponseEvent;
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

    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure OnFail(const Transaction: TIdSipTransaction; const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Transport: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
    procedure TransactionFail(Sender: TObject; const Reason: String);
    procedure Terminated(Sender: TIdSipTransaction);
    function  TransactionType: TIdSipTransactionClass; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipServerInviteTransaction = class(TTestTransaction)
  private
    TransactionConfirmed: Boolean;

    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure OnInitialRequestSentToTU(Sender: TObject; const R: TIdSipRequest);
    procedure ReceiveInvite;
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestIsClient;
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

    procedure MoveToCompletedState(const Tran: TIdSipTransaction);
    procedure MoveToProceedingState(const Tran: TIdSipTransaction);
    procedure Trying(Sender: TObject; const R: TIdSipRequest);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestIsClient;
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
    procedure CheckACK(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState(const Tran: TIdSipTransaction);
    procedure MoveToProceedingState(const Tran: TIdSipTransaction);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestACK;
    procedure TestInitialState;
    procedure TestInviteWithHostUnreachable;
    procedure TestIsClient;
    procedure TestMultipleInviteSending;
    procedure TestNoInviteResendingInProceedingState;
    procedure TestNonInviteMethodInInitialRequest;
    procedure TestPrematureDestruction;
    procedure TestReceive1xxInCallingState;
    procedure TestReceive1xxInCompletedState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxNoResendingOfRequest;
    procedure TestReceive1xxInTerminatedState;
    procedure TestReceive2xxInCallingState;
    procedure TestReceive2xxInCompletedState;
    procedure TestReceive2xxInProceedingState;
    procedure TestReceive3xxInCallingState;
    procedure TestReceive3xxInCompletedState;
    procedure TestReceive3xxInProceedingState;
    procedure TestReliableTransportNoInviteRetransmissions;
    procedure TestTimerDFired;
    procedure TestTimeout;
    procedure TestTransportErrorInCompletedState;
  end;

  TestTIdSipClientNonInviteTransaction = class(TTestTransaction)
  private
    procedure MoveToProceedingState(const Tran: TIdSipTransaction);
    procedure MoveToCompletedState(const Tran: TIdSipTransaction);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialRequestSent;
    procedure TestInitialState;
    procedure TestIsClient;
    procedure TestMultipleRequestSendingInProceedingState;
    procedure TestMultipleRequestSendingInTryingState;
    procedure TestReceive1xxInCompletedState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxInTerminatedState;
    procedure TestReceive1xxInTryingState;
    procedure TestReceiveFinalResponseInProceedingState;
    procedure TestReceiveFinalResponseInTryingState;
    procedure TestTimeout;
    procedure TestTimerKFired;
    procedure TestTransportErrorInTryingState;
  end;

implementation

uses
  Classes, IdException, IdSipConsts, IdSipHeaders, SyncObjs,
  SysUtils, TestMessages, TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipTransactionDispatcher.Suite);
  Result.AddTest(TestTIdSipTransaction.Suite);
  Result.AddTest(TestTIdSipClientInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransaction.Suite);
  Result.AddTest(TestTIdSipClientNonInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerNonInviteTransaction.Suite);
end;

function Transaction(const S: TIdSipTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipTransactionState), Integer(S));
end;

//******************************************************************************
//* TestTIdSipTransactionDispatcher                                            *
//******************************************************************************
//* TestTIdSipTransactionDispatcher Public methods *****************************

procedure TestTIdSipTransactionDispatcher.SetUp;
var
  P: TIdSipParser;
begin
  inherited SetUp;

  Self.Core := TIdSipMockCore.Create;

  Self.D := TIdSipTransactionDispatcher.Create;

  Self.Core.Dispatcher := Self.D;

  Self.MockTransport := TIdSipMockTransport.Create(IdPORT_SIP);

  Self.MockTransport.TransportType := sttTCP;
  Self.D.AddTransport(Self.MockTransport);

  P := TIdSipParser.Create;
  try
    Self.ReceivedRequest := P.ParseAndMakeRequest(LocalLoopRequest);
    Self.TranRequest     := P.ParseAndMakeRequest(LocalLoopRequest);

    Self.ReceivedResponse := P.ParseAndMakeResponse(LocalLoopResponse);
  finally
    P.Free;
  end;

  Self.ReceivedResponse.StatusCode := SIPTrying;
  Self.ReceivedResponse.AddHeaders(Self.ReceivedRequest.Headers);

  Self.Invite := TIdSipRequest.Create;
  Self.Invite.Assign(Self.ReceivedRequest);

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

procedure TestTIdSipTransactionDispatcher.OnFail(const Transaction: TIdSipTransaction;
                                                 const Reason: String);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveRequest(const Request: TIdSipRequest;
                                                           const Transaction: TIdSipTransaction;
                                                           const Transport: TIdSipTransport);
begin
end;

procedure TestTIdSipTransactionDispatcher.OnReceiveResponse(const Response: TIdSipResponse;
                                                            const Transaction: TIdSipTransaction;
                                                            const Transport: TIdSipTransport);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnReceiveResponseFired := true;
end;

procedure TestTIdSipTransactionDispatcher.OnTerminated(const Transaction: TIdSipTransaction);
begin
  Check(not Transaction.IsClient, 'Client tran got the response - from the TU!');
  Self.OnTerminatedFired := true;
end;

//* TestTIdSipTransactionDispatcher Published methods **************************

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

  Self.Invite.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';

  Self.D.AddClientTransaction(Self.TranRequest);
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

    while (Length(Self.Response200.AsString) < 1300) do
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
    while (Length(Self.TranRequest.AsString) < 1300) do
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
{
procedure TestTIdSipTransactionDispatcher.TestTortureTest41;
var
  P: TIdSipParser;
  Req: TIdSipRequest;
begin
  P := TIdSipParser.Create;
  try
    Req := P.ParseAndMakeRequest(TortureTest41);
    try
      Self.MockTransport.FireOnRequest(Req);
      Fail('not implemented yet');
//      Self.MockTransport.
      CheckEquals(SIPSIPVersionNotSupported,
                  Self.MockTransport.LastResponse.StatusCode,
                  '');
    finally
      Req.Free;
    end;
  finally
    P.Free;
  end;
end;
}
//******************************************************************************
//* TestTIdSipTransaction                                                      *
//******************************************************************************
//* TestTIdSipTransaction Public methods ***************************************

procedure TestTIdSipTransaction.SetUp;
begin
  inherited SetUp;

  Self.Dispatch       := TIdSipMockTransactionDispatcher.Create;
  Self.Request := TIdSipRequest.Create;
end;

procedure TestTIdSipTransaction.TearDown;
begin
  Self.Request.Free;
  Self.Dispatch.Free;

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
    Tran := TIdSipClientInviteTransaction.Create(Self.Dispatch, Self.Request);
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
      Tran := TIdSipClientInviteTransaction.Create(Self.Dispatch, Self.Request);
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
begin
  R := TIdSipRequest.Create;
  try
    R.Method := MethodInvite;
    CheckEquals(TIdSipClientInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client INVITE');

    R.Method := MethodAck;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client ACK');

    R.Method := MethodBye;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client BYE');

    R.Method := MethodCancel;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client CANCEL');

    R.Method := MethodOptions;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client OPTIONS');

    R.Method := MethodRegister;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client REGISTER');

    R.Method := 'NewFangledMethod';
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetClientTransactionType(R).ClassName,
                'Client NewFangledMethod');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipTransaction.TestGetServerTransactionType;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Method := MethodInvite;
    CheckEquals(TIdSipServerInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server INVITE');

    R.Method := MethodAck;
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server ACK');

    R.Method := MethodBye;
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server BYE');

    R.Method := MethodCancel;
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server CANCEL');

    R.Method := MethodOptions;
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server OPTIONS');

    R.Method := MethodRegister;
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server REGISTER');

    R.Method := 'NewFangledMethod';
    CheckEquals(TIdSipServerNonInviteTransaction.ClassName,
                TIdSipTransaction.GetServerTransactionType(R).ClassName,
                'Server NewFangledMethod');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipTransaction.TestRemoveTransactionListener;
var
  Listener: TIdSipTestTransactionListener;
  Tran:     TIdSipTransaction;
  Response: TIdSipResponse;
begin
  Listener := TIdSipTestTransactionListener.Create;
  try
    Tran := TIdSipClientInviteTransaction.Create(Self.Dispatch, Self.Request);
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

  // this is just BasicRequest from TestIdSipParser
  Self.Request := TIdSipRequest.Create;
  Self.Request.Method                             := MethodInvite;
  Self.Request.MaxForwards                        := 70;
  Self.Request.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Request.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Request.From.DisplayName                   := 'Case';
  Self.Request.From.Address.URI                   := 'sip:case@fried.neurons.org';
  Self.Request.From.Tag                           := '1928301774';
  Self.Request.CallID                             := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.Request.CSeq.Method                        := 'INVITE';
  Self.Request.CSeq.SequenceNo                    := 314159;
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.Request.ContentType                        := 'text/plain';
  Self.Request.ContentLength                      := 29;
  Self.Request.Body                               := 'I am a message. Hear me roar!';

  Self.FailMsg        := '';
  Self.MockDispatcher := TIdSipMockTransactionDispatcher.Create;

  Self.Response                                    := TIdSipResponse.Create;
  Self.Response.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Response.AddHeader(ViaHeaderFull).Value     := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.Response.From.DisplayName                   := 'Case';
  Self.Response.From.Address.URI                   := 'sip:case@fried.neurons.org';
  Self.Response.From.Tag                           := '1928301774';
  Self.Response.CallID                             := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.Response.CSeq.Method                        := 'INVITE';
  Self.Response.CSeq.SequenceNo                    := 314159;
  Self.Response.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.Request.ContentLength                       := 0;

  Self.Tran := Self.TransactionType.Create(Self.MockDispatcher, Self.Request);
  Self.Tran.AddTransactionListener(Self);
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionProceeding := false;
  Self.TransactionTerminated := false;

  Self.MockDispatcher.Transport.TransportType := sttUDP;
  Self.MockDispatcher.Transport.HostName      := 'gw1.leo-ix.org';
end;

procedure TTestTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.Response.Free;
  Self.MockDispatcher.Free;
  Self.Request.Free;
  Self.Core.Free;

  inherited TearDown;
end;

//* TTestTransaction Protected methods *****************************************

procedure TTestTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TTestTransaction.OnFail(const Transaction: TIdSipTransaction;
                                                   const Reason: String);
begin
  Self.FailMsg           := Reason;
  Self.TransactionFailed := true;
end;

procedure TTestTransaction.OnReceiveRequest(const Request: TIdSipRequest;
                                            const Transaction: TIdSipTransaction;
                                            const Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckReceiveRequest) then
    Self.CheckReceiveRequest(Self, Request);
end;

procedure TTestTransaction.OnReceiveResponse(const Response: TIdSipResponse;
                                             const Transaction: TIdSipTransaction;
                                             const Transport: TIdSipTransport);
begin
  if Assigned(Self.CheckReceiveResponse) then
    Self.CheckReceiveResponse(Self, Response);
end;

procedure TTestTransaction.OnTerminated(const Transaction: TIdSipTransaction);
begin
  if Assigned(Self.CheckTerminated) then
    Self.CheckTerminated(Transaction);
end;

procedure TTestTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TTestTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TTestTransaction.Terminated(Sender: TIdSipTransaction);
begin
  Self.TransactionTerminated := true;
  Self.ThreadEvent.SetEvent;
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

procedure TestTIdSipServerInviteTransaction.OnInitialRequestSentToTU(Sender: TObject; const R: TIdSipRequest);
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
  Check(Self.Request.CSeq.IsEqualTo(R.CSeq),
              'CSeq');
  Check(R.Path.IsEqualTo(Self.Request.Path),
              'Via path differs');
  CheckEquals(Self.Request.ToHeader.Address,
              R.ToHeader.Address,
              'To address');
  Check(not R.ToHeader.HasTag,
              'To tag');
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
  Self.MoveToCompletedState;
  Self.Request.Method := MethodAck;
  Self.Tran.ReceiveRequest(Self.Request, Self.MockDispatcher.Transport);

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
  finally
    Tran.Free;
  end;

  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Reliable transports should not resend final response');
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
    Check(FirstResponse.IsEqualTo(SecondResponse),
          'Different response sent to initial request retransmission');
  finally
    FirstResponse.Free;
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestResponseRetransmissionInCompletedState;
begin
  Self.MoveToCompletedState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  // Long enough to send 2 responses, but not long enough for 3.
  // cf. RFC 3261, section 17.2.1
  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(2,
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
    Check(R.CSeq.IsEqualTo(Self.Request.CSeq),
                'CSeq headers must match');
    Check(R.From.IsEqualTo(Self.Request.From),
                'From headers must match');
    Check(R.ToHeader.IsEqualTo(Self.Request.ToHeader),
                'To headers must match');
    Check(R.Path.IsEqualTo(Self.Request.Path),
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
  Sleep(3*InitialT1);
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Timer G wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerHFired;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request,
                                      500);
  try
    Tran.AddTransactionListener(Self);
    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);

    Response.StatusCode := SIPMultipleChoices;
    Tran.SendResponse(Self.Response);

    Sleep(750);
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
  Sleep(6000);

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

procedure TestTIdSipServerNonInviteTransaction.MoveToCompletedState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipServerNonInviteTransaction.MoveToProceedingState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipServerNonInviteTransaction.Trying(Sender: TObject; const R: TIdSipRequest);
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
                                      Self.Request,
                                      100);
  try
    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);
    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);

    Sleep(200);

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
    Check(FirstResponse.IsEqualTo(SecondResponse),
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
    Check(FirstResponse.IsEqualTo(SecondResponse),
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
                                      Self.Request,
                                      100);
  try
    Self.CheckTerminated := Self.Terminated;

    Tran.ReceiveRequest(Self.Request,
                        Self.MockDispatcher.Transport);

    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);

    Sleep(200);

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
end;

//* TestTIdSipClientInviteTransaction Protected methods ************************

function TestTIdSipClientInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Private methods **************************

procedure TestTIdSipClientInviteTransaction.CheckACK(Sender: TObject; const R: TIdSipResponse);
var
  Ack:    TIdSipRequest;
  Routes: TIdSipHeadersFilter;
begin
  Ack := Self.MockDispatcher.Transport.LastACK;

  CheckEquals(MethodAck,                      Ack.Method,         'Method');
  CheckEquals(Self.Request.SipVersion, Ack.SipVersion,     'SIP-Version');
  CheckEquals(Self.Request.RequestUri, Ack.RequestUri,     'Request-URI');
  CheckEquals(Self.Request.CallID,     Ack.CallID,         'Call-ID');
  CheckEquals(Self.Request.From.Value, Ack.From.Value,     'From');
  CheckEquals(R.ToHeader.Value,               Ack.ToHeader.Value, 'To');

  CheckEquals(1, Ack.Path.Length, 'Number of Via headers');
  CheckEquals(Self.Request.LastHop.Value,
              Ack.LastHop.Value,
              'Topmost Via');

  CheckEquals(Self.Request.CSeq.SequenceNo, Ack.CSeq.SequenceNo, 'CSeq sequence no');
  CheckEquals(MethodAck,                           Ack.CSeq.Method,     'CSeq method');


  CheckEquals(0,  Ack.ContentLength, 'Content-Length');
  CheckEquals('', Ack.Body,          'Body of ACK is recommended to be empty');

  Routes := TIdSipHeadersFilter.Create(Ack.Headers, RouteHeader);
  try
    CheckEquals(2,                            Routes.Count,          'Number of Route headers');
    CheckEquals('wsfrank <sip:192.168.1.43>', Routes.Items[0].Value, '1st Route');
    CheckEquals('localhost <sip:127.0.0.1>',  Routes.Items[1].Value, '2nd Route');
  finally
    Routes.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.MoveToCompletedState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipClientInviteTransaction.MoveToProceedingState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipClientInviteTransaction.TestMultipleInviteSending;
begin
  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(3,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientInviteTransaction.TestNoInviteResendingInProceedingState;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Sleep(1000);
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
  Tran: TIdSipTransaction;
begin
  // When the INVITE is sent, if there's a network error we go directly to the
  // Terminated state. Terminated transactions are immediately killed by the
  // dispatcher. This means that sending requests should be the last thing done
  // by a method.
  Tran := Self.MockDispatcher.AddClientTransaction(Self.Request);
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Tran.SendRequest;
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

procedure TestTIdSipClientInviteTransaction.TestReceive1xxNoResendingOfRequest;
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Sleep(1500);
  CheckEquals(0, Self.MockDispatcher.Transport.SentRequestCount, 'Request was resent');

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
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
begin
  Self.Response.StatusCode := SIPOK;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
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
begin
  Self.MoveToProceedingState(Self.Tran);

  Self.Response.StatusCode := SIPOK;
  Self.Tran.ReceiveResponse(Self.Response, Self.MockDispatcher.Transport);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
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

procedure TestTIdSipClientInviteTransaction.TestReliableTransportNoInviteRetransmissions;
var
  Tran: TIdSipTransaction;
begin
  // Hack: we terminate Self.Tran so it doesn't keep sending INVITEs
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Self.MockDispatcher.Transport.TransportType := sttTCP;
  Self.Request.LastHop.Transport := sttTCP;

  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.SendRequest;

    // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
    // messages being sent.
    Sleep(2000);
    CheckEquals(1,
                Self.MockDispatcher.Transport.SentRequestCount,
                'Reliable transports should not resend INVITE');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDFired;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request,
                                      250);
  try
    Tran.AddTransactionListener(Self);
    Self.CheckTerminated := Self.Terminated;

    Tran.SendRequest;

    Self.MoveToProceedingState(Tran);
    Self.MoveToCompletedState(Tran);
    Sleep(500);

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
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request,
                                      500);
  try
    Tran.AddTransactionListener(Self);
    Tran.SendRequest;

    Sleep(750);

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

function TestTIdSipClientNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientNonInviteTransaction;
end;

//* TestTIdSipClientNonInviteTransaction Private methods ***********************

procedure TestTIdSipClientNonInviteTransaction.MoveToProceedingState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipClientNonInviteTransaction.MoveToCompletedState(const Tran: TIdSipTransaction);
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

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInProceedingState;
var
 Tran: TIdSipTransaction;
begin
  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);

  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request);
  try
    Tran.SendRequest;
    Self.MoveToProceedingState(Tran);
    Self.MockDispatcher.Transport.ResetSentRequestCount;
    // TimerE is supposed to now have an interval of 4s
    Sleep(5000);
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
    Self.MockDispatcher.Transport.ResetSentRequestCount;
    Tran.SendRequest;

    // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
    // messages being sent.
    Sleep(2000);
    CheckEquals(3,
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

procedure TestTIdSipClientNonInviteTransaction.TestTimeout;
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.TransactionType.Create(Self.MockDispatcher,
                                      Self.Request,
                                      500);
  try
    Tran.AddTransactionListener(Self);
    Tran.SendRequest;

    Sleep(750);
    CheckEquals(Transaction(itsTerminated),
                Transaction(Tran.State),
                'Timeout');

    Check(Self.TransactionFailed,
          'Listener not told about failure');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerKFired;
begin
  Self.CheckTerminated := Self.Terminated;

  Self.MoveToProceedingState(Self.Tran);
  Self.MoveToCompletedState(Self.Tran);
  Sleep(6000);

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

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
