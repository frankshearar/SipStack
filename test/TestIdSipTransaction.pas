unit TestIdSipTransaction;

interface

uses
  IdSipMessage, IdSipParser, IdSipTransport, IdSipTransaction,
  TestFramework;

  // Transactions behave slightly differently if a reliable transport is used -
  // certain messages are not resent. To this end, we test unreliable transports
  // by default, only checking that those certain messages are not resent when
  // using reliable transports in tests like TestReliableTransportFoo
type
  TestTIdSipTransactionDispatcher = class(TTestCase)
  private
    D:                TIdSipTransactionDispatcher;
    ReceivedRequest:  TIdSipRequest;
    ReceivedResponse: TIdSipResponse;
    TranRequest:      TIdSipRequest;

//    procedure UseRFC2543Requests;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCountTransport;
    procedure TestClearTransports;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteServer;
//    procedure TestMatchInviteRFC2543;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
//    procedure TestMatchNonInviteRFC2543;
  end;

  TestTIdSipTransaction = class(TTestCase)
  published
    procedure TestGetTransactionType;
  end;

  TestTIdSipClientInviteTransaction = class(TTestCase)
  private
    FailMsg:               String;
    InitialRequest:        TIdSipRequest;
    MockTransport:         TIdSipMockTransport;
    Response:              TIdSipResponse;
    Tran:                  TIdSipClientInviteTransaction;
    TransactionProceeding: Boolean;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionTerminated: Boolean;

    procedure CheckACK(Sender: TObject; const R: TIdSipResponse);
    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToProceedingState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
    procedure Terminated(Sender: TObject);
    procedure TransactionFail(Sender: TObject; const Reason: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestACK;
    procedure TestCompletedStateNetworkProblem;
    procedure TestInitialState;
    procedure TestInviteWithHostUnreachable;
    procedure TestMultipleInviteSending;
    procedure TestNoInviteResendingInProceedingState;
    procedure TestNonInviteMethodInInitialRequest;
    procedure TestReceive1xxInCallingState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxNoResendingOfRequest;
    procedure TestReceive2xxInCallingState;
    procedure TestReceive2xxInProceedingState;
    procedure TestReceive3xxInCallingState;
    procedure TestReceive3xxInCompletedState;
    procedure TestReceive3xxInProceedingState;
    procedure TestReliableTransportNoInviteRetransmissions;
    procedure TestTimerDFired;
    procedure TestTimeout;
  end;

  TestTIdSipServerInviteTransaction = class(TTestCase)
  private
    CheckReceiveInviteFired:                                      Boolean;
    CheckReceive2xxFromTUInProceedingStateFired:                  Boolean;
    CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired: Boolean;
    CheckSending100Fired:                                         Boolean;
    FailMsg:                                                      String;
    InitialRequest:                                               TIdSipRequest;
    InitialRequestSentToTU:                                       Boolean;
    MockTransport:                                                TIdSipMockTransport;
    Request:                                                      TIdSipRequest;
    Response:                                                     TIdSipResponse;
    Tran:                                                         TIdSipServerInviteTransaction;
    TransactionProceeding:                                        Boolean;
    TransactionCompleted:                                         Boolean;
    TransactionConfirmed:                                         Boolean;
    TransactionFailed:                                            Boolean;
    TransactionTerminated:                                        Boolean;

    procedure CheckReceiveInvite(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceive2xxFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceiveNonTryingProvisionalFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSending100(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure OnInitialRequestSentToTU(Sender: TObject; const R: TIdSipRequest);
    procedure ReceiveInvite;
    procedure Terminated(Sender: TObject);
    procedure TransactionFail(Sender: TObject; const Reason: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestCompletedStateNetworkProblem;
    procedure TestProceedingStateNetworkProblem;
    procedure TestReceive2xxFromTUInProceedingState;
    procedure TestReceiveAckInCompletedState;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveInviteInCompletedState;
    procedure TestReceiveInviteInProceedingState;
    procedure TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
    procedure TestReliableTransportNoFinalResponseRetransmissions;
    procedure TestResponseRetransmissionInCompletedState;
    procedure TestTimerGStops;
    procedure TestTimeout;
    procedure TestTimerIFired;
    procedure TestSending100;
  end;

  TestTIdSipClientNonInviteTransaction = class(TTestCase)
  private
    FailMsg:               String;
    InitialRequest:        TIdSipRequest;
    MockTransport:         TIdSipMockTransport;
    Response:              TIdSipResponse;
    Tran:                  TIdSipClientNonInviteTransaction;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionProceeding: Boolean;
    TransactionTerminated: Boolean;

    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToProceedingState;
    procedure MoveToCompletedState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
    procedure Terminated(Sender: TObject);
    procedure TransactionFail(Sender: TObject; const Reason: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialRequestSent;
    procedure TestInitialState;
    procedure TestMultipleRequestSendingInProceedingState;
    procedure TestMultipleRequestSendingInTryingState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxInTryingState;
    procedure TestReceiveFinalResponseInProceedingState;
    procedure TestReceiveFinalResponseInTryingState;
    procedure TestTimeout;
    procedure TestTimerKFired;
    procedure TestTryingStateTransportError;
  end;

  TestTIdSipServerNonInviteTransaction = class(TTestCase)
  private
    FailMsg:               String;
    InitialRequest:        TIdSipRequest;
    MockTransport:         TIdSipMockTransport;
    Request:               TIdSipRequest;
    Response:              TIdSipResponse;
    Tran:                  TIdSipServerNonInviteTransaction;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionProceeding: Boolean;
    TransactionTerminated: Boolean;
    TransactionTrying:     Boolean;

    procedure MoveToCompletedState;
    procedure MoveToProceedingState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure Terminated(Sender: TObject);
    procedure TransactionFail(Sender: TObject; const Reason: String);
    procedure Trying(Sender: TObject; const R: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveFinalResponseFromTUInTryingState;
    procedure TestReceiveProvisionalResponseFromTUInProceedingState;
    procedure TestReceiveProvisionalResponseFromTUInTryingState;
    procedure TestReReceiveInitialRequestInCompletedState;
    procedure TestReReceiveInitialRequestInProceedingState;
    procedure TestResponseFromTUInCompletedState;
    procedure TestTimerJFired;
    procedure TestTransportErrorInCompletedState;
    procedure TestTransportErrorInProceedingState;
  end;

implementation

uses
  Classes, IdException, SysUtils, TestMessages, TypInfo;

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
  S: TStringStream;
begin
  inherited SetUp;

  Self.D := TIdSipTransactionDispatcher.Create;

  P := TIdSipParser.Create;
  try
    S := TStringStream.Create(BasicRequest);
    try
      P.Source := S;

      Self.ReceivedRequest := P.ParseAndMakeMessage as TIdSipRequest;
      S.Seek(0, soFromBeginning);
      Self.TranRequest := P.ParseAndMakeMessage as TIdSipRequest;
    finally
      S.Free;
    end;
  finally
    P.Free;
  end;

  Self.ReceivedResponse := TIdSipResponse.Create;
  Self.ReceivedResponse.StatusCode := SIPTrying;

  Self.ReceivedResponse.Headers.Add(Self.ReceivedRequest.Headers);
end;

procedure TestTIdSipTransactionDispatcher.TearDown;
begin
  Self.ReceivedResponse.Free;
  Self.TranRequest.Free;
  Self.ReceivedRequest.Free;

  Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipTransactionDispatcher Private methods ****************************
{
procedure TestTIdSipTransactionDispatcher.UseRFC2543Requests;
begin
  Self.ReceivedRequest.SIPVersion := 'SIP/1.0';
  Self.TranRequest.SIPVersion     := 'SIP/1.0';

  Self.ReceivedRequest.Path.LastHop.Value := 'SIP/1.0/TCP localhost';
  Self.TranRequest.Path.LastHop.Value     := 'SIP/1.0/TCP localhost';
end;
}
//* TestTIdSipTransactionDispatcher Published methods **************************

procedure TestTIdSipTransactionDispatcher.TestAddAndCountTransport;
var
  T1, T2: TIdSipMockTransport;
begin
  CheckEquals(0, Self.D.TransportCount, 'Initial value of TransportCount');

  T1 := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T1);
    CheckEquals(1, Self.D.TransportCount, 'After one AddTransport');

    T2 := TIdSipMockTransport.Create;
    try
      Self.D.AddTransport(T1);
      CheckEquals(2, Self.D.TransportCount, 'After two AddTransports');
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
  CheckEquals(0, Self.D.TransportCount, 'Initial value of TransportCount');

  T1 := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T1);

    T2 := TIdSipMockTransport.Create;
    try
      Self.D.AddTransport(T1);
      CheckEquals(2, Self.D.TransportCount, 'After two AddTransports');
      Self.D.ClearTransports;
      CheckEquals(0, Self.D.TransportCount, 'After Clear');
    finally
      T2.Free;
    end;
  finally
    T1.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestMatchInviteClient;
begin
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Identical headers');

  Self.ReceivedResponse.Headers.Add(ContentLanguageHeader).Value := 'es';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Identical headers + irrelevant headers');

  (Self.ReceivedResponse.Headers[FromHeaderFull] as TIdSipFromToHeader).Tag := '1';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different From tag');
  Self.ReceivedResponse.Headers[FromHeaderFull].Value := Self.TranRequest.Headers[FromHeaderFull].Value;

  (Self.ReceivedResponse.Headers[ToHeaderFull] as TIdSipFromToHeader).Tag := '1';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different To tag');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchInviteClientAckWithInvite;
begin
  Self.ReceivedResponse.CSeq.Method := MethodAck;
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'ACK match against INVITE');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchInviteClientDifferentCSeqMethod;
begin
  Self.ReceivedResponse.CSeq.Method := MethodCancel;

  Check(not Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different CSeq method');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchInviteClientDifferentViaBranch;
begin
  Self.ReceivedResponse.Path.LastHop.Branch := BranchMagicCookie + 'foo';

  Check(not Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different Via branch');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchInviteServer;
begin
  Check(Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'Identical INVITE request');

  Self.ReceivedRequest.Path.LastHop.SentBy := 'cougar';
  Check(not Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'Different sent-by');
  Self.ReceivedRequest.Path.LastHop.SentBy := Self.TranRequest.Path.LastHop.SentBy;

  Self.ReceivedRequest.Path.LastHop.Branch := 'z9hG4bK6';
  Check(not Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'Different branch');

  Self.ReceivedRequest.Path.LastHop.Branch := Self.TranRequest.Path.LastHop.Branch;
  Self.ReceivedRequest.Method := MethodAck;
  Check(Self.D.Match(Self.ReceivedRequest, Self.TranRequest), 'ACK');

  Self.ReceivedRequest.Path.LastHop.SentBy := 'cougar';
  Check(not Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'ACK but different sent-by');
  Self.ReceivedRequest.Path.LastHop.SentBy := Self.TranRequest.Path.LastHop.SentBy;

  Self.ReceivedRequest.Path.LastHop.Branch := 'z9hG4bK6';
  Check(not Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'ACK but different branch');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchNonInviteClient;
begin
  Self.ReceivedResponse.CSeq.Method := MethodCancel;
  Self.TranRequest.Method           := MethodCancel;

  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Identical headers');

  Self.ReceivedResponse.Headers.Add(ContentLanguageHeader).Value := 'es';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Identical headers + irrelevant headers');

  (Self.ReceivedResponse.Headers[FromHeaderFull] as TIdSipFromToHeader).Tag := '1';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different From tag');
  Self.ReceivedResponse.Headers[FromHeaderFull].Value := Self.TranRequest.Headers[FromHeaderFull].Value;

  (Self.ReceivedResponse.Headers[ToHeaderFull] as TIdSipFromToHeader).Tag := '1';
  Check(Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different To tag');

  Self.ReceivedResponse.CSeq.Method := MethodRegister;
  Check(not Self.D.Match(Self.ReceivedResponse, Self.TranRequest),
        'Different method');
end;

procedure TestTIdSipTransactionDispatcher.TestMatchNonInviteServer;
begin
  Self.ReceivedRequest.Method := MethodCancel;
  Self.TranRequest.Method     := MethodCancel;

  Check(Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'Identical CANCEL request');

  Self.ReceivedRequest.Method := MethodRegister;
  Check(not Self.D.Match(Self.ReceivedRequest, Self.TranRequest),
        'Different method');
end;

//******************************************************************************
//* TestTIdSipTransaction                                                      *
//******************************************************************************
//* TestTIdSipTransaction Published methods ************************************

procedure TestTIdSipTransaction.TestGetTransactionType;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Method := MethodInvite;
    CheckEquals(TIdSipClientInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client INVITE');

    R.Method := MethodAck;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client ACK');

    R.Method := MethodBye;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client BYE');

    R.Method := MethodCancel;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client CANCEL');

    R.Method := MethodOptions;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client OPTIONS');

    R.Method := MethodRegister;
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client REGISTER');

    R.Method := 'NewFangledMethod';
    CheckEquals(TIdSipClientNonInviteTransaction.ClassName,
                TIdSipTransaction.GetTransactionType(R).ClassName,
                'Client NewFangledMethod');
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipClientInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipClientInviteTransaction Public methods ***************************

procedure TestTIdSipClientInviteTransaction.SetUp;
begin
  inherited SetUp;

  // this is just BasicRequest from TestIdSipParser
  Self.InitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Method                           := MethodInvite;
  Self.InitialRequest.MaxForwards                      := 70;
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.From.DisplayName                 := 'Case';
  Self.InitialRequest.From.Address.URI                 := 'sip:case@fried.neurons.org';
  Self.InitialRequest.From.Tag                         := '1928301774';
  Self.InitialRequest.CallID                           := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.InitialRequest.CSeq.Method                      := 'INVITE';
  Self.InitialRequest.CSeq.SequenceNo                  := 314159;
  Self.InitialRequest.Headers[ContactHeaderFull].Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.InitialRequest.ContentType                      := 'text/plain';
  Self.InitialRequest.ContentLength                    := 29;
  Self.InitialRequest.Body                             := 'I am a message. Hear me roar!';

  Self.Response := TIdSipResponse.Create;

  Self.MockTransport := TIdSipMockTransport.Create;

  Self.Tran := TIdSipClientInviteTransaction.Create;

  Self.FailMsg               := '';
  Self.TransactionProceeding := false;
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionTerminated := false;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
end;

procedure TestTIdSipClientInviteTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.MockTransport.Free;
  Self.Response.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipClientInviteTransaction Private methods **************************

procedure TestTIdSipClientInviteTransaction.CheckACK(Sender: TObject; const R: TIdSipResponse);
var
  Ack:    TIdSipRequest;
  Routes: TIdSipHeadersFilter;
begin
  Ack := Self.MockTransport.LastACK;

  CheckEquals(MethodAck,                      Ack.Method,         'Method');
  CheckEquals(Self.InitialRequest.SipVersion, Ack.SipVersion,     'SIP-Version');
  CheckEquals(Self.InitialRequest.RequestUri, Ack.RequestUri,     'Request-URI');
  CheckEquals(Self.InitialRequest.CallID,     Ack.CallID,         'Call-ID');
  CheckEquals(Self.InitialRequest.From.Value, Ack.From.Value,     'From');
  CheckEquals(R.ToHeader.Value,               Ack.ToHeader.Value, 'To');

  CheckEquals(1, Ack.Path.Length, 'Number of Via headers');
  CheckEquals(Self.InitialRequest.Path.LastHop.Value,
              Ack.Path.LastHop.Value,
              'Topmost Via');

  CheckEquals(Self.InitialRequest.CSeq.SequenceNo, Ack.CSeq.SequenceNo, 'CSeq sequence no');
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

procedure TestTIdSipClientInviteTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
end;

procedure TestTIdSipClientInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState precondition');

  Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipClientInviteTransaction.MoveToProceedingState;
begin
  CheckEquals(Transaction(itsCalling),
              Transaction(Self.Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipClientInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

procedure TestTIdSipClientInviteTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipClientInviteTransaction.Terminated(Sender: TObject);
begin
  Self.TransactionTerminated := true;
end;

procedure TestTIdSipClientInviteTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

//* TestTIdSipClientInviteTransaction Published methods ************************

procedure TestTIdSipClientInviteTransaction.TestACK;
begin
  Self.Response.Headers.Add(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>';
  Self.Response.Headers.Add(RouteHeader).Value := 'localhost <sip:127.0.0.1>';

  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.CheckACK;
  Self.MoveToCompletedState;

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Sent ack');
end;

procedure TestTIdSipClientInviteTransaction.TestCompletedStateNetworkProblem;
begin
  Self.Tran.OnFail := Self.TransactionFail;

  Self.MoveToProceedingState;

  Response.StatusCode := SIPMultipleChoices;
  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Connection timed out');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsCalling),
              Transaction(Self.Tran.State),
              'Wrong initial state');
end;

procedure TestTIdSipClientInviteTransaction.TestInviteWithHostUnreachable;
begin
  Self.Tran.OnFail := Self.TransactionFail;

  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Connection timed out');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestMultipleInviteSending;
begin
  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(3, Self.MockTransport.SentRequestCount, 'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientInviteTransaction.TestNoInviteResendingInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.MockTransport.ResetSentRequestCount;
  Sleep(1000);
  CheckEquals(0, Self.MockTransport.SentRequestCount, 'Timer A wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransaction.TestNonInviteMethodInInitialRequest;
begin
  Self.InitialRequest.Method := MethodAck;

  try
    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Fail('Failed to bail out on non-INVITE method');
  except
  end;
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInCallingState;
begin
  Self.Tran.OnReceiveResponse := Self.Proceeding;

  Self.MoveToProceedingState;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.Tran.OnReceiveResponse := Self.Proceeding;
  Self.Response.StatusCode := SIPRinging;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxNoResendingOfRequest;
begin
  Self.MoveToProceedingState;

  Self.MockTransport.ResetSentRequestCount;
  Sleep(1500);
  CheckEquals(0, Self.MockTransport.SentRequestCount, 'Request was resent');

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCallingState;
begin
  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCallingState;
begin
  Self.Tran.OnReceiveResponse := Self.Completed;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Completed;
  Self.MoveToCompletedState;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(2, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Completed;

  Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReliableTransportNoInviteRetransmissions;
begin
  Self.InitialRequest.Path.LastHop.Transport := sttTCP;
  Self.MockTransport.ResetSentRequestCount;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(1, Self.MockTransport.SentRequestCount, 'Reliable transports should not resend INVITE');
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 250);

  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Sleep(500);

  Check(Self.TransactionTerminated, 'TimerD didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
end;

procedure TestTIdSipClientInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 500);
  Sleep(750);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Timeout');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

//******************************************************************************
//* TestTIdSipServerInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipServerInviteTransaction Public methods ***************************

procedure TestTIdSipServerInviteTransaction.SetUp;
begin
  inherited SetUp;

  // this is just BasicRequest from TestIdSipParser
  Self.InitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Method                           := MethodInvite;
  Self.InitialRequest.MaxForwards                      := 70;
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.From.DisplayName                 := 'Case';
  Self.InitialRequest.From.Address.URI                 := 'sip:case@fried.neurons.org';
  Self.InitialRequest.From.Tag                         := '1928301774';
  Self.InitialRequest.CallID                           := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.InitialRequest.CSeq.Method                      := 'INVITE';
  Self.InitialRequest.CSeq.SequenceNo                  := 314159;
  Self.InitialRequest.Headers[ContactHeaderFull].Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.InitialRequest.ContentType                      := 'text/plain';
  Self.InitialRequest.ContentLength                    := 29;
  Self.InitialRequest.Body                             := 'I am a message. Hear me roar!';

  Self.Request := TIdSipRequest.Create;
  Self.Request.Assign(Self.InitialRequest);
  Self.Response := TIdSipResponse.Create;

  Self.MockTransport := TIdSipMockTransport.Create;

  Self.Tran := TIdSipServerInviteTransaction.Create;

  Self.CheckReceiveInviteFired                                      := false;
  Self.CheckReceive2xxFromTUInProceedingStateFired                  := false;
  Self.CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired := false;
  Self.CheckSending100Fired                                         := false;
  Self.FailMsg                                                      := '';
  Self.InitialRequestSentToTU                                       := false;
  Self.TransactionProceeding                                        := false;
  Self.TransactionCompleted                                         := false;
  Self.TransactionConfirmed                                         := false;
  Self.TransactionFailed                                            := false;
  Self.TransactionTerminated                                        := false;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
end;

procedure TestTIdSipServerInviteTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.MockTransport.Free;
  Self.Response.Free;
  Self.Request.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipServerInviteTransaction Private methods **************************

procedure TestTIdSipServerInviteTransaction.CheckReceiveInvite(Sender: TObject; const R: TIdSipResponse);
begin
  CheckEquals(SIPRinging, R.StatusCode, 'Unexpected response sent');

  CheckEquals(1, R.Path.Length, 'Too many Via headers');
  CheckEquals(Self.InitialRequest.Path.LastHop.Value,
              R.Path.LastHop.Value,
              'Topmost Via');

  Self.CheckReceiveInviteFired := true;
end;

procedure TestTIdSipServerInviteTransaction.CheckReceive2xxFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
begin
  CheckEquals(SIPOK, R.StatusCode, 'Unexpected response sent');

  Self.CheckReceive2xxFromTUInProceedingStateFired := true;
end;

procedure TestTIdSipServerInviteTransaction.CheckReceiveNonTryingProvisionalFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
begin
  CheckEquals(SIPRinging, R.StatusCode, 'Unexpected response sent');

  Self.CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired := true;
end;

procedure TestTIdSipServerInviteTransaction.CheckSending100(Sender: TObject; const R: TIdSipResponse);
begin
  CheckEquals(SipVersion,  R.SipVersion, 'SIP-Version');
  CheckEquals(SIPTrying,   R.StatusCode, 'Status-Code');
  CheckEquals(RSSIPTrying, R.StatusText, 'Status-Text');

  CheckEquals('100', R.Headers[TimestampHeader].Value, 'Timestamp');

  CheckEquals(1, R.Path.Length, 'Via path');
  CheckEquals(Self.InitialRequest.Path.LastHop.Value,
              R.Path.LastHop.Value,
              'Topmost Via');

  Self.CheckSending100Fired := true;
end;

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
  Self.Tran.HandleMessage(Self.Request);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipServerInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

procedure TestTIdSipServerInviteTransaction.OnInitialRequestSentToTU(Sender: TObject; const R: TIdSipRequest);
begin
  Self.InitialRequestSentToTU := true;
end;

procedure TestTIdSipServerInviteTransaction.ReceiveInvite;
begin
  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  Self.MockTransport.OnResponse := Self.CheckReceiveInvite;
  Self.Tran.HandleMessage(Self.InitialRequest);

  Check(Self.CheckReceiveInviteFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.Terminated(Sender: TObject);
begin
  Self.TransactionTerminated := true;
end;

procedure TestTIdSipServerInviteTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

//* TestTIdSipServerInviteTransaction Published methods ************************

procedure TestTIdSipServerInviteTransaction.TestInitialRequestSentToTU;
begin
  Self.Tran.OnReceiveRequest := Self.OnInitialRequestSentToTU;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
  Check(InitialRequestSentToTU, 'Initial request not sent to TU');
end;

procedure TestTIdSipServerInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'Initial state');
end;

procedure TestTIdSipServerInviteTransaction.TestCompletedStateNetworkProblem;
begin
  Self.MoveToCompletedState;

  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockTransport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestProceedingStateNetworkProblem;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockTransport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReceive2xxFromTUInProceedingState;
begin
  Self.MockTransport.OnResponse := Self.CheckReceive2xxFromTUInProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              '200 from TU');

  Check(Self.CheckReceive2xxFromTUInProceedingStateFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveAckInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Request.Method := MethodAck;
  Self.Tran.HandleMessage(Self.Request);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              '200 from TU');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.ReceiveInvite;

  CheckEquals(Transaction(itsCompleted),
              Transaction(Tran.State),
              'Received an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInProceedingState;
begin
  Self.ReceiveInvite;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'Received an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  StatusCode: Cardinal;
begin
  for StatusCode := 300 to 699 do begin
    Self.Response.StatusCode := StatusCode;

    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.Tran.SendResponse(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Tran.State),
                'Received a ' + IntToStr(StatusCode) + ' from TU');
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
begin
  Self.MockTransport.OnResponse := Self.CheckReceiveNonTryingProvisionalFromTUInProceedingState;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Non-trying provisional');

  Check(Self.CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReliableTransportNoFinalResponseRetransmissions;
begin
  Self.InitialRequest.Path.LastHop.Transport := sttTLS;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Self.MoveToCompletedState;
  Self.MockTransport.ResetSentResponseCount;

  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(0, Self.MockTransport.SentResponseCount, 'Reliable transports should not resend final response');
end;

procedure TestTIdSipServerInviteTransaction.TestResponseRetransmissionInCompletedState;
begin
  Self.MoveToCompletedState;
  Self.MockTransport.ResetSentResponseCount;

  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(2, Self.MockTransport.SentResponseCount, 'Insufficient or too many responses sent');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerGStops;
begin
  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;

  Self.MockTransport.ResetSentResponseCount;
  Sleep(3*InitialT1);
  CheckEquals(0, Self.MockTransport.SentResponseCount, 'Timer G wasn''t stopped');
end;

procedure TestTIdSipServerInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 500);
  Self.MoveToCompletedState;
  Sleep(750);
  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Timeout');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestTimerIFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;
  Sleep(6000);

  Check(Self.TransactionTerminated, 'TimerI didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');  
end;

procedure TestTIdSipServerInviteTransaction.TestSending100;
begin
  Self.InitialRequest.Headers.Add(TimestampHeader).Value := '100';
  Self.MockTransport.OnResponse := Self.CheckSending100;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Check(Self.CheckSending100Fired, 'Event didn''t fire');
end;

//******************************************************************************
//* TestTIdSipClientNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipClientNonInviteTransaction Public methods ************************

procedure TestTIdSipClientNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  // this is just BasicRequest from TestIdSipParser
  Self.InitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Method                           := MethodOptions;
  Self.InitialRequest.MaxForwards                      := 70;
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.From.DisplayName                 := 'Case';
  Self.InitialRequest.From.Address.URI                 := 'sip:case@fried.neurons.org';
  Self.InitialRequest.From.Tag                         := '1928301774';
  Self.InitialRequest.CallID                           := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.InitialRequest.CSeq.Method                      := 'INVITE';
  Self.InitialRequest.CSeq.SequenceNo                  := 314159;
  Self.InitialRequest.Headers[ContactHeaderFull].Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.InitialRequest.ContentType                      := 'text/plain';
  Self.InitialRequest.ContentLength                    := 0;

  Self.Response := TIdSipResponse.Create;

  Self.MockTransport := TIdSipMockTransport.Create;

  Self.Tran := TIdSipClientNonInviteTransaction.Create;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Self.FailMsg               := '';
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionProceeding := false;
  Self.TransactionTerminated := false;
end;

procedure TestTIdSipClientNonInviteTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.MockTransport.Free;
  Self.Response.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipClientNonInviteTransaction Private methods ***********************

procedure TestTIdSipClientNonInviteTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
end;

procedure TestTIdSipClientNonInviteTransaction.MoveToProceedingState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipClientNonInviteTransaction.MoveToCompletedState;
begin
  Check(Self.Tran.State in [itsTrying, itsProceeding],
        'Unexpected state '
      + Transaction(Self.Tran.State)
      + ' in MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipClientNonInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

procedure TestTIdSipClientNonInviteTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipClientNonInviteTransaction.Terminated(Sender: TObject);
begin
  Self.TransactionTerminated := true;
end;

procedure TestTIdSipClientNonInviteTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

//* TestTIdSipClientNonInviteTransaction Published methods *********************

procedure TestTIdSipClientNonInviteTransaction.TestInitialRequestSent;
begin
  CheckEquals(1,
              Self.MockTransport.SentRequestCount,
              'Too many or too few requests sent');
end;

procedure TestTIdSipClientNonInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'Incorrect initial state');
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.MockTransport.ResetSentRequestCount;
  // TimerE is supposed to now have an interval of 4s
  Sleep(5000);
  CheckEquals(1, Self.MockTransport.SentRequestCount, 'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInTryingState;
begin
  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(3, Self.MockTransport.SentRequestCount, 'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Proceeding;

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInTryingState;
begin
  Self.Tran.OnReceiveResponse := Self.Proceeding;
  Self.MoveToProceedingState;

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceiveFinalResponseInProceedingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.TransactionCompleted := false;
    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MoveToProceedingState;

    Self.Tran.OnReceiveResponse := Self.Completed;
    Self.Response.StatusCode := 100*I;
    Self.Tran.HandleMessage(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

    Check(Self.TransactionCompleted, 'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceiveFinalResponseInTryingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.TransactionCompleted := false;
    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

    Self.Tran.OnReceiveResponse := Self.Completed;
    Self.Response.StatusCode := 100*I;
    Self.Tran.HandleMessage(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

    Check(Self.TransactionCompleted, 'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 500);
  Sleep(750);
  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Timeout');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimerKFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Sleep(6000);

  Check(Self.TransactionTerminated, 'TimerK didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTryingStateTransportError;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

//******************************************************************************
//* TestTIdSipServerNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipServerNonInviteTransaction Public methods ************************

procedure TestTIdSipServerNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  // this is just BasicRequest from TestIdSipParser
  Self.InitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Method                           := MethodOptions;
  Self.InitialRequest.MaxForwards                      := 70;
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
  Self.InitialRequest.From.DisplayName                 := 'Case';
  Self.InitialRequest.From.Address.URI                 := 'sip:case@fried.neurons.org';
  Self.InitialRequest.From.Tag                         := '1928301774';
  Self.InitialRequest.CallID                           := 'a84b4c76e66710@gw1.leo-ix.org';
  Self.InitialRequest.CSeq.Method                      := 'INVITE';
  Self.InitialRequest.CSeq.SequenceNo                  := 314159;
  Self.InitialRequest.Headers[ContactHeaderFull].Value := 'sip:wintermute@tessier-ashpool.co.lu';
  Self.InitialRequest.ContentType                      := 'text/plain';
  Self.InitialRequest.ContentLength                    := 29;
  Self.InitialRequest.Body                             := 'I am a message. Hear me roar!';

  Self.Request := TIdSipRequest.Create;
  Self.Request.Assign(Self.InitialRequest);
  Self.Response := TIdSipResponse.Create;

  Self.MockTransport := TIdSipMockTransport.Create;

  Self.Tran := TIdSipServerNonInviteTransaction.Create;

  Self.FailMsg               := '';
  Self.TransactionProceeding := false;
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionTerminated := false;
  Self.TransactionTrying     := false;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
end;

procedure TestTIdSipServerNonInviteTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.MockTransport.Free;
  Self.Response.Free;
  Self.Request.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipServerNonInviteTransaction Private methods ***********************

procedure TestTIdSipServerNonInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState precondition');

  Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');

end;

procedure TestTIdSipServerNonInviteTransaction.MoveToProceedingState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipServerNonInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

procedure TestTIdSipServerNonInviteTransaction.Terminated(Sender: TObject);
begin
  Self.TransactionTerminated := true;
end;

procedure TestTIdSipServerNonInviteTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

procedure TestTIdSipServerNonInviteTransaction.Trying(Sender: TObject; const R: TIdSipRequest);
begin
  Self.TransactionTrying := true;
end;

//* TestTIdSipServerNonInviteTransaction Published methods *********************

procedure TestTIdSipServerNonInviteTransaction.TestInitialRequestSentToTU;
begin
  Self.Tran.OnReceiveRequest := Self.Trying;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Check(Self.TransactionTrying, 'TU not informed of initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'Incorrect initial state');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MoveToProceedingState;
    Self.Response.StatusCode := I*100;

    Self.MockTransport.ResetSentResponseCount;
    Self.Tran.HandleMessage(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

    CheckEquals(1,
                Self.MockTransport.SentResponseCount,
                'Transport wasn''t given a '
              + IntToStr(Self.Response.StatusCode)
              + ' Response to send');
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInTryingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.ResetSentResponseCount;
    Self.Response.StatusCode := I*100;
    Self.Tran.HandleMessage(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

    CheckEquals(1,
                Self.MockTransport.SentResponseCount,
                'Transport wasn''t given a '
              + IntToStr(Self.Response.StatusCode)
              + ' Response to send');
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveProvisionalResponseFromTUInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.MockTransport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

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
  Self.Tran.HandleMessage(Self.Response);

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
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Self.MockTransport.ResetSentResponseCount;

  Self.Tran.HandleMessage(Self.InitialRequest);

  CheckEquals(1,
              Self.MockTransport.SentResponseCount,
              'Response not sent to re-received initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReReceiveInitialRequestInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.MockTransport.ResetSentResponseCount;

  Self.Tran.HandleMessage(Self.InitialRequest);

  CheckEquals(1,
              Self.MockTransport.SentResponseCount,
              'Response not sent to re-received initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestResponseFromTUInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.MockTransport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(0, Self.MockTransport.SentResponseCount, 'Response from TU wasn''t dropped on the floor');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTimerJFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 100);

  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Sleep(200);

  Check(Self.TransactionTerminated, 'TimerJ didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInCompletedState;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleMessage(Self.InitialRequest);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed, 'TU not informed of transport error');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MoveToProceedingState;

  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleMessage(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed, 'TU not informed of transport error');
end;

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
