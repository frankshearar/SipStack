unit TestIdSipTransaction;

interface

uses
  IdSipMessage, IdSipTransport, IdSipTransaction, TestFramework;

  // Transactions behave slightly differently if a reliable transport is used -
  // certain messages are not resent. To this end, we test unreliable transports
  // by default, only checking that those certain messages are not resent when
  // using reliable transports in tests like TestReliableTransportFoo
type
  TestTIdSipTransactionDispatcher = class(TTestCase)
  private
    D:                TIdSipTransactionDispatcher;
    Invite:           TIdSipRequest;
    Options:          TIdSipRequest;
    ReceivedRequest:  TIdSipRequest;
    ReceivedResponse: TIdSipResponse;
    Response200:      TIdSipResponse;
    TranRequest:      TIdSipRequest;
  private
    procedure CheckDispatchToCorrectTransaction(Sender: TObject; const R: TIdSipResponse);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCountTransport;
    procedure TestClearTransports;
    procedure TestCreateNewTransaction;
    procedure TestDispatchToCorrectTransaction;
    procedure TestDropUnmatchedResponse;
    procedure TestMatchInviteClient;
    procedure TestMatchInviteClientAckWithInvite;
    procedure TestMatchInviteClientDifferentCSeqMethod;
    procedure TestMatchInviteClientDifferentViaBranch;
    procedure TestMatchInviteServer;
    procedure TestMatchNonInviteClient;
    procedure TestMatchNonInviteServer;
    procedure TestWillUseReliableTransport;
  end;

  TestTIdSipTransaction = class(TTestCase)
  published
    procedure TestGetTransactionType;
  end;

  TTestTransaction = class(TTestCase)
  protected
    FailMsg:               String;
    InitialRequest:        TIdSipRequest;
    MockDispatcher:        TIdSipMockTransactionDispatcher;
    Response:              TIdSipResponse;
    Tran:                  TIdSipTransaction;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;
    TransactionProceeding: Boolean;
    TransactionTerminated: Boolean;

    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
    procedure TransactionFail(Sender: TObject; const Reason: String);
    procedure Terminated(Sender: TObject);
    function  TransactionType: TIdSipTransactionClass; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipClientInviteTransaction = class(TTestTransaction)
  private
    procedure CheckACK(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToProceedingState;
    procedure OnFail(Sender: TObject; const Reason: String);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  published
    procedure Test100ResponseInCompletedState;
    procedure Test100ResponseInTerminatedState;
    procedure Test200ResponseInCompletedState;
    procedure TestACK;
    procedure TestTranportErrorInCompletedState;
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

  TestTIdSipServerInviteTransaction = class(TTestTransaction)
  private
    CheckReceiveInviteFired:                                      Boolean;
    CheckReceive2xxFromTUInProceedingStateFired:                  Boolean;
    CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired: Boolean;
    CheckSending100Fired:                                         Boolean;
    Request:                                                      TIdSipRequest;
    TransactionConfirmed:                                         Boolean;

    procedure CheckReceiveInvite(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceive2xxFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceiveNonTryingProvisionalFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSending100(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure OnInitialRequestSentToTU(Sender: TObject; const R: TIdSipRequest);
    procedure ReceiveInvite;
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
    procedure TestReceive2xxFromTUInProceedingState;
    procedure TestReceiveAckInCompletedState;
    procedure TestReceiveFinalResponseFromTUInProceedingState;
    procedure TestReceiveInviteInCompletedState;
    procedure TestReceiveInviteInConfirmedState;
    procedure TestReceiveInviteInProceedingState;
    procedure TestReceiveInviteInTerminatedState;
    procedure TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
    procedure TestReliableTransportNoFinalResponseRetransmissions;
    procedure TestResponseRetransmissionInCompletedState;
    procedure TestSending100;
    procedure TestTimerGStops;
    procedure TestTimeout;
    procedure TestTimerIFired;
    procedure TestTranportErrorInCompletedState;
    procedure TestTransportErrorInProceedingState;
  end;

  TestTIdSipClientNonInviteTransaction = class(TTestTransaction)
  private
    procedure MoveToProceedingState;
    procedure MoveToCompletedState;
    procedure OnFail(Sender: TObject; const Reason: String);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestInitialRequestSent;
    procedure TestInitialState;
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

  TestTIdSipServerNonInviteTransaction = class(TTestTransaction)
  private
    Request:           TIdSipRequest;
    TransactionTrying: Boolean;

    procedure MoveToCompletedState;
    procedure MoveToProceedingState;
    procedure OnFail(Sender: TObject; const Reason: String);
    procedure Trying(Sender: TObject; const R: TIdSipRequest);
  protected
    function TransactionType: TIdSipTransactionClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialRequestSentToTU;
    procedure TestInitialState;
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
  end;

implementation

uses
  Classes, IdException, IdSipConsts, IdSipHeaders, SysUtils, TestMessages,
  TypInfo;

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

    S := TStringStream.Create(BasicResponse);
    try
      P.Source := S;
      Self.ReceivedResponse := P.ParseAndMakeMessage as TIdSipResponse;
    finally
      S.Free;
    end;
  finally
    P.Free;
  end;

  Self.ReceivedResponse.StatusCode := SIPTrying;

  Self.ReceivedResponse.Headers.Add(Self.ReceivedRequest.Headers);

  Self.Invite := TIdSipRequest.Create;
  Self.Invite.Assign(Self.ReceivedRequest);

  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.ReceivedRequest);
  Self.Options.Method := MethodOptions;

  Self.Response200 := TIdSipResponse.Create;
  Self.Response200.Assign(Self.ReceivedResponse);
  Self.Response200.StatusCode := SIPOK;
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

  inherited TearDown;
end;

//* TestTIdSipTransactionDispatcher Private methods ****************************

procedure TestTIdSipTransactionDispatcher.CheckDispatchToCorrectTransaction(Sender: TObject; const R: TIdSipResponse);
begin
  CheckEquals(TIdSipServerInviteTransaction.ClassName,
              Sender.ClassName,
              'INVITE transaction');
end;
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

procedure TestTIdSipTransactionDispatcher.TestCreateNewTransaction;
var
  T: TIdSipMockTransport;
begin
  T := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T);

    CheckEquals(0,
                Self.D.TransactionCount,
                'Initially there should be no transactions');

    T.FireOnRequest(Self.Invite);

    CheckEquals(1,
                Self.D.TransactionCount,
                'No new transaction was created');
  finally
    T.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestDispatchToCorrectTransaction;
var
  T:           TIdSipMockTransport;
  InviteTran:  TIdSipTransaction;
  OptionsTran: TIdSipTransaction;
begin
  T := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T);

    T.FireOnRequest(Self.Invite);
    T.FireOnRequest(Self.Options);
    CheckEquals(2, Self.D.TransactionCount, 'Sanity check on transaction count');

    InviteTran  := Self.D.TransactionAt(0);
    OptionsTran := Self.D.TransactionAt(0);

    InviteTran.OnReceiveResponse  := Self.CheckDispatchToCorrectTransaction;
    OptionsTran.OnReceiveResponse := Self.CheckDispatchToCorrectTransaction;

    T.FireOnResponse(Self.Response200);
  finally
    T.Free;
  end;
end;

procedure TestTIdSipTransactionDispatcher.TestDropUnmatchedResponse;
var
  T: TIdSipMockTransport;
begin
  T := TIdSipMockTransport.Create;
  try
    Self.D.AddTransport(T);

    CheckEquals(0,
                Self.D.TransactionCount,
                'Initially there should be no transactions');

    T.FireOnResponse(Self.ReceivedResponse);

    CheckEquals(0,
                Self.D.TransactionCount,
                'Response wasn''t dropped');
  finally
    T.Free;
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

procedure TestTIdSipTransactionDispatcher.TestWillUseReliableTransport;
var
  R: TIdSipRequest;
begin
  R := TIdSipRequest.Create;
  try
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';

    Check(Self.D.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/TLS gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.D.WillUseReliableTranport(R), 'TLS');

    R.Path.LastHop.Value := 'SIP/2.0/UDP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(not Self.D.WillUseReliableTranport(R), 'TCP');

    R.Path.LastHop.Value := 'SIP/2.0/SCTP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    Check(Self.D.WillUseReliableTranport(R), 'SCTP');
  finally
    R.Free;
  end;
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
//* TTestTransaction                                                           *
//******************************************************************************
//* TTestTransaction Public methods ********************************************

procedure TTestTransaction.SetUp;
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

  Self.FailMsg               := '';
  Self.MockDispatcher         := TIdSipMockTransactionDispatcher.Create;
  Self.Response              := TIdSipResponse.Create;
  Self.Tran                  := Self.TransactionType.Create;
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
  Self.TransactionProceeding := false;
  Self.TransactionTerminated := false;

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
end;

procedure TTestTransaction.TearDown;
begin
  Self.Tran.Free;
  Self.MockDispatcher.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TTestTransaction Protected methods *****************************************

procedure TTestTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
end;

procedure TTestTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
end;

procedure TTestTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

procedure TTestTransaction.Terminated(Sender: TObject);
begin
  Self.TransactionTerminated := true;
end;

//******************************************************************************
//* TestTIdSipClientInviteTransaction                                          *
//******************************************************************************
//* TestTIdSipClientInviteTransaction Private methods **************************

procedure TestTIdSipClientInviteTransaction.CheckACK(Sender: TObject; const R: TIdSipResponse);
var
  Ack:    TIdSipRequest;
  Routes: TIdSipHeadersFilter;
begin
  Ack := Self.MockDispatcher.Transport.LastACK;

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

procedure TestTIdSipClientInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

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
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState postcondition');
end;

procedure TestTIdSipClientInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

function TestTIdSipClientInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientInviteTransaction;
end;

//* TestTIdSipClientInviteTransaction Published methods ************************

procedure TestTIdSipClientInviteTransaction.Test100ResponseInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Completed state');
end;

procedure TestTIdSipClientInviteTransaction.Test100ResponseInTerminatedState;
begin
  // This test is a sanity check. We should never ever manage to get
  // a response in the Terminated state.

  Self.MoveToProceedingState;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Transport layer failed');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received a 1xx in the Terminated state');
end;

procedure TestTIdSipClientInviteTransaction.Test200ResponseInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'Received a 2xx in the Completed state');
end;

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

procedure TestTIdSipClientInviteTransaction.TestTranportErrorInCompletedState;
begin
  Self.Tran.OnFail := Self.TransactionFail;

  Self.MoveToProceedingState;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleResponse(Self.Response);

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

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

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
  CheckEquals(3,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientInviteTransaction.TestNoInviteResendingInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Sleep(1000);
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Timer A wasn''t stopped');
end;

procedure TestTIdSipClientInviteTransaction.TestNonInviteMethodInInitialRequest;
begin
  Self.InitialRequest.Method := MethodAck;

  try
    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
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
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxNoResendingOfRequest;
begin
  Self.MoveToProceedingState;

  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Sleep(1500);
  CheckEquals(0, Self.MockDispatcher.Transport.SentRequestCount, 'Request was resent');

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'State on receiving a 100');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCallingState;
begin
  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCallingState;
begin
  Self.Tran.OnReceiveResponse := Self.Completed;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Completed;
  Self.MoveToCompletedState;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(2, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Completed;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockDispatcher.Transport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReliableTransportNoInviteRetransmissions;
begin
  Self.InitialRequest.Path.LastHop.Transport := sttTCP;
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(1,
              Self.MockDispatcher.Transport.SentRequestCount,
              'Reliable transports should not resend INVITE');
end;

procedure TestTIdSipClientInviteTransaction.TestTimerDFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 250);

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
  
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 500);

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

  Self.Request := TIdSipRequest.Create;
  Self.Request.Assign(Self.InitialRequest);

  Self.CheckReceiveInviteFired                                      := false;
  Self.CheckReceive2xxFromTUInProceedingStateFired                  := false;
  Self.CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired := false;
  Self.CheckSending100Fired                                         := false;
  Self.TransactionConfirmed                                         := false;
end;

procedure TestTIdSipServerInviteTransaction.TearDown;
begin
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipServerInviteTransaction Protected methods ************************

function TestTIdSipServerInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipServerInviteTransaction;
end;

//* TestTIdSipServerInviteTransaction Private methods **************************

procedure TestTIdSipServerInviteTransaction.CheckReceiveInvite(Sender: TObject; const R: TIdSipResponse);
begin
//  CheckEquals(SIPRinging, R.StatusCode, 'Unexpected response sent');

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
  Self.Tran.HandleResponse(Self.Response);

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
  Self.Tran.HandleRequest(Self.Request);

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
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipServerInviteTransaction.ReceiveInvite;
begin
//  Self.Response.StatusCode := SIPRinging;
//  Self.Tran.HandleResponse(Self.Response);

  Self.MockDispatcher.Transport.OnResponse := Self.CheckReceiveInvite;
  Self.Tran.HandleRequest(Self.InitialRequest);

  Check(Self.CheckReceiveInviteFired, 'Event didn''t fire');
end;

//* TestTIdSipServerInviteTransaction Published methods ************************

procedure TestTIdSipServerInviteTransaction.TestInitialRequestSentToTU;
begin
  Self.Tran.OnReceiveRequest := Self.OnInitialRequestSentToTU;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
  Check(Self.TransactionProceeding, 'Initial request not sent to TU');
end;

procedure TestTIdSipServerInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Tran.State),
              'Initial state');
end;

procedure TestTIdSipServerInviteTransaction.TestReceive2xxFromTUInProceedingState;
begin
  Self.MockDispatcher.Transport.OnResponse := Self.CheckReceive2xxFromTUInProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              '200 from TU');

  Check(Self.CheckReceive2xxFromTUInProceedingStateFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveAckInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Request.Method := MethodAck;
  Self.Tran.HandleRequest(Self.Request);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Self.Tran.State),
              '200 from TU');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Tran.OnReceiveResponse := Self.Completed;
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

  Self.Tran.HandleRequest(Self.InitialRequest);

  CheckEquals(Transaction(itsConfirmed),
              Transaction(Tran.State),
              'Received an INVITE');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'After receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInProceedingState;
begin
  Self.Tran.OnReceiveResponse := Self.Proceeding;
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
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Tran.HandleRequest(Self.InitialRequest);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Received an INVITE');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response count after receiving an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  StatusCode: Cardinal;
begin
  for StatusCode := 300 to 699 do begin
    Self.Response.StatusCode := StatusCode;

    Self.TransactionCompleted := false;
    Self.Tran.OnReceiveResponse := Self.Completed;

    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
    Self.Tran.HandleResponse(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Tran.State),
                'Received a ' + IntToStr(StatusCode) + ' from TU');

    Check(Self.TransactionCompleted,
          'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
begin
  Self.MockDispatcher.Transport.OnResponse := Self.CheckReceiveNonTryingProvisionalFromTUInProceedingState;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Non-trying provisional');

  Check(Self.CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired,
        'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReliableTransportNoFinalResponseRetransmissions;
begin
  Self.InitialRequest.Path.LastHop.Transport := sttTLS;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Self.MoveToCompletedState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Reliable transports should not resend final response');
end;

procedure TestTIdSipServerInviteTransaction.TestResponseRetransmissionInCompletedState;
begin
  Self.MoveToCompletedState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  Sleep(3*InitialT1 + (InitialT1 div 2));
  CheckEquals(2,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Insufficient or too many responses sent');
end;

procedure TestTIdSipServerInviteTransaction.TestSending100;
begin
  Self.InitialRequest.Headers.Add(TimestampHeader).Value := '100';
  Self.MockDispatcher.Transport.OnResponse := Self.CheckSending100;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Check(Self.CheckSending100Fired, 'Event didn''t fire');
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

procedure TestTIdSipServerInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 500);
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

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Self.MoveToCompletedState;
  Self.MoveToConfirmedState;
  Sleep(6000);

  Check(Self.TransactionTerminated, 'TimerI didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
end;

procedure TestTIdSipServerInviteTransaction.TestTranportErrorInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

//******************************************************************************
//* TestTIdSipClientNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipClientNonInviteTransaction Public methods ************************

procedure TestTIdSipClientNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.InitialRequest.Method := MethodOptions;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
end;

//* TestTIdSipClientNonInviteTransaction Protected methods *********************

function TestTIdSipClientNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipClientNonInviteTransaction;
end;

//* TestTIdSipClientNonInviteTransaction Private methods ***********************

procedure TestTIdSipClientNonInviteTransaction.MoveToProceedingState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'MoveToProceedingState precondition');

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

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
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipClientNonInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

//* TestTIdSipClientNonInviteTransaction Published methods *********************

procedure TestTIdSipClientNonInviteTransaction.TestInitialRequestSent;
begin
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
  CheckEquals(1,
              Self.MockDispatcher.Transport.SentRequestCount,
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
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  // TimerE is supposed to now have an interval of 4s
  Sleep(5000);
  CheckEquals(1, Self.MockDispatcher.Transport.SentRequestCount, 'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientNonInviteTransaction.TestMultipleRequestSendingInTryingState;
begin
  Self.MockDispatcher.Transport.ResetSentRequestCount;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
  // messages being sent.
  Sleep(2000);
  CheckEquals(3, Self.MockDispatcher.Transport.SentRequestCount, 'Insufficient or too many requests sent');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.Tran.OnReceiveResponse := Self.Completed;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  Check(not Self.TransactionCompleted,
        'Response not dropped');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.Tran.OnReceiveResponse := Self.Proceeding;

  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientNonInviteTransaction.TestReceive1xxInTerminatedState;
begin
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  Self.Tran.OnReceiveResponse := Self.Completed;
  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

  Check(not Self.TransactionCompleted,
        'Response not dropped');
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
    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
    Self.MoveToProceedingState;

    Self.Tran.OnReceiveResponse := Self.Completed;
    Self.Response.StatusCode := 100*I;
    Self.Tran.HandleResponse(Self.Response);

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
    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

    Self.Tran.OnReceiveResponse := Self.Completed;
    Self.Response.StatusCode := 100*I;
    Self.Tran.HandleResponse(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'Received a ' + IntToStr(Self.Response.StatusCode) + ' in Trying state');

    Check(Self.TransactionCompleted, 'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipClientNonInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 500);

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

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Sleep(6000);

  Check(Self.TransactionTerminated, 'TimerK didn''t fire');

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Terminated');

  CheckEquals('', Self.FailMsg, 'Unexpected fail');
end;

procedure TestTIdSipClientNonInviteTransaction.TestTransportErrorInTryingState;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Check(Self.TransactionFailed,
        'Event didn''t fire');
end;

//******************************************************************************
//* TestTIdSipServerNonInviteTransaction                                       *
//******************************************************************************
//* TestTIdSipServerNonInviteTransaction Public methods ************************

procedure TestTIdSipServerNonInviteTransaction.SetUp;
begin
  inherited SetUp;

  Self.InitialRequest.Method := MethodOptions;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Self.Request := TIdSipRequest.Create;
  Self.Request.Assign(Self.InitialRequest);

  Self.TransactionTrying := false;
end;

procedure TestTIdSipServerNonInviteTransaction.TearDown;
begin
  Self.Request.Free;

  inherited TearDown;
end;

//* TestTIdSipServerNonInviteTransaction Protected methods *********************

function TestTIdSipServerNonInviteTransaction.TransactionType: TIdSipTransactionClass;
begin
  Result := TIdSipServerNonInviteTransaction;
end;

//* TestTIdSipServerNonInviteTransaction Private methods ***********************

procedure TestTIdSipServerNonInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToCompletedState precondition');

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.HandleResponse(Self.Response);

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
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsProceeding),
              Transaction(Self.Tran.State),
              'MoveToProceedingState postcondition');
end;

procedure TestTIdSipServerNonInviteTransaction.OnFail(Sender: TObject; const Reason: String);
begin
  Self.FailMsg := Reason;
end;

procedure TestTIdSipServerNonInviteTransaction.Trying(Sender: TObject; const R: TIdSipRequest);
begin
  Self.TransactionTrying := true;
end;

//* TestTIdSipServerNonInviteTransaction Published methods *********************

procedure TestTIdSipServerNonInviteTransaction.TestInitialRequestSentToTU;
begin
  Self.Tran.OnReceiveRequest := Self.Trying;
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);

  Check(Self.TransactionTrying, 'TU not informed of initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestInitialState;
begin
  CheckEquals(Transaction(itsTrying),
              Transaction(Self.Tran.State),
              'Incorrect initial state');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInCompletedState;
var
  SentResponseCount: Cardinal;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsCompleted),
              Transaction(Self.Tran.State),
              'State - response not simply ignored');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'SentResponseCount - response not simply ignored');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInProceedingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
    Self.MoveToProceedingState;
    Self.Response.StatusCode := I*100;

    Self.TransactionCompleted := false;
    Self.Tran.OnReceiveResponse := Self.Completed;

    Self.MockDispatcher.Transport.ResetSentResponseCount;
    Self.Tran.HandleResponse(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

    CheckEquals(1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Transport wasn''t given a '
              + IntToStr(Self.Response.StatusCode)
              + ' Response to send');

    Check(Self.TransactionCompleted, 'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInTerminatedState;
var
  SentResponseCount: Cardinal;
begin
  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 100);
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Sleep(150);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Transaction not yet timed out');

  SentResponseCount := Self.MockDispatcher.Transport.SentResponseCount;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'State - response not simply ignored');

  CheckEquals(SentResponseCount,
              Self.MockDispatcher.Transport.SentResponseCount,
              'SentResponseCount - response not simply ignored');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveFinalResponseFromTUInTryingState;
var
  I: Integer;
begin
  for I := 2 to 6 do begin
    Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest);
    Self.MockDispatcher.Transport.ResetSentResponseCount;
    Self.Response.StatusCode := I*100;

    Self.TransactionCompleted := false;
    Self.Tran.OnReceiveResponse := Self.Completed;

    Self.Tran.HandleResponse(Self.Response);

    CheckEquals(Transaction(itsCompleted),
                Transaction(Self.Tran.State),
                'TU gave us a ' + IntToStr(Self.Response.StatusCode) + ' Response');

    CheckEquals(1,
                Self.MockDispatcher.Transport.SentResponseCount,
                'Transport wasn''t given a '
              + IntToStr(Self.Response.StatusCode)
              + ' Response to send');

    Check(Self.TransactionCompleted, 'Event didn''t fire: ' + IntToStr(Self.Response.StatusCode));
  end;
end;

procedure TestTIdSipServerNonInviteTransaction.TestReceiveProvisionalResponseFromTUInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.MockDispatcher.Transport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

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
  Self.Tran.HandleResponse(Self.Response);

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
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  Self.Tran.HandleRequest(Self.InitialRequest);

  CheckEquals(1,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response not sent to re-received initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestReReceiveInitialRequestInProceedingState;
begin
  Self.MoveToProceedingState;
  Self.MockDispatcher.Transport.ResetSentResponseCount;

  Self.Tran.HandleRequest(Self.InitialRequest);

  CheckEquals(1,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response not sent to re-received initial request');
end;

procedure TestTIdSipServerNonInviteTransaction.TestResponseFromTUInCompletedState;
begin
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.MockDispatcher.Transport.ResetSentResponseCount;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(0,
              Self.MockDispatcher.Transport.SentResponseCount,
              'Response from TU wasn''t dropped on the floor');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTimerJFired;
begin
  Self.Tran.OnFail       := Self.OnFail;
  Self.Tran.OnTerminated := Self.Terminated;

  Self.Tran.Initialise(Self.MockDispatcher, Self.InitialRequest, 100);

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

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Tran.HandleRequest(Self.InitialRequest);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'TU not informed of transport error');
end;

procedure TestTIdSipServerNonInviteTransaction.TestTransportErrorInProceedingState;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MoveToProceedingState;

  Self.MockDispatcher.Transport.FailWith := EIdConnectTimeout;
  Self.Response.StatusCode := SIPTrying;
  Self.Tran.HandleResponse(Self.Response);

  CheckEquals(Transaction(itsTerminated),
              Transaction(Self.Tran.State),
              'Error trying to send a response');
  Check(Self.TransactionFailed,
        'TU not informed of transport error');
end;

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
