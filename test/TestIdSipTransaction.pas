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

  TestTIdSipClientInviteTransaction = class(TTestCase)
  private
    InitialRequest:        TIdSipRequest;
    MockTransport:         TIdSipMockTransport;
    Response:              TIdSipResponse;
    Tran:                  TIdSipClientInviteTransaction;
    TransactionProceeding: Boolean;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;

    procedure CheckACK(Sender: TObject; const R: TIdSipResponse);
    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToProceedingState;
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
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
    procedure TestTimeout;
    procedure TestReceive1xxInCallingState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxNoResendingOfRequest;
    procedure TestReceive2xxInCallingState;
    procedure TestReceive2xxInProceedingState;
    procedure TestReceive3xxInCallingState;
    procedure TestReceive3xxInCompletedState;
    procedure TestReceive3xxInProceedingState;
    procedure TestReliableTransportNoInviteRetransmissions;
  end;

  TestTIdSipServerInviteTransaction = class(TTestCase)
  private
    CheckReceiveInviteFired:                                      Boolean;
    CheckReceive2xxFromTUInProceedingStateFired:                  Boolean;
    CheckReceiveNonTryingProvisionalFromTUInProceedingStateFired: Boolean;
    CheckSending100Fired:                                         Boolean;
    InitialRequest:                                               TIdSipRequest;
    MockTransport:                                                TIdSipMockTransport;
    Request:                                                      TIdSipRequest;
    Response:                                                     TIdSipResponse;
    Tran:                                                         TIdSipServerInviteTransaction;
    TransactionProceeding:                                        Boolean;
    TransactionCompleted:                                         Boolean;
    TransactionConfirmed:                                         Boolean;
    TransactionFailed:                                            Boolean;

    procedure CheckReceiveInvite(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceive2xxFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckReceiveNonTryingProvisionalFromTUInProceedingState(Sender: TObject; const R: TIdSipResponse);
    procedure CheckSending100(Sender: TObject; const R: TIdSipResponse);
    procedure MoveToCompletedState;
    procedure MoveToConfirmedState;
    procedure ReceiveInvite;
    procedure TransactionFail(Sender: TObject; const Reason: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
    procedure TestSending100;
  end;

implementation

uses
  IdException, SysUtils, TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipClientInviteTransaction.Suite);
  Result.AddTest(TestTIdSipServerInviteTransaction.Suite);
end;

function InviteStateToStr(const S: TIdSipInviteTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipInviteTransactionState), Integer(S));
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

  Self.TransactionProceeding := false;
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;

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
  CheckEquals(Self.InitialRequest.Request,    Ack.Request,        'Request-URI');
  CheckEquals(Self.InitialRequest.CallID,     Ack.CallID,         'Call-ID');
  CheckEquals(Self.InitialRequest.From.Value, Ack.From.Value,     'From');
  CheckEquals(R.ToHeader.Value,               Ack.ToHeader.Value, 'To');

  CheckEquals(1, Ack.Path.Length, 'Number of Via headers');
  CheckEquals(Self.InitialRequest.Path.FirstHop.Value,
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
  Response.StatusCode := SIPMultipleChoices;
  Self.MockTransport.FireOnResponse(Self.Response);
end;

procedure TestTIdSipClientInviteTransaction.MoveToProceedingState;
begin
  Self.Response.StatusCode := SIPTrying;
  Self.MockTransport.FireOnResponse(Self.Response);
end;

procedure TestTIdSipClientInviteTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
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

  Self.Tran.OnCompleted := Self.CheckACK;

  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Self.Tran.State),
              'Sent ack');
end;

procedure TestTIdSipClientInviteTransaction.TestCompletedStateNetworkProblem;
begin
  Self.Tran.OnFail := Self.TransactionFail;

  Self.MoveToProceedingState;

  Response.StatusCode := SIPMultipleChoices;
  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              'Connection timed out');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestInitialState;
begin
  CheckEquals(InviteStateToStr(itsCalling),
              InviteStateToStr(Self.Tran.State),
              'Wrong initial state');
end;

procedure TestTIdSipClientInviteTransaction.TestInviteWithHostUnreachable;
begin
  Self.Tran.OnFail := Self.TransactionFail;

  Self.MockTransport.FailWith := EIdConnectTimeout;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
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

procedure TestTIdSipClientInviteTransaction.TestTimeout;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest, 500);
  Sleep(750);
  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              'Timeout');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInCallingState;
begin
  Self.Tran.OnProceeding := Self.Proceeding;

  Self.MoveToProceedingState;

  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.Tran.OnProceeding := Self.Proceeding;
  Self.Response.StatusCode := SIPRinging;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 100');
  Check(Self.TransactionProceeding, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive1xxNoResendingOfRequest;
begin
  Self.MoveToProceedingState;

  Self.MockTransport.ResetSentRequestCount;
  Sleep(1500);
  CheckEquals(0, Self.MockTransport.SentRequestCount, 'Request was resent');

  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 100');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInCallingState;
begin
  Self.Response.StatusCode := SIPOK;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive2xxInProceedingState;
begin
  Self.MoveToProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 200');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCallingState;
begin
  Self.Tran.OnCompleted := Self.Completed;

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInCompletedState;
begin
  Self.Tran.OnCompleted := Self.Completed;
  Self.MoveToProceedingState;
  Self.MoveToCompletedState;

  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Self.Tran.State),
              'State on receiving a 300');
  CheckEquals(2, Self.MockTransport.ACKCount, 'Incorrect ACK count');
  Check(Self.TransactionCompleted, 'Event didn''t fire');
end;

procedure TestTIdSipClientInviteTransaction.TestReceive3xxInProceedingState;
begin
  Self.Tran.OnCompleted := Self.Completed;
  Self.MoveToProceedingState;

  Response.StatusCode := SIPMultipleChoices;
  Self.MockTransport.FireOnResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Self.Tran.State),
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
  Self.TransactionProceeding                                        := false;
  Self.TransactionCompleted                                         := false;
  Self.TransactionConfirmed                                         := false;
  Self.TransactionFailed                                            := false;

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
  CheckEquals(Self.InitialRequest.Path.FirstHop.Value,
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
  CheckEquals(Self.InitialRequest.Path.FirstHop.Value,
              R.Path.LastHop.Value,
              'Topmost Via');

  Self.CheckSending100Fired := true;
end;

procedure TestTIdSipServerInviteTransaction.MoveToCompletedState;
begin
  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Tran.State),
              'MoveToCompletedState');

  Self.Response.StatusCode := SIPMultipleChoices;
  Self.Tran.SendResponse(Self.Response);
end;

procedure TestTIdSipServerInviteTransaction.MoveToConfirmedState;
begin
  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Tran.State),
              'MoveToCompletedState');

  Self.Request.Method := MethodAck;
  Self.MockTransport.SendRequest(Self.Request);
end;

procedure TestTIdSipServerInviteTransaction.ReceiveInvite;
begin
  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  Self.MockTransport.OnResponse := Self.CheckReceiveInvite;
  Self.MockTransport.FireOnRequest(Self.InitialRequest);

  Check(Self.CheckReceiveInviteFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

//* TestTIdSipServerInviteTransaction Published methods ************************

procedure TestTIdSipServerInviteTransaction.TestInitialState;
begin
  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Tran.State),
              'Initial state');
end;

procedure TestTIdSipServerInviteTransaction.TestCompletedStateNetworkProblem;
begin
  Self.MoveToCompletedState;

  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockTransport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');  
end;

procedure TestTIdSipServerInviteTransaction.TestProceedingStateNetworkProblem;
begin
  Self.Tran.OnFail := Self.TransactionFail;
  Self.MockTransport.FailWith := EIdConnectTimeout;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              IntToStr(Self.Response.StatusCode) + ' from TU');

  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReceive2xxFromTUInProceedingState;
begin
  Self.MockTransport.OnResponse := Self.CheckReceive2xxFromTUInProceedingState;

  Self.Response.StatusCode := SIPOK;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              '200 from TU');

  Check(Self.CheckReceive2xxFromTUInProceedingStateFired, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveAckInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.Request.Method := MethodAck;
  Self.MockTransport.SendRequest(Self.Request);

  CheckEquals(InviteStateToStr(itsConfirmed),
              InviteStateToStr(Self.Tran.State),
              '200 from TU');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInCompletedState;
begin
  Self.MoveToCompletedState;

  Self.ReceiveInvite;

  CheckEquals(InviteStateToStr(itsCompleted),
              InviteStateToStr(Tran.State),
              'Received an INVITE');
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveInviteInProceedingState;
begin
  Self.ReceiveInvite;

  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Tran.State),
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

    CheckEquals(InviteStateToStr(itsCompleted),
                InviteStateToStr(Tran.State),
                'Received a ' + IntToStr(StatusCode) + ' from TU');
  end;
end;

procedure TestTIdSipServerInviteTransaction.TestReceiveNonTryingProvisionalResponseFromTUInProceedingState;
begin
  Self.MockTransport.OnResponse := Self.CheckReceiveNonTryingProvisionalFromTUInProceedingState;

  Self.Response.StatusCode := SIPRinging;
  Self.Tran.SendResponse(Self.Response);

  CheckEquals(InviteStateToStr(itsProceeding),
              InviteStateToStr(Self.Tran.State),
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
  CheckEquals(InviteStateToStr(itsTerminated),
              InviteStateToStr(Self.Tran.State),
              'Timeout');
  Check(Self.TransactionFailed, 'Event didn''t fire');
end;

procedure TestTIdSipServerInviteTransaction.TestSending100;
begin
  Self.InitialRequest.Headers.Add(TimestampHeader).Value := '100';
  Self.MockTransport.OnResponse := Self.CheckSending100;
  Self.Tran.Initialise(Self.MockTransport, Self.InitialRequest);

  Check(Self.CheckSending100Fired, 'Event didn''t fire');
end;

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
