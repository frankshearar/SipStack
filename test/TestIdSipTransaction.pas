unit TestIdSipTransaction;

interface

uses
  IdSipMessage, IdSipParser, IdSipTransport, IdSipTransaction, IdThread,
  SysUtils, TestFramework;

type
  TestTIdSipTransactionTimer = class(TTestCase)
  private
    GotException:    Boolean;
    Tick:            Boolean;
    TickAccumulator: Cardinal;
    Timer:           TIdSipTransactionTimer;
    procedure OnAccumulatorTimer(Sender: TObject);
    procedure OnException(AThread: TIdThread; AException: Exception);
    procedure OnRaiseExceptionTimer(Sender: TObject);
    procedure OnTickTimer(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExceptionHandling;
    procedure TestMultipleTicks;
    procedure TestTick;
    procedure TestReset;
  end;

  TestTIdSipClientTransaction = class(TTestCase)
  private
    InitialRequest:        TIdSipRequest;
    MockTransport:         TIdSipMockTransport;
    Response:              TIdSipResponse;
    TransactionProceeding: Boolean;
    TransactionCompleted:  Boolean;
    TransactionFailed:     Boolean;

    procedure CheckACK(Sender: TObject; const R: TIdSipResponse);
    procedure Completed(Sender: TObject; const R: TIdSipResponse);
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
    procedure TestMultipleRequestSending;
    procedure TestTimeout;
    procedure TestReceive1xxInCallingState;
    procedure TestReceive1xxInProceedingState;
    procedure TestReceive1xxNoResendingOfRequest;
    procedure TestReceive2xxInCallingState;
    procedure TestReceive2xxInProceedingState;
    procedure TestReceive3xxInCallingState;
    procedure TestReceive3xxInCompletedState;
    procedure TestReceive3xxInProceedingState;
  end;

  TestTIdSipServerTransaction = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  IdException, TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
//  Result.AddTest(TestTIdSipTransactionTimer.Suite);
  Result.AddTest(TestTIdSipClientTransaction.Suite);
  Result.AddTest(TestTIdSipServerTransaction.Suite);
end;

function ClientInviteStateToStr(const S: TIdSipInviteTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipInviteTransactionState), Integer(S));
end;

//******************************************************************************
//* TestTIdSipTransactionTimer                                                 *
//******************************************************************************
//* TestTIdSipTransactionTimer Public methods **********************************

procedure TestTIdSipTransactionTimer.SetUp;
begin
  GotException    := false;
  Tick            := false;
  TickAccumulator := 0;
  Timer           := TIdSipTransactionTimer.Create(true);
end;

procedure TestTIdSipTransactionTimer.TearDown;
begin
  Timer.Stop;
  Timer.WaitFor;
  Timer.Free;
end;

//* TestTIdSipTransactionTimer Private methods *********************************

procedure TestTIdSipTransactionTimer.OnAccumulatorTimer(Sender: TObject);
begin
  Inc(TickAccumulator);
  Self.Check(Self.Timer = Sender, 'Unknown Sender');
end;

procedure TestTIdSipTransactionTimer.OnException(AThread: TIdThread; AException: Exception);
begin
  GotException := true;
end;

procedure TestTIdSipTransactionTimer.OnRaiseExceptionTimer(Sender: TObject);
begin
  raise Exception.Create('OnRaiseExceptionTimer');
end;

procedure TestTIdSipTransactionTimer.OnTickTimer(Sender: TObject);
begin
  Tick := true;
  Self.Check(Self.Timer = Sender, 'Unknown Sender');
end;

//* TestTIdSipTransactionTimer Published methods *******************************

procedure TestTIdSipTransactionTimer.TestExceptionHandling;
begin
  Timer.OnException := Self.OnException;
  Timer.OnTimer     := Self.OnRaiseExceptionTimer;
  Timer.Interval    := 100;
  Timer.Start;
  Sleep(375);

  Check(GotException, 'Main thread never heard of the exception');
end;

procedure TestTIdSipTransactionTimer.TestMultipleTicks;
begin
  Timer.OnTimer := Self.OnAccumulatorTimer;

  Timer.Interval   := 100;
  Timer.Start;
  Sleep(375);

  CheckEquals(3, TickAccumulator, 'Unexpected number of ticks');
end;

procedure TestTIdSipTransactionTimer.TestTick;
begin
  Timer.OnTimer := Self.OnTickTimer;

  Timer.Interval := 100;
  Timer.Start;
  Sleep(150);

  Check(Tick, 'Event didn''t fire');
end;

procedure TestTIdSipTransactionTimer.TestReset;
begin
  Timer.OnTimer := Self.OnTickTimer;
  Timer.Interval := 1000;
  Timer.Start;
  Sleep(500);

  Check(not Tick, 'Ticked prematurely before Reset');
  Timer.Reset;
  Sleep(500);
  Check(not Tick, 'Ticked prematurely after Reset');
end;

//******************************************************************************
//* TestTIdSipClientTransaction                                                *
//******************************************************************************
//* TestTIdSipClientTransaction Public methods *********************************

procedure TestTIdSipClientTransaction.SetUp;
begin
  inherited SetUp;

  // this is just BasicRequest from TestIdSipParser
  Self.InitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Method                           := MethodInvite;
  Self.InitialRequest.MaxForwards                      := 70;
  Self.InitialRequest.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
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

  Self.TransactionProceeding := false;
  Self.TransactionCompleted  := false;
  Self.TransactionFailed     := false;
end;

procedure TestTIdSipClientTransaction.TearDown;
begin
  Self.MockTransport.Free;
  Self.Response.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipClientTransaction Private methods ********************************

procedure TestTIdSipClientTransaction.CheckACK(Sender: TObject; const R: TIdSipResponse);
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

procedure TestTIdSipClientTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
end;

procedure TestTIdSipClientTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipClientTransaction.TransactionFail(Sender: TObject; const Reason: String);
begin
  Self.TransactionFailed := true;
end;

//* TestTIdSipClientTransaction Published methods ******************************

procedure TestTIdSipClientTransaction.TestACK;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;
  Self.Response.Headers.Add(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>';
  Self.Response.Headers.Add(RouteHeader).Value := 'localhost <sip:127.0.0.1>';

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.OnCompleted := Self.CheckACK;
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.FireOnResponse(Self.Response);
    Response.StatusCode := SIPMultipleChoices;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsCompleted),
                ClientInviteStateToStr(Tran.State),
                'Sent ack');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestCompletedStateNetworkProblem;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Tran.OnFail := Self.TransactionFail;

    Self.MockTransport.FireOnResponse(Self.Response);

    Response.StatusCode := SIPMultipleChoices;
    Self.MockTransport.FailWith := EIdConnectTimeout;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'Connection timed out');
    Check(Self.TransactionFailed, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestInitialState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Check(itsCalling = Tran.State, 'Wrong initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestInviteWithHostUnreachable;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.OnFail := Self.TransactionFail;

    Self.MockTransport.FailWith := EIdConnectTimeout;
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);

    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'Connection timed out');
    Check(Self.TransactionFailed, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestMultipleRequestSending;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    // The immediate send, plus 500ms wait, plus 1000ms wait should result in 3
    // messages being sent.
    Sleep(2000);
    CheckEquals(3, Self.MockTransport.SentCount, 'Insufficient requests sent');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestTimeout;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest, 500);
    Tran.OnFail := Self.TransactionFail;
    Sleep(750);
    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'Timeout');
    Check(Self.TransactionFailed, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxInCallingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Tran.OnProceeding := Self.Proceeding;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsProceeding),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 100');
    Check(Self.TransactionProceeding, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxInProceedingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.FireOnResponse(Response);

    Tran.OnProceeding := Self.Proceeding;
    Self.Response.StatusCode := SIPRinging;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsProceeding),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 100');
    Check(Self.TransactionProceeding, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxNoResendingOfRequest;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := 100;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.FireOnResponse(Self.Response);
    Self.MockTransport.ResetSentCount;
    Sleep(1500);
    CheckEquals(0, Self.MockTransport.SentCount, 'Request was resent');

    CheckEquals(ClientInviteStateToStr(itsProceeding),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 100');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive2xxInCallingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPOK;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 200');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive2xxInProceedingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Self.MockTransport.FireOnResponse(Self.Response);
    Self.Response.StatusCode := SIPOK;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 200');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInCallingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPMultipleChoices;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Tran.OnCompleted := Self.Completed;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsCompleted),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 300');
    CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
    Check(Self.TransactionCompleted, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInCompletedState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Tran.OnCompleted := Self.Completed;
    Self.MockTransport.FireOnResponse(Self.Response);
    Response.StatusCode := SIPMultipleChoices;
    Self.MockTransport.FireOnResponse(Self.Response);
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsCompleted),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 300');
    CheckEquals(2, Self.MockTransport.ACKCount, 'Incorrect ACK count');
    Check(Self.TransactionCompleted, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInProceedingState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Self.Response.StatusCode := SIPTrying;

  Tran := TIdSipClientInviteTransaction.Create;
  try
    Tran.Initialise(Self.MockTransport, Self.InitialRequest);
    Tran.OnCompleted := Self.Completed;
    Self.MockTransport.FireOnResponse(Self.Response);
    Response.StatusCode := SIPMultipleChoices;
    Self.MockTransport.FireOnResponse(Self.Response);

    CheckEquals(ClientInviteStateToStr(itsCompleted),
                ClientInviteStateToStr(Tran.State),
                'State on receiving a 300');
    CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
    Check(Self.TransactionCompleted, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipServerTransaction                                                *
//******************************************************************************
//* TestTIdSipServerTransaction Public methods *********************************

procedure TestTIdSipServerTransaction.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipServerTransaction.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipServerTransaction Private methods ********************************
//* TestTIdSipServerTransaction Published methods ******************************

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
