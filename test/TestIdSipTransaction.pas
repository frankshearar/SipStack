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
    InitialRequest:          TIdSipRequest;
    MockTransport:           TIdSipMockTransport;
    TransactionProceeding:     Boolean;
    TransactionCompleted:    Boolean;
    TransactionTimeoutFired: Boolean;

    procedure Completed(Sender: TObject; const R: TIdSipResponse);
    procedure Proceeding(Sender: TObject; const R: TIdSipResponse);
    procedure TransactionTimeout(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitialState;
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
  TypInfo;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
//  Result.AddTest(TestTIdSipTransactionTimer.Suite);
  Result.AddTest(TestTIdSipClientTransaction.Suite);
  Result.AddTest(TestTIdSipServerTransaction.Suite);
end;

function ClientInviteStateToStr(const S: TIdInviteTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdInviteTransactionState), Integer(S));
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

  Self.MockTransport := TIdSipMockTransport.Create;

  Self.TransactionProceeding     := false;
  Self.TransactionCompleted    := false;
  Self.TransactionTimeoutFired := false;
end;

procedure TestTIdSipClientTransaction.TearDown;
begin
  Self.MockTransport.Free;
  Self.InitialRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipClientTransaction Private methods ********************************

procedure TestTIdSipClientTransaction.Completed(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionCompleted := true;
end;

procedure TestTIdSipClientTransaction.Proceeding(Sender: TObject; const R: TIdSipResponse);
begin
  Self.TransactionProceeding := true;
end;

procedure TestTIdSipClientTransaction.TransactionTimeout(Sender: TObject);
begin
  Self.TransactionTimeoutFired := true;
end;

//* TestTIdSipClientTransaction Published methods ******************************

procedure TestTIdSipClientTransaction.TestInitialState;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
  try
    Check(itsCalling = Tran.State, 'Wrong initial state');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestMultipleRequestSending;
var
  Tran: TIdSipClientInviteTransaction;
begin
  Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
  try
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
  Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest, 500);
  try
    Tran.OnTimeout := Self.TransactionTimeout;
    Sleep(750);
    CheckEquals(ClientInviteStateToStr(itsTerminated),
                ClientInviteStateToStr(Tran.State),
                'Timeout');
    Check(Self.TransactionTimeoutFired, 'Event didn''t fire');
  finally
    Tran.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxInCallingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPTrying;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Tran.OnProceeding := Self.Proceeding;
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsProceeding),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 100');
      Check(Self.TransactionProceeding, 'Event didn''t fire');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxInProceedingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPTrying;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Self.MockTransport.FireOnResponse(Response);

      Tran.OnProceeding := Self.Proceeding;
      Response.StatusCode := SIPRinging;
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsProceeding),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 100');
      Check(Self.TransactionProceeding, 'Event didn''t fire');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive1xxNoResendingOfRequest;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := 100;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Self.MockTransport.FireOnResponse(Response);
      Self.MockTransport.ResetSentCount;
      Sleep(1500);
      CheckEquals(0, Self.MockTransport.SentCount, 'Request was resent');

      CheckEquals(ClientInviteStateToStr(itsProceeding),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 100');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive2xxInCallingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPOK;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsTerminated),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 200');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive2xxInProceedingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPTrying;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Self.MockTransport.FireOnResponse(Response);
      Response.StatusCode := SIPOK;
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsTerminated),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 200');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInCallingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPMultipleChoices;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Tran.OnCompleted := Self.Completed;
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsCompleted),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 300');
      CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
      Check(Self.TransactionCompleted, 'Event didn''t fire');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInCompletedState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPTrying;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Tran.OnCompleted := Self.Completed;
      Self.MockTransport.FireOnResponse(Response);
      Response.StatusCode := SIPMultipleChoices;
      Self.MockTransport.FireOnResponse(Response);
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsCompleted),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 300');
      CheckEquals(2, Self.MockTransport.ACKCount, 'Incorrect ACK count');
      Check(Self.TransactionCompleted, 'Event didn''t fire');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipClientTransaction.TestReceive3xxInProceedingState;
var
  Response: TIdSipResponse;
  Tran:     TIdSipClientInviteTransaction;
begin
  Response := TIdSipResponse.Create;
  try
    Response.StatusCode := SIPTrying;

    Tran := TIdSipClientInviteTransaction.Create(Self.MockTransport, Self.InitialRequest);
    try
      Tran.OnCompleted := Self.Completed;
      Self.MockTransport.FireOnResponse(Response);
      Response.StatusCode := SIPMultipleChoices;
      Self.MockTransport.FireOnResponse(Response);

      CheckEquals(ClientInviteStateToStr(itsCompleted),
                  ClientInviteStateToStr(Tran.State),
                  'State on receiving a 300');
      CheckEquals(1, Self.MockTransport.ACKCount, 'Incorrect ACK count');
      Check(Self.TransactionCompleted, 'Event didn''t fire');
    finally
      Tran.Free;
    end;
  finally
    Response.Free;
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
