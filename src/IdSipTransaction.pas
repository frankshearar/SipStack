unit IdSipTransaction;

interface

uses
  Classes, QExtCtrls, IdSipMessage, IdSipParser, IdSipTransport, IdThread, SysUtils;

const
  InitialT1     = 500; // ms
  InitialT1_64  = 64*InitialT1;
  TimerDTimeout = 32000;

type
  TIdSipFailEvent = procedure(Sender: TObject; const Reason: String) of object;
  // Warning: these are both client and server states - itsCalling is client
  // only, itsConfirmed is server only.
  TIdSipInviteTransactionState = (itsCalling, itsProceeding, itsCompleted, itsConfirmed, itsTerminated);

  TIdSipTransactionTimer = class(TIdThread)
  private
    fInterval:  Cardinal;
    fOnTimer:   TNotifyEvent;
    fStart:     TDateTime;
    Resolution: Cardinal;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;

    function  ElapsedTime: TDateTime;
    procedure Reset;

    property Interval: Cardinal     read fInterval write fInterval;
    property OnTimer:  TNotifyEvent read fOnTimer write fOnTimer;
  end;

  TIdSipTransaction = class(TObject)
  private
    fOnFail: TIdSipFailEvent;
  protected
    fTransport: TIdSipAbstractTransport;

    procedure DoOnFail(const Reason: String);

    property Transport: TIdSipAbstractTransport read fTransport;
  public
    property OnFail: TIdSipFailEvent read fOnFail write fOnFail;
  end;

  TIdSipClientInviteTransaction = class(TIdSipTransaction)
  private
    fOnCompleted:   TIdSipResponseEvent;
    fOnProceeding:  TIdSipResponseEvent;
    fOnTerminated:  TIdSipResponseEvent;
    fState:         TIdSipInviteTransactionState;
    fTimeout:       Cardinal;
    InitialRequest: TIdSipRequest;
    TimerA:         TIdSipTransactionTimer;
    TimerB:         TIdSipTransactionTimer;
    TimerD:         TIdSipTransactionTimer;

    procedure ChangeToCompleted(const R: TIdSipResponse);
    procedure ChangeToProceeding(const R: TIdSipResponse);
    procedure ChangeToTerminated(const R: TIdSipResponse);
    procedure DoOnCompleted(const R: TIdSipResponse);
    procedure DoOnProceeding(const R: TIdSipResponse);
    procedure DoOnTerminated(const R: TIdSipResponse);
    procedure GenerateACK(const R: TIdSipResponse; Req: TIdSipRequest);
    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
    procedure OnResponse(Sender: TObject; const R: TIdSipResponse);
    procedure TrySendACK(const R: TIdSipResponse);
    procedure TrySendRequest(const R: TIdSipRequest);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Initialise(const Transport:      TIdSipAbstractTransport;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64);

    property OnCompleted:   TIdSipResponseEvent          read fOnCompleted write fOnCompleted;
    property OnProceeding:  TIdSipResponseEvent          read fOnProceeding write fOnProceeding;
    property OnTerminated:  TIdSipResponseEvent          read fOnTerminated write fOnTerminated;
    property State:         TIdSipInviteTransactionState read fState write fState;
    property Timeout:       Cardinal                     read fTimeout write fTimeout;
  end;

  TIdSipServerInviteTransaction = class(TIdSipTransaction)
  private
    fOnCompleted:   TIdSipResponseEvent;
    fOnConfirmed:   TIdSipResponseEvent;
    fOnProceeding:  TIdSipResponseEvent;
    fOnTerminated:  TIdSipResponseEvent;
    fState:         TIdSipInviteTransactionState;
    fTransport:     TIdSipAbstractTransport;
    InitialRequest: TIdSipRequest;
    TimerG:         TIdSipTransactionTimer;
    TimerH:         TIdSipTransactionTimer;
    TimerI:         TIdSipTransactionTimer;

    procedure Generate100(const R: TIdSipRequest; Res: TIdSipResponse);
    procedure TrySend100Response(const R: TIdSipRequest);
    procedure TrySendResponse(const R: TIdSipResponse);

    property Transport: TIdSipAbstractTransport read fTransport;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Initialise(const Transport:      TIdSipAbstractTransport;
                         const InitialRequest: TIdSipRequest);
    procedure SendResponse(const R: TIdSipResponse);

    property OnCompleted:   TIdSipResponseEvent          read fOnCompleted write fOnCompleted;
    property OnConfirmed:   TIdSipResponseEvent          read fOnConfirmed write fOnConfirmed;
    property OnProceeding:  TIdSipResponseEvent          read fOnProceeding write fOnProceeding;
    property OnTerminated:  TIdSipResponseEvent          read fOnTerminated write fOnTerminated;
    property State:         TIdSipInviteTransactionState read fState write fState;
  end;

implementation

uses
  DateUtils, IdException;

//******************************************************************************
//* TIdSipTransactionTimer                                                     *
//******************************************************************************
//* TIdSipTransactionTimer Public methods **************************************

constructor TIdSipTransactionTimer.Create(ACreateSuspended: Boolean = True);
begin
  Self.Resolution := 50;

  inherited Create(ACreateSuspended);
end;

function TIdSipTransactionTimer.ElapsedTime: TDateTime;
begin
  Result := Now - Self.fStart
end;

procedure TIdSipTransactionTimer.Reset;
begin
  Self.fStart := Now;
end;

//* TIdSipTransactionTimer Protected methods ***********************************

procedure TIdSipTransactionTimer.Run;
begin
  Self.Reset;
  while not Self.Terminated do begin
    Sleep(Self.Resolution);

    if (Self.ElapsedTime > (OneMillisecond * Self.Interval)) then begin
      Self.Reset;
      Self.OnTimer(Self);
    end;
  end;
end;

//******************************************************************************
//* TIdSipTransaction                                                          *
//******************************************************************************
//* TIdSipTransaction Protected methods ****************************************

procedure TIdSipTransaction.DoOnFail(const Reason: String);
begin
  if Assigned(Self.OnFail) then
    Self.OnFail(Self, Reason);
end;

//******************************************************************************
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerA          := TIdSipTransactionTimer.Create(true);
  Self.TimerA.Interval := InitialT1;
  Self.TimerA.OnTimer  := Self.OnTimerA;

  Self.TimerB          := TIdSipTransactionTimer.Create(true);
  Self.TimerB.OnTimer  := Self.OnTimerB;

  Self.TimerD          := TIdSipTransactionTimer.Create(true);
  Self.TimerD.Interval := TimerDTimeout;
  Self.TimerD.OnTimer  := Self.OnTimerD;
end;

destructor TIdSipClientInviteTransaction.Destroy;
begin
  Self.TimerD.TerminateAndWaitFor;
  Self.TimerD.Free;
  Self.TimerB.TerminateAndWaitFor;
  Self.TimerB.Free;
  Self.TimerA.TerminateAndWaitFor;
  Self.TimerA.Free;

  inherited Destroy;
end;

procedure TIdSipClientInviteTransaction.Initialise(const Transport:      TIdSipAbstractTransport;
                                                   const InitialRequest: TIdSipRequest;
                                                   const Timeout:        Cardinal = InitialT1_64);
begin
  Self.State := itsCalling;

  Self.TimerB.Interval := Timeout;

  fTransport := Transport;
  Self.InitialRequest := InitialRequest;
  Self.Transport.OnResponse := Self.OnResponse;

  Self.TrySendRequest(Self.InitialRequest);

  Self.TimerA.Start;
  Self.TimerB.Start;
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(const R: TIdSipResponse);
begin
  if (Self.State <> itsCompleted) then begin
    Self.State := itsCompleted;
    Self.TimerD.Start;
  end;

  Self.TrySendACK(R);
  Self.DoOnCompleted(R);
end;

procedure TIdSipClientInviteTransaction.ChangeToProceeding(const R: TIdSipResponse);
begin
  if (Self.State <> itsProceeding) then begin
    Self.State := itsProceeding;
    Self.TimerA.Stop;
  end;

  Self.DoOnProceeding(R);
end;

procedure TIdSipClientInviteTransaction.ChangeToTerminated(const R: TIdSipResponse);
begin
  Self.State := itsTerminated;
  Self.TimerA.Stop;
  Self.TimerB.Stop;
  Self.DoOnTerminated(R);
end;

procedure TIdSipClientInviteTransaction.DoOnCompleted(const R: TIdSipResponse);
begin
  if Assigned(Self.OnCompleted) then
    Self.OnCompleted(Self, R);
end;

procedure TIdSipClientInviteTransaction.DoOnProceeding(const R: TIdSipResponse);
begin
  if Assigned(Self.OnProceeding) then
    Self.OnProceeding(Self, R);
end;

procedure TIdSipClientInviteTransaction.DoOnTerminated(const R: TIdSipResponse);
begin
  if Assigned(Self.OnTerminated) then
    Self.OnTerminated(Self, R);
end;

procedure TIdSipClientInviteTransaction.GenerateACK(const R:   TIdSipResponse;
                                                          Req: TIdSipRequest);
var
  I:      Integer;
  Routes: TIdSipHeadersFilter;
begin
  Req.Method          := MethodAck;
  Req.Request         := Self.InitialRequest.Request;
  Req.SIPVersion      := Self.InitialRequest.SIPVersion;
  Req.CallID          := Self.InitialRequest.CallID;
  Req.From            := Self.InitialRequest.From;
  Req.ToHeader        := R.ToHeader;
  Req.Path.Add(Self.InitialRequest.Path.LastHop);
  Req.CSeq.SequenceNo := Self.InitialRequest.CSeq.SequenceNo;
  Req.CSeq.Method     := MethodAck;
  Req.ContentLength   := 0;
  Req.Body            := '';

  Routes := TIdSipHeadersFilter.Create(R.Headers, RouteHeader);
  try
    for I := 0 to Routes.Count - 1 do
      Req.Headers.Add(Routes.Items[I]);
  finally
    Routes.Free;
  end;
end;

procedure TIdSipClientInviteTransaction.OnTimerA(Sender: TObject);
begin
  Self.TimerA.Interval := Self.TimerA.Interval*2;
  Self.TrySendRequest(Self.InitialRequest);
end;

procedure TIdSipClientInviteTransaction.OnTimerB(Sender: TObject);
begin
  Self.State := itsTerminated;
  Self.DoOnFail('Timed out');
end;

procedure TIdSipClientInviteTransaction.OnTimerD(Sender: TObject);
begin
  Self.State := itsTerminated;
end;

procedure TIdSipClientInviteTransaction.OnResponse(Sender: TObject; const R: TIdSipResponse);
begin
  case Self.State of
    itsCalling: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R);
        2: Self.ChangeToTerminated(R);
      else
        Self.ChangeToCompleted(R);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: Self.DoOnProceeding(R);
        2: Self.ChangeToTerminated(R);
      else
        Self.ChangeToCompleted(R);
      end;
    end;

    itsCompleted: begin
      case R.StatusCode div 100 of
        3..6: Self.ChangeToCompleted(R);
      else
        raise Exception.Create('unhandled response in itsCompleted');
      end;
    end;
  else
    // should we just dump the response on the floor?
    // We shouldn't ever get 1xx response codes here, but you just never know.
    raise Exception.Create('unhandled Self.State');
  end;
end;

procedure TIdSipClientInviteTransaction.TrySendACK(const R: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := TIdSipRequest.Create;
  try
    Self.GenerateACK(R, Ack);
    Self.TrySendRequest(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TIdSipClientInviteTransaction.TrySendRequest(const R: TIdSipRequest);
begin
  try
    Self.Transport.SendRequest(R);
  except
    on E: EIdException do begin
      Self.DoOnFail(E.Message);
      Self.State := itsTerminated;
    end;
  end;
end;

//******************************************************************************
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerG := TIdSipTransactionTimer.Create;
  Self.TimerH := TIdSipTransactionTimer.Create;
  Self.TimerI := TIdSipTransactionTimer.Create;
end;

destructor TIdSipServerInviteTransaction.Destroy;
begin
  Self.TimerI.TerminateAndWaitFor;
  Self.TimerI.Free;
  Self.TimerH.TerminateAndWaitFor;
  Self.TimerH.Free;
  Self.TimerG.TerminateAndWaitFor;
  Self.TimerG.Free;

  inherited Destroy;
end;

procedure TIdSipServerInviteTransaction.Initialise(const Transport:      TIdSipAbstractTransport;
                                                   const InitialRequest: TIdSipRequest);
begin
  Self.State          := itsProceeding;
  Self.InitialRequest := InitialRequest;
  fTransport          := Transport;

  Self.TrySend100Response(Self.InitialRequest);
end;

procedure TIdSipServerInviteTransaction.SendResponse(const R: TIdSipResponse);
begin
  Self.TrySendResponse(R);
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.Generate100(const R: TIdSipRequest; Res: TIdSipResponse);
var
  I:                Integer;
  TimestampHeaders: TIdSipHeadersFilter;
begin
  Res.StatusCode := SIPTrying;
  Res.SIPVersion := SIPVersion;

  Res.From := R.From;
  Res.ToHeader := R.ToHeader;
  Res.CallID := R.CallID;
  Res.CSeq := R.CSeq;
  Res.Path := R.Path;

  TimestampHeaders := TIdSipHeadersFilter.Create(R.Headers, TimestampHeader);
  try
    for I := 0 to TimestampHeaders.Count - 1 do
      Res.Headers.Add(TimestampHeaders.Items[I]);
  finally
    TimestampHeaders.Free;
  end;

  Res.Path := R.Path;
end;

procedure TIdSipServerInviteTransaction.TrySend100Response(const R: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Self.Generate100(Self.InitialRequest, Response);

    Self.TrySendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipServerInviteTransaction.TrySendResponse(const R: TIdSipResponse);
begin
  try
    Self.Transport.SendResponse(R);
  except
    on E: EIdException do begin
      Self.DoOnFail(E.Message);
      Self.State := itsTerminated;
    end;
  end;
end;

end.
