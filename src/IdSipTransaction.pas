unit IdSipTransaction;

interface

uses
  Classes, QExtCtrls, IdSipMessage, IdSipParser, IdSipTransport, IdThread, SysUtils;

const
  InitialT1     = 500;   // ms
  InitialT1_64  = 64*InitialT1;
  TimerDTimeout = 32000; // ms
  T2            = 4000;  // ms
  T4            = 5000;  // ms

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
    fOnProceeding:  TIdSipResponseEvent;
    fOnCompleted:   TIdSipResponseEvent;
    fOnFail:        TIdSipFailEvent;
    fOnTerminated:  TIdSipNotifyEvent;
    fState:         TIdSipInviteTransactionState;
  protected
    fTransport:     TIdSipAbstractTransport;
    InitialRequest: TIdSipRequest;

    procedure ChangeToCompleted(const R: TIdSipResponse); virtual;
    procedure ChangeToProceeding(const R: TIdSipResponse); virtual;
    procedure ChangeToTerminated; virtual;
    procedure DoOnCompleted(const R: TIdSipResponse);
    procedure DoOnProceeding(const R: TIdSipResponse);
    procedure DoOnFail(const Reason: String);
    procedure DoOnTerminated;

    property Transport: TIdSipAbstractTransport read fTransport;
  public
    property OnCompleted:  TIdSipResponseEvent          read fOnCompleted write fOnCompleted;
    property OnFail:       TIdSipFailEvent              read fOnFail write fOnFail;
    property OnProceeding: TIdSipResponseEvent          read fOnProceeding write fOnProceeding;
    property OnTerminated: TIdSipNotifyEvent            read fOnTerminated write fOnTerminated;
    property State:        TIdSipInviteTransactionState read fState write fState;
  end;

  TIdSipClientInviteTransaction = class(TIdSipTransaction)
  private
    fTimeout:       Cardinal;
    TimerA:         TIdSipTransactionTimer;
    TimerB:         TIdSipTransactionTimer;
    TimerD:         TIdSipTransactionTimer;

    procedure GenerateACK(const R: TIdSipResponse; Req: TIdSipRequest);
    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
    procedure TryResendInvite;
    procedure OnResponse(Sender: TObject; const R: TIdSipResponse);
    procedure TrySendACK(const R: TIdSipResponse);
    procedure TrySendRequest(const R: TIdSipRequest);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse); override;
    procedure ChangeToProceeding(const R: TIdSipResponse); override;
    procedure ChangeToTerminated; override;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Initialise(const Transport:      TIdSipAbstractTransport;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64);

    property Timeout: Cardinal read fTimeout write fTimeout;
  end;

  TIdSipServerInviteTransaction = class(TIdSipTransaction)
  private
    fOnConfirmed:               TIdSipRequestEvent;
    fTransport:                 TIdSipAbstractTransport;
    LastProceedingResponseSent: Cardinal;
    TimerG:                     TIdSipTransactionTimer;
    TimerGHasFired:             Boolean;
    TimerH:                     TIdSipTransactionTimer;
    TimerI:                     TIdSipTransactionTimer;

    procedure ChangeToConfirmed(const R: TIdSipRequest);
    procedure DoOnConfirmed(const R: TIdSipRequest);
    procedure Generate100(const R: TIdSipRequest;
                                Res: TIdSipResponse);
    procedure GenerateResponse(const R: TIdSipRequest;
                                     Res: TIdSipResponse;
                               const StatusCode: Cardinal);
    procedure OnRequest(Sender: TObject; const R: TIdSipRequest);
    procedure OnTimerG(Sender: TObject);
    procedure OnTimerH(Sender: TObject);
    procedure OnTimerI(Sender: TObject);
    procedure TrySend100Response(const R: TIdSipRequest);
    procedure TrySendLastResponse(const R: TIdSipRequest);
    procedure TrySendResponse(const R: TIdSipResponse);

    property Transport: TIdSipAbstractTransport read fTransport;
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse); override;
    procedure ChangeToProceeding(const R: TIdSipResponse); override;
    procedure ChangeToTerminated; override;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Initialise(const Transport:      TIdSipAbstractTransport;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64);
    procedure SendResponse(const R: TIdSipResponse);

    property OnConfirmed: TIdSipRequestEvent read fOnConfirmed write fOnConfirmed;
  end;

implementation

uses
  DateUtils, IdException, Math;

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

procedure TIdSipTransaction.DoOnCompleted(const R: TIdSipResponse);
begin
  if Assigned(Self.OnCompleted) then
    Self.OnCompleted(Self, R);
end;

procedure TIdSipTransaction.DoOnProceeding(const R: TIdSipResponse);
begin
  if Assigned(Self.OnProceeding) then
    Self.OnProceeding(Self, R);
end;

procedure TIdSipTransaction.DoOnFail(const Reason: String);
begin
  if Assigned(Self.OnFail) then
    Self.OnFail(Self, Reason);
end;

procedure TIdSipTransaction.ChangeToCompleted(const R: TIdSipResponse);
begin
  Self.State := itsCompleted;
  Self.DoOnCompleted(R);
end;

procedure TIdSipTransaction.ChangeToProceeding(const R: TIdSipResponse);
begin
  Self.State := itsProceeding;
  Self.DoOnProceeding(R);
end;

procedure TIdSipTransaction.ChangeToTerminated;
begin
  Self.State := itsTerminated;
  Self.DoOnTerminated;
end;

procedure TIdSipTransaction.DoOnTerminated;
begin
  if Assigned(Self.OnTerminated) then
    Self.OnTerminated(Self);
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
  Self.State          := itsCalling;
  Self.InitialRequest := InitialRequest;
  fTransport          := Transport;

  Self.TimerB.Interval := Timeout;

  Self.Transport.OnResponse := Self.OnResponse;

  Self.TrySendRequest(Self.InitialRequest);

  Self.TimerA.Start;
  Self.TimerB.Start;
end;

//* TIdSipClientInviteTransaction Protected methods ****************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(const R: TIdSipResponse);
begin
  // It's unfortunate that we can't simply call inherited.
  // However, TrySendACK must be called before DoOnCompleted,
  // and we have to set Self.State to itsCompleted before
  // TrySendACK because a transport failure changes Self.State
  // to itsTerminated.

  Self.TimerD.Start;

  Self.State := itsCompleted;
  Self.TrySendACK(R);
  Self.DoOnCompleted(R);
end;

procedure TIdSipClientInviteTransaction.ChangeToProceeding(const R: TIdSipResponse);
begin
  inherited ChangeToProceeding(R);

  Self.TimerA.Stop;
end;

procedure TIdSipClientInviteTransaction.ChangeToTerminated;
begin
  inherited ChangeToTerminated;

  Self.TimerA.Stop;
  Self.TimerB.Stop;
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.GenerateACK(const R: TIdSipResponse;
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
  Req.Path.Add(Self.InitialRequest.Path.FirstHop);
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
  Self.TryResendInvite;
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

procedure TIdSipClientInviteTransaction.TryResendInvite;
begin
  if not Self.Transport.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendRequest(Self.InitialRequest);
end;

procedure TIdSipClientInviteTransaction.OnResponse(Sender: TObject; const R: TIdSipResponse);
begin
  case Self.State of
    itsCalling: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R);
        2: Self.ChangeToTerminated;
      else
        Self.ChangeToCompleted(R);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: Self.DoOnProceeding(R);
        2: Self.ChangeToTerminated;
      else
        Self.ChangeToCompleted(R);
      end;
    end;

    itsCompleted: begin
      case R.StatusCode div 100 of
        3..6: Self.ChangeToCompleted(R);
      else
        raise Exception.Create('unhandled response in ' + Self.ClassName + '.OnResponse, itsCompleted');
      end;
    end;
  else
    // should we just dump the response on the floor?
    // We shouldn't ever get 1xx response codes here, but you just never know.
    raise Exception.Create('unhandled Self.State in ' + Self.ClassName + '.OnResponse');
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
  Self.TimerG.Interval := InitialT1;
  Self.TimerG.OnTimer  := Self.OnTimerG;

  Self.TimerH := TIdSipTransactionTimer.Create;
  Self.TimerH.Interval := 64*InitialT1;
  Self.TimerH.OnTimer  := Self.OnTimerH;

  Self.TimerI := TIdSipTransactionTimer.Create;
  Self.TimerI.Interval := T4;
  Self.TimerI.OnTimer  := Self.OnTimerI;
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
                                                   const InitialRequest: TIdSipRequest;
                                                   const Timeout:        Cardinal = InitialT1_64);
begin
  Self.State          := itsProceeding;
  Self.InitialRequest := InitialRequest;
  fTransport          := Transport;

  Self.TimerH.Interval := Timeout;

  Self.TrySend100Response(Self.InitialRequest);

  Self.Transport.OnRequest := Self.OnRequest;
end;

procedure TIdSipServerInviteTransaction.SendResponse(const R: TIdSipResponse);
begin
  Self.TrySendResponse(R);
  if (Self.State = itsProceeding) then begin
    case R.StatusCode of
      101..199: Self.ChangeToProceeding(R);
      SIPOK:    Self.ChangeToTerminated;
      300..699: Self.ChangeToCompleted(R);
    else
      raise Exception.Create('Unhandled response in ' + Self.ClassName + '.SendReponse, itsProceeding')
    end;
  end;
end;

//* TIdSipServerInviteTransaction Protected methods ***************************

procedure TIdSipServerInviteTransaction.ChangeToCompleted(const R: TIdSipResponse);
begin
  inherited ChangeToCompleted(R);

  Self.TimerG.Start;
  Self.TimerH.Start;
end;

procedure TIdSipServerInviteTransaction.ChangeToProceeding(const R: TIdSipResponse);
begin
  inherited ChangeToProceeding(R);
end;

procedure TIdSipServerInviteTransaction.ChangeToTerminated;
begin
  inherited ChangeToTerminated;
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(const R: TIdSipRequest);
begin
  Self.State := itsConfirmed;
  Self.DoOnConfirmed(R);

  Self.TimerG.Stop;
  Self.TimerI.Start;
end;

procedure TIdSipServerInviteTransaction.DoOnConfirmed(const R: TIdSipRequest);
begin
  if Assigned(Self.OnConfirmed) then
    Self.OnConfirmed(Self, R);
end;

procedure TIdSipServerInviteTransaction.Generate100(const R: TIdSipRequest;
                                                          Res: TIdSipResponse);
begin
  Self.GenerateResponse(R, Res, SIPTrying);
end;

procedure TIdSipServerInviteTransaction.GenerateResponse(const R: TIdSipRequest;
                                                               Res: TIdSipResponse;
                                                         const StatusCode: Cardinal);
var
  I:                Integer;
  TimestampHeaders: TIdSipHeadersFilter;
begin
  Res.StatusCode := StatusCode;
  Res.SIPVersion := SIPVersion;

  Res.From     := R.From;
  Res.ToHeader := R.ToHeader;
  Res.CallID   := R.CallID;
  Res.CSeq     := R.CSeq;

  TimestampHeaders := TIdSipHeadersFilter.Create(R.Headers, TimestampHeader);
  try
    for I := 0 to TimestampHeaders.Count - 1 do
      Res.Headers.Add(TimestampHeaders.Items[I]);
  finally
    TimestampHeaders.Free;
  end;

  Res.Path.Add(Self.InitialRequest.Path.FirstHop);
end;

procedure TIdSipServerInviteTransaction.OnRequest(Sender: TObject; const R: TIdSipRequest);
begin
  case Self.State of
    itsProceeding: Self.TrySendLastResponse(R);
    itsCompleted: begin
      if (R.Method = MethodInvite) then
        Self.TrySendLastResponse(R)
      else if (R.Method = MethodAck) then
        Self.ChangeToConfirmed(R)
      else
        raise Exception.Create('Unhandled method in ' + Self.ClassName + '.OnRequest');
//    itsConfirmed:; // should we just drop these on the floor?
    end;
  else
    raise Exception.Create('Unhandled Self.State in ' + Self.ClassName + '.OnRequest');
  end;
end;

procedure TIdSipServerInviteTransaction.OnTimerG(Sender: TObject);
begin
  if Self.TimerGHasFired then begin
    Self.TimerG.Interval := 2*Self.TimerG.Interval;

    if (Self.TimerG.Interval > T2) then
      Self.TimerG.Interval := T2;

  end
  else begin
    Self.TimerG.Interval := Min(2*Self.TimerG.Interval, T2);
    Self.TimerGHasFired := true;
  end;

  if not Self.Transport.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendLastResponse(Self.InitialRequest);
end;

procedure TIdSipServerInviteTransaction.OnTimerH(Sender: TObject);
begin
  Self.State := itsTerminated;
  Self.DoOnFail('Timed out');
end;

procedure TIdSipServerInviteTransaction.OnTimerI(Sender: TObject);
begin
  Self.ChangeToTerminated;
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

procedure TIdSipServerInviteTransaction.TrySendLastResponse(const R: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Self.GenerateResponse(R, Response, Self.LastProceedingResponseSent);
    Self.TrySendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipServerInviteTransaction.TrySendResponse(const R: TIdSipResponse);
begin
  Self.LastProceedingResponseSent := R.StatusCode;
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
