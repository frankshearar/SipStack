unit IdSipTransaction;

interface

uses
  Classes, QExtCtrls, IdSipMessage, IdSipParser, IdSipTransport, IdThread, SysUtils;

const
  InitialT1     = 500; // ms
  InitialT1_64  = 64*InitialT1;
  TimerDTimeout = 32000;

type
  TIdInviteTransactionState = (itsCalling, itsProceeding, itsCompleted, itsTerminated);
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

  TIdSipClientTransaction = class(TObject)
  end;

  TIdSipClientInviteTransaction = class(TObject)
  private
    fOnCompleted:   TIdSipResponseEvent;
    fOnProceeding: TIdSipResponseEvent;
    fOnTerminated:  TIdSipResponseEvent;
    fOnTimeout:     TNotifyEvent;
    fState:         TIdInviteTransactionState;
    fTimeout:       Cardinal;
    fTransport:     TIdSipAbstractTransport;
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
    procedure DoOnTimeout;
    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
    procedure OnResponse(Sender: TObject; const R: TIdSipResponse);

    property Transport: TIdSipAbstractTransport read fTransport;
  public
    constructor Create(const Transport:      TIdSipAbstractTransport;
                       const InitialRequest: TIdSipRequest;
                       const Timeout:        Cardinal = InitialT1_64);
    destructor  Destroy; override;

    property OnCompleted:   TIdSipResponseEvent       read fOnCompleted write fOnCompleted;
    property OnProceeding:  TIdSipResponseEvent       read fOnProceeding write fOnProceeding;
    property OnTerminated:  TIdSipResponseEvent       read fOnTerminated write fOnTerminated;
    property OnTimeout:     TNotifyEvent              read fOnTimeout write fOnTimeout;
    property State:         TIdInviteTransactionState read fState write fState;
    property Timeout:       Cardinal                  read fTimeout write fTimeout;
  end;

  TIdSipClientNonInviteTransaction = class(TObject)
  end;

  TIdSipServerTransaction = class(TObject)
  end;

implementation

uses
  DateUtils;

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
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create(const Transport:      TIdSipAbstractTransport;
                                                 const InitialRequest: TIdSipRequest;
                                                 const Timeout:        Cardinal = InitialT1_64);
begin
  inherited Create;

  fTransport := Transport;
  Self.InitialRequest := InitialRequest;
  Self.Transport.OnResponse := Self.OnResponse;

  Self.State := itsCalling;

  Self.TimerA            := TIdSipTransactionTimer.Create(true);
  Self.TimerA.Interval   := InitialT1;
  Self.TimerA.OnTimer    := Self.OnTimerA;

  Self.TimerB            := TIdSipTransactionTimer.Create(true);
  Self.TimerB.Interval   := Timeout;
  Self.TimerB.OnTimer    := Self.OnTimerB;

  Self.TimerD            := TIdSipTransactionTimer.Create(true);
  Self.TimerD.Interval   := TimerDTimeout;
  Self.TimerD.OnTimer    := Self.OnTimerD;

  Self.Transport.SendRequest(InitialRequest);
  Self.TimerA.Start;
  Self.TimerB.Start;
end;

destructor TIdSipClientInviteTransaction.Destroy;
begin
  Self.TimerA.TerminateAndWaitFor;
  Self.TimerA.Free;
  Self.TimerB.TerminateAndWaitFor;
  Self.TimerB.Free;

  inherited Destroy;
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(const R: TIdSipResponse);
begin
  if (Self.State <> itsCompleted) then begin
    Self.State := itsCompleted;
    Self.TimerD.Start;
  end;

  Self.Transport.SendACK(R);
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

procedure TIdSipClientInviteTransaction.DoOnTimeout;
begin
  if Assigned(Self.OnTimeout) then
    Self.OnTimeout(Self);
end;

procedure TIdSipClientInviteTransaction.OnTimerA(Sender: TObject);
begin
  Self.TimerA.Interval := Self.TimerA.Interval*2;
  Self.Transport.SendRequest(Self.InitialRequest); 
end;

procedure TIdSipClientInviteTransaction.OnTimerB(Sender: TObject);
begin
  Self.State := itsTerminated;
  Self.DoOnTimeout;
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
        1: begin
             Self.ChangeToProceeding(R);
           end;
        2: begin
             Self.ChangeToTerminated(R);
           end;
      else
        Self.ChangeToCompleted(R);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: begin
             Self.DoOnProceeding(R);
           end;
        2: begin
             Self.ChangeToTerminated(R);
           end;
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
    raise Exception.Create('unhandled Self.State');
  end;
end;

end.
