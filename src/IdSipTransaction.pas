unit IdSipTransaction;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdSipMessage, IdSipTimer,
  IdSipTransport, SyncObjs;

const
  DefaultT1    = 500;   // ms
  DefaultT1_64 = 64*DefaultT1;
  DefaultT2    = 4000;  // ms
  DefaultT4    = 5000;  // ms

const
  SessionTimeoutMsg = 'Timed out';

type
  TIdSipFailEvent = procedure(Sender: TObject; const Reason: String) of object;
  // This covers all states - INVITE, non-INVITE, client, server.
  TIdSipTransactionState = (itsCalling, itsCompleted, itsConfirmed,
                            itsProceeding, itsTerminated, itsTrying);

  TIdSipTransaction = class;
  TIdSipTransactionClass = class of TIdSipTransaction;

  // OnTerminated signals to the Listener that Transaction has terminated,
  // and the Listener must remove any references to Transaction - after this
  // notification the transaction reference becomes invalid. If you don't clean
  // up your reference you will have a dangling pointer.
  IIdSipTransactionListener = interface
    ['{77B97FA0-7073-40BC-B3F0-7E53ED02213F}']
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Receiver: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
  end;

  IIdSipUnhandledMessageListener = interface
    ['{0CB5037D-B9B3-4FB6-9201-80A0F10DB23A}']
    procedure OnReceiveUnhandledRequest(const Request: TIdSipRequest;
                                        const Transaction: TIdSipTransaction;
                                        const Receiver: TIdSipTransport);
    procedure OnReceiveUnhandledResponse(const Response: TIdSipResponse;
                                         const Transaction: TIdSipTransaction;
                                         const Receiver: TIdSipTransport);
  end;

  // I represent the single connection point between the transport layer and the
  // transaction layer. I manage transactions, dispatch messages to the
  // appropriate transaction or fire events on messages that don't match any
  // transactions.
  //
  // I do not manage any transports that may be given to me.
  TIdSipTransactionDispatcher = class(TIdInterfacedObject,
                                      IIdSipTransactionListener,
                                      IIdSipTransportListener)
  private
    fT1Interval:     Cardinal;
    fT2Interval:     Cardinal;
    fT4Interval:     Cardinal;
    MsgListenerLock: TCriticalSection;
    MsgListeners:    TList;
    Transports:      TObjectList;
    TransportLock:   TCriticalSection;
    Transactions:    TObjectList;
    TransactionLock: TCriticalSection;

    procedure DeliverToTransaction(const Request: TIdSipRequest;
                                   const Receiver: TIdSipTransport); overload;
    procedure DeliverToTransaction(const Response: TIdSipResponse;
                                   const Receiver: TIdSipTransport); overload;
    function  FindTransaction(const R: TIdSipMessage;
                              ClientTran: Boolean): TIdSipTransaction;
    function  TransactionAt(const Index: Cardinal): TIdSipTransaction;
    function  TransportAt(const Index: Cardinal): TIdSipTransport;
  protected
    function  FindAppropriateTransport(const Msg: TIdSipMessage): TIdSipTransport;
    procedure NotifyListenersOfUnhandledRequest(const Request: TIdSipRequest;
                                                const Transaction: TIdSipTransaction;
                                                const Receiver: TIdSipTransport);
    procedure NotifyListenersOfUnhandledResponse(const Response: TIdSipResponse;
                                                 const Transaction: TIdSipTransaction;
                                                 const Receiver: TIdSipTransport);

    // IIdSipTransactionListener
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport); overload;
    procedure OnTerminated(const Transaction: TIdSipTransaction);

    // IIdSipTransportListener
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Receiver: TIdSipTransport); overload;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
    procedure AddTransport(const Transport: TIdSipTransport);
    function  AddClientTransaction(const InitialRequest: TIdSipRequest): TIdSipTransaction;
    function  AddServerTransaction(const InitialRequest: TIdSipRequest;
                                   const Receiver: TIdSipTransport): TIdSipTransaction;
    procedure ClearTransports;
    function  LoopDetected(const Request: TIdSipRequest): Boolean;
    procedure RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
    procedure RemoveUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
    procedure Send(const Msg: TIdSipMessage); virtual;
    function  TransactionCount: Integer;
    function  TransportCount: Integer;
    function  WillUseReliableTranport(const R: TIdSipMessage): Boolean;

    property T1Interval: Cardinal read fT1Interval write fT1Interval;
    property T2Interval: Cardinal read fT2Interval write fT2Interval;
    property T4Interval: Cardinal read fT4Interval write fT4Interval;
  end;

  // I am a SIP Transaction. As such, I am a finite state machine. I swallow
  // inappropriate messages, and inform my Dispatcher of interesting events.
  // These include the establishment of a new dialog, or my termination.
  //
  // Should I be terminated, for instance by a transport failure, my owning
  // Dispatcher immediately destroys me. Therefore, be sure that if you call
  // TrySendRequest it is the last call in a method as I could be dead before
  // the next line of the method is reached!
  TIdSipTransaction = class(TIdInterfacedObject)
  private
    fInitialRequest:  TIdSipRequest;
    fState:           TIdSipTransactionState;
    fDispatcher:      TIdSipTransactionDispatcher;
    TranListenerLock: TCriticalSection;
    TranListeners:    TList;
  protected
    FirstTime: Boolean;

    procedure ChangeToCompleted; overload; virtual;
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const T: TIdSipTransport); overload; virtual;
    procedure ChangeToProceeding; overload; virtual;
    procedure ChangeToProceeding(const R: TIdSipRequest;
                                 const T: TIdSipTransport); overload;
    procedure ChangeToProceeding(const R: TIdSipResponse;
                                 const T: TIdSipTransport); overload;
    procedure ChangeToTerminated(Quiet: Boolean); overload;
    procedure ChangeToTerminated(const R: TIdSipResponse;
                                 const T: TIdSipTransport); overload; virtual;
    procedure DoOnFail(const Reason: String); virtual;
    procedure NotifyOfFailure(const Reason: String);
    procedure NotifyOfTermination;
    procedure NotifyOfRequest(const R: TIdSipRequest;
                              const T: TIdSipTransport);
    procedure NotifyOfResponse(const R: TIdSipResponse;
                               const Receiver: TIdSipTransport);
    procedure SetState(const Value: TIdSipTransactionState); virtual;
    procedure TryResendInitialRequest;
    procedure TrySendRequest(const R: TIdSipRequest);
    procedure TrySendResponse(const R: TIdSipResponse); virtual;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher;
  public
    class function GetClientTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;
    class function GetServerTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;

    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); virtual;
    destructor  Destroy; override;

    procedure AddTransactionListener(const Listener: IIdSipTransactionListener);
    function  IsClient: Boolean; virtual; abstract;
    function  IsInvite: Boolean; virtual; abstract;
    function  IsNull: Boolean; virtual; abstract;
    function  IsServer: Boolean;
    procedure ReceiveRequest(const R: TIdSipRequest;
                            const T: TIdSipTransport); virtual;
    procedure ReceiveResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); virtual;
    procedure SendRequest; virtual;
    procedure SendResponse(const R: TIdSipResponse); virtual;
    procedure RemoveTransactionListener(const Listener: IIdSipTransactionListener);

    property InitialRequest: TIdSipRequest          read fInitialRequest;
    property State:          TIdSipTransactionState read fState;
  end;

  TIdSipServerTransaction = class(TIdSipTransaction)
  protected
    LastResponseSent: TIdSipResponse;
  public
    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); override;
    destructor Destroy; override;

    function  IsClient: Boolean; override;
  end;

  TIdSipServerInviteTransaction = class;
  TIdSipServerInviteTransactionTimer = class(TObject)
  private
    T2:     Cardinal;
    Owner:  TIdSipServerInviteTransaction;
    State:  TIdSipTransactionState;
    TimerG: TIdSipTimer;
    TimerH: TIdSipTimer;
    TimerI: TIdSipTimer;

    procedure OnTimerG(Sender: TObject);
    procedure OnTimerH(Sender: TObject);
    procedure OnTimerI(Sender: TObject);
  public
    constructor Create(const OwnerTran: TIdSipServerInviteTransaction;
                       const T1: Cardinal = DefaultT1;
                       const T2: Cardinal = DefaultT2;
                       const T4: Cardinal = DefaultT4);
    destructor  Destroy; override;

    procedure ChangeState(NewState: TIdSipTransactionState);
    procedure FireTimerG;
    procedure FireTimerH;
    procedure FireTimerI;
    procedure StartTimerG;
    procedure StartTimerH;
    procedure StartTimerI;
    procedure StopTimerG;
    procedure StopTimerH;
    procedure StopTimerI;
    function  TimerGIsRunning: Boolean;
    function  TimerHIsRunning: Boolean;
    function  TimerHInterval: Cardinal;
    function  TimerIIsRunning: Boolean;
  end;

  TIdSipServerInviteTransaction = class(TIdSipServerTransaction)
  private
    Timer: TIdSipServerInviteTransactionTimer;

    procedure ChangeToConfirmed(const R: TIdSipRequest;
                                const T: TIdSipTransport);
    function  Create100Response(const R: TIdSipRequest): TIdSipResponse;
    procedure TrySend100Response(const R: TIdSipRequest);
    procedure TrySendLastResponse(const R: TIdSipRequest);
  protected
    procedure SetState(const Value: TIdSipTransactionState); override;
    procedure TrySendResponse(const R: TIdSipResponse); override;
  public
    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerG;
    procedure FireTimerH;
    procedure FireTimerI;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(const R: TIdSipRequest;
                             const T: TIdSipTransport); override;
    procedure SendResponse(const R: TIdSipResponse); override;
  end;

  TIdSipServerNonInviteTransaction = class(TIdSipServerTransaction)
  private
    TimerJ: TIdSipTimer;

    procedure ChangeToTrying;
    procedure OnTimerJ(Sender: TObject);
    procedure TrySendLastResponse(const R: TIdSipRequest);
  protected
    procedure ChangeToCompleted; override;
    procedure TrySendResponse(const R: TIdSipResponse); override;
  public
    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerJ;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(const R: TIdSipRequest;
                             const T: TIdSipTransport); override;
    procedure SendResponse(const R: TIdSipResponse); override;
  end;

  TIdSipClientTransaction = class(TIdSipTransaction)
  public
    function  IsClient: Boolean; override;
  end;

  TIdSipClientInviteTransaction = class;
  TIdSipClientInviteTransactionTimer = class(TObject)
  private
    Lock:   TCriticalSection;
    Owner:  TIdSipClientInviteTransaction;
    State:  TIdSipTransactionState;
    TimerA: TIdSipTimer;
    TimerB: TIdSipTimer;
    TimerD: TIdSipTimer;

    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
  public
    constructor Create(const OwnerTran: TIdSipClientInviteTransaction;
                       const T1: Cardinal = DefaultT1);
    destructor  Destroy; override;

    procedure ChangeState(NewState: TIdSipTransactionState);
    procedure FireTimerA;
    procedure Start;
    procedure StartTimerA;
    procedure StartTimerB;
    procedure StartTimerD;
    procedure StopTimerA;
    procedure StopTimerB;
    procedure StopTimerD;
    function  TimerAInterval: Cardinal;
    function  TimerAIsRunning: Boolean;
    function  TimerBIsRunning: Boolean;
    function  TimerDIsRunning: Boolean;
  end;

  TIdSipClientInviteTransaction = class(TIdSipClientTransaction)
  private
    Timer: TIdSipClientInviteTransactionTimer;

    procedure ChangeToCalling;
    procedure TrySendACK(const R: TIdSipResponse);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const T: TIdSipTransport); override;
    procedure SetState(const Value: TIdSipTransactionState); override;
  public
    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    function  CreateACK(const R: TIdSipResponse): TIdSipRequest;
    procedure FireTimerA;
    procedure FireTimerB;
    procedure FireTimerD;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(const R: TIdSipResponse;
                              const T: TIdSipTransport); override;
    procedure SendRequest; override;
  end;

  TIdSipClientNonInviteTransaction = class;
  TIdSipClientNonInviteTransactionTimer = class(TObject)
  private
    Lock:   TCriticalSection;
    Owner:  TIdSipClientNonInviteTransaction;
    State:  TIdSipTransactionState;
    T2:     Cardinal;
    TimerE: TIdSipTimer;
    TimerF: TIdSipTimer;
    TimerK: TIdSipTimer;

    procedure OnTimerE(Sender: TObject);
    procedure OnTimerF(Sender: TObject);
    procedure OnTimerK(Sender: TObject);
  public
    constructor Create(const OwnerTran: TIdSipClientNonInviteTransaction;
                       const T1: Cardinal = DefaultT1;
                       const T2: Cardinal = DefaultT2;
                       const T4: Cardinal = DefaultT4);
    destructor  Destroy; override;

    procedure ChangeState(NewState: TIdSipTransactionState);
    procedure FireTimerE;
    procedure ResetTimerE(NewInterval: Cardinal);
    procedure Start;
    procedure StartTimerE;
    procedure StartTimerF;
    procedure StartTimerK;
    procedure StopTimerE;
    procedure StopTimerF;
    procedure StopTimerK;
    function  TimerEInterval: Cardinal;
    function  TimerEIsRunning: Boolean;
    function  TimerFIsRunning: Boolean;
    function  TimerKInterval: Cardinal;
    function  TimerKIsRunning: Boolean;
  end;

  TIdSipClientNonInviteTransaction = class(TIdSipClientTransaction)
  private
    Timer: TIdSipClientNonInviteTransactionTimer;
  protected
    procedure ChangeToTrying;
    procedure SetState(const Value: TIdSipTransactionState); override;
  public
    constructor Create(const Dispatcher: TIdSipTransactionDispatcher;
                       const InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerE;
    procedure FireTimerF;
    procedure FireTimerK;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(const R: TIdSipResponse;
                              const T: TIdSipTransport); override;
    procedure SendRequest; override;
  end;

implementation

uses
  IdException, IdSipConsts, IdSipDialogID, IdSipHeaders, Math, SysUtils;

//******************************************************************************
//* TIdSipTransactionDispatcher                                                *
//******************************************************************************
//* TIdSipTransactionDispatcher Public methods *********************************

constructor TIdSipTransactionDispatcher.Create;
begin
  inherited Create;

  Self.MsgListenerLock := TCriticalSection.Create;
  Self.MsgListeners    := TList.Create;

  Self.Transports    := TObjectList.Create(false);
  Self.TransportLock := TCriticalSection.Create;

  Self.Transactions    := TObjectList.Create(true);
  Self.TransactionLock := TCriticalSection.Create;

  Self.T1Interval := DefaultT1;
  Self.T2Interval := DefaultT2;
  Self.T4Interval := DefaultT4;
end;

destructor TIdSipTransactionDispatcher.Destroy;
begin
  Self.TransactionLock.Free;
  Self.Transactions.Free;
  Self.TransportLock.Free;
  Self.Transports.Free;
  Self.MsgListeners.Free;
  Self.MsgListenerLock.Free;

  inherited Destroy;
end;

procedure TIdSipTransactionDispatcher.AddUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
begin
  Self.MsgListenerLock.Acquire;
  try
    Self.MsgListeners.Add(Pointer(Listener));
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.AddTransport(const Transport: TIdSipTransport);
begin
  Self.TransportLock.Acquire;
  try
    Self.Transports.Add(Transport);
    Transport.AddTransportListener(Self);
  finally
    Self.TransportLock.Release;
  end;
end;

function TIdSipTransactionDispatcher.AddClientTransaction(const InitialRequest: TIdSipRequest): TIdSipTransaction;
begin
  Result := nil;

  Self.TransactionLock.Acquire;
  try
    try
      Result := TIdSipTransaction.GetClientTransactionType(InitialRequest).Create(Self, InitialRequest);
      Self.Transactions.Add(Result);
    except
      Self.Transactions.Remove(Result);
      Result := nil;

      raise;
    end;
  finally
    Self.TransactionLock.Release;
  end;
end;

function TIdSipTransactionDispatcher.AddServerTransaction(const InitialRequest: TIdSipRequest;
                                                          const Receiver: TIdSipTransport): TIdSipTransaction;
begin
  Result := nil;

  Self.TransactionLock.Acquire;
  try
    try
      Result := TIdSipTransaction.GetServerTransactionType(InitialRequest).Create(Self, InitialRequest);
      Self.Transactions.Add(Result);
    except
      Self.Transactions.Remove(Result);

      raise;
    end;
  finally
    Self.TransactionLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.ClearTransports;
begin
  Self.Transports.Clear;
end;

function TIdSipTransactionDispatcher.LoopDetected(const Request: TIdSipRequest): Boolean;
var
  I: Integer;
begin
  // cf. RFC 3261 section 8.2.2.2
  Result := false;

  Self.TransactionLock.Acquire;
  try
    I := 0;
    while (I < Self.Transactions.Count) and not Result do begin
      if Self.TransactionAt(I).IsServer then
        Result := Request.From.IsEqualTo(Self.TransactionAt(I).InitialRequest.From)
              and (Request.CallID = Self.TransactionAt(I).InitialRequest.CallID)
              and (Request.CSeq.IsEqualTo(Self.TransactionAt(I).InitialRequest.CSeq))
              and not Request.Match(Self.TransactionAt(I).InitialRequest);
      Inc(I);
    end;
  finally
    Self.TransactionLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
begin
  Assert(itsTerminated = TerminatedTransaction.State,
         'Transactions must only be removed when they''re terminated');

  Self.TransactionLock.Acquire;
  try
    Self.Transactions.Remove(TerminatedTransaction);
  finally
    Self.TransactionLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.RemoveUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
begin
  Self.MsgListenerLock.Acquire;
  try
    Self.MsgListeners.Remove(Pointer(Listener));
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.Send(const Msg: TIdSipMessage);
var
  MsgLen:       Cardinal;
  RewrittenVia: Boolean;
begin
  MsgLen := Length(Msg.AsString);
  RewrittenVia := (MsgLen > 1300) and (Msg.LastHop.Transport = sttUDP);

  if RewrittenVia then
    Msg.LastHop.Transport := sttTCP;

  try
    Self.FindAppropriateTransport(Msg).Send(Msg);
  except
    on EIdException do begin
      Msg.LastHop.Transport := sttUDP;

      Self.FindAppropriateTransport(Msg).Send(Msg);
    end;
  end;
end;

function TIdSipTransactionDispatcher.TransactionCount: Integer;
begin
  Self.TransactionLock.Acquire;
  try
    Result := Self.Transactions.Count;
  finally
    Self.TransactionLock.Release;
  end;
end;

function TIdSipTransactionDispatcher.TransportCount: Integer;
begin
  Result := Self.Transports.Count;
end;

function TIdSipTransactionDispatcher.WillUseReliableTranport(const R: TIdSipMessage): Boolean;
begin
  Assert(R.Path.Length > 0, 'Messages must have at least one Via header');

  Result := R.LastHop.Transport <> sttUDP;

//  Result := Self.FindAppropriateTransport(R).IsReliable;
end;

//* TIdSipTransactionDispatcher Protected methods ******************************

function TIdSipTransactionDispatcher.FindAppropriateTransport(const Msg: TIdSipMessage): TIdSipTransport;
var
  I: Integer;
begin
  Result := nil;

  Self.TransportLock.Acquire;
  try
    I := 0;

    while (I < Self.Transports.Count)
      and (Self.TransportAt(I).GetTransportType <> Msg.LastHop.Transport) do
      Inc(I);

    // What should we do if there are no appropriate transports to use?
    // It means that someone didn't configure the dispatcher properly,
    // most likely.
    if (I < Self.Transports.Count) then
      Result := Self.TransportAt(I)
    else
      raise EUnknownTransport.Create('The dispatcher cannot find a '
                                   + TransportToStr(Msg.LastHop.Transport)
                                   + ' transport for a message');
  finally
    Self.TransportLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyListenersOfUnhandledRequest(const Request: TIdSipRequest;
                                                                        const Transaction: TIdSipTransaction;
                                                                        const Receiver: TIdSipTransport);
var
  I: Integer;
begin
  Self.MsgListenerLock.Acquire;
  try
    for I := 0 to Self.MsgListeners.Count - 1 do
      IIdSipUnhandledMessageListener(Self.MsgListeners[I]).OnReceiveUnhandledRequest(Request,
                                                                                     Transaction,
                                                                                     Receiver);
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyListenersOfUnhandledResponse(const Response: TIdSipResponse;
                                                                         const Transaction: TIdSipTransaction;
                                                                         const Receiver: TIdSipTransport);
var
  I: Integer;
begin
  Self.MsgListenerLock.Acquire;
  try
    for I := 0 to Self.MsgListeners.Count - 1 do
      IIdSipUnhandledMessageListener(Self.MsgListeners[I]).OnReceiveUnhandledResponse(Response,
                                                                                      Transaction,
                                                                                      Receiver);
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.OnFail(const Transaction: TIdSipTransaction;
                                             const Reason: String);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(const Request: TIdSipRequest;
                                                       const Transaction: TIdSipTransaction;
                                                       const Receiver: TIdSipTransport);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(const Response: TIdSipResponse;
                                                        const Transaction: TIdSipTransaction;
                                                        const Receiver: TIdSipTransport);
begin
end;

procedure TIdSipTransactionDispatcher.OnTerminated(const Transaction: TIdSipTransaction);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(const Request: TIdSipRequest;
                                                       const Receiver: TIdSipTransport);
begin
  Self.DeliverToTransaction(Request, Receiver);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(const Response: TIdSipResponse;
                                                        const Receiver: TIdSipTransport);
begin
  Self.DeliverToTransaction(Response, Receiver);
end;

//* TIdSipTransactionDispatcher Private methods ********************************

procedure TIdSipTransactionDispatcher.DeliverToTransaction(const Request: TIdSipRequest;
                                                           const Receiver: TIdSipTransport);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Request, false);

  if Assigned(Tran) then
    Tran.ReceiveRequest(Request, Receiver)
  else begin
    if Request.IsAck then begin
      Self.NotifyListenersOfUnhandledRequest(Request, nil, Receiver);
      // This is DANGEROUS and it SUCKS --------------^^^
    end
    else begin
      Tran := Self.AddServerTransaction(Request, Receiver);

      Self.NotifyListenersOfUnhandledRequest(Request, Tran, Receiver);
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(const Response: TIdSipResponse;
                                                           const Receiver: TIdSipTransport);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Response, true);

  if Assigned(Tran) then
    Tran.ReceiveResponse(Response, Receiver)
  else
    Self.NotifyListenersOfUnhandledResponse(Response, nil, Receiver);
    // TODO: this is ugly and dangerous.              ^^^
end;

function TIdSipTransactionDispatcher.FindTransaction(const R: TIdSipMessage;
                                                     ClientTran: Boolean): TIdSipTransaction;
var
  I: Integer;
begin
  Result := nil;

  Self.TransactionLock.Acquire;
  try
    I := 0;
    while (I < Self.Transactions.Count) and not Assigned(Result) do
      if (Self.TransactionAt(I).IsClient = ClientTran) and Self.TransactionAt(I).InitialRequest.Match(R) then
        Result := Self.TransactionAt(I)
      else
       Inc(I);
  finally
    Self.TransactionLock.Release;
  end;
end;

function TIdSipTransactionDispatcher.TransactionAt(const Index: Cardinal): TIdSipTransaction;
begin
  Result := Self.Transactions[Index] as TIdSipTransaction;
end;

function TIdSipTransactionDispatcher.TransportAt(const Index: Cardinal): TIdSipTransport;
begin
  Result := Self.Transports[Index] as TIdSipTransport;
end;

//******************************************************************************
//* TIdSipTransaction                                                          *
//******************************************************************************
//* TIdSipTransaction Public methods *******************************************

class function TIdSipTransaction.GetClientTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;
begin
  if Request.IsInvite then
    Result := TIdSipClientInviteTransaction
  else
    Result := TIdSipClientNonInviteTransaction;
end;

class function TIdSipTransaction.GetServerTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;
begin
  if Request.IsInvite then
    Result := TIdSipServerInviteTransaction
  else
    Result := TIdSipServerNonInviteTransaction;
end;

constructor TIdSipTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                     const InitialRequest: TIdSipRequest);
begin
  inherited Create;

  Self.fDispatcher := Dispatcher;

  Self.fInitialRequest  := TIdSipRequest.Create;
  Self.InitialRequest.Assign(InitialRequest);

  Self.TranListenerLock := TCriticalSection.Create;
  Self.TranListeners    := TList.Create;

  Self.FirstTime := true;
end;

destructor TIdSipTransaction.Destroy;
begin
  Self.TranListeners.Free;
  Self.TranListenerLock.Free;
  Self.InitialRequest.Free;

  inherited Create;
end;

procedure TIdSipTransaction.AddTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListenerLock.Acquire;
  try
    Self.TranListeners.Add(Pointer(Listener));
  finally
    Self.TranListenerLock.Release;
  end;
end;

function TIdSipTransaction.IsServer: Boolean;
begin
  Result := not Self.IsClient;
end;

procedure TIdSipTransaction.ReceiveRequest(const R: TIdSipRequest;
                                           const T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.ReceiveResponse(const R: TIdSipResponse;
                                            const T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.SendRequest;
begin
end;

procedure TIdSipTransaction.SendResponse(const R: TIdSipResponse);
begin
end;

procedure TIdSipTransaction.RemoveTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListenerLock.Acquire;
  try
    Self.TranListeners.Remove(Pointer(Listener));
  finally
    Self.TranListenerLock.Release;
  end;
end;

//* TIdSipTransaction Protected methods ****************************************

procedure TIdSipTransaction.ChangeToCompleted;
begin
  Self.SetState(itsCompleted);
end;

procedure TIdSipTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                              const T: TIdSipTransport);
begin
  Self.ChangeToCompleted;

  Self.NotifyOfResponse(R, T);
end;

procedure TIdSipTransaction.ChangeToProceeding;
begin
  Self.SetState(itsProceeding);
end;

procedure TIdSipTransaction.ChangeToProceeding(const R: TIdSipRequest;
                                               const T: TIdSipTransport);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfRequest(R, T);
end;

procedure TIdSipTransaction.ChangeToProceeding(const R: TIdSipResponse;
                                               const T: TIdSipTransport);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfResponse(R, T);
end;

procedure TIdSipTransaction.ChangeToTerminated(Quiet: Boolean);
begin
  Self.SetState(itsTerminated);

  if not Quiet then
    Self.NotifyOfTermination;
end;

procedure TIdSipTransaction.ChangeToTerminated(const R: TIdSipResponse;
                                               const T: TIdSipTransport);
begin
  Self.NotifyOfResponse(R, T);
  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.DoOnFail(const Reason: String);
begin
  Self.NotifyOfFailure(Reason);
  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.NotifyOfFailure(const Reason: String);
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnFail(Self, Reason);
  finally
    Self.TranListenerLock.Release;
  end;
end;

procedure TIdSipTransaction.NotifyOfTermination;
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnTerminated(Self);
  finally
    Self.TranListenerLock.Release;
  end;

  // WARNING - I am not sure if this is safe or even sane. We're
  // asking an object to commit suicide.
  Self.Dispatcher.RemoveTransaction(Self);
end;

procedure TIdSipTransaction.NotifyOfRequest(const R: TIdSipRequest;
                                            const T: TIdSipTransport);
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnReceiveRequest(R, Self, T);
  finally
    Self.TranListenerLock.Release;
  end;
end;

procedure TIdSipTransaction.NotifyOfResponse(const R: TIdSipResponse;
                                             const Receiver: TIdSipTransport);
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnReceiveResponse(R, Self, Receiver);
  finally
    Self.TranListenerLock.Release;
  end;
end;

procedure TIdSipTransaction.SetState(const Value: TIdSipTransactionState);
begin
  fState := Value;
end;

procedure TIdSipTransaction.TryResendInitialRequest;
begin
  if not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendRequest(Self.InitialRequest);
end;

procedure TIdSipTransaction.TrySendRequest(const R: TIdSipRequest);
var
  CopyOfRequest: TIdSipRequest;
begin
  CopyOfRequest := TIdSipRequest.Create;
  try
    CopyOfRequest.Assign(R);
    try
      Self.Dispatcher.Send(CopyOfRequest);
    except
      on E: EIdException do
        Self.DoOnFail(E.Message);
    end;
  finally
    CopyOfRequest.Free;
  end;
end;

procedure TIdSipTransaction.TrySendResponse(const R: TIdSipResponse);
var
  CopyOfResponse: TIdSipResponse;
begin
  CopyOfResponse := TIdSipResponse.Create;
  try
    CopyOfResponse.Assign(R);
    try
      Self.Dispatcher.Send(CopyOfResponse);
    except
      on E: EIdException do
        Self.DoOnFail(E.Message);
    end;
  finally
    CopyOfResponse.Free;
  end;
end;

//******************************************************************************
//* TIdSipServerTransaction                                                    *
//******************************************************************************
//* TIdSipServerTransaction Public methods *************************************

constructor TIdSipServerTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                           const InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.LastResponseSent := TIdSipResponse.Create;
end;

destructor TIdSipServerTransaction.Destroy;
begin
  Self.LastResponseSent.Free;

  inherited Destroy;
end;

function TIdSipServerTransaction.IsClient: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdSipServerInviteTransactionTimer                                         *
//******************************************************************************
//* TIdSipServerInviteTransactionTimer Public methods **************************

constructor TIdSipServerInviteTransactionTimer.Create(const OwnerTran: TIdSipServerInviteTransaction;
                                                      const T1: Cardinal = DefaultT1;
                                                      const T2: Cardinal = DefaultT2;
                                                      const T4: Cardinal = DefaultT4);
begin
  inherited Create;
  Self.Owner := OwnerTran;

  Self.TimerG := TIdSipTimer.Create;
  Self.TimerG.Interval := T1;
  Self.TimerG.OnTimer  := Self.OnTimerG;

  Self.TimerH := TIdSipTimer.Create;
  Self.TimerH.Interval := 64*T1;
  Self.TimerH.OnTimer  := Self.OnTimerH;

  Self.TimerI := TIdSipTimer.Create;
  Self.TimerI.Interval := T4;
  Self.TimerI.OnTimer  := Self.OnTimerI;

  Self.T2 := T2
end;

destructor TIdSipServerInviteTransactionTimer.Destroy;
begin
  Self.TimerI.TerminateAndWaitFor;
  Self.TimerI.Free;

  Self.TimerH.TerminateAndWaitFor;
  Self.TimerH.Free;

  Self.TimerG.TerminateAndWaitFor;
  Self.TimerG.Free;

  inherited Destroy;
end;

procedure TIdSipServerInviteTransactionTimer.ChangeState(NewState: TIdSipTransactionState);
begin
  Self.State := NewState;

  case NewState of
    itsCompleted: begin
      Self.StartTimerG;
      Self.StartTimerH;
    end;
    itsConfirmed: begin
      Self.StopTimerG;
      Self.StopTimerH;
      Self.StartTimerI;
    end;
    itsTerminated: begin
      Self.StopTimerG;
      Self.StopTimerH;
      Self.StopTimerI;
    end;
  end;
end;

procedure TIdSipServerInviteTransactionTimer.FireTimerG;
begin
  if (Self.TimerG.Interval < T2) then
    Self.TimerG.Interval := 2*Self.TimerG.Interval
  else
    Self.TimerG.Interval := T2;

  Self.Owner.FireTimerG;
end;

procedure TIdSipServerInviteTransactionTimer.FireTimerH;
begin
  Self.Owner.FireTimerH;
end;

procedure TIdSipServerInviteTransactionTimer.FireTimerI;
begin
  Self.Owner.FireTimerI;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerG;
begin
  Self.TimerG.Start;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerH;
begin
  Self.TimerH.Start;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerI;
begin
  Self.TimerI.Start;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerG;
begin
  Self.TimerG.Stop;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerH;
begin
  Self.TimerH.Stop;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerI;
begin
  Self.TimerI.Stop;
end;

function TIdSipServerInviteTransactionTimer.TimerGIsRunning: Boolean;
begin
  Result := not Self.TimerG.Stopped;
end;

function TIdSipServerInviteTransactionTimer.TimerHIsRunning: Boolean;
begin
  Result := not Self.TimerH.Stopped;
end;

function TIdSipServerInviteTransactionTimer.TimerHInterval: Cardinal;
begin
  Result := Self.TimerH.Interval;
end;

function TIdSipServerInviteTransactionTimer.TimerIIsRunning: Boolean;
begin
  Result := not Self.TimerI.Stopped;  
end;

//* TIdSipServerInviteTransactionTimer Private methods *************************

procedure TIdSipServerInviteTransactionTimer.OnTimerG(Sender: TObject);
begin
  Self.FireTimerG;
end;

procedure TIdSipServerInviteTransactionTimer.OnTimerH(Sender: TObject);
begin
  Self.FireTimerH;
end;

procedure TIdSipServerInviteTransactionTimer.OnTimerI(Sender: TObject);
begin
  Self.FireTimerI;
end;

//******************************************************************************
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                                 const InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.Timer := TIdSipServerInviteTransactionTimer.Create(Self,
                                                          Self.Dispatcher.T1Interval,
                                                          Self.Dispatcher.T2Interval,
                                                          Self.Dispatcher.T4Interval);

  Self.ChangeToProceeding;
end;

destructor TIdSipServerInviteTransaction.Destroy;
begin
  Self.Timer.Free;

  inherited Destroy;
end;

procedure TIdSipServerInviteTransaction.FireTimerG;
begin
  if (Self.State = itsCompleted)
    and not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendLastResponse(Self.InitialRequest);
end;

procedure TIdSipServerInviteTransaction.FireTimerH;
begin
  Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipServerInviteTransaction.FireTimerI;
begin
  Self.ChangeToTerminated(true);
end;

function TIdSipServerInviteTransaction.IsInvite: Boolean;
begin
  Result := true;
end;

function TIdSipServerInviteTransaction.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdSipServerInviteTransaction.ReceiveRequest(const R: TIdSipRequest;
                                                       const T: TIdSipTransport);
begin
  inherited ReceiveRequest(R, T);

  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.TrySend100Response(Self.InitialRequest);

    Self.ChangeToProceeding(R, T);
  end else begin
    case Self.State of
      itsProceeding: Self.TrySendLastResponse(R);

      itsCompleted: begin
        if R.IsInvite then
          Self.TrySendLastResponse(R)
        else if R.IsAck then
          Self.ChangeToConfirmed(R, T);
      end;
    end;
  end;
end;

procedure TIdSipServerInviteTransaction.SendResponse(const R: TIdSipResponse);
begin
  Self.TrySendResponse(R);
  if (Self.State = itsProceeding) then begin
    case (R.StatusCode div 100) of
      1:    Self.ChangeToProceeding;
      2:    Self.ChangeToTerminated(false);
      3..6: Self.ChangeToCompleted;
    end;
  end;
end;

//* TIdSipServerInviteTransaction Protected methods ***************************

procedure TIdSipServerInviteTransaction.SetState(const Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(const R: TIdSipRequest;
                                                          const T: TIdSipTransport);
begin
  Self.SetState(itsConfirmed);
end;

function TIdSipServerInviteTransaction.Create100Response(const R: TIdSipRequest): TIdSipResponse;
begin
  Result := TIdSipResponse.Create;
  try
    Result.StatusCode := SIPTrying;
    Result.SIPVersion := SIPVersion;

    Result.From     := R.From;
    Result.ToHeader := R.ToHeader;
    Result.CallID   := R.CallID;
    Result.CSeq     := R.CSeq;

    Result.AddHeaders(Self.InitialRequest.Path);
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipServerInviteTransaction.TrySend100Response(const R: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.Create100Response(Self.InitialRequest);
  try
    Self.TrySendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipServerInviteTransaction.TrySendLastResponse(const R: TIdSipRequest);
begin
  Self.TrySendResponse(Self.LastResponseSent);
end;

procedure TIdSipServerInviteTransaction.TrySendResponse(const R: TIdSipResponse);
begin
  if (not R.IsEqualTo(Self.LastResponseSent)) then
    Self.LastResponseSent.Assign(R);

  inherited TrySendResponse(R);
end;

//******************************************************************************
//* TIdSipServerNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipServerNonInviteTransaction Public methods ****************************

constructor TIdSipServerNonInviteTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                                    const InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.ChangeToTrying;

  Self.TimerJ          := TIdSipTimer.Create(true);
  Self.TimerJ.Interval := 64*Self.Dispatcher.T1Interval;
  Self.TimerJ.OnTimer  := Self.OnTimerJ;
end;

destructor TIdSipServerNonInviteTransaction.Destroy;
begin
  Self.TimerJ.TerminateAndWaitFor;
  Self.TimerJ.Free;

  inherited Destroy;
end;

procedure TIdSipServerNonInviteTransaction.FireTimerJ;
begin
  Self.ChangeToTerminated(true);
end;

function TIdSipServerNonInviteTransaction.IsInvite: Boolean;
begin
  Result := false;
end;

function TIdSipServerNonInviteTransaction.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdSipServerNonInviteTransaction.ReceiveRequest(const R: TIdSipRequest;
                                                          const T: TIdSipTransport);
begin
  inherited ReceiveRequest(R, T);

  if Self.FirstTime then begin
    Self.FirstTime := false;
    Self.ChangeToTrying;
    Self.NotifyOfRequest(R, T);
  end
  else begin
    if (Self.State in [itsCompleted, itsProceeding]) then
      Self.TrySendLastResponse(R);
  end;
end;

procedure TIdSipServerNonInviteTransaction.SendResponse(const R: TIdSipResponse);
begin
  if (Self.State in [itsTrying, itsProceeding]) then begin
    if R.IsFinal then
      Self.ChangeToCompleted
    else begin
      Self.LastResponseSent.Assign(R);
      Self.ChangeToProceeding;
    end;

    Self.TrySendResponse(R);
  end;
end;

//* TIdSipServerNonInviteTransaction Protected methods *************************

procedure TIdSipServerNonInviteTransaction.ChangeToCompleted;
begin
  inherited ChangeToCompleted;

  Self.TimerJ.Start;
end;

procedure TIdSipServerNonInviteTransaction.TrySendResponse(const R: TIdSipResponse);
begin
  Self.LastResponseSent.Assign(R);

  inherited TrySendResponse(R);
end;

//* TIdSipServerNonInviteTransaction Private methods ***************************

procedure TIdSipServerNonInviteTransaction.ChangeToTrying;
begin
  Self.SetState(itsTrying);
end;

procedure TIdSipServerNonInviteTransaction.OnTimerJ(Sender: TObject);
begin
  Self.FireTimerJ;
end;

procedure TIdSipServerNonInviteTransaction.TrySendLastResponse(const R: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.Create;
  try
    Response.Assign(Self.LastResponseSent);
    Self.TrySendResponse(Response);
  finally
    Response.Free;
  end;
end;

//******************************************************************************
//* TIdSipClientTransaction                                                    *
//******************************************************************************
//* TIdSipClientTransaction Public methods *************************************

function TIdSipClientTransaction.IsClient: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdSipClientInviteTransactionTimer                                         *
//******************************************************************************
//* TIdSipClientInviteTransactionTimer Public methods **************************

constructor TIdSipClientInviteTransactionTimer.Create(const OwnerTran: TIdSipClientInviteTransaction;
                                                      const T1: Cardinal = DefaultT1);
begin
  inherited Create;
  Self.Lock := TCriticalSection.Create;
  Self.Owner := OwnerTran;

  Self.TimerA          := TIdSipTimer.Create(true);
  Self.TimerA.Interval := T1;
  Self.TimerA.OnTimer  := Self.OnTimerA;

  Self.TimerB          := TIdSipTimer.Create(true);
  Self.TimerB.Interval := 64*T1;
  Self.TimerB.OnTimer  := Self.OnTimerB;

  Self.TimerD          := TIdSipTimer.Create(true);
  Self.TimerD.Interval := 64*T1;
  Self.TimerD.OnTimer  := Self.OnTimerD;
end;

destructor TIdSipClientInviteTransactionTimer.Destroy;
begin
  Self.TimerD.TerminateAndWaitFor;
  Self.TimerD.Free;

  Self.TimerB.TerminateAndWaitFor;
  Self.TimerB.Free;

  Self.TimerA.TerminateAndWaitFor;
  Self.TimerA.Free;

  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipClientInviteTransactionTimer.ChangeState(NewState: TIdSipTransactionState);
begin
  Self.State := NewState;

  if (NewState <> itsCalling) then begin
    Self.StopTimerA;
    Self.StopTimerB;
  end;
  
  case NewState of
    itsCompleted:  Self.StartTimerD;
    itsTerminated: Self.StopTimerD;
  end;

end;

procedure TIdSipClientInviteTransactionTimer.FireTimerA;
begin
  Self.Lock.Acquire;
  try
    Self.TimerA.Interval := 2*Self.TimerA.Interval;
  finally
    Self.Lock.Release;
  end;

  Owner.FireTimerA;
end;

procedure TIdSipClientInviteTransactionTimer.Start;
begin
  Self.StartTimerA;
  Self.StartTimerB;
end;

procedure TIdSipClientInviteTransactionTimer.StartTimerA;
begin
  Self.TimerA.Start;
end;

procedure TIdSipClientInviteTransactionTimer.StartTimerB;
begin
  Self.TimerB.Start;
end;

procedure TIdSipClientInviteTransactionTimer.StartTimerD;
begin
  Self.TimerD.Start;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerA;
begin
  Self.TimerA.Stop;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerB;
begin
  Self.TimerB.Stop;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerD;
begin
  Self.TimerD.Stop;
end;

function TIdSipClientInviteTransactionTimer.TimerAInterval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.TimerA.Interval;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipClientInviteTransactionTimer.TimerAIsRunning: Boolean;
begin
  Result := not Self.TimerA.Stopped;
end;

function TIdSipClientInviteTransactionTimer.TimerBIsRunning: Boolean;
begin
  Result := not Self.TimerB.Stopped;
end;

function TIdSipClientInviteTransactionTimer.TimerDIsRunning: Boolean;
begin
  Result := not Self.TimerD.Stopped;
end;

//* TIdSipClientInviteTransactionTimer Private methods *************************

procedure TIdSipClientInviteTransactionTimer.OnTimerA(Sender: TObject);
begin
  Self.FireTimerA;
end;

procedure TIdSipClientInviteTransactionTimer.OnTimerB(Sender: TObject);
begin
  Self.StopTimerB;
  Owner.FireTimerB;
end;

procedure TIdSipClientInviteTransactionTimer.OnTimerD(Sender: TObject);
begin
  Self.StopTimerD;
  Owner.FireTimerD;

  Self.TimerA.Stop;
  Self.TimerB.Stop;
end;

//******************************************************************************
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                                 const InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.Timer := TIdSipClientInviteTransactionTimer.Create(Self,
                                                          Self.Dispatcher.T1Interval);
  Self.ChangeToCalling;  
end;

destructor TIdSipClientInviteTransaction.Destroy;
begin
  Self.Timer.Free;
  inherited Destroy;
end;

function TIdSipClientInviteTransaction.CreateACK(const R: TIdSipResponse): TIdSipRequest;
var
  Routes: TIdSipHeadersFilter;
begin
  Result := TIdSipRequest.Create;
  try
    Result.Method          := MethodAck;
    Result.RequestUri      := Self.InitialRequest.RequestUri;
    Result.SIPVersion      := Self.InitialRequest.SIPVersion;
    Result.CallID          := Self.InitialRequest.CallID;
    Result.From            := Self.InitialRequest.From;
    Result.MaxForwards     := Result.DefaultMaxForwards;
    Result.ToHeader        := R.ToHeader;
    Result.Path.Add(Self.InitialRequest.LastHop);
    Result.CSeq.SequenceNo := Self.InitialRequest.CSeq.SequenceNo;
    Result.CSeq.Method     := MethodAck;
    Result.ContentLength   := 0;
    Result.Body            := '';

    Routes := TIdSipHeadersFilter.Create(R.Headers, RouteHeader);
    try
      Result.AddHeaders(Routes);
    finally
      Routes.Free;
    end;
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipClientInviteTransaction.FireTimerA;
begin
  if (Self.State = itsCalling) then
    Self.TryResendInitialRequest;
end;

procedure TIdSipClientInviteTransaction.FireTimerB;
begin
  if (Self.State = itsCalling) then
    Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipClientInviteTransaction.FireTimerD;
begin
  Self.ChangeToTerminated(true);
end;

function TIdSipClientInviteTransaction.IsInvite: Boolean;
begin
  Result := true;
end;

function TIdSipClientInviteTransaction.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdSipClientInviteTransaction.ReceiveResponse(const R: TIdSipResponse;
                                                        const T: TIdSipTransport);
begin
  case Self.State of
    itsCalling: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, T);
        2: Self.ChangeToTerminated(R, T);
      else
        Self.ChangeToCompleted(R, T);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, T);
        2: Self.ChangeToTerminated(R, T);
      else
        Self.ChangeToCompleted(R, T);
      end;
    end;

    itsCompleted: begin
      if R.IsFinal then
        Self.ChangeToCompleted(R, T);
    end;
  end;
end;

procedure TIdSipClientInviteTransaction.SendRequest;
begin
  inherited SendRequest;

  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.ChangeToCalling;

    Self.Timer.Start;

    Self.TrySendRequest(Self.InitialRequest);
  end;
end;

//* TIdSipClientInviteTransaction Protected methods ****************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                                          const T: TIdSipTransport);
var
  FirstResponse: Boolean;
begin
  FirstResponse := Self.State <> itsCompleted;

  // It's unfortunate that we can't simply call inherited.
  // However, TrySendACK must be called before NotifyOfResponse,
  // and we have to set Self.State to itsCompleted before
  // TrySendACK because a transport failure changes Self.State
  // to itsTerminated.

  Self.SetState(itsCompleted);
  Self.TrySendACK(R);
  if FirstResponse then
    Self.NotifyOfResponse(R, T);
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCalling;
begin
  Self.SetState(itsCalling);
end;

procedure TIdSipClientInviteTransaction.SetState(const Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

procedure TIdSipClientInviteTransaction.TrySendACK(const R: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.CreateACK(R);
  try
    Self.TrySendRequest(Ack);
  finally
    Ack.Free;
  end;
end;

//******************************************************************************
//* TIdSipClientNonInviteTransactionTimer                                      *
//******************************************************************************
//* TIdSipClientNonInviteTransactionTimer Public methods ***********************

constructor TIdSipClientNonInviteTransactionTimer.Create(const OwnerTran: TIdSipClientNonInviteTransaction;
                                                         const T1: Cardinal = DefaultT1;
                                                         const T2: Cardinal = DefaultT2;
                                                         const T4: Cardinal = DefaultT4);
begin
  inherited Create;
  Self.Lock := TCriticalSection.Create;
  Self.Owner := OwnerTran;

  Self.TimerE          := TIdSipTimer.Create(true);
  Self.TimerE.Interval := T1;
  Self.TimerE.OnTimer  := Self.OnTimerE;

  Self.TimerF          := TIdSipTimer.Create(true);
  Self.TimerF.Interval := 64*T1;
  Self.TimerF.OnTimer  := Self.OnTimerF;

  Self.TimerK          := TIdSipTimer.Create(true);
  Self.TimerK.Interval := T4;
  Self.TimerK.OnTimer  := Self.OnTimerK;

  Self.T2 := T2;
end;

destructor TIdSipClientNonInviteTransactionTimer.Destroy;
begin
  Self.TimerK.TerminateAndWaitFor;
  Self.TimerK.Free;

  Self.TimerF.TerminateAndWaitFor;
  Self.TimerF.Free;

  Self.TimerE.TerminateAndWaitFor;
  Self.TimerE.Free;

  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipClientNonInviteTransactionTimer.ChangeState(NewState: TIdSipTransactionState);
begin
  Self.State := NewState;

  case NewState of
    itsCompleted: begin
      Self.StopTimerE;
      Self.StopTimerF;
      Self.StartTimerK;
    end;
    itsTerminated: begin
      Self.StopTimerE;
      Self.StopTimerF;
      Self.StopTimerK;
    end;
  end;
end;

procedure TIdSipClientNonInviteTransactionTimer.FireTimerE;
begin
  Self.Lock.Acquire;
  try
    if (Self.State = itsTrying) then begin
      if (Self.TimerE.Interval < Self.T2) then
        Self.TimerE.Interval := 2*Self.TimerE.Interval
      else
        Self.TimerE.Interval := Self.T2;
    end
    else if (Self.State = itsProceeding) then begin
      Self.TimerE.Interval := Self.T2;
    end;
  finally
    Self.Lock.Release;
  end;

  Owner.FireTimerE;
end;

procedure TIdSipClientNonInviteTransactionTimer.ResetTimerE(NewInterval: Cardinal);
begin
  Self.StopTimerE;
  Self.Lock.Acquire;
  try
    Self.TimerE.Interval := NewInterval;
  finally
    Self.Lock.Release;
  end;
  Self.StartTimerE;
end;

procedure TIdSipClientNonInviteTransactionTimer.Start;
begin
  Self.StartTimerE;
  Self.StartTimerF;
end;

procedure TIdSipClientNonInviteTransactionTimer.StartTimerE;
begin
  Self.TimerE.Start;
end;

procedure TIdSipClientNonInviteTransactionTimer.StartTimerF;
begin
  Self.TimerF.Start;
end;

procedure TIdSipClientNonInviteTransactionTimer.StartTimerK;
begin
  Self.TimerK.Start;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerE;
begin
  Self.TimerE.Stop;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerF;
begin
  Self.TimerF.Stop;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerK;
begin
  Self.TimerK.Stop;
end;

function TIdSipClientNonInviteTransactionTimer.TimerEInterval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.TimerE.Interval;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipClientNonInviteTransactionTimer.TimerEIsRunning: Boolean;
begin
  Result := not Self.TimerE.Stopped;
end;

function TIdSipClientNonInviteTransactionTimer.TimerFIsRunning: Boolean;
begin
  Result := not Self.TimerF.Stopped;
end;

function TIdSipClientNonInviteTransactionTimer.TimerKInterval: Cardinal;
begin
  Result := Self.TimerK.Interval;
end;

function TIdSipClientNonInviteTransactionTimer.TimerKIsRunning: Boolean;
begin
  Result := not Self.TimerK.Stopped;
end;

//* TIdSipClientNonInviteTransactionTimer Private methods **********************

procedure TIdSipClientNonInviteTransactionTimer.OnTimerE(Sender: TObject);
begin
  Self.FireTimerE;
end;

procedure TIdSipClientNonInviteTransactionTimer.OnTimerF(Sender: TObject);
begin
  Owner.FireTimerF;
end;

procedure TIdSipClientNonInviteTransactionTimer.OnTimerK(Sender: TObject);
begin
  Owner.FireTimerK;
end;

//******************************************************************************
//* TIdSipClientNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipClientNonInviteTransaction Public methods ****************************

constructor TIdSipClientNonInviteTransaction.Create(const Dispatcher: TIdSipTransactionDispatcher;
                                                    const InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.Timer := TIdSipClientNonInviteTransactionTimer.Create(Self,
                                                             Self.Dispatcher.T1Interval,
                                                             Self.Dispatcher.T2Interval,
                                                             Self.Dispatcher.T4Interval);

  Self.ChangeToTrying;
end;

destructor TIdSipClientNonInviteTransaction.Destroy;
begin
  Self.Timer.Free;

  inherited Destroy;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerE;
begin
  if (Self.State in [itsTrying, itsProceeding]) then
    Self.TryResendInitialRequest;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerF;
begin
  Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipClientNonInviteTransaction.FireTimerK;
begin
  Self.ChangeToTerminated(true);
end;

function TIdSipClientNonInviteTransaction.IsInvite: Boolean;
begin
  Result := false;
end;

function TIdSipClientNonInviteTransaction.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdSipClientNonInviteTransaction.ReceiveResponse(const R: TIdSipResponse;
                                                           const T: TIdSipTransport);
begin
  if (Self.State in [itsTrying, itsProceeding]) then begin
    if R.IsFinal then
      Self.ChangeToCompleted(R, T)
    else
      Self.ChangeToProceeding(R, T);
  end;
end;

procedure TIdSipClientNonInviteTransaction.SendRequest;
begin
  inherited SendRequest;

  if Self.FirstTime then begin
    Self.FirstTime := false;
    Self.ChangeToTrying;
    Self.Timer.Start;
    Self.TrySendRequest(Self.InitialRequest);
  end;
end;

//* TIdSipClientNonInviteTransaction Protected methods *************************

procedure TIdSipClientNonInviteTransaction.ChangeToTrying;
begin
  Self.SetState(itsTrying);
end;

procedure TIdSipClientNonInviteTransaction.SetState(const Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

end.
