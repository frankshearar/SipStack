unit IdSipTransaction;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdRTPTimerQueue, IdSipMessage,
  IdSipTimer, IdSipTransport, IdThread, SyncObjs, SysUtils;

const
  DefaultT1    = 500;   // ms
  DefaultT1_64 = 64*DefaultT1;
  DefaultT2    = 4000;  // ms
  DefaultT4    = 5000;  // ms

const
  MaximumUDPMessageSize = 1300;
  SessionTimeoutMsg     = 'Timed out';

type
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
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  end;

  // I allow interested parties to listen for requests/responses that do not
  // match any current transactions.
  IIdSipUnhandledMessageListener = interface
    ['{0CB5037D-B9B3-4FB6-9201-80A0F10DB23A}']
    procedure OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport);
    procedure OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
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

    procedure DeliverToTransaction(Request: TIdSipRequest;
                                   Receiver: TIdSipTransport); overload;
    procedure DeliverToTransaction(Response: TIdSipResponse;
                                   Receiver: TIdSipTransport); overload;
    function  FindTransaction(R: TIdSipMessage;
                              ClientTran: Boolean): TIdSipTransaction;
    function  TransactionAt(Index: Cardinal): TIdSipTransaction;
    function  TransportAt(Index: Cardinal): TIdSipTransport;
  protected
    function  FindAppropriateTransport(Msg: TIdSipMessage): TIdSipTransport;
    procedure NotifyListenersOfUnhandledRequest(Request: TIdSipRequest;
                                                Transaction: TIdSipTransaction;
                                                Receiver: TIdSipTransport);
    procedure NotifyListenersOfUnhandledResponse(Response: TIdSipResponse;
                                                 Transaction: TIdSipTransaction;
                                                 Receiver: TIdSipTransport);

    // IIdSipTransactionListener
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport); overload;
    procedure OnTerminated(Transaction: TIdSipTransaction);

    // IIdSipTransportListener
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport); overload;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
    procedure AddTransport(Transport: TIdSipTransport);
    function  AddClientTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
    function  AddServerTransaction(InitialRequest: TIdSipRequest;
                                   Receiver: TIdSipTransport): TIdSipTransaction;
    procedure ClearTransports;
    function  LoopDetected(Request: TIdSipRequest): Boolean;
    procedure RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
    procedure RemoveUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
    procedure Send(Msg: TIdSipMessage); virtual;
    function  TransactionCount: Integer;
    function  TransportCount: Integer;
    function  WillUseReliableTranport(R: TIdSipMessage): Boolean;

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
  // TrySendRequest that it is the last call in a method as I could be dead
  // before the next line of the method is reached! This means, further, that
  // anything that triggers ChangeToTerminated had better be the final line
  // of a call stack after which nothing happens except stack clearup. So if
  // Foo calls Bar which calls Baz which calls ChangeToTerminated then
  // ChangeToTerminated is the last line of Baz, and Baz is the last line of
  // Bar, and Bar is the last line of Foo.                
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
    procedure ChangeToCompleted(R: TIdSipResponse;
                                T: TIdSipTransport); overload; virtual;
    procedure ChangeToProceeding; overload; virtual;
    procedure ChangeToProceeding(R: TIdSipRequest;
                                 T: TIdSipTransport); overload;
    procedure ChangeToProceeding(R: TIdSipResponse;
                                 T: TIdSipTransport); overload;
    procedure ChangeToTerminated(Quiet: Boolean); overload;
    procedure ChangeToTerminated(R: TIdSipResponse;
                                 T: TIdSipTransport); overload; virtual;
    procedure DoOnTimeout(Transport: TIdSipTransport;
                          Request: TIdSipRequest;
                          const Reason: String);
    procedure DoOnTransportError(Transport: TIdSipTransport;
                                 Request: TIdSipRequest;
                                 const Reason: String); overload;
    procedure DoOnTransportError(Transport: TIdSipTransport;
                                 Response: TIdSipResponse;
                                 const Reason: String); overload;
    procedure NotifyOfFailure(const Reason: String);
    procedure NotifyOfTermination;
    procedure NotifyOfRequest(R: TIdSipRequest;
                              T: TIdSipTransport);
    procedure NotifyOfResponse(R: TIdSipResponse;
                               Receiver: TIdSipTransport);
    procedure SetState(Value: TIdSipTransactionState); virtual;
    procedure TryResendInitialRequest;
    procedure TrySendRequest(R: TIdSipRequest);
    procedure TrySendResponse(R: TIdSipResponse); virtual;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher;
  public
    class function GetClientTransactionType(Request: TIdSipRequest): TIdSipTransactionClass;
    class function GetServerTransactionType(Request: TIdSipRequest): TIdSipTransactionClass;

    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); virtual;
    destructor  Destroy; override;

    procedure AddTransactionListener(const Listener: IIdSipTransactionListener);
    procedure ExceptionRaised(E: Exception);
    function  IsClient: Boolean; virtual; abstract;
    function  IsInvite: Boolean; virtual; abstract;
    function  IsNull: Boolean; virtual; abstract;
    function  IsServer: Boolean;
    function  LoopDetected(Request: TIdSipRequest): Boolean;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); virtual;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); virtual;
    procedure SendRequest; virtual;
    procedure SendResponse(R: TIdSipResponse); virtual;
    procedure RemoveTransactionListener(const Listener: IIdSipTransactionListener);

    property InitialRequest: TIdSipRequest          read fInitialRequest;
    property State:          TIdSipTransactionState read fState;
  end;

  TIdSipServerTransaction = class(TIdSipTransaction)
  protected
    LastResponseSent: TIdSipResponse;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor Destroy; override;

    function  IsClient: Boolean; override;
  end;

  TIdSipServerInviteTransaction = class;
  TIdSipServerInviteTransactionTimer = class(TObject)
  private
    fTimerGInterval:  Cardinal;
    fTimerGIsRunning: Boolean;
    fTimerHInterval:  Cardinal;
    fTimerHIsRunning: Boolean;
    fTimerIInterval:  Cardinal;
    fTimerIIsRunning: Boolean;
    Owner:            TIdSipServerInviteTransaction;
    State:            TIdSipTransactionState;
    T2:               Cardinal;
    Timer:            TIdRTPTimerQueue;

    procedure OnException(T: TIdThread;
                          E: Exception);
    procedure OnTimerG(Sender: TObject);
    procedure OnTimerH(Sender: TObject);
    procedure OnTimerI(Sender: TObject);
  public
    constructor Create(OwnerTran: TIdSipServerInviteTransaction;
                       T1: Cardinal = DefaultT1;
                       T2: Cardinal = DefaultT2;
                       T4: Cardinal = DefaultT4);
    destructor  Destroy; override;

    procedure ChangeState(NewState: TIdSipTransactionState);
    procedure FireTimerG;
    procedure StartTimerG;
    procedure StartTimerH;
    procedure StartTimerI;
    procedure StopTimerG;
    procedure StopTimerH;
    procedure StopTimerI;
    function  TimerGInterval: Cardinal;
    function  TimerGIsRunning: Boolean;
    function  TimerHInterval: Cardinal;
    function  TimerHIsRunning: Boolean;
    function  TimerIInterval: Cardinal;
    function  TimerIIsRunning: Boolean;
  end;

  TIdSipServerInviteTransaction = class(TIdSipServerTransaction)
  private
    Timer: TIdSipServerInviteTransactionTimer;

    procedure ChangeToConfirmed(R: TIdSipRequest;
                                T: TIdSipTransport);
    function  Create100Response(R: TIdSipRequest): TIdSipResponse;
    procedure TrySend100Response(R: TIdSipRequest);
    procedure TrySendLastResponse(R: TIdSipRequest);
  protected
    procedure SetState(Value: TIdSipTransactionState); override;
    procedure TrySendResponse(R: TIdSipResponse); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerG;
    procedure FireTimerH;
    procedure FireTimerI;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); override;
    procedure SendResponse(R: TIdSipResponse); override;
  end;

  TIdSipServerNonInviteTransaction = class(TIdSipServerTransaction)
  private
    TimerJ: TIdSipTimer;

    procedure ChangeToTrying;
    procedure OnTimerJ(Sender: TObject);
    procedure TrySendLastResponse(R: TIdSipRequest);
  protected
    procedure ChangeToCompleted; override;
    procedure TrySendResponse(R: TIdSipResponse); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerJ;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); override;
    procedure SendResponse(R: TIdSipResponse); override;
  end;

  TIdSipClientTransaction = class(TIdSipTransaction)
  public
    function IsClient: Boolean; override;
  end;

  TIdSipClientInviteTransaction = class;
  TIdSipClientInviteTransactionTimer = class(TObject)
  private
    fTimerAInterval:  Cardinal;
    fTimerAIsRunning: Boolean;
    fTimerBInterval:  Cardinal;
    fTimerBIsRunning: Boolean;
    fTimerDInterval:  Cardinal;
    fTimerDIsRunning: Boolean;
    Lock:             TCriticalSection;
    Owner:            TIdSipClientInviteTransaction;
    State:            TIdSipTransactionState;
    Timer:            TIdRTPTimerQueue;

    procedure OnException(T: TIdThread;
                          E: Exception);
    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
  public
    constructor Create(OwnerTran: TIdSipClientInviteTransaction;
                       T1: Cardinal = DefaultT1);
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
    function  TimerBInterval: Cardinal;
    function  TimerBIsRunning: Boolean;
    function  TimerDInterval: Cardinal;
    function  TimerDIsRunning: Boolean;
  end;

  TIdSipClientInviteTransaction = class(TIdSipClientTransaction)
  private
    Timer: TIdSipClientInviteTransactionTimer;

    procedure ChangeToCalling;
    procedure TrySendACK(R: TIdSipResponse);
  protected
    procedure ChangeToCompleted(R: TIdSipResponse;
                                T: TIdSipTransport); override;
    procedure SetState(Value: TIdSipTransactionState); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerA;
    procedure FireTimerB;
    procedure FireTimerD;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); override;
    procedure SendRequest; override;
  end;

  TIdSipClientNonInviteTransaction = class;
  TIdSipClientNonInviteTransactionTimer = class(TObject)
  private
    fTimerEInterval:  Cardinal;
    fTimerEIsRunning: Boolean;
    fTimerFInterval:  Cardinal;
    fTimerFIsRunning: Boolean;
    fTimerKInterval:  Cardinal;
    fTimerKIsRunning: Boolean;
    Lock:             TCriticalSection;
    Owner:            TIdSipClientNonInviteTransaction;
    State:            TIdSipTransactionState;
    T2:               Cardinal;
    Timer:            TIdRTPTimerQueue;

    procedure OnException(T: TIdThread;
                          E: Exception);
    procedure OnTimerE(Sender: TObject);
    procedure OnTimerF(Sender: TObject);
    procedure OnTimerK(Sender: TObject);
  public
    constructor Create(OwnerTran: TIdSipClientNonInviteTransaction;
                       T1: Cardinal = DefaultT1;
                       T2: Cardinal = DefaultT2;
                       T4: Cardinal = DefaultT4);
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
    function  TimerFInterval: Cardinal;
    function  TimerFIsRunning: Boolean;
    function  TimerKInterval: Cardinal;
    function  TimerKIsRunning: Boolean;
  end;

  TIdSipClientNonInviteTransaction = class(TIdSipClientTransaction)
  private
    Timer: TIdSipClientNonInviteTransactionTimer;
  protected
    procedure ChangeToTrying;
    procedure SetState(Value: TIdSipTransactionState); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure FireTimerE;
    procedure FireTimerF;
    procedure FireTimerK;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); override;
    procedure SendRequest; override;
  end;

  TIdSipNullTransaction = class(TIdSipTransaction)
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    function IsClient: Boolean; override;
    function IsInvite: Boolean; override;
    function IsNull: Boolean; override;
  end;

implementation

uses
  IdException, IdSipConsts, IdSipDialogID, Math;

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

procedure TIdSipTransactionDispatcher.AddTransport(Transport: TIdSipTransport);
begin
  Self.TransportLock.Acquire;
  try
    Self.Transports.Add(Transport);
    Transport.AddTransportListener(Self);
  finally
    Self.TransportLock.Release;
  end;
end;

function TIdSipTransactionDispatcher.AddClientTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
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

function TIdSipTransactionDispatcher.AddServerTransaction(InitialRequest: TIdSipRequest;
                                                          Receiver: TIdSipTransport): TIdSipTransaction;
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

function TIdSipTransactionDispatcher.LoopDetected(Request: TIdSipRequest): Boolean;
var
  I: Integer;
begin
  // In Smalltalk:
  // | tran |
  // self transactionLock critical:
  //     [^ (self transactions
  //         detect: [ :each | each loopDetected: request ]
  //         ifNone: [nil]]) notNil.
  // ^ tran notNil

  // cf. RFC 3261 section 8.2.2.2
  Result := false;

  Self.TransactionLock.Acquire;
  try
    I := 0;
    while (I < Self.Transactions.Count) and not Result do begin
      if Self.TransactionAt(I).IsServer then
        Result := Self.TransactionAt(I).LoopDetected(Request);
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

procedure TIdSipTransactionDispatcher.Send(Msg: TIdSipMessage);
var
  MsgLen:       Cardinal;
  RewrittenVia: Boolean;
begin
  MsgLen := Length(Msg.AsString);
  RewrittenVia := (MsgLen > MaximumUDPMessageSize) and (Msg.LastHop.Transport = sttUDP);

  if RewrittenVia then
    Msg.LastHop.Transport := sttTCP;

  try
    Self.FindAppropriateTransport(Msg).Send(Msg);
  except
    on EIdSipTransport do begin
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

function TIdSipTransactionDispatcher.WillUseReliableTranport(R: TIdSipMessage): Boolean;
begin
  Assert(R.Path.Length > 0, 'Messages must have at least one Via header');

  Result := R.LastHop.Transport <> sttUDP;

//  Result := Self.FindAppropriateTransport(R).IsReliable;
end;

//* TIdSipTransactionDispatcher Protected methods ******************************

function TIdSipTransactionDispatcher.FindAppropriateTransport(Msg: TIdSipMessage): TIdSipTransport;
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

procedure TIdSipTransactionDispatcher.NotifyListenersOfUnhandledRequest(Request: TIdSipRequest;
                                                                        Transaction: TIdSipTransaction;
                                                                        Receiver: TIdSipTransport);
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

procedure TIdSipTransactionDispatcher.NotifyListenersOfUnhandledResponse(Response: TIdSipResponse;
                                                                         Transaction: TIdSipTransaction;
                                                                         Receiver: TIdSipTransport);
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

procedure TIdSipTransactionDispatcher.OnFail(Transaction: TIdSipTransaction;
                                             const Reason: String);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                       Transaction: TIdSipTransaction;
                                                       Receiver: TIdSipTransport);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                        Transaction: TIdSipTransaction;
                                                        Receiver: TIdSipTransport);
begin
end;

procedure TIdSipTransactionDispatcher.OnTerminated(Transaction: TIdSipTransaction);
begin
end;

procedure TIdSipTransactionDispatcher.OnException(E: Exception;
                                                  const Reason: String);
begin
end;                                                  

procedure TIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport);
begin
  Self.DeliverToTransaction(Request, Receiver);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport);
begin
  Self.DeliverToTransaction(Response, Receiver);
end;

procedure TIdSipTransactionDispatcher.OnRejectedMessage(const Msg: String;
                                                        const Reason: String);
begin
end;

//* TIdSipTransactionDispatcher Private methods ********************************

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Request: TIdSipRequest;
                                                           Receiver: TIdSipTransport);
var
  NullTran: TIdSipTransaction;
  Tran:     TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Request, false);

  if Assigned(Tran) then
    Tran.ReceiveRequest(Request, Receiver)
  else begin
    if Request.IsAck then begin
      NullTran := TIdSipNullTransaction.Create(Self, Request);
      try
        Self.NotifyListenersOfUnhandledRequest(Request, NullTran, Receiver);
      finally
        NullTran.Free;
      end;
    end
    else begin
      Tran := Self.AddServerTransaction(Request, Receiver);

      Self.NotifyListenersOfUnhandledRequest(Request, Tran, Receiver);
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Response: TIdSipResponse;
                                                           Receiver: TIdSipTransport);
var
  NullTran: TIdSipTransaction;
  Tran:     TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Response, true);

  if Assigned(Tran) then
    Tran.ReceiveResponse(Response, Receiver)
  else begin
    NullTran := TIdSipNullTransaction.Create(Self, nil);
    try
      Self.NotifyListenersOfUnhandledResponse(Response, NullTran, Receiver);
    finally
      NullTran.Free;
    end;
  end;
end;

function TIdSipTransactionDispatcher.FindTransaction(R: TIdSipMessage;
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

function TIdSipTransactionDispatcher.TransactionAt(Index: Cardinal): TIdSipTransaction;
begin
  Result := Self.Transactions[Index] as TIdSipTransaction;
end;

function TIdSipTransactionDispatcher.TransportAt(Index: Cardinal): TIdSipTransport;
begin
  Result := Self.Transports[Index] as TIdSipTransport;
end;

//******************************************************************************
//* TIdSipTransaction                                                          *
//******************************************************************************
//* TIdSipTransaction Public methods *******************************************

class function TIdSipTransaction.GetClientTransactionType(Request: TIdSipRequest): TIdSipTransactionClass;
begin
  if Request.IsInvite then
    Result := TIdSipClientInviteTransaction
  else
    Result := TIdSipClientNonInviteTransaction;
end;

class function TIdSipTransaction.GetServerTransactionType(Request: TIdSipRequest): TIdSipTransactionClass;
begin
  if Request.IsInvite then
    Result := TIdSipServerInviteTransaction
  else
    Result := TIdSipServerNonInviteTransaction;
end;

constructor TIdSipTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                     InitialRequest: TIdSipRequest);
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

procedure TIdSipTransaction.ExceptionRaised(E: Exception);
begin
  Self.NotifyOfFailure(Self.ClassName + 'raised an '
                     + E.ClassName + ': ' + E.Message);
end;

function TIdSipTransaction.IsServer: Boolean;
begin
  Result := not Self.IsClient;
end;

function TIdSipTransaction.LoopDetected(Request: TIdSipRequest): Boolean;
begin
  Result := Request.From.IsEqualTo(Self.InitialRequest.From)
    and (Request.CallID = Self.InitialRequest.CallID)
    and (Request.CSeq.IsEqualTo(Self.InitialRequest.CSeq))
    and not Request.Match(Self.InitialRequest);
end;

procedure TIdSipTransaction.ReceiveRequest(R: TIdSipRequest;
                                           T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.ReceiveResponse(R: TIdSipResponse;
                                            T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.SendRequest;
begin
end;

procedure TIdSipTransaction.SendResponse(R: TIdSipResponse);
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

procedure TIdSipTransaction.ChangeToCompleted(R: TIdSipResponse;
                                              T: TIdSipTransport);
begin
  Self.ChangeToCompleted;

  Self.NotifyOfResponse(R, T);
end;

procedure TIdSipTransaction.ChangeToProceeding;
begin
  Self.SetState(itsProceeding);
end;

procedure TIdSipTransaction.ChangeToProceeding(R: TIdSipRequest;
                                               T: TIdSipTransport);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfRequest(R, T);
end;

procedure TIdSipTransaction.ChangeToProceeding(R: TIdSipResponse;
                                               T: TIdSipTransport);
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

procedure TIdSipTransaction.ChangeToTerminated(R: TIdSipResponse;
                                               T: TIdSipTransport);
begin
  Self.NotifyOfResponse(R, T);
  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.DoOnTimeout(Transport: TIdSipTransport;
                                        Request: TIdSipRequest;
                                        const Reason: String);
var
  Timeout: TIdSipResponse;
begin
  Self.NotifyOfFailure(Reason);

  Timeout := TIdSipResponse.InResponseTo(Request, SIPRequestTimeout);
  try
    Self.NotifyOfResponse(Timeout, Transport);
  finally
    Timeout.Free;
  end;

  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.DoOnTransportError(Transport: TIdSipTransport;
                                               Request: TIdSipRequest;
                                               const Reason: String);
var
  Error: TIdSipResponse;
begin
  Self.NotifyOfFailure(Reason);

  Error := TIdSipResponse.InResponseTo(Request, SIPServiceUnavailable);
  try
    Self.NotifyOfResponse(Error, Transport);
  finally
    Error.Free;
  end;

  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.DoOnTransportError(Transport: TIdSipTransport;
                                               Response: TIdSipResponse;
                                               const Reason: String);
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

procedure TIdSipTransaction.NotifyOfRequest(R: TIdSipRequest;
                                            T: TIdSipTransport);
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

procedure TIdSipTransaction.NotifyOfResponse(R: TIdSipResponse;
                                             Receiver: TIdSipTransport);
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

procedure TIdSipTransaction.SetState(Value: TIdSipTransactionState);
begin
  fState := Value;
end;

procedure TIdSipTransaction.TryResendInitialRequest;
begin
  if not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendRequest(Self.InitialRequest);
end;

procedure TIdSipTransaction.TrySendRequest(R: TIdSipRequest);
var
  CopyOfRequest: TIdSipRequest;
begin
  CopyOfRequest := TIdSipRequest.Create;
  try
    CopyOfRequest.Assign(R);
    try
      Self.Dispatcher.Send(CopyOfRequest);
    except
      on E: EIdSipTransport do
        Self.DoOnTransportError(E.Transport,
                                E.SipMessage as TIdSipRequest,
                                E.Message);
    end;
  finally
    CopyOfRequest.Free;
  end;
end;

procedure TIdSipTransaction.TrySendResponse(R: TIdSipResponse);
var
  CopyOfResponse: TIdSipResponse;
begin
  CopyOfResponse := TIdSipResponse.Create;
  try
    CopyOfResponse.Assign(R);
    try
      Self.Dispatcher.Send(CopyOfResponse);
    except
      on E: EIdSipTransport do
        Self.DoOnTransportError(E.Transport,
                                E.SipMessage as TIdSipResponse,
                                E.Message);
    end;
  finally
    CopyOfResponse.Free;
  end;
end;

//******************************************************************************
//* TIdSipServerTransaction                                                    *
//******************************************************************************
//* TIdSipServerTransaction Public methods *************************************

constructor TIdSipServerTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                           InitialRequest: TIdSipRequest);
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

constructor TIdSipServerInviteTransactionTimer.Create(OwnerTran: TIdSipServerInviteTransaction;
                                                      T1: Cardinal = DefaultT1;
                                                      T2: Cardinal = DefaultT2;
                                                      T4: Cardinal = DefaultT4);
begin
  inherited Create;
  Self.Owner := OwnerTran;

  Self.Timer := TIdRTPTimerQueue.Create(false);
  Self.Timer.OnException := Self.OnException;

  Self.fTimerGInterval := T1;
  Self.fTimerHInterval := 64*T1;
  Self.fTimerIInterval := T4;

  Self.T2 := T2
end;

destructor TIdSipServerInviteTransactionTimer.Destroy;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

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
  if (Self.fTimerGInterval < T2) then
    Self.fTimerGInterval := 2*Self.fTimerGInterval
  else
    Self.fTimerGInterval := T2;

  Self.Owner.FireTimerG;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerG;
begin
  Self.Timer.AddEvent(Self.TimerGInterval, Self.OnTimerG);
  Self.fTimerGIsRunning := true;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerH;
begin
  Self.Timer.AddEvent(Self.TimerHInterval, Self.OnTimerH);
  Self.fTimerHIsRunning := true;
end;

procedure TIdSipServerInviteTransactionTimer.StartTimerI;
begin
  Self.Timer.AddEvent(Self.TimerIInterval, Self.OnTimerI);
  Self.fTimerIIsRunning := true;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerG;
begin
  Self.fTimerGIsRunning := false;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerH;
begin
  Self.fTimerHIsRunning := false;
end;

procedure TIdSipServerInviteTransactionTimer.StopTimerI;
begin
  Self.fTimerIIsRunning := false;
end;

function TIdSipServerInviteTransactionTimer.TimerGInterval: Cardinal;
begin
  Result := Self.fTimerGInterval;
end;

function TIdSipServerInviteTransactionTimer.TimerGIsRunning: Boolean;
begin
  Result := Self.fTimerGIsRunning;
end;

function TIdSipServerInviteTransactionTimer.TimerHInterval: Cardinal;
begin
  Result := Self.fTimerHInterval;
end;

function TIdSipServerInviteTransactionTimer.TimerHIsRunning: Boolean;
begin
  Result := Self.fTimerHIsRunning;
end;

function TIdSipServerInviteTransactionTimer.TimerIInterval: Cardinal;
begin
  Result := Self.fTimerIInterval;
end;

function TIdSipServerInviteTransactionTimer.TimerIIsRunning: Boolean;
begin
  Result := Self.fTimerIIsRunning;
end;

//* TIdSipServerInviteTransactionTimer Private methods *************************

procedure TIdSipServerInviteTransactionTimer.OnException(T: TIdThread;
                                                         E: Exception);
begin
  Self.Owner.ExceptionRaised(E);
end;

procedure TIdSipServerInviteTransactionTimer.OnTimerG(Sender: TObject);
begin
  if Self.TimerGIsRunning then begin
    Self.FireTimerG;
    Self.Timer.AddEvent(Self.TimerGInterval, Self.OnTimerG);
  end;
end;

procedure TIdSipServerInviteTransactionTimer.OnTimerH(Sender: TObject);
begin
  if Self.TimerHIsRunning then begin
    Self.Owner.FireTimerH;
    Self.Timer.AddEvent(Self.TimerHInterval, Self.OnTimerH);
  end;
end;

procedure TIdSipServerInviteTransactionTimer.OnTimerI(Sender: TObject);
begin
  if Self.TimerIIsRunning then begin
    Self.Owner.FireTimerI;
    Self.Timer.AddEvent(Self.TimerIInterval, Self.OnTimerI);
  end;
end;

//******************************************************************************
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
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
  Self.DoOnTimeout(Self.Dispatcher.FindAppropriateTransport(Self.InitialRequest),
                   Self.InitialRequest,
                   SessionTimeoutMsg);
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

procedure TIdSipServerInviteTransaction.ReceiveRequest(R: TIdSipRequest;
                                                       T: TIdSipTransport);
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

procedure TIdSipServerInviteTransaction.SendResponse(R: TIdSipResponse);
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

procedure TIdSipServerInviteTransaction.SetState(Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(R: TIdSipRequest;
                                                          T: TIdSipTransport);
begin
  Self.SetState(itsConfirmed);
end;

function TIdSipServerInviteTransaction.Create100Response(R: TIdSipRequest): TIdSipResponse;
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

procedure TIdSipServerInviteTransaction.TrySend100Response(R: TIdSipRequest);
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

procedure TIdSipServerInviteTransaction.TrySendLastResponse(R: TIdSipRequest);
begin
  Self.TrySendResponse(Self.LastResponseSent);
end;

procedure TIdSipServerInviteTransaction.TrySendResponse(R: TIdSipResponse);
begin
  if (not R.IsEqualTo(Self.LastResponseSent)) then
    Self.LastResponseSent.Assign(R);

  inherited TrySendResponse(R);
end;

//******************************************************************************
//* TIdSipServerNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipServerNonInviteTransaction Public methods ****************************

constructor TIdSipServerNonInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                    InitialRequest: TIdSipRequest);
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

procedure TIdSipServerNonInviteTransaction.ReceiveRequest(R: TIdSipRequest;
                                                          T: TIdSipTransport);
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

procedure TIdSipServerNonInviteTransaction.SendResponse(R: TIdSipResponse);
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

procedure TIdSipServerNonInviteTransaction.TrySendResponse(R: TIdSipResponse);
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

procedure TIdSipServerNonInviteTransaction.TrySendLastResponse(R: TIdSipRequest);
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

constructor TIdSipClientInviteTransactionTimer.Create(OwnerTran: TIdSipClientInviteTransaction;
                                                      T1: Cardinal = DefaultT1);
begin
  inherited Create;
  Self.Lock := TCriticalSection.Create;
  Self.Owner := OwnerTran;

  Self.Timer := TIdRTPTimerQueue.Create(false);
  Self.Timer.OnException := Self.OnException;

  Self.fTimerAInterval := T1;
  Self.fTimerBInterval := 64*T1;
  Self.fTimerDInterval := 64*T1;
end;

destructor TIdSipClientInviteTransactionTimer.Destroy;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

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
    Self.fTimerAInterval := 2*Self.fTimerAInterval;
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
  Self.Timer.AddEvent(Self.TimerAInterval, Self.OnTimerA);
  Self.fTimerAIsRunning := true;
end;

procedure TIdSipClientInviteTransactionTimer.StartTimerB;
begin
  Self.Timer.AddEvent(Self.TimerBInterval, Self.OnTimerB);
  Self.fTimerBIsRunning := true;
end;

procedure TIdSipClientInviteTransactionTimer.StartTimerD;
begin
  Self.Timer.AddEvent(Self.TimerDInterval, Self.OnTimerD);
  Self.fTimerDIsRunning := true;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerA;
begin
  Self.fTimerAIsRunning := false;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerB;
begin
  Self.fTimerBIsRunning := false;
end;

procedure TIdSipClientInviteTransactionTimer.StopTimerD;
begin
  Self.fTimerDIsRunning := false;
end;

function TIdSipClientInviteTransactionTimer.TimerAInterval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.fTimerAInterval;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipClientInviteTransactionTimer.TimerAIsRunning: Boolean;
begin
  Result := Self.fTimerAIsRunning;
end;

function TIdSipClientInviteTransactionTimer.TimerBInterval: Cardinal;
begin
  Result := Self.fTimerBInterval;
end;

function TIdSipClientInviteTransactionTimer.TimerBIsRunning: Boolean;
begin
  Result := Self.fTimerBIsRunning;
end;

function TIdSipClientInviteTransactionTimer.TimerDInterval: Cardinal;
begin
  Result := Self.fTimerDInterval;
end;

function TIdSipClientInviteTransactionTimer.TimerDIsRunning: Boolean;
begin
  Result := Self.fTimerDIsRunning;
end;

//* TIdSipClientInviteTransactionTimer Private methods *************************

procedure TIdSipClientInviteTransactionTimer.OnException(T: TIdThread;
                                                          E: Exception);
begin
  Self.Owner.ExceptionRaised(E);
end;

procedure TIdSipClientInviteTransactionTimer.OnTimerA(Sender: TObject);
begin
  if Self.TimerAIsRunning then begin
    Self.FireTimerA;
    Self.Timer.AddEvent(Self.TimerAInterval, Self.OnTimerA);
  end;
end;

procedure TIdSipClientInviteTransactionTimer.OnTimerB(Sender: TObject);
begin
  if Self.TimerBIsRunning then begin
    Self.StopTimerB;
    Owner.FireTimerB;
  end;
end;

procedure TIdSipClientInviteTransactionTimer.OnTimerD(Sender: TObject);
begin
  if Self.TimerDIsRunning then begin
    Self.StopTimerD;
    Owner.FireTimerD;

    Self.StopTimerA;
    Self.StopTimerB;
  end;
end;

//******************************************************************************
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
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

procedure TIdSipClientInviteTransaction.FireTimerA;
begin
  if (Self.State = itsCalling) then
    Self.TryResendInitialRequest;
end;

procedure TIdSipClientInviteTransaction.FireTimerB;
begin
  if (Self.State = itsCalling) then
    Self.DoOnTimeout(Self.Dispatcher.FindAppropriateTransport(Self.InitialRequest),
                     Self.InitialRequest,
                     SessionTimeoutMsg);
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

procedure TIdSipClientInviteTransaction.ReceiveResponse(R: TIdSipResponse;
                                                        T: TIdSipTransport);
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

procedure TIdSipClientInviteTransaction.ChangeToCompleted(R: TIdSipResponse;
                                                          T: TIdSipTransport);
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

procedure TIdSipClientInviteTransaction.SetState(Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

procedure TIdSipClientInviteTransaction.TrySendACK(R: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.InitialRequest.AckFor(R);
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

constructor TIdSipClientNonInviteTransactionTimer.Create(OwnerTran: TIdSipClientNonInviteTransaction;
                                                         T1: Cardinal = DefaultT1;
                                                         T2: Cardinal = DefaultT2;
                                                         T4: Cardinal = DefaultT4);
begin
  inherited Create;
  Self.Lock := TCriticalSection.Create;
  Self.Owner := OwnerTran;

  Self.Timer := TIdRTPTimerQueue.Create(false);
  Self.Timer.OnException := Self.OnException;

  Self.fTimerEInterval := T1;
  Self.fTimerFInterval := 64*T1;
  Self.fTimerKInterval := T4;

  Self.T2 := T2;
end;

destructor TIdSipClientNonInviteTransactionTimer.Destroy;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;
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
      if (Self.fTimerEInterval < Self.T2) then
        Self.fTimerEInterval := 2*Self.fTimerEInterval
      else
        Self.fTimerEInterval := Self.T2;
    end
    else if (Self.State = itsProceeding) then begin
      Self.fTimerEInterval := Self.T2;
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
    Self.fTimerEInterval := NewInterval;
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
  Self.Timer.AddEvent(Self.TimerEInterval, Self.OnTimerE);
  Self.fTimerEIsRunning := true;
end;

procedure TIdSipClientNonInviteTransactionTimer.StartTimerF;
begin
  Self.Timer.AddEvent(Self.TimerFInterval, Self.OnTimerF);
  Self.fTimerFIsRunning := true;
end;

procedure TIdSipClientNonInviteTransactionTimer.StartTimerK;
begin
  Self.Timer.AddEvent(Self.TimerKInterval, Self.OnTimerK);
  Self.fTimerKIsRunning := true;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerE;
begin
  Self.fTimerEIsRunning := false;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerF;
begin
  Self.fTimerFIsRunning := false;
end;

procedure TIdSipClientNonInviteTransactionTimer.StopTimerK;
begin
  Self.fTimerKIsRunning := false;
end;

function TIdSipClientNonInviteTransactionTimer.TimerEInterval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.fTimerEInterval;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipClientNonInviteTransactionTimer.TimerEIsRunning: Boolean;
begin
  Result := Self.fTimerEIsRunning;
end;

function TIdSipClientNonInviteTransactionTimer.TimerFInterval: Cardinal;
begin
  Result := Self.fTimerFInterval;
end;

function TIdSipClientNonInviteTransactionTimer.TimerFIsRunning: Boolean;
begin
  Result := Self.fTimerFIsRunning;
end;

function TIdSipClientNonInviteTransactionTimer.TimerKInterval: Cardinal;
begin
  Result := Self.fTimerKInterval;
end;

function TIdSipClientNonInviteTransactionTimer.TimerKIsRunning: Boolean;
begin
  Result := Self.fTimerKIsRunning;
end;

//* TIdSipClientNonInviteTransactionTimer Private methods **********************

procedure TIdSipClientNonInviteTransactionTimer.OnException(T: TIdThread;
                                                            E: Exception);
begin
  Self.Owner.ExceptionRaised(E);
end;

procedure TIdSipClientNonInviteTransactionTimer.OnTimerE(Sender: TObject);
begin
  if Self.TimerEIsRunning then begin
    Self.FireTimerE;
    Self.Timer.AddEvent(Self.TimerEInterval, Self.OnTimerE);
  end;
end;

procedure TIdSipClientNonInviteTransactionTimer.OnTimerF(Sender: TObject);
begin
  if Self.TimerEIsRunning then begin
    Self.Owner.FireTimerF;
    Self.Timer.AddEvent(Self.TimerEInterval, Self.OnTimerE);
  end;
end;

procedure TIdSipClientNonInviteTransactionTimer.OnTimerK(Sender: TObject);
begin
  if Self.TimerKIsRunning then begin
    Self.Owner.FireTimerK;
    Self.Timer.AddEvent(Self.TimerKInterval, Self.OnTimerK);
  end;
end;

//******************************************************************************
//* TIdSipClientNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipClientNonInviteTransaction Public methods ****************************

constructor TIdSipClientNonInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                    InitialRequest: TIdSipRequest);
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
  Self.DoOnTimeout(Self.Dispatcher.FindAppropriateTransport(Self.InitialRequest),
                   Self.InitialRequest,
                   SessionTimeoutMsg);
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

procedure TIdSipClientNonInviteTransaction.ReceiveResponse(R: TIdSipResponse;
                                                           T: TIdSipTransport);
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

procedure TIdSipClientNonInviteTransaction.SetState(Value: TIdSipTransactionState);
begin
  inherited SetState(Value);
  Self.Timer.ChangeState(Value);
end;

//******************************************************************************
//* TIdSipNullTransaction                                                      *
//******************************************************************************
//* TIdSipNullTransaction Public methods ***************************************

constructor TIdSipNullTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                         InitialRequest: TIdSipRequest);
var
  NothingRequest: TIdSipRequest;
begin
  // Ignore InitialRequest. We treat Null Transactions as a special case.
  NothingRequest := TIdSipRequest.Create;
  try
    inherited Create(Dispatcher, NothingRequest);
  finally
    NothingRequest.Free;
  end;
end;

function TIdSipNullTransaction.IsClient: Boolean;
begin
  Result := false;
end;

function TIdSipNullTransaction.IsInvite: Boolean;
begin
  Result := false;
end;

function TIdSipNullTransaction.IsNull: Boolean;
begin
  Result := true;
end;

end.
