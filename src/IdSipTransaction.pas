unit IdSipTransaction;

interface

uses
  Classes, Contnrs, IdSipInterfacedObject, IdSipMessage, IdSipTimer,
  IdSipTransport, SyncObjs;

const
  InitialT1     = 500;   // ms
  InitialT1_64  = 64*InitialT1;
  TimerDTimeout = 32000; // ms
  T2            = 4000;  // ms
  T4            = 5000;  // ms

const
  SessionTimeoutMsg = 'Timed out';

type
  TIdSipFailEvent = procedure(Sender: TObject; const Reason: String) of object;
  // This covers all states - INVITE, non-INVITE, client, server.
  TIdSipTransactionState = (itsCalling, itsCompleted, itsConfirmed,
                            itsProceeding, itsTerminated, itsTrying);

  TIdSipTransaction = class;
  TIdSipTransactionClass = class of TIdSipTransaction;

  // OnTerminated is a signal to the Listener that Transaction has terminated,
  // and the Listener must remove any references to Transaction - after this
  // notification the transaction is dead, gone, finished and you're asking
  // for an access violation if you don't remove the reference.
  IIdSipTransactionListener = interface
    ['{77B97FA0-7073-40BC-B3F0-7E53ED02213F}']
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Transport: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
  end;

  // I am the single connection point between the transport layer and the
  // transaction layer. It is my responsibility to manage transactions, dispatch
  // messages to the appropriate transaction or fire events on messages that
  // don't match any transactions.
  //
  // I do not manage any transports that may be given to me.
  TIdSipTransactionDispatcher = class(TIdSipMessageSubject,
                                      IIdSipTransactionListener,
                                      IIdSipTransportListener)
  private
    fOnTransactionFail: TIdSipFailEvent;
    Transports:         TObjectList;
    TransportLock:      TCriticalSection;
    Transactions:       TObjectList;
    TransactionLock:    TCriticalSection;

    function  AddTransaction(const TransactionType: TIdSipTransactionClass;
                             const InitialRequest: TIdSipRequest): TIdSipTransaction;
    procedure CheckMessage(const Msg: TIdSipMessage);
    procedure DeliverToTransaction(const Msg: TIdSipMessage; const T: TIdSipTransport);
    function  FindTransaction(const R: TIdSipMessage): TIdSipTransaction;
    function  TransactionAt(const Index: Cardinal): TIdSipTransaction;
    function  TransportAt(const Index: Cardinal): TIdSipTransport;
  protected
    procedure DoOnTransactionFail(Sender: TObject; const Reason: String);
    function  FindAppropriateTransport(const Msg: TIdSipMessage): TIdSipTransport;
    procedure OnTransportRequest(Sender: TObject; const R: TIdSipRequest);
    procedure OnTransportResponse(Sender: TObject; const R: TIdSipResponse);

    // IIdSipTransactionListener
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction); overload;
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Transport: TIdSipTransport); overload;
    procedure OnTerminated(const Transaction: TIdSipTransaction);

    // IIdSipTransportListener
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport); overload;
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport); overload;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddTransport(const Transport: TIdSipTransport);
    function  AddClientTransaction(const InitialRequest: TIdSipRequest): TIdSipTransaction;
    function  AddServerTransaction(const InitialRequest: TIdSipRequest): TIdSipTransaction;
    procedure ClearTransports;
    procedure DoOnUnhandledRequest(const R: TIdSipRequest);
    procedure DoOnUnhandledResponse(const R: TIdSipResponse);
    function  LoopDetected(const Request: TIdSipRequest): Boolean;
    procedure RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
    procedure SendToTransaction(const Msg: TIdSipMessage);
    procedure Send(const Msg: TIdSipMessage); virtual;
    function  TransactionCount: Integer;
    function  TransportCount: Integer;
    function  WillUseReliableTranport(const R: TIdSipMessage): Boolean;

    property OnTransactionFail: TIdSipFailEvent read fOnTransactionFail write fOnTransactionFail;
  end;

  // I am a SIP Transaction. As such, I am a finite state machine. I swallow
  // inappropriate messages, and inform my Dispatcher of interesting events.
  // These include the establishment of a new dialog, or my termination.
  TIdSipTransaction = class(TIdSipInterfacedObject)
  private
    fInitialRequest:  TIdSipRequest;
    fState:           TIdSipTransactionState;
    fDispatcher:      TIdSipTransactionDispatcher;
    TranListenerLock: TCriticalSection;
    TranListeners:    TList;
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const Transport: TIdSipTransport); virtual;
    procedure ChangeToProceeding; overload;
    procedure ChangeToProceeding(const R: TIdSipRequest); overload; virtual;
    procedure ChangeToProceeding(const R: TIdSipResponse;
                                 const T: TIdSipTransport); overload; virtual;
    procedure ChangeToTerminated(const R: TIdSipRequest); overload;
    procedure ChangeToTerminated(const R: TIdSipResponse); overload;
    procedure DoOnFail(const Reason: String); virtual;
    procedure NotifyOfFailure(const Reason: String);
    procedure NotifyOfTermination;
    procedure NotifyOfRequest(const R: TIdSipRequest);
    procedure NotifyOfResponse(const R: TIdSipResponse;
                               const Transport: TIdSipTransport);
    procedure SetState(const Value: TIdSipTransactionState);
    procedure TryResendInitialRequest;
    procedure TrySendRequest(const R: TIdSipRequest);
    procedure TrySendResponse(const R: TIdSipResponse); virtual;

    property InitialRequest: TIdSipRequest               read fInitialRequest;
    property Dispatcher:     TIdSipTransactionDispatcher read fDispatcher;
  public
    class function GetClientTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;
    class function GetServerTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddTransactionListener(const Listener: IIdSipTransactionListener);
    procedure HandleRequest(const R: TIdSipRequest;
                            const T: TIdSipTransport); virtual;
    procedure HandleResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); virtual;
    procedure Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64); virtual;
    function  IsClient: Boolean; virtual; abstract;
    procedure RemoveTransactionListener(const Listener: IIdSipTransactionListener);

    property State: TIdSipTransactionState read fState;
  end;

  TIdSipClientInviteTransaction = class(TIdSipTransaction)
  private
    fTimeout: Cardinal;
    TimerA:   TIdSipTimer;
    TimerB:   TIdSipTimer;
    TimerD:   TIdSipTimer;

    procedure ChangeToCalling;
    function  CreateACK(const R: TIdSipResponse): TIdSipRequest;
    procedure OnTimerA(Sender: TObject);
    procedure OnTimerB(Sender: TObject);
    procedure OnTimerD(Sender: TObject);
    procedure TrySendACK(const R: TIdSipResponse);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const Transport: TIdSipTransport); override;
    procedure ChangeToProceeding(const R: TIdSipResponse;
                                 const T: TIdSipTransport); override;
    procedure EstablishDialog(const R: TIdSipResponse;
                              const T: TIdSipTransport);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure HandleResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); override;
    procedure Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64); override;
    function IsClient: Boolean; override;

    property Timeout: Cardinal read fTimeout write fTimeout;
  end;

  TIdSipServerInviteTransaction = class(TIdSipTransaction)
  private
    LastResponseSent: TIdSipResponse;
    TimerG:           TIdSipTimer;
    TimerGHasFired:   Boolean;
    TimerH:           TIdSipTimer;
    TimerI:           TIdSipTimer;

    procedure ChangeToConfirmed(const R: TIdSipRequest);
    function  Create100Response(const R: TIdSipRequest): TIdSipResponse;
    procedure EstablishDialog(const R: TIdSipResponse;
                              const T: TIdSipTransport);
    procedure OnTimerG(Sender: TObject);
    procedure OnTimerH(Sender: TObject);
    procedure OnTimerI(Sender: TObject);
    procedure TrySend100Response(const R: TIdSipRequest);
    procedure TrySendLastResponse(const R: TIdSipRequest);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const Transport: TIdSipTransport); override;
    procedure ChangeToProceeding(const R: TIdSipRequest); overload; override;
    procedure TrySendResponse(const R: TIdSipResponse); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure HandleRequest(const R: TIdSipRequest;
                            const T: TIdSipTransport); override;
    procedure HandleResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); override;
    procedure Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64); override;
    function IsClient: Boolean; override;
  end;

  TIdSipClientNonInviteTransaction = class(TIdSipTransaction)
  private
    TimerE: TIdSipTimer;
    TimerF: TIdSipTimer;
    TimerK: TIdSipTimer;

    procedure OnTimerE(Sender: TObject);
    procedure OnTimerF(Sender: TObject);
    procedure OnTimerK(Sender: TObject);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const Transport: TIdSipTransport); override;
    procedure ChangeToProceeding(const R: TIdSipResponse;
                                 const T: TIdSipTransport); override;
    procedure ChangeToTrying;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure HandleResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); override;
    procedure Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64); override;
    function IsClient: Boolean; override;
  end;

  TIdSipServerNonInviteTransaction = class(TIdSipTransaction)
  private
    LastResponseSent: TIdSipResponse;
    TimerJ:                 TIdSipTimer;

    procedure ChangeToTrying(const R: TIdSipRequest);
    procedure OnTimerJ(Sender: TObject);
    procedure TrySendLastResponse(const R: TIdSipRequest);
  protected
    procedure ChangeToCompleted(const R: TIdSipResponse;
                                const T: TIdSipTransport); override;
    procedure ChangeToProceeding(const R: TIdSipResponse;
                                 const T: TIdSipTransport); override;
    procedure TrySendResponse(const R: TIdSipResponse); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure HandleRequest(const R: TIdSipRequest;
                            const T: TIdSipTransport); override;
    procedure HandleResponse(const R: TIdSipResponse;
                             const T: TIdSipTransport); override;
    procedure Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                         const InitialRequest: TIdSipRequest;
                         const Timeout:        Cardinal = InitialT1_64); override;
    function IsClient: Boolean; override;
  end;

  // I deliver messages to transactions. My sole reason for existence is to
  // prevent typecasting in methods like
  // TIdSipTransactionDispatcher.DeliverToTransaction. Therefore I'm a bit of
  // an experiment, in seeing how far we can push this Visitor business.
  TIdSipTransactionDeliverer = class(TIdSipInterfacedObject, IIdSipMessageVisitor)
  private
    Dispatcher:  TIdSipTransactionDispatcher;
    Transaction: TIdSipTransaction;
    Transport:   TIdSipTransport;
  public
    constructor Create(const Tran: TIdSipTransaction;
                       const T: TIdSipTransport;
                       const D: TIdSipTransactionDispatcher);

    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
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

  Self.Transports   := TObjectList.Create(false);
  Self.Transactions := TObjectList.Create(true);

  Self.TransportLock   := TCriticalSection.Create;
  Self.TransactionLock := TCriticalSection.Create;
end;

destructor TIdSipTransactionDispatcher.Destroy;
begin
  Self.TransactionLock.Free;
  Self.TransportLock.Free;
  Self.Transactions.Free;
  Self.Transports.Free;

  inherited Destroy;
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
  Result := Self.AddTransaction(TIdSipTransaction.GetClientTransactionType(InitialRequest), InitialRequest);
end;

function TIdSipTransactionDispatcher.AddServerTransaction(const InitialRequest: TIdSipRequest): TIdSipTransaction;
begin
  Result := Self.AddTransaction(TIdSipTransaction.GetServerTransactionType(InitialRequest), InitialRequest);
end;

procedure TIdSipTransactionDispatcher.ClearTransports;
begin
  Self.Transports.Clear;
end;

procedure TIdSipTransactionDispatcher.DoOnUnhandledRequest(const R: TIdSipRequest);
begin
  Self.NotifyMessageListeners(R);
end;

procedure TIdSipTransactionDispatcher.DoOnUnhandledResponse(const R: TIdSipResponse);
begin
  Self.NotifyMessageListeners(R);
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

procedure TIdSipTransactionDispatcher.SendToTransaction(const Msg: TIdSipMessage);
var
  Deliverator: TIdSipTransactionDeliverer;
  Tran:        TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Msg);

  Deliverator := TIdSipTransactionDeliverer.Create(Tran,
                                                   Self.FindAppropriateTransport(Msg),
                                                   Self);
  try
    Msg.Accept(Deliverator);
  finally
    Deliverator.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.Send(const Msg: TIdSipMessage);
var
  MsgLen: Cardinal;
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

procedure TIdSipTransactionDispatcher.DoOnTransactionFail(Sender: TObject; const Reason: String);
begin
  if Assigned(Self.OnTransactionFail) then
    Self.OnTransactionFail(Self, Reason);
end;

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

procedure TIdSipTransactionDispatcher.OnTransportRequest(Sender: TObject; const R: TIdSipRequest);
begin
end;

procedure TIdSipTransactionDispatcher.OnTransportResponse(Sender: TObject; const R: TIdSipResponse);
begin
end;

procedure TIdSipTransactionDispatcher.OnFail(const Transaction: TIdSipTransaction;
                                             const Reason: String);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(const Request: TIdSipRequest;
                                                       const Transaction: TIdSipTransaction);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(const Response: TIdSipResponse;
                                                        const Transaction: TIdSipTransaction;
                                                        const Transport: TIdSipTransport);
begin
end;

procedure TIdSipTransactionDispatcher.OnTerminated(const Transaction: TIdSipTransaction);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(const Request: TIdSipRequest;
                                                       const Transport: TIdSipTransport);
begin
  Self.CheckMessage(Request);
  Self.DeliverToTransaction(Request, Transport);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(const Response: TIdSipResponse;
                                                        const Transport: TIdSipTransport);
begin
  Self.CheckMessage(Response);
  Self.DeliverToTransaction(Response, Transport);
end;

//* TIdSipTransactionDispatcher Private methods ********************************

function TIdSipTransactionDispatcher.AddTransaction(const TransactionType: TIdSipTransactionClass;
                                                    const InitialRequest: TIdSipRequest): TIdSipTransaction;
var
  Index: Integer;
begin
  Result := nil;

  Self.TransactionLock.Acquire;
  try
    try
      Index := Self.Transactions.Add(TransactionType.Create);
      Result := Self.TransactionAt(Index);
      Result.AddTransactionListener(Self);

      Result.Initialise(Self, InitialRequest);
    except
      Self.Transactions.Remove(Result);

      raise;
    end;
  finally
    Self.TransactionLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.CheckMessage(const Msg: TIdSipMessage);
begin
  // Transport-layer-wide checks
  if (Msg.SIPVersion <> SipVersion) then
    //
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(const Msg: TIdSipMessage;
                                                           const T: TIdSipTransport);
var
  Deliverator: TIdSipTransactionDeliverer;
  Tran:        TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Msg);

  Deliverator := TIdSipTransactionDeliverer.Create(Tran, T, Self);
  try
    Msg.Accept(Deliverator);
  finally
    Deliverator.Free;
  end;
end;

function TIdSipTransactionDispatcher.FindTransaction(const R: TIdSipMessage): TIdSipTransaction;
var
  I: Integer;
begin
  Result := nil;

  Self.TransactionLock.Acquire;
  try
    I := 0;
    while (I < Self.Transactions.Count) and (Result = nil) do
      if Self.TransactionAt(I).InitialRequest.Match(R) then
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
  if (Request.IsInvite) then
    Result := TIdSipClientInviteTransaction
  else
    Result := TIdSipClientNonInviteTransaction;
end;

class function TIdSipTransaction.GetServerTransactionType(const Request: TIdSipRequest): TIdSipTransactionClass;
begin
  if (Request.IsInvite) then
    Result := TIdSipServerInviteTransaction
  else
    Result := TIdSipServerNonInviteTransaction;
end;

constructor TIdSipTransaction.Create;
begin
  inherited Create;

  Self.fInitialRequest  := TIdSipRequest.Create;
  Self.TranListenerLock := TCriticalSection.Create;
  Self.TranListeners    := TList.Create;
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

procedure TIdSipTransaction.HandleRequest(const R: TIdSipRequest;
                                          const T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.HandleResponse(const R: TIdSipResponse;
                                           const T: TIdSipTransport);
begin
end;

procedure TIdSipTransaction.Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                                       const InitialRequest: TIdSipRequest;
                                       const Timeout:        Cardinal = InitialT1_64);
begin
  Self.fDispatcher := Dispatcher;

  Self.InitialRequest.Assign(InitialRequest);
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

procedure TIdSipTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                              const Transport: TIdSipTransport);
begin
  Self.SetState(itsCompleted);
  Self.NotifyOfResponse(R, Transport);
end;

procedure TIdSipTransaction.ChangeToProceeding;
begin
  Self.SetState(itsProceeding);
end;

procedure TIdSipTransaction.ChangeToProceeding(const R: TIdSipRequest);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfRequest(R);
end;

procedure TIdSipTransaction.ChangeToProceeding(const R: TIdSipResponse;
                                               const T: TIdSipTransport);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfResponse(R, T);
end;

procedure TIdSipTransaction.ChangeToTerminated(const R: TIdSipRequest);
begin
  Self.SetState(itsTerminated);
  Self.NotifyOfRequest(R);
  Self.NotifyOfTermination;
end;

procedure TIdSipTransaction.ChangeToTerminated(const R: TIdSipResponse);
begin
  Self.SetState(itsTerminated);
  Self.NotifyOfResponse(R, nil);
  Self.NotifyOfTermination;
end;

procedure TIdSipTransaction.DoOnFail(const Reason: String);
begin
  Self.SetState(itsTerminated);
  Self.NotifyOfFailure(Reason);
  Self.NotifyOfTermination;
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

procedure TIdSipTransaction.NotifyOfRequest(const R: TIdSipRequest);
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnReceiveRequest(R, Self);
  finally
    Self.TranListenerLock.Release;
  end;
end;

procedure TIdSipTransaction.NotifyOfResponse(const R: TIdSipResponse;
                                             const Transport: TIdSipTransport);
var
  I: Integer;
begin
  Self.TranListenerLock.Acquire;
  try
    for I := 0 to Self.TranListeners.Count - 1 do
      IIdSipTransactionListener(Self.TranListeners[I]).OnReceiveResponse(R, Self, Transport);
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
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerA          := TIdSipTimer.Create(true);
  Self.TimerA.Interval := InitialT1;
  Self.TimerA.OnTimer  := Self.OnTimerA;

  Self.TimerB          := TIdSipTimer.Create(true);
  Self.TimerB.OnTimer  := Self.OnTimerB;

  Self.TimerD          := TIdSipTimer.Create(true);
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

procedure TIdSipClientInviteTransaction.HandleResponse(const R: TIdSipResponse;
                                                       const T: TIdSipTransport);
begin
  case Self.State of
    itsCalling: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, T);
        2: Self.EstablishDialog(R, T);
      else
        Self.ChangeToCompleted(R, T);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, T);
        2: Self.EstablishDialog(R, T);
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

procedure TIdSipClientInviteTransaction.Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                                                   const InitialRequest: TIdSipRequest;
                                                   const Timeout:        Cardinal = InitialT1_64);
begin
  inherited Initialise(Dispatcher, InitialRequest, Timeout);

  Self.ChangeToCalling;

  Self.TimerB.Interval := Timeout;
  Self.TimerD.Interval := Timeout;

  Self.TrySendRequest(Self.InitialRequest);

  Self.TimerA.Start;
  Self.TimerB.Start;
end;

function TIdSipClientInviteTransaction.IsClient: Boolean;
begin
  Result := true;
end;

//* TIdSipClientInviteTransaction Protected methods ****************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                                          const Transport: TIdSipTransport);
begin
  // It's unfortunate that we can't simply call inherited.
  // However, TrySendACK must be called before DoOnReceiveResponse,
  // and we have to set Self.State to itsCompleted before
  // TrySendACK because a transport failure changes Self.State
  // to itsTerminated.

  Self.TimerB.Stop;
  Self.TimerD.Start;

  Self.SetState(itsCompleted);
  Self.TrySendACK(R);
  Self.NotifyOfResponse(R, Transport);
end;

procedure TIdSipClientInviteTransaction.EstablishDialog(const R: TIdSipResponse;
                                                        const T: TIdSipTransport);
{
var
  Dialog:   TIdSipDialog;
  ID:       TIdSipDialogID;
  RouteSet: TIdSipHeadersFilter;
}
begin
{
  ID := TIdSipDialogID.Create(Self.InitialRequest.CallID,
                              Self.InitialRequest.From.Tag,
                              R.ToHeader.Tag);
  try
    RouteSet := TIdSipHeadersFilter.Create(Self.InitialRequest.Headers,
                                           RecordRouteHeader);
    try
      Dialog := TIdSipDialog.Create(ID,
                                    Self.InitialRequest.CSeq.SequenceNo,
                                    0,
                                    Self.InitialRequest.From.Address,
                                    Self.InitialRequest.ToHeader.Address,
                                    R.FirstContact.Address,
                                    T.IsSecure and (Self.InitialRequest.FirstContact.HasSipsUri),
                                    RouteSet);
      try
        // create a Dialog and hand it up
//        Self.DoOnNewDialog(Dialog);
      finally
        Dialog.Free;
      end;
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
}
  Self.ChangeToTerminated(R);
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCalling;
begin
  Self.SetState(itsCalling);
end;

procedure TIdSipClientInviteTransaction.ChangeToProceeding(const R: TIdSipResponse;
                                                           const T: TIdSipTransport);
begin
  inherited ChangeToProceeding(R, T);

  Self.TimerA.Stop;
  Self.TimerB.Stop;
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

procedure TIdSipClientInviteTransaction.OnTimerA(Sender: TObject);
begin
  Self.TimerA.Interval := Self.TimerA.Interval*2;
  Self.TryResendInitialRequest;
end;

procedure TIdSipClientInviteTransaction.OnTimerB(Sender: TObject);
begin
  Self.TimerB.Stop;
  Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipClientInviteTransaction.OnTimerD(Sender: TObject);
begin
  Self.SetState(itsTerminated);
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
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerG := TIdSipTimer.Create;
  Self.TimerG.Interval := InitialT1;
  Self.TimerG.OnTimer  := Self.OnTimerG;

  Self.TimerH := TIdSipTimer.Create;
  Self.TimerH.Interval := 64*InitialT1;
  Self.TimerH.OnTimer  := Self.OnTimerH;

  Self.TimerI := TIdSipTimer.Create;
  Self.TimerI.Interval := T4;
  Self.TimerI.OnTimer  := Self.OnTimerI;

  Self.LastResponseSent := TIdSipResponse.Create;
end;

destructor TIdSipServerInviteTransaction.Destroy;
begin
  Self.LastResponseSent.Free;

  Self.TimerI.TerminateAndWaitFor;
  Self.TimerI.Free;
  Self.TimerH.TerminateAndWaitFor;
  Self.TimerH.Free;
  Self.TimerG.TerminateAndWaitFor;
  Self.TimerG.Free;

  inherited Destroy;
end;

procedure TIdSipServerInviteTransaction.HandleRequest(const R: TIdSipRequest;
                                                      const T: TIdSipTransport);
begin
  case Self.State of
    itsProceeding: Self.TrySendLastResponse(R);

    itsCompleted: begin
      if R.IsInvite then
        Self.TrySendLastResponse(R)
      else if R.IsAck then
        Self.ChangeToConfirmed(R);
    end;
  end;
end;

procedure TIdSipServerInviteTransaction.HandleResponse(const R: TIdSipResponse;
                                                       const T: TIdSipTransport);
begin
  Self.TrySendResponse(R);
  if (Self.State = itsProceeding) then begin
    case (R.StatusCode div 100) of
      1:    Self.ChangeToProceeding;
      2:    Self.EstablishDialog(R, T);
      3..6: Self.ChangeToCompleted(R, T);
    end;
  end;
end;

procedure TIdSipServerInviteTransaction.Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                                                   const InitialRequest: TIdSipRequest;
                                                   const Timeout:        Cardinal = InitialT1_64);
begin
  inherited Initialise(Dispatcher, InitialRequest, Timeout);

//  Self.ChangeToProceeding(Self.InitialRequest);
  Self.SetState(itsProceeding);
  Self.NotifyOfRequest(InitialRequest);

  Self.TimerH.Interval := Timeout;

  Self.TrySend100Response(Self.InitialRequest);
end;

function TIdSipServerInviteTransaction.IsClient: Boolean;
begin
  Result := false;
end;

//* TIdSipServerInviteTransaction Protected methods ***************************

procedure TIdSipServerInviteTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                                          const Transport: TIdSipTransport);
begin
  inherited ChangeToCompleted(R, Transport);

  Self.TimerG.Start;
  Self.TimerH.Start;
end;

procedure TIdSipServerInviteTransaction.ChangeToProceeding(const R: TIdSipRequest);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfRequest(R);
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(const R: TIdSipRequest);
begin
  Self.SetState(itsConfirmed);
  Self.NotifyOfRequest(R);

  Self.TimerG.Stop;
  Self.TimerH.Stop;
  Self.TimerI.Start;
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

procedure TIdSipServerInviteTransaction.EstablishDialog(const R: TIdSipResponse;
                                                        const T: TIdSipTransport);
{
var
  ID:       TIdSipDialogID;
  Dialog:   TIdSipDialog;
  RouteSet: TIdSipHeaderList;
}
begin
{
  // Create a Dialog and hand it up

  ID := TIdSipDialogID.Create(Self.InitialRequest.CallID,
                              R.ToHeader.Tag,
                              Self.InitialRequest.From.Tag);
  try
    RouteSet := TIdSipHeadersFilter.Create(Self.InitialRequest.Headers,
                                           RecordRouteHeader);
    try
      Dialog := TIdSipDialog.Create(ID,
                                    0,
                                    Self.InitialRequest.CSeq.SequenceNo,
                                    Self.InitialRequest.ToHeader.Address,
                                    Self.InitialRequest.From.Address,
                                    Self.InitialRequest.FirstContact.Address,
                                    T.IsSecure and (Self.InitialRequest.HasSipsUri),
                                    RouteSet);
      try
//        Self.DoOnNewDialog(Dialog);
      finally
        Dialog.Free;
      end;
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
}
  Self.ChangeToTerminated(R);
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

  if not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendLastResponse(Self.InitialRequest);
end;

procedure TIdSipServerInviteTransaction.OnTimerH(Sender: TObject);
begin
  Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipServerInviteTransaction.OnTimerI(Sender: TObject);
begin
  Self.SetState(itsTerminated);
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
//* TIdSipClientNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipClientNonInviteTransaction Public methods ****************************

constructor TIdSipClientNonInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerE          := TIdSipTimer.Create(true);
  Self.TimerE.Interval := InitialT1;
  Self.TimerE.OnTimer  := Self.OnTimerE;

  Self.TimerF          := TIdSipTimer.Create(true);
  Self.TimerF.OnTimer  := Self.OnTimerF;

  Self.TimerK          := TIdSipTimer.Create(true);
  Self.TimerK.Interval := T4;
  Self.TimerK.OnTimer  := Self.OnTimerK;
end;

destructor TIdSipClientNonInviteTransaction.Destroy;
begin
  Self.TimerK.TerminateAndWaitFor;
  Self.TimerK.Free;
  Self.TimerF.TerminateAndWaitFor;
  Self.TimerF.Free;
  Self.TimerE.TerminateAndWaitFor;
  Self.TimerE.Free;

  inherited Destroy;
end;

procedure TIdSipClientNonInviteTransaction.HandleResponse(const R: TIdSipResponse;
                                                          const T: TIdSipTransport);
begin
  case Self.State of
    itsTrying: begin
      if R.IsFinal then
        Self.ChangeToCompleted(R, T)
      else
        Self.ChangeToProceeding(R, T);
    end;

    itsProceeding: begin
      if R.IsFinal then
        Self.ChangeToCompleted(R, T)
      else
        Self.ChangeToProceeding(R, T);
    end;
  end;
end;

procedure TIdSipClientNonInviteTransaction.Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                                                      const InitialRequest: TIdSipRequest;
                                                      const Timeout:        Cardinal = InitialT1_64);
begin
  inherited Initialise(Dispatcher, InitialRequest, Timeout);

  Self.ChangeToTrying;

  Self.TimerF.Interval := Timeout;

  Self.TrySendRequest(Self.InitialRequest);
  Self.TimerE.Start;
  Self.TimerF.Start;
end;

function TIdSipClientNonInviteTransaction.IsClient: Boolean;
begin
  Result := true;
end;

//* TIdSipClientNonInviteTransaction Protected methods *************************

procedure TIdSipClientNonInviteTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                                             const Transport: TIdSipTransport);
begin
  inherited ChangeToCompleted(R, Transport);

  Self.TimerE.Stop;
  Self.TimerF.Stop;
  Self.TimerK.Start;
end;

procedure TIdSipClientNonInviteTransaction.ChangeToProceeding(const R: TIdSipResponse;
                                                              const T: TIdSipTransport);
begin
  inherited ChangeToProceeding(R, T);

  Self.TimerE.Interval := T2;
end;

procedure TIdSipClientNonInviteTransaction.ChangeToTrying;
begin
  Self.SetState(itsTrying);
end;

//* TIdSipClientNonInviteTransaction Private methods ***************************

procedure TIdSipClientNonInviteTransaction.OnTimerE(Sender: TObject);
begin
  if (Self.State = itsTrying) then begin
    Self.TimerE.Interval := 2*Self.TimerE.Interval;

    if (Self.TimerE.Interval > T2) then
      Self.TimerE.Interval := T2;
  end;

  Self.TryResendInitialRequest;
end;

procedure TIdSipClientNonInviteTransaction.OnTimerF(Sender: TObject);
begin
  Self.DoOnFail(SessionTimeoutMsg);
end;

procedure TIdSipClientNonInviteTransaction.OnTimerK(Sender: TObject);
begin
  Self.SetState(itsTerminated);
end;

//******************************************************************************
//* TIdSipServerNonInviteTransaction                                           *
//******************************************************************************
//* TIdSipServerNonInviteTransaction Public methods ****************************

constructor TIdSipServerNonInviteTransaction.Create;
begin
  inherited Create;

  Self.TimerJ         := TIdSipTimer.Create(true);
  Self.TimerJ.OnTimer := Self.OnTimerJ;

  Self.LastResponseSent := TIdSipResponse.Create;
end;

destructor TIdSipServerNonInviteTransaction.Destroy;
begin
  Self.LastResponseSent.Free;

  Self.TimerJ.TerminateAndWaitFor;
  Self.TimerJ.Free;

  inherited Destroy;
end;

procedure TIdSipServerNonInviteTransaction.HandleRequest(const R: TIdSipRequest;
                                                         const T: TIdSipTransport);
begin
  case Self.State of
    itsCompleted, itsProceeding: begin
      Self.TrySendLastResponse(R);
    end;
  else
    raise Exception.Create('unhandled Self.State in ' + Self.ClassName + '.HandleRequest');
  end;
end;

procedure TIdSipServerNonInviteTransaction.HandleResponse(const R: TIdSipResponse;
                                                          const T: TIdSipTransport);
begin
  case Self.State of
    itsTrying, itsProceeding: begin
      if R.IsFinal then
        Self.ChangeToCompleted(R, T)
      else begin
        Self.LastResponseSent.Assign(R);
        Self.ChangeToProceeding(R, T);
      end;
    end;
  end;
end;

procedure TIdSipServerNonInviteTransaction.Initialise(const Dispatcher:     TIdSipTransactionDispatcher;
                                                      const InitialRequest: TIdSipRequest;
                                                      const Timeout:        Cardinal = InitialT1_64);
begin
  inherited Initialise(Dispatcher, InitialRequest, Timeout);

  Self.TimerJ.Interval := Timeout;

  Self.ChangeToTrying(Self.InitialRequest);
end;

function TIdSipServerNonInviteTransaction.IsClient: Boolean;
begin
  Result := false;
end;

//* TIdSipServerNonInviteTransaction Protected methods *************************

procedure TIdSipServerNonInviteTransaction.ChangeToCompleted(const R: TIdSipResponse;
                                                             const T: TIdSipTransport);
begin
  inherited ChangeToCompleted(R, T);

  Self.TrySendResponse(R);
  Self.TimerJ.Start;
end;

procedure TIdSipServerNonInviteTransaction.ChangeToProceeding(const R: TIdSipResponse;
                                                              const T: TIdSipTransport);
begin
  inherited ChangeToProceeding(R, T);

  Self.TrySendResponse(R);
end;

procedure TIdSipServerNonInviteTransaction.TrySendResponse(const R: TIdSipResponse);
begin
  Self.LastResponseSent.Assign(R);

  inherited TrySendResponse(R);
end;

//* TIdSipServerNonInviteTransaction Private methods ***************************

procedure TIdSipServerNonInviteTransaction.ChangeToTrying(const R: TIdSipRequest);
begin
  Self.SetState(itsTrying);

  Self.NotifyOfRequest(R);
end;

procedure TIdSipServerNonInviteTransaction.OnTimerJ(Sender: TObject);
begin
  Self.SetState(itsTerminated);
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
//* TIdSipTransactionDeliverer                                                 *
//******************************************************************************
//* TIdSipTransactionDeliverer Public methods **********************************

constructor TIdSipTransactionDeliverer.Create(const Tran: TIdSipTransaction;
                                              const T: TIdSipTransport;
                                              const D: TIdSipTransactionDispatcher);
begin
  Self.Transport   := T;
  Self.Dispatcher  := D;
  Self.Transaction := Tran;
end;

procedure TIdSipTransactionDeliverer.VisitRequest(const Request: TIdSipRequest);
begin
  if Assigned(Self.Transaction) then
    Self.Transaction.HandleRequest(Request, Self.Transport)
  else
    Self.Dispatcher.DoOnUnhandledRequest(Request);
end;

procedure TIdSipTransactionDeliverer.VisitResponse(const Response: TIdSipResponse);
begin
  if Assigned(Self.Transaction) then
    Self.Transaction.HandleResponse(Response, Self.Transport)
  else
    Self.Dispatcher.DoOnUnhandledResponse(Response);
end;

end.
