{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTransaction;

interface

uses
  Classes, Contnrs, IdBaseThread, IdInterfacedObject, IdNotification,
  IdSipMessage, IdSipTimer, IdSipTransport, IdTimerQueue, SyncObjs, SysUtils;

const
  DefaultT1    = 500;   // milliseconds
  DefaultT1_64 = 64*DefaultT1;
  DefaultT2    = 4000;  // milliseconds
  DefaultT4    = 5000;  // milliseconds

const
  MaximumUDPMessageSize = 1300;
  SessionTimeoutMsg     = 'Timed out';

type
  // This covers all states - INVITE, non-INVITE, client, server.
  TIdSipTransactionState = (itsCalling, itsCompleted, itsConfirmed,
                            itsProceeding, itsTerminated, itsTrying);

  TIdSipTransaction = class;
  TIdSipTransactionClass = class of TIdSipTransaction;
  TIdSipClientInviteTransaction = class;

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
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
  end;

  TIdSipTransactionProc = procedure(Tran: TIdSipTransaction) of object;

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
    fTimer:          TIdTimerQueue;
    MsgListeners:    TIdNotificationList;
    TimerLock:       TCriticalSection;
    Transports:      TObjectList;
    TransportLock:   TCriticalSection;
    Transactions:    TObjectList;
    TransactionLock: TCriticalSection;

    procedure ClientInviteTransactionTimerA(Tran: TIdSipTransaction);
    procedure ClientInviteTransactionTimerB(Tran: TIdSipTransaction);
    procedure ClientInviteTransactionTimerD(Tran: TIdSipTransaction);
    procedure ClientNonInviteTransactionTimerE(Tran: TIdSipTransaction);
    procedure ClientNonInviteTransactionTimerF(Tran: TIdSipTransaction);
    procedure ClientNonInviteTransactionTimerK(Tran: TIdSipTransaction);
    procedure ServerInviteTransactionTimerG(Tran: TIdSipTransaction);
    procedure ServerInviteTransactionTimerH(Tran: TIdSipTransaction);
    procedure ServerInviteTransactionTimerI(Tran: TIdSipTransaction);
    procedure ServerNonInviteTransactionTimerJ(Tran: TIdSipTransaction);
    procedure DeliverToTransaction(Request: TIdSipRequest;
                                   Receiver: TIdSipTransport); overload;
    procedure DeliverToTransaction(Response: TIdSipResponse;
                                   Receiver: TIdSipTransport); overload;
    procedure FindTransactionAndPerform(Event: TObject;
                                        ClientTran: Boolean;
                                        Proc: TIdSipTransactionProc);
    function  FindTransaction(R: TIdSipMessage;
                              ClientTran: Boolean): TIdSipTransaction;

    procedure RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
    function  TransactionAt(Index: Cardinal): TIdSipTransaction;
    function  TransportAt(Index: Cardinal): TIdSipTransport;
  protected
    function  FindAppropriateTransport(Msg: TIdSipMessage): TIdSipTransport;
    procedure NotifyListenersOfRequest(Request: TIdSipRequest;
                                       Receiver: TIdSipTransport);
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
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
    procedure OnClientInviteTransactionTimerA(Event: TObject);
    procedure OnClientInviteTransactionTimerB(Event: TObject);
    procedure OnClientInviteTransactionTimerD(Event: TObject);
    procedure OnClientNonInviteTransactionTimerE(Event: TObject);
    procedure OnClientNonInviteTransactionTimerF(Event: TObject);
    procedure OnClientNonInviteTransactionTimerK(Event: TObject);
    procedure OnServerInviteTransactionTimerG(Event: TObject);
    procedure OnServerInviteTransactionTimerH(Event: TObject);
    procedure OnServerInviteTransactionTimerI(Event: TObject);
    procedure OnServerNonInviteTransactionTimerJ(Event: TObject);
    procedure RemoveUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
    procedure ScheduleEvent(Event: TNotifyEvent;
                            WaitTime: Cardinal;
                            Request: TIdSipMessage);
    procedure Send(Msg: TIdSipMessage); virtual;
    procedure SendRequest(Request: TIdSipRequest); virtual;
    procedure SendResponse(Response: TIdSipResponse); virtual;
    function  TransactionCount: Integer;
    function  TransportCount: Integer;
    function  WillUseReliableTranport(R: TIdSipMessage): Boolean;

    property T1Interval: Cardinal      read fT1Interval write fT1Interval;
    property T2Interval: Cardinal      read fT2Interval write fT2Interval;
    property T4Interval: Cardinal      read fT4Interval write fT4Interval;
    property Timer:      TIdTimerQueue read fTimer write fTimer;
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
    fLastResponse:    TIdSipResponse;
    TranListeners:    TIdNotificationList;
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
    procedure NotifyOfRequest(R: TIdSipRequest;
                              T: TIdSipTransport);
    procedure NotifyOfResponse(R: TIdSipResponse;
                               Receiver: TIdSipTransport);
    procedure NotifyOfTermination;
    procedure SetState(Value: TIdSipTransactionState); virtual;
    procedure TryResendInitialRequest;
    procedure TrySendRequest(R: TIdSipRequest);
    procedure TrySendResponse(R: TIdSipResponse); virtual;
  public
    class function CreateClientTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                               Request: TIdSipRequest): TIdSipTransaction;
    class function CreateServerTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                               Request: TIdSipRequest): TIdSipTransaction;

    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); virtual;
    destructor  Destroy; override;

    procedure AddTransactionListener(const Listener: IIdSipTransactionListener);
    procedure ExceptionRaised(E: Exception);
    function  IsClient: Boolean; virtual; abstract;
    function  IsInvite: Boolean; virtual; abstract;
    function  IsNull: Boolean; virtual; abstract;
    function  IsServer: Boolean;
    function  IsTerminated: Boolean;
    function  Match(Msg: TIdSipMessage): Boolean;
    function  LoopDetected(Request: TIdSipRequest): Boolean;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); virtual;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); virtual;
    procedure RemoveTransactionListener(const Listener: IIdSipTransactionListener);
    procedure SendRequest; virtual;
    procedure SendResponse(R: TIdSipResponse); virtual;
    procedure Terminate(Quiet: Boolean);

    property Dispatcher:     TIdSipTransactionDispatcher read fDispatcher;
    property InitialRequest: TIdSipRequest               read fInitialRequest;
    property LastResponse:   TIdSipResponse              read fLastResponse;
    property State:          TIdSipTransactionState      read fState;
  end;

  TIdSipServerTransaction = class(TIdSipTransaction)
  protected
    LastResponseSent: TIdSipResponse;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    function  IsClient: Boolean; override;
  end;

  TIdSipServerInviteTransaction = class(TIdSipServerTransaction)
  private
    TimerGInterval: Cardinal;

    procedure ChangeToConfirmed(R: TIdSipRequest;
                                T: TIdSipTransport);
    function  CreateTryingResponse(R: TIdSipRequest): TIdSipResponse;
    procedure ScheduleTimerG;
    procedure ScheduleTimerH;
    procedure ScheduleTimerI;
    procedure TrySendTryingResponse(R: TIdSipRequest);
    procedure TrySendLastResponse(R: TIdSipRequest);
  protected
    procedure ChangeToCompleted; override;
    procedure TrySendResponse(R: TIdSipResponse); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    procedure FireTimerG;
    procedure FireTimerH;
    procedure FireTimerI;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  TimerHInterval: Cardinal;
    function  TimerIInterval: Cardinal;
  end;

  TIdSipServerNonInviteTransaction = class(TIdSipServerTransaction)
  private
    procedure ChangeToTrying;
    procedure TrySendLastResponse(R: TIdSipRequest);
  protected
    procedure ChangeToCompleted; override;
    procedure TrySendResponse(R: TIdSipResponse); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    procedure FireTimerJ;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveRequest(R: TIdSipRequest;
                             T: TIdSipTransport); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  TimerJInterval: Cardinal;
  end;

  TIdSipClientTransaction = class(TIdSipTransaction)
  public
    function IsClient: Boolean; override;
  end;

  TIdSipClientInviteTransaction = class(TIdSipClientTransaction)
  private
    TimerAInterval: Cardinal;

    procedure ChangeToCalling;
    procedure ScheduleTimerA;
    procedure ScheduleTimerB;
    procedure ScheduleTimerD;
    procedure TrySendACK(R: TIdSipResponse);
  protected
    procedure ChangeToCompleted(R: TIdSipResponse;
                                T: TIdSipTransport); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    procedure FireTimerA;
    procedure FireTimerB;
    procedure FireTimerD;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); override;
    procedure SendRequest; override;
    function  TimerBInterval: Cardinal;
    function  TimerDInterval: Cardinal;
  end;

  TIdSipClientNonInviteTransaction = class(TIdSipClientTransaction)
  private
    TimerEInterval: Cardinal;

    procedure RecalculateTimerE;
    procedure ScheduleTimerE;
    procedure ScheduleTimerF;
    procedure ScheduleTimerK;
  protected
    procedure ChangeToCompleted; override;
    procedure ChangeToTrying;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    procedure FireTimerE;
    procedure FireTimerF;
    procedure FireTimerK;
    function  IsInvite: Boolean; override;
    function  IsNull: Boolean; override;
    procedure ReceiveResponse(R: TIdSipResponse;
                              T: TIdSipTransport); override;
    procedure SendRequest; override;
    function  TimerFInterval: Cardinal;
    function  TimerKInterval: Cardinal;
  end;

  TIdSipNullTransaction = class(TIdSipTransaction)
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    function IsClient: Boolean; override;
    function IsInvite: Boolean; override;
    function IsNull: Boolean; override;
  end;

  TIdSipTransactionDispatcherMethod = class(TIdMethod)
  private
    fReceiver: TIdSipTransport;
  public
    property Receiver: TIdSipTransport read fReceiver write fReceiver;
  end;

  TIdSipUnhandledMessageListenerReceiveRequestMethod = class(TIdSipTransactionDispatcherMethod)
  private
    fRequest: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Request:  TIdSipRequest read fRequest write fRequest;
  end;

  TIdSipUnhandledMessageListenerReceiveResponseMethod = class(TIdSipTransactionDispatcherMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipTransactionMethod = class(TIdMethod)
  private
    fTransaction: TIdSipTransaction;
  public
    property Transaction: TIdSipTransaction read fTransaction write fTransaction;
  end;

  TIdSipTransactionListenerFailMethod = class(TIdSipTransactionMethod)
  private
    fReason: String;
  public
    procedure Run(const Subject: IInterface); override;

    property Reason: String read fReason write fReason;
  end;

  TIdSipTransactionListenerReceiveRequestMethod = class(TIdSipTransactionMethod)
  private
    fReceiver: TIdSipTransport;
    fRequest:  TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Request:  TIdSipRequest   read fRequest write fRequest;
  end;

  TIdSipTransactionListenerReceiveResponseMethod = class(TIdSipTransactionMethod)
  private
    fReceiver: TIdSipTransport;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Receiver: TIdSipTransport read fReceiver write fReceiver;
    property Response: TIdSipResponse  read fResponse write fResponse;
  end;

  TIdSipTransactionListenerTerminatedMethod = class(TIdSipTransactionMethod)
  public
    procedure Run(const Subject: IInterface); override;
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

  Self.MsgListeners := TIdNotificationList.Create;
  Self.TimerLock    := TCriticalSection.Create;

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
  Self.TransactionLock.Acquire;
  try
    Self.Transactions.Free;
  finally
    Self.TransactionLock.Release;
  end;
  Self.TransactionLock.Free;

  Self.TransportLock.Acquire;
  try
    Self.Transports.Free;
  finally
    Self.TransportLock.Release;
  end;
  Self.TransportLock.Free;

  Self.TimerLock.Free;
  Self.MsgListeners.Free;

  inherited Destroy;
end;

procedure TIdSipTransactionDispatcher.AddUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
begin
  Self.MsgListeners.AddListener(Listener);
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
      Result := TIdSipTransaction.CreateClientTransactionType(Self, InitialRequest);
      Result.AddTransactionListener(Self);
      Self.Transactions.Add(Result);
    except
      if (Self.Transactions.IndexOf(Result) <> -1) then
        Self.Transactions.Remove(Result)
      else
        Result.Free;

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
      Result := TIdSipTransaction.CreateServerTransactionType(Self, InitialRequest);
      Result.AddTransactionListener(Self);
      Self.Transactions.Add(Result);
    except
      if (Self.Transactions.IndexOf(Result) <> -1) then
        Self.Transactions.Remove(Result)
      else
        Result.Free;

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
  // self transactionLock critical:
  //     [^ (self transactions
  //         detect: [ :each | each loopDetected: request ]
  //         ifNone: [nil]]) notNil.

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

procedure TIdSipTransactionDispatcher.OnClientInviteTransactionTimerA(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientInviteTransactionTimerA);
end;

procedure TIdSipTransactionDispatcher.OnClientInviteTransactionTimerB(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientInviteTransactionTimerB);
end;

procedure TIdSipTransactionDispatcher.OnClientInviteTransactionTimerD(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientInviteTransactionTimerD);
end;

procedure TIdSipTransactionDispatcher.OnClientNonInviteTransactionTimerE(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientNonInviteTransactionTimerE);
end;

procedure TIdSipTransactionDispatcher.OnClientNonInviteTransactionTimerF(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientNonInviteTransactionTimerF);
end;

procedure TIdSipTransactionDispatcher.OnClientNonInviteTransactionTimerK(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 true,
                                 Self.ClientNonInviteTransactionTimerK);
end;

procedure TIdSipTransactionDispatcher.OnServerInviteTransactionTimerG(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 false,
                                 Self.ServerInviteTransactionTimerG);
end;

procedure TIdSipTransactionDispatcher.OnServerInviteTransactionTimerH(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 false,
                                 Self.ServerInviteTransactionTimerH);
end;

procedure TIdSipTransactionDispatcher.OnServerInviteTransactionTimerI(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 false,
                                 Self.ServerInviteTransactionTimerI);
end;

procedure TIdSipTransactionDispatcher.OnServerNonInviteTransactionTimerJ(Event: TObject);
begin
  Self.FindTransactionAndPerform(Event,
                                 false,
                                 Self.ServerNonInviteTransactionTimerJ);
end;

procedure TIdSipTransactionDispatcher.RemoveUnhandledMessageListener(const Listener: IIdSipUnhandledMessageListener);
begin
  Self.MsgListeners.RemoveListener(Listener);
end;

procedure TIdSipTransactionDispatcher.ScheduleEvent(Event: TNotifyEvent;
                                                    WaitTime: Cardinal;
                                                    Request: TIdSipMessage);
begin
  Self.TimerLock.Acquire;
  try
    if Assigned(Self.Timer) then
      Self.Timer.AddEvent(WaitTime, Event, Request);
  finally
    Self.TimerLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.Send(Msg: TIdSipMessage);
var
  MsgLen:       Cardinal;
  RewrittenVia: Boolean;
begin
  // Don't call this method. Transactions use this method to send messages to
  // the Transport layer.

  MsgLen := Length(Msg.AsString);
  RewrittenVia := (MsgLen > MaximumUDPMessageSize) and (Msg.LastHop.Transport = sttUDP);

  if RewrittenVia then
    Msg.LastHop.Transport := sttTCP;

  try
    Self.FindAppropriateTransport(Msg).Send(Msg);
  except
    on EIdSipTransport do begin
      Msg.LastHop.Transport := sttUDP;

      // If this too raises an EIdSipTransport exception, the transaction will
      // receive the exception and handle it accordingly.
      Self.FindAppropriateTransport(Msg).Send(Msg);
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.SendRequest(Request: TIdSipRequest);
var
  Tran: TIdSipTransaction;
begin
  if Request.IsAck then begin
    // Since ACKs to 200s live outside of transactions, we must send the
    // ACK directly to the transport layer.
    Self.Send(Request);
  end
  else begin
    Tran := Self.FindTransaction(Request, true);

    if Assigned(Tran) then
      Tran.SendRequest
    else begin
      Tran := Self.AddClientTransaction(Request);
      Tran.SendRequest;
    end;

    if Tran.IsTerminated then
      Self.RemoveTransaction(Tran);
  end;
end;

procedure TIdSipTransactionDispatcher.SendResponse(Response: TIdSipResponse);
var
  Tran: TIdSipTransaction;
begin
  // We have a problem here. If we have an RFC 2543 non-INVITE transaction on
  // the go, we CANNOT match Response to it. What the HELL do we do??
  // TODO
  // cf RFC 3261, section 17.2.3, last paragraph
  Tran := Self.FindTransaction(Response, false);

  if Assigned(Tran) then begin
    Tran.SendResponse(Response);
    if Tran.IsTerminated then
      Self.RemoveTransaction(Tran);
  end
  else
    Self.Send(Response);
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

procedure TIdSipTransactionDispatcher.NotifyListenersOfRequest(Request: TIdSipRequest;
                                                               Receiver: TIdSipTransport);
var
  Notification: TIdSipUnhandledMessageListenerReceiveRequestMethod;
begin
  Notification := TIdSipUnhandledMessageListenerReceiveRequestMethod.Create;
  try
    Notification.Receiver := Receiver;
    Notification.Request  := Request;

    Self.MsgListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                                Receiver: TIdSipTransport);
var
  Notification: TIdSipUnhandledMessageListenerReceiveResponseMethod;
begin
  Notification := TIdSipUnhandledMessageListenerReceiveResponseMethod.Create;
  try
    Notification.Receiver := Receiver;
    Notification.Response := Response;

    Self.MsgListeners.Notify(Notification);
  finally
    Notification.Free;
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
  Self.NotifyListenersOfRequest(Request, Receiver);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                        Transaction: TIdSipTransaction;
                                                        Receiver: TIdSipTransport);
begin
  Self.NotifyListenersOfResponse(Response, Receiver);
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

procedure TIdSipTransactionDispatcher.ClientInviteTransactionTimerA(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientInviteTransaction) then
    (Tran as TIdSipClientInviteTransaction).FireTimerA;
end;

procedure TIdSipTransactionDispatcher.ClientInviteTransactionTimerB(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientInviteTransaction) then
    (Tran as TIdSipClientInviteTransaction).FireTimerB;
end;

procedure TIdSipTransactionDispatcher.ClientInviteTransactionTimerD(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientInviteTransaction) then
    (Tran as TIdSipClientInviteTransaction).FireTimerD;
end;

procedure TIdSipTransactionDispatcher.ClientNonInviteTransactionTimerE(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientNonInviteTransaction) then
    (Tran as TIdSipClientNonInviteTransaction).FireTimerE;
end;

procedure TIdSipTransactionDispatcher.ClientNonInviteTransactionTimerF(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientNonInviteTransaction) then
    (Tran as TIdSipClientNonInviteTransaction).FireTimerF;
end;

procedure TIdSipTransactionDispatcher.ClientNonInviteTransactionTimerK(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipClientNonInviteTransaction) then
    (Tran as TIdSipClientNonInviteTransaction).FireTimerK;
end;

procedure TIdSipTransactionDispatcher.ServerInviteTransactionTimerG(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipServerInviteTransaction) then
    (Tran as TIdSipServerInviteTransaction).FireTimerG;
end;

procedure TIdSipTransactionDispatcher.ServerInviteTransactionTimerH(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipServerInviteTransaction) then
    (Tran as TIdSipServerInviteTransaction).FireTimerH;
end;

procedure TIdSipTransactionDispatcher.ServerInviteTransactionTimerI(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipServerInviteTransaction) then
    (Tran as TIdSipServerInviteTransaction).FireTimerI;
end;

procedure TIdSipTransactionDispatcher.ServerNonInviteTransactionTimerJ(Tran: TIdSipTransaction);
begin
  if (Tran is TIdSipServerNonInviteTransaction) then
    (Tran as TIdSipServerNonInviteTransaction).FireTimerJ;
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Request: TIdSipRequest;
                                                           Receiver: TIdSipTransport);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Request, false);

  if Assigned(Tran) then begin
    Tran.ReceiveRequest(Request, Receiver);
  end
  else begin
    // An ACK does not belong inside a transaction when a UAS sends a 2xx in
    // response to an INVITE
    if Request.IsAck then begin
      Self.NotifyListenersOfRequest(Request, Receiver);
    end
    else begin
      Tran := Self.AddServerTransaction(Request, Receiver);
      Tran.ReceiveRequest(Request, Receiver);
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Response: TIdSipResponse;
                                                           Receiver: TIdSipTransport);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Response, true);

  if Assigned(Tran) then begin
    Tran.ReceiveResponse(Response, Receiver);

    if Tran.IsTerminated then
      Self.RemoveTransaction(Tran);
  end
  else begin
    // Sometimes responses won't match a transaction. For instance, a UA A might
    // send an INVITE to B. B responds with a 200, which terminates that INVITE
    // transaction. For some reason, A's ACK never reaches B, so B resends the
    // 200. That arrives here, in this clause - even though the 200 doesn't
    // match a transaction, we most definitely do want the Transaction-User
    // layer to see this message!
    Self.NotifyListenersOfResponse(Response, Receiver);
  end;
end;

procedure TIdSipTransactionDispatcher.FindTransactionAndPerform(Event: TObject;
                                                                ClientTran: Boolean;
                                                                Proc: TIdSipTransactionProc);
var
  Request: TIdSipRequest;
  Tran:    TIdSipTransaction;
begin
  // Used only by the On<transaction type><timer name> methods

  Request := (Event as TIdNotifyEventWait).Data as TIdSipRequest;
  try
    Tran := Self.FindTransaction(Request, ClientTran);
    if Assigned(Tran) and not Tran.IsTerminated then begin
      Proc(Tran);

      if Tran.IsTerminated then
        Self.RemoveTransaction(Tran);
    end;
  finally
    Request.Free;
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
      if (Self.TransactionAt(I).IsClient = ClientTran)
        and Self.TransactionAt(I).Match(R) then
        Result := Self.TransactionAt(I)
      else
       Inc(I);
  finally
    Self.TransactionLock.Release;
  end;
end;

procedure TIdSipTransactionDispatcher.RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
begin
  // Three reasons why a transaction would terminate: one of its timers fired,
  // or a transport error occurred. Transport errors can only occur in
  // Self.Send(Request|Response); timers happen in
  // <transaction type>Transaction<timer name> methods -
  // ClientInviteTransactionTimerA etc.; 2xx terminate client INVITE transactions.

  Assert(TerminatedTransaction.IsTerminated,
         'Transactions must only be removed when they''re terminated');

  Self.TransactionLock.Acquire;
  try
    Self.Transactions.Remove(TerminatedTransaction);
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

class function TIdSipTransaction.CreateClientTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                                             Request: TIdSipRequest): TIdSipTransaction;
begin
  if Request.IsInvite then
    Result := TIdSipClientInviteTransaction.Create(Dispatcher, Request)
  else
    Result := TIdSipClientNonInviteTransaction.Create(Dispatcher, Request)
end;

class function TIdSipTransaction.CreateServerTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                                             Request: TIdSipRequest): TIdSipTransaction;
begin
  if Request.IsInvite then
    Result := TIdSipServerInviteTransaction.Create(Dispatcher, Request)
  else
    Result := TIdSipServerNonInviteTransaction.Create(Dispatcher, Request)
end;

constructor TIdSipTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                     InitialRequest: TIdSipRequest);
begin
  inherited Create;

  Self.fDispatcher := Dispatcher;

  Self.fInitialRequest := TIdSipRequest.Create;
  Self.InitialRequest.Assign(InitialRequest);
  Self.fLastResponse := TIdSipResponse.Create;

  Self.TranListeners := TIdNotificationList.Create;

  Self.FirstTime := true;
end;

destructor TIdSipTransaction.Destroy;
begin
  Self.TranListeners.Free;
  Self.LastResponse.Free;
  Self.InitialRequest.Free;

  inherited Destroy;
end;

procedure TIdSipTransaction.AddTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListeners.AddListener(Listener);
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

function TIdSipTransaction.IsTerminated: Boolean;
begin
  Result := Self.State = itsTerminated;
end;

function TIdSipTransaction.Match(Msg: TIdSipMessage): Boolean;
begin
  Result := Self.InitialRequest.Match(Msg);

  if not Msg.LastHop.IsRFC3261Branch
     and Msg.IsRequest
     and (Msg as TIdSipRequest).IsAck then
    Result := Result
          and (Msg.ToHeader.Tag = Self.LastResponse.ToHeader.Tag)
end;

function TIdSipTransaction.LoopDetected(Request: TIdSipRequest): Boolean;
begin
  Result := Request.From.Equals(Self.InitialRequest.From)
       and (Request.CallID = Self.InitialRequest.CallID)
       and (Request.CSeq.Equals(Self.InitialRequest.CSeq))
       and not Self.Match(Request);
end;

procedure TIdSipTransaction.ReceiveRequest(R: TIdSipRequest;
                                           T: TIdSipTransport);
begin
  // By default we do nothing
end;

procedure TIdSipTransaction.ReceiveResponse(R: TIdSipResponse;
                                            T: TIdSipTransport);
begin
  // By default we do nothing
end;

procedure TIdSipTransaction.RemoveTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListeners.RemoveListener(Listener);
end;

procedure TIdSipTransaction.SendRequest;
begin
  // By default we do nothing
end;

procedure TIdSipTransaction.SendResponse(R: TIdSipResponse);
begin
  Self.LastResponse.Assign(R);
end;

procedure TIdSipTransaction.Terminate(Quiet: Boolean);
begin
  Self.ChangeToTerminated(Quiet);
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

  // Local timeouts look the same as receiving a Request Timeout from the
  // network.
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

  // A network error looks the same as receiving a Service Unavailable from
  // the network.
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
  Notification: TIdSipTransactionListenerFailMethod;
begin
  Notification := TIdSipTransactionListenerFailMethod.Create;
  try
    Notification.Reason      := Reason;
    Notification.Transaction := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransaction.NotifyOfRequest(R: TIdSipRequest;
                                            T: TIdSipTransport);
var
  Notification: TIdSipTransactionListenerReceiveRequestMethod;
begin
  Notification := TIdSipTransactionListenerReceiveRequestMethod.Create;
  try
    Notification.Receiver    := T;
    Notification.Request     := R;
    Notification.Transaction := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransaction.NotifyOfResponse(R: TIdSipResponse;
                                             Receiver: TIdSipTransport);
var
  Notification: TIdSipTransactionListenerReceiveResponseMethod;
begin
  Notification := TIdSipTransactionListenerReceiveResponseMethod.Create;
  try
    Notification.Receiver    := Receiver;
    Notification.Response    := R;
    Notification.Transaction := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransaction.NotifyOfTermination;
var
  Notification: TIdSipTransactionListenerTerminatedMethod;
begin
  Notification := TIdSipTransactionListenerTerminatedMethod.Create;
  try
    Notification.Transaction := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
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

  Self.LastResponseSent := Self.LastResponse;
end;

function TIdSipServerTransaction.IsClient: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.ChangeToProceeding;
end;

procedure TIdSipServerInviteTransaction.FireTimerG;
begin
  if not (Self.State = itsCompleted) then Exit;

  if (Self.State = itsCompleted)
    and not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendLastResponse(Self.InitialRequest);

  if (Self.TimerGInterval < Self.Dispatcher.T2Interval) then
    Self.TimerGInterval := 2*Self.TimerGInterval
  else
    Self.TimerGInterval := Self.Dispatcher.T2Interval;

  Self.ScheduleTimerG;
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
  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.TrySendTryingResponse(Self.InitialRequest);

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

function TIdSipServerInviteTransaction.TimerHInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

function TIdSipServerInviteTransaction.TimerIInterval: Cardinal;
begin
  // Result in milliseconds
  Result := Self.Dispatcher.T4Interval;
end;

//* TIdSipServerInviteTransaction Protected methods ***************************

procedure TIdSipServerInviteTransaction.ChangeToCompleted;
begin
  inherited ChangeToCompleted;

  Self.TimerGInterval := Self.Dispatcher.T1Interval;
  Self.ScheduleTimerG;
  Self.ScheduleTimerH;
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(R: TIdSipRequest;
                                                          T: TIdSipTransport);
begin
  Self.SetState(itsConfirmed);

  Self.ScheduleTimerI;
end;

function TIdSipServerInviteTransaction.CreateTryingResponse(R: TIdSipRequest): TIdSipResponse;
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

procedure TIdSipServerInviteTransaction.ScheduleTimerG;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnServerInviteTransactionTimerG,
                                Self.TimerGInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipServerInviteTransaction.ScheduleTimerH;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnServerInviteTransactionTimerH,
                                Self.TimerHInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipServerInviteTransaction.ScheduleTimerI;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnServerInviteTransactionTimerI,
                                Self.TimerIInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipServerInviteTransaction.TrySendTryingResponse(R: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateTryingResponse(Self.InitialRequest);
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
  if (not R.Equals(Self.LastResponseSent)) then
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

function TIdSipServerNonInviteTransaction.TimerJInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

//* TIdSipServerNonInviteTransaction Protected methods *************************

procedure TIdSipServerNonInviteTransaction.ChangeToCompleted;
begin
  inherited ChangeToCompleted;

  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnServerNonInviteTransactionTimerJ,
                                Self.TimerJInterval,
                                Self.InitialRequest.Copy);
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
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.TimerAInterval := Self.Dispatcher.T1Interval;

  Self.ChangeToCalling;
end;

procedure TIdSipClientInviteTransaction.FireTimerA;
begin
  if (Self.State <> itsCalling) then Exit;

  Self.TryResendInitialRequest;

  Self.TimerAInterval := 2*Self.TimerAInterval;
  Self.ScheduleTimerA;
end;

procedure TIdSipClientInviteTransaction.FireTimerB;
begin
  if (Self.State <> itsCalling) then Exit;

  Self.DoOnTimeout(Self.Dispatcher.FindAppropriateTransport(Self.InitialRequest),
                   Self.InitialRequest,
                   SessionTimeoutMsg);
end;

procedure TIdSipClientInviteTransaction.FireTimerD;
begin
  if (Self.State <> itsCompleted) then Exit;

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

    Self.ScheduleTimerA;

    Self.TrySendRequest(Self.InitialRequest);
  end;
end;

function TIdSipClientInviteTransaction.TimerBInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

function TIdSipClientInviteTransaction.TimerDInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

//* TIdSipClientInviteTransaction Protected methods ****************************

procedure TIdSipClientInviteTransaction.ChangeToCompleted(R: TIdSipResponse;
                                                          T: TIdSipTransport);
var
  FirstResponse: Boolean;
begin
  FirstResponse := Self.State <> itsCompleted;

  // It's unfortunate that we can't simply call inherited.
  // However, TrySendACK must be called before NotifyOfResponse, (todo: why?)
  // and we have to set Self.State to itsCompleted before
  // TrySendACK because a transport failure changes Self.State
  // to itsTerminated.

  Self.SetState(itsCompleted);
  Self.ScheduleTimerB;
  Self.ScheduleTimerD;

  Self.TrySendACK(R);

  if FirstResponse then
    Self.NotifyOfResponse(R, T);
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCalling;
begin
  Self.SetState(itsCalling);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerA;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientInviteTransactionTimerA,
                                Self.TimerAInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerB;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientInviteTransactionTimerB,
                                Self.TimerBInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerD;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientInviteTransactionTimerD,
                                Self.TimerDInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipClientInviteTransaction.TrySendACK(R: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  if not R.IsOK then begin
    Ack := Self.InitialRequest.AckFor(R);
    try
      Self.TrySendRequest(Ack);
    finally
      Ack.Free;
    end;
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

  Self.TimerEInterval := Self.Dispatcher.T1Interval;

  Self.ChangeToTrying;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerE;
begin
  if (Self.State in [itsTrying, itsProceeding]) then
    Self.TryResendInitialRequest;

  Self.RecalculateTimerE;
  Self.ScheduleTimerE;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerF;
begin
  if not (Self.State in [itsTrying, itsProceeding]) then Exit;

  Self.DoOnTimeout(Self.Dispatcher.FindAppropriateTransport(Self.InitialRequest),
                   Self.InitialRequest,
                   SessionTimeoutMsg);
end;

procedure TIdSipClientNonInviteTransaction.FireTimerK;
begin
  if (Self.State <> itsCompleted) then Exit;

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
    Self.TrySendRequest(Self.InitialRequest);
  end;
end;

function TIdSipClientNonInviteTransaction.TimerFInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

function TIdSipClientNonInviteTransaction.TimerKInterval: Cardinal;
begin
  // Result in milliseconds
  Result := Self.Dispatcher.T4Interval;
end;

//* TIdSipClientNonInviteTransaction Protected methods *************************

procedure TIdSipClientNonInviteTransaction.ChangeToCompleted;
begin
  inherited ChangeToCompleted;

  Self.ScheduleTimerK;
end;

procedure TIdSipClientNonInviteTransaction.ChangeToTrying;
begin
  Self.SetState(itsTrying);

  Self.ScheduleTimerF;
end;

//* TIdSipClientNonInviteTransaction Protected methods *************************

procedure TIdSipClientNonInviteTransaction.RecalculateTimerE;
begin
  if (Self.State = itsTrying) then begin
    if (Self.TimerEInterval < Self.Dispatcher.T2Interval) then
      Self.TimerEInterval := 2 * Self.TimerEInterval
    else
      Self.TimerEInterval := Self.Dispatcher.T2Interval;
  end
  else if (Self.State = itsProceeding) then
    Self.TimerEInterval := Self.Dispatcher.T2Interval;
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerE;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientNonInviteTransactionTimerE,
                                Self.TimerEInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerF;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientNonInviteTransactionTimerF,
                                Self.TimerFInterval,
                                Self.InitialRequest.Copy);
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerK;
begin
  Self.Dispatcher.ScheduleEvent(Self.Dispatcher.OnClientNonInviteTransactionTimerK,
                                Self.TimerKInterval,
                                Self.InitialRequest.Copy);
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

//******************************************************************************
//* TIdSipUnhandledMessageListenerReceiveRequestMethod                         *
//******************************************************************************
//* TIdSipUnhandledMessageListenerReceiveRequestMethod Public methods **********

procedure TIdSipUnhandledMessageListenerReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipUnhandledMessageListener).OnReceiveRequest(Self.Request,
                                                               Self.Receiver);
end;

//******************************************************************************
//* TIdSipUnhandledMessageListenerReceiveResponseMethod                        *
//******************************************************************************
//* TIdSipUnhandledMessageListenerReceiveResponseMethod Public methods *********

procedure TIdSipUnhandledMessageListenerReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipUnhandledMessageListener).OnReceiveResponse(Self.Response,
                                                                Self.Receiver);
end;

//******************************************************************************
//* TIdSipTransactionListenerFailMethod                                        *
//******************************************************************************
//* TIdSipTransactionListenerFailMethod Public methods *************************

procedure TIdSipTransactionListenerFailMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnFail(Self.Transaction,
                                                Self.Reason);
end;

//******************************************************************************
//* TIdSipTransactionListenerReceiveRequestMethod                              *
//******************************************************************************
//* TIdSipTransactionListenerReceiveRequestMethod Public methods ***************

procedure TIdSipTransactionListenerReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnReceiveRequest(Self.Request,
                                                          Self.Transaction,
                                                          Self.Receiver);
end;

//******************************************************************************
//* TIdSipTransactionListenerReceiveResponseMethod                             *
//******************************************************************************
//* TIdSipTransactionListenerReceiveResponseMethod Public methods **************

procedure TIdSipTransactionListenerReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnReceiveResponse(Self.Response,
                                                           Self.Transaction,
                                                           Self.Receiver);
end;

//******************************************************************************
//* TIdSipTransactionListenerTerminatedMethod                                  *
//******************************************************************************
//* TIdSipTransactionListenerTerminatedMethod Public methods *******************

procedure TIdSipTransactionListenerTerminatedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnTerminated(Self.Transaction);
end;

end.
