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
  Classes, Contnrs, IdBaseThread, IdConnectionBindings, IdInterfacedObject,
  IdNotification, IdRoutingTable, IdSipAuthentication, IdSipLocation,
  IdSipLocator, IdSipMessage, IdSipTransport, IdSipTransportAddressSpace,
  IdTimerQueue, PluggableLogging, SysUtils;

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
                     FailedMessage: TIdSipMessage;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Binding: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Binding: TIdConnectionBindings);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  end;

  TIdSipTransactionDispatcher = class;
  // I allow interested parties to listen for requests/responses that do not
  // match any current transactions.
  IIdSipTransactionDispatcherListener = interface
    ['{0CB5037D-B9B3-4FB6-9201-80A0F10DB23A}']
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Binding: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Binding: TIdConnectionBindings);
    procedure OnTransportException(FailedMessage: TIdSipMessage;
                                   const Reason: String);
  end;

  // I allow interested parties to track the addition and removal of transports
  // from some container.
  IIdSipTransportManagementListener = interface
    ['{333DD25E-A77F-49EE-B197-1231686B3FF4}']
    procedure OnAddedTransport(Transport: TIdSipTransport);
    procedure OnRemovedTransport(Transport: TIdSipTransport);
  end;

  TIdSipTransactionProc = procedure(Tran: TIdSipTransaction) of object;
  TIdSipResponseLocationsList = class;

  // I represent the single connection point between the transport layer and the
  // transaction layer. I manage transactions, dispatch messages to the
  // appropriate transaction or fire events on messages that don't match any
  // transactions.
  //
  // I manage the lifetimes of transports that you give me via the AddTransport
  // method. This means that you can no longer have two TransactionDispatchers
  // reading messages from the same transport object.
  TIdSipTransactionDispatcher = class(TIdInterfacedObject,
                                      IIdSipTransactionListener,
                                      IIdSipTransportListener)
  private
    fLocator:                 TIdSipAbstractLocator;
    fRoutingTable:            TIdRoutingTable;
    fT1Interval:              Cardinal;
    fT2Interval:              Cardinal;
    fT4Interval:              Cardinal;
    fTimer:                   TIdTimerQueue;
    ManagementListeners:      TIdNotificationList;
    MsgListeners:             TIdNotificationList;
    fTransports:              TIdSipTransports;
    TransactionlessResponses: TIdSipResponseLocationsList;
    Transactions:             TObjectList;
    TransportTypes:           TIdSipTransportSpecifiers;

    procedure DeliverToTransaction(Request: TIdSipRequest;
                                   Binding: TIdConnectionBindings); overload;
    procedure DeliverToTransaction(Response: TIdSipResponse;
                                   Binding: TIdConnectionBindings); overload;
    function  GetDefaultPreferredTransportType: String;
    procedure LogException(FailedMessage: TIdSipMessage;
                           E: Exception;
                           const Reason: String);
    procedure NotifyOfTransportAddition(Transport: TIdSipTransport);
    procedure NotifyOfTransportRemoval(Transport: TIdSipTransport);
    procedure SetDefaultPreferredTransportType(Value: String);
    function  TransactionAt(Index: Cardinal): TIdSipTransaction;
    function  TransportAt(Index: Cardinal): TIdSipTransport;
    procedure TryResendMessage(FailedMessage: TIdSipMessage;
                               E: Exception;
                               const Reason: String);
  protected
    function  FindAppropriateTransport(Dest: TIdSipLocation): TIdSipTransport;
    procedure NotifyOfTransportException(FailedMessage: TIdSipMessage;
                                         E: Exception;
                                         const Reason: String);
    procedure NotifyOfException(FailedMessage: TIdSipMessage;
                                E: Exception;
                                const Reason: String);
    procedure NotifyOfRequest(Request: TIdSipRequest;
                              Binding: TIdConnectionBindings);
    procedure NotifyOfResponse(Response: TIdSipResponse;
                               Binding: TIdConnectionBindings);

    // IIdSipTransactionListener
    procedure OnFail(Transaction: TIdSipTransaction;
                     FailedMessage: TIdSipMessage;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Binding: TIdConnectionBindings); overload;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Binding: TIdConnectionBindings); overload;
    procedure OnTerminated(Transaction: TIdSipTransaction);

    // IIdSipTransportListener
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings); overload; virtual;
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings); overload; virtual;
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure SetLocator(Value: TIdSipAbstractLocator);
    procedure SetRoutingTable(Value: TIdRoutingTable);
    procedure SetTimer(Value: TIdTimerQueue);
  public
    constructor Create(Timer: TIdTimerQueue;
                       Locator: TIdSipAbstractLocator); virtual;
    destructor  Destroy; override;

    procedure AddTransactionDispatcherListener(const Listener: IIdSipTransactionDispatcherListener);
    procedure AddTransportBinding(const Transport: String;
                                  const Address: String;
                                  Port: Cardinal);
    procedure AddTransportListener(Listener: IIdSipTransportListener; Priority: Integer = 0);
    function  AddClientTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
    procedure AddManagementListener(Listener: IIdSipTransportManagementListener);
    function  AddServerTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
    procedure ClearAllPreferredTransportTypes;
    procedure ClearTransports;
    procedure FindServersFor(Response: TIdSipResponse;
                             Result: TIdSipLocations);
    function  FindTransaction(R: TIdSipMessage;
                              ClientTran: Boolean): TIdSipTransaction;
    procedure LocalBindings(Bindings: TIdSipLocations);
    procedure Log(Description: String;
                  Severity: TSeverityLevel;
                  EventRef: Cardinal;
                  DebugInfo: String);
    function  LoopDetected(Request: TIdSipRequest): Boolean;
    function  PreferredTransportTypeFor(Address: String): String;
    procedure RemoveManagementListener(Listener: IIdSipTransportManagementListener);
    procedure RemovePreferredTransportTypeFor(AddressSpace: String);
    procedure RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
    procedure RemoveTransactionDispatcherListener(const Listener: IIdSipTransactionDispatcherListener);
    procedure RemoveTransportListener(Listener: IIdSipTransportListener);
    procedure ScheduleEvent(Event: TIdWait;
                            WaitTime: Cardinal); overload;
    procedure SendToTransport(Msg: TIdSipMessage;
                              Dest: TIdSipLocation); virtual;
    procedure SendRequest(Request: TIdSipRequest;
                          Dest: TIdSipLocation); virtual;
    procedure SendResponse(Response: TIdSipResponse); virtual;
    procedure SetPreferredTransportTypeFor(AddressSpace: String; TransportType: String);
    procedure StartAllTransports;
    procedure StopAllTransports;
    function  TransactionCount: Integer;
    function  TransportCount: Integer;
    function  WillUseReliableTranport(R: TIdSipMessage): Boolean;

    property DefaultPreferredTransportType: String                read GetDefaultPreferredTransportType write SetDefaultPreferredTransportType;
    property Locator:                       TIdSipAbstractLocator read fLocator write fLocator;
    property RoutingTable:                  TIdRoutingTable       read fRoutingTable write SetRoutingTable;
    property T1Interval:                    Cardinal              read fT1Interval write fT1Interval;
    property T2Interval:                    Cardinal              read fT2Interval write fT2Interval;
    property T4Interval:                    Cardinal              read fT4Interval write fT4Interval;
    property Timer:                         TIdTimerQueue         read fTimer;
    property Transports:                    TIdSipTransports      read fTransports;
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
    fInitialRequest:     TIdSipRequest;
    fState:              TIdSipTransactionState;
    fDispatcher:         TIdSipTransactionDispatcher;
    fLastResponse:       TIdSipResponse; // Used by server transactions
    fInitialDestination: TIdSipLocation; // Used by client transactions
    TranListeners:       TIdNotificationList;
  protected
    FirstTime: Boolean;

    procedure ChangeToCompleted; overload; virtual;
    procedure ChangeToCompleted(R: TIdSipResponse;
                                Binding: TIdConnectionBindings); overload; virtual;
    procedure ChangeToProceeding; overload; virtual;
    procedure ChangeToProceeding(R: TIdSipRequest;
                                 Binding: TIdConnectionBindings); overload;
    procedure ChangeToProceeding(R: TIdSipResponse;
                                 Binding: TIdConnectionBindings); overload;
    procedure ChangeToTerminated(Quiet: Boolean); overload;
    procedure ChangeToTerminated(R: TIdSipResponse;
                                 Binding: TIdConnectionBindings); overload; virtual;
    procedure DoOnTimeout(Request: TIdSipRequest;
                          const Reason: String);
    procedure LogCreation(InitialRequest: TIdSipRequest);
    procedure LogReceivedMessage(Message: TIdSipMessage;
                                 Binding: TIdConnectionBindings);
    procedure LogSentMessage(Message: TIdSipMessage;
                             Target: TIdSipLocation);
    procedure LogStateChange(OldState, NewState: TIdSipTransactionState);
    procedure NotifyOfFailure(const Reason: String); overload; virtual;
    procedure NotifyOfFailure(FailedMessage: TIdSipMessage;
                              const Reason: String); overload;
    procedure NotifyOfRequest(R: TIdSipRequest;
                              Binding: TIdConnectionBindings);
    procedure NotifyOfResponse(R: TIdSipResponse;
                               Binding: TIdConnectionBindings);
    procedure NotifyOfTermination;
    procedure SetState(Value: TIdSipTransactionState); virtual;
    procedure TryResendInitialRequest;
    procedure TrySendRequest(R: TIdSipRequest;
                             Dest: TIdSipLocation);
  public
    class function CreateClientTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                               Request: TIdSipRequest): TIdSipTransaction;
    class function CreateServerTransactionType(Dispatcher: TIdSipTransactionDispatcher;
                                               Request: TIdSipRequest): TIdSipTransaction;

    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); virtual;
    destructor  Destroy; override;

    procedure AddTransactionListener(const Listener: IIdSipTransactionListener);
    procedure DoOnTransportError(Msg: TIdSipMessage;
                                 const Reason: String); virtual;
    procedure ExceptionRaised(E: Exception);
    function  IsClient: Boolean; virtual;
    function  IsInvite: Boolean; virtual;
    function  IsNull: Boolean; virtual;
    function  IsServer: Boolean;
    function  IsTerminated: Boolean;
    function  Match(Msg: TIdSipMessage): Boolean; virtual;
    procedure Log(Description: String;
                  Severity: TSeverityLevel;
                  EventRef: Cardinal;
                  DebugInfo: String);
    function  LoopDetected(Request: TIdSipRequest): Boolean;
    procedure ReceiveRequest(R: TIdSipRequest;
                             Binding: TIdConnectionBindings); virtual;
    procedure ReceiveResponse(R: TIdSipResponse;
                              Binding: TIdConnectionBindings); virtual;
    procedure RemoveTransactionListener(const Listener: IIdSipTransactionListener);
    procedure SendRequest(Dest: TIdSipLocation); virtual;
    procedure SendResponse(R: TIdSipResponse); virtual;
    procedure Terminate(Quiet: Boolean);

    property Dispatcher:         TIdSipTransactionDispatcher read fDispatcher;
    property InitialRequest:     TIdSipRequest               read fInitialRequest;
    property InitialDestination: TIdSipLocation              read fInitialDestination;
    property LastResponse:       TIdSipResponse              read fLastResponse;
    property State:              TIdSipTransactionState      read fState;
  end;

  // Some notes on sending responses: According to RFC 3261 section 18.2.2 and
  // RFC 3263 section 5, a response can be sent to several possible locations,
  // depending on its topmost Via's sent-by.
  //
  // TrySendResponse, TrySendResponseTo and DoOnTransportError implement an
  // asynchronous algorithm to send these responses:
  // First we store the DNS query results from RFC 3261/3263's algorithms in
  // TargetLocations. Then when the Transaction-User layer sends a response, we
  // copy this set of locations to an entry in SentResponses. SentResponses
  // stores those locations, per response, that have not yet been attempted.
  // TrySendResponse sends the response to the first location in
  // TargetLocations, using TrySendResponseTo. Should that attempt fail, the
  // TransactionDispatcher will try the next location, again using
  // TrySendResponseTo. If no locations succeed, the transaction fails, and is
  // terminated.
  //
  // This algorithm is almostthe same algorithm as the Transaction-User
  // TIdSipAction uses to send requests.
  TIdSipServerTransaction = class(TIdSipTransaction)
  private
    procedure StoreResponseLocations(R: TIdSipResponse);
  protected
    TargetLocations:  TIdSipLocations;
    LastResponseSent: TIdSipResponse;
    SentResponses:    TIdSipResponseLocationsList;

    procedure TrySendLastResponse; virtual;
    procedure TrySendResponse(R: TIdSipResponse); virtual;
    procedure TrySendResponseTo(R: TIdSipResponse;
                                Dest: TIdSipLocation); virtual;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    destructor  Destroy; override;

    procedure DoOnTransportError(Msg: TIdSipMessage;
                                 const Reason: String); override;
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure NotifyOfFailure(const Reason: String); overload; override;
    procedure SendResponse(R: TIdSipResponse); override;
  end;

  TIdSipServerInviteTransaction = class(TIdSipServerTransaction)
  private
    fTimerGInterval: Cardinal;
    HasSentResponse: Boolean;

    procedure ChangeToConfirmed(R: TIdSipRequest);
    procedure ScheduleTimerG;
    procedure ScheduleTimerH;
    procedure ScheduleTimerI;
  protected
    procedure ChangeToCompleted; override;
    procedure TrySendLastResponse; override;
    procedure TrySendResponseTo(R: TIdSipResponse;
                                Dest: TIdSipLocation); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    procedure FireTimerG;
    procedure FireTimerH;
    procedure FireTimerI;
    function  IsInvite: Boolean; override;
    procedure ReceiveRequest(R: TIdSipRequest;
                             Binding: TIdConnectionBindings); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  TimerGInterval: Cardinal;
    function  TimerHInterval: Cardinal;
    function  TimerIInterval: Cardinal;
  end;

  TIdSipServerNonInviteTransaction = class(TIdSipServerTransaction)
  private
    procedure ChangeToTrying;
    procedure ScheduleTimerJ;
  protected
    procedure TrySendResponseTo(R: TIdSipResponse;
                                Dest: TIdSipLocation); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;
    procedure FireTimerJ;
    procedure ReceiveRequest(R: TIdSipRequest;
                             Binding: TIdConnectionBindings); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  TimerJInterval: Cardinal;
  end;

  TIdSipClientTransaction = class(TIdSipTransaction)
  protected
    LastTriedLocation: TIdSipLocation;
  public
    function  IsClient: Boolean; override;
    procedure NotifyOfFailure(const Reason: String); overload; override;
    procedure SendRequest(Dest: TIdSipLocation); override;
  end;

  TIdSipClientInviteTransaction = class(TIdSipClientTransaction)
  private
    fTimerAInterval: Cardinal;

    procedure ChangeToCalling;
    procedure ScheduleTimerA;
    procedure ScheduleTimerB;
    procedure ScheduleTimerD;
    procedure TrySendACK(R: TIdSipResponse);
  protected
    procedure ChangeToCompleted(R: TIdSipResponse;
                                Binding: TIdConnectionBindings); override;
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    procedure FireTimerA;
    procedure FireTimerB;
    procedure FireTimerD;
    function  IsInvite: Boolean; override;
    procedure ReceiveResponse(R: TIdSipResponse;
                              Binding: TIdConnectionBindings); override;
    procedure SendRequest(Dest: TIdSipLocation); override;
    function  TimerAInterval: Cardinal;
    function  TimerBInterval: Cardinal;
    function  TimerDInterval: Cardinal;
  end;

  TIdSipClientNonInviteTransaction = class(TIdSipClientTransaction)
  private
    fTimerEInterval: Cardinal;

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
    procedure ReceiveResponse(R: TIdSipResponse;
                              Binding: TIdConnectionBindings); override;
    procedure SendRequest(Dest: TIdSipLocation); override;
    function  TimerEInterval: Cardinal;
    function  TimerFInterval: Cardinal;
    function  TimerKInterval: Cardinal;
  end;

  TIdSipNullTransaction = class(TIdSipTransaction)
  public
    constructor Create(Dispatcher: TIdSipTransactionDispatcher;
                       InitialRequest: TIdSipRequest); override;

    function IsNull: Boolean; override;
  end;

  TIdSipTransactionWait = class(TIdWait)
  private
    fTransactionID: String;
    Transaction:    TIdSipTransaction;

    procedure LogToTransactionLayer(Severity: TSeverityLevel;
                                    SourceDescription: String;
                                    RefCode: Cardinal;
                                    Description,
                                    BinaryData: String);
  protected
    procedure AfterFiredTimer(Tran: TIdSipTransaction); virtual;
    procedure LogTrigger; override;
    function  TimerName: String; virtual;
    procedure FireTimer(Tran: TIdSipTransaction); virtual;
  public
    procedure Trigger; override;

    property TransactionID: String read fTransactionID write fTransactionID;
  end;

  TIdSipTerminatingTransactionWait = class(TIdSipTransactionWait)
  protected
    procedure AfterFiredTimer(Tran: TIdSipTransaction); override;
  end;

  TIdSipServerInviteTransactionTimerGWait = class(TIdSipTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function TimerName: String; override;
  end;

  TIdSipServerInviteTransactionTimerHWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipServerInviteTransactionTimerIWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipServerNonInviteTransactionTimerJWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientInviteTransactionTimerAWait = class(TIdSipTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientInviteTransactionTimerBWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientInviteTransactionTimerDWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientNonInviteTransactionTimerEWait = class(TIdSipTransactionWait)
  public
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientNonInviteTransactionTimerFWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipClientNonInviteTransactionTimerKWait = class(TIdSipTerminatingTransactionWait)
  protected
    procedure FireTimer(Tran: TIdSipTransaction); override;
    function  TimerName: String; override;
  end;

  TIdSipResponseLocationsList = class(TObject)
  private
    Responses: TIdSipResponseList;
    Locations: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Response: TIdSipResponse;
                  Locations: TIdSipLocations);
    function  Contains(Response: TIdSipResponse): Boolean;
    function  LocationsFor(Response: TIdSipResponse): TIdSipLocations;
  end;

  TIdSipTransactionDispatcherMethod = class(TIdNotification)
  private
    fBinding: TIdConnectionBindings;
  public
    property Binding: TIdConnectionBindings read fBinding write fBinding;
  end;

  TIdSipTransactionDispatcherListenerFailedSendMethod = class(TIdSipTransactionDispatcherMethod)
  private
    fFailedMessage: TIdSipMessage;
    fReason:        String;
  public
    procedure Run(const Subject: IInterface); override;

    property FailedMessage: TIdSipMessage read fFailedMessage write fFailedMessage;
    property Reason:        String        read fReason write fReason;
  end;

  TIdSipTransactionDispatcherListenerReceiveRequestMethod = class(TIdSipTransactionDispatcherMethod)
  private
    fRequest: TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Request: TIdSipRequest read fRequest write fRequest;
  end;

  TIdSipTransactionDispatcherListenerReceiveResponseMethod = class(TIdSipTransactionDispatcherMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipTransportManagementMethod = class(TIdNotification)
  private
    fTransport: TIdSipTransport;
  public
    property Transport: TIdSipTransport read fTransport write fTransport;
  end;

  TIdSipTransportAddedMethod = class(TIdSipTransportManagementMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSipTransportRemovedMethod = class(TIdSipTransportManagementMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;


  TIdSipTransactionMethod = class(TIdNotification)
  private
    fTransaction: TIdSipTransaction;
  public
    property Transaction: TIdSipTransaction read fTransaction write fTransaction;
  end;

  TIdSipTransactionListenerFailMethod = class(TIdSipTransactionMethod)
  private
    fFailedMessage: TIdSipMessage;
    fReason:        String;
  public
    procedure Run(const Subject: IInterface); override;

    property FailedMessage: TIdSipMessage read fFailedMessage write fFailedMessage;
    property Reason:        String        read fReason write fReason;
  end;

  TIdSipTransactionListenerReceiveRequestMethod = class(TIdSipTransactionMethod)
  private
    fBinding:  TIdConnectionBindings;
    fRequest:  TIdSipRequest;
  public
    procedure Run(const Subject: IInterface); override;

    property Binding: TIdConnectionBindings read fBinding write fBinding;
    property Request: TIdSipRequest            read fRequest write fRequest;
  end;

  TIdSipTransactionListenerReceiveResponseMethod = class(TIdSipTransactionMethod)
  private
    fBinding:  TIdConnectionBindings;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Binding:  TIdConnectionBindings read fBinding write fBinding;
    property Response: TIdSipResponse           read fResponse write fResponse;
  end;

  TIdSipTransactionListenerTerminatedMethod = class(TIdSipTransactionMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

const
  DefaultT1    = 500;   // milliseconds
  DefaultT1_64 = 64*DefaultT1;
  DefaultT2    = 4000;  // milliseconds
  DefaultT4    = 5000;  // milliseconds

const
  SessionTimeoutMsg = 'Timed out';

function StateToStr(S: TIdSipTransactionState): String;

implementation

uses
  IdException, IdRegisteredObject, IdSipDialogID, Math, RuntimeSafety, TypInfo;

const
  AtLeastOneVia         = 'Messages must have at least one Via header';
  CantFindTransport     = 'The dispatcher cannot find a %s transport for a '
                        + 'message';
  ExceptionRaised       = '%s raised an %s: %s';
  MustGenerateNewBranch = 'You must generate a new transaction branch.';
  MustHaveAuthorization = 'You must supply an Authorization or '
                        + 'Proxy-Authorization header';
  NoLocatorAssigned     = 'You must assign a Locator to the TransactionDispatcher';
  OnlyRemoveTranWhenTerminated
                        = 'Transactions must only be removed when they''re '
                        + 'terminated';

const
  RSNoLocationFound     = 'No destination addresses found for URI %s';
  RSNoLocationSucceeded = 'Attempted message sends to all destination addresses failed for URI %s';

//******************************************************************************
//* Unit Public functions & procedures                                         *
//******************************************************************************

function StateToStr(S: TIdSipTransactionState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipTransactionState), Integer(S));
end;

//******************************************************************************
//* TIdSipTransactionDispatcher                                                *
//******************************************************************************
//* TIdSipTransactionDispatcher Public methods *********************************

constructor TIdSipTransactionDispatcher.Create(Timer: TIdTimerQueue;
                                               Locator: TIdSipAbstractLocator);
begin
  inherited Create;

  Self.SetLocator(Locator);
  Self.SetTimer(Timer);

  Self.ManagementListeners := TIdNotificationList.Create;
  Self.MsgListeners        := TIdNotificationList.Create;

  Self.fTransports := TIdSipTransports.Create;

  // To hold state on those responses that do not attach to transactions (i.e.,
  // retransmissions of 200 OKs to INVITEs).
  Self.TransactionlessResponses := TIdSipResponseLocationsList.Create;

  Self.Transactions   := TObjectList.Create(true);
  Self.TransportTypes := TIdSipTransportSpecifiers.Create;

  Self.T1Interval := DefaultT1;
  Self.T2Interval := DefaultT2;
  Self.T4Interval := DefaultT4;
end;

destructor TIdSipTransactionDispatcher.Destroy;
begin
  Self.TransportTypes.Free;
  Self.Transactions.Free;
  Self.TransactionlessResponses.Free;
  Self.Transports.Free;

  Self.MsgListeners.Free;
  Self.ManagementListeners.Free;

  inherited Destroy;
end;

procedure TIdSipTransactionDispatcher.AddTransactionDispatcherListener(const Listener: IIdSipTransactionDispatcherListener);
begin
  Self.MsgListeners.AddListener(Listener);
end;

procedure TIdSipTransactionDispatcher.AddTransportBinding(const Transport: String;
                                                          const Address: String;
                                                          Port: Cardinal);
var
  I: Integer;
  T: TIdSipTransport;
begin
  T := nil;
  I := 0;
  while (I < Self.Transports.Count) and not Assigned(T) do begin
    if IsEqual(Transport, Self.Transports[I].GetTransportType) then
      T := Self.Transports[I]
    else
      Inc(I);
  end;

  if (I < Self.Transports.Count) then
    T.AddBinding(Address, Port)
  else begin
    T := TIdSipTransportRegistry.TransportTypeFor(Transport).Create;
    T.HostName     := Address;
    T.Timer        := Self.Timer;
    T.RoutingTable := Self.RoutingTable;

    // Indy servers instantiate with one binding.
    T.SetFirstBinding(Address, Port);

    Self.Transports.Add(T);
    T.AddTransportListener(Self);
    Self.NotifyOfTransportAddition(T);
  end;
end;

procedure TIdSipTransactionDispatcher.AddTransportListener(Listener: IIdSipTransportListener; Priority: Integer = 0);
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    Self.Transports[I].AddTransportListener(Listener, Priority);
end;

function TIdSipTransactionDispatcher.AddClientTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
begin
  Result := nil;

  try
    Result := TIdSipTransaction.CreateClientTransactionType(Self, InitialRequest);
    Result.AddTransactionListener(Self);
    Self.Transactions.Add(Result);
  except
    if (Self.Transactions.IndexOf(Result) <> ItemNotFoundIndex) then
      Self.Transactions.Remove(Result)
    else
      Result.Free;

    raise;
  end;
end;

procedure TIdSipTransactionDispatcher.AddManagementListener(Listener: IIdSipTransportManagementListener);
begin
  Self.ManagementListeners.AddListener(Listener);
end;

function TIdSipTransactionDispatcher.AddServerTransaction(InitialRequest: TIdSipRequest): TIdSipTransaction;
begin
  Result := nil;

  try
    Result := TIdSipTransaction.CreateServerTransactionType(Self, InitialRequest);
    Result.AddTransactionListener(Self);
    Self.Transactions.Add(Result);
  except
    if (Self.Transactions.IndexOf(Result) <> ItemNotFoundIndex) then
      Self.Transactions.Remove(Result)
    else
      Result.Free;

    raise;
  end;
end;

procedure TIdSipTransactionDispatcher.ClearAllPreferredTransportTypes;
begin
  Self.TransportTypes.ClearAllParameters;
end;

procedure TIdSipTransactionDispatcher.ClearTransports;
var
  I: Integer;
begin
  // This looks a tad lame. We don't have a Self.RemoveTransportBinding though,
  // which would otherwise be the better place for a notification.
  for I := 0 to Self.Transports.Count - 1 do
    Self.NotifyOfTransportRemoval(Self.Transports[I]);

  Self.Transports.Clear;
end;

procedure TIdSipTransactionDispatcher.FindServersFor(Response: TIdSipResponse;
                                                     Result: TIdSipLocations);
begin
  if not Assigned(Self.Locator) then
    raise Exception.Create(NoLocatorAssigned);

  Self.Locator.FindServersFor(Response, Result);
end;

function TIdSipTransactionDispatcher.FindTransaction(R: TIdSipMessage;
                                                     ClientTran: Boolean): TIdSipTransaction;
var
  I: Integer;
begin
  Result := nil;

  I := 0;
  while (I < Self.Transactions.Count) and not Assigned(Result) do
    if (Self.TransactionAt(I).IsClient = ClientTran)
      and Self.TransactionAt(I).Match(R) then
      Result := Self.TransactionAt(I)
    else
     Inc(I);
end;

procedure TIdSipTransactionDispatcher.LocalBindings(Bindings: TIdSipLocations);
var
  I: Integer;
begin
  for I := 0 to Self.TransportCount - 1 do
    Self.TransportAt(I).LocalBindings(Bindings);
end;

procedure TIdSipTransactionDispatcher.Log(Description: String;
                                          Severity: TSeverityLevel;
                                          EventRef: Cardinal;
                                          DebugInfo: String);
begin
  LogEntry(Description, Self.ClassName, Severity, EventRef, DebugInfo);
end;

function TIdSipTransactionDispatcher.LoopDetected(Request: TIdSipRequest): Boolean;
var
  I: Integer;
begin
  // cf. RFC 3261 section 8.2.2.2
  Result := false;

  I := 0;
  while (I < Self.Transactions.Count) and not Result do begin
    if Self.TransactionAt(I).IsServer then
      Result := Self.TransactionAt(I).LoopDetected(Request);
    Inc(I);
  end;
end;

function TIdSipTransactionDispatcher.PreferredTransportTypeFor(Address: String): String;
begin
  Result := Self.TransportTypes.TransportTypeFor(Address)
end;

procedure TIdSipTransactionDispatcher.RemoveManagementListener(Listener: IIdSipTransportManagementListener);
begin
  Self.ManagementListeners.RemoveListener(Listener);
end;

procedure TIdSipTransactionDispatcher.RemovePreferredTransportTypeFor(AddressSpace: String);
begin
  Self.TransportTypes.RemoveTransportFor(AddressSpace);
end;

procedure TIdSipTransactionDispatcher.RemoveTransaction(TerminatedTransaction: TIdSipTransaction);
begin
  // Three reasons why a transaction would terminate:
  //   1. one of its timers fired,
  //   2. a transport error occurred, or
  //   3. RFC 3261 says the transaction terminates.
  // Transport errors can only occur in Self.Send(Request|Response); timers
  // happen in <transaction type>Transaction<timer name>Wait objects -
  // ClientInviteTransactionTimerA etc.; 2xx responses terminate client INVITE
  // transactions.

  Assert(TerminatedTransaction.IsTerminated,
         OnlyRemoveTranWhenTerminated);

   Self.Transactions.Remove(TerminatedTransaction);
end;

procedure TIdSipTransactionDispatcher.RemoveTransactionDispatcherListener(const Listener: IIdSipTransactionDispatcherListener);
begin
  Self.MsgListeners.RemoveListener(Listener);
end;

procedure TIdSipTransactionDispatcher.RemoveTransportListener(Listener: IIdSipTransportListener);
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    Self.Transports[I].RemoveTransportListener(Listener);
end;

procedure TIdSipTransactionDispatcher.ScheduleEvent(Event: TIdWait;
                                                    WaitTime: Cardinal);
begin
  if Assigned(Self.Timer) then
    Self.Timer.AddEvent(WaitTime, Event);
end;

procedure TIdSipTransactionDispatcher.SendToTransport(Msg: TIdSipMessage;
                                                      Dest: TIdSipLocation);
var
  T: TIdSipTransport;
begin
  T := Self.FindAppropriateTransport(Dest);

  if Assigned(T) then
    T.Send(Msg, Dest)
  else
    raise Exception.Create('What do we do when the dispatcher can''t find a Transport?');
end;

procedure TIdSipTransactionDispatcher.SendRequest(Request: TIdSipRequest;
                                                  Dest: TIdSipLocation);
var
  Tran: TIdSipTransaction;
begin
  if Request.IsAck then begin
    // Since ACKs to 200s live outside of transactions, we must send the
    // ACK directly to the transport layer.
    Self.SendToTransport(Request, Dest);
  end
  else begin
    Tran := Self.FindTransaction(Request, true);

    if not Assigned(Tran) then
      Tran := Self.AddClientTransaction(Request);

    Tran.SendRequest(Dest);

    if Tran.IsTerminated then
      Self.RemoveTransaction(Tran);
  end;
end;

procedure TIdSipTransactionDispatcher.SendResponse(Response: TIdSipResponse);
var
  CurrentTarget:      TIdSipLocation;
  Destinations:       TIdSipLocations;
  LocalAddress:       TIdSipLocation;
  LocalBindings:      TIdSipLocations;
  Tran:               TIdSipTransaction;
  RemainingLocations: TIdSipLocations;
begin
  Tran := Self.FindTransaction(Response, false);

  if Assigned(Tran) then begin
    Tran.SendResponse(Response);
    if Tran.IsTerminated then
      Self.RemoveTransaction(Tran);
  end
  else begin
    // We send responses to non-existent transactions when, for instance, we
    // resend OK responses to an INVITE: the first one terminates the
    // transaction, but the Transaction-User layer resend the OKs until we
    // receive an ACK (cf. RFC 3261, section 13.3.1.4).

    if Self.TransactionlessResponses.Contains(Response) then begin
      // We're being asked to resend a response, for instance as part of the
      // 200 OK retransmissions of a TIdSipInboundInvite.
      Self.Locator.FindServersFor(Response,
                                  Self.TransactionlessResponses.LocationsFor(Response));
    end
    else begin
      Destinations := TIdSipLocations.Create;
      try
        Self.Locator.FindServersFor(Response, Destinations);
        Self.TransactionlessResponses.Add(Response, Destinations);
      finally
        Destinations.Free;
      end;
    end;

    RemainingLocations := Self.TransactionlessResponses.LocationsFor(Response);

    if RemainingLocations.IsEmpty then begin
      Self.NotifyOfException(Response,
                             nil,
                             Format(RSNoLocationSucceeded, [Response.LastHop.SentBy]));
    end
    else begin
      LocalBindings := TIdSipLocations.Create;
      try
        Self.LocalBindings(LocalBindings);
        CurrentTarget := RemainingLocations.First.Copy;
        try
          RemainingLocations.RemoveFirst;

          LocalAddress := TIdSipLocation.Create;
          try
            Self.RoutingTable.BestLocalAddress(LocalBindings, CurrentTarget, LocalAddress);
            Response.RewriteLocationHeaders(LocalAddress);
            Self.SendToTransport(Response, CurrentTarget);
          finally
            LocalAddress.Free;
          end;
        finally
          CurrentTarget.Free;
        end;
      finally
        LocalBindings.Free;
      end;
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.SetPreferredTransportTypeFor(AddressSpace: String; TransportType: String);
begin
  // TransportType contains any value valid for a URI "transport" parameter.
  Self.TransportTypes.SetTransportFor(AddressSpace, TransportType);
end;

procedure TIdSipTransactionDispatcher.StartAllTransports;
var
  I: Integer;
begin
  for I := 0 to Self.TransportCount - 1 do
    Self.Transports[I].Start;
end;

procedure TIdSipTransactionDispatcher.StopAllTransports;
var
  I: Integer;
begin
  for I := 0 to Self.TransportCount - 1 do
    Self.Transports[I].Stop;
end;

function TIdSipTransactionDispatcher.TransactionCount: Integer;
begin
  Result := Self.Transactions.Count;
end;

function TIdSipTransactionDispatcher.TransportCount: Integer;
begin
  Result := Self.Transports.Count;
end;

function TIdSipTransactionDispatcher.WillUseReliableTranport(R: TIdSipMessage): Boolean;
begin
  Assert(R.Path.Length > 0, AtLeastOneVia);

  Result := R.LastHop.Transport <> UdpTransport;
end;

//* TIdSipTransactionDispatcher Protected methods ******************************

function TIdSipTransactionDispatcher.FindAppropriateTransport(Dest: TIdSipLocation): TIdSipTransport;
var
  I: Integer;
begin
  I := 0;

  while (I < Self.Transports.Count)
    and (Self.TransportAt(I).GetTransportType <> Dest.Transport) do
    Inc(I);

  // What should we do if there are no appropriate transports to use?
  // It means that someone didn't configure the dispatcher properly,
  // most likely.
  if (I < Self.Transports.Count) then
    Result := Self.TransportAt(I)
  else
    raise EUnknownTransport.Create(Format(CantFindTransport,
                                          [Dest.Transport]));
end;

procedure TIdSipTransactionDispatcher.NotifyOfTransportException(FailedMessage: TIdSipMessage;
                                                                 E: Exception;
                                                                 const Reason: String);
var
  Tran: TIdSipTransaction;
begin
  Self.LogException(FailedMessage, E, Reason);

  Tran := Self.FindTransaction(FailedMessage, FailedMessage.IsRequest);

  if Assigned(Tran) then begin
    Tran.DoOnTransportError(FailedMessage, Reason);
    Self.NotifyOfException(FailedMessage, E, Reason);
  end
  else
    Self.TryResendMessage(FailedMessage, E, Reason);
end;

procedure TIdSipTransactionDispatcher.NotifyOfException(FailedMessage: TIdSipMessage;
                                                        E: Exception;
                                                        const Reason: String);
var
  Notification: TIdSipTransactionDispatcherListenerFailedSendMethod;
begin
  Notification := TIdSipTransactionDispatcherListenerFailedSendMethod.Create;
  try
    Notification.FailedMessage := FailedMessage;
    Notification.Reason        := Reason;

    Self.MsgListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyOfRequest(Request: TIdSipRequest;
                                                      Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransactionDispatcherListenerReceiveRequestMethod;
begin
  Notification := TIdSipTransactionDispatcherListenerReceiveRequestMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Request := Request;

    Self.MsgListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyOfResponse(Response: TIdSipResponse;
                                                       Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransactionDispatcherListenerReceiveResponseMethod;
begin
  Notification := TIdSipTransactionDispatcherListenerReceiveResponseMethod.Create;
  try
    Notification.Binding  := Binding;
    Notification.Response := Response;

    Self.MsgListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.OnFail(Transaction: TIdSipTransaction;
                                             FailedMessage: TIdSipMessage;
                                             const Reason: String);
begin
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                       Transaction: TIdSipTransaction;
                                                       Binding: TIdConnectionBindings);
begin
  Self.NotifyOfRequest(Request, Binding);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                        Transaction: TIdSipTransaction;
                                                        Binding: TIdConnectionBindings);
begin
  Self.NotifyOfResponse(Response, Binding);
end;

procedure TIdSipTransactionDispatcher.OnTerminated(Transaction: TIdSipTransaction);
begin
end;

procedure TIdSipTransactionDispatcher.OnException(FailedMessage: TIdSipMessage;
                                                  E: Exception;
                                                  const Reason: String);
begin
  Self.NotifyOfTransportException(FailedMessage, E, Reason);
end;

procedure TIdSipTransactionDispatcher.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport;
                                                       Source: TIdConnectionBindings);
begin
  Self.DeliverToTransaction(Request, Source);
end;

procedure TIdSipTransactionDispatcher.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport;
                                                        Source: TIdConnectionBindings);
begin
  Self.DeliverToTransaction(Response, Source);
end;

procedure TIdSipTransactionDispatcher.OnRejectedMessage(const Msg: String;
                                                        const Reason: String;
                                                        Source: TIdConnectionBindings);
begin
end;

procedure TIdSipTransactionDispatcher.SetLocator(Value: TIdSipAbstractLocator);
begin
  Self.fLocator := Value;
end;

procedure TIdSipTransactionDispatcher.SetRoutingTable(Value: TIdRoutingTable);
var
  I: Integer;
begin
  Self.fRoutingTable := Value;

  for I := 0 to Self.TransportCount - 1 do
    Self.Transports[I].RoutingTable := Value;
end;

procedure TIdSipTransactionDispatcher.SetTimer(Value: TIdTimerQueue);
begin
  Self.fTimer := Value;
end;

//* TIdSipTransactionDispatcher Private methods ********************************

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Request: TIdSipRequest;
                                                           Binding: TIdConnectionBindings);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Request, false);

  if Assigned(Tran) then begin
    Tran.ReceiveRequest(Request, Binding);
  end
  else begin
    // An ACK does not belong inside a transaction when a UAS sends a 2xx in
    // response to an INVITE
    if Request.IsAck then begin
      Self.NotifyOfRequest(Request, Binding);
    end
    else begin
      Tran := Self.AddServerTransaction(Request);
      Tran.ReceiveRequest(Request, Binding);
    end;
  end;
end;

procedure TIdSipTransactionDispatcher.DeliverToTransaction(Response: TIdSipResponse;
                                                           Binding: TIdConnectionBindings);
var
  Tran: TIdSipTransaction;
begin
  Tran := Self.FindTransaction(Response, true);

  if Assigned(Tran) then begin
    Tran.ReceiveResponse(Response, Binding);

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
    Self.NotifyOfResponse(Response, Binding);
  end;
end;

function TIdSipTransactionDispatcher.GetDefaultPreferredTransportType: String;
begin
  Result := Self.TransportTypes.DefaultTransportType;
end;

procedure TIdSipTransactionDispatcher.LogException(FailedMessage: TIdSipMessage;
                                                   E: Exception;
                                                   const Reason: String);
const
  LogMsg = '%s sending %s: %s';
begin
  Self.Log(Format(LogMsg, [E.ClassName, FailedMessage.Description, Reason]),
           slError,
           LogEventException,
           FailedMessage.AsString);
end;

procedure TIdSipTransactionDispatcher.NotifyOfTransportAddition(Transport: TIdSipTransport);
var
  Notification: TIdSipTransportAddedMethod;
begin
  Notification := TIdSipTransportAddedMethod.Create;
  try
    Notification.Transport := Transport;

    Self.ManagementListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.NotifyOfTransportRemoval(Transport: TIdSipTransport);
var
  Notification: TIdSipTransportRemovedMethod;
begin
  Notification := TIdSipTransportRemovedMethod.Create;
  try
    Notification.Transport := Transport;

    Self.ManagementListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransactionDispatcher.SetDefaultPreferredTransportType(Value: String);
begin
  Self.TransportTypes.DefaultTransportType := Value;
end;

function TIdSipTransactionDispatcher.TransactionAt(Index: Cardinal): TIdSipTransaction;
begin
  Result := Self.Transactions[Index] as TIdSipTransaction;
end;

function TIdSipTransactionDispatcher.TransportAt(Index: Cardinal): TIdSipTransport;
begin
  Result := Self.Transports[Index] as TIdSipTransport;
end;

procedure TIdSipTransactionDispatcher.TryResendMessage(FailedMessage: TIdSipMessage;
                                                       E: Exception;
                                                       const Reason: String);
var
  RemainingLocations: TIdSipLocations;
begin
  if FailedMessage.IsResponse then begin
    if not Self.TransactionlessResponses.Contains(FailedMessage as TIdSipResponse) then begin
      // A response for which the transaction no longer exists. For instance, we
      // receive an INVITE, send a 180 Ringing, send a 200 OK. The 200 OK send
      // succeeds, and then the 180 Ringing send fails. By this time the
      // transaction has terminated. In this case, just forget about the message
      // send, since the response has no effect on the transaction at this
      // point.
      Exit;
    end;

    RemainingLocations := Self.TransactionlessResponses.LocationsFor(FailedMessage as TIdSipResponse);

    if RemainingLocations.IsEmpty then begin
      Self.NotifyOfException(FailedMessage,
                             E,
                             Format(RSNoLocationSucceeded, [FailedMessage.LastHop.SentBy]));
    end
    else begin
      Self.SendToTransport(FailedMessage, RemainingLocations.First);

      RemainingLocations.RemoveFirst;
    end;
  end
  else begin
    // A request, like an ACK to a 200 OK we've earlier received. Sending an
    // ACK happens outside of a transaction!
    Self.NotifyOfException(FailedMessage, E, Reason);
  end;
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
  Self.LogCreation(InitialRequest);
end;

destructor TIdSipTransaction.Destroy;
begin
  Self.TranListeners.Free;
  Self.LastResponse.Free;
  Self.InitialRequest.Free;
  Self.InitialDestination.Free;

  inherited Destroy;
end;

procedure TIdSipTransaction.AddTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListeners.AddListener(Listener);
end;

procedure TIdSipTransaction.DoOnTransportError(Msg: TIdSipMessage;
                                               const Reason: String);
begin
  Self.NotifyOfFailure(Msg, Reason);
  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.ExceptionRaised(E: Exception);
begin
  Self.NotifyOfFailure(Format(IdSipTransaction.ExceptionRaised,
                              [Self.ClassName, E.ClassName, E.Message]));
end;

function TIdSipTransaction.IsClient: Boolean;
begin
  Result := false;
end;

function TIdSipTransaction.IsInvite: Boolean;
begin
  Result := false;
end;

function TIdSipTransaction.IsNull: Boolean;
begin
  Result := false;
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
end;

procedure TIdSipTransaction.Log(Description: String;
                                Severity: TSeverityLevel;
                                EventRef: Cardinal;
                                DebugInfo: String);
begin
  Self.Dispatcher.Log(Description, Severity, EventRef, DebugInfo);
end;

function TIdSipTransaction.LoopDetected(Request: TIdSipRequest): Boolean;
begin
  Result := Request.From.Equals(Self.InitialRequest.From)
       and (Request.CallID = Self.InitialRequest.CallID)
       and (Request.CSeq.Equals(Self.InitialRequest.CSeq))
       and not Self.Match(Request);
end;

procedure TIdSipTransaction.ReceiveRequest(R: TIdSipRequest;
                                           Binding: TIdConnectionBindings);
begin
  // By default we do nothing
end;

procedure TIdSipTransaction.ReceiveResponse(R: TIdSipResponse;
                                            Binding: TIdConnectionBindings);
begin
  // By default we do nothing
end;

procedure TIdSipTransaction.RemoveTransactionListener(const Listener: IIdSipTransactionListener);
begin
  Self.TranListeners.RemoveListener(Listener);
end;

procedure TIdSipTransaction.SendRequest(Dest: TIdSipLocation);
begin
  Self.fInitialDestination := Dest.Copy;
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
                                              Binding: TIdConnectionBindings);
begin
  Self.ChangeToCompleted;

  Self.NotifyOfResponse(R, Binding);
end;

procedure TIdSipTransaction.ChangeToProceeding;
begin
  Self.SetState(itsProceeding);
end;

procedure TIdSipTransaction.ChangeToProceeding(R: TIdSipRequest;
                                               Binding: TIdConnectionBindings);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfRequest(R, Binding);
end;

procedure TIdSipTransaction.ChangeToProceeding(R: TIdSipResponse;
                                               Binding: TIdConnectionBindings);
begin
  Self.ChangeToProceeding;
  Self.NotifyOfResponse(R, Binding);
end;

procedure TIdSipTransaction.ChangeToTerminated(Quiet: Boolean);
begin
  Self.SetState(itsTerminated);

  if not Quiet then
    Self.NotifyOfTermination;
end;

procedure TIdSipTransaction.ChangeToTerminated(R: TIdSipResponse;
                                               Binding: TIdConnectionBindings);
begin
  Self.NotifyOfResponse(R, Binding);
  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.DoOnTimeout(Request: TIdSipRequest;
                                        const Reason: String);
var
  FakeBinding: TIdConnectionBindings;
  Timeout: TIdSipResponse;
begin
  Self.NotifyOfFailure(Reason);

  // Local timeouts look the same as receiving a Request Timeout from the
  // network.

  // This transport should refer to the transport that received
  // Self.InitialRequest for server transactions!
  Timeout := TIdSipResponse.InResponseTo(Request, SIPRequestTimeout);
  try
    FakeBinding := TIdConnectionBindings.Create;
    try
      FakeBinding.LocalIP   := Self.InitialRequest.LastHop.SentBy;
      FakeBinding.LocalPort := Self.InitialRequest.LastHop.Port;
      FakeBinding.Transport := Self.InitialRequest.LastHop.Transport;
//      FakeBinding.PeerIP    := Self.Last

      Self.NotifyOfResponse(Timeout, FakeBinding);
    finally
      FakeBinding.Free;
    end;
  finally
    Timeout.Free;
  end;

  Self.ChangeToTerminated(false);
end;

procedure TIdSipTransaction.LogCreation(InitialRequest: TIdSipRequest);
const
  Msg = 'Transaction %s created with branch-id %s';
begin
  // '' because we'll immediately either LogReceivedMessage or LogSentMessage,
  // which will log InitialRequest.AsString anyway.
  Self.Log(Format(Msg, [Self.ID, InitialRequest.LastHop.Branch]),
           slDebug,
           0,
           '');
end;

procedure TIdSipTransaction.LogReceivedMessage(Message: TIdSipMessage;
                                               Binding: TIdConnectionBindings);
const
  Msg = 'Transaction %s received %s on %s';
begin
  Self.Log(Format(Msg, [Self.ID, Message.Description, Binding.AsString]),
           slDebug,
           0,
           Message.AsString);
end;

procedure TIdSipTransaction.LogSentMessage(Message: TIdSipMessage;
                                           Target: TIdSipLocation);
const
  Msg = 'Transaction %s sent %s to %s';
begin
  Self.Log(Format(Msg, [Self.ID, Message.Description, Target.AsString]),
           slDebug,
           0,
           Message.AsString);
end;

procedure TIdSipTransaction.LogStateChange(OldState, NewState: TIdSipTransactionState);
const
  Msg = 'Transaction %s changed state: %s -> %s';
begin
  Self.Log(Format(Msg, [Self.ID, StateToStr(OldState), StateToStr(NewState)]),
           slDebug,
           0,
           '');
end;

procedure TIdSipTransaction.NotifyOfFailure(const Reason: String);
begin
  RaiseAbstractError(Self.ClassName, 'NotifyOfFailure(String)');
end;

procedure TIdSipTransaction.NotifyOfFailure(FailedMessage: TIdSipMessage;
                                            const Reason: String);
var
  Notification: TIdSipTransactionListenerFailMethod;
begin
  Notification := TIdSipTransactionListenerFailMethod.Create;
  try
    Notification.FailedMessage := FailedMessage;
    Notification.Reason        := Reason;
    Notification.Transaction   := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;


procedure TIdSipTransaction.NotifyOfRequest(R: TIdSipRequest;
                                            Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransactionListenerReceiveRequestMethod;
begin
  Notification := TIdSipTransactionListenerReceiveRequestMethod.Create;
  try
    Notification.Binding     := Binding;
    Notification.Request     := R;
    Notification.Transaction := Self;

    Self.TranListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipTransaction.NotifyOfResponse(R: TIdSipResponse;
                                             Binding: TIdConnectionBindings);
var
  Notification: TIdSipTransactionListenerReceiveResponseMethod;
begin
  Notification := TIdSipTransactionListenerReceiveResponseMethod.Create;
  try
    Notification.Binding     := Binding;
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
  if (Self.fState <> Value) then begin
    Self.LogStateChange(Self.fState, Value);
    Self.fState := Value;
  end;
end;

procedure TIdSipTransaction.TryResendInitialRequest;
begin
  if not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendRequest(Self.InitialRequest, Self.InitialDestination);
end;

procedure TIdSipTransaction.TrySendRequest(R: TIdSipRequest;
                                           Dest: TIdSipLocation);
begin
  Self.LogSentMessage(R, Dest);
  Self.Dispatcher.SendToTransport(R, Dest);
end;

//******************************************************************************
//* TIdSipServerTransaction                                                    *
//******************************************************************************
//* TIdSipServerTransaction Public methods *************************************

constructor TIdSipServerTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                           InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.LastResponseSent  := Self.LastResponse;
  Self.SentResponses     := TIdSipResponseLocationsList.Create;
  Self.TargetLocations   := TIdSipLocations.Create;
end;

destructor TIdSipServerTransaction.Destroy;
begin
  Self.TargetLocations.Free;
  Self.SentResponses.Free;

  inherited Destroy;
end;

procedure TIdSipServerTransaction.DoOnTransportError(Msg: TIdSipMessage;
                                                           const Reason: String);
var
  Destination:       TIdSipLocation;
  ResponseLocations: TIdSipLocations;
  Response:          TIdSipResponse;
begin
  // You tried to send a response. It failed. The TransactionDispatcher invokes
  // this method to try the next possible location.

  // A request should NEVER end up here.
  if Msg.IsRequest then Exit;

  Response := Msg as TIdSipResponse;

  if not Self.SentResponses.Contains(Response) then Exit;

  ResponseLocations := Self.SentResponses.LocationsFor(Response);
  if not ResponseLocations.IsEmpty then begin
    // It's important to remove the location BEFORE sending the response,
    // because if a transport exception occurs we will try send to the first
    // location in our list. If we haven't already removed the last-attempted
    // location we will simply loop infinitely.
    Destination := ResponseLocations.First.Copy;
    try
      ResponseLocations.RemoveFirst;
      Self.TrySendResponseTo(Response, Destination);
    finally
      Destination.Free;
    end;
  end
  else begin
    Self.NotifyOfFailure(Response,
                         Format(RSNoLocationSucceeded, [Response.LastHop.SentBy]));
    Self.Terminate(true);
  end;
end;

function TIdSipServerTransaction.Match(Msg: TIdSipMessage): Boolean;
begin
  Result := inherited Match(Msg);

  // cf. RFC 3261, section 17.2.3, and TIdSipRequest.MatchRFC2543Request
  if not Msg.LastHop.IsRFC3261Branch and Msg.IsAck then
    Result := Result
          and (Msg.ToHeader.Tag = Self.LastResponse.ToHeader.Tag)
end;

procedure TIdSipServerTransaction.NotifyOfFailure(const Reason: String);
begin
  Self.NotifyOfFailure(Self.LastResponseSent, Reason);
end;

procedure TIdSipServerTransaction.SendResponse(R: TIdSipResponse);
begin
end;

//* TIdSipServerTransaction Protected methods **********************************

procedure TIdSipServerTransaction.TrySendLastResponse;
begin
  Self.TrySendResponse(Self.LastResponseSent);
end;

procedure TIdSipServerTransaction.TrySendResponse(R: TIdSipResponse);
var
  PossibleLocations: TIdSipLocations;
  Target:            TIdSipLocation;
begin
  if Self.TargetLocations.IsEmpty then
    Self.Dispatcher.FindServersFor(R, Self.TargetLocations);

  Self.StoreResponseLocations(R);

  PossibleLocations := Self.SentResponses.LocationsFor(R);

  if PossibleLocations.IsEmpty then begin
    // The Locator should at the least return a location based on the
    // Request-URI. Thus this clause should never execute. Still, this
    // clause protects the code that follows.

    Self.NotifyOfFailure(R, Format(RSNoLocationFound, [R.LastHop.SentBy]));
  end
  else begin
    Target := PossibleLocations.First.Copy;
    try
      PossibleLocations.RemoveFirst;

      Self.TrySendResponseTo(R, Target);
    finally
      Target.Free;
    end;
  end;
end;

procedure TIdSipServerTransaction.TrySendResponseTo(R: TIdSipResponse;
                                                    Dest: TIdSipLocation);
var
  LocalAddress:  TIdSipLocation;
  LocalBindings: TIdSipLocations;
begin
  // Some explanation: SentResponses contains a bunch of responses that we've
  // sent. (We might have sent several because we sent some provisional
  // responses before a final one.) SentResponses contains copies of those
  // responses, but those responses can't have the correct Contact information
  // because the Transaction-User layer doesn't find out to what Locations to
  // send the response. Thus, we look up the Locations FIRST and then EDIT the
  // response before sending it. If we edit first, then we won't find the
  // locations, because the newly-edited response was never added to
  // SentResponses.

  Self.LogSentMessage(R, Dest);

  LocalBindings := TIdSipLocations.Create;
  try
    Self.Dispatcher.LocalBindings(LocalBindings);

    // We're changing the Contact URI, but just adding a transport parameter is
    // harmless, so we don't worry about the Contact being set through a
    // directive.

    R.SetPreferredTransport(Self.Dispatcher.PreferredTransportTypeFor(Dest.IPAddress));

    LocalAddress := TIdSipLocation.Create;
    try
      Self.Dispatcher.RoutingTable.BestLocalAddress(LocalBindings, Dest, LocalAddress);
      R.RewriteLocationHeaders(LocalAddress);
    finally
      LocalAddress.Free;
    end;

    Self.Dispatcher.SendToTransport(R, Dest);
  finally
    LocalBindings.Free;
  end;
end;

//* TIdSipServerTransaction Private methods ************************************

procedure TIdSipServerTransaction.StoreResponseLocations(R: TIdSipResponse);
begin
  if Self.SentResponses.Contains(R) then
    Self.SentResponses.LocationsFor(R).AddLocations(Self.TargetLocations)
  else
    Self.SentResponses.Add(R, Self.TargetLocations);
end;

//******************************************************************************
//* TIdSipServerInviteTransaction                                              *
//******************************************************************************
//* TIdSipServerInviteTransaction Public methods *******************************

constructor TIdSipServerInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.HasSentResponse := false;
  
  Self.ChangeToProceeding;
end;

procedure TIdSipServerInviteTransaction.FireTimerG;
begin
  if not (Self.State = itsCompleted) then Exit;

  // cf. RFC 3261, section 26.3.2.4: "UAs and proxy servers SHOULD challenge
  // questionable requests with only a single 401 (Unauthorized) or 407 (Proxy
  // Authentication Required), forgoing the normal response retransmission
  // algorithm, and thus behaving statelessly towards unauthenticated requests."
  if Self.LastResponseSent.IsAuthenticationChallenge then Exit;

  if (Self.State = itsCompleted)
    and not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
    Self.TrySendLastResponse;

  if (Self.TimerGInterval < Self.Dispatcher.T2Interval) then
    Self.fTimerGInterval := 2*Self.TimerGInterval
  else
    Self.fTimerGInterval := Self.Dispatcher.T2Interval;

  Self.ScheduleTimerG;
end;

procedure TIdSipServerInviteTransaction.FireTimerH;
begin
  if (Self.State <> itsCompleted) then Exit;

  Self.DoOnTimeout(Self.InitialRequest,
                   SessionTimeoutMsg);
end;

procedure TIdSipServerInviteTransaction.FireTimerI;
begin
  if (Self.State <> itsConfirmed) then Exit;

  Self.ChangeToTerminated(true);
end;

function TIdSipServerInviteTransaction.IsInvite: Boolean;
begin
  Result := true;
end;

procedure TIdSipServerInviteTransaction.ReceiveRequest(R: TIdSipRequest;
                                                       Binding: TIdConnectionBindings);
begin
  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.ChangeToProceeding(R, Binding);
  end else begin
    case Self.State of
      itsProceeding: Self.TrySendLastResponse;

      itsCompleted: begin
        if R.IsInvite then
          Self.TrySendLastResponse
        else if R.IsAck then
          Self.ChangeToConfirmed(R);
      end;
    end;
  end;
end;

procedure TIdSipServerInviteTransaction.SendResponse(R: TIdSipResponse);
begin
  Self.HasSentResponse := true;

  if (Self.State = itsProceeding) then begin
    Self.TrySendResponse(R);

    case (R.StatusCode div 100) of
      1:    Self.ChangeToProceeding;
      2:    Self.ChangeToTerminated(false);
      3..6: Self.ChangeToCompleted;
    end;
  end;
end;

function TIdSipServerInviteTransaction.TimerGInterval: Cardinal;
begin
  // Result in milliseconds
  Result := Self.fTimerGInterval;
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

  Self.fTimerGInterval := Self.Dispatcher.T1Interval;

  // See the comment in FireTimerG.
  if not Self.LastResponseSent.IsAuthenticationChallenge then
    Self.ScheduleTimerG;

  Self.ScheduleTimerH;
end;

procedure TIdSipServerInviteTransaction.TrySendLastResponse;
begin
  if not Self.HasSentResponse then Exit;

  inherited TrySendLastResponse;
end;

procedure TIdSipServerInviteTransaction.TrySendResponseTo(R: TIdSipResponse;
                                                          Dest: TIdSipLocation);
begin
  if (not R.Equals(Self.LastResponseSent)) then
    Self.LastResponseSent.Assign(R);

  inherited TrySendResponseTo(R, Dest);
end;

//* TIdSipServerInviteTransaction Private methods ******************************

procedure TIdSipServerInviteTransaction.ChangeToConfirmed(R: TIdSipRequest);
begin
  Self.SetState(itsConfirmed);

  Self.ScheduleTimerI;
end;

procedure TIdSipServerInviteTransaction.ScheduleTimerG;
var
  Wait: TIdSipServerInviteTransactionTimerGWait;
begin
  Wait := TIdSipServerInviteTransactionTimerGWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerGInterval);
end;

procedure TIdSipServerInviteTransaction.ScheduleTimerH;
var
  Wait: TIdSipServerInviteTransactionTimerHWait;
begin
  Wait := TIdSipServerInviteTransactionTimerHWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerHInterval);
end;

procedure TIdSipServerInviteTransaction.ScheduleTimerI;
var
  Wait: TIdSipServerInviteTransactionTimerIWait;
begin
  Wait := TIdSipServerInviteTransactionTimerIWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerIInterval);
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
  if (Self.State <> itsCompleted) then Exit;

  Self.ChangeToTerminated(true);
end;

procedure TIdSipServerNonInviteTransaction.ReceiveRequest(R: TIdSipRequest;
                                                          Binding: TIdConnectionBindings);
begin
  inherited ReceiveRequest(R, Binding);

  if Self.FirstTime then begin
    Self.FirstTime := false;
    Self.ChangeToTrying;
    Self.NotifyOfRequest(R, Binding);
  end
  else begin
    if (Self.State in [itsCompleted, itsProceeding]) then
      Self.TrySendLastResponse;
  end;
end;

procedure TIdSipServerNonInviteTransaction.SendResponse(R: TIdSipResponse);
begin
  if (Self.State in [itsTrying, itsProceeding]) then begin
    Self.TrySendResponse(R);

    if R.IsFinal then begin
      Self.ChangeToCompleted;
      Self.ScheduleTimerJ;
    end
    else begin
      Self.LastResponseSent.Assign(R);
      Self.ChangeToProceeding;
    end;
  end;
end;

function TIdSipServerNonInviteTransaction.TimerJInterval: Cardinal;
begin
  // Result in milliseconds
  Result := 64*Self.Dispatcher.T1Interval;
end;

//* TIdSipServerNonInviteTransaction Protected methods *************************

procedure TIdSipServerNonInviteTransaction.TrySendResponseTo(R: TIdSipResponse;
                                                             Dest: TIdSipLocation);
begin
  Self.LastResponseSent.Assign(R);

  inherited TrySendResponseTo(R, Dest);
end;

//* TIdSipServerNonInviteTransaction Private methods ***************************

procedure TIdSipServerNonInviteTransaction.ChangeToTrying;
begin
  Self.SetState(itsTrying);
end;

procedure TIdSipServerNonInviteTransaction.ScheduleTimerJ;
var
  Wait: TIdSipServerNonInviteTransactionTimerJWait;
begin
  Wait := TIdSipServerNonInviteTransactionTimerJWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerJInterval);
end;

//******************************************************************************
//* TIdSipClientTransaction                                                    *
//******************************************************************************
//* TIdSipClientTransaction Public methods *************************************

function TIdSipClientTransaction.IsClient: Boolean;
begin
  Result := true;
end;

procedure TIdSipClientTransaction.NotifyOfFailure(const Reason: String);
begin
  Self.NotifyOfFailure(Self.InitialRequest, Reason);
end;

procedure TIdSipClientTransaction.SendRequest(Dest: TIdSipLocation); 
begin
  inherited SendRequest(Dest);

  Self.LastTriedLocation := Dest;
end;

//******************************************************************************
//* TIdSipClientInviteTransaction                                              *
//******************************************************************************
//* TIdSipClientInviteTransaction Public methods *******************************

constructor TIdSipClientInviteTransaction.Create(Dispatcher: TIdSipTransactionDispatcher;
                                                 InitialRequest: TIdSipRequest);
begin
  inherited Create(Dispatcher, InitialRequest);

  Self.fTimerAInterval := Self.Dispatcher.T1Interval;

  Self.ChangeToCalling;
end;

procedure TIdSipClientInviteTransaction.FireTimerA;
begin
  if (Self.State <> itsCalling) then Exit;

  Self.TryResendInitialRequest;

  Self.fTimerAInterval := 2*Self.TimerAInterval;
  Self.ScheduleTimerA;
end;

procedure TIdSipClientInviteTransaction.FireTimerB;
begin
  if (Self.State <> itsCalling) then Exit;

  Self.DoOnTimeout(Self.InitialRequest,
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

procedure TIdSipClientInviteTransaction.ReceiveResponse(R: TIdSipResponse;
                                                        Binding: TIdConnectionBindings);
begin
  case Self.State of
    itsCalling: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, Binding);
        2: Self.ChangeToTerminated(R, Binding);
      else
        Self.ChangeToCompleted(R, Binding);
      end;
    end;

    itsProceeding: begin
      case R.StatusCode div 100 of
        1: Self.ChangeToProceeding(R, Binding);
        2: Self.ChangeToTerminated(R, Binding);
      else
        Self.ChangeToCompleted(R, Binding);
      end;
    end;

    itsCompleted: begin
      if R.IsFinal then
        Self.ChangeToCompleted(R, Binding);
    end;
  end;
end;

procedure TIdSipClientInviteTransaction.SendRequest(Dest: TIdSipLocation); 
begin
  inherited SendRequest(Dest);

  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.ChangeToCalling;

    Self.ScheduleTimerA;
    Self.ScheduleTimerB;

    Self.TrySendRequest(Self.InitialRequest, Dest);
  end;
end;

function TIdSipClientInviteTransaction.TimerAInterval: Cardinal;
begin
  // Result in milliseconds
  Result := Self.fTimerAInterval;
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
                                                          Binding: TIdConnectionBindings);
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
  Self.ScheduleTimerD;

  Self.TrySendACK(R);

  if FirstResponse then
    Self.NotifyOfResponse(R, Binding);
end;

//* TIdSipClientInviteTransaction Private methods ******************************

procedure TIdSipClientInviteTransaction.ChangeToCalling;
begin
  Self.SetState(itsCalling);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerA;
var
  Wait: TIdSipClientInviteTransactionTimerAWait;
begin
  Wait := TIdSipClientInviteTransactionTimerAWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerAInterval);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerB;
var
  Wait: TIdSipClientInviteTransactionTimerBWait;
begin
  Wait := TIdSipClientInviteTransactionTimerBWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerBInterval);
end;

procedure TIdSipClientInviteTransaction.ScheduleTimerD;
var
  Wait: TIdSipClientInviteTransactionTimerDWait;
begin
  Wait := TIdSipClientInviteTransactionTimerDWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerDInterval);
end;

procedure TIdSipClientInviteTransaction.TrySendACK(R: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  if not R.IsOK then begin
    Ack := Self.InitialRequest.AckFor(R);
    try
      Self.TrySendRequest(Ack, Self.InitialDestination);
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

  Self.fTimerEInterval := Self.Dispatcher.T1Interval;

  Self.ChangeToTrying;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerE;
begin
  if not (Self.State in [itsTrying, itsProceeding]) then Exit;

  Self.TryResendInitialRequest;

  Self.RecalculateTimerE;
  Self.ScheduleTimerE;
end;

procedure TIdSipClientNonInviteTransaction.FireTimerF;
begin
  if not (Self.State in [itsTrying, itsProceeding]) then Exit;

  Self.DoOnTimeout(Self.InitialRequest,
                   SessionTimeoutMsg);
end;

procedure TIdSipClientNonInviteTransaction.FireTimerK;
begin
  if (Self.State <> itsCompleted) then Exit;

  Self.ChangeToTerminated(true);
end;

procedure TIdSipClientNonInviteTransaction.ReceiveResponse(R: TIdSipResponse;
                                                           Binding: TIdConnectionBindings);
begin
  if (Self.State = itsTerminated) then
    Exit;

  if (Self.State in [itsTrying, itsProceeding]) then begin
    if R.IsFinal then
      Self.ChangeToCompleted(R, Binding)
    else
      Self.ChangeToProceeding(R, Binding);
  end;
end;

procedure TIdSipClientNonInviteTransaction.SendRequest(Dest: TIdSipLocation); 
begin
  inherited SendRequest(Dest);

  if Self.FirstTime then begin
    Self.FirstTime := false;

    Self.ChangeToTrying;

    if not Self.Dispatcher.WillUseReliableTranport(Self.InitialRequest) then
      Self.ScheduleTimerE;
    Self.ScheduleTimerF;

    Self.TrySendRequest(Self.InitialRequest, Dest);
  end;
end;

function TIdSipClientNonInviteTransaction.TimerEInterval: Cardinal;
begin
  // Result in milliseconds
  Result := Self.fTimerEInterval;
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
end;

//* TIdSipClientNonInviteTransaction Protected methods *************************

procedure TIdSipClientNonInviteTransaction.RecalculateTimerE;
begin
  if (Self.State = itsTrying) then begin
    if (Self.TimerEInterval < Self.Dispatcher.T2Interval) then
      Self.fTimerEInterval := 2 * Self.TimerEInterval
    else
      Self.fTimerEInterval := Self.Dispatcher.T2Interval;
  end
  else if (Self.State = itsProceeding) then
    Self.fTimerEInterval := Self.Dispatcher.T2Interval;
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerE;
var
  Wait: TIdSipClientNonInviteTransactionTimerEWait;
begin
  Wait := TIdSipClientNonInviteTransactionTimerEWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerEInterval);
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerF;
var
  Wait: TIdSipClientNonInviteTransactionTimerFWait;
begin
  Wait := TIdSipClientNonInviteTransactionTimerFWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerFInterval);
end;

procedure TIdSipClientNonInviteTransaction.ScheduleTimerK;
var
  Wait: TIdSipClientNonInviteTransactionTimerKWait;
begin
  Wait := TIdSipClientNonInviteTransactionTimerKWait.Create;
  Wait.TransactionID := Self.ID;

  Self.Dispatcher.ScheduleEvent(Wait,
                                Self.TimerKInterval);
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

function TIdSipNullTransaction.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdSipTransactionWait                                                      *
//******************************************************************************
//* TIdSipTransactionWait Public methods ***************************************

procedure TIdSipTransactionWait.Trigger;
var
  Target: TObject;
begin
  // Wait objects usually log to the TIdTimerQueue's logger:
  inherited Trigger;

  Target := TIdObjectRegistry.FindObject(Self.TransactionID);

  if Assigned(Target) and (Target is TIdSipTransaction) then begin
    Self.Transaction := Target as TIdSipTransaction;

    // Log the Trigger to the log the Transaction uses.
    Self.OnLog := Self.LogToTransactionLayer;
    try
      Self.FireTimer(Self.Transaction);
      Self.LogTrigger;
    finally
      Self.AfterFiredTimer(Self.Transaction);
    end;
  end;
end;

//* TIdSipTransactionWait Protected methods ************************************

procedure TIdSipTransactionWait.AfterFiredTimer(Tran: TIdSipTransaction);
begin
  // By default, do nothing.
end;

procedure TIdSipTransactionWait.LogTrigger;
const
  LogMsg = 'Transaction %s timer %s triggered';
begin
  Self.OnLog(slDebug,
             '',
             LogEventRefTimerEvent,
             Format(LogMsg, [Self.TransactionID, Self.TimerName]),
             '');
end;

function TIdSipTransactionWait.TimerName: String;
begin
  Result := '';
  RaiseAbstractError(Self.ClassName, 'TimerName');
end;

procedure TIdSipTransactionWait.FireTimer(Tran: TIdSipTransaction);
begin
  RaiseAbstractError(Self.ClassName, 'FireTimer');
end;

//* TIdSipTransactionWait Private methods **************************************

procedure TIdSipTransactionWait.LogToTransactionLayer(Severity: TSeverityLevel;
                                                      SourceDescription: String;
                                                      RefCode: Cardinal;
                                                      Description,
                                                      BinaryData: String);
begin
  if not Assigned(Self.Transaction) then Exit;

  Self.Transaction.Log(Description, Severity, LogEventRefTimerEvent, '');
end;

//******************************************************************************
//* TIdSipTerminatingTransactionWait                                           *
//******************************************************************************
//* TIdSipTerminatingTransactionWait Protected methods *************************

procedure TIdSipTerminatingTransactionWait.AfterFiredTimer(Tran: TIdSipTransaction);
begin
  // Sometimes a terminating timer won't terminate the transaction. For
  // instance, a client INVITE in the Proceeding state mustn't terminate if
  // a scheduled TimerBWait triggers.
  if Tran.IsTerminated then
    Tran.Dispatcher.RemoveTransaction(Tran);
end;

//******************************************************************************
//* TIdSipServerInviteTransactionTimerGWait                                    *
//******************************************************************************
//* TIdSipServerInviteTransactionTimerGWait Protected methods ******************

procedure TIdSipServerInviteTransactionTimerGWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipServerInviteTransaction).FireTimerG;
end;

function TIdSipServerInviteTransactionTimerGWait.TimerName: String;
begin
  Result := 'G';
end;

//******************************************************************************
//* TIdSipServerInviteTransactionTimerHWait                                    *
//******************************************************************************
//* TIdSipServerInviteTransactionTimerHWait Protected methods ******************

procedure TIdSipServerInviteTransactionTimerHWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipServerInviteTransaction).FireTimerH;
end;

function TIdSipServerInviteTransactionTimerHWait.TimerName: String;
begin
  Result := 'H';
end;

//******************************************************************************
//* TIdSipServerInviteTransactionTimerIWait                                    *
//******************************************************************************
//* TIdSipServerInviteTransactionTimerIWait Protected methods ******************

procedure TIdSipServerInviteTransactionTimerIWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipServerInviteTransaction).FireTimerI;
end;

function TIdSipServerInviteTransactionTimerIWait.TimerName: String;
begin
  Result := 'I';
end;

//******************************************************************************
//* TIdSipServerNonInviteTransactionTimerJWait                                 *
//******************************************************************************
//* TIdSipServerNonInviteTransactionTimerJWait Protected methods ***************

procedure TIdSipServerNonInviteTransactionTimerJWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipServerNonInviteTransaction).FireTimerJ;
end;

function TIdSipServerNonInviteTransactionTimerJWait.TimerName: String;
begin
  Result := 'J';
end;

//******************************************************************************
//* TIdSipClientInviteTransactionTimerAWait                                    *
//******************************************************************************
//* TIdSipClientInviteTransactionTimerAWait Protected methods ******************

procedure TIdSipClientInviteTransactionTimerAWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientInviteTransaction).FireTimerA;
end;

function TIdSipClientInviteTransactionTimerAWait.TimerName: String;
begin
  Result := 'A';
end;

//******************************************************************************
//* TIdSipClientInviteTransactionTimerBWait                                    *
//******************************************************************************
//* TIdSipClientInviteTransactionTimerBWait Protected methods ******************

procedure TIdSipClientInviteTransactionTimerBWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientInviteTransaction).FireTimerB;
end;

function TIdSipClientInviteTransactionTimerBWait.TimerName: String;
begin
  Result := 'B';
end;

//******************************************************************************
//* TIdSipClientInviteTransactionTimerDWait                                    *
//******************************************************************************
//* TIdSipClientInviteTransactionTimerDWait Protected methods ******************

procedure TIdSipClientInviteTransactionTimerDWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientInviteTransaction).FireTimerD;
end;

function TIdSipClientInviteTransactionTimerDWait.TimerName: String;
begin
  Result := 'D';
end;

//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerEWait                                 *
//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerEWait Protected methods ***************

procedure TIdSipClientNonInviteTransactionTimerEWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientNonInviteTransaction).FireTimerE;
end;

function TIdSipClientNonInviteTransactionTimerEWait.TimerName: String;
begin
  Result := 'E';
end;

//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerFWait                                 *
//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerFWait Protected methods ***************

procedure TIdSipClientNonInviteTransactionTimerFWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientNonInviteTransaction).FireTimerF;
end;

function TIdSipClientNonInviteTransactionTimerFWait.TimerName: String;
begin
  Result := 'F';
end;

//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerKWait                                 *
//******************************************************************************
//* TIdSipClientNonInviteTransactionTimerKWait Protected methods ***************

procedure TIdSipClientNonInviteTransactionTimerKWait.FireTimer(Tran: TIdSipTransaction);
begin
  (Tran as TIdSipClientNonInviteTransaction).FireTimerK;
end;

function TIdSipClientNonInviteTransactionTimerKWait.TimerName: String;
begin
  Result := 'K';
end;

//******************************************************************************
//* TIdSipResponseLocationsList                                                *
//******************************************************************************
//* TIdSipResponseLocationsList Public methods *********************************

constructor TIdSipResponseLocationsList.Create;
begin
  inherited Create;

  Self.Locations := TObjectList.Create(true);
  Self.Responses := TIdSipResponseList.Create;
end;

destructor TIdSipResponseLocationsList.Destroy;
begin
  Self.Responses.Free;
  Self.Locations.Free;

  inherited Destroy;
end;

procedure TIdSipResponseLocationsList.Add(Response: TIdSipResponse;
                                         Locations: TIdSipLocations);
var
  NewLocations: TIdSipLocations;
begin
  try
    Self.Responses.AddCopy(Response);

    NewLocations := TIdSipLocations.Create;
    NewLocations.AddLocations(Locations);

    Self.Locations.Add(NewLocations);
  except
    raise;
  end;
end;

function TIdSipResponseLocationsList.Contains(Response: TIdSipResponse): Boolean;
begin
  Result := Self.Responses.Contains(Response);
end;

function TIdSipResponseLocationsList.LocationsFor(Response: TIdSipResponse): TIdSipLocations;
var
  Index: Integer;
begin
  Index := Self.Responses.IndexOf(Response);

  Result := Self.Locations[Index] as TIdSipLocations;
end;

//******************************************************************************
//* TIdSipTransactionDispatcherListenerFailedSendMethod                        *
//******************************************************************************
//* TIdSipTransactionDispatcherListenerFailedSendMethod Public methods *********

procedure TIdSipTransactionDispatcherListenerFailedSendMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionDispatcherListener).OnTransportException(Self.FailedMessage,
                                                                        Self.Reason);
end;

//******************************************************************************
//* TIdSipTransactionDispatcherListenerReceiveRequestMethod                    *
//******************************************************************************
//* TIdSipTransactionDispatcherListenerReceiveRequestMethod Public methods *****

procedure TIdSipTransactionDispatcherListenerReceiveRequestMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionDispatcherListener).OnReceiveRequest(Self.Request,
                                                                    Self.Binding);
end;

//******************************************************************************
//* TIdSipTransactionDispatcherListenerReceiveResponseMethod                   *
//******************************************************************************
//* TIdSipTransactionDispatcherListenerReceiveResponseMethod Public methods ****

procedure TIdSipTransactionDispatcherListenerReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionDispatcherListener).OnReceiveResponse(Self.Response,
                                                                     Self.Binding);
end;

//******************************************************************************
//* TIdSipTransportAddedMethod                                                 *
//******************************************************************************
//* TIdSipTransportAddedMethod Public methods **********************************

procedure TIdSipTransportAddedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportManagementListener).OnAddedTransport(Self.Transport);
end;

//******************************************************************************
//* TIdSipTransportRemovedMethod                                               *
//******************************************************************************
//* TIdSipTransportRemovedMethod Public methods ********************************

procedure TIdSipTransportRemovedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransportManagementListener).OnRemovedTransport(Self.Transport);
end;

//******************************************************************************
//* TIdSipTransactionListenerFailMethod                                        *
//******************************************************************************
//* TIdSipTransactionListenerFailMethod Public methods *************************

procedure TIdSipTransactionListenerFailMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnFail(Self.Transaction,
                                                Self.FailedMessage,
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
                                                          Self.Binding);
end;

//******************************************************************************
//* TIdSipTransactionListenerReceiveResponseMethod                             *
//******************************************************************************
//* TIdSipTransactionListenerReceiveResponseMethod Public methods **************

procedure TIdSipTransactionListenerReceiveResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipTransactionListener).OnReceiveResponse(Self.Response,
                                                           Self.Transaction,
                                                           Self.Binding);
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
