unit IdSipCore;

// Some overarching principles followed in this implementation of a SIP/2.0
// (RFC 3261) stack:
// * We rely on short-circuited evaluation of Boolean expressions.
// * We manually manage the lifetime of all objects. We do NOT use reference
//   counting for objects that implement interfaces.
// * We use Value Objects when possible.
// * If an object A receives some object B that it expects to store as data
//   then A must store a COPY of B. Typical objects are: TIdSipURI,
//   TIdSipDialogID, TIdSipMessage.
// * Each layer has references to the layers beneath it. We try to make each layer
//   aware of ONLY the layer immediately below it, but that's not always
//   possible. We NEVER let a lower layer know about layers above it. Thus, the
//   transport layer DOES NOT know about transactions, etc.
// * We propogate messages up the stack using Events or Listeners, and method
//   calls to propogate messages down the stack. We give preference to the more
//   flexible Listeners.
// * We avoid typecasting as much as possible by using polymorphism and, in
//   certain situations where polymorphism can't cut it, the Visitor pattern.
// * TObjectLists always manage the lifetime of the objects they contain. Except
//   in the case of Transports in the Dispatcher.

interface

uses
  Classes, Contnrs, IdSdp, IdSipDialog, IdException, IdInterfacedObject,
  IdSipHeaders, IdSipMessage, IdSipTimer, IdSipTransaction, IdSipTransport,
  SyncObjs;

type
  TIdSipRegistration = class;
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(const Session: TIdSipSession) of object;

  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // Unfortunately I never know the type of Observed and have to typecast
  // the Observed, but c'est la vie.
  IIdSipObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(Observed: TObject);
  end;

  // I provide a protocol for using a registrar. You send a REGISTER, and
  // listen for the events below.
  //
  // You can use OnAuthenticationChallenge to authenticate to a proxy (or
  // registrar). Note that we cannot distinguish between (1) you contacting
  // the proxy/registrar for the first time and it asking for credentials,
  // and (2) you offering invalid credentials.
  //
  // OnFailure and OnSuccess, apart from the obvious, tell you that the
  // registration agent has terminated, and that you should remove all
  // of your references to it.
  IIdSipRegistrationListener = interface
    ['{D3FA9A3D-ED8A-48D3-8068-38E8F9EE2140}']
    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);
  end;

  // I am the protocol of things that listen for Sessions:
  // * OnNewSession tells us that someone wants to talk to us - we may refuse or
  //   allow the session.
  // * OnSessionEstablished tells us when a session has been fully established.
  // * OnSessionEnded tells us when the session's finished. This could be
  //   because someone hung up, or because the outbound call failed (the request
  //   timed out, a transport error occurec, etc). OnSessionEnded lets us clean
  //   up. The Session referenced becomes invalid after this point. In other
  //   words, you'd better say goodbye to the Session in your implementation of
  //   this method. Accessing your reference to the Session will probably fail
  //   with an access violation.
  IIdSipSessionListener = interface
    ['{59B3C476-D3CA-4C5E-AA2B-2BB587A5A716}']
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
  end;

  IIdSipUserAgentListener = interface
    ['{E365D17F-054B-41AB-BB18-0C339715BFA3}']
    procedure OnInboundCall(Session: TIdSipSession);
  end;

  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  //
  // ReceiveRequest returns true if I handled the request, and false if I
  // rejected the request.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                             IIdSipUnhandledMessageListener)
  private
    fDispatcher: TIdSipTransactionDispatcher;
    fHostName:   String;

    procedure OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport); overload;
    procedure OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport); overload;
    procedure SetDispatcher(Value: TIdSipTransactionDispatcher);
  public
    constructor Create; virtual;

    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; virtual; abstract;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; virtual; abstract;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    function  NextCallID: String;
    function  ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Receiver: TIdSipTransport): Boolean; virtual;
    function  ReceiveResponse(Response: TIdSipResponse;
                              Transaction: TIdSipTransaction;
                              Receiver: TIdSipTransport): Boolean; virtual;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:   String                      read fHostName write fHostName;
  end;

  // I represent the heart of a User Agent. We split User Agent functionality
  // into multiple levels because some SIP entities (like registrars) behave
  // very similarly to "normal" User Agents. In fact, we can regard a registrar
  // as simply a User Agent that responds to REGISTER methods.

  // I provide the canonical place to reject messages that have correct syntax
  // but that we don't or can't accept. This includes unsupported SIP versions,
  // unrecognised methods, etc.
  TIdSipAbstractUserAgent = class(TIdSipAbstractCore)
  private
    BranchLock:              TCriticalSection;
    fAllowedContentTypeList: TStrings;
    fAllowedLanguageList:    TStrings;
    fAllowedMethodList:      TStrings;
    fAllowedSchemeList:      TStrings;
    fFrom:                   TIdSipFromHeader;
    fLastBranch:             Cardinal;
    fUserAgentName:          String;

    function  GetFrom: TIdSipFromHeader;
    procedure RejectRequestMethodNotAllowed(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentEncoding(Request: TIdSipRequest;
                                                  Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentLanguage(Request: TIdSipRequest;
                                                  Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentType(Request: TIdSipRequest;
                                              Transaction: TIdSipTransaction);
    procedure RejectUnsupportedSipVersion(Request: TIdSipRequest;
                                          Transaction: TIdSipTransaction);
    procedure ResetLastBranch;
    procedure SetFrom(Value: TIdSipFromHeader);
  protected
    procedure RejectRequestBadExtension(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction);

    property AllowedContentTypeList: TStrings read fAllowedContentTypeList;
    property AllowedLanguageList:    TStrings read fAllowedLanguageList;
    property AllowedMethodList:      TStrings read fAllowedMethodList;
    property AllowedSchemeList:      TStrings read fAllowedSchemeList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddAllowedContentType(const MimeType: String);
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedMethod(const Method: String);
    procedure AddAllowedScheme(const Scheme: String);
    function  AllowedContentTypes: String;
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; overload; override;
    function  HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(Request: TIdSipRequest): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  NextBranch: String;
    function  NextInitialSequenceNo: Cardinal;
    function  NextTag: String;
    function  ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Receiver: TIdSipTransport): Boolean; override;
    procedure ReturnResponse(Request: TIdSipRequest;
                             Reason: Cardinal;
                             Transaction: TIdSipTransaction);

    property From:          TIdSipFromHeader read GetFrom write SetFrom;
    property UserAgentName: String           read fUserAgentName write fUserAgentName;
  end;

  // I keep track of information a User Agent needs when making a REGISTER to
  // a particular registrar.
  TIdSipRegistrationInfo = class(TObject)
  private
    fCallID: String;
    fRegistrar: TIdSipUri;
    fSequenceNo: Cardinal;
  public
    constructor Create;
    destructor  Destroy; override;

    property CallID:     String    read fCallID write fCallID;
    property Registrar:  TIdSipUri read fRegistrar;
    property SequenceNo: Cardinal  read fSequenceNo write fSequenceNo;
  end;

  // I (usually) represent a human being in the SIP network. I:
  // * inform any listeners when new sessions become established, modified or
  //   terminated;
  // * allow my users to make outgoing "calls";
  // * clean up established Sessions
  TIdSipUserAgentCore = class(TIdSipAbstractUserAgent)
  private
    fContact:                 TIdSipContactHeader;
    KnownRegistrars:          TObjectList;
    ObserverLock:             TCriticalSection;
    Observers:                TList;
    RegistrationListenerLock: TCriticalSection;
    RegistrationListeners:    TList;
    RegistrationLock:         TCriticalSection;
    Registrations:            TObjectList;
    SessionLock:              TCriticalSection;
    Sessions:                 TObjectList;
    UserAgentListenerLock:    TCriticalSection;
    UserAgentListeners:       TList;

    function  AddInboundSession(Invite: TIdSipRequest;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport): TIdSipSession;
    procedure AddKnownRegistrar(Registrar: TIdSipUri;
                                const CallID: String;
                                SequenceNo: Cardinal);
    function  AddOutboundSession: TIdSipSession;
    function  AddRegistration: TIdSipRegistration;
    function  CallIDFor(Registrar: TIdSipUri): String;
    function  DefaultFrom: String;
    function  DefaultHostName: String;
    function  DefaultUserAgent: String;
    function  FindSession(const Msg: TIdSipMessage): TIdSipSession;
    function  GetContact: TIdSipContactHeader;
    function  IndexOfRegistrar(Registrar: TIdSipUri): Integer;
    function  KnowsRegistrar(Registrar: TIdSipUri): Boolean;
    procedure NotifyOfInboundCall(Session: TIdSipSession);
    procedure NotifyOfChange;
    procedure ProcessAck(Ack: TIdSipRequest;
                         Transaction: TIdSipTransaction;
                         Receiver: TIdSipTransport);
    procedure ProcessInvite(Invite: TIdSipRequest;
                            Transaction: TIdSipTransaction;
                            Receiver: TIdSipTransport);
    function  RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
    procedure RejectBadRequest(Request: TIdSipRequest;
                               const Reason: String;
                               Transaction: TIdSipTransaction);
    procedure SendByeToAppropriateSession(Bye: TIdSipRequest;
                                          Transaction: TIdSipTransaction;
                                          Receiver: TIdSipTransport);
    function  NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
    procedure SetContact(Value: TIdSipContactHeader);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdSipObserver);
    procedure AddUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  Call(Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String): TIdSipSession;
    function  CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(Dest: TIdSipToHeader;
                           const Body: String;
                           const MimeType: String): TIdSipRequest;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; overload; override;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    function  ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Receiver: TIdSipTransport): Boolean; override;
    function  ReceiveResponse(Response: TIdSipResponse;
                              Transaction: TIdSipTransaction;
                              Receiver: TIdSipTransport): Boolean; override;
    function  RegisterWith(Registrar: TIdSipUri): TIdSipRegistration;
    function  RegistrationCount: Integer;
    procedure RemoveObserver(const Listener: IIdSipObserver);
    procedure RemoveRegistration(Registration: TIdSipRegistration);
    procedure RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
    procedure RemoveSession(Session: TIdSipSession);
    function  SessionCount: Integer;
    procedure HangUpAllCalls;
    function  Username: String;

    property Contact: TIdSipContactHeader read GetContact write SetContact;
  end;

  // As per section 13.3.1.4 of RFC 3261, a Session will resend a 2xx response
  // to an INVITE until it receives an ACK. Thus I provide an exponential
  // back-off timer starting with an interval of T1 milliseconds and capping
  // the interval at T2 milliseconds.
  TIdSipSessionTimer = class(TObject)
  private
    Lock:    TCriticalSection;
    T1:      Cardinal;
    T2:      Cardinal;
    Session: TIdSipSession;
    Timer:   TIdSipTimer;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create(Session: TIdSipSession;
                       T1: Cardinal;
                       T2: Cardinal);
    destructor  Destroy; override;

    procedure Fire;
    procedure Start;
    procedure Stop;

    function Interval: Cardinal;
  end;

  TIdSipProcedure = procedure(ObjectOrIntf: Pointer) of object;

  // I am a SIP session. As such, I represent both what my dialog represents
  // (a long-term relationship between two peers in a SIP network) and also
  // the media streams initiated between those peers.
  //
  // Note that when you call my Terminate method, my owning UserAgent will
  // destroy me, and your reference to me will no longer be valid. The same
  // thing goes for when I notify you that I have terminated via
  // OnEndedSession.
  TIdSipSession = class(TIdInterfacedObject,
                        IIdSipTransactionListener)
  private
    fCore:               TIdSipUserAgentCore;
    fDialog:             TIdSipDialog;
    fInvite:             TIdSipRequest;
    fIsEstablished:      Boolean;
    fIsTerminated:       Boolean;
    fPayloadProcessor:   TIdSdpPayloadProcessor;
    fReceivedAck:        Boolean;
    InitialTran:         TIdSipTransaction;
    InitialTransport:    TIdSipTransport;
    IsInboundCall:       Boolean;
    LastResponse:        TIdSipResponse;
    OkTimer:             TIdSipSessionTimer;
    OpenTransactionLock: TCriticalSection;
    OpenTransactions:    TList;
    SessionListenerLock: TCriticalSection;
    SessionListeners:    TList;

    procedure AddOpenTransaction(Transaction: TIdSipTransaction);
    procedure ApplyTo(List: TList;
                      Lock: TCriticalSection;
                      Proc: TIdSipProcedure);
    procedure CreateInternal(UA: TIdSipUserAgentCore);
    function  CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
    function  CreateOutboundDialog(Response: TIdSipResponse;
                                   Receiver: TIdSipTransport): TIdSipDialog;
    procedure MarkAsTerminated;
    procedure MarkAsTerminatedProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEndedSession(const Reason: String);
    procedure NotifyOfEstablishedSession;
    procedure NotifyOfEstablishedSessionProc(ObjectOrIntf: Pointer);
    procedure NotifyOfModifiedSession(Invite: TIdSipRequest);
    procedure NotifyOfModifiedSessionProc(ObjectOrIntf: Pointer);
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
    procedure RejectOutOfOrderRequest(Request: TIdSipRequest;
                                      Transaction: TIdSipTransaction);
    procedure RejectRequest(Request: TIdSipRequest;
                            Transaction: TIdSipTransaction);
    procedure RemoveTransaction(Transaction: TIdSipTransaction);
    procedure SendAck(Final: TIdSipResponse);
    procedure SendBye;
    procedure SendCancel;
    procedure TerminateOpenTransaction(Transaction: TIdSipTransaction);

    property Core:          TIdSipUserAgentCore read fCore;
    property IsEstablished: Boolean             read fIsEstablished write fIsEstablished;
  public
    constructor Create(UA: TIdSipUserAgentCore); overload;
    constructor Create(UA: TIdSipUserAgentCore;
                       Invite: TIdSipRequest;
                       InitialTransaction: TIdSipTransaction;
                       Receiver: TIdSipTransport); overload;
    destructor  Destroy; override;

    function  AcceptCall(const Offer, ContentType: String): String;
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    procedure Cancel;
    procedure Call(Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String);
    function  CreateAck: TIdSipRequest;
    function  CreateCancel(Invite: TIdSipRequest): TIdSipRequest;
    function  DialogEstablished: Boolean;
    procedure Terminate;
    procedure Modify;
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);
    procedure ResendLastResponse; virtual;

    property Dialog:           TIdSipDialog           read fDialog;
    property Invite:           TIdSipRequest          read fInvite;
    property IsTerminated:     Boolean                read fIsTerminated;
    property PayloadProcessor: TIdSdpPayloadProcessor read fPayloadProcessor;
    property ReceivedAck:      Boolean                read fReceivedAck;
  end;

  // I piggyback on a transaction in a blocking I/O fashion to provide a UAC
  // with a way to register with a registrar. I take care of things like
  // doing stuff with error responses, asking for authentication, etc.
  //
  // It makes no sense to access me once my Transaction has terminated. In
  // other words once you've received notification of my success or failure,
  // erase your references to me.
  TIdSipRegistration = class(TIdInterfacedObject,
                             IIdSipTransactionListener)
  private
    Bindings:     TIdSipContacts;
    UA:           TIdSipUserAgentCore;
    ListenerLock: TCriticalSection;
    Listeners:    TList;

    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest;
    procedure NotifyOfAuthenticationChallenge(Response: TIdSipResponse);
    procedure NotifyOfFailure(CurrentBindings: TIdSipContacts;
                              const Reason: String);
    procedure NotifyOfSuccess(CurrentBindings: TIdSipContacts);
    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
    procedure ReissueRequest(Registrar: TIdSipUri;
                             MinimumExpiry: Cardinal);
    procedure Send(Request: TIdSipRequest);
    procedure Terminate;
  public
    constructor Create(UA: TIdSipUserAgentCore);
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    procedure Register(Registrar: TIdSipUri; Bindings: TIdSipContacts); overload;
    procedure Register(Registrar: TIdSipUri; Contact: TIdSipContactHeader); overload;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);
  end;

  EIdSipBadSyntax = class(EIdException);

const
  MissingContactHeader = 'Missing Contact Header';

implementation

uses
  IdGlobal, IdSimpleParser, IdSipConsts, IdSipDialogID,
  IdRandom, IdStack, SysUtils, IdUDPServer;

const
  RemoteHangUp = 'Remote end hung up';

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
  inherited Create;
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := IntToHex(TIdRandomNumber.NextCardinal, 8) + '@' + Self.HostName;
end;

function TIdSipAbstractCore.ReceiveRequest(Request: TIdSipRequest;
                                           Transaction: TIdSipTransaction;
                                           Receiver: TIdSipTransport): Boolean;
begin
  Result := false;
end;

function TIdSipAbstractCore.ReceiveResponse(Response: TIdSipResponse;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport): Boolean;
begin
  // cf RFC 3261 section 8.1.3.3
  Result := Response.Path.Count = 1;
end;

//* TIdSipAbstractCore Private methods *****************************************

procedure TIdSipAbstractCore.OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                                       Transaction: TIdSipTransaction;
                                                       Receiver: TIdSipTransport);
begin
  Self.ReceiveRequest(Request, Transaction, Receiver);
end;

procedure TIdSipAbstractCore.OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                                        Transaction: TIdSipTransaction;
                                                        Receiver: TIdSipTransport);
begin
  Self.ReceiveResponse(Response, Transaction, Receiver);
end;

procedure TIdSipAbstractCore.SetDispatcher(Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;

  fDispatcher.AddUnhandledMessageListener(Self);
end;

//******************************************************************************
//* TIdSipAbstractUserAgent                                                    *
//******************************************************************************
//* TIdSipAbstractUserAgent Public methods *************************************

constructor TIdSipAbstractUserAgent.Create;
begin
  inherited Create;

  Self.fAllowedMethodList := TStringList.Create;
  Self.fAllowedSchemeList := TStringList.Create;

  Self.BranchLock := TCriticalSection.Create;
  Self.ResetLastBranch;
end;

destructor TIdSipAbstractUserAgent.Destroy;
begin
  Self.BranchLock.Free;
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractUserAgent.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = -1) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipAbstractUserAgent.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = -1) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipAbstractUserAgent.AddAllowedMethod(const Method: String);
begin
  if not TIdSipParser.IsToken(Method) then
    raise EIdSipBadSyntax.Create('Not a token');

  if (Self.AllowedMethodList.IndexOf(Method) = -1) then
    Self.AllowedMethodList.Add(Method);
end;

procedure TIdSipAbstractUserAgent.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = -1) then
    Self.AllowedSchemeList.Add(Scheme);
end;

function TIdSipAbstractUserAgent.AllowedContentTypes: String;
begin
  Result := Self.AllowedContentTypeList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedLanguages: String;
begin
  Result := Self.AllowedLanguageList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedMethods: String;
begin
  Result := Self.AllowedMethodList.CommaText;
end;

function TIdSipAbstractUserAgent.AllowedSchemes: String;
begin
  Result := Self.AllowedSchemeList.CommaText;
end;

function TIdSipAbstractUserAgent.CreateRequest(Dest: TIdSipToHeader): TIdSipRequest;
var
  Transport: String;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := Dest.Address;

    Result.CallID          := Self.NextCallID;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;
    Result.From            := Self.From;
    Result.From.Tag        := Self.NextTag;
    Result.MaxForwards     := Result.DefaultMaxForwards;
    Result.ToHeader        := Dest;

    // The transport must be discovered using RFC 3263
    // TODO: Lies. Pure hack to get X-Lite talking
    if (Pos(TransportParam, Dest.AsString) > 0) then
      Transport := TransportParamUDP // Todo: replace IdUri completely. It's just crap.
    else
      Transport := TransportParamTCP;

    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/' + Transport + ' localhost';
    Result.LastHop.Branch := Self.NextBranch;

    if (Self.UserAgentName <> '') then
      Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipAbstractUserAgent.CreateResponse(Request: TIdSipRequest;
                                                ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request, ResponseCode);

  if not Request.ToHeader.HasTag then
    Result.ToHeader.Tag := Self.NextTag;
end;

function TIdSipAbstractUserAgent.HasUnknownContentEncoding(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipAbstractUserAgent.HasUnknownContentLanguage(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentLanguageHeader)
       and (Self.AllowedLanguageList.IndexOf(Request.FirstHeader(ContentLanguageHeader).Value) = -1);
end;

function TIdSipAbstractUserAgent.HasUnknownContentType(Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentTypeHeaderFull)
       and (Self.AllowedContentTypeList.IndexOf(Request.FirstHeader(ContentTypeHeaderFull).Value) = -1);
end;

function TIdSipAbstractUserAgent.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipAbstractUserAgent.IsMethodAllowed(const Method: String): Boolean;
begin
  Result := Self.AllowedMethodList.IndexOf(Method) >= 0;
end;

function TIdSipAbstractUserAgent.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipAbstractUserAgent.NextBranch: String;
begin
  Self.BranchLock.Acquire;
  try
    // TODO
    // This is a CRAP way to generate a branch.
    // cf. RFC 3261 section 8.1.1.7
    // While this (almost) satisfies the uniqueness constraint (the branch is
    // unique for the lifetime of the instantiation of the UA), it just
    // seems sensible to generate an unguessable branch.
    Result := BranchMagicCookie + IntToStr(Self.fLastBranch);

    Inc(Self.fLastBranch);
  finally
    Self.BranchLock.Release;
  end;
end;

function TIdSipAbstractUserAgent.NextInitialSequenceNo: Cardinal;
begin
  Result := TIdRandomNumber.NextCardinal($7FFFFFFF);
end;

function TIdSipAbstractUserAgent.NextTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := IntToHex(TIdRandomNumber.NextCardinal, 8)
          + IntToHex(TIdRandomNumber.NextCardinal, 8);
end;

function TIdSipAbstractUserAgent.ReceiveRequest(Request: TIdSipRequest;
                                                Transaction: TIdSipTransaction;
                                                Receiver: TIdSipTransport): Boolean;
begin
  Result := false;

  if (Request.SIPVersion <> SipVersion) then begin
    Self.RejectUnsupportedSipVersion(Request, Transaction);
  end;

  // cf RFC 3261 section 8.2
  // inspect the method - 8.2.1
  if not Request.IsAck and not Self.IsMethodAllowed(Request.Method) then begin
    Self.RejectRequestMethodNotAllowed(Request, Transaction);
    Exit;
  end;

  // inspect the headers - 8.2.2

  // To & Request-URI - 8.2.2.1
  if not Self.IsSchemeAllowed(Request.RequestUri.Scheme) then begin
    Self.ReturnResponse(Request, SIPUnsupportedURIScheme, Transaction);
    Exit;
  end;

  // Merged requests - 8.2.2.2
  if not Request.ToHeader.HasTag and Self.Dispatcher.LoopDetected(Request) then begin
    Self.ReturnResponse(Request, SIPLoopDetected, Transaction);
    Exit;
  end;

  // Require - 8.2.2.3
  if Request.HasHeader(RequireHeader) then begin
    Self.RejectRequestBadExtension(Request, Transaction);
    Exit;
  end;

  // Content processing - 8.2.3
  if Self.HasUnknownContentEncoding(Request) then begin
    Self.RejectRequestUnknownContentEncoding(Request, Transaction);
    Exit;
  end;

  if Self.HasUnknownContentLanguage(Request) then begin
    Self.RejectRequestUnknownContentLanguage(Request, Transaction);
    Exit;
  end;

  if Self.HasUnknownContentType(Request) then begin
    Self.RejectRequestUnknownContentType(Request, Transaction);
    Exit;
  end;

  Result := true;
end;

procedure TIdSipAbstractUserAgent.ReturnResponse(Request: TIdSipRequest;
                                                 Reason: Cardinal;
                                                 Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, Reason);
  try
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TIdSipAbstractUserAgent Protected methods **********************************

procedure TIdSipAbstractUserAgent.RejectRequestBadExtension(Request: TIdSipRequest;
                                                            Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadExtension);
  try
    Response.AddHeader(UnsupportedHeader).Value := Request.FirstHeader(RequireHeader).Value;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TIdSipAbstractUserAgent Private methods ************************************

function TIdSipAbstractUserAgent.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
end;

procedure TIdSipAbstractUserAgent.RejectRequestMethodNotAllowed(Request: TIdSipRequest;
                                                                Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPMethodNotAllowed);
  try
    Response.StatusText := Response.StatusText + ' (' + Request.Method + ')';
    Response.AddHeader(AllowHeader).Value := Self.AllowedMethods;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentEncoding(Request: TIdSipRequest;
                                                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptEncodingHeader).Value := '';

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentLanguage(Request: TIdSipRequest;
                                                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.AllowedLanguages;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectRequestUnknownContentType(Request: TIdSipRequest;
                                                                  Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptHeader).Value := SdpMimeType;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.RejectUnsupportedSipVersion(Request: TIdSipRequest;
                                                              Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPSIPVersionNotSupported);
  try
    Response.AddHeader(AcceptHeader).Value := SdpMimeType;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipAbstractUserAgent.ResetLastBranch;
begin
  Self.BranchLock.Acquire;
  try
    Self.fLastBranch := 0;
  finally
    Self.BranchLock.Release;
  end;
end;

procedure TIdSipAbstractUserAgent.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);
end;

//******************************************************************************
//* TIdSipRegistrationInfo                                                     *
//******************************************************************************
//* TIdSipRegistrationInfo Public methods **************************************

constructor TIdSipRegistrationInfo.Create;
begin
  inherited Create;

  Self.fRegistrar := TIdSipUri.Create;
end;

destructor TIdSipRegistrationInfo.Destroy;
begin
  Self.Registrar.Free;

  inherited Destroy;
end;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

constructor TIdSipUserAgentCore.Create;
begin
  inherited Create;

  Self.KnownRegistrars := TObjectList.Create(true);

  Self.ObserverLock             := TCriticalSection.Create;
  Self.Observers                := TList.Create;
  Self.RegistrationListenerLock := TCriticalSection.Create;
  Self.RegistrationListeners    := TList.Create;
  Self.RegistrationLock         := TCriticalSection.Create;
  Self.Registrations            := TObjectList.Create;
  Self.SessionLock              := TCriticalSection.Create;
  Self.Sessions                 := TObjectList.Create;
  Self.UserAgentListenerLock    := TCriticalSection.Create;
  Self.UserAgentListeners       := TList.Create;

  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;

  Self.AddAllowedContentType(SdpMimeType);
  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);

  Self.AddAllowedScheme(SipScheme);

  Self.From.Value    := Self.DefaultFrom;
  Self.HostName      := Self.DefaultHostName;
  Self.UserAgentName := Self.DefaultUserAgent;
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;
  Self.Contact.Free;
  Self.From.Free;
  Self.UserAgentListeners.Free;
  Self.UserAgentListenerLock.Free;  
  Self.Sessions.Free;
  Self.SessionLock.Free;
  Self.Registrations.Free;
  Self.RegistrationLock.Free;
  Self.RegistrationListeners.Free;
  Self.RegistrationListenerLock.Free;
  Self.Observers.Free;
  Self.ObserverLock.Free;
  Self.KnownRegistrars.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.AddObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Add(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.AddUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListenerLock.Acquire;
  try
    Self.UserAgentListeners.Add(Pointer(Listener));
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

function TIdSipUserAgentCore.Call(Dest: TIdSipToHeader;
                                  const InitialOffer: String;
                                  const MimeType: String): TIdSipSession;
begin
  Result := Self.AddOutboundSession;
  Result.Call(Dest, InitialOffer, MimeType);
end;

function TIdSipUserAgentCore.CreateBye(Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Self.CreateRequest(Dialog);
    Result.Method      := MethodBye;
    Result.CSeq.Method := Result.Method;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateInvite(Dest: TIdSipToHeader;
                                          const Body: String;
                                          const MimeType: String): TIdSipRequest;
begin
  Result := Self.CreateRequest(Dest);
  try
    Result.Method      := MethodInvite;
    Result.CSeq.Method := MethodInvite;

    Result.Body := Body;
    Result.ContentLength := Length(Body);

    if (Result.ContentLength > 0) then begin
      Result.ContentDisposition.Value := DispositionSession;
      Result.ContentType              := MimeType;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
begin
  Result := Self.CreateRequest(Registrar);
  try
    Self.AddKnownRegistrar(Registrar.Address,
                           Result.CallID,
                           Result.CSeq.SequenceNo);

    Result.Method := MethodRegister;
    Result.RequestUri.EraseUserInfo;

    Result.CSeq.Method     := MethodRegister;
    Result.CSeq.SequenceNo := Self.NextSequenceNoFor(Registrar.Address);

    Result.CallID := Self.CallIDFor(Registrar.Address);

    Result.ToHeader.Value := Self.Contact.Value;
    Result.From.Value     := Self.Contact.Value;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateRequest(Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := inherited CreateRequest(Dest);

  Result.AddHeader(Self.Contact);

  if Dest.HasSipsUri then
    Result.FirstContact.Address.Scheme := SipsScheme;
end;

function TIdSipUserAgentCore.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
var
  FirstRoute: TIdSipRouteHeader;
  Routes:     TIdSipHeaderList;
begin
  Result := TIdSipRequest.Create;
  try
    Result.MaxForwards      := Result.DefaultMaxForwards;
    Result.ToHeader.Address := Dialog.RemoteURI;
    Result.ToHeader.Tag     := Dialog.ID.RemoteTag;
    Result.From.Address     := Dialog.LocalURI;
    Result.From.Tag         := Dialog.ID.LocalTag;
    Result.CallID           := Dialog.ID.CallID;

    Result.CSeq.SequenceNo := Dialog.NextLocalSequenceNo;
    Result.CSeq.Method     := Result.Method;

    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/TCP localhost';
    Result.LastHop.Branch := Self.NextBranch;

    if (Dialog.RouteSet.IsEmpty) then begin
      Result.RequestUri := Dialog.RemoteTarget;
    end
    else begin
      Dialog.RouteSet.First;
      FirstRoute := Dialog.RouteSet.CurrentHeader as TIdSipRouteHeader;

      if FirstRoute.IsLooseRoutable then begin
        Result.RequestUri := Dialog.RemoteTarget;

        while Dialog.RouteSet.HasNext do begin
          Result.AddHeader(RouteHeader).Assign(Dialog.RouteSet.CurrentHeader);
          Dialog.RouteSet.Next;
        end;
      end
      else begin
        Result.RequestUri := FirstRoute.Address;
        // RFC 3261 section 12.2.1.1
        Result.RequestUri.Headers.Clear;
        Result.RequestUri.RemoveParameter(MethodParam);

        // Yes, we skip the first route. We use the 1st entry as the
        // Request-URI, remember?
        Routes := Dialog.RouteSet.GetAllButFirst;
        try
          Routes.First;
          while Routes.HasNext do begin
            Result.AddHeader(RouteHeader).Assign(Routes.CurrentHeader);
            Routes.Next;
          end;
        finally
          Routes.Free;
        end;

        (Result.AddHeader(RouteHeader) as TIdSipRouteHeader).Address := Dialog.RemoteURI;
      end;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateResponse(Request: TIdSipRequest;
                                            ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request,
                                        ResponseCode,
                                        Self.Contact);

  if not Request.ToHeader.HasTag then
    Result.ToHeader.Tag := Self.NextTag;

  if (Self.UserAgentName <> '') then
    Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
end;

function TIdSipUserAgentCore.ReceiveRequest(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport): Boolean;
begin
  Result := inherited ReceiveRequest(Request, Transaction, Receiver);

  if not Result then Exit;
  
  Result := false;

  // Processing the request - 8.2.5
  if Request.IsInvite then begin
    // Section 8.1.1.8 says that a request that can start a dialog (like an
    // INVITE), MUST contain a Contact.
    if not Request.HasHeader(ContactHeaderFull) then begin
      Self.RejectBadRequest(Request, MissingContactHeader, Transaction);
      Exit;
    end;

    Self.ProcessInvite(Request, Transaction, Receiver);
  end
  else if Request.IsAck then begin
    Self.ProcessAck(Request, Transaction, Receiver);
  end
  else if Request.IsBye then begin
    Self.SendByeToAppropriateSession(Request, Transaction, Receiver);
  end
  else if Request.IsCancel then
    raise Exception.Create('Handling CANCELs not implemented yet');

  // TIdSipSession generates the response - 8.2.6

  Result := true;
end;

function TIdSipUserAgentCore.ReceiveResponse(Response: TIdSipResponse;
                                             Transaction: TIdSipTransaction;
                                             Receiver: TIdSipTransport): Boolean;
var
  Session: TIdSipSession;
begin
  Result := inherited ReceiveResponse(Response, Transaction, Receiver);

  if not Result then Exit;
  
  // User Agents drop unmatched responses on the floor.
  // Except for 2xx's on a client INVITE. And these no longer belong to
  // a transaction, since the receipt of a 200 terminates a client INVITE
  // immediately. Hence the unusual clause below.
  if Response.IsOK and Transaction.IsNull then begin
    Session := Self.FindSession(Response);
    if Assigned(Session) then
      Session.OnReceiveResponse(Response, Transaction, Receiver);
  end;
end;

function TIdSipUserAgentCore.RegisterWith(Registrar: TIdSipUri): TIdSipRegistration;
begin
  Result := Self.AddRegistration;

  Result.Register(Registrar, Self.Contact);
end;

function TIdSipUserAgentCore.RegistrationCount: Integer;
begin
  // Return the number of ongoing registration attempts
  Self.RegistrationLock.Acquire;
  try
    Result := Self.Registrations.Count;
  finally
    Self.RegistrationLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.RemoveObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Remove(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.RemoveRegistration(Registration: TIdSipRegistration);
begin
  Self.RegistrationLock.Acquire;
  try
    Self.Registrations.Remove(Registration);
  finally
    Self.RegistrationLock.Release;
  end;
  Self.NotifyOfChange;
end;

procedure TIdSipUserAgentCore.RemoveSession(Session: TIdSipSession);
begin
  Self.SessionLock.Acquire;
  try
    Self.Sessions.Remove(Session);
  finally
    Self.SessionLock.Release;
  end;
  Self.NotifyOfChange;
end;

procedure TIdSipUserAgentCore.RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListenerLock.Acquire;
  try
    Self.UserAgentListeners.Remove(Pointer(Listener));
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

function TIdSipUserAgentCore.SessionCount: Integer;
begin
  Self.SessionLock.Acquire;
  try
    Result := Self.Sessions.Count;
  finally
    Self.SessionLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.HangUpAllCalls;
var
  CopyOfSessions: TObjectList;
  I:              Integer;
begin
  CopyOfSessions := TObjectList.Create(false);
  try
    Self.SessionLock.Acquire;
    try
      for I := 0 to Self.Sessions.Count - 1 do
        CopyOfSessions.Add(Self.Sessions[I]);
    finally
      Self.SessionLock.Release;
    end;

    for I := 0 to CopyOfSessions.Count - 1 do
      (CopyOfSessions[I] as TIdSipSession).Terminate;
  finally
    CopyOfSessions.Free;
  end;
end;

function TIdSipUserAgentCore.Username: String;
begin
  Result := Self.From.Address.Username;
end;

//* TIdSipUserAgentCore Private methods ****************************************

function TIdSipUserAgentCore.AddInboundSession(Invite: TIdSipRequest;
                                               Transaction: TIdSipTransaction;
                                               Receiver: TIdSipTransport): TIdSipSession;
begin
  Result := TIdSipSession.Create(Self, Invite, Transaction, Receiver);
  try
    Self.SessionLock.Acquire;
    try
      Self.Sessions.Add(Result);
    finally
      Self.SessionLock.Release;
    end;

    Self.NotifyOfInboundCall(Result);
    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSipUserAgentCore.AddKnownRegistrar(Registrar: TIdSipUri;
                                                const CallID: String;
                                                SequenceNo: Cardinal);
var
  NewReg: TIdSipRegistrationInfo;
begin
  if not Self.KnowsRegistrar(Registrar) then begin
    NewReg := TIdSipRegistrationInfo.Create;
    Self.KnownRegistrars.Add(NewReg);
    NewReg.CallID := CallID;
    NewReg.Registrar.Uri := Registrar.Uri;
    NewReg.SequenceNo := SequenceNo;
  end;
end;

function TIdSipUserAgentCore.AddOutboundSession: TIdSipSession;
begin
  Result := TIdSipSession.Create(Self);
  try
    Self.SessionLock.Acquire;
    try
      Self.Sessions.Add(Result);
    finally
      Self.SessionLock.Release;
    end;

    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.AddRegistration: TIdSipRegistration;
begin
  Result := TIdSipRegistration.Create(Self);
  try
    Self.RegistrationLock.Acquire;
    try
      Self.Registrations.Add(Result);
    finally
      Self.RegistrationLock.Release;
    end;
    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
  end;
end;


function TIdSipUserAgentCore.CallIDFor(Registrar: TIdSipUri): String;
begin
  Assert(Self.KnowsRegistrar(Registrar), 'A registrar wasn''t added');
  Result := Self.RegistrarAt(Self.IndexOfRegistrar(Registrar)).CallID
end;

function TIdSipUserAgentCore.DefaultFrom: String;
begin
  Result := 'unknown <sip:unknown@' + Self.HostName + '>';
end;

function TIdSipUserAgentCore.DefaultHostName: String;
begin
  Result := 'localhost';
end;

function TIdSipUserAgentCore.DefaultUserAgent: String;
begin
  Result := 'Indy SIP/2.0 Server v0.1';
end;

function TIdSipUserAgentCore.FindSession(const Msg: TIdSipMessage): TIdSipSession;
var
  DialogID: TIdSipDialogID;
  I:        Integer;
  Session:  TIdSipSession;
begin
  Result := nil;
  if Msg.IsRequest then
    DialogID := TIdSipDialogID.Create(Msg.CallID,
                                      Msg.ToHeader.Tag,
                                      Msg.From.Tag)
  else
    DialogID := TIdSipDialogID.Create(Msg.CallID,
                                      Msg.From.Tag,
                                      Msg.ToHeader.Tag);
  try
    Self.SessionLock.Acquire;
    try
      I := 0;
      while (I < Self.Sessions.Count) and not Assigned(Result) do begin
        Session := Self.Sessions[I] as TIdSipSession;
        if Session.IsEstablished and Session.Dialog.ID.IsEqualTo(DialogID) then
          Result := Session
        else
          Inc(I);
      end;
    finally
      Self.SessionLock.Release;
    end;
  finally
    DialogID.Free;
  end;
end;

function TIdSipUserAgentCore.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

function TIdSipUserAgentCore.IndexOfRegistrar(Registrar: TIdSipUri): Integer;
begin
  Result := 0;
  while (Result < Self.KnownRegistrars.Count) do
    if Self.RegistrarAt(Result).Registrar.Equals(Registrar) then
      Break
    else
      Inc(Result);

  if (Result >= Self.KnownRegistrars.Count) then
    Result := -1;
end;

function TIdSipUserAgentCore.KnowsRegistrar(Registrar: TIdSipUri): Boolean;
begin
  Result := Self.IndexOfRegistrar(Registrar) <> -1;
end;

procedure TIdSipUserAgentCore.NotifyOfInboundCall(Session: TIdSipSession);
var
  I: Integer;
begin
  Self.UserAgentListenerLock.Acquire;
  try
    for I := 0 to Self.UserAgentListeners.Count - 1 do
      IIdSipUserAgentListener(Self.UserAgentListeners[I]).OnInboundCall(Session);
  finally
    Self.UserAgentListenerLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.NotifyOfChange;
var
  I: Integer;
begin
  Self.ObserverLock.Acquire;
  try
    for I := 0 to Self.Observers.Count - 1 do
      IIdSipObserver(Self.Observers[I]).OnChanged(Self);
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.ProcessAck(Ack: TIdSipRequest;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Ack);

  // If Session = nil then we didn't match the ACK against any session, and so
  // we just drop it on the floor. There's no point in replying.
  if Assigned(Session) then
    Session.OnReceiveRequest(Ack, Transaction, Receiver);
end;

procedure TIdSipUserAgentCore.ProcessInvite(Invite: TIdSipRequest;
                                            Transaction: TIdSipTransaction;
                                            Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Assert(Invite.IsInvite,
         'ProcessInvite needs INVITE messages, '
       + 'not ' + Invite.Method + ' messages');

  Session := Self.FindSession(Invite);

  if Assigned(Session) then
    Session.OnReceiveRequest(Invite, Transaction, Receiver)
  else
    Self.AddInboundSession(Invite, Transaction, Receiver);
end;

function TIdSipUserAgentCore.RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
begin
  Result := Self.KnownRegistrars[Index] as TIdSipRegistrationInfo;
end;

procedure TIdSipUserAgentCore.RejectBadRequest(Request: TIdSipRequest;
                                               const Reason: String;
                                               Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;

    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.SendByeToAppropriateSession(Bye: TIdSipRequest;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Bye);

  if Assigned(Session) then
    Session.OnReceiveRequest(Bye,
                             Transaction,
                             Receiver)
  else
    Self.ReturnResponse(Bye,
                        SIPCallLegOrTransactionDoesNotExist,
                        Transaction);

end;

function TIdSipUserAgentCore.NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
var
  RegInfo: TIdSipRegistrationInfo;
begin
  Assert(Self.KnowsRegistrar(Registrar), 'A registrar wasn''t added');

  RegInfo := Self.RegistrarAt(Self.IndexOfRegistrar(Registrar));
  Result := RegInfo.SequenceNo;
  RegInfo.SequenceNo := Result + 1;
end;

procedure TIdSipUserAgentCore.SetContact(Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'A wildcard Contact header may not be used here');

  Self.Contact.Assign(Value);
end;

//******************************************************************************
//* TIdSipSessionTimer                                                         *
//******************************************************************************
//* TIdSipSessionTimer Public methods ******************************************

constructor TIdSipSessionTimer.Create(Session: TIdSipSession;
                                      T1: Cardinal;
                                      T2: Cardinal);
begin
  inherited Create;

  Self.Lock := TCriticalSection.Create;

  Self.Session := Session;
  Self.T1      := T1;
  Self.T2      := T2;

  Self.Timer := TIdSipTimer.Create;
  Self.Timer.Interval := Self.T1;
  Self.Timer.OnTimer  := Self.OnTimer;
end;

destructor TIdSipSessionTimer.Destroy;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipSessionTimer.Fire;
begin
  Self.Lock.Acquire;
  try
    Self.Timer.Interval := Min(2*Self.Timer.Interval, Self.T2);
  finally
    Self.Lock.Release;
  end;

  Self.Session.ResendLastResponse;
end;

procedure TIdSipSessionTimer.Start;
begin
  Self.Timer.Start;
end;

procedure TIdSipSessionTimer.Stop;
begin
  Self.Timer.Stop;
end;

function TIdSipSessionTimer.Interval: Cardinal;
begin
  Self.Lock.Acquire;
  try
    Result := Self.Timer.Interval;
  finally
    Self.Lock.Release;
  end;
end;

//* TIdSipSessionTimer Private methods *****************************************

procedure TIdSipSessionTimer.OnTimer(Sender: TObject);
begin
  Self.Fire;
end;

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

constructor TIdSipSession.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create;

  Self.CreateInternal(UA);
  Self.IsInboundCall := false;
end;

constructor TIdSipSession.Create(UA: TIdSipUserAgentCore;
                                 Invite: TIdSipRequest;
                                 InitialTransaction: TIdSipTransaction;
                                 Receiver: TIdSipTransport);
begin
  inherited Create;

  Self.CreateInternal(UA);
  Self.Invite.Assign(Invite);

  Self.IsInboundCall    := true;
  Self.InitialTran      := InitialTransaction;
  Self.InitialTransport := Receiver;

  Self.InitialTran.AddTransactionListener(Self);
end;

destructor TIdSipSession.Destroy;
begin
  Self.SessionListeners.Free;
  Self.SessionListenerLock.Free;

  Self.OpenTransactions.Free;
  Self.OpenTransactionLock.Free;

  Self.OkTimer.Free;

  Self.Invite.Free;
  Self.PayloadProcessor.Free;
  
  Self.LastResponse.Free;

  inherited Destroy;
end;

function TIdSipSession.AcceptCall(const Offer, ContentType: String): String;
var
  Response: TIdSipResponse;
begin
  // Offer contains a description of what data we expect to receive. Sometimes
  // we cannot meet this offer (e.g., the offer says "receive on port 8000" but
  // port 8000's already bound. We thus try to honour the offer as closely as
  // possible, and return the _actual_ offer sent.

  Result := '';

  if Self.IsInboundCall then begin
    // The type of payload processor depends on the ContentType passed in!
    Self.PayloadProcessor.StartListening(Offer);

    Response := Self.Core.CreateResponse(Self.Invite, SIPOK);
    try
      Result        := Self.PayloadProcessor.LocalSessionDescription;
      Response.Body := Result;

      Response.ContentLength := Length(Response.Body);
      Response.ContentType   := ContentType;
      Response.ToHeader.Tag  := Self.Core.NextTag;

      if not Self.DialogEstablished then begin
        fDialog := Self.CreateInboundDialog(Response);
        Self.NotifyOfEstablishedSession;
      end;

      Self.Dialog.HandleMessage(Self.Invite);
      Self.Dialog.HandleMessage(Response);

      Self.InitialTran.SendResponse(Response);
      Self.OkTimer.Start;
    finally
      Response.Free;
    end;
  end;
end;

procedure TIdSipSession.AddSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Add(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
  end;
end;

procedure TIdSipSession.Cancel;
begin
end;

procedure TIdSipSession.Call(Dest: TIdSipToHeader;
                             const InitialOffer: String;
                             const MimeType: String);
var
  Invite: TIdSipRequest;
begin
  if not Self.IsInboundCall and not Assigned(Self.InitialTran) then begin
    Invite := Self.Core.CreateInvite(Dest, InitialOffer, MimeType);
    try
      Self.Invite.Assign(Invite);

      Self.InitialTran := Self.Core.Dispatcher.AddClientTransaction(Self.Invite);
      Self.InitialTran.AddTransactionListener(Self);
      Self.InitialTran.SendRequest;
    finally
      Invite.Free;
    end;
  end;
end;

procedure TIdSipSession.Terminate;
begin
  if Self.DialogEstablished then begin
    Self.SendBye;
  end
  else begin
    Self.SendCancel;
  end;

  Self.MarkAsTerminated;
  Self.Core.RemoveSession(Self);
end;

procedure TIdSipSession.Modify;
begin
end;

procedure TIdSipSession.OnReceiveRequest(Request: TIdSipRequest;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  OK: TIdSipResponse;
begin
  if Self.IsTerminated then begin
    Self.RejectRequest(Request, Transaction);
    Exit;
  end;

  if Request.IsBye then begin
    Self.MarkAsTerminated;
    Self.Dialog.HandleMessage(Request);

    OK := Self.Core.CreateResponse(Request, SIPOK);
    try
      Transaction.SendResponse(OK);
    finally
      OK.Free;
    end;
    Self.NotifyOfEndedSession(RemoteHangUp);
  end
  else if Request.IsAck then begin
    Self.fReceivedAck := true;

    if Assigned(Self.OkTimer) then
      Self.OkTimer.Stop;
  end
  else if Request.IsInvite then begin
    if Self.Dialog.IsOutOfOrder(Request) then begin
      Self.RejectOutOfOrderRequest(Request, Transaction);
      Exit;
    end;

//    if Request.Disposition.IsSession then
//      Self.PayloadProcessor.RemoteSessionDescription := Request.Body;

    Self.AddOpenTransaction(Transaction);
    Self.NotifyOfModifiedSession(Request);
  end;
end;

procedure TIdSipSession.RemoveSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Remove(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
  end;
end;

procedure TIdSipSession.ResendLastResponse;
begin
  if Assigned(Self.LastResponse) then
    Self.Core.Dispatcher.Send(Self.LastResponse);
end;

//* TIdSipSession Private methods **********************************************

procedure TIdSipSession.AddOpenTransaction(Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Add(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.ApplyTo(List: TList;
                                Lock: TCriticalSection;
                                Proc: TIdSipProcedure);
var
  Copy: TList;
  I: Integer;
begin
  Copy := TList.Create;
  try
    Lock.Acquire;
    try
      for I := 0 to List.Count - 1 do
        Copy.Add(List[I]);
    finally
      Lock.Release;
    end;

    for I := 0 to Copy.Count - 1 do
      Proc(Copy[I]);
  finally
    Copy.Free;
  end;
end;

procedure TIdSipSession.CreateInternal(UA: TIdSipUserAgentCore);
begin
  Self.fIsEstablished := false;
  Self.fIsTerminated  := false;
  Self.fReceivedAck   := false;

  Self.fCore := UA;
  Self.fPayloadProcessor := TIdSdpPayloadProcessor.Create;
  Self.PayloadProcessor.Host     := Self.Core.HostName;
  Self.PayloadProcessor.Username := Self.Core.Username;

  Self.fInvite       := TIdSipRequest.Create;
  Self.IsEstablished := false;

  Self.OkTimer := TIdSipSessionTimer.Create(Self, DefaultT1, DefaultT2);

  Self.OpenTransactionLock := TCriticalSection.Create;
  Self.OpenTransactions    := TList.Create;

  Self.SessionListenerLock := TCriticalSection.Create;
  Self.SessionListeners    := TList.Create;
end;

function TIdSipSession.CreateInboundDialog(Response: TIdSipResponse): TIdSipDialog;
var
  ID:       TIdSipDialogID;
  RouteSet: TIdSipHeadersFilter;
begin
  try
    ID := TIdSipDialogID.Create(Response.CallID,
                                Response.ToHeader.Tag,
                                Self.Invite.From.Tag);
    try
      RouteSet := TIdSipHeadersFilter.Create(Self.Invite.Headers,
                                             RecordRouteHeader);
      try
        Result := TIdSipDialog.Create(ID,
                                       0,
                                       Self.Invite.CSeq.SequenceNo,
                                       Self.Invite.ToHeader.Address,
                                       Self.Invite.From.Address,
                                       Self.Invite.FirstContact.Address,
                                       Self.InitialTransport.IsSecure and (Self.Invite.HasSipsUri),
                                       RouteSet);
      finally
        RouteSet.Free;
      end;
    finally
      ID.Free;
    end;

    Self.LastResponse := TIdSipResponse.Create;
    Self.LastResponse.Assign(Response);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipSession.CreateOutboundDialog(Response: TIdSipResponse;
                                            Receiver: TIdSipTransport): TIdSipDialog;
var
  ID:       TIdSipDialogID;
  RouteSet: TIdSipHeadersFilter;
begin
  ID := TIdSipDialogID.Create(Self.Invite.CallID,
                              Self.Invite.From.Tag,
                              Response.ToHeader.Tag);
  try
    RouteSet := TIdSipHeadersFilter.Create(Self.Invite.Headers,
                                           RecordRouteHeader);
    try
      Result := TIdSipDialog.Create(ID,
                                    Self.Invite.CSeq.SequenceNo,
                                    0,
                                    Self.Invite.From.Address,
                                    Self.Invite.ToHeader.Address,
                                    Response.FirstContact.Address,
                                    Receiver.IsSecure and Self.Invite.FirstContact.HasSipsUri,
                                    RouteSet);
      Self.NotifyOfEstablishedSession;
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
end;

function TIdSipSession.CreateAck: TIdSipRequest;
begin
  try
    Result := Self.Core.CreateRequest(Self.Dialog);
    Result.Method          := MethodAck;
    Result.CSeq.SequenceNo := Self.Invite.CSeq.SequenceNo;
    Result.CSeq.Method     := Result.Method;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipSession.CreateCancel(Invite: TIdSipRequest): TIdSipRequest;
var
  RouteHeaders: TIdSipHeadersFilter;
begin
  Assert(Invite.IsInvite, 'Only INVITE requests may be CANCELled');
  try
    Result := TIdSipRequest.Create;
    Result.Method      := MethodCancel;
    Result.CSeq.Method := Result.Method;

    Result.RequestUri.Uri  := Invite.RequestUri.Uri;
    Result.CallID          := Invite.CallID;
    Result.ToHeader.Assign(Invite.ToHeader);
    Result.CSeq.SequenceNo := Invite.CSeq.SequenceNo;
    Result.From.Assign(Invite.From);

    Result.Path.Add(Invite.LastHop);

    RouteHeaders := TIdSipHeadersFilter.Create(Invite.Headers, RouteHeader);
    try
      Result.AddHeaders(RouteHeaders);
    finally
      RouteHeaders.Free;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipSession.DialogEstablished: Boolean;
begin
  Result := Assigned(Self.fDialog);
end;

procedure TIdSipSession.MarkAsTerminated;
begin
  Self.fIsTerminated := true;

  Self.ApplyTo(Self.OpenTransactions,
               Self.OpenTransactionLock,
               Self.MarkAsTerminatedProc);
end;

procedure TIdSipSession.MarkAsTerminatedProc(ObjectOrIntf: Pointer);
begin
  Self.TerminateOpenTransaction(TIdSipTransaction(ObjectOrIntf));
end;

procedure TIdSipSession.NotifyOfEndedSession(const Reason: String);
var
  Copy: TList;
  I: Integer;
begin
  Copy := TList.Create;
  try
    Self.SessionListenerLock.Acquire;
    try
      for I := 0 to Self.SessionListeners.Count - 1 do
        Copy.Add(Self.SessionListeners[I]);
    finally
      Self.SessionListenerLock.Release;
    end;

    for I := 0 to Copy.Count - 1 do
        IIdSipSessionListener(Copy[I]).OnEndedSession(Self, Reason);
  finally
    Copy.Free;
  end;

  Self.Core.RemoveSession(Self);
end;

procedure TIdSipSession.NotifyOfEstablishedSession;
begin
  Self.ApplyTo(Self.SessionListeners,
               Self.SessionListenerLock,
               Self.NotifyOfEstablishedSessionProc);

  Self.IsEstablished := true;
end;

procedure TIdSipSession.NotifyOfEstablishedSessionProc(ObjectOrIntf: Pointer);
begin
  IIdSipSessionListener(ObjectOrIntf).OnEstablishedSession(Self);
end;

procedure TIdSipSession.NotifyOfModifiedSession(Invite: TIdSipRequest);
begin
  Self.ApplyTo(Self.SessionListeners,
               Self.SessionListenerLock,
               Self.NotifyOfModifiedSessionProc);
end;

procedure TIdSipSession.NotifyOfModifiedSessionProc(ObjectOrIntf: Pointer);
begin
  IIdSipSessionListener(ObjectOrIntf).OnModifiedSession(Self, Invite);
end;

procedure TIdSipSession.OnFail(Transaction: TIdSipTransaction;
                               const Reason: String);
begin
  if (Transaction = Self.InitialTran) then
    Self.MarkAsTerminated;
end;

procedure TIdSipSession.OnReceiveResponse(Response: TIdSipResponse;
                                          Transaction: TIdSipTransaction;
                                          Receiver: TIdSipTransport);
begin
  // We should check for "and Response.ToHeader.HasTag" but that would prevent
  // us connecting to X-Lite, the non-compliant SIP phone.
  if not Response.IsTrying{ and Response.ToHeader.HasTag} then begin
    if not Self.DialogEstablished then begin
      fDialog := Self.CreateOutboundDialog(Response, Receiver);
      Self.NotifyOfEstablishedSession;
    end;

    Self.Dialog.HandleMessage(Response);
  end;

  if Response.IsOK then
    Self.SendAck(Response);

  // If we get, say, a transport error, or a timeout, or a rejection of our
  // call:
  if (Transaction = Self.InitialTran) then begin
    if Response.IsFinal and not Response.IsOK then begin
      Self.MarkAsTerminated;
      Self.NotifyOfEndedSession(Response.Description);
    end;
  end;
end;

procedure TIdSipSession.OnTerminated(Transaction: TIdSipTransaction);
begin
  Self.RemoveTransaction(Transaction);

//  if Self.IsTerminated then
//    Self.NotifyOfEndedSession;
end;

procedure TIdSipSession.RejectOutOfOrderRequest(Request: TIdSipRequest;
                                                Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Request,
                                       SIPInternalServerError);
  try
    Response.StatusText := RSSIPRequestOutOfOrder;
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RejectRequest(Request: TIdSipRequest;
                                      Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Request,
                                       SIPRequestTerminated);
  try
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipSession.RemoveTransaction(Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Remove(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.SendAck(Final: TIdSipResponse);
var
  Ack: TIdSipRequest;
begin
  Ack := Self.CreateAck;
  try
    Self.Core.Dispatcher.Send(Ack);
  finally
    Ack.Free;
  end;
end;

procedure TIdSipSession.SendBye;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.Core.CreateBye(Self.Dialog);
  try
    Self.Core.Dispatcher.AddClientTransaction(Bye).SendRequest;
  finally
    Bye.Free;
  end;
end;

procedure TIdSipSession.SendCancel;
var
  Cancel: TIdSipRequest;
begin
  Cancel := Self.CreateCancel(Self.Invite);
  try
    Self.Core.Dispatcher.AddClientTransaction(Cancel).SendRequest;
  finally
    Cancel.Free;
  end;
end;

procedure TIdSipSession.TerminateOpenTransaction(Transaction: TIdSipTransaction);
begin
  if not Transaction.IsClient then
    Self.RejectRequest(Transaction.InitialRequest, Transaction);
end;

//******************************************************************************
//* TIdSipRegistration                                                         *
//******************************************************************************
//* TIdSipRegistration Public methods ******************************************

constructor TIdSipRegistration.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create;

  Self.Bindings     := TIdSipContacts.Create;
  Self.ListenerLock := TCriticalSection.Create;
  Self.Listeners    := TList.Create;

  Self.UA := UA;
end;

destructor TIdSipRegistration.Destroy;
begin
  Self.Listeners.Free;
  Self.ListenerLock.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipRegistration.AddListener(const Listener: IIdSipRegistrationListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Add(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

//  Request := Self.CreateRequest(Registrar, Bindings);

procedure TIdSipRegistration.Register(Registrar: TIdSipUri; Bindings: TIdSipContacts);
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateRegister(Registrar, Bindings);
  try
    Self.Bindings.Clear;
    Self.Bindings.Add(Bindings);

    Self.Send(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipRegistration.Register(Registrar: TIdSipUri; Contact: TIdSipContactHeader);
var
  Binding: TIdSipContacts;
begin
  Binding := TIdSipContacts.Create;
  try
    Binding.Add(Contact);

    Self.Register(Registrar, Binding);
  finally
    Binding.Free;
  end;
end;

procedure TIdSipRegistration.RemoveListener(const Listener: IIdSipRegistrationListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Remove(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

//* TIdSipRegistration Private methods *****************************************

function TIdSipRegistration.CreateRegister(Registrar: TIdSipUri;
                                           Bindings: TIdSipContacts): TIdSipRequest;
var
  OldContacts: TIdSipContacts;
  ToHeader: TIdSipToHeader;
begin
  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Address := Registrar;

    Result := Self.UA.CreateRegister(ToHeader);

    // Bindings explicitly carries all Contact information. Thus we must remove
    // any Contact information already in Result.
    OldContacts := TIdSipContacts.Create(Result.Headers);
    try
      OldContacts.Clear;
    finally
      OldContacts.Free;
    end;

    Result.AddHeaders(Bindings);
  finally
    ToHeader.Free;
  end;

end;

procedure TIdSipRegistration.NotifyOfAuthenticationChallenge(Response: TIdSipResponse);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipRegistrationListener(Self.Listeners[I]).OnAuthenticationChallenge(Self, Response);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipRegistration.NotifyOfFailure(CurrentBindings: TIdSipContacts;
                                             const Reason: String);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipRegistrationListener(Self.Listeners[I]).OnFailure(Self,
                                                              CurrentBindings,
                                                              Reason);
  finally
    Self.ListenerLock.Release;
  end;

  Self.Terminate;
end;

procedure TIdSipRegistration.NotifyOfSuccess(CurrentBindings: TIdSipContacts);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipRegistrationListener(Self.Listeners[I]).OnSuccess(Self,
                                                              CurrentBindings);
  finally
    Self.ListenerLock.Release;
  end;

  Self.Terminate;
end;

procedure TIdSipRegistration.OnFail(Transaction: TIdSipTransaction;
                                    const Reason: String);
begin
end;

procedure TIdSipRegistration.OnReceiveRequest(Request: TIdSipRequest;
                                              Transaction: TIdSipTransaction;
                                              Receiver: TIdSipTransport);
begin
end;

procedure TIdSipRegistration.OnReceiveResponse(Response: TIdSipResponse;
                                               Transaction: TIdSipTransaction;
                                               Receiver: TIdSipTransport);
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create(Response.Headers);
  try
    case Response.StatusCode of
      SIPOK: begin
        Self.NotifyOfSuccess(Bindings);
      end;

      SIPUnauthorized,
      SIPProxyAuthenticationRequired:
        Self.NotifyOfAuthenticationChallenge(Response);
      SIPIntervalTooBrief: Self.ReissueRequest(Transaction.InitialRequest.RequestUri,
                                               Response.FirstMinExpires.NumericValue);
    else
      Self.NotifyOfFailure(Bindings,
                           IntToStr(Response.StatusCode)
                         + ' ' + Response.StatusText);
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipRegistration.OnTerminated(Transaction: TIdSipTransaction);
begin
  Transaction.RemoveTransactionListener(Self);
end;

procedure TIdSipRegistration.ReissueRequest(Registrar: TIdSipUri;
                                            MinimumExpiry: Cardinal);
var
  Bindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  // We received a 423 Interval Too Brief from the registrar. Therefore we
  // make a new REGISTER request with the registrar's minimum expiry.
  Request := Self.CreateRegister(Registrar, Self.Bindings);
  try
    Bindings := TIdSipContacts.Create(Request.Headers);
    try
      Bindings.First;
      while Bindings.HasNext do begin
        if Bindings.CurrentContact.WillExpire then
          Bindings.CurrentContact.Expires := Max(Bindings.CurrentContact.Expires,
                                                     MinimumExpiry);
        Bindings.Next;
      end;
    finally
      Bindings.Free;
    end;

    Request.FirstExpires.NumericValue := MinimumExpiry;
    Self.Send(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipRegistration.Send(Request: TIdSipRequest);
var
  Transaction: TIdSipTransaction;
begin
  Transaction := Self.UA.Dispatcher.AddClientTransaction(Request);
  Transaction.AddTransactionListener(Self);
  Transaction.SendRequest;
end;

procedure TIdSipRegistration.Terminate;
begin
  Self.UA.RemoveRegistration(Self);
end;

end.
