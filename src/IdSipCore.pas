unit IdSipCore;

// Some overarching principles followed in this implementation of a SIP/2.0
// (RFC 3261) stack:
// * We manually manage the lifetime of all objects. We do NOT use reference
//   counting for objects that implement interfaces.
// * We use Value Objects when possible.
// * If an object A receives some object B that it expects to store as data
//   then A must store a COPY of B. Typical objects are: TIdSipURI,
//   TIdSipDialogID, TIdSipMessage.
// * Each layer has references to the layers beneath it. We try to make each layer
//   aware of ONLY the layer immediately below it, but that's not always
//   possible.
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
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(const Session: TIdSipSession) of object;

  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // Unfortunately I never know the type of Observed and have to typecast
  // the Observed, but c'est la vie.
  IIdSipObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(const Observed: TObject);
  end;

  // I am the protocol of things that listen for Sessions:
  // * OnNewSession tells us that someone wants to talk to us - we may refuse or
  //   allow the session.
  // * OnSessionEstablished tells us when a session has been fully established.
  // * OnSessionEnded lets us clean up. The Session referenced becomes invalid
  //   after this point, and its very existence is not guaranteed. In
  //   other words, you'd better say goodbye to the Session in this method.
  IIdSipSessionListener = interface
    ['{59B3C476-D3CA-4C5E-AA2B-2BB587A5A716}']
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewSession(const Session: TIdSipSession);
  end;

  // TODO: there's redundance with this Hostname, and the Hostnames of the
  // transports attached to this core. It's not clear how to set up the
  // hostnames and bindings of the stack.
  TIdSipAbstractCore = class(TIdInterfacedObject,
                             IIdSipUnhandledMessageListener)
  private
    fDispatcher: TIdSipTransactionDispatcher;
    fHostName:   String;

    procedure OnReceiveUnhandledRequest(const Request: TIdSipRequest;
                                        const Transaction: TIdSipTransaction;
                                        const Receiver: TIdSipTransport); overload;
    procedure OnReceiveUnhandledResponse(const Response: TIdSipResponse;
                                         const Transaction: TIdSipTransaction;
                                         const Receiver: TIdSipTransport); overload;
    procedure SetDispatcher(const Value: TIdSipTransactionDispatcher);
  public
    constructor Create; virtual;

    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; overload; virtual; abstract;
    function  CreateRequest(const Dialog: TIdSipDialog): TIdSipRequest; overload; virtual; abstract;
    function  CreateResponse(const Request: TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    function  NextCallID: String;
    procedure ReceiveRequest(const Request: TIdSipRequest;
                             const Transaction: TIdSipTransaction;
                             const Receiver: TIdSipTransport); virtual; abstract;
    procedure ReceiveResponse(const Response: TIdSipResponse;
                              const Transaction: TIdSipTransaction;
                              const Receiver: TIdSipTransport); virtual; abstract;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:   String                      read fHostName write fHostName;
  end;

  // I (usually) represent a human being in the SIP network. I:
  // * inform any listeners when new sessions become established, modified or
  //   terminated;
  // * allow my users to make outgoing "calls";
  // * clean up established Sessions
  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    BranchLock:              TCriticalSection;
    fAllowedContentTypeList: TStrings;
    fAllowedLanguageList:    TStrings;
    fAllowedMethodList:      TStrings;
    fAllowedSchemeList:      TStrings;
    fContact:                TIdSipContactHeader;
    fFrom:                   TIdSipFromHeader;
    fLastBranch:             Cardinal;
    fUserAgentName:          String;
    ObserverLock:            TCriticalSection;
    Observers:               TList;
    SessionListenerLock:     TCriticalSection;
    SessionListeners:        TList;
    SessionLock:             TCriticalSection;
    Sessions:                TObjectList;

    function  AddInboundSession(const Invite: TIdSipRequest;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport): TIdSipSession;
    function  AddOutboundSession: TIdSipSession;
    function  FindSession(const Msg: TIdSipMessage): TIdSipSession;
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure NotifyOfNewSession(const Session: TIdSipSession);
    procedure NotifyOfChange;
    procedure ProcessAck(const Ack: TIdSipRequest;
                         const Transaction: TIdSipTransaction;
                         const Receiver: TIdSipTransport);
    procedure ProcessInvite(const Invite: TIdSipRequest;
                            const Transaction: TIdSipTransaction;
                            const Receiver: TIdSipTransport);
    procedure RejectBadRequest(const Request: TIdSipRequest;
                               const Reason: String;
                               const Transaction: TIdSipTransaction);
    procedure RejectRequestBadExtension(const Request: TIdSipRequest;
                                        const Transaction: TIdSipTransaction);
    procedure RejectRequestMethodNotAllowed(const Request: TIdSipRequest;
                                            const Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentEncoding(const Request: TIdSipRequest;
                                                  const Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentLanguage(const Request: TIdSipRequest;
                                                  const Transaction: TIdSipTransaction);
    procedure RejectRequestUnknownContentType(const Request: TIdSipRequest;
                                              const Transaction: TIdSipTransaction);
    procedure RejectUnsupportedSipVersion(const Request: TIdSipRequest;
                                          const Transaction: TIdSipTransaction);
    procedure ResetLastBranch;
    procedure SendByeToAppropriateSession(const Bye: TIdSipRequest;
                                          const Transaction: TIdSipTransaction;
                                          const Receiver: TIdSipTransport);
    procedure SetContact(const Value: TIdSipContactHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);

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
    procedure AddObserver(const Listener: IIdSipObserver);
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    function  AllowedContentTypes: String;
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    function  Call(const Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String): TIdSipSession;
    function  CreateBye(const Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(const Dest: TIdSipToHeader;
                           const Body: String;
                           const MimeType: String): TIdSipRequest;
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; overload; override;
    function  CreateRequest(const Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(const Request: TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    function  DefaultFrom: String;
    function  DefaultHostName: String;
    function  DefaultUserAgent: String;
    procedure ReceiveRequest(const Request: TIdSipRequest;
                             const Transaction: TIdSipTransaction;
                             const Receiver: TIdSipTransport); override;
    procedure ReceiveResponse(const Response: TIdSipResponse;
                              const Transaction: TIdSipTransaction;
                              const Receiver: TIdSipTransport); override;
    function  HasUnknownContentLanguage(const Request: TIdSipRequest): Boolean;
    function  HasUnknownContentEncoding(const Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(const Request: TIdSipRequest): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  NextBranch: String;
    function  NextInitialSequenceNo: Cardinal;
    function  NextTag: String;
    procedure RejectRequest(const Request: TIdSipRequest;
                            const Reason: Cardinal;
                            const Transaction: TIdSipTransaction;
                            const Receiver: TIdSipTransport);
    procedure RemoveObserver(const Listener: IIdSipObserver);
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);
    procedure RemoveSession(const Session: TIdSipSession);
    function  SessionCount: Integer;
    procedure HangUpAllCalls;
    function  Username: String;

    property Contact:       TIdSipContactHeader read GetContact write SetContact;
    property From:          TIdSipFromHeader    read GetFrom write SetFrom;
    property UserAgentName: String              read fUserAgentName write fUserAgentName;
  end;

  TIdSipSessionTimer = class(TObject)
  private
    Lock:  TCriticalSection;
    T1:    Cardinal;
    T2:    Cardinal;
    Timer: TIdSipTimer;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create(const Session: TIdSipSession;
                       const T1: Cardinal;
                       const T2: Cardinal);
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

    procedure AddOpenTransaction(const Transaction: TIdSipTransaction);
    procedure ApplyTo(const List: TList; const Lock: TCriticalSection; Proc: TIdSipProcedure);
    procedure CreateInternal(const UA: TIdSipUserAgentCore);
    function  CreateInboundDialog(const Response: TIdSipResponse): TIdSipDialog;
    function  CreateOutboundDialog(const Response: TIdSipResponse;
                                   const Receiver: TIdSipTransport): TIdSipDialog;
    procedure MarkAsTerminated;
    procedure MarkAsTerminatedProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEndedSession;
    procedure NotifyOfEndedSessionProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEstablishedSession;
    procedure NotifyOfEstablishedSessionProc(ObjectOrIntf: Pointer);
    procedure NotifyOfModifiedSession(const Invite: TIdSipRequest);
    procedure NotifyOfModifiedSessionProc(ObjectOrIntf: Pointer);
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
    procedure RejectOutOfOrderRequest(const Request: TIdSipRequest;
                                      const Transaction: TIdSipTransaction);
    procedure RejectRequest(const Request: TIdSipRequest;
                            const Transaction: TIdSipTransaction);
    procedure RemoveTransaction(const Transaction: TIdSipTransaction);
    procedure SendAck(const Final: TIdSipResponse);
    procedure SendBye;
    procedure SendCancel;
    procedure TerminateOpenTransaction(const Transaction: TIdSipTransaction);

    property Core:          TIdSipUserAgentCore read fCore;
    property IsEstablished: Boolean             read fIsEstablished write fIsEstablished;
  public
    constructor Create(const UA: TIdSipUserAgentCore); overload;
    constructor Create(const UA: TIdSipUserAgentCore;
                       const Invite: TIdSipRequest;
                       const InitialTransaction: TIdSipTransaction;
                       const Receiver: TIdSipTransport); overload;
    destructor  Destroy; override;

    function  AcceptCall(const Offer, ContentType: String): String;
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    procedure Cancel;
    procedure Call(const Dest: TIdSipToHeader;
                   const InitialOffer: String;
                   const MimeType: String);
    function  CreateAck: TIdSipRequest;
    function  CreateCancel(const Invite: TIdSipRequest): TIdSipRequest;
    function  DialogEstablished: Boolean;
    procedure Terminate;
    procedure Modify;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Receiver: TIdSipTransport);
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);
    procedure ResendLastResponse; virtual;

    property Dialog:           TIdSipDialog           read fDialog;
    property Invite:           TIdSipRequest          read fInvite;
    property IsTerminated:     Boolean                read fIsTerminated;
    property PayloadProcessor: TIdSdpPayloadProcessor read fPayloadProcessor;
    property ReceivedAck:      Boolean                read fReceivedAck;
  end;

  EIdSipBadSyntax = class(EIdException);

const
  MissingContactHeader = 'Missing Contact Header';

implementation

uses
  IdGlobal, IdSimpleParser, IdSipConsts, IdSipDialogID,
  IdRandom, IdStack, SysUtils, IdUDPServer;

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

//* TIdSipAbstractCore Private methods *****************************************

procedure TIdSipAbstractCore.OnReceiveUnhandledRequest(const Request: TIdSipRequest;
                                                       const Transaction: TIdSipTransaction;
                                                       const Receiver: TIdSipTransport);
begin
  Self.ReceiveRequest(Request, Transaction, Receiver);
end;

procedure TIdSipAbstractCore.OnReceiveUnhandledResponse(const Response: TIdSipResponse;
                                                        const Transaction: TIdSipTransaction;
                                                        const Receiver: TIdSipTransport);
begin
  Self.ReceiveResponse(Response, Transaction, Receiver);
end;

procedure TIdSipAbstractCore.SetDispatcher(const Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;

  fDispatcher.AddUnhandledMessageListener(Self);
end;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

constructor TIdSipUserAgentCore.Create;
begin
  inherited Create;

  Self.BranchLock          := TCriticalSection.Create;
  Self.ObserverLock        := TCriticalSection.Create;
  Self.Observers           := TList.Create;
  Self.SessionListenerLock := TCriticalSection.Create;
  Self.SessionListeners    := TList.Create;
  Self.SessionLock         := TCriticalSection.Create;
  Self.Sessions            := TObjectList.Create;

  Self.ResetLastBranch;
  Self.fAllowedContentTypeList := TStringList.Create;
  Self.fAllowedLanguageList    := TStringList.Create;
  Self.fAllowedMethodList      := TStringList.Create;
  Self.fAllowedSchemeList      := TStringList.Create;

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
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;
  Self.AllowedLanguageList.Free;
  Self.AllowedContentTypeList.Free;
  Self.Contact.Free;
  Self.From.Free;
  Self.Sessions.Free;
  Self.SessionLock.Free;
  Self.SessionListeners.Free;
  Self.SessionListenerLock.Free;
  Self.Observers.Free;
  Self.ObserverLock.Free;
  Self.BranchLock.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.AddAllowedContentType(const MimeType: String);
begin
  if (Trim(MimeType) <> '') then begin
    if (Self.AllowedContentTypeList.IndexOf(MimeType) = -1) then
      Self.AllowedContentTypeList.Add(MimeType);
  end;
end;

procedure TIdSipUserAgentCore.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = -1) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipUserAgentCore.AddAllowedMethod(const Method: String);
begin
  if not TIdSipParser.IsToken(Method) then
    raise EIdSipBadSyntax.Create('Not a token');

  if (Self.AllowedMethodList.IndexOf(Method) = -1) then
    Self.AllowedMethodList.Add(Method);
end;

procedure TIdSipUserAgentCore.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = -1) then
    Self.AllowedSchemeList.Add(Scheme);
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

procedure TIdSipUserAgentCore.AddSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Add(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
  end;
end;

function TIdSipUserAgentCore.AllowedContentTypes: String;
begin
  Result := Self.AllowedContentTypeList.CommaText;
end;

function TIdSipUserAgentCore.AllowedLanguages: String;
begin
  Result := Self.AllowedLanguageList.CommaText;
end;

function TIdSipUserAgentCore.AllowedMethods: String;
begin
  Result := Self.AllowedMethodList.CommaText;
end;

function TIdSipUserAgentCore.AllowedSchemes: String;
begin
  Result := Self.AllowedSchemeList.CommaText;
end;

function TIdSipUserAgentCore.Call(const Dest: TIdSipToHeader;
                                  const InitialOffer: String;
                                  const MimeType: String): TIdSipSession;
begin
  Result := Self.AddOutboundSession;
  Result.Call(Dest, InitialOffer, MimeType);
end;

function TIdSipUserAgentCore.CreateBye(const Dialog: TIdSipDialog): TIdSipRequest;
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

function TIdSipUserAgentCore.CreateInvite(const Dest: TIdSipToHeader;
                                          const Body: String;
                                          const MimeType: String): TIdSipRequest;
begin
  Result := Self.CreateRequest(Dest);
  try
    Result.Method := MethodInvite;

    Result.CSeq.Method     := MethodInvite;
    Result.CSeq.SequenceNo := Self.NextInitialSequenceNo;

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

function TIdSipUserAgentCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
var
  Transport: String;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := Dest.Address;

    if Dest.HasSipsUri then
      Self.Contact.Address.Scheme := SipsScheme;

    Result.AddHeader(Self.Contact);
    Result.CallID      := Self.NextCallID;
    Result.From        := Self.From;
    Result.From.Tag    := Self.NextTag;
    Result.MaxForwards := Result.DefaultMaxForwards;
    Result.ToHeader    := Dest;

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

function TIdSipUserAgentCore.CreateRequest(const Dialog: TIdSipDialog): TIdSipRequest;
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

function TIdSipUserAgentCore.CreateResponse(const Request:      TIdSipRequest;
                                            const ResponseCode: Cardinal): TIdSipResponse;
var
  FirstRR:          TIdSipRecordRouteHeader;
  ReqRecordRoutes:  TIdSipHeadersFilter;
  TimestampHeaders: TIdSipHeadersFilter;
begin
  Result := TIdSipResponse.Create;
  try
    Result.StatusCode := ResponseCode;

    // cf RFC 3261 section 8.2.6.1
    if (Result.StatusCode = SIPTrying) then begin
      TimestampHeaders := TIdSipHeadersFilter.Create(Request.Headers,
                                                     TimestampHeader);
      try
        Result.AddHeaders(TimestampHeaders);
      finally
        TimestampHeaders.Free;
      end;
    end;

    // cf RFC 3261 section 8.2.6.2
    Result.Path         := Request.Path;
    Result.CallID       := Request.CallID;
    Result.CSeq         := Request.CSeq;
    Result.From         := Request.From;
    Result.ToHeader     := Request.ToHeader;

    if not Request.ToHeader.HasTag then
      Result.ToHeader.Tag := Self.NextTag;

    // cf RFC 3261 section 12.1.1
    ReqRecordRoutes := TIdSipHeadersFilter.Create(Request.Headers, RecordRouteHeader);
    try
      Result.AddHeaders(ReqRecordRoutes);

      if (ReqRecordRoutes.Count > 0) then begin
        FirstRR := ReqRecordRoutes.Items[0] as TIdSipRecordRouteHeader;
        if (FirstRR.Address.IsSecure) then
          Self.Contact.Address.Scheme := SipsScheme;
      end;

      if Request.HasSipsUri then
        Self.Contact.Address.Scheme := SipsScheme;

      Result.AddHeader(Self.Contact);
//      Result.AddHeader(Self.From);

      if (Self.UserAgentName <> '') then
        Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
    finally
      ReqRecordRoutes.Free;
    end;
  except
    Result.Free;

    raise;
  end;
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

procedure TIdSipUserAgentCore.ReceiveRequest(const Request: TIdSipRequest;
                                             const Transaction: TIdSipTransaction;
                                             const Receiver: TIdSipTransport);
begin
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
    Self.RejectRequest(Request, SIPUnsupportedURIScheme, Transaction, Receiver);
    Exit;
  end;

  // Merged requests - 8.2.2.2
  if not Request.ToHeader.HasTag and Self.Dispatcher.LoopDetected(Request) then begin
    Self.RejectRequest(Request, SIPLoopDetected, Transaction, Receiver);
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

  // Processing the request - 8.2.5
  if Request.IsInvite then begin
    // Section 8.1.1.8 says that a request that can start a dialog (like an
    // INVITE), MUST contain a Contact.
    if not Request.HasHeader(ContactHeaderFull) then
      Self.RejectBadRequest(Request, MissingContactHeader, Transaction)
    else begin
      Self.ProcessInvite(Request, Transaction, Receiver);
    end;
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
end;

procedure TIdSipUserAgentCore.ReceiveResponse(const Response: TIdSipResponse;
                                              const Transaction: TIdSipTransaction;
                                              const Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  // User Agents drop unmatched responses on the floor.
  // Except for 2xx's on a client INVITE. And these no longer belong to
  // a transaction, since the receipt of a 200 terminates a client INVITE
  // immediately. Hence the unusual clause below.
  if Response.IsOK and not Assigned(Transaction) then begin
    Session := Self.FindSession(Response);
    if Assigned(Session) then
      Session.OnReceiveResponse(Response, Transaction, Receiver);
  end;
end;

function TIdSipUserAgentCore.HasUnknownContentLanguage(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentLanguageHeader)
       and (Self.AllowedLanguageList.IndexOf(Request.FirstHeader(ContentLanguageHeader).Value) = -1);
end;

function TIdSipUserAgentCore.HasUnknownContentEncoding(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipUserAgentCore.HasUnknownContentType(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentTypeHeaderFull)
       and (Self.AllowedContentTypeList.IndexOf(Request.FirstHeader(ContentTypeHeaderFull).Value) = -1);
end;

function TIdSipUserAgentCore.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipUserAgentCore.IsMethodAllowed(const Method: String): Boolean;
begin
  Result := Self.AllowedMethodList.IndexOf(Method) >= 0;
end;

function TIdSipUserAgentCore.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipUserAgentCore.NextBranch: String;
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

function TIdSipUserAgentCore.NextInitialSequenceNo: Cardinal;
begin
  Result := TIdRandomNumber.NextCardinal($80000000 - 1);
end;

function TIdSipUserAgentCore.NextTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := IntToHex(TIdRandomNumber.NextCardinal, 8)
          + IntToHex(TIdRandomNumber.NextCardinal, 8);
end;

procedure TIdSipUserAgentCore.RejectRequest(const Request: TIdSipRequest;
                                            const Reason: Cardinal;
                                            const Transaction: TIdSipTransaction;
                                            const Receiver: TIdSipTransport);
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

procedure TIdSipUserAgentCore.RemoveObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Remove(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.RemoveSession(const Session: TIdSipSession);
begin
  Self.SessionLock.Acquire;
  try
    Self.Sessions.Remove(Session);
  finally
    Self.SessionLock.Release;
  end;
  Self.NotifyOfChange;
end;

procedure TIdSipUserAgentCore.RemoveSessionListener(const Listener: IIdSipSessionListener);
begin
  Self.SessionListenerLock.Acquire;
  try
    Self.SessionListeners.Remove(Pointer(Listener));
  finally
    Self.SessionListenerLock.Release;
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

function TIdSipUserAgentCore.AddInboundSession(const Invite: TIdSipRequest;
                                               const Transaction: TIdSipTransaction;
                                               const Receiver: TIdSipTransport): TIdSipSession;
begin
  Result := TIdSipSession.Create(Self, Invite, Transaction, Receiver);
  try
    Self.SessionLock.Acquire;
    try
      Self.Sessions.Add(Result);
    finally
      Self.SessionLock.Release;
    end;

    Self.NotifyOfNewSession(Result);
    Self.NotifyOfChange;
  except
    FreeAndNil(Result);

    raise;
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

function TIdSipUserAgentCore.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
end;

procedure TIdSipUserAgentCore.NotifyOfNewSession(const Session: TIdSipSession);
var
  I: Integer;
begin
  Self.SessionListenerLock.Acquire;
  try
    for I := 0 to Self.SessionListeners.Count - 1 do
      IIdSipSessionListener(Self.SessionListeners[I]).OnNewSession(Session);
  finally
    Self.SessionListenerLock.Release;
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

procedure TIdSipUserAgentCore.ProcessAck(const Ack: TIdSipRequest;
                                         const Transaction: TIdSipTransaction;
                                         const Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Ack);

  // If Session = nil then we didn't match the ACK against any session, and so
  // we just drop it on the floor. There's no point in replying.
  if Assigned(Session) then
    Session.OnReceiveRequest(Ack, Transaction, Receiver);
end;

procedure TIdSipUserAgentCore.ProcessInvite(const Invite: TIdSipRequest;
                                            const Transaction: TIdSipTransaction;
                                            const Receiver: TIdSipTransport);
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

procedure TIdSipUserAgentCore.RejectBadRequest(const Request: TIdSipRequest;
                                               const Reason: String;
                                               const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectRequestBadExtension(const Request: TIdSipRequest;
                                                        const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectRequestMethodNotAllowed(const Request: TIdSipRequest;
                                                            const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectRequestUnknownContentEncoding(const Request: TIdSipRequest;
                                                                  const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectRequestUnknownContentLanguage(const Request: TIdSipRequest;
                                                                  const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectRequestUnknownContentType(const Request: TIdSipRequest;
                                                              const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.RejectUnsupportedSipVersion(const Request: TIdSipRequest;
                                                          const Transaction: TIdSipTransaction);
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

procedure TIdSipUserAgentCore.ResetLastBranch;
begin
  Self.BranchLock.Acquire;
  try
    Self.fLastBranch := 0;
  finally
    Self.BranchLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.SendByeToAppropriateSession(const Bye: TIdSipRequest;
                                                          const Transaction: TIdSipTransaction;
                                                          const Receiver: TIdSipTransport);
var
  Session: TIdSipSession;
begin
  Session := Self.FindSession(Bye);

  if Assigned(Session) then
    Session.OnReceiveRequest(Bye,
                             Transaction,
                             Receiver)
  else
    Self.RejectRequest(Bye,
                       SIPCallLegOrTransactionDoesNotExist,
                       Transaction,
                       Receiver);

end;

procedure TIdSipUserAgentCore.SetContact(const Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'A wildcard Contact header may not be used here');

  Self.Contact.Assign(Value);
end;

procedure TIdSipUserAgentCore.SetFrom(const Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);
end;

//******************************************************************************
//* TIdSipSessionTimer                                                         *
//******************************************************************************
//* TIdSipSessionTimer Public methods ******************************************

constructor TIdSipSessionTimer.Create(const Session: TIdSipSession;
                                      const T1: Cardinal;
                                      const T2: Cardinal);
begin
  inherited Create;

  Self.Lock := TCriticalSection.Create;

  Self.T1 := T1;
  Self.T2 := T2;

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
    Self.Timer.Interval := 2*Self.Timer.Interval;
  finally
    Self.Lock.Free;
  end;
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
    Self.Lock.Free;
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

constructor TIdSipSession.Create(const UA: TIdSipUserAgentCore);
begin
  inherited Create;

  Self.CreateInternal(UA);
  Self.IsInboundCall := false;
end;

constructor TIdSipSession.Create(const UA: TIdSipUserAgentCore;
                                 const Invite: TIdSipRequest;
                                 const InitialTransaction: TIdSipTransaction;
                                 const Receiver: TIdSipTransport);
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

procedure TIdSipSession.Call(const Dest: TIdSipToHeader;
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

procedure TIdSipSession.OnReceiveRequest(const Request: TIdSipRequest;
                                         const Transaction: TIdSipTransaction;
                                         const Receiver: TIdSipTransport);
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
    Self.NotifyOfEndedSession;
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

procedure TIdSipSession.AddOpenTransaction(const Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Add(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.ApplyTo(const List: TList;
                                const Lock: TCriticalSection;
                                Proc: TIdSipProcedure);
var
  I: Integer;
begin
  Lock.Acquire;
  try
    for I := 0 to List.Count - 1 do
      Proc(List[I]);
  finally
    Lock.Release;
  end;
end;

procedure TIdSipSession.CreateInternal(const UA: TIdSipUserAgentCore);
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

function TIdSipSession.CreateInboundDialog(const Response: TIdSipResponse): TIdSipDialog;
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

function TIdSipSession.CreateOutboundDialog(const Response: TIdSipResponse;
                                           const Receiver: TIdSipTransport): TIdSipDialog;
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

function TIdSipSession.CreateCancel(const Invite: TIdSipRequest): TIdSipRequest;
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

procedure TIdSipSession.NotifyOfEndedSession;
begin
  Self.ApplyTo(Self.SessionListeners,
               Self.SessionListenerLock,
               Self.NotifyOfEndedSessionProc);

  Self.Core.RemoveSession(Self);
end;

procedure TIdSipSession.NotifyOfEndedSessionProc(ObjectOrIntf: Pointer);
begin
  IIdSipSessionListener(ObjectOrIntf).OnEndedSession(Self);
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

procedure TIdSipSession.NotifyOfModifiedSession(const Invite: TIdSipRequest);
begin
  Self.ApplyTo(Self.SessionListeners,
               Self.SessionListenerLock,
               Self.NotifyOfModifiedSessionProc);
end;

procedure TIdSipSession.NotifyOfModifiedSessionProc(ObjectOrIntf: Pointer);
begin
  IIdSipSessionListener(ObjectOrIntf).OnModifiedSession(Self, Invite);
end;

procedure TIdSipSession.OnFail(const Transaction: TIdSipTransaction;
                               const Reason: String);
begin
  if (Transaction = Self.InitialTran) then
    Self.MarkAsTerminated;
end;

procedure TIdSipSession.OnReceiveResponse(const Response: TIdSipResponse;
                                          const Transaction: TIdSipTransaction;
                                          const Receiver: TIdSipTransport);
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

  if (Transaction = Self.InitialTran) then begin
    if Response.IsFinal and not Response.IsOK then begin
      Self.MarkAsTerminated;
      Self.NotifyOfEndedSession;
    end;
  end;
end;

procedure TIdSipSession.OnTerminated(const Transaction: TIdSipTransaction);
begin
  Self.RemoveTransaction(Transaction);

  if Self.IsTerminated then
    Self.NotifyOfEndedSession;
end;

procedure TIdSipSession.RejectOutOfOrderRequest(const Request: TIdSipRequest;
                                                const Transaction: TIdSipTransaction);
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

procedure TIdSipSession.RejectRequest(const Request: TIdSipRequest;
                                      const Transaction: TIdSipTransaction);
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

procedure TIdSipSession.RemoveTransaction(const Transaction: TIdSipTransaction);
begin
  Self.OpenTransactionLock.Acquire;
  try
    Self.OpenTransactions.Remove(Transaction);
  finally
    Self.OpenTransactionLock.Release;
  end;
end;

procedure TIdSipSession.SendAck(const Final: TIdSipResponse);
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

procedure TIdSipSession.TerminateOpenTransaction(const Transaction: TIdSipTransaction);
begin
  if not Transaction.IsClient then
    Self.RejectRequest(Transaction.InitialRequest, Transaction);
end;

end.
