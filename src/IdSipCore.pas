unit IdSipCore;

// Some overarching principles followed in this implementation of a SIP/2.0
// (RFC 3261) stack:
// * The lifetime of all objects is manually managed. Objects that implement
//   interfaces are NOT reference counted.
// * Value Objects are used when possible.
// * If an object A receives some object B that it is expected to store as data
//   then A must store a COPY of B. Typical objects are: TIdURI, TIdSipDialogID,
//   TIdSipMessage.
// * Each layer is aware of the layers beneath it. We try to make each layer
//   aware of ONLY the layer immediately below it, but that's not always
//   possible.
// * Events or Listeners are used to propogate messages up the stack, and method
//   calls to propogate messages down the stack. Preference is given to Listeners
//   as they're more flexible.
// * Typecasting is avoided as much as possible by using polymorphism and, in
//   certain situations where polymorphism can't cut it, the Visitor pattern.
// * TObjectLists always manage the lifetime of the objects they contain. Except
//   in the case of Transports in the Dispatcher.

interface

uses
  Classes, Contnrs, IdSipDialog, IdException, IdSipHeaders,
  IdSipInterfacedObject, IdSipMessage, IdSipTransaction, IdSipTransport, IdUri,
  SyncObjs;

type
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(const Session: TIdSipSession) of object;

  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // It's unfortunate that I never know the type of Observed and have to
  // typecast the Observed, but c'est la vie.
  IIdSipObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(const Observed: TObject);
  end;

  // I am the protocol of things that listen for Sessions:
  // * OnNewSession tells us that someone is calling us - we may refuse or
  //   allow the session.
  // * OnSessionEstablished tells us when a session is fully up and running.
  // * OnSessionEnded lets us clean up. The Session referenced is no longer
  //   valid after this point, and its very existence is not guaranteed. In
  //   other words, you'd better say goodbye to the Session in this method,
  //   because it's the last time you'll see it.
  IIdSipSessionListener = interface
    ['{59B3C476-D3CA-4C5E-AA2B-2BB587A5A716}']
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewSession(const Session: TIdSipSession);
  end;

  TIdSipAbstractCore = class(TIdSipInterfacedObject,
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

  // I am a User Agent. I (usually) represent a human being in the SIP network.
  // It is my responsibility to:
  // * inform any listeners when new sessions are established, modified or ended;
  // * allow my users to accept incoming "calls", make outgoing "calls";
  // * clean up Sessions that are established
  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    BranchLock:           TCriticalSection;
    fAllowedLanguageList: TStrings;
    fAllowedMethodList:   TStrings;
    fAllowedSchemeList:   TStrings;
    fContact:             TIdSipContactHeader;
    fFrom:                TIdSipFromHeader;
    fLastBranch:          Cardinal;
    fUserAgentName:       String;
    ObserverLock:         TCriticalSection;
    Observers:            TList;
    SessionListenerLock:  TCriticalSection;
    SessionListeners:     TList;
    SessionLock:          TCriticalSection;
    Sessions:             TObjectList;

    function  AddInboundSession(const Invite: TIdSipRequest;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport): TIdSipSession;
    function  AddOutboundSession: TIdSipSession;
    function  FindSession(const Msg: TIdSipMessage): TIdSipSession;
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure NotifyOfNewSession(const Session: TIdSipSession);
    procedure NotifyOfChange;
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

    property AllowedLanguageList: TStrings read fAllowedLanguageList;
    property AllowedMethodList:   TStrings read fAllowedMethodList;
    property AllowedSchemeList:   TStrings read fAllowedSchemeList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedMethod(const Method: String);
    procedure AddAllowedScheme(const Scheme: String);
    procedure AddObserver(const Listener: IIdSipObserver);
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    function  Call(const Dest: TIdSipToHeader): TIdSipSession;
    function  CreateBye(const Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateCancel(const Dialog: TIdSipDialog): TIdSipRequest;
    function  CreateInvite(const Dest: TIdSipToHeader; const Body: String = ''): TIdSipRequest;
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; overload; override;
    function  CreateRequest(const Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  CreateResponse(const Request: TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    function DefaultHostName: String;
    function DefaultUserAgent: String;
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
    function  NextTag: String;
    procedure RejectRequest(const Request: TIdSipRequest;
                            const Reason: Cardinal;
                            const Transaction: TIdSipTransaction;
                            const Receiver: TIdSipTransport);
    procedure RemoveObserver(const Listener: IIdSipObserver);
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);
    procedure RemoveSession(const Session: TIdSipSession);
    function  SessionCount: Integer;

    property Contact:       TIdSipContactHeader read GetContact write SetContact;
    property From:          TIdSipFromHeader    read GetFrom write SetFrom;
    property UserAgentName: String              read fUserAgentName write fUserAgentName;
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
  TIdSipSession = class(TIdSipInterfacedObject, IIdSipTransactionListener)
  private
    fCore:               TIdSipUserAgentCore;
    fDialog:             TIdSipDialog;
    fInvite:             TIdSipRequest;
    fIsEstablished:      Boolean;
    fIsTerminated:       Boolean;
    InitialTran:         TIdSipTransaction;
    InitialTransport:    TIdSipTransport;
    IsInboundCall:       Boolean;
    OpenTransactionLock: TCriticalSection;
    OpenTransactions:    TList;
    SessionListenerLock: TCriticalSection;
    SessionListeners:    TList;

    procedure AddOpenTransaction(const Transaction: TIdSipTransaction);
    procedure ApplyTo(const List: TList; const Lock: TCriticalSection; Proc: TIdSipProcedure);
    procedure CreateInternal(const UA: TIdSipUserAgentCore);
    procedure MarkAsTerminated;
    procedure MarkAsTerminatedProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEndedSession;
    procedure NotifyOfEndedSessionProc(ObjectOrIntf: Pointer);
    procedure NotifyOfEstablishedSession;
    procedure NotifyOfEstablishedSessionProc(ObjectOrIntf: Pointer);
    procedure NotifyOfModifiedSession(const Invite: TIdSipRequest);
    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Receiver: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
    procedure RejectRequest(const Request: TIdSipRequest;
                            const Transaction: TIdSipTransaction);
    procedure RemoveTransaction(const Transaction: TIdSipTransaction);
    procedure TerminateOpenTransaction(const Transaction: TIdSipTransaction);

    property Core:          TIdSipUserAgentCore read fCore;
    property Invite:        TIdSipRequest       read fInvite;
    property IsEstablished: Boolean             read fIsEstablished write fIsEstablished;
  public
    constructor Create(const UA: TIdSipUserAgentCore); overload;
    constructor Create(const UA: TIdSipUserAgentCore;
                       const Invite: TIdSipRequest;
                       const InitialTransaction: TIdSipTransaction;
                       const Receiver: TIdSipTransport); overload;
    destructor  Destroy; override;

    procedure AcceptCall;
    procedure AddSessionListener(const Listener: IIdSipSessionListener);
    procedure Cancel;
    procedure Call(const Dest: TIdSipToHeader);
    procedure Terminate;
    procedure Modify;
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Receiver: TIdSipTransport);
    procedure RemoveSessionListener(const Listener: IIdSipSessionListener);

    property Dialog:       TIdSipDialog read fDialog;
    property IsTerminated: Boolean      read fIsTerminated;
  end;

  EIdSipBadSyntax = class(EIdException);

const
  MissingContactHeader = 'Missing Contact Header';

implementation

uses
  IdGlobal, IdSipConsts, IdSipDialogID, IdSipRandom, IdStack, SysUtils;

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
  Result := IntToHex(TIdSipRandomNumber.Next, 8) + '@' + Self.HostName;
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
  Self.fAllowedLanguageList := TStringList.Create;
  Self.fAllowedMethodList := TStringList.Create;
  Self.fAllowedSchemeList := TStringList.Create;

  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);

  Self.AddAllowedScheme(SipScheme);

  Self.HostName      := Self.DefaultHostName;
  Self.UserAgentName := Self.DefaultUserAgent;
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;
  Self.AllowedLanguageList.Free;
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

function TIdSipUserAgentCore.Call(const Dest: TIdSipToHeader): TIdSipSession;
begin
  Result := Self.AddOutboundSession;
  Result.Call(Dest);
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

function TIdSipUserAgentCore.CreateCancel(const Dialog: TIdSipDialog): TIdSipRequest;
begin
  try
    Result := Self.CreateRequest(Dialog);
    Result.Method      := MethodCancel;
    Result.CSeq.Method := Result.Method;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateInvite(const Dest: TIdSipToHeader; const Body: String = ''): TIdSipRequest;
begin
  Result := CreateRequest(Dest);
  Result.Method := MethodInvite;

  Result.CSeq.Method     := MethodInvite;
  Result.CSeq.SequenceNo := 0;

  Result.Body := Body;
  Result.ContentLength := Length(Body);
end;

function TIdSipUserAgentCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
var
  Transport: String;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := Dest.Address;

    if Dest.HasSipsUri then
      Self.Contact.Address.Protocol := SipsScheme;

    Result.AddHeader(Self.Contact);
    Result.CallID   := Self.NextCallID;
    Result.From     := Self.From;
    Result.From.Tag := Self.NextTag;
    Result.ToHeader := Dest;

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
  I:          Integer;
begin
  Result := TIdSipRequest.Create;
  try
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
      FirstRoute := Dialog.RouteSet.Items[0] as TIdSipRouteHeader;

      if FirstRoute.IsLooseRoutable then begin
        Result.RequestUri := Dialog.RemoteTarget;

        for I := 0 to Dialog.RouteSet.Count - 1 do
          Result.AddHeader(RouteHeader).Assign(Dialog.RouteSet.Items[I]);
      end
      else begin
        Result.RequestUri := FirstRoute.Address;

        // Yes, from 1 to count - 1. We use the 1st entry as the Request-URI,
        // remember?
        // No, we can't just Assign() here because (a) we're not adding ALL
        // the headers, and (b) we're adding Route headers, and RouteSet
        // contains Record-Route headers.
        for I := 1 to Dialog.RouteSet.Count - 1 do begin
          Result.AddHeader(RouteHeader).Value := Dialog.RouteSet.Items[I].Value
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
    Result.CallID       := Request.CallID;
    Result.CSeq         := Request.CSeq;
    Result.From         := Request.From;
    Result.ToHeader     := Request.ToHeader;
    Result.ToHeader.Tag := Self.NextTag;
    Result.Path         := Request.Path;

    // cf RFC 3261 section 12.1.1
    ReqRecordRoutes := TIdSipHeadersFilter.Create(Request.Headers, RecordRouteHeader);
    try
      Result.AddHeaders(ReqRecordRoutes);

      if (ReqRecordRoutes.Count > 0) then begin
        FirstRR := ReqRecordRoutes.Items[0] as TIdSipRecordRouteHeader;
        if (FirstRR.Address.Protocol = SipsScheme) then
          Self.Contact.Address.Protocol := SipsScheme;
      end;

      if Request.HasSipsUri then
        Self.Contact.Address.Protocol := SipsScheme;

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
  if not Self.IsMethodAllowed(Request.Method) then begin
    Self.RejectRequestMethodNotAllowed(Request, Transaction);
    Exit;
  end;

  // inspect the headers - 8.2.2

  // To & Request-URI - 8.2.2.1
  if not Self.IsSchemeAllowed(Request.RequestUri.Protocol) then begin
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
  else if Request.IsBye then begin
    Self.SendByeToAppropriateSession(Request, Transaction, Receiver);
  end
  else if Request.IsCancel then
    raise Exception.Create('Handling CANCELs not implemented yet');

  // Generating the response - 8.2.6
end;

procedure TIdSipUserAgentCore.ReceiveResponse(const Response: TIdSipResponse;
                                              const Transaction: TIdSipTransaction;
                                              const Receiver: TIdSipTransport);
begin
  // User Agents drop unmatched responses on the floor.
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
       and (Request.FirstHeader(ContentTypeHeaderFull).Value <> SdpMimeType);
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

function TIdSipUserAgentCore.NextTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := IntToHex(TIdSipRandomNumber.Next, 8)
          + IntToHex(TIdSipRandomNumber.Next, 8);
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
  I:           Integer;
begin
  Result := nil;
  DialogID := TIdSipDialogID.Create(Msg.CallID,
                                    Msg.ToHeader.Tag,
                                    Msg.From.Tag);
  try
    Self.SessionLock.Acquire;
    try
      I := 0;
      while (I < Self.Sessions.Count) and not Assigned(Result) do begin
        if (Self.Sessions[I] as TIdSipSession).Dialog.ID.IsEqualTo(DialogID) then
          Result := Self.Sessions[I] as TIdSipSession
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
    Session.OnReceiveRequest(Bye, Transaction, Receiver)
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

  Assert((Value.Address.Protocol = SipScheme) or (Value.Address.Protocol = SipsScheme),
         'Only SIP or SIPS URIs may be used.');

  Self.Contact.Assign(Value);
end;

procedure TIdSipUserAgentCore.SetFrom(const Value: TIdSipFromHeader);
begin
  Assert((Value.Address.Protocol = SipScheme) or (Value.Address.Protocol = SipsScheme),
         'Only SIP or SIPS URIs may be used.');

  Self.From.Assign(Value);
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

  Self.Invite.Free;

  inherited Destroy;
end;

procedure TIdSipSession.AcceptCall;
var
  ID:       TIdSipDialogID;
  Response: TIdSipResponse;
  RouteSet: TIdSipHeaderList;
begin
  if Self.IsInboundCall then begin
    Response := Self.Core.CreateResponse(Invite, SIPOK);
    try
      Response.ToHeader.Tag := Self.Core.NextTag;

      if not Assigned(Self.Dialog) then begin
        ID := TIdSipDialogID.Create(Response.CallID,
                                    Response.ToHeader.Tag,
                                    Self.Invite.From.Tag);
        try
          RouteSet := TIdSipHeadersFilter.Create(Invite.Headers,
                                                 RecordRouteHeader);
          try
            fDialog := TIdSipDialog.Create(ID,
                                           0,
                                           Invite.CSeq.SequenceNo,
                                           Invite.ToHeader.Address,
                                           Invite.From.Address,
                                           Invite.FirstContact.Address,
                                           Self.InitialTransport.IsSecure and (Invite.HasSipsUri),
                                           RouteSet);
            Self.NotifyOfEstablishedSession;
          finally
            RouteSet.Free;
          end;
        finally
          ID.Free;
        end;
      end;

      Self.Dialog.HandleMessage(Invite);
      Self.Dialog.HandleMessage(Response);

      Self.InitialTran.SendResponse(Response);
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

procedure TIdSipSession.Call(const Dest: TIdSipToHeader);
var
  Invite: TIdSipRequest;
begin
  if not Self.IsInboundCall and not Assigned(Self.InitialTran) then begin
    Invite := Self.Core.CreateInvite(Dest);
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
var
  Bye:        TIdSipRequest;
  ClientTran: TIdSipTransaction;
begin
  Bye := Self.Core.CreateBye(Self.Dialog);
  try
    ClientTran := Self.Core.Dispatcher.AddClientTransaction(Bye);
    ClientTran.SendRequest;
  finally
    Bye.Free;
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
  else if Request.IsInvite then begin
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
  Self.fCore := UA;
  Self.fInvite := TIdSipRequest.Create;
  Self.IsEstablished := false;

  Self.OpenTransactionLock := TCriticalSection.Create;
  Self.OpenTransactions    := TList.Create;

  Self.SessionListenerLock := TCriticalSection.Create;
  Self.SessionListeners    := TList.Create;
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
var
  I: Integer;
begin
  Self.SessionListenerLock.Acquire;
  try
    for I := 0 to Self.SessionListeners.Count - 1 do
      IIdSipSessionListener(Self.SessionListeners[I]).OnModifiedSession(Self,
                                                                        Invite);
  finally
    Self.SessionListenerLock.Release;
  end;
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
var
  ID:       TIdSipDialogID;
  RouteSet: TIdSipHeadersFilter;
begin
  if not Assigned(Self.Dialog) then begin
    ID := TIdSipDialogID.Create(Self.Invite.CallID,
                                Self.Invite.From.Tag,
                                Response.ToHeader.Tag);
    try
      RouteSet := TIdSipHeadersFilter.Create(Self.Invite.Headers,
                                             RecordRouteHeader);
      try
        fDialog := TIdSipDialog.Create(ID,
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

  Self.Dialog.HandleMessage(Response);
  if (Transaction = Self.InitialTran) then begin
    if Response.IsFinal and (Response.StatusCode <> SIPOK) then begin
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

procedure TIdSipSession.TerminateOpenTransaction(const Transaction: TIdSipTransaction);
begin
  if not Transaction.IsClient then
    Self.RejectRequest(Transaction.InitialRequest, Transaction);
end;

end.
