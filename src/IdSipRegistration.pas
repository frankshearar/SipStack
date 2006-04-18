{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipRegistration;

interface

uses
  Classes, Contnrs, IdException, IdObservable, IdSipCore, IdSipMessage,
  IdNotification, IdTimerQueue, SyncObjs;

type
  TIdSipRegistrationInfo = class(TObject)
  private
    fCallID:     String;
    fRegistrar:  TIdSipUri;
    fSequenceNo: Cardinal;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create;
    destructor  Destroy; override;

    property CallID:     String    read fCallID write fCallID;
    property Registrar:  TIdSipUri read fRegistrar write SetRegistrar;
    property SequenceNo: Cardinal  read fSequenceNo write fSequenceNo;
  end;

  // I keep track of information a User Agent needs when making a REGISTER to
  // a particular registrar.
  // I store registration information for registrars with which you've
  // registered.
  TIdSipRegistrations = class(TObject)
  private
    KnownRegistrars: TObjectList;
    Lock:            TCriticalSection;

    function IndexOfRegistrar(Registrar: TIdSipUri): Integer;
    function KnowsRegistrar(Registrar: TIdSipUri): Boolean;
    function RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddKnownRegistrar(Registrar: TIdSipUri;
                                const CallID: String;
                                SequenceNo: Cardinal);
    function  CallIDFor(Registrar: TIdSipUri): String;
    function  NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
  end;

  // I represent a binding between and address-of-record and a URI. A SIP
  // Registrar (via a Binding Database) uses me to keep track of
  // registrations.
  TIdRegistrarBinding = class(TObject)
  private
    fAddressOfRecord: String;
    fCallID:          String;
    fInstanceID:      String;
    fSequenceNo:      Cardinal;
    fUri:             String;
    fValidUntil:      TDateTime;
  public
    constructor Create(const AddressOfRecord: String;
                       const CanonicalisedUri: String;
                       const InstanceID: String;
                       const CallID: String;
                       SequenceNo: Cardinal;
                       AbsoluteTimeout: TDateTime);

    property AddressOfRecord: String    read fAddressOfRecord write fAddressOfRecord;
    property CallID:          String    read fCallID write fCallID;
    property InstanceID:      String    read fInstanceID write fInstanceID;
    property SequenceNo:      Cardinal  read fSequenceNo write fSequenceNo;
    property Uri:             String    read fUri write fUri;
    property ValidUntil:      TDateTime read fValidUntil write fValidUntil;
  end;

  // I represent the relationship between an address-of-record, an instance-id,
  // and a GRUU: there is a one-to-many relationship between (address-of-record,
  // instance-id) ordered pairs and GRUUs.
  TIdGruuBinding = class(TObject)
  private
    fBinding:    String;
    fGruu:       String;
    fInstanceID: String;
  public
    constructor Create(const Binding: String;
                       const InstanceID: String;
                       const Gruu: String);

    property Binding:    String read fBinding write fBinding;
    property Gruu:       String read fGruu write fGruu;
    property InstanceID: String read fInstanceID write fInstanceID;
  end;

  // I provide a database that matches a SIP Address-of-record (a SIP or SIPS
  // URI) with one or more "bindings" (usually SIP/SIPS URIs, but they can
  // follow any URI scheme).
  //
  // My subclasses guarantee ACID properties for all of their implementations
  // of my methods. All the methods that actually affect records return True
  // if they succeeded and False otherwise. The predicate methods behave
  // almost the same, but we can't distinguish between the cases where we
  // can't authenticate a user's credentials (because the password didn't
  // match) and a failure to read the user's details (because a disk failed).
  // That works just fine though - it means that authorization/authentication
  // fails safe.
  //
  // Currently I only support the registration of SIP and SIPS URIs.
  TIdSipAbstractBindingDatabase = class(TIdObservable)
  private
    fDefaultExpiryTime: Cardinal;
    fUseGruu:           Boolean;

    function CorrectExpiry(Request: TIdSipRequest;
                           Contact: TIdSipContactHeader): Cardinal;
  protected
    function  AddBinding(const AddressOfRecord: String;
                         Contact: TIdSipContactHeader;
                         const CallID: String;
                         SequenceNo: Cardinal;
                         ExpiryTime: TDateTime): Boolean; virtual; abstract;
    function  Binding(const AddressOfRecord: String;
                      const CanonicalUri: String): TIdRegistrarBinding; virtual; abstract;
    function  CollectBindingsFor(const AddressOfRecord: String;
                                 Bindings: TIdSipContacts;
                                 CollectGruus: Boolean): Boolean; virtual;
    function  CreateGruu(const AddressOfRecord: String;
                         const SipInstance: String): String; virtual;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
  public
    constructor Create; override;

    function  AddBindings(Request: TIdSipRequest): Boolean;
    function  IsAuthorized(User: TIdSipAddressHeader;
                           AddressOfRecord: TIdSipUri): Boolean; virtual; abstract;
    function  IsValid(Request: TIdSipRequest): Boolean; virtual;
    function  BindingExpires(const AddressOfRecord: String;
                             const CanonicalUri: String): TDateTime;
    function  BindingsFor(Request: TIdSipRequest;
                          Contacts: TIdSipContacts): Boolean; virtual;
    function  NotOutOfOrder(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean;
    function  RemoveAllBindings(Request: TIdSipRequest): Boolean;
    function  RemoveBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; virtual; abstract;

    property DefaultExpiryTime: Cardinal read fDefaultExpiryTime write fDefaultExpiryTime;
    property UseGruu:           Boolean  read fUseGruu write fUseGruu;
  end;

  TIdSipOutboundRegistrationBase = class;

  // I provide a protocol for using a registrar. You send a REGISTER, and
  // listen for the events below.
  //
  // OnFailure and OnSuccess, apart from the obvious, tell you that the
  // registration agent has terminated, and that you should remove all
  // of your references to it.
  IIdSipRegistrationListener = interface(IIdSipActionListener)
    ['{D3FA9A3D-ED8A-48D3-8068-38E8F9EE2140}']
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                        ErrorCode: Cardinal;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                        CurrentBindings: TIdSipContacts);
  end;

  TIdSipRegisterModule = class;

  TIdSipRegistrar = class(TIdSipAbstractCore)
  private
    RegisterModule: TIdSipRegisterModule;

    function  GetBindingDB: TIdSipAbstractBindingDatabase;
    function  GetDefaultRegistrationExpiryTime: Cardinal;
    function  GetMinimumExpiryTime: Cardinal;
    procedure SetBindingDB(Value: TIdSipAbstractBindingDatabase);
    procedure SetDefaultRegistrationExpiryTime(Value: Cardinal);
    procedure SetMinimumExpiryTime(Value: Cardinal);
  protected
    function  GetUseGruu: Boolean; override;
    procedure SetUseGruu(Value: Boolean); override;
  public
    constructor Create; override;

    function RegistrationCount: Integer;

    property BindingDB:                     TIdSipAbstractBindingDatabase read GetBindingDB write SetBindingDB;
    property DefaultRegistrationExpiryTime: Cardinal                      read GetDefaultRegistrationExpiryTime write SetDefaultRegistrationExpiryTime;
    property MinimumExpiryTime:             Cardinal                      read GetMinimumExpiryTime write SetMinimumExpiryTime;
  end;

  TIdSipOutboundRegistration = class;
  TIdSipOutboundRegistrationQuery = class;
  TIdSipOutboundUnregistration = class;

  // I implement that functionality necessary for a User Agent to issue REGISTER
  // messages, that is, to register with a registrar.
  TIdSipOutboundRegisterModule = class(TIdSipMessageModule)
  private
    fAutoReRegister: Boolean;
    fHasRegistrar:   Boolean;
    fRegistrar:      TIdSipUri;
    fRequireGRUU:    Boolean;
    KnownRegistrars: TIdSipRegistrations;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create(UA: TIdSipAbstractCore); override;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; override;
    procedure CleanUp; override;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
    function  RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegistration;
    function  RegistrationCount: Integer;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregistration;
    function  WillAccept(Request: TIdSipRequest): Boolean; override;

    property AutoReRegister: Boolean   read fAutoReRegister write fAutoReRegister;
    property HasRegistrar:   Boolean   read fHasRegistrar write fHasRegistrar;
    property Registrar:      TIdSipUri read fRegistrar write SetRegistrar;
    property RequireGRUU:    Boolean   read fRequireGRUU write fRequireGRUU;
  end;

  // I implement that functionality necessary for a User Agent to respond to
  // REGISTER messages, that is, to act as a registrar.
  TIdSipRegisterModule = class(TIdSipMessageModule)
  private
    fBindingDB:         TIdSipAbstractBindingDatabase;
    fMinimumExpiryTime: Cardinal; // in seconds
  public
    constructor Create(UA: TIdSipAbstractCore); override;

    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;

    property BindingDB:         TIdSipAbstractBindingDatabase read fBindingDB write fBindingDB;
    property MinimumExpiryTime: Cardinal                      read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

  TIdSipRegister = class(TIdSipOwnedAction)
  public
    function IsRegistration: Boolean; override;
    function Method: String; override;
  end;

  TIdSipRegistration = class(TIdSipOwningAction)
  public
    function IsRegistration: Boolean; override;
    function Method: String; override;
  end;

  TIdSipInboundRegistration = class(TIdSipRegistration)
  private
    RegisterModule: TIdSipRegisterModule;

    function  AcceptRequest(Request: TIdSipRequest): Boolean;
    function  BindingDB: TIdSipAbstractBindingDatabase;
    procedure RejectExpireTooBrief(Request: TIdSipRequest);
    procedure RejectFailedRequest(Request: TIdSipRequest);
    procedure RejectForbidden(Request: TIdSipRequest);
    procedure RejectNotFound(Request: TIdSipRequest);
    procedure RejectRequest(Request: TIdSipRequest;
                            StatusCode: Cardinal);
    procedure ReturnCurrentBindings(Request: TIdSipRequest);
    procedure SendSimpleResponse(Request: TIdSipRequest;
                                StatusCode: Cardinal);
    procedure TryRemoveAllBindings(Request: TIdSipRequest);
    function  WillRemoveAllBindings(Request: TIdSipRequest): Boolean;
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    function  IsInbound: Boolean; override;
    procedure ReceiveRequest(Register: TIdSipRequest); override;
  end;

  // I piggyback on a transaction in a blocking I/O fashion to provide a UAC
  // with a way to register with a registrar. I take care of things like
  // doing stuff with error responses, asking for authentication, etc.
  //
  // It makes no sense to access me once my Transaction has terminated. In
  // other words once you've received notification of my success or failure,
  // erase your references to me.
  TIdSipOutboundRegisterBase = class(TIdSipRegister)
  private
    fBindings:  TIdSipContacts;
    fRegistrar: TIdSipUri;
    OutModule:  TIdSipOutboundRegisterModule;

    procedure ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
                                             MinimumExpiry: Cardinal);
    procedure RetryWithoutExtensions(Registrar: TIdSipUri;
                                     Response: TIdSipResponse);
    procedure SetBindings(Value: TIdSipContacts);
    procedure SetRegistrar(Value: TIdSipUri);
  protected
    function  CreateNewAttempt: TIdSipRequest; override;
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; virtual;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult; override;
    function  ReceiveOKResponse(Response: TIdSipResponse;
                                UsingSecureTransport: Boolean): TIdSipActionResult; override;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Bindings: TIdSipContacts); overload;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Contact: TIdSipContactHeader); overload;
    procedure SendRequest(Request: TIdSipRequest); overload; override;
    procedure Unregister(Registrar: TIdSipUri);
  public
    destructor Destroy; override;

    property Bindings:  TIdSipContacts read fBindings write SetBindings;
    property Registrar: TIdSipUri      read fRegistrar write SetRegistrar;
  end;

  TIdSipOutboundRegisterQuery = class(TIdSipOutboundRegisterBase)
  public
    procedure Send; override;
  end;

  TIdSipOutboundRegister = class(TIdSipOutboundRegisterBase)
  public
    procedure Send; override;
  end;

  TIdSipOutboundUnregister = class(TIdSipOutboundRegisterBase)
  private
    fIsWildCard: Boolean;
  protected
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    procedure Send; override;

    property IsWildCard: Boolean read fIsWildCard write fIsWildCard;
  end;

  TIdSipOutboundRegistrationBase = class(TIdSipRegistration,
                                         IIdSipActionListener,
                                         IIdSipOwnedActionListener,
                                         IIdSipActionRedirectorListener)
  private
    ChallengedAction:      TIdSipAction;
    fBindings:             TIdSipContacts;
    fRegistrar:            TIdSipUri;
    OutModule:             TIdSipOutboundRegisterModule;
    Redirector:            TIdSipActionRedirector;
    RegistrationListeners: TIdNotificationList;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse);
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String); overload;
    procedure OnFailure(Redirector: TIdSipActionRedirector;
                        Response: TIdSipResponse); overload;
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String); overload;
    procedure OnNewAction(Redirector: TIdSipActionRedirector;
                          NewAction: TIdSipAction);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                ErrorCode: Cardinal;
                                const Reason: String); overload;
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage); overload;
    procedure OnSuccess(Redirector: TIdSipActionRedirector;
                        SuccessfulAction: TIdSipAction;
                        Response: TIdSipResponse); overload;
    procedure SetBindings(Value: TIdSipContacts);
    procedure SetRegistrar(Value: TIdSipUri);
    procedure SetUaGruu(const GRUU: String);
  protected
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(ErrorCode: Cardinal;
                              const Reason: String); reintroduce; // TODO: Not all Actions will fail from a Response
    procedure NotifyOfSuccess(Response: TIdSipMessage); virtual;
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    function  Match(Msg: TIdSipMessage): Boolean; override;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);
    procedure Resend(AuthorizationCredentials: TIdSipAuthorizationHeader); override;
    procedure Send; override;

    property Bindings:  TIdSipContacts read fBindings write SetBindings;
    property Registrar: TIdSipUri      read fRegistrar write SetRegistrar;
  end;

  TIdSipOutboundRegistration = class(TIdSipOutboundRegistrationBase)
  private
    procedure ScheduleReregistration(MillisecondsToWait: Cardinal);
  protected
    procedure NotifyOfSuccess(Response: TIdSipMessage); override;
  public
    function CreateInitialAction: TIdSipOwnedAction; override;
    function ReregisterTime(Expires: Cardinal): Cardinal;
  end;

  TIdSipOutboundRegistrationQuery = class(TIdSipOutboundRegistrationBase)
  public
    function CreateInitialAction: TIdSipOwnedAction; override;
  end;

  TIdSipOutboundUnregistration = class(TIdSipOutboundRegistrationBase)
  private
    fIsWildCard: Boolean;
  public
    function CreateInitialAction: TIdSipOwnedAction; override;

    property IsWildCard: Boolean read fIsWildCard write fIsWildCard;
  end;

  TIdSipReregisterWait = class(TIdWait)
  private
    fRegisterModule: TIdSipOutboundRegisterModule;
    fRegistrar:      TIdSipUri;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property RegisterModule: TIdSipOutboundRegisterModule read fRegisterModule write fRegisterModule;
    property Registrar:      TIdSipUri                    read fRegistrar write SetRegistrar;
  end;

  TIdSipRegistrationMethod = class(TIdNotification)
  private
    fCurrentBindings: TIdSipContacts;
    fRegistration:    TIdSipOutboundRegistrationBase;
  public
    property CurrentBindings: TIdSipContacts                 read fCurrentBindings write fCurrentBindings;
    property Registration:    TIdSipOutboundRegistrationBase read fRegistration write fRegistration;
  end;

  TIdSipRegistrationFailedMethod = class(TIdSipRegistrationMethod)
  private
    fErrorCode: Cardinal;
    fReason:    String;
  public
    procedure Run(const Subject: IInterface); override;

    property ErrorCode: Cardinal read fErrorCode write fErrorCode;
    property Reason:    String   read fReason write fReason;
  end;

  TIdSipRegistrationSucceededMethod = class(TIdSipRegistrationMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  // I contain a bunch of GRUUs. My subclasses implement things like enforcing
  // the relationship between GRUUs, instance-IDs and addresses-of-record (see
  // TIdSipGruuBinding's class comment).
  TIdSipGruuBindings = class(TObject)
  public
    procedure AddBinding(const Contact, InstanceID, GRUU: String); virtual;
    procedure RemoveBinding(const Contact, InstanceID, GRUU: String); virtual;
    function  GetAnyGruuFor(const Contact, InstanceID: String): String; virtual;
    procedure GetGruusFor(const Contact, InstanceID: String; Gruus: TStrings); virtual;
  end;

  EIdSipRegistrarNotFound = class(EIdException)
  public
    constructor Create(const Msg: string); reintroduce;
  end;

implementation

uses
  IdSipAuthentication, IdSipConsts, Math, SysUtils;

const
  ItemNotFoundIndex = -1;
  TenMinutes        = 600; // seconds

resourcestring
  NoSuchRegistrar = 'No such registrar known: %s';

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

//* TIdSipRegistrationInfo Private methods *************************************

procedure TIdSipRegistrationInfo.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipRegistrations                                                        *
//******************************************************************************
//* TIdSipRegistrations Public methods *****************************************

constructor TIdSipRegistrations.Create;
begin
  inherited Create;

  Self.Lock            := TCriticalSection.Create;
  Self.KnownRegistrars := TObjectList.Create(true);
end;

destructor TIdSipRegistrations.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.KnownRegistrars.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipRegistrations.AddKnownRegistrar(Registrar: TIdSipUri;
                                                const CallID: String;
                                                SequenceNo: Cardinal);
var
  NewReg: TIdSipRegistrationInfo;
begin
  Self.Lock.Acquire;
  try
    if not Self.KnowsRegistrar(Registrar) then begin
      NewReg := TIdSipRegistrationInfo.Create;
      Self.KnownRegistrars.Add(NewReg);

      NewReg.CallID     := CallID;
      NewReg.Registrar  := Registrar;
      NewReg.SequenceNo := SequenceNo;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipRegistrations.CallIDFor(Registrar: TIdSipUri): String;
var
  Index: Integer;
begin
  Self.Lock.Acquire;
  try
    Index := Self.IndexOfRegistrar(Registrar);

    if (Index = ItemNotFoundIndex) then
      raise EIdSipRegistrarNotFound.Create(Registrar.Uri);

    Result := Self.RegistrarAt(Index).CallID;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipRegistrations.NextSequenceNoFor(Registrar: TIdSipUri): Cardinal;
var
  Index:   Integer;
  RegInfo: TIdSipRegistrationInfo;
begin
  Result := 0;

  Self.Lock.Acquire;
  try
    Index := Self.IndexOfRegistrar(Registrar);

    if (Index = ItemNotFoundIndex) then
      raise EIdSipRegistrarNotFound.Create(Registrar.Uri);

    RegInfo := Self.RegistrarAt(Index);
    Result := RegInfo.SequenceNo;
    RegInfo.SequenceNo := Result + 1;
  finally
    Self.Lock.Release;
  end;
end;

//* TIdSipRegistrations Private methods ****************************************

function TIdSipRegistrations.IndexOfRegistrar(Registrar: TIdSipUri): Integer;
begin
  Result := 0;
  while (Result < Self.KnownRegistrars.Count) do
    if Self.RegistrarAt(Result).Registrar.Equals(Registrar) then
      Break
    else
      Inc(Result);

  if (Result >= Self.KnownRegistrars.Count) then
    Result := ItemNotFoundIndex;
end;

function TIdSipRegistrations.KnowsRegistrar(Registrar: TIdSipUri): Boolean;
begin
  Result := Self.IndexOfRegistrar(Registrar) <> ItemNotFoundIndex;
end;

function TIdSipRegistrations.RegistrarAt(Index: Integer): TIdSipRegistrationInfo;
begin
  Result := Self.KnownRegistrars[Index] as TIdSipRegistrationInfo;
end;

//******************************************************************************
//* TIdRegistrarBinding                                                        *
//******************************************************************************
//* TIdRegistrarBinding Public methods *****************************************

constructor TIdRegistrarBinding.Create(const AddressOfRecord: String;
                                       const CanonicalisedUri: String;
                                       const InstanceID: String;
                                       const CallID: String;
                                       SequenceNo: Cardinal;
                                       AbsoluteTimeout: TDateTime);
begin
  inherited Create;

  Self.fAddressOfRecord := AddressOfRecord;
  Self.fCallID          := CallID;
  Self.fInstanceID      := InstanceID;
  Self.fSequenceNo      := SequenceNo;
  Self.fUri             := CanonicalisedUri;
  Self.fValidUntil      := AbsoluteTimeout;
end;

//******************************************************************************
//* TIdGruuBinding                                                             *
//******************************************************************************
//* TIdGruuBinding Publuic methods *********************************************

constructor TIdGruuBinding.Create(const Binding: String;
                                  const InstanceID: String;
                                  const Gruu: String);
begin
  inherited Create;

  Self.Binding    := Binding;
  Self.Gruu       := Gruu;
  Self.InstanceID := InstanceID;
end;

//******************************************************************************
//* TIdSipAbstractBindingDatabase                                              *
//******************************************************************************
//* TIdSipAbstractBindingDatabase Public methods *******************************

constructor TIdSipAbstractBindingDatabase.Create;
begin
  inherited Create;

  Self.DefaultExpiryTime := TenMinutes;
end;

function TIdSipAbstractBindingDatabase.AddBindings(Request: TIdSipRequest): Boolean;
var
  AddressOfRecord: String;
  Binding:         TIdRegistrarBinding;
  Contacts:        TIdSipContacts;
  Expiry:          Cardinal;
begin
  AddressOfRecord := Request.AddressOfRecord;

  Self.StartTransaction;
  try
    Result := true;
    Contacts := Request.Contacts;

    Contacts.First;
    while Contacts.HasNext do begin
      Binding := Self.Binding(AddressOfRecord,
                              Contacts.CurrentContact.AsCanonicalAddress);
      Expiry := Self.CorrectExpiry(Request,
                                   Contacts.CurrentContact);

      if Assigned(Binding) then begin
        Result := Result
              and Self.NotOutOfOrder(Request,
                                     Contacts.CurrentContact);
        if Result and (Expiry = 0) then begin
          Result := Result and Self.RemoveBinding(Request,
                                                  Contacts.CurrentContact);
        end;
      end
      else begin
        Result := Result and Self.AddBinding(AddressOfRecord,
                                             Contacts.CurrentContact,
                                             Request.CallID,
                                             Request.CSeq.SequenceNo,
                                             Expiry);
      end;

      Contacts.Next;
    end;

    if Result then
      Self.Commit
    else
      Self.Rollback;
  except
    Result := false;
    Self.Rollback;
  end;

  Self.NotifyListenersOfChange;
end;

function TIdSipAbstractBindingDatabase.IsValid(Request: TIdSipRequest): Boolean;
begin
  // Return true if the address-of-record in Request's To header is valid for
  // this database's domain.

  Result := true;
end;

function TIdSipAbstractBindingDatabase.BindingExpires(const AddressOfRecord: String;
                                                      const CanonicalUri: String): TDateTime;
begin
  Result := Self.Binding(AddressOfRecord, CanonicalUri).ValidUntil;
end;

function TIdSipAbstractBindingDatabase.BindingsFor(Request: TIdSipRequest;
                                                   Contacts: TIdSipContacts): Boolean;
begin
  Contacts.Clear;
  Result := Self.CollectBindingsFor(Request.AddressOfRecord,
                                    Contacts,
                                    Request.SupportsExtension(ExtensionGruu) and Self.UseGruu);
end;

function TIdSipAbstractBindingDatabase.NotOutOfOrder(Request: TIdSipRequest;
                                                     Contact: TIdSipContactHeader): Boolean;
var
  BindingRecord: TIdRegistrarBinding;
begin
  BindingRecord := Self.Binding(Request.AddressOfRecord,
                                Contact.AsAddressOfRecord);

  Result := BindingRecord.CallID <> Request.CallID;

  if not Result then
    Result := BindingRecord.SequenceNo < Request.CSeq.SequenceNo;
end;

function TIdSipAbstractBindingDatabase.RemoveAllBindings(Request: TIdSipRequest): Boolean;
var
  Contacts: TIdSipContacts;
begin
  Result := true;
  Self.StartTransaction;
  try
    Contacts := TIdSipContacts.Create;
    try
      Self.BindingsFor(Request, Contacts);
      Contacts.First;
      while Contacts.HasNext and Result do begin
        if Self.NotOutOfOrder(Request,
                              Contacts.CurrentContact) then
          Result := Result
                and Self.RemoveBinding(Request,
                                       Contacts.CurrentContact);

        Contacts.Next;
      end;
    finally
      Contacts.Free;
    end;

    if Result then
      Self.Commit
    else
      Self.Rollback;
  except
    Result := false;
    Self.Rollback;
  end;

  Self.NotifyListenersOfChange;
end;

//* TIdSipAbstractBindingDatabase Protected methods ****************************

function TIdSipAbstractBindingDatabase.CollectBindingsFor(const AddressOfRecord: String;
                                                          Bindings: TIdSipContacts;
                                                          CollectGruus: Boolean): Boolean;
begin
  // Return, in Bindings, all Contact URIs associated with AddressOfRecord. If
  // CollectGruus is true, add as a "gruu" parameter the GRUU of each Contact in
  // Bindings, if that Contact has a "+sip.instance" parameter.

  Result := true;
end;

function TIdSipAbstractBindingDatabase.CreateGruu(const AddressOfRecord: String;
                                                  const SipInstance: String): String;
begin
  // Return a newly-minted GRUU for the (AddressOfRecord, SipInstance) pair.
  //
  // As a basic way of creating a GRUU, just take the MD5 hash of the
  // concatenation of the address of record and the "+sip.instance" parameter,
  // and add that hash as the "opaque" parameter to the address of record.
  //
  // Other possibilities include using a better hash function (like SHA-1 or
  // SHA-512), adding Now to the hash input, and so on.

  Result := AddressOfRecord
          + ';opaque=' + MD5(AddressOfRecord + SipInstance);
end;

//* TIdSipAbstractBindingDatabase Private methods ******************************

function TIdSipAbstractBindingDatabase.CorrectExpiry(Request: TIdSipRequest;
                                                     Contact: TIdSipContactHeader): Cardinal;
begin
  if Contact.WillExpire then
    Result := Contact.Expires
  else begin
    if Request.HasHeader(ExpiresHeader) then
      Result := Request.Expires.NumericValue
    else
      Result := Self.DefaultExpiryTime;
  end;
end;

//******************************************************************************
//* TIdSipRegistrar                                                            *
//******************************************************************************
//* TIdSipRegistrar Public methods *********************************************

constructor TIdSipRegistrar.Create;
begin
  inherited Create;

  Self.RegisterModule := Self.AddModule(TIdSipRegisterModule) as TIdSipRegisterModule;
end;

function TIdSipRegistrar.RegistrationCount: Integer;
begin
  Result := Self.CountOf(MethodRegister);
end;

//* TIdSipRegistrar Protected methods ******************************************

function TIdSipRegistrar.GetUseGruu: Boolean;
begin
  Result := Self.BindingDB.UseGruu;
end;

procedure TIdSipRegistrar.SetUseGruu(Value: Boolean);
begin
  Self.BindingDB.UseGruu := Value;
end;

//* TIdSipRegistrar Private methods ********************************************

function TIdSipRegistrar.GetBindingDB: TIdSipAbstractBindingDatabase;
begin
  Result := Self.RegisterModule.BindingDB;
end;

function TIdSipRegistrar.GetDefaultRegistrationExpiryTime: Cardinal;
begin
  Result := Self.BindingDB.DefaultExpiryTime;
end;

function TIdSipRegistrar.GetMinimumExpiryTime: Cardinal;
begin
  Result := Self.RegisterModule.MinimumExpiryTime;
end;

procedure TIdSipRegistrar.SetBindingDB(Value: TIdSipAbstractBindingDatabase);
begin
  Self.RegisterModule.BindingDB := Value;
end;

procedure TIdSipRegistrar.SetDefaultRegistrationExpiryTime(Value: Cardinal);
begin
  Self.BindingDB.DefaultExpiryTime := Value;
end;

procedure TIdSipRegistrar.SetMinimumExpiryTime(Value: Cardinal);
begin
  Self.RegisterModule.MinimumExpiryTime := Value;
end;

//******************************************************************************
//* TIdSipOutboundRegisterModule                                               *
//******************************************************************************
//* TIdSipOutboundRegisterModule Public methods ********************************

constructor TIdSipOutboundRegisterModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);

  Self.KnownRegistrars := TIdSipRegistrations.Create;
  Self.fRegistrar      := TIdSipUri.Create('');

  Self.AutoReRegister := true;
  Self.HasRegistrar   := false;
end;

destructor TIdSipOutboundRegisterModule.Destroy;
begin
  Self.Registrar.Free;
  Self.KnownRegistrars.Free;

  inherited Destroy;
end;

function TIdSipOutboundRegisterModule.Accept(Request: TIdSipRequest;
                                             UsingSecureTransport: Boolean): TIdSipAction;
begin
  // As a purely UAC module, don't accept ANY requests.
  Result := nil;
end;

procedure TIdSipOutboundRegisterModule.CleanUp;
begin
  if Self.HasRegistrar then
    Self.UnregisterFrom(Self.Registrar).Send;
end;

function TIdSipOutboundRegisterModule.CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodRegister, Registrar);
  try
    Self.KnownRegistrars.AddKnownRegistrar(Registrar.Address,
                                           Result.CallID,
                                           Result.CSeq.SequenceNo);

    Result.RequestUri.EraseUserInfo;
    Result.CSeq.SequenceNo := Self.KnownRegistrars.NextSequenceNoFor(Registrar.Address);

    Result.CallID := Self.KnownRegistrars.CallIDFor(Registrar.Address);

    Result.ToHeader.Assign(Self.UserAgent.Contact);
    Result.From.Assign(Self.UserAgent.Contact);

    if Self.UserAgent.UseGruu then begin
      Result.FirstContact.Params[SipInstanceParam] := Self.UserAgent.InstanceID;

      if Self.RequireGRUU then
        Result.Require.Values.Add(ExtensionGruu);
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipOutboundRegisterModule.CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegistrationQuery) as TIdSipOutboundRegistrationQuery;
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegistration;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegistration) as TIdSipOutboundRegistration;
  Result.Bindings.Add(Self.UserAgent.Contact);
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.RegistrationCount: Integer;
begin
  Result := Self.UserAgent.CountOf(MethodRegister);
end;

function TIdSipOutboundRegisterModule.UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregistration;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundUnregistration) as TIdSipOutboundUnregistration;
  Result.Bindings.Add(Self.UserAgent.Contact);
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  // As a purely UAC module, don't accept ANY requests.
  Result := false;
end;

//* TIdSipOutboundRegisterModule Private methods *******************************

procedure TIdSipOutboundRegisterModule.SetRegistrar(Value: TIdSipUri);
begin
  Self.Registrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipRegisterModule                                                       *
//******************************************************************************
//* TIdSipRegisterModule Public methods ****************************************

constructor TIdSipRegisterModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);

  Self.AcceptsMethodsList.Add(MethodRegister);
end;

function TIdSipRegisterModule.Accept(Request: TIdSipRequest;
                                     UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := inherited Accept(Request, UsingSecureTransport);

  if not Assigned(Result) then
    Result := TIdSipInboundRegistration.CreateInbound(Self.UserAgent,
                                                      Request,
                                                      UsingSecureTransport);
end;

function TIdSipRegisterModule.AcceptsMethods: String;
begin
  Result := MethodRegister;
end;

//******************************************************************************
//* TIdSipRegister                                                             *
//******************************************************************************
//* TIdSipRegister Public methods **********************************************

function TIdSipRegister.IsRegistration: Boolean;
begin
  Result := true;
end;

function TIdSipRegister.Method: String;
begin
  Result := MethodRegister;
end;

//******************************************************************************
//* TIdSipRegistration                                                         *
//******************************************************************************
//* TIdSipRegistration Public methods ******************************************

function TIdSipRegistration.IsRegistration: Boolean;
begin
  Result := true;
end;

function TIdSipRegistration.Method: String;
begin
  Result := MethodRegister;
end;

//******************************************************************************
//* TIdSipInboundRegistration                                                  *
//******************************************************************************
//* TIdSipInboundRegistration Public methods ***********************************

function TIdSipInboundRegistration.IsInbound: Boolean;
begin
  Result := true;
end;

procedure TIdSipInboundRegistration.ReceiveRequest(Register: TIdSipRequest);
begin
  Assert(Register.IsRegister,
         'TIdSipAction.ReceiveRegister must only receive REGISTERs');

  if not Self.AcceptRequest(Register) then Exit;

  if Self.WillRemoveAllBindings(Register) then begin
    Self.TryRemoveAllBindings(Register);
    Exit;
  end;

  if not Self.BindingDB.AddBindings(Register) then begin
    Self.RejectFailedRequest(Register);
    Exit;
  end;

  Self.ReturnCurrentBindings(Register);

  Self.Terminate;
end;

//* TIdSipInboundRegistration Protected methods ********************************

function TIdSipInboundRegistration.CreateNewAttempt: TIdSipRequest;
var
  TempTo:    TIdSipToHeader;
  OutModule: TIdSipOutboundRegisterModule;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    // TODO: This is very hacky.
    OutModule := TIdSipOutboundRegisterModule.Create(Self.UA);
    try
      Result := OutModule.CreateRegister(TempTo);
    finally
      OutModule.Free;
    end;
  finally
    TempTo.Free;
  end;
end;

procedure TIdSipInboundRegistration.Initialise(UA: TIdSipAbstractCore;
                                               Request: TIdSipRequest;
                                               UsingSecureTransport: Boolean);
var
  RegModule: TIdSipMessageModule;
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.InitialRequest.Assign(Request);

  RegModule := Self.UA.ModuleFor(MethodRegister);
  Assert(not RegModule.IsNull,
         'The Transaction-User layer cannot process REGISTER requests without adding the Registration module to it');

  Self.RegisterModule := RegModule as TIdSipRegisterModule;
end;

//* TIdSipInboundRegistration Private methods **********************************

function TIdSipInboundRegistration.AcceptRequest(Request: TIdSipRequest): Boolean;
begin
  // cf RFC 3261 section 10.3
  // Steps 1, 2 & 3 - covered by Self.UA
  Result := true;

  // Step 4
  if not Self.BindingDB.IsAuthorized(Request.From, Request.RequestUri) then begin
    Self.RejectForbidden(Request);
    Result := false;
    Exit;
  end;

  // Step 5
  if not Self.BindingDB.IsValid(Request) then begin
    Self.RejectNotFound(Request);
    Result := false;
    Exit;
  end;

  // Step 6 (or part thereof)
  if Request.HasHeader(ContactHeaderFull) then begin
    if Request.FirstContact.IsWildCard then begin
      if (Request.ContactCount > 1) then begin
        Self.RejectRequest(Request, SIPBadRequest);
        Result := false;
        Exit;
      end;

      if Request.FirstContact.WillExpire
        and (Request.FirstContact.Expires = 0) then
          Result := true
      else begin
        Self.RejectRequest(Request, SIPBadRequest);
        Result := false;
        Exit;
      end;
    end
    else if Request.HasExpiry and (Request.QuickestExpiry < Self.RegisterModule.MinimumExpiryTime) then begin
      Self.RejectExpireTooBrief(Request);
      Result := false;
    end;
  end;

  // Steps 7 & 8 in ReceiveRequest
end;

function TIdSipInboundRegistration.BindingDB: TIdSipAbstractBindingDatabase;
begin
  Result := Self.RegisterModule.BindingDB;
end;

procedure TIdSipInboundRegistration.RejectExpireTooBrief(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.RegisterModule.MinimumExpiryTime);
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipInboundRegistration.RejectFailedRequest(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPInternalServerError);
end;

procedure TIdSipInboundRegistration.RejectForbidden(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPForbidden);
end;

procedure TIdSipInboundRegistration.RejectNotFound(Request: TIdSipRequest);
begin
  Self.RejectRequest(Request, SIPNotFound);
end;

procedure TIdSipInboundRegistration.RejectRequest(Request: TIdSipRequest;
                                                  StatusCode: Cardinal);
begin
  Self.SendSimpleResponse(Request, StatusCode);
end;

procedure TIdSipInboundRegistration.ReturnCurrentBindings(Request: TIdSipRequest);
var
  Bindings: TIdSipContacts;
  Date:     TIdSipDateHeader;
  Response: TIdSipResponse;
begin
  Bindings := TIdSipContacts.Create;
  try
    if Self.BindingDB.BindingsFor(Request,
                                  Bindings) then begin
      Response := Self.UA.CreateResponse(Request, SIPOK);
      try
        if Request.SupportsExtension(ExtensionGruu) then begin
          // We obviously support GRUU, because otherwise the Transaction-User core
          // would have rejected the request.

          // draft-ietf-sip-gruu section 7.1.2.1 says you MUST have this header,
          // RFC 3261 section 20 says not. The author thinks the draft's
          // behaviour is unnecessary since the fact that the GRUU's in a
          // "gruu" parameter indicates that, well, the "gruu" parameter value
          // is a GRUU!
          Response.Require.Values.Add(ExtensionGruu);
        end;

        Response.AddHeaders(Bindings);

        Date := TIdSipDateHeader.Create;
        try
          Date.Time.SetFromTDateTime(Now);
          Response.AddHeader(Date);
        finally
          Date.Free;
        end;

        Self.SendResponse(Response);
      finally
        Response.Free;
      end;
    end
    else begin
      // An error occured during the BindingDB finding all current contacts for
      // the address of record.
      Self.RejectFailedRequest(Request);
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipInboundRegistration.SendSimpleResponse(Request: TIdSipRequest;
                                                       StatusCode: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.UA.CreateResponse(Request,
                                     StatusCode);
  try
    Self.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipInboundRegistration.TryRemoveAllBindings(Request: TIdSipRequest);
begin
  if not Self.BindingDB.RemoveAllBindings(Request) then
    Self.RejectFailedRequest(Request)
  else
    Self.SendSimpleResponse(Request, SIPOK);
end;

function TIdSipInboundRegistration.WillRemoveAllBindings(Request: TIdSipRequest): Boolean;
begin
  Result := (Request.ContactCount = 1)
         and Request.FirstContact.IsWildCard
         and (Request.QuickestExpiry = 0);
end;

//******************************************************************************
//* TIdSipOutboundRegisterBase                                                 *
//******************************************************************************
//* TIdSipOutboundRegisterBase Public methods **********************************

destructor TIdSipOutboundRegisterBase.Destroy;
begin
  Self.fRegistrar.Free;
  Self.fBindings.Free;

  inherited Destroy;
end;

//* TIdSipOutboundRegisterBase Protected methods *******************************

function TIdSipOutboundRegisterBase.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.OutModule.CreateRegister(TempTo);
  finally
    TempTo.Free;
  end;
end;

function TIdSipOutboundRegisterBase.CreateRegister(Registrar: TIdSipUri;
                                               Bindings: TIdSipContacts): TIdSipRequest;
var
  ToHeader: TIdSipToHeader;
begin
  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Address := Registrar;

    Result := Self.OutModule.CreateRegister(ToHeader);

    // Bindings explicitly carries all Contact information. Thus we must remove
    // any Contact information already in Result.
    Result.Headers.RemoveAll(ContactHeaderFull);

    Result.AddHeaders(Bindings);
  finally
    ToHeader.Free;
  end;
end;

procedure TIdSipOutboundRegisterBase.Initialise(UA: TIdSipAbstractCore;
                                                Request: TIdSipRequest;
                                                UsingSecureTransport: Boolean);
var
  RegModule: TIdSipMessageModule;
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  RegModule := UA.ModuleFor(TIdSipOutboundRegisterModule);

  Assert(not RegModule.IsNull,
         'The Transaction-User layer cannot send REGISTER requests without adding the OutboundRegistration module to it');

  Self.OutModule := RegModule as TIdSipOutboundRegisterModule;

  Self.fBindings  := TIdSipContacts.Create;
  Self.fRegistrar := TIdSipUri.Create('');
end;

function TIdSipOutboundRegisterBase.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionResult;
begin
  Result := inherited ReceiveFailureResponse(Response);

  if (Result = arFailure) then begin
    case Response.StatusCode of
      SIPIntervalTooBrief: begin
        Self.ReissueRequestWithLongerExpiry(Self.InitialRequest.RequestUri,
                                            Response.MinExpires.NumericValue);
        Result := arSuccess;
      end;

      SIPBadExtension: begin
        if Self.InitialRequest.HasHeader(RequireHeader) then begin
          Self.RetryWithoutExtensions(Self.InitialRequest.RequestUri,
                                      Response);
          Result := arSuccess;
        end;
      end;
    else
      Result := arFailure;
    end;
  end;
end;

function TIdSipOutboundRegisterBase.ReceiveOKResponse(Response: TIdSipResponse;
                                                      UsingSecureTransport: Boolean): TIdSipActionResult;
begin
  Result := inherited ReceiveOKResponse(Response, UsingSecureTransport);

  Self.NotifyOfSuccess(Response);
  Self.Terminate;
end;

procedure TIdSipOutboundRegisterBase.RegisterWith(Registrar: TIdSipUri;
                                                  Bindings: TIdSipContacts);
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateRegister(Registrar, Bindings);
  try
    Self.SendRequest(Request);
  finally
    Request.Free;
  end;
end;

procedure TIdSipOutboundRegisterBase.RegisterWith(Registrar: TIdSipUri;
                                                  Contact: TIdSipContactHeader);
var
  Binding: TIdSipContacts;
begin
  Binding := TIdSipContacts.Create;
  try
    Binding.Add(Contact);

    Self.RegisterWith(Registrar, Binding);
  finally
    Binding.Free;
  end;
end;

procedure TIdSipOutboundRegisterBase.SendRequest(Request: TIdSipRequest);
begin
  Self.InitialRequest.Assign(Request);

  inherited SendRequest(Request);
end;

procedure TIdSipOutboundRegisterBase.Unregister(Registrar: TIdSipUri);
var
  RemovalBindings: TIdSipContacts;
  Request:         TIdSipRequest;
begin
  RemovalBindings := TIdSipContacts.Create;
  try
    RemovalBindings.Add(ContactHeaderFull);
    RemovalBindings.First;
    RemovalBindings.CurrentContact.IsWildCard := true;

    Request := Self.CreateRegister(Registrar, RemovalBindings);
    try
      Request.Expires.NumericValue := 0;

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    RemovalBindings.Free;
  end;
end;

//* TIdSipOutboundRegisterBase Private methods *************************************

procedure TIdSipOutboundRegisterBase.ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
                                                                    MinimumExpiry: Cardinal);
var
  Bindings: TIdSipContacts;
  OriginalBindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  // We received a 423 Interval Too Brief from the registrar. Therefore we
  // make a new REGISTER request with the registrar's minimum expiry.
  OriginalBindings := TIdSipContacts.Create(Self.InitialRequest.Headers);
  try
    Request := Self.CreateRegister(Registrar, OriginalBindings);
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

      Request.Expires.NumericValue := MinimumExpiry;
      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    OriginalBindings.Free;
  end;
end;

procedure TIdSipOutboundRegisterBase.RetryWithoutExtensions(Registrar: TIdSipUri;
                                                            Response: TIdSipResponse);
var
  Bindings: TIdSipContacts;
  Request: TIdSipRequest;
begin
  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.UA.Contact);

    Request := Self.CreateRegister(Registrar, Bindings);
    try
      if not Response.HasHeader(UnsupportedHeader) then begin
        // A 420 Bad Extension MUST have an Unsupported header. In the
        // interests of accepting liberally though, we just drop all
        // Requires.
        Request.RemoveAllHeadersNamed(RequireHeader);
      end
      else
        Request.Require.RemoveValues(Response.FirstUnsupported);

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipOutboundRegisterBase.SetBindings(Value: TIdSipContacts);
begin
  Self.fBindings.Clear;
  Self.fBindings.Add(Value);
end;

procedure TIdSipOutboundRegisterBase.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipOutboundRegisterQuery                                                *
//******************************************************************************
//* TIdSipOutboundRegisterQuery Public methods *********************************

procedure TIdSipOutboundRegisterQuery.Send;
begin
  inherited Send;

  Self.Bindings.Clear;
  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//******************************************************************************
//* TIdSipOutboundRegister                                                     *
//******************************************************************************
//* TIdSipOutboundRegister Public methods **************************************

procedure TIdSipOutboundRegister.Send;
begin
  inherited Send;

  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//******************************************************************************
//* TIdSipOutboundUnregister                                                   *
//******************************************************************************
//* TIdSipOutboundUnregister Public methods ************************************

procedure TIdSipOutboundUnregister.Send;
begin
  inherited Send;

  if Self.IsWildCard then begin
    Self.Bindings.Clear;
    Self.Bindings.Add(ContactHeaderFull);
    Self.Bindings.First;
    Self.Bindings.CurrentContact.IsWildCard := true;
  end else begin
    Self.Bindings.First;
    while Self.Bindings.HasNext do begin
      Self.Bindings.CurrentContact.Expires := ExpireNow;
      Self.Bindings.Next;
    end;
  end;

  Self.RegisterWith(Self.Registrar, Self.Bindings);
end;

//* TIdSipOutboundUnregister Protected methods *********************************

function TIdSipOutboundUnregister.CreateRegister(Registrar: TIdSipUri;
                                                 Bindings: TIdSipContacts): TIdSipRequest;
begin
  Result := inherited CreateRegister(Registrar, Bindings);

  Result.Expires.NumericValue := ExpireNow;
end;

procedure TIdSipOutboundUnregister.Initialise(UA: TIdSipAbstractCore;
                                              Request: TIdSipRequest;
                                              UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.IsWildCard := false;
end;

//******************************************************************************
//* TIdSipOutboundRegistrationBase                                             *
//******************************************************************************
//* TIdSipOutboundRegistrationBase Public methods ******************************

destructor TIdSipOutboundRegistrationBase.Destroy;
begin
  Self.RegistrationListeners.Free;
  Self.Redirector.Free;
  Self.Registrar.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundRegistrationBase.AddListener(const Listener: IIdSipRegistrationListener);
begin
  Self.RegistrationListeners.AddListener(Listener);
end;

function TIdSipOutboundRegistrationBase.Match(Msg: TIdSipMessage): Boolean;
begin
  // All responses must match only those actions controlled by the Redirector,
  // not this one.
  Result := false;
end;

procedure TIdSipOutboundRegistrationBase.RemoveListener(const Listener: IIdSipRegistrationListener);
begin
  Self.RegistrationListeners.RemoveListener(Listener);
end;

procedure TIdSipOutboundRegistrationBase.Resend(AuthorizationCredentials: TIdSipAuthorizationHeader);
begin
  if (Self.State = asInitialised) then
    raise EIdSipTransactionUser.Create('You cannot REsend if you didn''t send'
                                     + ' in the first place');

  if Self.Redirector.Contains(Self.ChallengedAction) then begin
    Self.Redirector.Resend(Self.ChallengedAction, AuthorizationCredentials);
    Self.InitialRequest.Assign(Self.ChallengedAction.InitialRequest);
  end
  else
    inherited Resend(AuthorizationCredentials);
end;

procedure TIdSipOutboundRegistrationBase.Send;
begin
  inherited Send;

  Self.Redirector.Send;
  Self.InitialRequest.Assign(Self.Redirector.InitialAction.InitialRequest);
end;

//* TIdSipOutboundRegistrationBase Protected methods ***************************

procedure TIdSipOutboundRegistrationBase.Initialise(UA: TIdSipAbstractCore;
                                                    Request: TIdSipRequest;
                                                    UsingSecureTransport: Boolean);
var
  RegModule: TIdSipMessageModule;
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  RegModule := UA.ModuleFor(TIdSipOutboundRegisterModule);

  Assert(not RegModule.IsNull,
         'The Transaction-User layer cannot send REGISTER requests without adding the OutboundRegistration module to it');

  Self.OutModule := RegModule as TIdSipOutboundRegisterModule;

  Self.fBindings  := TIdSipContacts.Create;
  Self.fRegistrar := TIdSipUri.Create('');

  Self.Redirector := TIdSipActionRedirector.Create(Self);
  Self.Redirector.AddListener(Self);
  Self.RegistrationListeners := TIdNotificationList.Create;
end;

procedure TIdSipOutboundRegistrationBase.NotifyOfFailure(ErrorCode: Cardinal;
                                                         const Reason: String);
var
  Notification: TIdSipRegistrationFailedMethod;
begin
  Notification := TIdSipRegistrationFailedMethod.Create;
  try
    Notification.ErrorCode    := ErrorCode;
    Notification.Registration := Self;
    Notification.Reason       := Reason;

    Self.RegistrationListeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipOutboundRegistrationBase.NotifyOfSuccess(Response: TIdSipMessage);
var
  CurrentBindings: TIdSipContacts;
  Notification:    TIdSipRegistrationSucceededMethod;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Notification := TIdSipRegistrationSucceededMethod.Create;
    try
      Notification.CurrentBindings := CurrentBindings;
      Notification.Registration    := Self;

      Self.RegistrationListeners.Notify(Notification);
    finally
      Notification.Free;
    end;

    if Response.SupportsExtension(ExtensionGruu) and Self.UA.UseGruu then
      Self.SetUaGruu(CurrentBindings.GruuFor(Self.UA.Contact));
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

//* TIdSipOutboundRegistrationBase Private methods *****************************

procedure TIdSipOutboundRegistrationBase.OnAuthenticationChallenge(Action: TIdSipAction;
                                                                   Challenge: TIdSipResponse);
begin
  Self.ChallengedAction := Action;
  Self.NotifyOfAuthenticationChallenge(Challenge);
end;

procedure TIdSipOutboundRegistrationBase.OnFailure(Action: TIdSipAction;
                                                   Response: TIdSipResponse;
                                                   const Reason: String);
begin
  // Do nothing. The Redirector handles this stuff.
end;

procedure TIdSipOutboundRegistrationBase.OnFailure(Redirector: TIdSipActionRedirector;
                                                   Response: TIdSipResponse);
begin
  Self.NotifyOfFailure(Response.StatusCode, Response.StatusText);
end;

procedure TIdSipOutboundRegistrationBase.OnNetworkFailure(Action: TIdSipAction;
                                                          ErrorCode: Cardinal;
                                                          const Reason: String);
begin
  Self.NotifyOfNetworkFailure(ErrorCode, Reason);
end;

procedure TIdSipOutboundRegistrationBase.OnNewAction(Redirector: TIdSipActionRedirector;
                                                     NewAction: TIdSipAction);
begin
  NewAction.AddActionListener(Self);
  (NewAction as TIdSipOwnedAction).AddOwnedActionListener(Self);
end;

procedure TIdSipOutboundRegistrationBase.OnRedirect(Action: TIdSipAction;
                                                    Redirect: TIdSipResponse);
begin
  // Do nothing. The Redirector handles this stuff.
end;

procedure TIdSipOutboundRegistrationBase.OnRedirectFailure(Redirector: TIdSipActionRedirector;
                                                           ErrorCode: Cardinal;
                                                           const Reason: String);
begin
  Self.NotifyOfFailure(ErrorCode, Reason);
end;

procedure TIdSipOutboundRegistrationBase.OnSuccess(Action: TIdSipAction;
                                                   Msg: TIdSipMessage);
begin
  // Do nothing. The Redirector handles this stuff.
end;

procedure TIdSipOutboundRegistrationBase.OnSuccess(Redirector: TIdSipActionRedirector;
                                                   SuccessfulAction: TIdSipAction;
                                                   Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);
end;

procedure TIdSipOutboundRegistrationBase.SetBindings(Value: TIdSipContacts);
begin
  Self.fBindings.Clear;
  Self.fBindings.Add(Value);
end;

procedure TIdSipOutboundRegistrationBase.SetRegistrar(Value: TIdSipUri);
begin
  Self.Registrar.Uri := Value.Uri;
end;

procedure TIdSipOutboundRegistrationBase.SetUaGruu(const GRUU: String);
begin
  Self.UA.Gruu.Address.Uri := GRUU;
end;

//******************************************************************************
//* TIdSipOutboundRegistration                                                 *
//******************************************************************************
//* TIdSipOutboundRegistration Public methods **********************************

function TIdSipOutboundRegistration.CreateInitialAction: TIdSipOwnedAction;
var
  Reg: TIdSipOutboundRegister;
begin
  Reg := Self.UA.AddOutboundAction(TIdSipOutboundRegister) as TIdSipOutboundRegister;
  Reg.Bindings  := Self.Bindings;
  Reg.Registrar := Self.Registrar;

  Result := Reg;
end;

function TIdSipOutboundRegistration.ReregisterTime(Expires: Cardinal): Cardinal;
begin
  // Expires magnitude:                  Result
  // Expires >= 20 minutes               Expires - 5 minutes
  // 1 minute <= Expires < 20 minutes    Expires - 1 minute
  // Expires < 1 minute                  0.8 * Expires

  // Postcondition: Result > 0

  if (Expires <= 1) then
    Result := 1
  else if (Expires < OneMinute) then
    Result := 4*(Expires div 5)
  else if (Expires < TwentyMinutes) then
    Result := Expires - OneMinute
  else
    Result := Expires - FiveMinutes;
end;

//* TIdSipOutboundRegistration Protected methods *******************************

procedure TIdSipOutboundRegistration.NotifyOfSuccess(Response: TIdSipMessage);
var
  ExpireTime: Cardinal;
  OurContact: TIdSipContactHeader;
begin
  inherited NotifyOfSuccess(Response);

  if Self.OutModule.AutoReRegister then begin
    // OurContact should always be assigned, because we've just REGISTERed it.
    // If it's not assigned then the registrar didn't save our registration,
    // and still returned a 2xx rather than an error response.
    OurContact := Response.Contacts.ContactFor(Self.InitialRequest.FirstContact);
    if Assigned(OurContact) then begin

      // ExpireTime represents a seconds value.
      // Using 0 as a sentinel value works because it means "now" - and
      // registrars really shouldn't return a 0. Remember, if a UAC sends a
      // REGISTER with an Expires of 0, the registrar will unregister those
      // contacts!
      ExpireTime := 0;
      if OurContact.WillExpire then
        ExpireTime := OurContact.Expires
      else if Response.HasHeader(ExpiresHeader) then
        ExpireTime := Response.Expires.NumericValue;

      if (ExpireTime > 0) then
        Self.ScheduleReregistration(Self.ReregisterTime(ExpireTime)*1000);
    end;
  end;
end;

//* TIdSipOutboundRegistration Private methods *********************************

procedure TIdSipOutboundRegistration.ScheduleReregistration(MillisecondsToWait: Cardinal);
var
  Reregister: TIdSipReregisterWait;
begin
  Reregister := TIdSipReregisterWait.Create;
  Reregister.RegisterModule := Self.OutModule;
  Reregister.Registrar      := Self.Registrar;
  Self.UA.ScheduleEvent(MillisecondsToWait, Reregister);
end;

//******************************************************************************
//* TIdSipOutboundRegistrationQuery                                            *
//******************************************************************************
//* TIdSipOutboundRegistrationQuery Public methods *****************************

function TIdSipOutboundRegistrationQuery.CreateInitialAction: TIdSipOwnedAction;
var
  Reg: TIdSipOutboundRegisterQuery;
begin
  Reg := Self.UA.AddOutboundAction(TIdSipOutboundRegisterQuery) as TIdSipOutboundRegisterQuery;
  Reg.Bindings  := Self.Bindings;
  Reg.Registrar := Self.Registrar;

  Result := Reg;
end;

//******************************************************************************
//* TIdSipOutboundUnregistration                                               *
//******************************************************************************
//* TIdSipOutboundUnregistration Public methods ********************************

function TIdSipOutboundUnregistration.CreateInitialAction: TIdSipOwnedAction;
var
  Reg: TIdSipOutboundUnregister;
begin
  Reg := Self.UA.AddOutboundAction(TIdSipOutboundUnregister) as TIdSipOutboundUnregister;
  Reg.Bindings   := Self.Bindings;
  Reg.IsWildCard := Self.IsWildCard;
  Reg.Registrar  := Self.Registrar;

  Result := Reg;
end;

//******************************************************************************
//* TIdSipReregisterWait                                                       *
//******************************************************************************
//* TIdSipReregisterWait Public methods ****************************************

constructor TIdSipReregisterWait.Create;
begin
  inherited Create;

  Self.fRegistrar := TIdSipUri.Create('');
end;

destructor TIdSipReregisterWait.Destroy;
begin
  Self.fRegistrar.Free;

  inherited Destroy;
end;

procedure TIdSipReregisterWait.Trigger;
begin
  Self.RegisterModule.RegisterWith(Self.Registrar).Send;
end;

//* TIdSipReregisterWait Private methods ***************************************

procedure TIdSipReregisterWait.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipRegistrationFailedMethod                                             *
//******************************************************************************
//* TIdSipRegistrationFailedMethod Public methods ******************************

procedure TIdSipRegistrationFailedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipRegistrationListener).OnFailure(Self.Registration,
                                                    Self.ErrorCode,
                                                    Self.Reason);
end;

//******************************************************************************
//* TIdSipRegistrationSucceededMethod                                          *
//******************************************************************************
//* TIdSipRegistrationSucceededMethod Public methods ***************************

procedure TIdSipRegistrationSucceededMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipRegistrationListener).OnSuccess(Self.Registration,
                                                    Self.CurrentBindings);
end;

//******************************************************************************
//* TIdSipGruuBindings                                                         *
//******************************************************************************
//* TIdSipGruuBindings Public methods ******************************************

procedure TIdSipGruuBindings.AddBinding(const Contact, InstanceID, GRUU: String);
begin
end;

procedure TIdSipGruuBindings.RemoveBinding(const Contact, InstanceID, GRUU: String);
begin
end;

function TIdSipGruuBindings.GetAnyGruuFor(const Contact, InstanceID: String): String;
var
  Gruus: TStrings;
begin
  Gruus := TStringList.Create;
  try
    Self.GetGruusFor(Contact, InstanceID, Gruus);

    if (Gruus.Count = 0) then
      Result := ''
    else
      Result := Gruus[0];
  finally
    Gruus.Free;
  end;
end;

procedure TIdSipGruuBindings.GetGruusFor(const Contact, InstanceID: String; Gruus: TStrings);
begin
end;

//******************************************************************************
//* EIdSipRegistrarNotFound                                                    *
//******************************************************************************
//* EIdSipRegistrarNotFound Public methods *************************************

constructor EIdSipRegistrarNotFound.Create(const Msg: string);
begin
  inherited Create(Format(NoSuchRegistrar, [Msg]));
end;

end.
