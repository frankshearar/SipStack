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
  Contnrs, IdException, IdObservable, IdSipCore, IdSipMessage, IdNotification,
  SyncObjs;

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
    fSequenceNo:      Cardinal;
    fUri:             String;
    fValidUntil:      TDateTime;
  public
    constructor Create(const AddressOfRecord: String;
                       const CanonicalisedUri: String;
                       const CallID: String;
                       SequenceNo: Cardinal;
                       AbsoluteTimeout: TDateTime);

    property AddressOfRecord: String    read fAddressOfRecord write fAddressOfRecord;
    property CallID:          String    read fCallID write fCallID;
    property SequenceNo:      Cardinal  read fSequenceNo write fSequenceNo;
    property Uri:             String    read fUri write fUri;
    property ValidUntil:      TDateTime read fValidUntil write fValidUntil;
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
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
  public
    constructor Create; override;

    function  AddBindings(Request: TIdSipRequest): Boolean;
    function  IsAuthorized(User: TIdSipAddressHeader;
                           AddressOfRecord: TIdSipUri): Boolean; virtual; abstract;
    function  IsValid(Request: TIdSipRequest): Boolean; virtual; abstract;
    function  BindingExpires(const AddressOfRecord: String;
                             const CanonicalUri: String): TDateTime;
    function  BindingsFor(Request: TIdSipRequest;
                          Contacts: TIdSipContacts): Boolean; virtual; abstract;
    function  NotOutOfOrder(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean;
    function  RemoveAllBindings(Request: TIdSipRequest): Boolean;
    function  RemoveBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; virtual; abstract;

    property DefaultExpiryTime: Cardinal read fDefaultExpiryTime write fDefaultExpiryTime;
  end;

  TIdSipOutboundRegistration = class;

  // I provide a protocol for using a registrar. You send a REGISTER, and
  // listen for the events below.
  //
  // OnFailure and OnSuccess, apart from the obvious, tell you that the
  // registration agent has terminated, and that you should remove all
  // of your references to it.
  IIdSipRegistrationListener = interface(IIdSipActionListener)
    ['{D3FA9A3D-ED8A-48D3-8068-38E8F9EE2140}']
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        Response: TIdSipResponse);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
  end;

  TIdSipRegisterModule = class;

  TIdSipRegistrar = class(TIdSipAbstractUserAgent)
  private
    RegisterModule: TIdSipRegisterModule;

    function  GetBindingDB: TIdSipAbstractBindingDatabase;
    function  GetDefaultRegistrationExpiryTime: Cardinal;
    function  GetMinimumExpiryTime: Cardinal;
    procedure SetBindingDB(Value: TIdSipAbstractBindingDatabase);
    procedure SetDefaultRegistrationExpiryTime(Value: Cardinal);
    procedure SetMinimumExpiryTime(Value: Cardinal);
  public
    constructor Create; override;

    function RegistrationCount: Integer;

    property BindingDB:                     TIdSipAbstractBindingDatabase read GetBindingDB write SetBindingDB;
    property DefaultRegistrationExpiryTime: Cardinal                      read GetDefaultRegistrationExpiryTime write SetDefaultRegistrationExpiryTime;
    property MinimumExpiryTime:             Cardinal                      read GetMinimumExpiryTime write SetMinimumExpiryTime;
  end;

  // I implement that functionality necessary for a User Agent to respond to
  // REGISTER messages, that is, to act as a registrar.
  TIdSipRegisterModule = class(TIdSipMessageModule)
  private
    fBindingDB:         TIdSipAbstractBindingDatabase;
    fMinimumExpiryTime: Cardinal; // in seconds
  public
    function Accept(Request: TIdSipRequest;
                    UsingSecureTransport: Boolean): TIdSipAction; override;
    function AcceptsMethods: String; override;
    function WillAccept(Request: TIdSipRequest): Boolean; override;

    property BindingDB:         TIdSipAbstractBindingDatabase read fBindingDB write fBindingDB;
    property MinimumExpiryTime: Cardinal                      read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

  TIdSipOutboundRegistrationQuery = class;
  TIdSipOutboundRegister = class;
  TIdSipOutboundUnregister = class;

  // I implement that functionality necessary for a User Agent to issue REGISTER
  // messages, that is, to register with a registrar.
  TIdSipOutboundRegisterModule = class(TIdSipMessageModule)
  private
    fAutoReRegister: Boolean;
    fHasRegistrar:   Boolean;
    fRegistrar:      TIdSipUri;
    KnownRegistrars: TIdSipRegistrations;

    procedure SetRegistrar(Value: TIdSipUri);
  public
    constructor Create(UA: TIdSipAbstractUserAgent); override;
    destructor  Destroy; override;

    function  Accept(Request: TIdSipRequest;
                     UsingSecureTransport: Boolean): TIdSipAction; override;
    function  AcceptsMethods: String; override;
    procedure CleanUp; override;
    function  CreateRegister(Registrar: TIdSipToHeader): TIdSipRequest;
    function  CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
    procedure OnReregister(Event: TObject);
    function  RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegister;
    function  RegistrationCount: Integer;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregister;
    function  WillAccept(Request: TIdSipRequest): Boolean; override;

    property AutoReRegister: Boolean   read fAutoReRegister write fAutoReRegister;
    property HasRegistrar:   Boolean   read fHasRegistrar write fHasRegistrar;
    property Registrar:      TIdSipUri read fRegistrar write SetRegistrar;
  end;

  TIdSipRegistration = class(TIdSipAction)
  protected
    OutModule: TIdSipOutboundRegisterModule;

    function CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    class function Method: String; override;

    function IsRegistration: Boolean; override;
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
    procedure SendSimpleResponse(Request: TIdSipRequest;
                                StatusCode: Cardinal);
  protected
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    function IsInbound: Boolean; override;
    procedure ReceiveRequest(Register: TIdSipRequest); override;
  end;

  // I piggyback on a transaction in a blocking I/O fashion to provide a UAC
  // with a way to register with a registrar. I take care of things like
  // doing stuff with error responses, asking for authentication, etc.
  //
  // It makes no sense to access me once my Transaction has terminated. In
  // other words once you've received notification of my success or failure,
  // erase your references to me.
  TIdSipOutboundRegistration = class(TIdSipRegistration)
  private
    fBindings:  TIdSipContacts;
    fRegistrar: TIdSipUri;

    procedure ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
                                             MinimumExpiry: Cardinal);
    procedure RetryWithoutExtensions(Registrar: TIdSipUri;
                                     Response: TIdSipResponse);
    procedure SetBindings(Value: TIdSipContacts);
    procedure SetRegistrar(Value: TIdSipUri);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; virtual;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
    procedure NotifyOfSuccess(Response: TIdSipResponse); virtual;
    function  ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus; override;
    function  ReceiveRedirectionResponse(Response: TIdSipResponse;
                                         UsingSecureTransport: Boolean): TIdSipActionStatus; override;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Bindings: TIdSipContacts); overload;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Contact: TIdSipContactHeader); overload;
    procedure SendRequest(Request: TIdSipRequest;
                          TryAgain: Boolean = true); overload; override;
    procedure Unregister(Registrar: TIdSipUri);
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipRegistrationListener);
    function  ReregisterTime(Expires: Cardinal): Cardinal;
    procedure RemoveListener(const Listener: IIdSipRegistrationListener);

    property Bindings:  TIdSipContacts read fBindings write SetBindings;
    property Registrar: TIdSipUri read fRegistrar write SetRegistrar;
  end;

  TIdSipOutboundRegistrationQuery = class(TIdSipOutboundRegistration)
  public
    procedure Send; override;
  end;

  TIdSipOutboundRegister = class(TIdSipOutboundRegistration)
  public
    procedure Send; override;
  end;

  TIdSipOutboundUnRegister = class(TIdSipOutboundRegistration)
  private
    fIsWildCard: Boolean;
  protected
    function  CreateRegister(Registrar: TIdSipUri;
                             Bindings: TIdSipContacts): TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractUserAgent;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    procedure Send; override;

    property IsWildCard: Boolean   read fIsWildCard write fIsWildCard;
  end;

  TIdSipRegistrationMethod = class(TIdNotification)
  private
    fCurrentBindings: TIdSipContacts;
    fRegistration:    TIdSipOutboundRegistration;
  public
    property CurrentBindings: TIdSipContacts             read fCurrentBindings write fCurrentBindings;
    property Registration:    TIdSipOutboundRegistration read fRegistration write fRegistration;
  end;

  TIdSipRegistrationFailedMethod = class(TIdSipRegistrationMethod)
  private
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Response: TIdSipResponse read fResponse write fResponse;
  end;

  TIdSipRegistrationSucceededMethod = class(TIdSipRegistrationMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  EIdSipRegistrarNotFound = class(EIdException)
  public
    constructor Create(const Msg: string); reintroduce;
  end;

implementation

uses
  IdSipConsts, Math, SysUtils;

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
                                       const CallID: String;
                                       SequenceNo: Cardinal;
                                       AbsoluteTimeout: TDateTime);
begin
  inherited Create;

  Self.fAddressOfRecord := AddressOfRecord;
  Self.fCallID          := CallID;
  Self.fSequenceNo      := SequenceNo;
  Self.fUri             := CanonicalisedUri;
  Self.fValidUntil      := AbsoluteTimeout;
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
    Contacts := TIdSipContacts.Create(Request.Headers);
    try
      Contacts.First;
      while Contacts.HasNext do begin
        Binding := Self.Binding(AddressOfRecord,
                                Contacts.CurrentContact.AsAddressOfRecord);
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

function TIdSipAbstractBindingDatabase.BindingExpires(const AddressOfRecord: String;
                                                      const CanonicalUri: String): TDateTime;
begin
  Result := Self.Binding(AddressOfRecord, CanonicalUri).ValidUntil;
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

function TIdSipAbstractBindingDatabase.CorrectExpiry(Request: TIdSipRequest;
                                                     Contact: TIdSipContactHeader): Cardinal;
begin
  if Contact.WillExpire then
    Result := Contact.Expires
  else begin
    if Request.HasHeader(ExpiresHeader) then
      Result := Request.FirstExpires.NumericValue
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
//* TIdSipRegisterModule                                                       *
//******************************************************************************
//* TIdSipRegisterModule Public methods ****************************************

function TIdSipRegisterModule.Accept(Request: TIdSipRequest;
                                     UsingSecureTransport: Boolean): TIdSipAction;
begin
  Result := TIdSipInboundRegistration.CreateInbound(Self.UserAgent,
                                                    Request,
                                                    UsingSecureTransport);
end;

function TIdSipRegisterModule.AcceptsMethods: String;
begin
  Result := MethodRegister;
end;

function TIdSipRegisterModule.WillAccept(Request: TIdSipRequest): Boolean;
begin
  Result := Request.IsRegister;
end;

//******************************************************************************
//* TIdSipOutboundRegisterModule                                               *
//******************************************************************************
//* TIdSipOutboundRegisterModule Public methods ********************************

constructor TIdSipOutboundRegisterModule.Create(UA: TIdSipAbstractUserAgent);
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

function TIdSipOutboundRegisterModule.AcceptsMethods: String;
begin
  // As a purely UAC module, don't offer to accept ANY requests.
  Result := '';
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

    Result.From.Value     := Self.UserAgent.From.AsAddressOfRecord;
    Result.ToHeader.Value := Self.UserAgent.From.AsAddressOfRecord;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipOutboundRegisterModule.CurrentRegistrationWith(Registrar: TIdSipUri): TIdSipOutboundRegistrationQuery;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegistrationQuery) as TIdSipOutboundRegistrationQuery;
end;

procedure TIdSipOutboundRegisterModule.OnReregister(Event: TObject);
var
  Request: TIdSipRequest;
begin
  Request := (Event as TIdSipMessageNotifyEventWait).Message as TIdSipRequest;
  Self.RegisterWith(Request.RequestUri).Send;
end;

function TIdSipOutboundRegisterModule.RegisterWith(Registrar: TIdSipUri): TIdSipOutboundRegister;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundRegister) as TIdSipOutboundRegister;
  Result.Bindings.Add(Self.UserAgent.Contact);
  Result.Registrar := Registrar;
end;

function TIdSipOutboundRegisterModule.RegistrationCount: Integer;
begin
  Result := Self.UserAgent.CountOf(MethodRegister);
end;

function TIdSipOutboundRegisterModule.UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregister;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundUnregister) as TIdSipOutboundUnregister;
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
//* TIdSipRegistration                                                         *
//******************************************************************************
//* TIdSipRegistration Public methods ******************************************

class function TIdSipRegistration.Method: String;
begin
  Result := MethodRegister;
end;

function TIdSipRegistration.IsRegistration: Boolean;
begin
  Result := true;
end;

//* TIdSipRegistration Protected methods ***************************************

function TIdSipRegistration.CreateNewAttempt: TIdSipRequest;
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

procedure TIdSipRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                        Request: TIdSipRequest;
                                        UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.OutModule := UA.ModuleFor(TIdSipOutboundRegisterModule) as TIdSipOutboundRegisterModule;
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
var
  Bindings: TIdSipContacts;
  Date:     TIdSipDateHeader;
  Response: TIdSipResponse;
begin
  Assert(Register.IsRegister, 'TIdSipAction.ReceiveRegister must only receive REGISTERs');

  if not Self.AcceptRequest(Register) then Exit;

  if (Register.ContactCount = 1)
    and Register.FirstContact.IsWildCard
    and (Register.QuickestExpiry = 0) then begin

    if not Self.BindingDB.RemoveAllBindings(Register) then
      Self.RejectFailedRequest(Register)
    else
      Self.SendSimpleResponse(Register, SIPOK);
    Exit;
  end;

  if not Self.BindingDB.AddBindings(Register) then begin
    Self.RejectFailedRequest(Register);
    Exit;
  end;

  Bindings := TIdSipContacts.Create;
  try
    if Self.BindingDB.BindingsFor(Register,
                                  Bindings) then begin
      Response := Self.UA.CreateResponse(Register, SIPOK);
      try
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
      Self.RejectFailedRequest(Register);
    end;
  finally
    Bindings.Free;
  end;

  Self.Terminate;
end;

//* TIdSipInboundRegistration Protected methods ********************************

procedure TIdSipInboundRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                               Request: TIdSipRequest;
                                               UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.InitialRequest.Assign(Request);
  Self.RegisterModule := Self.UA.ModuleFor(MethodRegister) as TIdSipRegisterModule;

  Assert(Assigned(Self.RegisterModule),
         'The Transaction-User layer cannot process REGISTER methods without adding the Registration module to it');
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

//******************************************************************************
//* TIdSipOutboundRegistration                                                 *
//******************************************************************************
//* TIdSipOutboundRegistration Public methods **********************************

destructor TIdSipOutboundRegistration.Destroy;
begin
  Self.fRegistrar.Free;
  Self.fBindings.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundRegistration.AddListener(const Listener: IIdSipRegistrationListener);
begin
  Self.Listeners.AddListener(Listener);
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

procedure TIdSipOutboundRegistration.RemoveListener(const Listener: IIdSipRegistrationListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

//* TIdSipOutboundRegistration Protected methods *******************************

procedure TIdSipOutboundRegistration.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfSuccess(Response);
end;

function TIdSipOutboundRegistration.CreateRegister(Registrar: TIdSipUri;
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

procedure TIdSipOutboundRegistration.Initialise(UA: TIdSipAbstractUserAgent;
                                                Request: TIdSipRequest;
                                                UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fBindings := TIdSipContacts.Create;
  Self.fRegistrar := TIdSipUri.Create('');
end;

procedure TIdSipOutboundRegistration.NotifyOfFailure(Response: TIdSipResponse);
var
  CurrentBindings: TIdSipContacts;
  Notification:    TIdSipRegistrationFailedMethod;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Notification := TIdSipRegistrationFailedMethod.Create;
    try
      Notification.CurrentBindings := CurrentBindings;
      Notification.Registration    := Self;
      Notification.Response        := Response;

      Self.Listeners.Notify(Notification);
    finally
      Notification.Free;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipOutboundRegistration.NotifyOfSuccess(Response: TIdSipResponse);
var
  CurrentBindings: TIdSipContacts;
  ExpireTime:      Cardinal;
  Notification:    TIdSipRegistrationSucceededMethod;
  OurContact:      TIdSipContactHeader;
begin
  CurrentBindings := TIdSipContacts.Create(Response.Headers);
  try
    Notification := TIdSipRegistrationSucceededMethod.Create;
    try
      Notification.CurrentBindings := CurrentBindings;
      Notification.Registration    := Self;

      Self.Listeners.Notify(Notification);
    finally
      Notification.Free;
    end;

    if Self.OutModule.AutoReRegister then begin
      // OurContact should always be assigned, because we've supposedly just
      // REGISTERed it. If it's not assigned then the registrar didn't actually
      // save our registration, and still had the cheek to return a 2xx rather
      // than some sort've error response.
      OurContact := CurrentBindings.ContactFor(Self.InitialRequest.FirstContact);
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
          ExpireTime := Response.FirstExpires.NumericValue;

        if (ExpireTime > 0) then
          Self.UA.ScheduleEvent(Self.OutModule.OnReregister,
                                Self.ReregisterTime(ExpireTime)*1000, // in milliseconds
                                Self.InitialRequest.Copy);
      end;
    end;
  finally
    CurrentBindings.Free;
  end;

  Self.Terminate;
end;

function TIdSipOutboundRegistration.ReceiveFailureResponse(Response: TIdSipResponse): TIdSipActionStatus;
begin
  Result := inherited ReceiveFailureResponse(Response);

  if (Result = asFailure) then begin
    case Response.StatusCode of
      SIPIntervalTooBrief: begin
        Self.ReissueRequestWithLongerExpiry(Self.InitialRequest.RequestUri,
                                            Response.FirstMinExpires.NumericValue);
        Result := asSuccess;
      end;

      SIPBadExtension: begin
        if Self.InitialRequest.HasHeader(RequireHeader) then begin
          Self.RetryWithoutExtensions(Self.InitialRequest.RequestUri,
                                      Response);
          Result := asSuccess;
        end;
      end;
    else
      Result := asFailure;
    end;
  end;
end;

function TIdSipOutboundRegistration.ReceiveRedirectionResponse(Response: TIdSipResponse;
                                                               UsingSecureTransport: Boolean): TIdSipActionStatus;
var
  NewAttempt: TIdSipOutboundRegister;
begin
  Result := asFailure;

  if Response.HasHeader(ContactHeaderFull) then begin
    NewAttempt := Self.OutModule.RegisterWith(Response.FirstContact.Address);
    NewAttempt.AddListeners(Self.Listeners);
    NewAttempt.Send;

    Result := asSuccess;
  end;
end;

procedure TIdSipOutboundRegistration.RegisterWith(Registrar: TIdSipUri;
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

procedure TIdSipOutboundRegistration.RegisterWith(Registrar: TIdSipUri;
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

procedure TIdSipOutboundRegistration.SendRequest(Request: TIdSipRequest;
                                                 TryAgain: Boolean = true);
begin
  Self.InitialRequest.Assign(Request);

  inherited SendRequest(Request, TryAgain);
end;

procedure TIdSipOutboundRegistration.Unregister(Registrar: TIdSipUri);
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
      Request.FirstExpires.NumericValue := 0;

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    RemovalBindings.Free;
  end;
end;

//* TIdSipOutboundRegistration Private methods *********************************

procedure TIdSipOutboundRegistration.ReissueRequestWithLongerExpiry(Registrar: TIdSipUri;
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

      Request.FirstExpires.NumericValue := MinimumExpiry;
      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    OriginalBindings.Free;
  end;
end;

procedure TIdSipOutboundRegistration.RetryWithoutExtensions(Registrar: TIdSipUri;
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
        // A 420 Bad Extension MUST have an unsupported header. In the
        // interests of accepting liberally though, we just drop all
        // Requires.
        Request.RemoveAllHeadersNamed(RequireHeader);
      end
      else
        Request.FirstRequire.RemoveValues(Response.FirstUnsupported);

      Self.SendRequest(Request);
    finally
      Request.Free;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TIdSipOutboundRegistration.SetBindings(Value: TIdSipContacts);
begin
  Self.fBindings.Clear;
  Self.fBindings.Add(Value);
end;

procedure TIdSipOutboundRegistration.SetRegistrar(Value: TIdSipUri);
begin
  Self.fRegistrar.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipOutboundRegistrationQuery                                            *
//******************************************************************************
//* TIdSipOutboundRegistrationQuery Public methods *****************************

procedure TIdSipOutboundRegistrationQuery.Send;
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
//* TIdSipOutboundUnRegister                                                   *
//******************************************************************************
//* TIdSipOutboundUnRegister Public methods ************************************

procedure TIdSipOutboundUnRegister.Send;
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

//* TIdSipOutboundUnRegister Protected methods *********************************

function TIdSipOutboundUnRegister.CreateRegister(Registrar: TIdSipUri;
                                                 Bindings: TIdSipContacts): TIdSipRequest;
begin
  Result := inherited CreateRegister(Registrar, Bindings);

  Result.FirstExpires.NumericValue := ExpireNow;
end;

procedure TIdSipOutboundUnRegister.Initialise(UA: TIdSipAbstractUserAgent;
                                              Request: TIdSipRequest;
                                              UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.IsWildCard := false;
end;

//******************************************************************************
//* TIdSipRegistrationFailedMethod                                             *
//******************************************************************************
//* TIdSipRegistrationFailedMethod Public methods ******************************

procedure TIdSipRegistrationFailedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipRegistrationListener).OnFailure(Self.Registration,
                                                    Self.CurrentBindings,
                                                    Self.Response);
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
//* EIdSipRegistrarNotFound                                                    *
//******************************************************************************
//* EIdSipRegistrarNotFound Public methods *************************************

constructor EIdSipRegistrarNotFound.Create(const Msg: string);
begin
  inherited Create(Format(NoSuchRegistrar, [Msg]));
end;

end.
