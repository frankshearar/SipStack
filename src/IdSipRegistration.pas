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
  Contnrs, IdException, IdObservable, IdSipMessage, SyncObjs;

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

  EIdSipRegistrarNotFound = class(EIdException)
  public
    constructor Create(const Msg: string); reintroduce;
  end;

implementation

uses
  IdSipConsts, SysUtils;

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
//* EIdSipRegistrarNotFound                                                    *
//******************************************************************************
//* EIdSipRegistrarNotFound Public methods *************************************

constructor EIdSipRegistrarNotFound.Create(const Msg: string);
begin
  inherited Create(Format(NoSuchRegistrar, [Msg]));
end;

end.
