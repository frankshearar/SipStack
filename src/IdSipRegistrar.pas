unit IdSipRegistrar;

interface

uses
  IdSipCore, IdSipDialog, IdSipHeaders, IdSipMessage, IdSipTransaction,
  IdSipTransport;

type
  // I represent a binding between and address-of-record and a URI. A SIP
  // Registrar (via a Binding Database) uses me to keep track of
  // registrations.
  TIdRegistrarBinding = class(TObject)
  private
    fCallID:     String;
    fSequenceNo: Cardinal;
    fUri:        String;
    fValidUntil: TDateTime;
  public
    constructor Create(const CanonicalisedUri: String;
                       const CallID: String;
                       SequenceNo: Cardinal;
                       AbsoluteTimeout: TDateTime);

    property CallID:     String    read fCallID write fCallID;
    property SequenceNo: Cardinal  read fSequenceNo write fSequenceNo;
    property Uri:        String    read fUri write fUri;
    property ValidUntil: TDateTime read fValidUntil write fValidUntil;
  end;

  // I provide a database that matches a SIP Address-of-record (a SIP or SIPS
  // URI) with one or more "bindings" (usually SIP/SIPS URIs, but they can
  // follow any URI scheme).
  //
  // My subclasses guarantee ACID properties for all of their implementations
  // of my methods.
  //
  // Currently I only support the registration of SIP and SIPS URIs.
  TIdSipAbstractBindingDatabase = class(TObject)
  private
    fDefaultExpiryTime: Cardinal;

    function CorrectExpiry(Request: TIdSipRequest;
                           Contact: TIdSipContactHeader): Cardinal;
  protected
    function  AddBinding(const AddressOfRecord: String;
                         Contact: TIdSipUri;
                         const CallID: String;
                         SequenceNo: Cardinal;
                         ExpiryTime: TDateTime): Boolean; virtual; abstract;
    function  Binding(const AddressOfRecord: String;
                      const CanonicalUri: String): TIdRegistrarBinding; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
  public
    constructor Create; virtual;

    function  AddBindings(Request: TIdSipRequest): Boolean;
    function  IsValid(Request: TIdSipRequest): Boolean; virtual; abstract;
    procedure BindingsFor(Request: TIdSipRequest;
                          Contacts: TIdSipContacts); virtual; abstract;
    function  MayRemoveBinding(Request: TIdSipRequest;
                               Contact: TIdSipContactHeader): Boolean;
    function  RemoveAllBindings(Request: TIdSipRequest): Boolean;
    function  RemoveBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; virtual; abstract;

    property DefaultExpiryTime: Cardinal read fDefaultExpiryTime write fDefaultExpiryTime;
  end;

  // I'm a User Agent Server that only accepts REGISTER requests. I provide a
  // means of administering a bindings database. For instance, the AOR
  // "sip:carol@chicago.com" might need to be routed to the URI
  // "sip:carol@cube2214a.chicago.com". I provide a means for Carol's user agent
  // to register her real URI with chicago.com's SIP proxy, as well as providing
  // proxy.chicago.com with a means of finding out this registration.
  TIdSipRegistrar = class(TIdSipAbstractUserAgent)
  private
    fBindingDB:         TIdSipAbstractBindingDatabase;
    fMinimumExpiryTime: Cardinal; // in seconds

    procedure Accept(Request: TIdSipRequest;
                     Transaction: TIdSipTransaction);
    function  GetDefaultExpiryTime: Cardinal;
    function  ProcessContacts(Request: TIdSipRequest): Boolean;
    procedure RejectExpireTooBrief(Request: TIdSipRequest;
                                   Transaction: TIdSipTransaction);
    procedure RejectNonRegister(Request: TIdSipRequest;
                                Transaction: TIdSipTransaction);
    procedure RejectNotFound(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction);
    procedure SetDefaultExpiryTime(Value: Cardinal);
  public
    constructor Create; override;

    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    function  ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Receiver: TIdSipTransport): Boolean; override;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              Transaction: TIdSipTransaction;
                              Receiver: TIdSipTransport); override;

    property BindingDB:         TIdSipAbstractBindingDatabase read fBindingDB write fBindingDB;
    property DefaultExpiryTime: Cardinal                      read GetDefaultExpiryTime write SetDefaultExpiryTime;
    property MinimumExpiryTime: Cardinal                      read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

implementation

uses
  IdSipConsts, SysUtils;

const
  TenMinutes = 600; // seconds  

//******************************************************************************
//* TIdRegistrarBinding                                                        *
//******************************************************************************
//* TIdRegistrarBinding Public methods *****************************************

constructor TIdRegistrarBinding.Create(const CanonicalisedUri: String;
                                       const CallID: String;
                                       SequenceNo: Cardinal;
                                       AbsoluteTimeout: TDateTime);
begin
  inherited Create;

  Self.fCallID     := CallID;
  Self.fSequenceNo := SequenceNo;
  Self.fUri        := CanonicalisedUri;
  Self.fValidUntil := AbsoluteTimeout;
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
  AddressOfRecord := Request.RequestUri.CanonicaliseAsAddressOfRecord;

  Self.StartTransaction;
  try
    Result := true;
    Contacts := TIdSipContacts.Create(Request.Headers);
    try
      Contacts.First;
      while Contacts.HasNext do begin
        Binding := Self.Binding(AddressOfRecord,
                                Contacts.CurrentContact.Address.CanonicaliseAsAddressOfRecord);
        Expiry := Self.CorrectExpiry(Request,
                                     Contacts.CurrentContact);

        if Assigned(Binding) then begin
          Result := Result
                and Self.MayRemoveBinding(Request,
                                          Contacts.CurrentContact);
          if Result and (Expiry = 0) then begin
            Result := Result and Self.RemoveBinding(Request,
                                                    Contacts.CurrentContact);
          end;
        end
        else begin
          Result := Result and Self.AddBinding(AddressOfRecord,
                                               Contacts.CurrentContact.Address,
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
end;

function TIdSipAbstractBindingDatabase.MayRemoveBinding(Request: TIdSipRequest;
                                                        Contact: TIdSipContactHeader): Boolean;
var
  BindingRecord: TIdRegistrarBinding;
begin
  BindingRecord := Self.Binding(Request.RequestUri.CanonicaliseAsAddressOfRecord,
                                Contact.Address.CanonicaliseAsAddressOfRecord);

  Result := BindingRecord.CallID <> Request.CallID;

  if not Result then
    Result := BindingRecord.SequenceNo < Request.CSeq.SequenceNo;
end;

function TIdSipAbstractBindingDatabase.RemoveAllBindings(Request: TIdSipRequest): Boolean;
var
  Contacts: TIdSipContacts;
begin
  Self.StartTransaction;
  try
    Contacts := TIdSipContacts.Create;
    try
      Self.BindingsFor(Request, Contacts);
      Contacts.First;
      while Contacts.HasNext do begin
        if Self.MayRemoveBinding(Request, Contacts.CurrentContact) then
          Self.RemoveBinding(Request, Contacts.CurrentContact)
        else
          raise Exception.Create('Error removing binding');
        Contacts.Next;
      end;
    finally
      Contacts.Free;
    end;

    Result := true;
    Self.Commit;
  except
    Result := false;
    Self.Rollback;
  end;
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

  Self.AddAllowedMethod(MethodRegister);
  Self.AddAllowedScheme(SipScheme);
  Self.AddAllowedScheme(SipsScheme);
end;

function TIdSipRegistrar.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  raise Exception.Create('Registrars may not create in-dialog requests');
end;

function TIdSipRegistrar.ReceiveRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport): Boolean;
begin
//      1. The registrar inspects the Request-URI to determine whether it
//         has access to bindings for the domain identified in the
//         Request-URI.  If not, and if the server also acts as a proxy
//         server, the server SHOULD forward the request to the addressed
//         domain, following the general behavior for proxying messages
//         described in Section 16.
  Result := inherited ReceiveRequest(Request, Transaction, Receiver);

  if not Result then Exit;

  if Request.IsRegister then begin
//      3. A registrar SHOULD authenticate the UAC.  Mechanisms for the
//         authentication of SIP user agents are described in Section 22.
//         Registration behavior in no way overrides the generic
//         authentication framework for SIP.  If no authentication
//         mechanism is available, the registrar MAY take the From address
//         as the asserted identity of the originator of the request.
//
//      4. The registrar SHOULD determine if the authenticated user is
//         authorized to modify registrations for this address-of-record.
//         For example, a registrar might consult an authorization
//         database that maps user names to a list of addresses-of-record
//         for which that user has authorization to modify bindings.  If
//         the authenticated user is not authorized to modify bindings,
//         the registrar MUST return a 403 (Forbidden) and skip the
//         remaining steps.

    if not Self.BindingDB.IsValid(Request) then begin
      Self.RejectNotFound(Request, Transaction);
      Exit;
    end;

    if Request.HasHeader(ContactHeaderFull) then begin
      if Request.FirstContact.IsWildCard then begin
        if (Request.ContactCount > 1) then begin
          Self.ReturnResponse(Request, SIPBadRequest, Transaction);
        end
        else if Request.FirstContact.WillExpire
            and (Request.FirstContact.Expires = 0) then begin
          // Actually do the handling of the wildcard
          // For each of the bindings, check the Call-ID stored with the
          // binding. If that's not the same as Request.CallID then remove the
          // binding, otherwise remove the binding if the stored
          // CSeq < Request.CSeq.SequenceNo. Otherwise abort (with a 400?)
          Self.BindingDB.RemoveAllBindings(Request);
          Self.Accept(Request, Transaction);
        end
        else begin
          Self.ReturnResponse(Request, SIPBadRequest, Transaction);
        end;
        Exit;
      end;

      if Request.HasExpiry and (Request.MinimumExpiry < Self.MinimumExpiryTime) then begin
        Self.RejectExpireTooBrief(Request, Transaction);
        Exit;
      end;

      // Self.BindingDatabase provides this step's atomicity, as required by
      // RFC 3261, section 10.3
      if not Self.ProcessContacts(Request) then begin
        // Adding the contacts failed. What to do?
        // return a 500!
      end;
    end;

    Self.Accept(Request, Transaction);
  end
  else begin
    Self.RejectNonRegister(Request, Transaction);
  end;
end;

procedure TIdSipRegistrar.ReceiveResponse(Response: TIdSipResponse;
                                          Transaction: TIdSipTransaction;
                                          Receiver: TIdSipTransport);
begin
end;

//* TIdSipRegistrar Private methods ********************************************

procedure TIdSipRegistrar.Accept(Request: TIdSipRequest;
                                 Transaction: TIdSipTransaction);
var
  Bindings: TIdSipContacts;
  Response: TIdSipResponse;
begin
  Bindings := TIdSipContacts.Create;
  try
    Self.BindingDB.BindingsFor(Request,
                               Bindings);

    Response := Self.CreateResponse(Request, SIPOK);
    try
      Response.AddHeaders(Bindings);
      Transaction.SendResponse(Response);
    finally
    end;
  finally
    Bindings.Free;
  end;
end;

function TIdSipRegistrar.GetDefaultExpiryTime: Cardinal;
begin
  Result := Self.BindingDB.DefaultExpiryTime;
end;

function TIdSipRegistrar.ProcessContacts(Request: TIdSipRequest): Boolean;
begin
  Result := Self.BindingDB.AddBindings(Request);
end;

procedure TIdSipRegistrar.RejectExpireTooBrief(Request: TIdSipRequest;
                                               Transaction: TIdSipTransaction);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.MinimumExpiryTime);
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipRegistrar.RejectNonRegister(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPMethodNotAllowed, Transaction);
end;

procedure TIdSipRegistrar.RejectNotFound(Request: TIdSipRequest;
                                         Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPNotFound, Transaction);
end;

procedure TIdSipRegistrar.SetDefaultExpiryTime(Value: Cardinal);
begin
  Self.BindingDB.DefaultExpiryTime := Value;
end;

end.
