unit IdSipRegistrar;

interface

uses
  IdSipCore, IdSipDialog, IdSipHeaders, IdSipMessage, IdSipTransaction,
  IdSipTransport;

type
  // I provide a database that matches a SIP Address-of-record (a SIP or SIPS
  // URI) with one or more "bindings" (usually SIP/SIPS URIs, but they can
  // follow any URI scheme).
  //
  // My subclasses guarantee ACID properties for all of their implementations
  // of my methods.
  TIdSipAbstractBindingDatabase = class(TObject)
  public
    procedure AddBinding(const AddressOfRecord: String;
                         Binding: TIdSipContactHeader); virtual; abstract;
    procedure AddBindings(const AddressOfRecord: String;
                          Bindings: TIdSipHeaderList); virtual; abstract;
    function  IsValid(AddressOfRecord: TIdSipUri): Boolean; virtual; abstract;
    procedure BindingsFor(const AddressOfRecord: String;
                          Bindings: TIdSipHeaders); virtual; abstract;
    procedure RemoveAllBindings(const AddressOfRecord: String); virtual; abstract;
    procedure RemoveBinding(const AddressOfRecord: String;
                            Binding: TIdSipContactHeader); virtual; abstract;
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
    function  ProcessContact(AddressOfRecord: TIdSipUri;
                             Contact: TIdSipContactHeader): Boolean;
    function  ProcessContacts(Request: TIdSipRequest;
                              Contacts: TIdSipHeaderList): Boolean;
    procedure RejectExpireTooBrief(Request: TIdSipRequest;
                                   Transaction: TIdSipTransaction);
    procedure RejectNonRegister(Request: TIdSipRequest;
                                Transaction: TIdSipTransaction);
    procedure RejectNotFound(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction);
  public
    constructor Create; override;

    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Receiver: TIdSipTransport); override;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              Transaction: TIdSipTransaction;
                              Receiver: TIdSipTransport); override;

    property BindingDB:         TIdSipAbstractBindingDatabase read fBindingDB write fBindingDB;
    property MinimumExpiryTime: Cardinal                      read fMinimumExpiryTime write fMinimumExpiryTime;
  end;

implementation

uses
  IdSipConsts, SysUtils;

//******************************************************************************
//* TIdSipRegistrar                                                            *
//******************************************************************************
//* TIdSipRegistrar Public methods *********************************************

constructor TIdSipRegistrar.Create;
begin
  inherited Create;

  Self.AddAllowedMethod(MethodRegister);
end;

function TIdSipRegistrar.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  raise Exception.Create('Registrars may not create in-dialog requests');
end;

procedure TIdSipRegistrar.ReceiveRequest(Request: TIdSipRequest;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
var
  Contacts: TIdSipHeadersFilter;
begin
  if Request.IsRegister then begin
//      1. The registrar inspects the Request-URI to determine whether it
//         has access to bindings for the domain identified in the
//         Request-URI.  If not, and if the server also acts as a proxy
//         server, the server SHOULD forward the request to the addressed
//         domain, following the general behavior for proxying messages
//         described in Section 16.
//
//      2. To guarantee that the registrar supports any necessary
//         extensions, the registrar MUST process the Require header field
//         values as described for UASs in Section 8.2.2.
//
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

    if not Self.BindingDB.IsValid(Request.RequestUri) then begin
      Self.RejectNotFound(Request, Transaction);
      Exit;
    end;

    if Request.HasHeader(ContactHeaderFull) then begin
      if Request.FirstContact.IsWildCard and Request.HasHeader(ExpiresHeader) then begin
      end;

      Contacts := TIdSipHeadersFilter.Create(Request.Headers, ContactHeaderFull);
      try
        // Self.BindingDatabase provides this step's atomicity, as required by
        // RFC 3261, section 10.3
        if not Self.ProcessContacts(Request, Contacts) then begin
          Self.RejectExpireTooBrief(Request, Transaction);
          Exit;
        end;
      finally
        Contacts.Free;
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
  Bindings: TIdSipHeaders;
  Response: TIdSipResponse;
begin
  Bindings := TIdSipHeaders.Create;
  try
    Self.BindingDB.BindingsFor(Request.RequestUri.CanonicaliseAsAddressOfRecord,
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

function TIdSipRegistrar.ProcessContact(AddressOfRecord: TIdSipUri;
                                        Contact: TIdSipContactHeader): Boolean;
begin
  Result := not (Contact.HasParam(ExpiresParam)
                and (Contact.Expires < Self.MinimumExpiryTime));

  if Result then
    Self.BindingDB.AddBinding(AddressOfRecord.CanonicaliseAsAddressOfRecord,
                              Contact);
end;

function TIdSipRegistrar.ProcessContacts(Request: TIdSipRequest;
                                         Contacts: TIdSipHeaderList): Boolean;
begin
  if Request.HasHeader(ExpiresHeader) then
    Result := ((Request.FirstHeader(ExpiresHeader) as TIdSipNumericHeader).NumericValue >= Self.MinimumExpiryTime)
  else
    Result := true;

  Contacts.First;
  while Contacts.HasNext and Result do begin
    Result := Result
           and Self.ProcessContact(Request.RequestUri,
                                   Contacts.CurrentHeader as TIdSipContactHeader);
    Contacts.Next;
  end;
end;

procedure TIdSipRegistrar.RejectExpireTooBrief(Request: TIdSipRequest;
                                               Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPIntervalTooBrief, Transaction);
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

end.
