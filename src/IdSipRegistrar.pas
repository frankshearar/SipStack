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

    property AddressOfRecord: String read fAddressOfRecord write fAddressOfRecord;
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
  // of my methods. All the methods that actually affect records return True
  // if they succeeded and False otherwise. The predicate methods behave
  // almost the same, but we can't distinguish between the cases where we
  // can't authenticate a user's credentials (because the password didn't
  // match) and a failure to read the user's details (because a disk failed).
  // That works just fine though - it means that authorization/authentication
  // fails safe.
  //
  // Currently I only support the registration of SIP and SIPS URIs.
  TIdSipAbstractBindingDatabase = class(TObject)
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
    constructor Create; virtual;

    function  AddBindings(Request: TIdSipRequest): Boolean;
    function  IsAuthorized(User: TIdSipAddressHeader): Boolean; virtual; abstract;
    function  IsValid(Request: TIdSipRequest): Boolean; virtual; abstract;
    function  BindingsFor(Request: TIdSipRequest;
                          Contacts: TIdSipContacts): Boolean; virtual; abstract;
    function  NotOutOfOrder(Request: TIdSipRequest;
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
    procedure RejectExpireTooBrief(Request: TIdSipRequest;
                                   Transaction: TIdSipTransaction);
    procedure RejectFailedRequest(Request: TIdSipRequest;
                                  Transaction: TIdSipTransaction);
    procedure RejectForbidden(Request: TIdSipRequest;
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
//      2. To guarantee that the registrar supports any necessary
//         extensions, the registrar MUST process the Require header field
//         values as described for UASs in Section 8.2.2.
  Result := inherited ReceiveRequest(Request, Transaction, Receiver);

  if not Result then Exit;

  // This check should always pass (that is, be redundant) since a Registrar's
  // sole entry in AllowedMethods is 'REGISTER'.
  if Request.IsRegister then begin
//      3. A registrar SHOULD authenticate the UAC.  Mechanisms for the
//         authentication of SIP user agents are described in Section 22.
//         Registration behavior in no way overrides the generic
//         authentication framework for SIP.  If no authentication
//         mechanism is available, the registrar MAY take the From address
//         as the asserted identity of the originator of the request.
{
    if not Self.Request.HasAuthenticationInfo
    or not Self.BindingDB.IsAuthenticated(Request) then begin
      Self.RejectUnauthorized(Request, Transaction);
      Exit;
    end;
}
//      4. The registrar SHOULD determine if the authenticated user is
//         authorized to modify registrations for this address-of-record.
//         For example, a registrar might consult an authorization
//         database that maps user names to a list of addresses-of-record
//         for which that user has authorization to modify bindings.  If
//         the authenticated user is not authorized to modify bindings,
//         the registrar MUST return a 403 (Forbidden) and skip the
//         remaining steps.

    if not Self.BindingDB.IsAuthorized(Request.From) then begin
      Self.RejectForbidden(Request, Transaction);
      Exit;
    end;

//      5. The registrar extracts the address-of-record from the To header
//         field of the request.  If the address-of-record is not valid
//         for the domain in the Request-URI, the registrar MUST send a
//         404 (Not Found) response and skip the remaining steps.  The URI
//         MUST then be converted to a canonical form.  To do that, all
//         URI parameters MUST be removed (including the user-param), and
//         any escaped characters MUST be converted to their unescaped
//         form.  The result serves as an index into the list of bindings.
    if not Self.BindingDB.IsValid(Request) then begin
      Self.RejectNotFound(Request, Transaction);
      Exit;
    end;

    if Request.HasHeader(ContactHeaderFull) then begin
//      6. The registrar checks whether the request contains the Contact
//         header field.  If not, it skips to the last step.  If the
//         Contact header field is present, the registrar checks if there
//         is one Contact field value that contains the special value "*"
//         and an Expires field.  If the request has additional Contact
//         fields or an expiration time other than zero, the request is
//         invalid, and the server MUST return a 400 (Invalid Request) and
//         skip the remaining steps.  If not, the registrar checks whether
//         the Call-ID agrees with the value stored for each binding.  If
//         not, it MUST remove the binding.  If it does agree, it MUST
//         remove the binding only if the CSeq in the request is higher
//         than the value stored for that binding.  Otherwise, the update
//         MUST be aborted and the request fails.
      if Request.FirstContact.IsWildCard then begin
        if (Request.ContactCount > 1) then begin
          Self.ReturnResponse(Request, SIPBadRequest, Transaction);
        end
        else if Request.FirstContact.WillExpire
            and (Request.FirstContact.Expires = 0) then begin
          // Actually do the handling of the wildcard.
          // For each of the bindings, check the Call-ID stored with the
          // binding. If that's not the same as Request.CallID then remove the
          // binding, otherwise remove the binding if the stored
          // CSeq < Request.CSeq.SequenceNo. Otherwise abort (with a 400?)
          if Self.BindingDB.RemoveAllBindings(Request) then
            Self.Accept(Request, Transaction)
          else
            Self.RejectFailedRequest(Request, Transaction);
        end
        else begin
          Self.ReturnResponse(Request, SIPBadRequest, Transaction);
        end;
        Exit;
      end;

      if Request.HasExpiry and (Request.QuickestExpiry < Self.MinimumExpiryTime) then begin
        Self.RejectExpireTooBrief(Request, Transaction);
        Exit;
      end;

      // Self.BindingDatabase provides this step's atomicity, as required by
      // RFC 3261, section 10.3
      if not Self.BindingDB.AddBindings(Request) then begin
        Self.RejectFailedRequest(Request, Transaction);
        Exit;
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
  // We don't expect to receive responses, so we just drop them on the floor.
end;

//* TIdSipRegistrar Private methods ********************************************

procedure TIdSipRegistrar.Accept(Request: TIdSipRequest;
                                 Transaction: TIdSipTransaction);
var
  Bindings: TIdSipContacts;
  Date:     TIdSipDateHeader;
  Response: TIdSipResponse;
begin
  Bindings := TIdSipContacts.Create;
  try
    if Self.BindingDB.BindingsFor(Request,
                                  Bindings) then begin
      Response := Self.CreateResponse(Request, SIPOK);
      try
        Response.AddHeaders(Bindings);

        Date := TIdSipDateHeader.Create;
        try
          Date.Time.SetFromTDateTime(Now);
          Response.AddHeader(Date);
        finally
          Date.Free;
        end;

        Transaction.SendResponse(Response);
      finally
        Response.Free;
      end;
    end
    else begin
      Self.RejectFailedRequest(Request, Transaction);
    end;
  finally
    Bindings.Free;
  end;
end;

function TIdSipRegistrar.GetDefaultExpiryTime: Cardinal;
begin
  Result := Self.BindingDB.DefaultExpiryTime;
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

procedure TIdSipRegistrar.RejectFailedRequest(Request: TIdSipRequest;
                                              Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPInternalServerError, Transaction);
end;

procedure TIdSipRegistrar.RejectForbidden(Request: TIdSipRequest;
                                          Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPForbidden, Transaction);
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
