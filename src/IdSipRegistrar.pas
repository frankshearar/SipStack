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

    function  GetDefaultExpiryTime: Cardinal;
    procedure RejectExpireTooBrief(Request: TIdSipRequest;
                                   Transaction: TIdSipTransaction);
    procedure RejectFailedRequest(Request: TIdSipRequest;
                                  Transaction: TIdSipTransaction);
    procedure RejectForbidden(Request: TIdSipRequest;
                              Transaction: TIdSipTransaction);
    procedure RejectNotFound(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction);
    procedure RejectUnauthorized(Request: TIdSipRequest;
                                 Transaction: TIdSipTransaction);
    procedure SetDefaultExpiryTime(Value: Cardinal);
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Transaction: TIdSipTransaction;
                           Receiver: TIdSipTransport); override;
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest;
                            Transaction: TIdSipTransaction); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create; override;

    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;

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
(*
function TIdSipRegistrar.ReceiveRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport): Boolean;
begin
  if Request.IsRegister then begin
    if Request.HasHeader(ContactHeaderFull) then begin
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
*)

//* TIdSipRegistrar Protected methods ******************************************

procedure TIdSipRegistrar.ActOnRequest(Request: TIdSipRequest;
                                       Transaction: TIdSipTransaction;
                                       Receiver: TIdSipTransport);
var
  Bindings: TIdSipContacts;
  Date:     TIdSipDateHeader;
  Response: TIdSipResponse;
begin
  if (Request.ContactCount = 1)
    and Request.FirstContact.IsWildCard
    and (Request.QuickestExpiry = 0) then begin

    if not Self.BindingDB.RemoveAllBindings(Request) then
      Self.RejectFailedRequest(Request, Transaction)
    else
      Self.ReturnResponse(Request, SIPOK, Transaction);
    Exit;
  end;

  if not Self.BindingDB.AddBindings(Request) then begin
    Self.RejectFailedRequest(Request, Transaction);
    Exit;
  end;

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

procedure TIdSipRegistrar.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                        Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction);
begin
  inherited RejectRequest(Reaction, Request, Transaction);

  case Reaction of
    uarBadRequest:
      Self.ReturnResponse(Request, SIPBadRequest, Transaction);
    uarExpireTooBrief:
      Self.RejectExpireTooBrief(Request, Transaction);
    uarForbidden:
      Self.RejectForbidden(Request, Transaction);
    uarNotFound:
      Self.RejectNotFound(Request, Transaction);
    uarUnauthorized:
      Self.RejectUnauthorized(Request, Transaction);
  end;
end;

function TIdSipRegistrar.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  // cf RFC 3261 section 10.3
  if (Result = uarAccept) then begin
{
    // Step 3
    if not Request.HasAuthenticationInfo
    or not Self.BindingDB.IsAuthenticated(Request) then
      Result := uarUnauthorized
}
    // Step 4
    if not Self.BindingDB.IsAuthorized(Request.From) then
      Result := uarForbidden
    // Step 5
    else if not Self.BindingDB.IsValid(Request) then
      Result := uarNotFound
    // Step 6 (or part thereof)
    else if Request.HasHeader(ContactHeaderFull) then begin
      if Request.FirstContact.IsWildCard then begin
        if (Request.ContactCount > 1) then
          Result := uarBadRequest
        else begin
          if Request.FirstContact.WillExpire
            and (Request.FirstContact.Expires = 0) then
            Result := uarAccept
          else
            Result := uarBadRequest;
        end;
      end
      else if Request.HasExpiry and (Request.QuickestExpiry < Self.MinimumExpiryTime) then
        Result := uarExpireTooBrief
    end;
  end;

  // The rest of Step 6 takes place in ActOnRequest
end;

//* TIdSipRegistrar Private methods ********************************************

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

procedure TIdSipRegistrar.RejectNotFound(Request: TIdSipRequest;
                                         Transaction: TIdSipTransaction);
begin
  Self.ReturnResponse(Request, SIPNotFound, Transaction);
end;

procedure TIdSipRegistrar.RejectUnauthorized(Request: TIdSipRequest;
                                             Transaction: TIdSipTransaction);
begin
  raise Exception.Create('TIdSipRegistrar.RejectUnauthorized not yet implemented');
end;

procedure TIdSipRegistrar.SetDefaultExpiryTime(Value: Cardinal);
begin
  Self.BindingDB.DefaultExpiryTime := Value;
end;

end.
