unit IdSipRegistrar;

interface

uses
  IdSipCore, IdSipDialog, IdSipHeaders, IdSipMessage, IdSipTransaction,
  IdSipTransport;

type
  // I provide a database that matches a SIP Address-of-record (a SIP or SIPS
  // URI) with one or more "bindings" (usually SIP/SIPS URIs, but they can
  // follow any URI scheme).
  TIdSipAbstractBindingDatabase = class(TObject)
  public
    procedure AddBinding(const AddressOfRecord: String;
                         Binding: TIdSipContactHeader); virtual; abstract;
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
    function  ProcessContact(Contact: TIdSipContactHeader;
                             Request: TIdSipRequest;
                             Transaction: TIdSipTransaction): Boolean;
    procedure RejectExpireTooBrief(Request: TIdSipRequest;
                                   Transaction: TIdSipTransaction);
    procedure RejectNonRegister(Request: TIdSipRequest;
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
  I:        Integer;
begin
  if Request.IsRegister then begin
    if Request.HasHeader(ExpiresHeader) and
      ((Request.FirstHeader(ExpiresHeader) as TIdSipNumericHeader).NumericValue < Self.MinimumExpiryTime) then begin
      Self.RejectExpireTooBrief(Request, Transaction);
      Exit;
    end;

    Contacts := TIdSipHeadersFilter.Create(Request.Headers, ContactHeaderFull);
    try
      Contacts.First;
      while Contacts.HasNext do begin
        if not Self.ProcessContact(Contacts.CurrentHeader as TIdSipContactHeader,
                                   Request,
                                   Transaction) then begin
          Self.RejectExpireTooBrief(Request, Transaction);
          Exit;
        end;
        Contacts.Next;
      end;
    finally
      Contacts.Free;
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
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPOK);
  try
    // load up all bindings for this AOR
    Transaction.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

function TIdSipRegistrar.ProcessContact(Contact: TIdSipContactHeader;
                                        Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction): Boolean;
begin
  Result := not (Contact.HasParam(ExpiresParam)
                and (Contact.Expires < Self.MinimumExpiryTime));

  if Result then;
end;

procedure TIdSipRegistrar.RejectExpireTooBrief(Request: TIdSipRequest;
                                               Transaction: TIdSipTransaction);
begin
  Self.RejectRequest(Request, SIPIntervalTooBrief, Transaction);
end;

procedure TIdSipRegistrar.RejectNonRegister(Request: TIdSipRequest;
                                            Transaction: TIdSipTransaction);
begin
  Self.RejectRequest(Request, SIPMethodNotAllowed, Transaction);
end;

end.
