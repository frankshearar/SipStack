unit TestIdSipRegistrar;

interface

uses
  Classes, Contnrs, IdSipConsts, IdSipHeaders, IdSipMessage,
  IdSipMockTransactionDispatcher, IdSipRegistrar, TestFramework;

type
  TIdSipMockBindingDatabase = class;

  // We test that the registrar returns the responses it should. The nitty
  // gritties of how the registrar preserves ACID properties, or the ins and
  // outs of actual database stuff doesn't interest us - look at
  // TestTIdSipAbstractBindingDatabase for that.
  TestTIdSipRegistrar = class(TTestCase)
  private
    DB:           TIdSipMockBindingDatabase;
    Dispatch:     TIdSipMockTransactionDispatcher;
    ExpireAll:    String;
    FirstContact: TIdSipContactHeader;
    Registrar:    TIdSipRegistrar;
    Request:      TIdSipRequest;

    procedure CheckResponse(Received: TIdSipContacts;
                            const Msg: String);
    procedure CheckServerReturned(ExpectedStatusCode: Cardinal;
                                  const Msg: String);
    procedure CheckServerReturnedOK(const Msg: String);
    procedure SimulateRemoteRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestComplicatedRegistration;
    procedure TestDatabaseUpdatesBindings;
    procedure TestDatabaseGetsExpiry;
    procedure TestFailedBindingsFor;
    procedure TestFailedRemoveAll;
    procedure TestInvalidAddressOfRecord;
    procedure TestOKResponseContainsAllBindings;
    procedure TestReceiveInvite;
    procedure TestReceiveRegister;
    procedure TestReceiveExpireTooShort;
    procedure TestReceiveExpireParamTooShort;
    procedure TestReceiveWildcard;
    procedure TestReceiveWildcardWithExtraContacts;
    procedure TestReceiveWildcardWithNonzeroExpiration;
    procedure TestRegisterAddsBindings;
    procedure TestRegisterAddsMultipleBindings;
    procedure TestUnauthorizedUser;
  end;

  TIdSipMockBindingDatabase = class(TIdSipAbstractBindingDatabase)
  private
    BindingStore:       TObjectList;
    fAuthorized:        Boolean;
    fFailAddBinding:    Boolean;
    fFailBindingsFor:   Boolean;
    fFailIsValid:       Boolean;
    fFailRemoveBinding: Boolean;

    procedure DeleteBinding(Index: Integer);
    function  GetBindings(Index: Integer): TIdRegistrarBinding;
    function  IndexOfBinding(const AddressOfRecord: String;
                             Contact: TIdSipContactHeader): Integer;
  protected
    function  AddBinding(const AddressOfRecord: String;
                         Contact: TIdSipContactHeader;
                         const CallID: String;
                         SequenceNo: Cardinal;
                         ExpiryTime: TDateTime): Boolean; override;
    function  Binding(const AddressOfRecord: String;
                      const CanonicalUri: String): TIdRegistrarBinding; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure StartTransaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  BindingCount: Integer;
    function  BindingsFor(Request: TIdSipRequest;
                          Contacts: TIdSipContacts): Boolean; override;
    function  IsAuthorized(User: TIdSipAddressHeader): Boolean; override;
    function  IsValid(Request: TIdSipRequest): Boolean; override;
    function  RemoveBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; override;

    property Authorized:               Boolean             read fAuthorized write fAuthorized;
    property Bindings[Index: Integer]: TIdRegistrarBinding read GetBindings;
    property FailAddBinding:           Boolean             read fFailAddBinding write fFailAddBinding;
    property FailBindingsFor:          Boolean             read fFailBindingsFor write fFailBindingsFor;
    property FailRemoveBinding:        Boolean             read fFailRemoveBinding write fFailRemoveBinding;
    property FailIsValid:              Boolean             read fFailIsValid write fFailIsValid;
  end;

  // We test binding rules here. We test those things that could cause a
  // binding removal to fail and other such situations.
  TestTIdSipAbstractBindingDatabase = class(TTestCase)
  private
    DB:      TIdSipAbstractBindingDatabase;
    Request: TIdSipRequest;

    procedure CheckExpiry(Expected: Cardinal;
                          const Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindingUpdates;
    procedure TestAddBindingWithExpiryParam;
    procedure TestAddBindingWithExpiryHeader;
    procedure TestAddBindingWithNoExpiries;
    procedure TestAddBindingWithZeroExpiresRemovesBinding;
    procedure TestAddExistingBindingOutOfOrderSeqNo;
    procedure TestNotOutOfOrder;
    procedure TestNotOutOfOrderEarlySeqNo;
    procedure TestNotOutOfOrderDifferentCallID;
    procedure TestRemoveAllBindings;
  end;

  TestTIdSipMockBindingDatabase = class(TTestCase)
  private
    CaseContact:    TIdSipContactHeader;
    CasesAOR:       TIdSipRequest;
    DB:             TIdSipMockBindingDatabase;
    Wintermute:     TIdSipContactHeader;
    WintermutesAOR: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindings;
    procedure TestBindingsFor;
    procedure TestBindingsForClearsList;
    procedure TestIsAuthorized;
    procedure TestIsValid;
    procedure TestFailAddBinding;
    procedure TestFailBindingsFor;
    procedure TestFailRemoveBinding;
    procedure TestRemoveAllBindings;
    procedure TestRemoveBinding;
    procedure TestRemoveBindingWhenNotPresent;
  end;

implementation

uses
  DateUtils, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistrar unit tests');
  Result.AddTest(TestTIdSipRegistrar.Suite);
  Result.AddTest(TestTIdSipAbstractBindingDatabase.Suite);
  Result.AddTest(TestTIdSipMockBindingDatabase.Suite);
end;

//******************************************************************************
//* TestTIdSipRegistrar                                                        *
//******************************************************************************
//* TestTIdSipRegistrar Public methods *****************************************

procedure TestTIdSipRegistrar.SetUp;
begin
  inherited SetUp;

  Self.ExpireAll := ContactWildCard + ';' + ExpiresParam + '=0';

  Self.DB := TIdSipMockBindingDatabase.Create;
  Self.DB.FailIsValid := false;
//  Self.DB.DefaultExpiryTime := 0;

  Self.Dispatch := TIdSipMockTransactionDispatcher.Create;
  Self.Registrar := TIdSipRegistrar.Create;
  Self.Registrar.BindingDB := Self.DB;
  Self.Registrar.Dispatcher := Self.Dispatch;
  Self.Registrar.MinimumExpiryTime := 3600;

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP proxy.tessier-ashpool.co.luna;branch='
                                               + BranchMagicCookie + 'f00L';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CallID := '1@selftest.foo';

  Self.FirstContact := Self.Request.FirstHeader(ContactHeaderFull) as TIdSipContactHeader;
end;

procedure TestTIdSipRegistrar.TearDown;
begin
  Self.Request.Free;
  Self.Registrar.Free;
  Self.Dispatch.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrar Private methods ****************************************

procedure TestTIdSipRegistrar.CheckResponse(Received: TIdSipContacts;
                                            const Msg: String);
var
  Expected: TIdSipContacts;
  I:        Integer;
begin
  Expected := TIdSipContacts.Create(Self.Dispatch.Transport.LastResponse.Headers);
  try
    Expected.First;
    Received.First;

    I := 0;
    while Expected.HasNext do begin
      Check(Received.HasNext, 'Received too few Expected');

      Check(Abs(Expected.CurrentContact.Expires
              - Received.CurrentContact.Expires) < 2,
            'Expires param; I = ' + IntToStr(I));

      Expected.CurrentContact.RemoveExpires;
      Received.CurrentContact.RemoveExpires;
      CheckEquals(Expected.CurrentContact.Address.Uri,
                  Received.CurrentContact.Address.Uri,
            'URI; I = ' + IntToStr(I));

      Expected.Next;
      Received.Next;
      Inc(I);
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipRegistrar.CheckServerReturned(ExpectedStatusCode: Cardinal;
                                                  const Msg: String);
begin
  Check(Self.Dispatch.Transport.SentResponseCount > 0,
        Msg + ': No responses ever sent');
  CheckEquals(ExpectedStatusCode,
              Self.Dispatch.Transport.LastResponse.StatusCode,
              Msg
            + ': Status code of last response ('
            + Self.Dispatch.Transport.LastResponse.StatusText
            + ')');
end;

procedure TestTIdSipRegistrar.CheckServerReturnedOK(const Msg: String);
begin
  Self.CheckServerReturned(SIPOK, Msg);
end;

procedure TestTIdSipRegistrar.SimulateRemoteRequest;
begin
  Self.Dispatch.Transport.FireOnRequest(Self.Request);
end;

//* TestTIdSipRegistrar Published methods **************************************

procedure TestTIdSipRegistrar.TestComplicatedRegistration;
var
  Bindings: TIdSipContacts;
begin
  // This plays with a mis-ordered request.
  // We register 'sip:wintermute@talking-head.tessier-ashpool.co.luna' with CSeq 999
  // We register 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna' with CSeq 1001
  // We try to remove all registrations with CSeq 1000
  // We expect to see only the talking-head-2. (The remove-all is out of
  // order only with respect to the talking-head-2 URI.

  Self.Request.FirstContact.Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.SequenceNo    := 999;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 1: registering <sip:wintermute@talking-head.tessier-ashpool.co.luna>');

  Self.Request.FirstContact.Value := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna';
  Self.Request.CSeq.SequenceNo    := 1001;
  Self.Request.LastHop.Branch     := Self.Request.LastHop.Branch + '1';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 2: registering <sip:wintermute@talking-head-2.tessier-ashpool.co.luna>');

  Self.Request.FirstContact.IsWildCard := true;
  Self.Request.FirstContact.Expires    := 0;        
  Self.Request.CSeq.SequenceNo    := 1000;
  Self.Request.LastHop.Branch     := Self.Request.LastHop.Branch + '1';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 3: unregistering everything, out-of-order');

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Bindings);
    Bindings.First;
    Check(not Bindings.IsEmpty, 'All bindings were removed');
    Bindings.CurrentContact.RemoveExpires;
    CheckEquals('sip:wintermute@talking-head-2.tessier-ashpool.co.luna',
                Bindings.CurrentContact.Address.Uri,
                'Wrong binding removed');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrar.TestDatabaseUpdatesBindings;
var
  Contacts: TIdSipContacts;
begin
  Self.Request.FirstContact.Expires := Self.Registrar.MinimumExpiryTime + 1;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('First registration');
  Self.Request.FirstContact.Expires := Self.Registrar.MinimumExpiryTime + 100;
  Self.Request.CSeq.Increment;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Second registration');

  Contacts := TIdSipContacts.Create;
  try
    Self.Registrar.BindingDB.BindingsFor(Self.Request, Contacts);
    Check(not Contacts.IsEmpty, 'No contacts? Binding deleted?');
    Contacts.First;
    CheckEquals(Self.Request.FirstContact.Address.Uri,
                Contacts.CurrentContact.Address.Uri,
                'Binding DB not updated');
  finally
    Contacts.Free;
  end;
end;

procedure TestTIdSipRegistrar.TestDatabaseGetsExpiry;
var
  Expiry: TDateTime;
begin
  Self.FirstContact.Expires := Self.Registrar.MinimumExpiryTime + 1;
  Expiry := Now + OneSecond*Self.FirstContact.Expires;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Attempted registration');

  CheckEquals(1, Self.DB.BindingCount, 'Binding not added');

  // Of course, we're comparing two floats. The tolerance should be sufficient
  // to take into account heavy CPU loads. Besides, what's 200ms between
  // friends?
  CheckEquals(Expiry,
              Self.DB.Binding(Self.Request.AddressOfRecord,
                              Self.FirstContact.AsAddressOfRecord).ValidUntil,
              200*OneMillisecond,
              'Binding won''t expire at right time');
end;

procedure TestTIdSipRegistrar.TestFailedBindingsFor;
begin
  Self.DB.FailBindingsFor := true;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPInternalServerError,
                           'BindingsFor failed');
end;

procedure TestTIdSipRegistrar.TestFailedRemoveAll;
begin
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Registration');

  // We must change the branch or the UAS will think we want to send the
  // request to the old REGISTER transaction, which we don't. That
  // transaction's still alive because its Timer J hasn't fired - that's
  // usually a 32 second wait.
  Self.Request.LastHop.Branch := Self.Request.LastHop.Branch + '1';
  Self.Request.CSeq.Increment;
  Self.Request.FirstContact.Value := '*;expires=0';
  Self.DB.FailRemoveBinding := true;

  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPInternalServerError,
                           'Binding database failed during removal of bindings');
end;

procedure TestTIdSipRegistrar.TestInvalidAddressOfRecord;
begin
  Self.DB.FailIsValid := true;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPNotFound,
                           'Invalid address-of-record');
end;

procedure TestTIdSipRegistrar.TestOKResponseContainsAllBindings;
var
  Bindings: TIdSipContacts;
begin
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Adding binding');

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Bindings);
    CheckResponse(Bindings, 'OK response doesn''t contain all bindings');
  finally
    Bindings.Free;
  end;

  Check(Self.Dispatch.Transport.LastResponse.HasHeader(DateHeader),
        'Registrars SHOULD put a Date header in a 200 OK');
end;

procedure TestTIdSipRegistrar.TestReceiveInvite;
begin
  Self.Request.Method := MethodInvite;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPMethodNotAllowed,
                           'INVITE');
end;

procedure TestTIdSipRegistrar.TestReceiveRegister;
begin
  Self.SimulateRemoteRequest;
  CheckEquals(1,
              Self.Dispatch.Transport.SentResponseCount,
              'No response sent');
  CheckNotEquals(SIPMethodNotAllowed,
                 Self.Dispatch.Transport.LastResponse.StatusCode,
                'Registrars MUST accept REGISTER');
end;

procedure TestTIdSipRegistrar.TestReceiveExpireTooShort;
var
  Response: TIdSipResponse;
begin
  Self.Request.AddHeader(ExpiresHeader).Value := IntToStr(Self.Registrar.MinimumExpiryTime - 1);
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPIntervalTooBrief,
                           'Expires header value too low');
  Response := Self.Dispatch.Transport.LastResponse;
  Check(Response.HasHeader(MinExpiresHeader),
        MinExpiresHeader + ' missing');
  CheckEquals(Self.Registrar.MinimumExpiryTime,
              Response.FirstMinExpires.NumericValue,
              MinExpiresHeader + ' value');
end;

procedure TestTIdSipRegistrar.TestReceiveExpireParamTooShort;
var
  Response: TIdSipResponse;
begin
  Self.FirstContact.Expires := Self.Registrar.MinimumExpiryTime - 1;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPIntervalTooBrief,
                           'Expires param value too low');

  Response := Self.Dispatch.Transport.LastResponse;
  Check(Response.HasHeader(MinExpiresHeader),
        MinExpiresHeader + ' missing');
  CheckEquals(Self.Registrar.MinimumExpiryTime,
              Response.FirstMinExpires.NumericValue,
              MinExpiresHeader + ' value');
end;

procedure TestTIdSipRegistrar.TestReceiveWildcard;
begin
  Self.DB.AddBindings(Self.Request);
  // Remember the rules of RFC 3261 section 10.3 step 6!
  Self.Request.CallID := Self.Request.CallID + '1';

  Self.FirstContact.Value := Self.ExpireAll;
  Self.SimulateRemoteRequest;

  Self.CheckServerReturnedOK('Wildcard contact');

  CheckEquals(0, Self.DB.BindingCount, 'No bindings removed');
end;

procedure TestTIdSipRegistrar.TestReceiveWildcardWithExtraContacts;
begin
  Self.FirstContact.Value := Self.ExpireAll;
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:hiro@enki.org';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPBadRequest,
                           'Wildcard contact with another contact');
end;

procedure TestTIdSipRegistrar.TestReceiveWildcardWithNonzeroExpiration;
begin
  Self.FirstContact.Value := Self.ExpireAll;
  Self.FirstContact.Expires := 1;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPBadRequest,
                           'Wildcard contact with non-zero expires');
end;

procedure TestTIdSipRegistrar.TestRegisterAddsBindings;
var
  Bindings: TIdSipContacts;
begin
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Registration');

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Bindings);
    CheckEquals(1, Bindings.Count, 'Binding not added');

    CheckEquals(Self.FirstContact.Address.Uri,
                Bindings.Items[0].Value,
                'First (only) binding');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrar.TestRegisterAddsMultipleBindings;
var
  Bindings:      TIdSipContacts;
  SecondBinding: String;
begin
  SecondBinding := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := SecondBinding;

  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Registration of multiple bindings');

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Bindings);
    CheckEquals(2, Bindings.Count, 'Incorrect number of bindings');

    CheckEquals(Self.FirstContact.Address.Uri,
                Bindings.Items[0].Value,
                'First binding');
    CheckEquals(SecondBinding,
                Bindings.Items[1].Value,
                'Second binding');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrar.TestUnauthorizedUser;
begin
  Self.DB.Authorized := false;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPForbidden,
                           'Unauthorized user''s request not rejected');
end;

//******************************************************************************
//* TIdSipMockBindingDatabase                                                  *
//******************************************************************************
//* TIdSipMockBindingDatabase Public methods ***********************************

constructor TIdSipMockBindingDatabase.Create;
begin
  inherited Create;

  Self.BindingStore := TObjectList.Create(true);

  Self.Authorized        := true;
  Self.FailAddBinding    := false;
  Self.FailBindingsFor   := false;
  Self.FailIsValid       := false;
  Self.FailRemoveBinding := false;
end;

destructor TIdSipMockBindingDatabase.Destroy;
begin
  Self.BindingStore.Free;

  inherited Destroy;
end;

function TIdSipMockBindingDatabase.BindingCount: Integer;
begin
  Result := Self.BindingStore.Count;
end;

function TIdSipMockBindingDatabase.BindingsFor(Request: TIdSipRequest;
                                               Contacts: TIdSipContacts): Boolean;
var
  AddressOfRecord: String;
  ContactValue:    String;
  I:               Integer;
begin
  Contacts.Clear;

  AddressOfRecord := Request.AddressOfRecord;

  for I := 0 to Self.BindingStore.Count - 1 do
    if (Self.Bindings[I].AddressOfRecord = AddressOfRecord) then begin
      ContactValue := Self.Bindings[I].Uri
                    + ';' + ExpiresParam + '=';

      if (Now > Self.Bindings[I].ValidUntil) then
        ContactValue := ContactValue + '0'
      else
        ContactValue := ContactValue + IntToStr(SecondsBetween(Self.Bindings[I].ValidUntil, Now));

      Contacts.Add(ContactHeaderFull).Value := ContactValue;
    end;

  Result := not Self.FailBindingsFor;
end;

function TIdSipMockBindingDatabase.IsAuthorized(User: TIdSipAddressHeader): Boolean;
begin
  Result := Self.Authorized;
end;

function TIdSipMockBindingDatabase.IsValid(Request: TIdSipRequest): Boolean;
begin
  Result := not Self.FailIsValid;
end;

function TIdSipMockBindingDatabase.RemoveBinding(Request: TIdSipRequest;
                                                 Contact: TIdSipContactHeader): Boolean;
begin
  Self.DeleteBinding(Self.IndexOfBinding(Request.AddressOfRecord,
                                         Contact));

  Result := not Self.FailRemoveBinding;
end;

//* TIdSipMockBindingDatabase Protected methods ********************************

function TIdSipMockBindingDatabase.AddBinding(const AddressOfRecord: String;
                                              Contact: TIdSipContactHeader;
                                              const CallID: String;
                                              SequenceNo: Cardinal;
                                              ExpiryTime: TDateTime): Boolean;
var
  Index:      Integer;
  NewBinding: TIdRegistrarBinding;
begin
  NewBinding := TIdRegistrarBinding.Create(AddressOfRecord,
                                           Contact.AsAddressOfRecord,
                                           CallID,
                                           SequenceNo,
                                           Now + OneSecond*ExpiryTime);
  try
    Self.BindingStore.Add(NewBinding);
  except
    Index := Self.BindingStore.IndexOf(NewBinding);
    if (Index <> -1) then
      Self.DeleteBinding(Index)
    else
      FreeAndNil(NewBinding);

    raise;
  end;

  Result := not Self.FailAddBinding;
end;

function TIdSipMockBindingDatabase.Binding(const AddressOfRecord: String;
                                           const CanonicalUri: String): TIdRegistrarBinding;
var
  I: Integer;
begin
  Result := nil;
  I := 0;

  while (I < Self.BindingCount) and not Assigned(Result) do begin
    if (Self.Bindings[I].AddressOfRecord = AddressOfRecord) and
      (Self.Bindings[I].Uri = CanonicalUri) then
      Result := Self.Bindings[I]
    else
      Inc(I);
  end;
end;

procedure TIdSipMockBindingDatabase.Commit;
begin
end;

procedure TIdSipMockBindingDatabase.Rollback;
begin
end;

procedure TIdSipMockBindingDatabase.StartTransaction;
begin
end;

//* TIdSipMockBindingDatabase Private methods **********************************

procedure TIdSipMockBindingDatabase.DeleteBinding(Index: Integer);
begin
  if (Index >= 0) then
    Self.BindingStore.Delete(Index);
end;

function TIdSipMockBindingDatabase.GetBindings(Index: Integer): TIdRegistrarBinding;
begin
  Result := Self.BindingStore[Index] as TIdRegistrarBinding;
end;

function TIdSipMockBindingDatabase.IndexOfBinding(const AddressOfRecord: String;
                                                  Contact: TIdSipContactHeader): Integer;
begin
  Result := 0;
  while (Result < Self.BindingCount)
    and not (Self.Bindings[Result].AddressOfRecord = AddressOfRecord)
    and not (Self.Bindings[Result].Uri = Contact.AsAddressOfRecord) do
    Inc(Result);

  if (Result >= Self.BindingCount) then
    Result := -1;
end;

//******************************************************************************
//* TestTIdSipAbstractBindingDatabase
//******************************************************************************
//* TestTIdSipAbstractBindingDatabase Public methods ***************************

procedure TestTIdSipAbstractBindingDatabase.SetUp;
begin
  inherited SetUp;

  Self.DB := TIdSipMockBindingDatabase.Create;

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.CallID := 'selftest';
  Self.Request.CSeq.Method := MethodRegister;
  Self.Request.CSeq.SequenceNo := 999;
  Self.Request.RequestUri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.From.Address := Self.Request.RequestUri;
  Self.Request.ToHeader.Address := Self.Request.RequestUri;
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP proxy.tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := Self.Request.RequestUri.Uri;
end;

procedure TestTIdSipAbstractBindingDatabase.TearDown;
begin
  Self.Request.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractBindingDatabase Private methods **************************

procedure TestTIdSipAbstractBindingDatabase.CheckExpiry(Expected: Cardinal;
                                                        const Msg: String);
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Bindings.First;
    Self.DB.BindingsFor(Self.Request, Bindings);
    Bindings.First;

    // We don't assume to know how the rounding off of a most-of-a-second
    // (a second minus the time difference between adding the binding and
    // checking the binding's expiry) will behave, so we accept a difference
    // of 1 second.
    Check(1 >= Abs(Expected
                 - Bindings.CurrentContact.Expires),
          Msg);
  finally
    Bindings.Free;
  end;
end;

//* TestTIdSipAbstractBindingDatabase Published methods ************************

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingUpdates;
var
  Contacts:        TIdSipContacts;
  OriginalContact: String;
begin
  OriginalContact := Self.Request.FirstContact.AsAddressOfRecord;
  Self.DB.AddBindings(Self.Request);

  Self.Request.FirstContact.Value := 'sip:wintermute@neuromancer.tessier-ashpool.co.luna';
  Self.Request.CSeq.Increment;
  Self.DB.AddBindings(Self.Request);

  Contacts := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Contacts);

    Check(not Contacts.IsEmpty,
          'Binding deleted?');

    Contacts.First;
    CheckEquals(OriginalContact,
                Contacts.CurrentContact.AsAddressOfRecord,
                'Original binding removed');

    Check(Contacts.HasNext,
          'New binding not added');
    Contacts.Next;
    CheckEquals(Self.Request.FirstContact.Value,
                Contacts.CurrentContact.AsAddressOfRecord,
                'New binding corrupted');
  finally
    Contacts.Free;
  end;
end;

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingWithExpiryParam;
begin
  Self.Request.FirstContact.Expires := 20;

  Self.DB.AddBindings(Self.Request);

  Self.CheckExpiry(Self.Request.FirstContact.Expires,
                   'Expiry param');
end;

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingWithExpiryHeader;
var
  ExpiryTime: Cardinal;
begin
  ExpiryTime := 20;
  Self.Request.AddHeader(ExpiresHeader).Value := IntToStr(ExpiryTime);

  Self.DB.AddBindings(Self.Request);

  Self.CheckExpiry(ExpiryTime, 'Expiry header');
end;

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingWithNoExpiries;
begin
  Self.DB.AddBindings(Self.Request);

  Self.CheckExpiry(Self.DB.DefaultExpiryTime, 'No expiry param or header');
end;

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingWithZeroExpiresRemovesBinding;
var
  Contacts: TIdSipContacts;
begin
  Self.DB.AddBindings(Self.Request);
  Self.Request.CSeq.Increment;
  Self.Request.FirstContact.Expires := 0;
  Self.DB.AddBindings(Self.Request);

  Contacts := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Contacts);
    Check(Contacts.IsEmpty, 'Binding not removed');
  finally
    Contacts.Free;
  end;
end;

procedure TestTIdSipAbstractBindingDatabase.TestAddExistingBindingOutOfOrderSeqNo;
begin
  Self.DB.AddBindings(Self.Request);
  Check(not Self.DB.AddBindings(Self.Request),
        'Out-of-order request not ignored (replay attack)');

  Self.Request.CSeq.SequenceNo := Self.Request.CSeq.SequenceNo - 1;
  Check(not Self.DB.AddBindings(Self.Request),
        'Out-of-order request not ignored (earlier CSeq no)');
end;

procedure TestTIdSipAbstractBindingDatabase.TestNotOutOfOrder;
begin
  Self.DB.AddBindings(Self.Request);
  Self.Request.CSeq.Increment;
  Check(Self.DB.NotOutOfOrder(Self.Request,
                              Self.Request.FirstContact),
        'Normal behaviour');
end;

procedure TestTIdSipAbstractBindingDatabase.TestNotOutOfOrderEarlySeqNo;
begin
  Self.DB.AddBindings(Self.Request);
  Self.Request.CSeq.SequenceNo := Self.Request.CSeq.SequenceNo - 1;
  Check(not Self.DB.NotOutOfOrder(Self.Request,
                                  Self.Request.FirstContact),
        'Same Call-ID, early sequence no');
end;

procedure TestTIdSipAbstractBindingDatabase.TestNotOutOfOrderDifferentCallID;
begin
  Self.DB.AddBindings(Self.Request);
  Self.Request.CallID := Self.Request.CallID + '1';
  Check(Self.DB.NotOutOfOrder(Self.Request,
                              Self.Request.FirstContact),
        'Different Call-ID');
end;

procedure TestTIdSipAbstractBindingDatabase.TestRemoveAllBindings;
var
  Bindings:        TIdSipContacts;
  OriginalRequest: TIdSipRequest;
begin
  OriginalRequest := TIdSipRequest.Create;
  try
    OriginalRequest.Assign(Self.Request);

    Self.DB.AddBindings(Self.Request);
    Self.Request.CSeq.Increment;
    Self.Request.FirstContact.Expires := 0;
    Self.Request.FirstContact.IsWildCard := true;
    Check(Self.DB.RemoveAllBindings(Self.Request),
          'Notification of failure');

    Bindings := TIdSipContacts.Create;
    try
      Self.DB.BindingsFor(OriginalRequest, Bindings);
      Check(Bindings.IsEmpty, 'Bindings not deleted');
    finally
      Bindings.Free;
    end;
  finally
    OriginalRequest.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipMockBindingDatabase                                              *
//******************************************************************************
//* TestTIdSipMockBindingDatabase Public methods *******************************

procedure TestTIdSipMockBindingDatabase.SetUp;
begin
  inherited SetUp;

  Self.DB := TIdSipMockBindingDatabase.Create;

  Self.CasesAOR := TIdSipRequest.Create;
  Self.CasesAOR.Method         := MethodRegister;
  Self.CasesAOR.ToHeader.Value := 'sip:case@fried.neurons.org';
  Self.CasesAOR.RequestUri.Uri := 'sip:fried.neurons.org';
  Self.CaseContact := Self.CasesAOR.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  Self.CaseContact.Value := 'Case <' + Self.CasesAOR.RequestUri.Uri + '>';

  Self.WintermutesAOR := TIdSipRequest.Create;
  Self.WintermutesAOR.Method         := MethodRegister;
  Self.WintermutesAOR.ToHeader.Value := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.WintermutesAOR.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Wintermute := Self.WintermutesAOR.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  Self.Wintermute.Value := 'Wintermute <' + Self.WintermutesAOR.RequestUri.Uri + '>';
end;

procedure TestTIdSipMockBindingDatabase.TearDown;
begin
  Self.WintermutesAOR.Free;
  Self.CasesAOR.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipMockBindingDatabase Published methods ****************************

procedure TestTIdSipMockBindingDatabase.TestAddBindings;
var
  Bindings: TIdSipContacts;
begin
  Self.WintermutesAOR.AddHeader(Self.CaseContact);
  Self.DB.AddBindings(Self.WintermutesAOR);

  Bindings := TIdSipContacts.Create(Self.WintermutesAOR.Headers);
  try
    CheckEquals(Bindings.Count,
                Self.DB.BindingCount,
                'Bindings not added');

    Bindings.First;
    CheckEquals(Bindings.CurrentContact.Address.Uri,
                Self.DB.Bindings[0].Uri);
    Bindings.Next;
    CheckEquals(Bindings.CurrentContact.Address.Uri,
                Self.DB.Bindings[1].Uri);
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsFor;
var
  Bindings: TIdSipContacts;
begin
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.DB.AddBindings(Self.CasesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);
    //Writeln;
    //Writeln(Bindings.AsString);
    CheckEquals(2, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForClearsList;
var
  Bindings: TIdSipContacts;
begin
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.Wintermute);

    Self.DB.BindingsFor(Self.CasesAOR, Bindings);
    CheckEquals(0, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestIsAuthorized;
begin
  Self.DB.Authorized := true;
  Check(Self.DB.IsAuthorized(Self.CaseContact), 'Authorized');
  Self.DB.Authorized := false;
  Check(not Self.DB.IsAuthorized(Self.CaseContact), 'not Authorized');
end;


procedure TestTIdSipMockBindingDatabase.TestIsValid;
begin
  Self.DB.FailIsValid := true;
  Check(not Self.DB.IsValid(Self.WintermutesAOR), 'FailIsValid');

  Self.DB.FailIsValid := false;
  Check(Self.DB.IsValid(Self.WintermutesAOR), 'not FailIsValid');
end;

procedure TestTIdSipMockBindingDatabase.TestFailAddBinding;
begin
  Self.DB.FailAddBinding := false;
  Check(Self.DB.AddBindings(Self.WintermutesAOR),
        'AddBindings failed');

  Self.WintermutesAOR.CSeq.Increment;
  Self.DB.RemoveAllBindings(Self.WintermutesAOR);
  Self.WintermutesAOR.CSeq.Increment;
  Self.DB.FailAddBinding := true;
  Check(not Self.DB.AddBindings(Self.WintermutesAOR),
        'AddBindings succeeded');
end;

procedure TestTIdSipMockBindingDatabase.TestFailBindingsFor;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Self.DB.FailBindingsFor := false;
    Check(Self.DB.BindingsFor(Self.CasesAOR, Bindings),
          'BindingsFor failed');

    Self.DB.FailBindingsFor := true;
    Check(not Self.DB.BindingsFor(Self.CasesAOR, Bindings),
          'BindingsFor succeeded');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestFailRemoveBinding;
begin
  Self.DB.FailRemoveBinding := false;
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.WintermutesAOR.CSeq.Increment;
  Check(Self.DB.RemoveAllBindings(Self.WintermutesAOR),
        'RemoveAllBindings failed');

  Self.WintermutesAOR.CSeq.Increment;
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.WintermutesAOR.CSeq.Increment;
  Self.DB.FailRemoveBinding := true;
  Check(not Self.DB.RemoveAllBindings(Self.WintermutesAOR),
        'RemoveAllBindings succeeded');
end;

procedure TestTIdSipMockBindingDatabase.TestRemoveAllBindings;
begin
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.DB.AddBindings(Self.CasesAOR);
  Self.WintermutesAOR.CallID := Self.WintermutesAOR.CallID + '1';
  Self.DB.RemoveAllBindings(Self.WintermutesAOR);
  CheckEquals(1,
              Self.DB.BindingCount,
              'Number of records removed');
  CheckEquals(Self.CaseContact.Address.Uri,
              Self.DB.Bindings[0].Uri,
              'Wrong records removed');
end;

procedure TestTIdSipMockBindingDatabase.TestRemoveBinding;
var
  Bindings:    TIdSipContacts;
  ToBeDeleted: TIdSipContactHeader;
begin
  ToBeDeleted := TIdSipContactHeader.Create;
  try
    ToBeDeleted.Assign(Self.Wintermute);
    Self.DB.AddBindings(Self.WintermutesAOR);

    Self.DB.AddBindings(Self.CasesAOR);
    Self.CaseContact.Address.Host := 'sip:case@111.public.booth.org';
    Self.DB.AddBindings(Self.CasesAOR);
    Self.Wintermute.Address.Host := 'talking-head.tessier-ashpool.co.luna';
    Self.DB.AddBindings(Self.WintermutesAOR);

    Self.DB.RemoveBinding(Self.WintermutesAOR, ToBeDeleted);

    Bindings := TIdSipContacts.Create;
    try
      Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);
      CheckEquals(1,
                  Bindings.Count,
                  'One of Wintermute''s bindings not removed');

      Self.DB.BindingsFor(Self.CasesAOR, Bindings);
      CheckEquals(2,
                  Bindings.Count,
                  'One of Case''s bindings removed');
    finally
      Bindings.Free;
    end;
  finally
    ToBeDeleted.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestRemoveBindingWhenNotPresent;
begin
  Self.DB.RemoveBinding(Self.CasesAOR, Self.CaseContact);
  CheckEquals(0, Self.DB.BindingCount, 'No bindings at all');

  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.DB.RemoveBinding(Self.CasesAOR, Self.CaseContact);
  CheckEquals(1, Self.DB.BindingCount, 'Binding not in DB');
end;

initialization
  RegisterTest('Registrar', Suite);
end.
