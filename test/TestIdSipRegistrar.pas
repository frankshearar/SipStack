{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipRegistrar;

interface

uses
  IdSipCore, IdSipMessage, IdSipMockBindingDatabase,
  IdSipMockTransactionDispatcher, IdSipRegistration, TestFramework;

type
  // We test that the registrar returns the responses it should. The nitty
  // gritties of how the registrar preserves ACID properties, or the ins and
  // outs of actual database stuff don't interest us - look at
  // TestTIdSipAbstractBindingDatabase for that.
  TestTIdSipRegistrar = class(TTestCase)
  private
    DB:           TIdSipMockBindingDatabase;
    Dispatch:     TIdSipMockTransactionDispatcher;
    ExpireAll:    String;
    FirstContact: TIdSipContactHeader;
    Registrar:    TIdSipUserAgentCore;
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
    procedure TestMethod;
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



implementation

uses
  DateUtils, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistrar unit tests');
  Result.AddTest(TestTIdSipRegistrar.Suite);
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
  Self.Registrar := TIdSipUserAgentCore.Create;
  Self.Registrar.BindingDB := Self.DB;
  Self.Registrar.Dispatcher := Self.Dispatch;
  Self.Registrar.MinimumExpiryTime := 3600;
  Self.Registrar.AddModule(TIdSipRegisterModule);
  Self.Registrar.RemoveModule(TIdSipInviteModule);

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP proxy.tessier-ashpool.co.luna;branch='
                                               + BranchMagicCookie + 'f00L';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CallID := '1@selftest.foo';
  Self.Request.From.Address.Uri := 'sip:case@fried.neurons.org';

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
  Bindings:   TIdSipContacts;
  OldContact: String;
  NewContact: String;
begin
  // This plays with a mis-ordered request.
  // We register 'sip:wintermute@talking-head.tessier-ashpool.co.luna' with CSeq 999
  // We register 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna' with CSeq 1001
  // We try to remove all registrations with CSeq 1000
  // We expect to see only the talking-head-2. (The remove-all is out of
  // order only with respect to the talking-head-2 URI.

  OldContact := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  NewContact := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna';

  Self.Request.FirstContact.Value := OldContact;
  Self.Request.CSeq.SequenceNo    := 999;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 1: registering <' + OldContact + '>');

  Self.Request.FirstContact.Value := NewContact;
  Self.Request.CSeq.SequenceNo    := 1001;
  Self.Request.LastHop.Branch     := Self.Request.LastHop.Branch + '1';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 2: registering <' + NewContact + '>');

  Self.Request.FirstContact.IsWildCard := true;
  Self.Request.FirstContact.Expires    := 0;
  Self.Request.CSeq.SequenceNo         := 1000;
  Self.Request.LastHop.Branch          := Self.Request.LastHop.Branch + '1';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturnedOK('Step 3: unregistering everything, out-of-order');

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Bindings);
    Bindings.First;
    Check(not Bindings.IsEmpty, 'All bindings were removed');
    Bindings.CurrentContact.RemoveExpires;
    CheckEquals(NewContact,
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
              Self.DB.BindingExpires(Self.Request.AddressOfRecord,
                                     Self.FirstContact.AsAddressOfRecord),
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

procedure TestTIdSipRegistrar.TestMethod;
begin
  CheckEquals(MethodRegister,
              TIdSipInboundRegistration.Method,
              'Inbound registration; Method');
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
  Self.Request.Method      := MethodInvite;
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.SimulateRemoteRequest;
  
  Self.CheckServerReturned(SIPMethodNotAllowed,
                           'INVITE');
end;

procedure TestTIdSipRegistrar.TestReceiveRegister;
var
  RegistrationCount: Cardinal;
begin
  RegistrationCount := Self.Registrar.RegistrationCount;
  Self.SimulateRemoteRequest;
  CheckEquals(1,
              Self.Dispatch.Transport.SentResponseCount,
              'No response sent');
  CheckNotEquals(SIPMethodNotAllowed,
                 Self.Dispatch.Transport.LastResponse.StatusCode,
                'Registrars MUST accept REGISTER');
  CheckEquals(RegistrationCount,
              Self.Registrar.RegistrationCount,
              'InboundRegistration object not freed');
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

initialization
  RegisterTest('Registrar', Suite);
end.
